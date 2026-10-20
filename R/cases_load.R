# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Load case data: validate, normalise, deduplicate, resolve institutions, write
#'
#' Ties together `R/cases.R`, `R/cases_dedup.R` and the cron
#' repository layer. A missing `care_line` is normalised to `"unknown"`
#' here, so an operator's extract step need not distinguish `NA` from the
#' string the schema stores. Institutions are normalised as cases are loaded: a
#' hospital or long-term care institution is kept as a first-class entity,
#' while a GP practice is collapsed to its municipality, keyed by a hash of
#' the source identifier so a later rename does not fracture the history.
#'
#' Deduplication looks beyond the current batch: before grouping positives
#' into episodes, EpiSODIC fetches each incoming
#' patient/pathogen's most recently stored episode anchor, so a
#' patient/pathogen combination that already has a case on file is
#' recognised correctly even if the positive that opened that episode
#' is not in this batch. This is what lets an operator send only a recent
#' window of positives on each run instead of the full case history every
#' time - see `episodic_cases_deduplicate()`.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame (or tibble) satisfying the case data
#'   requirements, e.g. from [episodic_synthetic_cases()].
#' @param pathogen_config A data frame from `episodic_db_pathogen_config()`.
#' @param run_id The `run_id` of the current detection run
#'   (`episodic_case.first_seen_run`).
#' @param origin_instance_id The instance each row was imported from, as
#'   `episodic_exchange_import()` stamps it, or `NA` for this instance's
#'   own cases: one value per row, or one for all. An imported row's
#'   `institution_key` is already the stored hash (the exporting instance
#'   hashed it, and its file declares so), so it is used as it is; hashing
#'   it again would match no institution anywhere.
#' @return Invisibly, a list with `n_supplied`, `n_deduplicated` and `n_inserted`.
#'
#' Not exported: an operator's own transform step supplies the case data
#' to [episodic_run_cron()] via `cases`, which calls this internally
#' with the pieces (`pathogen_config`, `run_id`) only a run in progress has
#' - never something a caller assembles by hand.
#' @keywords internal
#' @noRd
episodic_cases_load <- function(con,
                                cases,
                                pathogen_config,
                                run_id,
                                origin_instance_id = NA_character_) {
  # Validated by episodic_run_cron() before the run starts, so that a
  # data problem reaches the operator who can fix it rather than only the
  # run row - kept here as the last guard for any other caller.
  episodic_validate_cases(cases)

  # An absent care line, an R NA and a database NULL all mean the same
  # thing - we do not know which part of the health system this came from
  # - and the schema has one way of saying it. Normalise here rather than
  # asking every operator's extract step to do it, and rather than letting
  # a NOT NULL violation surface mid-run.
  cases$care_line[is.na(cases$care_line)] <- "unknown"

  # Added after validation: the case data interface is an allow-list, and
  # the origin is the run's knowledge about a row, not a column a feed
  # supplies.
  if (!length(origin_instance_id) %in% c(1L, nrow(cases))) {
    stop(
      "`origin_instance_id` must have one value, or one per case row ",
      "(", nrow(cases), "), not ", length(origin_instance_id), ".",
      call. = FALSE
    )
  }
  cases$origin_instance_id <- rep_len(as.character(origin_instance_id), nrow(cases))
  own <- is.na(cases$origin_instance_id)
  bad_hash <- !own & !grepl("^[0-9a-f]{40}$", cases$institution_key)
  if (any(bad_hash)) {
    stop(
      sum(bad_hash), " imported case row(s) carry an institution_key that ",
      "is not a stored SHA-1 hash (40 lowercase hexadecimal characters); ",
      "an exchange file always carries the hash, so these rows did not come ",
      "from episodic_exchange_import().",
      call. = FALSE
    )
  }

  existing <- episodic_db_last_case_dates(
    con,
    unique(cases$patient_key),
    unique(cases$pathogen)
  )
  deduped <- episodic_cases_deduplicate(
    cases,
    pathogen_config,
    existing = existing
  )

  institution_lookup <- episodic_institutions_resolve(con, deduped)

  to_insert <- deduped
  to_insert$institution_id <- unname(institution_lookup[
    episodic_institution_key_stored(to_insert)
  ])
  to_insert <- to_insert[, c(
    "source_key",
    "lab_number",
    "patient_key",
    "sample_date",
    "receipt_date",
    "pathogen",
    "care_line",
    "institution_id",
    "ward",
    "specialism",
    "pc",
    "sex",
    "age",
    "origin_instance_id"
  )]

  n_inserted <- episodic_db_case_insert_new(con, to_insert, run_id)

  invisible(list(
    n_supplied = nrow(cases),
    n_deduplicated = nrow(deduped),
    n_inserted = n_inserted
  ))
}

#' Upsert every distinct institution referenced in a batch of cases
#'
#' An imported row may add an institution this instance has never seen,
#' and never rename or retype one it already knows: an institution's
#' name and type are this instance's own record of it, and a partner who
#' spells a shared hospital differently must not change what the
#' dashboard calls it. Where one key arrives on both a local and an
#' imported row, the local row is the one kept.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame with the case data's
#'   `institution_key`/`institution_display_name`/`institution_type`/
#'   `care_line`/`municipality` columns, and optionally
#'   `origin_instance_id` (`NA`, or the column absent, for a local row).
#' @return A named integer vector mapping each institution's stored key
#'   (`episodic_institution_key_stored()`) to the database
#'   `institution_id`.
#' @keywords internal
#' @noRd
episodic_institutions_resolve <- function(con, cases) {
  # An institution is keyed on institution_key alone, so keep exactly one
  # row per key: the last, which is what episodic_check_cases() already
  # tells an operator to expect when one key carries two names.
  #
  # Taking distinct *combinations* of the five columns instead let one key
  # appear twice in the same batch - a hospital reporting two care lines is
  # enough, and nothing about the requirements forbids it - and a batched insert
  # cannot upsert a row against another row of its own statement: MariaDB
  # rejects the second copy on institution_key's UNIQUE index and the whole
  # run rolls back before a single case is written. The loop tolerated it
  # because each iteration was its own statement.
  imported <- if ("origin_instance_id" %in% names(cases)) {
    !is.na(cases$origin_instance_id)
  } else {
    rep(FALSE, nrow(cases))
  }
  # Imported rows first, so that the last row per key - the one kept -
  # is a local one wherever a local one exists.
  ordering <- order(!imported)
  ordered <- cases[ordering, , drop = FALSE]
  ordered_imported <- imported[ordering]
  ordered$hashed_key <- episodic_institution_key_stored(ordered)
  keep <- !duplicated(ordered$hashed_key, fromLast = TRUE)
  distinct <- ordered[
    keep,
    c(
      "institution_key",
      "hashed_key",
      "institution_display_name",
      "institution_type",
      "care_line",
      "municipality"
    ),
    drop = FALSE
  ]
  distinct_imported <- ordered_imported[keep]

  # Read what is on file once, write the batch once, then read the ids
  # back once - rather than a SELECT, an INSERT or UPDATE and a
  # LAST_INSERT_ID() per institution, which for a few hundred of them was
  # over a thousand round trips and most of a run's case-loading time.
  #
  # Reading the ids back by key also keeps LAST_INSERT_ID() out of this
  # path entirely, which matters beyond speed: it returns a BIGINT, and it
  # was that value's bit64::integer64 type - silently flattened to a
  # subnormal double on assignment into an ordinary vector - that once
  # wrote every case's institution_id to the database as 0.
  on_file <- episodic_db_get_query(
    con,
    "SELECT institution_id, institution_key FROM episodic_institution"
  )
  known <- match(distinct$hashed_key, on_file$institution_key)

  is_monitored <- as.integer(distinct$institution_type == "hospital")
  new <- is.na(known)
  if (any(new)) {
    episodic_db_write_many(
      con,
      table = "episodic_institution",
      cols = c(
        "institution_key",
        "display_name",
        "institution_type",
        "care_line",
        "municipality",
        "is_monitored",
        "is_active"
      ),
      values = list(
        institution_key = distinct$hashed_key[new],
        display_name = distinct$institution_display_name[new],
        institution_type = distinct$institution_type[new],
        care_line = distinct$care_line[new],
        municipality = distinct$municipality[new],
        is_monitored = is_monitored[new],
        is_active = rep(1L, sum(new))
      )
    )
  }
  update <- !new & !distinct_imported
  if (any(update)) {
    episodic_db_write_many(
      con,
      table = "episodic_institution",
      cols = c(
        "institution_key",
        "display_name",
        "institution_type",
        "care_line",
        "municipality",
        "is_monitored",
        "is_active"
      ),
      values = list(
        institution_key = distinct$hashed_key[update],
        display_name = distinct$institution_display_name[update],
        institution_type = distinct$institution_type[update],
        care_line = distinct$care_line[update],
        municipality = distinct$municipality[update],
        is_monitored = is_monitored[update],
        is_active = rep(1L, sum(update))
      ),
      key_cols = "institution_key",
      update_cols = c(
        "display_name",
        "institution_type",
        "care_line",
        "municipality",
        "is_monitored"
      )
    )
  }

  on_file <- episodic_db_get_query(
    con,
    "SELECT institution_id, institution_key FROM episodic_institution"
  )
  ids <- as.integer(
    on_file$institution_id[match(distinct$hashed_key, on_file$institution_key)]
  )
  stats::setNames(ids, distinct$hashed_key)
}

#' The stored form of each row's institution key
#'
#' A local row's key is the operator's own identifier, hashed here; an
#' imported row's is already the hash the sending instance stored, and is
#' used as it is. Matching on the stored form rather than on what the
#' row carries matters where the two could look alike: an operator who
#' hashes their own codes before handing them over supplies, as a raw
#' key, exactly the string a partner's file carries as a stored one, and
#' the two are different institutions here.
#'
#' @param cases A data frame with `institution_key` and optionally
#'   `origin_instance_id`.
#' @return A character vector of 40-character SHA-1 hex digests.
#' @keywords internal
#' @noRd
episodic_institution_key_stored <- function(cases) {
  imported <- if ("origin_instance_id" %in% names(cases)) {
    !is.na(cases$origin_instance_id)
  } else {
    rep(FALSE, nrow(cases))
  }
  stored <- as.character(cases$institution_key)
  stored[!imported] <- episodic_institution_key_hash(stored[!imported])
  stored
}

#' The stored form of an operator's own institution identifier
#'
#' `episodic_institution.institution_key` holds a SHA-1 of whatever
#' identifier the operator uses internally, never the identifier itself,
#' so an institution can be tracked across runs without EpiSODIC's
#' database carrying a laboratory's internal codes in the clear. Every
#' feed that names an institution therefore has to be hashed the same way
#' before it can be matched.
#'
#' In one place, because a second place to do the hashing is a place to
#' forget it. `vignette("data-format")` tells an operator the activity
#' feed's `institution_key` "matches the cases feed", so a loader that
#' compares that key raw against the stored hash matches nothing on any
#' correctly prepared deployment: activity rows skipped, patient-day
#' normalisation never engaged, and every run finishing `partial` with a
#' warning that the keys match no institution - true, and entirely the
#' wrong thing to send anyone looking for.
#'
#' @param institution_key A character vector of operator-supplied keys.
#' @return A character vector of 40-character SHA-1 hex digests.
#' @keywords internal
#' @noRd
episodic_institution_key_hash <- function(institution_key) {
  vapply(
    as.character(institution_key),
    function(k) digest::digest(k, algo = "sha1", serialize = FALSE),
    character(1),
    USE.NAMES = FALSE
  )
}
