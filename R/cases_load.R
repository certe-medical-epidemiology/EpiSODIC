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
#'   contract, e.g. from [episodic_synthetic_cases()].
#' @param pathogen_config A data frame from `episodic_db_pathogen_config()`.
#' @param run_id The `run_id` of the current detection run
#'   (`episodic_case.first_seen_run`).
#' @return Invisibly, a list with `n_supplied`, `n_deduplicated` and `n_inserted`.
#'
#' Not exported: an operator's own transform step supplies the case data
#' to [episodic_run_cron()] via `cases`, which calls this internally
#' with the pieces (`pathogen_config`, `run_id`) only a run in progress has
#' - never something a caller assembles by hand.
#' @keywords internal
#' @noRd
episodic_cases_load <- function(con, cases, pathogen_config, run_id) {
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
  to_insert$institution_id <- institution_lookup[to_insert$institution_key]
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
    "age"
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
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame with the case data contract's
#'   `institution_key`/`institution_display_name`/`institution_type`/
#'   `care_line`/`municipality` columns.
#' @return A named integer vector mapping the supplied `institution_key` to the
#'   database `institution_id`.
#' @keywords internal
#' @noRd
episodic_institutions_resolve <- function(con, cases) {
  # An institution is keyed on institution_key alone, so keep exactly one
  # row per key: the last, which is where the per-institution upsert loop
  # this replaced ended up, and what episodic_check_cases() already tells an
  # operator to expect when one key carries two names.
  #
  # Taking distinct *combinations* of the five columns instead let one key
  # appear twice in the same batch - a hospital reporting two care lines is
  # enough, and nothing about the contract forbids it - and a batched insert
  # cannot upsert a row against another row of its own statement: MariaDB
  # rejects the second copy on institution_key's UNIQUE index and the whole
  # run rolls back before a single case is written. The loop tolerated it
  # because each iteration was its own statement.
  distinct <- cases[
    !duplicated(cases$institution_key, fromLast = TRUE),
    c(
      "institution_key",
      "institution_display_name",
      "institution_type",
      "care_line",
      "municipality"
    ),
    drop = FALSE
  ]

  distinct$hashed_key <- vapply(
    distinct$institution_key,
    function(k) digest::digest(k, algo = "sha1", serialize = FALSE),
    character(1),
    USE.NAMES = FALSE
  )

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
  on_file <- DBI::dbGetQuery(
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
  if (any(!new)) {
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
        institution_key = distinct$hashed_key[!new],
        display_name = distinct$institution_display_name[!new],
        institution_type = distinct$institution_type[!new],
        care_line = distinct$care_line[!new],
        municipality = distinct$municipality[!new],
        is_monitored = is_monitored[!new],
        is_active = rep(1L, sum(!new))
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

  on_file <- DBI::dbGetQuery(
    con,
    "SELECT institution_id, institution_key FROM episodic_institution"
  )
  ids <- as.integer(
    on_file$institution_id[match(distinct$hashed_key, on_file$institution_key)]
  )
  stats::setNames(ids, distinct$institution_key)
}
