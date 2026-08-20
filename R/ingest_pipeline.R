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

#' Run ingestion: validate, deduplicate, resolve institutions, write cases
#'
#' Ties together `R/ingest_interface.R`, `R/ingest_dedup.R` and the cron
#' repository layer. Institutions are normalised on ingestion: a hospital
#' or long-term care institution is kept as a first-class entity, while
#' a GP practice is collapsed to its municipality, keyed by a hash of the
#' source identifier so a later rename does not fracture the history.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param raw A data frame satisfying the ingestion interface, e.g. from
#'   [episode_ingest_source_synthetic()].
#' @param pathogen_config A data frame from `episode_db_pathogen_config()`.
#' @param run_id The `run_id` of the current detection run
#'   (`episode_case.first_seen_run`).
#' @return Invisibly, a list with `n_raw`, `n_deduplicated` and `n_inserted`.
#'
#' Not exported: an operator's own transform step supplies a raw source to
#' [episode_run_cron()] via `ingest_source_fn`, which calls this internally
#' with the pieces (`pathogen_config`, `run_id`) only a run in progress has
#' - never something a caller assembles by hand.
#' @keywords internal
#' @noRd
episode_ingest_run <- function(con, raw, pathogen_config, run_id) {
  episode_ingest_validate_source(raw)
  deduped <- episode_dedup(raw, pathogen_config)

  institution_lookup <- episode_ingest_resolve_institutions(con, deduped)

  cases <- deduped
  cases$institution_id <- institution_lookup[cases$institution_key]
  cases <- cases[, c(
    "source_key", "patient_key", "sample_date", "receipt_date", "pathogen",
    "care_line", "institution_id", "ward",
    "specialism", "pc", "sex", "age"
  )]

  n_inserted <- episode_db_case_insert_new(con, cases, run_id)

  invisible(list(n_raw = nrow(raw), n_deduplicated = nrow(deduped), n_inserted = n_inserted))
}

#' Upsert every distinct institution referenced in a raw/deduped batch
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame with the ingestion interface's
#'   `institution_key`/`institution_display_name`/`institution_type`/
#'   `care_line`/`municipality` columns.
#' @return A named integer vector mapping the raw `institution_key` to the
#'   database `institution_id`.
#' @keywords internal
#' @noRd
episode_ingest_resolve_institutions <- function(con, cases) {
  distinct <- unique(cases[, c(
    "institution_key", "institution_display_name", "institution_type",
    "care_line", "municipality"
  )])

  ids <- integer(nrow(distinct))
  for (i in seq_len(nrow(distinct))) {
    row <- distinct[i, ]
    hashed_key <- digest::digest(row$institution_key, algo = "sha1", serialize = FALSE)
    ids[i] <- episode_db_institution_upsert(
      con,
      institution_key = hashed_key,
      display_name = row$institution_display_name,
      institution_type = row$institution_type,
      care_line = row$care_line,
      municipality = row$municipality,
      is_monitored = row$institution_type == "hospital"
    )
  }
  stats::setNames(ids, distinct$institution_key)
}
