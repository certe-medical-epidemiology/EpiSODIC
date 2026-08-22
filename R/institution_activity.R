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

#' Load optional institution activity (patient-days)
#'
#' Writes operator-supplied weekly hospital activity to
#' `episodic_institution_activity`, keyed on `institution_key` (resolved to
#' `institution_id` here, mirroring `episodic_denominators_load()`'s
#' own optional-source pattern). Entirely optional: a site with nothing to
#' supply here simply never calls this, and L1/L2 Farrington detection
#' falls back to raw counts - unnormalised, not broken: patient-day
#' normalisation is a refinement, not a requirement.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param activity A data frame (or tibble) with `institution_key`, `period_start`,
#'   `period_end`, `patient_days` (nullable `admissions`, `n_beds`,
#'   `source`).
#' @return Invisibly, the number of rows written (rows whose
#'   `institution_key` does not match a known institution are skipped,
#'   not an error - an operator's activity feed and case feed need not be
#'   perfectly synchronised).
#'
#' Not exported: an operator supplies a source to [episodic_run_cron()] via
#' `institution_activity`; this is the internal write step run
#' against it.
#' @keywords internal
#' @noRd
episodic_institution_activity_load <- function(con, activity) {
  required_cols <- c("institution_key", "period_start", "period_end", "patient_days")
  missing_cols <- setdiff(required_cols, names(activity))
  if (length(missing_cols) > 0) {
    stop(
      "Institution activity data is missing required column(s): ",
      paste(missing_cols, collapse = ", "), call. = FALSE
    )
  }

  institutions <- episodic_db_institutions(con)
  n_written <- 0L
  for (i in seq_len(nrow(activity))) {
    row <- activity[i, ]
    institution_id <- institutions$institution_id[institutions$institution_key == row$institution_key]
    if (length(institution_id) == 0) next
    episodic_db_institution_activity_upsert(
      con, institution_id = institution_id[1], period_start = row$period_start,
      period_end = row$period_end, patient_days = row$patient_days,
      admissions = row$admissions %||% NA, n_beds = row$n_beds %||% NA,
      source = row$source %||% NA
    )
    n_written <- n_written + 1L
  }
  invisible(n_written)
}

#' Add a hospital activity feed (patient-days)
#'
#' Raw case counts at a hospital can rise simply because the hospital is
#' busier, not because infection risk has increased. If you can supply
#' weekly patient-days (or admissions, or bed counts) per hospital,
#' EpiSODIC can normalise case counts against activity for a more reliable
#' signal. This feed is entirely optional: without it, detection falls back
#' to raw counts, which is a reasonable default, not a broken one.
#'
#' This function is a synthetic example showing the expected shape: weekly
#' patient-days per hospital, modelled as bed count times occupancy, with a
#' realistic winter peak. Use it as a template for your own data, which you
#' pass to [episodic_run_cron()] as `institution_activity` -
#' normally a data frame or `tibble`.
#'
#' @param institutions A data frame (or tibble) of institutions (as
#'   returned by your own institution registry), filtered internally to
#'   hospitals only.
#' @param start_date,end_date The period to generate weekly rows for.
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame with `institution_key`, `period_start`,
#'   `period_end`, `patient_days`, `n_beds`, `source`.
#' @examples
#' institutions <- data.frame(
#'   institution_key = "HOSP-1", institution_type = "hospital", n_beds = 320
#' )
#' activity <- episodic_synthetic_institution_activity(
#'   institutions, start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' head(activity)
#' @export
episodic_synthetic_institution_activity <- function(institutions, start_date = as.Date("2021-01-01"),
                                                            end_date = as.Date("2025-12-31"), seed = 1) {
  set.seed(seed)
  hospitals <- institutions[institutions$institution_type == "hospital", ]
  if (nrow(hospitals) == 0) {
    return(data.frame(institution_key = character(0), period_start = character(0),
                       period_end = character(0), patient_days = integer(0),
                       n_beds = integer(0), source = character(0), stringsAsFactors = FALSE))
  }

  week_starts <- seq(start_date, end_date, by = "week")
  rows <- lapply(seq_len(nrow(hospitals)), function(i) {
    h <- hospitals[i, ]
    doy <- as.integer(format(week_starts, "%j"))
    seasonal_occupancy <- 0.82 + 0.08 * cos(2 * pi * (doy - 15) / 365.25)  # winter peak
    patient_days <- round(h$n_beds * pmin(pmax(seasonal_occupancy, 0.5), 1) * 7 *
                             stats::runif(length(week_starts), 0.95, 1.05))
    data.frame(
      institution_key = h$institution_key,
      period_start = as.character(week_starts),
      period_end = as.character(week_starts + 6),
      patient_days = patient_days,
      n_beds = h$n_beds,
      source = "synthetic",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
