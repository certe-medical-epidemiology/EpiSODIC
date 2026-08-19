#' Ingest optional institution activity (patient-days)
#'
#' Writes operator-supplied weekly hospital activity to
#' `episode_institution_activity`, keyed on `institution_key` (resolved to
#' `institution_id` here, mirroring `episode_denominator_ingest_run()`'s
#' own optional-source pattern). Entirely optional: a site with nothing to
#' supply here simply never calls this, and L1/L2 Farrington detection
#' falls back to raw counts - unnormalised, as it always has been, not
#' broken (ARCHITECTURE.md section 7.1's patient-day normalisation is a
#' refinement, not a requirement).
#'
#' @param con A [DBI::DBIConnection-class].
#' @param activity A data frame with `institution_key`, `period_start`,
#'   `period_end`, `patient_days` (nullable `admissions`, `n_beds`,
#'   `source`).
#' @return Invisibly, the number of rows written (rows whose
#'   `institution_key` does not match a known institution are skipped,
#'   not an error - an operator's activity feed and case feed need not be
#'   perfectly synchronised).
#' @export
episode_institution_activity_ingest_run <- function(con, activity) {
  required_cols <- c("institution_key", "period_start", "period_end", "patient_days")
  missing_cols <- setdiff(required_cols, names(activity))
  if (length(missing_cols) > 0) {
    stop(
      "Institution activity source is missing required column(s): ",
      paste(missing_cols, collapse = ", "), call. = FALSE
    )
  }

  institutions <- episode_db_institutions(con)
  n_written <- 0L
  for (i in seq_len(nrow(activity))) {
    row <- activity[i, ]
    institution_id <- institutions$institution_id[institutions$institution_key == row$institution_key]
    if (length(institution_id) == 0) next
    episode_db_institution_activity_upsert(
      con, institution_id = institution_id[1], period_start = row$period_start,
      period_end = row$period_end, patient_days = row$patient_days,
      admissions = row$admissions %||% NA, n_beds = row$n_beds %||% NA,
      source = row$source %||% NA
    )
    n_written <- n_written + 1L
  }
  invisible(n_written)
}

#' Synthetic institution activity source
#'
#' A worked example of the optional activity contract
#' (`episode_institution_activity_ingest_run()`): weekly patient-days per
#' hospital, modelled as `n_beds * occupancy` with light seasonal
#' variation (winter admissions run higher) and noise. Not called by
#' [episode_run_cron()] unless an `institution_activity_source_fn` is
#' supplied; demonstrates the shape only, so the bundled demo can show
#' ARCHITECTURE.md section 7.1's incidence-density curve without a real
#' hospital feed.
#'
#' @param institutions A data frame from `episode_db_institutions()` (or
#'   `episode_synthetic_institutions()`'s own shape before insertion),
#'   filtered internally to `institution_type == "hospital"`.
#' @param start_date,end_date The period to generate weekly rows for.
#' @param seed RNG seed.
#' @return A data frame with `institution_key`, `period_start`,
#'   `period_end`, `patient_days`, `n_beds`, `source`.
#' @export
episode_synthetic_institution_activity_source <- function(institutions, start_date = as.Date("2021-01-01"),
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
