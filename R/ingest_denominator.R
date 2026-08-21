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

#' Ingest optional positivity metadata
#'
#' Writes the operator-supplied, pre-aggregated denominator table (see
#' `README.md`'s data format section) to `episodic_denominator`. Entirely
#' optional: a site with nothing to supply
#' here simply never calls this, and positivity panels stay blank for its
#' streams. Deliberately not a raw per-test linelist, so volume stays a
#' handful of aggregate rows per pathogen/period/stratum rather than every
#' individual test result.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param denominators A data frame with columns `pathogen`, `sample_date`,
#'   `care_line`, `area_code` (nullable) and `n_tests`.
#' @return Invisibly, the number of rows written.
#'
#' Not exported: an operator supplies a source to [episodic_run_cron()] via
#' `denominator_source_fn`; this is the internal write step run against it.
#' @keywords internal
#' @noRd
episodic_denominator_ingest_run <- function(con, denominators) {
  required_cols <- c("pathogen", "sample_date", "care_line", "area_code", "n_tests")
  missing_cols <- setdiff(required_cols, names(denominators))
  if (length(missing_cols) > 0) {
    stop(
      "Denominator source is missing required column(s): ",
      paste(missing_cols, collapse = ", "), call. = FALSE
    )
  }

  for (i in seq_len(nrow(denominators))) {
    row <- denominators[i, ]
    episodic_db_denominator_upsert(
      con, pathogen = row$pathogen, sample_date = row$sample_date,
      care_line = row$care_line, area_code = row$area_code, n_tests = row$n_tests
    )
  }
  invisible(nrow(denominators))
}

#' Add a testing-volume (positivity) feed
#'
#' Case counts alone cannot distinguish a rise in infections from a rise in
#' testing. If you can supply how many tests were performed - even as a
#' weekly aggregate, not per-test detail - EpiSODIC can show a positivity
#' rate alongside the case count, which is often the more meaningful signal.
#' This feed is entirely optional: skip it and positivity panels simply stay
#' blank.
#'
#' This function is a synthetic example showing the expected shape: weekly
#' counts of a multiplex GI PCR panel that also reports Norovirus. Use it as
#' a template for your own function, which you pass to [episodic_run_cron()]
#' as `denominator_source_fn`. Your function should return a data frame with
#' the same five columns: `pathogen`, `sample_date` (week start),
#' `care_line`, `area_code` (may be `NA`), and `n_tests`.
#'
#' @param start_date,end_date The period to generate weekly rows for.
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame with `pathogen`, `sample_date` (week start),
#'   `care_line`, `area_code`, `n_tests`.
#' @examples
#' denom <- episodic_denominator_source_synthetic(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' head(denom)
#' @export
episodic_denominator_source_synthetic <- function(start_date = as.Date("2021-01-01"),
                                                  end_date = as.Date("2025-12-31"),
                                                  seed = 1) {
  set.seed(seed)
  week_starts <- seq(start_date, end_date, by = "week")
  n <- length(week_starts)
  data.frame(
    pathogen = "Norovirus",
    sample_date = as.character(week_starts),
    care_line = "second",
    area_code = NA_character_,
    n_tests = stats::rpois(n, lambda = 40),
    stringsAsFactors = FALSE
  )
}
