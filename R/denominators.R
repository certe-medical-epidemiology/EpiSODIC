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

#' Load optional positivity metadata
#'
#' Writes the operator-supplied, pre-aggregated denominator table (see
#' `vignette("data-format")`'s "Positivity metadata" section) to
#' `episodic_denominator`. Entirely optional: a site with nothing to supply
#' here simply never calls this, and positivity panels stay blank for its
#' streams. Deliberately not a raw per-test linelist, so volume stays a
#' handful of aggregate rows per pathogen/period/stratum rather than every
#' individual test result.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param denominators A data frame (or tibble) with columns `pathogen`,
#'   `sample_date`, `care_line`, `area_code` (nullable) and `n_tests`.
#' @return Invisibly, a list with `n_supplied` and `n_written`. Every
#'   supplied row is written, so these are equal - a denominator row has
#'   nothing to match against and so cannot be skipped.
#'
#' Not exported: an operator supplies a source to [episodic_run_cron()] via
#' `denominators`; this is the internal write step run against it.
#' @keywords internal
#' @noRd
episodic_denominators_load <- function(con, denominators) {
  # Checked by episodic_run_cron() before the run starts as well, so a
  # problem with this feed reaches the operator the same way a problem
  # with the case feed does.
  episodic_validate_denominators(denominators)

  # Same reading of a missing care line as the case feed: unknown.
  denominators$care_line[is.na(denominators$care_line)] <- "unknown"

  for (i in seq_len(nrow(denominators))) {
    row <- denominators[i, ]
    episodic_db_denominator_upsert(
      con,
      pathogen = row$pathogen,
      sample_date = row$sample_date,
      care_line = row$care_line,
      area_code = row$area_code,
      n_tests = row$n_tests
    )
  }
  invisible(list(
    n_supplied = nrow(denominators),
    n_written = nrow(denominators)
  ))
}

#' Check the optional denominator feed against its own contract
#'
#' Split out of the load step so [episodic_run_cron()] can run it before
#' the run writes anything: an operator who supplies a testing-volume feed
#' should learn what is wrong with it where they can fix it, not from a
#' rolled-back transaction.
#'
#' @param denominators A data frame (or tibble) with columns `pathogen`,
#'   `sample_date`, `care_line`, `area_code` (nullable) and `n_tests`.
#' @return Invisibly, `NULL`. Throws otherwise.
#' @keywords internal
#' @noRd
episodic_validate_denominators <- function(denominators) {
  episodic_validate_columns(
    denominators,
    required = c(
      "pathogen",
      "sample_date",
      "care_line",
      "area_code",
      "n_tests"
    ),
    filled = c("pathogen", "sample_date", "n_tests"),
    what = "Denominator data"
  )
  episodic_validate_allowed(
    denominators,
    "care_line",
    episodic_care_lines,
    na_ok = TRUE,
    what = "Denominator data"
  )
  episodic_validate_dates(
    denominators,
    "sample_date",
    na_ok = FALSE,
    what = "Denominator data"
  )
  if (nrow(denominators) > 0 && !is.numeric(denominators$n_tests)) {
    stop(
      "Denominator data has a non-numeric `n_tests` (",
      paste(class(denominators$n_tests), collapse = "/"),
      "). Give the number of tests performed for this pathogen, period ",
      "and stratum as a number.",
      call. = FALSE
    )
  }
  invisible(NULL)
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
#' a template for your own data, which you pass to [episodic_run_cron()] as
#' `denominators` - a data frame or `tibble` with the same five
#' columns: `pathogen`, `sample_date` (week start), `care_line`,
#' `area_code` (may be `NA`), and `n_tests`.
#'
#' @param start_date,end_date The period to generate weekly rows for.
#'   Defaults to the five years up to today, matching
#'   [episodic_synthetic_cases()].
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame with `pathogen`, `sample_date` (week start),
#'   `care_line`, `area_code`, `n_tests`.
#' @examples
#' denom <- episodic_synthetic_denominators(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' head(denom)
#' @export
episodic_synthetic_denominators <- function(
  start_date = end_date - 5 * 365,
  end_date = Sys.Date(),
  seed = 1
) {
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
