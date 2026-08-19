#' Ingest optional positivity metadata
#'
#' Writes the operator-supplied, pre-aggregated denominator table (see
#' `QUESTIONS.md` item 22 and `README.md`'s data format section) to
#' `episode_denominator`. Entirely optional: a site with nothing to supply
#' here simply never calls this, and positivity panels stay blank for its
#' streams. Deliberately not a raw per-test linelist, so volume stays a
#' handful of aggregate rows per pathogen/period/stratum rather than every
#' individual test result.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param denominators A data frame with columns `pathogen`, `sample_date`,
#'   `care_line`, `area_code` (nullable) and `n_tests`.
#' @return Invisibly, the number of rows written.
#' @export
episode_denominator_ingest_run <- function(con, denominators) {
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
    episode_db_denominator_upsert(
      con, pathogen = row$pathogen, sample_date = row$sample_date,
      care_line = row$care_line, area_code = row$area_code, n_tests = row$n_tests
    )
  }
  invisible(nrow(denominators))
}

#' Synthetic positivity metadata source
#'
#' A worked example of the optional denominator contract
#' (`episode_denominator_ingest_run()`): weekly test-panel counts for a
#' multiplex GI PCR panel that reports Norovirus alongside several other
#' targets, the kind of aggregate a lab LIS can produce trivially even
#' though it would never hand over the underlying per-test rows. Not called
#' by [episode_run_cron()] unless a `denominator_source_fn` is supplied;
#' demonstrates the shape only.
#'
#' @param start_date,end_date The period to generate weekly rows for.
#' @param seed RNG seed.
#' @return A data frame with `pathogen`, `sample_date` (week start),
#'   `care_line`, `area_code`, `n_tests`.
#' @export
episode_denominator_source_synthetic <- function(start_date = as.Date("2021-01-01"),
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
