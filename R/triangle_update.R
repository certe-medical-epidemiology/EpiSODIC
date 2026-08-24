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

#' Reporting triangle
#'
#' Because sample date silently becomes receipt date when the physician
#' leaves it blank, reporting delay cannot be
#' read off the date columns. It is measured from observed accrual instead:
#' how many cases with a given sample date were visible on each successive
#' run date.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id The stream to update the triangle for.
#' @param cases_for_stream A data frame of that stream's current cases
#'   (all of them, not just this run's), with `sample_date`.
#' @param run_date The date of the current run (`as.character(Sys.Date())`
#'   in production; injectable for tests).
#' @return Invisibly, the number of `(sample_date, run_date)` rows written.
#' @keywords internal
#' @noRd
episodic_triangle_update <- function(
    con,
    stream_id,
    cases_for_stream,
    run_date) {
  if (nrow(cases_for_stream) == 0) {
    return(invisible(0L))
  }

  counts <- table(cases_for_stream$sample_date)
  sample_dates <- names(counts)
  n_cases <- as.integer(counts)

  for (i in seq_along(sample_dates)) {
    episodic_db_reporting_triangle_upsert(
      con,
      stream_id = stream_id,
      sample_date = sample_dates[i],
      run_date = run_date,
      n_cases = n_cases[i]
    )
  }
  invisible(length(sample_dates))
}

#' Empirical completion curve for a stream
#'
#' The proportion of eventually-reported cases for a given sample date
#' that were already visible D days later, averaged across historical
#' sample dates. Used to shade the "incompleteness zone" on the epi
#' curve and to size the rolling detection window - its own empirical
#' completion curve decides how many trailing days are under-ascertained
#' by construction.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id The stream to compute completeness for.
#' @param max_lag_days Maximum reporting lag (in days) to compute.
#' @return A data frame with columns `lag_days` and `completeness`
#'   (0-1, the median share of the final count visible at that lag).
#' @keywords internal
#' @noRd
episodic_triangle_completeness <- function(con, stream_id, max_lag_days = 21) {
  triangle <- DBI::dbGetQuery(
    con,
    "SELECT sample_date, run_date, n_cases FROM episodic_reporting_triangle WHERE stream_id = ?",
    params = list(stream_id)
  )
  if (nrow(triangle) == 0) {
    return(data.frame(lag_days = integer(0), completeness = numeric(0)))
  }

  triangle$sample_date <- as.Date(triangle$sample_date)
  triangle$run_date <- as.Date(triangle$run_date)
  triangle$lag_days <- as.integer(triangle$run_date - triangle$sample_date)

  final_counts <- stats::aggregate(n_cases ~ sample_date, triangle, max)
  names(final_counts)[2] <- "final_n"
  merged <- merge(triangle, final_counts, by = "sample_date")
  merged <- merged[
    merged$final_n > 0 & merged$lag_days >= 0 & merged$lag_days <= max_lag_days,
  ]
  if (nrow(merged) == 0) {
    return(data.frame(lag_days = integer(0), completeness = numeric(0)))
  }
  merged$share <- merged$n_cases / merged$final_n

  by_lag <- stats::aggregate(share ~ lag_days, merged, stats::median)
  names(by_lag) <- c("lag_days", "completeness")
  by_lag[order(by_lag$lag_days), ]
}
