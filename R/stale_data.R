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

#' How long this instance has gone without new cases of its own
#'
#' A case feed that stops refreshing does not fail: every run succeeds,
#' takes in nothing new, raises nothing, and looks exactly like a quiet
#' week. This puts a number on it: the days from the last run that took
#' in a new case of this instance's own (imported cases excluded, since
#' they come from another laboratory's feed) to this run.
#'
#' Counted on runs rather than on sample dates, because what stalls is
#' the delivery: a result for a sample taken last week that arrives today
#' is new data, and a feed handing over the same rows every day is not.
#'
#' A run that cannot be judged returns `NULL`, never a count of zero
#' days: the check is switched off (`stale_data.max_days_without_new_cases`
#' set to `~`), or no case of this instance's own has ever arrived.
#'
#' @param con A [DBI::DBIConnection-class], inside the run's transaction,
#'   after this run's cases are loaded.
#' @param config The resolved configuration.
#' @param run_date The run's date.
#' @return `NULL`, or a list: `days` (whole days since new cases last
#'   arrived, `0` when this run took some in), `last_new_cases_date` (the
#'   date of the run that took them in, a `Date`), `threshold` (the
#'   configured maximum) and `stale` (`days > threshold`).
#' @keywords internal
#' @noRd
episodic_stale_data_check <- function(con, config, run_date) {
  threshold <- episodic_stale_data_threshold(config)
  if (is.null(threshold)) {
    return(NULL)
  }
  last <- episodic_db_last_new_cases_date(con, run_date)
  if (is.na(last)) {
    return(NULL)
  }
  days <- as.integer(as.Date(run_date) - last)
  list(
    days = days,
    last_new_cases_date = last,
    threshold = threshold,
    stale = days > threshold
  )
}

#' The configured maximum number of days without new cases
#'
#' @param config The resolved configuration.
#' @return `NULL` when the check is switched off, otherwise a single
#'   whole number of at least 1; throws on anything else, since a
#'   threshold that cannot be read would otherwise switch the check off
#'   without saying so.
#' @keywords internal
#' @noRd
episodic_stale_data_threshold <- function(config) {
  x <- config$stale_data$max_days_without_new_cases
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x != round(x)) {
    stop(
      "`stale_data.max_days_without_new_cases` must be a whole number of ",
      "days, at least 1, or ~ to switch the check off.",
      call. = FALSE
    )
  }
  as.integer(x)
}

#' Log the stale-data check
#'
#' @param check From `episodic_stale_data_check()`.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_stale_data_trace <- function(check) {
  if (is.null(check)) {
    episodic_trace(
      "Stale-data check: not judged (switched off, or no case of this ",
      "instance's own has arrived yet)"
    )
    return(invisible(NULL))
  }
  if (isTRUE(check$stale)) {
    episodic_trace(
      "No new cases of this instance's own for ",
      check$days,
      " day(s), more than the ",
      check$threshold,
      " allowed: the last arrived in the run of ",
      format(check$last_new_cases_date),
      ". Check the extract that feeds EpiSODIC.",
      severity = "warn"
    )
  } else {
    episodic_trace(
      "Stale-data check: new cases last arrived in the run of ",
      format(check$last_new_cases_date),
      " (",
      check$days,
      " day(s) ago, ",
      check$threshold,
      " allowed)"
    )
  }
  invisible(NULL)
}
