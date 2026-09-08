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

#' Common detection record
#'
#' Every detector, whatever its source, produces the same shape of record
#' so that reconciliation (`R/reconcile_*.R`) never needs to know which
#' detector fired. This matches the columns of `episodic_detection`.
#'
#' @param stream_id The stream this detection belongs to.
#' @param detector One of `'clusters'`, `'farrington'`, `'ears'`, `'mem'`,
#'   `'rare_trigger'`, `'same_place'`.
#' @param first_day,last_day The interval the detection covers.
#' @param n_cases Case count in the interval.
#' @param expected,upperbound Statistical detector output; `NA` for
#'   detectors without a baseline model (e.g. `same_place`).
#' @param params A list of detector-specific attributes, stored as JSON.
#' @return A one-row data frame in the shape reconciliation expects.
#' @keywords internal
#' @noRd
episodic_detection_record <- function(stream_id,
                                      detector,
                                      first_day,
                                      last_day,
                                      n_cases,
                                      expected = NA_real_,
                                      upperbound = NA_real_,
                                      params = list()) {
  if (length(n_cases) == 0) {
    return(data.frame(
      stream_id = integer(0),
      detector = character(0),
      first_day = character(0),
      last_day = character(0),
      n_cases = integer(0),
      expected = numeric(0),
      upperbound = numeric(0),
      params = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    stream_id = stream_id,
    detector = detector,
    first_day = as.character(first_day),
    last_day = as.character(last_day),
    n_cases = as.integer(n_cases),
    expected = as.numeric(expected),
    upperbound = as.numeric(upperbound),
    params = as.character(jsonlite::toJSON(params, auto_unbox = TRUE)),
    stringsAsFactors = FALSE
  )
}

#' The oldest `last_day` a rule-based detector may still report
#'
#' `same_place` and `rare_trigger` need no baseline, so nothing in their
#' own logic bounds how far back they look - and unbounded is what they
#' were, rescanning the whole case history on every run and re-emitting
#' every hit it had ever contained. That is not a performance nicety: a
#' re-emitted historical detection matches its own settled cluster during
#' reconciliation and resets `runs_since_detected` to zero, so
#' `reconciliation.close_after_runs` never fires for it and the cluster
#' stays on the board for good. `farrington.max_weeks_tested` bounds the
#' statistical detector for the same reason; this is that bound for the
#' rule-based ones.
#'
#' @param run_date The date the run treats as "today".
#' @param lookback_days The configured window, in days. `NULL`, `NA` or a
#'   value that is not a positive finite number means "no bound", which
#'   is the pre-existing behaviour and is what a configuration that
#'   deliberately sets `lookback_days: ~` asks for.
#' @return A single `Date`, or `NULL` for "no bound".
#' @keywords internal
#' @noRd
episodic_detector_lookback_cutoff <- function(run_date, lookback_days) {
  if (length(lookback_days) != 1) {
    return(NULL)
  }
  lookback_days <- suppressWarnings(as.numeric(lookback_days))
  if (is.na(lookback_days) || !is.finite(lookback_days) || lookback_days < 0) {
    return(NULL)
  }
  as.Date(run_date) - lookback_days
}

#' Drop hit windows that ended before the lookback cutoff
#'
#' Judged on `last_day`, not `first_day`: a long window whose cases run
#' up to yesterday is current news however far back it started, and a
#' window that ended before the cutoff is settled history whatever its
#' length.
#'
#' @param windows A list of `list(first_day, last_day, n_cases)`.
#' @param cutoff From `episodic_detector_lookback_cutoff()`; `NULL` keeps
#'   every window.
#' @return The kept subset of `windows`.
#' @keywords internal
#' @noRd
episodic_detector_windows_within <- function(windows, cutoff) {
  if (is.null(cutoff) || length(windows) == 0) {
    return(windows)
  }
  keep <- vapply(
    windows,
    function(w) as.Date(w$last_day) >= cutoff,
    logical(1)
  )
  windows[keep]
}
