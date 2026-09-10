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

#' An empty detection result, in the shape every detector returns
#'
#' Five call sites built this by hand from `episodic_detection_record()`'s
#' zero-length branch, which is five places for the column set to drift
#' apart in.
#' @return A zero-row data frame with the detection columns.
#' @keywords internal
#' @noRd
episodic_detection_none <- function() {
  episodic_detection_record(
    integer(0),
    character(0),
    character(0),
    character(0),
    integer(0)
  )
}

#' Whether a detector is switched on for this run
#'
#' Each detector's own configuration section carries `enabled`, shipped
#' `true`. Switched off, the detector contributes no detections at all -
#' which is what a drop-one analysis needs (see
#' `episodic_validate_detection()`), and what an operator who runs, say,
#' no seasonal detector at all needs too.
#'
#' It lives in the configuration rather than in an argument to
#' `episodic_run_cron()` deliberately: which detectors ran changes what a
#' run computes, so it belongs inside `config_hash`, where two runs over
#' the same data with different detectors on cannot come out looking
#' identical.
#'
#' Fails closed on anything it cannot read as a single `TRUE`/`FALSE`,
#' the way `episodic_app_require_login()` does: a detector silently left
#' running by a malformed setting is the more dangerous of the two
#' mistakes only for a login wall, but a detector silently switched off
#' by one is the more dangerous here, so an unreadable value is an error
#' rather than either default.
#'
#' @param config The resolved configuration.
#' @param detector The configuration section's name, e.g. `"farrington"`.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
episodic_detector_enabled <- function(config, detector) {
  enabled <- config[[detector]]$enabled
  if (is.null(enabled)) {
    # An instance configuration cannot remove the key (the shipped
    # defaults always supply it and the merge is key-by-key), so this is
    # only reachable from a hand-built config in a test or a caller
    # passing a fragment. Treat it as configured on, which is what the
    # shipped defaults say.
    return(TRUE)
  }
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    stop(
      "`",
      detector,
      ".enabled` must be true or false, not ",
      paste(format(enabled), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  enabled
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
#' A backfill run lifts the bound entirely, once. The pathology above is
#' re-emission, not emission: a historical window matches a cluster that
#' is already on the board and resets its clock. On the first run against
#' a database there is no cluster on the board and no clock to reset, so
#' reporting the archive costs nothing and withholding it is what leaves
#' an operator with an empty dashboard and no way to tell that from a
#' broken one. Every run after it is bounded again, so nothing is ever
#' reported twice. See `episodic_run_is_backfill()`.
#'
#' @param run_date The date the run treats as "today".
#' @param lookback_days The configured window, in days. `NULL`, `NA` or a
#'   value that is not a positive finite number means "no bound", which
#'   is what a configuration that deliberately sets `lookback_days: ~`
#'   asks for.
#' @param backfill When `TRUE`, no bound at all, whatever
#'   `lookback_days` says.
#' @return A single `Date`, or `NULL` for "no bound".
#' @keywords internal
#' @noRd
episodic_detector_lookback_cutoff <- function(run_date,
                                              lookback_days,
                                              backfill = FALSE) {
  if (isTRUE(backfill)) {
    return(NULL)
  }
  if (length(lookback_days) != 1) {
    return(NULL)
  }
  lookback_days <- suppressWarnings(as.numeric(lookback_days))
  if (is.na(lookback_days) || !is.finite(lookback_days) || lookback_days < 0) {
    return(NULL)
  }
  as.Date(run_date) - lookback_days
}

#' Drop hit windows outside the run's own reporting window
#'
#' Judged on `last_day` at the back: a long window whose cases run up to
#' yesterday is current news however far back it started, and a window
#' that ended before the cutoff is settled history whatever its length.
#'
#' @param windows A list of `list(first_day, last_day, n_cases)`.
#' @param cutoff From `episodic_detector_lookback_cutoff()`; `NULL`
#'   applies no lower bound.
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

#' What a lookback window kept out of a detector's own output
#'
#' A rule-based detector that reports nothing says the same thing in the
#' trace whether it found nothing at all or found a hundred hits and
#' discarded every one of them for being older than
#' `lookback_days`. Those are opposite findings: the first is a quiet
#' catchment, the second is an archive the run is deliberately not
#' reporting, which is what a first run against a backfilled history
#' looks like. Recorded here as an attribute on the detector's own
#' result - the same way `episodic_farrington_insufficient()` records a
#' history shortfall - and read back by `episodic_detector_trace_lookback()`.
#'
#' @param x The detection record the detector is about to return.
#' @param dropped How many hits the lookback kept out.
#' @param cutoff The cutoff applied, from
#'   `episodic_detector_lookback_cutoff()`; `NULL` when unbounded.
#' @param lookback_days The configured window, for the message.
#' @return `x`, annotated.
#' @keywords internal
#' @noRd
episodic_detector_lookback_note <- function(x,
                                            dropped,
                                            cutoff,
                                            lookback_days) {
  attr(x, "episodic_lookback") <- list(
    dropped = as.integer(dropped),
    cutoff = cutoff,
    lookback_days = lookback_days
  )
  x
}

#' Say in the trace what the lookback kept out, when it kept anything out
#'
#' Silent when the detector reported everything it found, so an ordinary
#' nightly run gains no line; a run that discarded hits says how many,
#' from when, and which setting decided it.
#'
#' @param x A detection record annotated by
#'   `episodic_detector_lookback_note()`.
#' @param detector The detector's name, for the message.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_detector_trace_lookback <- function(x, detector) {
  note <- attr(x, "episodic_lookback", exact = TRUE)
  if (is.null(note) || is.null(note$dropped) || note$dropped < 1) {
    return(invisible(NULL))
  }
  episodic_trace(
    detector,
    " did not report ",
    note$dropped,
    " further hit(s), whose last case fell before ",
    format(note$cutoff),
    " (",
    detector,
    ".lookback_days is ",
    note$lookback_days,
    "). A run reports what falls inside that window, so a first run ",
    "against a backfilled history reports none of the archive it holds."
  )
  invisible(NULL)
}

#' Say where the case history ends relative to the date the run treats as today
#'
#' Both rule-based detectors report only what falls inside their
#' `lookback_days`, and Farrington only tests weeks that end on or before
#' `run_date`. An extract whose newest case is older than those windows
#' therefore produces a run that detects nothing while every log line in
#' it reads as an ordinary quiet night. Stating the span turns that into
#' something an operator can see; saying so outright, when the newest case
#' is beyond every rule-based lookback there is, names the reason before
#' they go looking for a defect in the detectors.
#'
#' A historical extract is detected against by giving `episodic_run_cron()`
#' a `run_date` inside the extract's own window, which is what a
#' prospective replay (`episodic_validate_detection()`) does week by week.
#'
#' @param cases The run's full case history, with `sample_date`.
#' @param config The resolved configuration.
#' @param run_date The date the run treats as today.
#' @param backfill From `episodic_run_is_backfill()`. The span is stated
#'   either way; the notice about the lookback windows is not, since on a
#'   backfill run they keep nothing out and there is nothing to warn of.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_trace_case_recency <- function(cases,
                                        config,
                                        run_date,
                                        backfill = FALSE) {
  # A diagnostic must never be the thing that stops a run, so it says
  # nothing at all about input it cannot read.
  if (is.null(cases) || nrow(cases) == 0 || is.null(cases$sample_date)) {
    return(invisible(NULL))
  }
  dates <- suppressWarnings(as.Date(cases$sample_date))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(invisible(NULL))
  }
  run_date <- as.Date(run_date)
  newest <- max(dates)
  behind <- as.integer(run_date - newest)
  episodic_trace(
    "Case history on file spans ",
    format(min(dates)),
    " to ",
    format(newest),
    ", ending ",
    behind,
    " day(s) before this run's date (",
    format(run_date),
    ")"
  )

  if (isTRUE(backfill)) {
    return(invisible(NULL))
  }

  # The longest window any rule-based detector will still report from. A
  # detector left unbounded (`lookback_days: ~`) reports from any date at
  # all, so there is nothing to warn about at any distance.
  cutoffs <- list(
    episodic_detector_lookback_cutoff(
      run_date,
      config$same_place$lookback_days
    ),
    episodic_detector_lookback_cutoff(
      run_date,
      config$rare_trigger$lookback_days
    )
  )
  if (any(vapply(cutoffs, is.null, logical(1)))) {
    return(invisible(NULL))
  }
  earliest <- min(do.call(c, cutoffs))
  if (newest >= earliest) {
    return(invisible(NULL))
  }
  episodic_trace(
    "the newest case on file is ",
    behind,
    " day(s) before this run's date, which is older than every ",
    "rule-based detector's lookback window (the longest reaches back to ",
    format(earliest),
    ") - same_place and rare_trigger can report nothing this run, ",
    "whatever the case history contains. Detect against a historical ",
    "extract by giving episodic_run_cron() a run_date inside the ",
    "extract's own window."
  )
  invisible(NULL)
}

#' A run's own cases: everything sampled on or before its `run_date`
#'
#' Farrington has always had this bound, since `episodic_weekly_bins()`
#' stops at the last complete week on or before `run_date` and a case
#' dated later simply falls in no bin. The rule-based detectors did not,
#' so a run replayed as of 2020 reported an outbreak from 2025 - which
#' makes `run_date` mean one thing for one detector and nothing at all
#' for the other two, and makes a prospective replay (the only way to
#' measure detection delay honestly) report the future.
#'
#' It matters in ordinary operation too, if less dramatically: a sample
#' date in the future is a data-entry error `episodic_check_cases()`
#' raises as advice rather than refusing, so it does reach the
#' detectors, and it should wait until the day it claims to be.
#'
#' @param cases A data frame with `sample_date`.
#' @param run_date The date the run treats as today.
#' @return The subset of `cases` sampled on or before `run_date`.
#' @keywords internal
#' @noRd
episodic_detector_cases_asof <- function(cases, run_date) {
  if (is.null(cases) || nrow(cases) == 0) {
    return(cases)
  }
  dates <- suppressWarnings(as.Date(cases$sample_date))
  cases[which(!is.na(dates) & dates <= as.Date(run_date)), , drop = FALSE]
}
