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

#' Farrington detector
#'
#' Wraps `surveillance::farringtonFlexible()` directly (CRAN, `Imports`, no
#' Certe dependency), replacing the earlier `certestats::detect_farrington()`
#' wrapper. `certestats` was only ever a thin linelist-to-`sts` adapter
#' around this exact function; owning that glue removes the last
#' Certe-specific package from the detection path and, unlike the
#' `certestats` wrapper it replaces, can actually be installed and tested in
#' any environment, CI included.
#'
#' Case dates are aggregated into weekly bins (Monday-starting), which is
#' standard practice for Farrington-style surveillance; `frequency = 52` is
#' used for the underlying `sts` object, the conventional approximation
#' (a true year is ~52.18 weeks, so bin-to-calendar alignment drifts
#' slightly over many years - a documented, widely-accepted simplification,
#' not an EpiSODIC-specific one). Detection is only evaluated for the most
#' recent complete week relative to `run_date`.
#'
#' @param cases_for_stream A data frame of a single stream's cases, with
#'   `sample_date`.
#' @param stream_id The stream these cases belong to.
#' @param config The resolved configuration; uses `config$farrington`.
#' @param run_date The date to treat as "today"; the most recent week
#'   evaluated is the one containing this date.
#' @param population An optional numeric vector of weekly population
#'   (patient-days) aligned to `episode_weekly_bins()`'s own weeks, from
#'   `episode_farrington_population_vector()`. `NULL` (default) fits on
#'   raw counts, unnormalised - what every stream without institution
#'   activity data gets.
#' @return A data frame of detection records (zero or one row: Farrington
#'   evaluates only the current week).
#' @references
#' Farrington CP, Andrews NJ, Beale AD, Catchpole MA (1996). "A Statistical
#' Algorithm for the Early Detection of Outbreaks of Infectious Disease."
#' *Journal of the Royal Statistical Society Series A*, 159(3), 547-563.
#' \doi{10.2307/2983331}
#'
#' Noufaily A, Enki DG, Farrington P, Garthwaite P, Andrews N, Charlett A
#' (2013). "An Improved Algorithm for Outbreak Detection in Multiple
#' Surveillance Systems." *Statistics in Medicine*, 32(7), 1206-1222.
#' \doi{10.1002/sim.5595}
#'
#' Salmon M, Schumacher D, Hoehle M (2016). "Monitoring Count Time Series
#' in R: Aberration Detection in Public Health Surveillance." *Journal of
#' Statistical Software*, 70(10), 1-35. \doi{10.18637/jss.v070.i10}
#' (the `surveillance` package, whose `farringtonFlexible()` implements
#' the Noufaily et al. algorithm and is called directly here).
#' @keywords internal
#' @noRd
episode_detect_farrington <- function(cases_for_stream, stream_id, config, run_date = Sys.Date(),
                                       population = NULL) {
  empty <- episode_detection_record(integer(0), character(0), character(0), character(0), integer(0))

  dates <- as.Date(cases_for_stream$sample_date)
  if (length(dates) == 0) return(empty)

  fc <- config$farrington
  weekly <- episode_weekly_bins(dates, run_date)

  min_weeks_required <- (fc$b + 1) * 52
  if (length(weekly$counts) < min_weeks_required) {
    return(empty)  # insufficient baseline history for the configured b
  }

  result <- episode_farrington_fit(weekly, range_idx = length(weekly$counts), fc = fc, population = population)
  if (is.null(result) || !isTRUE(as.logical(result@alarm[1, 1]))) return(empty)

  current_week_start <- weekly$week_start[length(weekly$week_start)]
  episode_detection_record(
    stream_id = stream_id, detector = "farrington",
    first_day = as.character(current_week_start),
    last_day = as.character(current_week_start + 6),
    n_cases = as.integer(result@observed[1, 1]),
    expected = as.numeric(result@control$expected[1, 1]),
    upperbound = as.numeric(result@upperbound[1, 1]),
    params = list(b = fc$b, w = fc$w, alpha = fc$alpha)
  )
}

#' Weekly Farrington trend points, for the multi-year trend panel
#'
#' The app performs cheap reads only and cannot re-run
#' `farringtonFlexible()` at render time, so the cron must persist a
#' continuous expected/upperbound series rather than only the current
#' week's alarm status (see `episode_detect_farrington()`). This
#' function returns that series; the caller
#' (`episode_run_cron()`) is responsible for upserting it into
#' `episode_stream_trend`.
#'
#' Only the trailing `n_weeks_to_compute` weeks are (re-)evaluated per call,
#' not the whole history: on a normal nightly run that is 1 (today's week
#' alone), which is cheap. A fresh stream with no trend rows yet gets a
#' larger backfill (bounded by `max_backfill_weeks`) so the panel is not
#' empty on first render, at the cost of a heavier one-off computation.
#'
#' @param cases_for_stream A data frame of a single stream's cases.
#' @param config The resolved configuration; uses `config$farrington`.
#' @param run_date The date to treat as "today".
#' @param n_weeks_existing How many trend weeks are already persisted for
#'   this stream (from `episode_db_stream_trend()`); determines how much
#'   backfill is attempted.
#' @param max_backfill_weeks Cap on how many weeks a single call will ever
#'   (re-)compute, to bound cron run time. Matches the multi-year trend
#'   panel's own display window.
#' @param population See `episode_detect_farrington()`.
#' @return A data frame with `week_start`, `n_cases`, `expected`,
#'   `upperbound` (zero rows if ineligible).
#' @keywords internal
#' @noRd
episode_farrington_trend <- function(cases_for_stream, config, run_date = Sys.Date(),
                                      n_weeks_existing = 0L, max_backfill_weeks = 156L,
                                      population = NULL) {
  empty <- data.frame(week_start = as.Date(character(0)), n_cases = integer(0),
                       expected = numeric(0), upperbound = numeric(0), stringsAsFactors = FALSE)

  dates <- as.Date(cases_for_stream$sample_date)
  if (length(dates) == 0) return(empty)

  fc <- config$farrington
  weekly <- episode_weekly_bins(dates, run_date)

  min_weeks_required <- (fc$b + 1) * 52
  if (length(weekly$counts) < min_weeks_required) return(empty)

  n_weeks_to_compute <- if (n_weeks_existing == 0) max_backfill_weeks else 1L
  range_idx <- seq(
    max(min_weeks_required, length(weekly$counts) - n_weeks_to_compute + 1),
    length(weekly$counts)
  )

  result <- episode_farrington_fit(weekly, range_idx = range_idx, fc = fc, population = population)
  if (is.null(result)) return(empty)

  data.frame(
    week_start = weekly$week_start[range_idx],
    n_cases = as.integer(result@observed[, 1]),
    expected = as.numeric(result@control$expected[, 1]),
    upperbound = as.numeric(result@upperbound[, 1]),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
episode_farrington_fit <- function(weekly, range_idx, fc, population = NULL) {
  use_population <- !is.null(population) && length(population) == length(weekly$counts)
  sts_obj <- surveillance::sts(
    observed = weekly$counts,
    start = c(as.integer(format(weekly$week_start[1], "%Y")), 1),
    frequency = 52,
    population = if (use_population) population else NULL
  )
  control <- list(
    range = range_idx, b = fc$b, w = fc$w, reweight = isTRUE(fc$reweight),
    weightsThreshold = fc$weightsThreshold, trend = isTRUE(fc$trend),
    pastWeeksNotIncluded = fc$pastWeeksNotIncluded, limit54 = unlist(fc$limit54),
    alpha = fc$alpha, populationOffset = use_population
  )
  tryCatch(
    suppressWarnings(surveillance::farringtonFlexible(sts_obj, control = control)),
    error = function(e) NULL
  )
}

#' Build a weekly patient-days vector aligned to Farrington's weekly bins
#'
#' Institution streams (L2) use `farringtonFlexible()`'s
#' `populationOffset` with weekly patient-days rather than raw counts,
#' since occupancy varies seasonally and a count-based baseline would
#' alarm on a busy February and stay silent through a quiet August at
#' identical transmission-per-patient-day. `NULL` when `institution_id`
#' is `NA` (non-institution streams) or no activity data exists at all
#' for it - callers pass `NULL` straight through to
#' `episode_detect_farrington()`/`episode_farrington_trend()`, which
#' then fit on raw counts.
#'
#' Only `episode_institution_activity` (L2, whole-institution) exists in
#' the schema - there is no ward-level (L1) activity table, so L1
#' streams always get `NULL` here regardless of `institution_id`.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param institution_id An `episode_institution` id, or `NA`.
#' @param level A stream's `level`, from `episode_stream`.
#' @param week_start The `Date` vector from `episode_weekly_bins()`.
#' @return A numeric vector the same length as `week_start`, or `NULL`.
#' @keywords internal
#' @noRd
episode_farrington_population_vector <- function(con, institution_id, level, week_start) {
  if (!identical(level, "pathogen_institution") || is.na(institution_id)) return(NULL)
  activity <- episode_db_institution_activity(con, institution_id)
  if (nrow(activity) == 0) return(NULL)

  activity_start <- as.Date(activity$period_start)
  activity_end <- as.Date(activity$period_end)
  fallback <- stats::median(activity$patient_days, na.rm = TRUE)

  vapply(week_start, function(ws) {
    hit <- which(activity_start <= ws & activity_end >= ws)
    if (length(hit) == 0 || is.na(activity$patient_days[hit[1]])) return(fallback)
    activity$patient_days[hit[1]]
  }, numeric(1))
}

#' Aggregate case dates into Monday-starting weekly counts
#'
#' @param dates A `Date` vector of sample dates.
#' @param run_date The last date to cover; the final bin is the week
#'   containing this date, even if it has no cases yet (a zero-count current
#'   week is a legitimate, informative data point for Farrington).
#' @return A list with `week_start` (a `Date` vector, one per bin) and
#'   `counts` (an integer vector, same length).
#' @keywords internal
#' @noRd
episode_weekly_bins <- function(dates, run_date) {
  floor_to_monday <- function(d) d - (as.integer(format(d, "%u")) - 1)
  first_week <- floor_to_monday(min(dates))
  last_week <- floor_to_monday(as.Date(run_date))
  week_start <- seq(first_week, last_week, by = "week")
  counts <- vapply(week_start, function(ws) sum(dates >= ws & dates < ws + 7), integer(1))
  list(week_start = week_start, counts = counts)
}
