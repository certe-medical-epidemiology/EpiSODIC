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

#' Eligibility gate
#'
#' The first layer of volume control: a stream needs sufficient baseline
#' history, a minimum median weekly count, and non-zero counts in a
#' reasonable share of baseline weeks before statistical detection is
#' attempted on it. Streams failing this gate fall through to EARS C2 or
#' the rare-pathogen path instead (`R/detect_*.R` handles those
#' fall-through detectors); only the gate itself lives here.
#'
#' The numeric thresholds are configurable defaults shipped in
#' `inst/config/default.yaml`, tuned for a typical department's signal
#' volume.
#'
#' @param cases_for_stream A data frame of cases belonging to one stream,
#'   with a `sample_date` column.
#' @param as_of The date detection is being run as of; baseline weeks are
#'   counted back from here.
#' @param config The resolved configuration (`episode_config_resolve()`);
#'   uses `config$eligibility`.
#' @return `TRUE` if the stream is eligible for statistical detection,
#'   `FALSE` otherwise.
#' @keywords internal
#' @noRd
episode_eligibility_gate <- function(cases_for_stream, as_of, config) {
  gate <- config$eligibility

  dates <- as.Date(cases_for_stream$sample_date)
  baseline_start <- as.Date(as_of) - gate$min_baseline_weeks * 7

  if (length(dates) == 0 || min(dates) > baseline_start) {
    return(FALSE)
  }

  weeks <- seq(baseline_start, as.Date(as_of), by = "week")
  week_bin <- cut(dates[dates >= baseline_start & dates <= as.Date(as_of)], breaks = weeks, right = FALSE)
  weekly_counts <- table(week_bin)

  if (length(weekly_counts) == 0) return(FALSE)

  median_weekly <- stats::median(as.numeric(weekly_counts))
  nonzero_share <- mean(as.numeric(weekly_counts) > 0)

  median_weekly >= gate$min_median_weekly_count && nonzero_share >= gate$min_nonzero_week_share
}
