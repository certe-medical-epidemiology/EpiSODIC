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

#' Priority score
#'
#' A weighted mean of seven rescaled components, 0-100. Any component
#' that cannot be computed at all drops out and the remaining weights
#' renormalise, rather than the component being scored zero while
#' keeping its weight: a stream that structurally *cannot* produce a
#' measurement must not be penalised relative to one that can.
#'
#' That rule was originally written for the density component (a stream
#' with no patient-day denominator) but applies just as forcefully to
#' `excess` and `ratio`, which only exist for detectors that fit a
#' baseline. `same_place` and `rare_trigger` never produce either, so
#' scoring their absence as zero-with-weight meant every ward-level
#' same-place cluster - the kind an infection prevention nurse has to act
#' on the same day - was systematically outranked by Farrington signals
#' purely because Farrington is the detector that happens to report an
#' expectation. Absence of a baseline is not evidence of a small excess.
#'
#' The rescaled components are anchored so that "unremarkable" is zero,
#' not a half score: `ratio` and `density_ratio` are both ratios against
#' an expectation, so it is their *excess over 1* that is rescaled. A
#' cluster observed exactly at its expected count contributes nothing to
#' the ratio component, which is the point of a priority ranking - and it
#' still ranks below a cluster whose ratio simply could not be computed,
#' correctly, since the first is a measurement of nothing unusual and the
#' second is no measurement at all.
#'
#' This is priority ranking, not exclusion: the score never removes a
#' candidate from the queue, it only orders it. Component weights are
#' configurable per instance in `inst/config/episodic_default_config.yaml`, so a
#' department can rebalance them against its own signal volume as
#' evidence accumulates.
#'
#' @param excess Observed minus upperbound (may be `NA`).
#' @param ratio Observed over expected, capped at 5 (may be `NA`).
#' @param severity_weight From `episodic_stream`/`episodic_pathogen_config`.
#' @param growth_slope Slope of the last three aggregation periods.
#' @param detector_agreement,n_detectors Count of detectors that fired, and
#'   how many exist in total.
#' @param density_ratio Incidence per 1,000 patient-days versus baseline,
#'   or `NA` when no patient-day denominator exists for this stream.
#' @param spatial_concentration A 0-1 concentration measure across PC
#'   (Gini or top-1 share).
#' @param weights A list from `config$priority_score$weights`.
#' @return A single numeric score, 0-100.
#' @keywords internal
#' @noRd
episodic_priority_score <- function(
    excess = NA,
    ratio = NA,
    severity_weight = 1,
    growth_slope = 0,
    detector_agreement = 1,
    n_detectors = 1,
    density_ratio = NA,
    spatial_concentration = 0,
    weights) {
  components <- c(
    excess_component = episodic_rescale(log1p(pmax(excess, 0, na.rm = FALSE))),
    ratio_component = episodic_rescale(pmin(ratio, 5) - 1),
    severity_component = pmin(pmax(severity_weight, 0), 1),
    growth_component = episodic_rescale(growth_slope),
    agreement_component = detector_agreement / max(n_detectors, 1),
    density_component = episodic_rescale(density_ratio - 1),
    spatial_component = pmin(pmax(spatial_concentration, 0), 1)
  )

  w <- unlist(weights[names(components)])

  # Drop every non-computable component and renormalise over what is
  # left, rather than zeroing it while keeping its weight.
  computable <- !is.na(components)
  if (!any(computable) || sum(w[computable]) <= 0) {
    return(0)
  }

  100 * sum(components[computable] * w[computable]) / sum(w[computable])
}

#' Cases falling inside a candidate episode's own interval
#'
#' @param cases A data frame with `sample_date`.
#' @param first_day,last_day The interval bounds (`Date` or ISO string).
#' @return The subset of `cases` inside `[first_day, last_day]`.
#' @keywords internal
#' @noRd
episodic_cases_in_window <- function(cases, first_day, last_day) {
  if (is.null(cases) || nrow(cases) == 0) {
    return(cases)
  }
  dates <- as.Date(cases$sample_date)
  cases[
    !is.na(dates) & dates >= as.Date(first_day) & dates <= as.Date(last_day), ,
    drop = FALSE
  ]
}

#' Growth slope over the last few aggregation periods
#'
#' Cases per week gained per week, from an ordinary least-squares fit
#' over the last `n_periods` weekly counts ending at `last_day`. Not a
#' fitted epidemic model - Rt is that - but the "is it still climbing"
#' term the priority score needs, on a scale (cases/week/week) that
#' `episodic_rescale()`'s own \eqn{x/(x+1)} squashing handles sensibly.
#'
#' A flat or falling candidate returns a slope of zero or below, which
#' `episodic_rescale()` floors at zero: a shrinking cluster contributes
#' nothing to the growth component rather than being scored negatively
#' against clusters with no growth data at all.
#'
#' The counts are taken from the stream's cases, not the cluster's, so a
#' newly-opened cluster is still read against what the stream was doing
#' in the weeks before it opened rather than starting from an artificial
#' zero.
#'
#' @param cases A data frame with `sample_date`.
#' @param last_day The last day of the candidate episode.
#' @param n_periods How many periods to fit over.
#' @param period_days Period length in days (weekly, matching every other
#'   aggregation in this codebase).
#' @return A single numeric, `0` when there is nothing to fit.
#' @keywords internal
#' @noRd
episodic_growth_slope <- function(
    cases,
    last_day,
    n_periods = 3L,
    period_days = 7L) {
  if (is.null(cases) || nrow(cases) == 0 || n_periods < 2L) {
    return(0)
  }
  last_day <- as.Date(last_day)
  if (is.na(last_day)) {
    return(0)
  }
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]

  counts <- vapply(
    seq_len(n_periods),
    function(k) {
      end <- last_day - period_days * (n_periods - k)
      start <- end - period_days + 1
      sum(dates >= start & dates <= end)
    },
    integer(1)
  )
  if (all(counts == 0)) {
    return(0)
  }

  # Closed-form OLS slope rather than stats::lm(): this runs once per
  # candidate per stream inside the run transaction, and fitting a
  # three-point regression through the full model machinery is pure
  # overhead there.
  x <- seq_len(n_periods)
  xc <- x - mean(x)
  slope <- sum(xc * (counts - mean(counts))) / sum(xc^2)
  if (is.na(slope)) 0 else slope
}

#' How concentrated a candidate's cases are across PC
#'
#' The share of cases falling in the single commonest PC, among cases
#' whose PC is known - deliberately the same quantity the dossier shows
#' as `episodic_app_concentration()$dominant_share` and the same one the
#' interpretation engine's concentration fragments threshold at 0.7/0.5,
#' so a cluster cannot be ranked on one concentration measure and then
#' described in the narrative by another.
#'
#' Cases with no PC are excluded from both numerator and denominator
#' rather than counted as "somewhere else": a missing postcode is absence
#' of evidence about localisation, not evidence of dispersal.
#'
#' @param cases A data frame with `pc`.
#' @return A single numeric in `[0, 1]`; `0` when no case has a PC.
#' @keywords internal
#' @noRd
episodic_spatial_concentration <- function(cases) {
  if (is.null(cases) || nrow(cases) == 0) {
    return(0)
  }
  pc <- cases$pc[!is.na(cases$pc)]
  if (length(pc) == 0) {
    return(0)
  }
  # Deliberately not table()/factor(): both sort their levels, and
  # sorting character data invokes locale-aware string collation
  # (Scollate()/ICU) - unlike plain hashing, collation is not guaranteed
  # safe against a string whose declared encoding does not match its
  # actual bytes, which is exactly what a client/server character-set
  # mismatch can hand back from a MariaDB connection. match()/unique()
  # only ever hash and byte-compare, never sort or collate the strings
  # themselves; tabulate() then counts the resulting integer codes,
  # where sorting is a plain numeric operation with no such risk.
  counts <- tabulate(match(pc, unique(pc)))
  max(counts) / sum(counts)
}

#' Rescale a value onto the zero-to-one range with a simple bounded transform
#'
#' Not specified numerically by the architecture beyond "rescale(...)"; a
#' logistic-style squashing function is used so that scores stay bounded
#' regardless of how extreme the input is, which a plain min-max rescale
#' (with no fixed reference range) cannot guarantee.
#' @keywords internal
#' @noRd
episodic_rescale <- function(x) {
  if (is.na(x)) {
    return(NA_real_)
  }
  x <- pmax(x, 0)
  x / (x + 1)
}
