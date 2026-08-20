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

#' Time-varying effective reproduction number (Rt) for a cluster
#'
#' `EpiEstim::estimate_R()` on sample-date incidence, sliding 7-day
#' windows, using the pathogen's serial interval from
#' `episode_pathogen_config`. This is Rt, the
#' time-varying *effective* reproduction number - not R0 - and any display
#' of it must say so.
#'
#' Suppressed entirely (returns `NULL`) where `rt_applicable` is `FALSE`,
#' or the serial interval is unset, or there is not enough case history
#' for even one window: no off-the-shelf message like "insufficient data"
#' is a substitute for simply not showing the panel, per how the rest of
#' this codebase treats optional analytical panels (`episode_cluster_
#' object()`'s other `NULL`-when-not-computable fields).
#'
#' Estimates whose window ends inside `incomplete_days` of the run date
#' are withheld entirely, rather than shown with a caveat: the reporting
#' delay means the most recent days are under-ascertained by construction
#' (`R/triangle_update.R`), so an Rt estimate ending there would read a
#' reporting artefact as a real change in transmission.
#'
#' `si_dist` (`gamma`/`lognormal`) from `episode_pathogen_config` is
#' recorded but not yet used to pick `EpiEstim`'s distribution family:
#' `estimate_R(method = "parametric_si")` only ever fits a Gamma-shaped
#' serial interval internally (mean/sd parameterised, `EpiEstim`'s own
#' standard usage), regardless of what a pathogen's own literature source
#' assumed. This is a known, documented simplification.
#'
#' @param cases A data frame of the cluster's cases, with `sample_date`.
#' @param pc A single-row pathogen config (`episode_db_pathogen_config_get()`).
#' @param incomplete_days From `episode_app_completeness()`; days at the
#'   trailing end of the case series considered under-ascertained.
#' @param window_days Sliding window width, days. Fixed at 7 (weekly),
#'   matching every other panel's own aggregation.
#' @return A data frame with `window_end` (`Date`), `mean`, `lower`,
#'   `upper` (2.5%/97.5% credible interval), or `NULL` if Rt cannot or
#'   should not be computed.
#' @references
#' Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). "A New Framework and
#' Software to Estimate Time-Varying Reproduction Numbers During
#' Epidemics." *American Journal of Epidemiology*, 178(9), 1505-1512.
#' \doi{10.1093/aje/kwt133} (the method `EpiEstim::estimate_R()`
#' implements and is called directly here).
#' @keywords internal
#' @noRd
episode_compute_rt <- function(cases, pc, incomplete_days = 0L, window_days = 7L) {
  if (is.null(pc) || !isTRUE(as.logical(pc$rt_applicable))) return(NULL)
  if (is.na(pc$si_mean_days) || is.na(pc$si_sd_days)) return(NULL)
  if (is.null(cases) || nrow(cases) == 0) return(NULL)
  if (!requireNamespace("EpiEstim", quietly = TRUE)) return(NULL)

  dates <- as.Date(cases$sample_date)
  all_days <- seq(min(dates), max(dates), by = "day")
  if (length(all_days) < window_days + 2) return(NULL)

  daily_incidence <- vapply(all_days, function(d) sum(dates == d), integer(1))

  t_start <- seq(2, length(all_days) - window_days + 1)
  t_end <- t_start + window_days - 1
  if (length(t_start) == 0) return(NULL)

  cfg <- EpiEstim::make_config(mean_si = pc$si_mean_days, std_si = pc$si_sd_days,
                                t_start = t_start, t_end = t_end)
  result <- tryCatch(
    suppressWarnings(EpiEstim::estimate_R(daily_incidence, method = "parametric_si", config = cfg)),
    error = function(e) NULL
  )
  if (is.null(result) || nrow(result$R) == 0) return(NULL)

  r <- result$R
  out <- data.frame(
    window_end = all_days[r$t_end],
    mean = r$`Mean(R)`, lower = r$`Quantile.0.025(R)`, upper = r$`Quantile.0.975(R)`,
    stringsAsFactors = FALSE
  )
  cutoff <- max(all_days) - incomplete_days
  out <- out[out$window_end <= cutoff, ]
  if (nrow(out) == 0) return(NULL)
  out
}
