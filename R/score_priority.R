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
#' A weighted mean of seven rescaled components, 0-100. The density
#' component only applies where a patient-day denominator exists;
#' elsewhere the remaining weights renormalise rather than depressing
#' the score for a purely structural reason - a stream with no
#' patient-day data available should not be penalised relative to one
#' that has it, since otherwise hospital signals would systematically
#' outrank community ones.
#'
#' This is priority ranking, not exclusion: the score never removes a
#' candidate from the queue, it only orders it. Component weights are
#' configurable per instance in `inst/config/default.yaml`, so a
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
episodic_priority_score <- function(excess = NA, ratio = NA, severity_weight = 1,
                                    growth_slope = 0, detector_agreement = 1, n_detectors = 1,
                                    density_ratio = NA, spatial_concentration = 0, weights) {
  components <- c(
    excess_component = episodic_rescale(log1p(pmax(excess, 0, na.rm = FALSE))),
    ratio_component = episodic_rescale(pmin(ratio, 5)),
    severity_component = pmin(pmax(severity_weight, 0), 1),
    growth_component = episodic_rescale(growth_slope),
    agreement_component = detector_agreement / max(n_detectors, 1),
    density_component = episodic_rescale(density_ratio),
    spatial_component = pmin(pmax(spatial_concentration, 0), 1)
  )

  w <- unlist(weights[names(components)])

  has_density <- !is.na(components["density_component"])
  if (!has_density) {
    components["density_component"] <- 0
    remaining <- setdiff(names(w), "density_component")
    w[remaining] <- w[remaining] + w["density_component"] * (w[remaining] / sum(w[remaining]))
    w["density_component"] <- 0
  }

  components[is.na(components)] <- 0

  100 * sum(components * w) / sum(w)
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
  if (is.na(x)) return(NA_real_)
  x <- pmax(x, 0)
  x / (x + 1)
}
