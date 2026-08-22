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

#' Classify a cluster's epidemic curve shape
#'
#' Whether all cases fall within a single maximum incubation period
#' distinguishes a point source from propagated transmission - the first
#' question in any outbreak investigation, since it redirects the enquiry
#' from person-to-person spread towards a common exposure. Derived purely
#' from the case date distribution against `incub_max_days`; no model is
#' fitted.
#'
#' The two-multiple boundary between "propagated" and "ambiguous" is a
#' deliberate, documented threshold rather than a fitted one: a span that
#' barely exceeds one incubation period is exactly what a point-source
#' exposure with staggered onset produces, while a span several times
#' longer is not explainable that way and points to person-to-person
#' spread instead.
#'
#' @param cases A data frame of the cluster's cases, with `sample_date`.
#' @param incub_max_days The pathogen's maximum incubation period, from
#'   `episodic_pathogen_config`.
#' @return One of `"point_source"`, `"propagated"`, `"ambiguous"`, or `NA`
#'   if `incub_max_days` is `NA` or there are fewer than 2 cases.
#' @keywords internal
#' @noRd
episodic_classify_curve_shape <- function(cases, incub_max_days) {
  if (is.na(incub_max_days) || is.null(cases) || nrow(cases) < 2) {
    return(NA_character_)
  }
  dates <- as.Date(cases$sample_date)
  span_days <- as.numeric(max(dates) - min(dates))

  if (span_days <= incub_max_days) {
    "point_source"
  } else if (span_days > 2 * incub_max_days) {
    "propagated"
  } else {
    "ambiguous"
  }
}
