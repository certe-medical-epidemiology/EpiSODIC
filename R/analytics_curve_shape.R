#' Classify a cluster's epidemic curve shape
#'
#' Whether all cases fall within a single maximum incubation period
#' distinguishes a point source from propagated transmission - "the first
#' question in any outbreak investigation, because it redirects the
#' enquiry from person-to-person spread towards a common exposure"
#' (ARCHITECTURE.md section 9). Derived purely from the case date
#' distribution against `incub_max_days`; no model is fitted.
#'
#' The two-multiple boundary for "propagated" (as opposed to "ambiguous")
#' is a judgement call, not a value ARCHITECTURE.md specifies numerically
#' - it only asks for "an intermediate verdict where the evidence is
#' ambiguous". A span that barely exceeds one incubation period is exactly
#' the case a point-source exposure with staggered onset produces; a span
#' several times longer is not explainable that way and points to
#' person-to-person spread. Flagged in QUESTIONS.md for calibration
#' alongside the other provisional M1 thresholds (M6).
#'
#' @param cases A data frame of the cluster's cases, with `sample_date`.
#' @param incub_max_days The pathogen's maximum incubation period, from
#'   `episode_pathogen_config`.
#' @return One of `"point_source"`, `"propagated"`, `"ambiguous"`, or `NA`
#'   if `incub_max_days` is `NA` or there are fewer than 2 cases.
#' @keywords internal
#' @noRd
episode_classify_curve_shape <- function(cases, incub_max_days) {
  if (is.na(incub_max_days) || is.null(cases) || nrow(cases) < 2) return(NA_character_)
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
