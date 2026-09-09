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

# The measurements a validation result is made of.
#
# Pure functions over data frames and vectors, like R/validate_match.R
# and for the same reason: these are the numbers a paper quotes, so they
# have to be testable without a detection run.
#
# One rule runs through all of it. A metric whose denominator is zero has
# no value, and `NA` is what it gets - never 0, which would read as a
# measurement of "none of them" and would be averaged into the summary as
# though it were one.

#' Wilson score interval for a binomial proportion
#'
#' The interval to quote for a proportion out of a small denominator, and
#' most of these denominators are small: six outbreaks a seed, and one
#' per seed once the sensitivity is broken down by shape. The normal
#' approximation gives intervals that run past 0 and 1 there, and the
#' Wilson interval does not.
#'
#' @param x Number of successes.
#' @param n Number of trials.
#' @param conf_level Confidence level.
#' @return A list of `estimate`, `lower` and `upper`, each the same
#'   length as `x`. All three are `NA` where `n` is zero: no trials is
#'   not a proportion of zero.
#' @keywords internal
#' @noRd
episodic_wilson_ci <- function(x, n, conf_level = 0.95) {
  x <- as.numeric(x)
  n <- as.numeric(n)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  estimate <- ifelse(n > 0, x / n, NA_real_)
  centre <- (x + z^2 / 2) / (n + z^2)
  half <- z / (n + z^2) * sqrt(x * (n - x) / n + z^2 / 4)
  list(
    estimate = estimate,
    lower = ifelse(n > 0, pmax(0, centre - half), NA_real_),
    upper = ifelse(n > 0, pmin(1, centre + half), NA_real_)
  )
}

#' Exact Poisson interval for a count over an exposure
#'
#' For rates - false alarms per stream-week - where a binomial interval
#' would be the wrong shape: the numerator is a count of events, not a
#' count of successes out of a fixed number of trials.
#'
#' @param x Number of events.
#' @param exposure The denominator the rate is per (stream-weeks here).
#' @param conf_level Confidence level.
#' @return A list of `estimate`, `lower` and `upper`; all `NA` where
#'   `exposure` is zero.
#' @keywords internal
#' @noRd
episodic_poisson_ci <- function(x, exposure, conf_level = 0.95) {
  x <- as.numeric(x)
  exposure <- as.numeric(exposure)
  alpha <- 1 - conf_level
  lower <- ifelse(x > 0, stats::qgamma(alpha / 2, shape = x), 0)
  upper <- stats::qgamma(1 - alpha / 2, shape = x + 1)
  list(
    estimate = ifelse(exposure > 0, x / exposure, NA_real_),
    lower = ifelse(exposure > 0, lower / exposure, NA_real_),
    upper = ifelse(exposure > 0, upper / exposure, NA_real_)
  )
}

#' Kaplan-Meier curve for time to detection, misses censored
#'
#' A median detection delay computed over the outbreaks that were
#' detected is biased downward, and the bias is worst exactly where the
#' method is weakest: the shapes it misses are the slow ones, and
#' dropping them makes the remaining median look fast. An outbreak never
#' detected is not a missing delay, it is a delay longer than the time it
#' was watched for, which is a right-censored observation.
#'
#' Implemented here rather than taken from `survival`, which the package
#' does not otherwise need: the estimator is four lines and adding a
#' dependency to a validation harness would put it in everyone's
#' installation.
#'
#' @param time Days to detection for a detected outbreak, or days
#'   watched for one that was not.
#' @param event `TRUE` where `time` is a detection, `FALSE` where it is a
#'   censoring time.
#' @return A data frame of `time`, `n_risk`, `n_event`, `n_censored`,
#'   `survival` (the share still undetected) and `detected` (its
#'   complement), one row per distinct event time, with a row at time 0.
#'   Only the origin row when there is nothing to estimate.
#'
#'   `n_censored` counts censorings falling on that same event time, and
#'   so does not add up to the number censored overall: a censoring
#'   between two event times has no row of its own. It is there to
#'   explain a drop in `n_risk`, not to be totalled. The estimate does
#'   not depend on it - Kaplan-Meier needs `n_risk` at the event times
#'   and nothing else.
#' @keywords internal
#' @noRd
episodic_validation_km <- function(time, event) {
  time <- as.numeric(time)
  event <- as.logical(event)
  if (length(time) != length(event)) {
    stop("`time` and `event` must be the same length.", call. = FALSE)
  }
  keep <- !is.na(time) & !is.na(event)
  time <- time[keep]
  event <- event[keep]
  origin <- data.frame(
    time = 0,
    n_risk = length(time),
    n_event = 0L,
    n_censored = 0L,
    survival = 1,
    detected = 0,
    stringsAsFactors = FALSE
  )
  if (length(time) == 0 || !any(event)) {
    return(origin)
  }
  times <- sort(unique(time[event]))
  n_risk <- vapply(times, function(t) sum(time >= t), integer(1))
  n_event <- vapply(times, function(t) sum(event & time == t), integer(1))
  n_censored <- vapply(times, function(t) sum(!event & time == t), integer(1))
  survival <- cumprod(1 - n_event / n_risk)
  rbind(
    origin,
    data.frame(
      time = times,
      n_risk = n_risk,
      n_event = n_event,
      n_censored = n_censored,
      survival = survival,
      detected = 1 - survival,
      stringsAsFactors = FALSE
    )
  )
}

#' The median of a Kaplan-Meier time-to-detection curve
#'
#' @param km From `episodic_validation_km()`.
#' @return The first time at which at least half have been detected, or
#'   `NA` when the curve never gets there - which is the honest answer
#'   for a method that misses more than half of what it is shown, and is
#'   the answer a median over detected outbreaks alone would hide.
#' @keywords internal
#' @noRd
episodic_validation_km_median <- function(km) {
  reached <- km$time[km$survival <= 0.5]
  if (length(reached) == 0) NA_real_ else min(reached)
}

#' Area under the ROC curve, by the Mann-Whitney identity
#'
#' Ties count a half, which is what the rank-sum form gives and what the
#' trapezoidal ROC gives.
#'
#' @param score The score being judged - the priority score, here.
#' @param positive `TRUE` for the class the score is meant to rank
#'   higher.
#' @return A single number, or `NA` when either class is empty or every
#'   score is missing. A one-class sample has no discrimination to
#'   measure; 0.5 would say "no better than chance", which is a different
#'   claim.
#' @keywords internal
#' @noRd
episodic_validation_auc <- function(score, positive) {
  keep <- !is.na(score) & !is.na(positive)
  score <- as.numeric(score[keep])
  positive <- as.logical(positive[keep])
  n1 <- sum(positive)
  n0 <- sum(!positive)
  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[positive]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' How well a score's value matches the outcome rate at that value
#'
#' The score orders the assessment queue, so its ranking is the claim
#' that matters most; but a score presented to an epidemiologist as a
#' number out of a hundred also implies a rate, and this is where that
#' implication can be checked.
#'
#' @param score The score.
#' @param positive `TRUE` for a true positive.
#' @param breaks Bin edges over the score's range.
#' @return One row per bin with `bin`, `n`, `mean_score`, `observed` and
#'   a Wilson interval on `observed`. Bins with no clusters in them are
#'   returned with `n = 0` and `NA` elsewhere, never with an observed
#'   rate of zero.
#' @keywords internal
#' @noRd
episodic_validation_calibration <- function(score,
                                            positive,
                                            breaks = seq(0, 100, by = 20)) {
  keep <- !is.na(score) & !is.na(positive)
  score <- as.numeric(score[keep])
  positive <- as.logical(positive[keep])
  bins <- cut(score, breaks = breaks, include.lowest = TRUE)
  levels_bin <- levels(bins)
  n <- vapply(levels_bin, function(b) sum(bins == b, na.rm = TRUE), integer(1))
  x <- vapply(
    levels_bin,
    function(b) sum(positive[which(bins == b)]),
    integer(1)
  )
  mean_score <- vapply(
    levels_bin,
    function(b) {
      values <- score[which(bins == b)]
      if (length(values) == 0) NA_real_ else mean(values)
    },
    numeric(1)
  )
  ci <- episodic_wilson_ci(x, n)
  data.frame(
    bin = levels_bin,
    n = as.integer(n),
    n_true_positive = as.integer(x),
    mean_score = mean_score,
    observed = ci$estimate,
    ci_low = ci$lower,
    ci_high = ci$upper,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' The columns every summary row has
#' @keywords internal
#' @noRd
episodic_validation_summary_empty <- function() {
  data.frame(
    metric = character(0),
    group_type = character(0),
    group = character(0),
    n_seeds = integer(0),
    median = numeric(0),
    q25 = numeric(0),
    q75 = numeric(0),
    numerator = numeric(0),
    denominator = numeric(0),
    estimate = numeric(0),
    ci_low = numeric(0),
    ci_high = numeric(0),
    interval = character(0),
    unit = character(0),
    stringsAsFactors = FALSE
  )
}

#' One summary row for a proportion, per seed and pooled
#'
#' Both are reported because they answer different questions and neither
#' subsumes the other. The median across seeds says what a typical
#' realisation looks like, which is what "seeds are the replicates" asks
#' for; the pooled proportion with its Wilson interval says how precisely
#' the whole study pins the number down.
#'
#' The across-seed distribution is withheld when every seed contributed a
#' denominator of one - a proportion out of a single trial is 0 or 1, and
#' the median of a column of those is not a summary of anything.
#'
#' @param metric,group_type,group Labels for the row.
#' @param numerator,denominator Vectors, one element per seed.
#' @param unit What the estimate is in.
#' @return One row of `episodic_validation_summary_empty()`'s shape.
#' @keywords internal
#' @noRd
episodic_validation_proportion_row <- function(metric,
                                               group_type,
                                               group,
                                               numerator,
                                               denominator,
                                               unit = "proportion") {
  per_seed <- ifelse(denominator > 0, numerator / denominator, NA_real_)
  degenerate <- all(denominator[!is.na(denominator)] <= 1)
  quartiles <- episodic_validation_quartiles(
    if (degenerate) numeric(0) else per_seed
  )
  pooled <- episodic_wilson_ci(sum(numerator), sum(denominator))
  data.frame(
    metric = metric,
    group_type = group_type,
    group = group,
    n_seeds = sum(denominator > 0),
    median = quartiles[["median"]],
    q25 = quartiles[["q25"]],
    q75 = quartiles[["q75"]],
    numerator = sum(numerator),
    denominator = sum(denominator),
    estimate = pooled$estimate,
    ci_low = pooled$lower,
    ci_high = pooled$upper,
    interval = "wilson",
    unit = unit,
    stringsAsFactors = FALSE
  )
}

#' One summary row for a rate: events over an exposure
#' @inheritParams episodic_validation_proportion_row
#' @param events,exposure Vectors, one element per seed.
#' @keywords internal
#' @noRd
episodic_validation_rate_row <- function(metric,
                                         group_type,
                                         group,
                                         events,
                                         exposure,
                                         unit) {
  per_seed <- ifelse(exposure > 0, events / exposure, NA_real_)
  quartiles <- episodic_validation_quartiles(per_seed)
  pooled <- episodic_poisson_ci(sum(events), sum(exposure))
  data.frame(
    metric = metric,
    group_type = group_type,
    group = group,
    n_seeds = sum(exposure > 0),
    median = quartiles[["median"]],
    q25 = quartiles[["q25"]],
    q75 = quartiles[["q75"]],
    numerator = sum(events),
    denominator = sum(exposure),
    estimate = pooled$estimate,
    ci_low = pooled$lower,
    ci_high = pooled$upper,
    interval = "poisson",
    unit = unit,
    stringsAsFactors = FALSE
  )
}

#' One summary row for a measured quantity: median within seed, then across
#'
#' Delays, recalls, fragmentation counts, clusters per week. Summarised
#' twice over - the median within each seed, then the median and
#' interquartile range of those - so that a seed that happened to produce
#' forty clusters does not outweigh one that produced four.
#'
#' @inheritParams episodic_validation_proportion_row
#' @param seed,value Parallel vectors, one element per observation.
#' @keywords internal
#' @noRd
episodic_validation_value_row <- function(metric,
                                          group_type,
                                          group,
                                          seed,
                                          value,
                                          unit) {
  keep <- !is.na(value)
  seed <- seed[keep]
  value <- as.numeric(value[keep])
  per_seed <- if (length(value) == 0) {
    numeric(0)
  } else {
    vapply(split(value, seed), stats::median, numeric(1))
  }
  quartiles <- episodic_validation_quartiles(per_seed)
  data.frame(
    metric = metric,
    group_type = group_type,
    group = group,
    n_seeds = length(per_seed),
    median = quartiles[["median"]],
    q25 = quartiles[["q25"]],
    q75 = quartiles[["q75"]],
    numerator = NA_real_,
    denominator = length(value),
    estimate = quartiles[["median"]],
    ci_low = NA_real_,
    ci_high = NA_real_,
    interval = NA_character_,
    unit = unit,
    stringsAsFactors = FALSE
  )
}

#' Median and quartiles, or `NA` where there is nothing to take them of
#' @keywords internal
#' @noRd
episodic_validation_quartiles <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(c(median = NA_real_, q25 = NA_real_, q75 = NA_real_))
  }
  quantiles <- stats::quantile(x, probs = c(0.5, 0.25, 0.75), names = FALSE)
  c(median = quantiles[1], q25 = quantiles[2], q75 = quantiles[3])
}
