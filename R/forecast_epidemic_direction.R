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


#' The level of the interval around an epidemic's growth rate
#' @keywords internal
#' @noRd
episodic_direction_interval_level <- 0.9

#' The epidemic-direction settings of a run, checked
#'
#' Read from `forecast.epidemic_direction`, with the shipped defaults for
#' a hand-built configuration without the section, as
#' `episodic_nowcast_settings()` does.
#'
#' @param config The resolved configuration.
#' @return A list with `enabled` (logical), `window_weeks` (the weeks the
#'   growth rate is estimated over, at least 3), `min_cases` (the fewest
#'   reported cases in them to estimate from) and `n_samples` (the
#'   draws the probabilities are computed from).
#' @keywords internal
#' @noRd
episodic_epidemic_direction_settings <- function(config) {
  settings <- config$forecast$epidemic_direction
  if (is.null(settings)) {
    settings <- episodic_config_resolve_files(NA)$forecast$epidemic_direction
  }
  problem <- function(key, must) {
    stop(
      "`forecast.epidemic_direction.",
      key,
      "` must be ",
      must,
      ", not ",
      paste(format(settings[[key]]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  whole <- function(key, minimum) {
    value <- settings[[key]]
    if (
      !is.numeric(value) || length(value) != 1 || is.na(value) ||
        value != round(value) || value < minimum
    ) {
      problem(key, paste0("a whole number of at least ", minimum))
    }
    as.integer(value)
  }
  enabled <- settings$enabled
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    problem("enabled", "true or false")
  }
  list(
    enabled = enabled,
    window_weeks = whole("window_weeks", 3),
    min_cases = whole("min_cases", 1),
    n_samples = whole("n_samples", 100)
  )
}

#' The weekly growth rate of a short series of weekly counts
#'
#' A log-linear Poisson regression of the counts on the week, whose slope
#' is the growth rate per week, with its standard error scaled by the
#' Pearson dispersion (quasi-Poisson). The dispersion is floored at 1:
#' with a few weeks and two parameters it is estimated on very few
#' degrees of freedom, and an estimate below 1 would claim less noise
#' than Poisson counts carry.
#'
#' @param y Weekly counts, oldest first, at least three.
#' @return `NULL` when there are no cases or the fit does not converge to
#'   a finite slope, as when every case falls in one week, otherwise a
#'   numeric vector with `rate` and `se`, and `intercept` (the log
#'   expected count of the first week), `se_intercept`, `covariance`
#'   (of intercept and rate) and `dispersion`, for extrapolating the
#'   trend (`episodic_outlook_trend_paths()`).
#' @keywords internal
#' @noRd
episodic_growth_rate_fit <- function(y) {
  if (sum(y) == 0) {
    return(NULL)
  }
  week <- seq_along(y) - 1
  x <- cbind(1, week)
  fit <- tryCatch(
    withCallingHandlers(
      stats::glm.fit(x, y, family = stats::poisson()),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || !isTRUE(fit$converged) || isTRUE(fit$boundary)) {
    return(NULL)
  }
  mu <- fit$fitted.values
  dispersion <- max(1, sum((y - mu)^2 / mu) / (length(y) - 2))
  unscaled <- solve(crossprod(x, x * mu))
  rate <- unname(fit$coefficients[2])
  se <- sqrt(dispersion * unscaled[2, 2])
  if (!is.finite(rate) || !is.finite(se)) {
    return(NULL)
  }
  c(
    rate = rate,
    se = se,
    intercept = unname(fit$coefficients[1]),
    se_intercept = sqrt(dispersion * unscaled[1, 1]),
    covariance = dispersion * unscaled[1, 2],
    dispersion = dispersion
  )
}

#' The direction of one epidemic: growth rate and whether it is past its peak
#'
#' The growth rate is estimated over the stream's last `window_weeks`
#' weeks that have elapsed and are either fully reported or nowcast
#' (`episodic_nowcast_stream()`), by `episodic_growth_rate_fit()`. A week
#' is fully reported once its Sunday is `max_delay_days` or more before
#' `asof`. A week that is neither ends the series: its count is still
#' incomplete and nothing estimates its final count, so it is left out
#' rather than read as final, and the window ends before it. A week that
#' begins before `observed_from` is left out too: the case data does not
#' reach back over all of it, so its count is not a count of zero.
#'
#' The uncertainty of the nowcast weeks is carried through by sampling:
#' each draw takes every nowcast week's final count from its predictive
#' distribution, fits the growth rate, and draws a rate from the fit's
#' sampling distribution. The pooled rates give the rate's median and
#' interval and the probability that it is above zero. The probability
#' that the epidemic is past its peak is the share of draws in which the
#' rate is below zero and the window's last week is below the highest
#' week since `first_week`: it says the epidemic is declining from a peak
#' already seen, not that it will not rise again.
#'
#' @param case_dates The sample dates of the stream's cases.
#' @param observed_from The earliest sample date in the case data: the
#'   first day any count is a measurement.
#' @param first_week The Monday of the epidemic's first week: its onset
#'   week, or the week of its first case.
#' @param asof The date the data is current as of: this run's date.
#' @param week_pmfs The stream's `week_pmfs` from this run's nowcast, or
#'   an empty list when it has none.
#' @param max_delay_days The nowcast's reporting horizon.
#' @param settings `episodic_epidemic_direction_settings()`'s output.
#' @param seed Seed for the draws; the session's own random stream is
#'   restored afterwards.
#' @return A list with `status` (`"computed"` or `"insufficient_data"`),
#'   `detail` (`NA`, or the reason code: `"weeks"` when fewer than
#'   `window_weeks` observed weeks are fully reported or nowcast,
#'   `"cases"` when they hold fewer than `min_cases` reported cases,
#'   `"model"` when the counts do not give a finite growth rate),
#'   `params` and `estimates`, a data frame with `quantity`
#'   (`"growth_rate_week"`, `"p_growing"` and,
#'   when the epidemic's first week is inside the series,
#'   `"p_past_peak"`), `estimate`, `lower`, `upper` and `interval_level`
#'   (`NA` for the probabilities); zero rows unless `"computed"`.
#' @keywords internal
#' @noRd
episodic_epidemic_direction_forecast <- function(case_dates,
                                                 observed_from,
                                                 first_week,
                                                 asof,
                                                 week_pmfs,
                                                 max_delay_days,
                                                 settings,
                                                 seed) {
  asof <- as.Date(asof)
  first_week <- episodic_week_start(first_week)
  window_weeks <- settings$window_weeks
  last_week <- episodic_last_complete_week_start(asof)
  observed_from <- as.Date(observed_from)
  first_observed <- episodic_week_start(observed_from)
  if (first_observed < observed_from) {
    first_observed <- first_observed + 7L
  }
  # Far enough back that a window of full length remains once the weeks
  # still inside the reporting horizon without a nowcast are left out.
  lookback_weeks <- window_weeks + ceiling(max_delay_days / 7)
  start <- max(
    first_observed,
    min(first_week, last_week - 7L * lookback_weeks)
  )
  weeks <- if (start <= last_week) seq(start, last_week, by = 7) else as.Date(character(0))
  dates <- as.Date(case_dates)
  dates <- dates[!is.na(dates) & dates >= start & dates <= last_week + 6]
  observed <- tabulate(
    as.integer(dates - start) %/% 7L + 1L,
    nbins = length(weeks)
  )
  final <- weeks + 6 <= asof - max_delay_days
  pmfs <- lapply(format(weeks), function(w) week_pmfs[[w]])
  known <- final | !vapply(pmfs, is.null, logical(1))
  usable <- if (all(known)) length(weeks) else which(!known)[1] - 1L

  params <- list(
    method = "loglinear_quasipoisson",
    asof = format(asof),
    observed_from = format(observed_from),
    first_week = format(first_week),
    window_weeks = window_weeks,
    max_delay_days = as.integer(max_delay_days),
    n_samples = settings$n_samples,
    seed = seed
  )
  none <- data.frame(
    quantity = character(0),
    estimate = numeric(0),
    lower = numeric(0),
    upper = numeric(0),
    interval_level = numeric(0),
    stringsAsFactors = FALSE
  )
  outcome <- function(detail) {
    list(status = "insufficient_data", detail = detail, params = params, estimates = none)
  }
  if (usable < window_weeks) {
    return(outcome("weeks"))
  }
  window <- (usable - window_weeks + 1L):usable
  params$window_start <- format(weeks[window[1]])
  params$window_end <- format(weeks[usable])
  params$weeks_behind <- as.integer(last_week - weeks[usable]) %/% 7L
  params$n_nowcast_weeks <- sum(!final[window])
  params$n_cases <- sum(observed[window])
  if (params$n_cases < settings$min_cases) {
    return(outcome("cases"))
  }

  previous_seed <- episodic_seed_snapshot()
  on.exit(episodic_seed_restore(previous_seed), add = TRUE)
  set.seed(seed)
  n <- settings$n_samples
  draws <- matrix(observed[seq_len(usable)], nrow = n, ncol = usable, byrow = TRUE)
  for (i in which(!final[seq_len(usable)])) {
    draws[, i] <- sample.int(length(pmfs[[i]]), n, replace = TRUE, prob = pmfs[[i]]) - 1L
  }
  # Draws that agree on the window's counts share one fit.
  key <- apply(draws[, window, drop = FALSE], 1, paste, collapse = ",")
  distinct <- unique(key)
  fits <- lapply(distinct, function(k) {
    episodic_growth_rate_fit(as.numeric(strsplit(k, ",", fixed = TRUE)[[1]]))
  })
  if (any(vapply(fits, is.null, logical(1)))) {
    return(outcome("model"))
  }
  fits <- do.call(rbind, fits)[match(key, distinct), , drop = FALSE]
  rates <- stats::rnorm(n, mean = fits[, "rate"], sd = fits[, "se"])

  tail <- (1 - episodic_direction_interval_level) / 2
  limits <- stats::quantile(rates, c(tail, 1 - tail), names = FALSE)
  estimates <- data.frame(
    quantity = c("growth_rate_week", "p_growing"),
    estimate = c(stats::median(rates), mean(rates > 0)),
    lower = c(limits[1], NA),
    upper = c(limits[2], NA),
    interval_level = c(episodic_direction_interval_level, NA),
    stringsAsFactors = FALSE
  )
  first <- match(first_week, weeks)
  if (!is.na(first) && first <= usable) {
    highest <- apply(draws[, first:usable, drop = FALSE], 1, max)
    estimates <- rbind(estimates, data.frame(
      quantity = "p_past_peak",
      estimate = mean(rates < 0 & draws[, usable] < highest),
      lower = NA,
      upper = NA,
      interval_level = NA,
      stringsAsFactors = FALSE
    ))
  }
  list(
    status = "computed",
    detail = NA_character_,
    params = params,
    estimates = estimates
  )
}

#' The direction of every open epidemic, and store it
#'
#' Called once per run, after the nowcast, for every detected
#' epidemic-scale cluster that is neither merged, suppressed nor closed.
#' Every one gets a row, with the reason when there is no estimate. A
#' seasonal epidemic's first week is its onset week; any other's is the
#' week of its first case. An estimate that fails is recorded as
#' `"failed"` and announced, and does not fail the run.
#'
#' @param con A [DBI::DBIConnection-class], inside the run's transaction.
#' @param run_id,run_date This run.
#' @param config The resolved configuration.
#' @param streams Every stream, from `episodic_db_streams()`.
#' @param stream_cases_for `episodic_stream_case_index()`'s function over
#'   this run's cases.
#' @param week_pmfs This run's nowcast `week_pmfs`, by stream ID
#'   (`episodic_nowcast_run()`).
#' @return Invisibly, a named integer vector: how many epidemics were
#'   `computed`, `insufficient_data` and `failed`.
#' @keywords internal
#' @noRd
episodic_epidemic_direction_run <- function(con,
                                            run_id,
                                            run_date,
                                            config,
                                            streams,
                                            stream_cases_for,
                                            week_pmfs) {
  counts <- c(computed = 0L, insufficient_data = 0L, failed = 0L)
  settings <- episodic_epidemic_direction_settings(config)
  if (!settings$enabled) {
    episodic_trace("Epidemic direction switched off (forecast.epidemic_direction.enabled)")
    return(invisible(counts))
  }
  max_delay_days <- episodic_nowcast_settings(config)$max_delay_days
  open <- episodic_db_clusters_not_closed(con, "epidemic")
  open <- open[open$origin == "detected", , drop = FALSE]
  if (nrow(open) == 0) {
    episodic_trace("Epidemic direction: no open epidemic")
    return(invisible(counts))
  }
  observed_from <- as.Date(episodic_db_get_query(
    con,
    "SELECT MIN(sample_date) AS first_day FROM episodic_case"
  )$first_day)
  for (i in seq_len(nrow(open))) {
    cluster_id <- open$cluster_id[i]
    stream <- streams[streams$stream_id == open$stream_id[i], , drop = FALSE]
    if (nrow(stream) == 0) {
      stop(
        "Open epidemic E-",
        cluster_id,
        " is on stream ",
        open$stream_id[i],
        ", which this run does not know.",
        call. = FALSE
      )
    }
    season <- episodic_db_epidemic_season(con, cluster_id)
    onset <- if (is.null(season)) NA else season$onset_week_start
    first_week <- if (is.na(onset)) {
      episodic_week_start(as.Date(open$first_day[i]))
    } else {
      as.Date(onset)
    }
    result <- tryCatch(
      episodic_epidemic_direction_forecast(
        stream_cases_for(stream[1, ])$sample_date,
        observed_from = observed_from,
        first_week = first_week,
        asof = run_date,
        week_pmfs = week_pmfs[[as.character(open$stream_id[i])]] %||% list(),
        max_delay_days = max_delay_days,
        settings = settings,
        seed = cluster_id
      ),
      error = function(e) {
        list(
          status = "failed",
          detail = conditionMessage(e),
          params = list(method = "loglinear_quasipoisson", asof = format(as.Date(run_date))),
          estimates = NULL
        )
      }
    )
    if (identical(result$status, "failed")) {
      episodic_trace(
        "Epidemic direction failed for E-",
        cluster_id,
        ": ",
        result$detail,
        severity = "warn"
      )
    }
    episodic_db_cluster_forecast_insert(
      con,
      run_id = run_id,
      cluster_id = cluster_id,
      kind = "epidemic_direction",
      method = "loglinear_quasipoisson",
      status = result$status,
      detail = result$detail,
      params = result$params,
      estimates = result$estimates
    )
    counts[[result$status]] <- counts[[result$status]] + 1L
  }
  episodic_trace(
    "Epidemic direction: ",
    counts[["computed"]],
    " of ",
    nrow(open),
    " open epidemic(s) estimated, ",
    counts[["insufficient_data"]],
    " without enough reported weeks or cases, ",
    counts[["failed"]],
    " failed"
  )
  invisible(counts)
}
