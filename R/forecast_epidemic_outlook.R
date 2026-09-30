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


#' The epidemic-outlook settings of a run, checked
#'
#' Read from `forecast.epidemic_outlook`, with the shipped defaults for a
#' hand-built configuration without the section, as
#' `episodic_nowcast_settings()` does.
#'
#' @param config The resolved configuration.
#' @return A list with `enabled` (logical), `horizon_weeks` (weeks ahead,
#'   1 to 8), `min_history_weeks` (fully reported weeks the model needs,
#'   at least 26), `n_samples` (simulated paths, at least 100) and
#'   `min_scored` (scored target weeks a pathogen needs before the
#'   dossier shows its outlook, at least 1).
#' @keywords internal
#' @noRd
episodic_epidemic_outlook_settings <- function(config) {
  settings <- config$forecast$epidemic_outlook
  if (is.null(settings)) {
    settings <- episodic_config_resolve_files(NA)$forecast$epidemic_outlook
  }
  problem <- function(key, must) {
    stop(
      "`forecast.epidemic_outlook.",
      key,
      "` must be ",
      must,
      ", not ",
      paste(format(settings[[key]]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  whole <- function(key, minimum, maximum = Inf) {
    value <- settings[[key]]
    if (
      !is.numeric(value) || length(value) != 1 || is.na(value) ||
        value != round(value) || value < minimum || value > maximum
    ) {
      problem(
        key,
        if (is.finite(maximum)) {
          paste0("a whole number from ", minimum, " to ", maximum)
        } else {
          paste0("a whole number of at least ", minimum)
        }
      )
    }
    as.integer(value)
  }
  enabled <- settings$enabled
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    problem("enabled", "true or false")
  }
  list(
    enabled = enabled,
    horizon_weeks = whole("horizon_weeks", 1, 8),
    min_history_weeks = whole("min_history_weeks", 26),
    n_samples = whole("n_samples", 100),
    min_scored = whole("min_scored", 1)
  )
}

#' The MEM bands an outlook gives the chance of reaching
#' @keywords internal
#' @noRd
episodic_outlook_bands <- c("low", "medium", "high", "very_high")

#' Which weekly counts reach each MEM band
#'
#' A count reaches `low` when it is above the pre-epidemic threshold, and
#' each higher band when it is at or above that band's threshold, as
#' `episodic_mem_intensity_level()` places a single count.
#'
#' @param counts Counts, any shape.
#' @param thresholds `episodic_epidemic_stored_thresholds()`'s output, or
#'   `NULL`.
#' @return A named list, one logical array of `counts`' shape per band in
#'   `episodic_outlook_bands`, with `NULL` for a band whose threshold is
#'   unknown.
#' @keywords internal
#' @noRd
episodic_outlook_band_reached <- function(counts, thresholds) {
  pre <- if (is.null(thresholds)) NA_real_ else as.numeric(thresholds$pre_epidemic)
  intensity <- if (is.null(thresholds)) NULL else thresholds$intensity
  out <- stats::setNames(vector("list", length(episodic_outlook_bands)), episodic_outlook_bands)
  if (!is.na(pre)) {
    out["low"] <- list(counts > pre)
  }
  if (!is.null(intensity)) {
    for (band in c("medium", "high", "very_high")) {
      out[band] <- list(counts >= intensity[[band]])
    }
  }
  out
}

#' An open epidemic's outlook: its stream's weekly counts in the weeks ahead
#'
#' The outlook is an equally weighted ensemble of two models, combined by
#' averaging their quantiles, as the European and US COVID-19 forecast
#' hubs combine theirs: a statistical model of the stream's whole
#' history, which knows its seasons and levels, and the extrapolated
#' trend of its last weeks, which follows a rise or fall the history has
#' not seen. Averaging quantiles keeps the combined forecast as sharp as
#' its components are on average, where pooling their draws would widen
#' it to cover both.
#'
#' The first model is an endemic-epidemic model (`surveillance::hhh4()`,
#' negative binomial), fitted to the stream's fully reported weeks: each week's expected
#' count is the endemic level, seasonal (one sine-cosine pair of period
#' 52 weeks) for a seasonal epidemic and constant otherwise, plus an
#' autoregressive share of the week before. A week is fully reported
#' once its Sunday is `max_delay_days` or more before `asof`; weeks that
#' begin before `observed_from` are not part of the series.
#'
#' Paths are then simulated from the last fully reported week, each with
#' its own parameters drawn from the fit's approximate sampling
#' distribution, so the intervals carry the uncertainty of the estimates
#' as well as the counts' own noise. A week still being reported is taken
#' from the nowcast's predictive distribution when there is one, and
#' otherwise drawn from the model but never below what has already been
#' reported for it (`episodic_outlook_draw_at_least()`); the week in
#' progress, the first target, likewise.
#' The targets are the `horizon_weeks` weeks after the last complete
#' week.
#'
#' The second extrapolates, along each of those paths, the log-linear
#' trend of its last `trend_window_weeks` weeks to the target weeks
#' (`episodic_outlook_trend_paths()`), the trend the direction tile
#' reports (`episodic_growth_rate_fit()`). It takes part only when every
#' week of that window, up to the last complete week, is data: fully
#' reported, or from the nowcast. A trend is a short-range forecast:
#' extended from a window that ended weeks back, it would carry its
#' growth rate over far more weeks than it was measured on. Where a week
#' of the window is not data, or the trend cannot be fitted, as when
#' every case of the window fell in one week, the outlook is the first
#' model's alone, and says so in `components`.
#'
#' For each target the result gives the mean and the quantiles of
#' `episodic_forecast_probs`, and, for a seasonal epidemic with stored
#' thresholds, the chance of reaching each MEM band that week, all read
#' from the averaged quantiles; and the chance of reaching each band in
#' at least one target week, read likewise from the averaged
#' distribution of each path's highest target week
#' (`episodic_outlook_combine()`).
#'
#' @param case_dates The sample dates of the stream's cases.
#' @param observed_from The earliest sample date in the case data.
#' @param asof The date the data is current as of: this run's date.
#' @param week_pmfs The stream's `week_pmfs` from this run's nowcast, or
#'   an empty list.
#' @param max_delay_days The nowcast's reporting horizon.
#' @param thresholds `episodic_epidemic_stored_thresholds()`'s output for
#'   a seasonal epidemic, or `NULL`; a seasonal epidemic's model has the
#'   seasonal terms.
#' @param settings `episodic_epidemic_outlook_settings()`'s output.
#' @param trend_window_weeks The weeks the trend is fitted over:
#'   `forecast.epidemic_direction.window_weeks`, so it is the trend the
#'   direction tile reports.
#' @param seed Seed for the simulation; the session's own random stream
#'   is restored afterwards.
#' @return A list with `status` (`"computed"` or `"insufficient_data"`),
#'   `detail` (`NA`, or `"history"` when fewer than `min_history_weeks`
#'   fully reported weeks exist, `"model"` when the model does not fit or
#'   its parameters have no usable sampling distribution), `params`,
#'   `weeks` (a data frame with `target_date`, `horizon`, `mean`, a
#'   column per `episodic_forecast_probs` and `p_low`, `p_medium`,
#'   `p_high`, `p_very_high`, `NA` where the band's threshold is unknown;
#'   no rows unless `"computed"`) and `estimates` (`p_reach_<band>` for
#'   each band with a threshold, in `episodic_cluster_forecast_estimate`'s
#'   shape).
#' @keywords internal
#' @noRd
episodic_epidemic_outlook_forecast <- function(case_dates,
                                               observed_from,
                                               asof,
                                               week_pmfs,
                                               max_delay_days,
                                               thresholds,
                                               settings,
                                               trend_window_weeks,
                                               seed) {
  asof <- as.Date(asof)
  observed_from <- as.Date(observed_from)
  seasonal <- !is.null(thresholds)
  last_week <- episodic_last_complete_week_start(asof)
  first_observed <- episodic_week_start(observed_from)
  if (first_observed < observed_from) {
    first_observed <- first_observed + 7L
  }
  horizon <- settings$horizon_weeks
  targets <- last_week + 7L * seq_len(horizon)
  weeks <- if (first_observed <= last_week) {
    seq(first_observed, last_week, by = 7)
  } else {
    as.Date(character(0))
  }
  all_weeks <- c(weeks, targets)
  dates <- as.Date(case_dates)
  dates <- dates[!is.na(dates) & dates >= first_observed & dates <= max(all_weeks) + 6]
  reported <- tabulate(
    as.integer(dates - first_observed) %/% 7L + 1L,
    nbins = length(all_weeks)
  )
  final <- all_weeks + 6 <= asof - max_delay_days
  n_final <- sum(final)

  params <- list(
    method = "hhh4_negbin",
    asof = format(asof),
    observed_from = format(observed_from),
    seasonal = seasonal,
    horizon_weeks = horizon,
    max_delay_days = as.integer(max_delay_days),
    n_samples = settings$n_samples,
    seed = seed,
    n_history_weeks = n_final
  )
  none_weeks <- episodic_outlook_weeks_empty()
  none_estimates <- data.frame(
    quantity = character(0),
    estimate = numeric(0),
    lower = numeric(0),
    upper = numeric(0),
    interval_level = numeric(0),
    stringsAsFactors = FALSE
  )
  outcome <- function(detail) {
    list(
      status = "insufficient_data",
      detail = detail,
      params = params,
      weeks = none_weeks,
      estimates = none_estimates
    )
  }
  if (n_final < settings$min_history_weeks) {
    return(outcome("history"))
  }
  history <- reported[seq_len(n_final)]
  params$last_final_week <- format(all_weeks[n_final])

  fit <- episodic_outlook_fit(history, seasonal, autoregressive = TRUE)
  # An autoregressive share estimated at its boundary of zero has no
  # usable sampling distribution: its log-scale standard error runs into
  # the thousands, and draws from it would be explosive paths the data
  # never suggested. The endemic model alone then describes the series.
  if (!is.null(fit) && fit$se[["ar.1"]] > episodic_outlook_max_ar_se) {
    fit <- NULL
  }
  autoregressive <- !is.null(fit)
  if (!autoregressive) {
    fit <- episodic_outlook_fit(history, seasonal, autoregressive = FALSE)
  }
  if (is.null(fit)) {
    return(outcome("model"))
  }
  params$autoregressive <- autoregressive
  theta <- fit$theta
  sigma <- tryCatch(chol(fit$vcov), error = function(e) NULL)
  if (is.null(sigma)) {
    return(outcome("model"))
  }
  params$coefficients <- as.list(theta)

  previous_seed <- episodic_seed_snapshot()
  on.exit(episodic_seed_restore(previous_seed), add = TRUE)
  set.seed(seed)
  n <- settings$n_samples
  draws <- matrix(stats::rnorm(n * length(theta)), n) %*% sigma
  draws <- sweep(draws, 2, theta, "+")
  colnames(draws) <- names(theta)
  ar <- if (autoregressive) exp(draws[, "ar.1"]) else rep(0, n)
  size <- exp(draws[, grep("overdisp", colnames(draws))])
  endemic <- function(i) {
    # The fit's time index counts from 0 at the series' first week.
    t <- i - 1
    log_nu <- draws[, "end.1"]
    if (seasonal) {
      log_nu <- log_nu +
        draws[, "end.sin(2 * pi * t/52)"] * sin(2 * pi * t / 52) +
        draws[, "end.cos(2 * pi * t/52)"] * cos(2 * pi * t / 52)
    }
    exp(log_nu)
  }

  paths <- matrix(NA_real_, nrow = n, ncol = length(all_weeks))
  paths[, seq_len(n_final)] <- rep(history, each = n)
  n_nowcast <- 0L
  # Weeks whose count is data: fully reported, or estimated by the
  # nowcast from what has been reported, as opposed to drawn from the
  # model.
  known <- final
  for (i in (n_final + 1L):length(all_weeks)) {
    pmf <- week_pmfs[[format(all_weeks[i])]]
    if (!is.null(pmf)) {
      known[i] <- TRUE
      paths[, i] <- sample.int(length(pmf), n, replace = TRUE, prob = pmf) - 1L
      n_nowcast <- n_nowcast + 1L
      next
    }
    mu <- ar * paths[, i - 1L] + endemic(i)
    paths[, i] <- episodic_outlook_draw_at_least(mu, size, reported[i])
  }
  params$n_nowcast_weeks <- n_nowcast
  # The last complete week as the paths have it: what a forecast that
  # expects no change would carry forward, the baseline it is scored
  # against.
  params$baseline_count <- stats::median(paths[, length(weeks)])

  target_cols <- length(weeks) + seq_len(horizon)
  components <- list(hhh4 = paths[, target_cols, drop = FALSE])
  # The trend is extrapolated from current data only: its window ends at
  # the last complete week, and every week in it is fully reported or
  # nowcast. Fitted to weeks the model drew, it would extrapolate the
  # model rather than the epidemic; ending weeks back, it would carry
  # a growth rate over far more weeks than it was measured on, which a
  # log-linear trend does not survive.
  last_known <- length(weeks)
  trend_cols <- last_known - rev(seq_len(trend_window_weeks)) + 1L
  if (min(trend_cols) >= 1L && all(known[trend_cols])) {
    trend <- episodic_outlook_trend_paths(
      paths[, trend_cols, drop = FALSE],
      ahead = target_cols - last_known,
      floors = reported[target_cols]
    )
    if (!is.null(trend)) {
      components$trend <- trend
      params$trend_window_end <- format(all_weeks[last_known])
    }
  }
  params$components <- names(components)
  params$trend_window_weeks <- as.integer(trend_window_weeks)
  combined <- episodic_outlook_combine(components, thresholds)
  week_rows <- data.frame(
    target_date = format(targets),
    horizon = seq_len(horizon),
    stringsAsFactors = FALSE
  )
  list(
    status = "computed",
    detail = NA_character_,
    params = params,
    weeks = cbind(week_rows, combined$weeks),
    estimates = combined$estimates
  )
}

#' Extrapolate the trend of each path's last weeks
#'
#' Along each simulated path, the log-linear trend of its last weeks
#' (`episodic_growth_rate_fit()`) is extended to the target weeks: an
#' intercept and rate drawn from the fit's sampling distribution, and a
#' count drawn around the trend with the fit's dispersion (negative
#' binomial with the variance `dispersion` times the mean; Poisson at a
#' dispersion of 1), never below what the target week has already
#' reported (`episodic_outlook_draw_at_least()`). Paths that agree on
#' their window share one fit.
#'
#' @param window A matrix of counts, one row per path and one column per
#'   week of the trend's window, oldest first.
#' @param ahead How many weeks after the window's last week each target
#'   week is.
#' @param floors What each target week has already reported, in order.
#' @return A matrix, one row per path and one column per target week, or
#'   `NULL` when a path's trend cannot be fitted or its extrapolation is
#'   not a finite count.
#' @keywords internal
#' @noRd
episodic_outlook_trend_paths <- function(window, ahead, floors) {
  n <- nrow(window)
  key <- apply(window, 1, paste, collapse = ",")
  distinct <- unique(key)
  fits <- lapply(distinct, function(k) {
    episodic_growth_rate_fit(as.numeric(strsplit(k, ",", fixed = TRUE)[[1]]))
  })
  if (any(vapply(fits, is.null, logical(1)))) {
    return(NULL)
  }
  fits <- do.call(rbind, fits)[match(key, distinct), , drop = FALSE]
  # The intercept and rate drawn jointly: a bivariate normal from two
  # independent draws and the fit's correlation between them.
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  correlation <- fits[, "covariance"] / (fits[, "se_intercept"] * fits[, "se"])
  correlation <- pmin(pmax(correlation, -1), 1)
  intercept <- fits[, "intercept"] + fits[, "se_intercept"] * z1
  rate <- fits[, "rate"] + fits[, "se"] * (correlation * z1 + sqrt(1 - correlation^2) * z2)
  last <- ncol(window) - 1L
  excess <- fits[, "dispersion"] - 1
  out <- matrix(NA_real_, nrow = n, ncol = length(floors))
  for (h in seq_along(floors)) {
    mu <- exp(intercept + rate * (last + ahead[h]))
    if (!all(is.finite(mu))) {
      return(NULL)
    }
    # Variance `dispersion` times the mean: a negative binomial whose size
    # is the mean over the excess dispersion, Poisson without any.
    size <- ifelse(excess > 0, mu / excess, Inf)
    out[, h] <- episodic_outlook_draw_at_least(mu, size, floors[h])
  }
  if (!all(is.finite(out))) {
    return(NULL)
  }
  out
}

#' Combine outlook models by averaging their quantiles
#'
#' Each component's simulated counts give its quantile function for each
#' target week, and for the highest target week along each path; the
#' combined forecast's quantile function is their average. The stored
#' quantiles and the mean are read from it directly, and the chance of
#' reaching each band as the share of a fine grid of its quantiles (every
#' thousandth) at or above the band's threshold: for a week's count, and
#' for the highest week, which is the chance of reaching the band in any
#' target week. With one component this is that component's forecast.
#'
#' @param components A named list of matrices, one row per path and one
#'   column per target week.
#' @param thresholds `episodic_epidemic_stored_thresholds()`'s output, or
#'   `NULL`.
#' @return A list with `weeks` (a data frame, one row per target week:
#'   `mean`, a column per `episodic_forecast_probs`, and `p_low`,
#'   `p_medium`, `p_high`, `p_very_high`, `NA` where the band has no
#'   threshold) and `estimates` (`p_reach_<band>` for each band with one).
#' @keywords internal
#' @noRd
episodic_outlook_combine <- function(components, thresholds) {
  grid <- seq_len(999) / 1000
  averaged <- function(values, probs) {
    Reduce(`+`, lapply(values, stats::quantile, probs = probs, names = FALSE, type = 1)) /
      length(values)
  }
  horizon <- ncol(components[[1]])
  weeks <- data.frame(
    mean = vapply(seq_len(horizon), function(h) {
      mean(vapply(components, function(m) mean(m[, h]), numeric(1)))
    }, numeric(1))
  )
  for (q in names(episodic_forecast_probs)) {
    weeks[[q]] <- vapply(seq_len(horizon), function(h) {
      averaged(lapply(components, function(m) m[, h]), episodic_forecast_probs[[q]])
    }, numeric(1))
  }
  week_grid <- lapply(seq_len(horizon), function(h) {
    averaged(lapply(components, function(m) m[, h]), grid)
  })
  highest_grid <- averaged(
    lapply(components, function(m) apply(m, 1, max)),
    grid
  )
  estimates <- data.frame(
    quantity = character(0),
    estimate = numeric(0),
    lower = numeric(0),
    upper = numeric(0),
    interval_level = numeric(0),
    stringsAsFactors = FALSE
  )
  for (band in episodic_outlook_bands) {
    column <- paste0("p_", band)
    reached <- episodic_outlook_band_reached(highest_grid, thresholds)[[band]]
    if (is.null(reached)) {
      weeks[[column]] <- NA_real_
      next
    }
    weeks[[column]] <- vapply(week_grid, function(g) {
      mean(episodic_outlook_band_reached(g, thresholds)[[band]])
    }, numeric(1))
    estimates <- rbind(estimates, data.frame(
      quantity = paste0("p_reach_", band),
      estimate = mean(reached),
      lower = NA_real_,
      upper = NA_real_,
      interval_level = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  list(weeks = weeks, estimates = estimates)
}

#' The largest log-scale standard error of the autoregressive share an
#' outlook draws from
#'
#' One is a factor of `e` either way at one standard error: wide, but a
#' share the data speak to. Beyond it the share is not identified, and
#' the outlook uses the endemic model alone.
#' @keywords internal
#' @noRd
episodic_outlook_max_ar_se <- 1

#' Draw negative binomial counts no lower than a floor
#'
#' Each draw comes from the negative binomial with mean `mu` and size
#' `size` restricted to `floor` and above, sampled on the upper tail so a
#' floor far into the tail stays finite. Where the model puts no mass
#' left at or above the floor, as when far more has already been
#' reported than it expects, the draw is the floor itself: the reported
#' count is what is known, and the model has nothing to add to it.
#'
#' @param mu,size Per-draw mean and size.
#' @param floor The lowest count allowed, the same for every draw.
#' @return A numeric vector of counts, one per draw.
#' @keywords internal
#' @noRd
episodic_outlook_draw_at_least <- function(mu, size, floor) {
  n <- length(mu)
  if (floor <= 0) {
    return(stats::rnbinom(n, mu = mu, size = size))
  }
  tail <- stats::pnbinom(floor - 1, mu = mu, size = size, lower.tail = FALSE)
  v <- stats::runif(n) * tail
  draws <- stats::qnbinom(v, mu = mu, size = size, lower.tail = FALSE)
  draws[!(tail > 0) | !is.finite(draws)] <- floor
  pmax(draws, floor)
}

#' Fit an outlook's model to a stream's fully reported weeks
#'
#' @param history Weekly counts, oldest first.
#' @param seasonal Whether the endemic level has the sine-cosine pair of
#'   period 52 weeks.
#' @param autoregressive Whether the model has the autoregressive share of
#'   the week before.
#' @return `NULL` when `surveillance::hhh4()` fails or does not converge
#'   or its estimates are not finite, otherwise a list with `theta` (the
#'   estimates on the fit's own scale: the log autoregressive share and
#'   minus the log overdispersion, where the normal approximation of their
#'   sampling distribution holds), `se` and `vcov` on that scale.
#' @keywords internal
#' @noRd
episodic_outlook_fit <- function(history, seasonal, autoregressive) {
  control <- list(
    end = list(f = if (seasonal) {
      surveillance::addSeason2formula(~1, period = 52)
    } else {
      ~1
    }),
    family = "NegBin1",
    subset = 2:length(history)
  )
  if (autoregressive) {
    control$ar <- list(f = ~1)
  }
  # `surveillance` reports an optimiser that did not converge with
  # `cat()`; captured, since the convergence flag below decides it.
  fit <- NULL
  utils::capture.output(
    fit <- tryCatch(
      suppressWarnings(surveillance::hhh4(
        surveillance::sts(observed = matrix(history), frequency = 52),
        control = control
      )),
      error = function(e) NULL
    )
  )
  if (is.null(fit) || !isTRUE(fit$convergence)) {
    return(NULL)
  }
  theta <- stats::coef(fit, reparamPsi = FALSE)
  vcov <- stats::vcov(fit, reparamPsi = FALSE)
  se <- sqrt(diag(vcov))
  if (!all(is.finite(theta)) || !all(is.finite(se))) {
    return(NULL)
  }
  list(theta = theta, se = se, vcov = vcov)
}

#' An empty outlook week table, with every column in place
#' @keywords internal
#' @noRd
episodic_outlook_weeks_empty <- function() {
  out <- data.frame(
    target_date = character(0),
    horizon = integer(0),
    mean = numeric(0),
    stringsAsFactors = FALSE
  )
  for (col in c(names(episodic_forecast_probs), paste0("p_", episodic_outlook_bands))) {
    out[[col]] <- numeric(0)
  }
  out
}

#' The outlook of every open epidemic, and store it
#'
#' Called once per run, after the nowcast, for every detected
#' epidemic-scale cluster that is neither merged, suppressed nor closed.
#' Every one gets a row, with the reason when there is no outlook. A
#' seasonal epidemic's model is seasonal and its outlook carries the
#' chances of reaching the MEM bands it was opened against. An outlook
#' that fails is recorded as `"failed"` and announced, and does not fail
#' the run.
#'
#' @param con A [DBI::DBIConnection-class], inside the run's transaction.
#' @param run_id,run_date This run.
#' @param config The resolved configuration.
#' @param streams Every stream, from `episodic_db_streams()`.
#' @param stream_cases_for `episodic_stream_case_index()`'s function over
#'   this run's cases.
#' @param week_pmfs This run's nowcast `week_pmfs`, by stream ID.
#' @return Invisibly, a named integer vector: how many epidemics were
#'   `computed`, `insufficient_data` and `failed`.
#' @keywords internal
#' @noRd
episodic_epidemic_outlook_run <- function(con,
                                          run_id,
                                          run_date,
                                          config,
                                          streams,
                                          stream_cases_for,
                                          week_pmfs) {
  counts <- c(computed = 0L, insufficient_data = 0L, failed = 0L)
  settings <- episodic_epidemic_outlook_settings(config)
  if (!settings$enabled) {
    episodic_trace("Epidemic outlook switched off (forecast.epidemic_outlook.enabled)")
    return(invisible(counts))
  }
  max_delay_days <- episodic_nowcast_settings(config)$max_delay_days
  trend_window_weeks <- episodic_epidemic_direction_settings(config)$window_weeks
  open <- episodic_db_clusters_not_closed(con, "epidemic")
  open <- open[open$origin == "detected", , drop = FALSE]
  if (nrow(open) == 0) {
    episodic_trace("Epidemic outlook: no open epidemic")
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
    result <- tryCatch(
      episodic_epidemic_outlook_forecast(
        stream_cases_for(stream[1, ])$sample_date,
        observed_from = observed_from,
        asof = run_date,
        week_pmfs = week_pmfs[[as.character(open$stream_id[i])]] %||% list(),
        max_delay_days = max_delay_days,
        thresholds = episodic_epidemic_stored_thresholds(season),
        settings = settings,
        trend_window_weeks = trend_window_weeks,
        seed = cluster_id
      ),
      error = function(e) {
        list(
          status = "failed",
          detail = conditionMessage(e),
          params = list(method = "hhh4_negbin", asof = format(as.Date(run_date))),
          weeks = NULL,
          estimates = NULL
        )
      }
    )
    if (identical(result$status, "failed")) {
      episodic_trace(
        "Epidemic outlook failed for E-",
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
      kind = "epidemic_outlook",
      method = "hhh4_negbin",
      status = result$status,
      detail = result$detail,
      params = result$params,
      estimates = result$estimates,
      weeks = result$weeks
    )
    counts[[result$status]] <- counts[[result$status]] + 1L
  }
  episodic_trace(
    "Epidemic outlook: ",
    counts[["computed"]],
    " of ",
    nrow(open),
    " open epidemic(s) forecast, ",
    counts[["insufficient_data"]],
    " without enough history or a model that fits, ",
    counts[["failed"]],
    " failed"
  )
  invisible(counts)
}
