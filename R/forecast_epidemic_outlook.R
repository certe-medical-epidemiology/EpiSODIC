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
#' An endemic-epidemic model (`surveillance::hhh4()`, negative binomial)
#' is fitted to the stream's fully reported weeks: each week's expected
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
#' For each target the result gives the mean and the quantiles of
#' `episodic_forecast_probs`, and, for a seasonal epidemic with stored
#' thresholds, the chance of reaching each MEM band that week; and the
#' chance of reaching each band in at least one target week.
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
  for (i in (n_final + 1L):length(all_weeks)) {
    pmf <- week_pmfs[[format(all_weeks[i])]]
    if (!is.null(pmf)) {
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
  counts <- paths[, target_cols, drop = FALSE]
  reached <- episodic_outlook_band_reached(counts, thresholds)
  week_rows <- data.frame(
    target_date = format(targets),
    horizon = seq_len(horizon),
    mean = colMeans(counts),
    stringsAsFactors = FALSE
  )
  for (q in names(episodic_forecast_probs)) {
    week_rows[[q]] <- apply(counts, 2, stats::quantile, probs = episodic_forecast_probs[[q]], names = FALSE, type = 1)
  }
  estimates <- none_estimates
  for (band in episodic_outlook_bands) {
    column <- paste0("p_", band)
    if (is.null(reached[[band]])) {
      week_rows[[column]] <- NA_real_
      next
    }
    week_rows[[column]] <- colMeans(reached[[band]])
    estimates <- rbind(estimates, data.frame(
      quantity = paste0("p_reach_", band),
      estimate = mean(apply(reached[[band]], 1, any)),
      lower = NA_real_,
      upper = NA_real_,
      interval_level = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  list(
    status = "computed",
    detail = NA_character_,
    params = params,
    weeks = week_rows,
    estimates = estimates
  )
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
  fit <- tryCatch(
    suppressWarnings(surveillance::hhh4(
      surveillance::sts(observed = matrix(history), frequency = 52),
      control = control
    )),
    error = function(e) NULL
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
