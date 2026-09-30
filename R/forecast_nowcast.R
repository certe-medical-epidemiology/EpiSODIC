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

#' The quantile levels every stored forecast carries
#'
#' The median and the bounds of the central 50%, 80%, 90% and 95%
#' prediction intervals: the set the weighted interval score is computed
#' over (`episodic_forecast_wis()`), so a stored forecast can be scored
#' without keeping its whole predictive distribution. Named by the column
#' each is stored in on `episodic_forecast_value`.
#' @keywords internal
#' @noRd
episodic_forecast_probs <- c(
  q025 = 0.025,
  q05 = 0.05,
  q10 = 0.10,
  q25 = 0.25,
  q50 = 0.50,
  q75 = 0.75,
  q90 = 0.90,
  q95 = 0.95,
  q975 = 0.975
)

#' Concentration of the Dirichlet prior on the reporting-delay distribution
#'
#' `surveillance::nowcast()`'s own default for `gd.prior.kappa`: a weak
#' prior that lets a stream's own reporting history decide the delay
#' distribution as soon as it has any. A fixed method constant rather
#' than a setting, like the quantile levels above.
#' @keywords internal
#' @noRd
episodic_nowcast_delay_prior_kappa <- 0.1

#' The nowcast settings of a run, checked
#'
#' Read from `forecast.nowcast` in the resolved configuration. Every value
#' changes what a run computes, so the section is inside `config_hash`.
#' A hand-built configuration without the section (a test, a caller
#' passing a fragment) gets the shipped defaults, which is what an
#' instance configuration always resolves to, since the merge is key by
#' key and cannot remove a key.
#'
#' Checked here rather than by `episodic_config_validate()` alone: that
#' checks types against the shipped defaults, and `delay_window_days`
#' ships as null, which carries no type to check against. A value that
#' cannot mean what it says stops the run, as a malformed detector
#' setting does (`episodic_detector_enabled()`).
#'
#' @param config The resolved configuration.
#' @return A list with `enabled` (logical), `max_delay_days`,
#'   `min_reported_cases`, `prior_window_days`, `n_samples` (integers),
#'   `min_completeness` (numeric) and `delay_window_days` (integer, or
#'   `NULL` for the stream's whole reporting history).
#' @keywords internal
#' @noRd
episodic_nowcast_settings <- function(config) {
  settings <- config$forecast$nowcast
  if (is.null(settings)) {
    settings <- episodic_config_resolve_files(NA)$forecast$nowcast
  }
  problem <- function(key, must) {
    stop(
      "`forecast.nowcast.",
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
  max_delay_days <- whole("max_delay_days", 1)
  min_completeness <- settings$min_completeness
  if (
    !is.numeric(min_completeness) || length(min_completeness) != 1 ||
      is.na(min_completeness) || min_completeness <= 0 || min_completeness > 1
  ) {
    problem("min_completeness", "a number above 0 and at most 1")
  }
  delay_window_days <- if (is.null(settings$delay_window_days)) {
    NULL
  } else {
    whole("delay_window_days", max_delay_days)
  }
  list(
    enabled = enabled,
    max_delay_days = max_delay_days,
    min_completeness = as.numeric(min_completeness),
    min_reported_cases = whole("min_reported_cases", 1),
    prior_window_days = whole("prior_window_days", 1),
    delay_window_days = delay_window_days,
    n_samples = whole("n_samples", 100)
  )
}

#' Nowcast a stream's most recent days and weeks
#'
#' A case becomes visible to EpiSODIC in the run that first loads it, so
#' the counts of the most recent sample dates are too low by construction:
#' part of what was sampled on them has not been reported yet. This
#' estimates what those counts will be once reporting is complete, with
#' `surveillance::nowcast()`'s `bayes.trunc` method (Höhle and an der
#' Heiden 2014): the reporting-delay distribution is estimated from the
#' stream's own reporting triangle, allowing for the right truncation the
#' most recent sample dates are subject to, and each day's final count
#' gets a full predictive distribution rather than a point estimate.
#'
#' The reporting date of a case is the `run_date` of the run that first
#' saw it (`episodic_case.first_seen_run`), so its resolution is the
#' schedule the cron runs on: under a daily schedule it is a day, under a
#' weekly one a week, and a run that did not happen makes the cases it
#' would have seen look later than they were. Both widen the estimated
#' delay rather than narrow it.
#'
#' Only sample dates on or after `observation_start`, the first run on
#' the database, carry a reporting delay that was observed. A case sampled
#' before it was reported to the first run, when EpiSODIC was not yet
#' watching, so its delay says when the database began rather than how
#' long the laboratory took; fed to the delay estimate, a backfilled
#' archive would read as years of reporting delay.
#'
#' The prior for each day's final count is a gamma centred on the
#' stream's mean daily count over the `prior_window_days` most recent
#' days whose reporting is complete, with a standard deviation equal to
#' its mean: weak enough that an outbreak running well above that level
#' is carried by the reported counts, not pulled back towards baseline.
#' Half a case is added to the window's count, so a stream with no case
#' in it has a small positive prior mean rather than a degenerate one.
#'
#' A day whose expected completeness (the estimated delay distribution's
#' cumulative share at its lag) is below `min_completeness` is not
#' nowcast: so little of it has arrived that its estimate is the prior
#' rather than the data, and a band drawn from it would read as a
#' measurement. A week is nowcast only when it has fully elapsed and
#' every one of its days is either past the reporting horizon or
#' nowcast; its distribution is the convolution of its days', which
#' treats them as independent given the delay distribution, as the
#' method itself does.
#'
#' A case whose sample date is later than the run that first saw it
#' cannot have a reporting delay. It is left out of the delay estimate
#' and counted on its sample date as reported, and the number of such
#' cases is recorded in the result's `params`.
#'
#' @param cases A data frame of the stream's cases, with `sample_date`
#'   and `report_date` (the `run_date` of each case's `first_seen_run`).
#' @param asof The date the nowcast is made on: this run's `run_date`.
#' @param observation_start The `run_date` of the first run on the
#'   database.
#' @param settings `episodic_nowcast_settings()`'s output.
#' @param seed Seed for the sampling inside `surveillance::nowcast()`, so
#'   the same data and configuration give the same nowcast. The session's
#'   own random stream is restored afterwards.
#' @return A list with `status` (`"computed"` or `"insufficient_data"`),
#'   `detail` (`NA`, or the reason code for `"insufficient_data"`:
#'   `"reporting_history"` when too few cases have a fully observed
#'   reporting delay, `"completeness"` when no day is complete enough to
#'   nowcast), `params` (a list of what the nowcast was computed from) and
#'   `values` (a data frame with `resolution` (`"day"` or `"week"`),
#'   `target_date` (text), `n_observed` and `mean`, and one column per
#'   `episodic_forecast_probs`; zero rows unless `"computed"`). An error
#'   inside `surveillance::nowcast()` is not caught here.
#' @keywords internal
#' @noRd
episodic_nowcast_stream <- function(cases,
                                    asof,
                                    observation_start,
                                    settings,
                                    seed) {
  asof <- as.Date(asof)
  observation_start <- as.Date(observation_start)
  sample_date <- as.Date(cases$sample_date)
  report_date <- as.Date(cases$report_date)
  if (anyNA(sample_date) || anyNA(report_date)) {
    stop(
      "Every case passed to `episodic_nowcast_stream()` needs a sample ",
      "date and a reporting date.",
      call. = FALSE
    )
  }
  max_delay <- settings$max_delay_days

  reported <- sample_date <= asof & report_date <= asof
  impossible <- reported & report_date < sample_date
  in_window <- reported & !impossible & sample_date >= observation_start
  delay <- as.integer(report_date - sample_date)
  horizon <- asof - max_delay
  # The cases whose reporting has been watched for the full horizon,
  # inside the sliding window when there is one: what the delay
  # distribution is chiefly estimated from.
  delay_observed <- in_window & sample_date <= horizon
  if (!is.null(settings$delay_window_days)) {
    delay_observed <- delay_observed &
      report_date > asof - settings$delay_window_days
  }
  n_delay_observed <- sum(delay_observed)

  params <- list(
    method = "bayes.trunc",
    asof = as.character(asof),
    observation_start = as.character(observation_start),
    max_delay_days = max_delay,
    min_completeness = settings$min_completeness,
    min_reported_cases = settings$min_reported_cases,
    prior_window_days = settings$prior_window_days,
    delay_window_days = settings$delay_window_days,
    n_samples = settings$n_samples,
    delay_prior_kappa = episodic_nowcast_delay_prior_kappa,
    seed = seed,
    n_cases_delay_observed = n_delay_observed,
    n_cases_reported_before_sampled = sum(impossible),
    n_cases_delay_beyond_max = sum(in_window & delay > max_delay)
  )
  empty <- episodic_forecast_values_empty()
  if (n_delay_observed < settings$min_reported_cases) {
    return(list(
      status = "insufficient_data",
      detail = "reporting_history",
      params = params,
      values = empty
    ))
  }

  prior_from <- max(observation_start, horizon - settings$prior_window_days + 1L)
  prior_days <- as.integer(horizon - prior_from) + 1L
  prior_n <- sum(in_window & sample_date >= prior_from & sample_date <= horizon)
  mean_lambda <- (prior_n + 0.5) / prior_days
  params$prior_mean_daily <- mean_lambda

  # `observation_start` is on or before the horizon here, since some
  # case sampled between the two has a fully observed delay.
  when <- seq(horizon + 1L, asof, by = "day")
  # Counted over every reported case, the ones left out of the delay
  # estimate included: the day's count on the curve counts them, and so
  # must its nowcast.
  observed <- vapply(
    seq_along(when),
    function(i) sum(reported & sample_date == when[i]),
    integer(1)
  )
  offset <- vapply(
    seq_along(when),
    function(i) sum(impossible & sample_date == when[i]),
    integer(1)
  )
  # Wide enough that no retained day's predictive distribution reaches
  # its edge: each is centred near observed / completeness, and never
  # far above the prior when little has been observed. Checked below
  # rather than trusted.
  support <- as.integer(ceiling(max(
    100,
    3 * (max(observed) + 1) / settings$min_completeness,
    10 * mean_lambda
  )))
  params$support_max <- support

  previous_seed <- episodic_seed_snapshot()
  on.exit(episodic_seed_restore(previous_seed), add = TRUE)
  set.seed(seed)
  warnings <- character(0)
  fit <- NULL
  # `nowcast()` narrates its progress with `cat()` and `message()`. Its
  # one expected warning, delays beyond `D` being set to `D`, is already
  # counted in `params`; any other is kept there too rather than dropped.
  utils::capture.output(
    fit <- withCallingHandlers(
      suppressMessages(surveillance::nowcast(
        now = asof,
        when = when,
        data = data.frame(
          dEvent = sample_date[in_window],
          dReport = report_date[in_window]
        ),
        dEventCol = "dEvent",
        dReportCol = "dReport",
        method = "bayes.trunc",
        D = max_delay,
        m = settings$delay_window_days,
        control = list(
          dRange = c(observation_start, asof),
          alpha = 0.05,
          nSamples = settings$n_samples,
          N.tInf.prior = structure(
            "poisgamma",
            mean.lambda = mean_lambda,
            var.lambda = mean_lambda^2
          ),
          N.tInf.max = support,
          gd.prior.kappa = episodic_nowcast_delay_prior_kappa,
          predPMF = TRUE
        )
      )),
      warning = function(w) {
        message_text <- conditionMessage(w)
        if (!grepl("forced to have a delay of D", message_text, fixed = TRUE)) {
          warnings <<- c(warnings, message_text)
        }
        invokeRestart("muffleWarning")
      }
    )
  )
  if (length(warnings) > 0) {
    params$warnings <- as.list(warnings)
  }

  delay_cdf <- fit@delayCDF[["bayes.trunc"]]
  if (length(delay_cdf) != max_delay + 1L) {
    stop(
      "`surveillance::nowcast()` returned a delay distribution over ",
      length(delay_cdf),
      " delays, not the ",
      max_delay + 1L,
      " asked for.",
      call. = FALSE
    )
  }
  lag <- as.integer(asof - when)
  completeness <- delay_cdf[lag + 1L]
  retained <- completeness >= settings$min_completeness
  if (!any(retained)) {
    return(list(
      status = "insufficient_data",
      detail = "completeness",
      params = params,
      values = empty
    ))
  }

  pmfs <- vector("list", length(when))
  for (i in which(retained)) {
    pmf <- fit@predPMF[[as.character(when[i])]][["bayes.trunc"]]
    if (is.null(pmf)) {
      stop(
        "`surveillance::nowcast()` returned no predictive distribution ",
        "for ",
        format(when[i]),
        ".",
        call. = FALSE
      )
    }
    pmf <- as.numeric(pmf)
    # The support is sized above; a distribution that still has mass
    # near its edge has been cut off there, and its upper quantiles
    # would be the edge rather than an estimate.
    edge <- floor(0.9 * (length(pmf) - 1L))
    if (sum(pmf[(edge + 1L):length(pmf)]) > 1e-4) {
      stop(
        "The nowcast for ",
        format(when[i]),
        " reaches the edge of its support (",
        support,
        " cases), so its upper quantiles cannot be read from it.",
        call. = FALSE
      )
    }
    pmfs[[i]] <- c(rep(0, offset[i]), pmf)
  }

  day_rows <- lapply(which(retained), function(i) {
    episodic_forecast_value_row("day", when[i], observed[i], pmfs[[i]])
  })

  # Weeks that have fully elapsed and reach into the nowcast window.
  # Their days before the window are past the reporting horizon, and
  # count as final.
  first_week <- episodic_week_start(min(when))
  last_week <- episodic_last_complete_week_start(asof)
  week_rows <- list()
  if (last_week >= first_week) {
    weeks <- seq(first_week, last_week, by = "week")
    for (w in seq_along(weeks)) {
      week <- weeks[w]
      days <- week + 0:6
      idx <- match(days, when)
      nowcast_idx <- idx[!is.na(idx)]
      if (!all(retained[nowcast_idx])) {
        next
      }
      final <- sum(reported & sample_date %in% days[is.na(idx)])
      pmf <- c(rep(0, final), 1)
      for (i in nowcast_idx) {
        pmf <- episodic_pmf_convolve(pmf, pmfs[[i]])
      }
      week_rows[[length(week_rows) + 1L]] <- episodic_forecast_value_row(
        "week",
        week,
        sum(reported & sample_date %in% days),
        pmf
      )
    }
  }

  list(
    status = "computed",
    detail = NA_character_,
    params = params,
    values = do.call(rbind, c(day_rows, week_rows))
  )
}

#' An empty forecast value table, with every column in place
#' @keywords internal
#' @noRd
episodic_forecast_values_empty <- function() {
  out <- data.frame(
    resolution = character(0),
    target_date = character(0),
    n_observed = integer(0),
    mean = numeric(0),
    stringsAsFactors = FALSE
  )
  for (col in names(episodic_forecast_probs)) {
    out[[col]] <- numeric(0)
  }
  out
}

#' One forecast value row from a predictive distribution
#'
#' @param resolution `"day"` or `"week"`.
#' @param target_date The day, or the Monday of the week.
#' @param n_observed What had been reported for it when the forecast was
#'   made.
#' @param pmf The predictive probability mass function over `0, 1, ...`.
#' @return A one-row data frame shaped like
#'   `episodic_forecast_values_empty()`.
#' @keywords internal
#' @noRd
episodic_forecast_value_row <- function(resolution, target_date, n_observed, pmf) {
  pmf <- pmf / sum(pmf)
  cdf <- cumsum(pmf)
  counts <- seq_along(pmf) - 1L
  out <- data.frame(
    resolution = resolution,
    target_date = format(as.Date(target_date)),
    n_observed = as.integer(n_observed),
    mean = sum(counts * pmf),
    stringsAsFactors = FALSE
  )
  for (col in names(episodic_forecast_probs)) {
    # The smallest count whose cumulative probability reaches the level,
    # less a rounding allowance so a level the distribution reaches
    # exactly is not missed by the last bit of a floating-point sum.
    out[[col]] <- as.numeric(
      counts[which(cdf >= episodic_forecast_probs[[col]] - 1e-9)[1]]
    )
  }
  out
}

#' The distribution of the sum of two independent counts
#'
#' @param a,b Probability mass functions over `0, 1, ...`.
#' @return The mass function of their sum, trimmed of a negligible upper
#'   tail and renormalised.
#' @keywords internal
#' @noRd
episodic_pmf_convolve <- function(a, b) {
  out <- stats::convolve(a, rev(b), type = "open")
  # The transform leaves rounding noise of either sign where the exact
  # result is zero.
  out[out < 0] <- 0
  keep <- max(which(out > 1e-12))
  out <- out[seq_len(keep)]
  out / sum(out)
}

#' Nowcast every stream that carries an open cluster, and store it
#'
#' Called once per run, after reconciliation and suppression, so the set
#' of open clusters is the one this run leaves behind. A stream is
#' nowcast when it carries a detected cluster that is neither merged,
#' suppressed nor closed, of either scale: that is where a dossier will
#' show its most recent days. A manual cluster has no case data of this
#' instance to nowcast.
#'
#' Every such stream gets a row in `episodic_forecast`, whether or not a
#' nowcast could be made, so the dossier can say why there is none. A
#' nowcast that fails is recorded as `"failed"` with its error and
#' announced; it does not fail the run, whose detection results do not
#' depend on it. A warning the method raised is announced too, as well as
#' kept in the forecast's `params`.
#'
#' The reporting date of a case is its `first_seen_run`'s `run_date`,
#' read from the committed runs and this one: a case can only have been
#' first seen by a run that committed, since a failed run rolls back
#' every case it loaded.
#'
#' @param con A [DBI::DBIConnection-class], inside the run's transaction.
#' @param run_id,run_date This run.
#' @param config The resolved configuration.
#' @param streams Every stream, from `episodic_db_streams()`.
#' @param stream_cases_for `episodic_stream_case_index()`'s function over
#'   this run's cases.
#' @return Invisibly, a named integer vector: how many streams were
#'   `computed`, `insufficient_data` and `failed`.
#' @keywords internal
#' @noRd
episodic_nowcast_run <- function(con,
                                 run_id,
                                 run_date,
                                 config,
                                 streams,
                                 stream_cases_for) {
  counts <- c(computed = 0L, insufficient_data = 0L, failed = 0L)
  settings <- episodic_nowcast_settings(config)
  if (!settings$enabled) {
    episodic_trace("Nowcast switched off (forecast.nowcast.enabled)")
    return(invisible(counts))
  }
  open <- rbind(
    episodic_db_clusters_not_closed(con, "outbreak"),
    episodic_db_clusters_not_closed(con, "epidemic")
  )
  stream_ids <- sort(unique(open$stream_id[open$origin == "detected"]))
  if (length(stream_ids) == 0) {
    episodic_trace("Nowcast: no stream carries an open cluster")
    return(invisible(counts))
  }

  runs <- episodic_db_get_query(
    con,
    sprintf(
      "SELECT run_id, run_date FROM episodic_detection_run
        WHERE status IN (%s) OR run_id = ?",
      paste(rep("?", length(episodic_run_statuses_complete)), collapse = ", ")
    ),
    params = c(as.list(episodic_run_statuses_complete), list(run_id))
  )
  runs$run_date <- as.Date(substr(runs$run_date, 1, 10))
  observation_start <- min(runs$run_date)

  for (stream_id in stream_ids) {
    stream <- streams[streams$stream_id == stream_id, , drop = FALSE]
    if (nrow(stream) == 0) {
      # Clusters are opened only on streams the run enumerated, so an
      # open cluster on a stream the run does not know is a broken
      # database, not a stream to skip.
      stop(
        "Open cluster on stream ",
        stream_id,
        ", which this run does not know.",
        call. = FALSE
      )
    }
    cases <- stream_cases_for(stream[1, ])
    cases <- data.frame(
      sample_date = cases$sample_date,
      report_date = runs$run_date[match(cases$first_seen_run, runs$run_id)]
    )
    result <- tryCatch(
      episodic_nowcast_stream(
        cases,
        asof = run_date,
        observation_start = observation_start,
        settings = settings,
        seed = stream_id
      ),
      error = function(e) {
        list(
          status = "failed",
          detail = conditionMessage(e),
          params = list(method = "bayes.trunc", asof = as.character(run_date)),
          values = episodic_forecast_values_empty()
        )
      }
    )
    if (identical(result$status, "failed")) {
      episodic_trace(
        "Nowcast failed for stream ",
        stream_id,
        " (",
        episodic_stream_label(stream[1, ]),
        "): ",
        result$detail,
        severity = "warn"
      )
    }
    for (warning_text in unlist(result$params$warnings)) {
      episodic_trace(
        "Nowcast for stream ",
        stream_id,
        " (",
        episodic_stream_label(stream[1, ]),
        ") warned: ",
        warning_text,
        severity = "warn"
      )
    }
    episodic_db_forecast_insert(
      con,
      run_id = run_id,
      stream_id = stream_id,
      kind = "nowcast",
      method = "bayes.trunc",
      status = result$status,
      detail = result$detail,
      params = result$params,
      values = result$values
    )
    counts[[result$status]] <- counts[[result$status]] + 1L
  }
  episodic_trace(
    "Nowcast: ",
    counts[["computed"]],
    " of ",
    length(stream_ids),
    " stream(s) with an open cluster nowcast, ",
    counts[["insufficient_data"]],
    " without enough reported yet to nowcast from, ",
    counts[["failed"]],
    " failed"
  )
  invisible(counts)
}
