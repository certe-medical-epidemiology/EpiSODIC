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

#' The outbreak-end settings of a run, checked
#'
#' Read from `forecast.outbreak_end`, with the shipped defaults for a
#' hand-built configuration without the section, as
#' `episodic_nowcast_settings()` does.
#'
#' @param config The resolved configuration.
#' @return A list with `enabled` (logical) and `probability_threshold`
#'   (numeric, the probability whose date the dossier reports).
#' @keywords internal
#' @noRd
episodic_outbreak_end_settings <- function(config) {
  settings <- config$forecast$outbreak_end
  if (is.null(settings)) {
    settings <- episodic_config_resolve_files(NA)$forecast$outbreak_end
  }
  enabled <- settings$enabled
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    stop(
      "`forecast.outbreak_end.enabled` must be true or false, not ",
      paste(format(enabled), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  threshold <- settings$probability_threshold
  if (
    !is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) ||
      threshold <= 0 || threshold >= 1
  ) {
    stop(
      "`forecast.outbreak_end.probability_threshold` must be a number ",
      "above 0 and below 1, not ",
      paste(format(threshold), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  list(enabled = enabled, probability_threshold = as.numeric(threshold))
}

#' The cumulative distribution function of a serial interval
#'
#' From the mean and standard deviation in the pathogen configuration,
#' in the family `si_dist` names: gamma, lognormal, or Weibull, whose
#' shape is solved from the coefficient of variation.
#'
#' @param mean,sd The serial interval's mean and standard deviation, in
#'   days; both positive.
#' @param dist `"gamma"`, `"lognormal"` or `"weibull"`.
#' @return A function of days, vectorised.
#' @keywords internal
#' @noRd
episodic_serial_interval_cdf <- function(mean, sd, dist) {
  if (
    !is.numeric(mean) || !is.numeric(sd) || length(mean) != 1 ||
      length(sd) != 1 || is.na(mean) || is.na(sd) || mean <= 0 || sd <= 0
  ) {
    stop("A serial interval needs a positive mean and standard deviation.", call. = FALSE)
  }
  switch(dist,
    gamma = {
      shape <- (mean / sd)^2
      rate <- mean / sd^2
      function(x) stats::pgamma(x, shape = shape, rate = rate)
    },
    lognormal = {
      sigma2 <- log(1 + (sd / mean)^2)
      mu <- log(mean) - sigma2 / 2
      function(x) stats::plnorm(x, meanlog = mu, sdlog = sqrt(sigma2))
    },
    weibull = {
      cv2 <- (sd / mean)^2
      shape <- stats::uniroot(
        function(k) gamma(1 + 2 / k) / gamma(1 + 1 / k)^2 - 1 - cv2,
        interval = c(0.05, 100),
        tol = 1e-10
      )$root
      scale <- mean / gamma(1 + 1 / shape)
      function(x) stats::pweibull(x, shape = shape, scale = scale)
    },
    stop("Unknown serial interval distribution \"", dist, "\".", call. = FALSE)
  )
}

#' The probability that an outbreak is over, by date
#'
#' The probability that none of an outbreak's cases goes on to cause a
#' case that has not already been reported, following Nishiura, Miyamatsu
#' and Mizumoto (2016, Emerging Infectious Diseases 22: 146-148) with
#' the negative binomial offspring distribution of Linton et al. (2022,
#' Philosophical Transactions of the Royal Society B 377: 20210112): each
#' case contributes `(1 + (R / k) (1 - q)) ^ -k`, where `R` and `k` are
#' the offspring distribution's mean and dispersion and `q` is the
#' probability that one of its offspring would already have been seen.
#'
#' In the original, `q` is the serial interval's cumulative probability
#' at the time since the case, which treats every case sampled before
#' today as reported. Here a case's offspring sampled on day `s` counts
#' as seen by day `t` only with the stream's reporting completeness at
#' lag `t - s`, so a quiet last few days are not read as evidence while
#' their cases may still be on their way. The serial interval is
#' discretised to days by the mass each day's half-open interval
#' `[d - 0.5, d + 0.5)` carries, with day 0 from 0.
#'
#' Sample dates stand in for onset dates, as they do for Rt
#' (`episodic_compute_rt()`), and the offspring distribution is the
#' pathogen's configured one, not estimated from the outbreak. For each
#' date from `from` on, the probability assumes no further case has been
#' reported by then; it rises with each such date, and the series ends
#' at the first date it reaches `until_probability`, or after
#' `max_days`.
#'
#' @param case_dates The outbreak's case sample dates.
#' @param from The first date to evaluate: the date the data is current as
#'   of.
#' @param si_cdf `episodic_serial_interval_cdf()`'s function.
#' @param end_r,end_k The offspring distribution's mean and dispersion.
#' @param completeness The stream's completeness by reporting lag, from
#'   `episodic_triangle_completeness()`: `lag_days` and `completeness`.
#'   Read as non-decreasing in the lag (its running maximum), and as
#'   complete beyond its last lag.
#' @param until_probability Where the series may stop.
#' @param max_days The most dates evaluated after `from`.
#' @return A data frame with `target_date` (`Date`) and `probability`.
#' @keywords internal
#' @noRd
episodic_outbreak_end_curve <- function(case_dates,
                                        from,
                                        si_cdf,
                                        end_r,
                                        end_k,
                                        completeness,
                                        until_probability = 0.99,
                                        max_days = 365L) {
  case_dates <- as.Date(case_dates)
  from <- as.Date(from)
  if (length(case_dates) == 0 || anyNA(case_dates) || any(case_dates > from)) {
    stop(
      "An outbreak-end probability needs case dates, none after the date ",
      "it is evaluated from.",
      call. = FALSE
    )
  }
  completeness <- completeness[order(completeness$lag_days), , drop = FALSE]
  observed_by_lag <- cummax(pmin(pmax(completeness$completeness, 0), 1))
  observed_at <- function(lag) {
    out <- rep(1, length(lag))
    known <- lag <= max(completeness$lag_days)
    out[known] <- observed_by_lag[match(lag[known], completeness$lag_days)]
    # A lag inside the curve's range that the curve has no row for has no
    # measurement; the nearest shorter lag's, which the running maximum
    # makes a lower bound, stands in for it.
    missing <- known & is.na(out)
    for (i in which(missing)) {
      shorter <- completeness$lag_days <= lag[i]
      out[i] <- if (any(shorter)) max(observed_by_lag[shorter]) else 0
    }
    out
  }

  horizon <- as.integer(from - min(case_dates)) + as.integer(max_days)
  days <- 0:horizon
  si_mass <- si_cdf(days + 0.5) - si_cdf(pmax(days - 0.5, 0))

  probability_at <- function(t) {
    since <- as.integer(t - case_dates)
    q <- vapply(since, function(tau) {
      d <- 0:tau
      sum(si_mass[d + 1L] * observed_at(tau - d))
    }, numeric(1))
    prod((1 + (end_r / end_k) * (1 - q))^(-end_k))
  }

  dates <- from + 0:max_days
  probability <- numeric(0)
  for (i in seq_along(dates)) {
    probability[i] <- probability_at(dates[i])
    if (probability[i] >= until_probability) {
      break
    }
  }
  data.frame(
    target_date = dates[seq_along(probability)],
    probability = probability
  )
}

#' The outbreak-end forecast for one outbreak
#'
#' Decides whether the method applies and has what it needs, and computes
#' it when it does.
#'
#' The method is a transmission model, so it applies only to a pathogen
#' configured as transmissible (`rt_applicable`) with a complete serial
#' interval; for any other it is `NULL`, and nothing is recorded. It does
#' not apply either to an outbreak whose case dates all fall within one
#' maximum incubation period (`episodic_classify_curve_shape()`'s
#' `"point_source"`): such cases share an exposure rather than infecting
#' each other, and a serial interval says nothing about when a common
#' source stops.
#'
#' @param cases The outbreak's cases, with `sample_date`.
#' @param pc The pathogen's configuration row, or `NULL`.
#' @param completeness `episodic_triangle_completeness()` for its stream.
#' @param asof The date the data is current as of: this run's date.
#' @param probability_threshold The probability whose date the dossier
#'   reports; the series runs until it reaches this or 0.99, whichever is
#'   higher.
#' @return `NULL` when the method does not apply to the pathogen;
#'   otherwise a list with `status` (`"computed"`, `"not_applicable"` or
#'   `"insufficient_data"`), `detail` (`NA`, or a reason code:
#'   `"point_source"`, `"parameters"` when `end_r` and `end_k` are not
#'   configured, `"reporting_delay"` when the stream's reporting delay was
#'   never measured), `params`, and `values` (`target_date`,
#'   `probability`; empty unless `"computed"`).
#' @keywords internal
#' @noRd
episodic_outbreak_end_forecast <- function(cases,
                                           pc,
                                           completeness,
                                           asof,
                                           probability_threshold = 0.95) {
  if (
    is.null(pc) || !isTRUE(as.logical(pc$rt_applicable)) ||
      is.na(pc$si_mean_days) || is.na(pc$si_sd_days) || is.na(pc$si_dist)
  ) {
    return(NULL)
  }
  asof <- as.Date(asof)
  case_dates <- as.Date(cases$sample_date)
  params <- list(
    method = "nishiura_nb",
    asof = format(asof),
    n_cases = length(case_dates),
    last_case_date = format(max(case_dates)),
    case_free_days = as.integer(pc$case_free_days),
    reporting_horizon_days = episodic_triangle_max_lag_days,
    si_mean_days = as.numeric(pc$si_mean_days),
    si_sd_days = as.numeric(pc$si_sd_days),
    si_dist = as.character(pc$si_dist),
    end_r = as.numeric(pc$end_r),
    end_k = as.numeric(pc$end_k)
  )
  none <- data.frame(target_date = as.Date(character(0)), probability = numeric(0))
  outcome <- function(status, detail) {
    list(status = status, detail = detail, params = params, values = none)
  }
  shape <- episodic_classify_curve_shape(cases, pc$incub_max_days)
  params$curve_shape <- shape
  if (identical(shape, "point_source")) {
    return(outcome("not_applicable", "point_source"))
  }
  if (is.na(params$end_r) || is.na(params$end_k)) {
    return(outcome("insufficient_data", "parameters"))
  }
  if (nrow(completeness) == 0) {
    return(outcome("insufficient_data", "reporting_delay"))
  }
  list(
    status = "computed",
    detail = NA_character_,
    params = params,
    values = episodic_outbreak_end_curve(
      case_dates,
      from = asof,
      si_cdf = episodic_serial_interval_cdf(
        params$si_mean_days,
        params$si_sd_days,
        params$si_dist
      ),
      end_r = params$end_r,
      end_k = params$end_k,
      completeness = completeness,
      until_probability = max(0.99, probability_threshold)
    )
  )
}

#' Forecast the end of every open outbreak, and store it
#'
#' Called once per run, after reconciliation and suppression, for every
#' detected outbreak-scale cluster that is neither merged, suppressed nor
#' closed. An outbreak whose pathogen the method does not apply to gets
#' no row; every other gets one, with the reason when there is no
#' probability. A forecast that fails is recorded as `"failed"` and
#' announced, and does not fail the run.
#'
#' @param con A [DBI::DBIConnection-class], inside the run's transaction.
#' @param run_id,run_date This run.
#' @param config The resolved configuration.
#' @return Invisibly, a named integer vector: how many outbreaks were
#'   `computed`, `not_applicable`, `insufficient_data` and `failed`.
#' @keywords internal
#' @noRd
episodic_outbreak_end_run <- function(con, run_id, run_date, config) {
  counts <- c(computed = 0L, not_applicable = 0L, insufficient_data = 0L, failed = 0L)
  settings <- episodic_outbreak_end_settings(config)
  if (!settings$enabled) {
    episodic_trace("Outbreak-end forecast switched off (forecast.outbreak_end.enabled)")
    return(invisible(counts))
  }
  open <- episodic_db_clusters_not_closed(con, "outbreak")
  open <- open[open$origin == "detected", , drop = FALSE]
  n_forecast <- 0L
  for (i in seq_len(nrow(open))) {
    cluster_id <- open$cluster_id[i]
    stream <- episodic_db_get_query(
      con,
      "SELECT stream_id, pathogen FROM episodic_stream WHERE stream_id = ?",
      params = list(open$stream_id[i])
    )
    pc <- episodic_db_pathogen_config_get(con, stream$pathogen[1])
    result <- tryCatch(
      episodic_outbreak_end_forecast(
        episodic_db_cluster_cases(con, cluster_id),
        pc,
        completeness = episodic_triangle_completeness(con, stream$stream_id[1]),
        asof = run_date,
        probability_threshold = settings$probability_threshold
      ),
      error = function(e) {
        list(
          status = "failed",
          detail = conditionMessage(e),
          params = list(method = "nishiura_nb", asof = format(as.Date(run_date))),
          values = data.frame(target_date = as.Date(character(0)), probability = numeric(0))
        )
      }
    )
    if (is.null(result)) {
      next
    }
    n_forecast <- n_forecast + 1L
    if (identical(result$status, "failed")) {
      episodic_trace(
        "Outbreak-end forecast failed for O-",
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
      kind = "outbreak_end",
      method = "nishiura_nb",
      status = result$status,
      detail = result$detail,
      params = result$params,
      values = result$values
    )
    counts[[result$status]] <- counts[[result$status]] + 1L
  }
  episodic_trace(
    "Outbreak-end forecast: ",
    counts[["computed"]],
    " of ",
    n_forecast,
    " open outbreak(s) of a transmissible pathogen computed, ",
    counts[["not_applicable"]],
    " point source, ",
    counts[["insufficient_data"]],
    " without the parameters or reporting history, ",
    counts[["failed"]],
    " failed"
  )
  invisible(counts)
}
