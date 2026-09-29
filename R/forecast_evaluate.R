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

#' Weighted interval score of quantile forecasts
#'
#' The weighted interval score of Bracher et al. (2021, PLOS Computational
#' Biology 17: e1008618) over the median and the central 50%, 80%, 90%
#' and 95% intervals `episodic_forecast_probs` stores: a proper score
#' for a forecast given as quantiles, in the unit of the count, lower is
#' better. Each interval contributes its width plus a penalty of `2 /
#' alpha` per unit the outcome falls outside it, weighted by `alpha / 2`,
#' and the median its absolute error weighted by one half, all divided by
#' the number of intervals plus one half.
#'
#' A point forecast is the special case with no intervals, whose score is
#' its absolute error: which is what a forecast is compared against when
#' its alternative is to take the count reported so far as final.
#'
#' @param forecasts A data frame with one column per
#'   `episodic_forecast_probs`.
#' @param y The outcomes, one per row.
#' @return A numeric vector, one score per row.
#' @keywords internal
#' @noRd
episodic_forecast_wis <- function(forecasts, y) {
  intervals <- list(
    c("q25", "q75", 0.5),
    c("q10", "q90", 0.2),
    c("q05", "q95", 0.1),
    c("q025", "q975", 0.05)
  )
  total <- 0.5 * abs(y - forecasts$q50)
  for (interval in intervals) {
    lower <- forecasts[[interval[1]]]
    upper <- forecasts[[interval[2]]]
    alpha <- as.numeric(interval[3])
    score <- (upper - lower) +
      (2 / alpha) * pmax(lower - y, 0) +
      (2 / alpha) * pmax(y - upper, 0)
    total <- total + (alpha / 2) * score
  }
  total / (length(intervals) + 0.5)
}

#' Every stored nowcast value, scored where its target is fully reported
#'
#' A nowcast estimates the count a day or week will have once its
#' reporting is complete, so it is scored against that count only when
#' the reporting horizon it was made with (`max_delay_days`) has passed
#' for the whole target by the latest completed run. The outcome is the
#' stream's count for the target now, read through the same membership
#' rule the runs use (`episodic_db_cases_for_stream_id()`).
#'
#' A value whose target has not yet passed its horizon is pending, not
#' dropped and not scored: it has no outcome yet.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography The geography stream membership is decided under.
#' @return A data frame, one row per stored value: `forecast_id`,
#'   `run_id`, `stream_id`, `level`, `resolution`, `target_date`,
#'   `n_observed`, the quantile columns, `final` (logical), and, `NA`
#'   unless `final`, `outcome`, `wis`, `ae_reported` (the absolute error
#'   of the count reported when the nowcast was made, taken as final),
#'   `in_50` and `in_90` (whether the outcome fell inside the central 50%
#'   and 90% intervals).
#' @keywords internal
#' @noRd
episodic_nowcast_scores <- function(con, geography = episodic_geography_config()) {
  values <- episodic_db_get_query(
    con,
    paste0(
      "SELECT f.forecast_id, f.run_id, f.stream_id, f.params, s.level,
              v.resolution, v.target_date, v.n_observed, ",
      paste0("v.", names(episodic_forecast_probs), collapse = ", "),
      " FROM episodic_forecast_value v
         JOIN episodic_forecast f ON f.forecast_id = v.forecast_id
         JOIN episodic_stream s ON s.stream_id = f.stream_id
        WHERE f.kind = 'nowcast' AND f.status = 'computed'
        ORDER BY f.forecast_id, v.resolution, v.target_date"
    )
  )
  values$final <- logical(nrow(values))
  for (col in c("outcome", "wis", "ae_reported")) {
    values[[col]] <- rep(NA_real_, nrow(values))
  }
  values$in_50 <- rep(NA, nrow(values))
  values$in_90 <- rep(NA, nrow(values))
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  if (nrow(values) == 0 || is.null(run)) {
    values$params <- NULL
    return(values)
  }
  asof <- as.Date(substr(run$run_date, 1, 10))

  # Parsed once per forecast, not once per value: a forecast carries a
  # few dozen values, all made with the same horizon.
  forecasts <- values[!duplicated(values$forecast_id), c("forecast_id", "params")]
  horizon <- vapply(
    forecasts$params,
    function(p) {
      as.integer(jsonlite::fromJSON(p, simplifyVector = TRUE)$max_delay_days)
    },
    integer(1)
  )
  max_delay <- horizon[match(values$forecast_id, forecasts$forecast_id)]
  values$params <- NULL
  start <- as.Date(values$target_date)
  end <- start + ifelse(values$resolution == "week", 6L, 0L)
  values$final <- end + max_delay <= asof

  for (stream_id in unique(values$stream_id[values$final])) {
    rows <- which(values$final & values$stream_id == stream_id)
    cases <- episodic_db_cases_for_stream_id(
      con,
      stream_id,
      columns = "sample_date",
      first_day = format(min(start[rows])),
      last_day = format(max(end[rows])),
      geography = geography
    )
    dates <- as.Date(cases$sample_date)
    values$outcome[rows] <- vapply(
      rows,
      function(i) sum(dates >= start[i] & dates <= end[i]),
      integer(1)
    )
  }
  scored <- values$final
  y <- values$outcome[scored]
  values$wis[scored] <- episodic_forecast_wis(values[scored, , drop = FALSE], y)
  values$ae_reported[scored] <- abs(y - values$n_observed[scored])
  values$in_50[scored] <- values$q25[scored] <= y & y <= values$q75[scored]
  values$in_90[scored] <- values$q05[scored] <= y & y <= values$q95[scored]
  values
}

#' How well the nowcasts have done, per level and resolution
#'
#' The Performance screen's nowcast section: for each lattice level and
#' resolution that has any stored nowcast value, how many are scored and
#' how many are still pending, the share of outcomes inside the central
#' 50% and 90% intervals, the mean weighted interval score, and the mean
#' absolute error of taking the count reported at the time as final.
#' Coverage near 50% and 90% says the intervals are as wide as they
#' should be; a mean score below that absolute error says the nowcast
#' improved on reading the reported count as final.
#'
#' Every rate has the scored values as its denominator, so a group with
#' none scored has `NA` for each, not zero.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography See `episodic_nowcast_scores()`.
#' @return A data frame with `level`, `resolution`, `n_scored`,
#'   `n_pending`, `coverage_50`, `coverage_90`, `mean_wis` and
#'   `mean_ae_reported`, ordered by level from finest to coarsest, then
#'   day before week; zero rows when nothing has been nowcast.
#' @keywords internal
#' @noRd
episodic_nowcast_performance <- function(con, geography = episodic_geography_config()) {
  scores <- episodic_nowcast_scores(con, geography = geography)
  empty <- data.frame(
    level = character(0),
    resolution = character(0),
    n_scored = integer(0),
    n_pending = integer(0),
    coverage_50 = numeric(0),
    coverage_90 = numeric(0),
    mean_wis = numeric(0),
    mean_ae_reported = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(scores) == 0) {
    return(empty)
  }
  groups <- unique(scores[c("level", "resolution")])
  groups <- groups[order(
    match(groups$level, episodic_archive_levels),
    match(groups$resolution, c("day", "week"))
  ), , drop = FALSE]
  rows <- lapply(seq_len(nrow(groups)), function(g) {
    in_group <- scores$level == groups$level[g] &
      scores$resolution == groups$resolution[g]
    scored <- scores[in_group & scores$final, , drop = FALSE]
    mean_or_na <- function(x) if (length(x) == 0) NA_real_ else mean(x)
    data.frame(
      level = groups$level[g],
      resolution = groups$resolution[g],
      n_scored = nrow(scored),
      n_pending = sum(in_group & !scores$final),
      coverage_50 = mean_or_na(scored$in_50),
      coverage_90 = mean_or_na(scored$in_90),
      mean_wis = mean_or_na(scored$wis),
      mean_ae_reported = mean_or_na(scored$ae_reported),
      stringsAsFactors = FALSE
    )
  })
  rbind(empty, do.call(rbind, rows))
}

#' Every stored outbreak-end probability, scored where its outcome is known
#'
#' Each computed forecast's probability for the date it was made is set
#' against what happened: the outbreak counts as over when no case of its
#' stream was sampled in the case-free window after the last case the
#' forecast knew of (`case_free_days` from then), which is the same window
#' after which EpiSODIC opens a new cluster rather than extending this
#' one. The outcome is known once that window has passed and its
#' reporting horizon with it (`reporting_horizon_days`, the longest lag
#' the completion curve measures); until then the forecast is pending,
#' neither scored nor dropped.
#'
#' The score is the Brier score, `(probability - over)^2`, lower is
#' better. Forecasts made on successive runs about the same outbreak are
#' each scored, so they are not independent of one another.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography The geography stream membership is decided under.
#' @return A data frame, one row per computed forecast: `cluster_forecast_id`,
#'   `run_id`, `cluster_id`, `pathogen`, `probability`, `final`
#'   (logical), and, `NA` unless `final`, `over` (logical) and `brier`.
#' @keywords internal
#' @noRd
episodic_outbreak_end_scores <- function(con, geography = episodic_geography_config()) {
  forecasts <- episodic_db_get_query(
    con,
    "SELECT f.cluster_forecast_id, f.run_id, f.cluster_id, f.params,
            c.stream_id, s.pathogen, v.probability
       FROM episodic_cluster_forecast f
       JOIN episodic_cluster c ON c.cluster_id = f.cluster_id
       JOIN episodic_stream s ON s.stream_id = c.stream_id
       JOIN (
         SELECT cluster_forecast_id, MIN(target_date) AS first_date
           FROM episodic_cluster_forecast_value
          GROUP BY cluster_forecast_id
       ) first ON first.cluster_forecast_id = f.cluster_forecast_id
       JOIN episodic_cluster_forecast_value v
         ON v.cluster_forecast_id = f.cluster_forecast_id
        AND v.target_date = first.first_date
      WHERE f.kind = 'outbreak_end' AND f.status = 'computed'
      ORDER BY f.cluster_forecast_id"
  )
  out <- data.frame(
    cluster_forecast_id = forecasts$cluster_forecast_id,
    run_id = forecasts$run_id,
    cluster_id = forecasts$cluster_id,
    pathogen = forecasts$pathogen,
    probability = forecasts$probability,
    final = logical(nrow(forecasts)),
    over = rep(NA, nrow(forecasts)),
    brier = rep(NA_real_, nrow(forecasts)),
    stringsAsFactors = FALSE
  )
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  if (nrow(forecasts) == 0 || is.null(run)) {
    return(out)
  }
  asof <- as.Date(substr(run$run_date, 1, 10))
  params <- lapply(forecasts$params, jsonlite::fromJSON, simplifyVector = TRUE)
  last_case <- as.Date(vapply(params, function(p) p$last_case_date, character(1)))
  window_end <- last_case +
    vapply(params, function(p) as.integer(p$case_free_days), integer(1))
  horizon <- vapply(params, function(p) as.integer(p$reporting_horizon_days), integer(1))
  out$final <- window_end + horizon <= asof

  for (stream_id in unique(forecasts$stream_id[out$final])) {
    rows <- which(out$final & forecasts$stream_id == stream_id)
    dates <- as.Date(episodic_db_cases_for_stream_id(
      con,
      stream_id,
      columns = "sample_date",
      first_day = format(min(last_case[rows]) + 1L),
      last_day = format(max(window_end[rows])),
      geography = geography
    )$sample_date)
    out$over[rows] <- vapply(
      rows,
      function(i) !any(dates > last_case[i] & dates <= window_end[i]),
      logical(1)
    )
  }
  scored <- out$final
  out$brier[scored] <- (out$probability[scored] - as.numeric(out$over[scored]))^2
  out
}

#' How well the outbreak-end probabilities have done, per pathogen
#'
#' The Performance screen's outbreak-end section: for each pathogen with
#' a stored probability, how many are scored and pending, the mean
#' probability given, the share of those outbreaks that were in fact over,
#' and the mean Brier score. A mean probability close to the share over
#' says the probabilities are calibrated on average. Every rate has the
#' scored forecasts as its denominator, so a pathogen with none scored
#' has `NA` for each.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography See `episodic_outbreak_end_scores()`.
#' @return A data frame with `pathogen`, `n_scored`, `n_pending`,
#'   `mean_probability`, `share_over` and `mean_brier`, by pathogen; zero
#'   rows when nothing has been forecast.
#' @keywords internal
#' @noRd
episodic_outbreak_end_performance <- function(con, geography = episodic_geography_config()) {
  scores <- episodic_outbreak_end_scores(con, geography = geography)
  empty <- data.frame(
    pathogen = character(0),
    n_scored = integer(0),
    n_pending = integer(0),
    mean_probability = numeric(0),
    share_over = numeric(0),
    mean_brier = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(scores) == 0) {
    return(empty)
  }
  mean_or_na <- function(x) if (length(x) == 0) NA_real_ else mean(x)
  rows <- lapply(sort(unique(scores$pathogen)), function(pathogen) {
    of <- scores$pathogen == pathogen
    scored <- scores[of & scores$final, , drop = FALSE]
    data.frame(
      pathogen = pathogen,
      n_scored = nrow(scored),
      n_pending = sum(of & !scores$final),
      mean_probability = mean_or_na(scored$probability),
      share_over = mean_or_na(scored$over),
      mean_brier = mean_or_na(scored$brier),
      stringsAsFactors = FALSE
    )
  })
  rbind(empty, do.call(rbind, rows))
}

#' Every stored epidemic direction, scored where its outcome is known
#'
#' The direction's probabilities are about the weeks it was estimated
#' over, some of them still being reported when it was made. Once the
#' last of those weeks has passed its reporting horizon
#' (`max_delay_days`), the same growth rate is fitted to their counts as
#' now reported (`episodic_growth_rate_fit()`), and each probability is
#' set against what that fit says: the epidemic was growing when its rate
#' is above zero, and past its peak when its rate is below zero and the
#' window's last week is below the highest week since the epidemic's
#' first. Until then a forecast is pending, neither scored nor dropped;
#' one whose counts now give no finite rate has no outcome and is not
#' scored.
#'
#' The score is the Brier score, lower is better. The outcome is itself
#' an estimate from the final counts, so the score measures what the
#' nowcast weeks and the sampling added, not a prediction of the future.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography The geography stream membership is decided under.
#' @return A data frame, one row per computed forecast and quantity
#'   (`"p_growing"`, `"p_past_peak"`): `cluster_forecast_id`, `run_id`,
#'   `cluster_id`, `pathogen`, `quantity`, `probability`, `final`
#'   (logical), and, `NA` unless an outcome is known, `outcome` (logical)
#'   and `brier`.
#' @keywords internal
#' @noRd
episodic_epidemic_direction_scores <- function(con, geography = episodic_geography_config()) {
  forecasts <- episodic_db_get_query(
    con,
    "SELECT f.cluster_forecast_id, f.run_id, f.cluster_id, f.params,
            c.stream_id, s.pathogen, e.quantity, e.estimate
       FROM episodic_cluster_forecast f
       JOIN episodic_cluster c ON c.cluster_id = f.cluster_id
       JOIN episodic_stream s ON s.stream_id = c.stream_id
       JOIN episodic_cluster_forecast_estimate e
         ON e.cluster_forecast_id = f.cluster_forecast_id
      WHERE f.kind = 'epidemic_direction' AND f.status = 'computed'
        AND e.quantity IN ('p_growing', 'p_past_peak')
      ORDER BY f.cluster_forecast_id, e.quantity"
  )
  out <- data.frame(
    cluster_forecast_id = forecasts$cluster_forecast_id,
    run_id = forecasts$run_id,
    cluster_id = forecasts$cluster_id,
    pathogen = forecasts$pathogen,
    quantity = forecasts$quantity,
    probability = forecasts$estimate,
    final = logical(nrow(forecasts)),
    outcome = rep(NA, nrow(forecasts)),
    brier = rep(NA_real_, nrow(forecasts)),
    stringsAsFactors = FALSE
  )
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  if (nrow(forecasts) == 0 || is.null(run)) {
    return(out)
  }
  asof <- as.Date(substr(run$run_date, 1, 10))
  ids <- unique(forecasts$cluster_forecast_id)
  params <- lapply(
    forecasts$params[match(ids, forecasts$cluster_forecast_id)],
    jsonlite::fromJSON,
    simplifyVector = TRUE
  )
  window_end <- as.Date(vapply(params, function(p) p$window_end, character(1)))
  horizon <- vapply(params, function(p) as.integer(p$max_delay_days), integer(1))
  final <- window_end + 6L + horizon <= asof
  out$final <- final[match(out$cluster_forecast_id, ids)]

  for (stream_id in unique(forecasts$stream_id[out$final])) {
    stream_ids <- ids[final & ids %in% forecasts$cluster_forecast_id[forecasts$stream_id == stream_id]]
    of <- match(stream_ids, ids)
    earliest <- min(as.Date(vapply(params[of], function(p) {
      min(p$window_start, p$first_week)
    }, character(1))))
    dates <- as.Date(episodic_db_cases_for_stream_id(
      con,
      stream_id,
      columns = "sample_date",
      first_day = format(earliest),
      last_day = format(max(window_end[of]) + 6L),
      geography = geography
    )$sample_date)
    for (j in of) {
      p <- params[[j]]
      weeks_of <- function(from, to) {
        weeks <- seq(as.Date(from), as.Date(to), by = 7)
        vapply(weeks, function(w) sum(dates >= w & dates <= w + 6L), integer(1))
      }
      fit <- episodic_growth_rate_fit(weeks_of(p$window_start, p$window_end))
      if (is.null(fit)) {
        next
      }
      rows <- which(out$cluster_forecast_id == ids[j])
      for (row in rows) {
        out$outcome[row] <- if (identical(out$quantity[row], "p_growing")) {
          fit[["rate"]] > 0
        } else {
          since <- weeks_of(p$first_week, p$window_end)
          fit[["rate"]] < 0 && since[length(since)] < max(since)
        }
      }
    }
  }
  known <- !is.na(out$outcome)
  out$brier[known] <- (out$probability[known] - as.numeric(out$outcome[known]))^2
  out
}

#' How well the epidemic directions have done, per pathogen
#'
#' The Performance screen's epidemic-direction section: for each pathogen
#' and each probability (`"p_growing"`, `"p_past_peak"`), how many are
#' scored and pending, the mean probability given, the share that turned
#' out true, and the mean Brier score. Every rate has the scored
#' forecasts as its denominator, so a row with none scored has `NA` for
#' each.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography See `episodic_epidemic_direction_scores()`.
#' @return A data frame with `pathogen`, `quantity`, `n_scored`,
#'   `n_pending`, `mean_probability`, `share_true` and `mean_brier`; zero
#'   rows when nothing has been estimated.
#' @keywords internal
#' @noRd
episodic_epidemic_direction_performance <- function(con, geography = episodic_geography_config()) {
  scores <- episodic_epidemic_direction_scores(con, geography = geography)
  empty <- data.frame(
    pathogen = character(0),
    quantity = character(0),
    n_scored = integer(0),
    n_pending = integer(0),
    mean_probability = numeric(0),
    share_true = numeric(0),
    mean_brier = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(scores) == 0) {
    return(empty)
  }
  mean_or_na <- function(x) if (length(x) == 0) NA_real_ else mean(x)
  groups <- unique(scores[c("pathogen", "quantity")])
  groups <- groups[order(groups$pathogen, groups$quantity), , drop = FALSE]
  rows <- lapply(seq_len(nrow(groups)), function(g) {
    of <- scores$pathogen == groups$pathogen[g] & scores$quantity == groups$quantity[g]
    scored <- scores[of & !is.na(scores$outcome), , drop = FALSE]
    data.frame(
      pathogen = groups$pathogen[g],
      quantity = groups$quantity[g],
      n_scored = nrow(scored),
      n_pending = sum(of & !scores$final),
      mean_probability = mean_or_na(scored$probability),
      share_true = mean_or_na(scored$outcome),
      mean_brier = mean_or_na(scored$brier),
      stringsAsFactors = FALSE
    )
  })
  rbind(empty, do.call(rbind, rows))
}

#' Every stored epidemic outlook week, scored where its week is reported
#'
#' Each computed outlook's forecast of a week's count is set against that
#' count once the week has passed the reporting horizon the outlook was
#' made with (`max_delay_days`), read through the same membership rule
#' the runs use; until then it is pending, neither scored nor dropped.
#'
#' Besides its interval coverage and weighted interval score
#' (`episodic_forecast_wis()`), each is scored against the forecast that
#' expects no change: the last complete week's count as the outlook's
#' paths had it (`baseline_count`), with Poisson quantiles. The ratio of
#' the two scores says whether the model earns its complexity.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography The geography stream membership is decided under.
#' @param pathogen `NULL` for every pathogen, or the one pathogen to
#'   score, as the dossier's track record needs.
#' @return A data frame, one row per computed forecast and target week:
#'   `cluster_forecast_id`, `run_id`, `cluster_id`, `pathogen`,
#'   `horizon`, `target_date`, `final` (logical), and, `NA` unless
#'   `final`, `observed`, `in_50`, `in_90`, `wis` and `wis_baseline`.
#' @keywords internal
#' @noRd
episodic_epidemic_outlook_scores <- function(con,
                                             geography = episodic_geography_config(),
                                             pathogen = NULL) {
  forecasts <- episodic_db_get_query(
    con,
    paste(
      "SELECT f.cluster_forecast_id, f.run_id, f.cluster_id, f.params,
            c.stream_id, s.pathogen, w.target_date, w.horizon,
            w.q025, w.q05, w.q10, w.q25, w.q50, w.q75, w.q90, w.q95, w.q975
       FROM episodic_cluster_forecast f
       JOIN episodic_cluster c ON c.cluster_id = f.cluster_id
       JOIN episodic_stream s ON s.stream_id = c.stream_id
       JOIN episodic_cluster_forecast_week w
         ON w.cluster_forecast_id = f.cluster_forecast_id
      WHERE f.kind = 'epidemic_outlook' AND f.status = 'computed'",
      if (is.null(pathogen)) "" else "AND s.pathogen = ?",
      "ORDER BY f.cluster_forecast_id, w.target_date"
    ),
    params = if (is.null(pathogen)) NULL else list(pathogen)
  )
  n <- nrow(forecasts)
  out <- data.frame(
    cluster_forecast_id = forecasts$cluster_forecast_id,
    run_id = forecasts$run_id,
    cluster_id = forecasts$cluster_id,
    pathogen = forecasts$pathogen,
    horizon = as.integer(forecasts$horizon),
    target_date = as.Date(forecasts$target_date),
    final = logical(n),
    observed = rep(NA_integer_, n),
    in_50 = rep(NA, n),
    in_90 = rep(NA, n),
    wis = rep(NA_real_, n),
    wis_baseline = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  if (n == 0 || is.null(run)) {
    return(out)
  }
  asof <- as.Date(substr(run$run_date, 1, 10))
  ids <- unique(forecasts$cluster_forecast_id)
  params <- lapply(
    forecasts$params[match(ids, forecasts$cluster_forecast_id)],
    jsonlite::fromJSON,
    simplifyVector = TRUE
  )
  at <- match(forecasts$cluster_forecast_id, ids)
  horizon_days <- vapply(params, function(p) as.integer(p$max_delay_days), integer(1))[at]
  baseline <- vapply(params, function(p) as.numeric(p$baseline_count), numeric(1))[at]
  out$final <- out$target_date + 6L + horizon_days <= asof

  for (stream_id in unique(forecasts$stream_id[out$final])) {
    rows <- which(out$final & forecasts$stream_id == stream_id)
    dates <- as.Date(episodic_db_cases_for_stream_id(
      con,
      stream_id,
      columns = "sample_date",
      first_day = format(min(out$target_date[rows])),
      last_day = format(max(out$target_date[rows]) + 6L),
      geography = geography
    )$sample_date)
    out$observed[rows] <- vapply(
      out$target_date[rows],
      function(w) sum(dates >= w & dates <= w + 6L),
      integer(1)
    )
  }
  scored <- which(out$final)
  if (length(scored) > 0) {
    y <- out$observed[scored]
    quantiles <- forecasts[scored, names(episodic_forecast_probs), drop = FALSE]
    out$in_50[scored] <- y >= quantiles$q25 & y <= quantiles$q75
    out$in_90[scored] <- y >= quantiles$q05 & y <= quantiles$q95
    out$wis[scored] <- episodic_forecast_wis(quantiles, y)
    persistence <- as.data.frame(lapply(
      episodic_forecast_probs,
      function(p) stats::qpois(p, baseline[scored])
    ))
    out$wis_baseline[scored] <- episodic_forecast_wis(persistence, y)
  }
  out
}

#' How well the epidemic outlooks have done, per pathogen and horizon
#'
#' The Performance screen's outlook section, and the track record the
#' dossier requires before it shows a pathogen's outlook: how many target
#' weeks are scored and pending, the share inside the 50% and 90%
#' intervals, the mean weighted interval score, and that score relative
#' to the no-change forecast (below 1: the model does better). Every rate
#' has the scored weeks as its denominator, so a row with none scored has
#' `NA` for each, and so does the relative score when the no-change
#' forecast scored zero throughout.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param geography,pathogen See `episodic_epidemic_outlook_scores()`.
#' @return A data frame with `pathogen`, `horizon`, `n_scored`,
#'   `n_pending`, `coverage_50`, `coverage_90`, `mean_wis` and
#'   `relative_wis`; zero rows when nothing has been forecast.
#' @keywords internal
#' @noRd
episodic_epidemic_outlook_performance <- function(con,
                                                  geography = episodic_geography_config(),
                                                  pathogen = NULL) {
  scores <- episodic_epidemic_outlook_scores(con, geography = geography, pathogen = pathogen)
  empty <- data.frame(
    pathogen = character(0),
    horizon = integer(0),
    n_scored = integer(0),
    n_pending = integer(0),
    coverage_50 = numeric(0),
    coverage_90 = numeric(0),
    mean_wis = numeric(0),
    relative_wis = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(scores) == 0) {
    return(empty)
  }
  mean_or_na <- function(x) if (length(x) == 0) NA_real_ else mean(x)
  groups <- unique(scores[c("pathogen", "horizon")])
  groups <- groups[order(groups$pathogen, groups$horizon), , drop = FALSE]
  rows <- lapply(seq_len(nrow(groups)), function(g) {
    of <- scores$pathogen == groups$pathogen[g] & scores$horizon == groups$horizon[g]
    scored <- scores[of & scores$final, , drop = FALSE]
    baseline <- sum(scored$wis_baseline)
    data.frame(
      pathogen = groups$pathogen[g],
      horizon = groups$horizon[g],
      n_scored = nrow(scored),
      n_pending = sum(of & !scores$final),
      coverage_50 = mean_or_na(scored$in_50),
      coverage_90 = mean_or_na(scored$in_90),
      mean_wis = mean_or_na(scored$wis),
      relative_wis = if (nrow(scored) == 0 || baseline == 0) {
        NA_real_
      } else {
        sum(scored$wis) / baseline
      },
      stringsAsFactors = FALSE
    )
  })
  rbind(empty, do.call(rbind, rows))
}
