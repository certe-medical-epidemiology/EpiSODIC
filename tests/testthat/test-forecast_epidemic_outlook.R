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


# A seasonal stream's case dates, peaking around ISO week 5, from 2021 to
# `to`.
outlook_case_dates <- function(to = as.Date("2025-02-16"), seed = 3) {
  set.seed(seed)
  days <- seq(as.Date("2021-01-04"), as.Date(to), by = "day")
  week <- as.integer(format(days, "%V"))
  lambda <- 1 + 12 * exp(-((pmin(abs(week - 5), 52 - abs(week - 5)))^2) / 18)
  rep(days, stats::rpois(length(days), lambda))
}

outlook_settings <- function(...) {
  settings <- episodic_epidemic_outlook_settings(episodic_config_resolve(NA))
  utils::modifyList(settings, list(...))
}

outlook_thresholds <- list(
  pre_epidemic = 20,
  post_epidemic = 15,
  intensity = c(medium = 40, high = 70, very_high = 90)
)

test_that("the epidemic-outlook settings are read and checked", {
  settings <- episodic_epidemic_outlook_settings(episodic_config_resolve(NA))
  expect_true(settings$enabled)
  expect_identical(settings$horizon_weeks, 4L)
  expect_identical(settings$min_scored, 20L)
  config <- episodic_config_resolve(NA)
  config$forecast$epidemic_outlook$horizon_weeks <- 9
  expect_error(episodic_epidemic_outlook_settings(config), "horizon_weeks.*from 1 to 8")
  config$forecast$epidemic_outlook$horizon_weeks <- 4
  config$forecast$epidemic_outlook$min_history_weeks <- 10
  expect_error(episodic_epidemic_outlook_settings(config), "at least 26")
  expect_identical(episodic_epidemic_outlook_settings(list())$n_samples, 1000L)
})

test_that("a count reaches a band above the pre-epidemic threshold, and each higher one at its threshold", {
  reached <- episodic_outlook_band_reached(c(20, 21, 40, 69, 90), outlook_thresholds)
  expect_identical(reached$low, c(FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_identical(reached$medium, c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(reached$high, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(reached$very_high, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # Without intensity thresholds only the pre-epidemic one is known.
  only_pre <- episodic_outlook_band_reached(c(1, 30), list(pre_epidemic = 20, intensity = NULL))
  expect_identical(only_pre$low, c(FALSE, TRUE))
  expect_null(only_pre$high)
  expect_true(all(vapply(episodic_outlook_band_reached(1, NULL), is.null, logical(1))))
})

test_that("a seasonal epidemic's outlook gives each week ahead a distribution and the chance of each band", {
  result <- episodic_epidemic_outlook_forecast(
    outlook_case_dates(),
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-16"),
    week_pmfs = list(),
    max_delay_days = 21,
    thresholds = outlook_thresholds,
    settings = outlook_settings(),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(result$status, "computed")
  weeks <- result$weeks
  expect_identical(weeks$target_date, format(as.Date("2025-02-10") + 7 * 1:4))
  expect_identical(weeks$horizon, 1:4)
  quantiles <- as.matrix(weeks[names(episodic_forecast_probs)])
  expect_true(all(apply(quantiles, 1, function(q) !is.unsorted(q))))
  # A higher band is never likelier than a lower one, and reaching it in
  # any week is at least as likely as in the likeliest single week.
  bands <- as.matrix(weeks[paste0("p_", episodic_outlook_bands)])
  expect_true(all(apply(bands, 1, function(p) !is.unsorted(rev(p)))))
  reach <- result$estimates
  expect_identical(reach$quantity, paste0("p_reach_", episodic_outlook_bands))
  expect_true(all(reach$estimate >= apply(bands, 2, max)))
  expect_true(result$params$seasonal)
  expect_identical(result$params$last_final_week, "2025-01-20")
})

test_that("weeks still being reported come from the nowcast, or never below what is reported", {
  dates <- outlook_case_dates()
  # The nowcast says the last complete week will end at exactly 150.
  at_150 <- c(rep(0, 150), 1)
  nowcast <- episodic_epidemic_outlook_forecast(
    dates,
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-16"),
    week_pmfs = list("2025-02-10" = at_150),
    max_delay_days = 21,
    thresholds = NULL,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(nowcast$params$n_nowcast_weeks, 1L)
  expect_identical(nowcast$params$baseline_count, 150)

  # The week in progress already has cases reported: no path has fewer.
  partial <- c(dates, rep(as.Date("2025-02-17"), 500))
  floored <- episodic_epidemic_outlook_forecast(
    partial,
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-18"),
    week_pmfs = list(),
    max_delay_days = 21,
    thresholds = NULL,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_gte(floored$weeks$q025[1], 500)
})

test_that("a draw is never below its floor, and a floor beyond the model's reach is the draw", {
  set.seed(1)
  draws <- episodic_outlook_draw_at_least(rep(10, 500), rep(5, 500), 12)
  expect_true(all(draws >= 12))
  # Restricted to 12 and above, the draws follow the model's tail there.
  expect_equal(
    mean(draws == 12),
    stats::dnbinom(12, mu = 10, size = 5) / stats::pnbinom(11, mu = 10, size = 5, lower.tail = FALSE),
    tolerance = 0.05
  )
  expect_identical(episodic_outlook_draw_at_least(c(1, 1), c(5, 5), 1e6), c(1e6, 1e6))
  expect_true(all(is.finite(episodic_outlook_draw_at_least(rep(50, 100), rep(20, 100), 500))))
})

test_that("the ensemble averages the models' quantiles, and reads the band chances from them", {
  set.seed(1)
  low <- matrix(stats::rpois(2000, 10), ncol = 2)
  high <- matrix(stats::rpois(2000, 30), ncol = 2)
  alone <- episodic_outlook_combine(list(hhh4 = low), outlook_thresholds)
  expect_equal(alone$weeks$q50, apply(low, 2, stats::quantile, probs = 0.5, names = FALSE, type = 1))
  expect_equal(alone$weeks$mean, colMeans(low))
  # One model's band chance is the share of its paths in the band, to
  # the grid's thousandth.
  expect_equal(alone$weeks$p_low, colMeans(low > 20), tolerance = 0.002)
  both <- episodic_outlook_combine(list(hhh4 = low, trend = high), outlook_thresholds)
  for (q in names(episodic_forecast_probs)) {
    expect_equal(
      both$weeks[[q]],
      (apply(low, 2, stats::quantile, probs = episodic_forecast_probs[[q]], names = FALSE, type = 1) +
        apply(high, 2, stats::quantile, probs = episodic_forecast_probs[[q]], names = FALSE, type = 1)) / 2
    )
  }
  expect_equal(both$weeks$mean, (colMeans(low) + colMeans(high)) / 2)
  # The median of the averaged forecast lies between the two models'.
  expect_true(all(both$weeks$q50 > alone$weeks$q50))
  reach <- both$estimates$estimate[both$estimates$quantity == "p_reach_low"]
  expect_gte(reach, max(both$weeks$p_low))
})

test_that("the trend joins the outlook only when its window is data, up to the last complete week", {
  dates <- outlook_case_dates()
  # Everything fully reported: both models.
  both <- episodic_epidemic_outlook_forecast(
    dates,
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-16"),
    week_pmfs = list(),
    max_delay_days = 0,
    thresholds = outlook_thresholds,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(both$params$components, c("hhh4", "trend"))
  expect_identical(both$params$trend_window_end, "2025-02-10")
  # The last weeks still being reported without a nowcast: the history
  # model alone, rather than a trend fitted to its draws or extended from
  # weeks back.
  alone <- episodic_epidemic_outlook_forecast(
    dates,
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-16"),
    week_pmfs = list(),
    max_delay_days = 21,
    thresholds = outlook_thresholds,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(alone$params$components, "hhh4")
  # With a nowcast of those weeks, both again.
  pmfs <- stats::setNames(
    rep(list(stats::dpois(0:200, 60)), 3),
    format(as.Date("2025-01-27") + 7 * 0:2)
  )
  nowcast <- episodic_epidemic_outlook_forecast(
    dates,
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-16"),
    week_pmfs = pmfs,
    max_delay_days = 21,
    thresholds = outlook_thresholds,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(nowcast$params$components, c("hhh4", "trend"))
})

test_that("a trend that cannot be fitted is left out, and a fitted one respects what is reported", {
  one_week <- matrix(c(0, 0, 0, 25), nrow = 10, ncol = 4, byrow = TRUE)
  expect_null(episodic_outlook_trend_paths(one_week, ahead = 1:2, floors = c(0, 0)))
  set.seed(1)
  steady <- matrix(c(20, 22, 19, 21), nrow = 200, ncol = 4, byrow = TRUE)
  paths <- episodic_outlook_trend_paths(steady, ahead = 1:2, floors = c(40, 0))
  expect_true(all(paths[, 1] >= 40))
  expect_equal(stats::median(paths[, 2]), 21, tolerance = 0.3)
  # Extended further, the same trend is less certain.
  set.seed(1)
  near <- episodic_outlook_trend_paths(steady, ahead = 1, floors = 0)
  set.seed(1)
  far <- episodic_outlook_trend_paths(steady, ahead = 6, floors = 0)
  expect_gt(diff(stats::quantile(far, c(0.05, 0.95))), diff(stats::quantile(near, c(0.05, 0.95))))
})

test_that("an epidemic without thresholds has no band chances, and too little history gives a reason", {
  plain <- episodic_epidemic_outlook_forecast(
    outlook_case_dates(),
    observed_from = as.Date("2021-01-04"),
    asof = as.Date("2025-02-16"),
    week_pmfs = list(),
    max_delay_days = 21,
    thresholds = NULL,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(plain$status, "computed")
  expect_false(plain$params$seasonal)
  expect_true(all(is.na(plain$weeks[paste0("p_", episodic_outlook_bands)])))
  expect_identical(nrow(plain$estimates), 0L)

  short <- episodic_epidemic_outlook_forecast(
    outlook_case_dates(),
    observed_from = as.Date("2024-06-03"),
    asof = as.Date("2025-02-16"),
    week_pmfs = list(),
    max_delay_days = 21,
    thresholds = NULL,
    settings = outlook_settings(),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(short$status, "insufficient_data")
  expect_identical(short$detail, "history")
  expect_identical(nrow(short$weeks), 0L)
})

test_that("a series with no week-to-week dependence falls back to the endemic model", {
  set.seed(1)
  days <- seq(as.Date("2021-01-04"), as.Date("2025-02-16"), by = "day")
  dates <- rep(days, stats::rpois(length(days), 1.5))
  fit <- episodic_outlook_fit(
    tabulate(as.integer(dates - days[1]) %/% 7L + 1L),
    seasonal = FALSE,
    autoregressive = TRUE
  )
  result <- episodic_epidemic_outlook_forecast(
    dates,
    observed_from = days[1],
    asof = as.Date("2025-02-16"),
    week_pmfs = list(),
    max_delay_days = 21,
    thresholds = NULL,
    settings = outlook_settings(n_samples = 200),
    trend_window_weeks = 4L,
    seed = 1
  )
  expect_identical(result$status, "computed")
  expect_identical(
    result$params$autoregressive,
    fit$se[["ar.1"]] <= episodic_outlook_max_ar_se
  )
  # Whichever model, a stable series keeps its level.
  expect_equal(result$weeks$q50, rep(10, 4), tolerance = 0.3)
})

test_that("an outlook is reproducible and leaves the session's random stream alone", {
  run <- function() {
    episodic_epidemic_outlook_forecast(
      outlook_case_dates(),
      observed_from = as.Date("2021-01-04"),
      asof = as.Date("2025-02-16"),
      week_pmfs = list(),
      max_delay_days = 21,
      thresholds = outlook_thresholds,
      settings = outlook_settings(n_samples = 200),
      trend_window_weeks = 4L,
      seed = 5
    )
  }
  set.seed(99)
  first <- run()
  after <- stats::runif(1)
  set.seed(99)
  expect_identical(run(), first)
  expect_identical(stats::runif(1), after)
})

test_that("a run forecasts each open epidemic, stores it by week, and scores it once reported", {
  skip_on_cran()
  skip_if_not_installed("mem")
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2021-01-01"),
    end_date = as.Date("2025-02-16"),
    seed = 2
  )
  pathogen_config <- tempfile(fileext = ".csv")
  pc <- episodic_test_pathogen_config()
  pc$mem_mode <- "yes"
  utils::write.csv(pc, pathogen_config, row.names = FALSE)
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(path, pathogen_config)))
  log <- testthat::capture_messages(episodic_run_cron(
    db_path = path,
    cases = cases,
    pathogen_config_path = pathogen_config,
    run_date = as.Date("2025-02-16")
  ))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  open <- episodic_db_clusters_not_closed(con, "epidemic")
  open <- open[open$origin == "detected", , drop = FALSE]
  expect_gt(nrow(open), 0)
  forecasts <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster_forecast WHERE kind = 'epidemic_outlook'"
  )
  expect_setequal(forecasts$cluster_id, open$cluster_id)
  expect_true(any(forecasts$status == "computed"))
  expect_true(any(grepl("Epidemic outlook: ", log, fixed = TRUE)))

  season <- DBI::dbGetQuery(con, "SELECT cluster_id FROM episodic_epidemic_season")
  seasonal_id <- intersect(season$cluster_id, forecasts$cluster_id[forecasts$status == "computed"])[1]
  latest <- episodic_db_cluster_forecast_latest(con, seasonal_id, "epidemic_outlook")
  expect_identical(nrow(latest$weeks), 4L)
  expect_true(inherits(latest$weeks$target_date, "Date"))
  expect_false(anyNA(latest$weeks$p_low))
  expect_true(all(grepl("^p_reach_", latest$estimates$quantity)))

  # Not yet any week reported: pending, and the dossier withholds it.
  scores <- episodic_epidemic_outlook_scores(con)
  expect_false(any(scores$final))
  obj <- episodic_epidemic_object(con, seasonal_id, lang = "en")
  expect_false(obj$outlook$shown)
  expect_identical(obj$outlook$n_scored, 0L)
  html <- as.character(episodic_ui_epidemic_outlook_panel(obj, lang = "en"))
  expect_match(html, "Withheld until outlooks for this pathogen have been scored on 20 weeks; 0 so far", fixed = TRUE)

  # An outlook written as an earlier run would have, for weeks now fully
  # reported: scored against the counts, and against no change.
  run_id <- DBI::dbGetQuery(con, "SELECT MIN(run_id) AS id FROM episodic_detection_run")$id
  DBI::dbExecute(
    con,
    "DELETE FROM episodic_cluster_forecast_week WHERE cluster_forecast_id IN
       (SELECT cluster_forecast_id FROM episodic_cluster_forecast
         WHERE run_id = ? AND cluster_id = ? AND kind = 'epidemic_outlook')",
    params = list(run_id, seasonal_id)
  )
  DBI::dbExecute(
    con,
    "DELETE FROM episodic_cluster_forecast_estimate WHERE cluster_forecast_id IN
       (SELECT cluster_forecast_id FROM episodic_cluster_forecast
         WHERE run_id = ? AND cluster_id = ? AND kind = 'epidemic_outlook')",
    params = list(run_id, seasonal_id)
  )
  DBI::dbExecute(
    con,
    "DELETE FROM episodic_cluster_forecast WHERE run_id = ? AND cluster_id = ? AND kind = 'epidemic_outlook'",
    params = list(run_id, seasonal_id)
  )
  week <- data.frame(target_date = "2024-12-02", horizon = 1L, mean = 10)
  for (q in names(episodic_forecast_probs)) week[[q]] <- 10
  week[paste0("p_", episodic_outlook_bands)] <- NA_real_
  episodic_db_cluster_forecast_insert(
    con,
    run_id = run_id,
    cluster_id = seasonal_id,
    kind = "epidemic_outlook",
    method = "hhh4_negbin",
    status = "computed",
    detail = NA,
    params = list(max_delay_days = 21L, baseline_count = 3),
    weeks = week
  )
  scores <- episodic_epidemic_outlook_scores(con)
  scored <- scores[scores$final, ]
  expect_identical(nrow(scored), 1L)
  stream_id <- open$stream_id[open$cluster_id == seasonal_id]
  dates <- as.Date(episodic_db_cases_for_stream_id(con, stream_id, columns = "sample_date")$sample_date)
  observed <- sum(dates >= as.Date("2024-12-02") & dates <= as.Date("2024-12-08"))
  expect_identical(scored$observed, observed)
  expect_equal(scored$wis, abs(observed - 10))
  expect_equal(
    scored$wis_baseline,
    episodic_forecast_wis(as.data.frame(lapply(episodic_forecast_probs, stats::qpois, lambda = 3)), observed)
  )
  performance <- episodic_epidemic_outlook_performance(con)
  row <- performance[performance$n_scored > 0, ]
  expect_identical(row$horizon, 1L)
  expect_equal(row$relative_wis, scored$wis / scored$wis_baseline)
})

test_that("the dossier shows an outlook with a track record as a table, its note and its marks on the curve", {
  weeks <- data.frame(
    target_date = as.Date("2025-02-17") + 7 * 0:1,
    horizon = 1:2,
    mean = c(50, 40),
    q025 = c(20, 10), q05 = c(25, 12), q10 = c(30, 15), q25 = c(40, 25),
    q50 = c(50, 38), q75 = c(60, 50), q90 = c(70, 60), q95 = c(80, 70), q975 = c(90, 80),
    p_low = c(0.95, 0.8), p_medium = c(0.6, 0.4), p_high = c(0.2, 0.1), p_very_high = c(0.004, 0.001)
  )
  obj <- list(outlook = list(
    forecast = list(
      status = "computed",
      params = list(n_history_weeks = 212, components = c("hhh4", "trend"), trend_window_weeks = 4L),
      weeks = weeks,
      estimates = data.frame(
        quantity = paste0("p_reach_", episodic_outlook_bands),
        estimate = c(0.97, 0.7, 0.25, 0.005)
      )
    ),
    n_scored = 40L,
    coverage_90 = 0.875,
    relative_wis = 0.8,
    min_scored = 20L,
    min_history_weeks = 104L,
    shown = TRUE
  ))
  html <- as.character(episodic_ui_epidemic_outlook_panel(obj, lang = "en"))
  expect_match(html, "Outlook", fixed = TRUE)
  expect_match(html, "50 (25 to 80)", fixed = TRUE)
  expect_match(html, "High or above", fixed = TRUE)
  expect_match(html, "&lt; 1%", fixed = TRUE)
  expect_match(html, "Any of these weeks", fixed = TRUE)
  expect_match(html, "the average of two forecasts", fixed = TRUE)
  history_only <- obj
  history_only$outlook$forecast$params$components <- "hhh4"
  expect_match(
    as.character(episodic_ui_epidemic_outlook_panel(history_only, lang = "en")),
    "trend of the last weeks is left out",
    fixed = TRUE
  )
  expect_match(html, "scored on 40 weeks: 88% fell inside the 90% interval, and their interval score was 0.80 times", fixed = TRUE)

  withheld <- obj
  withheld$outlook$shown <- FALSE
  withheld$outlook$n_scored <- 7L
  expect_match(
    as.character(episodic_ui_epidemic_outlook_panel(withheld, lang = "en")),
    "scored on 20 weeks; 7 so far",
    fixed = TRUE
  )
  # A record that does not beat expecting no change withholds it too.
  worse <- obj
  worse$outlook$shown <- FALSE
  worse$outlook$relative_wis <- 1.25
  expect_match(
    as.character(episodic_ui_epidemic_outlook_panel(worse, lang = "en")),
    "over 40 scored weeks, outlooks for this pathogen did no better than expecting no change (relative interval score 1.25)",
    fixed = TRUE
  )
  insufficient <- obj
  insufficient$outlook$forecast <- list(status = "insufficient_data", detail = "history")
  expect_match(
    as.character(episodic_ui_epidemic_outlook_panel(insufficient, lang = "en")),
    "fewer than 104 fully reported weeks",
    fixed = TRUE
  )
  expect_null(episodic_ui_epidemic_outlook_panel(list(outlook = NULL), lang = "en"))
  for (lang in c("en", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    expect_no_error(episodic_ui_epidemic_outlook_panel(obj, lang = lang))
    expect_no_error(episodic_ui_epidemic_outlook_panel(worse, lang = lang))
    expect_no_error(episodic_ui_epidemic_outlook_panel(history_only, lang = lang))
    for (detail in c("history", "model")) {
      insufficient$outlook$forecast$detail <- detail
      expect_no_error(episodic_ui_epidemic_outlook_panel(insufficient, lang = lang))
    }
  }

  weekly <- data.frame(
    week_start = as.Date("2025-01-20") + 7 * 0:3,
    n_cases = c(30, 45, 60, 55),
    incomplete = FALSE
  )
  chart <- episodic_ui_pathogen_curve_chart(weekly, lang = "en", outlook = weeks)
  expect_identical(
    as.character(ggplot2::get_guide_data(chart, "shape")$.label),
    "Outlook (90% interval)"
  )
  expect_true("GeomLinerange" %in% vapply(chart$layers, function(l) class(l$geom)[1], character(1)))
})

test_that("the Performance screen lists the outlook's scores, or says there are none", {
  outlook <- data.frame(
    pathogen = "Influenza A virus",
    horizon = 1:2,
    n_scored = c(12L, 0L),
    n_pending = c(3L, 4L),
    coverage_50 = c(0.5, NA),
    coverage_90 = c(0.9, NA),
    mean_wis = c(3.2, NA),
    relative_wis = c(0.75, NA)
  )
  html <- as.character(episodic_ui_performance_outlook(outlook, lang = "en"))
  expect_match(html, "Epidemic outlook", fixed = TRUE)
  expect_match(html, "Score relative to no change", fixed = TRUE)
  expect_match(html, "<td>0.75</td>", fixed = TRUE)
  expect_match(
    as.character(episodic_ui_performance_outlook(outlook[0, ], lang = "en")),
    "No epidemic outlook has been stored yet.",
    fixed = TRUE
  )
})
