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


# A stream's case dates with `weekly` cases in each week from `from`,
# spread over the days of the week.
direction_case_dates <- function(from, weekly) {
  from <- as.Date(from)
  days <- lapply(seq_along(weekly), function(i) {
    as.integer(from) + 7L * (i - 1L) + (seq_len(weekly[i]) - 1L) %% 7L
  })
  as.Date(unlist(days), origin = "1970-01-01")
}

direction_settings <- function(...) {
  settings <- episodic_epidemic_direction_settings(episodic_config_resolve(NA))
  utils::modifyList(settings, list(...))
}

test_that("the epidemic-direction settings are read and checked", {
  settings <- episodic_epidemic_direction_settings(episodic_config_resolve(NA))
  expect_true(settings$enabled)
  expect_identical(settings$window_weeks, 4L)
  config <- episodic_config_resolve(NA)
  config$forecast$epidemic_direction$window_weeks <- 2
  expect_error(episodic_epidemic_direction_settings(config), "window_weeks.*at least 3")
  config$forecast$epidemic_direction$window_weeks <- 4
  config$forecast$epidemic_direction$enabled <- "yes"
  expect_error(episodic_epidemic_direction_settings(config), "true or false")
  # A hand-built configuration without the section has the shipped one.
  expect_identical(episodic_epidemic_direction_settings(list())$n_samples, 1000L)
})

test_that("the growth rate is the log-linear slope, with Poisson noise as its floor", {
  fit <- episodic_growth_rate_fit(c(10, 20, 40, 80))
  expect_equal(fit[["rate"]], log(2), tolerance = 1e-6)
  poisson <- stats::glm(y ~ t, family = stats::poisson(), data = data.frame(y = c(10, 20, 40, 80), t = 0:3))
  # Counts less dispersed than Poisson keep Poisson's standard error.
  expect_equal(fit[["se"]], sqrt(stats::vcov(poisson)[2, 2]), tolerance = 1e-6)

  noisy <- c(10, 40, 12, 60)
  quasi <- stats::glm(y ~ t, family = stats::quasipoisson(), data = data.frame(y = noisy, t = 0:3))
  expect_equal(episodic_growth_rate_fit(noisy)[["se"]], sqrt(stats::vcov(quasi)[2, 2]), tolerance = 1e-6)

  # Every case in one week has no finite slope.
  expect_null(episodic_growth_rate_fit(c(0, 0, 0, 25)))
  expect_null(episodic_growth_rate_fit(c(0, 0, 0, 0)))
})

test_that("a doubling epidemic is growing, and a halving one is past its peak", {
  asof <- as.Date("2025-03-09") # a Sunday: its week has elapsed
  from <- as.Date("2025-01-06")
  growing <- episodic_epidemic_direction_forecast(
    direction_case_dates(from, c(5, 10, 20, 40, 80, 160, 320, 640, 1280)),
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 0,
    settings = direction_settings(),
    seed = 1
  )
  expect_identical(growing$status, "computed")
  estimate <- function(result, quantity) {
    result$estimates$estimate[result$estimates$quantity == quantity]
  }
  expect_equal(estimate(growing, "growth_rate_week"), log(2), tolerance = 0.02)
  expect_equal(estimate(growing, "p_growing"), 1)
  expect_equal(estimate(growing, "p_past_peak"), 0)
  expect_identical(growing$params$window_start, "2025-02-10")
  expect_identical(growing$params$window_end, "2025-03-03")
  expect_identical(growing$params$weeks_behind, 0L)
  rate <- growing$estimates[growing$estimates$quantity == "growth_rate_week", ]
  expect_true(rate$lower < rate$estimate && rate$estimate < rate$upper)
  expect_identical(rate$interval_level, 0.9)

  falling <- episodic_epidemic_direction_forecast(
    direction_case_dates(from, c(20, 80, 320, 640, 320, 160, 80, 40, 20)),
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 0,
    settings = direction_settings(),
    seed = 1
  )
  expect_equal(estimate(falling, "growth_rate_week"), -log(2), tolerance = 0.02)
  expect_equal(estimate(falling, "p_growing"), 0)
  expect_equal(estimate(falling, "p_past_peak"), 1)
})

test_that("weeks still being reported come from the nowcast, and without one the window ends before them", {
  asof <- as.Date("2025-03-09")
  from <- as.Date("2025-01-06")
  # The last two weeks are within the reporting horizon; what has been
  # reported of them is far below their final count.
  dates <- direction_case_dates(from, c(50, 50, 50, 50, 50, 50, 50, 5, 1))
  settings <- direction_settings(n_samples = 200)
  without <- episodic_epidemic_direction_forecast(
    dates,
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 14,
    settings = settings,
    seed = 1
  )
  # Never read as a collapse: the incomplete weeks are left out.
  expect_identical(without$params$window_end, "2025-02-17")
  expect_identical(without$params$weeks_behind, 2L)
  expect_lt(abs(without$estimates$estimate[1]), 0.1)

  # A nowcast that puts both weeks at their final 50 restores them.
  at_50 <- c(rep(0, 50), 1)
  with <- episodic_epidemic_direction_forecast(
    dates,
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list("2025-02-24" = at_50, "2025-03-03" = at_50),
    max_delay_days = 14,
    settings = settings,
    seed = 1
  )
  expect_identical(with$params$window_end, "2025-03-03")
  expect_identical(with$params$weeks_behind, 0L)
  expect_identical(with$params$n_nowcast_weeks, 2L)
  expect_lt(abs(with$estimates$estimate[1]), 0.1)

  # Nowcast uncertainty widens the interval.
  spread <- stats::dpois(0:200, 50)
  wide <- episodic_epidemic_direction_forecast(
    dates,
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list("2025-02-24" = spread, "2025-03-03" = spread),
    max_delay_days = 14,
    settings = settings,
    seed = 1
  )
  width <- function(r) r$estimates$upper[1] - r$estimates$lower[1]
  expect_gt(width(wide), width(with))
})

test_that("a young epidemic without a nowcast still gets a full window, from before its first week", {
  asof <- as.Date("2025-03-09")
  from <- as.Date("2025-01-06")
  result <- episodic_epidemic_direction_forecast(
    direction_case_dates(from, c(10, 12, 11, 13, 12, 14, 30, 40, 50)),
    observed_from = from,
    first_week = as.Date("2025-03-03"),
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 14,
    settings = direction_settings(),
    seed = 1
  )
  expect_identical(result$status, "computed")
  expect_identical(result$params$window_start, "2025-01-27")
  expect_identical(result$params$window_end, "2025-02-17")
  # Its first week is after the window, so there is no peak to be past.
  expect_false("p_past_peak" %in% result$estimates$quantity)
})

test_that("weeks before the case data begins are not counted as weeks without cases", {
  asof <- as.Date("2025-03-09")
  dates <- direction_case_dates("2025-02-10", c(30, 30, 30, 30))
  result <- episodic_epidemic_direction_forecast(
    dates,
    observed_from = as.Date("2025-02-12"),
    first_week = as.Date("2025-02-10"),
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 0,
    settings = direction_settings(),
    seed = 1
  )
  # From the first week the data covers whole, which leaves three.
  expect_identical(result$status, "insufficient_data")
  expect_identical(result$detail, "weeks")

  three <- episodic_epidemic_direction_forecast(
    dates,
    observed_from = as.Date("2025-02-12"),
    first_week = as.Date("2025-02-10"),
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 0,
    settings = direction_settings(window_weeks = 3L),
    seed = 1
  )
  expect_identical(three$params$window_start, "2025-02-17")
  expect_lt(abs(three$estimates$estimate[1]), 0.1)
  # The epidemic's first week is not observed whole, so whether it is
  # past its peak is not estimated rather than estimated from part of it.
  expect_false("p_past_peak" %in% three$estimates$quantity)
})

test_that("too few cases or counts without a slope give a reason, not an estimate", {
  asof <- as.Date("2025-03-09")
  from <- as.Date("2025-01-06")
  few <- episodic_epidemic_direction_forecast(
    direction_case_dates(from, c(1, 2, 1, 3, 2, 1, 2, 3, 2)),
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 0,
    settings = direction_settings(),
    seed = 1
  )
  expect_identical(few$detail, "cases")
  expect_identical(nrow(few$estimates), 0L)

  one_week <- episodic_epidemic_direction_forecast(
    direction_case_dates(from, c(0, 0, 0, 0, 0, 0, 0, 0, 40)),
    observed_from = from,
    first_week = from,
    asof = asof,
    week_pmfs = list(),
    max_delay_days = 0,
    settings = direction_settings(),
    seed = 1
  )
  expect_identical(one_week$detail, "model")
})

test_that("the direction is reproducible and leaves the session's random stream alone", {
  asof <- as.Date("2025-03-09")
  from <- as.Date("2025-01-06")
  dates <- direction_case_dates(from, c(50, 50, 50, 50, 50, 50, 50, 5, 1))
  run <- function() {
    episodic_epidemic_direction_forecast(
      dates,
      observed_from = from,
      first_week = from,
      asof = asof,
      week_pmfs = list("2025-02-24" = stats::dpois(0:200, 50), "2025-03-03" = stats::dpois(0:200, 50)),
      max_delay_days = 14,
      settings = direction_settings(n_samples = 200),
      seed = 7
    )
  }
  set.seed(99)
  first <- run()
  after <- stats::runif(1)
  set.seed(99)
  expect_identical(run(), first)
  expect_identical(stats::runif(1), after)
})

test_that("a cluster forecast refuses a quantity its kind does not estimate", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(
    episodic_db_cluster_forecast_insert(
      con,
      run_id = 1L,
      cluster_id = 1L,
      kind = "epidemic_direction",
      method = "loglinear_quasipoisson",
      status = "computed",
      detail = NA,
      params = list(),
      estimates = data.frame(quantity = "doubling_time", estimate = 1, lower = NA, upper = NA, interval_level = NA)
    ),
    "does not estimate \"doubling_time\""
  )
})

test_that("a run estimates the direction of each open epidemic, keeps it per run, and scores it once reported", {
  db <- nowcast_cron_database(direction_cron_config(), n_runs = 28L)
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  open <- episodic_db_clusters_not_closed(con, "epidemic")
  expect_gt(nrow(open), 0)
  last_run <- episodic_db_latest_run(con)
  forecasts <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster_forecast WHERE run_id = ? AND kind = 'epidemic_direction'",
    params = list(last_run$run_id)
  )
  expect_setequal(forecasts$cluster_id, open$cluster_id)
  expect_true(all(forecasts$status == "computed"))
  # Earlier runs had too few observed weeks, and said so.
  early <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT detail FROM episodic_cluster_forecast
      WHERE kind = 'epidemic_direction' AND status = 'insufficient_data'"
  )
  expect_identical(early$detail, "weeks")

  latest <- episodic_db_cluster_forecast_latest(con, open$cluster_id[1], "epidemic_direction")
  expect_setequal(latest$estimates$quantity, c("growth_rate_week", "p_growing"))
  obj <- episodic_epidemic_object(con, open$cluster_id[1], lang = "en")
  expect_identical(obj$direction$run_id, last_run$run_id)
  html <- as.character(episodic_ui_epidemic_stat_grid(obj, lang = "en"))
  expect_match(html, "Weekly growth", fixed = TRUE)
  expect_match(html, "chance it is growing", fixed = TRUE)
  expect_true(any(grepl("Epidemic direction: 1 of 1 open epidemic", db$log, fixed = TRUE)))

  scores <- episodic_epidemic_direction_scores(con)
  expect_setequal(unique(scores$quantity), "p_growing")
  expect_true(any(scores$final))
  known <- !is.na(scores$outcome)
  expect_equal(scores$brier[known], (scores$probability[known] - scores$outcome[known])^2)
  # The outcome is the sign of the rate refitted to the window's counts
  # as now reported.
  first <- scores[which(known)[1], ]
  params <- jsonlite::fromJSON(DBI::dbGetQuery(
    con,
    "SELECT params FROM episodic_cluster_forecast WHERE cluster_forecast_id = ?",
    params = list(first$cluster_forecast_id)
  )$params)
  dates <- as.Date(DBI::dbGetQuery(con, "SELECT sample_date FROM episodic_case")$sample_date)
  weeks <- seq(as.Date(params$window_start), as.Date(params$window_end), by = 7)
  counts <- vapply(weeks, function(w) sum(dates >= w & dates <= w + 6), integer(1))
  expect_identical(first$outcome, episodic_growth_rate_fit(counts)[["rate"]] > 0)

  performance <- episodic_epidemic_direction_performance(con)
  expect_identical(performance$quantity, "p_growing")
  expect_identical(performance$n_scored, sum(known))
  expect_identical(performance$n_pending, sum(!scores$final))
  expect_equal(performance$mean_brier, mean(scores$brier[known]))

  # The latest run's forecast, as though its window had ended in the last
  # week and was still being reported: pending, not scored.
  run <- episodic_db_latest_run(con)
  cluster_id <- open$cluster_id[1]
  DBI::dbExecute(
    con,
    "DELETE FROM episodic_cluster_forecast_estimate WHERE cluster_forecast_id IN
       (SELECT cluster_forecast_id FROM episodic_cluster_forecast
         WHERE run_id = ? AND cluster_id = ? AND kind = 'epidemic_direction')",
    params = list(run$run_id, cluster_id)
  )
  DBI::dbExecute(
    con,
    "DELETE FROM episodic_cluster_forecast
      WHERE run_id = ? AND cluster_id = ? AND kind = 'epidemic_direction'",
    params = list(run$run_id, cluster_id)
  )
  pending_id <- episodic_db_cluster_forecast_insert(
    con,
    run_id = run$run_id,
    cluster_id = cluster_id,
    kind = "epidemic_direction",
    method = "loglinear_quasipoisson",
    status = "computed",
    detail = NA,
    params = list(
      window_start = "2025-01-06", window_end = "2025-01-20",
      first_week = "2024-12-30", max_delay_days = 21L
    ),
    estimates = data.frame(quantity = "p_growing", estimate = 0.7, lower = NA, upper = NA, interval_level = NA)
  )
  pending <- episodic_epidemic_direction_scores(con)
  pending <- pending[pending$cluster_forecast_id == pending_id, ]
  expect_false(pending$final)
  expect_true(is.na(pending$outcome))
  expect_true(is.na(pending$brier))
  expect_identical(episodic_epidemic_direction_performance(con)$n_pending, 1L)
})

test_that("switched off, no run estimates a direction", {
  db <- nowcast_cron_database(
    c("  epidemic_direction:", "    enabled: false", direction_cron_config()[-(1:4)]),
    n_runs = 3L
  )
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_cluster_forecast WHERE kind = 'epidemic_direction'")$n,
    0L
  )
  expect_true(any(grepl("Epidemic direction switched off", db$log, fixed = TRUE)))
})

test_that("the dossier shows the direction as growth and chance past the peak, or why there is none", {
  direction <- list(
    status = "computed",
    detail = NA,
    params = list(window_start = "2025-02-10", window_end = "2025-03-03"),
    estimates = data.frame(
      quantity = c("growth_rate_week", "p_growing", "p_past_peak"),
      estimate = c(log(2), 0.999, 0.004),
      lower = c(0.5, NA, NA),
      upper = c(0.9, NA, NA),
      interval_level = c(0.9, NA, NA)
    )
  )
  html <- paste(vapply(
    episodic_ui_epidemic_direction_stats(direction, lang = "en"),
    as.character,
    character(1)
  ), collapse = "")
  expect_match(html, "Weekly growth", fixed = TRUE)
  expect_match(html, "trend over 10 Feb - 9 Mar 2025", fixed = TRUE)
  expect_match(html, "+100%", fixed = TRUE)
  expect_match(html, "doubling about every 7 days; &gt; 99% chance it is growing", fixed = TRUE)
  expect_match(html, "Past its peak", fixed = TRUE)
  expect_match(html, "&lt; 1%", fixed = TRUE)

  unclear <- direction
  unclear$estimates$estimate[1] <- 0.05
  unclear$estimates$lower[1] <- -0.1
  unclear$estimates$upper[1] <- 0.2
  unclear$estimates$estimate[2] <- 0.7
  unclear$estimates <- unclear$estimates[1:2, ]
  tiles <- episodic_ui_epidemic_direction_stats(unclear, lang = "en")
  expect_length(tiles, 1)
  expect_match(
    as.character(tiles[[1]]),
    "no clear direction (-10% to +22%); 70% chance it is growing",
    fixed = TRUE
  )

  withheld <- list(status = "insufficient_data", detail = "weeks", params = list(), estimates = NULL)
  tiles <- episodic_ui_epidemic_direction_stats(withheld, lang = "en")
  expect_length(tiles, 1)
  expect_match(as.character(tiles[[1]]), "Too few weeks reported yet", fixed = TRUE)
  expect_length(episodic_ui_epidemic_direction_stats(NULL, lang = "en"), 0)

  # Every reason and every language has its text.
  for (lang in c("en", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    for (detail in c("weeks", "cases", "model")) {
      withheld$detail <- detail
      expect_no_error(episodic_ui_epidemic_direction_stats(withheld, lang = lang))
    }
    expect_no_error(episodic_ui_epidemic_direction_stats(direction, lang = lang))
  }
})

test_that("the typical past season is placed on the epidemic's own weeks, and says it is not a forecast", {
  season <- data.frame(
    onset_week_start = "2025-01-06",
    seasons_used = "2021/2022, 2022/2023, 2023/2024",
    typical_onset_week = 51L,
    typical_onset_shift_weeks = 2L,
    typical_length_weeks = 10,
    typical_length_lower = 8,
    typical_length_upper = 11.3,
    pre_epidemic_threshold = 5,
    post_epidemic_threshold = 4,
    intensity_medium = NA,
    intensity_high = NA,
    intensity_very_high = NA
  )
  weeks <- data.frame(week_offset = -1:1, lower = c(-0.5, 2, 5), middle = c(1, 4, 9), upper = c(2, 7, 14))
  typical <- episodic_epidemic_typical(season, weeks)
  expect_equal(typical$curve$week_start, as.Date(c("2024-12-30", "2025-01-06", "2025-01-13")))
  # A count cannot be below zero, and neither can its interval.
  expect_identical(typical$curve$lower[1], 0)
  expect_identical(typical$n_seasons, 3L)

  html <- as.character(episodic_ui_epidemic_typical_stat(typical, lang = "en"))
  expect_match(html, "Typical epidemic", fixed = TRUE)
  expect_match(html, "in 3 past seasons, not a forecast", fixed = TRUE)
  expect_match(html, "10 weeks", fixed = TRUE)
  expect_match(html, "usually 8 to 12 weeks; this one started 2 weeks later", fixed = TRUE)
  typical$onset_shift_weeks <- -1L
  expect_match(
    as.character(episodic_ui_epidemic_typical_stat(typical, lang = "en")),
    "this one started 1 week earlier",
    fixed = TRUE
  )

  # No typical season stored: nothing, never a curve of zeros.
  expect_null(episodic_epidemic_typical(season, weeks[0, ]))
  season$typical_length_weeks <- NA
  expect_null(episodic_epidemic_typical(season, weeks))
  expect_null(episodic_epidemic_typical(NULL, weeks))
  expect_null(episodic_ui_epidemic_typical_stat(NULL, lang = "en"))
})

test_that("the weekly curve draws the typical season behind its bars only when it has one", {
  weekly <- data.frame(
    week_start = as.Date("2025-01-06") + 7 * 0:3,
    n_cases = c(3, 8, 12, 9),
    incomplete = FALSE
  )
  plain <- episodic_ui_pathogen_curve_chart(weekly, lang = "en")
  weekly$typical_low <- c(NA, 1, 3, 2)
  weekly$typical_mid <- c(NA, 4, 8, 6)
  weekly$typical_high <- c(NA, 7, 14, 10)
  with <- episodic_ui_pathogen_curve_chart(weekly, lang = "en")
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomRibbon" %in% geoms(plain))
  expect_identical(unname(head(geoms(with), 3)), c("GeomRibbon", "GeomLine", "GeomCol"))
})

test_that("the Performance screen lists the direction's scores, or says there are none", {
  direction <- data.frame(
    pathogen = "Influenza A virus",
    quantity = c("p_growing", "p_past_peak"),
    n_scored = c(4L, 0L),
    n_pending = c(1L, 2L),
    mean_probability = c(0.6, NA),
    share_true = c(0.5, NA),
    mean_brier = c(0.12, NA)
  )
  html <- as.character(episodic_ui_performance_direction(direction, lang = "en"))
  expect_match(html, "Epidemic direction", fixed = TRUE)
  expect_match(html, "Growing", fixed = TRUE)
  expect_match(html, "Past its peak", fixed = TRUE)
  expect_match(html, "<td>0.12</td>", fixed = TRUE)
  expect_match(
    as.character(episodic_ui_performance_direction(direction[0, ], lang = "en")),
    "No epidemic direction has been stored yet.",
    fixed = TRUE
  )
})
