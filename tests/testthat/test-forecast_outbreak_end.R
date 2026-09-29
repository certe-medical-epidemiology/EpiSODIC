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

complete_reporting <- data.frame(lag_days = 0:21, completeness = 1)

test_that("the outbreak-end settings are read and checked", {
  settings <- episodic_outbreak_end_settings(episodic_test_config())
  expect_true(settings$enabled)
  expect_identical(settings$probability_threshold, 0.95)
  expect_identical(episodic_outbreak_end_settings(list()), settings)
  config <- episodic_test_config()
  config$forecast$outbreak_end$probability_threshold <- 1
  expect_error(episodic_outbreak_end_settings(config), "probability_threshold")
  config$forecast$outbreak_end$probability_threshold <- 0.9
  config$forecast$outbreak_end$enabled <- NA
  expect_error(episodic_outbreak_end_settings(config), "enabled")
})

test_that("each serial interval family has the mean it was given", {
  for (dist in c("gamma", "lognormal", "weibull")) {
    cdf <- episodic_serial_interval_cdf(10, 3, dist)
    # The mean of a non-negative variable is the integral of its survival.
    expect_equal(
      stats::integrate(function(x) 1 - cdf(x), 0, Inf)$value,
      10,
      tolerance = 1e-4,
      info = dist
    )
  }
  expect_error(episodic_serial_interval_cdf(10, 3, "normal"), "Unknown serial interval")
  expect_error(episodic_serial_interval_cdf(0, 3, "gamma"), "positive mean")
  expect_error(episodic_serial_interval_cdf(NA_real_, 3, "gamma"), "positive mean")
})

test_that("the probability follows the model for a single case", {
  cdf <- episodic_serial_interval_cdf(3.6, 1.9, "gamma")
  case <- as.Date("2026-01-01")
  curve <- episodic_outbreak_end_curve(
    case,
    from = as.Date("2026-01-04"),
    si_cdf = cdf,
    end_r = 1.5,
    end_k = 0.5,
    completeness = complete_reporting
  )
  # With reporting complete, an offspring counts as seen once its day's
  # serial-interval mass has passed: the discretised distribution up to
  # the day evaluated, which is the continuous one at half a day on.
  tau <- as.integer(curve$target_date - case)
  q <- cdf(tau + 0.5)
  expect_equal(curve$probability, (1 + (1.5 / 0.5) * (1 - q))^-0.5)
  # It rises with every quiet day, and stops once it reaches 0.99.
  expect_true(all(diff(curve$probability) > 0))
  expect_gte(utils::tail(curve$probability, 1), 0.99)
  expect_lt(utils::tail(curve$probability, 2)[1], 0.99)
})

test_that("an outbreak with no onward transmission is over, and an old one nearly so", {
  cdf <- episodic_serial_interval_cdf(3.6, 1.9, "gamma")
  none <- episodic_outbreak_end_curve(
    as.Date(c("2026-01-01", "2026-01-03")),
    from = as.Date("2026-01-04"),
    si_cdf = cdf,
    end_r = 0,
    end_k = 0.5,
    completeness = complete_reporting
  )
  expect_identical(none$probability, 1)
  old <- episodic_outbreak_end_curve(
    as.Date("2025-06-01"),
    from = as.Date("2026-01-04"),
    si_cdf = cdf,
    end_r = 2,
    end_k = 0.5,
    completeness = complete_reporting
  )
  expect_identical(nrow(old), 1L)
  expect_gt(old$probability, 0.99)
})

test_that("quiet days still being reported count for less than reported ones", {
  cdf <- episodic_serial_interval_cdf(3.6, 1.9, "gamma")
  cases <- as.Date(c("2026-01-01", "2026-01-02", "2026-01-04"))
  args <- list(
    case_dates = cases,
    from = as.Date("2026-01-08"),
    si_cdf = cdf,
    end_r = 1.2,
    end_k = 0.5
  )
  complete <- do.call(episodic_outbreak_end_curve, c(args, list(completeness = complete_reporting)))
  slow <- do.call(episodic_outbreak_end_curve, c(args, list(
    completeness = data.frame(lag_days = 0:21, completeness = pmin(1, (0:21) / 7))
  )))
  expect_lt(slow$probability[1], complete$probability[1])
  # A completion curve that dips is read as its running maximum: a lag
  # cannot be less complete than a shorter one.
  dipping <- data.frame(lag_days = 0:21, completeness = c(0.2, 0.6, 0.4, rep(1, 19)))
  monotone <- data.frame(lag_days = 0:21, completeness = c(0.2, 0.6, 0.6, rep(1, 19)))
  expect_identical(
    do.call(episodic_outbreak_end_curve, c(args, list(completeness = dipping))),
    do.call(episodic_outbreak_end_curve, c(args, list(completeness = monotone)))
  )
})

test_that("a case after the date evaluated is an error, not a probability", {
  expect_error(
    episodic_outbreak_end_curve(
      as.Date(c("2026-01-01", "2026-01-09")),
      from = as.Date("2026-01-08"),
      si_cdf = episodic_serial_interval_cdf(3.6, 1.9, "gamma"),
      end_r = 1,
      end_k = 0.5,
      completeness = complete_reporting
    ),
    "none after"
  )
})

test_that("the forecast applies only where the model does, and says why when it cannot run", {
  pc <- data.frame(
    rt_applicable = 1L,
    si_mean_days = 3.6,
    si_sd_days = 1.9,
    si_dist = "gamma",
    incub_max_days = 3,
    case_free_days = 14L,
    end_r = 0.8,
    end_k = 0.5
  )
  cases <- data.frame(sample_date = as.Date("2026-01-01") + c(0, 4, 9))
  asof <- as.Date("2026-01-12")

  computed <- episodic_outbreak_end_forecast(cases, pc, complete_reporting, asof)
  expect_identical(computed$status, "computed")
  expect_identical(computed$values$target_date[1], asof)
  expect_identical(computed$params$last_case_date, "2026-01-10")
  expect_identical(computed$params$case_free_days, 14L)

  # Not transmissible, or no serial interval: the method does not apply,
  # and there is nothing to record.
  expect_null(episodic_outbreak_end_forecast(cases, NULL, complete_reporting, asof))
  expect_null(episodic_outbreak_end_forecast(
    cases, transform(pc, rt_applicable = 0L), complete_reporting, asof
  ))
  expect_null(episodic_outbreak_end_forecast(
    cases, transform(pc, si_sd_days = NA_real_), complete_reporting, asof
  ))

  # Every case within one maximum incubation period: a common source.
  point <- episodic_outbreak_end_forecast(
    data.frame(sample_date = as.Date("2026-01-01") + c(0, 1, 2)),
    pc,
    complete_reporting,
    asof
  )
  expect_identical(point$status, "not_applicable")
  expect_identical(point$detail, "point_source")
  expect_identical(nrow(point$values), 0L)

  unset <- episodic_outbreak_end_forecast(
    cases, transform(pc, end_r = NA_real_, end_k = NA_real_), complete_reporting, asof
  )
  expect_identical(unset$status, "insufficient_data")
  expect_identical(unset$detail, "parameters")

  unmeasured <- episodic_outbreak_end_forecast(
    cases, pc, data.frame(lag_days = integer(0), completeness = numeric(0)), asof
  )
  expect_identical(unmeasured$status, "insufficient_data")
  expect_identical(unmeasured$detail, "reporting_delay")
})

test_that("an offspring distribution in the pathogen configuration is complete and in range", {
  good <- data.frame(pathogen = c("A", "B"), end_r = c(0.8, NA), end_k = c(0.5, NA))
  expect_identical(episodic_pathogen_config_check(good, "test.csv"), good)
  expect_error(
    episodic_pathogen_config_check(
      data.frame(pathogen = "A", end_r = 0.8, end_k = NA), "test.csv"
    ),
    "A: end_r and end_k are set together"
  )
  expect_error(
    episodic_pathogen_config_check(
      data.frame(pathogen = "A", end_r = -1, end_k = 0.5), "test.csv"
    ),
    "A: end_r must be at least 0"
  )
  expect_error(
    episodic_pathogen_config_check(
      data.frame(pathogen = "A", end_r = "high", end_k = 0.5), "test.csv"
    ),
    "A: end_r and end_k must be numbers"
  )
  # The shipped configuration passes.
  expect_no_error(episodic_pathogen_config_resolve(NA))
})

test_that("a run forecasts the end of each open outbreak of a transmissible pathogen", {
  db <- nowcast_cron_database(pathogen_lines = outbreak_end_pathogen_lines())
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  open <- episodic_db_clusters_not_closed(con, "outbreak")
  expect_gt(nrow(open), 0)
  last_run <- episodic_db_latest_run(con)
  forecasts <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster_forecast WHERE run_id = ?",
    params = list(last_run$run_id)
  )
  expect_setequal(forecasts$cluster_id, open$cluster_id)
  expect_true(all(forecasts$status == "computed"))
  # Kept per run.
  expect_gt(
    DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT run_id) AS n FROM episodic_cluster_forecast")$n,
    1L
  )

  latest <- episodic_db_cluster_forecast_latest(con, open$cluster_id[1], "outbreak_end")
  expect_identical(latest$run_id, last_run$run_id)
  expect_identical(latest$values$target_date[1], as.Date(last_run$run_date))
  expect_true(all(latest$values$probability >= 0 & latest$values$probability <= 1))
  expect_true(all(diff(latest$values$probability) >= 0))

  obj <- episodic_cluster_object(con, open$cluster_id[1], lang = "en")
  html <- as.character(episodic_ui_stat_grid(obj, lang = "en"))
  expect_match(html, "End of outbreak", fixed = TRUE)
  expect_match(html, "chance it is over", fixed = TRUE)
  expect_match(html, "Case-free", fixed = TRUE)
})

test_that("an outbreak without a configured offspring distribution says so, and one of another pathogen says nothing", {
  unset <- nowcast_cron_database(
    n_runs = 4L,
    pathogen_lines = outbreak_end_pathogen_lines(end_r = NA, end_k = NA)
  )
  on.exit(unlink(c(unset$path, unset$config_path)))
  con <- episodic_db_connect(unset$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  forecasts <- DBI::dbGetQuery(con, "SELECT * FROM episodic_cluster_forecast")
  expect_gt(nrow(forecasts), 0)
  expect_true(all(forecasts$status == "insufficient_data"))
  expect_true(all(forecasts$detail == "parameters"))
  cluster_id <- episodic_db_clusters_not_closed(con, "outbreak")$cluster_id[1]
  html <- as.character(episodic_ui_stat_grid(
    episodic_cluster_object(con, cluster_id, lang = "en"),
    lang = "en"
  ))
  expect_match(html, "R and k not configured", fixed = TRUE)

  # "Test pathogen" with no configuration row is not transmissible.
  plain <- nowcast_cron_database(n_runs = 4L)
  on.exit(unlink(c(plain$path, plain$config_path)), add = TRUE)
  plain_con <- episodic_db_connect(plain$path)
  on.exit(DBI::dbDisconnect(plain_con), add = TRUE)
  expect_identical(
    DBI::dbGetQuery(plain_con, "SELECT COUNT(*) AS n FROM episodic_cluster_forecast")$n,
    0L
  )
  cluster_id <- episodic_db_clusters_not_closed(plain_con, "outbreak")$cluster_id[1]
  html <- as.character(episodic_ui_stat_grid(
    episodic_cluster_object(plain_con, cluster_id, lang = "en"),
    lang = "en"
  ))
  expect_no_match(html, "End of outbreak", fixed = TRUE)
})

test_that("the dossier tile gives the probability and when it reaches the threshold", {
  forecast <- function(p, from = as.Date("2026-01-10")) {
    list(
      status = "computed",
      values = data.frame(target_date = from + seq_along(p) - 1L, probability = p)
    )
  }
  html <- function(f) as.character(episodic_ui_outbreak_end_stat(f, 0.95, lang = "en"))
  expect_match(html(forecast(c(0.42, 0.8, 0.96, 0.99))), "42%", fixed = TRUE)
  expect_match(html(forecast(c(0.42, 0.8, 0.96, 0.99))), "95% on 12 January 2026 if no new case", fixed = TRUE)
  expect_match(html(forecast(0.97)), "95% reached", fixed = TRUE)
  expect_match(html(forecast(c(0.2, 0.5))), "95% not before 11 January 2026", fixed = TRUE)
  # Never a rounded 0% or 100%.
  expect_match(html(forecast(0.996)), "&gt; 99%", fixed = TRUE)
  expect_match(html(forecast(c(0.001, 0.99))), "&lt; 1%", fixed = TRUE)
  expect_match(
    html(list(status = "not_applicable", detail = "point_source")),
    "point source, not modelled",
    fixed = TRUE
  )
  expect_match(html(list(status = "failed", detail = "boom")), "see the run log", fixed = TRUE)
  expect_null(episodic_ui_outbreak_end_stat(NULL, 0.95, lang = "en"))
})

test_that("an outbreak-end probability is scored once its case-free window is reported", {
  db <- nowcast_cron_database(pathogen_lines = outbreak_end_pathogen_lines())
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  # As the runs wrote them, every window is still open: all pending.
  scores <- episodic_outbreak_end_scores(con)
  expect_gt(nrow(scores), 0)
  expect_false(any(scores$final))
  expect_true(all(is.na(scores$brier)))
  expect_true(all(is.na(scores$over)))

  # Two forecasts whose windows have passed, written as a run would, one
  # on each side of the stream's cases.
  run <- episodic_db_latest_run(con)
  cluster <- episodic_db_clusters_not_closed(con, "outbreak")[1, ]
  stream_dates <- sort(as.Date(episodic_db_cases_for_stream_id(
    con,
    cluster$stream_id,
    columns = "sample_date"
  )$sample_date))
  # Each in place of what an earlier run wrote about this cluster: one
  # forecast per run, cluster and kind.
  write <- function(run_id, last_case, case_free_days, probability) {
    DBI::dbExecute(
      con,
      "DELETE FROM episodic_cluster_forecast_value WHERE cluster_forecast_id IN
         (SELECT cluster_forecast_id FROM episodic_cluster_forecast
           WHERE run_id = ? AND cluster_id = ?)",
      params = list(run_id, cluster$cluster_id)
    )
    DBI::dbExecute(
      con,
      "DELETE FROM episodic_cluster_forecast WHERE run_id = ? AND cluster_id = ?",
      params = list(run_id, cluster$cluster_id)
    )
    episodic_db_cluster_forecast_insert(
      con,
      run_id = run_id,
      cluster_id = cluster$cluster_id,
      kind = "outbreak_end",
      method = "nishiura_nb",
      status = "computed",
      detail = NA,
      params = list(
        last_case_date = format(last_case),
        case_free_days = case_free_days,
        reporting_horizon_days = 0L
      ),
      values = data.frame(target_date = last_case, probability = probability)
    )
  }
  continued <- write(run$run_id - 1L, stream_dates[1], 3L, 0.3)
  after_last <- write(run$run_id - 2L, max(stream_dates), 0L, 0.9)

  scores <- episodic_outbreak_end_scores(con)
  row <- function(id) scores[scores$cluster_forecast_id == id, ]
  expect_true(row(continued)$final)
  expect_false(row(continued)$over)
  expect_equal(row(continued)$brier, 0.3^2)
  # A window of no days has no case in it: over.
  expect_true(row(after_last)$final)
  expect_true(row(after_last)$over)
  expect_equal(row(after_last)$brier, 0.1^2)

  performance <- episodic_outbreak_end_performance(con)
  expect_identical(performance$pathogen, "Test pathogen")
  expect_identical(performance$n_scored, 2L)
  expect_identical(performance$n_pending, sum(!scores$final))
  expect_equal(performance$mean_probability, 0.6)
  expect_equal(performance$share_over, 0.5)

  html <- as.character(episodic_ui_performance_screen(
    episodic_app_performance(con, lang = "en"),
    lang = "en"
  ))
  expect_match(html, "End-of-outbreak probability", fixed = TRUE)
})

test_that("a stat can say what its value measures, between caption and value", {
  html <- as.character(episodic_ui_stat("Caption", "42", "context", lead = "what it is"))
  expect_true(
    regexpr("Caption", html) < regexpr("what it is", html) &&
      regexpr("what it is", html) < regexpr("42", html) &&
      regexpr("42", html) < regexpr("context", html)
  )
  expect_match(html, "episodic-stat-lead", fixed = TRUE)
  expect_no_match(
    as.character(episodic_ui_stat("Caption", "42")),
    "episodic-stat-lead",
    fixed = TRUE
  )
})
