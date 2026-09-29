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

nowcast_settings <- function(...) {
  config <- episodic_test_config()
  overrides <- list(...)
  config$forecast$nowcast[names(overrides)] <- overrides
  episodic_nowcast_settings(config)
}

test_that("the shipped nowcast settings are read and checked", {
  settings <- episodic_nowcast_settings(episodic_test_config())
  expect_true(settings$enabled)
  expect_identical(settings$max_delay_days, 21L)
  expect_null(settings$delay_window_days)

  # A configuration without the section is on the shipped defaults.
  expect_identical(episodic_nowcast_settings(list()), settings)

  config <- episodic_test_config()
  config$forecast$nowcast$min_completeness <- 0
  expect_error(episodic_nowcast_settings(config), "min_completeness")
  config <- episodic_test_config()
  config$forecast$nowcast$max_delay_days <- 2.5
  expect_error(episodic_nowcast_settings(config), "max_delay_days")
  config <- episodic_test_config()
  config$forecast$nowcast$enabled <- "yes"
  expect_error(episodic_nowcast_settings(config), "enabled")
  # A sliding window shorter than the horizon cannot estimate the delays
  # the horizon allows for.
  config <- episodic_test_config()
  config$forecast$nowcast$delay_window_days <- 10
  expect_error(episodic_nowcast_settings(config), "delay_window_days")
  config$forecast$nowcast$delay_window_days <- 60
  expect_identical(episodic_nowcast_settings(config)$delay_window_days, 60L)
})

test_that("a nowcast estimates the final counts of the days still being reported", {
  truth <- nowcast_line_list("2026-03-01", "2026-06-30")
  asof <- as.Date("2026-06-30")
  seen <- truth[truth$report_date <= asof, ]
  result <- episodic_nowcast_stream(
    seen,
    asof = asof,
    observation_start = as.Date("2026-03-01"),
    settings = nowcast_settings(max_delay_days = 7L),
    seed = 1
  )
  expect_identical(result$status, "computed")
  days <- result$values[result$values$resolution == "day", ]
  expect_gt(nrow(days), 0)

  # Never fewer than already reported, and the quantiles in order.
  q <- as.matrix(days[names(episodic_forecast_probs)])
  expect_true(all(q >= days$n_observed))
  expect_true(all(apply(q, 1, function(x) all(diff(x) >= 0))))
  expect_true(all(days$mean >= days$n_observed))

  # Reported counts are as the line list has them.
  expect_identical(
    days$n_observed,
    vapply(
      as.Date(days$target_date),
      function(d) sum(seen$sample_date == d),
      integer(1)
    )
  )

  # Today has about a tenth of its cases reported, under the 25% a day
  # needs to be nowcast: it gets no row, rather than a row saying the
  # prior.
  expect_false(format(asof) %in% days$target_date)

  # Over the incomplete days, the nowcast is closer to what is finally
  # reported than the reported counts are.
  final <- vapply(
    as.Date(days$target_date),
    function(d) sum(truth$sample_date == d),
    integer(1)
  )
  expect_lt(sum(abs(days$q50 - final)), sum(abs(days$n_observed - final)))
})

test_that("a week is nowcast as the sum of its days, only once it has fully elapsed", {
  truth <- nowcast_line_list("2026-03-01", "2026-06-30")
  # A Wednesday: the week of 2026-06-22 has elapsed, the current one has
  # not.
  asof <- as.Date("2026-07-01")
  seen <- truth[truth$report_date <= asof, ]
  result <- episodic_nowcast_stream(
    seen,
    asof = asof,
    observation_start = as.Date("2026-03-01"),
    settings = nowcast_settings(max_delay_days = 7L),
    seed = 1
  )
  weeks <- result$values[result$values$resolution == "week", ]
  expect_true("2026-06-22" %in% weeks$target_date)
  expect_false("2026-06-29" %in% weeks$target_date)
  week <- weeks[weeks$target_date == "2026-06-22", ]
  days <- result$values[
    result$values$resolution == "day" &
      as.Date(result$values$target_date) >= as.Date("2026-06-22") &
      as.Date(result$values$target_date) <= as.Date("2026-06-28"),
  ]
  expect_identical(
    week$n_observed,
    sum(seen$sample_date >= as.Date("2026-06-22") &
      seen$sample_date <= as.Date("2026-06-28"))
  )
  # Days of the week past the horizon count as reported; the mean of a
  # sum is the sum of the means.
  final_days <- seq(as.Date("2026-06-22"), as.Date("2026-06-28"), by = "day")
  final_days <- final_days[!format(final_days) %in% days$target_date]
  expect_equal(
    week$mean,
    sum(days$mean) + sum(seen$sample_date %in% final_days),
    tolerance = 1e-6
  )
})

test_that("a stream whose delays were never watched long enough is not nowcast", {
  truth <- nowcast_line_list("2026-03-01", "2026-06-30")
  asof <- as.Date("2026-06-30")
  seen <- truth[truth$report_date <= asof, ]
  # A backfill: everything sampled before the first run was reported to
  # it, so none of it carries an observed delay.
  backfilled <- seen
  backfilled$report_date <- pmax(backfilled$report_date, as.Date("2026-06-27"))
  result <- episodic_nowcast_stream(
    backfilled,
    asof = asof,
    observation_start = as.Date("2026-06-27"),
    settings = nowcast_settings(max_delay_days = 7L),
    seed = 1
  )
  expect_identical(result$status, "insufficient_data")
  expect_identical(result$detail, "reporting_history")
  expect_identical(nrow(result$values), 0L)
  expect_identical(result$params$n_cases_delay_observed, 0L)
})

test_that("a case reported before its sample date is counted, not used for the delay", {
  truth <- nowcast_line_list("2026-03-01", "2026-06-30")
  asof <- as.Date("2026-06-30")
  seen <- truth[truth$report_date <= asof, ]
  odd <- data.frame(
    sample_date = as.Date("2026-06-28"),
    report_date = as.Date("2026-06-26")
  )
  settings <- nowcast_settings(max_delay_days = 7L)
  plain <- episodic_nowcast_stream(seen, asof, as.Date("2026-03-01"), settings, seed = 1)
  with_odd <- episodic_nowcast_stream(
    rbind(seen, odd), asof, as.Date("2026-03-01"), settings,
    seed = 1
  )
  expect_identical(with_odd$params$n_cases_reported_before_sampled, 1L)
  day <- function(result) {
    result$values[result$values$resolution == "day" &
      result$values$target_date == "2026-06-28", ]
  }
  expect_identical(day(with_odd)$n_observed, day(plain)$n_observed + 1L)
  expect_identical(day(with_odd)$q50, day(plain)$q50 + 1)
})

test_that("a nowcast is reproducible and leaves the session's random stream alone", {
  truth <- nowcast_line_list("2026-03-01", "2026-06-30")
  seen <- truth[truth$report_date <= as.Date("2026-06-30"), ]
  settings <- nowcast_settings(max_delay_days = 7L)
  set.seed(42)
  expected_draw <- stats::runif(1)
  set.seed(42)
  first <- episodic_nowcast_stream(seen, "2026-06-30", "2026-03-01", settings, seed = 7)
  expect_identical(stats::runif(1), expected_draw)
  second <- episodic_nowcast_stream(seen, "2026-06-30", "2026-03-01", settings, seed = 7)
  expect_identical(first$values, second$values)
})

test_that("summing independent counts convolves their distributions", {
  a <- stats::dpois(0:60, 3)
  b <- stats::dpois(0:60, 5)
  sum_pmf <- episodic_pmf_convolve(a, b)
  expect_equal(sum_pmf[1:30], stats::dpois(0:29, 8), tolerance = 1e-8)
  expect_equal(sum(sum_pmf), 1)
})

test_that("a value row reads its quantiles from the distribution", {
  row <- episodic_forecast_value_row(
    "day",
    as.Date("2026-06-01"),
    2L,
    c(0, 0, 0.5, 0.25, 0.25)
  )
  expect_identical(row$target_date, "2026-06-01")
  expect_equal(row$mean, 2 * 0.5 + 3 * 0.25 + 4 * 0.25)
  expect_identical(row$q25, 2)
  expect_identical(row$q50, 2)
  expect_identical(row$q75, 3)
  expect_identical(row$q975, 4)
})

test_that("a run nowcasts every stream carrying an open cluster, and keeps each run's nowcast", {
  db <- nowcast_cron_database()
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  open <- rbind(
    episodic_db_clusters_not_closed(con, "outbreak"),
    episodic_db_clusters_not_closed(con, "epidemic")
  )
  expect_gt(nrow(open), 0)
  last_run <- episodic_db_latest_run(con)
  forecasts <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_forecast WHERE run_id = ?",
    params = list(last_run$run_id)
  )
  expect_setequal(forecasts$stream_id, unique(open$stream_id))
  expect_true(all(forecasts$kind == "nowcast"))
  expect_true(all(forecasts$status == "computed"))

  # Kept per run, not replaced: earlier runs' nowcasts are still there.
  expect_gt(
    DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT run_id) AS n FROM episodic_forecast")$n,
    1L
  )

  # What the dossier reads is the latest run's.
  latest <- episodic_db_forecast_latest(con, forecasts$stream_id[1], "nowcast")
  expect_identical(latest$run_id, last_run$run_id)
  expect_identical(latest$status, "computed")
  expect_gt(nrow(latest$values), 0)
  expect_identical(latest$params$max_delay_days, 3L)
  expect_true(all(latest$values$q50 >= latest$values$n_observed))
})

test_that("the first run on a database records why it could not nowcast", {
  db <- nowcast_cron_database(n_runs = 1L)
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  forecasts <- DBI::dbGetQuery(con, "SELECT * FROM episodic_forecast")
  # The row says why there is no nowcast, rather than being absent.
  expect_gt(nrow(forecasts), 0)
  expect_true(all(forecasts$status == "insufficient_data"))
  expect_true(all(forecasts$detail == "reporting_history"))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_forecast_value")$n,
    0L
  )
})

test_that("a nowcast that fails is recorded and announced, and the run still succeeds", {
  testthat::local_mocked_bindings(
    episodic_nowcast_stream = function(...) stop("simulated failure")
  )
  db <- nowcast_cron_database(n_runs = 4L)
  # Added to, not replacing, the handler that restores the mocked binding.
  on.exit(unlink(c(db$path, db$config_path)), add = TRUE)
  expect_true(any(grepl(
    "Nowcast failed for stream [0-9]+ .*: simulated failure",
    db$log
  )))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_identical(episodic_db_latest_run(con)$status, "success")
  forecasts <- DBI::dbGetQuery(con, "SELECT * FROM episodic_forecast")
  expect_gt(nrow(forecasts), 0)
  expect_true(all(forecasts$status == "failed"))
  expect_true(all(forecasts$detail == "simulated failure"))
})

test_that("switched off, a run nowcasts nothing", {
  db <- nowcast_cron_database(config_lines = "    enabled: false", n_runs = 4L)
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_forecast")$n,
    0L
  )
})

test_that("the forecast writer refuses a kind it does not know, and values without a computed status", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(
    episodic_db_forecast_insert(
      con, 1L, 1L, "forecast", "x", "computed", NA, list(),
      episodic_forecast_values_empty()
    ),
    "Unknown forecast kind"
  )
  values <- episodic_forecast_value_row("day", "2025-01-01", 1L, c(0, 1))
  expect_error(
    episodic_db_forecast_insert(
      con, 1L, 1L, "nowcast", "bayes.trunc", "failed", "e", list(), values
    ),
    "cannot carry values"
  )
})

test_that("a warning from the nowcast method is announced in the run log", {
  real <- episodic_nowcast_stream
  testthat::local_mocked_bindings(
    episodic_nowcast_stream = function(...) {
      result <- real(...)
      result$params$warnings <- list("simulated warning")
      result
    }
  )
  db <- nowcast_cron_database(n_runs = 4L)
  # Added to, not replacing, the handler that restores the mocked binding.
  on.exit(unlink(c(db$path, db$config_path)), add = TRUE)
  expect_true(any(grepl(
    "Nowcast for stream [0-9]+ .* warned: simulated warning",
    db$log
  )))
})
