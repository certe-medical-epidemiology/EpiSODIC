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

wis_quantiles <- function(values) {
  as.data.frame(as.list(stats::setNames(values, names(episodic_forecast_probs))))
}

test_that("the weighted interval score matches its definition", {
  forecast <- wis_quantiles(c(0, 1, 2, 3, 4, 6, 7, 8, 9))
  # Inside every interval: half the median's error plus each interval's
  # width weighted by alpha / 2, over 4.5.
  expect_equal(
    episodic_forecast_wis(forecast, 5),
    (0.5 * 1 + 0.25 * 3 + 0.1 * 5 + 0.05 * 7 + 0.025 * 9) / 4.5
  )
  # Outside the three inner intervals: each adds 2 / alpha per unit.
  expect_equal(
    episodic_forecast_wis(forecast, 10),
    (0.5 * 6 + 0.25 * (3 + 4 * 4) + 0.1 * (5 + 10 * 3) +
      0.05 * (7 + 20 * 2) + 0.025 * (9 + 40 * 1)) / 4.5
  )
  # A point forecast scores its absolute error.
  point <- wis_quantiles(rep(4, 9))
  expect_equal(episodic_forecast_wis(point, c(4, 7, 1)), c(0, 3, 3))
})

test_that("a nowcast is scored only once its target is past the reporting horizon", {
  db <- nowcast_cron_database()
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  scores <- episodic_nowcast_scores(con)
  expect_gt(sum(scores$final), 0)
  expect_gt(sum(!scores$final), 0)

  asof <- as.Date(episodic_db_latest_run(con)$run_date)
  end <- as.Date(scores$target_date) +
    ifelse(scores$resolution == "week", 6L, 0L)
  expect_identical(scores$final, end + 3L <= asof)

  # A pending value has no outcome and no score, not a score of zero.
  pending <- scores[!scores$final, ]
  expect_true(all(is.na(pending$outcome)))
  expect_true(all(is.na(pending$wis)))
  expect_true(all(is.na(pending$in_90)))

  # The outcome is the stream's count now.
  final <- scores[scores$final & scores$resolution == "day", ]
  row <- final[1, ]
  stream_cases <- episodic_db_cases_for_stream_id(
    con,
    row$stream_id,
    columns = "sample_date"
  )
  expect_identical(
    as.integer(row$outcome),
    sum(stream_cases$sample_date == row$target_date)
  )
  expect_true(all(final$wis >= 0))
  expect_identical(
    final$ae_reported,
    abs(final$outcome - final$n_observed)
  )
})

test_that("the Performance screen's nowcast rows add up, and say NA without a denominator", {
  db <- nowcast_cron_database()
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  scores <- episodic_nowcast_scores(con)
  performance <- episodic_nowcast_performance(con)
  expect_gt(nrow(performance), 0)
  for (i in seq_len(nrow(performance))) {
    group <- scores[scores$level == performance$level[i] &
      scores$resolution == performance$resolution[i], ]
    expect_identical(
      performance$n_scored[i] + performance$n_pending[i],
      nrow(group)
    )
    if (performance$n_scored[i] == 0) {
      expect_true(is.na(performance$coverage_90[i]))
      expect_true(is.na(performance$mean_wis[i]))
    } else {
      expect_equal(performance$coverage_90[i], mean(group$in_90, na.rm = TRUE))
    }
  }

  # Nothing stored, nothing to show: no rows, not rows of zeros.
  empty <- episodic_test_db()
  on.exit(DBI::dbDisconnect(empty), add = TRUE)
  expect_identical(nrow(episodic_nowcast_performance(empty)), 0L)
})

test_that("the Performance screen shows the nowcast section, with or without rows", {
  db <- nowcast_cron_database()
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  html <- as.character(episodic_ui_performance_screen(
    episodic_app_performance(con, lang = "en"),
    lang = "en"
  ))
  expect_match(html, "Nowcast accuracy", fixed = TRUE)
  expect_match(html, "L1 · ward", fixed = TRUE)
  expect_match(html, "pending", fixed = TRUE)

  empty_con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(empty_con), add = TRUE)
  empty <- episodic_ui_performance_nowcast(
    episodic_nowcast_performance(empty_con),
    lang = "en"
  )
  expect_match(
    as.character(empty),
    "No nowcast has been stored yet.",
    fixed = TRUE
  )
})
