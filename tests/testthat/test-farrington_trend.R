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

farrington_cases_from_weekly_counts <- function(week_starts, counts) {
  dates <- unlist(lapply(seq_along(week_starts), function(i) {
    if (counts[i] == 0) return(character(0))
    as.character(week_starts[i] + sample(0:6, counts[i], replace = TRUE))
  }))
  data.frame(sample_date = dates, stringsAsFactors = FALSE)
}

test_that("episodic_farrington_trend() backfills up to max_backfill_weeks on a fresh stream", {
  set.seed(1)
  n_weeks <- 5 * 52 + 20  # enough margin above min_weeks_required (4*52) for a 20-week backfill
  week_starts <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks)
  counts <- stats::rpois(n_weeks, lambda = 3)
  cases <- farrington_cases_from_weekly_counts(week_starts, counts)
  config <- episodic_test_config()

  trend <- episodic_farrington_trend(cases, config, run_date = week_starts[n_weeks] + 3,
                                     n_weeks_existing = 0, max_backfill_weeks = 20)
  expect_equal(nrow(trend), 20)
  expect_true(all(c("week_start", "n_cases", "expected", "upperbound") %in% names(trend)))
})

test_that("episodic_farrington_trend() only computes one week when trend rows already exist", {
  set.seed(2)
  n_weeks <- 4 * 52 + 10
  week_starts <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks)
  counts <- stats::rpois(n_weeks, lambda = 3)
  cases <- farrington_cases_from_weekly_counts(week_starts, counts)
  config <- episodic_test_config()

  trend <- episodic_farrington_trend(cases, config, run_date = week_starts[n_weeks] + 3,
                                     n_weeks_existing = 100)
  expect_equal(nrow(trend), 1)
  expect_equal(trend$week_start[1], week_starts[n_weeks])
})

test_that("episodic_farrington_trend() returns zero rows for insufficient history", {
  cases <- data.frame(sample_date = as.character(seq(as.Date("2025-01-01"), as.Date("2025-03-01"), by = "day")))
  config <- episodic_test_config()
  trend <- episodic_farrington_trend(cases, config, run_date = as.Date("2025-03-01"))
  expect_equal(nrow(trend), 0)
})

test_that("episodic_db_stream_trend_upsert() and episodic_db_stream_trend() round-trip and update in place", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_region", "Test pathogen"), level = "pathogen_region",
    pathogen = "Test pathogen", observed_date = "2025-01-06"
  )
  episodic_db_stream_trend_upsert(con, stream_id, "2025-01-06", n_cases = 3, expected = 2.1, upperbound = 5.0)
  episodic_db_stream_trend_upsert(con, stream_id, "2025-01-06", n_cases = 4, expected = 2.1, upperbound = 5.0)

  rows <- episodic_db_stream_trend(con, stream_id)
  expect_equal(nrow(rows), 1)
  expect_equal(rows$n_cases[1], 4)
})
