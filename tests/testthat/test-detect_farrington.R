# These are effectively golden-file tests against the real
# surveillance::farringtonFlexible() (CRAN, installable anywhere), unlike
# the old certestats wrapper which could never be exercised in CI. See
# QUESTIONS.md for the switch.

farrington_cases_from_weekly_counts <- function(week_starts, counts) {
  dates <- unlist(lapply(seq_along(week_starts), function(i) {
    if (counts[i] == 0) return(character(0))
    as.character(week_starts[i] + sample(0:6, counts[i], replace = TRUE))
  }))
  data.frame(sample_date = dates, stringsAsFactors = FALSE)
}

test_that("a sharp current-week spike against a stable baseline fires an alarm", {
  set.seed(1)
  n_weeks <- 4 * 52 + 10  # comfortably above the default b=3 requirement
  week_starts <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks)  # Mondays
  counts <- stats::rpois(n_weeks, lambda = 2)
  counts[n_weeks] <- 25L  # unmistakable spike in the current week

  cases <- farrington_cases_from_weekly_counts(week_starts, counts)
  config <- episode_test_config()
  run_date <- week_starts[n_weeks] + 3

  result <- episode_detect_farrington(cases, stream_id = 1L, config = config, run_date = run_date)
  expect_equal(nrow(result), 1)
  expect_equal(result$detector[1], "farrington")
  expect_equal(result$n_cases[1], 25)
  expect_true(result$upperbound[1] < 25)
})

test_that("a flat, unremarkable series produces no alarm", {
  set.seed(2)
  n_weeks <- 4 * 52 + 10
  week_starts <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks)
  counts <- stats::rpois(n_weeks, lambda = 3)  # no injected spike anywhere

  cases <- farrington_cases_from_weekly_counts(week_starts, counts)
  config <- episode_test_config()
  run_date <- week_starts[n_weeks] + 3

  result <- episode_detect_farrington(cases, stream_id = 1L, config = config, run_date = run_date)
  expect_equal(nrow(result), 0)
})

test_that("insufficient baseline history returns no detection rather than erroring", {
  cases <- data.frame(sample_date = as.character(seq(as.Date("2025-01-01"), as.Date("2025-03-01"), by = "day")))
  config <- episode_test_config()
  result <- episode_detect_farrington(cases, stream_id = 1L, config = config, run_date = as.Date("2025-03-01"))
  expect_equal(nrow(result), 0)
})

test_that("an empty cases data frame returns no detection", {
  cases <- data.frame(sample_date = character(0))
  config <- episode_test_config()
  result <- episode_detect_farrington(cases, stream_id = 1L, config = config, run_date = as.Date("2025-01-01"))
  expect_equal(nrow(result), 0)
})

test_that("episode_weekly_bins() covers the full range and fills zero-count weeks", {
  dates <- as.Date(c("2025-01-01", "2025-01-01", "2025-01-15"))
  weekly <- EpiSODIC:::episode_weekly_bins(dates, run_date = as.Date("2025-01-20"))
  expect_equal(sum(weekly$counts), 3)
  expect_true(any(weekly$counts == 0))  # the week between the two case-weeks
  expect_equal(length(weekly$week_start), length(weekly$counts))
})
