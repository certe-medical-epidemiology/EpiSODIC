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

# These are effectively golden-file tests against the real
# surveillance::farringtonFlexible() (CRAN, installable anywhere).

farrington_cases_from_weekly_counts <- function(week_starts, counts) {
  dates <- unlist(lapply(seq_along(week_starts), function(i) {
    if (counts[i] == 0) {
      return(character(0))
    }
    as.character(week_starts[i] + sample(0:6, counts[i], replace = TRUE))
  }))
  data.frame(sample_date = dates, stringsAsFactors = FALSE)
}

test_that("a sharp current-week spike against a stable baseline fires an alarm", {
  set.seed(1)
  n_weeks <- 4 * 52 + 10 # comfortably above the default b=3 requirement
  week_starts <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks) # Mondays
  counts <- stats::rpois(n_weeks, lambda = 2)
  counts[n_weeks] <- 25L # unmistakable spike in the current week

  cases <- farrington_cases_from_weekly_counts(week_starts, counts)
  config <- episodic_test_config()
  run_date <- week_starts[n_weeks] + 3

  result <- episodic_detect_farrington(
    cases,
    stream_id = 1L,
    config = config,
    run_date = run_date
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$detector[1], "farrington")
  expect_equal(result$n_cases[1], 25)
  expect_true(result$upperbound[1] < 25)
})

test_that("a flat, unremarkable series produces no alarm", {
  set.seed(2)
  n_weeks <- 4 * 52 + 10
  week_starts <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks)
  counts <- stats::rpois(n_weeks, lambda = 3) # no injected spike anywhere

  cases <- farrington_cases_from_weekly_counts(week_starts, counts)
  config <- episodic_test_config()
  run_date <- week_starts[n_weeks] + 3

  result <- episodic_detect_farrington(
    cases,
    stream_id = 1L,
    config = config,
    run_date = run_date
  )
  expect_equal(nrow(result), 0)
})

test_that("insufficient baseline history returns no detection rather than erroring", {
  cases <- data.frame(
    sample_date = as.character(seq(
      as.Date("2025-01-01"),
      as.Date("2025-03-01"),
      by = "day"
    ))
  )
  config <- episodic_test_config()
  result <- episodic_detect_farrington(
    cases,
    stream_id = 1L,
    config = config,
    run_date = as.Date("2025-03-01")
  )
  expect_equal(nrow(result), 0)
})

test_that("an empty cases data frame returns no detection", {
  cases <- data.frame(sample_date = character(0))
  config <- episodic_test_config()
  result <- episodic_detect_farrington(
    cases,
    stream_id = 1L,
    config = config,
    run_date = as.Date("2025-01-01")
  )
  expect_equal(nrow(result), 0)
})

test_that("episodic_weekly_bins() covers the full range and fills zero-count weeks", {
  dates <- as.Date(c("2025-01-01", "2025-01-01", "2025-01-15"))
  weekly <- EpiSODIC:::episodic_weekly_bins(
    dates,
    run_date = as.Date("2025-01-20")
  )
  expect_equal(sum(weekly$counts), 3)
  expect_true(any(weekly$counts == 0)) # the week between the two case-weeks
  expect_equal(length(weekly$week_start), length(weekly$counts))
})

test_that("a run tests every week it owes, so a missed run leaves no untested week", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  config <- episodic_config_resolve()

  # no completed run behind it: an instance starting against a backfilled
  # history opens on the current picture, bounded by the cap
  expect_equal(
    episodic_farrington_weeks_owed(con, as.Date("2025-06-30"), config),
    8L
  )

  finish_run_on <- function(date) {
    run_id <- episodic_db_run_start(con, "h", "a")
    DBI::dbExecute(
      con,
      "UPDATE episodic_detection_run SET status = 'success', finished_at = ?
       WHERE run_id = ?",
      params = list(paste0(date, "T02:00:00Z"), run_id)
    )
  }

  finish_run_on("2025-06-29")
  expect_equal(
    episodic_farrington_weeks_owed(con, as.Date("2025-06-30"), config),
    1L
  )

  # a fortnight's outage owes both of the weeks nothing has looked at
  finish_run_on("2025-06-16")
  expect_equal(
    episodic_farrington_weeks_owed(con, as.Date("2025-06-30"), config),
    2L
  )

  # and a very long outage is still bounded by the cap
  finish_run_on("2024-01-01")
  expect_equal(
    episodic_farrington_weeks_owed(con, as.Date("2025-06-30"), config),
    8L
  )
})

test_that("episodic_detect_farrington() reports a record per alarming week it tested", {
  # A signal that ended a fortnight ago is invisible to a detector that
  # only ever looks at the current week.
  run_date <- as.Date("2025-06-30")
  weeks <- seq(run_date - 7 * 259, run_date, by = "week")
  set.seed(4)
  counts <- stats::rpois(length(weeks), lambda = 4)
  spike_weeks <- weeks[length(weeks) - c(4, 3)]
  counts[length(weeks) - c(4, 3)] <- 60L

  cases <- data.frame(
    sample_date = as.character(rep(weeks, times = counts)),
    stringsAsFactors = FALSE
  )
  config <- episodic_config_resolve()
  detect <- function(n_weeks) {
    episodic_detect_farrington(
      cases,
      stream_id = 1L,
      config = config,
      run_date = run_date,
      n_weeks = n_weeks
    )
  }

  current_only <- detect(1L)
  expect_false(any(as.character(spike_weeks) %in% current_only$first_day))

  caught_up <- detect(8L)
  expect_true(all(as.character(spike_weeks) %in% caught_up$first_day))
  expect_true(all(caught_up$n_cases[
    caught_up$first_day %in% as.character(spike_weeks)
  ] == 60))
  expect_true(all(caught_up$detector == "farrington"))
  # every record carries what the effect-size floor is measured against,
  # which is what keeps the merely-significant weeks out of the queue
  expect_false(any(is.na(caught_up$expected)))
  expect_false(any(is.na(caught_up$upperbound)))
})
