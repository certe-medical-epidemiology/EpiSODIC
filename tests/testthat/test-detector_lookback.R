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

# The rule-based detectors need no baseline, so nothing in their own
# logic bounds how far back they look. Unbounded is what they were: every
# run rescanned the whole case history and re-emitted every hit it had
# ever contained. That is not merely wasteful - a re-emitted historical
# detection matches its own settled cluster during reconciliation and
# resets runs_since_detected to zero, so reconciliation.close_after_runs
# can never fire for it and the cluster stays on the board for good.

lookback_case <- function(source_key, sample_date, institution_id, pathogen = "Test pathogen", ward = "ICU") {
  data.frame(
    source_key = source_key,
    pathogen = pathogen,
    institution_id = institution_id,
    ward = ward,
    sample_date = sample_date,
    stringsAsFactors = FALSE
  )
}

lookback_institution <- function(con) {
  id <- episodic_test_institution(
    con,
    key = paste("lookback", stats::runif(1)),
    display_name = "Test Institution",
    institution_type = "hospital",
    is_monitored = 1L
  )
  data.frame(
    institution_id = id,
    institution_type = "hospital",
    is_monitored = 1L,
    stringsAsFactors = FALSE
  )
}

test_that("episodic_detector_lookback_cutoff() bounds only when asked to", {
  run_date <- as.Date("2025-06-30")
  expect_equal(
    episodic_detector_lookback_cutoff(run_date, 90),
    as.Date("2025-04-01")
  )
  expect_null(episodic_detector_lookback_cutoff(run_date, NULL))
  expect_null(episodic_detector_lookback_cutoff(run_date, NA))
  expect_null(episodic_detector_lookback_cutoff(run_date, "not a number"))
  expect_null(episodic_detector_lookback_cutoff(run_date, -1))
})

test_that("a window is judged on its last day, not its first", {
  cutoff <- as.Date("2025-04-01")
  windows <- list(
    list(first_day = "2025-01-01", last_day = "2025-01-20", n_cases = 3),
    # started long before the cutoff but still producing cases: current
    list(first_day = "2025-02-01", last_day = "2025-06-01", n_cases = 9),
    list(first_day = "2025-05-01", last_day = "2025-05-10", n_cases = 4)
  )
  kept <- episodic_detector_windows_within(windows, cutoff)
  expect_equal(length(kept), 2)
  expect_equal(vapply(kept, function(w) w$last_day, character(1)),
               c("2025-06-01", "2025-05-10"))
  # no cutoff keeps everything
  expect_equal(length(episodic_detector_windows_within(windows, NULL)), 3)
})

test_that("same_place reports a recent hit and stops re-reporting an old one", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institutions <- lookback_institution(con)
  id <- institutions$institution_id[1]
  config <- episodic_test_config()
  config$same_place$lookback_days <- 90

  cases <- rbind(
    lookback_case("K1", "2020-01-01", id),
    lookback_case("K2", "2020-01-03", id),
    lookback_case("K3", "2020-01-05", id),
    lookback_case("K4", "2025-06-01", id),
    lookback_case("K5", "2025-06-03", id),
    lookback_case("K6", "2025-06-05", id)
  )

  # A run five years later still sees the 2020 outbreak in its input, and
  # must not report it again.
  recent <- episodic_detect_same_place(
    con,
    cases,
    institutions,
    config,
    run_date = as.Date("2025-06-30")
  )
  expect_equal(nrow(recent), 1)
  expect_equal(recent$first_day[1], "2025-06-01")

  # Run as of 2020, the 2020 hit is the one that is current.
  historical <- episodic_detect_same_place(
    con,
    cases,
    institutions,
    config,
    run_date = as.Date("2020-01-31")
  )
  expect_equal(nrow(historical), 1)
  expect_equal(historical$first_day[1], "2020-01-01")

  # Bound removed: both hits come back, which is what the instance asked
  # for by setting it to null.
  config$same_place$lookback_days <- NULL
  both <- episodic_detect_same_place(
    con,
    cases,
    institutions,
    config,
    run_date = as.Date("2025-06-30")
  )
  expect_equal(nrow(both), 2)
})

test_that("rare_trigger stops re-firing on a case it already reported years ago", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  id <- lookback_institution(con)$institution_id[1]
  config <- episodic_test_config()
  config$rare_trigger$lookback_days <- 90

  cases <- rbind(
    lookback_case("K1", "2020-05-05", id, pathogen = "Candida auris"),
    lookback_case("K2", "2025-06-10", id, pathogen = "Candida auris")
  )

  recent <- episodic_detect_rare_trigger(
    con,
    cases,
    config,
    run_date = as.Date("2025-06-30")
  )
  expect_equal(nrow(recent), 1)
  expect_equal(recent$first_day[1], "2025-06-10")

  config$rare_trigger$lookback_days <- NULL
  expect_equal(
    nrow(episodic_detect_rare_trigger(
      con,
      cases,
      config,
      run_date = as.Date("2025-06-30")
    )),
    2
  )
})

test_that("the shipped defaults bound both rule-based detectors", {
  defaults <- episodic_config_resolve(NA)
  expect_true(is.numeric(defaults$same_place$lookback_days))
  expect_true(is.numeric(defaults$rare_trigger$lookback_days))
})
