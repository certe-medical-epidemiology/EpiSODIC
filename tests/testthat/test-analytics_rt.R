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

test_that("episodic_compute_rt() returns NULL when rt_applicable is FALSE", {
  skip_if_not_installed("EpiEstim")
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:30))
  pc <- data.frame(rt_applicable = 0, si_mean_days = 3, si_sd_days = 1.5)
  expect_null(episodic_compute_rt(cases, pc))
})

test_that("episodic_compute_rt() returns NULL when the serial interval is unset", {
  skip_if_not_installed("EpiEstim")
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:30))
  pc <- data.frame(rt_applicable = 1, si_mean_days = NA, si_sd_days = NA)
  expect_null(episodic_compute_rt(cases, pc))
})

test_that("episodic_compute_rt() returns NULL with too little case history for even one window", {
  skip_if_not_installed("EpiEstim")
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:3))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)
  expect_null(episodic_compute_rt(cases, pc))
})

test_that("episodic_compute_rt() returns a well-formed series with credible intervals for enough case history", {
  skip_if_not_installed("EpiEstim")
  set.seed(1)
  dates <- as.Date("2025-01-01") + sample(0:40, 150, replace = TRUE)
  cases <- data.frame(sample_date = as.character(dates))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)

  rt <- episodic_compute_rt(cases, pc)
  expect_false(is.null(rt))
  expect_true(all(c("window_end", "mean", "lower", "upper") %in% names(rt)))
  expect_true(all(rt$lower <= rt$mean & rt$mean <= rt$upper))
})

test_that("episodic_compute_rt() withholds windows ending inside incomplete_days of asof", {
  skip_if_not_installed("EpiEstim")
  set.seed(1)
  dates <- as.Date("2025-01-01") + sample(0:40, 150, replace = TRUE)
  cases <- data.frame(sample_date = as.character(dates))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)
  asof <- max(dates)

  full <- episodic_compute_rt(cases, pc, incomplete_days = 0, asof = asof)
  truncated <- episodic_compute_rt(cases, pc, incomplete_days = 10, asof = asof)
  expect_true(nrow(truncated) < nrow(full))
  expect_true(max(truncated$window_end) <= max(full$window_end) - 10)
})

test_that("episodic_compute_rt() measures incompleteness from asof, not from the series' own last case", {
  # A cluster whose last case is long past has no under-ascertained tail:
  # its trailing windows must survive, however large incomplete_days is.
  skip_if_not_installed("EpiEstim")
  set.seed(1)
  dates <- as.Date("2025-01-01") + sample(0:40, 150, replace = TRUE)
  cases <- data.frame(sample_date = as.character(dates))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)

  stale <- episodic_compute_rt(
    cases,
    pc,
    incomplete_days = 10,
    asof = max(dates) + 200
  )
  fresh <- episodic_compute_rt(
    cases,
    pc,
    incomplete_days = 0,
    asof = max(dates)
  )
  expect_equal(nrow(stale), nrow(fresh))
})

test_that("episodic_compute_rt() does not estimate inside one mean serial interval of the series start", {
  # The renewal denominator has no infection history to condition on
  # there, which biases the earliest estimates upwards.
  skip_if_not_installed("EpiEstim")
  set.seed(2)
  dates <- as.Date("2025-01-01") + sample(0:40, 150, replace = TRUE)
  cases <- data.frame(sample_date = as.character(dates))
  asof <- max(dates)

  short_si <- episodic_compute_rt(
    cases,
    data.frame(rt_applicable = 1, si_mean_days = 2, si_sd_days = 1),
    asof = asof
  )
  long_si <- episodic_compute_rt(
    cases,
    data.frame(rt_applicable = 1, si_mean_days = 12, si_sd_days = 4),
    asof = asof
  )
  # Same series, same window width: the longer serial interval simply
  # starts estimating later, so it yields strictly fewer windows and its
  # first window ends later.
  expect_true(nrow(long_si) < nrow(short_si))
  expect_true(min(long_si$window_end) > min(short_si$window_end))
  # Never before the very first window EpiEstim itself permits.
  expect_true(min(short_si$window_end) >= min(as.Date(cases$sample_date)) + 7)
})
