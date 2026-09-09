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

# The arithmetic a validation result is made of, tested on its own.

test_that("the Wilson interval stays inside 0 and 1 at the extremes", {
  ci <- episodic_wilson_ci(c(0, 6, 3), c(6, 6, 6))
  expect_equal(ci$estimate, c(0, 1, 0.5))
  expect_true(all(ci$lower >= 0))
  expect_true(all(ci$upper <= 1))
  expect_gt(ci$upper[1], 0)
  expect_lt(ci$lower[2], 1)
  # Six out of six is not "certainly all of them".
  expect_lt(ci$upper[2], 1.0000001)
})

test_that("no trials is not a proportion of zero", {
  ci <- episodic_wilson_ci(0, 0)
  expect_true(is.na(ci$estimate))
  expect_true(is.na(ci$lower))
  expect_true(is.na(ci$upper))
})

test_that("a Poisson rate is events over exposure, with an exact interval", {
  ci <- episodic_poisson_ci(5, 1000)
  expect_equal(ci$estimate, 0.005)
  expect_lt(ci$lower, 0.005)
  expect_gt(ci$upper, 0.005)
  # Zero events is a real observation; no exposure is not.
  expect_equal(episodic_poisson_ci(0, 1000)$estimate, 0)
  expect_true(is.na(episodic_poisson_ci(0, 0)$estimate))
})

test_that("Kaplan-Meier keeps the outbreaks nothing found", {
  # Four outbreaks: found on days 5 and 10, and two watched for 30 days
  # without ever being found.
  km <- episodic_validation_km(
    c(5, 10, 30, 30),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(km$time, c(0, 5, 10))
  expect_equal(km$n_risk, c(4L, 4L, 3L))
  expect_equal(km$survival[2], 0.75)
  expect_equal(km$survival[3], 0.5)
  # Half detected by day 10, and the median says so - where a median
  # over the two detected outbreaks alone would have said 7.5.
  expect_equal(episodic_validation_km_median(km), 10)
})

test_that("a curve that never reaches half has no median", {
  km <- episodic_validation_km(c(5, 30, 30, 30), c(TRUE, FALSE, FALSE, FALSE))
  expect_true(is.na(episodic_validation_km_median(km)))
})

test_that("Kaplan-Meier with nothing detected is a flat curve, not an error", {
  km <- episodic_validation_km(c(30, 30), c(FALSE, FALSE))
  expect_equal(nrow(km), 1)
  expect_equal(km$survival, 1)
})

test_that("AUC separates a score that ranks from one that does not", {
  expect_equal(
    episodic_validation_auc(c(10, 9, 2, 1), c(TRUE, TRUE, FALSE, FALSE)),
    1
  )
  expect_equal(
    episodic_validation_auc(c(1, 2, 9, 10), c(TRUE, TRUE, FALSE, FALSE)),
    0
  )
  # All tied is chance, by the half-credit convention.
  expect_equal(
    episodic_validation_auc(c(5, 5, 5, 5), c(TRUE, TRUE, FALSE, FALSE)),
    0.5
  )
})

test_that("AUC of a one-class sample is unknown, not chance", {
  expect_true(is.na(episodic_validation_auc(c(1, 2, 3), c(TRUE, TRUE, TRUE))))
})

test_that("an empty calibration bin has no observed rate", {
  calibration <- episodic_validation_calibration(
    c(10, 15, 90),
    c(FALSE, FALSE, TRUE),
    breaks = c(0, 20, 40, 60, 80, 100)
  )
  expect_equal(calibration$n, c(2L, 0L, 0L, 0L, 1L))
  expect_equal(calibration$observed[1], 0)
  expect_true(is.na(calibration$observed[2]))
  expect_equal(calibration$observed[5], 1)
})

test_that("a proportion row carries both the per-seed spread and the pool", {
  row <- episodic_validation_proportion_row(
    "sensitivity",
    "overall",
    "all outbreaks",
    numerator = c(3, 4, 6),
    denominator = c(6, 6, 6)
  )
  expect_equal(row$numerator, 13)
  expect_equal(row$denominator, 18)
  expect_equal(row$estimate, 13 / 18)
  expect_equal(row$median, 4 / 6)
  expect_equal(row$n_seeds, 3L)
  expect_equal(row$interval, "wilson")
})

test_that("a proportion out of one trial gets no across-seed median", {
  row <- episodic_validation_proportion_row(
    "sensitivity",
    "outbreak_shape",
    "WAVE",
    numerator = c(1, 0, 1),
    denominator = c(1, 1, 1)
  )
  expect_true(is.na(row$median))
  expect_equal(row$estimate, 2 / 3)
})

test_that("a value row takes the median within each seed, then across them", {
  row <- episodic_validation_value_row(
    "delay_from_first_case",
    "overall",
    "detected outbreaks",
    seed = c(1, 1, 1, 2),
    value = c(2, 4, 30, 100),
    unit = "days"
  )
  # Seed 1's median is 4 and seed 2's is 100, so the median across seeds
  # is 52 - the single outlying seed does not get to weigh three times.
  expect_equal(row$median, 52)
  expect_equal(row$n_seeds, 2L)
  expect_equal(row$denominator, 4)
})

test_that("a value row with nothing measurable reports NA, not zero", {
  row <- episodic_validation_value_row(
    "delay_from_first_case",
    "overall",
    "detected outbreaks",
    seed = c(1, 2),
    value = c(NA_real_, NA_real_),
    unit = "days"
  )
  expect_true(is.na(row$median))
  expect_equal(row$n_seeds, 0L)
})
