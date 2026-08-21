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

test_that("episodic_classify_curve_shape() classifies within-one-incubation-period clusters as point_source", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 1, 2)))
  expect_equal(episodic_classify_curve_shape(cases, incub_max_days = 3), "point_source")
})

test_that("episodic_classify_curve_shape() classifies a span past twice incub_max_days as propagated", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 20, 40)))
  expect_equal(episodic_classify_curve_shape(cases, incub_max_days = 3), "propagated")
})

test_that("episodic_classify_curve_shape() classifies the zone in between as ambiguous", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 5)))
  expect_equal(episodic_classify_curve_shape(cases, incub_max_days = 3), "ambiguous")
})

test_that("episodic_classify_curve_shape() returns NA when incub_max_days is NA or fewer than 2 cases", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 5)))
  expect_true(is.na(episodic_classify_curve_shape(cases, incub_max_days = NA)))
  expect_true(is.na(episodic_classify_curve_shape(cases[1, , drop = FALSE], incub_max_days = 3)))
})
