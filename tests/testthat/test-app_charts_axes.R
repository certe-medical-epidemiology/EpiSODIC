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

y_scale <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_scales_y[[1]]
}

axes_series <- function(positivity = c(0.02, 0.03, 0.06, 0.04)) {
  data.frame(
    week_start = as.Date("2025-01-06") + (seq_along(positivity) - 1) * 7,
    n_tests = c(200, 190, 210, 205)[seq_along(positivity)],
    n_cases = round(positivity * 200),
    positivity = positivity
  )
}

test_that("every chart's y axis starts at its data and leaves a quarter of headroom", {
  expected <- episodic_chart_y_expand()
  expect_equal(as.numeric(expected), c(0, 0, 0.25, 0))

  curve <- data.frame(
    sample_date = as.Date("2025-01-01") + 0:3,
    n_cases = c(1, 2, 0, 3),
    incomplete = FALSE
  )
  trend <- data.frame(
    week_start = as.Date("2025-01-06") + (0:3) * 7,
    n_cases = c(1, 2, 3, 4),
    expected = 1,
    upperbound = 2
  )
  weekly <- data.frame(
    week_start = as.Date("2025-01-06") + (0:3) * 7,
    n_cases = c(4, 8, 6, 2),
    incomplete = FALSE
  )
  rt <- data.frame(
    window_end = as.Date("2025-01-08") + 0:3,
    mean = c(1.4, 1.2, 0.9, 0.8),
    lower = c(1.0, 0.9, 0.6, 0.5),
    upper = c(1.8, 1.6, 1.3, 1.1)
  )
  charts <- list(
    epi_curve = episodic_ui_epi_curve_chart(curve, lang = "en"),
    trend = episodic_ui_trend_chart(trend, lang = "en"),
    pathogen_curve = episodic_ui_pathogen_curve_chart(weekly, lang = "en"),
    denominator = episodic_ui_denominator_chart(axes_series(), lang = "en"),
    rt = episodic_ui_rt_chart(rt, lang = "en")
  )
  for (name in names(charts)) {
    expect_equal(
      as.numeric(y_scale(charts[[name]])$expand),
      as.numeric(expected),
      info = name
    )
  }
})

test_that("positivity is scaled to its own maximum, not to a fixed 0-100% axis", {
  series <- axes_series()
  p <- episodic_ui_denominator_chart(series, lang = "en")
  line <- ggplot2::layer_data(p, 2)
  # The week with the highest positivity sits level with the tallest bar.
  expect_equal(max(line$y, na.rm = TRUE), max(series$n_tests))

  # The secondary axis reads positivity in per cent, over the range the
  # series actually covers (6% at most here, plus the headroom).
  labels <- as.numeric(ggplot2::get_guide_data(p, "y.sec")$.label)
  expect_true(max(labels) < 10)
  expect_true(max(labels) >= 5)
})

test_that("a positivity above 100% is left out rather than drawn at 100%", {
  series <- axes_series(c(0.02, 1.4, 0.06, 0.04))
  p <- episodic_ui_denominator_chart(series, lang = "en")
  line <- ggplot2::layer_data(p, 2)
  expect_true(is.na(line$y[2]))
  expect_equal(max(line$y, na.rm = TRUE), max(series$n_tests))
})

test_that("a series measured at 0% throughout is drawn along the floor", {
  series <- axes_series(c(0, 0, 0, 0))
  p <- episodic_ui_denominator_chart(series, lang = "en")
  line <- ggplot2::layer_data(p, 2)
  expect_equal(line$y, c(0, 0, 0, 0))
})

test_that("Rt is drawn on a log2 axis labelled in Rt itself", {
  rt <- data.frame(
    window_end = as.Date("2025-01-08") + 0:3,
    mean = c(2, 1, 0.5, 0.25),
    lower = c(1, 0.5, 0.25, 0),
    upper = c(4, 2, 1, 0.5)
  )
  p <- episodic_ui_rt_chart(rt, lang = "en")
  line <- ggplot2::layer_data(p, 3)
  expect_equal(line$y, c(1, 0, -1, -2))
  # The reference line is Rt = 1.
  expect_equal(ggplot2::layer_data(p, 1)$yintercept, 0)

  axis <- episodic_chart_rt_axis(
    log2(c(rt$mean, rt$upper, rt$lower[rt$lower > 0])),
    lang = "en"
  )
  expect_equal(axis$breaks, -2:2)
  expect_equal(axis$labels, c("0.25", "0.5", "1", "2", "4"))
  # And the labels follow the session language's own decimal mark.
  expect_equal(
    episodic_chart_rt_axis(c(-1, 1), lang = "nl")$labels,
    c("0,5", "1", "2")
  )
})

test_that("an Rt interval reaching zero runs to the axis edge instead of vanishing", {
  rt <- data.frame(
    window_end = as.Date("2025-01-08") + 0:1,
    mean = c(1.2, 0.8),
    lower = c(0, 0.4),
    upper = c(2.4, 1.6)
  )
  p <- episodic_ui_rt_chart(rt, lang = "en")
  ribbon <- p$data
  expect_equal(ribbon$log_lower[1], -Inf)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("an Rt axis always shows a doubling and a halving either side of 1", {
  axis <- episodic_chart_rt_axis(log2(c(0.95, 1.05)), lang = "en")
  expect_equal(axis$breaks, -1:1)
  expect_equal(axis$limits, c(-1, 1))
})

test_that("the positivity feed is summed per ISO week, whatever its period starts", {
  denom <- data.frame(
    sample_date = c("2025-08-25", "2025-08-28", "2025-09-01", "2025-09-02"),
    n_tests = c(10, 15, 20, 5)
  )
  weekly <- episodic_app_denominator_weekly(denom)
  expect_equal(weekly$week_start, as.Date(c("2025-08-25", "2025-09-01")))
  expect_equal(weekly$n_tests, c(25, 25))

  none <- episodic_app_denominator_weekly(
    data.frame(sample_date = "not a date", n_tests = 3)
  )
  expect_equal(nrow(none), 0)
})
