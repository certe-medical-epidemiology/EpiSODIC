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

episode_mem_synthetic_seasons <- function(n_seasons = 5, seed = 42) {
  set.seed(seed)
  dates <- c()
  for (yr in seq_len(n_seasons) + 2018) {
    season_days <- seq(as.Date(sprintf("%d-10-01", yr)), as.Date(sprintf("%d-05-15", yr + 1)), by = "day")
    lambda <- 2 + 15 * exp(-((as.integer(format(season_days, "%m")) - 1)^2) / 8)
    n <- stats::rpois(length(season_days), lambda / 10)
    dates <- c(dates, rep(season_days, n))
  }
  data.frame(sample_date = as.character(as.Date(dates, origin = "1970-01-01")))
}

test_that("episode_mem_season_week() assigns weeks 40-52/1-20 to a season, and NA off-season", {
  jan <- episode_mem_season_week(as.Date("2025-01-15"))
  expect_equal(jan$season, "2024/2025")
  oct <- episode_mem_season_week(as.Date("2025-10-15"))
  expect_equal(oct$season, "2025/2026")
  july <- episode_mem_season_week(as.Date("2025-07-15"))
  expect_true(is.na(july$season))
})

test_that("episode_mem_status() returns NULL off-season, with too few prior seasons, or without mem installed", {
  skip_if_not_installed("mem")
  cases <- episode_mem_synthetic_seasons(n_seasons = 5)
  expect_null(episode_mem_status(cases, run_date = as.Date("2024-07-01")))  # off-season
  expect_null(episode_mem_status(data.frame(sample_date = character(0))))   # no data
  # only the current season exists, zero priors
  first_season_only <- cases[as.Date(cases$sample_date) < as.Date("2020-05-16"), , drop = FALSE]
  expect_null(episode_mem_status(first_season_only, run_date = as.Date("2020-01-15")))
})

test_that("episode_mem_status() returns a well-formed status once enough seasons exist", {
  skip_if_not_installed("mem")
  cases <- episode_mem_synthetic_seasons(n_seasons = 5)
  status <- episode_mem_status(cases, run_date = as.Date("2024-01-15"))
  expect_false(is.null(status))
  expect_true(all(c("epidemic_started", "current_week_count", "pre_epidemic_threshold",
                     "post_epidemic_threshold", "week_start", "week_end") %in% names(status)))
  expect_true(status$pre_epidemic_threshold <= status$post_epidemic_threshold)
  expect_equal(status$week_end, status$week_start + 6)
})

test_that("episode_detect_mem() fires a detection during peak season and none off-season", {
  skip_if_not_installed("mem")
  cases <- episode_mem_synthetic_seasons(n_seasons = 5)

  det_peak <- episode_detect_mem(cases, stream_id = 1L, run_date = as.Date("2024-01-15"))
  expect_equal(nrow(det_peak), 1)
  expect_equal(det_peak$detector[1], "mem")
  expect_equal(det_peak$stream_id[1], 1L)

  det_off <- episode_detect_mem(cases, stream_id = 1L, run_date = as.Date("2024-07-01"))
  expect_equal(nrow(det_off), 0)
})

test_that("episode_detect_mem() returns an empty record with no cases or mem not installed", {
  expect_equal(nrow(episode_detect_mem(data.frame(sample_date = character(0)), 1L)), 0)
})

test_that("episode_closure_criterion_met() with mem_applicable uses mem_status's post-epidemic threshold, never case-free days", {
  # below threshold -> closure met
  status_low <- list(current_week_count = 2, post_epidemic_threshold = 5)
  expect_true(episode_closure_criterion_met(
    "2025-01-01", "possible_epidemic", case_free_days = 14, mem_applicable = TRUE, mem_status = status_low
  ))
  # above threshold -> not met, regardless of how long case-free
  status_high <- list(current_week_count = 20, post_epidemic_threshold = 5)
  expect_false(episode_closure_criterion_met(
    "2020-01-01", "possible_epidemic", case_free_days = 14, mem_applicable = TRUE, mem_status = status_high
  ))
  # no mem_status at all (mem unavailable/off-season) -> never closes via this criterion
  expect_false(episode_closure_criterion_met(
    "2020-01-01", "possible_epidemic", case_free_days = 14, mem_applicable = TRUE, mem_status = NULL
  ))
})
