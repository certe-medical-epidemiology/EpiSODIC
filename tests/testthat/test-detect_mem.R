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

episodic_mem_synthetic_seasons <- function(n_seasons = 5, seed = 42) {
  set.seed(seed)
  dates <- c()
  for (yr in seq_len(n_seasons) + 2018) {
    # Deliberately wider than the week 40-20 surveillance window at both
    # ends, so every generated season is one the case data spans end to
    # end and therefore usable as MEM history
    # (`episodic_mem_observed_seasons()`).
    season_days <- seq(
      as.Date(sprintf("%d-09-01", yr)),
      as.Date(sprintf("%d-06-30", yr + 1)),
      by = "day"
    )
    lambda <- 2 + 15 * exp(-((as.integer(format(season_days, "%m")) - 1)^2) / 8)
    n <- stats::rpois(length(season_days), lambda / 10)
    dates <- c(dates, rep(season_days, n))
  }
  data.frame(sample_date = as.character(as.Date(dates, origin = "1970-01-01")))
}

test_that("episodic_mem_season_week() assigns weeks 40-52/1-20 to a season, and NA off-season", {
  jan <- episodic_mem_season_week(as.Date("2025-01-15"))
  expect_equal(jan$season, "2024/2025")
  oct <- episodic_mem_season_week(as.Date("2025-10-15"))
  expect_equal(oct$season, "2025/2026")
  july <- episodic_mem_season_week(as.Date("2025-07-15"))
  expect_true(is.na(july$season))
})

test_that("episodic_mem_status() returns NULL with no data, or too few fully-observed prior seasons", {
  skip_if_not_installed("mem")
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  expect_null(episodic_mem_status(
    data.frame(sample_date = character(0)),
    run_date = as.Date("2024-01-15")
  ))
  # only the current season exists, zero priors
  first_season_only <- cases[
    as.Date(cases$sample_date) < as.Date("2020-05-16"), ,
    drop = FALSE
  ]
  expect_null(episodic_mem_status(
    first_season_only,
    run_date = as.Date("2020-01-15")
  ))
})

test_that("episodic_mem_status() reports off-season as a fact, not as an inability to compute", {
  # The distinction is what gives a seasonal cluster a closure route in
  # July; see episodic_closure_criterion_met().
  status <- episodic_mem_status(
    episodic_mem_synthetic_seasons(n_seasons = 5),
    run_date = as.Date("2024-07-01")
  )
  expect_false(is.null(status))
  expect_false(status$in_season)
  expect_false(status$epidemic_started)
})

test_that("episodic_mem_status() is knowable off-season even with no data at all", {
  expect_false(
    episodic_mem_status(
      data.frame(sample_date = character(0)),
      run_date = as.Date("2024-07-01")
    )$in_season
  )
})

test_that("episodic_mem_evaluation_week() is the last fully-elapsed week, never the one in progress", {
  # 2024-01-17 is a Wednesday: the week in progress starts 2024-01-15,
  # so the last complete one starts 2024-01-08.
  wk <- episodic_mem_evaluation_week(as.Date("2024-01-17"))
  expect_equal(wk$week_start, as.Date("2024-01-08"))
  expect_equal(wk$season, "2023/2024")
  # and on a Monday, last week - not the day-old week just begun
  expect_equal(
    episodic_mem_evaluation_week(as.Date("2024-01-15"))$week_start,
    as.Date("2024-01-08")
  )
})

test_that("episodic_iso_week_start() returns the Monday of an ISO week", {
  expect_equal(episodic_iso_week_start(2024, 1), as.Date("2024-01-01"))
  expect_equal(episodic_iso_week_start(2021, 1), as.Date("2021-01-04"))
  expect_equal(episodic_iso_week_start(2019, 40), as.Date("2019-09-30"))
  expect_equal(as.integer(format(episodic_iso_week_start(2023, 40), "%u")), 1L)
})

test_that("episodic_mem_observed_seasons() drops seasons the data does not span end to end", {
  # A site whose data begins in January carries a season that is
  # three-quarters structural zeros; it must not become MEM history.
  partial <- data.frame(
    sample_date = as.character(seq(
      as.Date("2021-01-01"),
      as.Date("2023-08-01"),
      by = "week"
    ))
  )
  kept <- episodic_mem_observed_seasons(
    c("2020/2021", "2021/2022", "2022/2023"),
    partial
  )
  expect_equal(kept, c("2021/2022", "2022/2023")) # 2020/2021 began before the data did
  expect_equal(
    episodic_mem_observed_seasons(character(0), partial),
    character(0)
  )
})

test_that("episodic_mem_status() returns a well-formed status once enough seasons exist", {
  skip_if_not_installed("mem")
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  status <- episodic_mem_status(cases, run_date = as.Date("2024-01-15"))
  expect_false(is.null(status))
  expect_true(all(
    c(
      "in_season",
      "epidemic_started",
      "current_week_count",
      "pre_epidemic_threshold",
      "post_epidemic_threshold",
      "week_start",
      "week_end"
    ) %in%
      names(status)
  ))
  expect_true(status$in_season)
  expect_true(status$pre_epidemic_threshold <= status$post_epidemic_threshold)
  expect_equal(status$week_end, status$week_start + 6)
})

test_that("episodic_detect_mem() fires a detection during peak season and none off-season", {
  skip_if_not_installed("mem")
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)

  det_peak <- episodic_detect_mem(
    cases,
    stream_id = 1L,
    run_date = as.Date("2024-01-15")
  )
  expect_equal(nrow(det_peak), 1)
  expect_equal(det_peak$detector[1], "mem")
  expect_equal(det_peak$stream_id[1], 1L)

  det_off <- episodic_detect_mem(
    cases,
    stream_id = 1L,
    run_date = as.Date("2024-07-01")
  )
  expect_equal(nrow(det_off), 0)
})

test_that("episodic_detect_mem() returns an empty record with no cases or mem not installed", {
  expect_equal(
    nrow(episodic_detect_mem(data.frame(sample_date = character(0)), 1L)),
    0
  )
})

test_that("episodic_closure_criterion_met() in season uses mem_status's post-epidemic threshold, never case-free days", {
  # below threshold -> closure met
  status_low <- list(
    in_season = TRUE,
    current_week_count = 2,
    post_epidemic_threshold = 5
  )
  expect_true(episodic_closure_criterion_met(
    "2025-01-01",
    "possible_epidemic",
    case_free_days = 14,
    mem_applicable = TRUE,
    mem_status = status_low
  ))
  # above threshold -> not met, regardless of how long case-free
  status_high <- list(
    in_season = TRUE,
    current_week_count = 20,
    post_epidemic_threshold = 5
  )
  expect_false(episodic_closure_criterion_met(
    "2020-01-01",
    "possible_epidemic",
    case_free_days = 14,
    mem_applicable = TRUE,
    mem_status = status_high
  ))
  # no mem_status at all (mem unavailable, or too little history) ->
  # never closes via this criterion
  expect_false(episodic_closure_criterion_met(
    "2020-01-01",
    "possible_epidemic",
    case_free_days = 14,
    mem_applicable = TRUE,
    mem_status = NULL
  ))
})

test_that("episodic_closure_criterion_met() out of season falls back to the case-free interval", {
  off <- list(
    in_season = FALSE,
    current_week_count = NA_integer_,
    post_epidemic_threshold = NA_real_
  )
  # Long case-free: the season is over and so is the cluster.
  expect_true(episodic_closure_criterion_met(
    "2025-04-01",
    "confirmed_epidemic",
    case_free_days = 14,
    mem_applicable = TRUE,
    mem_status = off,
    today = as.Date("2025-07-01")
  ))
  # Cases last week: the season lapsing around it is not on its own a
  # reason to call the cluster closable.
  expect_false(episodic_closure_criterion_met(
    "2025-06-28",
    "confirmed_epidemic",
    case_free_days = 14,
    mem_applicable = TRUE,
    mem_status = off,
    today = as.Date("2025-07-01")
  ))
})
