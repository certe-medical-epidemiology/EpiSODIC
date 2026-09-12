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

# -- Fixtures -----------------------------------------------------------

episodic_mem_synthetic_seasons <- function(n_seasons = 5,
                                           seed = 42,
                                           peak_month = 1) {
  set.seed(seed)
  dates <- c()
  for (yr in seq_len(n_seasons) + 2018) {
    all_days <- seq(
      as.Date(sprintf("%d-01-01", yr)),
      as.Date(sprintf("%d-12-31", yr)),
      by = "day"
    )
    months <- as.integer(format(all_days, "%m"))
    lambda <- 2 + 15 * exp(-((months - peak_month)^2) / 4)
    n <- stats::rpois(length(all_days), lambda / 10)
    dates <- c(dates, rep(all_days, n))
  }
  data.frame(sample_date = as.character(as.Date(dates, origin = "1970-01-01")))
}

episodic_mem_synthetic_flat <- function(n_years = 5, seed = 99) {
  set.seed(seed)
  dates <- c()
  for (yr in seq_len(n_years) + 2018) {
    all_days <- seq(
      as.Date(sprintf("%d-01-01", yr)),
      as.Date(sprintf("%d-12-31", yr)),
      by = "day"
    )
    n <- stats::rpois(length(all_days), rep(0.2 * yr, length(all_days)))
    dates <- c(dates, rep(all_days, n))
  }
  data.frame(sample_date = as.character(as.Date(dates, origin = "1970-01-01")))
}

# -- Season assignment --------------------------------------------------

test_that("episodic_mem_season_week() assigns every week to a season, never NA", {
  aw <- 40L
  jan <- episodic_mem_season_week(as.Date("2025-01-15"), aw)
  expect_equal(jan$season, "2024/2025")
  oct <- episodic_mem_season_week(as.Date("2025-10-15"), aw)
  expect_equal(oct$season, "2025/2026")
  july <- episodic_mem_season_week(as.Date("2025-07-15"), aw)
  expect_equal(july$season, "2024/2025")
  expect_false(is.na(july$season))
})

test_that("episodic_mem_season_week() with anchor_week = 1 gives calendar-year seasons", {
  w <- episodic_mem_season_week(as.Date("2025-06-15"), 1L)
  expect_equal(w$season, "2025/2026")
  w2 <- episodic_mem_season_week(as.Date("2024-12-30"), 1L)
  expect_equal(w2$season, "2025/2026")
})

test_that("episodic_mem_season_week() with non-40 anchor works correctly", {
  w <- episodic_mem_season_week(as.Date("2025-01-15"), 20L)
  expect_equal(w$season, "2024/2025")
  w2 <- episodic_mem_season_week(as.Date("2025-06-15"), 20L)
  expect_equal(w2$season, "2025/2026")
})

# -- Anchor derivation -------------------------------------------------

test_that("episodic_mem_season_anchor() returns NULL with insufficient history", {
  config <- episodic_config_resolve(NA)
  cases <- data.frame(
    sample_date = as.character(seq(
      as.Date("2023-01-01"),
      as.Date("2023-12-31"),
      by = "week"
    ))
  )
  expect_null(episodic_mem_season_anchor(cases, config))
})

test_that("episodic_mem_season_anchor() places a winter-peaking pathogen's anchor in summer", {
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5, peak_month = 1)
  anchor <- episodic_mem_season_anchor(cases, config)
  expect_false(is.null(anchor))
  expect_true(anchor$anchor_week >= 20 && anchor$anchor_week <= 40)
})

test_that("episodic_mem_season_anchor() places a summer-peaking pathogen's anchor in winter", {
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5, peak_month = 7)
  anchor <- episodic_mem_season_anchor(cases, config)
  expect_false(is.null(anchor))
  expect_true(anchor$anchor_week <= 10 || anchor$anchor_week >= 45)
})

test_that("episodic_mem_season_anchor() returns a well-formed list", {
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  anchor <- episodic_mem_season_anchor(cases, config)
  expect_true(all(c("anchor_week", "trough_run", "climatology") %in%
    names(anchor)))
  expect_true(is.integer(anchor$anchor_week))
  expect_true(anchor$anchor_week >= 1 && anchor$anchor_week <= 52)
  expect_length(anchor$climatology, 52)
})

# -- Seasonality test ---------------------------------------------------

test_that("episodic_mem_seasonality() returns seasonal = TRUE for a peaked series", {
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5, peak_month = 1)
  seas <- episodic_mem_seasonality(cases, config)
  expect_false(is.null(seas))
  expect_true(seas$seasonal)
  expect_true(seas$statistic > 0.40)
})

test_that("episodic_mem_seasonality() returns seasonal = FALSE for a flat+trend series", {
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_flat(n_years = 5)
  seas <- episodic_mem_seasonality(cases, config)
  expect_false(is.null(seas))
  expect_false(seas$seasonal)
})

test_that("episodic_mem_seasonality() returns NULL with insufficient history", {
  config <- episodic_config_resolve(NA)
  cases <- data.frame(
    sample_date = as.character(seq(
      as.Date("2023-01-01"),
      as.Date("2023-06-30"),
      by = "week"
    ))
  )
  expect_null(episodic_mem_seasonality(cases, config))
})

# -- Circular runs ------------------------------------------------------

test_that("episodic_mem_circular_runs() finds wrap-around runs", {
  runs <- episodic_mem_circular_runs(c(1, 2, 3, 50, 51, 52), 52)
  expect_true(length(runs) == 1)
  expect_true(50 %in% runs[[1]])
  expect_true(3 %in% runs[[1]])
})

test_that("episodic_mem_circular_runs() handles empty input", {
  expect_length(episodic_mem_circular_runs(integer(0), 52), 0)
})

test_that("episodic_mem_circular_runs() finds separate non-wrapping runs", {
  runs <- episodic_mem_circular_runs(c(5, 6, 7, 20, 21, 22), 52)
  expect_length(runs, 2)
})

# -- MEM status ---------------------------------------------------------

test_that("episodic_mem_status() returns NULL without an anchor", {
  skip_if_not_installed("mem")
  expect_null(episodic_mem_status(
    data.frame(sample_date = character(0)),
    run_date = as.Date("2024-01-15"),
    anchor = NULL
  ))
})

test_that("episodic_mem_status() returns NULL with empty cases", {
  skip_if_not_installed("mem")
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  anchor <- episodic_mem_season_anchor(cases, config)
  expect_null(episodic_mem_status(
    data.frame(sample_date = character(0)),
    run_date = as.Date("2024-01-15"),
    config = config,
    anchor = anchor
  ))
})

test_that("episodic_mem_status() takes its season requirement from the configuration, not a literal", {
  skip_if_not_installed("mem")
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  run_date <- as.Date("2024-01-15")
  config <- episodic_config_resolve(NA)
  anchor <- episodic_mem_season_anchor(cases, config)
  expect_false(is.null(anchor))

  expect_equal(as.integer(config$mem$min_seasons), 2L)
  expect_false(is.null(episodic_mem_status(cases, run_date, config, anchor)))

  config$mem$min_seasons <- 99
  expect_null(episodic_mem_status(cases, run_date, config, anchor))
})

test_that("episodic_mem_status() returns a well-formed status once enough seasons exist", {
  skip_if_not_installed("mem")
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  anchor <- episodic_mem_season_anchor(cases, config)
  status <- episodic_mem_status(cases,
    run_date = as.Date("2024-01-15"),
    config = config,
    anchor = anchor
  )
  expect_false(is.null(status))
  expect_true(all(
    c(
      "in_trough",
      "epidemic_started",
      "current_week_count",
      "pre_epidemic_threshold",
      "post_epidemic_threshold",
      "anchor_week",
      "week_start",
      "week_end"
    ) %in%
      names(status)
  ))
  expect_true(status$pre_epidemic_threshold <= status$post_epidemic_threshold)
  expect_equal(status$week_end, status$week_start + 6)
  expect_equal(status$anchor_week, anchor$anchor_week)
})

# -- Evaluation week ----------------------------------------------------

test_that("episodic_mem_evaluation_week() is the last fully-elapsed week, never the one in progress", {
  aw <- 40L
  wk <- episodic_mem_evaluation_week(as.Date("2024-01-17"), aw)
  expect_equal(wk$week_start, as.Date("2024-01-08"))
  expect_equal(wk$season, "2023/2024")
  expect_equal(
    episodic_mem_evaluation_week(as.Date("2024-01-15"), aw)$week_start,
    as.Date("2024-01-08")
  )
})

# -- ISO week start -----------------------------------------------------

test_that("episodic_iso_week_start() returns the Monday of an ISO week", {
  expect_equal(episodic_iso_week_start(2024, 1), as.Date("2024-01-01"))
  expect_equal(episodic_iso_week_start(2021, 1), as.Date("2021-01-04"))
  expect_equal(episodic_iso_week_start(2019, 40), as.Date("2019-09-30"))
  expect_equal(as.integer(format(episodic_iso_week_start(2023, 40), "%u")), 1L)
})

# -- Observed seasons ---------------------------------------------------

test_that("episodic_mem_observed_seasons() drops seasons the data does not span end to end", {
  aw <- 40L
  partial <- data.frame(
    sample_date = as.character(seq(
      as.Date("2021-01-01"),
      as.Date("2023-08-01"),
      by = "week"
    ))
  )
  kept <- episodic_mem_observed_seasons(
    c("2020/2021", "2021/2022", "2022/2023"),
    partial,
    aw
  )
  expect_equal(kept, "2021/2022")
  expect_equal(
    episodic_mem_observed_seasons(character(0), partial, aw),
    character(0)
  )
})

# -- Week order ---------------------------------------------------------

test_that("episodic_mem_week_order() returns 52 labels from anchor to anchor-1", {
  ord_40 <- episodic_mem_week_order(40L)
  expect_length(ord_40, 52)
  expect_equal(ord_40[1], "40")
  expect_equal(ord_40[52], "39")

  ord_1 <- episodic_mem_week_order(1L)
  expect_equal(ord_1[1], "1")
  expect_equal(ord_1[52], "52")
})

# -- Detection ----------------------------------------------------------

test_that("episodic_detect_mem() fires a detection during peak season", {
  skip_if_not_installed("mem")
  cases <- episodic_mem_synthetic_seasons(n_seasons = 6)
  det_peak <- episodic_detect_mem(
    cases,
    stream_id = 1L,
    run_date = as.Date("2024-01-15")
  )
  expect_equal(nrow(det_peak), 1)
  expect_equal(det_peak$detector[1], "mem")
  expect_equal(det_peak$stream_id[1], 1L)
})

test_that("episodic_detect_mem() returns an empty record with no cases", {
  expect_equal(
    nrow(episodic_detect_mem(data.frame(sample_date = character(0)), 1L)),
    0
  )
})

# -- mem_mode overrides -------------------------------------------------

test_that("mem_mode = 'no' suppresses MEM even for a seasonal pathogen", {
  skip_if_not_installed("mem")
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5)
  det <- episodic_detect_mem(
    cases,
    stream_id = 1L,
    run_date = as.Date("2024-01-15"),
    mem_mode = "no"
  )
  expect_equal(nrow(det), 0)
})

test_that("mem_mode = 'yes' forces MEM past a failing seasonality test", {
  skip_if_not_installed("mem")
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_flat(n_years = 5)

  seas <- episodic_mem_seasonality(cases, config)
  expect_false(is.null(seas))
  expect_false(seas$seasonal)

  det <- episodic_detect_mem(
    cases,
    stream_id = 1L,
    run_date = as.Date("2022-01-15"),
    config = config,
    mem_mode = "yes"
  )
  expect_true(nrow(det) <= 1)
})

# -- Summer-peaking pathogen fires in summer ----------------------------

test_that("summer-peaking series produces a winter anchor and can fire in summer", {
  skip_if_not_installed("mem")
  config <- episodic_config_resolve(NA)
  cases <- episodic_mem_synthetic_seasons(n_seasons = 5, peak_month = 7)

  anchor <- episodic_mem_season_anchor(cases, config)
  expect_false(is.null(anchor))
  expect_true(anchor$anchor_week <= 10 || anchor$anchor_week >= 45)

  seas <- episodic_mem_seasonality(cases, config)
  expect_false(is.null(seas))
  expect_true(seas$seasonal)

  status <- episodic_mem_status(cases,
    run_date = as.Date("2023-07-15"),
    config = config,
    anchor = anchor
  )
  expect_false(is.null(status))
})

# -- Schema migration ---------------------------------------------------

test_that("schema v6 migration adds mem_mode column", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  cols <- DBI::dbListFields(con, "episodic_pathogen_config")
  expect_true("mem_mode" %in% cols)
  expect_false("mem_applicable" %in% cols)

  pc <- DBI::dbGetQuery(con, "SELECT mem_mode FROM episodic_pathogen_config LIMIT 1")
  if (nrow(pc) > 0) {
    expect_true(pc$mem_mode[1] %in% c("auto", "yes", "no"))
  }
})
