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

test_that("a curve's nowcast note says what is drawn, or why nothing is", {
  expect_null(episodic_ui_nowcast_note(NULL, drawn = FALSE, lang = "en"))
  computed <- list(status = "computed", detail = NA_character_)
  expect_match(
    episodic_ui_nowcast_note(computed, drawn = TRUE, lang = "en"),
    "median and 90% interval",
    fixed = TRUE
  )
  # A computed nowcast with nothing inside this curve's range has nothing
  # to explain.
  expect_null(episodic_ui_nowcast_note(computed, drawn = FALSE, lang = "en"))
  expect_match(
    episodic_ui_nowcast_note(
      list(status = "insufficient_data", detail = "reporting_history"),
      drawn = FALSE,
      lang = "en"
    ),
    "too few cases",
    fixed = TRUE
  )
  expect_match(
    episodic_ui_nowcast_note(
      list(status = "insufficient_data", detail = "completeness"),
      drawn = FALSE,
      lang = "en"
    ),
    "none of the most recent days",
    fixed = TRUE
  )
  expect_match(
    episodic_ui_nowcast_note(
      list(status = "failed", detail = "boom"),
      drawn = FALSE,
      lang = "en"
    ),
    "failed in the latest run",
    fixed = TRUE
  )
  expect_error(
    episodic_ui_nowcast_note(list(status = "other"), drawn = FALSE, lang = "en"),
    "Unknown nowcast status"
  )
})

test_that("nowcast marks are drawn only where a curve has them", {
  curve <- data.frame(
    sample_date = seq(as.Date("2025-01-01"), by = "day", length.out = 5),
    n_cases = c(3L, 2L, 4L, 1L, 0L),
    incomplete = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_length(episodic_chart_nowcast_layers(curve, x = "sample_date"), 0)
  curve$nowcast_low <- c(NA, NA, 4, 1, 0)
  curve$nowcast_mid <- c(NA, NA, 5, 3, 1)
  curve$nowcast_high <- c(NA, NA, 7, 6, 4)
  expect_length(episodic_chart_nowcast_layers(curve, x = "sample_date"), 2)

  plot <- episodic_ui_epi_curve_chart(curve, lang = "en")
  built <- ggplot2::ggplot_build(plot)
  # Bars, the interval lines and the median points; the y axis reaches
  # the top of the highest interval.
  expect_length(built$data, 3)
  expect_identical(nrow(built$data[[2]]), 3L)
  expect_gte(max(built$layout$panel_params[[1]]$y.range), 7)

  weekly <- data.frame(
    week_start = seq(as.Date("2025-01-06"), by = "week", length.out = 3),
    n_cases = c(10L, 12L, 6L),
    incomplete = c(FALSE, TRUE, TRUE),
    nowcast_low = c(NA, 12, 8),
    nowcast_mid = c(NA, 13, 11),
    nowcast_high = c(NA, 15, 16)
  )
  built <- ggplot2::ggplot_build(episodic_ui_pathogen_curve_chart(weekly, lang = "en"))
  expect_identical(nrow(built$data[[length(built$data)]]), 2L)
})

test_that("the dossier curve carries the stream's latest nowcast from the cluster's first case on", {
  db <- nowcast_cron_database()
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  open <- episodic_db_clusters_not_closed(con, "outbreak")
  expect_gt(nrow(open), 0)
  cluster_id <- open$cluster_id[1]
  obj <- episodic_cluster_object(con, cluster_id, lang = "en")
  expect_identical(obj$nowcast$status, "computed")

  curve <- episodic_app_epi_curve(
    con,
    cluster_id,
    completeness = obj$completeness,
    nowcast = obj$nowcast
  )
  days <- obj$nowcast$values[obj$nowcast$values$resolution == "day", ]
  days <- days[as.Date(days$target_date) >= min(curve$sample_date), ]
  expect_gt(nrow(days), 0)
  # Every nowcast day inside the cluster's span is on the curve, with its
  # quantiles; a day without one has none, not a zero.
  at <- match(as.Date(days$target_date), curve$sample_date)
  expect_false(anyNA(at))
  expect_identical(curve$nowcast_mid[at], days$q50)
  expect_identical(curve$nowcast_low[at], days$q05)
  expect_identical(curve$nowcast_high[at], days$q95)
  expect_true(all(is.na(curve$nowcast_mid[-at])))
  expect_equal(max(curve$sample_date), max(c(
    as.Date(days$target_date),
    as.Date(episodic_db_cluster_cases(con, cluster_id)$sample_date)
  )))

  html <- as.character(episodic_ui_epicurve_panel(con, cluster_id, obj, lang = "en"))
  expect_match(html, "median and 90% interval", fixed = TRUE)
})

test_that("a closed cluster does not carry its stream's nowcast", {
  db <- nowcast_cron_database()
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  cluster_id <- episodic_db_clusters_not_closed(con, "outbreak")$cluster_id[1]
  expect_false(is.null(episodic_cluster_object(con, cluster_id, lang = "en")$nowcast))
  user_id <- episodic_db_app_user_insert(
    con,
    "tester",
    "Test User",
    "t@example.com",
    "hash"
  )
  episodic_app_submit_closure(con, cluster_id, user_id)
  obj <- episodic_cluster_object(con, cluster_id, lang = "en")
  expect_null(obj$nowcast)
  curve <- episodic_app_epi_curve(con, cluster_id, nowcast = obj$nowcast)
  expect_true(all(is.na(curve$nowcast_mid)))
})

test_that("a curve without a nowcast has the columns, all NA", {
  db <- nowcast_cron_database(n_runs = 1L)
  on.exit(unlink(c(db$path, db$config_path)))
  con <- episodic_db_connect(db$path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  cluster_id <- episodic_db_clusters_not_closed(con, "outbreak")$cluster_id[1]
  obj <- episodic_cluster_object(con, cluster_id, lang = "en")
  expect_identical(obj$nowcast$status, "insufficient_data")
  curve <- episodic_app_epi_curve(con, cluster_id, nowcast = obj$nowcast)
  expect_true(all(is.na(curve$nowcast_mid)))
  html <- as.character(episodic_ui_epicurve_panel(con, cluster_id, obj, lang = "en"))
  expect_match(html, "too few cases", fixed = TRUE)
})
