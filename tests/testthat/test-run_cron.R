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

test_that("episode_run_cron() completes successfully end to end on a small synthetic window", {
  path <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episode_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-08-31"), seed = 3
    )
  }
  run_id <- episode_run_cron(path, ingest_source_fn = small_source, run_date = as.Date("2024-08-31"))

  con <- episode_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  run <- episode_db_latest_run(con)
  expect_equal(run$status, "success")
  expect_false(is.na(run$config_hash))
  expect_equal(nchar(run$config_hash), 40)
  expect_true(nchar(run$config_snapshot) > 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n, 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_stream")$n, 0)
})

test_that("running the cron twice over the same window is idempotent on case counts", {
  path <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episode_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 9
    )
  }
  episode_run_cron(path, ingest_source_fn = small_source, run_date = as.Date("2024-06-30"))
  con <- episode_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  n_cases_1 <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n
  DBI::dbDisconnect(con)

  episode_run_cron(path, ingest_source_fn = small_source, run_date = as.Date("2024-06-30"))
  con <- episode_db_connect(path)
  n_cases_2 <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n

  expect_equal(n_cases_1, n_cases_2)  # same source_keys: no duplicate case rows
})

test_that("episode_run_cron() writes denominator rows only when a denominator_source_fn is supplied", {
  path_without <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episode_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
    )
  }
  episode_run_cron(path_without, ingest_source_fn = small_source, run_date = as.Date("2024-06-30"))
  con_without <- episode_db_connect(path_without)
  on.exit(DBI::dbDisconnect(con_without))
  expect_equal(DBI::dbGetQuery(con_without, "SELECT COUNT(*) n FROM episode_denominator")$n, 0)

  path_with <- tempfile(fileext = ".sqlite")
  small_denom <- function() {
    episode_denominator_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
    )
  }
  episode_run_cron(path_with, ingest_source_fn = small_source, denominator_source_fn = small_denom,
                    run_date = as.Date("2024-06-30"))
  con_with <- episode_db_connect(path_with)
  on.exit(DBI::dbDisconnect(con_with), add = TRUE)
  expect_gt(DBI::dbGetQuery(con_with, "SELECT COUNT(*) n FROM episode_denominator")$n, 0)
})

test_that("episode_run_cron() writes institution activity rows only when institution_activity_source_fn is supplied", {
  small_source <- function() {
    episode_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
    )
  }

  path_without <- tempfile(fileext = ".sqlite")
  episode_run_cron(path_without, ingest_source_fn = small_source, run_date = as.Date("2024-06-30"))
  con_without <- episode_db_connect(path_without)
  on.exit(DBI::dbDisconnect(con_without))
  expect_equal(DBI::dbGetQuery(con_without, "SELECT COUNT(*) n FROM episode_institution_activity")$n, 0)

  path_with <- tempfile(fileext = ".sqlite")
  episode_run_cron(path_with, ingest_source_fn = small_source,
                    institution_activity_source_fn = function(institutions) {
                      episode_synthetic_institution_activity_source(
                        institutions, start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30")
                      )
                    },
                    run_date = as.Date("2024-06-30"))
  con_with <- episode_db_connect(path_with)
  on.exit(DBI::dbDisconnect(con_with), add = TRUE)
  expect_gt(DBI::dbGetQuery(con_with, "SELECT COUNT(*) n FROM episode_institution_activity")$n, 0)
})

test_that("episode_resolve_source() accepts a function, a data frame, or NULL, and errors otherwise", {
  df <- data.frame(x = 1)
  expect_null(episode_resolve_source(NULL))
  expect_identical(episode_resolve_source(df), df)
  expect_identical(episode_resolve_source(function() df), df)
  expect_identical(episode_resolve_source(function(y) y, 5), 5)
  expect_error(episode_resolve_source(1), "function or a data frame")
})

test_that("episode_run_cron() accepts a data frame directly for ingest/denominator/institution_activity_source_fn, not only a function", {
  small_cases <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
  )
  small_denom <- episode_denominator_source_synthetic(
    start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
  )

  path <- tempfile(fileext = ".sqlite")
  episode_run_cron(path, ingest_source_fn = small_cases, denominator_source_fn = small_denom,
                    run_date = as.Date("2024-06-30"))
  con <- episode_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n, 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_denominator")$n, 0)

  institutions <- episode_db_institutions(con)
  small_activity <- episode_synthetic_institution_activity_source(
    institutions, start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30")
  )
  path2 <- tempfile(fileext = ".sqlite")
  episode_run_cron(path2, ingest_source_fn = small_cases, institution_activity_source_fn = small_activity,
                    run_date = as.Date("2024-06-30"))
  con2 <- episode_db_connect(path2)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_gt(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM episode_institution_activity")$n, 0)
})

test_that("episode_lattice_enumerate() creates distinct streams per level", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second",
    is_monitored = TRUE
  )
  cases <- data.frame(
    pathogen = "Test organism", sample_date = "2025-01-01", care_line = "second",
    institution_id = institution_id, ward = "ICU", pc = "9711", stringsAsFactors = FALSE
  )
  institutions <- episode_db_institutions(con)
  episode_lattice_enumerate(con, cases, institutions)

  streams <- episode_db_streams(con)
  expect_setequal(
    streams$level,
    c("pathogen_ward", "pathogen_institution", "pathogen_area", "pathogen_province", "pathogen_region")
  )
})
