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

test_that("episodic_run_cron() completes successfully end to end on a small synthetic window", {
  path <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episodic_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-08-31"), seed = 3
    )
  }
  run_id <- episodic_run_cron(path, ingest_source = small_source, run_date = as.Date("2024-08-31"))

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  run <- episodic_db_latest_run(con)
  expect_equal(run$status, "success")
  expect_false(is.na(run$config_hash))
  expect_equal(nchar(run$config_hash), 40)
  expect_true(nchar(run$config_snapshot) > 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n, 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_stream")$n, 0)
})

test_that("running the cron twice over the same window is idempotent on case counts", {
  path <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episodic_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 9
    )
  }
  episodic_run_cron(path, ingest_source = small_source, run_date = as.Date("2024-06-30"))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  n_cases_1 <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n
  DBI::dbDisconnect(con)

  episodic_run_cron(path, ingest_source = small_source, run_date = as.Date("2024-06-30"))
  con <- episodic_db_connect(path)
  n_cases_2 <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n

  expect_equal(n_cases_1, n_cases_2)  # same source_keys: no duplicate case rows
})

test_that("episodic_run_cron() writes denominator rows only when a denominator_source is supplied", {
  path_without <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episodic_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
    )
  }
  episodic_run_cron(path_without, ingest_source = small_source, run_date = as.Date("2024-06-30"))
  con_without <- episodic_db_connect(path_without)
  on.exit(DBI::dbDisconnect(con_without))
  expect_equal(DBI::dbGetQuery(con_without, "SELECT COUNT(*) n FROM episodic_denominator")$n, 0)

  path_with <- tempfile(fileext = ".sqlite")
  small_denom <- function() {
    episodic_denominator_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
    )
  }
  episodic_run_cron(path_with, ingest_source = small_source, denominator_source = small_denom,
                    run_date = as.Date("2024-06-30"))
  con_with <- episodic_db_connect(path_with)
  on.exit(DBI::dbDisconnect(con_with), add = TRUE)
  expect_gt(DBI::dbGetQuery(con_with, "SELECT COUNT(*) n FROM episodic_denominator")$n, 0)
})

test_that("episodic_run_cron() writes institution activity rows only when institution_activity_source is supplied", {
  small_source <- function() {
    episodic_ingest_source_synthetic(
      start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
    )
  }

  path_without <- tempfile(fileext = ".sqlite")
  episodic_run_cron(path_without, ingest_source = small_source, run_date = as.Date("2024-06-30"))
  con_without <- episodic_db_connect(path_without)
  on.exit(DBI::dbDisconnect(con_without))
  expect_equal(DBI::dbGetQuery(con_without, "SELECT COUNT(*) n FROM episodic_institution_activity")$n, 0)

  path_with <- tempfile(fileext = ".sqlite")
  episodic_run_cron(path_with, ingest_source = small_source,
                    institution_activity_source = function(institutions) {
                      episodic_synthetic_institution_activity_source(
                        institutions, start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30")
                      )
                    },
                    run_date = as.Date("2024-06-30"))
  con_with <- episodic_db_connect(path_with)
  on.exit(DBI::dbDisconnect(con_with), add = TRUE)
  expect_gt(DBI::dbGetQuery(con_with, "SELECT COUNT(*) n FROM episodic_institution_activity")$n, 0)
})

test_that("episodic_resolve_source() accepts a data frame, a function, or NULL, and errors otherwise", {
  df <- data.frame(x = 1)
  expect_null(episodic_resolve_source(NULL))
  expect_identical(episodic_resolve_source(df), df)
  expect_identical(episodic_resolve_source(function() df), df)
  expect_identical(episodic_resolve_source(function(y) y, 5), 5)
  expect_error(episodic_resolve_source(1), "data frame")
})

test_that("episodic_run_cron() accepts a data frame directly for ingest/denominator/institution_activity_source, not only a function", {
  small_cases <- episodic_ingest_source_synthetic(
    start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
  )
  small_denom <- episodic_denominator_source_synthetic(
    start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30"), seed = 11
  )

  path <- tempfile(fileext = ".sqlite")
  episodic_run_cron(path, ingest_source = small_cases, denominator_source = small_denom,
                    run_date = as.Date("2024-06-30"))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n, 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_denominator")$n, 0)

  institutions <- episodic_db_institutions(con)
  small_activity <- episodic_synthetic_institution_activity_source(
    institutions, start_date = as.Date("2024-06-01"), end_date = as.Date("2024-06-30")
  )
  path2 <- tempfile(fileext = ".sqlite")
  episodic_run_cron(path2, ingest_source = small_cases, institution_activity_source = small_activity,
                    run_date = as.Date("2024-06-30"))
  con2 <- episodic_db_connect(path2)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_gt(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM episodic_institution_activity")$n, 0)
})

test_that("episodic_lattice_enumerate() creates distinct streams per level", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episodic_db_institution_upsert(
    con, institution_key = digest::digest("hosp", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second",
    is_monitored = TRUE
  )
  cases <- data.frame(
    pathogen = "Test pathogen", sample_date = "2025-01-01", care_line = "second",
    institution_id = institution_id, ward = "ICU", pc = "9711", stringsAsFactors = FALSE
  )
  institutions <- episodic_db_institutions(con)
  episodic_lattice_enumerate(con, cases, institutions)

  streams <- episodic_db_streams(con)
  expect_setequal(
    streams$level,
    c("pathogen_ward", "pathogen_institution", "pathogen_area", "pathogen_province", "pathogen_region")
  )
})
