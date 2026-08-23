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
    episodic_synthetic_cases(
      start_date = as.Date("2024-06-01"),
      end_date = as.Date("2024-08-31"),
      seed = 3
    )
  }
  run_id <- episodic_run_cron(
    path,
    cases = small_source,
    run_date = as.Date("2024-08-31")
  )

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
    episodic_synthetic_cases(
      start_date = as.Date("2024-06-01"),
      end_date = as.Date("2024-06-30"),
      seed = 9
    )
  }
  episodic_run_cron(
    path,
    cases = small_source,
    run_date = as.Date("2024-06-30")
  )
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  n_cases_1 <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n
  DBI::dbDisconnect(con)

  episodic_run_cron(
    path,
    cases = small_source,
    run_date = as.Date("2024-06-30")
  )
  con <- episodic_db_connect(path)
  n_cases_2 <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n

  expect_equal(n_cases_1, n_cases_2) # same source_keys: no duplicate case rows
})

test_that("a second positive sent in a later, non-overlapping run still joins its earlier episode", {
  # The scenario "send a recent window, not the full history" depends on
  # this: two positives for the same patient/pathogen, within episode_days
  # of each other, arriving in two separate episodic_run_cron() calls
  # with different source_keys and no shared rows between them. Without
  # cross-run deduplication this would insert two cases instead of one.
  path <- tempfile(fileext = ".sqlite")
  raw_case <- function(source_key, sample_date) {
    data.frame(
      source_key = source_key,
      patient_key = "P1",
      sample_date = sample_date,
      receipt_date = sample_date,
      pathogen = "Test pathogen",
      care_line = "second",
      institution_key = "HOSP-01",
      institution_display_name = "Hospital",
      institution_type = "hospital",
      municipality = NA_character_,
      ward = "ICU",
      specialism = "Interne",
      pc = "9711",
      sex = "M",
      age = 40L,
      stringsAsFactors = FALSE
    )
  }

  episodic_run_cron(
    path,
    cases = raw_case("K1", "2025-01-01"),
    run_date = as.Date("2025-01-01")
  )
  # Test pathogen is not in inst/config/pathogen_config.csv, so it falls
  # back to the schema default of 30 days - day 20 is well within that.
  episodic_run_cron(
    path,
    cases = raw_case("K2", "2025-01-20"),
    run_date = as.Date("2025-01-20")
  )

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  cases <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_case WHERE patient_key = 'P1'"
  )
  expect_equal(nrow(cases), 1)
  expect_equal(cases$sample_date, "2025-01-01") # the episode's anchor, unchanged
})

test_that("episodic_run_cron() writes denominator rows only when a denominators is supplied", {
  path_without <- tempfile(fileext = ".sqlite")
  small_source <- function() {
    episodic_synthetic_cases(
      start_date = as.Date("2024-06-01"),
      end_date = as.Date("2024-06-30"),
      seed = 11
    )
  }
  episodic_run_cron(
    path_without,
    cases = small_source,
    run_date = as.Date("2024-06-30")
  )
  con_without <- episodic_db_connect(path_without)
  on.exit(DBI::dbDisconnect(con_without))
  expect_equal(
    DBI::dbGetQuery(
      con_without,
      "SELECT COUNT(*) n FROM episodic_denominator"
    )$n,
    0
  )

  path_with <- tempfile(fileext = ".sqlite")
  small_denom <- function() {
    episodic_synthetic_denominators(
      start_date = as.Date("2024-06-01"),
      end_date = as.Date("2024-06-30"),
      seed = 11
    )
  }
  episodic_run_cron(
    path_with,
    cases = small_source,
    denominators = small_denom,
    run_date = as.Date("2024-06-30")
  )
  con_with <- episodic_db_connect(path_with)
  on.exit(DBI::dbDisconnect(con_with), add = TRUE)
  expect_gt(
    DBI::dbGetQuery(con_with, "SELECT COUNT(*) n FROM episodic_denominator")$n,
    0
  )
})

test_that("episodic_run_cron() writes institution activity rows only when institution_activity is supplied", {
  small_source <- function() {
    episodic_synthetic_cases(
      start_date = as.Date("2024-06-01"),
      end_date = as.Date("2024-06-30"),
      seed = 11
    )
  }

  path_without <- tempfile(fileext = ".sqlite")
  episodic_run_cron(
    path_without,
    cases = small_source,
    run_date = as.Date("2024-06-30")
  )
  con_without <- episodic_db_connect(path_without)
  on.exit(DBI::dbDisconnect(con_without))
  expect_equal(
    DBI::dbGetQuery(
      con_without,
      "SELECT COUNT(*) n FROM episodic_institution_activity"
    )$n,
    0
  )

  path_with <- tempfile(fileext = ".sqlite")
  episodic_run_cron(
    path_with,
    cases = small_source,
    institution_activity = function(institutions) {
      episodic_synthetic_institution_activity(
        institutions,
        start_date = as.Date("2024-06-01"),
        end_date = as.Date("2024-06-30")
      )
    },
    run_date = as.Date("2024-06-30")
  )
  con_with <- episodic_db_connect(path_with)
  on.exit(DBI::dbDisconnect(con_with), add = TRUE)
  expect_gt(
    DBI::dbGetQuery(
      con_with,
      "SELECT COUNT(*) n FROM episodic_institution_activity"
    )$n,
    0
  )
})

test_that("a run records what each feed delivered, not just that it succeeded", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 3
  )
  denom <- episodic_synthetic_denominators(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30")
  )
  episodic_run_cron(
    path,
    cases = cases,
    denominators = denom,
    run_date = as.Date("2024-06-30")
  )

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  run <- episodic_db_latest_run(con)

  expect_identical(run$status, "success")
  expect_equal(run$n_cases_supplied, nrow(cases))
  expect_true(run$n_cases_deduplicated <= run$n_cases_supplied)
  expect_gt(run$n_cases_inserted, 0)
  expect_equal(run$n_denominators_written, nrow(denom))
  expect_true(is.na(run$n_activity_supplied)) # no activity feed was given
})

test_that("a run that skipped activity rows finishes 'partial', not 'success'", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 4
  )
  # an activity feed keyed on institutions the case feed never mentions -
  # the exact situation that used to report a clean success
  activity <- data.frame(
    institution_key = c("UNKNOWN-1", "UNKNOWN-2"),
    period_start = "2024-06-03",
    period_end = "2024-06-09",
    patient_days = 900,
    stringsAsFactors = FALSE
  )
  suppressWarnings(
    episodic_run_cron(
      path,
      cases = cases,
      institution_activity = activity,
      run_date = as.Date("2024-06-30")
    )
  )

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  run <- episodic_db_latest_run(con)

  expect_identical(run$status, "partial")
  expect_equal(run$n_activity_supplied, 2)
  expect_equal(run$n_activity_written, 0)
  expect_equal(run$n_activity_skipped, 2)
})

test_that("a partial run still counts as the latest usable run", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 5
  )
  activity <- data.frame(
    institution_key = "UNKNOWN-1",
    period_start = "2024-06-03",
    period_end = "2024-06-09",
    patient_days = 900,
    stringsAsFactors = FALSE
  )
  suppressWarnings(
    episodic_run_cron(
      path,
      cases = cases,
      institution_activity = activity,
      run_date = as.Date("2024-06-30")
    )
  )

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(
    episodic_db_latest_run(con, status = episodic_run_statuses_complete)$status,
    "partial"
  )
})

test_that("case data that violates the contract fails the run out loud, and records why", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 6
  )
  cases$pathogen <- NULL # violates the case data contract

  # Reaching the operator matters as much as recording it: a run that
  # returned quietly here left somebody staring at an empty dashboard.
  expect_error(
    episodic_run_cron(path, cases = cases, run_date = as.Date("2024-06-30")),
    "pathogen"
  )

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  run <- episodic_db_latest_run(con)

  expect_identical(run$status, "failed")
  expect_true(is.na(run$n_cases_supplied))
  expect_true(is.na(run$n_cases_inserted))
  expect_match(run$error_text, "pathogen")
  # the message an operator can act on, not only the fact of a failure
  expect_match(run$error_text, "missing required", fixed = TRUE)
})

test_that("a run over an empty extract warns rather than quietly writing nothing", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 6
  )

  expect_warning(
    episodic_run_cron(
      path,
      cases = cases[0, ],
      run_date = as.Date("2024-06-30")
    ),
    "no rows"
  )
})

test_that("an NA care_line is stored as 'unknown', not rejected", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 7
  )
  cases$care_line <- NA_character_

  episodic_run_cron(path, cases = cases, run_date = as.Date("2024-06-30"))

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  stored <- DBI::dbGetQuery(con, "SELECT DISTINCT care_line FROM episodic_case")
  expect_identical(stored$care_line, "unknown")
})

test_that("episodic_resolve_data() accepts a data frame, a function, or NULL, and errors otherwise", {
  df <- data.frame(x = 1)
  expect_null(episodic_resolve_data(NULL))
  expect_identical(episodic_resolve_data(df), df)
  expect_identical(episodic_resolve_data(function() df), df)
  expect_identical(episodic_resolve_data(function(y) y, 5), 5)
  expect_error(episodic_resolve_data(1), "data frame")
})

test_that("episodic_run_cron() accepts a data frame directly for cases/denominators/institution_activity, not only a function", {
  small_cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 11
  )
  small_denom <- episodic_synthetic_denominators(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 11
  )

  path <- tempfile(fileext = ".sqlite")
  episodic_run_cron(
    path,
    cases = small_cases,
    denominators = small_denom,
    run_date = as.Date("2024-06-30")
  )
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n, 0)
  expect_gt(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_denominator")$n,
    0
  )

  institutions <- episodic_db_institutions(con)
  small_activity <- episodic_synthetic_institution_activity(
    institutions,
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30")
  )
  path2 <- tempfile(fileext = ".sqlite")
  episodic_run_cron(
    path2,
    cases = small_cases,
    institution_activity = small_activity,
    run_date = as.Date("2024-06-30")
  )
  con2 <- episodic_db_connect(path2)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_gt(
    DBI::dbGetQuery(
      con2,
      "SELECT COUNT(*) n FROM episodic_institution_activity"
    )$n,
    0
  )
})

test_that("episodic_lattice_enumerate() creates distinct streams per level", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episodic_db_institution_upsert(
    con,
    institution_key = digest::digest("hosp", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital",
    institution_type = "hospital",
    care_line = "second",
    is_monitored = TRUE
  )
  cases <- data.frame(
    pathogen = "Test pathogen",
    sample_date = "2025-01-01",
    care_line = "second",
    institution_id = institution_id,
    ward = "ICU",
    pc = "9711",
    stringsAsFactors = FALSE
  )
  institutions <- episodic_db_institutions(con)
  episodic_lattice_enumerate(con, cases, institutions)

  streams <- episodic_db_streams(con)
  expect_setequal(
    streams$level,
    c(
      "pathogen_ward",
      "pathogen_institution",
      "pathogen_area",
      "pathogen_province",
      "pathogen_region"
    )
  )
})

test_that("a geographic stream gets its own area's cases, not the whole region's", {
  # The bug this guards: only lattice enumeration knew how a case maps to
  # a region code, so every area and province stream was handed the whole
  # catchment and reported the region's counts under its own name - one
  # signal, and a cluster per area to go with it.
  cases <- data.frame(
    pathogen = "Norovirus",
    institution_id = NA_integer_,
    ward = NA_character_,
    pc = c("9711", "9712", "8911", "7811", NA),
    stringsAsFactors = FALSE
  )
  stream <- function(level, region_code) {
    data.frame(
      level = level,
      pathogen = "Norovirus",
      institution_id = NA_integer_,
      ward = NA_character_,
      region_code = region_code,
      stringsAsFactors = FALSE
    )
  }

  area <- episodic_cases_for_stream(cases, stream("pathogen_area", "GEBIED-97"))
  expect_equal(area$pc, c("9711", "9712"))

  province <- episodic_cases_for_stream(
    cases,
    stream("pathogen_province", "PROV_GRONINGEN")
  )
  expect_equal(province$pc, c("9711", "9712"))

  # the whole catchment is every case, including the one with no postcode
  # to place it by
  region <- episodic_cases_for_stream(
    cases,
    stream("pathogen_region", episodic_region_code_all)
  )
  expect_equal(nrow(region), 5)
})

test_that("episodic_case_region_code() places a case the same way at every level", {
  cases <- data.frame(
    pc = c("9711", "8911", "7811", NA),
    stringsAsFactors = FALSE
  )
  expect_equal(
    episodic_case_region_code(cases, "pathogen_area"),
    c("GEBIED-97", "GEBIED-89", "GEBIED-78", NA)
  )
  expect_equal(
    episodic_case_region_code(cases, "pathogen_province"),
    c("PROV_GRONINGEN", "PROV_FRYSLAN", "PROV_DRENTHE", NA)
  )
  expect_equal(
    episodic_case_region_code(cases, "pathogen_region"),
    rep(episodic_region_code_all, 4)
  )
  # a level with no geography places nothing
  expect_true(all(is.na(episodic_case_region_code(cases, "pathogen_ward"))))
  expect_equal(
    episodic_case_region_code(cases[0, , drop = FALSE], "pathogen_area"),
    character(0)
  )
})
