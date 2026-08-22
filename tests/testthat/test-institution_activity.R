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

test_that("episodic_synthetic_institution_activity() returns weekly rows for hospitals only", {
  institutions <- data.frame(
    institution_key = c("h1", "h2", "l1"),
    n_beds = c(400, 200, 80),
    institution_type = c("hospital", "hospital", "ltc_institution"),
    stringsAsFactors = FALSE
  )
  activity <- episodic_synthetic_institution_activity(
    institutions,
    start_date = as.Date("2025-01-06"),
    end_date = as.Date("2025-02-03")
  )
  expect_setequal(unique(activity$institution_key), c("h1", "h2"))
  expect_true(all(activity$patient_days > 0))
  expect_true(all(activity$patient_days <= activity$n_beds * 7 * 1.1)) # sanity: never wildly above capacity
})

test_that("episodic_synthetic_institution_activity() returns zero rows when there are no hospitals", {
  institutions <- data.frame(
    institution_key = "l1",
    n_beds = 80,
    institution_type = "ltc_institution",
    stringsAsFactors = FALSE
  )
  activity <- episodic_synthetic_institution_activity(institutions)
  expect_equal(nrow(activity), 0)
})

test_that("episodic_institution_activity_load() resolves institution_key to institution_id and writes rows", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  institution <- episodic_db_institutions(env$con)

  activity <- data.frame(
    institution_key = institution$institution_key[1],
    period_start = "2025-03-01",
    period_end = "2025-03-07",
    patient_days = 500,
    stringsAsFactors = FALSE
  )
  counts <- episodic_institution_activity_load(env$con, activity)
  expect_equal(counts$n_supplied, 1)
  expect_equal(counts$n_written, 1)
  expect_equal(counts$n_skipped, 0)

  rows <- episodic_db_institution_activity(
    env$con,
    institution$institution_id[1]
  )
  expect_true("2025-03-01" %in% rows$period_start)
})

test_that("episodic_institution_activity_load() skips rows for an unknown institution_key without erroring", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  activity <- data.frame(
    institution_key = "does-not-exist",
    period_start = "2025-03-01",
    period_end = "2025-03-07",
    patient_days = 500,
    stringsAsFactors = FALSE
  )
  counts <- suppressWarnings(episodic_institution_activity_load(
    env$con,
    activity
  ))
  expect_equal(counts$n_written, 0)
  expect_equal(counts$n_skipped, 1)
})

test_that("episodic_institution_activity_load() warns, naming the unmatched keys, rather than skipping in silence", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  activity <- data.frame(
    institution_key = c("ghost-a", "ghost-b"),
    period_start = "2025-03-01",
    period_end = "2025-03-07",
    patient_days = 500,
    stringsAsFactors = FALSE
  )
  expect_warning(
    episodic_institution_activity_load(env$con, activity),
    "ghost-a"
  )
  expect_warning(
    episodic_institution_activity_load(env$con, activity),
    "2 of 2"
  )
})

test_that("episodic_institution_activity_load() is silent when nothing is skipped", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  institution <- episodic_db_institutions(env$con)
  activity <- data.frame(
    institution_key = institution$institution_key[1],
    period_start = "2025-03-01",
    period_end = "2025-03-07",
    patient_days = 500,
    stringsAsFactors = FALSE
  )
  expect_silent(episodic_institution_activity_load(env$con, activity))
})

test_that("episodic_institution_activity_load() rejects an unparseable period date", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  institution <- episodic_db_institutions(env$con)
  activity <- data.frame(
    institution_key = institution$institution_key[1],
    period_start = "01-03-2025",
    period_end = "2025-03-07",
    patient_days = 500,
    stringsAsFactors = FALSE
  )
  expect_error(
    episodic_institution_activity_load(env$con, activity),
    "period_start"
  )
})

test_that("episodic_institution_activity_load() errors clearly when required columns are missing", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_error(
    episodic_institution_activity_load(
      env$con,
      data.frame(institution_key = "x")
    ),
    "required column"
  )
})
