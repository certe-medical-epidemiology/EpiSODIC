test_that("episode_synthetic_institution_activity_source() returns weekly rows for hospitals only", {
  institutions <- data.frame(
    institution_key = c("h1", "h2", "l1"), n_beds = c(400, 200, 80),
    institution_type = c("hospital", "hospital", "ltc_institution"), stringsAsFactors = FALSE
  )
  activity <- episode_synthetic_institution_activity_source(
    institutions, start_date = as.Date("2025-01-06"), end_date = as.Date("2025-02-03")
  )
  expect_setequal(unique(activity$institution_key), c("h1", "h2"))
  expect_true(all(activity$patient_days > 0))
  expect_true(all(activity$patient_days <= activity$n_beds * 7 * 1.1))  # sanity: never wildly above capacity
})

test_that("episode_synthetic_institution_activity_source() returns zero rows when there are no hospitals", {
  institutions <- data.frame(institution_key = "l1", n_beds = 80, institution_type = "ltc_institution",
                              stringsAsFactors = FALSE)
  activity <- episode_synthetic_institution_activity_source(institutions)
  expect_equal(nrow(activity), 0)
})

test_that("episode_institution_activity_ingest_run() resolves institution_key to institution_id and writes rows", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  institution <- episode_db_institutions(env$con)

  activity <- data.frame(
    institution_key = institution$institution_key[1], period_start = "2025-03-01", period_end = "2025-03-07",
    patient_days = 500, stringsAsFactors = FALSE
  )
  n <- episode_institution_activity_ingest_run(env$con, activity)
  expect_equal(n, 1)

  rows <- episode_db_institution_activity(env$con, institution$institution_id[1])
  expect_true("2025-03-01" %in% rows$period_start)
})

test_that("episode_institution_activity_ingest_run() skips rows for an unknown institution_key without erroring", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  activity <- data.frame(
    institution_key = "does-not-exist", period_start = "2025-03-01", period_end = "2025-03-07",
    patient_days = 500, stringsAsFactors = FALSE
  )
  expect_equal(episode_institution_activity_ingest_run(env$con, activity), 0)
})

test_that("episode_institution_activity_ingest_run() errors clearly when required columns are missing", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_error(
    episode_institution_activity_ingest_run(env$con, data.frame(institution_key = "x")),
    "required column"
  )
})
