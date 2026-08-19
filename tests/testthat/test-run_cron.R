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

test_that("episode_lattice_enumerate() creates distinct streams per level", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second",
    is_monitored = TRUE
  )
  cases <- data.frame(
    mo_code = "TEST_MO", sample_date = "2025-01-01", care_line = "second",
    institution_id = institution_id, ward = "ICU", pc4 = "9711", stringsAsFactors = FALSE
  )
  institutions <- episode_db_institutions(con)
  episode_lattice_enumerate(con, cases, institutions)

  streams <- episode_db_streams(con)
  expect_setequal(
    streams$level,
    c("pathogen_ward", "pathogen_institution", "pathogen_area", "pathogen_province", "pathogen_region")
  )
})
