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
