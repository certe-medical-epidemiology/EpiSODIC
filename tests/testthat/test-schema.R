test_that("episode_db_create() builds every expected table", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  tables <- DBI::dbListTables(con)
  expect_true(all(c(
    "episode_stream", "episode_stream_mute", "episode_detection_run",
    "episode_institution", "episode_institution_activity", "episode_pathogen_config",
    "episode_case", "episode_detection", "episode_cluster", "episode_cluster_case",
    "episode_assessment_event", "episode_reporting_triangle", "episode_denominator",
    "episode_cluster_state", "episode_app_user",
    "episode_report_render"
  ) %in% tables))
})

test_that("episode_db_create() refuses to overwrite an existing file without overwrite = TRUE", {
  path <- tempfile(fileext = ".sqlite")
  con1 <- episode_db_create(path)
  DBI::dbDisconnect(con1)
  expect_error(episode_db_create(path), "already exists")
  con2 <- episode_db_create(path, overwrite = TRUE)
  expect_true(DBI::dbIsValid(con2))
  DBI::dbDisconnect(con2)
})

test_that("episode_db_connect() errors on a missing file", {
  expect_error(episode_db_connect(tempfile()), "No database file")
})

test_that("WAL mode and foreign keys are enabled on connect", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_equal(tolower(DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1]]), "wal")
  expect_equal(DBI::dbGetQuery(con, "PRAGMA foreign_keys")[[1]], 1L)
})
