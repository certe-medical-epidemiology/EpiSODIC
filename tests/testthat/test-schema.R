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
