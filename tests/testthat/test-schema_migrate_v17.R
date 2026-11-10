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

# Schema version 17 adds the tests behind statistical detections:
# episodic_detection.p_value and .alpha, episodic_stream_trend.p_value,
# and episodic_detection_run_detector.

v17_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_detection_run_detector")
  DBI::dbExecute(con, "ALTER TABLE episodic_detection DROP COLUMN p_value")
  DBI::dbExecute(con, "ALTER TABLE episodic_detection DROP COLUMN alpha")
  DBI::dbExecute(con, "ALTER TABLE episodic_stream_trend DROP COLUMN p_value")
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key("pathogen_region", "X", NA, "R1", NA, NA),
    level = "pathogen_region",
    pathogen = "X",
    care_line = NA,
    region_code = "R1",
    institution_id = NA,
    ward = NA,
    observed_date = "2026-01-05"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_stream_trend (stream_id, week_start, n_cases, expected, upperbound)
     VALUES (?, '2026-01-05', 4, 2.5, 6.1)",
    params = list(stream_id)
  )
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 16 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("migration 17 adds the test columns and table, keeps rows, and a second pass changes nothing", {
  path <- v17_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_true(DBI::dbExistsTable(con, "episodic_detection_run_detector"))
  expect_true(all(c("p_value", "alpha") %in% DBI::dbListFields(con, "episodic_detection")))
  expect_true("p_value" %in% DBI::dbListFields(con, "episodic_stream_trend"))

  # A row from before the migration has no p-value, not a p-value of 0.
  trend <- DBI::dbGetQuery(con, "SELECT n_cases, upperbound, p_value FROM episodic_stream_trend")
  expect_equal(trend$n_cases, 4L)
  expect_equal(trend$upperbound, 6.1)
  expect_true(is.na(trend$p_value))

  fields <- lapply(
    c("episodic_detection", "episodic_stream_trend", "episodic_detection_run_detector"),
    function(table) DBI::dbListFields(con, table)
  )
  expect_no_error(episodic_db_migrations()[["17"]](con, "sqlite"))
  expect_identical(
    lapply(
      c("episodic_detection", "episodic_stream_trend", "episodic_detection_run_detector"),
      function(table) DBI::dbListFields(con, table)
    ),
    fields
  )
})

test_that("a p-value outside [0, 1] is refused by the schema", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key("pathogen_region", "X", NA, "R1", NA, NA),
    level = "pathogen_region",
    pathogen = "X",
    care_line = NA,
    region_code = "R1",
    institution_id = NA,
    ward = NA,
    observed_date = "2026-01-05"
  )
  expect_error(DBI::dbExecute(
    con,
    "INSERT INTO episodic_stream_trend (stream_id, week_start, n_cases, p_value)
     VALUES (?, '2026-01-05', 4, 1.5)",
    params = list(stream_id)
  ))
})
