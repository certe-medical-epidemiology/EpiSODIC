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

# Schema version 11 adds forecasts per run (episodic_forecast) and their
# values (episodic_forecast_value), filled from the next detection run on.

# A version-10 database: the current schema without either table.
v11_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_forecast_value")
  DBI::dbExecute(con, "DROP TABLE episodic_forecast")
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 10 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("a fresh database has both forecast tables", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbExistsTable(con, "episodic_forecast"))
  expect_true(DBI::dbExistsTable(con, "episodic_forecast_value"))
})

test_that("migration 11 adds both tables, and a second pass changes nothing", {
  path <- v11_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))

  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_setequal(
    DBI::dbListFields(con, "episodic_forecast"),
    c(
      "forecast_id", "run_id", "stream_id", "kind", "method", "status",
      "detail", "params", "created_at"
    )
  )
  expect_setequal(
    DBI::dbListFields(con, "episodic_forecast_value"),
    c(
      "forecast_id", "resolution", "target_date", "n_observed", "mean",
      names(episodic_forecast_probs)
    )
  )

  migration <- episodic_db_migrations()[["11"]]
  expect_no_error(migration(con, "sqlite"))
})
