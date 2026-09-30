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

# Schema version 12 adds the offspring distribution of an outbreak's cases
# (episodic_pathogen_config.end_r and end_k) and forecasts about one
# cluster (episodic_cluster_forecast and its values).

# A version-11 database: the current schema without any of it.
v12_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_cluster_forecast_value")
  DBI::dbExecute(con, "DROP TABLE episodic_cluster_forecast")
  DBI::dbExecute(con, "ALTER TABLE episodic_pathogen_config DROP COLUMN end_r")
  DBI::dbExecute(con, "ALTER TABLE episodic_pathogen_config DROP COLUMN end_k")
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 11 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("a fresh database has the offspring columns and the cluster forecast tables", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbExistsTable(con, "episodic_cluster_forecast"))
  expect_true(DBI::dbExistsTable(con, "episodic_cluster_forecast_value"))
  expect_true(all(c("end_r", "end_k") %in% DBI::dbListFields(con, "episodic_pathogen_config")))
})

test_that("migration 12 adds all of it, and a second pass changes nothing", {
  path <- v12_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))

  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_true(all(c("end_r", "end_k") %in% DBI::dbListFields(con, "episodic_pathogen_config")))
  expect_setequal(
    DBI::dbListFields(con, "episodic_cluster_forecast"),
    c(
      "cluster_forecast_id", "run_id", "cluster_id", "kind", "method",
      "status", "detail", "params", "created_at"
    )
  )
  expect_setequal(
    DBI::dbListFields(con, "episodic_cluster_forecast_value"),
    c("cluster_forecast_id", "target_date", "probability")
  )
  # The added columns carry their range checks.
  expect_error(DBI::dbExecute(
    con,
    "INSERT INTO episodic_pathogen_config (pathogen, end_r, end_k) VALUES ('X', -1, 0.5)"
  ))

  migration <- episodic_db_migrations()[["12"]]
  expect_no_error(migration(con, "sqlite"))
})
