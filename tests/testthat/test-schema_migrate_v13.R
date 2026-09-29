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

# Schema version 13 adds the typical past season of a seasonal epidemic
# (the typical_* columns of episodic_epidemic_season and
# episodic_epidemic_typical_week) and a cluster forecast's estimates
# (episodic_cluster_forecast_estimate).

# A version-12 database: the current schema without any of it.
v13_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_epidemic_typical_week")
  DBI::dbExecute(con, "DROP TABLE episodic_cluster_forecast_estimate")
  for (column in v13_typical_columns) {
    DBI::dbExecute(con, paste("ALTER TABLE episodic_epidemic_season DROP COLUMN", column))
  }
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 12 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("a fresh database has the typical season and the cluster forecast estimates", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbExistsTable(con, "episodic_epidemic_typical_week"))
  expect_true(DBI::dbExistsTable(con, "episodic_cluster_forecast_estimate"))
  expect_true(all(v13_typical_columns %in% DBI::dbListFields(con, "episodic_epidemic_season")))
})

test_that("migration 13 adds all of it, and a second pass changes nothing", {
  path <- v13_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))

  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_true(all(v13_typical_columns %in% DBI::dbListFields(con, "episodic_epidemic_season")))
  expect_setequal(
    DBI::dbListFields(con, "episodic_epidemic_typical_week"),
    c("cluster_id", "week_offset", "lower", "middle", "upper")
  )
  expect_setequal(
    DBI::dbListFields(con, "episodic_cluster_forecast_estimate"),
    c("cluster_forecast_id", "quantity", "estimate", "lower", "upper", "interval_level")
  )
  # The added columns carry their range checks.
  expect_error(DBI::dbExecute(
    con,
    "INSERT INTO episodic_epidemic_season (cluster_id, season_label, anchor_week, typical_onset_week)
     VALUES (1, '2024/2025', 30, 54)"
  ))
  expect_error(DBI::dbExecute(
    con,
    "INSERT INTO episodic_epidemic_season (cluster_id, season_label, anchor_week, typical_length_weeks)
     VALUES (1, '2024/2025', 30, 0)"
  ))

  migration <- episodic_db_migrations()[["13"]]
  expect_no_error(migration(con, "sqlite"))
})
