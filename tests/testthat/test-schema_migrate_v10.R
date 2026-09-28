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

# Schema version 10 adds a seasonal epidemic's weekly counts and bands
# (episodic_epidemic_week) and the cluster an assessed cluster would have
# been suppressed behind (episodic_cluster.would_be_suppressed_by). Both
# are filled by the next detection run.

# A version-9 database: the current schema without either addition.
v10_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_epidemic_week")
  DBI::dbExecute(con, "ALTER TABLE episodic_cluster DROP COLUMN would_be_suppressed_by")
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 9 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("a fresh database has the epidemic week table and the would-be-suppressed column", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbExistsTable(con, "episodic_epidemic_week"))
  expect_true(episodic_db_column_exists(
    con, "sqlite", "episodic_cluster", "would_be_suppressed_by"
  ))
})

test_that("migration 10 adds both, and a second pass changes nothing", {
  path <- v10_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))

  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_true(DBI::dbExistsTable(con, "episodic_epidemic_week"))
  expect_true(episodic_db_column_exists(
    con, "sqlite", "episodic_cluster", "would_be_suppressed_by"
  ))
  expect_setequal(
    DBI::dbListFields(con, "episodic_epidemic_week"),
    c("cluster_id", "week_start", "n_cases", "intensity_level", "run_id")
  )

  migration <- episodic_db_migrations()[["10"]]
  expect_no_error(migration(con, "sqlite"))
})
