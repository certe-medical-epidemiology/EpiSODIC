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

# Schema version 16 adds the record of plugin runs
# (episodic_plugin_run_event).

v16_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_plugin_run_event")
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 15 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("migration 16 adds the plugin run record, and a second pass changes nothing", {
  path <- v16_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_true(DBI::dbExistsTable(con, "episodic_plugin_run_event"))
  columns <- DBI::dbListFields(con, "episodic_plugin_run_event")
  expect_no_error(episodic_db_migrations()[["16"]](con, "sqlite"))
  expect_identical(DBI::dbListFields(con, "episodic_plugin_run_event"), columns)
})
