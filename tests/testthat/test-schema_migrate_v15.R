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

# Schema version 15 adds the instance's identity (episodic_instance), the
# tables of import and export between instances, and the instance an imported
# case came from (episodic_case.origin_instance_id).

v15_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  for (table in rev(episodic_db_import_export_tables())) {
    DBI::dbExecute(con, paste("DROP TABLE", table))
  }
  DBI::dbExecute(con, "ALTER TABLE episodic_case DROP COLUMN origin_instance_id")
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 14 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("migration 15 adds the import and export tables and gives the database an identifier", {
  path <- v15_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  log <- testthat::capture_messages(episodic_db_migrate(path))
  expect_true(any(grepl("This instance's identifier is [0-9a-f]{10}\\.", log)))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  for (table in episodic_db_import_export_tables()) {
    expect_true(DBI::dbExistsTable(con, table), info = table)
  }
  expect_true(episodic_instance_id_valid(episodic_db_instance_id(con)))
  expect_true(episodic_db_column_exists(con, "sqlite", "episodic_case", "origin_instance_id"))
})

test_that("a second pass of migration 15 keeps the identifier it gave", {
  path <- v15_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  first <- episodic_db_instance_id(con)
  expect_no_error(episodic_db_migrations()[["15"]](con, "sqlite"))
  expect_identical(episodic_db_instance_id(con), first)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_instance")$n,
    1L
  )
})
