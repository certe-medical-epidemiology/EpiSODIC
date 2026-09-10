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

# A schema EpiSODIC shares with another application.
#
# An ordinary way to deploy: EpiSODIC's tables beside another system's in
# one MySQL schema. Answering "does the database exist" from the presence
# of any table at all makes the co-tenant's tables look like an existing
# EpiSODIC instance in a schema EpiSODIC has never touched.
#
# The SQLite half is testable anywhere; the routing itself needs a server
# with a schema in it, and lives in test-mariadb_live.R.

test_that("migration refuses a database EpiSODIC never created", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE brmo_orders (id INTEGER PRIMARY KEY)")
  DBI::dbDisconnect(con)

  # Not "adopted at version 1": there is nothing here to adopt, and
  # stamping it would leave a database claiming to be current with none of
  # the tables a run needs.
  expect_error(episodic_db_migrate(path), "never created anything in")
  expect_error(episodic_db_migrate(path), "episodic_db_create")

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_false(DBI::dbExistsTable(con, "episodic_schema_version"))
})

test_that("migration names the core tables it could not find", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path))
  con <- episodic_db_create(path)
  # A real pre-versioning database has the core tables and no version
  # table. Drop the version table only, and it must still be adopted.
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbDisconnect(con)
  expect_message(episodic_db_migrate(path), "Adopted this database")

  # Take one core table away and it must not be.
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbExecute(con, "DROP TABLE episodic_cluster_case")
  DBI::dbExecute(con, "DROP TABLE episodic_cluster")
  DBI::dbDisconnect(con)
  expect_error(episodic_db_migrate(path), "episodic_cluster")
})

test_that("the core tables are a subset of the schema's own", {
  # If a core table is ever renamed, this fails here rather than by
  # quietly refusing every migration that comes along afterwards.
  expect_true(all(
    episodic_db_core_tables() %in% episodic_db_schema_tables()
  ))
})
