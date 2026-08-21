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

# These cover the dialect logic that does not require an actual MariaDB
# server: DSN building/parsing, dialect detection, and the schema
# rewriting episode_db_schema_statements() applies for "mariadb". An
# actual MariaDB/MySQL connection is exercised only manually/in CI with a
# real server available - RSQLite's own tests already cover the SQLite path.

test_that("episode_db_dialect() tells a SQLite path from a MariaDB DSN", {
  expect_equal(episode_db_dialect("/path/to/episode.sqlite"), "sqlite")
  expect_equal(episode_db_dialect("episode.sqlite"), "sqlite")
  expect_equal(episode_db_dialect("mysql://user:pw@localhost:3306/episodic"), "mariadb")
  expect_equal(episode_db_dialect("mariadb://user:pw@localhost/episodic"), "mariadb")
})

test_that("episode_db_dsn_mariadb() builds a DSN and URL-encodes credentials", {
  dsn <- episode_db_dsn_mariadb(
    host = "db.internal", dbname = "episodic", user = "app", password = "simple"
  )
  expect_equal(dsn, "mysql://app:simple@db.internal:3306/episodic")

  dsn_special <- episode_db_dsn_mariadb(
    host = "db.internal", dbname = "episodic", user = "app", password = "p@ss:w/ord"
  )
  expect_false(grepl("p@ss:w/ord", dsn_special, fixed = TRUE))
  expect_equal(episode_db_dialect(dsn_special), "mariadb")
})

test_that("episode_db_dsn_mysql() is an alias for episode_db_dsn_mariadb()", {
  args <- list(host = "db.internal", dbname = "episodic", user = "app", password = "simple")
  expect_equal(do.call(episode_db_dsn_mysql, args), do.call(episode_db_dsn_mariadb, args))
})

test_that("episode_db_dsn_mariadb() round-trips through episode_db_parse_mariadb_dsn()", {
  dsn <- episode_db_dsn_mariadb(
    host = "db.internal", dbname = "episodic", user = "app", password = "p@ss:w/ord!",
    port = 3307L
  )
  parts <- episode_db_parse_mariadb_dsn(dsn)
  expect_equal(parts$host, "db.internal")
  expect_equal(parts$dbname, "episodic")
  expect_equal(parts$user, "app")
  expect_equal(parts$password, "p@ss:w/ord!")
  expect_equal(parts$port, 3307L)
})

test_that("episode_db_parse_mariadb_dsn() defaults to port 3306", {
  parts <- episode_db_parse_mariadb_dsn("mysql://app:secret@localhost/episodic")
  expect_equal(parts$port, 3306L)
})

test_that("episode_db_parse_mariadb_dsn() errors on a malformed DSN", {
  expect_error(episode_db_parse_mariadb_dsn("mysql://not-a-valid-dsn"), "Not a valid")
})

test_that("episode_db_schema_statements(\"mariadb\") rewrites AUTOINCREMENT and drops the PRAGMA line", {
  statements <- episode_db_schema_statements("mariadb")
  combined <- paste(statements, collapse = "\n")
  expect_false(grepl("AUTOINCREMENT", combined, fixed = TRUE))
  expect_true(grepl("AUTO_INCREMENT", combined, fixed = TRUE))
  expect_false(any(grepl("^PRAGMA", statements)))
})

test_that("episode_db_schema_statements(\"mariadb\") bounds the four TEXT UNIQUE columns", {
  statements <- episode_db_schema_statements("mariadb")
  combined <- paste(statements, collapse = "\n")
  expect_true(grepl("stream_key      VARCHAR(40) NOT NULL UNIQUE", combined, fixed = TRUE))
  expect_true(grepl("institution_key  VARCHAR(40) NOT NULL UNIQUE", combined, fixed = TRUE))
  expect_true(grepl("source_key     VARCHAR(191) NOT NULL UNIQUE", combined, fixed = TRUE))
  expect_true(grepl("username      VARCHAR(191) NOT NULL UNIQUE", combined, fixed = TRUE))
})

test_that("episode_db_schema_statements(\"sqlite\") is unaffected by the mariadb rewriting", {
  statements <- episode_db_schema_statements("sqlite")
  combined <- paste(statements, collapse = "\n")
  expect_true(grepl("AUTOINCREMENT", combined, fixed = TRUE))
  expect_false(grepl("AUTO_INCREMENT", combined, fixed = TRUE))
  expect_true(any(grepl("^PRAGMA", statements)))
})

test_that("episode_db_create() with a mysql:// DSN errors clearly when RMariaDB is not installed", {
  skip_if(requireNamespace("RMariaDB", quietly = TRUE), "RMariaDB is installed")
  expect_error(
    episode_db_create("mysql://app:secret@localhost/episodic"),
    "RMariaDB"
  )
})
