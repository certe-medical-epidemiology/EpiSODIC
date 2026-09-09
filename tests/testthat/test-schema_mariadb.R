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

# These cover the dialect logic that does not require an actual MariaDB
# server: DSN building/parsing, dialect detection, and the schema
# rewriting episodic_db_schema_statements() applies for "mariadb". An
# actual MariaDB/MySQL connection is exercised only manually/in CI with a
# real server available - RSQLite's own tests already cover the SQLite path.

test_that("episodic_db_dialect() tells a SQLite path from a MariaDB DSN", {
  expect_equal(episodic_db_dialect("/path/to/episodic.sqlite"), "sqlite")
  expect_equal(episodic_db_dialect("episodic.sqlite"), "sqlite")
  expect_equal(
    episodic_db_dialect("mysql://user:pw@localhost:3306/episodic"),
    "mariadb"
  )
  expect_equal(
    episodic_db_dialect("mariadb://user:pw@localhost/episodic"),
    "mariadb"
  )
})

test_that("episodic_db_dsn_mariadb() builds a DSN and URL-encodes credentials", {
  dsn <- episodic_db_dsn_mariadb(
    host = "db.internal",
    dbname = "episodic",
    user = "app",
    password = "simple"
  )
  expect_equal(dsn, "mysql://app:simple@db.internal:3306/episodic")

  dsn_special <- episodic_db_dsn_mariadb(
    host = "db.internal",
    dbname = "episodic",
    user = "app",
    password = "p@ss:w/ord"
  )
  expect_false(grepl("p@ss:w/ord", dsn_special, fixed = TRUE))
  expect_equal(episodic_db_dialect(dsn_special), "mariadb")
})

test_that("episodic_db_dsn_mysql() is an alias for episodic_db_dsn_mariadb()", {
  args <- list(
    host = "db.internal",
    dbname = "episodic",
    user = "app",
    password = "simple"
  )
  expect_equal(
    do.call(episodic_db_dsn_mysql, args),
    do.call(episodic_db_dsn_mariadb, args)
  )
})

test_that("episodic_db_dsn_mariadb() round-trips through episodic_db_parse_mariadb_dsn()", {
  dsn <- episodic_db_dsn_mariadb(
    host = "db.internal",
    dbname = "episodic",
    user = "app",
    password = "p@ss:w/ord!",
    port = 3307L
  )
  parts <- episodic_db_parse_mariadb_dsn(dsn)
  expect_equal(parts$host, "db.internal")
  expect_equal(parts$dbname, "episodic")
  expect_equal(parts$user, "app")
  expect_equal(parts$password, "p@ss:w/ord!")
  expect_equal(parts$port, 3307L)
})

test_that("episodic_db_parse_mariadb_dsn() defaults to port 3306", {
  parts <- episodic_db_parse_mariadb_dsn(
    "mysql://app:secret@localhost/episodic"
  )
  expect_equal(parts$port, 3306L)
})

test_that("episodic_db_parse_mariadb_dsn() errors on a malformed DSN", {
  expect_error(
    episodic_db_parse_mariadb_dsn("mysql://not-a-valid-dsn"),
    "Not a valid"
  )
})

test_that("episodic_db_schema_statements(\"mariadb\") rewrites AUTOINCREMENT and drops the PRAGMA line", {
  statements <- episodic_db_schema_statements("mariadb")
  combined <- paste(statements, collapse = "\n")
  expect_false(grepl("AUTOINCREMENT", combined, fixed = TRUE))
  expect_true(grepl("AUTO_INCREMENT", combined, fixed = TRUE))
  expect_false(any(grepl("^PRAGMA", statements)))
})

test_that("episodic_db_schema_statements(\"mariadb\") bounds the four TEXT UNIQUE columns", {
  statements <- episodic_db_schema_statements("mariadb")
  combined <- paste(statements, collapse = "\n")
  expect_true(grepl(
    "stream_key      VARCHAR(40) NOT NULL UNIQUE",
    combined,
    fixed = TRUE
  ))
  expect_true(grepl(
    "institution_key  VARCHAR(40) NOT NULL UNIQUE",
    combined,
    fixed = TRUE
  ))
  expect_true(grepl(
    "source_key     VARCHAR(191) NOT NULL UNIQUE",
    combined,
    fixed = TRUE
  ))
  expect_true(grepl(
    "lab_number     VARCHAR(191) NOT NULL",
    combined,
    fixed = TRUE
  ))
  expect_true(grepl(
    "username      VARCHAR(191) NOT NULL UNIQUE",
    combined,
    fixed = TRUE
  ))
})

test_that("episodic_db_schema_statements(\"sqlite\") is unaffected by the mariadb rewriting", {
  statements <- episodic_db_schema_statements("sqlite")
  combined <- paste(statements, collapse = "\n")
  expect_true(grepl("AUTOINCREMENT", combined, fixed = TRUE))
  expect_false(grepl("AUTO_INCREMENT", combined, fixed = TRUE))
  expect_true(any(grepl("^PRAGMA", statements)))
})

test_that("episodic_db_schema_statements(\"mariadb\") bounds episodic_app_config_event's indexed TEXT columns", {
  statements <- episodic_db_schema_statements("mariadb")
  combined <- paste(statements, collapse = "\n")
  expect_true(grepl(
    "section     VARCHAR(20) NOT NULL CHECK (section IN ('notifications'))",
    combined,
    fixed = TRUE
  ))
  expect_true(grepl(
    "CREATE TABLE episodic_app_config_event (\n  event_id    INTEGER PRIMARY KEY AUTO_INCREMENT,\n  user_id     INTEGER NOT NULL REFERENCES episodic_app_user(user_id),\n  created_at  VARCHAR(30) NOT NULL,",
    combined,
    fixed = TRUE
  ))
})

test_that("episodic_db_create() with a mysql:// DSN errors clearly when RMariaDB is not installed", {
  skip_if(requireNamespace("RMariaDB", quietly = TRUE), "RMariaDB is installed")
  expect_error(
    episodic_db_create("mysql://app:secret@localhost/episodic"),
    "RMariaDB"
  )
})

test_that("episodic_db_schema_statements(\"mariadb\") bounds episodic_app_login_failure's indexed TEXT column", {
  statements <- episodic_db_schema_statements("mariadb")
  create <- grep(
    "CREATE TABLE episodic_app_login_failure",
    statements,
    value = TRUE,
    fixed = TRUE
  )
  expect_length(create, 1)
  # attempted_at carries the table's only index, and MySQL rejects a bare
  # TEXT column in a key specification without a key length.
  expect_match(create, "attempted_at VARCHAR(30) NOT NULL", fixed = TRUE)
  # username and reason are not indexed, so they stay TEXT.
  expect_match(create, "username     TEXT NOT NULL", fixed = TRUE)
})

test_that("every indexed column in the schema is safe for MySQL", {
  # Keyed off the schema file rather than a hand-kept list: a new
  # CREATE INDEX on a TEXT column fails at table-creation time on
  # MariaDB and nowhere else, which is the worst place to find out.
  #
  # Scoped per table, which is the whole difficulty: `created_at` and
  # `sample_date` are indexed in one table each and plain TEXT in half a
  # dozen others, where they are perfectly safe. Matching on the column
  # name alone flags every one of those.
  schema_path <- system.file("sql", "schema.sql", package = "EpiSODIC")
  if (identical(schema_path, "")) {
    schema_path <- file.path("inst", "sql", "schema.sql")
  }
  skip_if_not(file.exists(schema_path))
  lines <- readLines(schema_path, warn = FALSE)

  indexed <- list()
  for (line in grep("^CREATE INDEX", lines, value = TRUE)) {
    table <- sub("^CREATE INDEX\\s+\\S+\\s+ON\\s+([A-Za-z0-9_]+)\\s*\\(.*$", "\\1", line)
    cols <- trimws(strsplit(
      sub("^.*\\(([^)]*)\\).*$", "\\1", line),
      ",",
      fixed = TRUE
    )[[1]])
    cols <- gsub("`", "", cols, fixed = TRUE)
    indexed[[table]] <- unique(c(indexed[[table]], cols))
  }
  expect_gt(length(indexed), 0)

  statements <- episodic_db_schema_statements("mariadb")
  for (table in names(indexed)) {
    block <- grep(
      paste0("CREATE TABLE\\s+", table, "\\s*\\("),
      statements,
      value = TRUE
    )
    expect_length(block, 1)
    block_lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
    for (column in indexed[[table]]) {
      offending <- grep(
        paste0("^\\s*", column, "\\s+TEXT\\b"),
        block_lines,
        value = TRUE
      )
      expect_equal(
        offending,
        character(0),
        info = paste(table, column, sep = ".")
      )
    }
  }
})
