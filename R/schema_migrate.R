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

#' Build a MariaDB/MySQL DSN for `EPISODIC_DB`
#'
#' `EPISODIC_DB` accepts either a filesystem path (a SQLite database, the
#' default) or a `mysql://user:password@host:port/dbname` DSN pointing at a
#' MariaDB or MySQL server instead - every function that takes `db_path`
#' (or falls back to `EPISODIC_DB`) dispatches on which of the two it was
#' given. This helper builds that DSN string from its parts and
#' URL-encodes `user`/`password`, so credentials containing `:`, `@` or
#' `/` do not break the DSN. `episode_db_dsn_mysql()` is an alias for the
#' same function - the DSN and everything downstream of it is identical
#' either way, so use whichever name matches the server you actually run.
#'
#' @param host Server hostname or IP address.
#' @param dbname Database (schema) name.
#' @param user,password Credentials.
#' @param port TCP port. Defaults to `3306`.
#' @return A single string, ready to pass as `db_path` or to
#'   `Sys.setenv(EPISODIC_DB = ...)`.
#' @examples
#' episode_db_dsn_mariadb(
#'   host = "db.internal", dbname = "episodic",
#'   user = "episodic_app", password = "s3cr3t!"
#' )
#' @export
episode_db_dsn_mariadb <- function(host, dbname, user, password, port = 3306L) {
  stopifnot(
    is.character(host), nzchar(host),
    is.character(dbname), nzchar(dbname),
    is.character(user), is.character(password)
  )
  sprintf(
    "mysql://%s:%s@%s:%d/%s",
    utils::URLencode(user, reserved = TRUE),
    utils::URLencode(password, reserved = TRUE),
    host, as.integer(port),
    utils::URLencode(dbname, reserved = TRUE)
  )
}

#' @rdname episode_db_dsn_mariadb
#' @export
episode_db_dsn_mysql <- episode_db_dsn_mariadb

#' @keywords internal
#' @noRd
episode_db_dialect <- function(path) {
  if (grepl("^(mysql|mariadb)://", path)) "mariadb" else "sqlite"
}

#' Whether a database already exists at `path`
#'
#' For SQLite this is just `file.exists()`; a `mysql://` DSN always
#' "exists" as a server + schema, so this connects and checks whether the
#' EpiSODIC tables have already been created in it. Used by entry points
#' such as [episode_run_cron()] that connect to an already-initialised
#' database or create a fresh one, whichever applies.
#' @param path Path to a SQLite file, or a `mysql://` DSN.
#' @return `TRUE`/`FALSE`.
#' @keywords internal
#' @noRd
episode_db_exists <- function(path) {
  if (episode_db_dialect(path) == "sqlite") {
    return(file.exists(path))
  }
  con <- episode_db_mariadb_connect(path)
  on.exit(DBI::dbDisconnect(con))
  length(DBI::dbListTables(con)) > 0
}

#' @keywords internal
#' @noRd
episode_db_parse_mariadb_dsn <- function(x) {
  m <- regmatches(x, regexec(
    "^(?:mysql|mariadb)://(?:([^:@/]*)(?::([^@/]*))?@)?([^:@/]+)(?::([0-9]+))?/([^?]+)$",
    x
  ))[[1]]
  if (length(m) == 0) {
    stop(
      "Not a valid MariaDB/MySQL DSN: '", x, "'. Expected the form ",
      "mysql://user:password@host:port/dbname - see episode_db_dsn_mariadb().",
      call. = FALSE
    )
  }
  list(
    user = utils::URLdecode(m[2]),
    password = utils::URLdecode(m[3]),
    host = m[4],
    port = if (nzchar(m[5])) as.integer(m[5]) else 3306L,
    dbname = utils::URLdecode(m[6])
  )
}

#' @keywords internal
#' @noRd
episode_db_mariadb_connect <- function(dsn) {
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop(
      "Connecting to a MariaDB/MySQL EPISODIC_DB requires the 'RMariaDB' ",
      "package. Install it with install.packages(\"RMariaDB\").",
      call. = FALSE
    )
  }
  parts <- episode_db_parse_mariadb_dsn(dsn)
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = parts$host, port = parts$port, dbname = parts$dbname,
    user = parts$user, password = parts$password
  )
}

#' Create a fresh EpiSODIC database
#'
#' Creates a new, empty database at `path` and applies the canonical
#' schema shipped as `inst/sql/schema.sql`. Always builds from scratch;
#' refuses to run against a database that already contains tables so
#' that it cannot silently clobber existing data.
#'
#' @param path Path to a SQLite file, or a `mysql://` DSN (see
#'   [episode_db_dsn_mariadb()]) pointing at an empty MariaDB/MySQL
#'   database. For SQLite, must not already exist, or must be an
#'   empty/non-EpiSODIC file.
#' @param overwrite If `TRUE`, delete an existing SQLite file (or drop all
#'   tables in an existing MariaDB/MySQL database) first.
#' @return (Invisibly) an open [DBI::DBIConnection-class] to the new
#'   database. The caller is responsible for disconnecting it.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episode_db_create(db_path)
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episode_db_create <- function(path, overwrite = FALSE) {
  dialect <- episode_db_dialect(path)

  if (dialect == "sqlite") {
    if (file.exists(path)) {
      if (overwrite) {
        file.remove(path)
      } else {
        stop(
          "A file already exists at '", path, "'. Pass overwrite = TRUE to ",
          "replace it, or use episode_db_connect() to open an existing database.",
          call. = FALSE
        )
      }
    }
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
  } else {
    con <- episode_db_mariadb_connect(path)
    existing_tables <- DBI::dbListTables(con)
    if (length(existing_tables) > 0) {
      if (!overwrite) {
        DBI::dbDisconnect(con)
        stop(
          "Database already contains tables. Pass overwrite = TRUE to drop ",
          "and recreate them, or use episode_db_connect() to open an existing database.",
          call. = FALSE
        )
      }
      DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")
      for (tbl in existing_tables) DBI::dbRemoveTable(con, tbl)
      DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1")
    }
  }

  episode_db_pragmas(con)

  for (statement in episode_db_schema_statements(dialect)) {
    DBI::dbExecute(con, statement)
  }

  invisible(con)
}

#' Connect to an existing EpiSODIC database
#'
#' Opens a connection with the pragmas required by the architecture (for
#' SQLite: WAL journal mode, a busy timeout, and foreign key enforcement).
#'
#' @param path Path to an existing SQLite file, or a `mysql://` DSN (see
#'   [episode_db_dsn_mariadb()]) pointing at an existing MariaDB/MySQL
#'   database.
#' @return An open [DBI::DBIConnection-class].
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episode_db_create(db_path)
#' DBI::dbDisconnect(con)
#' con <- episode_db_connect(db_path)
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episode_db_connect <- function(path) {
  dialect <- episode_db_dialect(path)
  if (dialect == "sqlite") {
    if (!file.exists(path)) {
      stop("No database file found at '", path, "'.", call. = FALSE)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
  } else {
    con <- episode_db_mariadb_connect(path)
  }
  episode_db_pragmas(con)
  con
}

#' Open a connection to `db_path`, or the `EPISODIC_DB` environment variable
#'
#' A thin wrapper around [episode_db_connect()] for entry points that take
#' `db_path` rather than an already-open connection (e.g.
#' [episode_provision_user()]) - centralises the `EPISODIC_DB` default and
#' the "neither was given" error in one place, rather than duplicating the
#' resolve-then-connect logic in every such entry point.
#'
#' @param db_path Path to an existing SQLite database, or a `mysql://` DSN
#'   (see [episode_db_dsn_mariadb()]). Defaults to the `EPISODIC_DB`
#'   environment variable.
#' @return An open [DBI::DBIConnection-class]; the caller is responsible
#'   for disconnecting it.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episode_db_create(db_path)
#' DBI::dbDisconnect(con)
#' Sys.setenv(EPISODIC_DB = db_path)
#' con <- episode_db_open()
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episode_db_open <- function(db_path = Sys.getenv("EPISODIC_DB", unset = NA)) {
  if (is.na(db_path) || !nzchar(db_path)) {
    stop(
      "No database path given and EPISODIC_DB is not set. Pass db_path ",
      "explicitly, or set the EPISODIC_DB environment variable.",
      call. = FALSE
    )
  }
  episode_db_connect(db_path)
}

#' @keywords internal
#' @noRd
episode_db_pragmas <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
    DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  } else {
    DBI::dbExecute(con, "SET SESSION foreign_key_checks = 1")
  }
  invisible(con)
}

#' The id generated by the most recent `INSERT` on `con`
#'
#' `last_insert_rowid()` (SQLite) and `LAST_INSERT_ID()` (MariaDB/MySQL)
#' are dialect-specific spellings of the same thing; every insert helper
#' in `R/db_app_write.R` and `R/db_cron_write.R` goes through this instead
#' of hardcoding one dialect's function name.
#' @param con A [DBI::DBIConnection-class].
#' @return The last auto-generated id, as inserted on `con` by the current
#'   connection.
#' @keywords internal
#' @noRd
episode_db_last_insert_id <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  } else {
    DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID() AS id")$id[1]
  }
}

#' Load and dialect-adapt `inst/sql/schema.sql`
#'
#' The schema is written once, in SQLite syntax; for a MariaDB/MySQL
#' connection this rewrites the handful of tokens that differ
#' (`AUTOINCREMENT`, the SQLite-only `PRAGMA` line, and the few `TEXT`
#' columns that carry a `UNIQUE` constraint, which need a bounded
#' `VARCHAR` under MySQL/MariaDB) rather than maintaining a second schema
#' file.
#' @param dialect `"sqlite"` or `"mariadb"`.
#' @return A character vector of individual SQL statements.
#' @keywords internal
#' @noRd
episode_db_schema_statements <- function(dialect) {
  schema_path <- system.file("sql", "schema.sql", package = "EpiSODIC")
  if (identical(schema_path, "")) {
    # not-yet-installed package (devtools::load_all()) - fall back to source tree
    schema_path <- file.path("inst", "sql", "schema.sql")
  }
  schema_sql <- readLines(schema_path, warn = FALSE)
  schema_sql <- paste(schema_sql, collapse = "\n")

  if (dialect == "mariadb") {
    schema_sql <- sub("PRAGMA foreign_keys = ON;\n", "", schema_sql, fixed = TRUE)
    schema_sql <- gsub("AUTOINCREMENT", "AUTO_INCREMENT", schema_sql, fixed = TRUE)
    schema_sql <- sub(
      "stream_key      TEXT NOT NULL UNIQUE",
      "stream_key      VARCHAR(40) NOT NULL UNIQUE",
      schema_sql, fixed = TRUE
    )
    schema_sql <- sub(
      "institution_key  TEXT NOT NULL UNIQUE",
      "institution_key  VARCHAR(40) NOT NULL UNIQUE",
      schema_sql, fixed = TRUE
    )
    schema_sql <- sub(
      "source_key     TEXT NOT NULL UNIQUE,",
      "source_key     VARCHAR(191) NOT NULL UNIQUE,",
      schema_sql, fixed = TRUE
    )
    schema_sql <- sub(
      "username      TEXT NOT NULL UNIQUE,",
      "username      VARCHAR(191) NOT NULL UNIQUE,",
      schema_sql, fixed = TRUE
    )
  }

  episode_split_sql_statements(schema_sql)
}

#' Split a SQL file into individual statements
#'
#' `DBI::dbExecute()` on SQLite runs one statement at a time, so a schema
#' file with several `CREATE TABLE` statements has to be split first. This
#' splits on semicolons that terminate a statement, while respecting `--`
#' line comments so a semicolon inside a comment is not mistaken for one.
#'
#' @param sql A single string holding the full contents of a `.sql` file.
#' @return A character vector of individual statements, comments and blank
#'   entries removed.
#' @keywords internal
#' @noRd
episode_split_sql_statements <- function(sql) {
  lines <- strsplit(sql, "\n", fixed = TRUE)[[1]]
  lines <- sub("--.*$", "", lines)
  cleaned <- paste(lines, collapse = "\n")
  statements <- strsplit(cleaned, ";")[[1]]
  statements <- trimws(statements)
  statements[nzchar(statements)]
}
