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

#' Connect EpiSODIC to a MariaDB or MySQL server
#'
#' EpiSODIC stores its data in either a SQLite file (the default, and all
#' you need for a single-server deployment) or a MariaDB/MySQL database.
#' This function builds the connection string ("DSN") for the latter, so
#' you never have to hand-assemble one or worry about special characters
#' in your password breaking it. Use the resulting string as `db_path`
#' anywhere EpiSODIC expects one, or store it in the `EPISODIC_DB`
#' environment variable. `episodic_db_dsn_mysql()` is an identical alias -
#' use whichever name matches the server you run.
#'
#' @param host Server hostname or IP address.
#' @param dbname Database (schema) name.
#' @param user,password Credentials.
#' @param port TCP port. Defaults to `3306`.
#' @return A single string, ready to pass as `db_path` or to
#'   `Sys.setenv(EPISODIC_DB = ...)`.
#' @examples
#' episodic_db_dsn_mariadb(
#'   host = "db.internal", dbname = "episodic",
#'   user = "episodic_app", password = "s3cr3t!"
#' )
#' @export
episodic_db_dsn_mariadb <- function(
    host,
    dbname,
    user,
    password,
    port = 3306L) {
  stopifnot(
    is.character(host),
    nzchar(host),
    is.character(dbname),
    nzchar(dbname),
    is.character(user),
    is.character(password)
  )
  sprintf(
    "mysql://%s:%s@%s:%d/%s",
    utils::URLencode(user, reserved = TRUE),
    utils::URLencode(password, reserved = TRUE),
    host,
    as.integer(port),
    utils::URLencode(dbname, reserved = TRUE)
  )
}

#' @rdname episodic_db_dsn_mariadb
#' @export
episodic_db_dsn_mysql <- episodic_db_dsn_mariadb

#' @keywords internal
#' @noRd
episodic_db_dialect <- function(path) {
  if (grepl("^(mysql|mariadb)://", path)) "mariadb" else "sqlite"
}

#' Whether a database already exists at `path`
#'
#' For SQLite this is just `file.exists()`; a `mysql://` DSN always
#' "exists" as a server + schema, so this connects and checks whether the
#' EpiSODIC tables have already been created in it. Used by entry points
#' such as [episodic_run_cron()] that connect to an already-initialised
#' database or create a fresh one, whichever applies.
#' @param path Path to a SQLite file, or a `mysql://` DSN.
#' @return `TRUE`/`FALSE`.
#' @keywords internal
#' @noRd
episodic_db_exists <- function(path) {
  if (episodic_db_dialect(path) == "sqlite") {
    return(file.exists(path))
  }
  con <- episodic_db_mariadb_connect(path)
  on.exit(DBI::dbDisconnect(con))
  length(DBI::dbListTables(con)) > 0
}

#' @keywords internal
#' @noRd
episodic_db_parse_mariadb_dsn <- function(x) {
  m <- regmatches(
    x,
    regexec(
      "^(?:mysql|mariadb)://(?:([^:@/]*)(?::([^@/]*))?@)?([^:@/]+)(?::([0-9]+))?/([^?]+)$",
      x
    )
  )[[1]]
  if (length(m) == 0) {
    stop(
      "Not a valid MariaDB/MySQL DSN: '",
      x,
      "'. Expected the form ",
      "mysql://user:password@host:port/dbname - see episodic_db_dsn_mariadb().",
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
episodic_db_mariadb_connect <- function(dsn) {
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop(
      "Connecting to a MariaDB/MySQL EPISODIC_DB requires the 'RMariaDB' ",
      "package. Install it with install.packages(\"RMariaDB\").",
      call. = FALSE
    )
  }
  parts <- episodic_db_parse_mariadb_dsn(dsn)
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = parts$host,
    port = parts$port,
    dbname = parts$dbname,
    user = parts$user,
    password = parts$password
  )
}

#' Set up a new EpiSODIC database
#'
#' Run this once, when setting up a new EpiSODIC instance: it creates a new
#' database at `path` and builds all the required tables. Refuses to run
#' against a database that already has tables in it, so it cannot
#' accidentally overwrite existing surveillance data - use
#' [episodic_db_connect()] to open a database you have already set up.
#'
#' @param path Path to a SQLite file to create, or a `mysql://` DSN (see
#'   [episodic_db_dsn_mariadb()]) pointing at an empty MariaDB/MySQL
#'   database.
#' @param overwrite If `TRUE`, delete an existing SQLite file (or drop all
#'   tables in an existing MariaDB/MySQL database) first. Use with care -
#'   this destroys any data already there.
#' @return (Invisibly) an open [DBI::DBIConnection-class] to the new
#'   database. You are responsible for disconnecting it (with
#'   [DBI::dbDisconnect()]) when done.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episodic_db_create(db_path)
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episodic_db_create <- function(path, overwrite = FALSE) {
  dialect <- episodic_db_dialect(path)

  if (dialect == "sqlite") {
    if (file.exists(path)) {
      if (overwrite) {
        file.remove(path)
      } else {
        stop(
          "A file already exists at '",
          path,
          "'. Pass overwrite = TRUE to ",
          "replace it, or use episodic_db_connect() to open an existing database.",
          call. = FALSE
        )
      }
    }
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
  } else {
    con <- episodic_db_mariadb_connect(path)

    existing_tables <- DBI::dbListTables(con)
    intended_tables <- readLines(system.file("sql/schema.sql", package = "EpiSODIC"))
    intended_tables <- intended_tables[grepl("^\\s*CREATE TABLE", intended_tables, ignore.case = TRUE)]
    intended_tables <- gsub("^\\s*CREATE TABLE\\s+([a-zA-Z0-9_]+).*", "\\1", intended_tables, ignore.case = TRUE)

    if (length(intended_tables) == 0) {
      stop(
        "No 'CREATE TABLE' statements found in the schema file `inst/sql/schema.sql`. ",
        "The package installation may be corrupted.",
        call. = FALSE
      )
    }

    tables_to_drop <- intersect(existing_tables, intended_tables)

    if (length(tables_to_drop) > 0) {
      if (!overwrite) {
        stop(
          "Database already contains these EpiSODIC tables: ",
          paste(sprintf('"%s"', tables_to_drop), collapse = ", "),
          ". Pass overwrite = TRUE to drop and recreate them, ",
          "or use episodic_db_connect() to open an existing database.",
          call. = FALSE
        )
      }

      DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")
      on.exit(DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1"), add = TRUE, after = FALSE)

      dropped <- character(0)
      failed <- NULL
      for (tbl in tables_to_drop) {
        result <- tryCatch(
          {
            DBI::dbRemoveTable(con, tbl)
            TRUE
          },
          error = function(e) e
        )
        if (isTRUE(result)) {
          dropped <- c(dropped, tbl)
        } else {
          failed <- list(table = tbl, error = result)
          break
        }
      }

      if (!is.null(failed)) {
        stop(
          "Failed to drop table \"", failed$table, "\" while recreating the schema. ",
          "Successfully dropped: ", if (length(dropped)) paste(dropped, collapse = ", ") else "(none)",
          ". Database is now in a partially-dropped state and must be inspected manually. ",
          "Original error: ", conditionMessage(failed$error),
          call. = FALSE
        )
      }
    }
  }

  episodic_db_pragmas(con)

  for (statement in episodic_db_schema_statements(dialect)) {
    DBI::dbExecute(con, statement)
  }

  invisible(con)
}

#' Connect to an existing EpiSODIC database
#'
#' Opens a connection to a database you have already set up with
#' [episodic_db_create()], with the settings EpiSODIC needs enabled (for
#' SQLite: WAL journal mode, a busy timeout, and foreign key enforcement).
#' Remember to disconnect with [DBI::dbDisconnect()] when you are done.
#'
#' @param path Path to an existing SQLite file, or a `mysql://` DSN (see
#'   [episodic_db_dsn_mariadb()]) pointing at an existing MariaDB/MySQL
#'   database.
#' @return An open [DBI::DBIConnection-class].
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episodic_db_create(db_path)
#' DBI::dbDisconnect(con)
#' con <- episodic_db_connect(db_path)
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episodic_db_connect <- function(path) {
  dialect <- episodic_db_dialect(path)
  if (dialect == "sqlite") {
    if (!file.exists(path)) {
      stop("No database file found at '", path, "'.", call. = FALSE)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
  } else {
    con <- episodic_db_mariadb_connect(path)
  }
  episodic_db_pragmas(con)
  con
}

#' Connect using the `EPISODIC_DB` environment variable
#'
#' Like [episodic_db_connect()], but falls back to the `EPISODIC_DB`
#' environment variable when you do not pass a path explicitly - handy for
#' one-off console use, e.g. [episodic_provision_user()] uses it
#' internally so provisioning an account needs only a username and
#' password, not a connection you build yourself first.
#'
#' @param db_path Path to an existing SQLite database, or a `mysql://` DSN
#'   (see [episodic_db_dsn_mariadb()]). Defaults to the `EPISODIC_DB`
#'   environment variable.
#' @return An open [DBI::DBIConnection-class]; you are responsible for
#'   disconnecting it.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episodic_db_create(db_path)
#' DBI::dbDisconnect(con)
#' Sys.setenv(EPISODIC_DB = db_path)
#' con <- episodic_db_open()
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episodic_db_open <- function(db_path = Sys.getenv("EPISODIC_DB", unset = NA)) {
  if (is.na(db_path) || !nzchar(db_path)) {
    stop(
      "No database path given and EPISODIC_DB is not set. Pass db_path ",
      "explicitly, or set the EPISODIC_DB environment variable.",
      call. = FALSE
    )
  }
  episodic_db_connect(db_path)
}

#' @keywords internal
#' @noRd
episodic_db_pragmas <- function(con) {
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
episodic_db_last_insert_id <- function(con) {
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
#' columns that carry a `UNIQUE` constraint or a `DEFAULT` value, which
#' need a bounded `VARCHAR` under MySQL/MariaDB - MySQL rejects a
#' `DEFAULT` on `BLOB`/`TEXT` columns outright) rather than maintaining a
#' second schema file.
#' @param dialect `"sqlite"` or `"mariadb"`.
#' @return A character vector of individual SQL statements.
#' @keywords internal
#' @noRd
episodic_db_schema_statements <- function(dialect) {
  schema_path <- system.file("sql", "schema.sql", package = "EpiSODIC")
  if (identical(schema_path, "")) {
    # not-yet-installed package (devtools::load_all()) - fall back to source tree
    schema_path <- file.path("inst", "sql", "schema.sql")
  }
  schema_sql <- readLines(schema_path, warn = FALSE)
  schema_sql <- paste(schema_sql, collapse = "\n")

  if (dialect == "mariadb") {
    schema_sql <- sub(
      "PRAGMA foreign_keys = ON;\n",
      "",
      schema_sql,
      fixed = TRUE
    )
    schema_sql <- gsub(
      "AUTOINCREMENT",
      "AUTO_INCREMENT",
      schema_sql,
      fixed = TRUE
    )
    schema_sql <- sub(
      "stream_key      TEXT NOT NULL UNIQUE",
      "stream_key      VARCHAR(40) NOT NULL UNIQUE",
      schema_sql,
      fixed = TRUE
    )
    schema_sql <- sub(
      "institution_key  TEXT NOT NULL UNIQUE",
      "institution_key  VARCHAR(40) NOT NULL UNIQUE",
      schema_sql,
      fixed = TRUE
    )
    schema_sql <- sub(
      "source_key     TEXT NOT NULL UNIQUE,",
      "source_key     VARCHAR(191) NOT NULL UNIQUE,",
      schema_sql,
      fixed = TRUE
    )
    schema_sql <- sub(
      "username      TEXT NOT NULL UNIQUE,",
      "username      VARCHAR(191) NOT NULL UNIQUE,",
      schema_sql,
      fixed = TRUE
    )
    # MySQL (unlike MariaDB and SQLite) rejects a DEFAULT on BLOB/TEXT
    # columns outright [1101], so this DEFAULT-carrying TEXT column also
    # needs a bounded VARCHAR under the mariadb dialect.
    schema_sql <- sub(
      "denominator     TEXT NOT NULL DEFAULT 'none' CHECK (denominator IN (",
      "denominator     VARCHAR(20) NOT NULL DEFAULT 'none' CHECK (denominator IN (",
      schema_sql,
      fixed = TRUE
    )
  }

  episodic_split_sql_statements(schema_sql)
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
episodic_split_sql_statements <- function(sql) {
  lines <- strsplit(sql, "\n", fixed = TRUE)[[1]]
  lines <- sub("--.*$", "", lines)
  cleaned <- paste(lines, collapse = "\n")
  statements <- strsplit(cleaned, ";")[[1]]
  statements <- trimws(statements)
  statements[nzchar(statements)]
}
