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

#' Create a fresh EpiSODIC SQLite database
#'
#' Creates a new SQLite database file at `path` and applies the canonical
#' schema shipped as `inst/sql/schema.sql`. Always builds from scratch;
#' refuses to run against a file that already contains EpiSODIC tables
#' so that it cannot silently clobber existing data.
#'
#' @param path Path to the SQLite file to create. Must not already exist,
#'   or must be an empty/non-EpiSODIC SQLite file.
#' @param overwrite If `TRUE`, delete an existing file at `path` first.
#' @return (Invisibly) an open [DBI::DBIConnection-class] to the new
#'   database. The caller is responsible for disconnecting it.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episode_db_create(db_path)
#' DBI::dbDisconnect(con)
#' file.remove(db_path)
#' @export
episode_db_create <- function(path, overwrite = FALSE) {
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
  episode_db_pragmas(con)

  schema_path <- system.file("sql", "schema.sql", package = "EpiSODIC")
  if (identical(schema_path, "")) {
    # not-yet-installed package (devtools::load_all()) - fall back to source tree
    schema_path <- file.path("inst", "sql", "schema.sql")
  }
  schema_sql <- readLines(schema_path, warn = FALSE)
  schema_sql <- paste(schema_sql, collapse = "\n")

  statements <- episode_split_sql_statements(schema_sql)
  for (statement in statements) {
    DBI::dbExecute(con, statement)
  }

  invisible(con)
}

#' Connect to an existing EpiSODIC SQLite database
#'
#' Opens a connection with the pragmas required by the architecture
#' (WAL journal mode, a busy timeout, and foreign key enforcement).
#'
#' @param path Path to an existing SQLite file.
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
  if (!file.exists(path)) {
    stop("No database file found at '", path, "'.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
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
#' @param db_path Path to an existing SQLite database. Defaults to the
#'   `EPISODIC_DB` environment variable.
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
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  invisible(con)
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
