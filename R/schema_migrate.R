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
  port = 3306L
) {
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
    password = parts$password,
    # Never bit64::integer64, RMariaDB's default. An integer64 is a double
    # holding an integer's bit pattern, and every base operation that drops
    # its class (subassignment into an ordinary vector, `c()`, `ifelse()`)
    # silently turns it into a subnormal double instead of erroring - which
    # is how ids fetched here ended up written back as 0. Nothing in this
    # package needs 64-bit ids, so the safest thing is for them never to
    # exist: `episodic_db_last_insert_id()` guards the same boundary for
    # callers that reach the database another way.
    bigint = "integer"
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
    intended_tables <- episodic_db_schema_tables()
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
      on.exit(
        DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1"),
        add = TRUE,
        after = FALSE
      )

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
          "Failed to drop table \"",
          failed$table,
          "\" while recreating the schema. ",
          "Successfully dropped: ",
          if (length(dropped)) paste(dropped, collapse = ", ") else "(none)",
          ". Database is now in a partially-dropped state and must be inspected manually. ",
          "Original error: ",
          conditionMessage(failed$error),
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

#' Empty every EpiSODIC table, keeping the schema itself
#'
#' A hard reset back to "freshly created, no data" - every row in every
#' EpiSODIC table is deleted, but the tables, indexes and constraints
#' stay exactly as [episodic_db_create()] built them, so the database is
#' immediately ready for a new [episodic_run_cron()] without a schema
#' migration. This includes `episodic_app_user`: dashboard accounts are
#' data too, and are deleted along with everything else - there is
#' nothing this function leaves behind to sign in with afterwards.
#'
#' Deliberately hard to trigger by accident:
#'
#' \describe{
#'   \item{Interactive only}{Refuses outright under [interactive()]
#'     `FALSE` - a script, a cron job, or any other unattended context
#'     can never reach the confirmation prompt, so it can never reach
#'     the deletion either.}
#'   \item{Typed confirmation}{Prints exactly what is about to be
#'     deleted (every table, and its row count) and then requires you to
#'     type the database's own name back at a prompt. A bare yes/no is
#'     answerable on autopilot; typing the name back is not.}
#' }
#'
#' @param path Path to an existing SQLite file, or a `mysql://` DSN (see
#'   [episodic_db_dsn_mariadb()]) pointing at an existing MariaDB/MySQL
#'   database.
#' @return Invisibly, the character vector of tables that were
#'   truncated - `character(0)` if you cancelled, or if the database had
#'   no EpiSODIC tables to begin with.
#' @examples
#' \dontrun{
#' episodic_db_truncate(db_path)
#' }
#' @export
episodic_db_truncate <- function(path) {
  if (!interactive()) {
    stop(
      "episodic_db_truncate() only runs in an interactive session - never ",
      "from a script, a cron job, or any other unattended context. This is ",
      "deliberate: it permanently deletes every row in every EpiSODIC table, ",
      "including dashboard accounts.",
      call. = FALSE
    )
  }

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dialect <- episodic_db_dialect(path)

  tables <- intersect(episodic_db_schema_tables(), DBI::dbListTables(con))
  if (length(tables) == 0) {
    message(
      "No EpiSODIC tables found on this connection - nothing to truncate."
    )
    return(invisible(character(0)))
  }

  counts <- vapply(
    tables,
    function(tbl) {
      tryCatch(
        as.numeric(DBI::dbGetQuery(
          con,
          paste0("SELECT COUNT(*) AS n FROM ", tbl)
        )$n[1]),
        error = function(e) NA_real_
      )
    },
    numeric(1)
  )
  total_rows <- sum(counts, na.rm = TRUE)

  # Both RSQLite and RMariaDB report this the same way, so it is the one
  # thing that reliably names "this database" regardless of dialect -
  # the file path for SQLite, the schema name for MariaDB/MySQL.
  dbname <- tryCatch(DBI::dbGetInfo(con)$dbname, error = function(e) NULL)
  if (is.null(dbname) || is.na(dbname) || !nzchar(dbname)) {
    dbname <- path
  }

  cat(
    "This will permanently delete all data in ",
    length(tables),
    " EpiSODIC table(s) (",
    format(total_rows, big.mark = ",", scientific = FALSE),
    " row(s) total) on:\n\n  ",
    dbname,
    "\n\nTables: ",
    paste(tables, collapse = ", "),
    "\n\nThe schema itself is kept - this is not the same as ",
    "episodic_db_create(overwrite = TRUE), which drops and rebuilds the ",
    "tables from scratch.\n\n",
    sep = ""
  )
  answer <- readline(paste0(
    "Type the database name shown above (",
    dbname,
    ") to confirm, or anything else to cancel: "
  ))
  if (!identical(trimws(answer), dbname)) {
    message("Cancelled - no data was deleted.")
    return(invisible(character(0)))
  }

  restore_fk <- if (dialect == "mariadb") {
    "SET FOREIGN_KEY_CHECKS = 1"
  } else {
    # SQLite has no per-statement equivalent of MariaDB's FK-check
    # toggle; the pragma is connection-wide.
    "PRAGMA foreign_keys = ON"
  }
  DBI::dbExecute(
    con,
    if (dialect == "mariadb") {
      "SET FOREIGN_KEY_CHECKS = 0"
    } else {
      "PRAGMA foreign_keys = OFF"
    }
  )
  # A safety net for the error path only - the happy path restores this
  # explicitly, right after the loop below, and disconnects after that.
  # `on.exit(..., add = TRUE)` appends to the *end* of the connection's
  # exit-handler list; con's own on.exit(dbDisconnect(con)) above was
  # registered first and so would otherwise run first, closing the
  # connection before this handler got to use it - RMariaDB's own
  # response to a query issued against an already-freed connection is
  # not a catchable R error but a C++ bad_weak_ptr abort. `after = FALSE`
  # prepends instead, so this runs before the disconnect even then.
  on.exit(
    tryCatch(DBI::dbExecute(con, restore_fk), error = function(e) NULL),
    add = TRUE,
    after = FALSE
  )

  truncated <- character(0)
  for (tbl in tables) {
    if (dialect == "mariadb") {
      DBI::dbExecute(con, paste0("TRUNCATE TABLE ", tbl))
    } else {
      DBI::dbExecute(con, paste0("DELETE FROM ", tbl))
      # DELETE alone leaves AUTOINCREMENT counters where they were;
      # dropping the table's own row from sqlite_sequence is what makes
      # the next insert start back at 1, matching what TRUNCATE does on
      # MariaDB and what a genuinely empty table implies. sqlite_sequence
      # itself only exists once at least one AUTOINCREMENT table has
      # been created, so its absence (a schema with none, or none used
      # yet) is not a real failure.
      tryCatch(
        DBI::dbExecute(
          con,
          "DELETE FROM sqlite_sequence WHERE name = ?",
          params = list(tbl)
        ),
        error = function(e) NULL
      )
    }
    truncated <- c(truncated, tbl)
  }

  DBI::dbExecute(con, restore_fk)

  message(
    length(truncated),
    " table(s) truncated (",
    format(total_rows, big.mark = ",", scientific = FALSE),
    " row(s) deleted)."
  )
  invisible(truncated)
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
    # Say what the client speaks rather than inheriting whatever my.cnf
    # happens to set. Left implicit, the connection charset is decided
    # off-host and can differ from the server's own
    # (`character_set_connection = utf8mb4` against
    # `character_set_server = utf8` is what this deployment had), which is
    # a standing invitation for a string to arrive declaring one encoding
    # while holding bytes for another. Declaring it here makes what comes
    # back deterministic and identical on every machine that connects.
    DBI::dbExecute(con, "SET NAMES utf8mb4")
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
#'   connection, as a plain `integer`.
#' @keywords internal
#' @noRd
episodic_db_last_insert_id <- function(con) {
  id <- if (inherits(con, "SQLiteConnection")) {
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  } else {
    DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID() AS id")$id[1]
  }

  # Returned as a plain integer on purpose. MariaDB's LAST_INSERT_ID() is
  # BIGINT, and RMariaDB's default (`bigint = "integer64"`) hands that back
  # as a bit64::integer64 - which is physically a double carrying the
  # integer's *bit pattern*, not its value. Assigning one into an ordinary
  # vector (`ids <- integer(n); ids[i] <- ...`, as
  # `episodic_institutions_resolve()` does) drops the class and keeps the
  # payload, so institution 368 silently became 1.8e-321 and was then
  # written into an INTEGER column as 0. That is how every case in a
  # MariaDB run ended up pointing at a non-existent institution 0, while
  # SQLite - whose last_insert_rowid() is a plain numeric - was correct all
  # along. `as.numeric()` is bit64's own method and yields the real value,
  # never the bit pattern.
  id <- as.numeric(id)
  if (is.na(id) || id > .Machine$integer.max) {
    stop(
      "Could not read the last inserted id from the database (got ",
      format(id),
      "). This should not happen; the run cannot safely continue, since ",
      "every row written after this point would reference the wrong id.",
      call. = FALSE
    )
  }
  as.integer(id)
}

#' Every table name `inst/sql/schema.sql` declares
#'
#' Parsed from the schema file's own `CREATE TABLE` statements rather
#' than kept as a separate hand-maintained list, so a new table is
#' picked up here the moment it is added to the schema.
#'
#' @return A character vector of table names, in the order the schema
#'   creates them (parents before the children that reference them).
#' @keywords internal
#' @noRd
episodic_db_schema_tables <- function() {
  schema_path <- system.file("sql", "schema.sql", package = "EpiSODIC")
  if (identical(schema_path, "")) {
    # not-yet-installed package (devtools::load_all()) - fall back to source tree
    schema_path <- file.path("inst", "sql", "schema.sql")
  }
  lines <- readLines(schema_path, warn = FALSE)
  create_lines <- lines[grepl("^\\s*CREATE TABLE", lines, ignore.case = TRUE)]
  tables <- gsub(
    "^\\s*CREATE TABLE\\s+([a-zA-Z0-9_]+).*",
    "\\1",
    create_lines,
    ignore.case = TRUE
  )
  if (length(tables) == 0) {
    stop(
      "No 'CREATE TABLE' statements found in the schema file `inst/sql/schema.sql`. ",
      "The package installation may be corrupted.",
      call. = FALSE
    )
  }
  tables
}

#' Load and dialect-adapt `inst/sql/schema.sql`
#'
#' The schema is written once, in SQLite syntax; for a MariaDB/MySQL
#' connection this rewrites the handful of tokens that differ
#' (`AUTOINCREMENT`, the SQLite-only `PRAGMA` line, and every `TEXT`
#' column that is unsafe as-is under MySQL - one carrying a `UNIQUE` or
#' `PRIMARY KEY` constraint, a `DEFAULT` value, or used in a `CREATE
#' INDEX`/composite `PRIMARY KEY`, all of which MySQL rejects on a bare
#' `TEXT`/`BLOB` column ("BLOB/TEXT column can't have a default value"
#' \[1101\]; "BLOB/TEXT column used in key specification without a key
#' length" \[1170\]) - given a bounded `VARCHAR` instead) rather than
#' maintaining a second schema file. `episodic_denominator`'s nullable
#' `area_code` is also, in the schema itself rather than here, kept out
#' of any `PRIMARY KEY` (a surrogate `denominator_id` plus a `UNIQUE`
#' constraint stand in for it) - MySQL implicitly forces every
#' `PRIMARY KEY` column `NOT NULL`, which would otherwise reject a row
#' with no area stratum.
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

    # Every TEXT column that is either UNIQUE/PRIMARY KEY, carries a
    # DEFAULT, or appears in a CREATE INDEX or composite PRIMARY KEY,
    # rewritten to a bounded VARCHAR. Bounds: 40 for a fixed-length hash
    # key, 191 for an otherwise-unconstrained free-text key (the
    # conventional safe width for an indexed utf8mb4 column under
    # MySQL's default 767-byte key-part limit), 20 for a short CHECK'd
    # enum, 10 for an ISO 8601 YYYY-MM-DD date stored as text.
    #
    # Scoped per table (not a flat find/replace over the whole file):
    # several tables pad their column names to different widths, so the
    # same "colname   TEXT NOT NULL," text can occur verbatim in more
    # than one CREATE TABLE block, and a flat sub() would silently only
    # touch whichever one happens to come first.
    text_to_varchar <- list(
      episodic_stream = c(
        "stream_key      TEXT NOT NULL UNIQUE" = "stream_key      VARCHAR(40) NOT NULL UNIQUE",
        "  pathogen        TEXT NOT NULL,  -- raw lab-provided string, deliberately unconstrained free text" = "  pathogen        VARCHAR(191) NOT NULL,  -- raw lab-provided string, deliberately unconstrained free text",
        "denominator     TEXT NOT NULL DEFAULT 'none' CHECK (denominator IN (" = "denominator     VARCHAR(20) NOT NULL DEFAULT 'none' CHECK (denominator IN ("
      ),
      episodic_institution = c(
        "institution_key  TEXT NOT NULL UNIQUE" = "institution_key  VARCHAR(40) NOT NULL UNIQUE"
      ),
      episodic_institution_activity = c(
        "  period_start   TEXT NOT NULL," = "  period_start   VARCHAR(10) NOT NULL,"
      ),
      episodic_pathogen_config = c(
        "  pathogen        TEXT NOT NULL PRIMARY KEY,  -- matches episodic_case.pathogen exactly" = "  pathogen        VARCHAR(191) NOT NULL PRIMARY KEY,  -- matches episodic_case.pathogen exactly"
      ),
      episodic_case = c(
        "source_key     TEXT NOT NULL UNIQUE," = "source_key     VARCHAR(191) NOT NULL UNIQUE,",
        "  lab_number     TEXT NOT NULL,  -- the lab's own specimen/culture number; not unique, unlike source_key" = "  lab_number     VARCHAR(191) NOT NULL,  -- the lab's own specimen/culture number; not unique, unlike source_key",
        "  patient_key    TEXT NOT NULL," = "  patient_key    VARCHAR(191) NOT NULL,",
        "  sample_date    TEXT NOT NULL," = "  sample_date    VARCHAR(10) NOT NULL,",
        "  pathogen       TEXT NOT NULL,  -- raw lab-provided string, used verbatim" = "  pathogen       VARCHAR(191) NOT NULL,  -- raw lab-provided string, used verbatim",
        "care_line      TEXT NOT NULL DEFAULT 'unknown' CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown'))," = "care_line      VARCHAR(20) NOT NULL DEFAULT 'unknown' CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown')),"
      ),
      episodic_stream_trend = c(
        "  week_start TEXT NOT NULL," = "  week_start VARCHAR(10) NOT NULL,"
      ),
      episodic_denominator = c(
        "  pathogen       TEXT NOT NULL," = "  pathogen       VARCHAR(191) NOT NULL,",
        "  sample_date    TEXT NOT NULL," = "  sample_date    VARCHAR(10) NOT NULL,",
        "  care_line      TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown'))," = "  care_line      VARCHAR(20) NOT NULL CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown')),",
        "  area_code      TEXT," = "  area_code      VARCHAR(191),"
      ),
      episodic_app_user = c(
        "username      TEXT NOT NULL UNIQUE," = "username      VARCHAR(191) NOT NULL UNIQUE,"
      ),
      episodic_app_config_event = c(
        "  created_at  TEXT NOT NULL," = "  created_at  VARCHAR(30) NOT NULL,",
        "  section     TEXT NOT NULL CHECK (section IN ('notifications'))," = "  section     VARCHAR(20) NOT NULL CHECK (section IN ('notifications')),"
      )
    )
    for (table in names(text_to_varchar)) {
      # (?s) makes "." span newlines - these blocks are multi-line.
      block_pattern <- paste0("(?s)CREATE TABLE ", table, " \\(.*?\\n\\);")
      block_span <- regexpr(block_pattern, schema_sql, perl = TRUE)
      block <- regmatches(schema_sql, block_span)
      rules <- text_to_varchar[[table]]
      for (pattern in names(rules)) {
        new_block <- sub(pattern, rules[[pattern]], block, fixed = TRUE)
        if (identical(new_block, block)) {
          stop(
            "episodic_db_schema_statements(): pattern not found in ",
            table,
            ": '",
            pattern,
            "'. inst/sql/schema.sql may have changed - ",
            "update the mariadb rewrite rules to match.",
            call. = FALSE
          )
        }
        block <- new_block
      }
      # regmatches<-() substitutes the matched span with `block` literally
      # (unlike sub()'s replacement string, it never interprets \1/\\-style
      # backreferences in it), which matters here since `block` is
      # arbitrary already-rewritten SQL text, not a hand-written pattern.
      regmatches(schema_sql, block_span) <- block
    }
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
