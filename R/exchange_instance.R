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

# The instance's own identity: ten lowercase hexadecimal characters,
# generated once per database and never changed. An exchange file names
# its sender by it, and an importing instance prefixes the sender's keys
# with it (see R/exchange_import.R).

#' The tables schema version 15 adds
#'
#' One list, read by the migration to version 15, so the tables a fresh
#' database is created with and the tables a migrated one gains cannot
#' drift apart.
#' @return A character vector of table names, parents before children.
#' @keywords internal
#' @noRd
episodic_db_exchange_tables <- function() {
  c("episodic_instance")
}

#' A fresh instance identifier
#'
#' Five bytes from libsodium's cryptographic random source, written as
#' ten lowercase hexadecimal characters: 40 bits, so two instances that
#' never coordinated draw the same identifier with a probability that
#' stays negligible far beyond any realistic number of partners. A
#' clock, a host name or a counter would each be shared by two
#' laboratories sooner or later.
#' @return A single string matching `^[0-9a-f]{10}$`.
#' @keywords internal
#' @noRd
episodic_instance_id_generate <- function() {
  tolower(sodium::bin2hex(sodium::random(5L)))
}

#' Whether a value is a well-formed instance identifier
#' @param x Anything.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
episodic_instance_id_valid <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    grepl("^[0-9a-f]{10}$", x)
}

#' Give a database its instance identifier, unless it already has one
#'
#' Called by `episodic_db_create()` and by the migration to schema
#' version 15, and by nothing else: the identifier is written once and
#' never replaced, because partners hold files and prefixed keys that
#' name it. A database that already carries a row keeps it; one that
#' carries a malformed row, or more than one, is refused rather than
#' repaired, since either means somebody wrote to the table by hand and
#' which identifier the partners know is not something this function can
#' decide.
#' @param con A [DBI::DBIConnection-class] on a database that has the
#'   `episodic_instance` table.
#' @return The identifier, invisibly.
#' @keywords internal
#' @noRd
episodic_db_instance_ensure <- function(con) {
  rows <- episodic_db_get_query(
    con,
    "SELECT instance_id FROM episodic_instance"
  )
  if (nrow(rows) == 0) {
    id <- episodic_instance_id_generate()
    episodic_db_execute(
      con,
      "INSERT INTO episodic_instance (instance_row, instance_id, created_at)
       VALUES (1, ?, ?)",
      params = list(id, episodic_now())
    )
    return(invisible(id))
  }
  invisible(episodic_db_instance_id(con))
}

#' The instance identifier a database carries
#'
#' Fails loudly on a database without one, or with a malformed one: an
#' export stamped with a missing or invented identifier would put cases
#' under the wrong provenance at every partner that imports it.
#' @param con A [DBI::DBIConnection-class].
#' @return A single string matching `^[0-9a-f]{10}$`.
#' @keywords internal
#' @noRd
episodic_db_instance_id <- function(con) {
  rows <- episodic_db_get_query(
    con,
    "SELECT instance_id FROM episodic_instance"
  )
  if (nrow(rows) == 0) {
    stop(
      "This database carries no instance identifier. A database created ",
      "or migrated by this version of EpiSODIC always has one; run ",
      "episodic_db_migrate() to give it one.",
      call. = FALSE
    )
  }
  if (nrow(rows) > 1) {
    stop(
      "This database carries ", nrow(rows), " instance identifiers, and ",
      "an instance has exactly one. The table episodic_instance has been ",
      "written to by hand; restore the identifier your partners know.",
      call. = FALSE
    )
  }
  id <- as.character(rows$instance_id[1])
  if (!episodic_instance_id_valid(id)) {
    stop(
      "This database's instance identifier (\"", id, "\") is not ten ",
      "lowercase hexadecimal characters. The table episodic_instance has ",
      "been written to by hand; restore the identifier your partners know.",
      call. = FALSE
    )
  }
  id
}

#' This Instance's Identifier
#'
#' Every EpiSODIC database carries an identifier of its own: ten
#' hexadecimal characters, generated once from a cryptographic random
#' source when the database is created (or brought up to schema version
#' 15) and never changed afterwards. It is what an exchange file names
#' its sender by, and what an importing instance prefixes the sender's
#' `source_key`, `patient_key` and `lab_number` with, so that two
#' laboratories' keys can never be mistaken for one another. Give it to
#' a partner together with your public keys; the Info screen of the
#' dashboard shows it too.
#'
#' @param db_path Path to an existing SQLite database, or a `mysql://`
#'   DSN (see [episodic_db_dsn_mariadb()]). Defaults to the
#'   `EPISODIC_DB` environment variable.
#' @return A single string of ten lowercase hexadecimal characters.
#' @seealso `vignette("exchange")`
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' DBI::dbDisconnect(episodic_db_create(db_path))
#' episodic_instance_id(db_path)
#' file.remove(db_path)
#' @export
episodic_instance_id <- function(db_path = Sys.getenv("EPISODIC_DB", unset = NA)) {
  con <- episodic_db_open(db_path)
  on.exit(DBI::dbDisconnect(con))
  episodic_db_instance_id(con)
}
