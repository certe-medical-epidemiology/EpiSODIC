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

# The package's only route to a query. Every read and write goes through
# `episodic_db_get_query()` or `episodic_db_execute()` rather than calling
# `DBI::dbGetQuery()`/`DBI::dbExecute()` itself, and what the two add is
# one thing: every argument is a plain value before the driver sees it.
#
# That matters because an R argument is a promise. Passed straight to
# DBI, `params = list(x)` is evaluated only once RMariaDB has already
# prepared the statement and made it the connection's current result. If
# evaluating `x` queries the same connection - a reactive handed to the
# dossier unevaluated, a scoring closure, any caller-supplied argument
# that builds something from the database - RMariaDB allows one active
# result per connection, so the nested query cancels and closes the
# prepared statement, and the outer `dbBind()` then binds to a statement
# that no longer exists. That is a native access violation rather than
# an R condition: the process dies, with nothing for `tryCatch()` to
# catch and nothing in the log. RSQLite permits concurrent results on one
# connection, so the same code is harmless there and no SQLite test sees
# it.
#
# Forcing at this one point makes the ordering structural rather than a
# convention every call site has to remember. test-db_reentrancy.R holds
# it in place by walking the installed namespace for any other function
# calling a DBI query function directly.

#' Run a query and fetch its result
#'
#' `DBI::dbGetQuery()` with every argument evaluated before the statement
#' is prepared; see the note at the top of this file for why.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param statement The SQL, with `?` placeholders.
#' @param params A list of values for the placeholders, or `NULL`.
#' @return A data frame.
#' @keywords internal
#' @noRd
episodic_db_get_query <- function(con, statement, params = NULL) {
  force(con)
  force(statement)
  force(params)
  if (is.null(params)) {
    DBI::dbGetQuery(con, statement)
  } else {
    DBI::dbGetQuery(con, statement, params = params)
  }
}

#' Run a statement and return the number of rows it affected
#'
#' `DBI::dbExecute()` with every argument evaluated before the statement
#' is prepared; see the note at the top of this file for why.
#'
#' @inheritParams episodic_db_get_query
#' @return The number of rows affected.
#' @keywords internal
#' @noRd
episodic_db_execute <- function(con, statement, params = NULL) {
  force(con)
  force(statement)
  force(params)
  if (is.null(params)) {
    DBI::dbExecute(con, statement)
  } else {
    DBI::dbExecute(con, statement, params = params)
  }
}

#' Run a query over a set of ids, in chunks
#'
#' The batch readers ask for every row belonging to a set of clusters or
#' streams, and on the Archive or the rail that set is every cluster the
#' instance holds - thousands after a backfill. One placeholder per id
#' in a single statement stops working at MySQL's 65,535-placeholder
#' limit and makes every query in between one very long statement to
#' prepare. Chunked at `episodic_db_chunk_size`, over the ids sorted, so
#' a query ordered by that id first returns the chunks already in order.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param statement The SQL, with a single `%s` where the `IN` list's
#'   placeholders go.
#' @param ids The ids, in any order; duplicates are dropped.
#' @return A data frame, the chunks' results bound in id order. With no
#'   ids, the correctly shaped empty result of the statement run for no
#'   id at all.
#' @keywords internal
#' @noRd
episodic_db_get_query_in <- function(con, statement, ids) {
  ids <- sort(unique(ids))
  if (length(ids) == 0) {
    return(episodic_db_get_query(con, sprintf(statement, "NULL")))
  }
  chunks <- split(ids, ceiling(seq_along(ids) / episodic_db_chunk_size))
  parts <- lapply(chunks, function(chunk) {
    episodic_db_get_query(
      con,
      sprintf(statement, paste(rep("?", length(chunk)), collapse = ", ")),
      params = as.list(chunk)
    )
  })
  out <- do.call(rbind, unname(parts))
  rownames(out) <- NULL
  out
}
