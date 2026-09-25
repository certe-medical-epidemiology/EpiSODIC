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

# The MariaDB-only crash these guard against: an R argument is a promise,
# so `params = list(x)` handed straight to DBI is evaluated only after the
# driver has prepared the statement on the connection. If evaluating `x`
# queries that same connection, RMariaDB cancels and closes the prepared
# statement and the outer dbBind() binds to one that no longer exists.
# The session dies natively: no condition, no tryCatch, nothing in the R
# log.
#
# Reproduced without any of EpiSODIC, against a MariaDB connection:
#
#   con <- DBI::dbConnect(RMariaDB::MariaDB(), <dsn>)
#   nested <- function() DBI::dbGetQuery(con, "SELECT 1 AS a")$a[1]
#   DBI::dbGetQuery(con, "SELECT ? AS b", params = list(nested()))
#
# Every query in the package goes through episodic_db_get_query() or
# episodic_db_execute() (R/db_query.R), which force their arguments
# first. RSQLite permits concurrent results on one connection, so the
# crash itself cannot happen here; what these tests hold is the ordering
# (nothing is evaluated while a DBI call is on the stack) and the
# routing (nothing else calls DBI's query functions). The crash itself
# is exercised against a real server in test-mariadb_live.R.

# Whether a DBI query function is anywhere on the call stack.
inside_dbi_query <- function() {
  heads <- vapply(
    sys.calls(),
    function(cl) paste(deparse(cl[[1]]), collapse = ""),
    character(1)
  )
  any(grepl(
    "^(DBI::)?db(GetQuery|SendQuery|SendStatement|Execute|Bind|Fetch)$",
    heads
  ))
}

test_that("query parameters are evaluated before the statement is prepared", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  evaluated_inside <- NA
  delayedAssign("lazy_value", {
    evaluated_inside <- inside_dbi_query()
    # A query of its own on the same connection, as a reactive or a
    # scoring closure would run.
    episodic_db_get_query(con, "SELECT 7 AS a")$a[1]
  })
  got <- episodic_db_get_query(con, "SELECT ? AS b", params = list(lazy_value))
  expect_equal(got$b, 7)
  expect_false(is.na(evaluated_inside))
  expect_false(evaluated_inside)

  evaluated_inside <- NA
  delayedAssign("lazy_id", {
    evaluated_inside <- inside_dbi_query()
    episodic_db_get_query(con, "SELECT 1 AS a")$a[1]
  })
  episodic_db_execute(
    con,
    "UPDATE episodic_cluster SET n_cases = n_cases WHERE cluster_id = ?",
    params = list(lazy_id)
  )
  expect_false(is.na(evaluated_inside))
  expect_false(evaluated_inside)
})

test_that("a lazily supplied cluster id is resolved before the state lookup queries", {
  # The shape of the epidemic-dossier crash: an id whose evaluation builds
  # something from the database, handed down unevaluated into a read.
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  evaluated_inside <- NA
  delayedAssign("lazy_id", {
    evaluated_inside <- inside_dbi_query()
    episodic_db_get_query(con, "SELECT 999 AS id")$id[1]
  })
  expect_equal(episodic_app_derive_state_for_cluster(con, lazy_id), "new")
  expect_false(is.na(evaluated_inside))
  expect_false(evaluated_inside)
})

test_that("nothing in the package calls a DBI query function directly", {
  # Walks the installed namespace rather than the source, so it holds on
  # an installed package as well as under devtools::load_all(). Nested
  # function definitions are part of their enclosing body and are walked
  # with it.
  forbidden <- c(
    "dbGetQuery", "dbExecute", "dbSendQuery", "dbSendStatement",
    "dbBind", "dbFetch", "dbAppendTable", "dbWriteTable"
  )
  allowed <- c("episodic_db_get_query", "episodic_db_execute")

  calls_found <- function(expr) {
    if (is.call(expr)) {
      head <- expr[[1]]
      own <- if (
        is.call(head) &&
          identical(head[[1]], as.name("::")) &&
          identical(head[[2]], as.name("DBI")) &&
          as.character(head[[3]]) %in% forbidden
      ) {
        as.character(head[[3]])
      } else if (is.name(head) && as.character(head) %in% forbidden) {
        as.character(head)
      } else {
        character(0)
      }
      c(own, unlist(lapply(as.list(expr)[-1], calls_found)))
    } else if (is.function(expr)) {
      calls_found(body(expr))
    } else {
      character(0)
    }
  }

  ns <- asNamespace("EpiSODIC")
  offenders <- character(0)
  for (name in setdiff(ls(ns, all.names = TRUE), allowed)) {
    obj <- get(name, envir = ns)
    if (!is.function(obj)) {
      next
    }
    found <- calls_found(body(obj))
    if (length(found) > 0) {
      offenders <- c(
        offenders,
        sprintf("%s() calls %s", name, paste(unique(found), collapse = ", "))
      )
    }
  }
  # Route it through episodic_db_get_query() or episodic_db_execute().
  expect_equal(offenders, character(0))

  # ...and the two that are allowed still do, so the walk cannot pass by
  # having nothing to find.
  expect_true("dbGetQuery" %in% calls_found(body(ns$episodic_db_get_query)))
  expect_true("dbExecute" %in% calls_found(body(ns$episodic_db_execute)))
})

test_that("episodic_db_last_insert_id() returns a plain integer", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  id <- episodic_db_run_start(con, "h", "a")

  # MariaDB's LAST_INSERT_ID() is BIGINT, which RMariaDB would otherwise
  # hand back as a bit64::integer64 - a double carrying the integer's bit
  # pattern. Subassigning one into an ordinary vector drops the class and
  # keeps the payload, so a real id becomes a subnormal double and is
  # written into an INTEGER column as 0. Asserting the plain type here is
  # what keeps that out of the database.
  #
  # The run row stands in for every id read back this way: clusters,
  # detections and runs all take theirs from here.
  # episodic_institutions_resolve() does not, since it writes the batch
  # and reads the ids back by key, which keeps LAST_INSERT_ID() out of
  # that path altogether.
  expect_type(id, "integer")
  expect_false(inherits(id, "integer64"))
  expect_equal(id, 1L)

  stored <- DBI::dbGetQuery(
    con,
    "SELECT run_id FROM episodic_detection_run"
  )$run_id
  expect_equal(stored, 1L)
  expect_false(any(stored == 0))
})
