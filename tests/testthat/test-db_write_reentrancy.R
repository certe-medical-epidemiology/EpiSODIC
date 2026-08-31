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
# so an inline `params = list(...)` is not evaluated at the call site but
# inside dbExecute()/dbGetQuery(), after the driver has prepared a
# statement on the connection. If evaluating any element then queries that
# same connection, RMariaDB cancels and frees the prepared statement and
# dbBind() - which, unlike dbFetch(), never checks that its result is
# still active - binds into freed memory. The session dies natively: no
# condition, no tryCatch, nothing in the R log.
#
# Reproduced without any of EpiSODIC, against a MariaDB connection:
#
#   con <- DBI::dbConnect(RMariaDB::MariaDB(), <dsn>)
#   DBI::dbExecute(con, "CREATE TEMPORARY TABLE t (id INT, x DOUBLE)")
#   DBI::dbExecute(con, "INSERT INTO t VALUES (1, 0)")
#   nested <- function() DBI::dbGetQuery(con, "SELECT 1 AS a")$a[1]
#   DBI::dbExecute(con, "UPDATE t SET x = ? WHERE id = ?",
#                  params = list(nested(), 1L))   # <- kills the session
#
# Hoisting that last call's `nested()` into a local beforehand is the
# whole fix, and is what these tests hold in place.
#
# None of that is reproducible on SQLite, which permits concurrent results
# on one connection, so these are source-level invariants in the style of
# test-insert_only.R rather than behavioural tests. The behavioural
# companion (a scoring closure that really does query mid-reconciliation)
# lives in test-reconcile.R.

test_that("the database write helpers never bind an inline params list", {
  r_dir <- episodic_test_r_source_dir()
  skip_if(
    is.na(r_dir),
    "R/ source directory not found (e.g. checking an installed, non-source package)"
  )

  write_files <- file.path(r_dir, c("db_cron_write.R", "db_app_write.R"))
  expect_true(all(file.exists(write_files)))

  offenders <- character(0)
  for (f in write_files) {
    lines <- readLines(f, warn = FALSE)
    code <- lines[!grepl("^\\s*#", lines)]
    hits <- grep("params\\s*=\\s*list\\(", code, perl = TRUE)
    if (length(hits) > 0) {
      offenders <- c(
        offenders,
        sprintf("%s: %s", basename(f), trimws(code[hits]))
      )
    }
  }
  # Build the list into a `params` local first, then pass `params = params`.
  expect_equal(offenders, character(0))
})

test_that("every params local is a plain value before its statement opens", {
  r_dir <- episodic_test_r_source_dir()
  skip_if(
    is.na(r_dir),
    "R/ source directory not found (e.g. checking an installed, non-source package)"
  )

  # The counterpart to the test above: `params = params` is only safe if a
  # `params <- list(...)` really does precede it. Counting them per file
  # catches a half-applied edit that leaves a `params = params` bound to a
  # stale or absent local.
  for (f in file.path(r_dir, c("db_cron_write.R", "db_app_write.R"))) {
    code <- readLines(f, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    uses <- sum(grepl("^\\s*params = params\\s*$", code, perl = TRUE))
    decls <- sum(grepl("^\\s*params <- list\\(", code, perl = TRUE))
    expect_true(uses > 0, info = basename(f))
    expect_equal(decls, uses, info = basename(f))
  }
})

test_that("reconciliation never passes its scoring closure as a lazy argument", {
  r_dir <- episodic_test_r_source_dir()
  skip_if(
    is.na(r_dir),
    "R/ source directory not found (e.g. checking an installed, non-source package)"
  )

  # priority_score_fn() runs its own queries (episodic_app_density()), so
  # inlining it as `priority_score = priority_score_fn(candidate)` in a
  # cluster write is precisely the shape that killed the session. It must
  # be forced into a local first.
  code <- readLines(file.path(r_dir, "reconcile.R"), warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  inlined <- grep("=\\s*priority_score_fn\\(", code, perl = TRUE)
  expect_equal(trimws(code[inlined]), character(0))

  # ...and it is still actually called, so the test cannot pass by the
  # scoring having been dropped altogether.
  expect_true(any(grepl("priority_score <- priority_score_fn\\(", code)))
})

test_that("episodic_db_last_insert_id() returns a plain integer", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  id <- episodic_db_run_start(con, "h", "a")

  # MariaDB's LAST_INSERT_ID() is BIGINT, which RMariaDB would otherwise
  # hand back as a bit64::integer64 - a double carrying the integer's bit
  # pattern. Subassigning one into an ordinary vector drops the class and
  # keeps the payload, so a real id silently became a subnormal double and
  # was written into an INTEGER column as 0. Asserting the plain type here
  # is what stops that reaching the database again.
  #
  # The run row stands in for every id read back this way. Institutions no
  # longer are: episodic_institutions_resolve(), where that bug surfaced,
  # now writes the batch and reads the ids back by key, which keeps
  # LAST_INSERT_ID() out of that path altogether. Clusters, detections and
  # runs still take theirs from here.
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
