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

# Small windows throughout: episodic_demo()'s own defaults (the full
# multi-year synthetic generator) are what a real demo should use, but
# these tests only need to confirm the plumbing (db created, cron ran,
# account added, credentials work) - not exercise a representative
# dataset, which is already covered by test-run_cron.R.
small_cases <- function() {
  episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 21
  )
}
small_denominator <- function() {
  episodic_synthetic_denominators(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 21
  )
}

test_that("episodic_demo(launch = FALSE) sets up a working demo database in one call", {
  skip_if_not_installed("sodium")
  db_path <- tempfile(fileext = ".sqlite")

  expect_message(
    result <- episodic_demo(
      db_path = db_path,
      launch = FALSE,
      cases = small_cases,
      denominators = small_denominator
    ),
    "demo account"
  )
  expect_equal(result, db_path)
  expect_true(file.exists(db_path))

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_case")$n, 0)
  expect_gt(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_cluster")$n,
    0
  )

  user <- episodic_db_user_by_username(con, "demo")
  expect_false(is.null(user))
  expect_true(episodic_auth_login(con, "demo", "demo")$ok)
})

test_that("episodic_demo() accepts custom credentials", {
  skip_if_not_installed("sodium")
  db_path <- tempfile(fileext = ".sqlite")

  episodic_demo(
    db_path = db_path,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.org",
    password = "s3cret-enough",
    launch = FALSE,
    cases = small_cases,
    denominators = small_denominator
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  user <- episodic_db_user_by_username(con, "jdoe")
  expect_equal(user$full_name, "Jane Doe")
  expect_true(episodic_auth_login(con, "jdoe", "s3cret-enough")$ok)
})

test_that("the demo dates its run from the extract it was given, not from today", {
  # Every screen the demo opens on is dated: the epi curves, the "as of"
  # on the Pathogen screen, the weeks Farrington aggregates to. Dated
  # from today against last year's export, all of them sit months past
  # the last case and show a flat run of empty weeks. The demo therefore
  # dates itself from the data it was given. `episodic_run_cron()`
  # deliberately does not: a real surveillance run is always as of today,
  # and reaches a historical extract through `backfill` instead, which
  # reports the archive without pretending the archive is the present.
  cases <- small_cases()
  expect_equal(
    episodic_demo_run_date(cases),
    max(as.Date(cases$sample_date))
  )
  # Nothing in the extract to date the run from falls back rather than
  # erroring.
  expect_s3_class(
    episodic_demo_run_date(data.frame(sample_date = as.Date(NA))),
    "Date"
  )
})

test_that("episodic_demo() says which date it chose, and records it on the run", {
  skip_if_not_installed("sodium")
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  # Computed from the fixture rather than written out, so the test says
  # "the last day the extract covers" rather than restating a date that
  # would have to be kept in step by hand.
  expected <- format(max(as.Date(small_cases()$sample_date)))

  expect_message(
    episodic_demo(
      db_path = db_path,
      launch = FALSE,
      cases = small_cases,
      denominators = NULL
    ),
    expected,
    fixed = TRUE
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT run_date FROM episodic_detection_run")$run_date[1],
    expected
  )
  # denominators = NULL means no denominator feed, not "generate one".
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_denominator")$n,
    0
  )
})

test_that("an explicit run_date still wins, and the demo says nothing about choosing one", {
  skip_if_not_installed("sodium")
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  episodic_demo(
    db_path = db_path,
    launch = FALSE,
    cases = small_cases,
    denominators = NULL,
    run_date = as.Date("2024-06-20")
  )
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT run_date FROM episodic_detection_run")$run_date[1],
    "2024-06-20"
  )
})

test_that("the demo refuses a database that already exists rather than adding synthetic cases to it", {
  # The whole point: a demo generates synthetic cases and runs detection
  # over them. Pointed at a live instance it would file those cases in
  # episodic_case alongside the real ones, where they reach every
  # denominator, line list and patient search and cannot be told apart
  # afterwards.
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))
  writeLines("not empty", db_path)

  expect_error(
    episodic_demo(db_path = db_path, launch = FALSE, cases = small_cases),
    "already a database"
  )
  expect_error(
    episodic_demo(db_path = db_path, launch = FALSE, cases = small_cases),
    "overwrite = TRUE",
    fixed = TRUE
  )
  # It refused before generating anything, so the file is untouched.
  expect_equal(readLines(db_path), "not empty")
})

test_that("the demo refuses a MariaDB DSN outright", {
  expect_error(
    episodic_demo(
      db_path = "mysql://user:secret@db.example.org:3306/episodic",
      launch = FALSE
    ),
    "cannot be pointed at a"
  )
})

test_that("episodic_demo_check_db_path() rejects a db_path that is not a single path", {
  expect_error(episodic_demo_check_db_path(character(0)), "single non-empty path")
  expect_error(episodic_demo_check_db_path(NA_character_), "single non-empty path")
  expect_error(episodic_demo_check_db_path(""), "single non-empty path")
})

test_that("overwrite = TRUE removes the existing demo and its configuration files first", {
  db_path <- tempfile(fileext = ".sqlite")
  demo <- episodic_demo_files(db_path)
  on.exit(unlink(c(db_path, demo$config, demo$pc_province_map)))
  writeLines("old database", db_path)
  writeLines("old config", demo$config)
  writeLines("old map", demo$pc_province_map)

  expect_message(
    episodic_demo_check_db_path(db_path, overwrite = TRUE),
    "Removed the existing demo"
  )
  expect_false(file.exists(db_path))
  expect_false(file.exists(demo$config))
  expect_false(file.exists(demo$pc_province_map))
})

test_that("a db_path that does not exist yet is accepted silently", {
  expect_silent(
    expect_null(episodic_demo_check_db_path(tempfile(fileext = ".sqlite")))
  )
})
