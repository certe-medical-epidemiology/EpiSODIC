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
# account provisioned, credentials work) - not exercise a representative
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
