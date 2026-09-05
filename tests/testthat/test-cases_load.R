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

# One institution_key, two rows. Nothing in the case data requirements says a
# hospital may not report both a second- and a third-line care line, and
# real extracts do. While institutions were upserted one at a time this was
# invisible - each row was its own statement, and the second updated what
# the first inserted. Batched into a single INSERT it is a duplicate on
# institution_key's UNIQUE index, which on MariaDB aborts the run before a
# single case is written:
#
#   Duplicate entry '73a1f32a...' for key 'institution_key' [1062]
#
# The key alone decides identity, so the batch must carry one row per key.

episodic_test_two_care_lines <- function() {
  data.frame(
    institution_key = c("HOSP-01", "HOSP-01", "LTC-01"),
    institution_display_name = c(
      "Test Hospital",
      "Test Hospital",
      "Test Care Home"
    ),
    institution_type = c("hospital", "hospital", "ltc_institution"),
    care_line = c("second", "third", "first"),
    municipality = c("Groningen", "Groningen", "Assen"),
    stringsAsFactors = FALSE
  )
}

test_that("an institution_key appearing twice in one batch resolves to one institution", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  ids <- episodic_institutions_resolve(con, episodic_test_two_care_lines())

  expect_named(ids, c("HOSP-01", "LTC-01"))
  expect_false(anyNA(ids))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_institution")$n,
    2
  )
})

test_that("the last row read wins for the columns institution_key does not decide", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  episodic_institutions_resolve(con, episodic_test_two_care_lines())

  # What episodic_check_cases() tells an operator to expect, and what the
  # per-institution upsert loop ended on.
  hospital <- DBI::dbGetQuery(
    con,
    "SELECT care_line, is_monitored FROM episodic_institution
      WHERE institution_type = 'hospital'"
  )
  expect_equal(nrow(hospital), 1)
  expect_equal(hospital$care_line, "third")
  expect_equal(as.integer(hospital$is_monitored), 1L)
})

test_that("resolving the same batch again neither duplicates nor renumbers", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  cases <- episodic_test_two_care_lines()
  first <- episodic_institutions_resolve(con, cases)
  second <- episodic_institutions_resolve(con, cases)

  expect_equal(second, first)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_institution")$n,
    2
  )
})
