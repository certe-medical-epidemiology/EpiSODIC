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

# An instance's identity: ten lowercase hexadecimal characters, one per
# database, written once and never replaced.

test_that("a generated identifier is ten lowercase hexadecimal characters", {
  for (i in 1:20) {
    id <- episodic_instance_id_generate()
    expect_match(id, "^[0-9a-f]{10}$")
    expect_true(episodic_instance_id_valid(id))
  }
})

test_that("two generated identifiers differ", {
  ids <- vapply(1:50, function(i) episodic_instance_id_generate(), character(1))
  expect_length(unique(ids), 50)
})

test_that("a malformed identifier is not valid", {
  expect_false(episodic_instance_id_valid("ABCDEF0123"))
  expect_false(episodic_instance_id_valid("abc"))
  expect_false(episodic_instance_id_valid("abcdef01234"))
  expect_false(episodic_instance_id_valid("abcdefg123"))
  expect_false(episodic_instance_id_valid(NA_character_))
  expect_false(episodic_instance_id_valid(c("abcdef0123", "abcdef0124")))
  expect_false(episodic_instance_id_valid(123))
})

test_that("a fresh database carries exactly one identifier", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM episodic_instance")
  expect_equal(nrow(rows), 1L)
  expect_true(episodic_instance_id_valid(rows$instance_id))
  expect_identical(episodic_instance_id(path), rows$instance_id)
})

test_that("two fresh databases carry different identifiers", {
  a <- episodic_test_db_path()
  b <- episodic_test_db_path()
  on.exit(unlink(c(a, b)))
  expect_false(identical(episodic_instance_id(a), episodic_instance_id(b)))
})

test_that("ensuring an identifier on a database that has one keeps it", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  first <- episodic_db_instance_id(con)
  expect_identical(episodic_db_instance_ensure(con), first)
  expect_identical(episodic_db_instance_id(con), first)
})

test_that("the table refuses a second row", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(DBI::dbExecute(
    con,
    "INSERT INTO episodic_instance (instance_row, instance_id, created_at)
     VALUES (2, 'abcdef0123', '2026-01-01T00:00:00Z')"
  ))
})

test_that("a database without an identifier is refused by name", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DELETE FROM episodic_instance")
  expect_error(episodic_db_instance_id(con), "no instance identifier")
})

test_that("a hand-written malformed identifier is refused, not repaired", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "UPDATE episodic_instance SET instance_id = 'ABCDEFGHIJ'")
  expect_error(episodic_db_instance_id(con), "not ten")
  expect_error(episodic_db_instance_ensure(con), "not ten")
})

test_that("the Info screen shows the identifier when it has a database", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  id <- episodic_db_instance_id(con)
  html <- as.character(episodic_ui_info_screen(con, lang = "en"))
  expect_match(html, paste0("<code>", id, "</code>"), fixed = TRUE)
  expect_match(html, episodic_tr("info.about.instance", lang = "en"), fixed = TRUE)
})

test_that("the Info screen says why an identifier cannot be read rather than leaving it out", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DELETE FROM episodic_instance")
  html <- as.character(episodic_ui_info_screen(con, lang = "en"))
  expect_match(html, "no instance identifier", fixed = TRUE)
})
