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

same_place_case <- function(source_key, sample_date, institution_id, pathogen = "Test organism",
                             ward = "ICU") {
  data.frame(
    source_key = source_key, pathogen = pathogen, institution_id = institution_id, ward = ward,
    sample_date = sample_date, stringsAsFactors = FALSE
  )
}

same_place_add_institution <- function(con, type = "hospital", monitored = 1L) {
  key <- digest::digest(paste(type, monitored, stats::runif(1)), algo = "sha1", serialize = FALSE)
  id <- episodic_db_institution_upsert(
    con, institution_key = key, display_name = "Test Institution", institution_type = type,
    care_line = "second", is_monitored = as.logical(monitored)
  )
  data.frame(institution_id = id, institution_type = type, is_monitored = monitored,
             stringsAsFactors = FALSE)
}

test_that("n or more cases within k days at the same ward fires a hit", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institutions <- same_place_add_institution(con)
  id <- institutions$institution_id[1]
  cases <- rbind(
    same_place_case("K1", "2025-01-01", id), same_place_case("K2", "2025-01-03", id),
    same_place_case("K3", "2025-01-05", id)
  )
  config <- episodic_test_config()
  result <- episodic_detect_same_place(con, cases, institutions, config)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_cases[1], 3)
  expect_equal(result$detector[1], "same_place")
})

test_that("fewer than n cases produces no hit", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institutions <- same_place_add_institution(con)
  id <- institutions$institution_id[1]
  cases <- rbind(same_place_case("K1", "2025-01-01", id), same_place_case("K2", "2025-01-03", id))
  config <- episodic_test_config()
  result <- episodic_detect_same_place(con, cases, institutions, config)
  expect_equal(nrow(result), 0)
})

test_that("cases spread beyond k days do not combine into one hit", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institutions <- same_place_add_institution(con)
  id <- institutions$institution_id[1]
  cases <- rbind(
    same_place_case("K1", "2025-01-01", id), same_place_case("K2", "2025-01-02", id),
    same_place_case("K3", "2025-03-01", id), same_place_case("K4", "2025-03-02", id),
    same_place_case("K5", "2025-03-03", id)
  )
  config <- episodic_test_config()
  result <- episodic_detect_same_place(con, cases, institutions, config)
  expect_equal(nrow(result), 1)  # only the March run of 3 clears the default n=3
  expect_equal(result$n_cases[1], 3)
})

test_that("a per-pathogen override tightens the threshold (norovirus: n=3 within 7 days)", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institutions <- same_place_add_institution(con)
  id <- institutions$institution_id[1]
  cases <- rbind(
    same_place_case("K1", "2025-01-01", id, pathogen = "Norovirus"),
    same_place_case("K2", "2025-01-09", id, pathogen = "Norovirus"),  # 8 days later: outside 7-day window
    same_place_case("K3", "2025-01-10", id, pathogen = "Norovirus")
  )
  config <- episodic_test_config()
  result <- episodic_detect_same_place(con, cases, institutions, config)
  expect_equal(nrow(result), 0)  # never 3 cases within any 7-day window
})

test_that("non-hospital institutions are scanned at institution level, not ward level", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institutions <- same_place_add_institution(con, type = "ltc_institution", monitored = 0L)
  id <- institutions$institution_id[1]
  cases <- rbind(
    same_place_case("K1", "2025-01-01", id, ward = NA_character_),
    same_place_case("K2", "2025-01-03", id, ward = NA_character_),
    same_place_case("K3", "2025-01-05", id, ward = NA_character_)
  )
  config <- episodic_test_config()
  result <- episodic_detect_same_place(con, cases, institutions, config)
  expect_equal(nrow(result), 1)

  stream <- DBI::dbGetQuery(con, "SELECT level, ward FROM episodic_stream WHERE stream_id = ?",
                             params = list(result$stream_id[1]))
  expect_equal(stream$level[1], "pathogen_institution")
  expect_true(is.na(stream$ward[1]))
})
