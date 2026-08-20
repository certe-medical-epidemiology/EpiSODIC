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

# Smaller-scope exported functions, one file to keep the test suite easy to
# navigate; every exported function gets direct coverage.

test_that("episode_eligibility_gate() passes a stream with a full year of steady weekly counts", {
  config <- episode_test_config()
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-01-01"), by = "week")
  cases <- data.frame(sample_date = as.character(dates))
  expect_true(episode_eligibility_gate(cases, as.Date("2024-01-01"), config))
})

test_that("episode_eligibility_gate() fails a stream with too little baseline history", {
  config <- episode_test_config()
  cases <- data.frame(sample_date = as.character(seq(as.Date("2024-01-01"), as.Date("2024-01-15"), by = "day")))
  expect_false(episode_eligibility_gate(cases, as.Date("2024-01-15"), config))
})

test_that("episode_eligibility_gate() fails a stream with a full year but almost no cases", {
  config <- episode_test_config()
  cases <- data.frame(sample_date = c("2023-02-01"))  # single case in the whole baseline year
  expect_false(episode_eligibility_gate(cases, as.Date("2024-01-01"), config))
})

test_that("episode_closure_criterion_met() fires once case_free_days has elapsed", {
  expect_false(episode_closure_criterion_met("2025-01-01", NA, case_free_days = 14, today = as.Date("2025-01-10")))
  expect_true(episode_closure_criterion_met("2025-01-01", NA, case_free_days = 14, today = as.Date("2025-01-15")))
})

test_that("episode_closure_criterion_met() extends to two incubation periods for a confirmed epidemic", {
  # case_free_days alone (14) would fire at day 15, but 2 * incub_max_days (2*10=20) is stricter
  expect_false(episode_closure_criterion_met(
    "2025-01-01", "confirmed_epidemic", case_free_days = 14, incub_max_days = 10,
    today = as.Date("2025-01-16")
  ))
  expect_true(episode_closure_criterion_met(
    "2025-01-01", "confirmed_epidemic", case_free_days = 14, incub_max_days = 10,
    today = as.Date("2025-01-22")
  ))
})

test_that("episode_closure_criterion_met() never fires for mem_applicable streams in M1", {
  expect_false(episode_closure_criterion_met(
    "2020-01-01", NA, case_free_days = 14, mem_applicable = TRUE, today = as.Date("2030-01-01")
  ))
})

test_that("episode_priority_score() stays within 0-100 across a range of inputs", {
  weights <- episode_test_config()$priority_score$weights
  scores <- vapply(1:20, function(i) {
    episode_priority_score(
      excess = i, ratio = i / 3, severity_weight = 0.8, growth_slope = i - 10,
      detector_agreement = (i %% 6) + 1, n_detectors = 6, density_ratio = if (i %% 2 == 0) i / 5 else NA,
      spatial_concentration = (i %% 10) / 10, weights = weights
    )
  }, numeric(1))
  expect_true(all(scores >= 0 & scores <= 100))
})

test_that("episode_priority_score() renormalises weights when density_ratio is NA", {
  weights <- episode_test_config()$priority_score$weights
  with_density <- episode_priority_score(
    excess = 5, ratio = 2, severity_weight = 1, detector_agreement = 1, n_detectors = 1,
    density_ratio = 1, weights = weights
  )
  without_density <- episode_priority_score(
    excess = 5, ratio = 2, severity_weight = 1, detector_agreement = 1, n_detectors = 1,
    density_ratio = NA, weights = weights
  )
  expect_false(identical(with_density, without_density))
  expect_true(is.finite(without_density))
})

test_that("episode_triangle_update() and episode_triangle_completeness() round-trip", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_region", "Test organism"), level = "pathogen_region",
    pathogen = "Test organism", observed_date = "2025-01-05"
  )
  cases <- data.frame(sample_date = c("2025-01-01", "2025-01-01", "2025-01-02"))
  episode_triangle_update(con, stream_id, cases, run_date = "2025-01-05")

  triangle <- DBI::dbGetQuery(con, "SELECT * FROM episode_reporting_triangle WHERE stream_id = ?",
                               params = list(stream_id))
  expect_equal(nrow(triangle), 2)
  expect_equal(sum(triangle$n_cases), 3)

  completeness <- episode_triangle_completeness(con, stream_id)
  expect_true(all(completeness$completeness >= 0 & completeness$completeness <= 1))
})

test_that("episode_db_denominator_upsert() and episode_denominator_ingest_run() round-trip", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))

  episode_db_denominator_upsert(con, pathogen = "Norovirus", sample_date = "2025-01-06",
                                 care_line = "second", area_code = NA, n_tests = 40)
  # upsert: same key, different n_tests, must update not duplicate
  episode_db_denominator_upsert(con, pathogen = "Norovirus", sample_date = "2025-01-06",
                                 care_line = "second", area_code = NA, n_tests = 55)

  rows <- DBI::dbGetQuery(con, "SELECT * FROM episode_denominator")
  expect_equal(nrow(rows), 1)
  expect_equal(rows$n_tests[1], 55)

  denom <- episode_denominator_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-02-28"), seed = 1
  )
  n_written <- episode_denominator_ingest_run(con, denom)
  expect_equal(n_written, nrow(denom))
  rows_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_denominator")
  expect_gt(rows_after$n, 1)
})

test_that("episode_denominator_ingest_run() rejects a source missing required columns", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  bad <- data.frame(pathogen = "Norovirus", sample_date = "2025-01-01")
  expect_error(episode_denominator_ingest_run(con, bad), "missing required")
})

test_that("episode_split_sql_statements() splits on semicolons and drops comments", {
  sql <- "-- a comment with a ; inside\nCREATE TABLE a (x INT);\nCREATE TABLE b (y INT);\n"
  statements <- EpiSODIC:::episode_split_sql_statements(sql)
  expect_equal(length(statements), 2)
  expect_true(all(grepl("^CREATE TABLE", statements)))
})
