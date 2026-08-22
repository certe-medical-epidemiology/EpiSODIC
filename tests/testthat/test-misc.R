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

# Smaller-scope exported functions, one file to keep the test suite easy to
# navigate; every exported function gets direct coverage.

test_that("episodic_eligibility_gate() passes a stream with a full year of steady weekly counts", {
  config <- episodic_test_config()
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-01-01"), by = "week")
  cases <- data.frame(sample_date = as.character(dates))
  expect_true(episodic_eligibility_gate(cases, as.Date("2024-01-01"), config))
})

test_that("episodic_eligibility_gate() fails a stream with too little baseline history", {
  config <- episodic_test_config()
  cases <- data.frame(
    sample_date = as.character(seq(
      as.Date("2024-01-01"),
      as.Date("2024-01-15"),
      by = "day"
    ))
  )
  expect_false(episodic_eligibility_gate(cases, as.Date("2024-01-15"), config))
})

test_that("episodic_eligibility_gate() fails a stream with a full year but almost no cases", {
  config <- episodic_test_config()
  cases <- data.frame(sample_date = c("2023-02-01")) # single case in the whole baseline year
  expect_false(episodic_eligibility_gate(cases, as.Date("2024-01-01"), config))
})

test_that("episodic_closure_criterion_met() fires once case_free_days has elapsed", {
  expect_false(episodic_closure_criterion_met(
    "2025-01-01",
    NA,
    case_free_days = 14,
    today = as.Date("2025-01-10")
  ))
  expect_true(episodic_closure_criterion_met(
    "2025-01-01",
    NA,
    case_free_days = 14,
    today = as.Date("2025-01-15")
  ))
})

test_that("episodic_closure_criterion_met() extends to two incubation periods for a confirmed epidemic", {
  # case_free_days alone (14) would fire at day 15, but 2 * incub_max_days (2*10=20) is stricter
  expect_false(episodic_closure_criterion_met(
    "2025-01-01",
    "confirmed_epidemic",
    case_free_days = 14,
    incub_max_days = 10,
    today = as.Date("2025-01-16")
  ))
  expect_true(episodic_closure_criterion_met(
    "2025-01-01",
    "confirmed_epidemic",
    case_free_days = 14,
    incub_max_days = 10,
    today = as.Date("2025-01-22")
  ))
})

test_that("episodic_closure_criterion_met() never fires for mem_applicable streams in M1", {
  expect_false(episodic_closure_criterion_met(
    "2020-01-01",
    NA,
    case_free_days = 14,
    mem_applicable = TRUE,
    today = as.Date("2030-01-01")
  ))
})

test_that("episodic_priority_score() stays within 0-100 across a range of inputs", {
  weights <- episodic_test_config()$priority_score$weights
  scores <- vapply(
    1:20,
    function(i) {
      episodic_priority_score(
        excess = i,
        ratio = i / 3,
        severity_weight = 0.8,
        growth_slope = i - 10,
        detector_agreement = (i %% 6) + 1,
        n_detectors = 6,
        density_ratio = if (i %% 2 == 0) i / 5 else NA,
        spatial_concentration = (i %% 10) / 10,
        weights = weights
      )
    },
    numeric(1)
  )
  expect_true(all(scores >= 0 & scores <= 100))
})

test_that("episodic_priority_score() renormalises weights when density_ratio is NA", {
  weights <- episodic_test_config()$priority_score$weights
  with_density <- episodic_priority_score(
    excess = 5,
    ratio = 2,
    severity_weight = 1,
    detector_agreement = 1,
    n_detectors = 1,
    density_ratio = 1,
    weights = weights
  )
  without_density <- episodic_priority_score(
    excess = 5,
    ratio = 2,
    severity_weight = 1,
    detector_agreement = 1,
    n_detectors = 1,
    density_ratio = NA,
    weights = weights
  )
  expect_false(identical(with_density, without_density))
  expect_true(is.finite(without_density))
})

test_that("episodic_triangle_update() and episodic_triangle_completeness() round-trip", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key("pathogen_region", "Test pathogen"),
    level = "pathogen_region",
    pathogen = "Test pathogen",
    observed_date = "2025-01-05"
  )
  cases <- data.frame(sample_date = c("2025-01-01", "2025-01-01", "2025-01-02"))
  episodic_triangle_update(con, stream_id, cases, run_date = "2025-01-05")

  triangle <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_reporting_triangle WHERE stream_id = ?",
    params = list(stream_id)
  )
  expect_equal(nrow(triangle), 2)
  expect_equal(sum(triangle$n_cases), 3)

  completeness <- episodic_triangle_completeness(con, stream_id)
  expect_true(all(
    completeness$completeness >= 0 & completeness$completeness <= 1
  ))
})

test_that("episodic_db_denominator_upsert() and episodic_denominators_load() round-trip", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  episodic_db_denominator_upsert(
    con,
    pathogen = "Norovirus",
    sample_date = "2025-01-06",
    care_line = "second",
    area_code = NA,
    n_tests = 40
  )
  # upsert: same key, different n_tests, must update not duplicate
  episodic_db_denominator_upsert(
    con,
    pathogen = "Norovirus",
    sample_date = "2025-01-06",
    care_line = "second",
    area_code = NA,
    n_tests = 55
  )

  rows <- DBI::dbGetQuery(con, "SELECT * FROM episodic_denominator")
  expect_equal(nrow(rows), 1)
  expect_equal(rows$n_tests[1], 55)

  denom <- episodic_synthetic_denominators(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-02-28"),
    seed = 1
  )
  counts <- episodic_denominators_load(con, denom)
  expect_equal(counts$n_written, nrow(denom))
  expect_equal(counts$n_supplied, nrow(denom))
  rows_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) n FROM episodic_denominator"
  )
  expect_gt(rows_after$n, 1)
})

test_that("episodic_denominators_load() rejects a source missing required columns", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  bad <- data.frame(pathogen = "Norovirus", sample_date = "2025-01-01")
  expect_error(episodic_denominators_load(con, bad), "missing required")
})

test_that("episodic_denominators_load() rejects bad values before the database has to", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  ok <- data.frame(
    pathogen = "Norovirus",
    sample_date = "2025-01-06",
    care_line = "second",
    area_code = NA_character_,
    n_tests = 40,
    stringsAsFactors = FALSE
  )

  bad <- ok
  bad$care_line <- "hospital"
  expect_error(episodic_denominators_load(con, bad), "care_line")

  bad <- ok
  bad$sample_date <- "06-01-2025"
  expect_error(episodic_denominators_load(con, bad), "sample_date")

  bad <- ok
  bad$n_tests <- "forty"
  expect_error(episodic_denominators_load(con, bad), "n_tests")

  bad <- ok
  bad$pathogen <- NA_character_
  expect_error(episodic_denominators_load(con, bad), "pathogen")
})

test_that("episodic_denominators_load() reads an NA care_line as 'unknown', like the case feed", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  denom <- data.frame(
    pathogen = "Norovirus",
    sample_date = "2025-01-06",
    care_line = NA_character_,
    area_code = NA_character_,
    n_tests = 40,
    stringsAsFactors = FALSE
  )
  episodic_denominators_load(con, denom)
  rows <- DBI::dbGetQuery(con, "SELECT care_line FROM episodic_denominator")
  expect_identical(rows$care_line, "unknown")
})

test_that("episodic_split_sql_statements() splits on semicolons and drops comments", {
  sql <- "-- a comment with a ; inside\nCREATE TABLE a (x INT);\nCREATE TABLE b (y INT);\n"
  statements <- EpiSODIC:::episodic_split_sql_statements(sql)
  expect_equal(length(statements), 2)
  expect_true(all(grepl("^CREATE TABLE", statements)))
})

test_that("episodic_priority_score() renormalises around any component it cannot compute, not just density", {
  # same_place and rare_trigger fit no baseline, so excess and ratio do
  # not exist for them. Scoring that absence as zero-with-weight meant
  # every ward-level same-place cluster was systematically outranked by
  # Farrington signals purely because Farrington is the detector that
  # happens to report an expectation.
  weights <- episodic_test_config()$priority_score$weights
  no_baseline <- episodic_priority_score(
    excess = NA,
    ratio = NA,
    severity_weight = 0.9,
    growth_slope = 3,
    detector_agreement = 1,
    n_detectors = 4,
    density_ratio = NA,
    spatial_concentration = 0.9,
    weights = weights
  )
  # Identical evidence, but with a baseline that says the count is
  # exactly as expected: measured-and-unremarkable must rank below
  # not-measurable.
  at_baseline <- episodic_priority_score(
    excess = 0,
    ratio = 1,
    severity_weight = 0.9,
    growth_slope = 3,
    detector_agreement = 1,
    n_detectors = 4,
    density_ratio = NA,
    spatial_concentration = 0.9,
    weights = weights
  )
  expect_true(is.finite(no_baseline))
  expect_gt(no_baseline, at_baseline)
})

test_that("episodic_priority_score() anchors ratio and density at 'as expected', not at half a score", {
  weights <- list(
    excess_component = 0,
    ratio_component = 1,
    severity_component = 0,
    growth_component = 0,
    agreement_component = 0,
    density_component = 0,
    spatial_component = 0
  )
  expect_equal(episodic_priority_score(ratio = 1, weights = weights), 0)
  expect_gt(episodic_priority_score(ratio = 3, weights = weights), 0)
  expect_true(episodic_priority_score(ratio = 5, weights = weights) <= 100)
})

test_that("episodic_priority_score() still returns a number when nothing at all is computable", {
  weights <- list(
    excess_component = 1,
    ratio_component = 1,
    severity_component = 0,
    growth_component = 0,
    agreement_component = 0,
    density_component = 1,
    spatial_component = 0
  )
  expect_equal(
    episodic_priority_score(
      excess = NA,
      ratio = NA,
      density_ratio = NA,
      weights = weights
    ),
    0
  )
})

test_that("episodic_growth_slope() is positive while climbing, zero once flat or falling", {
  climbing <- data.frame(
    sample_date = rep(
      seq(as.Date("2025-01-06"), by = "week", length.out = 3),
      times = c(2, 5, 11)
    )
  )
  expect_gt(
    episodic_growth_slope(climbing, last_day = as.Date("2025-01-26")),
    0
  )

  flat <- data.frame(
    sample_date = rep(
      seq(as.Date("2025-01-06"), by = "week", length.out = 3),
      times = c(4, 4, 4)
    )
  )
  expect_equal(episodic_growth_slope(flat, last_day = as.Date("2025-01-26")), 0)

  falling <- data.frame(
    sample_date = rep(
      seq(as.Date("2025-01-06"), by = "week", length.out = 3),
      times = c(11, 5, 2)
    )
  )
  expect_lt(episodic_growth_slope(falling, last_day = as.Date("2025-01-26")), 0)

  expect_equal(
    episodic_growth_slope(
      data.frame(sample_date = as.Date(character(0))),
      last_day = as.Date("2025-01-26")
    ),
    0
  )
})

test_that("episodic_spatial_concentration() ignores cases with no postcode entirely", {
  # A missing postcode is absence of evidence about localisation, not
  # evidence of dispersal.
  cases <- data.frame(
    pc = c("9711", "9711", "9711", NA, NA),
    stringsAsFactors = FALSE
  )
  expect_equal(episodic_spatial_concentration(cases), 1)

  mixed <- data.frame(
    pc = c("9711", "9711", "9712", "9713"),
    stringsAsFactors = FALSE
  )
  expect_equal(episodic_spatial_concentration(mixed), 0.5)

  expect_equal(episodic_spatial_concentration(data.frame(pc = c(NA, NA))), 0)
})

test_that("episodic_cases_in_window() keeps only the candidate episode's own cases", {
  cases <- data.frame(
    sample_date = as.character(seq(
      as.Date("2025-01-01"),
      by = "day",
      length.out = 10
    ))
  )
  window <- episodic_cases_in_window(cases, "2025-01-03", "2025-01-05")
  expect_equal(nrow(window), 3)
})
