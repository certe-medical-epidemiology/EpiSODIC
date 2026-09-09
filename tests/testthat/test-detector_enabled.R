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

# `<detector>.enabled`: a detector switched off contributes nothing, and
# says which one it was rather than reporting "found 0".

test_that("every detector ships switched on", {
  config <- episodic_test_config()
  for (detector in episodic_validation_detectors()) {
    expect_true(
      episodic_detector_enabled(config, detector),
      info = detector
    )
  }
})

test_that("a value that is not true or false is refused", {
  config <- episodic_test_config()
  config$farrington$enabled <- "yes"
  expect_error(episodic_detector_enabled(config, "farrington"), "true or false")
  config$farrington$enabled <- NA
  expect_error(episodic_detector_enabled(config, "farrington"), "true or false")
  config$farrington$enabled <- c(TRUE, TRUE)
  expect_error(episodic_detector_enabled(config, "farrington"), "true or false")
})

test_that("same_place switched off finds nothing where it would have fired", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episodic_test_institution(con, "hosp-enabled")
  institutions <- episodic_db_institutions(con)
  cases <- data.frame(
    case_id = 1:3,
    source_key = sprintf("E%d", 1:3),
    patient_key = sprintf("P%d", 1:3),
    sample_date = c("2025-03-01", "2025-03-03", "2025-03-05"),
    pathogen = "Norovirus",
    care_line = "second",
    institution_id = institution_id,
    ward = "B4",
    stringsAsFactors = FALSE
  )
  config <- episodic_test_config()
  run_date <- as.Date("2025-03-06")

  on_result <- episodic_detect_same_place(
    con,
    cases,
    institutions,
    config,
    run_date = run_date
  )
  expect_gt(nrow(on_result), 0)

  config$same_place$enabled <- FALSE
  off_result <- episodic_detect_same_place(
    con,
    cases,
    institutions,
    config,
    run_date = run_date
  )
  expect_equal(nrow(off_result), 0)
  expect_named(off_result, names(on_result))
})

test_that("rare_trigger switched off finds nothing where it would have fired", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episodic_test_institution(con, "hosp-enabled-rare")
  cases <- data.frame(
    case_id = 1L,
    source_key = "R1",
    patient_key = "P1",
    sample_date = "2025-03-01",
    pathogen = "Neisseria meningitidis",
    care_line = "second",
    institution_id = institution_id,
    ward = "B4",
    stringsAsFactors = FALSE
  )
  config <- episodic_test_config()
  run_date <- as.Date("2025-03-02")

  expect_gt(
    nrow(episodic_detect_rare_trigger(con, cases, config, run_date = run_date)),
    0
  )
  config$rare_trigger$enabled <- FALSE
  expect_equal(
    nrow(episodic_detect_rare_trigger(con, cases, config, run_date = run_date)),
    0
  )
})

test_that("farrington and mem switched off return nothing at all", {
  config <- episodic_test_config()
  config$farrington$enabled <- FALSE
  config$mem$enabled <- FALSE
  cases <- data.frame(
    sample_date = as.character(seq(
      as.Date("2020-01-01"),
      as.Date("2025-01-01"),
      by = "day"
    )),
    stringsAsFactors = FALSE
  )
  farrington <- episodic_detect_farrington(
    cases,
    stream_id = 1L,
    config = config,
    run_date = as.Date("2025-01-01")
  )
  expect_equal(nrow(farrington), 0)
  # Switched off is not the same statement as "not enough baseline", so
  # it must not carry the shortfall a short history reports.
  expect_null(episodic_farrington_shortfall(farrington))
  expect_equal(
    nrow(episodic_detect_mem(cases, 1L, as.Date("2025-01-01"), config)),
    0
  )
})

test_that("switching a detector off changes the config hash", {
  config <- episodic_test_config()
  off <- config
  off$mem$enabled <- FALSE
  expect_false(
    identical(
      episodic_config_hash(config)$hash,
      episodic_config_hash(off)$hash
    )
  )
})

test_that("an instance configuration may switch a detector off", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(c("farrington:", "  enabled: false"), path)
  resolved <- episodic_config_resolve(path)
  expect_false(episodic_detector_enabled(resolved, "farrington"))
  expect_true(episodic_detector_enabled(resolved, "same_place"))
})
