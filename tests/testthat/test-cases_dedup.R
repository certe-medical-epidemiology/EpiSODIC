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

pathogen_config_fixture <- data.frame(
  pathogen = "Test pathogen",
  episode_days = 30,
  stringsAsFactors = FALSE
)

raw_case <- function(
  source_key,
  patient_key,
  sample_date,
  pathogen = "Test pathogen"
) {
  data.frame(
    source_key = source_key,
    patient_key = patient_key,
    sample_date = sample_date,
    receipt_date = sample_date,
    pathogen = pathogen,
    care_line = "second",
    institution_key = "HOSP-01",
    institution_display_name = "Hospital",
    institution_type = "hospital",
    municipality = NA_character_,
    ward = "ICU",
    specialism = "Interne",
    pc = "9711",
    sex = "M",
    age = 40L,
    stringsAsFactors = FALSE
  )
}

test_that("positives for the same patient within the episode window collapse to one case", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P1", "2025-01-10"),
    raw_case("K3", "P1", "2025-01-20")
  )
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$sample_date, "2025-01-01") # earliest kept, sample date is the anchor
})

test_that("a gap longer than episode_days starts a new episode", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P1", "2025-03-01") # 59 days later, beyond the 30-day window
  )
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("a positive within episode_days of an already-stored episode is dropped, not inserted as new", {
  raw <- raw_case("K2", "P1", "2025-01-20") # 19 days after the stored anchor below
  deduped <- episodic_cases_deduplicate(
    raw,
    pathogen_config_fixture,
    existing = c("P1Test pathogen" = "2025-01-01")
  )
  expect_equal(nrow(deduped), 0)
})

test_that("a positive beyond episode_days of an already-stored episode starts a new one", {
  raw <- raw_case("K2", "P1", "2025-03-01") # 59 days after the stored anchor below
  deduped <- episodic_cases_deduplicate(
    raw,
    pathogen_config_fixture,
    existing = c("P1Test pathogen" = "2025-01-01")
  )
  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$sample_date, "2025-03-01")
})

test_that("existing anchors for other patients/pathogens are ignored", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  deduped <- episodic_cases_deduplicate(
    raw,
    pathogen_config_fixture,
    existing = c("P2Other pathogen" = "2025-01-01")
  )
  expect_equal(nrow(deduped), 1)
})

test_that("a batch spanning both a continuation and a new episode keeps only the new one", {
  raw <- rbind(
    raw_case("K2", "P1", "2025-01-10"), # within 30 days of the stored anchor: continuation
    raw_case("K3", "P1", "2025-03-01") # beyond it: new episode
  )
  deduped <- episodic_cases_deduplicate(
    raw,
    pathogen_config_fixture,
    existing = c("P1Test pathogen" = "2025-01-01")
  )
  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$source_key, "K3")
})

test_that("without existing anchors, behaviour is unchanged (existing defaults to NULL)", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
})

test_that("different patients are never merged", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P2", "2025-01-01")
  )
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("different pathogens for the same patient are never merged", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", pathogen = "Test pathogen"),
    raw_case("K2", "P1", "2025-01-01", pathogen = "Other pathogen")
  )
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("the same positive tagged under two pathogen values (e.g. E. coli and ETEC) is not merged", {
  # deliberately mirrors the operator's own transform: one ETEC positive can
  # legitimately appear as two rows, "Escherichia coli" and "ETEC", so each
  # is watched on its own
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", pathogen = "Escherichia coli"),
    raw_case("K1-ETEC", "P1", "2025-01-01", pathogen = "ETEC")
  )
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("a pathogen missing from pathogen_config falls back to the schema default of 30 days", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", pathogen = "Unknown pathogen"),
    raw_case("K2", "P1", "2025-01-20", pathogen = "Unknown pathogen")
  )
  deduped <- episodic_cases_deduplicate(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
})

test_that("an empty input returns an empty output without error", {
  empty <- raw_case("K1", "P1", "2025-01-01")[0, ]
  expect_equal(
    nrow(episodic_cases_deduplicate(empty, pathogen_config_fixture)),
    0
  )
})

test_that("episodic_validate_cases() rejects a column outside the allow-list", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  raw$requesting_clinician <- "Dr Smith"
  expect_error(episodic_validate_cases(raw), "allow-list")
})

test_that("episodic_validate_cases() rejects a missing required column", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  raw$pc <- NULL
  expect_error(episodic_validate_cases(raw), "missing required")
})

test_that("episodic_validate_cases() rejects duplicate source_key values", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K1", "P2", "2025-01-02")
  )
  expect_error(episodic_validate_cases(raw), "duplicate")
})

test_that("episodic_validate_cases() accepts a well-formed data set", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  expect_silent(episodic_validate_cases(raw))
  expect_identical(episodic_validate_cases(raw), raw)
})

test_that("episodic_validate_cases() also accepts a function returning the data set", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  expect_silent(episodic_validate_cases(function() raw))
  expect_identical(episodic_validate_cases(function() raw), raw)
})

test_that("episodic_validate_cases() rejects something that is no data set at all", {
  expect_error(episodic_validate_cases(1), "data frame")
  expect_error(episodic_validate_cases(function() 1), "data frame")
})

test_that("episodic_validate_cases() rejects NA in a column that must always be filled", {
  for (column in episodic_case_columns_required) {
    cases <- raw_case("K1", "P1", "2025-01-01")
    cases[[column]] <- NA
    expect_error(episodic_validate_cases(cases), column, fixed = TRUE)
  }
})

test_that("episodic_validate_cases() allows NA in the optional columns", {
  cases <- raw_case("K1", "P1", "2025-01-01")
  for (column in c(
    "receipt_date",
    "care_line",
    "municipality",
    "ward",
    "specialism",
    "pc",
    "sex",
    "age"
  )) {
    cases[[column]] <- NA
  }
  expect_silent(episodic_validate_cases(cases))
})

test_that("episodic_validate_cases() accepts an NA care_line and leaves it alone", {
  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$care_line <- NA_character_
  expect_silent(episodic_validate_cases(cases))
  expect_true(is.na(episodic_validate_cases(cases)$care_line))
})

test_that("episodic_validate_cases() rejects a value outside the allowed set", {
  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$care_line <- "primary_care"
  expect_error(episodic_validate_cases(cases), "care_line", fixed = TRUE)

  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$institution_type <- "clinic"
  expect_error(episodic_validate_cases(cases), "institution_type", fixed = TRUE)

  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$sex <- "male"
  expect_error(episodic_validate_cases(cases), "sex", fixed = TRUE)
})

test_that("episodic_validate_cases() accepts every documented allowed value", {
  for (value in episodic_care_lines) {
    cases <- raw_case("K1", "P1", "2025-01-01")
    cases$care_line <- value
    expect_silent(episodic_validate_cases(cases))
  }
  for (value in episodic_institution_types) {
    cases <- raw_case("K1", "P1", "2025-01-01")
    cases$institution_type <- value
    expect_silent(episodic_validate_cases(cases))
  }
  for (value in episodic_sex_codes) {
    cases <- raw_case("K1", "P1", "2025-01-01")
    cases$sex <- value
    expect_silent(episodic_validate_cases(cases))
  }
})

test_that("episodic_validate_cases() rejects a date that does not read as YYYY-MM-DD", {
  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$receipt_date <- "not a date"
  expect_error(episodic_validate_cases(cases), "receipt_date", fixed = TRUE)
})

test_that("episodic_validate_cases() rejects a date that only parses because of a trailing remainder", {
  # as.Date(format = "%Y-%m-%d") matches a prefix and ignores the rest, so
  # a day-first date reads as the year 1 instead of failing. Everything
  # here parses; none of it is the date the operator meant.
  for (value in c(
    "01-01-2025",
    "1-1-2025",
    "2025-01-01T00:00:00",
    "2025-01-01 extra"
  )) {
    cases <- raw_case("K1", "P1", value)
    expect_error(episodic_validate_cases(cases), "sample_date", fixed = TRUE)
  }
})

test_that("episodic_validate_cases() rejects an ISO-shaped string that is not a real date", {
  cases <- raw_case("K1", "P1", "2025-13-45")
  expect_error(episodic_validate_cases(cases), "sample_date", fixed = TRUE)
})

test_that("episodic_validate_cases() accepts Date columns as well as ISO strings", {
  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$sample_date <- as.Date(cases$sample_date)
  cases$receipt_date <- as.Date(cases$receipt_date)
  expect_silent(episodic_validate_cases(cases))
})

test_that("episodic_validate_cases() rejects an age that is not numeric", {
  cases <- raw_case("K1", "P1", "2025-01-01")
  cases$age <- "40-49"
  expect_error(episodic_validate_cases(cases), "age", fixed = TRUE)
})

test_that("episodic_db_last_case_dates() returns the latest sample_date per patient/pathogen already stored", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  institution_id <- episodic_db_institution_upsert(
    con,
    institution_key = digest::digest(
      "hosp-dedup",
      algo = "sha1",
      serialize = FALSE
    ),
    display_name = "Test Hospital",
    institution_type = "hospital",
    care_line = "second",
    is_monitored = TRUE
  )
  run_id <- episodic_db_run_start(con, "host", "account")
  cases <- data.frame(
    source_key = c("K1", "K2"),
    patient_key = c("P1", "P1"),
    sample_date = c("2025-01-01", "2025-01-20"),
    receipt_date = c("2025-01-01", "2025-01-20"),
    pathogen = c("Test pathogen", "Other pathogen"),
    care_line = "second",
    institution_id = institution_id,
    ward = "ICU",
    specialism = "Interne",
    pc = "9711",
    sex = "M",
    age = 40L,
    stringsAsFactors = FALSE
  )
  episodic_db_case_insert_new(con, cases, run_id)

  result <- episodic_db_last_case_dates(con, "P1", "Test pathogen")
  expect_equal(unname(result), "2025-01-01")
  expect_equal(names(result), "P1Test pathogen")

  # a patient/pathogen combination with nothing stored yet
  expect_equal(
    length(episodic_db_last_case_dates(con, "P2", "Test pathogen")),
    0
  )
})

test_that("episodic_db_last_case_dates() returns an empty named vector when either input is empty", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  result <- episodic_db_last_case_dates(con, character(0), "Test pathogen")
  expect_equal(length(result), 0)
  expect_true(is.character(result))
})
