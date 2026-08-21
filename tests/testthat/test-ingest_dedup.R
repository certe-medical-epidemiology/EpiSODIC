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
  pathogen = "Test pathogen", episode_days = 30, stringsAsFactors = FALSE
)

raw_case <- function(source_key, patient_key, sample_date, pathogen = "Test pathogen") {
  data.frame(
    source_key = source_key, patient_key = patient_key, sample_date = sample_date,
    receipt_date = sample_date, pathogen = pathogen,
    care_line = "second", institution_key = "HOSP-01",
    institution_display_name = "Hospital", institution_type = "hospital",
    municipality = NA_character_, ward = "ICU", specialism = "Interne",
    pc = "9711", sex = "M", age = 40L, stringsAsFactors = FALSE
  )
}

test_that("isolates for the same patient within the episode window collapse to one case", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P1", "2025-01-10"),
    raw_case("K3", "P1", "2025-01-20")
  )
  deduped <- episodic_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$sample_date, "2025-01-01")  # earliest kept, sample date is the anchor
})

test_that("a gap longer than episode_days starts a new episode", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P1", "2025-03-01")  # 59 days later, beyond the 30-day window
  )
  deduped <- episodic_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("different patients are never merged", {
  raw <- rbind(raw_case("K1", "P1", "2025-01-01"), raw_case("K2", "P2", "2025-01-01"))
  deduped <- episodic_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("different pathogens for the same patient are never merged", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", pathogen = "Test pathogen"),
    raw_case("K2", "P1", "2025-01-01", pathogen = "Other pathogen")
  )
  deduped <- episodic_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("the same isolate tagged under two pathogen values (e.g. E. coli and ETEC) is not merged", {
  # deliberately mirrors the operator's own transform: one ETEC isolate can
  # legitimately appear as two rows, "Escherichia coli" and "ETEC", so each
  # is watched on its own
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", pathogen = "Escherichia coli"),
    raw_case("K1-ETEC", "P1", "2025-01-01", pathogen = "ETEC")
  )
  deduped <- episodic_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("a pathogen missing from pathogen_config falls back to the schema default of 30 days", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", pathogen = "Unknown pathogen"),
    raw_case("K2", "P1", "2025-01-20", pathogen = "Unknown pathogen")
  )
  deduped <- episodic_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
})

test_that("an empty input returns an empty output without error", {
  empty <- raw_case("K1", "P1", "2025-01-01")[0, ]
  expect_equal(nrow(episodic_dedup(empty, pathogen_config_fixture)), 0)
})

test_that("episodic_ingest_validate_source() rejects a column outside the allow-list", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  raw$requesting_clinician <- "Dr Smith"
  expect_error(episodic_ingest_validate_source(raw), "allow-list")
})

test_that("episodic_ingest_validate_source() rejects a missing required column", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  raw$pc <- NULL
  expect_error(episodic_ingest_validate_source(raw), "missing required")
})

test_that("episodic_ingest_validate_source() rejects duplicate source_key values", {
  raw <- rbind(raw_case("K1", "P1", "2025-01-01"), raw_case("K1", "P2", "2025-01-02"))
  expect_error(episodic_ingest_validate_source(raw), "duplicate")
})

test_that("episodic_ingest_validate_source() accepts a well-formed source", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  expect_silent(episodic_ingest_validate_source(raw))
})
