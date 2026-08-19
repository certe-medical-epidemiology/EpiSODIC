pathogen_config_fixture <- data.frame(
  mo_code = "TEST_MO", episode_days = 30, stringsAsFactors = FALSE
)

raw_case <- function(source_key, patient_key, sample_date, mo_code = "TEST_MO") {
  data.frame(
    source_key = source_key, patient_key = patient_key, sample_date = sample_date,
    receipt_date = sample_date, mo_code = mo_code, determination = "DET",
    material = "faeces", care_line = "second", institution_key = "HOSP-01",
    institution_display_name = "Hospital", institution_type = "hospital",
    municipality = NA_character_, ward = "ICU", specialism = "Interne",
    pc4 = "9711", sex = "M", age = 40L, stringsAsFactors = FALSE
  )
}

test_that("isolates for the same patient within the episode window collapse to one case", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P1", "2025-01-10"),
    raw_case("K3", "P1", "2025-01-20")
  )
  deduped <- episode_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$sample_date, "2025-01-01")  # earliest kept, sample date is the anchor
})

test_that("a gap longer than episode_days starts a new episode", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01"),
    raw_case("K2", "P1", "2025-03-01")  # 59 days later, beyond the 30-day window
  )
  deduped <- episode_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("different patients are never merged", {
  raw <- rbind(raw_case("K1", "P1", "2025-01-01"), raw_case("K2", "P2", "2025-01-01"))
  deduped <- episode_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("different organisms for the same patient are never merged", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", mo_code = "TEST_MO"),
    raw_case("K2", "P1", "2025-01-01", mo_code = "OTHER_MO")
  )
  deduped <- episode_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 2)
})

test_that("an organism missing from pathogen_config falls back to the schema default of 30 days", {
  raw <- rbind(
    raw_case("K1", "P1", "2025-01-01", mo_code = "UNKNOWN_MO"),
    raw_case("K2", "P1", "2025-01-20", mo_code = "UNKNOWN_MO")
  )
  deduped <- episode_dedup(raw, pathogen_config_fixture)
  expect_equal(nrow(deduped), 1)
})

test_that("an empty input returns an empty output without error", {
  empty <- raw_case("K1", "P1", "2025-01-01")[0, ]
  expect_equal(nrow(episode_dedup(empty, pathogen_config_fixture)), 0)
})

test_that("episode_ingest_validate_source() rejects a column outside the allow-list", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  raw$requesting_clinician <- "Dr Smith"
  expect_error(episode_ingest_validate_source(raw), "allow-list")
})

test_that("episode_ingest_validate_source() rejects a missing required column", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  raw$pc4 <- NULL
  expect_error(episode_ingest_validate_source(raw), "missing required")
})

test_that("episode_ingest_validate_source() rejects duplicate source_key values", {
  raw <- rbind(raw_case("K1", "P1", "2025-01-01"), raw_case("K1", "P2", "2025-01-02"))
  expect_error(episode_ingest_validate_source(raw), "duplicate")
})

test_that("episode_ingest_validate_source() accepts a well-formed source", {
  raw <- raw_case("K1", "P1", "2025-01-01")
  expect_silent(episode_ingest_validate_source(raw))
})
