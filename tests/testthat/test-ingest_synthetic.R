test_that("episode_ingest_source_synthetic() produces a valid ingestion source", {
  raw <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-06-30"), seed = 42
  )
  expect_silent(episode_ingest_validate_source(raw))
  expect_gt(nrow(raw), 0)
})

test_that("the same seed reproduces identical data", {
  raw1 <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-03-31"), seed = 7
  )
  raw2 <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-03-31"), seed = 7
  )
  expect_equal(nrow(raw1), nrow(raw2))
  expect_equal(sum(as.integer(as.factor(raw1$pathogen))), sum(as.integer(as.factor(raw2$pathogen))))
})

test_that("the injected point-source outbreak is present as a tight ward-level cluster", {
  raw <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 1
  )
  outbreak <- raw[grepl("^PT-OUTBREAK-PS-", raw$patient_key), ]
  expect_equal(nrow(outbreak), 14)
  expect_equal(length(unique(outbreak$ward)), 1)
  span <- diff(range(as.Date(outbreak$sample_date)))
  expect_lt(as.numeric(span), 14)  # tightly bunched, point-source shape
})

test_that("the injected propagated outbreak spans generation-interval-spaced waves", {
  raw <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 1
  )
  outbreak <- raw[grepl("^PT-OUTBREAK-PROP-", raw$patient_key), ]
  expect_equal(nrow(outbreak), 15)
  span <- diff(range(as.Date(outbreak$sample_date)))
  expect_gt(as.numeric(span), 40)  # spread across generations, propagated shape
})

test_that("episode_denominator_source_synthetic() produces a valid denominator source", {
  denom <- episode_denominator_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 3
  )
  expect_true(all(c("pathogen", "sample_date", "care_line", "area_code", "n_tests") %in% names(denom)))
  expect_gt(nrow(denom), 0)
  expect_true(all(denom$n_tests >= 0))
})
