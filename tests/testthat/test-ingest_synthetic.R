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

test_that("episode_ingest_source_synthetic_calibration() produces a valid source with real signal volume for one pathogen", {
  raw <- episode_ingest_source_synthetic_calibration(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"),
    pathogen = "Clostridioides difficile", n_bumps_per_month = 3, seed = 5
  )
  expect_silent(episode_ingest_validate_source(raw))

  # far more volume than the baseline alone would produce for this organism
  baseline_only <- episode_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 5
  )
  n_baseline <- sum(baseline_only$pathogen == "Clostridioides difficile")
  n_calibration <- sum(raw$pathogen == "Clostridioides difficile")
  expect_gt(n_calibration, n_baseline)

  # every volume-generated case is traceable as synthetic, never mistaken for real data
  volume_cases <- raw[grepl("^PT-VOL-", raw$patient_key), ]
  expect_true(all(volume_cases$pathogen == "Clostridioides difficile"))
  expect_true(all(volume_cases$institution_type %in% c("hospital", "ltc_institution")))
})

test_that("episode_ingest_source_synthetic_calibration() responds to n_bumps_per_month", {
  few <- episode_ingest_source_synthetic_calibration(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), n_bumps_per_month = 1, seed = 9
  )
  many <- episode_ingest_source_synthetic_calibration(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), n_bumps_per_month = 8, seed = 9
  )
  n_few <- sum(grepl("^PT-VOL-", few$patient_key))
  n_many <- sum(grepl("^PT-VOL-", many$patient_key))
  expect_gt(n_many, n_few)
})

test_that("episode_ingest_source_synthetic_calibration() runs through detection and produces many clusters for the named pathogen", {
  db_path <- tempfile(fileext = ".sqlite")
  episode_run_cron(
    db_path,
    ingest_source_fn = function() episode_ingest_source_synthetic_calibration(
      start_date = as.Date("2024-01-01"), end_date = as.Date("2024-06-30"), n_bumps_per_month = 4, seed = 5
    ),
    run_date = as.Date("2024-06-30")
  )
  con <- episode_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  clusters <- episode_db_clusters(con)
  streams <- episode_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(clusters$stream_id, streams$stream_id)]
  n_cdiff_clusters <- sum(clusters$pathogen == "Clostridioides difficile")
  expect_gt(n_cdiff_clusters, 10)  # real signal volume, not the 0-2 the baseline alone would give
})

test_that("episode_denominator_source_synthetic() produces a valid denominator source", {
  denom <- episode_denominator_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 3
  )
  expect_true(all(c("pathogen", "sample_date", "care_line", "area_code", "n_tests") %in% names(denom)))
  expect_gt(nrow(denom), 0)
  expect_true(all(denom$n_tests >= 0))
})
