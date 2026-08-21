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

test_that("episodic_ingest_source_synthetic() produces a valid ingestion source", {
  raw <- episodic_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-06-30"), seed = 42
  )
  expect_silent(episodic_ingest_validate_source(raw))
  expect_gt(nrow(raw), 0)
})

test_that("the same seed reproduces identical data", {
  raw1 <- episodic_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-03-31"), seed = 7
  )
  raw2 <- episodic_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-03-31"), seed = 7
  )
  expect_equal(nrow(raw1), nrow(raw2))
  expect_equal(sum(as.integer(as.factor(raw1$pathogen))), sum(as.integer(as.factor(raw2$pathogen))))
})

test_that("the injected point-source outbreak is present as a tight ward-level cluster", {
  raw <- episodic_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 1
  )
  outbreak <- raw[grepl("^PT-OUTBREAK-PS-", raw$patient_key), ]
  expect_equal(nrow(outbreak), 14)
  expect_equal(length(unique(outbreak$ward)), 1)
  span <- diff(range(as.Date(outbreak$sample_date)))
  expect_lt(as.numeric(span), 14)  # tightly bunched, point-source shape
})

test_that("the injected propagated outbreak spans generation-interval-spaced waves", {
  raw <- episodic_ingest_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 1
  )
  outbreak <- raw[grepl("^PT-OUTBREAK-PROP-", raw$patient_key), ]
  expect_equal(nrow(outbreak), 15)
  span <- diff(range(as.Date(outbreak$sample_date)))
  expect_gt(as.numeric(span), 40)  # spread across generations, propagated shape
})

test_that("episodic_ingest_source_synthetic_calibration() produces a valid source with real signal volume for one pathogen", {
  raw <- episodic_ingest_source_synthetic_calibration(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"),
    pathogen = "Clostridioides difficile", n_bumps_per_month = 3, seed = 5
  )
  expect_silent(episodic_ingest_validate_source(raw))

  # far more volume than the baseline alone would produce for this organism
  baseline_only <- episodic_ingest_source_synthetic(
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

test_that("episodic_ingest_source_synthetic_calibration() responds to n_bumps_per_month", {
  few <- episodic_ingest_source_synthetic_calibration(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), n_bumps_per_month = 1, seed = 9
  )
  many <- episodic_ingest_source_synthetic_calibration(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), n_bumps_per_month = 8, seed = 9
  )
  n_few <- sum(grepl("^PT-VOL-", few$patient_key))
  n_many <- sum(grepl("^PT-VOL-", many$patient_key))
  expect_gt(n_many, n_few)
})

test_that("episodic_ingest_source_synthetic_calibration() runs through detection and produces many clusters for the named pathogen", {
  db_path <- tempfile(fileext = ".sqlite")
  episodic_run_cron(
    db_path,
    ingest_source = function() episodic_ingest_source_synthetic_calibration(
      start_date = as.Date("2024-01-01"), end_date = as.Date("2024-06-30"), n_bumps_per_month = 4, seed = 5
    ),
    run_date = as.Date("2024-06-30")
  )
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  clusters <- episodic_db_clusters(con)
  streams <- episodic_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(clusters$stream_id, streams$stream_id)]
  n_cdiff_clusters <- sum(clusters$pathogen == "Clostridioides difficile")
  expect_gt(n_cdiff_clusters, 10)  # real signal volume, not the 0-2 the baseline alone would give
})

test_that("episodic_denominator_source_synthetic() produces a valid denominator source", {
  denom <- episodic_denominator_source_synthetic(
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"), seed = 3
  )
  expect_true(all(c("pathogen", "sample_date", "care_line", "area_code", "n_tests") %in% names(denom)))
  expect_gt(nrow(denom), 0)
  expect_true(all(denom$n_tests >= 0))
})
