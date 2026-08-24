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

# How many windows `same_place` would fire on, without a database: the
# detector's own grouping (ward inside a hospital, institution elsewhere)
# and its own window scan.
episodic_synthetic_same_place_count <- function(cases) {
  config <- episodic_config_resolve()
  place <- ifelse(
    cases$institution_type == "hospital" & !is.na(cases$ward),
    paste(cases$institution_key, cases$ward, sep = "|"),
    cases$institution_key
  )
  key <- paste(cases$pathogen, place, sep = "\r")
  groups <- split(seq_len(nrow(cases)), key)
  total <- 0L
  for (g in groups) {
    grp <- cases[g, ]
    rule <- episodic_same_place_rule(config, grp$pathogen[1])
    total <- total +
      length(episodic_same_place_hit_windows(
        sort(as.Date(grp$sample_date)),
        n = rule$n,
        k_days = rule$k_days
      ))
  }
  total
}

test_that("episodic_synthetic_cases() produces valid case data", {
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-06-30"),
    seed = 42
  )
  expect_silent(episodic_validate_cases(raw))
  expect_gt(nrow(raw), 0)
})

test_that("the same seed reproduces identical data", {
  raw1 <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-03-31"),
    seed = 7
  )
  raw2 <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-03-31"),
    seed = 7
  )
  expect_equal(nrow(raw1), nrow(raw2))
  expect_equal(
    sum(as.integer(as.factor(raw1$pathogen))),
    sum(as.integer(as.factor(raw2$pathogen)))
  )
})

test_that("all six outbreaks are injected, sized from one case to a regional wave", {
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 1
  )
  tag <- function(prefix) sum(startsWith(raw$patient_key, prefix))
  expect_equal(tag("PT-OUTBREAK-RARE-"), 1)
  expect_equal(tag("PT-OUTBREAK-WARD-"), 3)
  expect_equal(tag("PT-OUTBREAK-LTC-"), 8)
  expect_equal(tag("PT-OUTBREAK-PS-"), 14)
  expect_equal(tag("PT-OUTBREAK-PROP-"), 15)
  expect_gt(tag("PT-OUTBREAK-WAVE-"), 100)
  # the one case of an always-notable pathogen the rare_trigger list carries
  expect_equal(
    raw$pathogen[startsWith(raw$patient_key, "PT-OUTBREAK-RARE-")],
    "Neisseria meningitidis"
  )
})

test_that("an outbreak anchored to end_date is clipped to the window asked for", {
  # A generator asked for one month must not answer with the three months
  # its outbreaks happen to be anchored across.
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 2
  )
  dates <- as.Date(raw$sample_date)
  expect_true(all(dates >= as.Date("2024-06-01")))
  expect_true(all(dates <= as.Date("2024-06-30")))
})

test_that("patients recur, so deduplication has something to collapse", {
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 1
  )
  baseline <- raw[!startsWith(raw$patient_key, "PT-OUTBREAK-"), ]
  expect_lt(length(unique(baseline$patient_key)), nrow(baseline))
  # and a repeat positive is the same patient in the same place, not a
  # different person who happens to share a key
  repeated <- baseline$patient_key[duplicated(baseline$patient_key)][1]
  same <- baseline[baseline$patient_key == repeated, ]
  expect_equal(length(unique(same$institution_key)), 1)
  expect_equal(length(unique(same$sex)), 1)
})

test_that("the baseline hardly ever trips same_place by coincidence", {
  # The whole point of the generator's shape: a baseline that keeps
  # tripping the rule-based detectors by chance buries the outbreaks the
  # demo exists to show. This is the regression guard for that - it once
  # produced hundreds of three-case clusters.
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2022-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 1
  )
  baseline <- raw[!startsWith(raw$patient_key, "PT-OUTBREAK-"), ]
  expect_lt(episodic_synthetic_same_place_count(baseline), 6)
})

test_that("the regional wave is too thin, anywhere, for a rule-based detector to see", {
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 1
  )
  wave <- raw[startsWith(raw$patient_key, "PT-OUTBREAK-WAVE-"), ]
  expect_equal(episodic_synthetic_same_place_count(wave), 0)
})

test_that("the injected point-source outbreak is present as a tight ward-level cluster", {
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 1
  )
  outbreak <- raw[grepl("^PT-OUTBREAK-PS-", raw$patient_key), ]
  expect_equal(nrow(outbreak), 14)
  expect_equal(length(unique(outbreak$ward)), 1)
  span <- diff(range(as.Date(outbreak$sample_date)))
  expect_lt(as.numeric(span), 14) # tightly bunched, point-source shape
})

test_that("the injected propagated outbreak spans generation-interval-spaced waves", {
  raw <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 1
  )
  outbreak <- raw[grepl("^PT-OUTBREAK-PROP-", raw$patient_key), ]
  expect_equal(nrow(outbreak), 15)
  span <- diff(range(as.Date(outbreak$sample_date)))
  expect_gt(as.numeric(span), 40) # spread across generations, propagated shape
})

test_that("episodic_synthetic_cases_calibration() produces a valid source with real signal volume for one pathogen", {
  raw <- episodic_synthetic_cases_calibration(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    pathogen = "Clostridioides difficile",
    n_bumps_per_month = 3,
    seed = 5
  )
  expect_silent(episodic_validate_cases(raw))

  # far more volume than the baseline alone would produce for this pathogen
  baseline_only <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 5
  )
  n_baseline <- sum(baseline_only$pathogen == "Clostridioides difficile")
  n_calibration <- sum(raw$pathogen == "Clostridioides difficile")
  expect_gt(n_calibration, n_baseline)

  # every volume-generated case is traceable as synthetic, never mistaken for real data
  volume_cases <- raw[grepl("^PT-VOL-", raw$patient_key), ]
  expect_true(all(volume_cases$pathogen == "Clostridioides difficile"))
  expect_true(all(
    volume_cases$institution_type %in% c("hospital", "ltc_institution")
  ))
})

test_that("episodic_synthetic_cases_calibration() responds to n_bumps_per_month", {
  few <- episodic_synthetic_cases_calibration(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    n_bumps_per_month = 1,
    seed = 9
  )
  many <- episodic_synthetic_cases_calibration(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    n_bumps_per_month = 8,
    seed = 9
  )
  n_few <- sum(grepl("^PT-VOL-", few$patient_key))
  n_many <- sum(grepl("^PT-VOL-", many$patient_key))
  expect_gt(n_many, n_few)
})

test_that("episodic_synthetic_cases_calibration() runs through detection and produces many clusters for the named pathogen", {
  db_path <- tempfile(fileext = ".sqlite")
  episodic_run_cron(
    db_path = db_path,
    cases = function() {
      episodic_synthetic_cases_calibration(
        start_date = as.Date("2024-01-01"),
        end_date = as.Date("2024-06-30"),
        n_bumps_per_month = 4,
        seed = 5
      )
    },
    run_date = as.Date("2024-06-30")
  )
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  clusters <- episodic_db_clusters(con)
  streams <- episodic_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(
    clusters$stream_id,
    streams$stream_id
  )]
  n_cdiff_clusters <- sum(clusters$pathogen == "Clostridioides difficile")
  expect_gt(n_cdiff_clusters, 10) # real signal volume, not the 0-2 the baseline alone would give
})

test_that("episodic_synthetic_denominators() produces a valid denominator source", {
  denom <- episodic_synthetic_denominators(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 3
  )
  expect_true(all(
    c("pathogen", "sample_date", "care_line", "area_code", "n_tests") %in%
      names(denom)
  ))
  expect_gt(nrow(denom), 0)
  expect_true(all(denom$n_tests >= 0))
})
