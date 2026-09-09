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

# The harness end to end, kept deliberately tiny: a fixed end date, two
# thirds of a year of history and two weekly runs. Enough to prove the
# replay, the capture and the matching hold together; far too little to
# say anything about detection performance, which is what
# data-raw/validation/ is for.

tiny <- function(...) {
  episodic_validate_detection(
    end_date = as.Date("2026-06-28"),
    history_years = 0.6,
    evaluation_weeks = 2,
    ...
  )
}

test_that("the replay returns the documented structure", {
  skip_on_cran()
  result <- tiny()

  expect_s3_class(result, "episodic_validation")
  expect_named(
    result,
    c("outbreaks", "clusters", "runs", "overlap", "truth", "summary", "meta")
  )
  expect_equal(nrow(result$outbreaks), 6)
  expect_equal(nrow(result$truth$outbreaks), 6)
  expect_equal(nrow(result$runs), 2)
  expect_equal(
    result$runs$run_date,
    as.Date(c("2026-06-21", "2026-06-28"))
  )
  expect_true(all(c("detected", "delay_from_first", "censor_days") %in%
    names(result$outbreaks)))
  expect_true(all(c("true_positive", "precision", "priority_score") %in%
    names(result$clusters)))
})

test_that("the replay records what produced it", {
  skip_on_cran()
  result <- tiny()

  expect_equal(
    result$meta$package_version,
    as.character(utils::packageVersion("EpiSODIC"))
  )
  expect_match(result$meta$config_hash, "^[0-9a-f]{40}$")
  expect_equal(result$meta$min_recall, 0.5)
  expect_equal(result$meta$n_runs, 2)
  # Every rate in the summary is per stream-week, so the denominator has
  # to be reported with it and not left to be inferred.
  expect_equal(result$meta$n_stream_weeks, sum(result$runs$n_streams))
  expect_gt(result$meta$n_stream_weeks, 0)
})

test_that("the last run is the end of a complete week", {
  # A run stepped to an arbitrary weekday hands the statistical
  # detectors a partial week to test against full-week baselines.
  for (day in as.list(seq(as.Date("2026-06-22"), as.Date("2026-06-28"), 1))) {
    last <- episodic_validation_last_run_date(day)
    expect_equal(format(last, "%u"), "7", info = format(day))
    expect_lte(last, day)
  }
})

test_that("a run reaches only the cases sampled by its own date", {
  skip_on_cran()
  result <- tiny()

  # Nothing a run's cluster holds may be dated after the run itself:
  # that is the whole basis on which detection delay is measured.
  expect_true(all(result$overlap$n_outbreak_asof <= result$overlap$n_outbreak_full))
  first_run <- min(result$runs$run_date)
  early <- result$overlap[result$overlap$run_date == first_run, ]
  expect_true(all(early$n_overlap <= early$n_outbreak_asof))
})

test_that("what the replay found is matched to what was injected", {
  skip_on_cran()
  result <- tiny()

  # With no statistical baseline to fit against, this window can only
  # find what the rule-based detectors find - which is the shapes those
  # were built for, and not the diffuse regional wave.
  detected <- result$outbreaks$outbreak_id[result$outbreaks$detected]
  expect_true(all(c("RARE", "WARD", "LTC", "PS") %in% detected))
  expect_false("WAVE" %in% detected)

  matched <- result$outbreaks[result$outbreaks$detected, ]
  expect_true(all(matched$recall_full >= result$meta$min_recall))
  expect_true(all(!is.na(matched$cluster_id)))
})

test_that("a negative control has no outbreaks and still has an alarm rate", {
  skip_on_cran()
  result <- tiny(outbreaks = FALSE)

  expect_equal(nrow(result$outbreaks), 0)
  sensitivity <- result$summary[result$summary$metric == "sensitivity", ]
  # No outbreaks to find is not a sensitivity of zero.
  expect_true(is.na(sensitivity$estimate[1]))
  alarms <- result$summary[
    result$summary$metric == "alarms" & result$summary$group_type == "overall",
  ]
  expect_gt(alarms$denominator[1], 0)
  expect_false(is.na(alarms$estimate[1]))
})

test_that("a dropped detector raises nothing in the replay", {
  skip_on_cran()
  result <- tiny(detectors = "same_place")

  expect_equal(result$meta$detectors, "same_place")
  raised <- unique(unlist(strsplit(
    stats::na.omit(result$clusters$detectors),
    "+",
    fixed = TRUE
  )))
  expect_equal(raised, "same_place")
  # The single case of an always-notable pathogen is exactly what
  # rare_trigger exists for, so dropping it has to lose that outbreak.
  expect_false(result$outbreaks$detected[result$outbreaks$outbreak_id == "RARE"])
})

test_that("the harness leaves nothing behind", {
  skip_on_cran()
  before <- list.files(tempdir())
  tiny()
  after <- list.files(tempdir())
  left <- setdiff(after, before)
  expect_equal(
    grep("^episodic-validation", left, value = TRUE),
    character(0)
  )
})

test_that("arguments that cannot mean anything are refused", {
  expect_error(episodic_validate_detection(seeds = character(0)), "RNG seeds")
  expect_error(episodic_validate_detection(seeds = c(1, 1)), "repeats")
  expect_error(episodic_validate_detection(min_recall = 0), "min_recall")
  expect_error(episodic_validate_detection(min_precision = 1.5), "min_precision")
  expect_error(episodic_validate_detection(evaluation_weeks = 0), "evaluation_weeks")
  expect_error(episodic_validate_detection(history_years = -1), "history_years")
  expect_error(
    episodic_validate_detection(detectors = "clairvoyance"),
    "no such detector"
  )
  expect_error(
    episodic_validate_detection(config = list(mem = list(enabled = FALSE))),
    "`detectors`"
  )
})

test_that("the instance configuration written is one a run would accept", {
  path <- episodic_validation_config_file(
    detectors = c("same_place", "rare_trigger"),
    config = list(farrington = list(alpha = 0.01))
  )
  on.exit(unlink(dirname(path), recursive = TRUE))

  resolved <- episodic_config_resolve(path)
  expect_true(episodic_detector_enabled(resolved, "same_place"))
  expect_false(episodic_detector_enabled(resolved, "farrington"))
  expect_equal(resolved$farrington$alpha, 0.01)
  # It goes through the same validation an operator's file does, so a
  # misspelled key stops the study rather than quietly not applying.
  expect_error(
    episodic_config_resolve(
      episodic_validation_config_file(
        detectors = "same_place",
        config = list(farrington = list(alfa = 0.01))
      )
    ),
    "alfa"
  )
})

test_that("printing a result shows what it rests on", {
  skip_on_cran()
  result <- tiny()
  output <- utils::capture.output(print(result))
  expect_true(any(grepl("seeded outbreak", output)))
  expect_true(any(grepl("stream-week", output)))
  expect_true(any(grepl("sensitivity", output)))
})

test_that("thresholds can be varied without replaying the detection", {
  skip_on_cran()
  result <- tiny()
  strict <- episodic_validate_rethreshold(
    result,
    min_recall = 0.95,
    min_precision = 0.95
  )

  # What was raised, and when, and what it held, are measurements: no
  # threshold changes them.
  expect_equal(strict$runs, result$runs)
  expect_equal(strict$clusters$n_cases, result$clusters$n_cases)
  expect_equal(strict$meta$min_recall, 0.95)
  expect_equal(
    unname(strict$meta$rethresholded_from["min_recall"]),
    result$meta$min_recall
  )
  # A stricter rule can never call more outbreaks detected.
  expect_lte(sum(strict$outbreaks$detected), sum(result$outbreaks$detected))
})

test_that("rethresholding refuses anything that is not a validation result", {
  expect_error(
    episodic_validate_rethreshold(data.frame(a = 1)),
    "episodic_validate_detection"
  )
})
