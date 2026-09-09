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

# The naive rules, and the replay that measures them the same way.

cases_at <- function(place, dates, pathogen = "Norovirus", type = "hospital") {
  data.frame(
    source_key = sprintf("%s-%d", place, seq_along(dates)),
    pathogen = pathogen,
    institution_key = place,
    institution_type = type,
    ward = if (identical(type, "hospital")) "B4" else NA_character_,
    sample_date = as.Date(dates),
    stringsAsFactors = FALSE
  )
}

params <- list(
  n_cases = 3,
  k_days = 7,
  case_free_days = 14,
  baseline_weeks = 4,
  sd_multiplier = 2
)

test_that("the same-place rule fires on N cases within K days and not otherwise", {
  tight <- cases_at("H1", c("2025-03-01", "2025-03-03", "2025-03-05"))
  tight$place_key <- episodic_validation_place_key(tight)
  fired <- episodic_validation_rule_same_place(
    tight,
    as.Date("2025-03-06"),
    params
  )
  expect_equal(nrow(fired), 3)
  expect_equal(length(unique(fired$alarm_key)), 1)

  spread <- cases_at("H1", c("2025-03-01", "2025-03-10", "2025-03-19"))
  spread$place_key <- episodic_validation_place_key(spread)
  expect_equal(
    nrow(episodic_validation_rule_same_place(
      spread,
      as.Date("2025-03-20"),
      params
    )),
    0
  )
})

test_that("a quiet gap ends one alarm and starts the next", {
  cases <- cases_at("H1", c(
    "2025-03-01", "2025-03-02", "2025-03-03",
    "2025-04-01", "2025-04-02", "2025-04-03"
  ))
  cases$place_key <- episodic_validation_place_key(cases)
  fired <- episodic_validation_rule_same_place(
    cases,
    as.Date("2025-04-04"),
    params
  )
  expect_equal(length(unique(fired$alarm_key)), 2)
  expect_equal(nrow(fired), 6)
})

test_that("a place is a ward in a hospital and the institution elsewhere", {
  hospital <- cases_at("H1", rep("2025-03-01", 2))
  home <- cases_at("L1", rep("2025-03-01", 2), type = "ltc_institution")
  expect_false(
    identical(
      episodic_validation_place_key(hospital)[1],
      episodic_validation_place_key(home)[1]
    )
  )
  expect_true(grepl("B4", episodic_validation_place_key(hospital)[1]))
})

test_that("the Shewhart rule fires on a week above its own control limit", {
  quiet <- seq(as.Date("2025-01-06"), as.Date("2025-03-24"), by = 7)
  baseline <- cases_at("H1", quiet)
  spike_week <- as.Date("2025-03-31")
  spike <- cases_at("H2", rep(spike_week, 12))
  cases <- rbind(baseline, spike)
  cases$source_key <- sprintf("K%d", seq_len(nrow(cases)))

  fired <- episodic_validation_rule_shewhart(
    cases,
    as.Date("2025-04-06"),
    params
  )
  expect_gt(nrow(fired), 0)
  expect_equal(length(unique(fired$alarm_key)), 1)

  # The same baseline with no spike in it is not an alarm.
  expect_equal(
    nrow(episodic_validation_rule_shewhart(
      baseline,
      as.Date("2025-03-30"),
      params
    )),
    0
  )
})

test_that("the Shewhart rule needs a baseline before it says anything", {
  short <- cases_at("H1", seq(as.Date("2025-03-03"), as.Date("2025-03-24"), 7))
  expect_equal(
    nrow(episodic_validation_rule_shewhart(
      short,
      as.Date("2025-03-30"),
      params
    )),
    0
  )
})

test_that("a comparator is reported in the same shape as a real replay", {
  skip_on_cran()
  result <- episodic_validate_comparator(
    method = "same_place",
    end_date = as.Date("2026-06-28"),
    history_years = 0.6,
    evaluation_weeks = 2
  )

  expect_s3_class(result, "episodic_validation")
  expect_named(
    result,
    c("outbreaks", "clusters", "runs", "overlap", "truth", "summary", "meta")
  )
  expect_equal(nrow(result$outbreaks), 6)
  expect_equal(result$meta$method, "same_place")
  # A rule this simple ranks nothing, so its triage has no score to
  # judge - NA rather than a score of zero it never gave.
  expect_true(all(is.na(result$clusters$priority_score)))
  expect_gt(result$meta$n_stream_weeks, 0)
})

test_that("the Shewhart comparator finds the diffuse wave the rules cannot", {
  skip_on_cran()
  result <- episodic_validate_comparator(
    method = "shewhart",
    end_date = as.Date("2026-06-28"),
    history_years = 2,
    evaluation_weeks = 2
  )
  detected <- result$outbreaks$outbreak_id[result$outbreaks$detected]
  expect_true("WAVE" %in% detected)
})
