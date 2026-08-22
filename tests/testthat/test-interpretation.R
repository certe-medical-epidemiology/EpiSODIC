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

# Exhaustive coverage of every fragment path: few inputs and an
# invisible failure mode (a wrong sentence in a clinical dossier) is
# exactly the kind of function that earns exhaustive tests, and every
# path must read as fluent Dutch.

base_cluster <- function(...) {
  defaults <- list(
    id = 1L,
    pathogen = "Test pathogen",
    n_cases = 11L,
    expected = 1.4,
    ratio = 7.9,
    detectors = c("same_place"),
    priority_score = 60,
    place = "Test Hospital",
    concentration = NULL,
    denominator = NULL,
    demography = NULL,
    completeness = list(incomplete_days = 0),
    density = NULL
  )
  modifyList(defaults, list(...))
}

test_that("every fragment key referenced by the registry exists in both language files", {
  fragments <- EpiSODIC:::episodic_interpretation_fragments()
  keys <- vapply(fragments, function(f) f$key, character(1))
  nl <- episodic_i18n_load("nl")
  en <- episodic_i18n_load("en")
  expect_true(
    all(keys %in% names(nl)),
    info = paste(setdiff(keys, names(nl)), collapse = ", ")
  )
  expect_true(
    all(keys %in% names(en)),
    info = paste(setdiff(keys, names(en)), collapse = ", ")
  )
})

test_that("slots fire in the fixed documented order", {
  cluster <- base_cluster(
    concentration = list(
      dominant_label = "Ward A",
      dominant_share = 0.9,
      dominant_n = 9,
      total = 11
    ),
    denominator = list(
      n_tests_first = 100,
      n_tests_last = 100,
      positivity_first = 0.01,
      positivity_last = 0.05
    ),
    demography = list(
      shifted = TRUE,
      dominant_band = "60-79",
      baseline_band = "40-59"
    ),
    completeness = list(incomplete_days = 9)
  )
  result <- episodic_interpretation_generate(cluster)
  fired_slots <- sub("\\..*$", "", result$fired)
  expect_equal(
    fired_slots,
    c(
      "magnitude",
      "concentration",
      "denominator",
      "demography",
      "completeness",
      "recommendation"
    )
  )
})

test_that("magnitude: rare_trigger takes priority over ratio-based fragments", {
  cluster <- base_cluster(detectors = "rare_trigger", ratio = 10)
  result <- episodic_interpretation_generate(cluster)
  expect_equal(result$fired[1], "magnitude.rare_trigger")
})

test_that("magnitude: high ratio (>=3) fires the high_ratio fragment", {
  cluster <- base_cluster(ratio = 5)
  result <- episodic_interpretation_generate(cluster)
  expect_equal(result$fired[1], "magnitude.high_ratio")
})

test_that("magnitude: moderate ratio (>=1.5, <3) fires the moderate fragment", {
  cluster <- base_cluster(ratio = 2)
  result <- episodic_interpretation_generate(cluster)
  expect_equal(result$fired[1], "magnitude.moderate_ratio")
})

test_that("magnitude: low ratio falls back to the default fragment", {
  cluster <- base_cluster(ratio = 1.1)
  result <- episodic_interpretation_generate(cluster)
  expect_equal(result$fired[1], "magnitude.default")
})

test_that("concentration slot is skipped entirely when no concentration data exists", {
  cluster <- base_cluster(concentration = NULL)
  result <- episodic_interpretation_generate(cluster)
  expect_false(any(startsWith(result$fired, "concentration.")))
})

test_that("concentration: high share (>=0.7), moderate (>=0.5) and diffuse (<0.5) are mutually exclusive", {
  high <- base_cluster(
    concentration = list(
      dominant_label = "Ward A",
      dominant_share = 0.8,
      dominant_n = 8,
      total = 10
    )
  )
  moderate <- base_cluster(
    concentration = list(
      dominant_label = "Ward A",
      dominant_share = 0.55,
      dominant_n = 5,
      total = 9
    )
  )
  diffuse <- base_cluster(
    concentration = list(
      dominant_label = "Ward A",
      dominant_share = 0.3,
      dominant_n = 3,
      total = 10
    )
  )

  expect_true(
    "concentration.high" %in% episodic_interpretation_generate(high)$fired
  )
  expect_true(
    "concentration.moderate" %in%
      episodic_interpretation_generate(moderate)$fired
  )
  expect_true(
    "concentration.diffuse" %in% episodic_interpretation_generate(diffuse)$fired
  )
})

test_that("denominator slot is skipped entirely when no denominator metadata exists", {
  cluster <- base_cluster(denominator = NULL)
  result <- episodic_interpretation_generate(cluster)
  expect_false(any(startsWith(result$fired, "denominator.")))
})

test_that("denominator: rising volume with flat positivity is distinguished from rising positivity", {
  flat <- base_cluster(
    denominator = list(
      n_tests_first = 100,
      n_tests_last = 200,
      positivity_first = 0.02,
      positivity_last = 0.021
    )
  )
  rising <- base_cluster(
    denominator = list(
      n_tests_first = 100,
      n_tests_last = 105,
      positivity_first = 0.01,
      positivity_last = 0.05
    )
  )
  stable <- base_cluster(
    denominator = list(
      n_tests_first = 100,
      n_tests_last = 103,
      positivity_first = 0.02,
      positivity_last = 0.021
    )
  )

  expect_true(
    "denominator.rising_volume_flat_positivity" %in%
      episodic_interpretation_generate(flat)$fired
  )
  expect_true(
    "denominator.rising_positivity" %in%
      episodic_interpretation_generate(rising)$fired
  )
  expect_true(
    "denominator.stable" %in% episodic_interpretation_generate(stable)$fired
  )
})

test_that("demography slot only fires when a shift is flagged", {
  shifted <- base_cluster(
    demography = list(
      shifted = TRUE,
      dominant_band = "60-79",
      baseline_band = "40-59"
    )
  )
  unshifted <- base_cluster(
    demography = list(
      shifted = FALSE,
      dominant_band = "40-59",
      baseline_band = "40-59"
    )
  )
  no_data <- base_cluster(demography = NULL)

  expect_true(
    "demography.shifted" %in% episodic_interpretation_generate(shifted)$fired
  )
  expect_false(any(startsWith(
    episodic_interpretation_generate(unshifted)$fired,
    "demography."
  )))
  expect_false(any(startsWith(
    episodic_interpretation_generate(no_data)$fired,
    "demography."
  )))
})

test_that("completeness slot only fires when incomplete_days > 0", {
  incomplete <- base_cluster(completeness = list(incomplete_days = 9))
  complete <- base_cluster(completeness = list(incomplete_days = 0))

  expect_true(
    "completeness.warning" %in%
      episodic_interpretation_generate(incomplete)$fired
  )
  expect_false(any(startsWith(
    episodic_interpretation_generate(complete)$fired,
    "completeness."
  )))
})

test_that("recommendation always fires exactly one fragment, tiered by priority/detector", {
  rare <- base_cluster(detectors = "rare_trigger", priority_score = 10)
  high <- base_cluster(priority_score = 85)
  moderate <- base_cluster(priority_score = 60)
  low <- base_cluster(priority_score = 10)

  expect_equal(
    tail(episodic_interpretation_generate(rare)$fired, 1),
    "recommendation.rare_trigger"
  )
  expect_equal(
    tail(episodic_interpretation_generate(high)$fired, 1),
    "recommendation.high_priority"
  )
  expect_equal(
    tail(episodic_interpretation_generate(moderate)$fired, 1),
    "recommendation.moderate_priority"
  )
  expect_equal(
    tail(episodic_interpretation_generate(low)$fired, 1),
    "recommendation.default"
  )
})

test_that("episodic_interpretation_paragraphs() excludes the recommendation, episodic_interpretation_recommendation() returns only it", {
  cluster <- base_cluster(priority_score = 85)
  full <- episodic_interpretation_generate(cluster)
  paragraphs <- episodic_interpretation_paragraphs(cluster)
  recommendation <- episodic_interpretation_recommendation(cluster)

  expect_equal(length(paragraphs), length(full$text) - 1)
  expect_equal(recommendation, full$text[length(full$text)])
  expect_false(recommendation %in% paragraphs)
})

test_that("every rendered fragment is free of unrendered {placeholder} tokens, in every shipped language", {
  cluster <- base_cluster(
    concentration = list(
      dominant_label = "Ward A",
      dominant_share = 0.9,
      dominant_n = 9,
      total = 11
    ),
    denominator = list(
      n_tests_first = 100,
      n_tests_last = 100,
      positivity_first = 0.01,
      positivity_last = 0.05
    ),
    demography = list(
      shifted = TRUE,
      dominant_band = "60-79",
      baseline_band = "40-59"
    ),
    completeness = list(incomplete_days = 9)
  )
  for (lang in c("nl", "en", "es", "fr", "de", "zh", "hi", "ar")) {
    result <- episodic_interpretation_generate(cluster, lang = lang)
    for (text in result$text) {
      expect_false(
        grepl("\\{[a-zA-Z_]+\\}", text),
        info = paste(lang, ":", text)
      )
    }
  }
})

test_that("Dutch number agreement in the magnitude fragment is correct at n=1 and n>1", {
  one_case <- base_cluster(n_cases = 1L, ratio = 5)
  many_cases <- base_cluster(n_cases = 11L, ratio = 5)

  text_one <- episodic_interpretation_generate(one_case, lang = "nl")$text[1]
  text_many <- episodic_interpretation_generate(many_cases, lang = "nl")$text[1]

  expect_match(text_one, "^1 casus\\b")
  expect_match(text_many, "^11 casussen\\b")
})

test_that("the engine never errors on a minimal cluster object with everything optional set to NULL", {
  minimal <- list(
    id = 1L,
    n_cases = 1L,
    expected = NA,
    ratio = NA,
    detectors = character(0),
    priority_score = NA,
    place = NA
  )
  expect_silent(result <- episodic_interpretation_generate(minimal))
  expect_true(length(result$text) >= 1) # magnitude.default and recommendation.default always fire
})
