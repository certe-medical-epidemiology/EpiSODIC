# Exhaustive coverage of every fragment path, per the standing brief
# section 6: "anything with few inputs and invisible failure modes gets
# exhaustive tests" and MILESTONES.md M2's "fluent Dutch across all
# fragment paths".

base_cluster <- function(...) {
  defaults <- list(
    id = 1L, pathogen = "Test organism", n_cases = 11L, expected = 1.4, ratio = 7.9,
    detectors = c("same_place"), priority_score = 60,
    place = "Test Hospital", concentration = NULL, denominator = NULL,
    demography = NULL, completeness = list(incomplete_days = 0), density = NULL
  )
  modifyList(defaults, list(...))
}

test_that("every fragment key referenced by the registry exists in both language files", {
  fragments <- EpiSODE:::episode_duiding_fragments()
  keys <- vapply(fragments, function(f) f$key, character(1))
  nl <- episode_i18n_load("nl")
  en <- episode_i18n_load("en")
  expect_true(all(keys %in% names(nl)), info = paste(setdiff(keys, names(nl)), collapse = ", "))
  expect_true(all(keys %in% names(en)), info = paste(setdiff(keys, names(en)), collapse = ", "))
})

test_that("slots fire in the fixed documented order", {
  cluster <- base_cluster(
    concentration = list(dominant_label = "Ward A", dominant_share = 0.9, dominant_n = 9, total = 11),
    denominator = list(n_tests_first = 100, n_tests_last = 100, positivity_first = 0.01, positivity_last = 0.05),
    demography = list(shifted = TRUE, dominant_band = "60-79", baseline_band = "40-59"),
    completeness = list(incomplete_days = 9)
  )
  result <- episode_duiding_generate(cluster)
  fired_slots <- sub("\\..*$", "", result$fired)
  expect_equal(fired_slots, c("magnitude", "concentration", "denominator", "demography", "completeness", "recommendation"))
})

test_that("magnitude: rare_trigger takes priority over ratio-based fragments", {
  cluster <- base_cluster(detectors = "rare_trigger", ratio = 10)
  result <- episode_duiding_generate(cluster)
  expect_equal(result$fired[1], "magnitude.rare_trigger")
})

test_that("magnitude: high ratio (>=3) fires the high_ratio fragment", {
  cluster <- base_cluster(ratio = 5)
  result <- episode_duiding_generate(cluster)
  expect_equal(result$fired[1], "magnitude.high_ratio")
})

test_that("magnitude: moderate ratio (>=1.5, <3) fires the moderate fragment", {
  cluster <- base_cluster(ratio = 2)
  result <- episode_duiding_generate(cluster)
  expect_equal(result$fired[1], "magnitude.moderate_ratio")
})

test_that("magnitude: low ratio falls back to the default fragment", {
  cluster <- base_cluster(ratio = 1.1)
  result <- episode_duiding_generate(cluster)
  expect_equal(result$fired[1], "magnitude.default")
})

test_that("concentration slot is skipped entirely when no concentration data exists", {
  cluster <- base_cluster(concentration = NULL)
  result <- episode_duiding_generate(cluster)
  expect_false(any(startsWith(result$fired, "concentration.")))
})

test_that("concentration: high share (>=0.7), moderate (>=0.5) and diffuse (<0.5) are mutually exclusive", {
  high <- base_cluster(concentration = list(dominant_label = "Ward A", dominant_share = 0.8, dominant_n = 8, total = 10))
  moderate <- base_cluster(concentration = list(dominant_label = "Ward A", dominant_share = 0.55, dominant_n = 5, total = 9))
  diffuse <- base_cluster(concentration = list(dominant_label = "Ward A", dominant_share = 0.3, dominant_n = 3, total = 10))

  expect_true("concentration.high" %in% episode_duiding_generate(high)$fired)
  expect_true("concentration.moderate" %in% episode_duiding_generate(moderate)$fired)
  expect_true("concentration.diffuse" %in% episode_duiding_generate(diffuse)$fired)
})

test_that("denominator slot is skipped entirely when no denominator metadata exists", {
  cluster <- base_cluster(denominator = NULL)
  result <- episode_duiding_generate(cluster)
  expect_false(any(startsWith(result$fired, "denominator.")))
})

test_that("denominator: rising volume with flat positivity is distinguished from rising positivity", {
  flat <- base_cluster(denominator = list(n_tests_first = 100, n_tests_last = 200, positivity_first = 0.02, positivity_last = 0.021))
  rising <- base_cluster(denominator = list(n_tests_first = 100, n_tests_last = 105, positivity_first = 0.01, positivity_last = 0.05))
  stable <- base_cluster(denominator = list(n_tests_first = 100, n_tests_last = 103, positivity_first = 0.02, positivity_last = 0.021))

  expect_true("denominator.rising_volume_flat_positivity" %in% episode_duiding_generate(flat)$fired)
  expect_true("denominator.rising_positivity" %in% episode_duiding_generate(rising)$fired)
  expect_true("denominator.stable" %in% episode_duiding_generate(stable)$fired)
})

test_that("demography slot only fires when a shift is flagged", {
  shifted <- base_cluster(demography = list(shifted = TRUE, dominant_band = "60-79", baseline_band = "40-59"))
  unshifted <- base_cluster(demography = list(shifted = FALSE, dominant_band = "40-59", baseline_band = "40-59"))
  no_data <- base_cluster(demography = NULL)

  expect_true("demography.shifted" %in% episode_duiding_generate(shifted)$fired)
  expect_false(any(startsWith(episode_duiding_generate(unshifted)$fired, "demography.")))
  expect_false(any(startsWith(episode_duiding_generate(no_data)$fired, "demography.")))
})

test_that("completeness slot only fires when incomplete_days > 0", {
  incomplete <- base_cluster(completeness = list(incomplete_days = 9))
  complete <- base_cluster(completeness = list(incomplete_days = 0))

  expect_true("completeness.warning" %in% episode_duiding_generate(incomplete)$fired)
  expect_false(any(startsWith(episode_duiding_generate(complete)$fired, "completeness.")))
})

test_that("recommendation always fires exactly one fragment, tiered by priority/detector", {
  rare <- base_cluster(detectors = "rare_trigger", priority_score = 10)
  high <- base_cluster(priority_score = 85)
  moderate <- base_cluster(priority_score = 60)
  low <- base_cluster(priority_score = 10)

  expect_equal(tail(episode_duiding_generate(rare)$fired, 1), "recommendation.rare_trigger")
  expect_equal(tail(episode_duiding_generate(high)$fired, 1), "recommendation.high_priority")
  expect_equal(tail(episode_duiding_generate(moderate)$fired, 1), "recommendation.moderate_priority")
  expect_equal(tail(episode_duiding_generate(low)$fired, 1), "recommendation.default")
})

test_that("episode_duiding_paragraphs() excludes the recommendation, episode_duiding_recommendation() returns only it", {
  cluster <- base_cluster(priority_score = 85)
  full <- episode_duiding_generate(cluster)
  paragraphs <- episode_duiding_paragraphs(cluster)
  recommendation <- episode_duiding_recommendation(cluster)

  expect_equal(length(paragraphs), length(full$text) - 1)
  expect_equal(recommendation, full$text[length(full$text)])
  expect_false(recommendation %in% paragraphs)
})

test_that("every rendered fragment is free of unrendered {placeholder} tokens, in both languages", {
  cluster <- base_cluster(
    concentration = list(dominant_label = "Ward A", dominant_share = 0.9, dominant_n = 9, total = 11),
    denominator = list(n_tests_first = 100, n_tests_last = 100, positivity_first = 0.01, positivity_last = 0.05),
    demography = list(shifted = TRUE, dominant_band = "60-79", baseline_band = "40-59"),
    completeness = list(incomplete_days = 9)
  )
  for (lang in c("nl", "en")) {
    result <- episode_duiding_generate(cluster, lang = lang)
    for (text in result$text) {
      expect_false(grepl("\\{[a-zA-Z_]+\\}", text), info = paste(lang, ":", text))
    }
  }
})

test_that("Dutch number agreement in the magnitude fragment is correct at n=1 and n>1", {
  one_case <- base_cluster(n_cases = 1L, ratio = 5)
  many_cases <- base_cluster(n_cases = 11L, ratio = 5)

  text_one <- episode_duiding_generate(one_case)$text[1]
  text_many <- episode_duiding_generate(many_cases)$text[1]

  expect_match(text_one, "^1 geval\\b")
  expect_match(text_many, "^11 gevallen\\b")
})

test_that("the engine never errors on a minimal cluster object with everything optional set to NULL", {
  minimal <- list(id = 1L, n_cases = 1L, expected = NA, ratio = NA, detectors = character(0),
                   priority_score = NA, place = NA)
  expect_silent(result <- episode_duiding_generate(minimal))
  expect_true(length(result$text) >= 1)  # magnitude.default and recommendation.default always fire
})
