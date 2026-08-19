test_that("episode_classify_curve_shape() classifies within-one-incubation-period clusters as point_source", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 1, 2)))
  expect_equal(episode_classify_curve_shape(cases, incub_max_days = 3), "point_source")
})

test_that("episode_classify_curve_shape() classifies a span past twice incub_max_days as propagated", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 20, 40)))
  expect_equal(episode_classify_curve_shape(cases, incub_max_days = 3), "propagated")
})

test_that("episode_classify_curve_shape() classifies the zone in between as ambiguous", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 5)))
  expect_equal(episode_classify_curve_shape(cases, incub_max_days = 3), "ambiguous")
})

test_that("episode_classify_curve_shape() returns NA when incub_max_days is NA or fewer than 2 cases", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + c(0, 5)))
  expect_true(is.na(episode_classify_curve_shape(cases, incub_max_days = NA)))
  expect_true(is.na(episode_classify_curve_shape(cases[1, , drop = FALSE], incub_max_days = 3)))
})
