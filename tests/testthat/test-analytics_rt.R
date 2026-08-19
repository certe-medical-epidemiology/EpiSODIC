test_that("episode_compute_rt() returns NULL when rt_applicable is FALSE", {
  skip_if_not_installed("EpiEstim")
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:30))
  pc <- data.frame(rt_applicable = 0, si_mean_days = 3, si_sd_days = 1.5)
  expect_null(episode_compute_rt(cases, pc))
})

test_that("episode_compute_rt() returns NULL when the serial interval is unset", {
  skip_if_not_installed("EpiEstim")
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:30))
  pc <- data.frame(rt_applicable = 1, si_mean_days = NA, si_sd_days = NA)
  expect_null(episode_compute_rt(cases, pc))
})

test_that("episode_compute_rt() returns NULL with too little case history for even one window", {
  skip_if_not_installed("EpiEstim")
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:3))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)
  expect_null(episode_compute_rt(cases, pc))
})

test_that("episode_compute_rt() returns a well-formed series with credible intervals for enough case history", {
  skip_if_not_installed("EpiEstim")
  set.seed(1)
  dates <- as.Date("2025-01-01") + sample(0:40, 150, replace = TRUE)
  cases <- data.frame(sample_date = as.character(dates))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)

  rt <- episode_compute_rt(cases, pc)
  expect_false(is.null(rt))
  expect_true(all(c("window_end", "mean", "lower", "upper") %in% names(rt)))
  expect_true(all(rt$lower <= rt$mean & rt$mean <= rt$upper))
})

test_that("episode_compute_rt() withholds windows ending inside incomplete_days", {
  skip_if_not_installed("EpiEstim")
  set.seed(1)
  dates <- as.Date("2025-01-01") + sample(0:40, 150, replace = TRUE)
  cases <- data.frame(sample_date = as.character(dates))
  pc <- data.frame(rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5)

  full <- episode_compute_rt(cases, pc, incomplete_days = 0)
  truncated <- episode_compute_rt(cases, pc, incomplete_days = 10)
  expect_true(nrow(truncated) < nrow(full))
  expect_true(max(truncated$window_end) <= max(full$window_end) - 10)
})
