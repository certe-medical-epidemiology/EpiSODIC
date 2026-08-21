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

test_that("episode_config_resolve() with no EPISODIC_CONFIG loads only the shipped defaults", {
  config <- episode_config_resolve(NA)
  expect_true(is.list(config))
  expect_true(!is.null(config$reconciliation$close_after_runs))
  expect_equal(config$reconciliation$close_after_runs, 14)
})

test_that("an instance config overrides only the keys it sets, recursively", {
  instance_path <- tempfile(fileext = ".yaml")
  writeLines("reconciliation:\n  close_after_runs: 21\n", instance_path)
  config <- episode_config_resolve(instance_path)
  expect_equal(config$reconciliation$close_after_runs, 21)
  # untouched sibling key survives the merge
  expect_equal(config$reconciliation$cooldown_reopen_ratio, 1.5)
  # untouched top-level section survives entirely
  expect_equal(config$eligibility$min_baseline_weeks, 52)
})

test_that("a nonexistent EPISODIC_CONFIG path falls back to defaults without error", {
  config <- episode_config_resolve("/no/such/file.yaml")
  expect_equal(config$reconciliation$close_after_runs, 14)
})

test_that("episode_config_hash() is deterministic and key-order independent", {
  config <- episode_config_resolve(NA)
  h1 <- episode_config_hash(config)
  # reorder top-level keys: should not change the hash
  config_reordered <- config[rev(names(config))]
  h2 <- episode_config_hash(config_reordered)
  expect_equal(h1$hash, h2$hash)
  expect_equal(nchar(h1$hash), 40)
})

test_that("episode_config_hash() changes when a value changes", {
  config <- episode_config_resolve(NA)
  h1 <- episode_config_hash(config)
  config$reconciliation$close_after_runs <- 999
  h2 <- episode_config_hash(config)
  expect_false(identical(h1$hash, h2$hash))
})
