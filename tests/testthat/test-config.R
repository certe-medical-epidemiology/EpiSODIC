test_that("episode_config_resolve() with no EPISODE_CONFIG loads only the shipped defaults", {
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

test_that("a nonexistent EPISODE_CONFIG path falls back to defaults without error", {
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
