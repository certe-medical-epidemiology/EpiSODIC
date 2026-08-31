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

test_that("episodic_config_resolve() with no EPISODIC_CONFIG loads only the shipped defaults", {
  config <- episodic_config_resolve(NA)
  expect_true(is.list(config))
  expect_true(!is.null(config$reconciliation$close_after_runs))
  expect_equal(config$reconciliation$close_after_runs, 14)
})

test_that("an instance config overrides only the keys it sets, recursively", {
  instance_path <- tempfile(fileext = ".yaml")
  writeLines("reconciliation:\n  close_after_runs: 21\n", instance_path)
  config <- episodic_config_resolve(instance_path)
  expect_equal(config$reconciliation$close_after_runs, 21)
  # untouched sibling key survives the merge
  expect_equal(config$reconciliation$cooldown_reopen_ratio, 1.5)
  # untouched top-level section survives entirely
  expect_equal(config$eligibility$min_baseline_weeks, 52)
})

test_that("a nonexistent EPISODIC_CONFIG path falls back to defaults without error", {
  config <- episodic_config_resolve("/no/such/file.yaml")
  expect_equal(config$reconciliation$close_after_runs, 14)
})

test_that("episodic_config_hash() is deterministic and key-order independent", {
  config <- episodic_config_resolve(NA)
  h1 <- episodic_config_hash(config)
  # reorder top-level keys: should not change the hash
  config_reordered <- config[rev(names(config))]
  h2 <- episodic_config_hash(config_reordered)
  expect_equal(h1$hash, h2$hash)
  expect_equal(nchar(h1$hash), 40)
})

test_that("episodic_config_hash() changes when a value changes", {
  config <- episodic_config_resolve(NA)
  h1 <- episodic_config_hash(config)
  config$reconciliation$close_after_runs <- 999
  h2 <- episodic_config_hash(config)
  expect_false(identical(h1$hash, h2$hash))
})

test_that("episodic_config_resolve() with con = NULL never touches the database (the default, no admin override)", {
  config <- episodic_config_resolve(NA, con = NULL)
  expect_null(config$notifications)
})

test_that("episodic_config_resolve(con = ...) overlays the latest episodic_app_config_event on top of the YAML config", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- episodic_db_app_user_insert(
    con,
    "admin1",
    "Admin One",
    "a@x.nl",
    sodium::password_store("pw12345678"),
    is_admin = TRUE
  )

  # no override saved yet: DB-aware and DB-unaware resolution agree
  config_before <- episodic_config_resolve(NA, con = con)
  expect_null(config_before$notifications)

  episodic_db_app_config_event_insert(
    con,
    user_id,
    "notifications",
    jsonlite::toJSON(
      list(
        enabled = TRUE,
        channels = list(
          ntfy = list(enabled = TRUE, server = "https://ntfy.sh", topic = "t")
        )
      ),
      auto_unbox = TRUE
    )
  )

  config_after <- episodic_config_resolve(NA, con = con)
  expect_true(config_after$notifications$enabled)
  expect_equal(config_after$notifications$channels$ntfy$topic, "t")
  # untouched sections are unaffected by the DB overlay
  expect_equal(config_after$reconciliation$close_after_runs, 14)
})

test_that("episodic_config_resolve(con = ...) applies the most recent override, not the first", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- episodic_db_app_user_insert(
    con,
    "admin1",
    "Admin One",
    "a@x.nl",
    sodium::password_store("pw12345678"),
    is_admin = TRUE
  )
  episodic_db_app_config_event_insert(
    con,
    user_id,
    "notifications",
    jsonlite::toJSON(
      list(dashboard_url = "https://first.example"),
      auto_unbox = TRUE
    )
  )
  Sys.sleep(0.01)
  episodic_db_app_config_event_insert(
    con,
    user_id,
    "notifications",
    jsonlite::toJSON(
      list(dashboard_url = "https://second.example"),
      auto_unbox = TRUE
    )
  )

  config <- episodic_config_resolve(NA, con = con)
  expect_equal(config$notifications$dashboard_url, "https://second.example")
})

test_that("a DB notifications override does not change episodic_config_hash(), since notifications is stripped before hashing", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- episodic_db_app_user_insert(
    con,
    "admin1",
    "Admin One",
    "a@x.nl",
    sodium::password_store("pw12345678"),
    is_admin = TRUE
  )
  h_before <- episodic_config_hash(episodic_config_resolve(NA, con = con))

  episodic_db_app_config_event_insert(
    con,
    user_id,
    "notifications",
    jsonlite::toJSON(
      list(
        enabled = TRUE,
        channels = list(
          slack = list(enabled = TRUE, webhook_url = "https://hooks.example/x")
        )
      ),
      auto_unbox = TRUE
    )
  )
  h_after <- episodic_config_hash(episodic_config_resolve(NA, con = con))

  expect_equal(h_before$hash, h_after$hash)
})
