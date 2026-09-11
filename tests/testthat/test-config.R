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

test_that("a resolved configuration is cached per file, and a rewritten file is read again", {
  instance_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(instance_path))
  writeLines("reconciliation:\n  close_after_runs: 21\n", instance_path)
  expect_equal(
    episodic_config_resolve(instance_path)$reconciliation$close_after_runs,
    21
  )

  # The cache is keyed on what the file says, not on when it was last
  # written: a rewrite that keeps the same path and length - and, on a
  # filesystem with second-resolution timestamps, the same mtime - is
  # still a different configuration and has to be resolved as one.
  writeLines("reconciliation:\n  close_after_runs: 12\n", instance_path)
  expect_equal(
    episodic_config_resolve(instance_path)$reconciliation$close_after_runs,
    12
  )

  # And a second path with its own contents is its own answer, not the
  # one already cached.
  other_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(other_path), add = TRUE)
  writeLines("reconciliation:\n  close_after_runs: 30\n", other_path)
  expect_equal(
    episodic_config_resolve(other_path)$reconciliation$close_after_runs,
    30
  )
  expect_equal(
    episodic_config_resolve(instance_path)$reconciliation$close_after_runs,
    12
  )
})

test_that("a Settings-screen override is read from the database on every resolve, never cached with the file", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_null(episodic_config_resolve(NA, con = con)$notifications$ntfy$topic)

  user_id <- episodic_db_app_user_insert(
    con,
    "admin",
    "Admin User",
    "a@example.com",
    "hash",
    is_admin = TRUE
  )
  episodic_db_app_config_event_insert(
    con,
    user_id = user_id,
    section = "notifications",
    config_json = '{"ntfy": {"topic": "outbreaks"}}'
  )
  expect_equal(
    episodic_config_resolve(NA, con = con)$notifications$ntfy$topic,
    "outbreaks"
  )
})

test_that("a nonexistent EPISODIC_CONFIG path is refused, not quietly ignored", {
  # Ignoring it runs the instance on shipped defaults while the operator
  # believes their own thresholds, same_place overrides and notification
  # channels are in force, with nothing anywhere to say otherwise.
  expect_error(
    episodic_config_resolve("/no/such/file.yaml"),
    "no file exists there"
  )
})

test_that("an EPISODIC_CONFIG file that is not YAML is refused by name", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines("reconciliation: [unclosed", path)
  expect_error(episodic_config_resolve(path), "could not be read as YAML")
})

test_that("a misspelled configuration key is refused, with the key it meant", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(c("reconciliation:", "  close_after_run: 21"), path)
  err <- expect_error(episodic_config_resolve(path))
  expect_match(conditionMessage(err), "reconciliation.close_after_run", fixed = TRUE)
  expect_match(conditionMessage(err), "close_after_runs", fixed = TRUE)
})

test_that("an unknown top-level section is refused", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(c("reconcilliation:", "  close_after_runs: 21"), path)
  expect_error(episodic_config_resolve(path), "reconcilliation")
})

test_that("a value of the wrong type is refused rather than reaching arithmetic", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(c("reconciliation:", "  close_after_runs: fourteen"), path)
  expect_error(episodic_config_resolve(path), "must be a number, but is text")
})

test_that("null is refused where it would leave a caller holding NULL", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(c("reconciliation:", "  case_free_days_default: ~"), path)
  expect_error(episodic_config_resolve(path), "cannot be null")
})

test_that("null is accepted where a setting documents it as 'disable'", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(c("reconciliation:", "  stale_open_days: ~"), path)
  config <- episodic_config_resolve(path)
  # Present and null, not silently removed by the merge: `~` means what
  # YAML says it means.
  expect_true("stale_open_days" %in% names(config$reconciliation))
  expect_null(config$reconciliation$stale_open_days)
})

test_that("operator-named subtrees accept keys EpiSODIC cannot enumerate", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(
    c(
      "same_place:",
      "  overrides:",
      "    Some Local Pathogen: {n_cases: 2, k_days: 5}",
      "rare_trigger:",
      "  pathogens:",
      "    - Some Local Pathogen",
      "notifications:",
      "  enabled: true",
      "  channels:",
      "    ntfy: {enabled: true, server: 'https://ntfy.sh', topic: 't'}"
    ),
    path
  )
  config <- episodic_config_resolve(path)
  expect_equal(config$same_place$overrides[["Some Local Pathogen"]]$n_cases, 2)
  expect_true(isTRUE(config$notifications$enabled))
})

test_that("EpiSODIC ships closed to anonymous visitors", {
  # The shipped state is the state of every deployment where nobody read
  # the configuration file.
  defaults <- episodic_config_resolve(NA)
  expect_true(isTRUE(defaults$access$require_login))
  expect_true(episodic_app_require_login(defaults))
})

test_that("an access policy that cannot be read leaves the login wall up", {
  expect_true(episodic_app_require_login(list()))
  expect_true(episodic_app_require_login(list(access = list())))
  expect_true(episodic_app_require_login(list(access = list(require_login = "yes please"))))
  expect_false(episodic_app_require_login(list(access = list(require_login = FALSE))))
})

test_that("geography names are configuration, not hardcoded to one country", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path))
  writeLines(
    c(
      "geography:",
      "  region_code: NAIROBI_COUNTY",
      "  area_code_prefix: 'ZONE-'",
      "  area_pc_characters: 3"
    ),
    path
  )
  geography <- episodic_geography_config(episodic_config_resolve(path))
  expect_equal(geography$region_code, "NAIROBI_COUNTY")
  cases <- data.frame(pc = c("00100", NA), stringsAsFactors = FALSE)
  expect_equal(
    episodic_case_region_code(cases, "pathogen_area", geography),
    c("ZONE-001", NA)
  )
  expect_equal(
    episodic_case_region_code(cases, "pathogen_region", geography),
    rep("NAIROBI_COUNTY", 2)
  )
})

test_that("geography is hashed but report and access are not", {
  base <- episodic_config_resolve(NA)
  expect_false(identical(
    episodic_config_hash(base)$hash,
    episodic_config_hash(
      utils::modifyList(base, list(geography = list(region_code = "ELSEWHERE")))
    )$hash
  ))
  expect_identical(
    episodic_config_hash(base)$hash,
    episodic_config_hash(
      utils::modifyList(base, list(report = list(small_count_threshold = 11)))
    )$hash
  )
  expect_identical(
    episodic_config_hash(base)$hash,
    episodic_config_hash(
      utils::modifyList(base, list(access = list(require_login = FALSE)))
    )$hash
  )
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
