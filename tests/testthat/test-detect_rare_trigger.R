rare_case <- function(source_key, sample_date, pathogen, institution_id = 1L) {
  data.frame(
    source_key = source_key, pathogen = pathogen, institution_id = institution_id,
    sample_date = sample_date, stringsAsFactors = FALSE
  )
}

rare_add_institution <- function(con) {
  key <- digest::digest(paste("rare-trigger-test", stats::runif(1)), algo = "sha1", serialize = FALSE)
  episode_db_institution_upsert(
    con, institution_key = key, display_name = "Test Institution", institution_type = "hospital",
    care_line = "second", is_monitored = TRUE
  )
}

test_that("a single case of a curated rare pathogen fires a detection", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  id <- rare_add_institution(con)
  cases <- rare_case("K1", "2025-01-01", "Neisseria meningitidis", id)
  config <- episode_test_config()

  result <- episode_detect_rare_trigger(con, cases, episode_db_institutions(con), config)
  expect_equal(nrow(result), 1)
  expect_equal(result$detector[1], "rare_trigger")
  expect_equal(result$n_cases[1], 1)
})

test_that("a pathogen not on the curated list never fires", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  id <- rare_add_institution(con)
  cases <- rare_case("K1", "2025-01-01", "Campylobacter", id)
  config <- episode_test_config()

  result <- episode_detect_rare_trigger(con, cases, episode_db_institutions(con), config)
  expect_equal(nrow(result), 0)
})

test_that("matching is case-insensitive against the curated list", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  id <- rare_add_institution(con)
  cases <- rare_case("K1", "2025-01-01", "neisseria meningitidis", id)
  config <- episode_test_config()

  result <- episode_detect_rare_trigger(con, cases, episode_db_institutions(con), config)
  expect_equal(nrow(result), 1)
})

test_that("an empty cases data frame produces no detections", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  config <- episode_test_config()
  empty <- rare_case("K1", "2025-01-01", "Neisseria meningitidis")[0, ]
  result <- episode_detect_rare_trigger(con, empty, episode_db_institutions(con), config)
  expect_equal(nrow(result), 0)
})
