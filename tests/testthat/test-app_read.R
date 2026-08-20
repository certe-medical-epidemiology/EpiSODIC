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

test_that("episode_app_open_clusters() lists the cluster with derived state new", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  open <- episode_app_open_clusters(env$con)
  expect_equal(nrow(open), 1)
  expect_equal(open$state[1], "new")
  expect_equal(open$pathogen[1], "Norovirus")
})

test_that("a cluster with an assessment event is excluded once closed, included otherwise", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L, verdict = "artefact", rationale = "test")
  open <- episode_app_open_clusters(env$con)
  expect_equal(nrow(open), 0)  # artefact is terminal -> closed -> not open
})

test_that("a non-terminal verdict explicitly closed via episode_cluster_state (trigger = closure) reads as closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "possible_epidemic", rationale = "test")
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closable")

  episode_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed",
                                   trigger = "closure", user_id = 1L)
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")
})

test_that("a later assessment event re-opens a cluster that was previously explicitly closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "possible_epidemic", rationale = "test")
  episode_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed",
                                   trigger = "closure", user_id = 1L)
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")

  Sys.sleep(1.1)  # created_at has second resolution; ensure strict ordering
  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "confirmed_epidemic", rationale = "reopened")
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closable")
})

test_that("a cron auto-close (trigger = system) with no classification at all reads as closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episode_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed", trigger = "system")
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")
})

test_that("episode_cluster_object() populates concentration, density and case_free", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)

  expect_equal(obj$pathogen, "Norovirus")
  expect_equal(obj$n_cases, 6)
  expect_true(grepl("^Test Hospital", obj$place) && grepl("afdeling B4$", obj$place))
  expect_false(is.null(obj$concentration))
  expect_equal(obj$concentration$dominant_label, "9711")  # 4 of 6 cases in PC4 9711
  expect_false(is.null(obj$density))
  expect_equal(obj$density$value, round(6 / 1000 * 1000, 2))
  expect_equal(obj$case_free$need, 14)
  expect_true(obj$rt_applicable)
})

test_that("episode_cluster_object() feeds directly into episode_interpretation_generate() without error", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)
  result <- episode_interpretation_generate(obj)
  expect_true(length(result$text) >= 2)  # at least magnitude + recommendation
  expect_true("concentration.high" %in% result$fired || "concentration.moderate" %in% result$fired)
})

test_that("episode_app_epi_curve() returns one row per day with case counts", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  curve <- episode_app_epi_curve(env$con, env$cluster_id)
  expect_equal(sum(curve$n_cases), 6)
  expect_equal(nrow(curve), 4)  # 10th through 13th
})

test_that("episode_app_linelist() returns only the architecture-allowed fields", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  ll <- episode_app_linelist(env$con, env$cluster_id)
  expect_equal(nrow(ll), 6)
  expect_setequal(names(ll), c("source_key", "sample_date", "sex", "age", "pc4", "ward", "specialism"))
})

test_that("episode_app_detection_settings() reflects the cluster's detectors and pathogen config", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  settings <- episode_app_detection_settings(env$con, env$cluster_id)
  expect_equal(settings$detectors, "same_place")
  expect_equal(settings$case_free_days, 14)
  expect_true(settings$rt_applicable)
})

test_that("episode_app_streams_screen() surfaces the latest run's config_snapshot, parsed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episode_app_streams_screen(env$con)
  expect_equal(nrow(screen$streams), 1)
  expect_false(is.null(screen$config_snapshot))
  expect_equal(screen$config_snapshot$reconciliation$close_after_runs, 14)
})

test_that("episode_app_status() reports the latest run", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  status <- episode_app_status(env$con)
  expect_equal(status$status, "success")
  expect_equal(status$n_clusters_open, 1)
})

test_that("episode_cluster_object()'s concentration carries a full per-PC4 breakdown, not just the dominant one", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)

  expect_false(is.null(obj$concentration$rows))
  expect_setequal(obj$concentration$rows$label, c("9711", "9712", "9713"))
  expect_equal(obj$concentration$rows$n[obj$concentration$rows$label == "9711"], 4)
  expect_equal(sum(obj$concentration$rows$n), 6)
})

test_that("episode_triangle_completeness() returns an empty frame rather than erroring when no rows survive the lag window", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # sample_date far outside max_lag_days from run_date -> filtered to zero rows
  episode_db_reporting_triangle_upsert(env$con, stream_id = env$stream_id, sample_date = "2024-01-01",
                                        run_date = "2025-06-01", n_cases = 3)
  result <- episode_triangle_completeness(env$con, env$stream_id, max_lag_days = 21)
  expect_equal(nrow(result), 0)
})

test_that("episode_app_trend() and episode_app_denominator_series() handle absent data gracefully", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  trend <- episode_app_trend(env$con, env$stream_id)
  expect_equal(nrow(trend), 0)  # no trend rows ever written in this fixture

  series <- episode_app_denominator_series(env$con, "Norovirus", episode_db_cluster_cases(env$con, env$cluster_id))
  expect_equal(nrow(series), 0)  # no denominator rows supplied
})

test_that("episode_cluster_object() populates curve_shape from the fixture's tightly-bunched cases", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)
  # fixture cases span 2025-01-10 to 2025-01-13 (3 days), incub_max_days = 3
  expect_equal(obj$curve_shape, "point_source")
})

test_that("episode_cluster_object()'s rt field is NULL with too little history, without erroring", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)
  expect_true(obj$rt_applicable)  # fixture pathogen config sets rt_applicable = 1
  expect_null(obj$rt)  # only 4 days of case history - not enough for one Rt window
})

test_that("episode_app_density() computes a historical baseline density, not just the current value", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # add more activity periods covering the fixture's case history (2025-01-10..13)
  episode_db_institution_activity_upsert(env$con, institution_id = env$institution_id,
                                          period_start = "2024-12-01", period_end = "2024-12-31", patient_days = 900)

  stream <- DBI::dbGetQuery(env$con, "SELECT * FROM episode_stream WHERE stream_id = ?", params = list(env$stream_id))[1, ]
  cases <- episode_db_cluster_cases(env$con, env$cluster_id)
  density <- episode_app_density(env$con, stream, cases)

  expect_false(is.null(density))
  expect_false(is.na(density$value))
  expect_false(is.na(density$baseline))
})

test_that("episode_farrington_population_vector() is NULL for non-institution levels and levels with no activity data", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  weeks <- as.Date("2025-01-06") + (0:3) * 7
  expect_null(episode_farrington_population_vector(env$con, NA, "pathogen_ward", weeks))
  expect_null(episode_farrington_population_vector(env$con, env$institution_id, "pathogen_ward", weeks))  # L1: no ward-level activity table
})

test_that("episode_farrington_population_vector() returns a weekly vector for a pathogen_institution stream with activity data", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  weeks <- as.Date(c("2025-01-06", "2025-01-13", "2025-06-01"))  # last week outside the fixture's activity period
  pop <- episode_farrington_population_vector(env$con, env$institution_id, "pathogen_institution", weeks)

  expect_length(pop, 3)
  expect_equal(pop[1], 1000)  # fixture activity: 2025-01-01..31, patient_days = 1000
  expect_equal(pop[3], 1000)  # falls back to the median when no period covers the week
})

test_that("episode_app_similar_clusters() finds no precedent when there is no closed cluster of the same pathogen", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  similar <- episode_app_similar_clusters(env$con, env$cluster_id)
  expect_equal(nrow(similar), 0)  # the only other cluster of this pathogen is itself, and it isn't closed
})

test_that("episode_app_similar_clusters() finds a closed same-pathogen cluster and excludes the target itself", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  other_id <- episode_db_cluster_insert(
    env$con, stream_id = env$stream_id, first_day = "2024-01-10", last_day = "2024-01-13",
    n_cases = 5, priority_score = 60, detector_agreement = 1, run_id = env$run_id
  )
  user_id <- episode_db_app_user_insert(env$con, "jdoe", "Jane Doe", "j@x.nl", "hash")
  episode_db_assessment_event_insert(env$con, other_id, user_id, verdict = "artefact", rationale = "t")
  episode_db_cluster_state_insert(env$con, other_id, state = "closed", trigger = "closure", user_id = user_id)

  similar <- episode_app_similar_clusters(env$con, env$cluster_id)
  expect_equal(nrow(similar), 1)
  expect_equal(similar$cluster_id[1], other_id)
  expect_equal(similar$verdict_label[1], episode_tr("verdict.artefact"))
})
