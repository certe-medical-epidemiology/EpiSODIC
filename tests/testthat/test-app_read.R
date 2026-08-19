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
