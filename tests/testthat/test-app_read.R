app_read_setup <- function() {
  con <- episode_test_db()
  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp-app-read", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second",
    is_monitored = TRUE
  )
  episode_db_institution_activity_upsert(
    con, institution_id = institution_id, period_start = "2025-01-01", period_end = "2025-01-31",
    patient_days = 1000
  )
  pathogen_config <- data.frame(
    pathogen = "Norovirus", episode_days = 14, incub_min_days = 0.5, incub_max_days = 3,
    case_free_days = 14, cooldown_days = 14, rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5,
    si_dist = "gamma", mem_applicable = 0, severity_weight = 0.6, source_ref = NA,
    stringsAsFactors = FALSE
  )
  episode_db_pathogen_config_load(con, pathogen_config)

  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_ward", "Norovirus", institution_id = institution_id, ward = "B4"),
    level = "pathogen_ward", pathogen = "Norovirus", care_line = "second",
    institution_id = institution_id, ward = "B4", denominator = "patient_days",
    observed_date = "2025-01-15"
  )

  run_id <- episode_db_run_start(con, "host", "account")

  cases <- data.frame(
    source_key = sprintf("K%d", 1:6),
    patient_key = sprintf("P%d", 1:6),
    sample_date = c("2025-01-10", "2025-01-11", "2025-01-12", "2025-01-12", "2025-01-13", "2025-01-13"),
    receipt_date = c("2025-01-10", "2025-01-11", "2025-01-12", "2025-01-12", "2025-01-13", "2025-01-13"),
    pathogen = "Norovirus", care_line = "second", institution_id = institution_id, ward = "B4",
    specialism = "Interne", pc4 = c("9711", "9711", "9711", "9712", "9713", "9711"),
    sex = c("M", "F", "M", "F", "M", "F"), age = c(82, 79, 88, 45, 30, 76),
    first_seen_run = run_id, stringsAsFactors = FALSE
  )
  episode_db_case_insert_new(con, cases, run_id)

  cluster_id <- episode_db_cluster_insert(
    con, stream_id = stream_id, first_day = "2025-01-10", last_day = "2025-01-13", n_cases = 6,
    expected = 0.8, excess = 5.2, ratio = 7.5, priority_score = 91, detector_agreement = 1, run_id = run_id
  )
  all_cases <- DBI::dbGetQuery(con, "SELECT case_id FROM episode_case")
  for (case_id in all_cases$case_id) {
    episode_db_cluster_case_link(con, cluster_id, case_id)
  }
  detection_id <- episode_db_detection_insert(
    con, run_id = run_id, stream_id = stream_id, detector = "same_place",
    first_day = "2025-01-10", last_day = "2025-01-13", n_cases = 6, params_json = "{}"
  )
  episode_db_detection_set_cluster(con, detection_id, cluster_id)

  episode_db_run_finish(con, run_id, status = "success", n_streams = 1, n_detections = 1,
                         config_hash = strrep("a", 40), config_snapshot = '{"reconciliation":{"close_after_runs":14}}')

  list(con = con, institution_id = institution_id, stream_id = stream_id, cluster_id = cluster_id, run_id = run_id)
}

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

test_that("episode_cluster_object() feeds directly into episode_duiding_generate() without error", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)
  result <- episode_duiding_generate(obj)
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

test_that("episode_app_trend() and episode_app_denominator_series() handle absent data gracefully", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  trend <- episode_app_trend(env$con, env$stream_id)
  expect_equal(nrow(trend), 0)  # no trend rows ever written in this fixture

  series <- episode_app_denominator_series(env$con, "Norovirus", episode_db_cluster_cases(env$con, env$cluster_id))
  expect_equal(nrow(series), 0)  # no denominator rows supplied
})
