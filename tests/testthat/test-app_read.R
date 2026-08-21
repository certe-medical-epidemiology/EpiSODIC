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

test_that("episodic_app_open_clusters() lists the cluster with derived state new", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  open <- episodic_app_open_clusters(env$con)
  expect_equal(nrow(open), 1)
  expect_equal(open$state[1], "new")
  expect_equal(open$pathogen[1], "Norovirus")
})

test_that("a cluster with an assessment event is excluded once closed, included otherwise", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episodic_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L, verdict = "artefact", rationale = "test")
  open <- episodic_app_open_clusters(env$con)
  expect_equal(nrow(open), 0)  # artefact is terminal -> closed -> not open
})

test_that("a non-terminal verdict explicitly closed via episodic_cluster_state (trigger = closure) reads as closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episodic_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "possible_epidemic", rationale = "test")
  expect_equal(episodic_app_derive_state_for_cluster(env$con, env$cluster_id), "closable")

  episodic_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed",
                                   trigger = "closure", user_id = 1L)
  expect_equal(episodic_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")
})

test_that("a later assessment event re-opens a cluster that was previously explicitly closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episodic_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "possible_epidemic", rationale = "test")
  episodic_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed",
                                   trigger = "closure", user_id = 1L)
  expect_equal(episodic_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")

  Sys.sleep(1.1)  # created_at has second resolution; ensure strict ordering
  episodic_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "confirmed_epidemic", rationale = "reopened")
  expect_equal(episodic_app_derive_state_for_cluster(env$con, env$cluster_id), "closable")
})

test_that("a cron auto-close (trigger = system) with no classification at all reads as closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed", trigger = "system")
  expect_equal(episodic_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")
})

test_that("episodic_cluster_object() populates concentration, density and case_free", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episodic_cluster_object(env$con, env$cluster_id)

  expect_equal(obj$pathogen, "Norovirus")
  expect_equal(obj$n_cases, 6)
  expect_true(grepl("^Test Hospital", obj$place) && grepl("afdeling B4$", obj$place))
  expect_false(is.null(obj$concentration))
  expect_equal(obj$concentration$dominant_label, "9711")  # 4 of 6 cases in PC 9711
  expect_false(is.null(obj$density))
  expect_equal(obj$density$value, round(6 / 1000 * 1000, 2))
  expect_equal(obj$case_free$need, 14)
  expect_true(obj$rt_applicable)
})

test_that("episodic_cluster_object() feeds directly into episodic_interpretation_generate() without error", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episodic_cluster_object(env$con, env$cluster_id)
  result <- episodic_interpretation_generate(obj)
  expect_true(length(result$text) >= 2)  # at least magnitude + recommendation
  expect_true("concentration.high" %in% result$fired || "concentration.moderate" %in% result$fired)
})

test_that("episodic_app_epi_curve() returns one row per day with case counts", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  curve <- episodic_app_epi_curve(env$con, env$cluster_id)
  expect_equal(sum(curve$n_cases), 6)
  expect_equal(nrow(curve), 4)  # 10th through 13th
})

test_that("episodic_app_linelist() returns only the architecture-allowed fields", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  ll <- episodic_app_linelist(env$con, env$cluster_id)
  expect_equal(nrow(ll), 6)
  expect_setequal(names(ll), c("source_key", "sample_date", "sex", "age", "pc", "ward", "specialism"))
})

test_that("episodic_app_detection_settings() reflects the cluster's detectors and pathogen config", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  settings <- episodic_app_detection_settings(env$con, env$cluster_id)
  expect_equal(settings$detectors, "same_place")
  expect_equal(settings$case_free_days, 14)
  expect_true(settings$rt_applicable)
})

test_that("episodic_app_streams_screen() surfaces the latest run's config_snapshot, parsed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episodic_app_streams_screen(env$con)
  expect_equal(nrow(screen$streams), 1)
  expect_false(is.null(screen$config_snapshot))
  expect_equal(screen$config_snapshot$reconciliation$close_after_runs, 14)
})

test_that("episodic_app_status() reports the latest run", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  status <- episodic_app_status(env$con)
  expect_equal(status$status, "success")
  expect_equal(status$n_clusters_open, 1)
})

test_that("episodic_cluster_object()'s concentration carries a full per-PC breakdown, not just the dominant one", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episodic_cluster_object(env$con, env$cluster_id)

  expect_false(is.null(obj$concentration$rows))
  expect_setequal(obj$concentration$rows$label, c("9711", "9712", "9713"))
  expect_equal(obj$concentration$rows$n[obj$concentration$rows$label == "9711"], 4)
  expect_equal(sum(obj$concentration$rows$n), 6)
})

test_that("episodic_triangle_completeness() returns an empty frame rather than erroring when no rows survive the lag window", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # sample_date far outside max_lag_days from run_date -> filtered to zero rows
  episodic_db_reporting_triangle_upsert(env$con, stream_id = env$stream_id, sample_date = "2024-01-01",
                                        run_date = "2025-06-01", n_cases = 3)
  result <- episodic_triangle_completeness(env$con, env$stream_id, max_lag_days = 21)
  expect_equal(nrow(result), 0)
})

test_that("episodic_app_trend() and episodic_app_denominator_series() handle absent data gracefully", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  trend <- episodic_app_trend(env$con, env$stream_id)
  expect_equal(nrow(trend), 0)  # no trend rows ever written in this fixture

  series <- episodic_app_denominator_series(env$con, "Norovirus", episodic_db_cluster_cases(env$con, env$cluster_id))
  expect_equal(nrow(series), 0)  # no denominator rows supplied
})

test_that("episodic_cluster_object() populates curve_shape from the fixture's tightly-bunched cases", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episodic_cluster_object(env$con, env$cluster_id)
  # fixture cases span 2025-01-10 to 2025-01-13 (3 days), incub_max_days = 3
  expect_equal(obj$curve_shape, "point_source")
})

test_that("episodic_cluster_object()'s rt field is NULL with too little history, without erroring", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episodic_cluster_object(env$con, env$cluster_id)
  expect_true(obj$rt_applicable)  # fixture pathogen config sets rt_applicable = 1
  expect_null(obj$rt)  # only 4 days of case history - not enough for one Rt window
})

test_that("episodic_app_density() computes a historical baseline density, not just the current value", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # add more activity periods covering the fixture's case history (2025-01-10..13)
  episodic_db_institution_activity_upsert(env$con, institution_id = env$institution_id,
                                          period_start = "2024-12-01", period_end = "2024-12-31", patient_days = 900)

  stream <- DBI::dbGetQuery(env$con, "SELECT * FROM episodic_stream WHERE stream_id = ?", params = list(env$stream_id))[1, ]
  cases <- episodic_db_cluster_cases(env$con, env$cluster_id)
  density <- episodic_app_density(env$con, stream, cases)

  expect_false(is.null(density))
  expect_false(is.na(density$value))
  expect_false(is.na(density$baseline))
})

test_that("episodic_farrington_population_vector() is NULL for non-institution levels and levels with no activity data", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  weeks <- as.Date("2025-01-06") + (0:3) * 7
  expect_null(episodic_farrington_population_vector(env$con, NA, "pathogen_ward", weeks))
  expect_null(episodic_farrington_population_vector(env$con, env$institution_id, "pathogen_ward", weeks))  # L1: no ward-level activity table
})

test_that("episodic_farrington_population_vector() returns a weekly vector for a pathogen_institution stream with activity data", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  weeks <- as.Date(c("2025-01-06", "2025-01-13", "2025-06-01"))  # last week outside the fixture's activity period
  pop <- episodic_farrington_population_vector(env$con, env$institution_id, "pathogen_institution", weeks)

  expect_length(pop, 3)
  expect_equal(pop[1], 1000)  # fixture activity: 2025-01-01..31, patient_days = 1000
  expect_equal(pop[3], 1000)  # falls back to the median when no period covers the week
})

test_that("episodic_app_similar_clusters() finds no precedent when there is no closed cluster of the same pathogen", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  similar <- episodic_app_similar_clusters(env$con, env$cluster_id)
  expect_equal(nrow(similar), 0)  # the only other cluster of this pathogen is itself, and it isn't closed
})

test_that("episodic_app_similar_clusters() finds a closed same-pathogen cluster and excludes the target itself", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  other_id <- episodic_db_cluster_insert(
    env$con, stream_id = env$stream_id, first_day = "2024-01-10", last_day = "2024-01-13",
    n_cases = 5, priority_score = 60, detector_agreement = 1, run_id = env$run_id
  )
  user_id <- episodic_db_app_user_insert(env$con, "jdoe", "Jane Doe", "j@x.nl", "hash")
  episodic_db_assessment_event_insert(env$con, other_id, user_id, verdict = "artefact", rationale = "t")
  episodic_db_cluster_state_insert(env$con, other_id, state = "closed", trigger = "closure", user_id = user_id)

  similar <- episodic_app_similar_clusters(env$con, env$cluster_id)
  expect_equal(nrow(similar), 1)
  expect_equal(similar$cluster_id[1], other_id)
  expect_equal(similar$verdict_label[1], episodic_tr("verdict.artefact"))
})

test_that("episodic_app_doubling_time() refuses a doubling time for a flat series", {
  # The regression this exists for: regressing log(*cumulative*) cases on
  # day gives a positive slope for any series at all, because a constant
  # incidence still makes the cumulative count climb. Every cluster with
  # three cases used to report a finite doubling time.
  flat <- data.frame(sample_date = as.character(rep(seq(as.Date("2025-01-01"), by = "day",
                                                          length.out = 14), each = 2)))
  expect_true(is.na(episodic_app_doubling_time(flat, asof = as.Date("2025-01-14"))))

  declining <- data.frame(sample_date = as.character(rep(
    seq(as.Date("2025-01-01"), by = "day", length.out = 6), times = c(8, 6, 5, 3, 2, 1)
  )))
  expect_true(is.na(episodic_app_doubling_time(declining, asof = as.Date("2025-01-06"))))
})

test_that("episodic_app_doubling_time() reports a plausible doubling time for real growth", {
  # Doubling every 2 days: 1, 1, 2, 2, 4, 4, 8, 8 ...
  counts <- c(1, 1, 2, 2, 4, 4, 8, 8, 16, 16)
  days <- rep(seq(as.Date("2025-01-01"), by = "day", length.out = 10), times = counts)
  growing <- data.frame(sample_date = as.character(days))

  doubling <- episodic_app_doubling_time(growing, asof = as.Date("2025-01-10"))
  expect_false(is.na(doubling))
  expect_gt(doubling, 1)
  expect_lt(doubling, 4)
})

test_that("episodic_app_doubling_time() drops the under-ascertained tail before fitting", {
  # A genuinely growing series whose last three days are still being
  # reported reads as flattening if those days are fitted.
  counts <- c(1, 2, 4, 8, 16, 32, 1, 1, 1)
  days <- rep(seq(as.Date("2025-01-01"), by = "day", length.out = 9), times = counts)
  cases <- data.frame(sample_date = as.character(days))

  naive <- episodic_app_doubling_time(cases, incomplete_days = 0L, asof = as.Date("2025-01-09"))
  trimmed <- episodic_app_doubling_time(cases, incomplete_days = 3L, asof = as.Date("2025-01-09"))
  expect_false(is.na(trimmed))
  # Trimming the artefactual tail recovers the real, faster growth.
  expect_true(is.na(naive) || trimmed < naive)
})

test_that("episodic_app_completeness() reads the leading run of incomplete lags, not the largest one", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  sid <- env$stream_id

  # Completion curve: incomplete at lags 0-2, complete from lag 3, with a
  # single noisy dip at lag 11. A median over a modest number of sample
  # dates is not monotone, and taking the largest sub-95% lag used to
  # shade eleven days of complete data.
  triangle <- list(
    c(0, 1), c(1, 2), c(2, 3), c(3, 4), c(4, 4), c(5, 4), c(11, 3)
  )
  for (i in seq_along(triangle)) {
    lag <- triangle[[i]][1]
    seen <- triangle[[i]][2]
    episodic_db_reporting_triangle_upsert(
      env$con, stream_id = sid, sample_date = "2025-06-01",
      run_date = as.character(as.Date("2025-06-01") + lag), n_cases = seen
    )
  }
  expect_equal(episodic_app_completeness(env$con, sid)$incomplete_days, 3L)
})

test_that("episodic_app_completeness() shades everything when a stream never reaches 95%", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  for (lag in 0:5) {
    episodic_db_reporting_triangle_upsert(
      env$con, stream_id = env$stream_id, sample_date = "2025-06-01",
      run_date = as.character(as.Date("2025-06-01") + lag), n_cases = lag + 1
    )
  }
  # The eventual total only arrives well past max_lag_days, so no lag
  # inside the observable window is anywhere near 95% complete.
  episodic_db_reporting_triangle_upsert(
    env$con, stream_id = env$stream_id, sample_date = "2025-06-01",
    run_date = "2025-06-26", n_cases = 100L
  )
  expect_equal(episodic_app_completeness(env$con, env$stream_id)$incomplete_days, 6L)
})

test_that("episodic_app_data_asof() reads the latest successful run, not today", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  asof <- episodic_app_data_asof(env$con)
  expect_s3_class(asof, "Date")
  expect_equal(asof, Sys.Date())  # the fixture's run finished just now

  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(episodic_app_data_asof(con), Sys.Date())  # no run recorded at all
})

test_that("episodic_app_epi_curve() stops shading a cluster whose reporting finished long ago", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # Slow-reporting stream: incomplete at lags 0-4.
  for (lag in 0:5) {
    episodic_db_reporting_triangle_upsert(
      env$con, stream_id = env$stream_id, sample_date = "2025-06-01",
      run_date = as.character(as.Date("2025-06-01") + lag),
      n_cases = if (lag < 5) lag else 10
    )
  }
  # The fixture's cases are from January 2025 and the run is today, so
  # none of them can still be filling up. Anchoring the window on the
  # cluster's own last case day (as this used to) would fade its tail
  # permanently.
  curve <- episodic_app_epi_curve(env$con, env$cluster_id)
  expect_false(any(curve$incomplete))
})

test_that("episodic_app_concentration() measures the share among cases whose PC is known", {
  # Six cases, four in one PC and two with no PC at all: what was
  # observed is 100% concentration, not 67%. The diluted version fed both
  # the interpretation fragments and the priority score's spatial term.
  cases <- data.frame(pc = c("9711", "9711", "9711", "9711", NA, NA), stringsAsFactors = FALSE)
  concentration <- episodic_app_concentration(cases, "pathogen_region")
  expect_equal(concentration$dominant_share, 1)
  expect_equal(concentration$total, 4)
  expect_equal(concentration$n_unknown_pc, 2)

  expect_null(episodic_app_concentration(data.frame(pc = c(NA, NA)), "pathogen_region"))
})

test_that("episodic_app_denominator_series() computes positivity from region-wide cases, not cluster ones", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # Twice as many Norovirus cases region-wide as are in the cluster.
  extra <- data.frame(
    source_key = sprintf("EX%d", 1:6), patient_key = sprintf("EP%d", 1:6),
    sample_date = rep("2025-01-08", 6), receipt_date = rep("2025-01-08", 6),
    pathogen = "Norovirus", care_line = "first", institution_id = NA_integer_,
    ward = NA_character_, specialism = NA_character_, pc = "9800",
    sex = "M", age = 50L, first_seen_run = env$run_id, stringsAsFactors = FALSE
  )
  episodic_db_case_insert_new(env$con, extra, env$run_id)
  episodic_db_denominator_upsert(env$con, pathogen = "Norovirus", sample_date = "2025-01-06",
                                  care_line = "first", area_code = NA, n_tests = 100L)

  cluster_cases <- episodic_db_cluster_cases(env$con, env$cluster_id)
  series <- episodic_app_denominator_series(env$con, "Norovirus", cluster_cases)
  expect_equal(nrow(series), 1)
  # Week of 6 Jan: 6 region-wide extra cases plus the 4 cluster cases
  # sampled 10-12 Jan, over 100 tests.
  expect_equal(series$n_cases[1], 10L)
  expect_equal(series$n_cluster_cases[1], 4L)
  expect_equal(series$positivity[1], 0.10)
})

test_that("the archive lists cluster ids and links each row through to its dossier", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episodic_db_assessment_event_insert(env$con, env$cluster_id, user_id = 1L,
                                      verdict = "artefact", rationale = "test")

  archive <- episodic_app_archive(env$con, lang = "en")
  expect_equal(nrow(archive), 1)
  html <- as.character(episodic_ui_archive_screen(archive, lang = "en"))

  # last winter's assessment is only a useful precedent if you can open it
  expect_true(grepl("open_cluster", html, fixed = TRUE))
  expect_true(grepl("episodic-row-link", html, fixed = TRUE))
  expect_true(grepl(paste0(">", episodic_tr("dossier.cluster_ref", id = env$cluster_id, lang = "en"), "<"),
                     html, fixed = TRUE))
  # the id column leads, as it does on the Pathogen screen
  expect_lt(regexpr(episodic_tr("column.cluster", lang = "en"), html, fixed = TRUE),
             regexpr(episodic_tr("archive.col.pathogen", lang = "en"), html, fixed = TRUE))
})

test_that("episodic_ui_cluster_link_row() is the one place both cluster tables get their row from", {
  row <- as.character(episodic_ui_cluster_link_row(
    42L, lang = "en", shiny::tags$td("a cell")
  ))
  expect_true(grepl("Shiny.setInputValue", row, fixed = TRUE))
  expect_true(grepl("open_cluster", row, fixed = TRUE))
  expect_true(grepl("42", row, fixed = TRUE))
  expect_true(grepl("tabindex", row, fixed = TRUE))
  expect_true(grepl("a cell", row, fixed = TRUE))
  # the id cell comes first, before whatever cells the caller passed
  expect_lt(regexpr("episodic-cell-id", row, fixed = TRUE),
             regexpr("a cell", row, fixed = TRUE))
})
