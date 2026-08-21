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

# Property-based tests for reconciliation: extension, split, merge,
# backfill, rerun idempotence, out-of-order runs, and no partial state
# after a failed run.
#
# These tests exercise episode_reconcile_stream() against a real SQLite
# database (schema, streams, cases) rather than mocking the repository
# layer, since the algorithm's correctness properties are about what ends
# up in episode_cluster/episode_cluster_case, not about the internal
# candidate-matching logic in isolation. episode_reconcile_case_count()
# recomputes n_cases from episode_case rows (not from the detector's own
# count), so each detection interval below is backed by real case rows.

reconcile_setup <- function() {
  con <- episode_test_db()
  DBI::dbExecute(
    con,
    "INSERT INTO episode_institution
      (institution_key, display_name, institution_type, care_line, is_monitored, is_active)
     VALUES ('a11fa90cac541318d868206b1c13b9d196afb124', 'Test Hospital', 'hospital', 'second', 1, 1)"
  )
  institution_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]

  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_institution", "Test organism", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Test organism",
    institution_id = institution_id, observed_date = "2025-01-01"
  )

  list(con = con, institution_id = institution_id, stream_id = stream_id)
}

# Inserts n_cases real episode_case rows spanning [first_day, last_day] (so
# episode_reconcile_case_count() has something real to recount) and returns
# the matching detection record for episode_reconcile_stream().
reconcile_detect <- function(env, run_id, first_day, last_day, n_cases, detector = "same_place") {
  dates <- seq(as.Date(first_day), as.Date(last_day), length.out = n_cases)
  dates <- as.character(as.Date(dates))
  for (i in seq_len(n_cases)) {
    source_key <- sprintf("CASE-%s-%d-%d", first_day, run_id, i)
    DBI::dbExecute(
      env$con,
      "INSERT INTO episode_case (source_key, patient_key, sample_date, pathogen, care_line,
        institution_id, first_seen_run) VALUES (?, ?, ?, 'Test organism', 'second', ?, ?)",
      params = list(source_key, source_key, dates[i], env$institution_id, run_id)
    )
  }
  episode_detection_record(env$stream_id, detector, first_day, last_day, n_cases)
}

# no-op priority/assessment helpers for tests that do not care about them
noop_priority_score <- function(candidate) 50
noop_has_assessment <- function(cluster_id) FALSE
noop_verdict <- function(cluster_id) NA_character_

reconcile_run <- function(env, run_id, det, case_free_days = 14, close_after_runs = 14,
                           has_assessment_fn = noop_has_assessment, verdict_fn = noop_verdict) {
  episode_reconcile_stream(
    env$con, env$stream_id, det, case_free_days = case_free_days, run_id = run_id,
    close_after_runs = close_after_runs, priority_score_fn = noop_priority_score,
    has_assessment_fn = has_assessment_fn, verdict_fn = verdict_fn
  )
}

test_that("a cluster extended by a later case keeps its identity", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run1, det1)
  clusters1 <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters1), 1)
  original_id <- clusters1$cluster_id[1]

  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-08", "2025-01-08", 1)
  reconcile_run(env, run2, det2)

  clusters2 <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters2), 1)
  expect_equal(clusters2$cluster_id[1], original_id)
  expect_equal(clusters2$last_day[1], "2025-01-08")
  expect_equal(clusters2$n_cases[1], 4)
})

test_that("a cluster split by a case-free gap yields the correct number of clusters", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  case_free_days <- 14

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run1, det1, case_free_days = case_free_days)

  # a detection far enough away (beyond case_free_days) must open a new cluster
  run2 <- episode_db_run_start(env$con, "h", "a")
  far_start <- as.character(as.Date("2025-01-05") + case_free_days + 10)
  far_end <- as.character(as.Date("2025-01-05") + case_free_days + 12)
  det2 <- reconcile_detect(env, run2, far_start, far_end, 4)
  reconcile_run(env, run2, det2, case_free_days = case_free_days)

  clusters <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 2)
})

test_that("two clusters growing together merge, the older identity survives, assessment history kept", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episode_db_run_start(env$con, "h", "a")
  det_a <- reconcile_detect(env, run1, "2025-01-01", "2025-01-02", 2)
  reconcile_run(env, run1, det_a)
  older_id <- episode_db_clusters_for_stream(env$con, env$stream_id)$cluster_id[1]

  Sys.sleep(1.1)  # ensure a distinguishable opened_at for "older identity survives"
  run2 <- episode_db_run_start(env$con, "h", "a")
  det_b <- reconcile_detect(env, run2, "2025-01-20", "2025-01-21", 2)
  reconcile_run(env, run2, det_b, case_free_days = 3)
  clusters_before_merge <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters_before_merge), 2)
  younger_id <- setdiff(clusters_before_merge$cluster_id, older_id)

  # record an assessment on the younger cluster before it gets merged away
  episode_db_app_user_insert(env$con, "tester", "Test User", "tester@example.com", "hash")
  episode_db_assessment_event_insert(
    env$con, cluster_id = younger_id, user_id = 1L, verdict = "cluster_not_yet", rationale = "test"
  )

  # a candidate spanning both existing intervals forces a merge
  run3 <- episode_db_run_start(env$con, "h", "a")
  det_bridge <- episode_detection_record(env$stream_id, "same_place", "2025-01-01", "2025-01-21", 4)
  reconcile_run(env, run3, det_bridge)

  all_clusters <- episode_db_clusters(env$con)
  all_clusters <- all_clusters[all_clusters$stream_id == env$stream_id, ]
  survivors <- all_clusters[is.na(all_clusters$merged_into), ]
  expect_equal(nrow(survivors), 1)
  expect_equal(survivors$cluster_id[1], older_id)

  merged_away <- all_clusters[!is.na(all_clusters$merged_into), ]
  expect_equal(nrow(merged_away), 1)
  expect_equal(merged_away$cluster_id[1], younger_id)
  expect_equal(merged_away$merged_into[1], older_id)

  # assessment history on the merged-away cluster is not lost
  events <- episode_db_assessment_events(env$con, younger_id)
  expect_equal(nrow(events), 1)
})

test_that("a backfilled case with an older sample date attaches to the right cluster and resets the case-free clock", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-10", "2025-01-12", 3)
  reconcile_run(env, run1, det1)
  cluster_id <- episode_db_clusters_for_stream(env$con, env$stream_id)$cluster_id[1]

  # a late-arriving case with an EARLIER sample date than the cluster's current first_day
  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-05", "2025-01-05", 1)
  reconcile_run(env, run2, det2)

  clusters <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$cluster_id[1], cluster_id)
  expect_equal(clusters$first_day[1], "2025-01-05")
  expect_equal(clusters$runs_since_detected[1], 0)
})

test_that("running the same detection twice creates no new clusters and no duplicate rows", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run1, det1)
  n_after_first <- nrow(episode_db_clusters_for_stream(env$con, env$stream_id))

  # re-running the SAME detection (identical interval, no new cases) must be
  # idempotent: same interval matches the existing cluster, no new rows
  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- episode_detection_record(env$stream_id, "same_place", "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run2, det2)
  n_after_second <- nrow(episode_db_clusters_for_stream(env$con, env$stream_id))

  expect_equal(n_after_first, 1)
  expect_equal(n_after_second, 1)

  # re-running the same detection must not duplicate episode_cluster_case rows
  n_cluster_case_rows <- DBI::dbGetQuery(
    env$con, "SELECT COUNT(*) n FROM episode_cluster_case"
  )$n
  expect_equal(n_cluster_case_rows, 3)
})

test_that("runs applied out of order converge to the same state as in-order runs", {
  env1 <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env1$con))
  runs_inorder <- list(c("2025-01-01", "2025-01-02"), c("2025-01-10", "2025-01-11"))
  for (interval in runs_inorder) {
    run <- episode_db_run_start(env1$con, "h", "a")
    det <- reconcile_detect(env1, run, interval[1], interval[2], 2)
    reconcile_run(env1, run, det)
  }

  env2 <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env2$con), add = TRUE)
  runs_reversed <- rev(runs_inorder)
  for (interval in runs_reversed) {
    run <- episode_db_run_start(env2$con, "h", "a")
    det <- reconcile_detect(env2, run, interval[1], interval[2], 2)
    reconcile_run(env2, run, det)
  }

  clusters1 <- episode_db_clusters_for_stream(env1$con, env1$stream_id)
  clusters2 <- episode_db_clusters_for_stream(env2$con, env2$stream_id)
  expect_equal(nrow(clusters1), nrow(clusters2))
  expect_equal(sort(clusters1$first_day), sort(clusters2$first_day))
  expect_equal(sort(clusters1$last_day), sort(clusters2$last_day))
  expect_equal(sum(clusters1$n_cases), sum(clusters2$n_cases))
})

test_that("a failed run inside the cron transaction leaves no partial state", {
  path <- tempfile(fileext = ".sqlite")
  episode_db_create(path)
  con <- episode_db_connect(path)
  on.exit(DBI::dbDisconnect(con))

  n_streams_before <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_stream")$n
  n_cases_before <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n

  bad_source <- function() {
    raw <- episode_ingest_source_synthetic(
      start_date = as.Date("2024-01-01"), end_date = as.Date("2024-01-05")
    )
    raw$pathogen <- NULL  # violates the ingestion interface, forces an error mid-run
    raw
  }

  run_id <- episode_run_cron(path, ingest_source_fn = bad_source, run_date = as.Date("2024-01-05"))
  status <- DBI::dbGetQuery(con, "SELECT status FROM episode_detection_run WHERE run_id = ?",
                             params = list(run_id))$status[1]
  expect_equal(status, "failed")

  n_streams_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_stream")$n
  n_cases_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n
  expect_equal(n_streams_after, n_streams_before)
  expect_equal(n_cases_after, n_cases_before)
})

# The cool-down escape hatch: a candidate more
# than case_free_days but within cooldown_days of a terminal-verdict
# (artefact/expected_variation) closed cluster is absorbed into it rather
# than opening a new cluster - and, only if its excess materially exceeds
# what the terminal verdict was based on, flags changed_since_assessment
# so it surfaces as Herbeoordeling nodig (state_derive.R).

test_that("a candidate within cooldown_days of a terminal-verdict cluster is absorbed, not opened as new", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  terminal_verdict <- function(cluster_id) "artefact"

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)
  original_id <- episode_db_clusters_for_stream(env$con, env$stream_id)$cluster_id[1]

  # 20 days after the cluster's last_day: past case_free_days (14) but
  # within cooldown_days (21), and the candidate is only 2 cases - well
  # under the 1.5x escape-hatch ratio against the original 3.
  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-23", "2025-01-23", 2)
  episode_reconcile_stream(
    env$con, env$stream_id, det2, case_free_days = 14, run_id = run2,
    close_after_runs = 14, priority_score_fn = noop_priority_score,
    has_assessment_fn = function(id) TRUE, verdict_fn = terminal_verdict,
    cooldown_days = 21, cooldown_reopen_ratio = 1.5
  )

  clusters <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1)  # absorbed, not a second cluster
  expect_equal(clusters$cluster_id[1], original_id)
  expect_equal(clusters$last_day[1], "2025-01-23")
  expect_false(as.logical(clusters$changed_since_assessment[1]))  # below the reopen bar
})

test_that("a candidate within cooldown_days that materially exceeds the terminal verdict reopens as changed", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  terminal_verdict <- function(cluster_id) "expected_variation"

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)
  original_id <- episode_db_clusters_for_stream(env$con, env$stream_id)$cluster_id[1]

  # 10 new cases >= 3 * 1.5 -> clears the "half again" escape-hatch bar
  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-23", "2025-01-25", 10)
  episode_reconcile_stream(
    env$con, env$stream_id, det2, case_free_days = 14, run_id = run2,
    close_after_runs = 14, priority_score_fn = noop_priority_score,
    has_assessment_fn = function(id) TRUE, verdict_fn = terminal_verdict,
    cooldown_days = 21, cooldown_reopen_ratio = 1.5
  )

  clusters <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$cluster_id[1], original_id)
  expect_true(as.logical(clusters$changed_since_assessment[1]))

  # and that this actually surfaces as "reassess" through the full derive-state path
  events <- data.frame(event_id = 1L, verdict = "expected_variation", snooze_until = NA,
                        created_at = "2025-01-04T00:00:00Z")
  state <- episode_derive_state(events, changed_since_assessment = TRUE)
  expect_equal(state, "reassess")
})

test_that("a candidate beyond cooldown_days entirely opens a new cluster as usual", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  terminal_verdict <- function(cluster_id) "artefact"

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)

  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-03-01", "2025-03-01", 3)  # far past cooldown_days
  episode_reconcile_stream(
    env$con, env$stream_id, det2, case_free_days = 14, run_id = run2,
    close_after_runs = 14, priority_score_fn = noop_priority_score,
    has_assessment_fn = function(id) TRUE, verdict_fn = terminal_verdict,
    cooldown_days = 21, cooldown_reopen_ratio = 1.5
  )

  clusters <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 2)  # a genuinely new cluster, not absorbed
})

test_that("cooldown_days = NA disables the escape hatch entirely, for backward compatibility", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episode_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)

  run2 <- episode_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-23", "2025-01-23", 2)
  reconcile_run(env, run2, det2, case_free_days = 14, has_assessment_fn = function(id) TRUE,
                verdict_fn = function(id) "artefact")  # cooldown_days left at its NA default

  clusters <- episode_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 2)  # opens as new, exactly the pre-existing behaviour
})
