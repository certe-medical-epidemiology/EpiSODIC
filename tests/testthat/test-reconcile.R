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
# These tests exercise episodic_reconcile_stream() against a real SQLite
# database (schema, streams, cases) rather than mocking the repository
# layer, since the algorithm's correctness properties are about what ends
# up in episodic_cluster/episodic_cluster_case, not about the internal
# candidate-matching logic in isolation. episodic_reconcile_case_count()
# recomputes n_cases from episodic_case rows (not from the detector's own
# count), so each detection interval below is backed by real case rows.

reconcile_setup <- function() {
  con <- episodic_test_db()
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_institution
      (institution_key, display_name, institution_type, care_line, is_monitored, is_active)
     VALUES ('a11fa90cac541318d868206b1c13b9d196afb124', 'Test Hospital', 'hospital', 'second', 1, 1)"
  )
  institution_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[
    1
  ]

  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_institution",
      "Test pathogen",
      institution_id = institution_id
    ),
    level = "pathogen_institution",
    pathogen = "Test pathogen",
    institution_id = institution_id,
    observed_date = "2025-01-01"
  )

  list(con = con, institution_id = institution_id, stream_id = stream_id)
}

# Inserts n_cases real episodic_case rows spanning [first_day, last_day] (so
# episodic_reconcile_case_count() has something real to recount) and returns
# the matching detection record for episodic_reconcile_stream().
reconcile_detect <- function(
    env,
    run_id,
    first_day,
    last_day,
    n_cases,
    detector = "same_place") {
  dates <- seq(as.Date(first_day), as.Date(last_day), length.out = n_cases)
  dates <- as.character(as.Date(dates))
  for (i in seq_len(n_cases)) {
    source_key <- sprintf("CASE-%s-%d-%d", first_day, run_id, i)
    DBI::dbExecute(
      env$con,
      "INSERT INTO episodic_case (source_key, patient_key, sample_date, pathogen, care_line,
        institution_id, first_seen_run) VALUES (?, ?, ?, 'Test pathogen', 'second', ?, ?)",
      params = list(
        source_key,
        source_key,
        dates[i],
        env$institution_id,
        run_id
      )
    )
  }
  episodic_detection_record(
    env$stream_id,
    detector,
    first_day,
    last_day,
    n_cases
  )
}

# no-op priority/assessment helpers for tests that do not care about them
noop_priority_score <- function(candidate) 50
noop_has_assessment <- function(cluster_id) FALSE
noop_verdict <- function(cluster_id) NA_character_

reconcile_run <- function(
    env,
    run_id,
    det,
    case_free_days = 14,
    close_after_runs = 14,
    has_assessment_fn = noop_has_assessment,
    verdict_fn = noop_verdict) {
  episodic_reconcile_stream(
    env$con,
    env$stream_id,
    det,
    case_free_days = case_free_days,
    run_id = run_id,
    close_after_runs = close_after_runs,
    priority_score_fn = noop_priority_score,
    has_assessment_fn = has_assessment_fn,
    verdict_fn = verdict_fn
  )
}

test_that("a cluster extended by a later case keeps its identity", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run1, det1)
  clusters1 <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters1), 1)
  original_id <- clusters1$cluster_id[1]

  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-08", "2025-01-08", 1)
  reconcile_run(env, run2, det2)

  clusters2 <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters2), 1)
  expect_equal(clusters2$cluster_id[1], original_id)
  expect_equal(clusters2$last_day[1], "2025-01-08")
  expect_equal(clusters2$n_cases[1], 4)
})

test_that("a cluster split by a case-free gap yields the correct number of clusters", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  case_free_days <- 14

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run1, det1, case_free_days = case_free_days)

  # a detection far enough away (beyond case_free_days) must open a new cluster
  run2 <- episodic_db_run_start(env$con, "h", "a")
  far_start <- as.character(as.Date("2025-01-05") + case_free_days + 10)
  far_end <- as.character(as.Date("2025-01-05") + case_free_days + 12)
  det2 <- reconcile_detect(env, run2, far_start, far_end, 4)
  reconcile_run(env, run2, det2, case_free_days = case_free_days)

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 2)
})

test_that("two clusters growing together merge, the older identity survives, assessment history kept", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det_a <- reconcile_detect(env, run1, "2025-01-01", "2025-01-02", 2)
  reconcile_run(env, run1, det_a)
  older_id <- episodic_db_clusters_for_stream(
    env$con,
    env$stream_id
  )$cluster_id[1]

  Sys.sleep(1.1) # ensure a distinguishable opened_at for "older identity survives"
  run2 <- episodic_db_run_start(env$con, "h", "a")
  det_b <- reconcile_detect(env, run2, "2025-01-20", "2025-01-21", 2)
  reconcile_run(env, run2, det_b, case_free_days = 3)
  clusters_before_merge <- episodic_db_clusters_for_stream(
    env$con,
    env$stream_id
  )
  expect_equal(nrow(clusters_before_merge), 2)
  younger_id <- setdiff(clusters_before_merge$cluster_id, older_id)

  # record an assessment on the younger cluster before it gets merged away
  episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "tester@example.com",
    "hash"
  )
  episodic_db_assessment_event_insert(
    env$con,
    cluster_id = younger_id,
    user_id = 1L,
    verdict = "cluster_not_yet",
    rationale = "test"
  )

  # a candidate spanning both existing intervals forces a merge
  run3 <- episodic_db_run_start(env$con, "h", "a")
  det_bridge <- episodic_detection_record(
    env$stream_id,
    "same_place",
    "2025-01-01",
    "2025-01-21",
    4
  )
  reconcile_run(env, run3, det_bridge)

  all_clusters <- episodic_db_clusters(env$con)
  all_clusters <- all_clusters[all_clusters$stream_id == env$stream_id, ]
  survivors <- all_clusters[is.na(all_clusters$merged_into), ]
  expect_equal(nrow(survivors), 1)
  expect_equal(survivors$cluster_id[1], older_id)

  merged_away <- all_clusters[!is.na(all_clusters$merged_into), ]
  expect_equal(nrow(merged_away), 1)
  expect_equal(merged_away$cluster_id[1], younger_id)
  expect_equal(merged_away$merged_into[1], older_id)

  # assessment history on the merged-away cluster is not lost
  events <- episodic_db_assessment_events(env$con, younger_id)
  expect_equal(nrow(events), 1)
})

test_that("a backfilled case with an older sample date attaches to the right cluster and resets the case-free clock", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-10", "2025-01-12", 3)
  reconcile_run(env, run1, det1)
  cluster_id <- episodic_db_clusters_for_stream(
    env$con,
    env$stream_id
  )$cluster_id[1]

  # a late-arriving case with an EARLIER sample date than the cluster's current first_day
  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-05", "2025-01-05", 1)
  reconcile_run(env, run2, det2)

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$cluster_id[1], cluster_id)
  expect_equal(clusters$first_day[1], "2025-01-05")
  expect_equal(clusters$runs_since_detected[1], 0)
})

test_that("running the same detection twice creates no new clusters and no duplicate rows", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-05", 3)
  reconcile_run(env, run1, det1)
  n_after_first <- nrow(episodic_db_clusters_for_stream(env$con, env$stream_id))

  # re-running the SAME detection (identical interval, no new cases) must be
  # idempotent: same interval matches the existing cluster, no new rows
  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- episodic_detection_record(
    env$stream_id,
    "same_place",
    "2025-01-01",
    "2025-01-05",
    3
  )
  reconcile_run(env, run2, det2)
  n_after_second <- nrow(episodic_db_clusters_for_stream(
    env$con,
    env$stream_id
  ))

  expect_equal(n_after_first, 1)
  expect_equal(n_after_second, 1)

  # re-running the same detection must not duplicate episodic_cluster_case rows
  n_cluster_case_rows <- DBI::dbGetQuery(
    env$con,
    "SELECT COUNT(*) n FROM episodic_cluster_case"
  )$n
  expect_equal(n_cluster_case_rows, 3)
})

test_that("runs applied out of order converge to the same state as in-order runs", {
  env1 <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env1$con))
  runs_inorder <- list(
    c("2025-01-01", "2025-01-02"),
    c("2025-01-10", "2025-01-11")
  )
  for (interval in runs_inorder) {
    run <- episodic_db_run_start(env1$con, "h", "a")
    det <- reconcile_detect(env1, run, interval[1], interval[2], 2)
    reconcile_run(env1, run, det)
  }

  env2 <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env2$con), add = TRUE)
  runs_reversed <- rev(runs_inorder)
  for (interval in runs_reversed) {
    run <- episodic_db_run_start(env2$con, "h", "a")
    det <- reconcile_detect(env2, run, interval[1], interval[2], 2)
    reconcile_run(env2, run, det)
  }

  clusters1 <- episodic_db_clusters_for_stream(env1$con, env1$stream_id)
  clusters2 <- episodic_db_clusters_for_stream(env2$con, env2$stream_id)
  expect_equal(nrow(clusters1), nrow(clusters2))
  expect_equal(sort(clusters1$first_day), sort(clusters2$first_day))
  expect_equal(sort(clusters1$last_day), sort(clusters2$last_day))
  expect_equal(sum(clusters1$n_cases), sum(clusters2$n_cases))
})

test_that("a failed run inside the cron transaction leaves no partial state", {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))

  n_streams_before <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) n FROM episodic_stream"
  )$n
  n_cases_before <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) n FROM episodic_case"
  )$n

  bad_source <- function() {
    raw <- episodic_synthetic_cases(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2024-01-05")
    )
    raw$pathogen <- NULL # violates the case data contract, forces an error mid-run
    raw
  }

  # The run refuses out loud - the operator whose extract this is has to
  # hear about it - and still records the attempt.
  expect_error(
    episodic_run_cron(
      db_path = path,
      cases = bad_source,
      run_date = as.Date("2024-01-05")
    ),
    "pathogen"
  )
  status <- DBI::dbGetQuery(
    con,
    "SELECT status FROM episodic_detection_run ORDER BY run_id DESC LIMIT 1"
  )$status[1]
  expect_equal(status, "failed")

  n_streams_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) n FROM episodic_stream"
  )$n
  n_cases_after <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) n FROM episodic_case"
  )$n
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

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)
  original_id <- episodic_db_clusters_for_stream(
    env$con,
    env$stream_id
  )$cluster_id[1]

  # 20 days after the cluster's last_day: past case_free_days (14) but
  # within cooldown_days (21), and the candidate is only 2 cases - well
  # under the 1.5x escape-hatch ratio against the original 3.
  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-23", "2025-01-23", 2)
  episodic_reconcile_stream(
    env$con,
    env$stream_id,
    det2,
    case_free_days = 14,
    run_id = run2,
    close_after_runs = 14,
    priority_score_fn = noop_priority_score,
    has_assessment_fn = function(id) TRUE,
    verdict_fn = terminal_verdict,
    cooldown_days = 21,
    cooldown_reopen_ratio = 1.5
  )

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1) # absorbed, not a second cluster
  expect_equal(clusters$cluster_id[1], original_id)
  expect_equal(clusters$last_day[1], "2025-01-23")
  expect_false(as.logical(clusters$changed_since_assessment[1])) # below the reopen bar
})

test_that("a candidate within cooldown_days that materially exceeds the terminal verdict reopens as changed", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  terminal_verdict <- function(cluster_id) "expected_variation"

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)
  original_id <- episodic_db_clusters_for_stream(
    env$con,
    env$stream_id
  )$cluster_id[1]

  # 10 new cases >= 3 * 1.5 -> clears the "half again" escape-hatch bar
  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-23", "2025-01-25", 10)
  episodic_reconcile_stream(
    env$con,
    env$stream_id,
    det2,
    case_free_days = 14,
    run_id = run2,
    close_after_runs = 14,
    priority_score_fn = noop_priority_score,
    has_assessment_fn = function(id) TRUE,
    verdict_fn = terminal_verdict,
    cooldown_days = 21,
    cooldown_reopen_ratio = 1.5
  )

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$cluster_id[1], original_id)
  expect_true(as.logical(clusters$changed_since_assessment[1]))

  # and that this actually surfaces as "reassess" through the full derive-state path
  events <- data.frame(
    event_id = 1L,
    verdict = "expected_variation",
    snooze_until = NA,
    created_at = "2025-01-04T00:00:00Z"
  )
  state <- episodic_derive_state(events, changed_since_assessment = TRUE)
  expect_equal(state, "reassess")
})

test_that("a candidate beyond cooldown_days entirely opens a new cluster as usual", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  terminal_verdict <- function(cluster_id) "artefact"

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)

  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-03-01", "2025-03-01", 3) # far past cooldown_days
  episodic_reconcile_stream(
    env$con,
    env$stream_id,
    det2,
    case_free_days = 14,
    run_id = run2,
    close_after_runs = 14,
    priority_score_fn = noop_priority_score,
    has_assessment_fn = function(id) TRUE,
    verdict_fn = terminal_verdict,
    cooldown_days = 21,
    cooldown_reopen_ratio = 1.5
  )

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 2) # a genuinely new cluster, not absorbed
})

test_that("cooldown_days = NA disables the escape hatch entirely, for backward compatibility", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episodic_db_run_start(env$con, "h", "a")
  det1 <- reconcile_detect(env, run1, "2025-01-01", "2025-01-03", 3)
  reconcile_run(env, run1, det1, case_free_days = 14)

  run2 <- episodic_db_run_start(env$con, "h", "a")
  det2 <- reconcile_detect(env, run2, "2025-01-23", "2025-01-23", 2)
  reconcile_run(
    env,
    run2,
    det2,
    case_free_days = 14,
    has_assessment_fn = function(id) TRUE,
    verdict_fn = function(id) "artefact"
  ) # cooldown_days left at its NA default

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 2) # opens as new, exactly the pre-existing behaviour
})

test_that("episodic_reconcile_merge_detections() carries the baseline through, taking the most cautious of a group", {
  detections <- rbind(
    episodic_detection_record(
      1L,
      "farrington",
      "2025-01-01",
      "2025-01-07",
      12,
      expected = 3,
      upperbound = 6
    ),
    episodic_detection_record(
      1L,
      "mem",
      "2025-01-05",
      "2025-01-11",
      12,
      expected = NA_real_,
      upperbound = 8
    ),
    episodic_detection_record(1L, "same_place", "2025-01-06", "2025-01-09", 9)
  )
  candidates <- episodic_reconcile_merge_detections(detections)
  expect_equal(nrow(candidates), 1)
  expect_equal(candidates$expected, 3)
  # A higher upperbound means a smaller excess: a candidate several
  # detectors flagged is never made to look more aberrant than the most
  # cautious of them judged it.
  expect_equal(candidates$upperbound, 8)
  expect_equal(candidates$detector_agreement, 3L)
})

test_that("episodic_reconcile_merge_detections() reports no baseline when no detector fitted one", {
  detections <- rbind(
    episodic_detection_record(1L, "same_place", "2025-01-01", "2025-01-07", 5),
    episodic_detection_record(1L, "rare_trigger", "2025-01-03", "2025-01-06", 5)
  )
  candidates <- episodic_reconcile_merge_detections(detections)
  expect_true(is.na(candidates$expected))
  expect_true(is.na(candidates$upperbound))

  empty <- episodic_reconcile_merge_detections(
    episodic_detection_record(
      integer(0),
      character(0),
      character(0),
      character(0),
      integer(0)
    )
  )
  expect_equal(nrow(empty), 0)
  expect_true(all(c("expected", "upperbound") %in% names(empty)))
})

test_that("episodic_reconcile_candidate_metrics() derives excess and ratio, and declines to invent either", {
  full <- episodic_reconcile_candidate_metrics(
    data.frame(n_cases = 12, expected = 3, upperbound = 6)
  )
  expect_equal(full$expected, 3)
  expect_equal(full$excess, 6)
  expect_equal(full$ratio, 4)

  none <- episodic_reconcile_candidate_metrics(
    data.frame(n_cases = 5, expected = NA_real_, upperbound = NA_real_)
  )
  expect_true(is.na(none$excess))
  expect_true(is.na(none$ratio))

  # An expectation of zero is a real Farrington output; a ratio against
  # it is undefined rather than infinite.
  zero <- episodic_reconcile_candidate_metrics(
    data.frame(n_cases = 4, expected = 0, upperbound = 1)
  )
  expect_true(is.na(zero$ratio))
  expect_equal(zero$excess, 3)

  # Callers that predate the columns entirely must still work.
  legacy <- episodic_reconcile_candidate_metrics(data.frame(n_cases = 7))
  expect_true(is.na(legacy$expected))
  expect_true(is.na(legacy$ratio))
})

test_that("episodic_reconcile_stream() persists expected/excess/ratio onto the cluster it opens", {
  # These columns exist on episodic_cluster, and both the dossier's stat
  # grid and the interpretation engine's magnitude fragments read them -
  # but nothing ever wrote them, so every cluster carried NA and the O/E
  # ratio never appeared anywhere in the interface.
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))
  run_id <- episodic_db_run_start(env$con, "host", "account")

  det <- reconcile_detect(
    env,
    run_id,
    "2025-01-01",
    "2025-01-07",
    12,
    detector = "farrington"
  )
  det$expected <- 3
  det$upperbound <- 6
  reconcile_run(env, run_id, det)

  cluster <- DBI::dbGetQuery(
    env$con,
    "SELECT expected, excess, ratio FROM episodic_cluster"
  )
  expect_equal(nrow(cluster), 1)
  expect_equal(cluster$expected[1], 3)
  expect_equal(cluster$excess[1], 6)
  expect_equal(cluster$ratio[1], 4)
})

test_that("episodic_reconcile_stream() refreshes the baseline when a cluster is extended", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run1 <- episodic_db_run_start(env$con, "host", "account")
  det1 <- reconcile_detect(
    env,
    run1,
    "2025-01-01",
    "2025-01-07",
    12,
    detector = "farrington"
  )
  det1$expected <- 3
  det1$upperbound <- 6
  reconcile_run(env, run1, det1)

  run2 <- episodic_db_run_start(env$con, "host", "account")
  det2 <- reconcile_detect(
    env,
    run2,
    "2025-01-08",
    "2025-01-14",
    20,
    detector = "farrington"
  )
  det2$expected <- 4
  det2$upperbound <- 7
  reconcile_run(env, run2, det2)

  cluster <- DBI::dbGetQuery(
    env$con,
    "SELECT expected, excess, ratio FROM episodic_cluster"
  )
  expect_equal(nrow(cluster), 1)
  # The candidate episode this run was (re)detected on, not a stale one
  # from a fortnight ago.
  expect_equal(cluster$expected[1], 4)
  expect_equal(cluster$excess[1], 13)
  expect_equal(cluster$ratio[1], 5)
})

test_that("the effect-size floor keeps a statistically-true-but-small signal out of the queue", {
  # config$effect_size_floor was documented as "a signal must clear both
  # before it becomes a cluster" and read by nothing at all.
  metrics <- list(expected = 2, excess = 2, ratio = 2)
  expect_false(episodic_reconcile_clears_floor(metrics, 3, 1.5))
  expect_true(episodic_reconcile_clears_floor(metrics, 2, 1.5))

  # both thresholds have to be cleared, not either
  expect_false(episodic_reconcile_clears_floor(
    list(expected = 10, excess = 5, ratio = 1.2),
    3,
    1.5
  ))
})

test_that("a candidate with no effect size to measure is not floored out", {
  # same_place and rare_trigger carry no expected or upperbound, and a
  # Farrington week with expected 0 has no ratio. A floor that rejected
  # those would silence the detectors that need no baseline at all.
  bare <- list(expected = NA_real_, excess = NA_real_, ratio = NA_real_)
  expect_true(episodic_reconcile_clears_floor(bare, 3, 1.5))
  expect_true(episodic_reconcile_clears_floor(
    list(expected = 0, excess = 4, ratio = NA_real_),
    3,
    1.5
  ))
  # and no floor configured is no floor
  expect_true(episodic_reconcile_clears_floor(
    list(expected = 2, excess = 0, ratio = 1),
    NA,
    NA
  ))
})

test_that("a weak candidate does not open a cluster, but does extend one already open", {
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  # A Farrington week is only ever as strong as its distance from the
  # model's own upperbound, so a detection carries both.
  detect <- function(run_id, first_day, last_day, n_cases) {
    det <- reconcile_detect(
      env,
      run_id,
      first_day,
      last_day,
      n_cases,
      detector = "farrington"
    )
    det$expected <- 10
    det$upperbound <- 12
    det
  }
  reconcile <- function(run_id, det) {
    episodic_reconcile_stream(
      env$con,
      env$stream_id,
      det,
      case_free_days = 14,
      run_id = run_id,
      close_after_runs = 14,
      priority_score_fn = noop_priority_score,
      has_assessment_fn = noop_has_assessment,
      verdict_fn = noop_verdict,
      min_excess_over_upperbound = 3,
      min_ratio_observed_expected = 1.5
    )
  }
  clusters <- function() {
    episodic_db_clusters_for_stream(env$con, env$stream_id)
  }

  # 13 observed against an upperbound of 12 is a true alarm and a
  # one-case excess: not a dossier.
  run1 <- episodic_db_run_start(env$con, "h", "a")
  reconcile(run1, detect(run1, "2025-01-06", "2025-01-12", 13))
  expect_equal(nrow(clusters()), 0)

  # 30 against the same upperbound clears both thresholds and opens one
  run2 <- episodic_db_run_start(env$con, "h", "a")
  reconcile(run2, detect(run2, "2025-01-13", "2025-01-19", 30))
  expect_equal(nrow(clusters()), 1)

  # and the next week, weak again, still belongs to the outbreak already
  # open: the floor gates opening a dossier, not continuing one
  run3 <- episodic_db_run_start(env$con, "h", "a")
  reconcile(run3, detect(run3, "2025-01-20", "2025-01-26", 13))
  expect_equal(nrow(clusters()), 1)
  expect_equal(clusters()$last_day, "2025-01-26")
})

test_that("consecutive alarming weeks are one outbreak, not a new dossier every fortnight", {
  # Reconciliation matched each candidate against the clusters the run
  # started with, so a cluster went on advertising its original last day.
  # Once that was case_free_days behind, the next week of the same
  # outbreak stopped matching and opened a second dossier - and a third,
  # every 21 days for as long as the outbreak lasted. Unreachable while a
  # run produced one detection per stream; a run that tests several weeks
  # walks straight into it.
  env <- reconcile_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run_id <- episodic_db_run_start(env$con, "h", "a")
  weeks <- seq(as.Date("2026-07-06"), by = "week", length.out = 6)
  detections <- do.call(
    rbind,
    lapply(seq_along(weeks), function(i) {
      det <- reconcile_detect(
        env,
        run_id,
        as.character(weeks[i]),
        as.character(weeks[i] + 6),
        5,
        detector = "farrington"
      )
      det$expected <- 2
      det$upperbound <- 4
      det
    })
  )

  episodic_reconcile_stream(
    env$con,
    env$stream_id,
    detections,
    case_free_days = 14,
    run_id = run_id,
    close_after_runs = 14,
    priority_score_fn = noop_priority_score,
    has_assessment_fn = noop_has_assessment,
    verdict_fn = noop_verdict
  )

  clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$first_day, as.character(weeks[1]))
  expect_equal(clusters$last_day, as.character(weeks[6] + 6))
})
