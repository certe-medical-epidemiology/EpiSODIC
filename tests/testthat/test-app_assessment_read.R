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

test_that("episodic_app_assessment_timeline() combines assessment events and closures, chronologically", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    "hash"
  )

  expect_equal(
    nrow(episodic_app_assessment_timeline(env$con, env$cluster_id)),
    0
  )

  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id,
    verdict = "possible_epidemic",
    rationale = "watching this"
  )
  Sys.sleep(1.1)
  episodic_app_submit_closure(env$con, env$cluster_id, user_id)

  timeline <- episodic_app_assessment_timeline(
    env$con,
    env$cluster_id,
    lang = "nl"
  )
  expect_equal(nrow(timeline), 2)
  expect_equal(timeline$kind, c("assessment", "closure"))
  expect_true(all(timeline$actor == "Test User"))
  expect_equal(timeline$verdict_label[1], "Mogelijke epidemie")
  expect_true(is.na(timeline$verdict_label[2]))
})

test_that("episodic_app_assessment_timeline() labels a system actor (NA user_id) distinctly", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_cluster_state_insert(
    env$con,
    cluster_id = env$cluster_id,
    state = "closed",
    trigger = "closure"
  )

  timeline <- episodic_app_assessment_timeline(
    env$con,
    env$cluster_id,
    lang = "nl"
  )
  expect_equal(timeline$actor[1], "Systeem")
})

test_that("episodic_app_archive() lists only closed clusters, most recent first, and supports search", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_equal(nrow(episodic_app_archive(env$con)), 0) # nothing closed yet

  user_id <- episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    "hash"
  )
  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id,
    verdict = "artefact",
    rationale = "false alarm"
  )

  archive <- episodic_app_archive(env$con)
  expect_equal(nrow(archive), 1)
  expect_equal(archive$pathogen[1], "Norovirus")
  expect_false(is.na(archive$closed_at[1]))

  expect_equal(nrow(episodic_app_archive(env$con, query = "noro")), 1)
  expect_equal(nrow(episodic_app_archive(env$con, query = "influenza")), 0)
})

test_that("episodic_app_activity_log() surfaces assessments, closures, mutes, logins and runs, most recent first", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    sodium::password_store("pw12345")
  )

  episodic_auth_login(env$con, "tester", "pw12345")
  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id,
    verdict = "cluster_not_yet",
    rationale = "watching"
  )
  episodic_db_stream_mute_insert(
    env$con,
    stream_id = env$stream_id,
    muted_from = "2025-01-01",
    muted_until = "2025-02-01",
    reason = "seasonal",
    user_id = user_id
  )

  # A second run with a different status: episodic_tr() is a scalar helper,
  # so building the "action" column from a vectorised runs$status (rather
  # than vapply()-ing episodic_tr() over it) throws "the condition has
  # length > 1" as soon as more than one run exists with differing statuses.
  second_run <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(env$con, second_run, status = "failed")

  activity <- episodic_app_activity_log(env$con, lang = "nl")
  expect_true("aangemeld" %in% activity$action) # login
  expect_true("geclassificeerd" %in% activity$action) # assessment
  expect_true("signaleringsreeks gedempt" %in% activity$action) # mute
  expect_true(any(startsWith(activity$action, "detectierun"))) # the cron run from app_read_setup()
  expect_equal(sum(startsWith(activity$action, "detectierun")), 2) # both runs
  expect_true(any(activity$is_system)) # the cron run from app_read_setup()
  expect_false(all(activity$is_system)) # human actions too
  expect_true(all(diff(as.numeric(as.POSIXct(activity$at, tz = "UTC"))) <= 0)) # descending
})

test_that("episodic_app_activity_log() carries a load summary on run rows and nothing on human rows", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    sodium::password_store("pw12345")
  )
  episodic_auth_login(env$con, "tester", "pw12345")

  run_id <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(
    env$con,
    run_id,
    status = "success",
    n_cases_supplied = 400,
    n_cases_deduplicated = 350,
    n_cases_inserted = 120
  )

  activity <- episodic_app_activity_log(env$con, lang = "en")
  detail <- activity$detail[!is.na(activity$detail)]
  expect_true(any(grepl("120", detail, fixed = TRUE)))
  expect_true(any(grepl("400", detail, fixed = TRUE)))
  expect_true(is.na(activity$detail[activity$action == "signed in"]))
})

test_that("a partial run reads as skipped rows in the activity log, and names the count", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run_id <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(
    env$con,
    run_id,
    status = "partial",
    n_cases_supplied = 400,
    n_cases_deduplicated = 350,
    n_cases_inserted = 120,
    n_activity_supplied = 30,
    n_activity_written = 18,
    n_activity_skipped = 12
  )

  activity <- episodic_app_activity_log(env$con, lang = "en")
  partial <- activity[grepl("skipped", activity$action, fixed = TRUE), ]
  expect_equal(nrow(partial), 1)
  expect_match(partial$detail[1], "12")
  # every status the schema allows must have a translation, not [[key]]
  expect_false(any(grepl("[[", activity$action, fixed = TRUE)))
})

test_that("a run recorded before the load counters existed shows no summary rather than blanks", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run_id <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(env$con, run_id, status = "success") # counters left NA

  activity <- episodic_app_activity_log(env$con, lang = "en")
  runs <- activity[activity$is_system, ]
  expect_true(any(is.na(runs$detail)))
})

test_that("run rows carry their run_id so the screen can offer the run detail, human rows do not", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    sodium::password_store("pw12345")
  )
  episodic_auth_login(env$con, "tester", "pw12345")

  run_id <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(env$con, run_id, status = "success")

  activity <- episodic_app_activity_log(env$con, lang = "en")
  expect_true(run_id %in% activity$run_id[activity$is_system])
  expect_true(all(is.na(activity$run_id[!activity$is_system])))
})

test_that("a failed run shows its first line in the log, not the whole recorded message", {
  # The message names every offending column; the table has room for its
  # summary line, and the run detail has room for the rest.
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  run_id <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(
    env$con,
    run_id,
    status = "failed",
    error_text = paste0(
      "Case data cannot be used by EpiSODIC: 2 problems.\n",
      "  1. `sex` has 300 of 300 rows with a value outside the allowed set."
    )
  )

  activity <- episodic_app_activity_log(env$con, lang = "en")
  detail <- activity$detail[activity$is_system & !is.na(activity$detail)]
  expect_equal(detail, "Case data cannot be used by EpiSODIC: 2 problems.")
})

test_that("episodic_ui_run_modal() shows a failed run's whole message, and how to act on it", {
  run <- data.frame(
    run_id = 12L,
    host = "srv-01",
    account = "cron",
    started_at = "2025-06-01T08:00:00Z",
    finished_at = "2025-06-01T08:01:00Z",
    status = "failed",
    n_streams = NA_integer_,
    n_cases_supplied = NA_integer_,
    code_version = "0.5.0",
    config_hash = strrep("a", 40),
    error_text = paste0(
      "Case data cannot be used by EpiSODIC: 2 problems.\n",
      "  1. `sex` has 300 of 300 rows with a value outside the allowed set."
    ),
    stringsAsFactors = FALSE
  )
  rendered <- as.character(episodic_ui_run_modal(run, lang = "en"))

  expect_true(grepl("300 of 300 rows", rendered, fixed = TRUE))
  expect_true(grepl("episodic_check_cases()", rendered, fixed = TRUE))
  expect_true(grepl("srv-01", rendered, fixed = TRUE))
  expect_true(grepl("0.5.0", rendered, fixed = TRUE))
  # a run that never loaded anything says so, rather than showing zeroes
  expect_true(grepl("stopped before it read any data", rendered, fixed = TRUE))
  expect_false(grepl("[[", rendered, fixed = TRUE))
})

test_that("episodic_ui_run_modal() shows what a successful run loaded, and no failure section", {
  run <- data.frame(
    run_id = 13L,
    host = "srv-01",
    account = "cron",
    started_at = "2025-06-01T08:00:00Z",
    finished_at = "2025-06-01T08:01:00Z",
    status = "success",
    n_streams = 42L,
    n_detections = 3L,
    n_signals_new = 1L,
    n_signals_updated = 2L,
    n_cases_supplied = 400L,
    n_cases_deduplicated = 350L,
    n_cases_inserted = 120L,
    n_activity_skipped = NA_integer_,
    code_version = "0.5.0",
    config_hash = strrep("a", 40),
    error_text = NA_character_,
    stringsAsFactors = FALSE
  )
  rendered <- as.character(episodic_ui_run_modal(run, lang = "en"))

  expect_true(grepl("120", rendered, fixed = TRUE))
  expect_true(grepl("42", rendered, fixed = TRUE))
  expect_false(grepl("Why this run failed", rendered, fixed = TRUE))
  expect_false(grepl("episodic_check_cases()", rendered, fixed = TRUE))
  expect_false(grepl("[[", rendered, fixed = TRUE))
})
