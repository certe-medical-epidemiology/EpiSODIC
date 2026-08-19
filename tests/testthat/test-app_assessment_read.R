test_that("episode_app_assessment_timeline() combines assessment events and closures, chronologically", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")

  expect_equal(nrow(episode_app_assessment_timeline(env$con, env$cluster_id)), 0)

  episode_app_submit_assessment(env$con, env$cluster_id, user_id,
                                 verdict = "possible_epidemic", rationale = "watching this")
  Sys.sleep(1.1)
  episode_app_submit_closure(env$con, env$cluster_id, user_id)

  timeline <- episode_app_assessment_timeline(env$con, env$cluster_id)
  expect_equal(nrow(timeline), 2)
  expect_equal(timeline$kind, c("assessment", "closure"))
  expect_true(all(timeline$actor == "Test User"))
  expect_equal(timeline$verdict_label[1], "Mogelijke epidemie")
  expect_true(is.na(timeline$verdict_label[2]))
})

test_that("episode_app_assessment_timeline() labels a system actor (NA user_id) distinctly", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episode_db_cluster_state_insert(env$con, cluster_id = env$cluster_id, state = "closed", trigger = "closure")

  timeline <- episode_app_assessment_timeline(env$con, env$cluster_id)
  expect_equal(timeline$actor[1], "Systeem")
})

test_that("episode_app_archive() lists only closed clusters, most recent first, and supports search", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_equal(nrow(episode_app_archive(env$con)), 0)  # nothing closed yet

  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episode_app_submit_assessment(env$con, env$cluster_id, user_id,
                                 verdict = "artefact", rationale = "false alarm")

  archive <- episode_app_archive(env$con)
  expect_equal(nrow(archive), 1)
  expect_equal(archive$pathogen[1], "Norovirus")
  expect_false(is.na(archive$closed_at[1]))

  expect_equal(nrow(episode_app_archive(env$con, query = "noro")), 1)
  expect_equal(nrow(episode_app_archive(env$con, query = "influenza")), 0)
})

test_that("episode_app_activity_log() surfaces assessments, closures, mutes, logins and runs, most recent first", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com",
                                         sodium::password_store("pw12345"))

  episode_auth_login(env$con, "tester", "pw12345")
  episode_app_submit_assessment(env$con, env$cluster_id, user_id,
                                 verdict = "cluster_not_yet", rationale = "watching")
  episode_db_stream_mute_insert(env$con, stream_id = env$stream_id, muted_from = "2025-01-01",
                                 muted_until = "2025-02-01", reason = "seasonal", user_id = user_id)

  activity <- episode_app_activity_log(env$con)
  expect_true("aangemeld" %in% activity$action)          # login
  expect_true("geclassificeerd" %in% activity$action)    # assessment
  expect_true("stream gedempt" %in% activity$action)     # mute
  expect_true(any(startsWith(activity$action, "detectierun")))  # the cron run from app_read_setup()
  expect_true(any(activity$is_system))       # the cron run from app_read_setup()
  expect_false(all(activity$is_system))      # human actions too
  expect_true(all(diff(as.numeric(as.POSIXct(activity$at, tz = "UTC"))) <= 0))  # descending
})
