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

test_that("episode_app_submit_assessment() records a cluster_state transition when state actually changes", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")

  # new -> assessing (a rationale-only note, no verdict yet)
  episode_app_submit_assessment(env$con, env$cluster_id, user_id, rationale = "looking into it")
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "assessing")

  states <- episode_db_cluster_states(env$con, env$cluster_id)
  expect_equal(nrow(states), 1)
  expect_equal(states$state[1], "assessing")
  expect_equal(states$trigger[1], "assessment")
  expect_equal(states$user_id[1], user_id)
})

test_that("episode_app_submit_assessment() does not write a cluster_state row when the state does not change", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")

  episode_app_submit_assessment(env$con, env$cluster_id, user_id, rationale = "first note")
  expect_equal(nrow(episode_db_cluster_states(env$con, env$cluster_id)), 1)

  # a second rationale-only note: state stays "assessing" both times
  episode_app_submit_assessment(env$con, env$cluster_id, user_id, rationale = "second note")
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "assessing")
  expect_equal(nrow(episode_db_cluster_states(env$con, env$cluster_id)), 1)  # unchanged
})

test_that("episode_app_submit_assessment() with a terminal verdict transitions straight to closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")

  event_id <- episode_app_submit_assessment(env$con, env$cluster_id, user_id,
                                             verdict = "artefact", rationale = "detector artefact")
  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")

  states <- episode_db_cluster_states(env$con, env$cluster_id)
  expect_equal(states$state[1], "closed")
  expect_equal(states$event_id[1], event_id)
})

test_that("episode_app_submit_closure() closes a non-terminal classification without a new assessment event", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  episode_app_submit_assessment(env$con, env$cluster_id, user_id,
                                 verdict = "possible_epidemic", rationale = "watching")
  n_events_before <- nrow(episode_db_assessment_events(env$con, env$cluster_id))

  episode_app_submit_closure(env$con, env$cluster_id, user_id)

  expect_equal(episode_app_derive_state_for_cluster(env$con, env$cluster_id), "closed")
  expect_equal(nrow(episode_db_assessment_events(env$con, env$cluster_id)), n_events_before)  # no new event
  states <- episode_db_cluster_states(env$con, env$cluster_id)
  expect_equal(states$trigger[nrow(states)], "closure")
  expect_true(is.na(states$event_id[nrow(states)]))
})

test_that("episode_app_submit_assessment() rejects a blank rationale", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "tester", "Test User", "t@example.com", "hash")
  expect_error(episode_app_submit_assessment(env$con, env$cluster_id, user_id, rationale = ""))
})
