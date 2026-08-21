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

test_that("episode_baseline_excluded_windows() returns the window of a confirmed_epidemic cluster and nothing else", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  windows <- episode_baseline_excluded_windows(env$con, env$stream_id)
  expect_equal(nrow(windows), 0)  # fixture cluster has no confirmed_epidemic verdict yet

  user_id <- episode_db_app_user_insert(env$con, "jdoe", "Jane Doe", "j@x.nl", "hash")
  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id, verdict = "confirmed_epidemic",
                                      rationale = "test")

  windows <- episode_baseline_excluded_windows(env$con, env$stream_id)
  expect_equal(nrow(windows), 1)
  cluster <- DBI::dbGetQuery(env$con, "SELECT first_day, last_day FROM episode_cluster WHERE cluster_id = ?",
                              params = list(env$cluster_id))
  expect_equal(windows$first_day, cluster$first_day)
  expect_equal(windows$last_day, cluster$last_day)
})

test_that("a later verdict supersedes an earlier confirmed_epidemic one - only the latest counts", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episode_db_app_user_insert(env$con, "jdoe", "Jane Doe", "j@x.nl", "hash")

  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id, verdict = "confirmed_epidemic", rationale = "t1")
  episode_db_assessment_event_insert(env$con, env$cluster_id, user_id, verdict = "expected_variation", rationale = "t2")

  windows <- episode_baseline_excluded_windows(env$con, env$stream_id)
  expect_equal(nrow(windows), 0)  # latest verdict is no longer confirmed_epidemic
})

test_that("episode_baseline_exclude_cases() removes only cases inside an excluded window", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:9))
  windows <- data.frame(first_day = "2025-01-03", last_day = "2025-01-05", stringsAsFactors = FALSE)

  filtered <- episode_baseline_exclude_cases(cases, windows)
  expect_equal(nrow(filtered), 7)
  expect_false(any(as.Date(filtered$sample_date) >= as.Date("2025-01-03") & as.Date(filtered$sample_date) <= as.Date("2025-01-05")))
})

test_that("episode_baseline_exclude_cases() is a no-op with zero excluded windows", {
  cases <- data.frame(sample_date = as.character(as.Date("2025-01-01") + 0:9))
  empty_windows <- data.frame(first_day = character(0), last_day = character(0), stringsAsFactors = FALSE)
  expect_equal(nrow(episode_baseline_exclude_cases(cases, empty_windows)), 10)
})
