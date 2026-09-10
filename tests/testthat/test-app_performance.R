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

test_that("episodic_app_performance() returns empty-but-valid shapes on a fresh instance", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  perf <- episodic_app_performance(env$con)
  expect_equal(nrow(perf$classification_distribution), 0)
  expect_equal(perf$timeliness$to_first_assessment$n, 0)
  expect_true(is.na(perf$timeliness$to_first_assessment$median_days))

  # one detection exists (same_place, from app_read_setup()) but no
  # verdict yet, so it contributes to n_detections but not to PPV
  expect_equal(nrow(perf$by_detector_pathogen), 1)
  expect_equal(perf$by_detector_pathogen$detector, "same_place")
  expect_equal(perf$by_detector_pathogen$pathogen, "Norovirus")
  expect_equal(perf$by_detector_pathogen$n_detections, 1)
  expect_equal(perf$by_detector_pathogen$n_true_positive, 0)
  expect_equal(perf$by_detector_pathogen$n_false_positive, 0)
  expect_true(is.na(perf$by_detector_pathogen$ppv))
})

test_that("episodic_app_performance() counts a terminal verdict as true/false positive for every detector that flagged the cluster", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
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
    verdict = "confirmed_epidemic",
    rationale = "real outbreak"
  )

  perf <- episodic_app_performance(env$con)
  expect_equal(perf$by_detector_pathogen$n_true_positive, 1)
  expect_equal(perf$by_detector_pathogen$n_false_positive, 0)
  expect_equal(perf$by_detector_pathogen$ppv, 1)

  expect_equal(nrow(perf$classification_distribution), 1)
  expect_equal(perf$classification_distribution$verdict, "confirmed_epidemic")
  expect_equal(perf$classification_distribution$n, 1)

  expect_equal(perf$timeliness$to_first_assessment$n, 1)
  expect_false(is.na(perf$timeliness$to_first_assessment$median_days))
  expect_equal(perf$timeliness$to_classification$n, 1)
})

test_that("episodic_app_performance() excludes cluster_not_yet and unassessed clusters from PPV", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
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
    verdict = "cluster_not_yet",
    rationale = "still watching"
  )

  perf <- episodic_app_performance(env$con)
  expect_equal(perf$by_detector_pathogen$n_true_positive, 0)
  expect_equal(perf$by_detector_pathogen$n_false_positive, 0)
  expect_true(is.na(perf$by_detector_pathogen$ppv))
  expect_equal(perf$classification_distribution$verdict, "cluster_not_yet")
})

test_that("episodic_app_performance() only counts a cluster's latest verdict, not every historical one", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
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
    rationale = "looked like noise at first"
  )
  Sys.sleep(1.1)
  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id,
    verdict = "confirmed_epidemic",
    rationale = "turned out real"
  )

  perf <- episodic_app_performance(env$con)
  expect_equal(nrow(perf$classification_distribution), 1)
  expect_equal(perf$classification_distribution$verdict, "confirmed_epidemic")
  expect_equal(perf$by_detector_pathogen$n_true_positive, 1)
  expect_equal(perf$by_detector_pathogen$n_false_positive, 0)
})

test_that("time-to-detection excludes a cluster the backfill opened, and the rest do not", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  live <- episodic_app_performance(env$con)$timeliness
  expect_equal(live$to_detection$n, 1)

  # A cluster opened by a backfill run was opened on the day the archive
  # was imported, so `opened_at - first_day` is the age of the extract.
  # Left in, one three-year-old import moves a median that is supposed to
  # describe how quickly this instance notices things.
  DBI::dbExecute(
    env$con,
    "UPDATE episodic_cluster SET opened_in_backfill = 1 WHERE cluster_id = ?",
    params = list(env$cluster_id)
  )
  backfilled <- episodic_app_performance(env$con)$timeliness
  expect_equal(backfilled$to_detection$n, 0)
  expect_true(is.na(backfilled$to_detection$median_days))

  # The two measured from `opened_at` forwards time an epidemiologist
  # rather than a detector, so they are as honest here as anywhere and
  # are not excluded.
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
    verdict = "confirmed_epidemic",
    rationale = "real outbreak"
  )
  assessed <- episodic_app_performance(env$con)
  expect_equal(assessed$timeliness$to_first_assessment$n, 1)
  expect_equal(assessed$timeliness$to_classification$n, 1)
  # And PPV counts it: a backfilled cluster somebody actually judged is
  # evidence about the detectors like any other.
  expect_equal(assessed$by_detector_pathogen$n_true_positive, 1)
})

test_that("episodic_ui_performance_screen() renders without error, empty and populated", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  empty_ui <- episodic_ui_performance_screen(episodic_app_performance(env$con))
  expect_s3_class(empty_ui, "shiny.tag")

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
    verdict = "confirmed_epidemic",
    rationale = "real outbreak"
  )
  filled_ui <- episodic_ui_performance_screen(episodic_app_performance(env$con))
  rendered <- paste(as.character(filled_ui), collapse = "\n")
  expect_true(grepl("same_place", rendered, fixed = TRUE))
  expect_true(grepl("100%", rendered, fixed = TRUE))
})
