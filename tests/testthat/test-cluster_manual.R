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

test_that("episodic_add_manual_cluster() creates a single cluster with origin = 'manual'", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  user_id <- episodic_db_app_user_insert(
    con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )
  DBI::dbDisconnect(con)

  ids <- episodic_add_manual_cluster(
    db_path = db_path,
    user_id = user_id,
    pathogen = "Measles virus",
    level = "pathogen_area",
    first_day = "2025-01-10",
    last_day = "2025-01-20",
    region_code = "GR",
    case_dates = list(as.Date(c("2025-01-10", "2025-01-14", "2025-01-20"))),
    pc = list(c("9711AA", "9711AB", "9712CD")),
    note = "Reported by municipal health service."
  )
  expect_length(ids, 1L)

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  cluster <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(ids[1])
  )
  expect_equal(nrow(cluster), 1L)
  expect_equal(cluster$origin[1], "manual")
  expect_true(is.na(cluster$last_detected_run[1]))
  expect_equal(cluster$n_cases[1], 3L)

  manual_cases <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster_manual_case WHERE cluster_id = ?",
    params = list(ids[1])
  )
  expect_equal(nrow(manual_cases), 3L)
  expect_equal(sort(manual_cases$pc), sort(c("9711AA", "9711AB", "9712CD")))

  # Never touches the real case ledger.
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM episodic_case")), 0L)
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM episodic_cluster_case")), 0L)

  note <- episodic_db_cluster_note_current(con, ids[1])
  expect_equal(nrow(note), 1L)
  expect_equal(note$note_text[1], "Reported by municipal health service.")

  stream <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_stream WHERE stream_id = ?",
    params = list(cluster$stream_id[1])
  )
  expect_equal(stream$pathogen[1], "Measles virus")
  expect_equal(stream$region_code[1], "GR")
})

test_that("episodic_add_manual_cluster() is vectorised: one call creates several clusters", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  user_id <- episodic_db_app_user_insert(
    con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )
  DBI::dbDisconnect(con)

  ids <- episodic_add_manual_cluster(
    db_path = db_path,
    user_id = user_id,
    pathogen = c("Measles virus", "Mumps virus"),
    level = "pathogen_area",
    first_day = c("2025-01-10", "2025-02-01"),
    last_day = c("2025-01-20", "2025-02-10"),
    n_cases = c(4L, 2L),
    region_code = c("GR", "FR")
  )
  expect_length(ids, 2L)
  expect_equal(length(unique(ids)), 2L)

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  clusters <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM episodic_cluster WHERE cluster_id IN (%s) ORDER BY cluster_id",
      paste(ids, collapse = ",")
    )
  )
  expect_equal(nrow(clusters), 2L)
  expect_equal(clusters$n_cases, c(4L, 2L))
  expect_true(all(clusters$origin == "manual"))
})

test_that("episodic_add_manual_cluster() validates before writing anything, and rolls back on error", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  user_id <- episodic_db_app_user_insert(
    con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )
  DBI::dbDisconnect(con)

  # Mismatched per-cluster case-level detail lengths.
  expect_error(
    episodic_add_manual_cluster(
      db_path = db_path,
      user_id = user_id,
      pathogen = c("Measles virus", "Mumps virus"),
      level = "pathogen_area",
      first_day = c("2025-01-10", "2025-02-01"),
      last_day = c("2025-01-20", "2025-02-10"),
      case_dates = list(as.Date("2025-01-10"))
    ),
    "length"
  )

  # Invalid level.
  expect_error(
    episodic_add_manual_cluster(
      db_path = db_path,
      user_id = user_id,
      pathogen = "Measles virus",
      level = "pathogen_country",
      first_day = "2025-01-10",
      last_day = "2025-01-20",
      n_cases = 3L
    ),
    "level"
  )

  # Neither n_cases nor case-level detail.
  expect_error(
    episodic_add_manual_cluster(
      db_path = db_path,
      user_id = user_id,
      pathogen = "Measles virus",
      level = "pathogen_area",
      first_day = "2025-01-10",
      last_day = "2025-01-20"
    ),
    "n_cases"
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM episodic_cluster")), 0L)
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM episodic_stream")), 0L)
})

test_that("a manual cluster is excluded from episodic_reconcile_stream()'s matching and closure", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )

  # Fetch the stream identity app_read_setup() already built, so the
  # manual cluster below lands on exactly the same (actively-monitored)
  # stream_id.
  stream <- DBI::dbGetQuery(
    env$con,
    "SELECT * FROM episodic_stream WHERE stream_id = ?",
    params = list(env$stream_id)
  )
  manual_cluster_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$stream_id,
    first_day = "2024-12-01",
    last_day = "2024-12-05",
    n_cases = 2,
    priority_score = 10,
    detector_agreement = 1,
    run_id = NA,
    origin = "manual"
  )

  open_clusters <- episodic_db_clusters_for_stream(env$con, env$stream_id)
  expect_false(manual_cluster_id %in% open_clusters$cluster_id)

  # A fresh reconciliation run against the same stream must leave the
  # manual cluster completely untouched: no runs_since_detected bump, no
  # state transition, no change to last_detected_run.
  before <- DBI::dbGetQuery(
    env$con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(manual_cluster_id)
  )

  run_id <- episodic_db_run_start(env$con, "host", "account")
  no_detections <- episodic_detection_record(
    env$stream_id,
    "same_place",
    character(0),
    character(0),
    integer(0)
  )
  episodic_reconcile_stream(
    env$con,
    env$stream_id,
    no_detections,
    run_id = run_id,
    close_after_runs = 14,
    priority_score_fn = function(candidate) 0,
    has_assessment_fn = function(cluster_id) FALSE,
    verdict_fn = function(cluster_id) NA_character_
  )

  after <- DBI::dbGetQuery(
    env$con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(manual_cluster_id)
  )
  expect_equal(before, after)

  states <- DBI::dbGetQuery(
    env$con,
    "SELECT * FROM episodic_cluster_state WHERE cluster_id = ?",
    params = list(manual_cluster_id)
  )
  expect_equal(nrow(states), 0L)
})

test_that("episodic_ui_dossier() renders a manual cluster with the origin badge", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  user_id <- episodic_db_app_user_insert(
    con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )
  DBI::dbDisconnect(con)

  ids <- episodic_add_manual_cluster(
    db_path = db_path,
    user_id = user_id,
    pathogen = "Measles virus",
    level = "pathogen_area",
    first_day = "2025-01-10",
    last_day = "2025-01-20",
    region_code = "GR",
    case_dates = list(as.Date(c("2025-01-10", "2025-01-14", "2025-01-20"))),
    pc = list(c("9711AA", "9711AB", "9712CD"))
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  html_nl <- as.character(episodic_ui_dossier(con, ids[1], lang = "nl"))
  html_en <- as.character(episodic_ui_dossier(con, ids[1], lang = "en"))
  expect_true(grepl(episodic_tr("dossier.manual_badge", lang = "nl"), html_nl, fixed = TRUE))
  expect_true(grepl(episodic_tr("dossier.manual_badge", lang = "en"), html_en, fixed = TRUE))
})
