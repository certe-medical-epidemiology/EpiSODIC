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

# Schema version 9 drops three assessment columns nothing reads, and only
# where they hold no data: a column with a value on any row is kept.

v9_columns <- c("wpg_notifiable", "ggd_informed", "ggd_note")

v9_has <- function(con, column) {
  episodic_db_column_exists(con, "sqlite", "episodic_assessment_event", column)
}

# A cluster to hang an assessment on.
v9_cluster <- function(con) {
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_ward",
      "Norovirus",
      institution_id = "i1",
      ward = "w1"
    ),
    level = "pathogen_ward",
    pathogen = "Norovirus",
    ward = "w1",
    observed_date = "2025-01-01"
  )
  episodic_db_cluster_insert(
    con,
    stream_id = stream_id,
    first_day = "2025-01-01",
    last_day = "2025-01-10",
    n_cases = 3L,
    priority_score = 0.5,
    detector_agreement = 1L,
    run_id = episodic_db_run_start(con, "host", "account")
  )
}

# A version-8 database with one cluster and one assessment on file.
v9_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  schema_v8_assessment_columns(con)
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 8 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  cluster_id <- v9_cluster(con)
  user_id <- episodic_db_app_user_insert(
    con, "jdoe", "Jane Doe", "j@x.nl", "not-a-real-hash"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_assessment_event
       (cluster_id, user_id, created_at, verdict, rationale)
     VALUES (?, ?, '2025-01-01T00:00:00Z', 'artefact', 'kept')",
    params = list(cluster_id, user_id)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("a fresh database has none of the three columns", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  for (column in v9_columns) {
    expect_false(v9_has(con, column), info = column)
  }
})

test_that("migration 9 drops the three columns when they hold no data, and keeps every row", {
  path <- v9_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))

  expect_message(
    episodic_db_migrate(path),
    "Dropped episodic_assessment_event.wpg_notifiable"
  )

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  for (column in v9_columns) {
    expect_false(v9_has(con, column), info = column)
  }
  kept <- DBI::dbGetQuery(
    con,
    "SELECT verdict, rationale FROM episodic_assessment_event"
  )
  expect_identical(kept$verdict, "artefact")
  expect_identical(kept$rationale, "kept")
})

test_that("migration 9 keeps a column that holds a value on any row, and says so", {
  path <- v9_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "UPDATE episodic_assessment_event SET ggd_note = 'called'")
  DBI::dbDisconnect(con)

  messages <- character(0)
  withCallingHandlers(
    episodic_db_migrate(path),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl(
    "Kept episodic_assessment_event.ggd_note: it holds a value on 1 row",
    messages,
    fixed = TRUE
  )))

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_true(v9_has(con, "ggd_note"))
  expect_false(v9_has(con, "wpg_notifiable"))
  expect_false(v9_has(con, "ggd_informed"))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT ggd_note FROM episodic_assessment_event")$ggd_note,
    "called"
  )
  expect_identical(episodic_db_schema_version(con), episodic_schema_version)
})

test_that("migration 9 is idempotent and silent where the columns are already gone", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  migration <- episodic_db_migrations()[["9"]]
  expect_identical(migration(con, "sqlite"), character(0))
  expect_identical(migration(con, "sqlite"), character(0))
})

test_that("an assessment is recorded without the dropped columns", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  cluster_id <- v9_cluster(con)
  user_id <- episodic_db_app_user_insert(
    con, "jdoe", "Jane Doe", "j@x.nl", "not-a-real-hash"
  )
  event_id <- episodic_db_assessment_event_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    verdict = "artefact",
    rationale = "none"
  )
  expect_true(is.numeric(event_id) && event_id > 0)
})
