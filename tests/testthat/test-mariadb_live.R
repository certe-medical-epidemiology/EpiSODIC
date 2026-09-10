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

# The second dialect, against a server that has an opinion about it.
#
# `episodic_db_schema_statements("mariadb")` rewrites the SQLite schema
# into MySQL-safe DDL with a hand-maintained per-table find-and-replace
# table. test-schema_mariadb.R checks the strings it produces; nothing
# checked that a real server would accept them, and the first time one
# was asked it refused the very first statement.
#
# Everything here skips unless EPISODIC_TEST_MARIADB_DSN names a
# MariaDB/MySQL database this suite may create and drop tables in, so a
# laptop with no MariaDB stays green. CI sets it (see
# .github/workflows/mariadb.yaml). Point it at a scratch database and
# nothing else: every test below starts by dropping every EpiSODIC table
# it finds.

mariadb_dsn <- function() {
  skip_if_not_installed("RMariaDB")
  dsn <- Sys.getenv("EPISODIC_TEST_MARIADB_DSN", unset = "")
  skip_if(
    !nzchar(dsn),
    paste(
      "EPISODIC_TEST_MARIADB_DSN is not set. Set it to a mysql:// DSN for a",
      "scratch database to run the live MariaDB tests."
    )
  )
  dsn
}

mariadb_fresh <- function() {
  dsn <- mariadb_dsn()
  DBI::dbDisconnect(episodic_db_create(dsn, overwrite = TRUE))
  dsn
}

mariadb_cases <- function(end_date = as.Date("2025-06-29")) {
  episodic_synthetic_cases(
    start_date = end_date - 400,
    end_date = end_date,
    seed = 3
  )
}

test_that("the generated DDL is accepted by a real server", {
  dsn <- mariadb_dsn()
  con <- episodic_db_create(dsn, overwrite = TRUE)
  on.exit(DBI::dbDisconnect(con))

  expect_setequal(
    intersect(DBI::dbListTables(con), episodic_db_schema_tables()),
    episodic_db_schema_tables()
  )
  # The schema file is written in the order the tables read best, which
  # is not a topological order of their foreign keys: episodic_stream
  # references episodic_institution long before that table is declared.
  # SQLite does not mind; MariaDB refuses with errno 150 unless the
  # constraints are declared with checking off. They are still
  # constraints, so they are still enforced afterwards.
  expect_equal(
    DBI::dbGetQuery(con, "SELECT @@FOREIGN_KEY_CHECKS AS on_")$on_,
    1
  )
  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_cluster_case (cluster_id, case_id) VALUES (99, 99)"
    ),
    "foreign key",
    ignore.case = TRUE
  )
})

test_that("every reference in the schema exists as a constraint on the server", {
  dsn <- mariadb_fresh()
  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con))

  # The count, not merely "some": MySQL discards inline column-level
  # references silently, so a schema that lost half its constraints looks
  # exactly like one that kept them until something writes an orphan.
  declared <- sum(vapply(
    strsplit(episodic_db_schema_statements("mariadb"), "\n", fixed = TRUE),
    function(lines) sum(grepl("^\\s*FOREIGN KEY \\(", lines, perl = TRUE)),
    integer(1)
  ))
  created <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM information_schema.REFERENTIAL_CONSTRAINTS
      WHERE CONSTRAINT_SCHEMA = DATABASE()
        AND TABLE_NAME LIKE 'episodic\\_%'"
  )$n
  expect_gt(declared, 0)
  expect_equal(created, declared)
})

test_that("a created database connects and reports its schema version", {
  dsn <- mariadb_fresh()
  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
})

test_that("episodic_db_create() refuses a database that already has tables", {
  dsn <- mariadb_fresh()
  expect_error(episodic_db_create(dsn), "already contains")
})

test_that("a database with no version table is migrated without losing data", {
  dsn <- mariadb_fresh()
  con <- episodic_db_connect(dsn)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_institution
       (institution_key, display_name, institution_type, care_line)
     VALUES (?, 'Kept', 'hospital', 'second')",
    params = list(strrep("a", 40))
  )
  # Version 1 is "created before the version table existed"; the
  # login-failure table is what version 2 added, and the report version
  # register plus its unique index what version 3 did.
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbExecute(con, "DROP TABLE episodic_app_login_failure")
  DBI::dbExecute(con, "DROP TABLE episodic_report_version_claim")
  DBI::dbExecute(
    con,
    "DROP INDEX idx_episodic_report_render_version ON episodic_report_render"
  )
  DBI::dbDisconnect(con)

  expect_message(
    episodic_db_migrate(dsn),
    paste("schema version", episodic_schema_version)
  )

  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_true(DBI::dbExistsTable(con, "episodic_app_login_failure"))
  expect_true(DBI::dbExistsTable(con, "episodic_report_version_claim"))
  expect_true(episodic_db_index_exists(
    con,
    "mariadb",
    "idx_episodic_report_render_version",
    "episodic_report_render"
  ))
  # A migration never drops or rewrites data.
  expect_equal(
    DBI::dbGetQuery(con, "SELECT count(*) AS n FROM episodic_institution")$n,
    1
  )
})

test_that("a schema shared with another application is not mistaken for ours", {
  dsn <- mariadb_dsn()
  con <- episodic_db_connect(dsn, check_schema_version = FALSE)
  # With checks on, the tables cannot be dropped in an arbitrary order -
  # they really do reference each other now, which is the point of the
  # rest of this file.
  DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")
  for (table in DBI::dbListTables(con)) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS `", table, "`"))
  }
  DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1")
  # A co-tenant's table, and nothing of ours.
  DBI::dbExecute(con, "CREATE TABLE brmo_orders (id INT PRIMARY KEY)")
  DBI::dbDisconnect(con)

  expect_false(episodic_db_exists(dsn))

  # So a first run creates the schema rather than refusing for having no
  # schema version and sending the operator to episodic_db_migrate(),
  # which would stamp the schema as current with two tables in it.
  end_date <- as.Date("2025-06-29")
  suppressMessages(episodic_run_cron(
    cases = mariadb_cases(end_date),
    db_path = dsn,
    run_date = end_date
  ))

  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tables <- DBI::dbListTables(con)
  expect_true(all(episodic_db_schema_tables() %in% tables))
  # And the co-tenant is untouched.
  expect_true("brmo_orders" %in% tables)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
})

test_that("a full detection run completes against MariaDB", {
  skip_on_cran()
  dsn <- mariadb_fresh()
  end_date <- as.Date("2025-06-29")
  suppressMessages(episodic_run_cron(
    cases = mariadb_cases(end_date),
    db_path = dsn,
    run_date = end_date
  ))

  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con))
  run <- DBI::dbGetQuery(
    con,
    "SELECT status, n_streams, n_cases_inserted FROM episodic_detection_run"
  )
  expect_equal(nrow(run), 1)
  expect_equal(run$status, "success")
  expect_gt(run$n_streams, 0)
  expect_gt(run$n_cases_inserted, 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM episodic_cluster")$n, 0)
  # An id read back through LAST_INSERT_ID(), which MariaDB returns as a
  # BIGINT: an integer64 subassigned into an ordinary vector keeps its
  # bit pattern and loses its class, and a real id became a subnormal
  # double written into an INTEGER column as 0.
  ids <- DBI::dbGetQuery(con, "SELECT cluster_id FROM episodic_cluster")$cluster_id
  expect_false(any(ids == 0))
})

test_that("the dashboard reads what the run wrote, against MariaDB", {
  skip_on_cran()
  dsn <- mariadb_fresh()
  end_date <- as.Date("2025-06-29")
  suppressMessages(episodic_run_cron(
    cases = mariadb_cases(end_date),
    db_path = dsn,
    run_date = end_date
  ))

  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con))
  open <- episodic_app_open_clusters(con)
  expect_gt(nrow(open), 0)

  dossier <- episodic_cluster_object(con, open$cluster_id[1])
  expect_equal(dossier$id, open$cluster_id[1])
  expect_gt(dossier$n_cases, 0)

  activity <- episodic_app_activity_log(con)
  expect_gt(nrow(activity), 0)
})

test_that("a scoring closure that queries mid-reconciliation does not kill the session", {
  skip_on_cran()
  # RMariaDB allows one active result per connection. An R argument is a
  # promise, so an inline `params = list(priority_score_fn(candidate))`
  # is evaluated inside dbExecute(), after the driver has prepared a
  # statement - and if evaluating it queries the same connection, the
  # prepared statement is cancelled and freed and dbBind() writes into
  # freed memory. The session dies natively: no condition, nothing
  # tryCatch can see, nothing in the R log.
  #
  # test-db_write_reentrancy.R holds the fix in place by reading source,
  # because none of it is reproducible on SQLite. Here it can be held in
  # place by running: if reconciliation ever inlines the closure again,
  # this test does not fail, it takes the whole R process with it, which
  # is exactly as loud as this deserves to be.
  dsn <- mariadb_fresh()
  con <- episodic_db_connect(dsn)
  on.exit(DBI::dbDisconnect(con))

  institution_id <- episodic_test_institution(con, "mariadb-reentrancy")
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_ward",
      "Norovirus",
      institution_id = institution_id,
      ward = "B4"
    ),
    level = "pathogen_ward",
    pathogen = "Norovirus",
    care_line = "second",
    institution_id = institution_id,
    ward = "B4",
    denominator = "patient_days",
    observed_date = "2025-01-15"
  )
  run_id <- episodic_db_run_start(con, "host", "account")
  cases <- data.frame(
    source_key = sprintf("MDB%d", 1:4),
    lab_number = sprintf("LAB-MDB%d", 1:4),
    patient_key = sprintf("PMDB%d", 1:4),
    sample_date = c("2025-01-10", "2025-01-11", "2025-01-12", "2025-01-13"),
    receipt_date = c("2025-01-10", "2025-01-11", "2025-01-12", "2025-01-13"),
    pathogen = "Norovirus",
    care_line = "second",
    institution_id = institution_id,
    ward = "B4",
    specialism = "Interne",
    pc = "9711",
    sex = "M",
    age = 70L,
    first_seen_run = run_id,
    stringsAsFactors = FALSE
  )
  episodic_db_case_insert_new(con, cases, run_id)

  detections <- episodic_detection_record(
    stream_id = stream_id,
    detector = "same_place",
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    n_cases = 4L
  )
  detections$detection_id <- NA_integer_

  queried <- 0L
  result <- episodic_reconcile_stream(
    con,
    stream_id = stream_id,
    detections = detections,
    case_free_days = 14,
    run_id = run_id,
    close_after_runs = 14,
    priority_score_fn = function(candidate) {
      # The dangerous shape: a real query on the same connection, from
      # inside the value reconciliation is about to write.
      queried <<- queried + 1L
      DBI::dbGetQuery(
        con,
        "SELECT count(*) AS n FROM episodic_case WHERE pathogen = ?",
        params = list("Norovirus")
      )$n[1]
    },
    has_assessment_fn = function(cluster_id) FALSE,
    verdict_fn = function(cluster_id) NA_character_,
    today = as.Date("2025-01-14")
  )

  expect_gt(queried, 0L)
  expect_equal(result$n_new, 1L)
  stored <- DBI::dbGetQuery(
    con,
    "SELECT priority_score FROM episodic_cluster"
  )$priority_score
  expect_equal(stored, 4)
})
