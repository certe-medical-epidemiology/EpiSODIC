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

# A scheduled run brings a database one schema version behind the
# installed package forward before it runs, and refuses - out loud, and
# on a run row - a database it will not open.

# A SQLite database at the schema version before the current one: the
# current version's own table dropped and its version row removed.
auto_migrate_previous_version_db <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_detector_cache")
  DBI::dbExecute(
    con,
    "DELETE FROM episodic_schema_version WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_schema_version (version, applied_at) VALUES (?, '2025-01-01T00:00:00Z')",
    params = list(episodic_schema_version - 1L)
  )
  DBI::dbDisconnect(con)
  path
}

auto_migrate_config <- function(auto_migrate) {
  path <- tempfile(fileext = ".yaml")
  writeLines(
    c(
      "geography:",
      "  region_code: TEST_REGION",
      "database:",
      paste0("  auto_migrate: ", if (auto_migrate) "true" else "false")
    ),
    path
  )
  path
}

auto_migrate_cases <- function() {
  episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-08-31"),
    seed = 3
  )
}

auto_migrate_version <- function(path) {
  con <- episodic_db_connect(path, check_schema_version = FALSE)
  on.exit(DBI::dbDisconnect(con))
  episodic_db_schema_version(con)
}

auto_migrate_runs <- function(path) {
  con <- episodic_db_connect(path, check_schema_version = FALSE)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT status, error_text FROM episodic_detection_run ORDER BY run_id"
  )
}

test_that("a run migrates a database one schema version behind, then runs", {
  path <- auto_migrate_previous_version_db()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))

  log <- testthat::capture_messages(episodic_run_cron(
    cases = auto_migrate_cases(),
    db_path = path,
    run_date = as.Date("2024-08-31")
  ))

  expect_identical(auto_migrate_version(path), episodic_schema_version)
  expect_match(
    grep("Database migrated from", log, value = TRUE),
    paste0(
      "schema version ", episodic_schema_version - 1L,
      " to schema version ", episodic_schema_version
    )
  )
  runs <- auto_migrate_runs(path)
  expect_identical(runs$status, "success")

  # The copy taken before the first step is the database as it was.
  backup <- sprintf(
    "%s.schema-%d-to-%d.bak",
    path,
    episodic_schema_version - 1L,
    episodic_schema_version
  )
  expect_true(file.exists(backup))
  expect_identical(auto_migrate_version(backup), episodic_schema_version - 1L)
  con <- episodic_db_connect(backup, check_schema_version = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false(DBI::dbExistsTable(con, "episodic_detector_cache"))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_detection_run")$n,
    0L
  )
})

test_that("with auto_migrate off, a run refuses a database behind it and records why", {
  path <- auto_migrate_previous_version_db()
  config <- auto_migrate_config(FALSE)
  on.exit(unlink(c(path, config)))

  expect_error(
    suppressMessages(episodic_run_cron(
      cases = auto_migrate_cases(),
      db_path = path,
      episodic_config_path = config,
      run_date = as.Date("2024-08-31")
    )),
    "episodic_db_migrate\\(\\)"
  )

  expect_identical(auto_migrate_version(path), episodic_schema_version - 1L)
  runs <- auto_migrate_runs(path)
  expect_identical(runs$status, "failed")
  expect_match(runs$error_text, "auto_migrate")
  expect_length(Sys.glob(paste0(path, ".schema-*.bak")), 0L)
})

test_that("a run refuses a database newer than the installed package, and records why", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_schema_version (version, applied_at) VALUES (?, '2099-01-01T00:00:00Z')",
    params = list(episodic_schema_version + 1L)
  )
  DBI::dbDisconnect(con)

  expect_error(
    suppressMessages(episodic_run_cron(
      cases = auto_migrate_cases(),
      db_path = path,
      run_date = as.Date("2024-08-31")
    )),
    "newer than"
  )
  runs <- auto_migrate_runs(path)
  expect_identical(runs$status, "failed")
  expect_match(runs$error_text, "Upgrade the package")
})

test_that("a step another process already applied is skipped, not repeated", {
  path <- auto_migrate_previous_version_db()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  suppressMessages(episodic_db_migrate(path))

  # The step as a process that read the old version before waiting would
  # run it: the version row is already there, so nothing is stamped twice.
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  step <- episodic_db_migrations()[[as.character(episodic_schema_version)]]
  expect_false(episodic_db_migration_step(con, "sqlite", episodic_schema_version, step))
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM episodic_schema_version WHERE version = ?",
      params = list(episodic_schema_version)
    )$n,
    1L
  )
})

test_that("a failed step is rolled back under SQLite and says what the database is at", {
  path <- auto_migrate_previous_version_db()
  on.exit(unlink(path))
  con <- episodic_db_connect(path, check_schema_version = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_error(
    episodic_db_migration_step(
      con,
      "sqlite",
      episodic_schema_version,
      function(con, dialect) {
        episodic_db_execute(con, "CREATE TABLE episodic_half_done (x INTEGER)")
        stop("disk on fire")
      }
    ),
    paste0("unchanged at version ", episodic_schema_version - 1L, ". disk on fire")
  )
  expect_false(DBI::dbExistsTable(con, "episodic_half_done"))
  expect_identical(episodic_db_schema_version(con), episodic_schema_version - 1L)
})

test_that("a backup already taken before an earlier attempt is kept, not replaced", {
  path <- auto_migrate_previous_version_db()
  backup <- sprintf(
    "%s.schema-%d-to-%d.bak",
    path,
    episodic_schema_version - 1L,
    episodic_schema_version
  )
  on.exit(unlink(c(path, backup)))
  writeLines("the earlier copy", backup)

  expect_message(episodic_db_migrate(path), "Keeping the backup already at")
  expect_identical(readLines(backup), "the earlier copy")
  expect_identical(auto_migrate_version(path), episodic_schema_version)
})

test_that("a manual migration can be asked not to copy the database", {
  path <- auto_migrate_previous_version_db()
  on.exit(unlink(path))
  suppressMessages(episodic_db_migrate(path, backup = FALSE))
  expect_length(Sys.glob(paste0(path, ".schema-*.bak")), 0L)
  expect_identical(auto_migrate_version(path), episodic_schema_version)
})

test_that("a missing privilege to change the schema is named, with what to do about it", {
  mariadb <- episodic_db_migration_failure_message(
    8L,
    "CREATE command denied to user 'episodic'@'localhost' for table 'episodic_detector_cache'",
    "mariadb"
  )
  expect_match(mariadb, "CREATE, ALTER, INDEX and REFERENCES")
  expect_match(mariadb, "database.auto_migrate: false")
  expect_match(mariadb, "may already be in place")
  expect_match(mariadb, "command denied", fixed = TRUE)

  sqlite <- episodic_db_migration_failure_message(
    8L,
    "attempt to write a readonly database",
    "sqlite"
  )
  expect_match(sqlite, "cannot be written")
  expect_match(sqlite, "unchanged at version 7")

  other <- episodic_db_migration_failure_message(8L, "syntax error", "mariadb")
  expect_false(grepl("CREATE, ALTER", other))
})

test_that("auto_migrate is validated, and stays out of the config hash", {
  on <- episodic_config_resolve(NA)
  expect_true(on$database$auto_migrate)
  off <- on
  off$database$auto_migrate <- FALSE
  expect_identical(episodic_config_hash(off)$hash, episodic_config_hash(on)$hash)

  bad <- tempfile(fileext = ".yaml")
  on.exit(unlink(bad))
  writeLines(c("database:", "  auto_migrate: sometimes"), bad)
  expect_error(episodic_config_resolve(bad), "auto_migrate")
})

test_that("the dashboard's refusal of a database behind it says the next run fixes it", {
  path <- auto_migrate_previous_version_db()
  on.exit(unlink(path))
  expect_error(
    episodic_db_connect(path),
    "next episodic_run_cron\\(\\) brings it up to date"
  )
})

auto_migrate_notifying_config <- function(auto_migrate) {
  path <- auto_migrate_config(auto_migrate)
  write(
    c(
      "notifications:",
      "  enabled: true",
      "  triggers:",
      "    run_failure: true",
      "  channels:",
      "    ntfy:",
      "      enabled: true",
      "      server: \"https://ntfy.example.org\"",
      "      topic: \"episodic-test\""
    ),
    path,
    append = TRUE
  )
  path
}

test_that("a run refused by its database sends the run_failure notification", {
  path <- auto_migrate_previous_version_db()
  config <- auto_migrate_notifying_config(FALSE)
  on.exit(unlink(c(path, config)))
  sent <- list()
  local_mocked_bindings(
    episodic_notify_dispatch = function(channels, message) {
      sent[[length(sent) + 1L]] <<- message
      invisible(NULL)
    }
  )

  expect_error(
    suppressMessages(episodic_run_cron(
      cases = auto_migrate_cases(),
      db_path = path,
      episodic_config_path = config,
      run_date = as.Date("2024-08-31")
    )),
    "auto_migrate"
  )
  expect_length(sent, 1L)
  expect_match(sent[[1]]$plain, "auto_migrate", fixed = TRUE)
})

test_that("a run stopped by its pre-run data checks sends the run_failure notification", {
  path <- episodic_test_db_path()
  config <- auto_migrate_notifying_config(TRUE)
  on.exit(unlink(c(path, config)))
  sent <- list()
  local_mocked_bindings(
    episodic_notify_dispatch = function(channels, message) {
      sent[[length(sent) + 1L]] <<- message
      invisible(NULL)
    }
  )
  cases <- auto_migrate_cases()
  cases$pathogen <- NULL

  expect_error(
    suppressMessages(episodic_run_cron(
      cases = cases,
      db_path = path,
      episodic_config_path = config,
      run_date = as.Date("2024-08-31")
    )),
    "pathogen"
  )
  expect_length(sent, 1L)
  expect_match(sent[[1]]$plain, "pathogen", fixed = TRUE)
})

test_that("the run log names the database server it connected to", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_match(episodic_db_server_label(con), "^SQLite [0-9]+\\.[0-9]+")

  server <- function(version) {
    local_mocked_bindings(
      episodic_db_get_query = function(con, statement, params = NULL) {
        if (is.null(version)) stop("no VERSION() here")
        data.frame(v = version)
      }
    )
    episodic_db_server_label(structure(list(), class = "MySQLConnection"))
  }
  expect_identical(server("11.4.2-MariaDB-ubu2404"), "MariaDB 11.4.2")
  expect_identical(server("10.11.6-MariaDB-0+deb12u1-log"), "MariaDB 10.11.6")
  expect_identical(server("8.0.36"), "MySQL 8.0.36")
  expect_identical(server("8.4.0-commercial"), "MySQL 8.4.0")
  expect_identical(server(NULL), "MariaDB/MySQL")
})
