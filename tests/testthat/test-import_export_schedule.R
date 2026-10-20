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

# Export schedules and import sources: registered by an admin, carried
# out by episodic_run_cron(). Helpers in helper-import_export.R.

schedule_test_set <- function(con, user_id, sender, receiver, file, ...) {
  args <- list(
    con = con,
    user_id = user_id,
    name = "To partner",
    interval_days = 7,
    window_days = 14,
    mode = "public_key",
    recipients = import_export_test_key_line(receiver$encryption_public),
    signing_key_env = "IMPORT_EXPORT_TEST_SIGNING_KEY",
    destination_type = "file",
    destination = file
  )
  extra <- list(...)
  args[names(extra)] <- extra
  do.call(episodic_export_schedule_set, args)
}

test_that("a schedule a run could not carry out is refused when it is set", {
  receiver <- import_export_test_party()
  con <- episodic_test_db()
  on.exit({
    DBI::dbDisconnect(con)
    unlink(receiver$dir, recursive = TRUE)
  })
  user_id <- import_export_test_admin(con)
  file <- tempfile(fileext = ".episodic")
  expect_error(
    schedule_test_set(con, user_id, NULL, receiver, file, interval_days = 14, window_days = 7),
    "shorter than the interval"
  )
  expect_error(
    schedule_test_set(con, user_id, NULL, receiver, file, recipients = character(0)),
    "at least one recipient"
  )
  expect_error(
    schedule_test_set(con, user_id, NULL, receiver, file, recipients = import_export_test_key_line(receiver$signing_public)),
    "encryption public key is needed"
  )
  expect_error(
    schedule_test_set(con, user_id, NULL, receiver, file, signing_key_env = "not a name"),
    "not an environment variable name"
  )
  expect_error(
    schedule_test_set(
      con, user_id, NULL, receiver, NA,
      destination_type = "http_put", destination = "https://bucket.example/f?sig=1"
    ),
    "carries credentials"
  )
  expect_error(
    schedule_test_set(con, user_id, NULL, receiver, file, fields = c("pc", "postcode")),
    "postcode"
  )
  expect_error(
    schedule_test_set(con, user_id, NULL, receiver, file, mode = "passphrase"),
    "The passphrase is read from an environment variable"
  )
  expect_equal(nrow(episodic_db_export_schedule_events(con)), 0L)
})

test_that("the latest event per name is the current schedule, and a cancel ends it", {
  receiver <- import_export_test_party()
  con <- episodic_test_db()
  on.exit({
    DBI::dbDisconnect(con)
    unlink(receiver$dir, recursive = TRUE)
  })
  user_id <- import_export_test_admin(con)
  file <- tempfile(fileext = ".episodic")
  schedule_test_set(con, user_id, NULL, receiver, file)
  schedule_test_set(con, user_id, NULL, receiver, file, interval_days = 3)
  current <- episodic_export_schedules_current(con)
  expect_named(current, "To partner")
  expect_equal(current[["To partner"]]$interval_days, 3L)
  # Nothing secret is stored: the variable's name, never its value.
  expect_identical(current[["To partner"]]$signing_key_env, "IMPORT_EXPORT_TEST_SIGNING_KEY")
  episodic_import_export_cancel(con, user_id, "To partner", kind = "schedule")
  expect_length(episodic_export_schedules_current(con), 0L)
  expect_error(episodic_import_export_cancel(con, user_id, "To partner", kind = "schedule"), "no current schedule")
  # Every change kept.
  expect_equal(nrow(episodic_db_export_schedule_events(con)), 3L)
})

test_that("a schedule is due first, then every interval after a written export, not after a failed one", {
  schedule <- list(event_id = 5L, interval_days = 7L)
  none <- data.frame(schedule_event_id = integer(0), status = character(0), exported_at = character(0))
  expect_true(episodic_export_schedule_due(schedule, none, as.Date("2025-05-01")))
  exports <- data.frame(
    schedule_event_id = c(5L, 5L, 4L),
    status = c("written", "failed", "written"),
    exported_at = c("2025-05-01T02:00:00Z", "2025-05-06T02:00:00Z", "2025-05-07T02:00:00Z"),
    stringsAsFactors = FALSE
  )
  expect_false(episodic_export_schedule_due(schedule, exports, as.Date("2025-05-07")))
  expect_true(episodic_export_schedule_due(schedule, exports, as.Date("2025-05-08")))
  # Under changed settings (a new event) it is due at once.
  expect_true(episodic_export_schedule_due(list(event_id = 6L, interval_days = 7L), exports, as.Date("2025-05-02")))
})

test_that("a run carries out a due schedule after it commits, and records it against the run", {
  sender <- import_export_test_party()
  receiver <- import_export_test_party()
  db <- import_export_test_sender_db()
  file <- tempfile(fileext = ".episodic")
  on.exit(unlink(c(sender$dir, receiver$dir, db, file), recursive = TRUE))
  con <- episodic_db_connect(db)
  schedule_test_set(con, import_export_test_admin(con), sender, receiver, file, window_days = 60)
  DBI::dbDisconnect(con)
  withr::local_envvar(IMPORT_EXPORT_TEST_SIGNING_KEY = sender$signing_secret)

  run_id <- suppressMessages(suppressWarnings(episodic_run_cron(
    cases = import_export_test_cases()[0, ],
    db_path = db,
    run_date = as.Date("2025-04-30")
  )))
  expect_true(file.exists(file))
  imported <- suppressWarnings(episodic_cases_import(
    file,
    key = receiver$encryption_secret,
    trusted_senders = sender$signing_public
  ))
  # The window is the 60 days up to the run date.
  expect_equal(nrow(imported), 4L)
  expect_identical(attr(imported, "import")$from, "2025-03-02")
  expect_identical(attr(imported, "import")$to, "2025-04-30")

  con <- episodic_db_connect(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  row <- DBI::dbGetQuery(con, "SELECT * FROM episodic_export_event")
  expect_equal(nrow(row), 1L)
  expect_identical(row$initiated_by, "schedule")
  expect_identical(row$schedule_name, "To partner")
  expect_identical(row$run_id, run_id)
  expect_identical(row$status, "written")
  expect_identical(row$destination, file)

  # Not due again the next day.
  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = import_export_test_cases()[0, ],
    db_path = db,
    run_date = as.Date("2025-05-01")
  )))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_export_event")$n, 1L)
})

test_that("a schedule that fails is recorded and logged, and the run is unaffected", {
  receiver <- import_export_test_party()
  db <- import_export_test_sender_db()
  file <- tempfile(fileext = ".episodic")
  on.exit(unlink(c(receiver$dir, db, file), recursive = TRUE))
  con <- episodic_db_connect(db)
  schedule_test_set(con, import_export_test_admin(con), NULL, receiver, file)
  DBI::dbDisconnect(con)
  withr::local_envvar(IMPORT_EXPORT_TEST_SIGNING_KEY = NA)
  log <- testthat::capture_messages(suppressWarnings(episodic_run_cron(
    cases = import_export_test_cases()[0, ],
    db_path = db,
    run_date = as.Date("2025-04-30")
  )))
  expect_true(any(grepl("Export schedule \"To partner\" failed", log, fixed = TRUE)))
  expect_false(file.exists(file))
  con <- episodic_db_connect(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  row <- DBI::dbGetQuery(con, "SELECT status, error_text FROM episodic_export_event")
  expect_identical(row$status, "failed")
  expect_match(row$error_text, "IMPORT_EXPORT_TEST_SIGNING_KEY")
  runs <- DBI::dbGetQuery(con, "SELECT status FROM episodic_detection_run ORDER BY run_id DESC LIMIT 1")
  expect_identical(runs$status, "success")
})

source_test_set <- function(con, user_id, sender, file, ...) {
  args <- list(
    con = con,
    user_id = user_id,
    name = "From partner",
    mode = "public_key",
    key_env = "IMPORT_EXPORT_TEST_KEY",
    trusted_sender = paste(readLines(sender$signing_public), collapse = "\n"),
    location_type = "file",
    location = file
  )
  extra <- list(...)
  args[names(extra)] <- extra
  do.call(episodic_import_source_set, args)
}

test_that("a source is refused unless its key, sender and location are usable", {
  sender <- import_export_test_party()
  con <- episodic_test_db()
  on.exit({
    DBI::dbDisconnect(con)
    unlink(sender$dir, recursive = TRUE)
  })
  user_id <- import_export_test_admin(con)
  expect_error(source_test_set(con, user_id, sender, "f", key_env = NA), "secret encryption key is read")
  expect_error(
    source_test_set(con, user_id, sender, "f", trusted_sender = import_export_test_key_line(sender$encryption_public)),
    "signing public key is needed"
  )
  expect_error(source_test_set(con, user_id, sender, NA), "needs a path")
  # The pasted .pub file, comments and all, is stored as its key line.
  source_test_set(con, user_id, sender, "f")
  stored <- episodic_import_sources_current(con)[["From partner"]]$trusted_sender
  expect_identical(stored, import_export_test_key_line(sender$signing_public))
})

test_that("a run takes in a registered source's cases and records the import", {
  sender <- import_export_test_party()
  receiver <- import_export_test_party()
  db <- import_export_test_sender_db()
  receiver_db <- episodic_test_db_path()
  file <- tempfile(fileext = ".episodic")
  on.exit(unlink(c(sender$dir, receiver$dir, db, receiver_db, file), recursive = TRUE))
  episodic_cases_export(
    db_path = db, from = "2025-03-01", to = "2025-03-31", file = file,
    recipients = receiver$encryption_public, signing_key = sender$signing_secret
  )
  con <- episodic_db_connect(receiver_db)
  source_test_set(con, import_export_test_admin(con), sender, file)
  DBI::dbDisconnect(con)
  withr::local_envvar(IMPORT_EXPORT_TEST_KEY = receiver$encryption_secret)

  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = import_export_test_cases(),
    db_path = receiver_db,
    run_date = as.Date("2025-04-30")
  )))
  con <- episodic_db_connect(receiver_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_case")$n, 7L)
  event <- DBI::dbGetQuery(con, "SELECT * FROM episodic_import_event")
  expect_identical(event$source_name, "From partner")
  expect_identical(event$status, "imported")
  expect_identical(event$location, file)
  expect_identical(
    DBI::dbGetQuery(con, "SELECT status FROM episodic_detection_run")$status,
    "success"
  )
})

test_that("a source that fails marks the run partial, says why, and the run goes on", {
  sender <- import_export_test_party()
  receiver_db <- episodic_test_db_path()
  on.exit(unlink(c(sender$dir, receiver_db), recursive = TRUE))
  con <- episodic_db_connect(receiver_db)
  source_test_set(con, import_export_test_admin(con), sender, tempfile(fileext = ".episodic"))
  DBI::dbDisconnect(con)
  withr::local_envvar(IMPORT_EXPORT_TEST_KEY = NA)
  log <- testthat::capture_messages(suppressWarnings(episodic_run_cron(
    cases = import_export_test_cases(),
    db_path = receiver_db,
    run_date = as.Date("2025-04-30")
  )))
  expect_true(any(grepl("Import source \"From partner\" failed", log, fixed = TRUE)))
  con <- episodic_db_connect(receiver_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  run <- DBI::dbGetQuery(con, "SELECT status, error_text FROM episodic_detection_run")
  expect_identical(run$status, "partial")
  expect_match(run$error_text, "From partner")
  event <- DBI::dbGetQuery(con, "SELECT status, failure_reason FROM episodic_import_event")
  expect_identical(event$status, "failed")
  expect_identical(event$failure_reason, "environment")
  # The operator's own cases are in all the same.
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_case")$n, 4L)
})

test_that("a connection test of a source imports what the next run would, and writes nothing", {
  sender <- import_export_test_party()
  receiver <- import_export_test_party()
  db <- import_export_test_sender_db()
  receiver_db <- episodic_test_db_path()
  file <- tempfile(fileext = ".episodic")
  on.exit(unlink(c(sender$dir, receiver$dir, db, receiver_db, file), recursive = TRUE))
  episodic_cases_export(
    db_path = db, from = "2025-03-01", to = "2025-03-31", file = file,
    recipients = receiver$encryption_public, signing_key = sender$signing_secret
  )
  con <- episodic_db_connect(receiver_db)
  source_test_set(con, import_export_test_admin(con), sender, file)
  DBI::dbDisconnect(con)

  withr::local_envvar(IMPORT_EXPORT_TEST_KEY = NA)
  failing <- episodic_import_export_test_connection("From partner", kind = "source", db_path = receiver_db)
  expect_false(failing$ok)
  expect_true("import_export.test.secret_failed" %in% vapply(failing$lines, `[[`, "", "key"))

  withr::local_envvar(IMPORT_EXPORT_TEST_KEY = receiver$encryption_secret)
  passing <- episodic_import_export_test_connection("From partner", kind = "source", db_path = receiver_db)
  expect_true(passing$ok)
  keys <- vapply(passing$lines, `[[`, "", "key")
  expect_true(all(c("import_export.test.key_ok", "import_export.test.read_ok", "import_export.test.import_ok") %in% keys))
  expect_output(print(passing), "passed")

  con <- episodic_db_connect(receiver_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_case")$n, 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_import_event")$n, 0L)
})

test_that("a connection test of a schedule checks its keys and destination without exporting", {
  sender <- import_export_test_party()
  receiver <- import_export_test_party()
  db <- import_export_test_sender_db()
  file <- tempfile(fileext = ".episodic")
  on.exit(unlink(c(sender$dir, receiver$dir, db), recursive = TRUE))
  con <- episodic_db_connect(db)
  schedule_test_set(con, import_export_test_admin(con), sender, receiver, file)
  DBI::dbDisconnect(con)
  withr::local_envvar(IMPORT_EXPORT_TEST_SIGNING_KEY = sender$signing_secret)
  result <- episodic_import_export_test_connection("To partner", kind = "schedule", db_path = db)
  expect_true(result$ok)
  keys <- vapply(result$lines, `[[`, "", "key")
  expect_true(all(c("import_export.test.signing_ok", "import_export.test.recipients", "import_export.test.dir_ok") %in% keys))
  expect_false(file.exists(file))
})

test_that("every connection test line renders in every shipped language", {
  for (lang in c("en", "nl", "de", "fr", "es", "zh", "hi", "ar")) {
    for (line in list(
      episodic_import_export_test_line("env", FALSE, "import_export.test.env_missing", variable = "X"),
      episodic_import_export_test_line("reach", TRUE, "import_export.test.read_ok", location = "/f", bytes = 12345),
      episodic_import_export_test_line("import", TRUE, "import_export.test.import_ok", n = 3, instance = "abcdef0123", from = "2025-01-01", to = "2025-01-31")
    )) {
      text <- episodic_import_export_test_text(line, lang = lang)
      expect_false(grepl("{", text, fixed = TRUE), info = paste(lang, line$key))
      expect_false(grepl(line$key, text, fixed = TRUE), info = paste(lang, line$key))
    }
  }
})
