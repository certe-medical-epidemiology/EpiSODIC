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

test_that("episodic_valid_email() accepts plausible addresses and rejects the rest", {
  expect_true(episodic_valid_email("jdoe@example.com"))
  expect_true(episodic_valid_email("j.doe+alerts@sub.example.org"))
  expect_false(episodic_valid_email("not-an-email"))
  expect_false(episodic_valid_email("missing-at.example.com"))
  expect_false(episodic_valid_email("no-dot@example"))
  expect_false(episodic_valid_email(""))
})

test_that("episodic_report_subscription_parse_recipients() splits on comma, semicolon and newline", {
  parsed <- episodic_report_subscription_parse_recipients(
    "a@example.com, b@example.com;c@example.com\nd@example.com"
  )
  expect_setequal(
    parsed$valid,
    c("a@example.com", "b@example.com", "c@example.com", "d@example.com")
  )
  expect_length(parsed$invalid, 0L)
})

test_that("episodic_report_subscription_parse_recipients() trims whitespace and drops blanks", {
  parsed <- episodic_report_subscription_parse_recipients("  a@example.com , \n\n b@example.com  ")
  expect_setequal(parsed$valid, c("a@example.com", "b@example.com"))
})

test_that("episodic_report_subscription_parse_recipients() de-duplicates", {
  parsed <- episodic_report_subscription_parse_recipients("a@example.com, a@example.com")
  expect_equal(parsed$valid, "a@example.com")
})

test_that("episodic_report_subscription_parse_recipients() separates invalid entries", {
  parsed <- episodic_report_subscription_parse_recipients("a@example.com, not-an-email")
  expect_equal(parsed$valid, "a@example.com")
  expect_equal(parsed$invalid, "not-an-email")
})

test_that("episodic_report_subscription_parse_recipients() handles empty input", {
  expect_equal(episodic_report_subscription_parse_recipients("")$valid, character(0))
  expect_equal(episodic_report_subscription_parse_recipients(NA)$valid, character(0))
  expect_equal(episodic_report_subscription_parse_recipients(NULL)$valid, character(0))
})

test_that("recipients JSON round-trips", {
  emails <- c("a@example.com", "b@example.com")
  json <- episodic_report_subscription_recipients_to_json(emails)
  expect_equal(episodic_report_subscription_recipients_from_json(json), emails)
  expect_equal(episodic_report_subscription_recipients_from_json(NA), character(0))
  expect_equal(episodic_report_subscription_recipients_from_json(""), character(0))
})

test_that("episodic_report_subscription_current_all() reduces to the latest 'set' event per cluster", {
  events <- data.frame(
    event_id = 1:4,
    cluster_id = c(1L, 1L, 2L, 2L),
    user_id = 9L,
    created_at = c(
      "2026-01-01T00:00:00Z", "2026-01-05T00:00:00Z",
      "2026-01-01T00:00:00Z", "2026-01-02T00:00:00Z"
    ),
    action = c("set", "set", "set", "cancel"),
    interval_days = c(7L, 3L, 7L, NA),
    recipients = c(
      jsonlite::toJSON(c("a@example.com")),
      jsonlite::toJSON(c("b@example.com")),
      jsonlite::toJSON(c("c@example.com")),
      NA
    ),
    channel = c("smtp", "smtp", "sendmail", NA),
    include_linelist = c(0L, 1L, 0L, 0L),
    stringsAsFactors = FALSE
  )

  current <- episodic_report_subscription_current_all(events)
  # cluster 1: latest 'set' (event_id 2) wins
  expect_equal(current[["1"]]$interval_days, 3L)
  expect_equal(current[["1"]]$recipients, "b@example.com")
  expect_true(current[["1"]]$include_linelist)
  # cluster 2: latest event is a cancel -> no active schedule
  expect_null(current[["2"]])
})

test_that("episodic_report_subscription_current_all() returns an empty list for no events", {
  events <- data.frame(
    event_id = integer(0), cluster_id = integer(0), user_id = integer(0),
    created_at = character(0), action = character(0), interval_days = integer(0),
    recipients = character(0), channel = character(0), include_linelist = integer(0)
  )
  expect_equal(episodic_report_subscription_current_all(events), list())
})

test_that("episodic_report_subscription_due() is TRUE with no prior successful send", {
  subscription <- list(event_id = 1L, interval_days = 7L)
  sends <- data.frame(
    subscription_event_id = integer(0), status = character(0), sent_at = character(0)
  )
  expect_true(episodic_report_subscription_due(subscription, sends, run_date = "2026-01-10"))
})

test_that("episodic_report_subscription_due() ignores failed sends when judging the cadence", {
  subscription <- list(event_id = 1L, interval_days = 7L)
  sends <- data.frame(
    subscription_event_id = 1L,
    status = "failed",
    sent_at = "2026-01-10T07:00:00Z",
    stringsAsFactors = FALSE
  )
  # A failed attempt earlier today must not count as satisfying the
  # interval - the very next run should still try again.
  expect_true(episodic_report_subscription_due(subscription, sends, run_date = "2026-01-10"))
})

test_that("episodic_report_subscription_due() is FALSE within the interval of a successful send", {
  subscription <- list(event_id = 1L, interval_days = 7L)
  sends <- data.frame(
    subscription_event_id = 1L,
    status = "sent",
    sent_at = "2026-01-05T07:00:00Z",
    stringsAsFactors = FALSE
  )
  expect_false(episodic_report_subscription_due(subscription, sends, run_date = "2026-01-10"))
  expect_true(episodic_report_subscription_due(subscription, sends, run_date = "2026-01-12"))
})

test_that("episodic_report_subscription_due() ignores sends logged under a different schedule version", {
  # Changing the interval or recipient list writes a new event_id -
  # sends against the old settings must not satisfy the new schedule's
  # cadence, so the very next run sends immediately under the new terms.
  subscription <- list(event_id = 2L, interval_days = 7L)
  sends <- data.frame(
    subscription_event_id = 1L,
    status = "sent",
    sent_at = "2026-01-09T07:00:00Z",
    stringsAsFactors = FALSE
  )
  expect_true(episodic_report_subscription_due(subscription, sends, run_date = "2026-01-10"))
})

test_that("episodic_report_subscription_available_channels() keeps only enabled, email-capable channels", {
  config <- list(notifications = list(channels = list(
    smtp = list(enabled = TRUE),
    sendmail = list(enabled = FALSE),
    microsoft365 = list(enabled = TRUE),
    ntfy = list(enabled = TRUE),
    teams = list(enabled = TRUE)
  )))
  expect_setequal(
    episodic_report_subscription_available_channels(config),
    c("smtp", "microsoft365")
  )
})

test_that("episodic_report_subscription_available_channels() is empty with no channels configured", {
  expect_equal(episodic_report_subscription_available_channels(list(notifications = list())), character(0))
  expect_equal(episodic_report_subscription_available_channels(list()), character(0))
})

test_that("subscription events insert/read round-trip and derive the current schedule", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db_path)
  })

  user_id <- episodic_db_app_user_insert(
    con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )
  cluster_ids <- episodic_add_manual_cluster(
    db_path = db_path,
    user_id = user_id,
    pathogen = "Measles virus",
    level = "pathogen_area",
    first_day = "2025-01-10",
    last_day = "2025-01-20",
    region_code = "GR",
    case_dates = list(as.Date(c("2025-01-10", "2025-01-14"))),
    pc = list(c("9711AA", "9711AB"))
  )
  cluster_id <- cluster_ids[1]

  expect_null(episodic_report_subscription_current(con, cluster_id))

  episodic_db_report_subscription_event_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    action = "set",
    interval_days = 7L,
    recipients_json = episodic_report_subscription_recipients_to_json(c("a@example.com")),
    channel = "smtp",
    include_linelist = FALSE
  )
  current <- episodic_report_subscription_current(con, cluster_id)
  expect_equal(current$interval_days, 7L)
  expect_equal(current$recipients, "a@example.com")
  expect_equal(current$channel, "smtp")
  expect_false(current$include_linelist)

  # Setting again (e.g. changing the interval) supersedes, never updates.
  episodic_db_report_subscription_event_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    action = "set",
    interval_days = 14L,
    recipients_json = episodic_report_subscription_recipients_to_json(c("a@example.com", "b@example.com")),
    channel = "sendmail",
    include_linelist = TRUE
  )
  events <- episodic_db_report_subscription_events(con, cluster_id)
  expect_equal(nrow(events), 2L)
  current <- episodic_report_subscription_current(con, cluster_id)
  expect_equal(current$interval_days, 14L)
  expect_setequal(current$recipients, c("a@example.com", "b@example.com"))

  # Cancelling removes the active schedule.
  episodic_db_report_subscription_event_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    action = "cancel"
  )
  expect_null(episodic_report_subscription_current(con, cluster_id))

  events_all <- episodic_db_report_subscription_events_all(con)
  expect_equal(nrow(events_all), 3L)
})

test_that("subscription sends insert/read round-trip", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db_path)
  })

  user_id <- episodic_db_app_user_insert(
    con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "epidemiologist"
  )
  cluster_ids <- episodic_add_manual_cluster(
    db_path = db_path,
    user_id = user_id,
    pathogen = "Measles virus",
    level = "pathogen_area",
    first_day = "2025-01-10",
    last_day = "2025-01-20",
    region_code = "GR",
    case_dates = list(as.Date(c("2025-01-10", "2025-01-14"))),
    pc = list(c("9711AA", "9711AB"))
  )
  cluster_id <- cluster_ids[1]
  event_id <- episodic_db_report_subscription_event_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    action = "set",
    interval_days = 7L,
    recipients_json = episodic_report_subscription_recipients_to_json(c("a@example.com")),
    channel = "smtp",
    include_linelist = FALSE
  )
  run_id <- episodic_db_run_start(con, host = "test", account = "test", run_date = Sys.Date())
  report_id <- episodic_db_report_render_insert(
    con,
    cluster_id = cluster_id,
    user_id = NA,
    file_path = "report.html",
    file_sha256 = strrep("0", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = 1L
  )

  episodic_db_report_subscription_send_insert(
    con,
    cluster_id = cluster_id,
    subscription_event_id = event_id,
    run_id = run_id,
    report_id = NA,
    recipients_json = episodic_report_subscription_recipients_to_json(c("a@example.com")),
    status = "failed",
    error_text = "channel unreachable",
    final = FALSE
  )
  episodic_db_report_subscription_send_insert(
    con,
    cluster_id = cluster_id,
    subscription_event_id = event_id,
    run_id = run_id,
    report_id = report_id,
    recipients_json = episodic_report_subscription_recipients_to_json(c("a@example.com")),
    status = "sent",
    final = TRUE
  )

  sends <- episodic_db_report_subscription_sends(con, cluster_id)
  expect_equal(nrow(sends), 2L)
  # most recent first
  expect_equal(sends$status[1], "sent")
  expect_equal(sends$final[1], 1L)
  expect_equal(sends$status[2], "failed")
  expect_equal(sends$error_text[2], "channel unreachable")

  sends_all <- episodic_db_report_subscription_sends_all(con)
  expect_equal(nrow(sends_all), 2L)
})
