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

stale_data_cases <- function() {
  episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 9
  )
}

stale_data_config_file <- function(lines) {
  path <- tempfile(fileext = ".yaml")
  writeLines(lines, path)
  path
}

# A run row and, optionally, one case first seen in it, written directly:
# the check reads only these two tables, and a whole run would be slower
# without showing anything more.
stale_data_seed_run <- function(con, run_date, origin_instance_id = NULL) {
  episodic_db_execute(
    con,
    "INSERT INTO episodic_detection_run (host, account, started_at, run_date, status)
     VALUES ('h', 'a', ?, ?, 'success')",
    params = list(paste0(run_date, "T05:00:00Z"), format(run_date))
  )
  run_id <- episodic_db_get_query(con, "SELECT MAX(run_id) AS id FROM episodic_detection_run")$id
  episodic_db_execute(
    con,
    "INSERT INTO episodic_case (source_key, lab_number, patient_key, sample_date,
                                pathogen, first_seen_run, origin_instance_id)
     VALUES (?, ?, 'P', ?, 'Norovirus', ?, ?)",
    params = list(
      paste0("S", run_id),
      paste0("L", run_id),
      format(run_date),
      run_id,
      if (is.null(origin_instance_id)) NA_character_ else origin_instance_id
    )
  )
  invisible(run_id)
}

test_that("the days without new cases count from the last run that took in a case of the instance's own", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  config <- episodic_test_config()
  stale_data_seed_run(con, as.Date("2026-11-06"))
  # An imported case arriving later does not make the own feed fresh.
  stale_data_seed_run(con, as.Date("2026-11-08"), origin_instance_id = "abcdef0123")

  check <- episodic_stale_data_check(con, config, as.Date("2026-11-09"))
  expect_identical(check$days, 3L)
  expect_identical(check$last_new_cases_date, as.Date("2026-11-06"))
  expect_identical(check$threshold, 2L)
  expect_true(check$stale)

  check <- episodic_stale_data_check(con, config, as.Date("2026-11-08"))
  expect_identical(check$days, 2L)
  expect_false(check$stale)
})

test_that("a run is judged only on runs dated on or before it", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  stale_data_seed_run(con, as.Date("2026-11-01"))
  stale_data_seed_run(con, as.Date("2026-11-10"))
  check <- episodic_stale_data_check(con, episodic_test_config(), as.Date("2026-11-05"))
  expect_identical(check$last_new_cases_date, as.Date("2026-11-01"))
  expect_identical(check$days, 4L)
})

test_that("an instance with no case of its own is not judged, rather than judged fresh or stale", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  config <- episodic_test_config()
  expect_null(episodic_stale_data_check(con, config, as.Date("2026-11-10")))
  stale_data_seed_run(con, as.Date("2026-11-01"), origin_instance_id = "abcdef0123")
  expect_null(episodic_stale_data_check(con, config, as.Date("2026-11-10")))
})

test_that("the check is switched off by a null threshold and refuses one it cannot read", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  stale_data_seed_run(con, as.Date("2026-11-01"))
  config <- episodic_test_config()
  config$stale_data$max_days_without_new_cases <- NULL
  expect_null(episodic_stale_data_check(con, config, as.Date("2026-11-10")))
  for (bad in list(0, 1.5, -1, NA_real_, "2", c(2, 3))) {
    config$stale_data$max_days_without_new_cases <- bad
    expect_error(
      episodic_stale_data_check(con, config, as.Date("2026-11-10")),
      "max_days_without_new_cases",
      info = deparse(bad)
    )
  }
})

test_that("an operator's configuration can switch the check off with ~", {
  path <- stale_data_config_file(c("stale_data:", "  max_days_without_new_cases: ~"))
  on.exit(unlink(path))
  config <- episodic_config_resolve(path)
  expect_null(episodic_stale_data_threshold(config))
})

test_that("the stale-data threshold is left out of config_hash", {
  config <- episodic_test_config()
  changed <- config
  changed$stale_data$max_days_without_new_cases <- 9L
  expect_identical(
    episodic_config_hash(config)$hash,
    episodic_config_hash(changed)$hash
  )
})

test_that("a scheduled run warns in its log once the feed has been quiet for too long", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path))
  run <- function(date) {
    testthat::capture_messages(episodic_run_cron(
      db_path = path,
      cases = stale_data_cases,
      run_date = date
    ))
  }
  log <- run(as.Date("2024-06-30"))
  expect_true(any(grepl("new cases last arrived in the run of 2024-06-30 \\(0 day", log)))
  log <- run(as.Date("2024-07-02"))
  expect_false(any(grepl("No new cases of this instance's own", log)))
  log <- run(as.Date("2024-07-03"))
  expect_true(any(grepl(
    "No new cases of this instance's own for 3 day\\(s\\).*run of 2024-06-30",
    log
  )))
})

test_that("a stale feed is notified through the configured channels on every run while it lasts", {
  path <- tempfile(fileext = ".sqlite")
  config_path <- stale_data_config_file(c(
    "notifications:",
    "  enabled: true",
    "  triggers:",
    "    stale_data: true",
    "  channels:",
    "    ntfy:",
    "      enabled: true",
    "      server: https://ntfy.example.org",
    "      topic: alerts"
  ))
  on.exit(unlink(c(path, config_path)))
  sent <- character(0)
  local_mocked_bindings(
    episodic_notify_dispatch = function(channels, message) {
      sent <<- c(sent, message$title)
      invisible(NULL)
    }
  )
  run <- function(date) {
    suppressMessages(episodic_run_cron(
      db_path = path,
      cases = stale_data_cases,
      episodic_config_path = config_path,
      run_date = date
    ))
  }
  run(as.Date("2024-06-30"))
  run(as.Date("2024-07-02"))
  expect_length(sent, 0)
  run(as.Date("2024-07-03"))
  run(as.Date("2024-07-04"))
  expect_identical(
    sent,
    c(
      "EpiSODIC: no new case data for 3 days",
      "EpiSODIC: no new case data for 4 days"
    )
  )
})

test_that("episodic_notify() sends the stale-data message only when its trigger is on and the run did not fail", {
  config <- episodic_test_config()
  config$notifications <- list(
    enabled = TRUE,
    triggers = list(stale_data = TRUE),
    channels = list(ntfy = list(enabled = TRUE, server = "x", topic = "y"))
  )
  stale <- list(
    days = 5L,
    last_new_cases_date = as.Date("2026-11-05"),
    threshold = 2L,
    stale = TRUE
  )
  sent <- 0L
  local_mocked_bindings(
    episodic_notify_dispatch = function(channels, message) {
      sent <<- sent + 1L
      invisible(NULL)
    }
  )
  notify <- function(config, result) {
    episodic_notify(NULL, config, result, 1L, as.Date("2026-11-10"), "host")
  }
  notify(config, list(status = "success", n_signals_new = 0L, stale_data = stale))
  expect_identical(sent, 1L)
  notify(config, list(status = "partial", n_signals_new = 0L, stale_data = stale))
  expect_identical(sent, 2L)
  notify(config, list(status = "failed", n_signals_new = 0L, stale_data = stale))
  notify(config, list(status = "success", n_signals_new = 0L, stale_data = NULL))
  fresh <- stale
  fresh$stale <- FALSE
  notify(config, list(status = "success", n_signals_new = 0L, stale_data = fresh))
  off <- config
  off$notifications$triggers$stale_data <- FALSE
  notify(off, list(status = "success", n_signals_new = 0L, stale_data = stale))
  expect_identical(sent, 2L)
})

test_that("the stale-data message is written in the reader's language, numbers and dates included", {
  check <- list(
    days = 1234L,
    last_new_cases_date = as.Date("2026-11-05"),
    threshold = 2L,
    stale = TRUE
  )
  msg <- episodic_notify_build_stale_data(
    check,
    as.Date("2026-11-10"),
    "lab<host>",
    "https://dash.example.org",
    lang = "nl"
  )
  expect_identical(msg$title, "EpiSODIC: al 1.234 dagen geen nieuwe casusgegevens")
  expect_match(msg$plain, episodic_format_date(as.Date("2026-11-05"), lang = "nl"), fixed = TRUE)
  expect_match(msg$html, "lab&lt;host&gt;", fixed = TRUE)
  expect_false(grepl("lab<host>", msg$html, fixed = TRUE))
  expect_match(msg$html, "https://dash.example.org", fixed = TRUE)
  expect_match(msg$slack_text, "<https://dash.example.org|Dashboard openen>", fixed = TRUE)
  expect_identical(msg$html, episodic_notify_html_document(msg$html_content))
  card <- jsonlite::fromJSON(msg$teams_card, simplifyVector = FALSE)
  facts <- card$attachments[[1]]$content$body[[2]]$facts
  expect_length(facts, 5)
})

test_that("the stale-data message names one day in the singular and leaves out an absent dashboard", {
  check <- list(
    days = 1L,
    last_new_cases_date = as.Date("2026-11-09"),
    threshold = 1L,
    stale = FALSE
  )
  msg <- episodic_notify_build_stale_data(check, as.Date("2026-11-10"), "h", NULL, lang = "en")
  expect_identical(msg$title, "EpiSODIC: no new case data for 1 day")
  expect_false(grepl("http", msg$plain, fixed = TRUE))
  expect_false(grepl("<http", msg$slack_text, fixed = TRUE))
})
