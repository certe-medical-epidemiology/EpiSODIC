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

test_that("episodic_ui_report_schedule_section() shows the no-channel message when none is configured", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  local_mocked_bindings(
    episodic_config_resolve = function(...) list(notifications = list(channels = list()))
  )
  html <- as.character(episodic_ui_report_schedule_section(env$con, env$cluster_id, lang = "en"))
  expect_match(html, "No email-capable notification channel")
  expect_no_match(html, "report-schedule-interval")
})

test_that("episodic_ui_report_schedule_section() shows the setup form when a channel is available and no schedule exists", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  local_mocked_bindings(
    episodic_config_resolve = function(...) {
      list(notifications = list(channels = list(smtp = list(enabled = TRUE))))
    }
  )
  html <- as.character(episodic_ui_report_schedule_section(env$con, env$cluster_id, lang = "en"))
  expect_match(html, "report-schedule-interval")
  expect_match(html, "report-schedule-recipients")
  expect_match(html, "report_schedule_save")
})

test_that("episodic_ui_report_schedule_section() keeps an existing schedule visible and cancellable even if its channel was later disabled", {
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
  episodic_db_report_subscription_event_insert(
    env$con,
    cluster_id = env$cluster_id,
    user_id = user_id,
    action = "set",
    interval_days = 7L,
    recipients_json = episodic_report_subscription_recipients_to_json(c("colleague@example.com")),
    channel = "smtp",
    include_linelist = FALSE
  )

  # The channel it was set up with is no longer enabled anywhere.
  local_mocked_bindings(
    episodic_config_resolve = function(...) list(notifications = list(channels = list()))
  )
  html <- as.character(episodic_ui_report_schedule_section(env$con, env$cluster_id, lang = "en"))
  expect_match(html, "colleague@example.com")
  expect_match(html, "report_schedule_cancel")
  expect_no_match(html, "No email-capable notification channel")
})

test_that("episodic_ui_report_schedule_section() shows the finalised note instead of a cancel button once closed", {
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
  event_id <- episodic_db_report_subscription_event_insert(
    env$con,
    cluster_id = env$cluster_id,
    user_id = user_id,
    action = "set",
    interval_days = 7L,
    recipients_json = episodic_report_subscription_recipients_to_json(c("colleague@example.com")),
    channel = "smtp",
    include_linelist = FALSE
  )
  run_id <- episodic_db_run_start(env$con, host = "test", account = "test", run_date = Sys.Date())
  episodic_db_report_subscription_send_insert(
    env$con,
    cluster_id = env$cluster_id,
    subscription_event_id = event_id,
    run_id = run_id,
    report_id = NA,
    recipients_json = episodic_report_subscription_recipients_to_json(c("colleague@example.com")),
    status = "sent",
    final = TRUE
  )

  local_mocked_bindings(
    episodic_config_resolve = function(...) {
      list(notifications = list(channels = list(smtp = list(enabled = TRUE))))
    }
  )
  html <- as.character(episodic_ui_report_schedule_section(env$con, env$cluster_id, lang = "en"))
  expect_match(html, "final scheduled report has already been sent")
  expect_no_match(html, "report_schedule_cancel")
})
