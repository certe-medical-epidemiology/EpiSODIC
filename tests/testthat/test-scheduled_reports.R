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

test_that("episodic_scheduled_report_terminal() is TRUE for closed, merged and suppressed clusters", {
  expect_true(episodic_scheduled_report_terminal(
    data.frame(state = "closed", merged_into = NA_integer_, suppressed_by = NA_integer_)
  ))
  expect_true(episodic_scheduled_report_terminal(
    data.frame(state = "monitoring", merged_into = 5L, suppressed_by = NA_integer_)
  ))
  expect_true(episodic_scheduled_report_terminal(
    data.frame(state = "monitoring", merged_into = NA_integer_, suppressed_by = 5L)
  ))
  expect_false(episodic_scheduled_report_terminal(
    data.frame(state = "monitoring", merged_into = NA_integer_, suppressed_by = NA_integer_)
  ))
})

#' Test fixture: a fresh database with one epidemiologist and one
#' 'manual' cluster (so no detection pipeline needs to run), with a
#' scheduled-report subscription already set on it.
#' @keywords internal
#' @noRd
episodic_test_scheduled_report_fixture <- function(interval_days = 7L, channel = "smtp") {
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
    interval_days = interval_days,
    recipients_json = episodic_report_subscription_recipients_to_json(c("colleague@example.com")),
    channel = channel,
    include_linelist = FALSE
  )
  list(
    db_path = db_path,
    con = con,
    user_id = user_id,
    cluster_id = cluster_id,
    event_id = event_id,
    config = list(notifications = list(channels = list(
      smtp = list(enabled = TRUE, host = "smtp.example.org", from = "episodic@example.org"),
      sendmail = list(enabled = FALSE)
    )))
  )
}

#' Test double for `episodic_report_render()`: writes a real, minimal
#' `episodic_report_render` row (so the foreign key on
#' `episodic_report_subscription_send.report_id` is satisfiable) without
#' needing Quarto installed.
#' @keywords internal
#' @noRd
episodic_test_fake_render <- function(con, cluster_id) {
  report_id <- episodic_db_report_render_insert(
    con,
    cluster_id = cluster_id,
    user_id = NA,
    file_path = "fake-report.html",
    file_sha256 = strrep("0", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = 1L
  )
  list(file_path = "fake-report.html", report_id = report_id, version_no = 1L)
}

test_that("episodic_scheduled_reports_dispatch() sends a due report and logs it as sent", {
  fx <- episodic_test_scheduled_report_fixture()
  on.exit({
    DBI::dbDisconnect(fx$con)
    unlink(fx$db_path)
  })
  run_id <- episodic_db_run_start(fx$con, host = "test", account = "test", run_date = Sys.Date())

  captured <- new.env()
  local_mocked_bindings(
    episodic_report_render = function(con, cluster_id, output_dir, user_id, include_linelist, episodic_config_path, lang) {
      episodic_test_fake_render(con, cluster_id)
    },
    episodic_notify_smtp = function(channel, message) {
      captured$channel <- channel
      captured$message <- message
      invisible(NULL)
    }
  )

  episodic_scheduled_reports_dispatch(
    fx$con,
    fx$config,
    run_id = run_id,
    run_date = Sys.Date(),
    db_path = fx$db_path,
    episodic_config_path = NA
  )

  expect_equal(captured$channel$to, "colleague@example.com")
  expect_equal(captured$message$attachment_path, "fake-report.html")

  sends <- episodic_db_report_subscription_sends(fx$con, fx$cluster_id)
  expect_equal(nrow(sends), 1L)
  expect_equal(sends$status[1], "sent")
  expect_false(is.na(sends$report_id[1]))
  expect_equal(sends$final[1], 0L)
})

test_that("episodic_scheduled_reports_dispatch() does nothing when no subscription is due", {
  fx <- episodic_test_scheduled_report_fixture(interval_days = 30L)
  on.exit({
    DBI::dbDisconnect(fx$con)
    unlink(fx$db_path)
  })
  run_id_1 <- episodic_db_run_start(fx$con, host = "test", account = "test", run_date = Sys.Date())

  local_mocked_bindings(
    episodic_report_render = function(con, cluster_id, ...) episodic_test_fake_render(con, cluster_id),
    episodic_notify_smtp = function(...) invisible(NULL)
  )
  episodic_scheduled_reports_dispatch(
    fx$con, fx$config,
    run_id = run_id_1, run_date = Sys.Date(),
    db_path = fx$db_path, episodic_config_path = NA
  )
  expect_equal(nrow(episodic_db_report_subscription_sends(fx$con, fx$cluster_id)), 1L)

  # Immediately again, same day: the 30-day interval has not elapsed.
  run_id_2 <- episodic_db_run_start(fx$con, host = "test", account = "test", run_date = Sys.Date())
  episodic_scheduled_reports_dispatch(
    fx$con, fx$config,
    run_id = run_id_2, run_date = Sys.Date(),
    db_path = fx$db_path, episodic_config_path = NA
  )
  expect_equal(nrow(episodic_db_report_subscription_sends(fx$con, fx$cluster_id)), 1L)
})

test_that("episodic_scheduled_reports_dispatch() logs a failed send when the channel is disabled, without erroring", {
  fx <- episodic_test_scheduled_report_fixture(channel = "sendmail") # disabled in fx$config
  on.exit({
    DBI::dbDisconnect(fx$con)
    unlink(fx$db_path)
  })
  run_id <- episodic_db_run_start(fx$con, host = "test", account = "test", run_date = Sys.Date())

  expect_no_error(
    episodic_scheduled_reports_dispatch(
      fx$con, fx$config,
      run_id = run_id, run_date = Sys.Date(),
      db_path = fx$db_path, episodic_config_path = NA
    )
  )

  sends <- episodic_db_report_subscription_sends(fx$con, fx$cluster_id)
  expect_equal(nrow(sends), 1L)
  expect_equal(sends$status[1], "failed")
  expect_match(sends$error_text[1], "not|no longer enabled")
})

test_that("episodic_scheduled_reports_dispatch() sends one final report on closure, then stops", {
  fx <- episodic_test_scheduled_report_fixture()
  on.exit({
    DBI::dbDisconnect(fx$con)
    unlink(fx$db_path)
  })
  episodic_db_cluster_state_insert(
    fx$con,
    cluster_id = fx$cluster_id,
    state = "closed",
    trigger = "closure",
    user_id = fx$user_id
  )

  sent_count <- 0L
  local_mocked_bindings(
    episodic_report_render = function(con, cluster_id, ...) episodic_test_fake_render(con, cluster_id),
    episodic_notify_smtp = function(...) {
      sent_count <<- sent_count + 1L
      invisible(NULL)
    }
  )

  run_id_1 <- episodic_db_run_start(fx$con, host = "test", account = "test", run_date = Sys.Date())
  episodic_scheduled_reports_dispatch(
    fx$con, fx$config,
    run_id = run_id_1, run_date = Sys.Date(),
    db_path = fx$db_path, episodic_config_path = NA
  )
  sends <- episodic_db_report_subscription_sends(fx$con, fx$cluster_id)
  expect_equal(nrow(sends), 1L)
  expect_equal(sends$final[1], 1L)
  expect_equal(sent_count, 1L)

  # The cron runs again the next day - the schedule must not fire again.
  run_id_2 <- episodic_db_run_start(fx$con, host = "test", account = "test", run_date = Sys.Date() + 1)
  episodic_scheduled_reports_dispatch(
    fx$con, fx$config,
    run_id = run_id_2, run_date = Sys.Date() + 1,
    db_path = fx$db_path, episodic_config_path = NA
  )
  expect_equal(nrow(episodic_db_report_subscription_sends(fx$con, fx$cluster_id)), 1L)
  expect_equal(sent_count, 1L)
})

test_that("episodic_scheduled_reports_dispatch() does nothing with no subscriptions at all", {
  db_path <- episodic_test_db_path()
  con <- episodic_db_connect(db_path)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db_path)
  })
  run_id <- episodic_db_run_start(con, host = "test", account = "test", run_date = Sys.Date())
  expect_no_error(
    episodic_scheduled_reports_dispatch(
      con, list(notifications = list(channels = list())),
      run_id = run_id, run_date = Sys.Date(),
      db_path = db_path, episodic_config_path = NA
    )
  )
})
