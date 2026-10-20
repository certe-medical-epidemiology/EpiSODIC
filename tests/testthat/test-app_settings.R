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

test_that("episodic_settings_get_path() reads nested and missing keys", {
  x <- list(auth = list(username = "a"))
  expect_equal(episodic_settings_get_path(x, "auth.username"), "a")
  expect_null(episodic_settings_get_path(x, "auth.password"))
  expect_null(episodic_settings_get_path(x, "nope.at.all"))
  expect_null(episodic_settings_get_path(NULL, "auth.username"))
})

test_that("episodic_settings_set_path() writes nested keys, creating sublists as needed", {
  x <- episodic_settings_set_path(list(), "auth.username", "a")
  expect_equal(x$auth$username, "a")

  x <- episodic_settings_set_path(x, "auth.password", "p")
  expect_equal(x$auth$username, "a")
  expect_equal(x$auth$password, "p")

  x <- episodic_settings_set_path(x, "server", "https://ntfy.sh")
  expect_equal(x$server, "https://ntfy.sh")
})

test_that("episodic_settings_set_path() with NULL removes the leaf, like base[[key]] <- NULL always does", {
  x <- list(server = "https://ntfy.sh")
  x <- episodic_settings_set_path(x, "server", NULL)
  expect_false("server" %in% names(x))
})

test_that("every channel spec's field ids are unique within a channel", {
  specs <- episodic_settings_channel_specs()
  for (ch in names(specs)) {
    ids <- vapply(
      specs[[ch]]$fields,
      function(f) episodic_settings_field_id(ch, f$key),
      character(1)
    )
    expect_equal(length(ids), length(unique(ids)), info = ch)
  }
})

test_that("episodic_app_settings_build_notifications() assembles a full notifications list from submitted inputs", {
  input <- list(
    stg_notif_enabled = TRUE,
    stg_notif_dashboard_url = "https://dash.example.org",
    stg_notif_trigger_new_clusters = TRUE,
    stg_notif_trigger_run_failure = FALSE,
    stg_notif_ntfy_enabled = TRUE,
    stg_notif_ntfy_server = "https://ntfy.sh",
    stg_notif_ntfy_topic = "alerts",
    stg_notif_ntfy_priority = "4",
    stg_notif_ntfy_auth_username = "",
    stg_notif_ntfy_auth_password = "",
    stg_notif_smtp_enabled = FALSE,
    stg_notif_smtp_host = "",
    stg_notif_smtp_port = "",
    stg_notif_smtp_tls = TRUE,
    stg_notif_smtp_from = "",
    stg_notif_smtp_to = "",
    stg_notif_smtp_username = "",
    stg_notif_smtp_password = ""
  )

  notif <- episodic_app_settings_build_notifications(input, existing = NULL)

  expect_true(notif$enabled)
  expect_equal(notif$dashboard_url, "https://dash.example.org")
  expect_true(notif$triggers$new_clusters)
  expect_false(notif$triggers$run_failure)
  expect_true(notif$channels$ntfy$enabled)
  expect_equal(notif$channels$ntfy$server, "https://ntfy.sh")
  expect_equal(notif$channels$ntfy$topic, "alerts")
  expect_equal(notif$channels$ntfy$priority, 4)
  expect_null(notif$channels$ntfy$auth$username)
  expect_false(notif$channels$smtp$enabled)
  expect_null(notif$channels$smtp$host)
})

test_that("episodic_app_settings_build_notifications() parses a comma-separated recipient list", {
  input <- list(
    stg_notif_smtp_enabled = TRUE,
    stg_notif_smtp_to = "a@x.nl, b@x.nl,  c@x.nl"
  )
  notif <- episodic_app_settings_build_notifications(input, existing = NULL)
  expect_equal(unlist(notif$channels$smtp$to), c("a@x.nl", "b@x.nl", "c@x.nl"))
})

test_that("episodic_app_settings_build_notifications() keeps a secret unchanged when its field is left blank", {
  existing <- list(
    channels = list(
      teams = list(
        enabled = TRUE,
        webhook_url = "https://hooks.example/secret-token"
      )
    )
  )
  input <- list(
    stg_notif_teams_enabled = TRUE,
    stg_notif_teams_webhook_url = "" # left blank on screen
  )
  notif <- episodic_app_settings_build_notifications(input, existing = existing)
  expect_equal(
    notif$channels$teams$webhook_url,
    "https://hooks.example/secret-token"
  )
})

test_that("episodic_app_settings_build_notifications() overwrites a secret when a new value is submitted", {
  existing <- list(
    channels = list(
      teams = list(
        enabled = TRUE,
        webhook_url = "https://hooks.example/old-token"
      )
    )
  )
  input <- list(
    stg_notif_teams_enabled = TRUE,
    stg_notif_teams_webhook_url = "https://hooks.example/new-token"
  )
  notif <- episodic_app_settings_build_notifications(input, existing = existing)
  expect_equal(
    notif$channels$teams$webhook_url,
    "https://hooks.example/new-token"
  )
})

test_that("episodic_settings_flatten() flattens a nested config list to dotted keys", {
  flat <- episodic_settings_flatten(list(
    a = list(b = 1, c = list(d = 2)),
    e = c(1, 2, 3)
  ))
  expect_equal(flat[["a.b"]], "1")
  expect_equal(flat[["a.c.d"]], "2")
  expect_equal(flat[["e"]], "1, 2, 3")
})

# Rows the Settings screen once wrote to the database, which it no longer
# writes or applies.
settings_saved_row <- function(con, user_id, json, created_at) {
  episodic_db_execute(
    con,
    "INSERT INTO episodic_app_config_event (user_id, created_at, section, config_json)
     VALUES (?, ?, 'notifications', ?)",
    params = list(user_id, created_at, json)
  )
}

settings_admin <- function(con) {
  episodic_db_app_user_insert(con, "admin1", "Admin One", "a@x.nl", "hash", is_admin = TRUE)
}

test_that("episodic_db_app_config_latest() returns the most recent settings row, or NULL", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- settings_admin(con)
  expect_null(episodic_db_app_config_latest(con, "notifications"))
  settings_saved_row(con, user_id, '{"enabled":true}', "2026-01-01T10:00:00Z")
  settings_saved_row(con, user_id, '{"enabled":false}', "2026-01-02T10:00:00Z")
  expect_equal(episodic_db_app_config_latest(con, "notifications")$config_json, '{"enabled":false}')
})

test_that("settings once saved to the database are announced while they differ from the file, and not after", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  writeLines("notifications:\n  dashboard_url: https://file.example", path)
  withr::local_envvar(EPISODIC_CONFIG = path)
  user_id <- settings_admin(con)

  notices <- function() as.character(episodic_ui_settings_file_notices(con, episodic_config_resolve(), lang = "en"))
  expect_identical(notices(), "")

  settings_saved_row(con, user_id, '{"dashboard_url":"https://saved.example"}', "2026-01-01T10:00:00Z")
  expect_match(notices(), "no longer read", fixed = TRUE)

  episodic_config_set(c("notifications", "dashboard_url"), "https://saved.example", path)
  expect_identical(notices(), "")
})

test_that("without a configuration file the Settings screen says its settings cannot be saved", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  withr::local_envvar(EPISODIC_CONFIG = NA)
  expect_match(
    as.character(episodic_ui_settings_file_notices(con, episodic_config_resolve(), lang = "en")),
    "No configuration file is set",
    fixed = TRUE
  )
})

test_that("episodic_db_app_users() lists every account", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(episodic_db_app_users(con)), 0)

  episodic_db_app_user_insert(
    con,
    "b",
    "B",
    "b@x.nl",
    sodium::password_store("pw12345678")
  )
  episodic_db_app_user_insert(
    con,
    "a",
    "A",
    "a@x.nl",
    sodium::password_store("pw12345678")
  )

  users <- episodic_db_app_users(con)
  expect_equal(nrow(users), 2)
  expect_equal(users$username, c("a", "b")) # ORDER BY username
})
