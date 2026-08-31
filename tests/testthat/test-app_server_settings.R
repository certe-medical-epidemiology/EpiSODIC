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

# episodic_app_server_settings() re-checks episodic_user_is_admin() (via
# episodic_auth_refresh_user()) inside every observeEvent() handler,
# server-side, regardless of what the DOM/onclick happens to show - these
# tests exercise the real Shiny server (episodic_app_server_factory(),
# the same entry point production uses) end to end with shiny::testServer(),
# rather than only the pure helper functions, so a regression that
# silently dropped one of those checks would actually be caught.

app_settings_test_setup <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  admin_id <- episodic_db_app_user_insert(
    con,
    "admin1",
    "Admin One",
    "admin@x.nl",
    sodium::password_store("adminpass1"),
    is_admin = TRUE
  )
  viewer_id <- episodic_db_app_user_insert(
    con,
    "viewer1",
    "View Er",
    "viewer@x.nl",
    sodium::password_store("viewerpass1"),
    role = "viewer"
  )
  DBI::dbExecute(
    con,
    "UPDATE episodic_app_user SET must_change = 0 WHERE user_id IN (?, ?)",
    params = list(admin_id, viewer_id)
  )
  DBI::dbDisconnect(con)
  list(db_path = db_path, admin_id = admin_id, viewer_id = viewer_id)
}

test_that("a non-admin signed-in account sees settings.no_access and cannot save notification settings", {
  skip_if_not_installed("sodium")
  setup <- app_settings_test_setup()

  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    session$setInputs(
      auth_username_val = "viewer1",
      auth_password_val = "viewerpass1"
    )
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(nav_view = "settings")
    session$flushReact()
    rendered <- paste(output$settings_screen, collapse = "\n")
    expect_true(grepl("do not have access", rendered, fixed = TRUE))

    session$setInputs(stg_notif_enabled = TRUE)
    session$setInputs(settings_notif_save = 1)
    session$flushReact()

    DBI::dbDisconnect(con)
  })

  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_null(episodic_db_app_config_latest(con, "notifications"))
})

test_that("an is_admin account can save notification settings, which round-trip through episodic_config_resolve(con = ...)", {
  skip_if_not_installed("sodium")
  setup <- app_settings_test_setup()

  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    session$setInputs(
      auth_username_val = "admin1",
      auth_password_val = "adminpass1"
    )
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(nav_view = "settings")
    session$flushReact()
    rendered <- paste(output$settings_screen, collapse = "\n")
    expect_false(grepl("do not have access", rendered, fixed = TRUE))

    session$setInputs(stg_notif_enabled = TRUE)
    session$setInputs(stg_notif_ntfy_enabled = TRUE)
    session$setInputs(stg_notif_ntfy_server = "https://ntfy.sh")
    session$setInputs(stg_notif_ntfy_topic = "episodic-test")
    session$setInputs(settings_notif_save = 1)
    session$flushReact()

    rendered <- paste(output$settings_screen, collapse = "\n")
    expect_true(grepl("saved", rendered, fixed = TRUE))

    DBI::dbDisconnect(con)
  })

  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con))
  saved_config <- episodic_config_resolve(NA, con = con)
  expect_true(saved_config$notifications$enabled)
  expect_equal(saved_config$notifications$channels$ntfy$topic, "episodic-test")
})

test_that("an is_admin account can create a new dashboard account from the Settings screen", {
  skip_if_not_installed("sodium")
  setup <- app_settings_test_setup()

  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    session$setInputs(
      auth_username_val = "admin1",
      auth_password_val = "adminpass1"
    )
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(stg_user_new_username = "newperson")
    session$setInputs(stg_user_new_full_name = "New Person")
    session$setInputs(stg_user_new_email = "new@x.nl")
    session$setInputs(stg_user_new_password = "temporarypw1")
    session$setInputs(stg_user_new_role = "viewer")
    session$setInputs(settings_user_create = 1)
    session$flushReact()

    DBI::dbDisconnect(con)
  })

  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con))
  created <- episodic_db_user_by_username(con, "newperson")
  expect_false(is.null(created))
  expect_equal(created$role, "viewer")
})

test_that("an admin cannot revoke their own admin flag or deactivate their own account (self-lockout guard)", {
  skip_if_not_installed("sodium")
  setup <- app_settings_test_setup()

  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    session$setInputs(
      auth_username_val = "admin1",
      auth_password_val = "adminpass1"
    )
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(
      settings_user_admin_change = paste0(setup$admin_id, ":false")
    )
    session$flushReact()
    session$setInputs(nav_view = "settings")
    session$flushReact()
    rendered <- paste(output$settings_screen, collapse = "\n")
    expect_true(grepl("cannot remove your own", rendered, fixed = TRUE))

    DBI::dbDisconnect(con)
  })

  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con))
  admin_row <- episodic_db_user_by_id(con, setup$admin_id)
  expect_true(episodic_auth_is_admin(con, admin_row))
})

test_that("a signed-in admin session cannot keep writing settings after being deactivated by another admin mid-session", {
  skip_if_not_installed("sodium")
  setup <- app_settings_test_setup()
  con0 <- episodic_db_connect(setup$db_path)
  second_admin_id <- episodic_db_app_user_insert(
    con0,
    "admin2",
    "Admin Two",
    "admin2@x.nl",
    sodium::password_store("adminpass2"),
    is_admin = TRUE
  )
  DBI::dbExecute(
    con0,
    "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?",
    params = list(second_admin_id)
  )
  DBI::dbDisconnect(con0)

  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    session$setInputs(
      auth_username_val = "admin1",
      auth_password_val = "adminpass1"
    )
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    # A second admin, acting outside this session, deactivates admin1.
    con2 <- episodic_db_connect(setup$db_path)
    episodic_auth_set_active(con2, setup$admin_id, second_admin_id, FALSE)
    DBI::dbDisconnect(con2)

    # admin1's own cached current_user() reactiveVal still says "signed
    # in", but the settings write itself must still be refused.
    session$setInputs(stg_notif_enabled = TRUE)
    session$setInputs(settings_notif_save = 1)
    session$flushReact()

    DBI::dbDisconnect(con)
  })

  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_null(episodic_db_app_config_latest(con, "notifications"))
})
