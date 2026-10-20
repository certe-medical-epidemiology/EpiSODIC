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

# The Plugins screen (admin only, in the card, the render and every
# handler) and the plugin buttons on the dossiers (epidemiologists only),
# through the real server with shiny::testServer(). Helpers in
# helper-plugins.R.

app_plugins_setup <- function(env = parent.frame()) {
  instance <- plugin_test_instance(env = env)
  withr::local_envvar(instance$env, .local_envir = env)
  setup <- app_read_setup()
  db_path <- setup$con@dbname
  users <- list(
    admin = episodic_db_app_user_insert(setup$con, "admin1", "Admin One", "a@x.nl", sodium::password_store("adminpass1"), is_admin = TRUE),
    epi = episodic_db_app_user_insert(setup$con, "epi1", "Epi One", "e@x.nl", sodium::password_store("epipass111")),
    viewer = episodic_db_app_user_insert(setup$con, "viewer1", "View Er", "v@x.nl", sodium::password_store("viewerpass1"), role = "viewer")
  )
  DBI::dbExecute(setup$con, "UPDATE episodic_app_user SET must_change = 0")
  DBI::dbDisconnect(setup$con)
  withr::defer(unlink(db_path), envir = env)
  c(instance, list(db_path = db_path, cluster_id = setup$cluster_id, users = users))
}

app_plugins_sign_in <- function(session, username, password) {
  session$setInputs(auth_username_val = username, auth_password_val = password)
  session$setInputs(auth_login_submit = 1)
  session$flushReact()
}

app_plugins_events <- function(db_path) {
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "SELECT * FROM episodic_plugin_run_event")
}

app_plugins_reporting <- function(message = "Shared", ...) {
  plugin_test_spec(
    steps = list(plugin_test_step(paste0("cat('message=", message, "\\n', file = Sys.getenv('EPISODIC_RESULT'))"))),
    ...
  )
}

test_that("the Plugins card is an admin's only", {
  instance <- list(
    counts = list(streams = 1L, runs = 1L, users = 1L, clusters = 0L),
    archive_outbreaks = 0L, archive_epidemics = 0L, overall_ppv = NA_real_, version = "1.0.0"
  )
  expect_false(grepl('data-episodic-nav="plugins"', as.character(episodic_ui_instance_screen(instance, lang = "en")), fixed = TRUE))
  admin <- data.frame(user_id = 1L, is_admin = 1L)
  expect_true(grepl('data-episodic-nav="plugins"', as.character(episodic_ui_instance_screen(instance, current_user = admin, lang = "en")), fixed = TRUE))
  expect_identical(episodic_app_nav_group("plugins"), "instance")
})

test_that("a non-admin sees no Plugins screen and cannot enable a plugin", {
  skip_if_not_installed("sodium")
  setup <- app_plugins_setup()
  plugin_test_write(setup$dir, "share-regional")
  before <- readLines(setup$config_path)
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "epi1", "epipass111")
    session$setInputs(nav_view = "plugins")
    session$flushReact()
    expect_true(grepl(episodic_tr("settings.no_access", lang = "en"), paste(output$plugins_screen, collapse = ""), fixed = TRUE))
    session$setInputs(plugins_action = list(action = "confirm_enable", plugin = "share-regional", nonce = 1))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  expect_identical(readLines(setup$config_path), before)
})

test_that("an admin installs, enables, disables and uninstalls a plugin from the screen", {
  skip_if_not_installed("sodium")
  setup <- app_plugins_setup()
  source <- plugin_test_write(tempfile("src_"), "share-regional")
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "admin1", "adminpass1")
    session$setInputs(nav_view = "plugins")
    session$flushReact()
    expect_true(grepl(episodic_tr("plugins.list.empty", lang = "en"), paste(output$plugins_screen, collapse = ""), fixed = TRUE))

    session$setInputs(plugins_install_source = source)
    session$setInputs(plugins_action = list(action = "install", plugin = "", nonce = 1))
    session$flushReact()
    rendered <- paste(output$plugins_screen, collapse = "")
    expect_true(grepl(episodic_tr("plugins.install.done", name = "share-regional", lang = "en"), rendered, fixed = TRUE))
    expect_true(grepl(episodic_tr("plugins.status.disabled", lang = "en"), rendered, fixed = TRUE))

    session$setInputs(plugins_action = list(action = "confirm_enable", plugin = "share-regional", nonce = 2))
    session$flushReact()
    expect_identical(names(yaml::read_yaml(setup$config_path)$plugins$enabled), "share-regional")
    expect_true(grepl(episodic_tr("plugins.status.enabled", lang = "en"), paste(output$plugins_screen, collapse = ""), fixed = TRUE))

    session$setInputs(plugins_action = list(action = "disable", plugin = "share-regional", nonce = 3))
    session$flushReact()
    expect_length(yaml::read_yaml(setup$config_path)$plugins$enabled, 0)

    session$setInputs(plugins_action = list(action = "confirm_uninstall", plugin = "share-regional", nonce = 4))
    session$flushReact()
    expect_false(file.exists(file.path(setup$dir, "share-regional.yaml")))
    DBI::dbDisconnect(con)
  })
  expect_identical(readLines(setup$config_path)[1], "# Test instance")
})

test_that("an install that is refused says why on the screen", {
  skip_if_not_installed("sodium")
  setup <- app_plugins_setup()
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "admin1", "adminpass1")
    session$setInputs(nav_view = "plugins")
    session$setInputs(plugins_install_source = file.path(tempdir(), "no-such-plugin.yaml"))
    session$setInputs(plugins_action = list(action = "install", plugin = "", nonce = 1))
    session$flushReact()
    expect_true(grepl("No plugin file exists", paste(output$plugins_screen, collapse = ""), fixed = TRUE))
    DBI::dbDisconnect(con)
  })
})

test_that("an epidemiologist runs a plugin from the outbreak dossier, and the run is recorded", {
  skip_if_not_installed("sodium")
  skip_if_not_installed("processx")
  setup <- app_plugins_setup()
  plugin_test_write(setup$dir, "share-regional", app_plugins_reporting())
  episodic_plugin_enable("share-regional", setup$config_path)
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "epi1", "epipass111")
    session$setInputs(rail_select = setup$cluster_id)
    session$flushReact()
    buttons <- paste(output$plugin_buttons_outbreak, collapse = "")
    expect_true(grepl('data-plugin="share-regional"', buttons, fixed = TRUE))
    expect_true(grepl(">Share<", buttons, fixed = TRUE))

    session$setInputs(plugin_button = list(plugin = "share-regional", cluster = setup$cluster_id, nonce = 1))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  events <- app_plugins_events(setup$db_path)
  expect_identical(nrow(events), 1L)
  expect_identical(events$initiated_by, "dashboard")
  expect_identical(events$user_id, setup$users$epi)
  expect_identical(events$cluster_id, setup$cluster_id)
  expect_identical(events$status, "success")
})

test_that("a viewer is shown no plugin buttons, and a forged click runs nothing", {
  skip_if_not_installed("sodium")
  setup <- app_plugins_setup()
  plugin_test_write(setup$dir, "share-regional", app_plugins_reporting())
  episodic_plugin_enable("share-regional", setup$config_path)
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "viewer1", "viewerpass1")
    session$setInputs(rail_select = setup$cluster_id)
    session$flushReact()
    expect_false(grepl("share-regional", paste(output$plugin_buttons_outbreak, collapse = ""), fixed = TRUE))
    session$setInputs(plugin_button = list(plugin = "share-regional", cluster = setup$cluster_id, nonce = 1))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  expect_identical(nrow(app_plugins_events(setup$db_path)), 0L)
})

test_that("a click for a cluster other than the one on screen, or a plugin not enabled, runs nothing", {
  skip_if_not_installed("sodium")
  setup <- app_plugins_setup()
  plugin_test_write(setup$dir, "share-regional", app_plugins_reporting())
  plugin_test_write(setup$dir, "not-enabled", app_plugins_reporting())
  episodic_plugin_enable("share-regional", setup$config_path)
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "epi1", "epipass111")
    session$setInputs(rail_select = setup$cluster_id)
    session$flushReact()
    session$setInputs(plugin_button = list(plugin = "share-regional", cluster = setup$cluster_id + 999L, nonce = 1))
    session$flushReact()
    session$setInputs(plugin_button = list(plugin = "not-enabled", cluster = setup$cluster_id, nonce = 2))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  expect_identical(nrow(app_plugins_events(setup$db_path)), 0L)
})

test_that("a plugin that sends cases runs only after the confirmation naming its destination", {
  skip_if_not_installed("sodium")
  skip_if_not_installed("processx")
  setup <- app_plugins_setup()
  spec <- app_plugins_reporting(permissions = list("cluster_summary", "cluster_cases"), destination = "https://regional.example")
  plugin_test_write(setup$dir, "share-cases", spec)
  episodic_plugin_enable("share-cases", setup$config_path)
  modal <- episodic_ui_plugin_confirm_modal(episodic_plugin_read(file.path(setup$dir, "share-cases.yaml")), setup$cluster_id, lang = "en")
  expect_true(grepl("https://regional.example", as.character(modal), fixed = TRUE))
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "epi1", "epipass111")
    session$setInputs(rail_select = setup$cluster_id)
    session$flushReact()
    session$setInputs(plugin_button = list(plugin = "share-cases", cluster = setup$cluster_id, nonce = 1))
    session$flushReact()
    expect_identical(nrow(app_plugins_events(setup$db_path)), 0L)
    session$setInputs(plugin_confirm = list(plugin = "share-cases", cluster = setup$cluster_id, nonce = 2))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  events <- app_plugins_events(setup$db_path)
  expect_identical(events$permissions, '["cluster_summary","cluster_cases"]')
  expect_identical(events$destination, "https://regional.example")
})

test_that("a plugin's result is shown as text, never as markup, and its error output only to an admin", {
  plugin <- list(id = "p", name = "P", label = list(en = "Share"))
  outcome <- list(status = "failed", message = "<script>alert(1)</script>", link = "https://x.example/1", step = "Send", detail = "stack trace")
  for_epi <- as.character(episodic_ui_plugin_result_modal(plugin, outcome, is_admin = FALSE, lang = "en"))
  expect_false(grepl("<script>alert(1)</script>", for_epi, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", for_epi, fixed = TRUE))
  expect_false(grepl("stack trace", for_epi, fixed = TRUE))
  expect_true(grepl(episodic_tr("plugins.result.ask_admin", lang = "en"), for_epi, fixed = TRUE))
  for_admin <- as.character(episodic_ui_plugin_result_modal(plugin, outcome, is_admin = TRUE, lang = "en"))
  expect_true(grepl("stack trace", for_admin, fixed = TRUE))
})

test_that("the Plugins screen renders in every shipped language, with plugins and runs", {
  setup <- app_read_setup()
  withr::defer(DBI::dbDisconnect(setup$con))
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  plugin_test_write(instance$dir, "share-regional", plugin_test_spec(secrets = list("TOKEN"), source = "https://example.org/share-regional.yaml"))
  plugin_test_write(instance$dir, "broken", plugin_test_spec(trigger = "never"))
  episodic_config_set(c("plugins", "enabled", "gone"), list(permissions = list("cluster_summary")), instance$config_path)
  plugin <- episodic_plugin_read(file.path(instance$dir, "share-regional.yaml"))
  episodic_db_plugin_run_record(
    setup$con, plugin, "cluster_summary", "dashboard", episodic_now(),
    list(status = "success", message = "Shared", link = "https://x.example/1", step = NA, detail = NA),
    cluster_id = setup$cluster_id
  )
  data <- episodic_app_plugins(setup$con)
  for (lang in episodic_languages) {
    html <- as.character(episodic_ui_plugins_screen(data, lang = lang))
    expect_true(grepl(episodic_tr("nav.plugins", lang = lang), html, fixed = TRUE), info = lang)
    expect_false(grepl("[[", html, fixed = TRUE), info = lang)
  }
})

test_that("an epidemic plugin's button appears on the epidemic dossier and acts on that epidemic", {
  skip_if_not_installed("sodium")
  skip_if_not_installed("processx")
  setup <- app_plugins_setup()
  con <- episodic_db_connect(setup$db_path)
  stream_id <- DBI::dbGetQuery(con, "SELECT stream_id FROM episodic_cluster WHERE cluster_id = ?", params = list(setup$cluster_id))$stream_id
  epidemic_id <- episodic_db_cluster_insert(
    con,
    stream_id = stream_id, first_day = "2025-01-01", last_day = "2025-01-20", n_cases = 40,
    priority_score = 60, detector_agreement = 1
  )
  DBI::dbExecute(con, "UPDATE episodic_cluster SET scale = 'epidemic' WHERE cluster_id = ?", params = list(epidemic_id))
  DBI::dbDisconnect(con)
  plugin_test_write(setup$dir, "notify-region", app_plugins_reporting(placement = "epidemic"))
  episodic_plugin_enable("notify-region", setup$config_path)
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_plugins_sign_in(session, "epi1", "epipass111")
    session$setInputs(epidemic_select = epidemic_id)
    session$flushReact()
    expect_true(grepl('data-plugin="notify-region"', paste(output$plugin_buttons_epidemic, collapse = ""), fixed = TRUE))
    expect_false(grepl("notify-region", paste(output$plugin_buttons_outbreak, collapse = ""), fixed = TRUE))
    session$setInputs(plugin_button = list(plugin = "notify-region", cluster = epidemic_id, nonce = 1))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  events <- app_plugins_events(setup$db_path)
  expect_identical(events$cluster_id, epidemic_id)
  expect_identical(events$status, "success")
})
