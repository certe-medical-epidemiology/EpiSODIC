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

# The Import and export screen: admin-only in the card, in the render and in every
# handler, exercised through the real server with shiny::testServer().
# Helpers in helper-import_export.R.

app_import_export_setup <- function(db_path = episodic_test_db_path()) {
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  admin_id <- episodic_db_app_user_insert(
    con, "admin1", "Admin One", "admin@x.nl",
    sodium::password_store("adminpass1"),
    is_admin = TRUE
  )
  viewer_id <- episodic_db_app_user_insert(
    con, "viewer1", "View Er", "viewer@x.nl",
    sodium::password_store("viewerpass1"),
    role = "viewer"
  )
  DBI::dbExecute(
    con,
    "UPDATE episodic_app_user SET must_change = 0 WHERE user_id IN (?, ?)",
    params = list(admin_id, viewer_id)
  )
  list(db_path = db_path, admin_id = admin_id, viewer_id = viewer_id)
}

app_import_export_sign_in <- function(session, username, password) {
  session$setInputs(auth_username_val = username, auth_password_val = password)
  session$setInputs(auth_login_submit = 1)
  session$flushReact()
  session$setInputs(nav_view = "import_export")
  session$flushReact()
}

test_that("the Import and export card is an admin's only", {
  instance <- list(
    counts = list(streams = 1L, runs = 1L, users = 1L, clusters = 0L),
    archive_outbreaks = 0L,
    archive_epidemics = 0L,
    overall_ppv = NA_real_,
    version = "1.0.0"
  )
  expect_false(grepl('data-episodic-nav="import_export"', as.character(episodic_ui_instance_screen(instance, lang = "en")), fixed = TRUE))
  admin <- data.frame(user_id = 1L, is_admin = 1L)
  html <- as.character(episodic_ui_instance_screen(instance, current_user = admin, lang = "en"))
  expect_true(grepl('data-episodic-nav="import_export"', html, fixed = TRUE))
  expect_identical(episodic_app_nav_group("import_export"), "instance")
})

test_that("a non-admin sees no Import and export screen and can record nothing", {
  skip_if_not_installed("sodium")
  sender <- import_export_test_party()
  setup <- app_import_export_setup()
  on.exit(unlink(c(sender$dir, setup$db_path), recursive = TRUE))
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_import_export_sign_in(session, "viewer1", "viewerpass1")
    rendered <- paste(output$import_export_screen, collapse = "\n")
    expect_true(grepl(episodic_tr("settings.no_access", lang = "en"), rendered, fixed = TRUE))
    session$setInputs(
      impexp_so_name = "Sneaky",
      impexp_so_sender = paste(readLines(sender$signing_public), collapse = "\n"),
      impexp_so_loc = tempfile()
    )
    session$setInputs(impexp_source_save = 1)
    session$flushReact()
    expect_error(output$impexp_download)
    DBI::dbDisconnect(con)
  })
  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(nrow(episodic_db_import_source_events(con)), 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_export_event")$n, 0L)
})

test_that("an admin registers, tests and cancels an import source", {
  skip_if_not_installed("sodium")
  sender <- import_export_test_party()
  setup <- app_import_export_setup()
  file <- tempfile(fileext = ".episodic")
  on.exit(unlink(c(sender$dir, setup$db_path), recursive = TRUE))
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_import_export_sign_in(session, "admin1", "adminpass1")
    rendered <- paste(output$import_export_screen, collapse = "\n")
    expect_false(grepl(episodic_tr("settings.no_access", lang = "en"), rendered, fixed = TRUE))
    session$setInputs(
      impexp_so_name = "From partner",
      impexp_so_sender = paste(readLines(sender$signing_public), collapse = "\n"),
      impexp_so_loc = file
    )
    session$setInputs(impexp_source_save = 1)
    session$flushReact()
    rendered <- paste(output$import_export_screen, collapse = "\n")
    expect_true(grepl("From partner", rendered, fixed = TRUE))

    session$setInputs(impexp_action = list(action = "source_test", name = "From partner", nonce = 1))
    session$flushReact()
    rendered <- paste(output$import_export_screen, collapse = "\n")
    # The file is not there yet, and the screen says so.
    expect_true(grepl(episodic_tr("import_export.test.failed", name = "From partner", lang = "en"), rendered, fixed = TRUE))

    session$setInputs(impexp_action = list(action = "source_cancel", name = "From partner", nonce = 2))
    session$flushReact()
    DBI::dbDisconnect(con)
  })
  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  events <- episodic_db_import_source_events(con)
  expect_identical(events$action, c("set", "cancel"))
  expect_identical(events$user_id, rep(setup$admin_id, 2))
  expect_length(episodic_import_sources_current(con), 0L)
})

test_that("an admin's download is an export file, recorded as theirs", {
  skip_if_not_installed("sodium")
  sender <- import_export_test_party()
  receiver <- import_export_test_party()
  setup <- app_import_export_setup(import_export_test_sender_db())
  on.exit(unlink(c(sender$dir, receiver$dir, setup$db_path), recursive = TRUE))
  withr::local_envvar(EPISODIC_EXPORT_SIGNING_KEY = sender$signing_secret)
  server <- episodic_app_server_factory(setup$db_path, lang = "en")
  shiny::testServer(server, {
    app_import_export_sign_in(session, "admin1", "adminpass1")
    session$setInputs(
      impexp_dl_from = "2025-03-01",
      impexp_dl_to = "2025-03-31",
      impexp_dl_mode = "public_key",
      impexp_dl_recipients = paste(readLines(receiver$encryption_public), collapse = "\n")
    )
    path <- output$impexp_download
    imported <- suppressWarnings(episodic_cases_import(
      path,
      key = receiver$encryption_secret,
      trusted_senders = sender$signing_public
    ))
    expect_equal(nrow(imported), 3L)
    DBI::dbDisconnect(con)
  })
  con <- episodic_db_connect(setup$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  row <- DBI::dbGetQuery(con, "SELECT initiated_by, status, user_id FROM episodic_export_event")
  expect_identical(row$initiated_by, "download")
  expect_identical(row$status, "written")
  expect_identical(row$user_id, setup$admin_id)
})

test_that("the Import and export screen renders in every shipped language, with its records", {
  sender <- import_export_test_party()
  on.exit(unlink(sender$dir, recursive = TRUE))
  data <- list(
    instance_id = "abcdef0123",
    schedules = list(`To partner` = list(
      schedule_name = "To partner", event_id = 1L, interval_days = 7L, window_days = 28L,
      mode = "public_key", recipients = '["k"]', destination = "/srv/out.episodic",
      destination_env = NA_character_
    )),
    sources = list(`From partner` = list(
      source_name = "From partner", mode = "passphrase",
      trusted_sender = import_export_test_key_line(sender$signing_public),
      location = NA_character_, location_env = "PARTNER_URL"
    )),
    exports = data.frame(
      export_id = 1L, exported_at = "2025-04-30T02:00:00Z", initiated_by = "schedule",
      schedule_name = "To partner", user_id = NA_integer_, host = "h", account = "a",
      status = "written", mode = "public_key", date_from = "2025-04-03", date_to = "2025-04-30",
      n_cases = 1234L, recipient_fingerprints = paste0('["', strrep("a", 64), '"]'),
      file_sha256 = strrep("b", 64), destination = "/srv/out.episodic", error_text = NA_character_,
      stringsAsFactors = FALSE
    ),
    imports = data.frame(
      import_id = 1L, imported_at = "2025-04-30T02:00:00Z", run_id = 12L,
      source_name = "From partner", status = "failed", error_text = "nope",
      sender_instance_id = NA_character_, signer_fingerprint = NA_character_,
      date_from = NA_character_, date_to = NA_character_, n_cases = NA_integer_,
      file_sha256 = NA_character_, stringsAsFactors = FALSE
    ),
    users = c(`1` = "Admin One"),
    signing_fingerprint = strrep("c", 64)
  )
  test <- structure(
    list(name = "To partner", kind = "schedule", ok = FALSE, lines = list(
      episodic_import_export_test_line("env", FALSE, "import_export.test.env_missing", variable = "X"),
      episodic_import_export_test_line("upload", NA, "import_export.test.upload_unknown")
    )),
    class = "episodic_import_export_test"
  )
  for (lang in c("en", "nl", "de", "fr", "es", "zh", "hi", "ar", "en-US", "es-419")) {
    html <- as.character(episodic_ui_import_export_screen(
      data,
      episodic_config_resolve(),
      lang = lang,
      message = list(type = "error", text = "x"),
      test = test
    ))
    # No placeholder left unfilled.
    expect_false(grepl("\\{[a-z_]+\\}", html), info = lang)
    expect_true(grepl("abcdef0123", html, fixed = TRUE), info = lang)
    expect_true(grepl("env:PARTNER_URL", html, fixed = TRUE), info = lang)
    # The case count goes through the language's own number format.
    expect_true(grepl(episodic_format_number(1234L, lang = lang), html, fixed = TRUE), info = lang)
  }
})

test_that("pasted recipient keys are read one per key line, comments dropped", {
  expect_identical(
    episodic_app_import_export_key_lines("# comment\nepisodic-import-export-encryption-public-v1:aa\n\n  episodic-import-export-encryption-public-v1:bb  "),
    c("episodic-import-export-encryption-public-v1:aa", "episodic-import-export-encryption-public-v1:bb")
  )
  expect_identical(episodic_app_import_export_key_lines(NULL), character(0))
})
