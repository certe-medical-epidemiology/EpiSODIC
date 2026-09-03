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

# access.require_login. The property under test throughout is not "the
# data is hidden" but "the data was never rendered": a dashboard that
# renders its clusters and then covers them with a modal has already sent
# them to the browser, where a developer console reaches them in seconds.
# Every assertion below therefore reads what the *server* produced.

# An instance config setting access.require_login, pointed at by
# EPISODIC_CONFIG for the duration of `code`.
with_require_login <- function(value, code) {
  path <- tempfile(fileext = ".yaml")
  writeLines(
    c("access:", paste0("  require_login: ", if (value) "true" else "false")),
    path
  )
  old <- Sys.getenv("EPISODIC_CONFIG", unset = NA)
  on.exit(
    {
      unlink(path)
      if (is.na(old)) {
        Sys.unsetenv("EPISODIC_CONFIG")
      } else {
        Sys.setenv(EPISODIC_CONFIG = old)
      }
    },
    add = TRUE
  )
  Sys.setenv(EPISODIC_CONFIG = path)
  force(code)
}

# A database with one open cluster of a distinctively-named pathogen, and
# one account to sign in with.
require_login_db <- function(pathogen = "Norovirus") {
  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  user_id <- episodic_db_app_user_insert(
    con,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    sodium::password_store("initial123")
  )
  DBI::dbExecute(
    con,
    "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?",
    params = list(user_id)
  )
  institution_id <- episodic_test_institution(con, "hosp-require-login")
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_institution",
      pathogen,
      institution_id = institution_id
    ),
    level = "pathogen_institution",
    pathogen = pathogen,
    institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = stream_id,
    first_day = "2025-01-01",
    last_day = "2025-01-02",
    n_cases = 3,
    priority_score = 50,
    detector_agreement = 1,
    run_id = run_id
  )
  DBI::dbDisconnect(con)
  list(db_path = db_path, cluster_id = cluster_id, pathogen = pathogen)
}

test_that("require_login is read defensively, and off unless unambiguously on", {
  expect_false(episodic_app_require_login(list()))
  expect_false(episodic_app_require_login(list(access = list())))
  expect_false(episodic_app_require_login(list(access = list(
    require_login = FALSE
  ))))
  expect_true(episodic_app_require_login(list(access = list(
    require_login = TRUE
  ))))
  # A malformed value must never silently lock an instance out of its own
  # dashboard, nor silently open one that asked to be closed.
  expect_false(episodic_app_require_login(list(access = list(
    require_login = "perhaps"
  ))))
  expect_false(episodic_app_require_login(list(access = list(
    require_login = NA
  ))))
  expect_true(episodic_app_require_login(list(access = list(
    require_login = "TRUE"
  ))))
})

test_that("the shipped default leaves the app open, as it has always been", {
  config <- episodic_config_resolve(episodic_config_path = NA)
  expect_false(is.null(config$access))
  expect_false(episodic_app_require_login(config))
})

test_that("an instance YAML can close the app, through the ordinary config overlay", {
  with_require_login(TRUE, {
    expect_true(episodic_app_require_login(episodic_config_resolve()))
  })
  with_require_login(FALSE, {
    expect_false(episodic_app_require_login(episodic_config_resolve()))
  })
})

test_that("access is granted to a signed-in user, and to anyone when login is not required", {
  user <- list(user_id = 1L, full_name = "Jane Doe")
  expect_true(episodic_app_access_granted(FALSE, NULL))
  expect_true(episodic_app_access_granted(FALSE, user))
  expect_true(episodic_app_access_granted(TRUE, user))
  expect_false(episodic_app_access_granted(TRUE, NULL))
})

test_that("access.require_login is not part of config_hash, so it cannot make two runs look different", {
  open_config <- episodic_config_resolve(episodic_config_path = NA)
  closed_config <- open_config
  closed_config$access$require_login <- TRUE
  expect_equal(
    episodic_config_hash(open_config)$hash,
    episodic_config_hash(closed_config)$hash
  )
  # and it is kept out of the stored snapshot along with the hash
  expect_false(grepl(
    "require_login",
    episodic_config_hash(closed_config)$snapshot,
    fixed = TRUE
  ))
})

test_that("an anonymous session on a login-required instance is served no data at all", {
  skip_if_not_installed("sodium")
  env <- require_login_db()
  on.exit(unlink(env$db_path), add = TRUE)

  with_require_login(TRUE, {
    server <- episodic_app_server_factory(env$db_path, lang = "en")
    shiny::testServer(server, {
      session$flushReact()

      # Not one screen, not the navigation, not the status strip. What
      # the server produced is what a browser's developer tools can
      # reach, so these are the assertions that matter.
      main <- paste(output$main_view, collapse = "\n")
      expect_false(grepl(env$pathogen, main, fixed = TRUE))
      expect_true(grepl("episodic-locked-screen", main, fixed = TRUE))
      expect_true(grepl(
        episodic_tr("auth.required_title", lang = "en"),
        main,
        fixed = TRUE
      ))

      expect_equal(paste(output$nav_links, collapse = ""), "")
      expect_equal(paste(output$status_strip, collapse = ""), "")
      for (pane in list(
        output$rail_pane,
        output$dossier_pane,
        output$assessment_pane,
        output$archive_screen
      )) {
        expect_false(grepl(env$pathogen, paste(pane, collapse = "\n")))
      }

      # Switching view from the client reaches no data either: an input
      # is whatever the client says it is, so every screen has to be
      # behind the gate, not merely unreachable through the nav.
      for (v in c(
        "streams",
        "archive",
        "activity",
        "pathogen",
        "performance",
        "info",
        "settings"
      )) {
        session$setInputs(nav_view = v)
        session$flushReact()
        rendered <- paste(output$main_view, collapse = "\n")
        expect_true(
          grepl("episodic-locked-screen", rendered, fixed = TRUE),
          info = v
        )
        expect_false(grepl(env$pathogen, rendered, fixed = TRUE), info = v)
      }

      # The sign-in control itself stays, or there is no way in.
      expect_true(grepl(
        episodic_tr("auth.signin", lang = "en"),
        paste(output$auth_control, collapse = "\n")
      ))

      DBI::dbDisconnect(con)
    })
  })
})

test_that("signing in on a login-required instance opens the app, and signing out closes it again", {
  skip_if_not_installed("sodium")
  env <- require_login_db()
  on.exit(unlink(env$db_path), add = TRUE)

  with_require_login(TRUE, {
    server <- episodic_app_server_factory(env$db_path, lang = "en")
    shiny::testServer(server, {
      session$flushReact()
      expect_false(grepl(
        env$pathogen,
        paste(output$main_view, collapse = "\n"),
        fixed = TRUE
      ))

      session$setInputs(
        auth_username_val = "jdoe",
        auth_password_val = "initial123"
      )
      session$setInputs(auth_login_submit = 1)
      session$flushReact()

      expect_true(grepl(
        env$pathogen,
        paste(output$rail_pane, collapse = "\n"),
        fixed = TRUE
      ))
      expect_true(grepl(
        episodic_tr("nav.clusters", lang = "en"),
        paste(output$nav_links, collapse = "\n"),
        fixed = TRUE
      ))
      expect_false(grepl(
        "episodic-locked-screen",
        paste(output$main_view, collapse = "\n"),
        fixed = TRUE
      ))

      session$setInputs(auth_signout = 1)
      session$flushReact()

      expect_true(grepl(
        "episodic-locked-screen",
        paste(output$main_view, collapse = "\n"),
        fixed = TRUE
      ))
      expect_false(grepl(
        env$pathogen,
        paste(output$rail_pane, collapse = "\n"),
        fixed = TRUE
      ))
      expect_equal(paste(output$nav_links, collapse = ""), "")

      DBI::dbDisconnect(con)
    })
  })
})

test_that("an anonymous client cannot pull run detail out of a login-required instance by setting the input itself", {
  skip_if_not_installed("sodium")
  env <- require_login_db()
  on.exit(unlink(env$db_path), add = TRUE)

  with_require_login(TRUE, {
    server <- episodic_app_server_factory(env$db_path, lang = "en")
    shiny::testServer(server, {
      session$flushReact()
      # An observer runs whenever its input arrives, and any client can
      # set any input - so the run-detail modal is a data path that the
      # navigation being absent does nothing to close.
      session$setInputs(activity_run_detail = 1)
      expect_silent(session$flushReact())
      expect_false(grepl(
        env$pathogen,
        paste(output$main_view, collapse = "\n"),
        fixed = TRUE
      ))
      DBI::dbDisconnect(con)
    })
  })
})

test_that("with require_login off, the app behaves exactly as it always has for an anonymous reader", {
  skip_if_not_installed("sodium")
  env <- require_login_db()
  on.exit(unlink(env$db_path), add = TRUE)

  with_require_login(FALSE, {
    server <- episodic_app_server_factory(env$db_path, lang = "en")
    shiny::testServer(server, {
      session$flushReact()
      expect_true(grepl(
        env$pathogen,
        paste(output$rail_pane, collapse = "\n"),
        fixed = TRUE
      ))
      expect_true(grepl(
        episodic_tr("nav.clusters", lang = "en"),
        paste(output$nav_links, collapse = "\n"),
        fixed = TRUE
      ))
      expect_false(grepl(
        "episodic-locked-screen",
        paste(output$main_view, collapse = "\n"),
        fixed = TRUE
      ))
      DBI::dbDisconnect(con)
    })
  })
})

test_that("the sign-in modal cannot be dismissed when there is nothing behind it", {
  dismissible <- as.character(episodic_ui_login_modal(lang = "en"))
  expect_true(grepl("data-bs-dismiss", dismissible, fixed = TRUE))
  expect_true(grepl(
    episodic_tr("misc.close", lang = "en"),
    dismissible,
    fixed = TRUE
  ))

  locked <- as.character(episodic_ui_login_modal(
    dismissible = FALSE,
    lang = "en"
  ))
  expect_false(grepl("data-bs-dismiss", locked, fixed = TRUE))
  expect_true(grepl(
    episodic_tr("auth.required_note", lang = "en"),
    locked,
    fixed = TRUE
  ))
  # still a way to submit, or the modal is a dead end
  expect_true(grepl("auth_login_submit", locked, fixed = TRUE))
})

test_that("the locked screen and the access panel render in every shipped language", {
  for (lang in c("en", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    screen <- as.character(episodic_ui_locked_screen(lang = lang))
    expect_false(grepl("[[", screen, fixed = TRUE), info = lang)
    expect_true(grepl("auth_show_login", screen, fixed = TRUE), info = lang)

    for (required in c(TRUE, FALSE)) {
      panel <- as.character(episodic_ui_settings_access_panel(
        list(access = list(require_login = required)),
        lang = lang
      ))
      expect_false(grepl("[[", panel, fixed = TRUE), info = lang)
    }
  }
})

test_that("the Settings screen reports which way access is set, and does not file it under detection", {
  open_panel <- as.character(episodic_ui_settings_access_panel(
    list(access = list(require_login = FALSE)),
    lang = "en"
  ))
  expect_true(grepl(
    episodic_tr("settings.access.open", lang = "en"),
    open_panel,
    fixed = TRUE
  ))

  closed_panel <- as.character(episodic_ui_settings_access_panel(
    list(access = list(require_login = TRUE)),
    lang = "en"
  ))
  expect_true(grepl(
    episodic_tr("settings.access.closed", lang = "en"),
    closed_panel,
    fixed = TRUE
  ))

  # `access` belongs to its own panel, not to the detection-configuration
  # grid, which would label an access control as a detection setting.
  detection <- as.character(episodic_ui_settings_detection_panel(
    episodic_config_resolve(episodic_config_path = NA),
    lang = "en"
  ))
  expect_false(grepl("Require login", detection, fixed = TRUE))
  expect_true(grepl("Reconciliation", detection, fixed = TRUE))
})
