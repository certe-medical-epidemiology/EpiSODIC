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

test_that("output$auth_control actually renders the sign-in link (anonymous) and updates on login/logout", {
  skip_if_not_installed("sodium")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episode_db_create(db_path)
  user_id <- episode_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl",
                                         sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episode_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))
  DBI::dbDisconnect(con)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$flushReact()
    rendered <- paste(output$auth_control, collapse = "\n")
    expect_true(grepl("Aanmelden", rendered))
    expect_false(grepl("Aangemeld als", rendered))

    session$setInputs(auth_username_val = "jdoe", auth_password_val = "initial123")
    session$setInputs(auth_login_submit = 1)
    session$flushReact()
    rendered <- paste(output$auth_control, collapse = "\n")
    expect_true(grepl("Aangemeld als Jane Doe", rendered))
    expect_true(grepl("Afmelden", rendered))

    session$setInputs(auth_signout = 1)
    session$flushReact()
    rendered <- paste(output$auth_control, collapse = "\n")
    expect_true(grepl("Aanmelden", rendered))
    expect_false(grepl("Aangemeld als", rendered))
  })
})

test_that("the report-render button actually surfaces a clear error via output$report_render_error when quarto is unavailable", {
  skip_if_not_installed("sodium")
  skip_if(episode_quarto_available(), "quarto CLI is actually available in this environment")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episode_db_create(db_path)
  user_id <- episode_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episode_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))

  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp-server-report", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_institution", "Norovirus", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Norovirus", institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episode_db_run_start(con, "h", "a")
  cluster_id <- episode_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(auth_username_val = "jdoe", auth_password_val = "initial123")
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(report_render_submit = cluster_id)
    session$flushReact()
    rendered <- paste(output$report_render_error, collapse = "\n")
    expect_true(grepl("Quarto CLI", rendered, fixed = TRUE))
  })
})

test_that("closing a cluster actually updates the rail and the Archief screen without leaving the clusters view", {
  skip_if_not_installed("sodium")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episode_db_create(db_path)
  user_id <- episode_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episode_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))

  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp-server-rail", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_institution", "Norovirus", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Norovirus", institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episode_db_run_start(con, "h", "a")
  cluster_id <- episode_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(auth_username_val = "jdoe", auth_password_val = "initial123")
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    rail_before <- paste(output$rail_pane, collapse = "\n")
    expect_true(grepl("Norovirus", rail_before))
    archive_before <- paste(output$archive_screen, collapse = "\n")
    expect_false(grepl("Norovirus", archive_before))

    # Closing without ever touching nav_view (input$rail_select stays on
    # "clusters" throughout) - this is what actually happened in the app:
    # neither the rail nor the Archief screen has any reason to notice a
    # write unless something explicitly invalidates them.
    session$setInputs(assess_close = cluster_id)
    session$flushReact()

    rail_after <- paste(output$rail_pane, collapse = "\n")
    expect_false(grepl("Norovirus", rail_after))
    archive_after <- paste(output$archive_screen, collapse = "\n")
    expect_true(grepl("Norovirus", archive_after))
  })
})

test_that("bulk_assess_submit applies one classification to several clusters in one submit", {
  skip_if_not_installed("sodium")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episode_db_create(db_path)
  user_id <- episode_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episode_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))

  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp-bulk", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  run_id <- episode_db_run_start(con, "h", "a")
  cluster_ids <- vapply(c("Norovirus", "Influenza"), function(pathogen) {
    stream_id <- episode_db_stream_upsert(
      con, stream_key = episode_stream_key("pathogen_institution", pathogen, institution_id = institution_id),
      level = "pathogen_institution", pathogen = pathogen, institution_id = institution_id,
      observed_date = "2025-01-01"
    )
    episode_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01", last_day = "2025-01-02",
                               n_cases = 3, priority_score = 50, detector_agreement = 1, run_id = run_id)
  }, integer(1))
  DBI::dbDisconnect(con)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(auth_username_val = "jdoe", auth_password_val = "initial123")
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    rail_before <- paste(output$rail_pane, collapse = "\n")
    expect_true(grepl("Norovirus", rail_before))
    expect_true(grepl("Influenza", rail_before))

    session$setInputs(bulk_assess_submit = list(
      cluster_ids = unname(cluster_ids), verdict = "artefact", rationale = "batch: both were reagent lot issues"
    ))
    session$flushReact()

    rail_after <- paste(output$rail_pane, collapse = "\n")
    expect_false(grepl("Norovirus", rail_after))  # artefact is terminal: both close, both leave the rail
    expect_false(grepl("Influenza", rail_after))

    con <- episode_db_connect(db_path)
    on.exit(DBI::dbDisconnect(con))
    events <- DBI::dbGetQuery(con, "SELECT cluster_id, verdict, rationale FROM episode_assessment_event")
    expect_equal(nrow(events), 2)
    expect_true(all(events$verdict == "artefact"))
    expect_true(all(grepl("reagent lot", events$rationale)))
  })
})

test_that("bulk_assess_submit is a no-op without a rationale, even if the client bypasses its own JS guard", {
  skip_if_not_installed("sodium")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episode_db_create(db_path)
  user_id <- episode_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episode_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))
  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp-bulk2", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_institution", "Norovirus", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Norovirus", institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episode_db_run_start(con, "h", "a")
  cluster_id <- episode_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(auth_username_val = "jdoe", auth_password_val = "initial123")
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(bulk_assess_submit = list(cluster_ids = cluster_id, verdict = "artefact", rationale = ""))
    session$flushReact()

    con <- episode_db_connect(db_path)
    on.exit(DBI::dbDisconnect(con))
    expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_assessment_event")$n, 0)
  })
})

test_that("output$main_view actually renders the info screen when nav_view is set to 'info'", {
  db_path <- tempfile(fileext = ".sqlite")
  episode_db_create(db_path)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(nav_view = "info")
    session$flushReact()
    rendered <- paste(output$main_view, collapse = "\n")
    expect_true(grepl("<code>same_place</code>", rendered, fixed = TRUE))
  })
})

test_that("output$main_view actually renders the performance screen when nav_view is set to 'performance'", {
  db_path <- tempfile(fileext = ".sqlite")
  episode_db_create(db_path)

  server <- episode_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(nav_view = "performance")
    session$flushReact()
    rendered <- paste(output$main_view, collapse = "\n")
    expect_true(grepl("Prestatie", rendered, fixed = TRUE))
    expect_true(grepl("Tijdigheid", rendered, fixed = TRUE))
  })
})
