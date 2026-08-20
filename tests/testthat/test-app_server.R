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
