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
