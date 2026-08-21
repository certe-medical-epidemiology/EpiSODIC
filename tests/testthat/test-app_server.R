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
  con <- episodic_db_create(db_path)
  user_id <- episodic_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl",
                                         sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "nl")
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
  skip_if(episodic_quarto_available(), "quarto CLI is actually available in this environment")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  user_id <- episodic_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))

  institution_id <- episodic_db_institution_upsert(
    con, institution_key = digest::digest("hosp-server-report", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_institution", "Norovirus", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Norovirus", institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  cluster_id <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "nl")
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
  con <- episodic_db_create(db_path)
  user_id <- episodic_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))

  institution_id <- episodic_db_institution_upsert(
    con, institution_key = digest::digest("hosp-server-rail", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_institution", "Norovirus", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Norovirus", institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  cluster_id <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "nl")
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
  con <- episodic_db_create(db_path)
  user_id <- episodic_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))

  institution_id <- episodic_db_institution_upsert(
    con, institution_key = digest::digest("hosp-bulk", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  cluster_ids <- vapply(c("Norovirus", "Influenza"), function(pathogen) {
    stream_id <- episodic_db_stream_upsert(
      con, stream_key = episodic_stream_key("pathogen_institution", pathogen, institution_id = institution_id),
      level = "pathogen_institution", pathogen = pathogen, institution_id = institution_id,
      observed_date = "2025-01-01"
    )
    episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01", last_day = "2025-01-02",
                               n_cases = 3, priority_score = 50, detector_agreement = 1, run_id = run_id)
  }, integer(1))
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "nl")
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

    con <- episodic_db_connect(db_path)
    on.exit(DBI::dbDisconnect(con))
    events <- DBI::dbGetQuery(con, "SELECT cluster_id, verdict, rationale FROM episodic_assessment_event")
    expect_equal(nrow(events), 2)
    expect_true(all(events$verdict == "artefact"))
    expect_true(all(grepl("reagent lot", events$rationale)))
  })
})

test_that("bulk_assess_submit is a no-op without a rationale, even if the client bypasses its own JS guard", {
  skip_if_not_installed("sodium")

  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  user_id <- episodic_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", sodium::password_store("initial123"))
  DBI::dbExecute(con, "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?", params = list(user_id))
  institution_id <- episodic_db_institution_upsert(
    con, institution_key = digest::digest("hosp-bulk2", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second", is_monitored = TRUE
  )
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_institution", "Norovirus", institution_id = institution_id),
    level = "pathogen_institution", pathogen = "Norovirus", institution_id = institution_id,
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  cluster_id <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(auth_username_val = "jdoe", auth_password_val = "initial123")
    session$setInputs(auth_login_submit = 1)
    session$flushReact()

    session$setInputs(bulk_assess_submit = list(cluster_ids = cluster_id, verdict = "artefact", rationale = ""))
    session$flushReact()

    con <- episodic_db_connect(db_path)
    on.exit(DBI::dbDisconnect(con))
    expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episodic_assessment_event")$n, 0)
  })
})

test_that("output$main_view actually renders the info screen when nav_view is set to 'info'", {
  db_path <- tempfile(fileext = ".sqlite")
  episodic_db_create(db_path)

  server <- episodic_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(nav_view = "info")
    session$flushReact()
    rendered <- paste(output$main_view, collapse = "\n")
    expect_true(grepl("<code>same_place</code>", rendered, fixed = TRUE))
  })
})

test_that("output$main_view actually renders the performance screen when nav_view is set to 'performance'", {
  db_path <- tempfile(fileext = ".sqlite")
  episodic_db_create(db_path)

  server <- episodic_app_server_factory(db_path, lang = "nl")
  shiny::testServer(server, {
    session$setInputs(nav_view = "performance")
    session$flushReact()
    rendered <- paste(output$main_view, collapse = "\n")
    expect_true(grepl("Prestatie", rendered, fixed = TRUE))
    expect_true(grepl("Tijdigheid", rendered, fixed = TRUE))
  })
})

test_that("input$open_cluster jumps to the Clusters screen on that very cluster", {
  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_region", "Norovirus", region_code = "R"),
    level = "pathogen_region", pathogen = "Norovirus", region_code = "R",
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  first <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                      last_day = "2025-01-02", n_cases = 9, priority_score = 90,
                                      detector_agreement = 1, run_id = run_id)
  second <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-02-01",
                                       last_day = "2025-02-02", n_cases = 3, priority_score = 10,
                                       detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "en")
  shiny::testServer(server, {
    session$flushReact()
    # Matched as a text node (">#2<"), never as a bare "#2": the palette
    # ships #20C997 and #1A1A1A, so a loose needle matches any dossier.
    ref <- function(id) paste0(">", episodic_tr("dossier.cluster_ref", id = id, lang = "en"), "<")
    # the rail auto-selects the highest-priority cluster on load
    expect_true(grepl(ref(first), paste(output$dossier_pane, collapse = "\n"), fixed = TRUE))

    session$setInputs(nav_view = "pathogen")
    session$flushReact()
    session$setInputs(open_cluster = second)
    session$flushReact()

    dossier <- paste(output$dossier_pane, collapse = "\n")
    expect_true(grepl(ref(second), dossier, fixed = TRUE))
    expect_false(grepl(ref(first), dossier, fixed = TRUE))
    # and it actually switched screens
    expect_true(grepl("episodic-dossier", paste(output$main_view, collapse = "\n"), fixed = TRUE))
  })
})

test_that("a deep link to a closed cluster is not redirected to the top of the rail", {
  # The Pathogen screen lists closed clusters too, and the rail's
  # auto-select used to reset any selection that was not currently open -
  # which would have silently sent every such link somewhere else.
  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_region", "Norovirus", region_code = "R"),
    level = "pathogen_region", pathogen = "Norovirus", region_code = "R",
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  open_one <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                         last_day = "2025-01-02", n_cases = 9, priority_score = 90,
                                         detector_agreement = 1, run_id = run_id)
  closed <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2024-03-01",
                                       last_day = "2024-03-05", n_cases = 4, priority_score = 20,
                                       detector_agreement = 1, run_id = run_id)
  episodic_db_cluster_state_insert(con, cluster_id = closed, state = "closed", trigger = "system")
  DBI::dbDisconnect(con)

  server <- episodic_app_server_factory(db_path, lang = "en")
  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(open_cluster = closed)
    session$flushReact()

    ref <- function(id) paste0(">", episodic_tr("dossier.cluster_ref", id = id, lang = "en"), "<")
    dossier <- paste(output$dossier_pane, collapse = "\n")
    expect_true(grepl(ref(closed), dossier, fixed = TRUE))
    expect_false(grepl(ref(open_one), dossier, fixed = TRUE))
  })
})

test_that("episodic_app_cluster_viewable() accepts closed clusters but not merged-away ones", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_region", "Norovirus", region_code = "R"),
    level = "pathogen_region", pathogen = "Norovirus", region_code = "R",
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  survivor <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                         last_day = "2025-01-02", n_cases = 5, priority_score = 50,
                                         detector_agreement = 1, run_id = run_id)
  absorbed <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-03",
                                         last_day = "2025-01-04", n_cases = 2, priority_score = 20,
                                         detector_agreement = 1, run_id = run_id)

  expect_true(episodic_app_cluster_viewable(con, survivor))
  # closed is not a reason to refuse - the archive is worth re-reading
  episodic_db_cluster_state_insert(con, cluster_id = survivor, state = "closed", trigger = "system")
  expect_true(episodic_app_cluster_viewable(con, survivor))

  # merged is: its cases now count under the survivor, so its dossier is
  # stale rather than merely closed
  episodic_db_cluster_set_merged_into(con, absorbed, survivor)
  expect_false(episodic_app_cluster_viewable(con, absorbed))

  expect_false(episodic_app_cluster_viewable(con, NULL))
  expect_false(episodic_app_cluster_viewable(con, 999999L))
  expect_false(episodic_app_cluster_viewable(con, NA_integer_))
})

test_that("the navigation highlight follows a deep link, not just its own clicks", {
  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_region", "Norovirus", region_code = "R"),
    level = "pathogen_region", pathogen = "Norovirus", region_code = "R",
    observed_date = "2025-01-01"
  )
  run_id <- episodic_db_run_start(con, "h", "a")
  cluster_id <- episodic_db_cluster_insert(con, stream_id = stream_id, first_day = "2025-01-01",
                                           last_day = "2025-01-02", n_cases = 3, priority_score = 50,
                                           detector_agreement = 1, run_id = run_id)
  DBI::dbDisconnect(con)

  active <- function(html) {
    m <- regmatches(html, gregexpr('data-view="[a-z]+" class="episodic-nav-link active"', html))[[1]]
    if (length(m) == 0) {
      m <- regmatches(html, gregexpr('class="episodic-nav-link active" data-view="[a-z]+"', html))[[1]]
    }
    gsub('.*data-view="([a-z]+)".*', "\\1", m)
  }

  server <- episodic_app_server_factory(db_path, lang = "en")
  shiny::testServer(server, {
    session$flushReact()
    expect_equal(active(paste(output$nav_links, collapse = "\n")), "clusters")

    session$setInputs(nav_view = "pathogen")
    session$flushReact()
    expect_equal(active(paste(output$nav_links, collapse = "\n")), "pathogen")

    # the bug: opening a cluster from the Pathogen screen's table moved
    # the content but left the highlight behind on Pathogen
    session$setInputs(open_cluster = cluster_id)
    session$flushReact()
    expect_equal(active(paste(output$nav_links, collapse = "\n")), "clusters")
  })
})
