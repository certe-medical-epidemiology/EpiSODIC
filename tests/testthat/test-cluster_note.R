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

test_that("episodic_db_cluster_note_current() returns nothing for a cluster with no note", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  expect_equal(
    nrow(episodic_db_cluster_note_current(env$con, env$cluster_id)),
    0L
  )
})

test_that("episodic_db_cluster_note_insert()/_current() round-trip, and the latest note wins", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "viewer"
  )

  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "# First note")
  first <- episodic_db_cluster_note_current(env$con, env$cluster_id)
  expect_equal(nrow(first), 1L)
  expect_equal(first$note_text[1], "# First note")

  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "# Updated note")
  current <- episodic_db_cluster_note_current(env$con, env$cluster_id)
  expect_equal(nrow(current), 1L)
  expect_equal(current$note_text[1], "# Updated note")

  # Both rows still exist - this is event-sourced, never an UPDATE.
  all_notes <- DBI::dbGetQuery(
    env$con,
    "SELECT * FROM episodic_cluster_note WHERE cluster_id = ? ORDER BY note_id",
    params = list(env$cluster_id)
  )
  expect_equal(nrow(all_notes), 2L)
})

test_that("episodic_ui_render_markdown() renders markdown but neutralises embedded HTML", {
  rendered <- as.character(episodic_ui_render_markdown("# Title\n\nSome *emphasis*."))
  expect_true(grepl("<h1>Title</h1>", rendered, fixed = TRUE))
  expect_true(grepl("<em>emphasis</em>", rendered, fixed = TRUE))

  unsafe <- as.character(episodic_ui_render_markdown(
    "before <script>alert(1)</script> after"
  ))
  expect_false(grepl("<script>", unsafe, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", unsafe, fixed = TRUE))
})

test_that("the dossier wires the notes panel behind its own uiOutput", {
  # episodic_ui_notes_panel() itself is exercised below; what belongs to
  # episodic_ui_dossier() is only that the placeholder is there for
  # output$notes_pane (see app_server.R) to fill in - not the panel's
  # content, which a note save must be able to redraw without the rest of
  # the dossier (several panels of which are plots) redrawing with it.
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  html <- as.character(episodic_ui_dossier(env$con, env$cluster_id, lang = "en"))
  expect_true(grepl("notes_pane", html, fixed = TRUE))
})

test_that("the notes panel is shown to any signed-in role and hidden from an anonymous visitor", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  viewer <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "viewer",
    stringsAsFactors = FALSE
  )

  html_anon <- as.character(episodic_ui_notes_panel(env$con, env$cluster_id, NULL, lang = "en"))
  expect_true(grepl(episodic_tr("panel.notes.title", lang = "en"), html_anon, fixed = TRUE))
  expect_false(grepl("note_save_submit", html_anon, fixed = TRUE))

  html_viewer <- as.character(episodic_ui_notes_panel(
    env$con,
    env$cluster_id,
    viewer,
    lang = "en"
  ))
  expect_true(grepl("note_save_submit", html_viewer, fixed = TRUE))
  expect_true(grepl(episodic_tr("notes.edit_button", lang = "en"), html_viewer, fixed = TRUE))
})

test_that("the notes panel shows a saved note rendered as markdown", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "viewer"
  )
  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "*hello*")

  html <- as.character(episodic_ui_notes_panel(env$con, env$cluster_id, NULL, lang = "en"))
  expect_true(grepl("<em>hello</em>", html, fixed = TRUE))
})

test_that("the notes panel's History button appears only once a note has been saved", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  html_before <- as.character(episodic_ui_notes_panel(env$con, env$cluster_id, NULL, lang = "en"))
  expect_false(grepl("note_history_open", html_before, fixed = TRUE))

  user_id <- episodic_db_app_user_insert(
    env$con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "viewer"
  )
  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "first note")

  html_after <- as.character(episodic_ui_notes_panel(env$con, env$cluster_id, NULL, lang = "en"))
  # Attribute values come back HTML-escaped (htmltools turns ' into
  # &#39;), so match on the unquoted parts - see
  # test-app_ui.R's episodic_ui_nav_link() test for the same idiom.
  expect_true(grepl("note_history_open", html_after, fixed = TRUE))
  expect_true(grepl(
    sprintf("%d, {priority", env$cluster_id),
    html_after,
    fixed = TRUE
  ))
  expect_true(grepl(episodic_tr("notes.history_button", lang = "en"), html_after, fixed = TRUE))
})

test_that("episodic_db_cluster_note_history() returns every version, oldest first, joined to the username", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    username = "jdoe",
    full_name = "Jane Doe",
    email = "jdoe@example.com",
    password_hash = "x",
    role = "viewer"
  )

  expect_equal(nrow(episodic_db_cluster_note_history(env$con, env$cluster_id)), 0L)

  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "first")
  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "second")
  episodic_db_cluster_note_insert(env$con, env$cluster_id, user_id, "third")

  history <- episodic_db_cluster_note_history(env$con, env$cluster_id)
  expect_equal(nrow(history), 3L)
  expect_equal(history$note_text, c("first", "second", "third"))
  expect_true(all(history$username == "jdoe"))
  expect_true(all(diff(history$note_id) > 0))
})

test_that("episodic_ui_notes_history_modal() diffs every version against the one before it, newest first", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user1 <- episodic_db_app_user_insert(
    env$con,
    username = "afirst",
    full_name = "A First",
    email = "afirst@example.com",
    password_hash = "x",
    role = "viewer"
  )
  user2 <- episodic_db_app_user_insert(
    env$con,
    username = "zsecond",
    full_name = "Z Second",
    email = "zsecond@example.com",
    password_hash = "x",
    role = "viewer"
  )
  episodic_db_cluster_note_insert(env$con, env$cluster_id, user1, "alpha")
  # Appending rather than replacing keeps this version's diff an
  # insertion only ("alpha" carries over unchanged) - a full replacement
  # would show the old text too (as a deletion within this very entry),
  # which would confound a position-based ordering check below.
  episodic_db_cluster_note_insert(env$con, env$cluster_id, user2, "alpha bravo")

  html <- as.character(episodic_ui_notes_history_modal(env$con, env$cluster_id, lang = "en"))
  expect_true(grepl("afirst", html, fixed = TRUE))
  expect_true(grepl("zsecond", html, fixed = TRUE))
  expect_true(grepl("episodic-notes-diff-ins", html, fixed = TRUE))
  # newest first: the second version's entry (by "zsecond", its author)
  # must appear before the first version's entry (by "afirst")
  expect_true(regexpr("zsecond", html, fixed = TRUE) < regexpr("afirst", html, fixed = TRUE))
})

test_that("episodic_ui_notes_history_modal() shows an empty state when no note was ever saved", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  html <- as.character(episodic_ui_notes_history_modal(env$con, env$cluster_id, lang = "en"))
  expect_true(grepl(episodic_tr("notes.history_empty", lang = "en"), html, fixed = TRUE))
})
