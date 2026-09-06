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

# Not the test-i18n.R constant of the same name: test files load in
# alphabetical order (this one before test-i18n.R), so relying on that
# file's top-level binding here would be order-dependent.
episodic_mobile_shipped_langs <- c("nl", "en", "es", "fr", "de", "zh", "hi", "ar")

episodic_mobile_css <- function() {
  paste(
    readLines(
      system.file("app/www/episodic.css", package = "EpiSODIC"),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

test_that("episodic.css declares both mobile breakpoints", {
  css <- episodic_mobile_css()
  expect_true(grepl("@media (max-width: 1199px)", css, fixed = TRUE))
  expect_true(grepl(
    "@media (min-width: 768px) and (max-width: 1199px)",
    css,
    fixed = TRUE
  ))
  expect_true(grepl("@media (max-width: 767px)", css, fixed = TRUE))
})

test_that("the >= 1200px rendering rules are untouched by the responsive section", {
  css <- episodic_mobile_css()
  # The three-pane widths the whole responsive section exists to override
  # below 1200px, verbatim as they were before this section was added.
  expect_true(grepl(
    ".episodic-rail {\n  width: 250px;",
    css,
    fixed = TRUE
  ))
  expect_true(grepl(
    ".episodic-dossier {\n  flex: 1;",
    css,
    fixed = TRUE
  ))
  expect_true(grepl(
    ".episodic-assessment-rail {\n  width: 330px;",
    css,
    fixed = TRUE
  ))
  # .episodic-select's own shipped min-width, only relaxed inside the
  # media query added below it - not rewritten in place.
  expect_true(grepl(
    ".episodic-select {\n  min-width: 320px;\n  max-width: 100%;\n}",
    css,
    fixed = TRUE
  ))
})

test_that("the responsive section is appended after every pre-existing rule, not spliced into the cascade", {
  css <- episodic_mobile_css()
  marker <- regexpr("Responsive layout", css, fixed = TRUE)
  select_rule <- regexpr(
    ".episodic-select {\n  min-width: 320px;",
    css,
    fixed = TRUE
  )
  expect_true(marker > 0)
  expect_true(select_rule > 0)
  expect_true(marker > select_rule)
})

test_that("a rendered clusters view carries exactly one assess_verdict/assess_rationale/assess_snooze, across rail, dossier and the pane switcher together", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  fake_user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "epidemiologist",
    stringsAsFactors = FALSE
  )
  open <- data.frame(
    cluster_id = env$cluster_id,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "New",
    n_cases = 6L,
    priority_score = 40,
    first_day = "2025-01-01",
    last_day = "2025-01-10",
    stringsAsFactors = FALSE
  )

  # Assembled the same way the "clusters" branch of output$main_view does
  # in R/app_server.R - rail, dossier, assessment and the pane switcher as
  # siblings of one .episodic-body, so a duplicate id introduced by any of
  # them together (not just within one) would show up here.
  html <- paste(
    as.character(episodic_ui_rail(
      open,
      selected_id = env$cluster_id,
      lang = "en",
      current_user = fake_user
    )),
    as.character(episodic_ui_dossier(
      env$con,
      env$cluster_id,
      lang = "en",
      current_user = fake_user
    )),
    as.character(episodic_ui_assessment_rail(
      env$con,
      env$cluster_id,
      lang = "en",
      current_user = fake_user
    )),
    as.character(episodic_ui_pane_switcher(lang = "en")),
    collapse = "\n"
  )

  for (id in c("assess_verdict", "assess_rationale", "assess_snooze")) {
    expect_equal(
      lengths(regmatches(
        html,
        gregexpr(sprintf('id="%s"', id), html, fixed = TRUE)
      ))[[1]],
      1,
      info = id
    )
  }
})

test_that("episodic_ui_pane_switcher() renders three segments with translated labels in every shipped language", {
  for (lang in episodic_mobile_shipped_langs) {
    html <- as.character(episodic_ui_pane_switcher(lang = lang))
    expect_true(grepl("episodic-pane-tab", html, fixed = TRUE))
    expect_true(grepl(
      episodic_tr("nav.clusters", lang = lang),
      html,
      fixed = TRUE
    ))
    expect_true(grepl(
      episodic_tr("pane.dossier", lang = lang),
      html,
      fixed = TRUE
    ))
    expect_true(grepl(
      episodic_tr("assessment.verdict_label", lang = lang),
      html,
      fixed = TRUE
    ))
    # a translation miss falls through to episodic_tr()'s "[[key]]"
    # placeholder
    expect_false(grepl("[[", html, fixed = TRUE))
  }
})

test_that("episodic_app_ui() assembles without error in every shipped language", {
  for (lang in episodic_mobile_shipped_langs) {
    expect_s3_class(episodic_app_ui(lang), "shiny.tag.list")
  }
})

test_that("the new pane.dossier and nav.menu_label keys exist in every shipped language with a non-empty value", {
  for (lang in episodic_mobile_shipped_langs) {
    table <- episodic_i18n_load(lang)
    expect_true("pane.dossier" %in% names(table), info = lang)
    expect_true("nav.menu_label" %in% names(table), info = lang)
    expect_true(nzchar(table[["pane.dossier"]]), info = lang)
    expect_true(nzchar(table[["nav.menu_label"]]), info = lang)
  }
})

test_that("the interpretation/notes pair and the pathogen breakdown pair use classes, not inline flex styles", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  html <- as.character(episodic_ui_dossier(env$con, env$cluster_id, lang = "en"))
  expect_true(grepl("episodic-split-row", html, fixed = TRUE))
  expect_true(grepl("episodic-split-col-wide", html, fixed = TRUE))
  expect_false(grepl("style=\"display:flex;gap:16px;\"", html, fixed = TRUE))
})

test_that("episodicSelectPane/episodicSyncPaneBar/episodicToggleNav are each defined exactly once in the page head", {
  html <- as.character(episodic_app_ui("en"))
  for (fn in c("episodicSelectPane", "episodicSyncPaneBar", "episodicToggleNav")) {
    expect_equal(
      lengths(regmatches(
        html,
        gregexpr(sprintf("function %s(", fn), html, fixed = TRUE)
      ))[[1]],
      1,
      info = fn
    )
  }
})

test_that("the rail item's onclick still sets rail_select and now also switches the mobile pane to the dossier", {
  open <- data.frame(
    cluster_id = 3L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "New",
    n_cases = 2L,
    priority_score = NA_real_,
    first_day = "2025-01-01",
    last_day = "2025-01-05",
    stringsAsFactors = FALSE
  )
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "en"))
  expect_true(grepl("rail_select", html, fixed = TRUE))
  expect_true(grepl("episodicSelectPane('dossier')", html, fixed = TRUE))
  expect_true(grepl("episodicSyncPaneBar()", html, fixed = TRUE))
})
