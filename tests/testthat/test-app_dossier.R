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

test_that("episodic_ui_dossier() and episodic_ui_assessment_rail() render the fixture cluster without error, in both languages", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  expect_s3_class(
    episodic_ui_dossier(env$con, env$cluster_id, lang = "nl"),
    "shiny.tag"
  )
  expect_s3_class(
    episodic_ui_dossier(env$con, env$cluster_id, lang = "en"),
    "shiny.tag"
  )
  expect_s3_class(
    episodic_ui_assessment_rail(env$con, env$cluster_id, lang = "nl"),
    "shiny.tag"
  )
})

test_that("episodic_ui_dossier() renders the new M5 panels (Rt, similar clusters, report) with their expected content", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  fake_user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "admin",
    stringsAsFactors = FALSE
  )

  html_anon <- as.character(episodic_ui_dossier(
    env$con,
    env$cluster_id,
    lang = "nl"
  ))
  expect_true(grepl(
    episodic_tr("panel.rt.title", lang = "nl"),
    html_anon,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("panel.similar.title", lang = "nl"),
    html_anon,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("panel.report.title", lang = "nl"),
    html_anon,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("panel.report.empty", lang = "nl"),
    html_anon,
    fixed = TRUE
  ))
  # anonymous viewer gets no render button
  expect_false(grepl("report_render_submit", html_anon, fixed = TRUE))

  html_signed_in <- as.character(episodic_ui_dossier(
    env$con,
    env$cluster_id,
    lang = "nl",
    current_user = fake_user
  ))
  expect_true(grepl("report_render_submit", html_signed_in, fixed = TRUE))
  expect_true(grepl(
    episodic_tr("panel.report.render_button", lang = "nl"),
    html_signed_in,
    fixed = TRUE
  ))
})

test_that("episodic_ui_assessment_rail() renders the classification and mute pickers as coloured buttons, not <select>, with a mute intro paragraph", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  fake_user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "admin",
    stringsAsFactors = FALSE
  )

  rendered <- as.character(episodic_ui_assessment_rail(
    env$con,
    env$cluster_id,
    lang = "nl",
    current_user = fake_user
  ))
  expect_false(grepl("<select", rendered, fixed = TRUE))
  expect_true(grepl("episodic-picker-btn", rendered, fixed = TRUE))
  expect_true(grepl(
    episodic_tr("verdict.possible_epidemic", lang = "nl"),
    rendered,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("assessment.mute_reason.seasonal", lang = "nl"),
    rendered,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("assessment.mute_intro", lang = "nl"),
    rendered,
    fixed = TRUE
  ))

  # Wpg/GGD are Netherlands-specific and out of scope for a general
  # deployment; removed from the form entirely.
  expect_false(grepl("assess_wpg", rendered, fixed = TRUE))
  expect_false(grepl("assess_ggd", rendered, fixed = TRUE))

  # verdict buttons ordered mild/terminal to severe: artefact and
  # expected_variation (both terminal) before the escalating verdicts.
  pos <- function(needle) regexpr(needle, rendered, fixed = TRUE)
  expect_true(
    pos(episodic_tr("verdict.artefact", lang = "nl")) <
      pos(episodic_tr("verdict.cluster_not_yet", lang = "nl"))
  )
  expect_true(
    pos(episodic_tr("verdict.cluster_not_yet", lang = "nl")) <
      pos(episodic_tr("verdict.possible_epidemic", lang = "nl"))
  )
  expect_true(
    pos(episodic_tr("verdict.possible_epidemic", lang = "nl")) <
      pos(episodic_tr("verdict.confirmed_epidemic", lang = "nl"))
  )

  # hints describe what the verdict means, not what happens next
  expect_false(grepl("gestart", rendered, fixed = TRUE))
})

test_that("the concentration read model carries the full per-PC breakdown, not just the dominant PC", {
  # episodic_ui_geo_panel() itself renders either a bar breakdown or a
  # choropleth map, depending on whether sf and geographic reference data
  # happen to be available - a rendering choice, tested on its own terms
  # below. What actually matters here is the data both of those rendering
  # paths draw from.
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episodic_cluster_object(env$con, env$cluster_id)

  expect_true(all(c("9711", "9712", "9713") %in% obj$concentration$rows$label))

  panel <- episodic_ui_geo_panel(obj, lang = "nl")
  expect_s3_class(panel, "shiny.tag")
})

test_that("episodic_ui_bars() renders every row's label as text, not just the dominant one", {
  rows <- data.frame(label = c("9711", "9712", "9713"), n = c(5, 3, 1))
  rendered <- as.character(episodic_ui_bars(rows))
  expect_true(grepl("9711", rendered))
  expect_true(grepl("9712", rendered))
  expect_true(grepl("9713", rendered))
})

test_that("the status trajectory shows classifications, and labels the pre-assessment period 'unassessed' rather than 'new'", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    "hash"
  )

  obj <- episodic_cluster_object(env$con, env$cluster_id)
  timeline_before <- episodic_app_assessment_timeline(
    env$con,
    env$cluster_id,
    lang = "nl"
  )
  before <- as.character(episodic_ui_trajectory(
    obj,
    timeline_before,
    lang = "nl"
  ))
  expect_true(grepl(
    episodic_tr("statusverloop.unassessed", lang = "nl"),
    before,
    fixed = TRUE
  ))
  expect_false(grepl(
    episodic_tr("state.new", lang = "nl"),
    before,
    fixed = TRUE
  ))

  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id,
    verdict = "possible_epidemic",
    rationale = "watching this"
  )
  Sys.sleep(1.1)
  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id,
    verdict = "confirmed_epidemic",
    rationale = "confirmed on culture"
  )

  obj2 <- episodic_cluster_object(env$con, env$cluster_id, lang = "nl")
  timeline_after <- episodic_app_assessment_timeline(
    env$con,
    env$cluster_id,
    lang = "nl"
  )
  after <- as.character(episodic_ui_trajectory(
    obj2,
    timeline_after,
    lang = "nl"
  ))
  expect_true(grepl(
    episodic_tr("statusverloop.unassessed", lang = "nl"),
    after,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("verdict.possible_epidemic", lang = "nl"),
    after,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("verdict.confirmed_epidemic", lang = "nl"),
    after,
    fixed = TRUE
  ))
})

test_that("episodic_app_streams_screen() paginates, computing baseline_excluded only for the requested page", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  screen <- episodic_app_streams_screen(env$con, page = 1L, page_size = 1L)
  expect_equal(nrow(screen$streams), 1)
  expect_equal(screen$total, 1) # app_read_setup() creates exactly one stream
  expect_equal(screen$n_pages, 1)
  expect_true("baseline_excluded" %in% names(screen$streams))

  ui <- episodic_ui_streams_screen(screen, lang = "nl")
  expect_s3_class(ui, "shiny.tag")
  # a single page: no pager controls rendered
  expect_false(grepl("streams_page_select", as.character(ui), fixed = TRUE))
})

test_that("episodic_rt_unavailable_reason() distinguishes a missing serial interval from insufficient history", {
  pc_missing_si <- list(
    rt_applicable = TRUE,
    si_mean_days = NA,
    si_sd_days = NA
  )
  expect_equal(
    episodic_rt_unavailable_reason(pc_missing_si),
    "no_serial_interval"
  )

  pc_ok <- list(rt_applicable = TRUE, si_mean_days = 3, si_sd_days = 1.5)
  expect_true(
    episodic_rt_unavailable_reason(pc_ok) %in%
      c("insufficient_history", "epiestim_missing")
  )

  expect_true(is.na(episodic_rt_unavailable_reason(list(
    rt_applicable = FALSE
  ))))
  expect_true(is.na(episodic_rt_unavailable_reason(NULL)))
})

test_that("episodic_ui_streams_screen() and episodic_ui_status_strip() render the fixture run without error", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  screen <- episodic_app_streams_screen(env$con)
  expect_s3_class(episodic_ui_streams_screen(screen, lang = "nl"), "shiny.tag")

  status <- episodic_app_status(env$con)
  expect_s3_class(episodic_ui_status_strip(status, lang = "nl"), "shiny.tag")
})

test_that("the dossier title carries the cluster id beside the pathogen name", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  html <- as.character(episodic_ui_dossier(
    env$con,
    env$cluster_id,
    lang = "en"
  ))

  expect_true(grepl("episodic-dossier-id", html, fixed = TRUE))
  # as a text node: a bare "#1" also matches the palette's #1A1A1A
  expect_true(grepl(
    paste0(
      ">",
      episodic_tr("dossier.cluster_ref", id = env$cluster_id, lang = "en"),
      "<"
    ),
    html,
    fixed = TRUE
  ))
  # inside the title element, not further down the meta line
  title_pos <- regexpr("episodic-dossier-title", html, fixed = TRUE)
  meta_pos <- regexpr("episodic-dossier-meta", html, fixed = TRUE)
  id_pos <- regexpr("episodic-dossier-id", html, fixed = TRUE)
  expect_gt(id_pos, title_pos)
  expect_lt(id_pos, meta_pos)
})
