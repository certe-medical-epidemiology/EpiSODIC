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
    role = "epidemiologist",
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
    role = "epidemiologist",
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
  # app_read_setup()'s cluster is ward-level, so the verdict buttons read
  # "outbreak", not "epidemic" - see episodic_verdict_outbreak_levels().
  expect_true(grepl(
    episodic_verdict_label(
      "possible_epidemic",
      level = "pathogen_ward",
      lang = "nl"
    ),
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
  verdict_label_nl <- function(v) {
    episodic_verdict_label(v, level = "pathogen_ward", lang = "nl")
  }
  expect_true(
    pos(verdict_label_nl("artefact")) < pos(verdict_label_nl("cluster_not_yet"))
  )
  expect_true(
    pos(verdict_label_nl("cluster_not_yet")) <
      pos(verdict_label_nl("possible_epidemic"))
  )
  expect_true(
    pos(verdict_label_nl("possible_epidemic")) <
      pos(verdict_label_nl("confirmed_epidemic"))
  )

  # hints describe what the verdict means, not what happens next
  expect_false(grepl("gestart", rendered, fixed = TRUE))
})

test_that("a closed cluster's assessment form is hidden behind a Re-open button, reachable from any entry point", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  fake_user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "epidemiologist",
    stringsAsFactors = FALSE
  )

  open_rendered <- as.character(episodic_ui_assessment_rail(
    env$con,
    env$cluster_id,
    lang = "en",
    current_user = fake_user
  ))
  expect_false(grepl(
    episodic_tr("assessment.reopen_button", lang = "en"),
    open_rendered,
    fixed = TRUE
  ))
  expect_false(grepl('id="assess_form_fields" hidden', open_rendered, fixed = TRUE))

  episodic_db_app_user_insert(
    env$con,
    "tester",
    "Test User",
    "t@example.com",
    "hash"
  )
  episodic_app_submit_assessment(
    env$con,
    env$cluster_id,
    user_id = 1L,
    verdict = "artefact",
    rationale = "false alarm",
    close = TRUE
  )

  closed_rendered <- as.character(episodic_ui_assessment_rail(
    env$con,
    env$cluster_id,
    lang = "en",
    current_user = fake_user
  ))
  expect_true(grepl(
    episodic_tr("assessment.closed_notice", lang = "en"),
    closed_rendered,
    fixed = TRUE
  ))
  expect_true(grepl(
    episodic_tr("assessment.reopen_button", lang = "en"),
    closed_rendered,
    fixed = TRUE
  ))
  # the fields are still in the DOM (reachable from the rail, the
  # Archive, or any related/similar-clusters panel, all of which render
  # this same form for a given cluster_id) - just hidden until clicked
  expect_true(grepl('id="assess_form_fields" hidden', closed_rendered, fixed = TRUE))
  expect_true(grepl("assess_verdict", closed_rendered, fixed = TRUE))
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
    episodic_tr("status_trajectory.unassessed", lang = "nl"),
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
    episodic_tr("status_trajectory.unassessed", lang = "nl"),
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

test_that("the settings panel names the versions the run recorded, and says unknown when it recorded none", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  # The fixture's run finished without pkg_versions, which is what a run
  # from an older build looks like: the panel has to say so, not fail.
  html <- as.character(episodic_ui_settings_panel(
    env$con,
    env$cluster_id,
    lang = "en"
  ))
  expect_true(grepl(
    episodic_tr("panel.settings.pkg_versions", lang = "en"),
    html,
    fixed = TRUE
  ))
  expect_true(grepl(episodic_tr("misc.unknown", lang = "en"), html, fixed = TRUE))

  run_id <- episodic_db_run_start(env$con, "host", "account")
  episodic_db_run_finish(
    env$con,
    run_id,
    status = "success",
    pkg_versions = as.character(jsonlite::toJSON(
      list(EpiSODIC = "0.17.1"),
      auto_unbox = TRUE
    ))
  )
  html <- as.character(episodic_ui_settings_panel(
    env$con,
    env$cluster_id,
    lang = "en"
  ))
  expect_true(grepl("<code>EpiSODIC</code> v0.17.1", html, fixed = TRUE))
})

test_that("the dossier joins detector names with the session language's own word for 'and'", {
  # It was joined with " en " - Dutch - inside a translated sentence, on
  # every one of the eight languages.
  obj <- list(
    id = 41L,
    pathogen = "Norovirus",
    level = "pathogen_ward",
    care_line = "second",
    place = "Hospital A - B4",
    origin = "detected",
    changed_since_assessment = FALSE,
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    detectors = c("farrington", "same_place")
  )
  for (lang in c("en", "nl", "de")) {
    html <- as.character(episodic_ui_dossier_header(obj, "new", lang = lang))
    expect_true(
      grepl(episodic_tr("misc.list_separator_and", lang = lang), html, fixed = TRUE),
      info = lang
    )
  }
  expect_false(grepl(
    " en ",
    as.character(episodic_ui_dossier_header(obj, "new", lang = "en")),
    fixed = TRUE
  ))
})

test_that("the stat grid drops the case-free tile when there is no case to count days from", {
  # `case_free$since` is NA for a cluster with no case linked to it, and
  # `need < since` is then not a condition but an error that took the
  # whole dossier down.
  obj <- list(
    id = 42L,
    n_cases = 0L,
    n_positives = 0L,
    unique_patients = 0L,
    expected = NA_real_,
    ratio = NA_real_,
    priority_score = 12,
    doubling_days = NA_real_,
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    density = NULL,
    case_free = list(since = NA_integer_, need = 14L)
  )
  html <- as.character(episodic_ui_stat_grid(obj, lang = "en"))
  expect_false(grepl(
    episodic_tr("dossier.stat.case_free", lang = "en"),
    html,
    fixed = TRUE
  ))

  # With a case behind it, the tile is back.
  obj$case_free$since <- 3L
  expect_true(grepl(
    episodic_tr("dossier.stat.case_free", lang = "en"),
    as.character(episodic_ui_stat_grid(obj, lang = "en")),
    fixed = TRUE
  ))
})

test_that("the stat grid says 'unknown' rather than printing NA for a priority score it has not got", {
  obj <- list(
    id = 43L,
    n_cases = 3L,
    n_positives = 3L,
    unique_patients = 3L,
    # A real expectation, so "unknown" can only have come from the
    # priority score.
    expected = 1.2,
    ratio = NA_real_,
    priority_score = NA_real_,
    doubling_days = NA_real_,
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    density = NULL,
    case_free = list(since = NA_integer_, need = NA_integer_)
  )
  html <- as.character(episodic_ui_stat_grid(obj, lang = "en"))
  expect_false(grepl(">NA<", html, fixed = TRUE))
  expect_true(grepl(
    episodic_tr("misc.unknown", lang = "en"),
    html,
    fixed = TRUE
  ))
})

test_that("the stat grid writes its numbers the way the session language writes them", {
  # The whole point of episodic_format_number(): a Dutch reader seeing a
  # ratio of "1.4" reads fourteen hundred before reading 1.4.
  obj <- list(
    id = 44L,
    n_cases = 1234L,
    n_positives = 1234L,
    unique_patients = 1200L,
    expected = 1234.5,
    # An exactly representable half, so the assertion is about the marks
    # and not about how a binary double rounds.
    ratio = 1.5,
    priority_score = 61.6,
    doubling_days = NA_real_,
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    density = NULL,
    case_free = list(since = NA_integer_, need = NA_integer_)
  )

  en <- as.character(episodic_ui_stat_grid(obj, lang = "en"))
  expect_true(grepl(">1,234<", en, fixed = TRUE))
  expect_true(grepl("1,234.5", en, fixed = TRUE))
  expect_true(grepl(">1.5<", en, fixed = TRUE))

  nl <- as.character(episodic_ui_stat_grid(obj, lang = "nl"))
  expect_true(grepl(">1.234<", nl, fixed = TRUE))
  expect_true(grepl("1.234,5", nl, fixed = TRUE))
  expect_true(grepl(">1,5<", nl, fixed = TRUE))
  expect_false(grepl(">1,234<", nl, fixed = TRUE))
})
