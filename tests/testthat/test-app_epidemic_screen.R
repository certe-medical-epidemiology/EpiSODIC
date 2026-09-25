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

# ---------------------------------------------------------------------
# Where an epidemic stands in its course
# ---------------------------------------------------------------------

course_weekly <- function(n_cases, incomplete = rep(FALSE, length(n_cases))) {
  data.frame(
    week_start = as.Date("2025-12-01") + (seq_along(n_cases) - 1) * 7,
    n_cases = n_cases,
    incomplete = incomplete
  )
}

test_that("the latest complete week is compared with the week before it", {
  weekly <- course_weekly(
    c(2, 3, 10, 20, 25, 7),
    incomplete = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  course <- episodic_epidemic_course(weekly, first_day = "2025-12-15")
  # The last week is still filling, so the latest complete one is the
  # fifth: 25 against 20.
  expect_equal(course$latest_week, as.Date("2025-12-29"))
  expect_equal(course$latest_n, 25)
  expect_equal(course$previous_n, 20)
  expect_equal(course$change_pct, 25)
  expect_equal(course$peak_n, 25)
  expect_equal(course$peak_week, as.Date("2025-12-29"))
})

test_that("a change on a week with no cases is not a percentage", {
  weekly <- course_weekly(c(0, 4))
  course <- episodic_epidemic_course(weekly, first_day = "2025-12-01")
  expect_equal(course$latest_n, 4)
  expect_equal(course$previous_n, 0)
  expect_true(is.na(course$change_pct))
})

test_that("with every week still filling there is no latest complete week", {
  weekly <- course_weekly(c(3, 5, 8), incomplete = c(TRUE, TRUE, TRUE))
  course <- episodic_epidemic_course(weekly, first_day = "2025-12-01")
  expect_true(is.na(course$latest_week))
  expect_true(is.na(course$latest_n))
  expect_true(is.na(course$change_pct))
  expect_true(is.na(course$latest_level))
  # The peak so far is still a peak so far.
  expect_equal(course$peak_n, 8)
})

test_that("the peak is looked for from the epidemic's first week, not in the lead-in", {
  weekly <- course_weekly(c(40, 2, 5, 9))
  course <- episodic_epidemic_course(weekly, first_day = "2025-12-08")
  expect_equal(course$peak_n, 9)
})

test_that("intensity is read against the thresholds, and absent without them", {
  thresholds <- list(
    pre_epidemic = 5,
    post_epidemic = 4,
    intensity = c(medium = 10, high = 20, very_high = 30)
  )
  weekly <- course_weekly(c(8, 22, 12))
  course <- episodic_epidemic_course(weekly, "2025-12-01", thresholds)
  expect_equal(course$latest_level, "medium")
  expect_equal(course$peak_level, "high")

  without <- episodic_epidemic_course(weekly, "2025-12-01", NULL)
  expect_true(is.na(without$latest_level))
  expect_true(is.na(without$peak_level))
})

# ---------------------------------------------------------------------
# The dossier's panels
# ---------------------------------------------------------------------

epidemic_obj <- function(...) {
  obj <- list(
    id = 42L,
    stream_id = 1L,
    pathogen = "Influenza A",
    level = "pathogen_region",
    care_line = NA_character_,
    place = "Catchment",
    detectors = "mem",
    first_day = "2025-12-15",
    last_day = "2026-01-12",
    n_cases = 120,
    asof = as.Date("2026-01-14"),
    season = NULL,
    weekly = course_weekly(c(2, 3, 10, 20, 25, 30)),
    thresholds = NULL,
    course = episodic_epidemic_course(
      course_weekly(c(2, 3, 10, 20, 25, 30)),
      "2025-12-15"
    ),
    overlay = NULL,
    rt = data.frame(
      window_end = as.Date(c("2026-01-10", "2026-01-11")),
      mean = c(1.3, 1.25),
      lower = c(1.1, 1.05),
      upper = c(1.5, 1.45)
    ),
    rt_applicable = TRUE,
    rt_unavailable_reason = "insufficient_history",
    denominator = NULL,
    denominator_catchment_only = FALSE,
    concentration_geo = NULL,
    demography = NULL,
    care_lines = data.frame(label = c("Primary care", "Hospital care"), n = c(80L, 40L)),
    institutions = data.frame(
      institution_id = 1:2,
      display_name = c("Hospital A", "GP B"),
      n_cases = c(70L, 50L)
    ),
    concentration = list(
      top_institution = "Hospital A",
      top_share = 70 / 120,
      n_institutions = 2L
    ),
    during_outbreaks = data.frame(
      cluster_id = 7L,
      stream_id = 2L,
      first_day = "2025-12-20",
      last_day = "2026-01-02",
      n_cases = 6L,
      case_days = 4L,
      priority_score = 55,
      pathogen = "Influenza A",
      level = "pathogen_ward",
      level_label = "Ward",
      place = "Ward 3",
      state_label = "Monitoring",
      stringsAsFactors = FALSE
    )
  )
  utils::modifyList(obj, list(...))
}

test_that("the stat grid says where the epidemic stands", {
  html <- as.character(episodic_ui_epidemic_stat_grid(epidemic_obj(), lang = "en"))
  expect_true(grepl("Latest complete week", html, fixed = TRUE))
  expect_true(grepl("+20% on the week before", html, fixed = TRUE))
  expect_true(grepl("Peak so far", html, fixed = TRUE))
  expect_true(grepl("Latest Rt", html, fixed = TRUE))
  expect_true(grepl("1.25", html, fixed = TRUE))
  expect_true(grepl("95% CrI 1.05 to 1.45", html, fixed = TRUE))
  expect_false(grepl("[[", html, fixed = TRUE))
  # No thresholds, no intensity tile.
  expect_false(grepl("Intensity now", html, fixed = TRUE))
})

test_that("the stat grid leaves out what it cannot compute", {
  obj <- epidemic_obj(
    course = episodic_epidemic_course(
      course_weekly(c(3, 5), incomplete = c(TRUE, TRUE)),
      "2025-12-01"
    ),
    rt = NULL
  )
  html <- as.character(episodic_ui_epidemic_stat_grid(obj, lang = "en"))
  expect_false(grepl("Latest complete week", html, fixed = TRUE))
  expect_false(grepl("Latest Rt", html, fixed = TRUE))

  no_base <- epidemic_obj(
    course = episodic_epidemic_course(course_weekly(c(0, 4)), "2025-12-01")
  )
  html <- as.character(episodic_ui_epidemic_stat_grid(no_base, lang = "en"))
  expect_true(grepl("none the week before", html, fixed = TRUE))
  expect_false(grepl("NaN", html, fixed = TRUE))
  expect_false(grepl("Inf", html, fixed = TRUE))
})

test_that("the outbreaks during an epidemic are rows that open, hover and carry their state", {
  html <- as.character(episodic_ui_epidemic_during_panel(epidemic_obj(), lang = "en"))
  expect_true(grepl("episodic-row-link", html, fixed = TRUE))
  expect_true(grepl('data-episodic-outbreak="7"', html, fixed = TRUE))
  expect_true(grepl("O-7", html, fixed = TRUE))
  expect_true(grepl("Monitoring", html, fixed = TRUE))
  expect_true(grepl("Ward 3", html, fixed = TRUE))
})

test_that("the positivity note says when tests are the catchment's, not the area's", {
  series <- data.frame(
    week_start = as.Date("2025-12-01") + (0:3) * 7,
    n_tests = c(100, 120, 110, 130),
    n_cases = c(2, 4, 6, 5),
    positivity = c(0.02, 0.033, 0.055, 0.038)
  )
  catchment <- as.character(episodic_ui_epidemic_denominator_panel(
    epidemic_obj(denominator = series, denominator_catchment_only = FALSE),
    lang = "en"
  ))
  province <- as.character(episodic_ui_epidemic_denominator_panel(
    epidemic_obj(denominator = series, denominator_catchment_only = TRUE),
    lang = "en"
  ))
  note <- episodic_tr("epidemics.panel.denominator.catchment_only", lang = "en")
  expect_false(grepl(note, catchment, fixed = TRUE))
  expect_true(grepl(note, province, fixed = TRUE))
})

test_that("the epidemic dossier carries a geography panel, and says so when there is no postcode", {
  empty <- as.character(episodic_ui_epidemic_geo_panel(epidemic_obj(), lang = "en"))
  expect_true(grepl(episodic_tr("panel.geo.title", lang = "en"), empty, fixed = TRUE))
  expect_true(grepl(episodic_tr("panel.geo.empty", lang = "en"), empty, fixed = TRUE))

  with_pc <- epidemic_obj(concentration_geo = episodic_app_concentration(
    data.frame(pc = c("9711", "9711", "9712", NA))
  ))
  html <- as.character(episodic_ui_epidemic_geo_panel(with_pc, lang = "en"))
  expect_true(grepl("9711", html, fixed = TRUE))
  # One of four cases has no postcode, and the panel says so.
  expect_true(grepl(
    episodic_tr("panel.geo.unknown_pc", n = "1", total = "4", lang = "en"),
    html,
    fixed = TRUE
  ))
})

test_that("an Rt panel is not shown for a pathogen Rt does not apply to", {
  expect_null(episodic_ui_epidemic_rt_panel(
    epidemic_obj(rt_applicable = FALSE, rt = NULL),
    lang = "en"
  ))
  missing <- as.character(episodic_ui_epidemic_rt_panel(
    epidemic_obj(rt = NULL, rt_unavailable_reason = "no_serial_interval"),
    lang = "en"
  ))
  expect_true(grepl(
    episodic_tr("panel.rt.unavailable.no_serial_interval", lang = "en"),
    missing,
    fixed = TRUE
  ))
})

test_that("an epidemic's institutions are counted from its own cases", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  a <- episodic_test_institution(con, "inst-a", display_name = "Hospital A")
  b <- episodic_test_institution(con, "inst-b", display_name = "GP B")
  cases <- data.frame(institution_id = c(a, b, b, NA, b))
  inst <- episodic_epidemic_institutions(con, cases, lang = "en")
  expect_equal(inst$display_name, c("GP B", "Hospital A"))
  expect_equal(inst$n_cases, c(3L, 1L))

  none <- episodic_epidemic_institutions(
    con,
    data.frame(institution_id = NA_integer_),
    lang = "en"
  )
  expect_equal(nrow(none), 0)
})

test_that("an epidemic's length is its own course, not the time since it began", {
  html <- as.character(episodic_ui_epidemic_stat_grid(epidemic_obj(), lang = "en"))
  # 15 December 2025 to 12 January 2026: five ISO weeks.
  expect_true(grepl("5 weeks", html, fixed = TRUE))
})

test_that("a closed epidemic has no 'now': its intensity is read at the peak", {
  thresholds <- list(
    pre_epidemic = 5,
    post_epidemic = 4,
    intensity = c(medium = 10, high = 20, very_high = 30)
  )
  weekly <- course_weekly(c(8, 22, 12))
  obj <- epidemic_obj(
    closed = TRUE,
    course = episodic_epidemic_course(weekly, "2025-12-01", thresholds)
  )
  html <- as.character(episodic_ui_epidemic_stat_grid(obj, lang = "en"))
  expect_false(grepl("Latest complete week", html, fixed = TRUE))
  expect_false(grepl("Intensity now", html, fixed = TRUE))
  expect_true(grepl(episodic_tr("pathogen.stat.intensity", lang = "en"), html, fixed = TRUE))
  expect_true(grepl("High", html, fixed = TRUE))

  header <- as.character(episodic_ui_epidemic_header(obj, "closed", lang = "en"))
  expect_false(grepl(">High<", header, fixed = TRUE))
})

test_that("a history missing linked cases is stated, with both codes where they differ", {
  expect_null(episodic_ui_epidemic_history_problem(epidemic_obj(), lang = "en"))

  obj <- epidemic_obj(history_problem = list(
    n_read = 0,
    n_linked = 181,
    stream_code = "Hele Certe-regio",
    dashboard_code = "REGION"
  ))
  html <- as.character(episodic_ui_epidemic_history_problem(obj, lang = "en"))
  expect_true(grepl("episodic-dossier-problem", html, fixed = TRUE))
  expect_true(grepl("holds 0 of the 181 cases", html, fixed = TRUE))
  expect_true(grepl("Hele Certe-regio", html, fixed = TRUE))
  expect_true(grepl("REGION", html, fixed = TRUE))

  same_code <- epidemic_obj(history_problem = list(
    n_read = 3,
    n_linked = 5,
    stream_code = "GR",
    dashboard_code = NA_character_
  ))
  html <- as.character(episodic_ui_epidemic_history_problem(same_code, lang = "en"))
  expect_false(grepl("geography.region_code", html, fixed = TRUE))
})

test_that("the epidemic dossier carries its detection settings, titled for an epidemic", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  html <- as.character(episodic_ui_settings_panel(
    env$con,
    env$cluster_id,
    lang = "nl",
    obj = epidemic_obj(case_free_days = 14),
    scale = "epidemic"
  ))
  expect_true(grepl("Detectie-instellingen voor deze epidemie", html, fixed = TRUE))
  expect_false(grepl("[[", html, fixed = TRUE))
})

test_that("the rails give 'other' a care-line chip and leave 'unknown' without one", {
  expect_false(is.null(episodic_ui_care_line_colour("other")))
  expect_null(episodic_ui_care_line_colour("unknown"))
})

test_that("breakdown bars take the app's accent unless a caller names one", {
  rows <- data.frame(label = c("9711", "9712"), n = c(3L, 1L))
  plain <- as.character(episodic_ui_bars(rows, lang = "en"))
  expect_false(grepl("background:", plain, fixed = TRUE))
  named <- as.character(episodic_ui_bars(rows, colour = "#123456", lang = "en"))
  expect_true(grepl("background:#123456", named, fixed = TRUE))
})

test_that("one accent colours every screen, and no screen is tinted", {
  css <- paste(
    readLines(
      system.file("app/www/episodic.css", package = "EpiSODIC"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # The accent is defined once, as primary, and never per screen.
  expect_true(grepl("--episodic-accent: var(--episodic-primary);", css, fixed = TRUE))
  expect_false(grepl("\\[data-nav=\"[a-z]+\"\\] \\{ --episodic-accent", css))
  expect_false(grepl("--episodic-tint-", css, fixed = TRUE))
  expect_false(grepl(".episodic-screens { background", css, fixed = TRUE))
  # A section's colour is left to the active navigation link alone.
  for (view in c("outbreaks", "epidemics", "pathogens", "instance")) {
    expect_true(grepl(
      sprintf(
        '.episodic-shell[data-nav="%s"] .episodic-nav-link[data-view="%s"] { box-shadow: inset 0 -2px 0',
        view,
        view
      ),
      css,
      fixed = TRUE
    ), info = view)
  }
  expect_true(grepl(
    "background: var(--episodic-accent, var(--episodic-primary));",
    css,
    fixed = TRUE
  ))
})

test_that("charts draw their main series in primary, with no screen's colour", {
  pal <- episodic_palette()
  curve <- data.frame(
    sample_date = as.Date("2025-01-01") + 0:2,
    n_cases = c(1, 2, 3),
    incomplete = FALSE
  )
  p <- episodic_ui_epi_curve_chart(curve, lang = "en")
  expect_equal(unique(ggplot2::layer_data(p, 1)$fill), pal$primary)
  expect_false(exists("episodic_nav_accent", envir = asNamespace("EpiSODIC")))
})

# ---------------------------------------------------------------------
# The rails read what they list, not the whole history
# ---------------------------------------------------------------------

test_that("the rail's closed filter agrees with the state derivation", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  con <- env$con
  user_id <- episodic_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl", "hash")

  new_cluster <- function() {
    episodic_db_cluster_insert(
      con,
      stream_id = env$stream_id,
      first_day = "2025-01-10",
      last_day = "2025-01-13",
      n_cases = 3,
      priority_score = 40,
      detector_agreement = 1,
      run_id = env$run_id
    )
  }
  close_at <- function(cluster_id, at, trigger = "system") {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_cluster_state (cluster_id, state, entered_at, `trigger`)
       VALUES (?, 'closed', ?, ?)",
      params = list(cluster_id, at, trigger)
    )
  }
  assess_at <- function(cluster_id, at) {
    episodic_app_submit_assessment(
      con,
      cluster_id,
      user_id,
      verdict = "expected_variation",
      rationale = "seen"
    )
    DBI::dbExecute(
      con,
      "UPDATE episodic_assessment_event SET created_at = ? WHERE cluster_id = ?",
      params = list(at, cluster_id)
    )
  }

  closed_quietly <- new_cluster()
  close_at(closed_quietly, "2025-02-01 10:00:00")

  assessed_then_closed <- new_cluster()
  assess_at(assessed_then_closed, "2025-01-20 10:00:00")
  close_at(assessed_then_closed, "2025-02-01 10:00:00", trigger = "closure")

  reopened_by_assessment <- new_cluster()
  close_at(reopened_by_assessment, "2025-02-01 10:00:00")
  assess_at(reopened_by_assessment, "2025-02-05 10:00:00")

  closed_but_changed <- new_cluster()
  close_at(closed_but_changed, "2025-02-01 10:00:00")
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET changed_since_assessment = 1 WHERE cluster_id = ?",
    params = list(closed_but_changed)
  )

  never_closed <- new_cluster()

  listed <- episodic_app_open_clusters(con, lang = "en")$cluster_id

  # The same answer the full derivation over every cluster gives.
  every <- episodic_db_clusters(con, open_only = TRUE)
  every$state <- episodic_app_derive_states_batch(con, every)
  derived <- every$cluster_id[every$state != "closed" & every$scale == "outbreak"]
  expect_setequal(listed, derived)

  expect_false(closed_quietly %in% listed)
  expect_false(assessed_then_closed %in% listed)
  expect_true(reopened_by_assessment %in% listed)
  expect_true(closed_but_changed %in% listed)
  expect_true(never_closed %in% listed)
  expect_true(env$cluster_id %in% listed)

  # And what the database is asked for is already narrowed.
  prefiltered <- episodic_db_clusters_not_closed(con, "outbreak")$cluster_id
  expect_false(closed_quietly %in% prefiltered)
  expect_false(assessed_then_closed %in% prefiltered)
  expect_length(episodic_db_clusters_not_closed(con, "epidemic")$cluster_id, 0)
})

# ---------------------------------------------------------------------
# Screen-level spinners
# ---------------------------------------------------------------------

test_that("screen-level outputs get a spinner sized to the screen, and panels keep theirs", {
  css <- paste(
    readLines(
      system.file("app/www/episodic.css", package = "EpiSODIC"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_true(grepl("--episodic-spinner-size: 18px;", css, fixed = TRUE))
  expect_true(grepl(
    ".episodic-screen > .shiny-bound-output,",
    css,
    fixed = TRUE
  ))
  expect_true(grepl(
    "--episodic-spinner-size: clamp(3rem, 8vmin, 4.5rem);",
    css,
    fixed = TRUE
  ))
  expect_true(grepl(
    "width: var(--episodic-spinner-size);",
    css,
    fixed = TRUE
  ))
})
