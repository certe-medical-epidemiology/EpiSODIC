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

# Three winters of a seasonal pathogen, plus a handful of a non-seasonal
# one, so the screen has something to describe at both settings.
pathogen_screen_setup <- function() {
  con <- episodic_test_db()
  pathogen_config <- data.frame(
    pathogen = c("Influenza A", "Campylobacter"),
    episode_days = c(14, 30), incub_min_days = c(1, 1), incub_max_days = c(4, 7),
    case_free_days = c(14, 14), cooldown_days = c(14, 14),
    rt_applicable = c(1, 0), si_mean_days = c(2.6, NA), si_sd_days = c(1.1, NA),
    si_dist = c("gamma", NA), mem_applicable = c(1, 0), severity_weight = c(0.6, 0.7),
    source_ref = NA, stringsAsFactors = FALSE
  )
  episodic_db_pathogen_config_load(con, pathogen_config)

  stream_id <- episodic_db_stream_upsert(
    con, stream_key = episodic_stream_key("pathogen_region", "Influenza A", region_code = "REGION"),
    level = "pathogen_region", pathogen = "Influenza A", region_code = "REGION",
    observed_date = "2025-03-01"
  )
  run_id <- episodic_db_run_start(con, "host", "account")

  # Winter peaks in January of 2023, 2024 and 2025, rising each year.
  flu_dates <- unlist(lapply(seq_along(c(2023, 2024, 2025)), function(i) {
    year <- c(2023, 2024, 2025)[i]
    peak <- as.Date(sprintf("%d-01-15", year))
    as.character(rep(peak + seq(-28, 28, by = 7), times = i * 2))
  }))
  gi_dates <- as.character(as.Date("2024-08-01") + seq(0, 60, by = 6))

  dates <- c(flu_dates, gi_dates)
  pathogens <- c(rep("Influenza A", length(flu_dates)), rep("Campylobacter", length(gi_dates)))
  n <- length(dates)

  cases <- data.frame(
    source_key = sprintf("PS%04d", seq_len(n)),
    patient_key = sprintf("PP%04d", seq_len(n)),
    sample_date = dates, receipt_date = dates,
    pathogen = pathogens, care_line = "first",
    institution_id = NA_integer_, ward = NA_character_, specialism = NA_character_,
    pc = rep(c("9711", "9712", "9713"), length.out = n),
    sex = rep(c("M", "F"), length.out = n),
    age = rep(c(4, 31, 68, 82), length.out = n),
    first_seen_run = run_id, stringsAsFactors = FALSE
  )
  episodic_db_case_insert_new(con, cases, run_id)
  episodic_db_run_finish(con, run_id, status = "success", n_streams = 1, n_detections = 0)

  list(con = con, stream_id = stream_id, run_id = run_id)
}

test_that("episodic_app_resolve_period() turns a season preset into week 40 through week 20", {
  period <- episodic_app_resolve_period("season_current", asof = as.Date("2025-01-15"))
  expect_equal(period$season, "2024/2025")
  expect_equal(period$from, as.Date("2024-09-30"))
  # Never runs past the day the data is current as of.
  expect_equal(period$to, as.Date("2025-01-15"))
  expect_equal(period$previous$label, "2023/2024")
})

test_that("episodic_app_resolve_period() out of season still names the season that just ended", {
  # Mid-July is outside week 40-20 entirely; the answer is not "none".
  period <- episodic_app_resolve_period("season_current", asof = as.Date("2025-07-15"))
  expect_equal(period$season, "2024/2025")
  expect_equal(period$to, as.Date("2025-05-18"))
})

test_that("episodic_app_resolve_period() steps back one whole season for the previous preset", {
  period <- episodic_app_resolve_period("season_previous", asof = as.Date("2025-01-15"))
  expect_equal(period$season, "2023/2024")
  expect_equal(period$to, as.Date("2024-05-19"))
})

test_that("episodic_app_resolve_period() falls back rather than erroring on an unusable custom range", {
  asof <- as.Date("2025-01-15")
  # Blank date pickers hand over NULL, which as.Date() turns into a
  # zero-length Date - the shape that used to break the is.na() guard.
  blank <- episodic_app_resolve_period("custom", from = NULL, to = NULL, asof = asof)
  expect_equal(blank$id, "last_12m")
  expect_equal(blank$to, asof)

  reversed <- episodic_app_resolve_period("custom", from = "2025-01-10", to = "2024-01-10", asof = asof)
  expect_equal(reversed$id, "last_12m")

  nonsense <- episodic_app_resolve_period("not-a-preset", asof = asof)
  expect_equal(nonsense$season, "2024/2025")
})

test_that("episodic_app_resolve_period() honours an explicit custom range", {
  period <- episodic_app_resolve_period("custom", from = "2024-03-01", to = "2024-06-30",
                                         asof = as.Date("2025-01-15"))
  expect_equal(period$from, as.Date("2024-03-01"))
  expect_equal(period$to, as.Date("2024-06-30"))
  # The comparison period is the equally long window ending the day before.
  expect_equal(period$previous$to, as.Date("2024-02-29"))
  expect_equal(as.integer(period$to - period$from), as.integer(period$previous$to - period$previous$from))
})

test_that("episodic_app_weekly_counts() zero-fills weeks with no cases", {
  dates <- as.Date(c("2025-01-06", "2025-01-06", "2025-01-27"))
  weekly <- episodic_app_weekly_counts(dates, from = as.Date("2025-01-06"), to = as.Date("2025-01-27"))
  expect_equal(nrow(weekly), 4)
  expect_equal(weekly$n_cases, c(2L, 0L, 0L, 1L))
  expect_equal(weekly$week_start[1], as.Date("2025-01-06"))
})

test_that("episodic_app_pathogen_weekly() fades only the weeks still reporting", {
  cases <- data.frame(sample_date = as.Date(c("2025-01-06", "2025-01-13", "2025-01-20")))
  resolved <- list(from = as.Date("2025-01-06"), to = as.Date("2025-01-26"))
  weekly <- episodic_app_pathogen_weekly(cases, resolved, incomplete_days = 5L,
                                          asof = as.Date("2025-01-26"))
  expect_equal(nrow(weekly), 3)
  expect_equal(weekly$incomplete, c(FALSE, FALSE, TRUE))

  # And nothing at all when reporting is complete same-day.
  complete <- episodic_app_pathogen_weekly(cases, resolved, incomplete_days = 0L,
                                            asof = as.Date("2025-02-28"))
  expect_false(any(complete$incomplete))
})

test_that("episodic_app_pathogen_overlay() lays seasons out week 40 first, not week 1 first", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  cases <- episodic_db_cases_for_pathogen(env$con, "Influenza A")
  cases$sample_date <- as.Date(cases$sample_date)

  overlay <- episodic_app_pathogen_overlay(
    cases, list(to = as.Date("2025-01-15")), seasonal = TRUE
  )
  expect_equal(overlay$kind, "season")
  expect_equal(overlay$current, "2024/2025")
  # A season runs 40..52 then 1..20; ordering it numerically would put
  # January to the left of October.
  expect_equal(overlay$rows$week_label[1], "40")
  expect_equal(max(overlay$rows$week_index), 33)
  expect_true(all(c("2022/2023", "2023/2024", "2024/2025") %in% overlay$groups))
})

test_that("episodic_app_pathogen_overlay() uses calendar years for a non-seasonal pathogen", {
  # A season boundary drawn through October would cut a summer peak in
  # half and scatter it across two lines.
  dates <- as.Date(c("2023-07-01", "2023-07-08", "2024-07-01", "2024-07-08"))
  overlay <- episodic_app_pathogen_overlay(
    data.frame(sample_date = dates), list(to = as.Date("2024-08-01")), seasonal = FALSE
  )
  expect_equal(overlay$kind, "year")
  expect_equal(overlay$current, "2024")
  expect_equal(max(overlay$rows$week_index), 52)
  expect_equal(sum(overlay$rows$n_cases), 4)
})

test_that("episodic_app_pathogen_overlay() returns NULL with only one period of data", {
  overlay <- episodic_app_pathogen_overlay(
    data.frame(sample_date = as.Date(c("2024-07-01", "2024-07-08"))),
    list(to = as.Date("2024-08-01")), seasonal = FALSE
  )
  expect_null(overlay)
})

test_that("episodic_app_pathogen_summary() compares against the previous period and refuses a ratio to zero", {
  cases <- data.frame(sample_date = as.Date(c("2025-01-06", "2025-01-07", "2025-01-08")))
  resolved <- list(from = as.Date("2025-01-06"), to = as.Date("2025-01-12"),
                    previous = list(from = as.Date("2024-12-30"), to = as.Date("2025-01-05"),
                                    label = NA_character_))
  summary <- episodic_app_pathogen_summary(cases, cases, resolved)
  expect_equal(summary$n_cases, 3)
  expect_equal(summary$n_previous, 0)
  # "Up from nothing" is a sentence, not a percentage.
  expect_true(is.na(summary$change_pct))
  expect_equal(summary$peak_n, 3L)
})

test_that("episodic_app_pathogen_screen() describes the commonest pathogen by default", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episodic_app_pathogen_screen(env$con, period = "all", lang = "en")

  expect_equal(screen$pathogen, "Influenza A")
  expect_true(screen$seasonal)
  expect_gt(nrow(screen$pathogens), 1)
  expect_gt(screen$summary$n_cases, 0)
  expect_gt(nrow(screen$weekly), 50)
  expect_false(is.null(screen$concentration))
  expect_setequal(screen$concentration$rows$label, c("9711", "9712", "9713"))
  expect_false(is.null(screen$demography$bands))
})

test_that("episodic_app_pathogen_screen() honours an explicit pathogen and reports it as non-seasonal", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episodic_app_pathogen_screen(env$con, pathogen = "Campylobacter", period = "all", lang = "en")

  expect_equal(screen$pathogen, "Campylobacter")
  expect_false(screen$seasonal)
  expect_null(screen$mem)
  # rt_applicable is 0 for this pathogen, so Rt is suppressed entirely
  # rather than shown with a caveat.
  expect_null(screen$rt)
  expect_true(is.na(screen$rt_unavailable_reason))
})

test_that("episodic_app_pathogen_screen() falls back to the commonest pathogen for an unknown one", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episodic_app_pathogen_screen(env$con, pathogen = "Not a pathogen", period = "all", lang = "en")
  expect_equal(screen$pathogen, "Influenza A")
})

test_that("episodic_app_pathogen_screen() copes with an empty database", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  screen <- episodic_app_pathogen_screen(con, lang = "en")
  expect_equal(nrow(screen$pathogens), 0)
  expect_null(screen$pathogen)
  expect_false(is.null(screen$period))
})

test_that("episodic_app_pathogen_rt() clips to the period but conditions on history before it", {
  skip_if_not_installed("EpiEstim")
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  cases <- episodic_db_cases_for_pathogen(env$con, "Influenza A")
  cases$sample_date <- as.Date(cases$sample_date)
  pc <- episodic_db_pathogen_config_get(env$con, "Influenza A")

  resolved <- list(from = as.Date("2024-12-01"), to = as.Date("2025-02-15"))
  rt <- episodic_app_pathogen_rt(cases, pc, resolved, incomplete_days = 0L,
                                  asof = as.Date("2025-02-15"))
  skip_if(is.null(rt), "not enough synthetic history for an Rt window")
  expect_true(all(rt$window_end >= resolved$from))
  expect_true(all(rt$window_end <= resolved$to))
  # The first estimate falls inside the period rather than being pushed
  # past the burn-in of a series that started at the period boundary.
  expect_lt(as.integer(min(rt$window_end) - resolved$from), 21)
})

test_that("episodic_app_pathogen_clusters() links the epidemiological view back to the signals", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  cluster_id <- episodic_db_cluster_insert(
    env$con, stream_id = env$stream_id, first_day = "2025-01-08", last_day = "2025-01-22",
    n_cases = 12, expected = 3, excess = 5, ratio = 4, priority_score = 70,
    detector_agreement = 2, run_id = env$run_id
  )

  inside <- episodic_app_pathogen_clusters(
    env$con, "Influenza A",
    list(from = as.Date("2025-01-01"), to = as.Date("2025-01-31")), lang = "en"
  )
  expect_equal(nrow(inside), 1)
  expect_equal(inside$cluster_id[1], cluster_id)
  expect_equal(inside$level_label[1], episodic_tr("level.pathogen_region", lang = "en"))
  expect_equal(inside$state_label[1], episodic_tr("state.new", lang = "en"))

  outside <- episodic_app_pathogen_clusters(
    env$con, "Influenza A",
    list(from = as.Date("2023-01-01"), to = as.Date("2023-01-31")), lang = "en"
  )
  expect_equal(nrow(outside), 0)
})

test_that("episodic_ui_pathogen_screen() renders without error, empty database included", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episodic_app_pathogen_screen(env$con, period = "all", lang = "en")
  html <- as.character(episodic_ui_pathogen_screen(screen, lang = "en"))
  expect_true(grepl("pathogen_select", html, fixed = TRUE))
  expect_true(grepl("pathogen_period", html, fixed = TRUE))
  # every panel title resolves to real wording, not a [[missing.key]]
  for (key in c("pathogen.panel.curve.title", "pathogen.panel.overlay.title",
                 "pathogen.panel.rt.title", "pathogen.panel.clusters.title",
                 "pathogen.period.season_current")) {
    expect_true(grepl(episodic_tr(key, lang = "en"), html, fixed = TRUE), info = key)
  }

  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  empty_html <- as.character(episodic_ui_pathogen_screen(episodic_app_pathogen_screen(con, lang = "en"), lang = "en"))
  expect_true(grepl(episodic_tr("pathogen.empty", lang = "en"), empty_html, fixed = TRUE))
})

test_that("episodic_ui_intensity_colour() gives every MEM band a colour and never fails on an unknown one", {
  levels <- c("baseline", "low", "medium", "high", "very_high")
  colours <- vapply(levels, episodic_ui_intensity_colour, character(1))
  expect_equal(length(unique(colours)), length(levels))
  expect_true(is.character(episodic_ui_intensity_colour(NA_character_)))
})

test_that("episodic_mem_threshold_lines() orders thresholds by value and labels each one", {
  thresholds <- list(pre_epidemic = 5, post_epidemic = 4,
                      intensity = c(medium = 10, high = 20, very_high = 40),
                      seasons_used = c("2022/2023", "2023/2024"))
  lines <- episodic_mem_threshold_lines(thresholds, lang = "en")
  expect_equal(nrow(lines), 5)
  expect_false(is.unsorted(lines$value))
  expect_false(any(grepl("^\\[\\[", lines$label)))

  expect_null(episodic_mem_threshold_lines(NULL))
  # A fit that produced no usable numbers leaves the bands off rather
  # than taking the panel down.
  expect_null(episodic_mem_threshold_lines(list(pre_epidemic = NA_real_, post_epidemic = NA_real_)))
})

test_that("episodic_chart_week_axis() labels weeks, not just the odd month", {
  # ggplot2's default date scale gave two ticks - "Oct" and "Jan" - for a
  # quarter's worth of weeks, which is not enough to name the week a rise
  # started in.
  weeks <- seq(as.Date("2024-09-30"), by = "week", length.out = 14)
  axis <- episodic_chart_week_axis(weeks, lang = "en")
  expect_equal(length(axis$breaks), length(axis$labels))
  expect_gte(length(axis$breaks), 10)
  # ISO week number over the month it falls in
  expect_match(axis$labels[1], "^w40\n")
  expect_match(axis$labels[1], "Oct", fixed = TRUE)
  # the year is stated once at the start, then again only when it turns
  expect_equal(sum(grepl("2024", axis$labels, fixed = TRUE)), 1)
  expect_equal(sum(grepl("2025", axis$labels, fixed = TRUE)), 1)
})

test_that("episodic_chart_week_axis() puts a year-straddling week in the right year", {
  # The week beginning 30 December 2024 is ISO week 1 of 2025. Labelling
  # it from its Monday would read "w01 / Dec 2024" and never announce the
  # turn of the year at all.
  weeks <- seq(as.Date("2024-12-16"), by = "week", length.out = 4)
  axis <- episodic_chart_week_axis(weeks, lang = "en")
  expect_equal(axis$labels[3], "w01\nJan 2025")  # 16 Dec, 23 Dec, 30 Dec, 6 Jan
  expect_match(axis$labels[1], "^w51\nDec")
})

test_that("episodic_chart_week_axis() thins its ticks rather than crowding them", {
  weeks <- seq(as.Date("2023-01-02"), by = "week", length.out = 52)
  axis <- episodic_chart_week_axis(weeks, lang = "en", max_labels = 14L)
  expect_lte(length(axis$breaks), 14)
  expect_true(all(axis$breaks %in% weeks))
})

test_that("episodic_chart_week_axis() drops week numbers once the span is years", {
  weeks <- seq(as.Date("2020-01-06"), by = "week", length.out = 260)
  axis <- episodic_chart_week_axis(weeks, lang = "en")
  expect_lte(length(axis$breaks), 14)
  # week numbers stop being the useful unit; month and year take over
  expect_false(any(grepl("^w[0-9]", axis$labels)))
  expect_true(all(grepl("20[0-9]{2}$", axis$labels)))
})

test_that("episodic_chart_week_axis() copes with an empty or single-week series", {
  expect_null(episodic_chart_week_axis(as.Date(character(0))))
  one <- episodic_chart_week_axis(as.Date("2025-01-06"), lang = "en")
  expect_equal(length(one$breaks), 1)
})

test_that("episodic_chart_month_abbrevs() uses the same month names as the date ranges", {
  months <- episodic_chart_month_abbrevs(lang = "en")
  expect_equal(length(months), 12)
  expect_equal(months[1], episodic_tr("date.month.01", lang = "en"))
  expect_false(any(grepl("^\\[\\[", months)))
})

test_that("the signals table leads with the cluster id", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  cluster_id <- episodic_db_cluster_insert(
    env$con, stream_id = env$stream_id, first_day = "2025-01-08", last_day = "2025-01-22",
    n_cases = 12, expected = 3, excess = 5, ratio = 4, priority_score = 70,
    detector_agreement = 2, run_id = env$run_id
  )
  screen <- episodic_app_pathogen_screen(env$con, period = "all", lang = "en")
  html <- as.character(episodic_ui_pathogen_clusters_panel(screen, lang = "en"))

  expect_true(grepl(paste0(">", episodic_tr("dossier.cluster_ref", id = cluster_id, lang = "en"), "<"),
                     html, fixed = TRUE))
  expect_true(grepl("episodic-cell-id", html, fixed = TRUE))
  # id column first: its header precedes the period header
  expect_lt(regexpr(episodic_tr("pathogen.panel.clusters.col.id", lang = "en"), html, fixed = TRUE),
             regexpr(episodic_tr("pathogen.panel.clusters.col.period", lang = "en"), html, fixed = TRUE))
})

test_that("each cluster row links through to its dossier, by click and by keyboard", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  cluster_id <- episodic_db_cluster_insert(
    env$con, stream_id = env$stream_id, first_day = "2025-01-08", last_day = "2025-01-22",
    n_cases = 12, expected = 3, excess = 5, ratio = 4, priority_score = 70,
    detector_agreement = 2, run_id = env$run_id
  )
  screen <- episodic_app_pathogen_screen(env$con, period = "all", lang = "en")
  html <- as.character(episodic_ui_pathogen_clusters_panel(screen, lang = "en"))

  expect_true(grepl("open_cluster", html, fixed = TRUE))
  expect_true(grepl(as.character(cluster_id), html, fixed = TRUE))
  expect_true(grepl("episodic-row-link", html, fixed = TRUE))
  # a <tr> has no keyboard access of its own
  expect_true(grepl("tabindex", html, fixed = TRUE))
  expect_true(grepl("onkeydown", html, fixed = TRUE))
})

test_that("the clusters panel is titled for clusters, not for signals", {
  # They carry a verdict and a state; a signal is the detection that
  # started one, which is a different thing this codebase already names.
  title <- episodic_tr("pathogen.panel.clusters.title", lang = "en")
  expect_match(title, "[Cc]luster")
  expect_false(grepl("signal", title, ignore.case = TRUE))
  expect_match(episodic_tr("pathogen.panel.clusters.title", lang = "nl"), "Clusters", fixed = TRUE)
})

test_that("the pathogen picker says what its number counts", {
  env <- pathogen_screen_setup()
  on.exit(DBI::dbDisconnect(env$con))
  screen <- episodic_app_pathogen_screen(env$con, period = "all", lang = "en")
  html <- as.character(episodic_ui_pathogen_controls(screen, lang = "en"))
  # a bare "Influenza A (108)" reads as an identifier; the unit fixes that
  expect_true(grepl("cases in total", html, fixed = TRUE))
  expect_false(grepl("Influenza A (108)", html, fixed = TRUE))
})

test_that("the overlay stops the period in progress at the current week", {
  # A year still running was zero-filled to week 52, drawing a long flat
  # line along zero through weeks that have not happened - the most
  # prominent mark on a chart meant for comparing shapes, and not data.
  dates <- as.Date(c("2024-07-01", "2024-07-08", "2025-01-06", "2025-01-13"))
  overlay <- episodic_app_pathogen_overlay(
    data.frame(sample_date = dates), list(to = as.Date("2025-01-20")),
    seasonal = FALSE, asof = as.Date("2025-01-20")
  )
  running <- overlay$rows[overlay$rows$group == "2025", ]
  # week 4 of 2025 contains 20 January; everything after it is unobserved
  expect_false(any(is.na(running$n_cases[running$week_index <= 4])))
  expect_true(all(is.na(running$n_cases[running$week_index > 4])))

  # the finished year keeps its zeros: a quiet week there was observed
  finished <- overlay$rows[overlay$rows$group == "2024", ]
  expect_false(any(is.na(finished$n_cases)))
  expect_equal(sum(finished$n_cases), 2)
})

test_that("episodic_app_overlay_truncate() leaves a completed season alone", {
  week_order <- c(as.character(40:52), as.character(1:20))
  rows <- data.frame(group = rep("2023/2024", 33), week_index = seq_along(week_order),
                      week_label = week_order, n_cases = 1L, stringsAsFactors = FALSE)

  # mid-July is outside the week 40-20 window: that season is over, so
  # every one of its weeks was genuinely observed
  untouched <- episodic_app_overlay_truncate(rows, week_order, "2023/2024", seasonal = TRUE,
                                              asof = as.Date("2024-07-15"))
  expect_false(any(is.na(untouched$n_cases)))

  # and with no asof at all, nothing is assumed
  expect_false(any(is.na(episodic_app_overlay_truncate(rows, week_order, "2023/2024",
                                                        seasonal = TRUE, asof = NULL)$n_cases)))
})

test_that("episodic_app_overlay_truncate() cuts a season in progress at its current week", {
  week_order <- c(as.character(40:52), as.character(1:20))
  rows <- data.frame(group = rep("2024/2025", 33), week_index = seq_along(week_order),
                      week_label = week_order, n_cases = 1L, stringsAsFactors = FALSE)
  # 15 January 2025 is ISO week 3, which is position 16 in a season
  # running 40..52 then 1..20
  out <- episodic_app_overlay_truncate(rows, week_order, "2024/2025", seasonal = TRUE,
                                        asof = as.Date("2025-01-15"))
  expect_equal(which(is.na(out$n_cases)), 17:33)
})

test_that("episodic_chart_theme() sets axis and legend text readably", {
  pal <- episodic_palette()
  theme <- episodic_chart_theme()
  # faint is a hairline colour for rules; an axis set in it is one you
  # stop reading, and the axis is how you name the week a rise started
  expect_false(identical(theme$axis.text$colour, pal$faint))
  expect_equal(theme$axis.text$colour, pal$muted)
  expect_gte(theme$axis.text$size, 10)
  expect_gte(theme$legend.text$size, 10)
})
