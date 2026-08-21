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

test_that("episode_ui_format_datetime() converts stored UTC to the target timezone, not literal UTC", {
  # January: CET is UTC+1, no DST ambiguity. tz is passed explicitly rather
  # than via Sys.setenv(TZ = ...): Sys.timezone() caches its result for the
  # R session, so changing the env var after the first call has no effect.
  expect_equal(episode_ui_format_datetime("2025-01-15T10:00:00Z", tz = "Europe/Amsterdam"), "11:00")
  expect_equal(episode_ui_format_datetime("2025-01-15T10:00:00Z", fmt = "%d-%m-%Y %H:%M", tz = "Europe/Amsterdam"), "15-01-2025 11:00")
  expect_equal(episode_ui_format_datetime("2025-01-15T10:00:00Z", tz = "UTC"), "10:00")
})

test_that("episode_ui_format_datetime() returns 'unknown' for NA/NULL and the raw string for unparseable input", {
  expect_equal(episode_ui_format_datetime(NA), episode_tr("misc.unknown"))
  expect_equal(episode_ui_format_datetime(NULL), episode_tr("misc.unknown"))
  expect_equal(episode_ui_format_datetime("not-a-timestamp"), "not-a-timestamp")
})

test_that("episode_ui_rail() omits the ratio segment (not 'ratio NA') for a detector with no ratio, and shows a date range", {
  open <- data.frame(
    cluster_id = 1L, pathogen = "Norovirus", level_label = "L1", state = "new", state_label = "Nieuw",
    n_cases = 3L, ratio = NA_real_, first_day = "2025-01-07", last_day = "2025-01-15",
    stringsAsFactors = FALSE
  )
  html <- as.character(episode_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_false(grepl("ratio NA", html, fixed = TRUE))
  expect_true(grepl("7-15 jan. 2025", html, fixed = TRUE))

  open$ratio <- 2.3456
  html <- as.character(episode_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_true(grepl("ratio 2.3", html, fixed = TRUE))
})

test_that("episode_palette() and episode_brand_bar() return usable hex colours, role-named", {
  pal <- episode_palette()
  expect_type(pal, "list")
  expect_true(all(c("ink", "muted", "primary", "secondary", "tertiary", "success", "warning", "danger") %in% names(pal)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", unlist(pal))))

  bar <- episode_brand_bar()
  expect_length(bar, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", bar)))
})

test_that("an instance palette override propagates, other roles keep the shipped default", {
  override_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(primary = "#123456"), override_path)

  pal <- episode_palette_config_resolve(override_path)
  expect_equal(pal$primary, "#123456")
  expect_false(identical(pal$primary_dark, "#123456"))  # untouched keys keep the shipped default
  expect_false(identical(pal$secondary, "#123456"))
})

test_that("episode_ui_code_join() wraps each item in <code> and escapes, HTML-safe", {
  expect_equal(episode_ui_code_join(c("same_place", "farrington"), sep = " en "),
               "<code>same_place</code> en <code>farrington</code>")
  expect_equal(episode_ui_code_join("a&b<c>"), "<code>a&amp;b&lt;c&gt;</code>")
})

test_that("episode_ui_info_screen() renders in both languages and names every detector and state in code style", {
  for (lang in c("nl", "en")) {
    html <- as.character(episode_ui_info_screen(lang))
    expect_true(grepl("<code>farringtonFlexible</code>", html, fixed = TRUE))
    expect_true(grepl("<code>same_place</code>", html, fixed = TRUE))
    expect_true(grepl("<code>rare_trigger</code>", html, fixed = TRUE))
    # a translation miss falls through to episode_tr()'s "[[key]]" placeholder
    expect_false(grepl("[[", html, fixed = TRUE))
  }
})

test_that("app widgets render to shiny tags without error, including empty-data edge cases", {
  expect_s3_class(episode_ui_chip("Norovirus", "#4A647D", filled = TRUE), "shiny.tag")
  expect_s3_class(episode_ui_panel("Title", shiny::tags$p("body")), "shiny.tag")
  expect_s3_class(episode_ui_panel_empty("Title", "Nothing here"), "shiny.tag")
  expect_s3_class(episode_ui_stat("Label", 5, sub = "sub"), "shiny.tag")

  rows <- data.frame(label = c("A", "B"), n = c(3, 1))
  expect_s3_class(episode_ui_bars(rows), "shiny.tag.list")
  expect_s3_class(episode_ui_bars(data.frame(label = character(0), n = integer(0))), "shiny.tag")

  demo <- data.frame(band = c("0-19", "20-39"), m = c(2, 1), v = c(1, 3))
  expect_s3_class(episode_ui_pyramid(demo), "shiny.tag.list")
  expect_s3_class(episode_ui_pyramid(data.frame(band = character(0), m = integer(0), v = integer(0))), "shiny.tag")

  expect_s3_class(episode_ui_state_dot("new"), "shiny.tag")
  expect_type(episode_ui_state_colour("new"), "character")
  expect_type(episode_ui_state_colour("unknown_state"), "character")
})

test_that("episode_ui_italicise_taxon() HTML-escapes unconditionally", {
  out <- episode_ui_italicise_taxon(c("A&B <weird>", "Influenza A"))
  expect_equal(out[1], "A&amp;B &lt;weird&gt;")
})

test_that("episode_ui_italicise_taxon() italicises AMR-recognised binomials, leaves other names alone", {
  out <- episode_ui_italicise_taxon(c("Escherichia coli", "Influenza A"))
  expect_equal(out[1], "<i>Escherichia coli</i>")
  expect_equal(out[2], "Influenza A")  # not a binomial AMR recognises -> unitalicised
})

test_that("episode_app_ui() assembles a full page without error, in both languages", {
  expect_s3_class(episode_app_ui("nl"), "shiny.tag.list")
  expect_s3_class(episode_app_ui("en"), "shiny.tag.list")
})

test_that("chart builders produce ggplot objects for typical and edge-case inputs", {
  curve <- data.frame(sample_date = as.Date("2025-01-01") + 0:3, n_cases = c(1, 2, 0, 3), incomplete = c(FALSE, FALSE, TRUE, TRUE))
  expect_s3_class(episode_ui_epi_curve_chart(curve, lang = "nl"), "ggplot")
  expect_s3_class(episode_ui_epi_curve_chart(curve, lang = "en"), "ggplot")

  trend <- data.frame(week_start = as.Date("2025-01-06") + (0:5) * 7, n_cases = c(1, 2, 3, 4, 2, 1),
                       expected = c(1, 1, 1, 1, 1, 1), upperbound = c(2, 2, 2, 2, 2, 2))
  expect_s3_class(episode_ui_trend_chart(trend), "ggplot")

  series <- data.frame(week_start = as.Date("2025-01-06") + (0:3) * 7, n_tests = c(10, 12, 8, 15),
                        n_cases = c(1, 3, 2, 4), positivity = c(0.1, 0.25, 0.25, 0.27))
  expect_s3_class(episode_ui_denominator_chart(series), "ggplot")
})
