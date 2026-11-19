# ===================================================================== #
#  This open-source project is publicly available at:                   #
#  https://github.com/msberends/EpiSODIC                                #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at the University Medical Center Groningen, and non-profit #
#  organisation Certe Medical Diagnostics & Advice, in the Netherlands. #
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

test_that("episodic_ui_format_datetime() converts stored UTC to the target timezone, not literal UTC", {
  # January: CET is UTC+1, no DST ambiguity. tz is passed explicitly rather
  # than via Sys.setenv(TZ = ...): Sys.timezone() caches its result for the
  # R session, so changing the env var after the first call has no effect.
  expect_equal(
    episodic_ui_format_datetime(
      "2025-01-15T10:00:00Z",
      tz = "Europe/Amsterdam"
    ),
    "11:00"
  )
  expect_equal(
    episodic_ui_format_datetime(
      "2025-01-15T10:00:00Z",
      fmt = "%d-%m-%Y %H:%M",
      tz = "Europe/Amsterdam"
    ),
    "15-01-2025 11:00"
  )
  expect_equal(
    episodic_ui_format_datetime("2025-01-15T10:00:00Z", tz = "UTC"),
    "10:00"
  )
})

test_that("episodic_ui_format_datetime() returns 'unknown' for NA/NULL and the raw string for unparseable input", {
  expect_equal(episodic_ui_format_datetime(NA), episodic_tr("misc.unknown"))
  expect_equal(episodic_ui_format_datetime(NULL), episodic_tr("misc.unknown"))
  expect_equal(
    episodic_ui_format_datetime("not-a-timestamp"),
    "not-a-timestamp"
  )
})

test_that("episodic_ui_rail() shows a full-month date range and the priority score, never a ratio", {
  open <- data.frame(
    cluster_id = 1L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "Nieuw",
    n_cases = 3L,
    priority_score = NA_real_,
    first_day = "2025-01-07",
    last_day = "2025-01-15",
    stringsAsFactors = FALSE
  )
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_false(grepl("ratio", html, fixed = TRUE))
  expect_false(grepl("score NA", html, fixed = TRUE))
  expect_true(grepl("7-15 januari 2025", html, fixed = TRUE))

  open$priority_score <- 2.3456
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_true(grepl("score 2", html, fixed = TRUE))
})

test_that("episodic_ui_rail() marks each row with the cluster id every other opener reads back", {
  open <- data.frame(
    cluster_id = 7L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "Nieuw",
    n_cases = 3L,
    priority_score = NA_real_,
    first_day = "2025-01-07",
    last_day = "2025-01-15",
    stringsAsFactors = FALSE
  )
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  # A click that opens a cluster from outside the rail (a table row, a
  # "linked to #N" chip) has to be able to find and mark this exact row
  # - see setOutbreak() in inst/app/www/episodic-nav.js.
  expect_true(grepl('data-cluster-id="7"', html, fixed = TRUE))
})

test_that("episodic_ui_rail() renders a care-line chip for a recorded care line, and none for an unknown one", {
  open <- data.frame(
    cluster_id = 1L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "Nieuw",
    n_cases = 3L,
    priority_score = 50,
    first_day = "2025-01-07",
    last_day = "2025-01-15",
    care_line = "second",
    stringsAsFactors = FALSE
  )
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_true(grepl("2e lijn", html, fixed = TRUE))

  # "other" is a care line cases were found in, and says so.
  open$care_line <- "other"
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_true(grepl("episodic-chip", html, fixed = TRUE))
  expect_true(grepl("overig", html, fixed = TRUE))

  # "unknown" and a missing care line record nothing, so no chip.
  open$care_line <- "unknown"
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_false(grepl("episodic-chip", html, fixed = TRUE))

  open$care_line <- NA_character_
  html <- as.character(episodic_ui_rail(open, selected_id = NULL, lang = "nl"))
  expect_false(grepl("episodic-chip", html, fixed = TRUE))
})

# Writes `x` as an instance palette file and returns its path, removed
# when the calling test ends.
palette_file <- function(x, env = parent.frame()) {
  path <- tempfile(fileext = ".yaml")
  if (is.character(x)) writeLines(x, path) else yaml::write_yaml(x, path)
  withr::defer(unlink(path), envir = env)
  path
}

problem_reasons <- function(path) {
  vapply(episodic_palette_problems(path), function(p) p$reason, character(1))
}

test_that("episodic_palette() returns every role, component and derived variant as a hex colour", {
  pal <- episodic_palette()
  expect_type(pal, "list")
  expect_true(all(c(
    "ink", "muted", "surface", "bg",
    "primary", "primary_strong", "primary_soft", "primary_tint",
    "secondary", "tertiary", "success", "warning", "danger",
    paste0("severity_", episodic_severity_steps),
    "header_navbar", "header_brand_accent", "header_brand_bar",
    paste0("state_", c("new", "assessing", "monitoring", "closed", "reassess")),
    paste0("care_line_", c("first", "second", "third", "other")),
    paste0("chip_", c("level", "season", "during_epidemic", "linked_cluster", "flag", "mute_reason")),
    paste0("trend_", c("rising", "falling", "steady")),
    "chart_bars", "chart_mem_start", "chart_mem_end",
    paste0(episodic_palette_text_roles, "_text"),
    paste0(episodic_palette_text_roles, "_on"),
    "header_navbar_on"
  ) %in% names(pal)))
  colour_roles <- pal[!names(pal) %in% c("font", "font_size_base")]
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", unlist(colour_roles))))

  expect_true(nzchar(pal$font))
  expect_true(grepl("^[0-9.]+(px|rem|em|pt)$", pal$font_size_base))

  bar <- episodic_brand_bar()
  expect_length(bar, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", bar)))
})

test_that("the shipped palette and the template both resolve without a problem, and hold the same keys", {
  shipped <- episodic_palette_shipped()
  expect_true(is.list(shipped))
  template <- system.file("config", "episodic_style_template.yaml", package = "EpiSODIC")
  expect_true(nzchar(template))
  expect_no_warning(pal <- episodic_palette_config_resolve(template))
  expect_length(episodic_palette_problems(template), 0)
  expect_equal(pal, shipped)
  # Every key the shipped file has, the template has, at the same place.
  expect_equal(
    names(unlist(yaml::read_yaml(template))),
    names(unlist(episodic_palette_shipped_tree()))
  )
})

test_that("episodic_app_palette_css() injects colours, derived variants and typography, and no sequence", {
  css <- episodic_app_palette_css(episodic_palette())
  expect_true(grepl("^:root \\{", css))
  expect_true(grepl("--episodic-primary: #", css, fixed = TRUE))
  expect_true(grepl("--episodic-primary-text: #", css, fixed = TRUE))
  expect_true(grepl("--episodic-header-navbar-on: #", css, fixed = TRUE))
  expect_true(grepl("--episodic-font: ", css, fixed = TRUE))
  expect_true(grepl("--episodic-font-size-base: ", css, fixed = TRUE))
  expect_false(grepl("brand-bar", css, fixed = TRUE))
})

test_that("every CSS custom property the stylesheet reads from the palette is defined by it", {
  css_file <- system.file("app", "www", "episodic.css", package = "EpiSODIC")
  used <- unique(regmatches(
    readLines(css_file),
    gregexpr("var\\(--episodic-[a-z0-9-]+", readLines(css_file))
  ))
  used <- unique(sub("var(--episodic-", "", unlist(used), fixed = TRUE))
  defined <- gsub("_", "-", names(episodic_palette()), fixed = TRUE)
  # Defined by the stylesheet itself rather than by the palette.
  own <- c("accent", "accent-text", "spinner-size", "spinner-stroke", "spinner-top")
  expect_setequal(setdiff(used, c(defined, own)), character(0))
})

test_that("a role set in the file reaches every component that names it", {
  pal <- episodic_palette_config_resolve(palette_file(list(brand = list(primary = "#123456"))))
  expect_equal(pal$primary, "#123456")
  expect_equal(pal$state_assessing, "#123456")
  expect_equal(pal$care_line_first, "#123456")
  expect_equal(pal$chip_level, "#123456")
  # Untouched keys keep the shipped default.
  expect_equal(pal$secondary, episodic_palette_shipped()$secondary)
  expect_equal(pal$primary_strong, episodic_palette_shipped()$primary_strong)
})

test_that("swatches are named once and followed by every role and component that names them", {
  path <- palette_file(list(
    swatches = list(house_blue = "#4A647D", house_lilac = "#CEB9D6"),
    brand = list(primary = "house_blue", secondary = "house_lilac"),
    components = list(chip = list(level = "brand.secondary"))
  ))
  expect_no_warning(pal <- episodic_palette_config_resolve(path))
  expect_length(episodic_palette_problems(path), 0)
  expect_equal(pal$primary, "#4A647D")
  expect_equal(pal$state_assessing, "#4A647D")
  expect_equal(pal$secondary, "#CEB9D6")
  expect_equal(pal$chip_level, "#CEB9D6")
  expect_equal(pal$chip_during_epidemic, "#CEB9D6")
  # A severity step is named with its section.
  pal <- episodic_palette_config_resolve(palette_file(list(
    components = list(trend = list(rising = "severity.high"))
  )))
  expect_equal(pal$trend_rising, pal$severity_high)
})

test_that("an instance palette override can change typography without touching colours", {
  path <- palette_file(list(typography = list(font = "Arial, sans-serif", font_size_base = "15px")))
  pal <- episodic_palette_config_resolve(path)
  expect_equal(pal$font, "Arial, sans-serif")
  expect_equal(pal$font_size_base, "15px")
  expect_equal(pal$primary, episodic_palette_shipped()$primary)

  css <- episodic_app_palette_css(pal)
  expect_true(grepl("--episodic-font: Arial, sans-serif;", css, fixed = TRUE))
  expect_true(grepl("--episodic-font-size-base: 15px;", css, fixed = TRUE))
})

test_that("an invalid value keeps the shipped value for that key alone, and says so by its path", {
  shipped <- episodic_palette_shipped()
  path <- palette_file(c(
    "brand:",
    "  primary: \"blue-ish\"",
    "  secondary: \"#123456\"",
    "typography:",
    "  font_size_base: \"13\""
  ))
  expect_warning(pal <- episodic_palette_config_resolve(path), "EPISODIC_STYLE")
  expect_equal(pal$primary, shipped$primary)
  expect_equal(pal$font_size_base, shipped$font_size_base)
  expect_equal(pal$secondary, "#123456")

  problems <- episodic_palette_problems(path)
  expect_setequal(problem_reasons(path), c("invalid_colour", "invalid_size"))
  expect_setequal(
    vapply(problems, function(p) p$key, character(1)),
    c("brand.primary", "typography.font_size_base")
  )
})

test_that("a name that names nothing falls back to the shipped value, and the components following it do too", {
  path <- palette_file(list(brand = list(primary = "house_blue")))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(pal$primary, episodic_palette_shipped()$primary)
  expect_equal(pal$state_assessing, episodic_palette_shipped()$primary)
  # Reported once, for the key that holds the name, not again by every
  # component that follows it.
  expect_equal(problem_reasons(path), "unresolved_reference")
  expect_equal(episodic_palette_problems(path)[[1]]$key, "brand.primary")
})

test_that("names that lead back to themselves are reported once and resolve to a colour", {
  path <- palette_file(list(brand = list(primary = "secondary", secondary = "primary")))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(problem_reasons(path), "circular_reference")
  expect_match(pal$primary, "^#[0-9A-Fa-f]{6}$")
  expect_match(pal$secondary, "^#[0-9A-Fa-f]{6}$")

  path <- palette_file(list(brand = list(primary = "primary")))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(problem_reasons(path), "circular_reference")
  expect_equal(pal$primary, episodic_palette_shipped()$primary)
})

test_that("a key of the earlier flat format is reported with where it now lives, and not read", {
  path <- palette_file(list(
    primary = "#123456",
    primary_dark = "#000000",
    danger_dark = "#000000"
  ))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(pal, episodic_palette_shipped())
  problems <- episodic_palette_problems(path)
  by_key <- stats::setNames(problems, vapply(problems, function(p) p$key, character(1)))
  expect_equal(by_key$primary$reason, "moved_key")
  expect_equal(by_key$primary$target, "brand.primary")
  expect_equal(by_key$primary_dark$target, "brand.primary_strong")
  expect_equal(by_key$danger_dark$reason, "retired_key")
  expect_match(
    episodic_palette_problem_texts(list(by_key$primary), lang = "en"),
    "brand.primary",
    fixed = TRUE
  )
})

test_that("a swatch may not take a role's name or an unusable one, and holds hex colours only", {
  path <- palette_file(list(swatches = list(
    primary = "#123456",
    House = "#123456",
    plum = "primary",
    teal = "#008080"
  )))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  problems <- episodic_palette_problems(path)
  keys <- vapply(problems, function(p) p$key, character(1))
  reasons <- stats::setNames(problem_reasons(path), keys)
  expect_equal(unname(reasons["swatches.primary"]), "invalid_swatch_name")
  expect_equal(unname(reasons["swatches.House"]), "invalid_swatch_name")
  expect_equal(unname(reasons["swatches.plum"]), "invalid_colour")
  expect_false("swatches.teal" %in% keys)
  expect_equal(pal$primary, episodic_palette_shipped()$primary)
})

test_that("a section that is not a mapping, and a key no section has, are reported and ignored", {
  path <- palette_file(list(
    brand = "#123456",
    status = list(dangr = "#123456"),
    components = list(statez = list(new = "primary"), state = list(neww = "primary")),
    colours = list(primary = "#123456")
  ))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  problems <- episodic_palette_problems(path)
  got <- stats::setNames(problem_reasons(path), vapply(problems, function(p) p$key, character(1)))
  expect_equal(unname(got["brand"]), "invalid_section")
  expect_equal(unname(got["status.dangr"]), "unknown_key")
  expect_equal(unname(got["components.statez"]), "unknown_key")
  expect_equal(unname(got["components.state.neww"]), "unknown_key")
  expect_equal(unname(got["colours"]), "unknown_key")
  expect_equal(pal, episodic_palette_shipped())
})

test_that("an ink or muted too light to read is reported and used as set", {
  path <- palette_file(list(neutral = list(ink = "#CCCCCC")))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(pal$ink, "#CCCCCC")
  problems <- episodic_palette_problems(path)
  expect_true(all(problem_reasons(path) == "low_contrast"))
  expect_setequal(
    vapply(problems, function(p) p$against, character(1)),
    c("neutral.surface", "neutral.bg")
  )
  expect_match(
    episodic_palette_problem_texts(problems[1], lang = "nl"),
    "1,6:1",
    fixed = TRUE
  )
})

test_that("a colour too light to read is darkened just enough, keeping its hue", {
  lilac <- "#CEB9D6"
  readable <- episodic_colour_readable(lilac, "#FFFFFF")
  expect_gte(episodic_colour_contrast(readable, "#FFFFFF"), 4.5)
  # Not black: the least darkening that reaches the contrast.
  expect_lt(episodic_colour_contrast(readable, "#FFFFFF"), 5.5)
  rgb <- episodic_colour_rgb(readable)
  expect_true(rgb[3] > rgb[2]) # still a purple, blue above green
  # A colour that already reads is left as it is.
  expect_equal(episodic_colour_readable("#222222", "#FFFFFF"), "#222222")
  # Against a dark background it is lightened instead.
  expect_gte(episodic_colour_contrast(episodic_colour_readable("#3A4D5D", "#2B3A46", 3), "#2B3A46"), 3)
  # Against several backgrounds, it reads on every one.
  both <- episodic_colour_readable("#93984C", c("#FFFFFF", "#E9EADB"))
  expect_gte(episodic_colour_contrast(both, "#E9EADB"), 4.5)
})

test_that("a pale role's text variant reads on the page, and chart colours reach 3:1", {
  path <- palette_file(list(brand = list(secondary = "#CEB9D6", tertiary = "#D4C230")))
  pal <- episodic_palette_config_resolve(path)
  expect_equal(pal$secondary, "#CEB9D6")
  for (role in c("secondary_text", "tertiary_text")) {
    expect_gte(episodic_colour_contrast(pal[[role]], pal$surface), 4.5)
    expect_gte(episodic_colour_contrast(pal[[role]], pal$bg), 4.5)
  }
  expect_gte(episodic_colour_contrast(pal$chart_mem_start, pal$surface), 3)
  expect_gte(episodic_colour_contrast(pal$chart_mem_end, pal$surface), 3)
  expect_gte(episodic_colour_contrast(pal$header_brand_accent, pal$header_navbar), 3)
})

test_that("an outlined chip in a pale colour still has readable text", {
  path <- palette_file(list(brand = list(secondary = "#CEB9D6")))
  withr::local_envvar(EPISODIC_STYLE = path)
  pal <- episodic_palette()
  html <- as.character(episodic_ui_chip("During E-1", pal$chip_during_epidemic))
  text <- sub(".*color:(#[0-9A-Fa-f]{6});.*", "\\1", html)
  expect_false(identical(toupper(text), "#CEB9D6"))
  expect_gte(episodic_colour_contrast(text, pal$surface), 4.5)
  expect_gte(episodic_colour_contrast(text, pal$bg), 4.5)
})

test_that("the brand bar takes any number of stripes, and is kept whole when one cannot be used", {
  path <- palette_file(list(components = list(header = list(brand_bar = c("primary", "#000000")))))
  pal <- episodic_palette_config_resolve(path)
  expect_equal(pal$header_brand_bar, c(pal$primary, "#000000"))

  path <- palette_file(list(components = list(header = list(brand_bar = c("primary", "nothing")))))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(pal$header_brand_bar, episodic_palette_shipped()$header_brand_bar)
  expect_equal(problem_reasons(path), "unresolved_reference")
})

test_that("the severity scale ships five roles, as Bootswatch Yeti's own colours", {
  pal <- episodic_palette_shipped()
  expect_equal(
    unlist(pal[paste0("severity_", episodic_severity_steps)], use.names = FALSE),
    c("#ADB5BD", "#43AC6A", "#E99002", "#F04124", "#222222")
  )
  # From medium up, each step is darker than the one before it, so the
  # top of the scale still reads in order without colour vision and in
  # greyscale print. Yeti's green and amber are close in lightness, so
  # low and medium are told apart by hue and by the words on the chip.
  ramp <- vapply(
    pal[paste0("severity_", c("medium", "high", "very_high"))],
    episodic_colour_luminance,
    numeric(1)
  )
  expect_false(is.unsorted(rev(ramp)))
  expect_equal(length(unique(unlist(pal[paste0("severity_", episodic_severity_steps)]))), 5)
})

test_that("every verdict and every intensity band is a step on one severity scale", {
  pal <- episodic_palette()
  expect_equal(episodic_ui_verdict_severity("artefact"), "baseline")
  expect_equal(episodic_ui_verdict_severity("expected_variation"), "baseline")
  expect_equal(episodic_ui_verdict_severity("season_ended"), "baseline")
  expect_equal(episodic_ui_verdict_severity("cluster_not_yet"), "low")
  expect_equal(episodic_ui_verdict_severity("season_not_yet"), "low")
  expect_equal(episodic_ui_verdict_severity("possible_epidemic"), "medium")
  expect_equal(episodic_ui_verdict_severity("confirmed_epidemic"), "high")
  expect_equal(episodic_ui_verdict_severity("season_started"), "high")
  expect_equal(episodic_ui_verdict_colour("confirmed_epidemic"), pal$severity_high)
  expect_equal(episodic_ui_verdict_colour("possible_epidemic"), pal$severity_medium)
  expect_equal(episodic_ui_verdict_colour("artefact"), pal$severity_baseline)
  # The top step is a measured intensity, never a judgement.
  verdicts <- c(
    "artefact", "expected_variation", "cluster_not_yet", "possible_epidemic",
    "confirmed_epidemic", "season_started", "season_not_yet", "season_ended"
  )
  expect_false("very_high" %in% vapply(verdicts, episodic_ui_verdict_severity, character(1)))

  # Something that is not a step - no verdict, an unknown one, no
  # intensity measured - is muted, never drawn as the baseline step.
  expect_true(is.na(episodic_ui_verdict_severity(NA_character_)))
  expect_true(is.na(episodic_ui_verdict_severity("not_a_verdict")))
  expect_equal(episodic_ui_verdict_colour("not_a_verdict"), pal$muted)
  expect_equal(episodic_ui_severity_colour(NA_character_), pal$muted)
  expect_equal(episodic_ui_severity_colour(character(0)), pal$muted)
  expect_equal(episodic_ui_severity_colour("extreme"), pal$muted)
})

test_that("an instance recolours the severity scale for verdicts and intensity bands at once", {
  path <- palette_file(list(severity = list(high = "#7F0000")))
  withr::local_envvar(EPISODIC_STYLE = path)

  expect_equal(episodic_ui_verdict_colour("confirmed_epidemic"), "#7F0000")
  expect_equal(episodic_ui_intensity_colour("high"), "#7F0000")
  expect_equal(episodic_ui_severity_colour("medium"), episodic_palette_shipped()$severity_medium)
  expect_length(episodic_palette_problems(path), 0)
})

test_that("text on a filled colour is whichever of ink, white and black reads on it", {
  pal <- episodic_palette()
  expect_equal(episodic_colour_contrast("#000000", "#FFFFFF"), 21)
  expect_equal(episodic_colour_contrast("#777777", "#777777"), 1)
  expect_equal(episodic_ui_text_on("#222222"), "#FFFFFF")
  expect_equal(episodic_ui_text_on("#FFD43B"), pal$ink)
  # A mid-tone neither ink nor white reaches 4.5:1 on takes black.
  expect_equal(episodic_ui_text_on("#F04124"), "#000000")
  for (step in episodic_severity_steps) {
    fill <- episodic_ui_severity_colour(step)
    expect_gte(episodic_colour_contrast(fill, episodic_ui_text_on(fill)), 4.5)
  }
  # The alpha of an eight-digit colour is not part of its luminance.
  expect_equal(episodic_colour_luminance("#22222280"), episodic_colour_luminance("#222222"))
})

test_that("a filled chip and a selected picker button carry text that reads on their fill", {
  pal <- episodic_palette()
  chip <- as.character(episodic_ui_chip("x", pal$severity_baseline, filled = TRUE))
  expect_match(
    chip,
    sprintf("color:%s;background:%s;", pal$ink, pal$severity_baseline),
    fixed = TRUE
  )

  options <- list(
    list(value = "a", label = "A", colour = pal$severity_very_high),
    list(value = "b", label = "B", colour = pal$severity_baseline)
  )
  picker <- as.character(episodic_ui_picker("verdict", options, selected = "a"))
  expect_match(
    picker,
    sprintf("color:%s;", episodic_ui_text_on(pal$severity_very_high)),
    fixed = TRUE
  )
  expect_match(
    picker,
    sprintf("data-text-colour=\"%s\"", episodic_ui_text_on(pal$severity_baseline)),
    fixed = TRUE
  )
  expect_match(picker, "this.style.color=this.dataset.textColour", fixed = TRUE)
})

test_that("an unquoted hex colour, read by YAML as a comment, is reported rather than taken as empty", {
  path <- palette_file(c("brand:", "  primary: #123456"))
  expect_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(pal$primary, episodic_palette_shipped()$primary)
  problem <- episodic_palette_problems(path)[[1]]
  expect_equal(problem$reason, "invalid_colour")
  expect_equal(problem$value, "~")
})

test_that("a three-digit hex colour is accepted and written out in full", {
  path <- palette_file(list(brand = list(primary = "#1a2"), status = list(danger = "#11223380")))
  expect_no_warning(pal <- episodic_palette_config_resolve(path))
  expect_equal(pal$primary, "#11aa22")
  expect_equal(pal$danger, "#11223380")
  expect_length(episodic_palette_problems(path), 0)
})

test_that("a font stack that could end a CSS declaration is refused", {
  for (font in c("Arial; color: red", "Arial} body {", "<b>Arial</b>", "")) {
    path <- palette_file(list(typography = list(font = font)))
    expect_warning(pal <- episodic_palette_config_resolve(path))
    expect_equal(pal$font, episodic_palette_shipped()$font, info = font)
    expect_equal(problem_reasons(path), "invalid_font")
  }
})

test_that("a palette file that is missing, unreadable or not a mapping keeps the shipped palette without stopping", {
  shipped <- episodic_palette_shipped()

  missing <- tempfile(fileext = ".yaml")
  expect_warning(pal <- episodic_palette_config_resolve(missing), "does not exist")
  expect_equal(pal, shipped)
  expect_equal(problem_reasons(missing), "missing_file")

  unreadable <- palette_file("brand: [unclosed")
  expect_warning(pal <- episodic_palette_config_resolve(unreadable))
  expect_equal(pal, shipped)
  expect_equal(problem_reasons(unreadable), "unreadable")

  scalar <- palette_file("just a sentence")
  expect_warning(pal <- episodic_palette_config_resolve(scalar))
  expect_equal(pal, shipped)
  expect_equal(problem_reasons(scalar), "not_a_mapping")

  empty <- tempfile(fileext = ".yaml")
  file.create(empty)
  on.exit(unlink(empty))
  expect_no_warning(pal <- episodic_palette_config_resolve(empty))
  expect_equal(pal, shipped)
})

test_that("every palette problem is described in every shipped language", {
  problems <- list(
    list(reason = "missing_file", key = NA_character_, value = NA_character_),
    list(reason = "unreadable", key = NA_character_, value = "bad"),
    list(reason = "not_a_mapping", key = NA_character_, value = NA_character_),
    list(reason = "invalid_section", key = "brand", value = "#123456"),
    list(reason = "unknown_key", key = "brand.primry", value = "#123456"),
    list(reason = "moved_key", key = "primary", value = "#123456", target = "brand.primary"),
    list(reason = "retired_key", key = "danger_dark", value = "#123456"),
    list(reason = "invalid_swatch_name", key = "swatches.Primary", value = "#123456"),
    list(reason = "invalid_colour", key = "brand.primary", value = "blue"),
    list(reason = "invalid_font", key = "typography.font", value = ""),
    list(reason = "invalid_size", key = "typography.font_size_base", value = "13"),
    list(reason = "unresolved_reference", key = "brand.primary", value = "house"),
    list(reason = "circular_reference", key = "brand.primary", value = "secondary"),
    list(
      reason = "low_contrast", key = "neutral.ink", value = "#CCCCCC",
      against = "neutral.surface", ratio = 1.6
    )
  )
  expect_setequal(
    vapply(problems, function(p) p$reason, character(1)),
    episodic_palette_problem_reasons
  )
  for (lang in c("en", "en-US", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    texts <- episodic_palette_problem_texts(problems, lang = lang)
    expect_length(texts, length(problems))
    expect_false(any(grepl("[[", texts, fixed = TRUE)), info = lang)
    expect_false(any(grepl("{", gsub("\\{ \\}", "", texts), fixed = TRUE)), info = lang)
  }
  # the shipped value that stands in is named
  expect_match(
    episodic_palette_problem_texts(problems[9], lang = "en"),
    episodic_palette_shipped()$primary,
    fixed = TRUE
  )
})

test_that("a render process draws in the palette it is handed, not one it resolves itself", {
  path <- palette_file(list(brand = list(primary = "#123456")))
  withr::local_envvar(EPISODIC_STYLE = path)

  handed <- episodic_palette_shipped()
  handed$primary <- "#654321"
  episodic_palette_use(handed)
  expect_equal(episodic_palette()$primary, "#654321")

  # NULL, from a template written before `palette` was in its data,
  # leaves the process resolving its own
  expect_null(episodic_palette_use(NULL))
})

test_that("the cron run log names what is wrong with the palette, as a warning line", {
  path <- palette_file(list(brand = list(primary = "blue-ish")))
  withr::local_envvar(EPISODIC_STYLE = path)
  suppressWarnings(episodic_palette())

  expect_message(episodic_palette_trace(), "EPISODIC_STYLE: .*brand\\.primary")

  withr::local_envvar(EPISODIC_STYLE = NA)
  expect_silent(episodic_palette_trace())
})

test_that("episodic_ui_code_join() wraps each item in <code> and escapes, HTML-safe", {
  expect_equal(
    episodic_ui_code_join(c("same_place", "farrington"), sep = " en "),
    "<code>same_place</code> en <code>farrington</code>"
  )
  expect_equal(episodic_ui_code_join("a&b<c>"), "<code>a&amp;b&lt;c&gt;</code>")
})

test_that("episodic_ui_info_screen() renders in every shipped language and names every detector and state in code style", {
  for (lang in c("nl", "en", "es", "fr", "de", "zh", "hi", "ar")) {
    html <- as.character(episodic_ui_info_screen(lang = lang))
    expect_true(grepl("<code>farringtonFlexible</code>", html, fixed = TRUE))
    expect_true(grepl("<code>same_place</code>", html, fixed = TRUE))
    expect_true(grepl("<code>rare_trigger</code>", html, fixed = TRUE))
    # a translation miss falls through to episodic_tr()'s "[[key]]" placeholder
    expect_false(grepl("[[", html, fixed = TRUE))
  }
})

test_that("episodic_app_package_meta() reads only the first DESCRIPTION URL, not the whole comma-separated field", {
  meta <- episodic_app_package_meta()
  expect_false(is.null(meta))
  expect_equal(
    meta$url,
    "https://msberends.github.io/EpiSODIC"
  )
  expect_false(grepl(",", meta$url, fixed = TRUE))
  expect_equal(meta$license, "GPL-2")
})

test_that("episodic_ui_info_screen() shows the package's own version, description and license, beside the logo", {
  html <- as.character(episodic_ui_info_screen(lang = "en"))
  meta <- episodic_app_package_meta()
  expect_false(is.null(meta))
  expect_true(grepl(paste0("v", meta$version), html, fixed = TRUE))
  # the description contains "<doi:...>" markup, HTML-escaped on render -
  # a stable, escaping-safe substring instead of the raw field.
  expect_true(grepl("automated surveillance system", html, fixed = TRUE))
  # en is British English, so the label is spelled the British way; the
  # American spelling is what en-US carries the key for.
  expect_true(grepl(paste0("Licence: ", meta$license), html, fixed = TRUE))
  expect_true(grepl(meta$url, html, fixed = TRUE))
  expect_true(grepl("www/logo.svg", html, fixed = TRUE))

  american <- as.character(episodic_ui_info_screen(lang = "en-US"))
  expect_true(grepl(paste0("License: ", meta$license), american, fixed = TRUE))
})

test_that("episodic_ui_chip_link() opens a cluster through the shared attribute", {
  # The same attribute every other opener carries, so the rail's mark,
  # the dossier and the phone's pane all move together and no caller
  # writes a line of JavaScript of its own.
  chip <- as.character(
    episodic_ui_chip_link("Linked to #9", "#AA4A3F", cluster_id = 9L, lang = "en")
  )
  expect_true(grepl('data-episodic-outbreak="9"', chip, fixed = TRUE))
  expect_false(grepl("Shiny.setInputValue", chip, fixed = TRUE))
  expect_false(grepl("onclick", chip, fixed = TRUE))
})

test_that("app widgets render to shiny tags without error, including empty-data edge cases", {
  expect_s3_class(
    episodic_ui_chip("Norovirus", "#4A647D", filled = TRUE),
    "shiny.tag"
  )
  expect_s3_class(
    episodic_ui_panel("Title", shiny::tags$p("body")),
    "shiny.tag"
  )
  expect_s3_class(episodic_ui_panel_empty("Title", "Nothing here"), "shiny.tag")
  expect_s3_class(episodic_ui_stat("Label", 5, sub = "sub"), "shiny.tag")

  rows <- data.frame(label = c("A", "B"), n = c(3, 1))
  expect_s3_class(episodic_ui_bars(rows), "shiny.tag.list")
  expect_s3_class(
    episodic_ui_bars(data.frame(label = character(0), n = integer(0))),
    "shiny.tag"
  )

  demo <- data.frame(band = c("0-19", "20-39"), m = c(2, 1), v = c(1, 3))
  expect_s3_class(episodic_ui_pyramid(demo), "shiny.tag.list")
  expect_s3_class(
    episodic_ui_pyramid(data.frame(
      band = character(0),
      m = integer(0),
      v = integer(0)
    )),
    "shiny.tag"
  )

  expect_s3_class(episodic_ui_state_dot("new"), "shiny.tag")
  expect_type(episodic_ui_state_colour("new"), "character")
  expect_type(episodic_ui_state_colour("unknown_state"), "character")
})

test_that("episodic_ui_italicise_taxon() HTML-escapes unconditionally", {
  out <- episodic_ui_italicise_taxon(c("A&B <weird>", "Influenza A"))
  expect_equal(out[1], "A&amp;B &lt;weird&gt;")
})

test_that("episodic_ui_italicise_taxon() italicises AMR-recognised binomials, leaves other names alone", {
  out <- episodic_ui_italicise_taxon(c("Escherichia coli", "Influenza A"))
  expect_equal(out[1], "<i>Escherichia coli</i>")
  expect_equal(out[2], "Influenza A") # not a binomial AMR recognises -> unitalicised
})

test_that("episodic_app_ui() assembles a full page without error, in both languages", {
  expect_s3_class(episodic_app_ui("nl"), "shiny.tag.list")
  expect_s3_class(episodic_app_ui("en"), "shiny.tag.list")
})

test_that("chart builders produce ggplot objects for typical and edge-case inputs", {
  curve <- data.frame(
    sample_date = as.Date("2025-01-01") + 0:3,
    n_cases = c(1, 2, 0, 3),
    incomplete = c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_s3_class(episodic_ui_epi_curve_chart(curve, lang = "nl"), "ggplot")
  expect_s3_class(episodic_ui_epi_curve_chart(curve, lang = "en"), "ggplot")

  trend <- data.frame(
    week_start = as.Date("2025-01-06") + (0:5) * 7,
    n_cases = c(1, 2, 3, 4, 2, 1),
    expected = c(1, 1, 1, 1, 1, 1),
    upperbound = c(2, 2, 2, 2, 2, 2)
  )
  expect_s3_class(episodic_ui_trend_chart(trend), "ggplot")

  series <- data.frame(
    week_start = as.Date("2025-01-06") + (0:3) * 7,
    n_tests = c(10, 12, 8, 15),
    n_cases = c(1, 3, 2, 4),
    positivity = c(0.1, 0.25, 0.25, 0.27)
  )
  expect_s3_class(episodic_ui_denominator_chart(series), "ggplot")
})

test_that("episodic_ui_nav_links() renders four links and no highlight of its own", {
  html <- as.character(episodic_ui_nav_links(lang = "en"))
  for (view in c("outbreaks", "epidemics", "pathogens", "instance")) {
    expect_true(
      grepl(sprintf('data-view="%s"', view), html, fixed = TRUE),
      info = view
    )
  }
  expect_equal(
    lengths(regmatches(html, gregexpr("episodic-nav-link", html)))[[1]],
    4
  )
  # Archive is reached from an Instance card now, not from the bar - see
  # `episodic_ui_instance_screen()`.
  for (view in c("archive", "streams", "activity", "performance", "info", "settings")) {
    expect_false(
      grepl(sprintf('data-view="%s"', view), html, fixed = TRUE),
      info = view
    )
  }
  expect_false(grepl("active", html, fixed = TRUE))
})

test_that("every nav link carries the group its screen belongs to", {
  html <- as.character(episodic_ui_nav_links(lang = "en"))
  for (view in c("outbreaks", "epidemics", "pathogens", "instance")) {
    expect_true(
      grepl(
        sprintf('data-view="%s" data-episodic-nav="%s" data-nav="%s"', view, view, view),
        html,
        fixed = TRUE
      ),
      info = view
    )
  }
})

test_that("episodic_app_nav_group() sends every instance-level screen to one link", {
  expect_equal(episodic_app_nav_group("outbreaks"), "outbreaks")
  expect_equal(episodic_app_nav_group("epidemics"), "epidemics")
  expect_equal(episodic_app_nav_group("pathogens"), "pathogens")
  for (v in c("instance", "archive", "streams", "activity", "performance", "info", "settings")) {
    expect_equal(episodic_app_nav_group(v), "instance", info = v)
  }
  for (v in episodic_app_views()) {
    expect_true(
      episodic_app_nav_group(v) %in%
        c("outbreaks", "epidemics", "pathogens", "instance"),
      info = v
    )
  }
})

test_that("a nav link navigates through a data attribute, never an inline handler", {
  link <- as.character(episodic_ui_nav_link("archive", "Archive"))
  expect_true(grepl('data-episodic-nav="archive"', link, fixed = TRUE))
  # One delegated listener in a cached file, rather than a line of
  # JavaScript re-sent and re-parsed with every render of the bar.
  expect_false(grepl("onclick", link, fixed = TRUE))
  expect_false(grepl("Shiny.setInputValue", link, fixed = TRUE))
})

# The page's own scripts live in tags$head(), and htmltools lifts head
# content out into its own slot - so as.character() on the page returns
# the body alone and finds none of them. Rendered in full here.
episodic_test_ui_html <- function(lang = "en") {
  rendered <- htmltools::renderTags(episodic_app_ui(lang = lang))
  paste(
    paste(as.character(rendered$head), collapse = "\n"),
    rendered$html,
    sep = "\n"
  )
}

test_that("episodic_app_ui() loads its navigation from one cached script", {
  # One file, fetched once per browser, rather than a handful of
  # <script> blocks rebuilt into the page on every render. What it
  # contains is held in test-app_navigation.R.
  html <- episodic_test_ui_html()
  # `?v=<package version>` cache-busts across upgrades (see
  # episodic_app_ui()'s own asset_version); the fixed src is what this
  # test cares about, so it checks the prefix rather than an exact match.
  expect_true(grepl('src="www/episodic-nav.js?v=', html, fixed = TRUE))
  expect_false(grepl("function episodic", html, fixed = TRUE))
})

test_that("episodic_ui_pkg_versions_html() renders what a run recorded, and nothing when a run recorded nothing", {
  # No run has completed yet: the column is NA, not JSON. Parsing it is
  # what broke every dossier render on a fresh instance.
  expect_null(episodic_ui_pkg_versions_html(NA))
  expect_null(episodic_ui_pkg_versions_html(NA_character_))
  expect_null(episodic_ui_pkg_versions_html(NULL))
  expect_null(episodic_ui_pkg_versions_html(""))
  expect_null(episodic_ui_pkg_versions_html("   "))
  expect_null(episodic_ui_pkg_versions_html("{}"))

  html <- as.character(episodic_ui_pkg_versions_html(
    as.character(jsonlite::toJSON(
      list(EpiSODIC = "0.17.1", surveillance = "1.24.1"),
      auto_unbox = TRUE
    ))
  ))
  expect_true(grepl("<code>EpiSODIC</code> v0.17.1", html, fixed = TRUE))
  expect_true(grepl("<code>surveillance</code> v1.24.1", html, fixed = TRUE))
  expect_true(grepl("·", html, fixed = TRUE))
})

test_that("episodic_ui_pkg_versions_html() drops a package the run had no version for, rather than printing 'v NA'", {
  # episodic_pkg_versions() records NA for a package that was not
  # installed on the host that ran, which JSON-encodes to null. Not
  # installed is not a version, and an unlist() over the values silently
  # misaligns the ones that remain against the full name vector.
  json <- as.character(jsonlite::toJSON(
    list(EpiSODIC = "0.17.1", surveillance = NA, EpiEstim = "2.2-4"),
    auto_unbox = TRUE
  ))
  html <- as.character(episodic_ui_pkg_versions_html(json))
  expect_true(grepl("<code>EpiSODIC</code> v0.17.1", html, fixed = TRUE))
  expect_true(grepl("<code>EpiEstim</code> v2.2-4", html, fixed = TRUE))
  expect_false(grepl("surveillance", html, fixed = TRUE))
  expect_false(grepl("NA", html, fixed = TRUE))

  expect_null(episodic_ui_pkg_versions_html(as.character(jsonlite::toJSON(
    list(EpiSODIC = NA, surveillance = NA),
    auto_unbox = TRUE
  ))))
})

test_that("episodic_ui_format_stamp() spells the date in the session language and keeps the local clock", {
  # "15-01-2025" is one country's convention, and this stamp appears on
  # a dashboard shipped in eight languages.
  expect_equal(
    episodic_ui_format_stamp(
      "2025-01-15T10:00:00Z",
      lang = "en",
      tz = "Europe/Amsterdam"
    ),
    paste(episodic_format_date("2025-01-15", lang = "en"), "11:00")
  )
  expect_equal(
    episodic_ui_format_stamp(
      "2025-01-15T10:00:00Z",
      lang = "nl",
      tz = "Europe/Amsterdam"
    ),
    paste(episodic_format_date("2025-01-15", lang = "nl"), "11:00")
  )
  expect_false(identical(
    episodic_ui_format_stamp("2025-01-15T10:00:00Z", lang = "en", tz = "UTC"),
    episodic_ui_format_stamp("2025-01-15T10:00:00Z", lang = "zh", tz = "UTC")
  ))
})

test_that("episodic_ui_format_stamp() takes the date from local time, not from UTC", {
  # 23:30 UTC is the next day in Amsterdam: formatting the date half from
  # UTC and the time half from local time would print two different days'
  # worth of one instant.
  expect_equal(
    episodic_ui_format_stamp(
      "2025-01-15T23:30:00Z",
      lang = "en",
      tz = "Europe/Amsterdam"
    ),
    paste(episodic_format_date("2025-01-16", lang = "en"), "00:30")
  )
})

test_that("episodic_ui_format_stamp() can drop the time, and passes NA/unparseable input through", {
  expect_equal(
    episodic_ui_format_stamp(
      "2025-01-15T10:00:00Z",
      lang = "en",
      with_time = FALSE,
      tz = "UTC"
    ),
    episodic_format_date("2025-01-15", lang = "en")
  )
  expect_equal(episodic_ui_format_stamp(NA), episodic_tr("misc.unknown"))
  expect_equal(episodic_ui_format_stamp(NULL), episodic_tr("misc.unknown"))
  expect_equal(episodic_ui_format_stamp("not-a-timestamp"), "not-a-timestamp")
})

test_that("episodic_ui_epi_curve_chart() survives an incomplete flag that is NA", {
  # ifelse() on a bare NA gives an NA alpha, and a bar with an NA alpha
  # is simply not drawn - a case count silently missing from an epidemic
  # curve.
  curve <- data.frame(
    sample_date = seq(as.Date("2025-01-01"), by = "day", length.out = 3),
    n_cases = c(1, 2, 3),
    incomplete = c(FALSE, NA, TRUE)
  )
  p <- episodic_ui_epi_curve_chart(curve, lang = "en")
  expect_s3_class(p, "ggplot")
  expect_false(anyNA(p$data$alpha))
})
