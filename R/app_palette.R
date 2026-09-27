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

#' The Dashboard's Colour Palette and Typography
#'
#' Returns the colours and typography used throughout the EpiSODIC dashboard
#' and charts, as a named list. Useful if you want to match your own plots or
#' reports to the house style, or check what colour a given status uses.
#'
#' The palette ships with an organisation-neutral default
#' (`r doc_system_file("inst/config/episodic_default_style.yaml")`). To use your own institute's colours or fonts instead,
#' point the `EPISODIC_STYLE` environment variable at a YAML file
#' that overrides only the roles you want to change - anything you do not set
#' keeps its shipped default.
#'
#' The same palette governs everything EpiSODIC draws: the dashboard, the
#' outbreak reports rendered by [episodic_report_render()] (their charts,
#' text, headings, links and tables) and the emails it sends. The process
#' doing the render resolves it and hands it to the template, so a report
#' or an email always carries the colours and font of the instance that
#' produced it.
#'
#' Each value in the `EPISODIC_STYLE` file is checked: a colour must be a
#' quoted hex colour (`"#RGB"`, `"#RRGGBB"` or `"#RRGGBBAA"`), `font` a CSS
#' font-family stack and `font_size_base` a CSS length (`px`, `pt`, `rem`
#' or `em`). A value that fails keeps the shipped value for that role only,
#' and a file that does not exist or cannot be read keeps the whole shipped
#' palette. Neither stops the dashboard, a report or an email: the problem
#' is shown on the Info screen's reference-data panel, written to every
#' detection run's log, and raised once as a warning.
#'
#' This is independent of `episodic_config_resolve()`
#' on purpose: colours and typography never affect the `config_hash` recorded
#' with a detection run, since they have no bearing on reproducibility.
#'
#' @section Default font and colours:
#' These are all the default values, and all can be changed using a custom YAML file.
#'
#' `r doc_palette()`
#'
#' Of note:
#'
#' * `primary_dark` is the background colour of the navigation bar.
#' * `font` is a CSS font-family stack, and `font_size_base` is the app's base font size.
#'   * Every other font size in the dashboard is set in `rem` relative to it, so changing `font_size_base` scales the whole app's type proportionally (useful when swapping in a font that reads naturally smaller or larger than the default at the same pixel size).
#'   * Changing `font` only changes the CSS declaration; if it names a webfont rather than a system font, delivering that font (a self-hosted `@font-face` or a link to its provider) is the operator's own concern.
#'
#' @return A named list. The greyscale neutrals are
#'   `ink` (default text), `muted` (secondary text), `faint` (tertiary text),
#'   `border`, `bg_subtle`, `bg`, and `surface`. The semantic roles are
#'   `primary`, `secondary`, `tertiary`, `success`, `warning`, and `danger`,
#'   each with `_dark`/`_light`/`_tint` variants where used. `font` and
#'   `font_size_base` hold the app's typography, not a colour.
#' @examples
#' pal <- episodic_palette()
#' pal$primary
#' pal$danger
#' pal$font
#' pal$font_size_base
#' @export
episodic_palette <- function() {
  episodic_palette_config_resolve()
}

# A dossier render alone calls episodic_palette() seven times, charts nine -
# every one of them re-reading and re-parsing the same YAML file(s) from
# disk, on every render, for a value that cannot change within a running
# session (it is a function of the env var and shipped/instance files, none
# of which are touched while the app is up). Cached per resolved path, not
# globally, so a caller that does pass a different `palette_config_path`
# still gets the right file, just not re-read every time.
episodic_palette_cache <- new.env(parent = emptyenv())

#' Resolve the palette, validated role by role
#'
#' The shipped `episodic_default_style.yaml` with the instance's
#' `EPISODIC_STYLE` file merged over it, key by key. Every value the
#' instance supplies is checked before it is merged: a colour role must
#' be a hex colour, `font` a CSS font-family stack, `font_size_base` a
#' CSS length. A value that fails keeps the shipped value for that one
#' role; a file that is missing or cannot be read keeps the whole shipped
#' palette.
#'
#' The palette is display-only - it decides how the dashboard, the
#' reports and the emails look, never what a run computes - and it is
#' resolved by every process that draws anything, the cron included. So
#' a problem with it is announced (`episodic_palette_problems()`, the
#' Info screen's `EPISODIC_STYLE` row, one `warning()` per process and
#' one `warn` line per cron run) rather than refused: refusing would stop
#' the dashboard for every epidemiologist and every alert and scheduled
#' report with it, over a colour. The shipped value that stands in is the
#' defined default for that role, not a guess.
#'
#' @param palette_config_path Path to an instance palette YAML, or `NA`.
#' @return The resolved palette, a named list.
#' @keywords internal
#' @noRd
episodic_palette_config_resolve <- function(palette_config_path = Sys.getenv("EPISODIC_STYLE", unset = NA)) {
  episodic_palette_state(palette_config_path)$palette
}

#' What was wrong with the instance palette, if anything
#'
#' @param palette_config_path As for `episodic_palette_config_resolve()`.
#' @return A list of problems, each a list with `reason` (one of
#'   `"missing_file"`, `"unreadable"`, `"not_a_mapping"`,
#'   `"unknown_key"`, `"invalid_colour"`, `"invalid_font"`,
#'   `"invalid_size"`), `key` and `value` (both `NA` where the problem is
#'   with the file rather than one role). Empty when there is nothing
#'   wrong, including when no instance palette is configured.
#' @keywords internal
#' @noRd
episodic_palette_problems <- function(palette_config_path = Sys.getenv("EPISODIC_STYLE", unset = NA)) {
  episodic_palette_state(palette_config_path)$problems
}

#' The cached palette and its problems for one configured path
#' @keywords internal
#' @noRd
episodic_palette_state <- function(palette_config_path) {
  # `[[` on an environment requires a non-empty name - "" (from an unset
  # env var) errors with "zero-length variable name" - hence the sentinel
  # rather than caching under palette_config_path itself.
  configured <- length(palette_config_path) == 1 &&
    !is.na(palette_config_path) &&
    nzchar(palette_config_path)
  cache_key <- if (configured) palette_config_path else "._default"
  cached <- episodic_palette_cache[[cache_key]]
  if (!is.null(cached)) {
    return(cached)
  }

  base <- episodic_palette_shipped()
  problems <- list()
  if (configured) {
    resolved <- episodic_palette_merge_validated(base, palette_config_path)
    base <- resolved$palette
    problems <- resolved$problems
  }

  state <- list(palette = base, problems = problems)
  episodic_palette_cache[[cache_key]] <- state
  if (length(problems) > 0) {
    warning(
      "EPISODIC_STYLE ('", palette_config_path, "'): ",
      paste(episodic_palette_problem_texts(problems, lang = episodic_language_fallback), collapse = " "),
      call. = FALSE
    )
  }
  state
}

#' The shipped palette, as read from `episodic_default_style.yaml`
#' @keywords internal
#' @noRd
episodic_palette_shipped <- function() {
  defaults_path <- system.file("config", "episodic_default_style.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "episodic_default_style.yaml")
  }
  yaml::read_yaml(defaults_path)
}

#' Merge an instance palette file over the shipped one, role by role
#'
#' @param base The shipped palette.
#' @param palette_config_path A configured, non-empty path.
#' @return A list with `palette` and `problems`.
#' @keywords internal
#' @noRd
episodic_palette_merge_validated <- function(base, palette_config_path) {
  file_problem <- function(reason, value = NA_character_) {
    list(
      palette = base,
      problems = list(list(reason = reason, key = NA_character_, value = value))
    )
  }
  if (!file.exists(palette_config_path)) {
    return(file_problem("missing_file"))
  }
  instance <- tryCatch(
    yaml::read_yaml(palette_config_path),
    error = function(e) e
  )
  if (inherits(instance, "condition")) {
    return(file_problem("unreadable", conditionMessage(instance)))
  }
  if (is.null(instance)) {
    # An empty file overrides nothing, which is a valid thing to ask for.
    return(list(palette = base, problems = list()))
  }
  if (!is.list(instance) || is.null(names(instance)) || any(!nzchar(names(instance)))) {
    return(file_problem("not_a_mapping"))
  }

  problems <- list()
  for (key in names(instance)) {
    value <- instance[[key]]
    reason <- episodic_palette_value_problem(key, value, names(base))
    if (is.na(reason)) {
      base[[key]] <- episodic_palette_normalise(key, value)
    } else {
      problems <- c(problems, list(list(
        reason = reason,
        key = key,
        value = episodic_palette_value_label(value)
      )))
    }
  }
  list(palette = base, problems = problems)
}

#' The typography roles of the palette; every other role is a colour
#' @keywords internal
#' @noRd
episodic_palette_typography_roles <- c("font", "font_size_base")

#' Why one instance palette value cannot be used, or `NA` when it can
#'
#' A colour is `#RGB`, `#RRGGBB` or `#RRGGBBAA`: a hex colour is the one
#' spelling every consumer accepts alike - the dashboard's CSS, ggplot2,
#' Bootstrap's Sass in a report and an email client's inline style - where
#' a CSS colour name or `rgb()` is understood by some of them and not
#' others. A font stack may not carry `;`, `{`, `}`, `<`, `>`, a backslash
#' or a control character, since it is written into a stylesheet, a Sass
#' file and an inline `style` attribute, and any of those would end the
#' declaration it sits in.
#'
#' @param key The palette role.
#' @param value The instance's value for it.
#' @param known The shipped roles.
#' @return A single reason string, or `NA_character_`.
#' @keywords internal
#' @noRd
episodic_palette_value_problem <- function(key, value, known) {
  if (!key %in% known) {
    return("unknown_key")
  }
  single_string <- is.character(value) && length(value) == 1 && !is.na(value)
  if (identical(key, "font")) {
    ok <- single_string &&
      nzchar(trimws(value)) &&
      !grepl("[;{}<>\\\\[:cntrl:]]", value, perl = TRUE)
    return(if (ok) NA_character_ else "invalid_font")
  }
  if (identical(key, "font_size_base")) {
    ok <- single_string && grepl("^[0-9]*\\.?[0-9]+(px|pt|rem|em)$", value)
    return(if (ok) NA_character_ else "invalid_size")
  }
  ok <- single_string &&
    grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", value)
  if (ok) NA_character_ else "invalid_colour"
}

#' Write a valid palette value the way every consumer reads it
#'
#' `#RGB` is expanded to `#RRGGBB`, so a colour role holds one of the two
#' lengths R's own colour parser and Bootstrap's Sass both take without
#' question.
#' @keywords internal
#' @noRd
episodic_palette_normalise <- function(key, value) {
  if (key %in% episodic_palette_typography_roles) {
    return(value)
  }
  if (nchar(value) == 4) {
    chars <- strsplit(substring(value, 2), "")[[1]]
    value <- paste0("#", paste(rep(chars, each = 2), collapse = ""))
  }
  value
}

#' A rejected value as text a reader can recognise from their own file
#' @keywords internal
#' @noRd
episodic_palette_value_label <- function(value) {
  if (is.null(value)) {
    return("~")
  }
  if (is.list(value)) {
    return("(a nested list)")
  }
  paste(as.character(value), collapse = ", ")
}

#' The palette's problems as sentences
#'
#' @param problems From `episodic_palette_problems()`.
#' @param lang Language to write them in.
#' @return A character vector, one sentence per problem.
#' @keywords internal
#' @noRd
episodic_palette_problem_texts <- function(problems, lang = episodic_lang()) {
  shipped <- episodic_palette_shipped()
  vapply(problems, function(p) {
    # `role`, not `key`: `key` is episodic_tr()'s own first argument, the
    # translation key, and a placeholder of that name would replace it.
    episodic_tr(
      paste0("info.reference.style.", p$reason),
      role = p$key,
      value = p$value,
      default = if (!is.na(p$key) && p$key %in% names(shipped)) shipped[[p$key]] else "",
      lang = lang
    )
  }, character(1))
}

#' Say, once per cron run, what is wrong with the instance palette
#'
#' The cron resolves the palette for every email and report it sends, and
#' a problem with it never stops the run (see
#' `episodic_palette_config_resolve()`), so this is where it reaches the
#' run log - and through it the Activity screen - rather than only the
#' process's own warnings.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_palette_trace <- function() {
  problems <- episodic_palette_problems()
  if (length(problems) == 0) {
    return(invisible(NULL))
  }
  episodic_trace(
    "EPISODIC_STYLE: ",
    paste(episodic_palette_problem_texts(problems, lang = episodic_language_fallback), collapse = " "),
    severity = "warn"
  )
  invisible(NULL)
}

#' Hand a render process the palette the renderer resolved
#'
#' A report or an email is rendered by Quarto in an R process of its own,
#' which would otherwise resolve `episodic_palette()` afresh from whatever
#' environment it inherited. The renderer puts the palette it resolved into
#' the template's data, and the template's setup chunk passes it here, so
#' every chart in the document is drawn in exactly the colours the process
#' that asked for the render was using. Every later `episodic_palette()`
#' call in that process returns it.
#'
#' @param palette A palette, as returned by `episodic_palette()`. `NULL`
#'   leaves the process resolving its own, which is what a template
#'   written before `palette` was part of its data does.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_palette_use <- function(palette) {
  if (is.null(palette)) {
    return(invisible(NULL))
  }
  key <- Sys.getenv("EPISODIC_STYLE", unset = NA)
  key <- if (is.na(key) || !nzchar(key)) "._default" else key
  episodic_palette_cache[[key]] <- list(palette = palette, problems = list())
  invisible(NULL)
}

#' The five-colour brand bar used under the app header
#' @keywords internal
#' @noRd
episodic_brand_bar <- function() {
  p <- episodic_palette()
  c(p$warning, p$success, p$danger, p$primary, p$secondary)
}
