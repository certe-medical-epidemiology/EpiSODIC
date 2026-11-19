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

#' The Dashboard's Colour Palette and Typography
#'
#' Returns the colours and typography used throughout the EpiSODIC dashboard
#' and charts, as a named list. Useful if you want to match your own plots or
#' reports to the house style, or check what colour a given status uses.
#'
#' The palette ships with an organisation-neutral default
#' (`r doc_system_file("inst/config/episodic_default_style.yaml")`). To use
#' your own institute's colours or fonts instead, point the `EPISODIC_STYLE`
#' environment variable at a YAML file that overrides only what you want to
#' change; anything you do not set keeps its shipped default.
#' `r doc_system_file("inst/config/episodic_style_template.yaml")` is a
#' complete file to start from.
#'
#' The file is layered, from what a colour is to what it is used for:
#'
#' * `typography`: `font`, a CSS font-family stack, and `font_size_base`,
#'   the size every other size in the app scales from (all of them are
#'   in `rem`), to compensate for a font that reads smaller or larger than
#'   the default at the same pixel size. Changing `font` only changes the
#'   CSS declaration; a webfont named there still needs delivering (a
#'   self-hosted `@font-face` or a link to its provider).
#' * `swatches`: your organisation's own colours, under names of your
#'   choosing.
#' * `neutral`: text, lines and backgrounds (`ink`, `muted`, `faint`,
#'   `border`, `bg_subtle`, `bg`, `surface`).
#' * `brand`: `primary`, `primary_strong` (the navigation bar, headings),
#'   `primary_soft`, `primary_tint`, `secondary` and `tertiary`.
#' * `status`: `success`, `warning` and `danger`.
#' * `severity`: one ordinal scale, `baseline` to `very_high`, shared by an
#'   epidemiologist's verdicts and the Moving Epidemic Method's intensity
#'   bands, so a step has one colour on every screen.
#' * `components`: every place where a colour carries a meaning (a
#'   cluster's state, a care line, the chips in a dossier header, a trend,
#'   each series of a chart, the header), each naming the colour it is
#'   drawn in.
#'
#' Any colour may be a quoted hex value or the name of a colour defined
#' above it: a swatch, a neutral, brand or status role (`"primary"`), or a
#' severity step (`"severity.high"`). A house palette is so entered once,
#' as swatches, and every role and component follows it.
#'
#' Text is always readable, whatever the colours: nothing in the file sets
#' a text colour. Where a colour is written as text, it is darkened just
#' enough to reach a contrast of 4.5:1 (WCAG AA) against its background,
#' keeping its hue; text on a filled colour is ink, white or black,
#' whichever reads; chart lines and marks are lifted to 3:1 against the
#' chart's background.
#'
#' The same palette governs everything EpiSODIC draws: the dashboard, the
#' outbreak reports rendered by [episodic_report_render()] (their charts,
#' text, headings, links and tables) and the emails it sends. The process
#' doing the render resolves it and hands it to the template, so a report
#' or an email always carries the colours and font of the instance that
#' produced it.
#'
#' Each value in the `EPISODIC_STYLE` file is checked. A value that cannot
#' be used (not a hex colour, a name that names nothing, a key that is not
#' part of the palette) keeps the shipped value for that key only, and a
#' file that does not exist or cannot be read keeps the whole shipped
#' palette. Neither stops the dashboard, a report or an email: the problem
#' is shown on the Info screen's reference-data panel, written to every
#' detection run's log, and raised once as a warning. So is an `ink` or
#' `muted` too light to read against `surface` or `bg`.
#'
#' This is independent of `episodic_config_resolve()` on purpose: colours
#' and typography never affect the `config_hash` recorded with a detection
#' run, since they have no bearing on reproducibility.
#'
#' @section Default font and colours:
#' The shipped file, in full:
#'
#' `r doc_palette()`
#'
#' @return A named list, one flat entry per key: `font` and
#'   `font_size_base`; the neutrals, brand and status roles by their own
#'   names (`ink`, `primary`, `danger`, ...); the severity steps as
#'   `severity_baseline` to `severity_very_high`; and each component as
#'   `<group>_<key>` (`state_new`, `care_line_first`, `chart_bars`,
#'   `header_brand_bar`, ...), all resolved to hex colours. Derived from
#'   them, and not settable: `<role>_text`, the role darkened to read as
#'   text on `surface` and `bg`, and `<role>_on`, the text colour to write
#'   on a fill of the role.
#' @examples
#' pal <- episodic_palette()
#' pal$primary
#' pal$danger_text
#' pal$state_monitoring
#' pal$font
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

#' Resolve the palette, validated key by key
#'
#' The shipped `episodic_default_style.yaml` with the instance's
#' `EPISODIC_STYLE` file merged over it, key by key, then every name
#' resolved to a hex colour and the readable variants derived. A value
#' that cannot be used keeps the shipped value for that one key; a file
#' that is missing or cannot be read keeps the whole shipped palette.
#'
#' The palette is display-only - it decides how the dashboard, the
#' reports and the emails look, never what a run computes - and it is
#' resolved by every process that draws anything, the cron included. So
#' a problem with it is announced (`episodic_palette_problems()`, the
#' Info screen's `EPISODIC_STYLE` row, one `warning()` per process and
#' one `warn` line per cron run) rather than refused: refusing would stop
#' the dashboard for every epidemiologist and every alert and scheduled
#' report with it, over a colour. The shipped value that stands in is the
#' defined default for that key, not a guess.
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
#'   `episodic_palette_problem_reasons`), `key` (the dotted path of the
#'   key, such as `"brand.primary"`), `value`, and for some reasons
#'   `target` or `ratio`/`against`. `key` and `value` are `NA` where the
#'   problem is with the file rather than one key. Empty when there is
#'   nothing wrong, including when no instance palette is configured.
#' @keywords internal
#' @noRd
episodic_palette_problems <- function(palette_config_path = Sys.getenv("EPISODIC_STYLE", unset = NA)) {
  episodic_palette_state(palette_config_path)$problems
}

#' Every reason a palette problem can carry
#' @keywords internal
#' @noRd
episodic_palette_problem_reasons <- c(
  "missing_file",
  "unreadable",
  "not_a_mapping",
  "invalid_section",
  "unknown_key",
  "moved_key",
  "retired_key",
  "invalid_swatch_name",
  "invalid_colour",
  "invalid_font",
  "invalid_size",
  "unresolved_reference",
  "circular_reference",
  "low_contrast"
)

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

  state <- if (configured) {
    merged <- episodic_palette_merge_validated(
      episodic_palette_shipped_tree(),
      palette_config_path
    )
    built <- episodic_palette_build(merged$tree)
    list(palette = built$palette, problems = c(merged$problems, built$problems))
  } else {
    list(palette = episodic_palette_shipped(), problems = list())
  }

  episodic_palette_cache[[cache_key]] <- state
  if (length(state$problems) > 0) {
    warning(
      "EPISODIC_STYLE ('", palette_config_path, "'): ",
      paste(episodic_palette_problem_texts(state$problems, lang = episodic_language_fallback), collapse = " "),
      call. = FALSE
    )
  }
  state
}

#' The shipped palette file, as written: nested, names unresolved
#' @keywords internal
#' @noRd
episodic_palette_shipped_tree <- function() {
  defaults_path <- system.file("config", "episodic_default_style.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "episodic_default_style.yaml")
  }
  yaml::read_yaml(defaults_path)
}

#' The shipped palette, resolved
#'
#' The shipped file must resolve without a single problem: it is what
#' every key falls back to, so a problem in it has nothing left to fall
#' back to and is a fault in the package, raised as an error.
#' @return The flat palette, as `episodic_palette()` returns it.
#' @keywords internal
#' @noRd
episodic_palette_shipped <- function() {
  cached <- episodic_palette_cache[["._shipped"]]
  if (!is.null(cached)) {
    return(cached)
  }
  built <- episodic_palette_build(episodic_palette_shipped_tree(), shipped = NULL)
  if (length(built$problems) > 0) {
    stop(
      "The shipped episodic_default_style.yaml does not resolve: ",
      paste(vapply(built$problems, function(p) {
        paste0(p$reason, " at ", p$key)
      }, character(1)), collapse = "; "),
      call. = FALSE
    )
  }
  episodic_palette_cache[["._shipped"]] <- built$palette
  built$palette
}

#' The sections whose keys are colour roles other colours can name
#'
#' A role in `neutral`, `brand` or `status` is named bare (`"primary"`)
#' or with its section (`"brand.primary"`); a severity step only with
#' its section (`"severity.high"`), since `"high"` alone reads as an
#' adjective, not a colour. Components are where colours are used, so
#' nothing names them.
#' @keywords internal
#' @noRd
episodic_palette_role_sections <- c("neutral", "brand", "status")

#' The keys that hold a sequence of colours rather than one
#' @keywords internal
#' @noRd
episodic_palette_sequence_keys <- "components.header.brand_bar"

#' Keys of the flat format the palette had before it was layered
#'
#' Reported, with where each now lives, rather than read: a file written
#' for the flat format would otherwise be ignored key by key as unknown,
#' with no word on what to write instead. `NA` marks a key that is now
#' derived, and so has no place in the file at all.
#' @keywords internal
#' @noRd
episodic_palette_flat_keys <- c(
  font = "typography.font",
  font_size_base = "typography.font_size_base",
  ink = "neutral.ink",
  muted = "neutral.muted",
  faint = "neutral.faint",
  border = "neutral.border",
  bg_subtle = "neutral.bg_subtle",
  bg = "neutral.bg",
  surface = "neutral.surface",
  primary = "brand.primary",
  primary_dark = "brand.primary_strong",
  primary_light = "brand.primary_soft",
  primary_tint = "brand.primary_tint",
  secondary = "brand.secondary",
  tertiary = "brand.tertiary",
  success = "status.success",
  warning = "status.warning",
  danger = "status.danger",
  severity_baseline = "severity.baseline",
  severity_low = "severity.low",
  severity_medium = "severity.medium",
  severity_high = "severity.high",
  severity_very_high = "severity.very_high",
  secondary_dark = NA,
  tertiary_dark = NA,
  success_dark = NA,
  warning_dark = NA,
  danger_dark = NA
)

#' One palette problem
#' @keywords internal
#' @noRd
episodic_palette_problem <- function(reason, key = NA_character_, value = NA_character_, ...) {
  c(list(reason = reason, key = key, value = value), list(...))
}

#' Whether a value is a single non-missing string
#' @keywords internal
#' @noRd
episodic_palette_is_string <- function(value) {
  is.character(value) && length(value) == 1 && !is.na(value)
}

#' Whether a string is a hex colour: `#RGB`, `#RRGGBB` or `#RRGGBBAA`
#' @keywords internal
#' @noRd
episodic_palette_is_hex <- function(value) {
  episodic_palette_is_string(value) &&
    grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", value)
}

#' Whether a string is shaped like the name of a colour
#' @keywords internal
#' @noRd
episodic_palette_is_name <- function(value) {
  episodic_palette_is_string(value) &&
    grepl("^[a-z][a-z0-9_]*(\\.[a-z][a-z0-9_]*)?$", value)
}

#' Merge an instance palette file over the shipped one, key by key
#'
#' A colour is a hex colour or the name of another colour (checked for
#' shape here, and for whether it names anything in
#' `episodic_palette_build()`): a hex colour is the one spelling every
#' consumer accepts alike - the dashboard's CSS, ggplot2, Bootstrap's Sass
#' in a report and an email client's inline style - where a CSS colour
#' name or `rgb()` is understood by some of them and not others. A font
#' stack may not carry `;`, `{`, `}`, `<`, `>`, a backslash or a control
#' character, since it is written into a stylesheet, a Sass file and an
#' inline `style` attribute, and any of those would end the declaration it
#' sits in.
#'
#' @param tree The shipped palette tree.
#' @param palette_config_path A configured, non-empty path.
#' @return A list with `tree` (the merged tree, names unresolved) and
#'   `problems`.
#' @keywords internal
#' @noRd
episodic_palette_merge_validated <- function(tree, palette_config_path) {
  file_problem <- function(reason, value = NA_character_) {
    list(tree = tree, problems = list(episodic_palette_problem(reason, value = value)))
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
    return(list(tree = tree, problems = list()))
  }
  if (!episodic_palette_is_mapping(instance)) {
    return(file_problem("not_a_mapping"))
  }

  problems <- list()
  add <- function(...) problems[[length(problems) + 1]] <<- episodic_palette_problem(...)

  for (section in names(instance)) {
    value <- instance[[section]]
    if (!section %in% names(tree)) {
      if (section %in% names(episodic_palette_flat_keys)) {
        target <- episodic_palette_flat_keys[[section]]
        if (is.na(target)) {
          add("retired_key", section, episodic_palette_value_label(value))
        } else {
          add("moved_key", section, episodic_palette_value_label(value), target = target)
        }
      } else {
        add("unknown_key", section, episodic_palette_value_label(value))
      }
      next
    }
    if (is.null(value) || (is.list(value) && length(value) == 0)) {
      next
    }
    if (!episodic_palette_is_mapping(value)) {
      add("invalid_section", section, episodic_palette_value_label(value))
      next
    }
    if (identical(section, "swatches")) {
      merged <- episodic_palette_merge_swatches(tree$swatches, value)
    } else if (identical(section, "components")) {
      merged <- episodic_palette_merge_components(tree$components, value)
    } else {
      merged <- episodic_palette_merge_section(section, tree[[section]], value)
    }
    tree[[section]] <- merged$values
    problems <- c(problems, merged$problems)
  }
  list(tree = tree, problems = problems)
}

#' Whether a value read from YAML is a mapping with usable names
#' @keywords internal
#' @noRd
episodic_palette_is_mapping <- function(value) {
  is.list(value) && !is.null(names(value)) && all(nzchar(names(value)))
}

#' Merge one section of `role: value` lines
#' @keywords internal
#' @noRd
episodic_palette_merge_section <- function(section, shipped, instance) {
  problems <- list()
  for (key in names(instance)) {
    path <- paste0(section, ".", key)
    value <- instance[[key]]
    if (!key %in% names(shipped)) {
      problems[[length(problems) + 1]] <- episodic_palette_problem(
        "unknown_key", path, episodic_palette_value_label(value)
      )
      next
    }
    reason <- episodic_palette_value_problem(path, value)
    if (is.na(reason)) {
      shipped[[key]] <- episodic_palette_normalise(value)
    } else {
      problems[[length(problems) + 1]] <- episodic_palette_problem(
        reason, path, episodic_palette_value_label(value)
      )
    }
  }
  list(values = shipped, problems = problems)
}

#' Merge the swatches: names of the operator's choosing, hex colours only
#'
#' A swatch may not take the name of a role: `primary: "#123456"` among
#' the swatches would make `"primary"` mean two colours, and which one a
#' component then followed would be an accident of lookup order.
#' @keywords internal
#' @noRd
episodic_palette_merge_swatches <- function(shipped, instance) {
  shipped <- if (is.list(shipped)) shipped else list()
  taken <- episodic_palette_role_names(episodic_palette_shipped_tree())
  problems <- list()
  for (name in names(instance)) {
    path <- paste0("swatches.", name)
    value <- instance[[name]]
    if (!grepl("^[a-z][a-z0-9_]*$", name) || name %in% taken) {
      problems[[length(problems) + 1]] <- episodic_palette_problem(
        "invalid_swatch_name", path, episodic_palette_value_label(value)
      )
      next
    }
    if (!episodic_palette_is_hex(value)) {
      problems[[length(problems) + 1]] <- episodic_palette_problem(
        "invalid_colour", path, episodic_palette_value_label(value)
      )
      next
    }
    shipped[[name]] <- episodic_palette_normalise(value)
  }
  list(values = shipped, problems = problems)
}

#' Merge the components, group by group
#' @keywords internal
#' @noRd
episodic_palette_merge_components <- function(shipped, instance) {
  problems <- list()
  for (group in names(instance)) {
    value <- instance[[group]]
    path <- paste0("components.", group)
    if (!group %in% names(shipped)) {
      problems[[length(problems) + 1]] <- episodic_palette_problem(
        "unknown_key", path, episodic_palette_value_label(value)
      )
      next
    }
    if (is.null(value) || (is.list(value) && length(value) == 0)) {
      next
    }
    if (!episodic_palette_is_mapping(value)) {
      problems[[length(problems) + 1]] <- episodic_palette_problem(
        "invalid_section", path, episodic_palette_value_label(value)
      )
      next
    }
    merged <- episodic_palette_merge_section(path, shipped[[group]], value)
    shipped[[group]] <- merged$values
    problems <- c(problems, merged$problems)
  }
  list(values = shipped, problems = problems)
}

#' Why one instance palette value cannot be used, or `NA` when it can
#'
#' @param path The key's dotted path, such as `"brand.primary"`.
#' @param value The instance's value for it.
#' @return A single reason string, or `NA_character_`.
#' @keywords internal
#' @noRd
episodic_palette_value_problem <- function(path, value) {
  if (identical(path, "typography.font")) {
    ok <- episodic_palette_is_string(value) &&
      nzchar(trimws(value)) &&
      !grepl("[;{}<>\\\\[:cntrl:]]", value, perl = TRUE)
    return(if (ok) NA_character_ else "invalid_font")
  }
  if (identical(path, "typography.font_size_base")) {
    ok <- episodic_palette_is_string(value) &&
      grepl("^[0-9]*\\.?[0-9]+(px|pt|rem|em)$", value)
    return(if (ok) NA_character_ else "invalid_size")
  }
  colour_ok <- function(v) episodic_palette_is_hex(v) || episodic_palette_is_name(v)
  if (path %in% episodic_palette_sequence_keys) {
    ok <- (is.character(value) || is.list(value)) &&
      length(value) > 0 &&
      all(vapply(value, colour_ok, logical(1)))
    return(if (ok) NA_character_ else "invalid_colour")
  }
  if (colour_ok(value)) NA_character_ else "invalid_colour"
}

#' Write a valid palette value the way every consumer reads it
#'
#' `#RGB` is expanded to `#RRGGBB`, so a colour holds one of the two
#' lengths R's own colour parser and Bootstrap's Sass both take without
#' question. A sequence (the brand bar) is made a character vector.
#' @keywords internal
#' @noRd
episodic_palette_normalise <- function(value) {
  if (is.list(value) || length(value) > 1) {
    return(vapply(value, episodic_palette_normalise, character(1), USE.NAMES = FALSE))
  }
  if (episodic_palette_is_hex(value) && nchar(value) == 4) {
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
  if (is.list(value) && !is.null(names(value))) {
    return("(a nested list)")
  }
  paste(as.character(unlist(value)), collapse = ", ")
}

#' Every name another colour can refer to a role by
#' @keywords internal
#' @noRd
episodic_palette_role_names <- function(tree) {
  bare <- unlist(lapply(episodic_palette_role_sections, function(s) names(tree[[s]])))
  dotted <- unlist(lapply(
    c(episodic_palette_role_sections, "severity"),
    function(s) paste0(s, ".", names(tree[[s]]))
  ))
  c(bare, dotted, "severity")
}

#' The colour a name refers to, as written in the tree
#'
#' @return The spec (a hex colour or another name) and the path it was
#'   found at, or `NULL` when the name names nothing.
#' @keywords internal
#' @noRd
episodic_palette_lookup <- function(tree, name) {
  if (name %in% names(tree$swatches)) {
    return(list(spec = tree$swatches[[name]], path = paste0("swatches.", name)))
  }
  parts <- strsplit(name, ".", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    section <- parts[1]
    key <- parts[2]
    if (section %in% c(episodic_palette_role_sections, "severity") &&
      key %in% names(tree[[section]])) {
      return(list(spec = tree[[section]][[key]], path = name))
    }
    return(NULL)
  }
  for (section in episodic_palette_role_sections) {
    if (name %in% names(tree[[section]])) {
      return(list(spec = tree[[section]][[name]], path = paste0(section, ".", name)))
    }
  }
  NULL
}

#' Resolve a merged palette tree into the flat palette
#'
#' Every name is followed to a hex colour. A name that names nothing, or
#' a chain of names that comes back round to itself, is a problem for
#' the key that holds it, which then takes its shipped value: first as
#' the shipped file writes it, followed through this palette (so a
#' shipped `state.new: primary_soft` still follows the instance's own
#' `primary_soft`), and failing that the shipped colour itself. Each
#' role is resolved once and remembered, so a role that falls back is
#' reported once rather than again by every key that names it.
#'
#' @param tree A palette tree: the shipped one, or one merged over it.
#' @param shipped The resolved shipped palette to fall back to, or `NULL`
#'   when `tree` is the shipped tree itself.
#' @return A list with `palette` and `problems`.
#' @keywords internal
#' @noRd
episodic_palette_build <- function(tree, shipped = episodic_palette_shipped()) {
  shipped_tree <- if (is.null(shipped)) tree else episodic_palette_shipped_tree()
  problems <- list()
  add <- function(...) problems[[length(problems) + 1]] <<- episodic_palette_problem(...)
  memo <- new.env(parent = emptyenv())

  # Follows `spec` to a hex colour. `trail` holds the paths already being
  # followed, so a cycle is caught where it closes. Returns a list with
  # `colour`, or with `reason` and `name` when it cannot be followed.
  follow <- function(spec, trail) {
    if (episodic_palette_is_hex(spec)) {
      return(list(colour = episodic_palette_normalise(spec)))
    }
    found <- episodic_palette_lookup(tree, spec)
    if (is.null(found)) {
      return(list(reason = "unresolved_reference", name = spec))
    }
    if (found$path %in% trail) {
      return(list(reason = "circular_reference", name = spec))
    }
    if (!startsWith(found$path, "swatches.")) {
      return(list(colour = role(found$path, trail)))
    }
    follow(found$spec, c(trail, found$path))
  }

  fallback <- function(path, trail) {
    shipped_spec <- episodic_palette_path_get(shipped_tree, path)
    if (!is.null(shipped) && !is.null(shipped_spec)) {
      again <- follow(shipped_spec, trail)
      if (!is.null(again$colour)) {
        return(again$colour)
      }
    }
    if (is.null(shipped)) {
      return("#000000")
    }
    shipped[[episodic_palette_flat_name(path)]]
  }

  # A role's colour, resolved once: a failure is reported for the role
  # itself and every later reference gets its fallback.
  role <- function(path, trail = character(0)) {
    if (!is.null(memo[[path]])) {
      return(memo[[path]])
    }
    spec <- episodic_palette_path_get(tree, path)
    out <- follow(spec, c(trail, path))
    colour <- if (!is.null(out$colour)) {
      out$colour
    } else {
      add(out$reason, path, spec)
      fallback(path, c(trail, path))
    }
    memo[[path]] <- colour
    colour
  }

  colour_of <- function(path) {
    spec <- episodic_palette_path_get(tree, path)
    resolve_one <- function(s) {
      out <- follow(s, path)
      if (!is.null(out$colour)) {
        return(out$colour)
      }
      add(out$reason, path, s)
      NULL
    }
    # The brand bar is the one sequence: resolved stripe by stripe, and
    # taken whole from the shipped palette when any stripe fails, since a
    # bar with a stripe missing is not the bar the operator wrote either.
    if (path %in% episodic_palette_sequence_keys) {
      colours <- lapply(spec, resolve_one)
      if (any(vapply(colours, is.null, logical(1)))) {
        return(if (is.null(shipped)) "#000000" else shipped[[episodic_palette_flat_name(path)]])
      }
      return(unlist(colours))
    }
    resolve_one(spec) %||% fallback(path, path)
  }

  palette <- list(
    font = tree$typography$font,
    font_size_base = tree$typography$font_size_base
  )
  for (section in c(episodic_palette_role_sections, "severity")) {
    for (key in names(tree[[section]])) {
      path <- paste0(section, ".", key)
      palette[[episodic_palette_flat_name(path)]] <- role(path)
    }
  }
  for (group in names(tree$components)) {
    for (key in names(tree$components[[group]])) {
      path <- paste0("components.", group, ".", key)
      palette[[episodic_palette_flat_name(path)]] <- colour_of(path)
    }
  }

  palette <- episodic_palette_derive(palette)
  problems <- c(problems, episodic_palette_contrast_problems(palette))
  list(palette = palette, problems = problems)
}

#' A key's value in the tree, by dotted path
#' @keywords internal
#' @noRd
episodic_palette_path_get <- function(tree, path) {
  value <- tree
  for (part in strsplit(path, ".", fixed = TRUE)[[1]]) {
    if (!is.list(value) || is.null(value[[part]])) {
      return(NULL)
    }
    value <- value[[part]]
  }
  value
}

#' A key's name in the flat palette, from its dotted path
#'
#' Roles keep their own names (`brand.primary` is `primary`), severity
#' steps and components are prefixed with what they belong to
#' (`severity.high` is `severity_high`, `components.state.new` is
#' `state_new`).
#' @keywords internal
#' @noRd
episodic_palette_flat_name <- function(path) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  if (parts[1] %in% episodic_palette_role_sections) {
    return(parts[2])
  }
  if (identical(parts[1], "components")) {
    return(paste(parts[-1], collapse = "_"))
  }
  paste(parts, collapse = "_")
}

#' The roles whose readable-text and text-on-fill variants are derived
#' @keywords internal
#' @noRd
episodic_palette_text_roles <- c(
  "primary", "primary_strong", "secondary", "tertiary",
  "success", "warning", "danger"
)

#' Add the variants every consumer needs to stay readable
#'
#' `<role>_text` is the role as text on `surface` and `bg`, darkened to
#' 4.5:1 where it falls short; `<role>_on` is the colour to write on a
#' fill of the role. The header's accent is lifted to 3:1 against the
#' bar it sits on (it is large, bold text), and every chart colour to
#' 3:1 against `surface`, WCAG's minimum for a line or mark that carries
#' information.
#' @keywords internal
#' @noRd
episodic_palette_derive <- function(palette) {
  backgrounds <- c(palette$surface, palette$bg)
  for (role in episodic_palette_text_roles) {
    palette[[paste0(role, "_text")]] <- episodic_colour_readable(palette[[role]], backgrounds)
    palette[[paste0(role, "_on")]] <- episodic_colour_on(palette[[role]], palette$ink)
  }
  palette$header_navbar_on <- episodic_colour_on(palette$header_navbar, palette$ink)
  palette$header_brand_accent <- episodic_colour_readable(
    palette$header_brand_accent,
    palette$header_navbar,
    min_ratio = 3
  )
  for (name in grep("^chart_", names(palette), value = TRUE)) {
    palette[[name]] <- episodic_colour_readable(palette[[name]], palette$surface, min_ratio = 3)
  }
  palette
}

#' Text neutrals too light to read, reported but used as set
#'
#' `ink` and `muted` are the operator's own text colours, written as they
#' are: darkening them would override a choice made on purpose, so a
#' shortfall is said rather than corrected.
#' @keywords internal
#' @noRd
episodic_palette_contrast_problems <- function(palette) {
  problems <- list()
  for (role in c("ink", "muted")) {
    for (against in c("surface", "bg")) {
      ratio <- episodic_colour_contrast(palette[[role]], palette[[against]])
      if (ratio < 4.5) {
        problems[[length(problems) + 1]] <- episodic_palette_problem(
          "low_contrast",
          paste0("neutral.", role),
          palette[[role]],
          against = paste0("neutral.", against),
          ratio = ratio
        )
      }
    }
  }
  problems
}

#' A hex colour's red, green and blue channels, 0 to 255
#'
#' An `#RRGGBBAA` colour's alpha is dropped: it is not part of the
#' colour's own contrast.
#' @keywords internal
#' @noRd
episodic_colour_rgb <- function(colour) {
  strtoi(substring(colour, c(2, 4, 6), c(3, 5, 7)), base = 16L)
}

#' Mix two hex colours: `t` of the way from `a` to `b`
#' @keywords internal
#' @noRd
episodic_colour_mix <- function(a, b, t) {
  rgb <- round((1 - t) * episodic_colour_rgb(a) + t * episodic_colour_rgb(b))
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

#' The text colour to write on a filled background
#'
#' `ink` or white, whichever contrasts more with `background`, as long as
#' that reaches WCAG's 4.5:1 for small text; where neither does - a
#' mid-tone such as Bootswatch Yeti's own red sits between them - black,
#' which reaches further than `ink` on any fill light enough to defeat
#' white. The same rule as Bootstrap's `color-contrast()`.
#'
#' @param background A hex colour.
#' @param ink The palette's `ink`.
#' @return A hex colour.
#' @keywords internal
#' @noRd
episodic_colour_on <- function(background, ink) {
  candidates <- c(ink, "#FFFFFF")
  contrast <- vapply(
    candidates,
    function(fg) episodic_colour_contrast(background, fg),
    numeric(1)
  )
  if (max(contrast) >= 4.5) {
    return(unname(candidates[which.max(contrast)]))
  }
  if (episodic_colour_contrast(background, "#000000") > max(contrast)) {
    return("#000000")
  }
  unname(candidates[which.max(contrast)])
}

#' A colour made readable against its backgrounds, keeping its hue
#'
#' The colour itself when it already reaches `min_ratio` against every
#' background; otherwise mixed towards black (on light backgrounds) or
#' white (on dark ones) by the least amount that reaches it, in steps of
#' 2%. A pale lilac on white becomes a deep plum, not a grey.
#'
#' @param colour A hex colour.
#' @param backgrounds One or more hex colours it is read against.
#' @param min_ratio The contrast to reach: 4.5 for text, 3 for large
#'   text and for lines and marks.
#' @return A hex colour.
#' @keywords internal
#' @noRd
episodic_colour_readable <- function(colour, backgrounds, min_ratio = 4.5) {
  worst <- function(c) {
    min(vapply(backgrounds, function(b) episodic_colour_contrast(c, b), numeric(1)))
  }
  if (worst(colour) >= min_ratio) {
    return(colour)
  }
  target <- if (worst("#000000") >= worst("#FFFFFF")) "#000000" else "#FFFFFF"
  for (t in seq(0.02, 1, by = 0.02)) {
    candidate <- episodic_colour_mix(colour, target, t)
    if (worst(candidate) >= min_ratio) {
      return(candidate)
    }
  }
  target
}

#' The palette's problems as sentences
#'
#' @param problems From `episodic_palette_problems()`.
#' @param lang Language to write them in.
#' @return A character vector, one sentence per problem.
#' @keywords internal
#' @noRd
episodic_palette_problem_texts <- function(problems, lang = episodic_lang()) {
  shipped_tree <- episodic_palette_shipped_tree()
  vapply(problems, function(p) {
    default <- if (!is.na(p$key)) episodic_palette_path_get(shipped_tree, p$key)
    # `role`, not `key`: `key` is episodic_tr()'s own first argument, the
    # translation key, and a placeholder of that name would replace it.
    episodic_tr(
      paste0("info.reference.style.", p$reason),
      role = p$key,
      value = p$value,
      default = if (is.null(default)) "" else paste(unlist(default), collapse = ", "),
      target = p$target %||% "",
      against = p$against %||% "",
      ratio = if (is.null(p$ratio)) "" else episodic_format_number(p$ratio, digits = 1, lang = lang),
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

#' The stripe of colours under the app header, left to right
#' @keywords internal
#' @noRd
episodic_brand_bar <- function() {
  episodic_palette()$header_brand_bar
}
