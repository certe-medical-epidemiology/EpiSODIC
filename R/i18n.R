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

#' Translation lookup
#'
#' The dashboard is available in English, (Modern Standard) Arabic,
#' Dutch, French, German, Hindi, Mandarin Chinese, and Spanish. All user-facing
#' text is stored as translation keys (e.g. `"nav.clusters"`) rather than
#' hardcoded in R code, and `episodic_tr()` looks a key up in the requested
#' language. A key that does not exist in any language is shown as
#' `[[key]]` rather than silently left blank, so a missing translation is
#' easy to spot.
#' @keywords internal
#' @noRd
episodic_i18n_cache <- new.env(parent = emptyenv())

#' Load one language's flat translation table
#'
#' @param lang A language code: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`.
#' @return A named character vector (dotted key -> template string).
#' @keywords internal
#' @noRd
episodic_i18n_load <- function(lang) {
  cached <- episodic_i18n_cache[[lang]]
  if (!is.null(cached)) {
    return(cached)
  }

  path <- system.file("i18n", paste0(lang, ".json"), package = "EpiSODIC")
  if (identical(path, "")) {
    path <- file.path("inst", "i18n", paste0(lang, ".json"))
  }
  if (!file.exists(path)) {
    stop("No i18n file for language '", lang, "' at ", path, call. = FALSE)
  }

  raw <- jsonlite::fromJSON(path)
  flat <- unlist(raw)
  episodic_i18n_cache[[lang]] <- flat
  flat
}

#' The language to render in, resolved
#'
#' Every function that renders text takes `lang` and defaults it to the
#' `EPISODIC_LANGUAGE` environment variable, which is how an instance
#' picks its language once rather than at every call site. An unset (or
#' empty) variable means English - the same fallback `episodic_tr()`
#' applies to a key it cannot find in the requested language.
#'
#' Anything that *branches* on the language rather than looking a key up -
#' the charts' thousands separator, for instance - has to resolve it
#' first, or an unset variable would read as "not English" and take the
#' wrong branch while every word around it came out in English.
#'
#' @param lang A language code, or `""`/`NA` for "not set".
#' @return A single language code.
#' @keywords internal
#' @noRd
episodic_lang <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (length(lang) != 1 || is.na(lang) || !nzchar(lang)) {
    return("en")
  }
  lang
}

#' Translate a dashboard text key
#'
#' Looks up a piece of dashboard text by its key and language, substituting
#' any `{placeholder}` tokens in the template. Mostly useful if you are
#' writing your own Quarto outbreak report template
#' (`EPISODIC_QUARTO_REPORT`) and want it to read the same wording, in the
#' same language, as the dashboard itself.
#'
#' @param key A dotted key identifying the piece of text, e.g.
#'   `"nav.clusters"`. The full set of available keys and their wording in
#'   every shipped language lives in `inst/i18n/*.json` (one file per
#'   language: `en`, `ar`, `nl`, `fr`, `de`, `hi`, `zh`, `es`).
#' @param ... Named values substituted into `{name}` placeholders in the
#'   template.
#' @param lang Language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`, `"hi"`,
#'   `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE` environment
#'   variable, falling back to `"en"` if that is unset.
#' @param instance_i18n An optional named character vector of your own
#'   wording overrides (key -> template), checked before the shipped
#'   translations. `NULL` (the default) uses only the shipped text.
#' @keywords internal
#' @noRd
episodic_tr <- function(
  key,
  ...,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  instance_i18n = NULL
) {
  lang <- episodic_lang(lang)
  template <- NULL

  if (!is.null(instance_i18n) && key %in% names(instance_i18n)) {
    template <- instance_i18n[[key]]
  }
  if (is.null(template)) {
    table <- episodic_i18n_load(lang)
    if (key %in% names(table)) template <- table[[key]]
  }
  if (is.null(template) && lang != "en") {
    table_en <- episodic_i18n_load("en")
    if (key %in% names(table_en)) template <- table_en[[key]]
  }
  if (is.null(template)) {
    return(paste0("[[", key, "]]"))
  }

  episodic_i18n_substitute(template, list(...))
}

#' @keywords internal
#' @noRd
episodic_i18n_substitute <- function(template, values) {
  if (length(values) == 0) {
    return(template)
  }
  for (name in names(values)) {
    # Both sides literal. A placeholder name is never a pattern, and a
    # substituted value is never one either: run detail can carry a
    # Windows account name or a recorded error message, and a stray
    # backslash in one of those must not rewrite the sentence around it.
    template <- gsub(
      paste0("{", name, "}"),
      as.character(values[[name]]),
      template,
      fixed = TRUE
    )
  }
  template
}

#' Dutch/English count phrase with correct number agreement
#'
#' `1 geval` against `2 gevallen`. Deliberately takes explicit singular/plural forms
#' rather than guessing a plural suffix, since Dutch (and English) plurals
#' are often irregular.
#'
#' @param n A count.
#' @param singular,plural The singular and plural noun forms.
#' @param with_number If `TRUE` (default), prefix with the number.
#' @return `"1 geval"`, `"2 gevallen"`, `"0 gevallen"`, etc. Dutch and
#'   English both pluralise away from exactly 1.
#' @keywords internal
#' @noRd
episodic_count_phrase <- function(n, singular, plural, with_number = TRUE) {
  word <- if (n == 1) singular else plural
  if (with_number) paste(n, word) else word
}

#' Format a date range compactly, collapsing shared month/year
#'
#' `"7-15 January 2025"` rather than `"2025-01-07 to 2025-01-15"`: shared
#' month and year are stated once, not repeated per endpoint. Falls back
#' one step at a time as the range widens (same month -> same year ->
#' different years), and collapses to a single date when `x` and `y` are
#' the same day (a one-day cluster should not read "7-7 January 2025").
#' Spells the month out whenever the result only ever needs to name one
#' (a single date, or a range within one month) and abbreviates only
#' once two different months have to appear side by side, where
#' spelling both out would double the string's length for no gain in
#' clarity: "12 January - 4 February 2025" is what abbreviating there
#' avoids. This is every caller's date display in the app - there is no
#' opt-out, so that a range never renders one way on one screen and
#' another way on the next.
#'
#' @param x,y Range endpoints - `Date`, or a string `as.Date()` accepts.
#'   Order does not matter; the earlier date is always shown first.
#' @param lang Session language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @return A character string, or `episodic_tr("misc.unknown", lang =
#'   lang)` if either endpoint fails to parse.
#' @keywords internal
#' @noRd
episodic_format_date_range <- function(
  x,
  y,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  x <- tryCatch(as.Date(x), error = function(e) NA)
  y <- tryCatch(as.Date(y), error = function(e) NA)
  if (length(x) != 1 || length(y) != 1 || is.na(x) || is.na(y)) {
    return(episodic_tr("misc.unknown", lang = lang))
  }
  if (y < x) {
    tmp <- x
    x <- y
    y <- tmp
  }

  months_abbr <- vapply(
    sprintf("%02d", 1:12),
    function(mm) episodic_tr(paste0("date.month.", mm), lang = lang),
    character(1)
  )
  months_full <- vapply(
    sprintf("%02d", 1:12),
    function(mm) episodic_tr(paste0("date.month_full.", mm), lang = lang),
    character(1)
  )
  mon <- function(d) months_full[as.integer(format(d, "%m"))]
  mon_abbr <- function(d) months_abbr[as.integer(format(d, "%m"))]
  day <- function(d) as.integer(format(d, "%d"))
  yr <- function(d) format(d, "%Y")

  if (identical(x, y)) {
    sprintf("%d %s %s", day(x), mon(x), yr(x))
  } else if (format(x, "%Y-%m") == format(y, "%Y-%m")) {
    sprintf("%d-%d %s %s", day(x), day(y), mon(x), yr(x))
  } else if (yr(x) == yr(y)) {
    sprintf("%d %s - %d %s %s", day(x), mon_abbr(x), day(y), mon_abbr(y), yr(x))
  } else {
    sprintf(
      "%d %s %s - %d %s %s",
      day(x),
      mon_abbr(x),
      yr(x),
      day(y),
      mon_abbr(y),
      yr(y)
    )
  }
}

#' A single date, formatted the same way every other date in the app is
#'
#' `episodic_format_date_range(d, d, lang = lang)`, named for the common
#' case: one calendar date, spelled out in full (there is only ever the
#' one month name to show).
#'
#' @param d A single date - `Date`, or a string `as.Date()` accepts.
#' @param lang Session language.
#' @return A character string, or `episodic_tr("misc.unknown", lang =
#'   lang)` if `d` fails to parse.
#' @keywords internal
#' @noRd
episodic_format_date <- function(d, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_format_date_range(d, d, lang = lang)
}
