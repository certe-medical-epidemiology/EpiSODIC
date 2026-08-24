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
#' The dashboard is available in Dutch, English, Spanish, French, German,
#' Mandarin Chinese, Hindi, and (Modern Standard) Arabic. All user-facing
#' text is stored as translation keys (e.g. `"nav.clusters"`) rather than
#' hardcoded in R code, and [episodic_tr()] looks a key up in the requested
#' language. A key that does not exist in any language is shown as
#' `[[key]]` rather than silently left blank, so a missing translation is
#' easy to spot.
#'
#' @name i18n
NULL

#' @keywords internal
#' @noRd
episodic_i18n_cache <- new.env(parent = emptyenv())

#' Load one language's flat translation table
#'
#' @param lang A language code: `"nl"`, `"en"`, `"es"`, `"fr"`, `"de"`,
#'   `"zh"`, `"hi"`, or `"ar"`.
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
#' empty) variable means English - the same fallback [episodic_tr()]
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
#'   language: `nl`, `en`, `es`, `fr`, `de`, `zh`, `hi`, `ar`).
#' @param ... Named values substituted into `{name}` placeholders in the
#'   template.
#' @param lang Language: `"nl"`, `"en"`, `"es"`, `"fr"`, `"de"`, `"zh"`,
#'   `"hi"`, or `"ar"`. Defaults to the `EPISODIC_LANGUAGE` environment
#'   variable, falling back to `"en"` if that is unset.
#' @param instance_i18n An optional named character vector of your own
#'   wording overrides (key -> template), checked before the shipped
#'   translations. `NULL` (the default) uses only the shipped text.
#' @return A single character string.
#' @examples
#' episodic_tr("nav.clusters", lang = "nl")
#' episodic_tr("nav.clusters", lang = "en")
#' @export
episodic_tr <- function(
    key,
    ...,
    lang = Sys.getenv("EPISODIC_LANGUAGE"),
    instance_i18n = NULL) {
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
#' `"7-15 jan. 2025"` rather than `"2025-01-07 to 2025-01-15"`: shared
#' month and year are stated once, not repeated per endpoint. Falls back
#' one step at a time as the range widens (same month -> same year ->
#' different years), and collapses to a single date when `x` and `y` are
#' the same day (a one-day cluster should not read "7-7 jan. 2025").
#'
#' @param x,y Range endpoints - `Date`, or a string `as.Date()` accepts.
#'   Order does not matter; the earlier date is always shown first.
#' @param lang Session language: `"nl"`, `"en"`, `"es"`, `"fr"`, `"de"`,
#'   `"zh"`, `"hi"`, or `"ar"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @param full_month If `TRUE`, spell the month out (`date.month_full.NN`,
#'   Excel's `"mmmm"`) instead of the abbreviated form
#'   (`date.month.NN`, `"mmm"`) every other caller uses.
#' @return A character string, or `episodic_tr("misc.unknown", lang =
#'   lang)` if either endpoint fails to parse.
#' @keywords internal
#' @noRd
episodic_format_date_range <- function(
    x,
    y,
    lang = Sys.getenv("EPISODIC_LANGUAGE"),
    full_month = FALSE) {
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

  month_key <- if (full_month) "date.month_full." else "date.month."
  months <- vapply(
    sprintf("%02d", 1:12),
    function(mm) episodic_tr(paste0(month_key, mm), lang = lang),
    character(1)
  )
  mon <- function(d) months[as.integer(format(d, "%m"))]
  day <- function(d) as.integer(format(d, "%d"))
  yr <- function(d) format(d, "%Y")

  if (identical(x, y)) {
    sprintf("%d %s %s", day(x), mon(x), yr(x))
  } else if (format(x, "%Y-%m") == format(y, "%Y-%m")) {
    sprintf("%d-%d %s %s", day(x), day(y), mon(x), yr(x))
  } else if (yr(x) == yr(y)) {
    sprintf("%d %s - %d %s %s", day(x), mon(x), day(y), mon(y), yr(x))
  } else {
    sprintf("%d %s %s - %d %s %s", day(x), mon(x), yr(x), day(y), mon(y), yr(y))
  }
}
