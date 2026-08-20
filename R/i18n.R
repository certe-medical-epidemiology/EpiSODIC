#' i18n scaffolding
#'
#' Flat JSON, dotted keys, Dutch default, English fallback, missing keys
#' rendered visibly (`[[key]]`) rather than blank, so a gap is loud, not
#' silent. User-facing
#' strings are never hardcoded in R code; every string used by the app
#' lives in `inst/i18n/nl.json`/`en.json` and is looked up through
#' [episode_tr()].
#'
#' Fallback chain: an instance override (an operator-supplied overlay,
#' e.g. for house-style wording changes, located beside `EPISODE_CONFIG`
#' rather than in the repository - configuration is never committed to
#' the repository, the same rule detection settings follow), then the
#' requested session language, then `"en"`, then the key itself, wrapped
#' so a missing
#' translation is obvious in testing rather than silently blank.
#'
#' @name i18n
NULL

#' @keywords internal
#' @noRd
episode_i18n_cache <- new.env(parent = emptyenv())

#' Load one language's flat translation table
#'
#' @param lang A language code, `"nl"` or `"en"`.
#' @return A named character vector (dotted key -> template string).
#' @export
episode_i18n_load <- function(lang) {
  cached <- episode_i18n_cache[[lang]]
  if (!is.null(cached)) return(cached)

  path <- system.file("i18n", paste0(lang, ".json"), package = "EpiSODE")
  if (identical(path, "")) {
    path <- file.path("inst", "i18n", paste0(lang, ".json"))
  }
  if (!file.exists(path)) {
    stop("No i18n file for language '", lang, "' at ", path, call. = FALSE)
  }

  raw <- jsonlite::fromJSON(path)
  flat <- unlist(raw)
  episode_i18n_cache[[lang]] <- flat
  flat
}

#' Translate a key, with placeholder substitution
#'
#' @param key A dotted i18n key, e.g. `"rail.title"`.
#' @param ... Named placeholder values substituted into `{name}` tokens in
#'   the template.
#' @param lang Session language, `"nl"` (default) or `"en"`.
#' @param instance_i18n An optional named character vector of operator
#'   overrides (dotted key -> template), checked before the shipped
#'   translation files. `NULL` (the default) means no override.
#' @return A single character string, with placeholders substituted.
#' @export
episode_tr <- function(key, ..., lang = "nl", instance_i18n = NULL) {
  template <- NULL

  if (!is.null(instance_i18n) && key %in% names(instance_i18n)) {
    template <- instance_i18n[[key]]
  }
  if (is.null(template)) {
    table <- episode_i18n_load(lang)
    if (key %in% names(table)) template <- table[[key]]
  }
  if (is.null(template) && lang != "en") {
    table_en <- episode_i18n_load("en")
    if (key %in% names(table_en)) template <- table_en[[key]]
  }
  if (is.null(template)) {
    return(paste0("[[", key, "]]"))
  }

  episode_i18n_substitute(template, list(...))
}

#' @keywords internal
#' @noRd
episode_i18n_substitute <- function(template, values) {
  if (length(values) == 0) return(template)
  for (name in names(values)) {
    template <- gsub(paste0("\\{", name, "\\}"), as.character(values[[name]]), template, fixed = FALSE)
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
#' @export
episode_count_phrase <- function(n, singular, plural, with_number = TRUE) {
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
#' @param lang Session language, `"nl"` (default) or `"en"`.
#' @return A character string, or `episode_tr("misc.unknown", lang =
#'   lang)` if either endpoint fails to parse.
#' @export
episode_format_date_range <- function(x, y, lang = "nl") {
  x <- tryCatch(as.Date(x), error = function(e) NA)
  y <- tryCatch(as.Date(y), error = function(e) NA)
  if (length(x) != 1 || length(y) != 1 || is.na(x) || is.na(y)) {
    return(episode_tr("misc.unknown", lang = lang))
  }
  if (y < x) {
    tmp <- x; x <- y; y <- tmp
  }

  months <- if (lang == "nl") {
    c("jan.", "feb.", "mrt.", "apr.", "mei", "jun.", "jul.", "aug.", "sep.", "okt.", "nov.", "dec.")
  } else {
    month.abb
  }
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
