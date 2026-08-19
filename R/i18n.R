#' i18n scaffolding
#'
#' Flat JSON, dotted keys, Dutch default, English fallback, missing keys
#' rendered visibly rather than blank (MILESTONES.md M2). User-facing
#' strings are never hardcoded in R code; every string used by the app
#' lives in `inst/i18n/nl.json`/`en.json` and is looked up through
#' [episode_tr()].
#'
#' Fallback chain: an instance override (an operator-supplied overlay,
#' e.g. for house-style wording changes, located beside `EPISODE_CONFIG`
#' rather than in the repository, consistent with the standing brief's
#' "no configuration in the repository" rule), then the requested session
#' language, then `"en"`, then the key itself, wrapped so a missing
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
#' `1 geval` against `2 gevallen`: the watch-item MILESTONES.md calls out
#' explicitly for M2. Deliberately takes explicit singular/plural forms
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
