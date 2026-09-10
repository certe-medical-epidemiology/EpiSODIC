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
#' A regional variant (`episodic_language_variants`) carries only the
#' keys that differ from the language it belongs to, so loading one is
#' its base's table with the variant's own keys written over the top.
#' That is the whole reason variants are cheap: `en-US` says "License"
#' and writes its dates month-first, and inherits everything else rather
#' than copying it, where a copy would have to be kept in step with
#' every key added to `en.json` for ever.
#'
#' A variant key that its base does not have would be inherited by
#' nothing and read by nobody. `test-i18n.R` refuses one; this does not
#' check again per load.
#'
#' @param lang A language code, already resolved by `episodic_lang()`.
#' @return A named character vector (dotted key -> template string).
#' @keywords internal
#' @noRd
episodic_i18n_load <- function(lang) {
  cached <- episodic_i18n_cache[[lang]]
  if (!is.null(cached)) {
    return(cached)
  }

  flat <- episodic_i18n_read(lang)
  base <- episodic_language_variant_base(lang)
  if (!is.null(base)) {
    inherited <- episodic_i18n_load(base)
    inherited[names(flat)] <- unname(flat)
    flat <- inherited
  }
  episodic_i18n_cache[[lang]] <- flat
  flat
}

#' Read one `inst/i18n/<lang>.json` file, without inheritance
#' @param lang A language code.
#' @return A named character vector.
#' @keywords internal
#' @noRd
episodic_i18n_read <- function(lang) {
  path <- system.file("i18n", paste0(lang, ".json"), package = "EpiSODIC")
  if (identical(path, "")) {
    path <- file.path("inst", "i18n", paste0(lang, ".json"))
  }
  if (!file.exists(path)) {
    stop("No i18n file for language '", lang, "' at ", path, call. = FALSE)
  }
  unlist(jsonlite::fromJSON(path))
}

#' The languages EpiSODIC ships translations for
#'
#' One complete file each, in `inst/i18n/`. `en` is British English and
#' `es` is Spain's Spanish; their other regions are variants of these,
#' not files of their own - see `episodic_language_variants`.
#' @keywords internal
#' @noRd
episodic_languages <- c("en", "ar", "nl", "fr", "de", "hi", "zh", "es")

#' The language everything falls back to
#'
#' What an unset `EPISODIC_LANGUAGE` resolves to, what `episodic_tr()`
#' reads a key from when the requested language has not translated it,
#' and the language `episodic_lang()`'s own warnings are written in: a
#' warning about a language that could not be resolved cannot be
#' written in that language.
#' @keywords internal
#' @noRd
episodic_language_fallback <- "en"

#' Regional variants, and the language each one is a variant of
#'
#' A variant is a file holding *only* what differs from its base:
#' `en-US.json` is a spelling, a date order and a name, and `es-419.json`
#' is the number marks Latin America writes and a name. Everything else
#' is inherited (`episodic_i18n_load()`).
#'
#' Deliberately not a copy of the base file: `en-US.json` is seven keys
#' where `en.json` is six hundred and eighty-five. Two copies would have
#' to be kept in step with every key ever added to either, and an
#' omission there is silent - the variant serves the base's wording
#' rather than a missing key's own loud `[[key]]`.
#' @keywords internal
#' @noRd
episodic_language_variants <- c("en-US" = "en", "es-419" = "es")

#' Locale codes that name a shipped language exactly
#'
#' `en` *is* British English and `es` *is* Spain's Spanish, so `en-GB`
#' and `es-ES` are those files rather than variants of them. Accepted
#' rather than refused: an operator who writes the region out has said
#' precisely what they meant, and being told their language is not
#' shipped would be both wrong and unhelpful.
#' @keywords internal
#' @noRd
episodic_language_aliases <- c("en-GB" = "en", "es-ES" = "es")

#' The language a regional variant belongs to
#'
#' `episodic_language_variants` is an atomic vector, so `[[code]]` on a
#' code that is not a variant is an error rather than a `NULL` - and
#' every base language is such a code. Both callers ask the question of
#' any code at all, so they ask it here.
#'
#' @param code A language code.
#' @return The base language's code, or `NULL` when `code` names no
#'   variant.
#' @keywords internal
#' @noRd
episodic_language_variant_base <- function(code) {
  at <- match(code, names(episodic_language_variants))
  if (is.na(at)) NULL else unname(episodic_language_variants[[at]])
}

#' Every code that has a translation table behind it
#' @return A character vector of language and variant codes.
#' @keywords internal
#' @noRd
episodic_languages_all <- function() {
  c(episodic_languages, names(episodic_language_variants))
}

#' Languages written right to left
#'
#' Bases, not variants: a variant of a right-to-left language is
#' right-to-left too, which `episodic_lang_is_rtl()` gets by resolving to
#' the base first.
#' @keywords internal
#' @noRd
episodic_languages_rtl <- c("ar")

#' Warn about a given unsupported language code only once per session
#' @keywords internal
#' @noRd
episodic_lang_warned <- new.env(parent = emptyenv())

#' The language to render in, resolved
#'
#' Every function that renders text takes `lang` and defaults it to the
#' `EPISODIC_LANGUAGE` environment variable, which is how an instance
#' picks its language once rather than at every call site. An unset (or
#' empty) variable means English - the same fallback `episodic_tr()`
#' applies to a key it cannot find in the requested language.
#'
#' What a value can be, in the order it is tried:
#'
#' \describe{
#'   \item{a shipped language or variant}{`nl`, `en-US`, `es-419`.}
#'   \item{a locale naming one exactly}{`en-GB` and `es-ES` are `en`
#'     and `es` - see `episodic_language_aliases`.}
#'   \item{a region of a shipped language that is not itself shipped}{
#'     `nl-BE`, `es-MX`: the language, with a warning saying so once. A
#'     Belgian instance asking for `nl-BE` wants Dutch, and English is
#'     not a closer answer than Dutch is.}
#'   \item{anything else}{English, with a warning, once per value.}
#' }
#'
#' Case and separator do not matter (`en_us`, `EN-US`). Nothing here
#' raises: an unshipped value is a typo in an environment variable, and
#' a typo may not take a surveillance dashboard down.
#'
#' Anything that *branches* on the language rather than looking a key up
#' has to resolve it first, or an unset variable reads as "not English"
#' and takes the wrong branch while every word around it comes out in
#' English. Little does branch - conventions that vary by language are
#' keys of their own, see `episodic_format_number()`.
#'
#' @param lang A language code, or `""`/`NA` for "not set".
#' @return A single code, always one of `episodic_languages_all()`.
#' @keywords internal
#' @noRd
episodic_lang <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (length(lang) != 1 || is.na(lang) || !nzchar(lang)) {
    return(episodic_language_fallback)
  }
  code <- episodic_lang_normalise(lang)
  if (code %in% episodic_languages_all()) {
    return(code)
  }
  if (code %in% names(episodic_language_aliases)) {
    return(unname(episodic_language_aliases[[code]]))
  }

  base <- strsplit(code, "-", fixed = TRUE)[[1]][1]
  if (base %in% episodic_languages) {
    if (is.null(episodic_lang_warned[[code]])) {
      episodic_lang_warned[[code]] <- TRUE
      warning(
        "EpiSODIC ships no '",
        code,
        "' translations, so '",
        base,
        "' (",
        episodic_language_label(base, lang = episodic_language_fallback),
        ") is used instead - which is that language as it is written ",
        "where the base file was written, and may not be the regional ",
        "convention you meant. The variants that do exist: ",
        paste(
          episodic_language_choices(
            lang = episodic_language_fallback,
            variants_only = TRUE
          ),
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
    return(base)
  }

  # Keyed on the normalised code where there is one: `EPISODIC_LANGUAGE=-`
  # normalises to nothing at all, and an environment has no such name to
  # remember it by.
  warned_as <- if (nzchar(code)) code else lang
  if (is.null(episodic_lang_warned[[warned_as]])) {
    episodic_lang_warned[[warned_as]] <- TRUE
    warning(
      "EpiSODIC has no translations for language '",
      lang,
      "', so English is used instead. Set EPISODIC_LANGUAGE to one of: ",
      # The code and the name, not one or the other: the code is what
      # goes in the environment variable, and the name is what tells an
      # operator which code they want.
      paste(
        episodic_language_choices(lang = episodic_language_fallback),
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }
  episodic_language_fallback
}

#' A language code in the one shape everything else here compares against
#'
#' `en_us`, `EN-US` and `en-US` are the same request. The language
#' subtag is lowercased and an alphabetic region uppercased, which is
#' the BCP 47 convention; a numeric region (`419`, Latin America) is
#' left as it is, having no case to speak of.
#'
#' @param lang A language code as an operator wrote it.
#' @return A single normalised code.
#' @keywords internal
#' @noRd
episodic_lang_normalise <- function(lang) {
  parts <- strsplit(gsub("_", "-", lang, fixed = TRUE), "-", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return("")
  }
  code <- tolower(parts[1])
  if (length(parts) >= 2) {
    region <- parts[2]
    code <- paste0(
      code,
      "-",
      if (grepl("^[0-9]+$", region)) region else toupper(region)
    )
  }
  code
}

#' The shipped language a code renders in, variants resolved to their base
#'
#' For everything that is a property of the language rather than of the
#' region: which way it reads, which month names it has. `en-US` is
#' `en` here; `en-US` is still `en-US` to `episodic_lang()`, which is
#' what the HTML `lang` attribute and the translation table want.
#'
#' @inheritParams episodic_lang
#' @return A single code, always one of `episodic_languages`.
#' @keywords internal
#' @noRd
episodic_lang_base <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  resolved <- episodic_lang(lang)
  base <- episodic_language_variant_base(resolved)
  if (is.null(base)) resolved else base
}

#' What a language is called, rather than what its code is
#'
#' `"nl"` is what an operator sets `EPISODIC_LANGUAGE` to; it is not
#' what anyone calls the language. Every shipped file names all eight in
#' its own language, so an English message says "Dutch" and a Dutch
#' screen says "Nederlands", from the same key.
#'
#' @param code A language code. One that is not shipped comes back as
#'   itself - it is still the truest thing that can be said about it.
#' @param lang The language to name it *in*.
#' @return A single string.
#' @keywords internal
#' @noRd
episodic_language_label <- function(code,
                                    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (length(code) != 1 || is.na(code) || !code %in% episodic_languages_all()) {
    return(as.character(code))
  }
  episodic_tr(paste0("misc.language.", code), lang = lang)
}

#' Every shipped language, as "code (Name)"
#'
#' For the messages that have to name them - the ones telling an
#' operator which values `EPISODIC_LANGUAGE` takes. Aliases
#' (`episodic_language_aliases`) are accepted but not listed: `en-GB` is
#' `en` spelled out, and offering both as choices would suggest a
#' difference that is not there.
#'
#' @param lang The language to name them in.
#' @param variants_only List only the regional variants, for the message
#'   that has just told an operator their own region is not one of them.
#' @return A character vector, one entry per code.
#' @keywords internal
#' @noRd
episodic_language_choices <- function(lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                      variants_only = FALSE) {
  codes <- if (isTRUE(variants_only)) {
    names(episodic_language_variants)
  } else {
    episodic_languages_all()
  }
  vapply(
    codes,
    function(code) {
      sprintf("%s (%s)", code, episodic_language_label(code, lang = lang))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Whether a language is written right to left
#'
#' Arabic is one of the eight shipped languages, so the page needs a
#' `dir` attribute: without one the dashboard renders left to right and
#' every navigation bar, table and chart axis mirrors the wrong way
#' round. A predicate over `episodic_languages_rtl` rather than an
#' `identical(lang, "ar")` at each call site, so adding Hebrew, Persian
#' or Urdu is one entry rather than a search.
#'
#' @param lang A language code (resolved or not).
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_lang_is_rtl <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_lang_base(lang) %in% episodic_languages_rtl
}

#' `"rtl"` or `"ltr"`, for an HTML `dir` attribute
#' @inheritParams episodic_lang_is_rtl
#' @return A single string.
#' @keywords internal
#' @noRd
episodic_lang_dir <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (episodic_lang_is_rtl(lang)) "rtl" else "ltr"
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
#'   `"zh"`, or `"es"`, or a regional variant of one (`"en-US"`,
#'   `"es-419"`). Defaults to the `EPISODIC_LANGUAGE` environment
#'   variable, falling back to `"en"` if that is unset.
#' @param instance_i18n An optional named character vector of your own
#'   wording overrides (key -> template), checked before the shipped
#'   translations. `NULL` (the default) uses only the shipped text.
#' @keywords internal
#' @noRd
episodic_tr <- function(key,
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
  if (is.null(template) && lang != episodic_language_fallback) {
    table_en <- episodic_i18n_load(episodic_language_fallback)
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

#' Count phrase with correct number agreement
#'
#' `1 geval` against `2 gevallen`. Deliberately takes explicit singular/plural forms
#' rather than guessing a plural suffix, since Dutch (and English) plurals
#' are often irregular.
#'
#' The number itself goes through `episodic_format_number()`, so a count
#' of four figures reads as the language writes one - this is where most
#' of the app's counts reach a sentence.
#'
#' @param n A count.
#' @param singular,plural The singular and plural noun forms.
#' @param with_number If `TRUE` (default), prefix with the number.
#' @param lang Session language, for the number's own marks.
#' @return `"1 geval"`, `"2 gevallen"`, `"0 gevallen"`, etc. Dutch and
#'   English both pluralise away from exactly 1.
#' @keywords internal
#' @noRd
episodic_count_phrase <- function(n,
                                  singular,
                                  plural,
                                  with_number = TRUE,
                                  lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  word <- if (n == 1) singular else plural
  if (with_number) {
    paste(episodic_format_number(n, lang = lang), word)
  } else {
    word
  }
}

#' Format a date range compactly, collapsing shared month/year
#'
#' The four shapes - one date, a range inside one month, a range inside
#' one year, and a range across two - are `date.format.*` templates in
#' the language files, so each language writes a date the way it writes
#' one. `{month}` is the full month name in the first two (only one
#' month name appears, so there is room to spell it out) and the
#' abbreviation in the other two.
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
#'   `"hi"`, `"zh"`, or `"es"`, or a regional variant of
#'   one (`"en-US"`, `"es-419"`). Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @return A character string, or `episodic_tr("misc.unknown", lang =
#'   lang)` if either endpoint fails to parse.
#' @keywords internal
#' @noRd
episodic_format_date_range <- function(x,
                                       y,
                                       lang = Sys.getenv("EPISODIC_LANGUAGE")) {
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

  # Templates rather than sprintf() formats, because the order of a date
  # is a property of the language and not of this function: British
  # English writes 7 January 2025 and American English January 7, 2025,
  # German puts a point after the day, Spanish two "de"s in, and Chinese
  # writes the year first with 年月日 around the parts.
  #
  # Neither the day nor the year goes through `episodic_format_number()`:
  # a year is a name for a year, and "2.025" is not one.
  if (identical(x, y)) {
    episodic_tr(
      "date.format.single",
      day = day(x),
      month = mon(x),
      year = yr(x),
      lang = lang
    )
  } else if (format(x, "%Y-%m") == format(y, "%Y-%m")) {
    episodic_tr(
      "date.format.range_month",
      day_from = day(x),
      day_to = day(y),
      month = mon(x),
      year = yr(x),
      lang = lang
    )
  } else if (yr(x) == yr(y)) {
    episodic_tr(
      "date.format.range_year",
      day_from = day(x),
      month_from = mon_abbr(x),
      day_to = day(y),
      month_to = mon_abbr(y),
      year = yr(x),
      lang = lang
    )
  } else {
    episodic_tr(
      "date.format.range_full",
      day_from = day(x),
      month_from = mon_abbr(x),
      year_from = yr(x),
      day_to = day(y),
      month_to = mon_abbr(y),
      year_to = yr(y),
      lang = lang
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

#' Format a Number the Way the Session's Language Writes One
#'
#' English writes 1,234.5 and Dutch, German, French and Spanish write it
#' with the two marks the other way round. A surveillance dashboard that
#' ignores that is not merely untidy: a Dutch reader seeing an \eqn{R_t}
#' of "1.4" reads fourteen hundred before reading 1.4, on the one chart
#' where the difference between just above and just below 1 is the whole
#' point.
#'
#' The marks come from the language files, not from `options(OutDec)`
#' and not from the system locale: an EpiSODIC instance renders in the
#' language `EPISODIC_LANGUAGE` names, on whatever machine and under
#' whatever locale the operator happens to run R with, and two people
#' reading the same dashboard must not see two different numbers. Four
#' keys carry it, per language:
#'
#' \describe{
#'   \item{`misc.decimal.mark`}{What separates the whole part from the
#'     fraction.}
#'   \item{`misc.thousands.mark`}{What separates the groups of the whole
#'     part - a no-break space in French, empty for a language that does
#'     not group at all.}
#'   \item{`misc.thousands.grouping`}{The group sizes, right to left,
#'     comma-separated. `"3"` everywhere except Hindi, where `"3,2"`
#'     gives the Indian lakh/crore grouping (12,34,567, not 1,234,567).}
#'   \item{`misc.thousands.minimum`}{How many digits a number needs on
#'     top of one whole group before it is grouped at all (CLDR's
#'     `minimumGroupingDigits`). `"1"` everywhere except Spanish, where
#'     `"2"` means grouping starts at five digits: 2000, but 12.345.}
#' }
#'
#' Arabic gets the *Latin* marks (1,234.5), not `U+066B`/`U+066C`: those
#' belong with Arabic-Indic digits (١٢٣), and EpiSODIC renders Western
#' digits throughout - which is also what CLDR's `ar`-with-`latn`
#' numbering does.
#'
#' @param x A numeric vector.
#' @param digits Round to this many decimal places first. `NULL` (the
#'   default) leaves the value as it is. Trailing zeros are dropped
#'   either way, so `digits = 1` renders 2 as "2" and 2.35 as "2.4".
#' @param lang Session language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`, or a regional variant of
#'   one (`"en-US"`, `"es-419"`). Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @return A character vector the same length as `x`. `NA` in is
#'   `NA_character_` out - a number that does not exist is not a number
#'   to format, and every caller already decides for itself what to show
#'   in its place. Anything else that does not parse as a plain number
#'   (`Inf`, `NaN`) comes back as R renders it, rather than being
#'   silently rewritten.
#' @keywords internal
#' @noRd
episodic_format_number <- function(x,
                                   digits = NULL,
                                   lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (length(x) == 0) {
    return(character(0))
  }
  marks <- episodic_number_marks(lang)
  vapply(
    x,
    function(one) episodic_format_number_one(one, digits, marks),
    character(1),
    USE.NAMES = FALSE
  )
}

#' The four number-formatting values for one language
#'
#' Read through `episodic_tr()`, so an instance's own i18n override
#' reaches them like any other key. A grouping or minimum that is not a
#' positive whole number is a translation-file mistake rather than
#' something to render around: it warns once and falls back to grouping
#' by threes, which is what seven of the eight shipped languages use.
#'
#' @param lang Session language.
#' @return A list with `decimal`, `thousands`, `sizes` and `minimum`.
#' @keywords internal
#' @noRd
episodic_number_marks <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  lang <- episodic_lang(lang)
  whole <- function(key, fallback) {
    raw <- strsplit(episodic_tr(key, lang = lang), ",", fixed = TRUE)[[1]]
    parsed <- suppressWarnings(as.integer(trimws(raw)))
    if (length(parsed) == 0 || anyNA(parsed) || any(parsed < 1)) {
      if (is.null(episodic_number_warned[[paste0(lang, key)]])) {
        episodic_number_warned[[paste0(lang, key)]] <- TRUE
        warning(
          "The '",
          lang,
          "' translation file gives '",
          key,
          "' a value that is not a whole number, so numbers are grouped ",
          "the default way instead. Fix the value, or override the key ",
          "for this instance.",
          call. = FALSE
        )
      }
      return(fallback)
    }
    parsed
  }
  list(
    decimal = episodic_tr("misc.decimal.mark", lang = lang),
    thousands = episodic_tr("misc.thousands.mark", lang = lang),
    sizes = whole("misc.thousands.grouping", 3L),
    minimum = whole("misc.thousands.minimum", 1L)[1]
  )
}

#' Warn about one broken number key only once per session
#' @keywords internal
#' @noRd
episodic_number_warned <- new.env(parent = emptyenv())

#' One number, rendered with one language's marks
#'
#' The plain form is built first and then taken apart on whatever
#' character R used as its decimal point, rather than on `"."`: that is
#' what makes this immune to `options(OutDec = ",")` without having to
#' read, set or restore it. A string that does not match a plain signed
#' number at all is handed back untouched.
#'
#' @param x A single numeric.
#' @param digits Passed from `episodic_format_number()`.
#' @param marks From `episodic_number_marks()`.
#' @return A single string, or `NA_character_`.
#' @keywords internal
#' @noRd
episodic_format_number_one <- function(x, digits, marks) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (!is.null(digits)) {
    x <- round(x, digits)
  }
  plain <- format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  parts <- regmatches(
    plain,
    regexec("^(-?)([0-9]+)(?:[^0-9]([0-9]+))?$", plain)
  )[[1]]
  if (length(parts) == 0) {
    return(plain)
  }
  whole <- episodic_number_group(parts[3], marks)
  # A number with no fraction leaves that group unmatched, which
  # `regmatches()` reports as an empty string - never as a missing one,
  # but `nzchar(NA)` is TRUE and would append the word "NA" to a whole
  # number if it ever did.
  fraction <- if (length(parts) >= 4 && !is.na(parts[4])) parts[4] else ""
  if (nzchar(fraction)) {
    whole <- paste0(whole, marks$decimal, fraction)
  }
  paste0(parts[2], whole)
}

#' Insert one language's thousands mark into a run of digits
#'
#' Right to left, taking `sizes` in turn and repeating the last one -
#' `3` gives 1,234,567 and `3,2` gives 12,34,567, the Indian grouping.
#' Nothing is inserted below `sizes[1] + minimum` digits, which is what
#' keeps Spanish's four-digit numbers unseparated.
#'
#' @param digits A string of digits, no sign and no decimal part.
#' @param marks From `episodic_number_marks()`.
#' @return The same digits, grouped.
#' @keywords internal
#' @noRd
episodic_number_group <- function(digits, marks) {
  if (!nzchar(marks$thousands)) {
    return(digits)
  }
  # CLDR's minimumGroupingDigits: a number is grouped at all only once
  # its whole part is at least one group plus that many digits long.
  # With the 1 seven of the eight languages use, that is "from four
  # digits up", the ordinary rule; Spanish's 2 makes it "from five", so
  # 2000 stands unseparated while 12.345 and 1.234.567 do not. Read as a
  # length for the leading group instead, it leaves 1234567 unseparated
  # too, which no Spanish writes.
  if (nchar(digits) < marks$sizes[1] + marks$minimum) {
    return(digits)
  }
  chars <- strsplit(digits, "", fixed = TRUE)[[1]]
  groups <- character(0)
  last <- length(chars)
  step <- 1L
  while (last > 0) {
    size <- marks$sizes[min(step, length(marks$sizes))]
    first <- max(last - size + 1L, 1L)
    groups <- c(paste(chars[first:last], collapse = ""), groups)
    last <- first - 1L
    step <- step + 1L
  }
  paste(groups, collapse = marks$thousands)
}
