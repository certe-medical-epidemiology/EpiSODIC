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

#' The languages EpiSODIC ships translations for
#' @keywords internal
#' @noRd
episodic_languages <- c("en", "ar", "nl", "fr", "de", "hi", "zh", "es")

#' Languages written right to left
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
#' A value that is set but is not one EpiSODIC ships also means English,
#' with a warning, once per session per value. It used to mean a hard
#' error from `episodic_i18n_load()` on *every* render, which took the
#' whole dashboard down: `EPISODIC_LANGUAGE=pt` is a reasonable thing for
#' an operator to try, and so is the locale-shaped `en_GB` or `nl_NL`,
#' and none of them is a reason to serve a stack trace instead of a
#' surveillance dashboard.
#'
#' Anything that *branches* on the language rather than looking a key up
#' has to resolve it first, or an unset variable would read as "not
#' English" and take the wrong branch while every word around it came
#' out in English. (The charts' thousands separator used to be exactly
#' that; it is a key lookup now - see `episodic_format_number()`.)
#'
#' @param lang A language code, or `""`/`NA` for "not set".
#' @return A single language code, always one of `episodic_languages`.
#' @keywords internal
#' @noRd
episodic_lang <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (length(lang) != 1 || is.na(lang) || !nzchar(lang)) {
    return("en")
  }
  if (lang %in% episodic_languages) {
    return(lang)
  }
  if (is.null(episodic_lang_warned[[lang]])) {
    episodic_lang_warned[[lang]] <- TRUE
    warning(
      "EpiSODIC has no translations for language '",
      lang,
      "', so English is used instead. Set EPISODIC_LANGUAGE to one of: ",
      # The code and the name, not one or the other: the code is what
      # goes in the environment variable, and the name is what tells an
      # operator which code they want.
      paste(episodic_language_choices(lang = "en"), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  "en"
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
  if (length(code) != 1 || is.na(code) || !code %in% episodic_languages) {
    return(as.character(code))
  }
  episodic_tr(paste0("misc.language.", code), lang = lang)
}

#' Every shipped language, as "code (Name)"
#'
#' For the one message that has to name them all - the one telling an
#' operator which values `EPISODIC_LANGUAGE` takes.
#'
#' @param lang The language to name them in.
#' @return A character vector, one entry per shipped language.
#' @keywords internal
#' @noRd
episodic_language_choices <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  vapply(
    episodic_languages,
    function(code) {
      sprintf("%s (%s)", code, episodic_language_label(code, lang = lang))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Whether a language is written right to left
#'
#' Arabic is one of the eight shipped languages, and until this existed
#' the dashboard rendered it left to right with no `dir` attribute
#' anywhere on the page - every navigation bar, table and chart axis
#' mirrored the wrong way round. Kept as a predicate over
#' `episodic_languages_rtl` rather than an `identical(lang, "ar")` at
#' each of the call sites, so adding Hebrew, Persian or Urdu later is
#' one entry rather than a search.
#'
#' @param lang A language code (resolved or not).
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_lang_is_rtl <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_lang(lang) %in% episodic_languages_rtl
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
#'   `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE` environment
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
#'   \item{`misc.thousands.minimum`}{How many digits must stand before
#'     the first separator for a number to be grouped at all (CLDR's
#'     `minimumGroupingDigits`). `"1"` everywhere except Spanish, where
#'     four-digit numbers are written unseparated (2000, but 12.345).}
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
#'   `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
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
#' Nothing is inserted while fewer than `minimum` digits would stand
#' before the first separator, which is what keeps Spanish's four-digit
#' numbers unseparated.
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
  # CLDR's minimumGroupingDigits, which is a count of the digits standing
  # *before* the first separator, not a count of groups: Spanish writes
  # 2000 unseparated and 12.345 separated, and both have two groups.
  if (length(groups) == 1 || nchar(groups[1]) < marks$minimum) {
    return(digits)
  }
  paste(groups, collapse = marks$thousands)
}
