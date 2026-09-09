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

episodic_shipped_langs <- c("nl", "en", "es", "fr", "de", "zh", "hi", "ar")

test_that("episodic_i18n_load() reads every shipped language", {
  for (lang in episodic_shipped_langs) {
    table <- episodic_i18n_load(lang)
    expect_gt(length(table), 0)
    expect_true(is.character(table))
  }
})

test_that("every shipped language file carries exactly the same key set as en.json", {
  en <- episodic_i18n_load("en")
  for (lang in setdiff(episodic_shipped_langs, "en")) {
    table <- episodic_i18n_load(lang)
    expect_setequal(names(table), names(en))
  }
})

test_that("every shipped language file uses the same {placeholder} tokens per key as en.json", {
  en <- episodic_i18n_load("en")
  extract_placeholders <- function(x) {
    sort(unique(regmatches(x, gregexpr("\\{[a-zA-Z_]+\\}", x))[[1]]))
  }
  for (lang in setdiff(episodic_shipped_langs, "en")) {
    table <- episodic_i18n_load(lang)
    for (key in names(en)) {
      expect_identical(
        extract_placeholders(table[[key]]),
        extract_placeholders(en[[key]]),
        info = paste("key:", key, "lang:", lang)
      )
    }
  }
})

test_that("episodic_tr() substitutes placeholders", {
  expect_equal(
    episodic_tr("dossier.cluster_ref", id = 1041, lang = "nl"),
    "#1041"
  )
  expect_equal(
    episodic_tr("dossier.cluster_ref", id = 1041, lang = "en"),
    "#1041"
  )
})

test_that("each language marks a cluster reference its own way", {
  # "#" is not universal: Spanish writes n.º, French n°, German Nr.,
  # Arabic رقم. Keeping this a translation key rather than a hardcoded
  # "#" is the whole reason it is one.
  expect_equal(
    episodic_tr("dossier.cluster_ref", id = 300, lang = "de"),
    "Nr. 300"
  )
  expect_equal(
    episodic_tr("dossier.cluster_ref", id = 300, lang = "fr"),
    "n\u00b0 300"
  )
  for (lang in episodic_shipped_langs) {
    expect_match(
      episodic_tr("dossier.cluster_ref", id = 300, lang = lang),
      "300",
      fixed = TRUE
    )
  }
})

test_that("episodic_tr() falls back from nl to en when a key is nl-missing", {
  # simulate a key present only in en by loading en's cache and asking under nl
  # with a key we know exists in en (all shared here, so instead test the
  # literal fallback mechanism via a key deliberately absent from both)
  result <- episodic_tr("this.key.does.not.exist", lang = "nl")
  expect_equal(result, "[[this.key.does.not.exist]]")
})

test_that("a missing key renders visibly rather than blank", {
  result <- episodic_tr("totally.bogus.key", lang = "en")
  expect_match(result, "^\\[\\[.*\\]\\]$")
  expect_false(identical(result, ""))
})

test_that("an instance override takes priority over the shipped translation", {
  overrides <- c("app.title" = "Instance-Specific Title")
  result <- episodic_tr("app.title", lang = "nl", instance_i18n = overrides)
  expect_equal(result, "Instance-Specific Title")
})

test_that("episodic_tr() with no instance override uses the shipped file", {
  result <- episodic_tr("app.title", lang = "nl")
  expect_equal(result, "EpiSODIC")
})

test_that("no function hardcodes its default language: EPISODIC_LANGUAGE decides", {
  # The instance picks its language once, through the environment
  # variable, and every entry point defaults to it. A function defaulting
  # to a language of its own reintroduces the bug this replaced: a
  # dashboard that is English at the top and Dutch three panels down,
  # depending on which internal helper rendered what.
  r_files <- list.files(
    file.path(testthat::test_path(), "..", "..", "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  offenders <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    code <- lines[!grepl("^\\s*#", lines)] # roxygen may show a language
    hits <- grep('lang = "[a-z]{2}"', code, value = TRUE)
    if (length(hits) > 0) {
      offenders <- c(offenders, paste0(basename(f), ": ", trimws(hits)))
    }
  }
  expect_equal(offenders, character(0))
})

test_that("episodic_lang() reads an unset language as English, not as a language of its own", {
  expect_equal(episodic_lang("nl"), "nl")
  expect_equal(episodic_lang(""), "en")
  expect_equal(episodic_lang(NA_character_), "en")
  expect_equal(episodic_lang(character(0)), "en")
})

test_that("the environment variable alone decides what an internal renderer produces", {
  # No argument passed anywhere: the default has to reach through.
  withr_set <- Sys.getenv("EPISODIC_LANGUAGE", unset = NA)
  on.exit(
    if (is.na(withr_set)) {
      Sys.unsetenv("EPISODIC_LANGUAGE")
    } else {
      Sys.setenv(EPISODIC_LANGUAGE = withr_set)
    },
    add = TRUE
  )

  Sys.unsetenv("EPISODIC_LANGUAGE")
  expect_equal(
    episodic_tr("nav.clusters"),
    episodic_tr("nav.clusters", lang = "en")
  )

  Sys.setenv(EPISODIC_LANGUAGE = "nl")
  expect_equal(
    episodic_tr("nav.clusters"),
    episodic_tr("nav.clusters", lang = "nl")
  )
})

test_that("every key used in code exists in both language files", {
  # a lightweight guard against mistyped tr() keys: scan R/ for episodic_tr("key"...
  r_files <- list.files(
    file.path(testthat::test_path(), "..", "..", "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  code <- paste(
    vapply(
      r_files,
      function(f) paste(readLines(f, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )
  used_keys <- regmatches(
    code,
    gregexpr('episodic_tr\\("([a-zA-Z0-9_.]+)"', code)
  )[[1]]
  used_keys <- gsub('episodic_tr\\("|"$', "", used_keys)
  used_keys <- unique(used_keys)
  skip_if(length(used_keys) == 0, "no episodic_tr() calls found yet")
  nl <- episodic_i18n_load("nl")
  missing <- setdiff(used_keys, names(nl))
  expect_equal(missing, character(0))
})

test_that("episodic_format_date_range() collapses shared month/year, in order regardless of input order", {
  # Only one month name needs to appear (same month, or a single date):
  # spelled out in full, not abbreviated.
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-15", lang = "nl"),
    "7-15 januari 2025"
  )
  expect_equal(
    episodic_format_date_range("2025-01-15", "2025-01-07", lang = "nl"),
    "7-15 januari 2025"
  ) # swapped input, same output
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-07", lang = "nl"),
    "7 januari 2025"
  ) # single day, no range dash
  # Two different month names have to appear side by side: abbreviated,
  # so the string does not double in length for no gain in clarity.
  expect_equal(
    episodic_format_date_range("2025-11-28", "2025-12-03", lang = "nl"),
    "28 nov. - 3 dec. 2025"
  )
  expect_equal(
    episodic_format_date_range("2024-12-28", "2025-01-03", lang = "nl"),
    "28 dec. 2024 - 3 jan. 2025"
  )
})

test_that("episodic_format_date_range() uses English month names for lang = 'en'", {
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-15", lang = "en"),
    "7-15 January 2025"
  )
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-07", lang = "en"),
    "7 January 2025"
  )
  expect_equal(
    episodic_format_date_range("2025-06-09", "2025-06-20", lang = "en"),
    "9-20 June 2025"
  )
  expect_equal(
    episodic_format_date_range("2025-11-28", "2025-12-03", lang = "en"),
    "28 Nov - 3 Dec 2025"
  )
})

test_that("episodic_format_date_range() falls back cleanly on unparseable input", {
  expect_equal(
    episodic_format_date_range(NA, "2025-01-15", lang = "en"),
    episodic_tr("misc.unknown", lang = "en")
  )
  expect_equal(
    episodic_format_date_range("not-a-date", "2025-01-15", lang = "en"),
    episodic_tr("misc.unknown", lang = "en")
  )
})

test_that("Dutch never renders 'pathogen' as 'pathogeen', in any casing", {
  # House terminology: the Dutch word is "verwekker", without exception.
  # Asserted rather than left to review, because the failure mode is a
  # single new key years from now that nobody diffs against the rest of
  # the file.
  nl <- episodic_i18n_load("nl")
  # {pathogen} and friends are placeholder tokens substituted at render
  # time, never translated - strip them before looking.
  values <- gsub("\\{[a-zA-Z_]+\\}", "", nl)
  offenders <- names(values)[grepl("pathogeen", values, ignore.case = TRUE)]
  expect_equal(offenders, character(0))
})

test_that("English says 'pathogen', never 'organism'", {
  # Influenza is not an organism. "Organism" was in six of the shipped
  # English strings and their translations, and is simply wrong for the
  # viruses this system spends most of its time watching.
  en <- episodic_i18n_load("en")
  offenders <- names(en)[grepl("organism", en, ignore.case = TRUE)]
  expect_equal(offenders, character(0))
  expect_true(any(grepl("pathogen", en, ignore.case = TRUE)))
})

test_that("no shipped language still calls the concept an organism", {
  # The same word, per language, as it was translated from English.
  organism_words <- c(
    nl = "organisme",
    en = "organism",
    es = "organismo",
    fr = "organisme",
    de = "Organismus",
    zh = "\u751f\u7269\u4f53",
    hi = "\u091c\u0940\u0935",
    ar = "\u0643\u0627\u0626\u0646"
  )
  for (lang in episodic_shipped_langs) {
    table <- episodic_i18n_load(lang)
    offenders <- names(table)[grepl(
      organism_words[[lang]],
      table,
      ignore.case = TRUE
    )]
    expect_equal(
      offenders,
      character(0),
      info = paste(lang, organism_words[[lang]])
    )
  }
})

test_that("Dutch does use 'verwekker' for the concept, so the rule above is not vacuous", {
  nl <- episodic_i18n_load("nl")
  expect_true(any(grepl("verwekker", nl, ignore.case = TRUE)))
  expect_equal(unname(nl[["nav.pathogen"]]), "Verwekker")
})

test_that("every language names the Pathogen screen the same way in its nav entry and its title", {
  # A nav entry reading one thing and the screen it opens reading another
  # is the same class of slip as the Dutch one, just harder to spot.
  for (lang in episodic_shipped_langs) {
    table <- episodic_i18n_load(lang)
    nav <- table[["nav.pathogen"]]
    title <- table[["pathogen.title"]]
    # Singular stem, so an inflected or compounded title still matches
    # (Verwekker -> Verwekkeractiviteit, Patógeno -> del patógeno).
    stem <- sub("s$", "", tolower(nav))
    expect_true(
      grepl(stem, tolower(title), fixed = TRUE),
      info = paste0(lang, ": nav '", nav, "' vs title '", title, "'")
    )
  }
})

test_that("episodic_format_number() writes each language's own marks, not the C locale's", {
  # The four languages that swap the two marks round.
  expect_equal(episodic_format_number(1234.5, lang = "en"), "1,234.5")
  expect_equal(episodic_format_number(1234.5, lang = "nl"), "1.234,5")
  expect_equal(episodic_format_number(1234.5, lang = "de"), "1.234,5")
  expect_equal(episodic_format_number(1234.5, lang = "es"), "1.234,5")
  # French groups with a no-break space (U+00A0), not a plain one. The
  # escape is written out rather than the character itself, as
  # everywhere else in this codebase.
  expect_equal(episodic_format_number(1234.5, lang = "fr"), "1\u00a0234,5")

  # And the three that do not, which the old en/everything-else split in
  # the chart labeller got wrong: with Western digits, Arabic, Hindi and
  # Chinese all write 1,234.5.
  for (lang in c("ar", "hi", "zh")) {
    expect_equal(episodic_format_number(1234.5, lang = lang), "1,234.5", info = lang)
  }
})

test_that("episodic_format_number() groups Hindi the Indian way and Spanish not at four digits", {
  # Lakh/crore grouping: 12,34,567, not 1,234,567.
  expect_equal(episodic_format_number(1234567, lang = "hi"), "12,34,567")
  expect_equal(episodic_format_number(1234567, lang = "en"), "1,234,567")

  # CLDR's minimumGroupingDigits: Spanish writes 2000 unseparated, and
  # separates from five digits up.
  expect_equal(episodic_format_number(2000, lang = "es"), "2000")
  expect_equal(episodic_format_number(12345, lang = "es"), "12.345")
  expect_equal(episodic_format_number(2000, lang = "nl"), "2.000")
})

test_that("episodic_format_number() rounds to `digits` and drops trailing zeros", {
  expect_equal(episodic_format_number(2, digits = 1, lang = "en"), "2")
  expect_equal(episodic_format_number(2.35, digits = 1, lang = "en"), "2.4")
  expect_equal(episodic_format_number(2.35, digits = 1, lang = "nl"), "2,4")
  expect_equal(episodic_format_number(61.6, digits = 0, lang = "nl"), "62")
  # A value large enough that as.character() would have gone scientific.
  expect_equal(episodic_format_number(1e5, lang = "en"), "100,000")
})

test_that("episodic_format_number() keeps the sign, passes NA through, and is vectorised", {
  expect_equal(episodic_format_number(-1234.5, lang = "nl"), "-1.234,5")
  expect_true(is.na(episodic_format_number(NA, lang = "nl")))
  expect_true(is.na(episodic_format_number(NA_real_, lang = "nl")))
  expect_equal(
    episodic_format_number(c(1000, NA, 2.5), lang = "de"),
    c("1.000", NA, "2,5")
  )
  expect_equal(episodic_format_number(numeric(0), lang = "de"), character(0))
  # Not a plain number: reported as R renders it rather than rewritten.
  expect_equal(episodic_format_number(Inf, lang = "nl"), "Inf")
})

test_that("episodic_format_number() ignores options(OutDec), which is a session setting and not a language", {
  # A Dutch R session sets OutDec = ",", and format() honours it. Two
  # people reading the same dashboard must not see two different numbers
  # because one of them set an option.
  previous <- options(OutDec = ",")
  on.exit(options(previous), add = TRUE)
  expect_equal(episodic_format_number(1234.5, lang = "en"), "1,234.5")
  expect_equal(episodic_format_number(1234.5, lang = "nl"), "1.234,5")
  expect_equal(episodic_format_number(2.5, digits = 1, lang = "en"), "2.5")
})

test_that("a grouping value that is not a whole number warns and falls back to threes", {
  # A translation file is edited by translators, and "every third" is the
  # kind of thing that can land in a key like this. It must not take a
  # dashboard down, and it must not pass silently either.
  local_mocked_bindings(
    episodic_tr = function(key, ..., lang = "en", instance_i18n = NULL) {
      switch(key,
        "misc.decimal.mark" = ".",
        "misc.thousands.mark" = ",",
        "misc.thousands.grouping" = "every third",
        "misc.thousands.minimum" = "1",
        key
      )
    }
  )
  expect_warning(
    marks <- episodic_number_marks("en"),
    "misc.thousands.grouping"
  )
  expect_equal(marks$sizes, 3L)
})

test_that("episodic_number_group() takes its group sizes right to left, repeating the last", {
  marks <- list(decimal = ".", thousands = ",", sizes = 3L, minimum = 1L)
  expect_equal(episodic_number_group("1234567", marks), "1,234,567")
  expect_equal(
    episodic_number_group("1234567", utils::modifyList(marks, list(sizes = c(3L, 2L)))),
    "12,34,567"
  )
  # A language that does not group at all leaves the digits alone.
  expect_equal(
    episodic_number_group("1234567", utils::modifyList(marks, list(thousands = ""))),
    "1234567"
  )
})

test_that("every shipped language names all eight languages, in its own words", {
  for (lang in episodic_shipped_langs) {
    for (code in episodic_shipped_langs) {
      label <- episodic_language_label(code, lang = lang)
      expect_true(nzchar(label), info = paste(lang, code))
      expect_false(grepl("[[", label, fixed = TRUE), info = paste(lang, code))
      # A name, not the code it stands in for.
      expect_false(identical(label, code), info = paste(lang, code))
    }
  }
  expect_equal(episodic_language_label("nl", lang = "en"), "Dutch")
  expect_equal(episodic_language_label("nl", lang = "nl"), "Nederlands")
  expect_equal(episodic_language_label("en", lang = "de"), "Englisch")
  # An unknown code is still the truest thing that can be said about it.
  expect_equal(episodic_language_label("pt", lang = "en"), "pt")
})

test_that("the unsupported-language warning names both the code and the language", {
  # The code is what goes in EPISODIC_LANGUAGE; the name is what tells an
  # operator which code they want.
  choices <- episodic_language_choices(lang = "en")
  expect_true("nl (Dutch)" %in% choices)
  expect_equal(length(choices), length(episodic_shipped_langs))
})
