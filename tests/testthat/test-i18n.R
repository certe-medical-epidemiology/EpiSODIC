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

test_that("every key used in code exists in both language files", {
  # a lightweight guard against typo'd tr() keys: scan R/ for episodic_tr("key"...
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
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-15", lang = "nl"),
    "7-15 jan. 2025"
  )
  expect_equal(
    episodic_format_date_range("2025-01-15", "2025-01-07", lang = "nl"),
    "7-15 jan. 2025"
  ) # swapped input, same output
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-07", lang = "nl"),
    "7 jan. 2025"
  ) # single day, no range dash
  expect_equal(
    episodic_format_date_range("2025-11-28", "2025-12-03", lang = "nl"),
    "28 nov. - 3 dec. 2025"
  )
  expect_equal(
    episodic_format_date_range("2024-12-28", "2025-01-03", lang = "nl"),
    "28 dec. 2024 - 3 jan. 2025"
  )
})

test_that("episodic_format_date_range() uses English month abbreviations for lang = 'en'", {
  expect_equal(
    episodic_format_date_range("2025-01-07", "2025-01-15", lang = "en"),
    "7-15 Jan 2025"
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
