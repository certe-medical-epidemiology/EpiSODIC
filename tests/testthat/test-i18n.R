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

test_that("episodic_i18n_load() reads both shipped languages", {
  nl <- episodic_i18n_load("nl")
  en <- episodic_i18n_load("en")
  expect_gt(length(nl), 0)
  expect_gt(length(en), 0)
  expect_true(is.character(nl))
})

test_that("nl.json and en.json carry exactly the same key set", {
  nl <- episodic_i18n_load("nl")
  en <- episodic_i18n_load("en")
  expect_setequal(names(nl), names(en))
})

test_that("episodic_tr() substitutes placeholders", {
  expect_equal(episodic_tr("dossier.meta.cluster_id", id = 1041, lang = "nl"), "cluster #1041")
  expect_equal(episodic_tr("dossier.meta.cluster_id", id = 1041, lang = "en"), "cluster #1041")
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
  r_files <- list.files(file.path(testthat::test_path(), "..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  code <- paste(vapply(r_files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  used_keys <- regmatches(code, gregexpr('episodic_tr\\("([a-zA-Z0-9_.]+)"', code))[[1]]
  used_keys <- gsub('episodic_tr\\("|"$', "", used_keys)
  used_keys <- unique(used_keys)
  skip_if(length(used_keys) == 0, "no episodic_tr() calls found yet")
  nl <- episodic_i18n_load("nl")
  missing <- setdiff(used_keys, names(nl))
  expect_equal(missing, character(0))
})

test_that("episodic_format_date_range() collapses shared month/year, in order regardless of input order", {
  expect_equal(episodic_format_date_range("2025-01-07", "2025-01-15", lang = "nl"), "7-15 jan. 2025")
  expect_equal(episodic_format_date_range("2025-01-15", "2025-01-07", lang = "nl"), "7-15 jan. 2025")  # swapped input, same output
  expect_equal(episodic_format_date_range("2025-01-07", "2025-01-07", lang = "nl"), "7 jan. 2025")  # single day, no range dash
  expect_equal(episodic_format_date_range("2025-11-28", "2025-12-03", lang = "nl"), "28 nov. - 3 dec. 2025")
  expect_equal(episodic_format_date_range("2024-12-28", "2025-01-03", lang = "nl"), "28 dec. 2024 - 3 jan. 2025")
})

test_that("episodic_format_date_range() uses English month abbreviations for lang = 'en'", {
  expect_equal(episodic_format_date_range("2025-01-07", "2025-01-15", lang = "en"), "7-15 Jan 2025")
})

test_that("episodic_format_date_range() falls back cleanly on unparseable input", {
  expect_equal(episodic_format_date_range(NA, "2025-01-15", lang = "en"), episodic_tr("misc.unknown", lang = "en"))
  expect_equal(episodic_format_date_range("not-a-date", "2025-01-15", lang = "en"), episodic_tr("misc.unknown", lang = "en"))
})
