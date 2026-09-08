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

# The complete-week rule, the language fallback, the schema version, and
# the mail encoding: four things that were each individually silent, and
# each of which a laboratory outside the one this package was written at
# would have hit first.

# ---- Farrington weekly bins --------------------------------------------

test_that("the last weekly bin is the last COMPLETE week, not a partial one", {
  # 2025-06-30 is a Monday. The week starting that day has one day of data
  # in it; binning to it and comparing that against a baseline of full
  # weeks is a systematic undercount that suppresses the alarm.
  expect_equal(
    episodic_last_complete_week_start(as.Date("2025-06-30")),
    as.Date("2025-06-23")
  )
  # 2025-06-29 is a Sunday: its own week has just finished, so it counts.
  expect_equal(
    episodic_last_complete_week_start(as.Date("2025-06-29")),
    as.Date("2025-06-23")
  )
  expect_equal(
    episodic_last_complete_week_start(as.Date("2025-07-05")),
    as.Date("2025-06-23")
  )
  # %u is locale-independent, so a Monday is a Monday everywhere.
  expect_equal(episodic_week_start(as.Date("2025-06-25")), as.Date("2025-06-23"))
})

test_that("weekly bins end on a complete week and count every case in it", {
  dates <- as.Date(c("2025-06-23", "2025-06-24", "2025-06-29", "2025-07-01"))
  weekly <- episodic_weekly_bins(dates, run_date = as.Date("2025-07-02"))
  expect_equal(weekly$week_start[length(weekly$week_start)], as.Date("2025-06-23"))
  # The case on 2025-07-01 falls in the still-running week and is
  # correctly not counted anywhere yet.
  expect_equal(sum(weekly$counts), 3)
})

test_that("no complete week yet yields no bins rather than an error", {
  weekly <- episodic_weekly_bins(
    as.Date("2025-07-01"),
    run_date = as.Date("2025-07-02")
  )
  expect_equal(length(weekly$counts), 0)
  expect_s3_class(weekly$week_start, "Date")
})

# ---- Language resolution ------------------------------------------------

test_that("an unsupported EPISODIC_LANGUAGE falls back to English instead of crashing", {
  # A hard error here took the whole dashboard down on every render, and
  # a locale-shaped value is a reasonable thing for an operator to set.
  expect_warning(resolved <- episodic_lang("pt"), "no translations")
  expect_equal(resolved, "en")
  expect_warning(resolved <- episodic_lang("en_GB"), "no translations")
  expect_equal(resolved, "en")
  # Warned once per value per session, not on every render.
  expect_silent(episodic_lang("pt"))
  expect_equal(episodic_lang(""), "en")
  expect_equal(episodic_lang(NA), "en")
  expect_equal(episodic_lang("nl"), "nl")
})

test_that("every shipped language has a file, and Arabic is right to left", {
  for (lang in episodic_languages) {
    expect_type(episodic_i18n_load(lang), "character")
  }
  expect_true(episodic_lang_is_rtl("ar"))
  expect_equal(episodic_lang_dir("ar"), "rtl")
  expect_false(episodic_lang_is_rtl("en"))
  expect_equal(episodic_lang_dir("nl"), "ltr")
  expect_warning(expect_equal(episodic_lang_dir("pt-BR"), "ltr"))
})

test_that("the dashboard declares its language and direction on the document", {
  ui <- as.character(episodic_app_ui(lang = "ar"))
  expect_match(ui, "setAttribute\\('lang', 'ar'\\)")
  expect_match(ui, "setAttribute\\('dir', 'rtl'\\)")
  ui_en <- as.character(episodic_app_ui(lang = "en"))
  expect_match(ui_en, "setAttribute\\('dir', 'ltr'\\)")
})

test_that("the stylesheet stays free of physical left/right properties", {
  # Logical properties are what make the whole layout mirror itself in
  # Arabic with no per-direction rules to keep in step; a margin-left
  # added later is a component that stays stubbornly left-aligned.
  css_path <- system.file("app", "www", "episodic.css", package = "EpiSODIC")
  if (identical(css_path, "")) {
    css_path <- file.path("inst", "app", "www", "episodic.css")
  }
  skip_if_not(file.exists(css_path))
  declarations <- grep(
    "^\\s*(margin|padding|border)-(left|right)\\b|^\\s*text-align:\\s*(left|right)\\b",
    readLines(css_path, warn = FALSE),
    value = TRUE
  )
  expect_equal(declarations, character(0))
})

# ---- Schema versioning --------------------------------------------------

test_that("a freshly created database records the schema version it was built at", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
})

test_that("a database from before schema versioning is refused, then adopted", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))

  # Exactly the shape EpiSODIC 0.12.x left behind: every table but this one.
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbDisconnect(con)

  expect_error(episodic_db_connect(path), "episodic_db_migrate")
  expect_message(episodic_db_migrate(path), "schema version 1")

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), 1L)
})

test_that("a database from a newer EpiSODIC is refused rather than half-read", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_schema_version (version, applied_at, applied_by)
     VALUES (?, ?, ?)",
    params = list(episodic_schema_version + 1L, "2099-01-01T00:00:00Z", "99.0.0")
  )
  DBI::dbDisconnect(con)

  expect_error(episodic_db_connect(path), "newer than the version")
  expect_error(episodic_db_migrate(path), "newer than the version")
})

test_that("migrating an already-current database changes nothing", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  expect_message(version <- episodic_db_migrate(path), "schema version")
  expect_equal(version, episodic_schema_version)
})

# ---- Mail encoding ------------------------------------------------------

test_that("a non-ASCII subject is RFC 2047 encoded, and an ASCII one is left alone", {
  expect_equal(episodic_notify_header_encode("Outbreak report"), "Outbreak report")
  encoded <- episodic_notify_header_encode("Uitbraak Fryslân")
  expect_match(encoded, "^=\\?UTF-8\\?B\\?")
  expect_match(encoded, "\\?=$")
  expect_false(grepl("[^\x01-\x7F]", encoded, useBytes = TRUE))
})

test_that("a newline in a header value cannot inject a header", {
  # The subject is built from a pathogen name and a location, which came
  # from an operator's own data feed.
  expect_false(grepl(
    "[\r\n]",
    episodic_notify_header_encode("Norovirus\r\nBcc: someone@example.org")
  ))
})

test_that("base64 bodies are wrapped to the length RFC 2045 allows", {
  wrapped <- episodic_notify_base64_wrap(strrep("A", 500))
  expect_true(all(nchar(strsplit(wrapped, "\r\n")[[1]]) <= 76))
  expect_equal(gsub("[\r\n]", "", wrapped), strrep("A", 500))
  expect_equal(episodic_notify_base64_wrap(""), "")
})

test_that("a MIME message is 7-bit clean whatever language it is written in", {
  message_body <- "<p>إنذار تفشي</p>"
  mime <- episodic_notify_mime_message(
    from = "a@example.org",
    to = "b@example.org",
    subject = "爆发报告",
    html_body = message_body
  )
  expect_false(grepl("[^\x01-\x7F]", mime, useBytes = TRUE))
  expect_match(mime, "Content-Transfer-Encoding: base64", fixed = TRUE)
  expect_match(mime, "Subject: =?UTF-8?B?", fixed = TRUE)
})

test_that("an attached report is base64, wrapped, and named safely", {
  attachment <- tempfile(fileext = ".html")
  on.exit(unlink(attachment))
  writeLines(strrep("x", 4000), attachment)

  mime <- episodic_notify_mime_message(
    from = "a@example.org",
    to = c("b@example.org", "c@example.org"),
    subject = "Report",
    html_body = "<p>see attached</p>",
    attachment_path = attachment
  )
  expect_match(mime, "multipart/mixed", fixed = TRUE)
  expect_match(mime, "Content-Disposition: attachment", fixed = TRUE)
  expect_true(all(nchar(strsplit(mime, "\r\n")[[1]]) <= 998))
})
