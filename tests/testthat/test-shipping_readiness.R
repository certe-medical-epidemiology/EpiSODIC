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

# The complete-week rule, the language fallback, the schema version and
# the mail encoding: four things that fail silently when they fail, and
# that a laboratory outside the one this package was written at meets
# first.

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
  # Warned once per value per session, not on every render.
  expect_silent(episodic_lang("pt"))
  expect_equal(episodic_lang(""), "en")
  expect_equal(episodic_lang(NA), "en")
  expect_equal(episodic_lang("nl"), "nl")

  # `en_GB` is British English written out, which is what `en` is - not
  # a language EpiSODIC has no translations for.
  expect_silent(resolved <- episodic_lang("en_GB"))
  expect_equal(resolved, "en")
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
  # `as.character()` on a bslib page renders the body only - the <head>
  # is carried separately and comes back from `htmltools::renderTags()`,
  # which is where anything in `shiny::tags$head()` actually lands.
  head_of <- function(lang) {
    rendered <- htmltools::renderTags(episodic_app_ui(lang = lang))
    paste(
      c(as.character(rendered$head), as.character(rendered$html)),
      collapse = "\n"
    )
  }
  ar <- head_of("ar")
  expect_match(ar, "setAttribute('lang', 'ar')", fixed = TRUE)
  expect_match(ar, "setAttribute('dir', 'rtl')", fixed = TRUE)

  en <- head_of("en")
  expect_match(en, "setAttribute('lang', 'en')", fixed = TRUE)
  expect_match(en, "setAttribute('dir', 'ltr')", fixed = TRUE)

  # An unshipped language falls back to English in both attributes, so
  # the page never claims a language it has no text for.
  #
  # `episodic_lang()` warns once per unrecognised value per session, and
  # an earlier test in this file has already spent "pt" - so clear that
  # registry rather than assert a warning that has already been issued.
  rm(list = ls(envir = episodic_lang_warned), envir = episodic_lang_warned)
  expect_warning(pt <- head_of("pt"), "no translations")
  expect_match(pt, "setAttribute('lang', 'en')", fixed = TRUE)
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

test_that("a database from before schema versioning is refused, then adopted and brought forward", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))

  # Exactly the shape EpiSODIC 0.12.x left behind: no version table, and
  # none of the tables or indexes added since.
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbExecute(con, "DROP TABLE episodic_app_login_failure")
  DBI::dbExecute(con, "DROP TABLE episodic_report_version_claim")
  DBI::dbExecute(con, "DROP INDEX idx_episodic_report_render_version")
  DBI::dbDisconnect(con)

  expect_error(episodic_db_connect(path), "episodic_db_migrate")
  expect_message(
    version <- episodic_db_migrate(path),
    "schema version 1"
  )
  expect_equal(version, episodic_schema_version)

  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  # Every step it passed through is recorded, not just the last.
  expect_setequal(
    DBI::dbGetQuery(con, "SELECT version FROM episodic_schema_version")$version,
    seq_len(episodic_schema_version)
  )
  # And the migration built the real tables, not an approximation of them.
  expect_true(DBI::dbExistsTable(con, "episodic_app_login_failure"))
  expect_setequal(
    DBI::dbListFields(con, "episodic_app_login_failure"),
    c("failure_id", "attempted_at", "username", "user_id", "reason")
  )
  expect_true(DBI::dbExistsTable(con, "episodic_report_version_claim"))
  expect_setequal(
    DBI::dbListFields(con, "episodic_report_version_claim"),
    c("claim_id", "cluster_id", "version_no", "claimed_at", "claimed_by")
  )
  expect_true(episodic_db_index_exists(
    con,
    "sqlite",
    "idx_episodic_report_render_version",
    "episodic_report_render"
  ))
})

test_that("a migration that fails is rolled back and does not record its version", {
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbDisconnect(con)

  local_mocked_bindings(
    episodic_db_migrations = function() {
      list("2" = function(con, dialect) stop("deliberate failure", call. = FALSE))
    }
  )
  expect_error(episodic_db_migrate(path), "rolled back")

  con <- episodic_db_connect(path, check_schema_version = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  # Adopted at 1, and stopped there: the failed step wrote nothing.
  expect_equal(episodic_db_schema_version(con), 1L)
})

test_that("a migration re-run after a partial MariaDB-style failure completes", {
  # MariaDB and MySQL commit implicitly on DDL, so a step can leave its
  # table behind with no version row recorded. Every migration is
  # written to skip what it finds already done, so the second attempt
  # is the fix rather than a second failure.
  path <- episodic_test_db_path()
  on.exit(unlink(path))
  con <- episodic_db_connect(path)
  # Exactly that state: the new table present, the version table gone.
  DBI::dbExecute(con, "DROP TABLE episodic_schema_version")
  DBI::dbDisconnect(con)

  expect_message(
    version <- episodic_db_migrate(path),
    "schema version"
  )
  expect_equal(version, episodic_schema_version)
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

test_that("every version between 1 and the current one has a migration registered", {
  # A version bumped without a migration is a database nobody can bring
  # forward, discovered by an operator on upgrade day rather than here.
  migrations <- episodic_db_migrations()
  expect_setequal(
    names(migrations),
    as.character(seq_len(episodic_schema_version)[-1])
  )
  for (step in migrations) {
    expect_type(step, "closure")
    expect_named(formals(step), c("con", "dialect"))
  }
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
