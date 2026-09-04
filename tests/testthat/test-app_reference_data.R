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

# Restore whatever the environment held, whichever way a test leaves it.
with_pc_province_map <- function(value, code) {
  old <- Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("EPISODIC_PC_PROVINCE_MAP")
    } else {
      Sys.setenv(EPISODIC_PC_PROVINCE_MAP = old)
    },
    add = TRUE
  )
  if (is.na(value)) {
    Sys.unsetenv("EPISODIC_PC_PROVINCE_MAP")
  } else {
    Sys.setenv(EPISODIC_PC_PROVINCE_MAP = value)
  }
  force(code)
}

reference_row_for <- function(rows, variable) {
  match <- Filter(function(r) identical(r$variable, variable), rows)
  expect_equal(length(match), 1)
  match[[1]]
}

test_that("an unconfigured PC-to-province mapping says the demo ranges are standing in", {
  row <- with_pc_province_map(
    NA,
    episodic_app_reference_pc_province(NULL, lang = "en")
  )
  expect_equal(row$status, "default")
  expect_match(row$detail, "demo ranges", fixed = TRUE)
  expect_true(is.na(row$path))
})

test_that("a configured mapping reports what it actually holds", {
  mapping <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      pc = c("9713", "9714", "8911"),
      province_code = c("Groningen", "Groningen", "Fryslan")
    ),
    mapping,
    row.names = FALSE
  )
  on.exit(unlink(mapping), add = TRUE)

  row <- with_pc_province_map(
    mapping,
    episodic_app_reference_pc_province(NULL, lang = "en")
  )
  expect_equal(row$status, "in_use")
  expect_match(row$detail, "3 postcodes", fixed = TRUE)
  expect_match(row$detail, "2 provinces", fixed = TRUE)
  expect_match(row$detail, "Fryslan, Groningen", fixed = TRUE)
  # the path is carried, for a signed-in reader to compare against what
  # they set; the panel is what decides whether to show it
  expect_equal(row$path, mapping)
})

test_that("a mapping that cannot be used is a problem, with the reason in the row", {
  wrong_cols <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(postcode = "9713", province = "Groningen"),
    wrong_cols,
    row.names = FALSE
  )
  on.exit(unlink(wrong_cols), add = TRUE)

  row <- with_pc_province_map(
    wrong_cols,
    episodic_app_reference_pc_province(NULL, lang = "en")
  )
  expect_equal(row$status, "problem")
  expect_match(row$detail, "province_code", fixed = TRUE)
  expect_match(row$detail, "postcode", fixed = TRUE)
})

test_that("a mapping that matches none of the case data says so, which is the whole point", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con), add = TRUE)

  # Postcodes formatted one way in the mapping, another in the case data
  # - read fine, rejected by nothing, and matching nothing. Indistinguishable
  # from a file that was never read, until this row says otherwise.
  mismatched <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(pc = "9713 AB", province_code = "Groningen"),
    mismatched,
    row.names = FALSE
  )
  on.exit(unlink(mismatched), add = TRUE)

  row <- with_pc_province_map(
    mismatched,
    episodic_app_reference_pc_province(env$con, lang = "en")
  )
  expect_equal(row$status, "in_use")
  expect_match(row$detail, "None of the", fixed = TRUE)
  expect_match(row$detail, "not a prefix of them", fixed = TRUE)

  # and a mapping that does match reports the coverage rather than the alarm
  matching <- tempfile(fileext = ".csv")
  pcs <- DBI::dbGetQuery(
    env$con,
    "SELECT DISTINCT pc FROM episodic_case WHERE pc IS NOT NULL"
  )$pc
  expect_gt(length(pcs), 0)
  utils::write.csv(
    data.frame(pc = pcs, province_code = "Groningen"),
    matching,
    row.names = FALSE
  )
  on.exit(unlink(matching), add = TRUE)

  row <- with_pc_province_map(
    matching,
    episodic_app_reference_pc_province(env$con, lang = "en")
  )
  expect_match(
    row$detail,
    paste(length(pcs), "of the", length(pcs)),
    fixed = TRUE
  )
  expect_false(grepl("None of the", row$detail, fixed = TRUE))
})

test_that("the coverage check is over distinct postcodes, and is skipped without a connection", {
  expect_null(episodic_app_reference_pc_coverage(NULL))

  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con), add = TRUE)
  coverage <- episodic_app_reference_pc_coverage(env$con)
  expect_false(is.null(coverage))
  distinct <- DBI::dbGetQuery(
    env$con,
    "SELECT COUNT(DISTINCT pc) AS n FROM episodic_case WHERE pc IS NOT NULL"
  )$n
  expect_equal(coverage$total, distinct)
  expect_lte(coverage$matched, coverage$total)
})

test_that("every reference variable gets a row, with a known status", {
  rows <- episodic_app_reference_data(NULL, lang = "en")
  variables <- vapply(rows, function(r) r$variable, character(1))
  expect_setequal(
    variables,
    c(
      "EPISODIC_PC_PROVINCE_MAP",
      "EPISODIC_GEO_DATA",
      "EPISODIC_GEO_DATA_OVERLAY",
      "EPISODIC_LANGUAGE",
      "EPISODIC_CONFIG",
      "EPISODIC_STYLE",
      "EPISODIC_QUARTO_REPORT"
    )
  )
  for (row in rows) {
    expect_true(row$status %in% episodic_reference_statuses, info = row$variable)
    expect_true(nzchar(row$detail), info = row$variable)
  }
})

test_that("the reference panel renders in every shipped language, with no missing translation", {
  for (lang in c("en", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    html <- as.character(episodic_ui_info_reference_panel(NULL, lang = lang))
    expect_false(grepl("[[", html, fixed = TRUE), info = lang)
    expect_true(
      grepl("EPISODIC_PC_PROVINCE_MAP", html, fixed = TRUE),
      info = lang
    )
  }
})

test_that("the resolved path is shown to a signed-in reader and withheld from a visitor", {
  mapping <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(pc = "9713", province_code = "Groningen"),
    mapping,
    row.names = FALSE
  )
  on.exit(unlink(mapping), add = TRUE)

  # The Info screen needs no sign-in, and a filesystem path says more
  # about the machine than a visitor needs; the status and counts, which
  # are what answer "is my file being used", stay public either way.
  visitor <- with_pc_province_map(
    mapping,
    as.character(episodic_ui_info_reference_panel(NULL, lang = "en"))
  )
  expect_false(grepl(mapping, visitor, fixed = TRUE))
  expect_true(grepl("EPISODIC_PC_PROVINCE_MAP", visitor, fixed = TRUE))

  signed_in <- with_pc_province_map(
    mapping,
    as.character(episodic_ui_info_reference_panel(
      NULL,
      current_user = list(username = "tester"),
      lang = "en"
    ))
  )
  expect_true(grepl(mapping, signed_in, fixed = TRUE))
  expect_true(grepl("episodic-reference-path", signed_in, fixed = TRUE))
})

test_that("the Info screen carries the reference panel", {
  html <- as.character(episodic_ui_info_screen(lang = "en"))
  expect_true(grepl(
    episodic_tr("info.reference.title", lang = "en"),
    html,
    fixed = TRUE
  ))
  expect_true(grepl("EPISODIC_GEO_DATA", html, fixed = TRUE))
  expect_false(grepl("[[", html, fixed = TRUE))
})
