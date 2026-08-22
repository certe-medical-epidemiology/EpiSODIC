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

check_case <- function(
  source_key = "K1",
  patient_key = "P1",
  sample_date = "2025-01-01",
  pathogen = "Test pathogen",
  receipt_date = "2025-01-02"
) {
  data.frame(
    source_key = source_key,
    patient_key = patient_key,
    sample_date = sample_date,
    receipt_date = receipt_date,
    pathogen = pathogen,
    care_line = "second",
    institution_key = "HOSP-01",
    institution_display_name = "Hospital",
    institution_type = "hospital",
    municipality = NA_character_,
    ward = "ICU",
    specialism = "Interne",
    pc = "9711",
    sex = "M",
    age = 40L,
    stringsAsFactors = FALSE
  )
}

problems_of <- function(report) {
  report[report$severity == "problem", , drop = FALSE]
}

advice_of <- function(report) {
  report[report$severity == "advice", , drop = FALSE]
}

test_that("well-formed case data produces no problems at all", {
  report <- episodic_check_cases(check_case())
  expect_s3_class(report, "episodic_case_check")
  expect_equal(nrow(problems_of(report)), 0)
})

test_that("the report says what was read, so an operator can confirm it read what they think", {
  cases <- rbind(
    check_case("K1", "P1", "2025-01-01"),
    check_case("K2", "P2", "2025-03-31", pathogen = "Norovirus")
  )
  info <- attr(episodic_check_cases(cases), "summary")
  expect_identical(info$n_rows, 2L)
  expect_identical(info$n_pathogens, 2L)
  expect_identical(info$n_patients, 2L)
  expect_identical(info$n_institutions, 1L)
  expect_identical(info$date_min, as.Date("2025-01-01"))
  expect_identical(info$date_max, as.Date("2025-03-31"))
})

test_that("every problem is reported at once, not just the first", {
  # The whole point of the report: an operator fixing an extract should
  # learn everything that is wrong in one pass, not one problem per run.
  cases <- check_case("K1", "P1", "01-01-2025")
  cases$sex <- "male"
  cases$institution_type <- "clinic"
  cases$patient_key <- NA_character_
  report <- episodic_check_cases(cases)
  expect_setequal(
    problems_of(report)$column,
    c("patient_key", "institution_type", "sex", "sample_date")
  )
})

test_that("a missing column is named, and so is an unexpected one", {
  cases <- check_case()
  cases$pc <- NULL
  cases$postcode <- "9711"
  report <- problems_of(episodic_check_cases(cases))
  expect_true("missing_column" %in% report$issue)
  expect_true("unexpected_column" %in% report$issue)
  # and the likely rename is named too, rather than left to be guessed
  expect_match(
    report$fix[report$issue == "unexpected_column"],
    "pc",
    fixed = TRUE
  )
})

test_that("a duplicate source_key names the rows it is in", {
  cases <- rbind(check_case("K1", "P1"), check_case("K1", "P2"))
  report <- problems_of(episodic_check_cases(cases))
  expect_identical(report$issue, "duplicate_source_key")
  expect_identical(report$n_rows, 2L)
  expect_match(report$rows, "1, 2", fixed = TRUE)
})

test_that("an empty value in a column that must be filled is a problem, blank as well as NA", {
  for (value in list(NA_character_, "", "  ")) {
    cases <- check_case()
    cases$institution_key <- value
    report <- problems_of(episodic_check_cases(cases))
    expect_identical(report$issue, "empty_required_value")
    expect_identical(report$column, "institution_key")
  }
})

test_that("a value outside a fixed set is reported with the values that offended", {
  cases <- check_case()
  cases$sex <- "male"
  report <- problems_of(episodic_check_cases(cases))
  expect_identical(report$column, "sex")
  expect_match(report$values, "male", fixed = TRUE)
  expect_match(report$message, "M, F, U", fixed = TRUE)
})

test_that("a value that is right apart from its capitalisation says so", {
  cases <- check_case()
  cases$sex <- "m"
  report <- problems_of(episodic_check_cases(cases))
  expect_match(report$fix, "capitalisation", fixed = TRUE)
})

test_that("a day-first date is named for what it is, with the conversion to apply", {
  cases <- check_case("K1", "P1", "31-12-2025")
  report <- problems_of(episodic_check_cases(cases))
  expect_identical(report$column, "sample_date")
  expect_match(report$fix, "day-first", fixed = TRUE)
  expect_match(report$fix, "%d-%m-%Y", fixed = TRUE)
})

test_that("a date-time, an Excel serial and a YYYYMMDD date each get their own conversion", {
  hints <- c(
    "2025-01-01T00:00:00" = "time component",
    "45658" = "Excel",
    "20250101" = "YYYYMMDD"
  )
  for (value in names(hints)) {
    cases <- check_case("K1", "P1", value)
    report <- problems_of(episodic_check_cases(cases))
    expect_match(report$fix, hints[[value]], fixed = TRUE)
  }
})

test_that("a real Date column and an ISO string are both accepted", {
  cases <- check_case()
  cases$sample_date <- as.Date(cases$sample_date)
  cases$receipt_date <- as.Date(cases$receipt_date)
  expect_equal(nrow(problems_of(episodic_check_cases(cases))), 0)
})

test_that("an age given as an age group is a problem, not silently dropped", {
  cases <- check_case()
  cases$age <- "40-49"
  report <- problems_of(episodic_check_cases(cases))
  expect_identical(report$issue, "age_not_numeric")
})

test_that("something that is not a data set is reported rather than thrown", {
  report <- episodic_check_cases(1)
  expect_identical(problems_of(report)$issue, "not_a_data_set")
  report <- episodic_check_cases(function() 1)
  expect_identical(problems_of(report)$issue, "not_a_data_set")
})

test_that("a function returning the data set is checked like the data set itself", {
  cases <- check_case()
  expect_equal(
    nrow(problems_of(episodic_check_cases(function() cases))),
    0
  )
})

test_that("one pathogen spelled two ways is advised about, since it becomes two streams", {
  cases <- rbind(
    check_case("K1", "P1", "2025-01-01", pathogen = "Influenza A"),
    check_case("K2", "P2", "2025-01-02", pathogen = "influenza a")
  )
  advice <- advice_of(episodic_check_cases(cases))
  expect_true("pathogen_spelling_variants" %in% advice$issue)
})

test_that("a patient_key that never repeats is advised about, since deduplication cannot work", {
  cases <- do.call(
    rbind,
    lapply(seq_len(25), function(i) {
      check_case(paste0("K", i), paste0("P", i), "2025-01-01")
    })
  )
  advice <- advice_of(episodic_check_cases(cases))
  expect_true("patient_key_unique_per_row" %in% advice$issue)
})

test_that("a handful of rows, each its own patient, is not advised about - that is ordinary for a rare pathogen", {
  cases <- rbind(
    check_case("K1", "P1", "2025-01-01"),
    check_case("K2", "P2", "2025-01-02")
  )
  advice <- advice_of(episodic_check_cases(cases))
  expect_false("patient_key_unique_per_row" %in% advice$issue)
})

test_that("a patient_key that is simply the sample identifier is advised about however few rows there are", {
  cases <- check_case("K1", "K1", "2025-01-01")
  advice <- advice_of(episodic_check_cases(cases))
  expect_true("patient_key_unique_per_row" %in% advice$issue)
  expect_match(
    advice$message[advice$issue == "patient_key_unique_per_row"],
    "source_key",
    fixed = TRUE
  )
})

test_that("hospital rows without a ward are advised about, since L1 detection has nothing to group on", {
  cases <- check_case()
  cases$ward <- NA_character_
  advice <- advice_of(episodic_check_cases(cases))
  expect_true("ward_never_filled" %in% advice$issue)
})

test_that("a sample date in the future is advice, not a problem", {
  cases <- check_case("K1", "P1", as.character(Sys.Date() + 30))
  report <- episodic_check_cases(cases)
  expect_equal(nrow(problems_of(report)), 0)
  expect_true("sample_date_in_future" %in% advice_of(report)$issue)
})

test_that("a postcode the shipped map cannot place is advice, not a problem", {
  cases <- check_case()
  cases$pc <- "9713 AB"
  report <- episodic_check_cases(cases)
  expect_equal(nrow(problems_of(report)), 0)
  expect_true("pc_not_four_digits" %in% advice_of(report)$issue)
})

test_that("an institution keyed to two names is advised about", {
  cases <- rbind(check_case("K1", "P1"), check_case("K2", "P2"))
  cases$institution_display_name[2] <- "Hospital (new name)"
  advice <- advice_of(episodic_check_cases(cases))
  expect_true("institution_inconsistent" %in% advice$issue)
})

test_that("an empty extract is advice, since it is legitimate but rarely intended", {
  report <- episodic_check_cases(check_case()[0, ])
  expect_equal(nrow(problems_of(report)), 0)
  expect_true("no_rows" %in% advice_of(report)$issue)
})

test_that("factor columns are advised about, since read.csv is where they come from", {
  cases <- check_case()
  cases$pathogen <- as.factor(cases$pathogen)
  advice <- advice_of(episodic_check_cases(cases))
  expect_true("factor_column" %in% advice$issue)
})

test_that("the report prints, and says whether the data can be used", {
  expect_output(print(episodic_check_cases(check_case())), "ready")
  cases <- check_case()
  cases$sex <- "male"
  expect_output(print(episodic_check_cases(cases)), "problem")
})

test_that("episodic_validate_cases() reports every problem in one error, not only the first", {
  cases <- check_case("K1", "P1", "01-01-2025")
  cases$sex <- "male"
  expect_error(episodic_validate_cases(cases), "sample_date", fixed = TRUE)
  expect_error(episodic_validate_cases(cases), "sex", fixed = TRUE)
  expect_error(episodic_validate_cases(cases), "2 problems", fixed = TRUE)
})

test_that("episodic_validate_cases() stays silent about advice, which is not its job", {
  cases <- check_case()
  cases$ward <- NA_character_
  expect_silent(episodic_validate_cases(cases))
})
