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

# A run log is only useful if a line that says a component contributed
# nothing looks different from the forty lines of progress around it,
# and if every line remains one timestamped, greppable line whichever
# severity it carries.

test_that("every severity keeps the timestamp, the text and one line", {
  for (severity in c("plain", "warn", "danger")) {
    line <- capture_messages(
      episodic_trace("Farrington fitted nowhere", severity = severity)
    )
    expect_length(line, 1L)
    expect_true(grepl("Farrington fitted nowhere", line, fixed = TRUE))
    expect_true(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", line))
    expect_length(strsplit(trimws(line), "\n", fixed = TRUE)[[1]], 1L)
  }
})

test_that("a severe line is marked, and a plain one is not", {
  plain <- capture_messages(episodic_trace("nothing to see"))
  warned <- capture_messages(
    episodic_trace("nothing to see", severity = "warn")
  )
  danger <- capture_messages(
    episodic_trace("nothing to see", severity = "danger")
  )
  # The timestamp differs between calls, so it is the part before it
  # that says whether a line was marked at all.
  prefix <- function(x) sub("[0-9]{4}-[0-9]{2}-[0-9]{2}.*$", "", x)
  expect_equal(prefix(plain), "")
  expect_true(nzchar(prefix(warned)))
  expect_true(nzchar(prefix(danger)))
  expect_false(identical(prefix(warned), prefix(danger)))
})

test_that("a severe line stays one line however long it is", {
  long <- paste(rep("a stream that could not be tested", 20), collapse = ", ")
  line <- capture_messages(episodic_trace(long, severity = "warn"))
  expect_true(grepl(long, line, fixed = TRUE))
  expect_length(strsplit(trimws(line), "\n", fixed = TRUE)[[1]], 1L)
})

test_that("a trace line is text, not cli markup", {
  # Configuration paths, error messages and postcode examples reach the
  # log verbatim; a brace in one of them is not an interpolation.
  line <- capture_messages(episodic_trace(
    "EPISODIC_CONFIG is '/etc/{instance}/episodic.yaml'",
    severity = "danger"
  ))
  expect_true(grepl("{instance}", line, fixed = TRUE))
})

test_that("severity is a closed set", {
  expect_error(episodic_trace("something", severity = "critical"))
})

test_that("the debug trace carries a severity through", {
  line <- capture_messages(
    episodic_trace_debug(TRUE, "debug: something", severity = "warn")
  )
  expect_true(grepl("debug: something", line, fixed = TRUE))
  expect_false(grepl("^[0-9]", line))
  expect_silent(
    episodic_trace_debug(FALSE, "debug: something", severity = "warn")
  )
})

test_that("a severe line keeps the rows the caller wrote", {
  # A validation report names one problem per line, and the mark belongs
  # on the row carrying the timestamp, not around the whole report.
  line <- capture_messages(episodic_trace(
    "Pre-run checks failed: 2 problems.",
    "\n  1. `sex` has a value outside the allowed set.",
    "\n  2. `pc` is empty on 12 rows.",
    severity = "danger"
  ))
  rows <- strsplit(trimws(line), "\n", fixed = TRUE)[[1]]
  expect_length(rows, 3L)
  expect_true(grepl("Pre-run checks failed", rows[1], fixed = TRUE))
  expect_true(grepl("`pc` is empty on 12 rows.", rows[3], fixed = TRUE))
})
