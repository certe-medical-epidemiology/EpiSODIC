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

# Dutch number agreement: "1 geval" against "2 gevallen".

test_that("episodic_count_phrase() uses the singular for exactly 1", {
  expect_equal(episodic_count_phrase(1, "geval", "gevallen"), "1 geval")
  expect_equal(episodic_count_phrase(1, "isolaat", "isolaten"), "1 isolaat")
  expect_equal(episodic_count_phrase(1, "case", "cases"), "1 case")
})

test_that("episodic_count_phrase() uses the plural for 0 and for >1", {
  expect_equal(episodic_count_phrase(0, "geval", "gevallen"), "0 gevallen")
  expect_equal(episodic_count_phrase(2, "geval", "gevallen"), "2 gevallen")
  expect_equal(episodic_count_phrase(11, "geval", "gevallen"), "11 gevallen")
})

test_that("episodic_count_phrase() can omit the leading number", {
  expect_equal(
    episodic_count_phrase(1, "geval", "gevallen", with_number = FALSE),
    "geval"
  )
  expect_equal(
    episodic_count_phrase(5, "geval", "gevallen", with_number = FALSE),
    "gevallen"
  )
})

test_that("episodic_count_phrase() is exhaustively correct across a range of counts", {
  for (n in 0:20) {
    result <- episodic_count_phrase(n, "stream", "streams")
    expected_word <- if (n == 1) "stream" else "streams"
    expect_equal(result, paste(n, expected_word), info = paste("n =", n))
  }
})
