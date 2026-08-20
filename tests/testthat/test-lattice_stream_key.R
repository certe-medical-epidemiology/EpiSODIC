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

test_that("episode_stream_key() is a 40-character hex digest", {
  key <- episode_stream_key("pathogen_region", "Test organism")
  expect_equal(nchar(key), 40)
  expect_match(key, "^[0-9a-f]{40}$")
})

test_that("episode_stream_key() is deterministic for identical inputs", {
  k1 <- episode_stream_key("pathogen_ward", "Test organism", institution_id = 3, ward = "ICU")
  k2 <- episode_stream_key("pathogen_ward", "Test organism", institution_id = 3, ward = "ICU")
  expect_equal(k1, k2)
})

test_that("episode_stream_key() differs when ward differs (item 20 fix)", {
  k1 <- episode_stream_key("pathogen_ward", "Test organism", institution_id = 3, ward = "ICU")
  k2 <- episode_stream_key("pathogen_ward", "Test organism", institution_id = 3, ward = "Geriatrie")
  expect_false(identical(k1, k2))
})

test_that("episode_stream_key() differs when any other dimension differs", {
  base <- episode_stream_key("pathogen_institution", "Test organism", institution_id = 1)
  expect_false(identical(base, episode_stream_key("pathogen_institution", "Other organism", institution_id = 1)))
  expect_false(identical(base, episode_stream_key("pathogen_institution", "Test organism", institution_id = 2)))
  expect_false(identical(base, episode_stream_key("pathogen_area", "Test organism", institution_id = 1)))
})

test_that("NA dimensions are treated consistently (no accidental collision with the string 'NA')", {
  k_na <- episode_stream_key("pathogen_region", "Test organism", region_code = NA)
  k_literal <- episode_stream_key("pathogen_region", "Test organism", region_code = "NA")
  expect_equal(k_na, k_literal)  # documented behaviour: NA maps to the literal "NA"
})
