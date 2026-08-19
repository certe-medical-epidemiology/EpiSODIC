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
