# Dutch number agreement, MILESTONES.md M2's explicit watch-item:
# "1 geval" against "2 gevallen".

test_that("episode_count_phrase() uses the singular for exactly 1", {
  expect_equal(episode_count_phrase(1, "geval", "gevallen"), "1 geval")
  expect_equal(episode_count_phrase(1, "isolaat", "isolaten"), "1 isolaat")
  expect_equal(episode_count_phrase(1, "case", "cases"), "1 case")
})

test_that("episode_count_phrase() uses the plural for 0 and for >1", {
  expect_equal(episode_count_phrase(0, "geval", "gevallen"), "0 gevallen")
  expect_equal(episode_count_phrase(2, "geval", "gevallen"), "2 gevallen")
  expect_equal(episode_count_phrase(11, "geval", "gevallen"), "11 gevallen")
})

test_that("episode_count_phrase() can omit the leading number", {
  expect_equal(episode_count_phrase(1, "geval", "gevallen", with_number = FALSE), "geval")
  expect_equal(episode_count_phrase(5, "geval", "gevallen", with_number = FALSE), "gevallen")
})

test_that("episode_count_phrase() is exhaustively correct across a range of counts", {
  for (n in 0:20) {
    result <- episode_count_phrase(n, "stream", "streams")
    expected_word <- if (n == 1) "stream" else "streams"
    expect_equal(result, paste(n, expected_word), info = paste("n =", n))
  }
})
