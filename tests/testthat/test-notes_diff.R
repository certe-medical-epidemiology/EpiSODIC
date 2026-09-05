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

test_that("episodic_notes_diff_tokenize() splits on word/whitespace boundaries and round-trips", {
  expect_equal(episodic_notes_diff_tokenize(""), character(0))
  tokens <- episodic_notes_diff_tokenize("the  quick\nfox")
  expect_equal(tokens, c("the", "  ", "quick", "\n", "fox"))
  # re-concatenating every token must reproduce the original text
  # byte-for-byte - that is what lets the diff view keep the note's exact
  # original line breaks
  expect_equal(paste(tokens, collapse = ""), "the  quick\nfox")
})

test_that("episodic_notes_diff_tokens() marks identical text as entirely unchanged", {
  ops <- episodic_notes_diff_tokens("hello world", "hello world")
  expect_true(all(vapply(ops, function(x) x$type, character(1)) == "eq"))
  expect_equal(paste(vapply(ops, function(x) x$text, character(1)), collapse = ""), "hello world")
})

test_that("episodic_notes_diff_tokens() marks an empty old version as entirely inserted", {
  ops <- episodic_notes_diff_tokens("", "brand new note")
  expect_true(all(vapply(ops, function(x) x$type, character(1)) == "ins"))
  expect_equal(paste(vapply(ops, function(x) x$text, character(1)), collapse = ""), "brand new note")
})

test_that("episodic_notes_diff_tokens() marks a cleared note as entirely deleted", {
  ops <- episodic_notes_diff_tokens("old note text", "")
  expect_true(all(vapply(ops, function(x) x$type, character(1)) == "del"))
  expect_equal(paste(vapply(ops, function(x) x$text, character(1)), collapse = ""), "old note text")
})

test_that("episodic_notes_diff_tokens() finds the minimal word-level edit for an insertion", {
  ops <- episodic_notes_diff_tokens("the quick fox", "the quick brown fox")
  types <- vapply(ops, function(x) x$type, character(1))
  texts <- vapply(ops, function(x) x$text, character(1))
  expect_equal(paste(texts, collapse = ""), "the quick brown fox")
  # only "brown " is new; everything else, including the shared "fox", is
  # unchanged - a line-level diff would instead have marked the whole line
  # (and possibly the next one too) as changed
  expect_equal(types, c("eq", "eq", "eq", "eq", "ins", "ins", "eq"))
  expect_equal(texts, c("the", " ", "quick", " ", "brown", " ", "fox"))
})

test_that("episodic_notes_diff_html() escapes tokens and highlights insertions/deletions", {
  html <- as.character(episodic_notes_diff_html("old <b>text</b>", "new <b>text</b>"))
  # embedded markup in either version must never reach the page as live HTML
  expect_false(grepl("<b>", html, fixed = TRUE))
  expect_true(grepl("&lt;b&gt;", html, fixed = TRUE))
  expect_true(grepl('class="episodic-notes-diff-del"', html, fixed = TRUE))
  expect_true(grepl('class="episodic-notes-diff-ins"', html, fixed = TRUE))
})

test_that("episodic_notes_diff_tokens() falls back to coarse del+ins above the size cap", {
  old_words <- paste(rep("aaa", 3000), collapse = " ")
  new_words <- paste(rep("bbb", 3000), collapse = " ")
  # 3000*3000 word/whitespace token pairs comfortably exceeds
  # episodic_notes_diff_cap (4e6) once whitespace tokens are counted too
  ops <- episodic_notes_diff_tokens(old_words, new_words)
  types <- unique(vapply(ops, function(x) x$type, character(1)))
  expect_true(all(types %in% c("del", "ins")))
})
