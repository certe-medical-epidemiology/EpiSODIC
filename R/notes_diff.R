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

#' Tokenize free text into words and whitespace runs, for a word-level diff
#'
#' Splitting on `\S+|\s+` (rather than plain whitespace) keeps every run of
#' whitespace - including newlines - as its own token, so re-concatenating
#' the tokens byte-for-byte reproduces the original text. That is what
#' lets `episodic_notes_diff_html()` render a note's exact original line
#' breaks (via `white-space: pre-wrap` in `episodic.css`) instead of a
#' word-wrapped approximation of them.
#' @param text A single string.
#' @return A character vector of tokens.
#' @keywords internal
#' @noRd
episodic_notes_diff_tokenize <- function(text) {
  if (!nzchar(text)) {
    return(character(0))
  }
  regmatches(text, gregexpr("\\S+|\\s+", text))[[1]]
}

#' Above this many token pairs, skip the LCS backtrack and diff coarsely
#'
#' The DP table `episodic_notes_diff_tokens()` builds is O(n*m) in both
#' time and memory. Two short notes never come close to this; two very
#' long, entirely rewritten notes could otherwise allocate a table with
#' tens of millions of cells. Past the cap, the whole change is rendered
#' as one deletion followed by one insertion instead of a word-by-word
#' match - still a correct diff, just a coarser one.
#' @keywords internal
#' @noRd
episodic_notes_diff_cap <- 4e6

#' A word-level diff between two note versions
#'
#' Classic LCS backtrack over whitespace-preserving tokens (see
#' `episodic_notes_diff_tokenize()`) - the same algorithm line-based
#' `diff`/`git diff` use, applied to words instead of lines, since a free-
#' text note is rarely broken into meaningful lines the way source code
#' is.
#' @param old,new The previous and current note text.
#' @return A list of `list(type = "eq"|"ins"|"del", text = <token>)`, in
#'   order from the start of `new` to its end.
#' @keywords internal
#' @noRd
episodic_notes_diff_tokens <- function(old, new) {
  a <- episodic_notes_diff_tokenize(old)
  b <- episodic_notes_diff_tokenize(new)
  n <- length(a)
  m <- length(b)

  if (n == 0 && m == 0) {
    return(list())
  }
  if (as.numeric(n) * as.numeric(m) > episodic_notes_diff_cap) {
    return(c(
      lapply(a, function(x) list(type = "del", text = x)),
      lapply(b, function(x) list(type = "ins", text = x))
    ))
  }

  dp <- matrix(0L, nrow = n + 1, ncol = m + 1)
  if (n > 0 && m > 0) {
    for (i in n:1) {
      for (j in m:1) {
        dp[i, j] <- if (identical(a[i], b[j])) {
          dp[i + 1, j + 1] + 1L
        } else {
          max(dp[i + 1, j], dp[i, j + 1])
        }
      }
    }
  }

  out <- vector("list", n + m)
  n_out <- 0L
  i <- 1L
  j <- 1L
  while (i <= n && j <= m) {
    n_out <- n_out + 1L
    if (identical(a[i], b[j])) {
      out[[n_out]] <- list(type = "eq", text = a[i])
      i <- i + 1L
      j <- j + 1L
    } else if (dp[i + 1, j] >= dp[i, j + 1]) {
      out[[n_out]] <- list(type = "del", text = a[i])
      i <- i + 1L
    } else {
      out[[n_out]] <- list(type = "ins", text = b[j])
      j <- j + 1L
    }
  }
  while (i <= n) {
    n_out <- n_out + 1L
    out[[n_out]] <- list(type = "del", text = a[i])
    i <- i + 1L
  }
  while (j <= m) {
    n_out <- n_out + 1L
    out[[n_out]] <- list(type = "ins", text = b[j])
    j <- j + 1L
  }
  out[seq_len(n_out)]
}

#' Render a word-level diff as highlighted HTML
#'
#' Escapes every token before wrapping it in its `<span>` - `old`/`new` are
#' user-authored free text, the same trust boundary
#' `episodic_ui_render_markdown()` documents for the live note, so nothing
#' typed into a note can inject markup into the history modal. Unlike the
#' live note, the diff is not run through `commonmark::markdown_html()`:
#' markdown syntax can span several tokens (`**bold**`), so highlighting
#' individual changed words against *rendered* markdown would routinely
#' split a construct in two. Showing the raw note source instead is also
#' the more honest "what actually changed" view for a change history.
#' @param old,new The previous and current note text.
#' @return A `shiny::HTML()` value.
#' @keywords internal
#' @noRd
episodic_notes_diff_html <- function(old, new) {
  ops <- episodic_notes_diff_tokens(old, new)
  if (length(ops) == 0) {
    return(shiny::HTML(""))
  }
  pieces <- vapply(
    ops,
    function(op) {
      text <- htmltools::htmlEscape(op$text)
      switch(
        op$type,
        eq = text,
        ins = paste0('<span class="episodic-notes-diff-ins">', text, "</span>"),
        del = paste0('<span class="episodic-notes-diff-del">', text, "</span>")
      )
    },
    character(1)
  )
  shiny::HTML(paste(pieces, collapse = ""))
}
