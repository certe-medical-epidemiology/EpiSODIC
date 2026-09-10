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

#' Derive cluster state
#'
#' State is computed, never chosen, never stored on `episodic_cluster`.
#' This is a pure function: given the classification history and the
#' case-free clock, it returns the same state every time, with no side
#' effects and no database access, so it can be exhaustively unit tested
#' - a wrong output here is undetectable until it actually matters.
#'
#' State table:
#'
#' | State | Condition |
#' |---|---|
#' | New | No assessment event exists |
#' | Assessing | An event exists but no classification yet, or snoozed |
#' | Monitoring | Classified, not closed, not stale |
#' | Closed | Explicitly closed by a person or by the cron's own
#'   unassessed-and-stale rule - never implied by a verdict alone |
#' | Reassessment needed | New cases have arrived since the last
#'   assessment or closure |
#'
#' No verdict, including `artefact`/`expected_variation`, ever closes a
#' cluster by itself - closure is always a deliberate, separate act (see
#' `episodic_app_submit_closure()`), whether taken by a person or by the
#' cron's stale-and-unassessed rule. Five states, not six: a "closable"
#' state - a non-terminal verdict whose closure criterion has fired -
#' only means something where closure is automatic, and here it is
#' deliberate.
#'
#' @param events A data frame of this cluster's assessment events, ordered
#'   ascending by `created_at`/`event_id` (as returned by
#'   `episodic_db_assessment_events()`). May have zero rows.
#' @param changed_since_assessment Logical, from `episodic_cluster`. Takes
#'   priority over `explicitly_closed`: a cluster a person (or the cron)
#'   closed still needs a fresh look the moment new cases arrive on its
#'   stream, rather than staying silently `"closed"` forever.
#' @param explicitly_closed Logical, `TRUE` if a person closed this
#'   cluster as an act distinct from any classification - represented as
#'   an `episodic_cluster_state` row with `trigger = "closure"` (a person)
#'   or `"system"` (cron auto-close), not as a new assessment event; see
#'   `episodic_app_explicitly_closed()`. Checked even when `events` has
#'   zero rows: a cluster the cron auto-closed without anyone ever
#'   assessing it still has no assessment events.
#' @param today The current date, for evaluating `snooze_until`.
#' @return One of `"new"`, `"assessing"`, `"monitoring"`, `"closed"`,
#'   `"reassess"`.
#' @keywords internal
#' @noRd
episodic_derive_state <- function(events,
                                  changed_since_assessment = FALSE,
                                  explicitly_closed = FALSE,
                                  today = Sys.Date()) {
  if (nrow(events) == 0) {
    # A cluster the cron auto-closed without anyone ever assessing it
    # still has zero assessment events - so explicitly_closed must be
    # checked even here, or such a cluster would read as "new" forever
    # and never leave the open rail.
    if (!isTRUE(explicitly_closed)) {
      return("new")
    }
    return(if (isTRUE(changed_since_assessment)) "reassess" else "closed")
  }

  latest <- events[nrow(events), ]

  if (isTRUE(explicitly_closed)) {
    return(if (isTRUE(changed_since_assessment)) "reassess" else "closed")
  }

  if (is.na(latest$verdict)) {
    return("assessing")
  }

  snoozed <- !is.na(latest$snooze_until) &&
    as.Date(latest$snooze_until) >= as.Date(today)
  if (snoozed) {
    return("assessing")
  }

  if (isTRUE(changed_since_assessment)) {
    return("reassess")
  }

  # Every verdict, terminal or not, is "monitoring" - i.e. classified,
  # live, awaiting a deliberate closure decision - until someone actually
  # closes it.
  "monitoring"
}
