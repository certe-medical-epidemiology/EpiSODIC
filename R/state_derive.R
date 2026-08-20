#' Derive cluster state
#'
#' State is computed, never chosen, never stored on `episode_cluster`
#' (ARCHITECTURE.md section 6.1). This is a
#' pure function: given the classification history and the case-free clock,
#' it returns the same state every time, with no side effects and no
#' database access, so it can be exhaustively unit tested - a wrong
#' output here is undetectable until it actually matters.
#'
#' State table (ARCHITECTURE.md section 6.1):
#'
#' | State | Condition |
#' |---|---|
#' | Nieuw | No assessment event exists |
#' | In beoordeling | An event exists but no classification yet, or snoozed |
#' | Monitoring | Classified as an open epidemic verdict, not yet case-free |
#' | Af te sluiten | Epidemic verdict and the closure criterion has fired |
#' | Afgesloten | Classified artefact/normal variation, or explicitly closed |
#' | Herbeoordeling nodig | Classified, but data changed since that assessment |
#'
#' @param events A data frame of this cluster's assessment events, ordered
#'   ascending by `created_at`/`event_id` (as returned by
#'   `episode_db_assessment_events()`). May have zero rows.
#' @param changed_since_assessment Logical, from `episode_cluster`.
#' @param closure_criterion_met Logical, whether the case-free/MEM closure
#'   criterion has fired for this cluster's stream (see
#'   `R/reconcile_closure.R`). Ignored unless the latest verdict is a
#'   non-terminal epidemic verdict.
#' @param explicitly_closed Logical, `TRUE` if a person closed this cluster
#'   as an act distinct from any classification (ARCHITECTURE.md section
#'   6.1: "Closure is an act, not a classification") - represented as an
#'   `episode_cluster_state` row with `trigger = "closure"` (a person) or
#'   `"system"` (cron auto-close), not as a new assessment event; see
#'   `episode_app_explicitly_closed()`. Checked even when `events` has zero
#'   rows: a cluster the cron auto-closed without anyone ever assessing it
#'   still has no assessment events (ARCHITECTURE.md section 6, step 5).
#' @param today The current date, for evaluating `snooze_until`.
#' @return One of `"new"`, `"assessing"`, `"monitoring"`, `"closable"`,
#'   `"closed"`, `"reassess"`.
#' @keywords internal
#' @noRd
episode_derive_state <- function(events, changed_since_assessment = FALSE,
                                  closure_criterion_met = FALSE,
                                  explicitly_closed = FALSE, today = Sys.Date()) {
  if (nrow(events) == 0) {
    # A cluster the cron auto-closed without anyone ever assessing it
    # (ARCHITECTURE.md section 6, step 5: "no assessment exists" is itself
    # eligible for auto-closure) still has zero assessment events - so
    # explicitly_closed must be checked even here, or such a cluster would
    # read as "new" forever and never leave the open rail.
    return(if (isTRUE(explicitly_closed)) "closed" else "new")
  }

  latest <- events[nrow(events), ]

  if (explicitly_closed) {
    return("closed")
  }

  if (is.na(latest$verdict)) {
    return("assessing")
  }

  snoozed <- !is.na(latest$snooze_until) && as.Date(latest$snooze_until) >= as.Date(today)
  if (snoozed) {
    return("assessing")
  }

  terminal_verdicts <- c("artefact", "expected_variation")
  if (latest$verdict %in% terminal_verdicts) {
    # ARCHITECTURE.md section 6.5's cool-down escape hatch: a stream closed
    # as artefact/normal variation that later re-exceeds what that verdict
    # was based on must not stay silently "closed" - reconciliation flags
    # this via the same changed_since_assessment column it already uses
    # for non-terminal verdicts (R/reconcile.R), so the only change needed
    # here is to stop treating a terminal verdict as unconditionally final.
    if (isTRUE(changed_since_assessment)) {
      return("reassess")
    }
    return("closed")
  }

  if (isTRUE(changed_since_assessment)) {
    return("reassess")
  }

  # non-terminal verdict: cluster_not_yet, possible_epidemic, confirmed_epidemic
  if (isTRUE(closure_criterion_met)) {
    return("closable")
  }

  "monitoring"
}
