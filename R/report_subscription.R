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

#' How scheduled reports work
#'
#' A scheduled report is a standing subscription on one cluster: every
#' `interval_days`, [episodic_run_cron()] emails the outbreak report -
#' the same self-contained HTML [episodic_report_render()] produces on
#' demand - to a list of addresses, as an attachment. It exists for
#' people who need to follow a cluster without an EpiSODIC account, or
#' with only a `viewer` one: a ward manager, a department head, a
#' referring physician. This is deliberately not the same thing as
#' [episodic_notifications]: those alert *epidemiologists* that
#' something needs assessing; a scheduled report keeps *everyone else*
#' informed once it is being watched, on a cadence they chose, without
#' needing to open the dashboard at all.
#'
#' An `epidemiologist` sets a schedule from the cluster's Reports panel:
#' an interval in days, one or more recipient addresses, which
#' already-configured, email-capable notification channel to send
#' through (`smtp`, `sendmail` or `microsoft365` - see
#' [episodic_notifications]), and whether to include the case line list
#' (excluded by default, since these recipients are often outside the
#' organisation - see `vignette("deployment")` for how a custom Quarto
#' template can include it anyway). Every setting is stored as an
#' event, the same event-sourced shape as a cluster note: setting a new
#' schedule (even just changing the recipient list) starts its cadence
#' over, and the very next cron run sends the first report under the new
#' settings so the change is confirmed immediately rather than silently
#' taking effect days later.
#'
#' Each cron run checks every cluster's current schedule and sends
#' whichever are due - due the first time under a schedule's current
#' settings, or `interval_days` after the last *successful* send under
#' those settings. A failed send (a broken SMTP relay, say) does not
#' postpone the next attempt: the very next cron run tries again, and
#' every attempt, sent or failed, is logged to
#' `episodic_report_subscription_send`. A schedule stops automatically,
#' after one final report, the run a cluster closes, merges, or is
#' suppressed - further updates about an inactive cluster would be noise
#' rather than news. An epidemiologist can also cancel a schedule
#' outright at any time.
#'
#' @name episodic_scheduled_reports
NULL

#' Whether a string looks like a valid email address
#'
#' Deliberately permissive - this rejects only the unambiguous
#' non-addresses (no "@", no "." after it), never a real address that
#' happens to use an unusual TLD or a "+" alias. The border case of a
#' technically-invalid-but-deliverable address is one for the sending
#' mail server to judge, not this package.
#' @param x A character vector.
#' @return A logical vector, the same length as `x`.
#' @keywords internal
#' @noRd
episodic_valid_email <- function(x) {
  grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", x, perl = TRUE)
}

#' Parse a recipient list typed into the Reports panel's form
#'
#' Splits on comma, semicolon or newline - whichever an epidemiologist
#' happens to type between addresses - trims whitespace, drops empty
#' entries, and separates the valid addresses from the invalid ones so
#' the form can report exactly which entries are the problem instead of
#' rejecting the whole list with no detail.
#' @param text Freeform recipient input, one or more addresses.
#' @return A list with `valid` (character vector, de-duplicated) and
#'   `invalid` (character vector of the raw entries that did not look
#'   like an email address).
#' @keywords internal
#' @noRd
episodic_report_subscription_parse_recipients <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text %||% ""))) {
    return(list(valid = character(0), invalid = character(0)))
  }
  parts <- strsplit(text, "[,;\n]+")[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  ok <- episodic_valid_email(parts)
  list(
    valid = unique(parts[ok]),
    invalid = parts[!ok]
  )
}

#' @keywords internal
#' @noRd
episodic_report_subscription_recipients_to_json <- function(emails) {
  as.character(jsonlite::toJSON(as.character(emails)))
}

#' @keywords internal
#' @noRd
episodic_report_subscription_recipients_from_json <- function(json) {
  if (is.null(json) || is.na(json) || !nzchar(json)) {
    return(character(0))
  }
  as.character(jsonlite::fromJSON(json))
}

#' Reduce a subscription event history to "the current schedule per cluster"
#'
#' The current schedule for a cluster is its latest event; a latest event
#' of `"cancel"` (or no events at all) means the cluster has none. Pure
#' and side-effect-free by design, like `episodic_derive_state()`, so the
#' reduction itself is exhaustively unit-testable without a database.
#' @param events A data frame as returned by
#'   `episodic_db_report_subscription_events_all()` (or
#'   `episodic_db_report_subscription_events()` for one cluster),
#'   ordered by `cluster_id`, `created_at`, `event_id` ascending.
#' @return A named list, keyed by `cluster_id` (as character), of lists
#'   with `cluster_id`, `event_id`, `user_id`, `set_at`, `interval_days`,
#'   `recipients` (character vector), `channel`, `include_linelist`.
#'   Clusters with no active schedule are simply absent.
#' @keywords internal
#' @noRd
episodic_report_subscription_current_all <- function(events) {
  if (nrow(events) == 0) {
    return(list())
  }
  latest <- events[!duplicated(events$cluster_id, fromLast = TRUE), ]
  latest <- latest[latest$action == "set", , drop = FALSE]
  if (nrow(latest) == 0) {
    return(list())
  }
  out <- lapply(seq_len(nrow(latest)), function(i) {
    row <- latest[i, ]
    list(
      cluster_id = as.integer(row$cluster_id),
      event_id = as.integer(row$event_id),
      user_id = as.integer(row$user_id),
      set_at = row$created_at,
      interval_days = as.integer(row$interval_days),
      recipients = episodic_report_subscription_recipients_from_json(row$recipients),
      channel = row$channel,
      include_linelist = as.logical(row$include_linelist)
    )
  })
  stats::setNames(out, as.character(latest$cluster_id))
}

#' The current scheduled-report subscription for one cluster, if any
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return A list as described in
#'   `episodic_report_subscription_current_all()`, or `NULL` if the
#'   cluster has no active schedule.
#' @keywords internal
#' @noRd
episodic_report_subscription_current <- function(con, cluster_id) {
  events <- episodic_db_report_subscription_events(con, cluster_id)
  current <- episodic_report_subscription_current_all(events)
  current[[as.character(cluster_id)]]
}

#' Whether a cluster's current schedule is due to send today
#'
#' Due the first time under a schedule's current settings (no successful
#' send yet logged against its `event_id`), or `interval_days` after the
#' last *successful* one - a failed attempt does not push the next try
#' back, so a transient SMTP outage costs at most one missed day, not a
#' full interval. Pure and side-effect-free, taking already-fetched data
#' rather than a connection, so the cadence logic itself is unit-testable
#' without a database.
#' @param subscription One entry from
#'   `episodic_report_subscription_current_all()`.
#' @param sends A data frame as returned by
#'   `episodic_db_report_subscription_sends_all()` (or
#'   `episodic_db_report_subscription_sends()` for one cluster).
#' @param run_date The date to judge against (the cron run's own date).
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_report_subscription_due <- function(subscription, sends, run_date = Sys.Date()) {
  successful <- sends[
    sends$subscription_event_id == subscription$event_id &
      sends$status == "sent", ,
    drop = FALSE
  ]
  if (nrow(successful) == 0) {
    return(TRUE)
  }
  last_sent <- max(as.Date(successful$sent_at))
  as.numeric(as.Date(run_date) - last_sent) >= subscription$interval_days
}
