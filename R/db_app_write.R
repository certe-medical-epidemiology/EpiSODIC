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

# App-side writers: the app owns the judgements and only ever inserts,
# never updates and never deletes. This is what makes the whole
# concurrency question disappear: two people assessing the same cluster
# in the same minute produce two rows, both visible in the timeline.
# Nothing in this file contains an UPDATE or a DELETE statement; that
# absence is load-bearing and should be verified by inspection whenever
# this file changes. Parameters throughout are one row's worth of
# columns for the table each function name identifies - see
# inst/sql/schema.sql for the exact column requirements (nullability,
# enums, defaults).

# Every function here binds its parameters through a `params` local built
# immediately before the `DBI` call, never as an inline
# `params = list(...)` argument. That is deliberate and load-bearing, not
# a style preference. An R argument is a promise: written inline, the
# list is not evaluated at the call site but inside `dbExecute()`/
# `dbGetQuery()`, by which point the driver has already prepared a
# statement on `con`. If evaluating any element then queries that same
# connection - which a caller-supplied argument can do without this file
# knowing, and which `episodic_reconcile_stream()`'s `priority_score_fn`
# did - RMariaDB cancels and frees the prepared statement, and `dbBind()`
# binds into freed memory. That is a native crash: no R condition, no
# `tryCatch`, the session simply dies, and only ever against MariaDB
# (RSQLite permits concurrent results on one connection, so the same code
# is harmless there). Building the list first means every element is a
# plain value before any statement exists.

#' @keywords internal
#' @noRd
episodic_db_assessment_event_insert <- function(con,
                                                cluster_id,
                                                user_id,
                                                verdict = NA,
                                                rationale = "",
                                                wpg_notifiable = NA,
                                                ggd_informed = NA,
                                                ggd_note = NA,
                                                snooze_until = NA,
                                                supersedes = NA) {
  # Optional: `rationale` is free text, not a required justification - the
  # column stays NOT NULL (inst/sql/schema.sql), so a blank rationale is
  # stored as "" rather than NULL.
  rationale <- if (is.null(rationale) || is.na(rationale)) {
    ""
  } else {
    as.character(rationale)
  }
  params <- list(
    cluster_id,
    user_id,
    episodic_now(),
    verdict,
    rationale,
    if (is.na(wpg_notifiable)) NA else as.integer(wpg_notifiable),
    if (is.na(ggd_informed)) NA else as.integer(ggd_informed),
    ggd_note,
    snooze_until,
    supersedes
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_assessment_event
      (cluster_id, user_id, created_at, verdict, rationale, wpg_notifiable, ggd_informed,
       ggd_note, snooze_until, supersedes)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_note_insert <- function(con,
                                            cluster_id,
                                            user_id,
                                            note_text) {
  params <- list(
    cluster_id,
    user_id,
    episodic_now(),
    as.character(note_text)
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_cluster_note (cluster_id, user_id, created_at, note_text)
     VALUES (?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_stream_mute_insert <- function(con,
                                           stream_id,
                                           muted_from,
                                           muted_until,
                                           reason,
                                           note = NA,
                                           user_id) {
  params <- list(
    stream_id,
    muted_from,
    muted_until,
    reason,
    note,
    user_id,
    episodic_now()
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_stream_mute
      (stream_id, muted_from, muted_until, reason, note, user_id, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_state_insert <- function(con,
                                             cluster_id,
                                             state,
                                             trigger,
                                             event_id = NA,
                                             user_id = NA) {
  params <- list(cluster_id, state, episodic_now(), trigger, event_id, user_id)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_cluster_state (cluster_id, state, entered_at, `trigger`, event_id, user_id)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_report_render_insert <- function(con,
                                             cluster_id,
                                             user_id = NA,
                                             file_path,
                                             file_sha256,
                                             params_json,
                                             case_ids_json,
                                             version_no) {
  params <- list(
    cluster_id,
    user_id,
    episodic_now(),
    file_path,
    file_sha256,
    params_json,
    case_ids_json,
    version_no
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_report_render
      (cluster_id, user_id, rendered_at, file_path, file_sha256, params, case_ids, version_no)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' Set or cancel a cluster's scheduled-report subscription
#'
#' Event-sourced like every other app write: this always inserts, it
#' never updates the row a previous call wrote. The current schedule for
#' `cluster_id` is simply the latest row - see
#' `episodic_report_subscription_current()`.
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param user_id The epidemiologist setting or cancelling the schedule.
#' @param action `"set"` or `"cancel"`.
#' @param interval_days Send every this many days. `NA` when
#'   `action = "cancel"`.
#' @param recipients_json A JSON array of email addresses. `NA` when
#'   `action = "cancel"`.
#' @param channel Name of the `notifications.channels.*` entry to send
#'   through (`"smtp"`, `"sendmail"` or `"microsoft365"`). `NA` when
#'   `action = "cancel"`.
#' @param include_linelist Whether the emailed report includes the case
#'   line list. Ignored (stored as `FALSE`) when `action = "cancel"`.
#' @return The new `event_id`.
#' @keywords internal
#' @noRd
episodic_db_report_subscription_event_insert <- function(con,
                                                         cluster_id,
                                                         user_id,
                                                         action,
                                                         interval_days = NA,
                                                         recipients_json = NA,
                                                         channel = NA,
                                                         include_linelist = FALSE) {
  params <- list(
    cluster_id,
    user_id,
    episodic_now(),
    action,
    if (is.na(interval_days)) NA else as.integer(interval_days),
    recipients_json,
    channel,
    as.integer(isTRUE(include_linelist))
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_report_subscription_event
      (cluster_id, user_id, created_at, action, interval_days, recipients, channel, include_linelist)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_app_user_insert <- function(con,
                                        username,
                                        full_name,
                                        email,
                                        password_hash,
                                        role = "epidemiologist",
                                        is_admin = FALSE,
                                        must_change = TRUE) {
  params <- list(
    username,
    full_name,
    email,
    password_hash,
    role,
    as.integer(isTRUE(is_admin)),
    as.integer(isTRUE(must_change)),
    episodic_now()
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_app_user
      (username, full_name, email, password_hash, role, is_admin, is_active, must_change, created_at)
     VALUES (?, ?, ?, ?, ?, ?, 1, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' Record a sign-in attempt that did not succeed
#'
#' Append-only, like every other app-side write. Kept in its own table
#' rather than as an `episodic_app_user_event` type because a failure
#' may have no account behind it at all - a username nobody has - and
#' that table's `user_id` is `NOT NULL` by design.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param username The username as typed. Truncated here: it is
#'   whatever a visitor sent, and an audit row is not a place to store an
#'   unbounded string somebody else chose.
#' @param reason One of `"unknown_username"`, `"wrong_password"`,
#'   `"inactive_account"`.
#' @param user_id The account it names, or `NA` when it names none.
#' @return Invisibly, the new row's `failure_id`.
#' @keywords internal
#' @noRd
episodic_db_app_login_failure_insert <- function(con,
                                                 username,
                                                 reason,
                                                 user_id = NA) {
  params <- list(
    episodic_now(),
    substr(as.character(username %||% ""), 1, 191),
    if (is.na(user_id)) NA_integer_ else as.integer(user_id),
    reason
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_app_login_failure
      (attempted_at, username, user_id, reason)
     VALUES (?, ?, ?, ?)",
    params = params
  )
  invisible(episodic_db_last_insert_id(con))
}

#' @keywords internal
#' @noRd
episodic_db_app_user_event_insert <- function(con,
                                              user_id,
                                              event_type,
                                              actor_user_id = NA,
                                              password_hash = NA,
                                              new_role = NA,
                                              new_is_admin = NA,
                                              new_is_active = NA) {
  params <- list(
    user_id,
    episodic_now(),
    event_type,
    actor_user_id,
    password_hash,
    new_role,
    new_is_admin,
    new_is_active
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_app_user_event
      (user_id, created_at, event_type, actor_user_id, password_hash, new_role, new_is_admin, new_is_active)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_app_config_event_insert <- function(con,
                                                user_id,
                                                section,
                                                config_json,
                                                note = NA) {
  section <- match.arg(section, c("notifications"))
  params <- list(user_id, episodic_now(), section, config_json, note)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_app_config_event (user_id, created_at, section, config_json, note)
     VALUES (?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}
