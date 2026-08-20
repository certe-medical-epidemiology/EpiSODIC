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

#' App-side writers
#'
#' The app owns the judgements and only ever
#' inserts, never updates and never deletes.
#' This is what makes the whole concurrency question disappear: two people
#' assessing the same cluster in the same minute produce two rows, both
#' visible in the timeline. Nothing in this file contains an `UPDATE` or a
#' `DELETE` statement; that absence is load-bearing and should be
#' verified by inspection whenever this file changes.
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param user_id An `episode_app_user` id, or `NA` for a system-authored row.
#' @param verdict One of the five classification values, or `NA`.
#' @param rationale Mandatory free-text rationale.
#' @param wpg_notifiable,ggd_informed Logical or `NA`.
#' @param ggd_note Free text, or `NA`.
#' @param snooze_until A date, or `NA`.
#' @param supersedes An earlier `event_id` this event supersedes, or `NA`.
#' @param stream_id A stream id.
#' @param muted_from,muted_until Mute window bounds (dates).
#' @param reason One of the mute reasons in `episode_stream_mute.reason`.
#' @param note Free text, or `NA`.
#' @param state One of the `episode_cluster_state.state` values.
#' @param trigger One of the `episode_cluster_state.trigger` values.
#' @param event_id The assessment event that caused this transition, or `NA`.
#' @param file_path Path to the rendered report file.
#' @param file_sha256 SHA-256 hex digest of the rendered file.
#' @param params_json JSON-serialised render parameters.
#' @param case_ids_json JSON-serialised array of included case ids.
#' @param version_no The report's version number.
#' @param username,full_name,email,password_hash New user's fields.
#' @param role One of `"assessor"`, `"admin"`.
#' @param event_type One of `"login"`, `"password_change"`.
#' @name db_app_write
NULL

#' @rdname db_app_write
#' @keywords internal
#' @noRd
episode_db_assessment_event_insert <- function(con, cluster_id, user_id, verdict = NA,
                                                rationale, wpg_notifiable = NA,
                                                ggd_informed = NA, ggd_note = NA,
                                                snooze_until = NA, supersedes = NA) {
  stopifnot(is.character(rationale), nzchar(rationale))
  DBI::dbExecute(
    con,
    "INSERT INTO episode_assessment_event
      (cluster_id, user_id, created_at, verdict, rationale, wpg_notifiable, ggd_informed,
       ggd_note, snooze_until, supersedes)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(cluster_id, user_id, episode_now(), verdict, rationale,
                  if (is.na(wpg_notifiable)) NA else as.integer(wpg_notifiable),
                  if (is.na(ggd_informed)) NA else as.integer(ggd_informed),
                  ggd_note, snooze_until, supersedes)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_app_write
#' @keywords internal
#' @noRd
episode_db_stream_mute_insert <- function(con, stream_id, muted_from, muted_until, reason,
                                           note = NA, user_id) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_stream_mute
      (stream_id, muted_from, muted_until, reason, note, user_id, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(stream_id, muted_from, muted_until, reason, note, user_id, episode_now())
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_app_write
#' @keywords internal
#' @noRd
episode_db_cluster_state_insert <- function(con, cluster_id, state, trigger, event_id = NA,
                                             user_id = NA) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_cluster_state (cluster_id, state, entered_at, trigger, event_id, user_id)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(cluster_id, state, episode_now(), trigger, event_id, user_id)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_app_write
#' @keywords internal
#' @noRd
episode_db_report_render_insert <- function(con, cluster_id, user_id = NA, file_path,
                                             file_sha256, params_json, case_ids_json,
                                             version_no) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_report_render
      (cluster_id, user_id, rendered_at, file_path, file_sha256, params, case_ids, version_no)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(cluster_id, user_id, episode_now(), file_path, file_sha256, params_json,
                  case_ids_json, version_no)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_app_write
#' @keywords internal
#' @noRd
episode_db_app_user_insert <- function(con, username, full_name, email, password_hash,
                                        role = "assessor") {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_app_user
      (username, full_name, email, password_hash, role, is_active, must_change, created_at)
     VALUES (?, ?, ?, ?, ?, 1, 1, ?)",
    params = list(username, full_name, email, password_hash, role, episode_now())
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_app_write
#' @keywords internal
#' @noRd
episode_db_app_user_event_insert <- function(con, user_id, event_type, password_hash = NA) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_app_user_event (user_id, created_at, event_type, password_hash)
     VALUES (?, ?, ?, ?)",
    params = list(user_id, episode_now(), event_type, password_hash)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}
