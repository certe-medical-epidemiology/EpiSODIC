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
# inst/sql/schema.sql for the exact column contracts (nullability,
# enums, defaults).

#' @keywords internal
#' @noRd
episodic_db_assessment_event_insert <- function(
    con,
    cluster_id,
    user_id,
    verdict = NA,
    rationale,
    wpg_notifiable = NA,
    ggd_informed = NA,
    ggd_note = NA,
    snooze_until = NA,
    supersedes = NA) {
  stopifnot(is.character(rationale), nzchar(rationale))
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_assessment_event
      (cluster_id, user_id, created_at, verdict, rationale, wpg_notifiable, ggd_informed,
       ggd_note, snooze_until, supersedes)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
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
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_stream_mute_insert <- function(
    con,
    stream_id,
    muted_from,
    muted_until,
    reason,
    note = NA,
    user_id) {
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_stream_mute
      (stream_id, muted_from, muted_until, reason, note, user_id, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      stream_id,
      muted_from,
      muted_until,
      reason,
      note,
      user_id,
      episodic_now()
    )
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_state_insert <- function(
    con,
    cluster_id,
    state,
    trigger,
    event_id = NA,
    user_id = NA) {
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_cluster_state (cluster_id, state, entered_at, trigger, event_id, user_id)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(cluster_id, state, episodic_now(), trigger, event_id, user_id)
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_report_render_insert <- function(
    con,
    cluster_id,
    user_id = NA,
    file_path,
    file_sha256,
    params_json,
    case_ids_json,
    version_no) {
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_report_render
      (cluster_id, user_id, rendered_at, file_path, file_sha256, params, case_ids, version_no)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      cluster_id,
      user_id,
      episodic_now(),
      file_path,
      file_sha256,
      params_json,
      case_ids_json,
      version_no
    )
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_app_user_insert <- function(
    con,
    username,
    full_name,
    email,
    password_hash,
    role = "epidemiologist") {
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_app_user
      (username, full_name, email, password_hash, role, is_active, must_change, created_at)
     VALUES (?, ?, ?, ?, ?, 1, 1, ?)",
    params = list(
      username,
      full_name,
      email,
      password_hash,
      role,
      episodic_now()
    )
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_app_user_event_insert <- function(
    con,
    user_id,
    event_type,
    password_hash = NA) {
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_app_user_event (user_id, created_at, event_type, password_hash)
     VALUES (?, ?, ?, ?)",
    params = list(user_id, episodic_now(), event_type, password_hash)
  )
  episodic_db_last_insert_id(con)
}
