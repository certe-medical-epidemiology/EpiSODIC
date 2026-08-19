#' App-side writers
#'
#' The app owns the judgements (ARCHITECTURE.md section 5.0) and only ever
#' inserts, never updates and never deletes (standing brief hard rule 7).
#' This is what makes the whole concurrency question disappear: two people
#' assessing the same cluster in the same minute produce two rows, both
#' visible in the timeline. Nothing in this file contains an `UPDATE` or a
#' `DELETE` statement; that absence is load-bearing and should be verified
#' by inspection, per MILESTONES.md M3's definition of done.
#' @name db_app_write
NULL

#' @rdname db_app_write
#' @param con A [DBI::DBIConnection-class].
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
