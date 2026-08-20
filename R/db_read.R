#' Shared read helpers
#'
#' Plain `DBI`-based readers used by both the cron and the app. Read-only:
#' none of these functions write to the database. All SQL for the package
#' lives behind functions in `R/db_*.R`; nothing outside this layer issues
#' SQL directly.
#' @name db_read
NULL

#' @rdname db_read
#' @param con A [DBI::DBIConnection-class].
#' @export
episode_db_pathogen_config <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM episode_pathogen_config")
}

#' @rdname db_read
#' @param pathogen A single raw pathogen string.
#' @export
episode_db_pathogen_config_get <- function(con, pathogen) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_pathogen_config WHERE pathogen = ?",
    params = list(pathogen)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @rdname db_read
#' @param active_only If `TRUE` (default), only streams with `is_active = 1`.
#' @export
episode_db_streams <- function(con, active_only = TRUE) {
  sql <- "SELECT * FROM episode_stream"
  if (active_only) sql <- paste(sql, "WHERE is_active = 1")
  DBI::dbGetQuery(con, sql)
}

#' @rdname db_read
#' @param stream_key A single `stream_key`.
#' @export
episode_db_stream_get <- function(con, stream_key) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_stream WHERE stream_key = ?",
    params = list(stream_key)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @rdname db_read
#' @export
episode_db_institutions <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM episode_institution")
}

#' @rdname db_read
#' @param institution_key A single `institution_key`.
#' @export
episode_db_institution_get <- function(con, institution_key) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_institution WHERE institution_key = ?",
    params = list(institution_key)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @rdname db_read
#' @export
episode_db_cases <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM episode_case")
}

#' @rdname db_read
#' @export
episode_db_cases_for_pathogen <- function(con, pathogen) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_case WHERE pathogen = ? ORDER BY sample_date",
    params = list(pathogen)
  )
}

#' @rdname db_read
#' @param open_only If `TRUE`, exclude clusters with `merged_into` set.
#' @export
episode_db_clusters <- function(con, open_only = FALSE) {
  sql <- "SELECT * FROM episode_cluster"
  if (open_only) sql <- paste(sql, "WHERE merged_into IS NULL")
  DBI::dbGetQuery(con, sql)
}

#' @rdname db_read
#' @param stream_id A single `stream_id`.
#' @export
episode_db_clusters_for_stream <- function(con, stream_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_cluster WHERE stream_id = ? AND merged_into IS NULL",
    params = list(stream_id)
  )
}

#' @rdname db_read
#' @param cluster_id A single `cluster_id`.
#' @export
episode_db_cluster_cases <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT c.* FROM episode_case c
     INNER JOIN episode_cluster_case cc ON cc.case_id = c.case_id
     WHERE cc.cluster_id = ?",
    params = list(cluster_id)
  )
}

#' @rdname db_read
#' @export
episode_db_assessment_events <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_assessment_event WHERE cluster_id = ? ORDER BY created_at, event_id",
    params = list(cluster_id)
  )
}

#' @rdname db_read
#' @export
episode_db_cluster_states <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_cluster_state WHERE cluster_id = ? ORDER BY entered_at, state_id",
    params = list(cluster_id)
  )
}

#' @rdname db_read
#' @export
episode_db_stream_mutes <- function(con, stream_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_stream_mute WHERE stream_id = ? ORDER BY created_at",
    params = list(stream_id)
  )
}

#' @rdname db_read
#' @param username A single username.
#' @export
episode_db_user_by_username <- function(con, username) {
  res <- DBI::dbGetQuery(
    con, "SELECT * FROM episode_app_user WHERE username = ?",
    params = list(username)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @rdname db_read
#' @param user_id A single `user_id`.
#' @export
episode_db_user_by_id <- function(con, user_id) {
  res <- DBI::dbGetQuery(
    con, "SELECT * FROM episode_app_user WHERE user_id = ?",
    params = list(user_id)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @rdname db_read
#' @export
episode_db_app_user_events <- function(con, user_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_app_user_event WHERE user_id = ? ORDER BY created_at, event_id",
    params = list(user_id)
  )
}

#' @rdname db_read
#' @param run_id A single `run_id`.
#' @export
episode_db_detections_for_run <- function(con, run_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episode_detection WHERE run_id = ?",
    params = list(run_id)
  )
}

#' @rdname db_read
#' @param limit Maximum number of rows to return, most recent first.
#' @export
episode_db_runs <- function(con, limit = 200) {
  DBI::dbGetQuery(
    con, "SELECT * FROM episode_detection_run ORDER BY run_id DESC LIMIT ?",
    params = list(limit)
  )
}

#' @rdname db_read
#' @param status If given, only the latest run with this `status`.
#' @export
episode_db_latest_run <- function(con, status = NULL) {
  sql <- "SELECT * FROM episode_detection_run"
  if (!is.null(status)) {
    sql <- paste(sql, "WHERE status = ?")
    res <- DBI::dbGetQuery(con, paste(sql, "ORDER BY run_id DESC LIMIT 1"), params = list(status))
  } else {
    res <- DBI::dbGetQuery(con, paste(sql, "ORDER BY run_id DESC LIMIT 1"))
  }
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @rdname db_read
#' @export
episode_db_stream_trend <- function(con, stream_id) {
  DBI::dbGetQuery(
    con, "SELECT * FROM episode_stream_trend WHERE stream_id = ? ORDER BY week_start",
    params = list(stream_id)
  )
}

#' @rdname db_read
#' @export
episode_db_denominator_for_pathogen <- function(con, pathogen) {
  DBI::dbGetQuery(
    con, "SELECT * FROM episode_denominator WHERE pathogen = ? ORDER BY sample_date",
    params = list(pathogen)
  )
}

#' @rdname db_read
#' @export
episode_db_reports_for_cluster <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con, "SELECT * FROM episode_report_render WHERE cluster_id = ? ORDER BY version_no",
    params = list(cluster_id)
  )
}

#' @rdname db_read
#' @param institution_id An `episode_institution` id.
#' @export
episode_db_institution_activity <- function(con, institution_id) {
  DBI::dbGetQuery(
    con, "SELECT * FROM episode_institution_activity WHERE institution_id = ? ORDER BY period_start",
    params = list(institution_id)
  )
}
