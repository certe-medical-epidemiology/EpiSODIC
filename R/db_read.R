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

# Shared read helpers: plain DBI-based readers used by both the cron and
# the app. Read-only: none of these functions write to the database. All
# SQL for the package lives behind functions in R/db_*.R; nothing
# outside this layer issues SQL directly.

#' @param con A [DBI::DBIConnection-class].
#' @keywords internal
#' @noRd
episodic_db_pathogen_config <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM episodic_pathogen_config")
}

#' @param pathogen A single raw pathogen string.
#' @keywords internal
#' @noRd
episodic_db_pathogen_config_get <- function(con, pathogen) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_pathogen_config WHERE pathogen = ?",
    params = list(pathogen)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @param active_only If `TRUE` (default), only streams with `is_active = 1`.
#' @keywords internal
#' @noRd
episodic_db_streams <- function(con, active_only = TRUE) {
  sql <- "SELECT * FROM episodic_stream"
  if (active_only) {
    sql <- paste(sql, "WHERE is_active = 1")
  }
  DBI::dbGetQuery(con, sql)
}

#' @param stream_key A single `stream_key`.
#' @keywords internal
#' @noRd
episodic_db_stream_get <- function(con, stream_key) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_stream WHERE stream_key = ?",
    params = list(stream_key)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @keywords internal
#' @noRd
episodic_db_institutions <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM episodic_institution")
}

#' @param institution_key A single `institution_key`.
#' @keywords internal
#' @noRd
episodic_db_institution_get <- function(con, institution_key) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_institution WHERE institution_key = ?",
    params = list(institution_key)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @keywords internal
#' @noRd
episodic_db_cases <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM episodic_case")
}

#' @keywords internal
#' @noRd
episodic_db_cases_for_pathogen <- function(con, pathogen) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_case WHERE pathogen = ? ORDER BY sample_date",
    params = list(pathogen)
  )
}

#' The most recent stored episode anchor per patient/pathogen
#'
#' For every `patient_key`/`pathogen` combination already in
#' `episodic_case`, the latest `sample_date` on record - which is the
#' anchor date of that patient/pathogen's most recent episode, since
#' `episodic_cases_deduplicate()` only ever stores the earliest date of
#' each episode. Used by `episodic_cases_load()` so a run does not need
#' the full case history resent every time to deduplicate correctly: an
#' incoming positive close enough to this stored anchor is recognised as
#' the same, already-recorded episode.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param patient_keys,pathogens The distinct values from the incoming
#'   batch to look up. Filters to rows whose `patient_key` and `pathogen`
#'   both appear somewhere in the batch (not the whole table); the
#'   result may include a few patient/pathogen pairs that were not
#'   actually paired in the batch itself, which is harmless - the caller
#'   only ever looks up the exact pairs it has.
#' @return A named character vector of `YYYY-MM-DD` dates, one per
#'   patient/pathogen combination found, named `paste0(patient_key,
#'   pathogen)` (matching `episodic_cases_deduplicate()`'s own internal
#'   grouping key). Empty (but named-vector-shaped) if nothing matches or
#'   either input is empty.
#' @keywords internal
#' @noRd
episodic_db_last_case_dates <- function(con, patient_keys, pathogens) {
  empty <- stats::setNames(character(0), character(0))
  if (length(patient_keys) == 0 || length(pathogens) == 0) {
    return(empty)
  }

  patient_placeholders <- paste(rep("?", length(patient_keys)), collapse = ", ")
  pathogen_placeholders <- paste(rep("?", length(pathogens)), collapse = ", ")
  res <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT patient_key, pathogen, MAX(sample_date) AS last_date
       FROM episodic_case
       WHERE patient_key IN (%s) AND pathogen IN (%s)
       GROUP BY patient_key, pathogen",
      patient_placeholders,
      pathogen_placeholders
    ),
    params = c(as.list(patient_keys), as.list(pathogens))
  )
  if (nrow(res) == 0) {
    return(empty)
  }
  stats::setNames(
    as.character(res$last_date),
    paste0(res$patient_key, res$pathogen)
  )
}

#' @param open_only If `TRUE`, exclude clusters with `merged_into` set.
#' @keywords internal
#' @noRd
episodic_db_clusters <- function(
  con,
  open_only = FALSE,
  include_suppressed = FALSE
) {
  sql <- "SELECT * FROM episodic_cluster"
  where <- character(0)
  if (open_only) {
    where <- c(where, "merged_into IS NULL")
  }
  # A suppressed cluster is the same outbreak as one already in the
  # queue, seen at another level. It keeps everything it has, and the
  # dossier of the cluster that suppressed it is where it is shown; what
  # it must not do is appear in the queue as a second thing to assess.
  if (!include_suppressed) {
    where <- c(where, "suppressed_by IS NULL")
  }
  if (length(where) > 0) {
    sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  }
  DBI::dbGetQuery(con, sql)
}

#' Everything one cluster suppressed, for its own dossier
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The surviving cluster.
#' @return A data frame of suppressed clusters with their `pathogen` and
#'   `level`, oldest first; no rows when it suppressed nothing.
#' @keywords internal
#' @noRd
episodic_db_clusters_suppressed_by <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT c.*, s.pathogen, s.level
     FROM episodic_cluster c
     INNER JOIN episodic_stream s ON s.stream_id = c.stream_id
     WHERE c.suppressed_by = ?
     ORDER BY c.cluster_id",
    params = list(cluster_id)
  )
}

#' Other clusters holding some of the same cases
#'
#' Suppression only collapses clusters in one containment chain: a ward is
#' part of a hospital, an area is part of a province. It deliberately does
#' not link the two chains, because letting a diffuse regional signal
#' suppress a real ward outbreak would hide the more actionable of the
#' two. But the admin still has to know: a regional norovirus rise
#' driven by a ward outbreak and a nursing home is one set of cases in
#' three dossiers, and reading any of them without the others is reading
#' it wrong. These are the clusters that share cases with this one and
#' stand separately from it.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The cluster being viewed.
#' @return A data frame of clusters with their `pathogen`, `level` and
#'   `shared_cases`, most shared first. Suppressed and merged-away
#'   clusters are left out: those are not separate dossiers.
#' @keywords internal
#' @noRd
episodic_db_clusters_linked_to <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT c.*, s.pathogen, s.level, COUNT(*) AS shared_cases
     FROM episodic_cluster_case mine
     INNER JOIN episodic_cluster_case theirs ON theirs.case_id = mine.case_id
     INNER JOIN episodic_cluster c ON c.cluster_id = theirs.cluster_id
     INNER JOIN episodic_stream s ON s.stream_id = c.stream_id
     WHERE mine.cluster_id = ?
       AND theirs.cluster_id != ?
       AND c.merged_into IS NULL
       AND c.suppressed_by IS NULL
     GROUP BY c.cluster_id
     ORDER BY shared_cases DESC, c.cluster_id",
    params = list(cluster_id, cluster_id)
  )
}

#' Every cluster the lattice suppression pass has to weigh up
#'
#' Not merged away, not closed, with the pathogen and level of its stream
#' attached - suppression is a comparison between levels, so the level is
#' what it needs and the cluster table does not carry it.
#' @keywords internal
#' @noRd
episodic_db_clusters_for_suppression <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT c.*, s.pathogen, s.level
     FROM episodic_cluster c
     INNER JOIN episodic_stream s ON s.stream_id = c.stream_id
     WHERE c.merged_into IS NULL"
  )
}

#' @param stream_id A single `stream_id`.
#' @keywords internal
#' @noRd
episodic_db_clusters_for_stream <- function(con, stream_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster WHERE stream_id = ? AND merged_into IS NULL",
    params = list(stream_id)
  )
}

#' @param cluster_id A single `cluster_id`.
#' @keywords internal
#' @noRd
episodic_db_cluster_cases <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT c.* FROM episodic_case c
     INNER JOIN episodic_cluster_case cc ON cc.case_id = c.case_id
     WHERE cc.cluster_id = ?",
    params = list(cluster_id)
  )
}

#' @keywords internal
#' @noRd
episodic_db_assessment_events <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_assessment_event WHERE cluster_id = ? ORDER BY created_at, event_id",
    params = list(cluster_id)
  )
}

#' @keywords internal
#' @noRd
episodic_db_cluster_states <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster_state WHERE cluster_id = ? ORDER BY entered_at, state_id",
    params = list(cluster_id)
  )
}

#' @keywords internal
#' @noRd
episodic_db_stream_mutes <- function(con, stream_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_stream_mute WHERE stream_id = ? ORDER BY created_at",
    params = list(stream_id)
  )
}

#' @param username A single username.
#' @keywords internal
#' @noRd
episodic_db_user_by_username <- function(con, username) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_app_user WHERE username = ?",
    params = list(username)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @param user_id A single `user_id`.
#' @keywords internal
#' @noRd
episodic_db_user_by_id <- function(con, user_id) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_app_user WHERE user_id = ?",
    params = list(user_id)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @keywords internal
#' @noRd
episodic_db_app_user_events <- function(con, user_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_app_user_event WHERE user_id = ? ORDER BY created_at, event_id",
    params = list(user_id)
  )
}

#' @param run_id A single `run_id`.
#' @keywords internal
#' @noRd
episodic_db_detections_for_run <- function(con, run_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_detection WHERE run_id = ?",
    params = list(run_id)
  )
}

#' @param limit Maximum number of rows to return, most recent first.
#' @keywords internal
#' @noRd
episodic_db_runs <- function(con, limit = 200) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_detection_run ORDER BY run_id DESC LIMIT ?",
    params = list(limit)
  )
}

#' One run by its id
#'
#' The Activity screen's run detail reads the whole row: the counts, the
#' provenance, and - the point of it - `error_text` when the run failed.
#' @keywords internal
#' @noRd
episodic_db_run <- function(con, run_id) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_detection_run WHERE run_id = ?",
    params = list(run_id)
  )
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @param status If given, only the latest run with one of these
#'   statuses. Pass `episodic_run_statuses_complete` for "the latest run
#'   that produced usable results", which is what almost every caller
#'   means - a `partial` run completed and wrote its detections; it only
#'   skipped rows of an optional feed.
#' @keywords internal
#' @noRd
episodic_db_latest_run <- function(con, status = NULL) {
  sql <- "SELECT * FROM episodic_detection_run"
  if (!is.null(status)) {
    placeholders <- paste(rep("?", length(status)), collapse = ", ")
    sql <- paste0(sql, " WHERE status IN (", placeholders, ")")
    res <- DBI::dbGetQuery(
      con,
      paste(sql, "ORDER BY run_id DESC LIMIT 1"),
      params = as.list(status)
    )
  } else {
    res <- DBI::dbGetQuery(con, paste(sql, "ORDER BY run_id DESC LIMIT 1"))
  }
  if (nrow(res) == 0) NULL else res[1, ]
}

#' @keywords internal
#' @noRd
episodic_db_stream_trend <- function(con, stream_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_stream_trend WHERE stream_id = ? ORDER BY week_start",
    params = list(stream_id)
  )
}

#' @keywords internal
#' @noRd
episodic_db_denominator_for_pathogen <- function(con, pathogen) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_denominator WHERE pathogen = ? ORDER BY sample_date",
    params = list(pathogen)
  )
}

#' @keywords internal
#' @noRd
episodic_db_reports_for_cluster <- function(con, cluster_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_report_render WHERE cluster_id = ? ORDER BY version_no",
    params = list(cluster_id)
  )
}

#' @param institution_id An `episodic_institution` id.
#' @keywords internal
#' @noRd
episodic_db_institution_activity <- function(con, institution_id) {
  DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_institution_activity WHERE institution_id = ? ORDER BY period_start",
    params = list(institution_id)
  )
}
