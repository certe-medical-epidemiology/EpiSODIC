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

# Cron-side writers: the cron owns the facts and may upsert. These
# functions are the only place in the package that write to
# episodic_stream, episodic_institution, episodic_institution_activity,
# episodic_case, episodic_denominator,
# episodic_detection, episodic_cluster, episodic_cluster_case,
# episodic_detection_run and (for pre-renders) episodic_report_render. See
# R/db_app_write.R for the insert-only counterparts. Parameters
# throughout are one row's worth of columns for the table each function
# name identifies - see inst/sql/schema.sql for the exact column
# contracts (nullability, enums, defaults).

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
episodic_db_pathogen_config_load <- function(con, pathogen_config) {
  if (nrow(pathogen_config) == 0) {
    return(invisible(NULL))
  }
  cols <- c(
    "pathogen",
    "episode_days",
    "incub_min_days",
    "incub_max_days",
    "case_free_days",
    "cooldown_days",
    "rt_applicable",
    "si_mean_days",
    "si_sd_days",
    "si_dist",
    "mem_applicable",
    "severity_weight",
    "source_ref"
  )
  values <- as.list(pathogen_config[, cols, drop = FALSE])
  values$rt_applicable <- as.integer(values$rt_applicable)
  values$mem_applicable <- as.integer(values$mem_applicable)
  episodic_db_write_many(
    con,
    table = "episodic_pathogen_config",
    cols = cols,
    values = values,
    key_cols = "pathogen",
    update_cols = setdiff(cols, "pathogen")
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_institution_activity_upsert <- function(
    con,
    institution_id,
    period_start,
    period_end,
    patient_days = NA,
    admissions = NA,
    n_beds = NA,
    source = NA) {
  period_start <- episodic_sql_date(period_start)
  period_end <- episodic_sql_date(period_end)
  params <- list(institution_id, period_start)
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_institution_activity WHERE institution_id = ? AND period_start = ?",
    params = params
  )
  if (nrow(existing) > 0) {
    params <- list(
      period_end,
      patient_days,
      admissions,
      n_beds,
      source,
      institution_id,
      period_start
    )
    DBI::dbExecute(
      con,
      "UPDATE episodic_institution_activity SET period_end = ?, patient_days = ?, admissions = ?,
        n_beds = ?, source = ? WHERE institution_id = ? AND period_start = ?",
      params = params
    )
  } else {
    params <- list(
      institution_id,
      period_start,
      period_end,
      patient_days,
      admissions,
      n_beds,
      source
    )
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_institution_activity
        (institution_id, period_start, period_end, patient_days, admissions, n_beds, source)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = params
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_stream_upsert <- function(
    con,
    stream_key,
    level,
    pathogen,
    care_line = NA,
    region_code = NA,
    institution_id = NA,
    ward = NA,
    denominator = "none",
    severity_weight = 1.00,
    observed_date) {
  observed_date <- episodic_sql_date(observed_date)
  existing <- episodic_db_stream_get(con, stream_key)
  if (!is.null(existing)) {
    first_seen <- min(existing$first_seen, observed_date)
    last_seen <- max(existing$last_seen, observed_date)
    params <- list(first_seen, last_seen, stream_key)
    DBI::dbExecute(
      con,
      "UPDATE episodic_stream SET first_seen = ?, last_seen = ? WHERE stream_key = ?",
      params = params
    )
    return(existing$stream_id)
  }
  params <- list(
    stream_key,
    level,
    pathogen,
    care_line,
    region_code,
    institution_id,
    ward,
    denominator,
    severity_weight,
    observed_date,
    observed_date,
    episodic_now()
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_stream
      (stream_key, level, pathogen, care_line, region_code, institution_id,
       ward, denominator, severity_weight, is_active, first_seen, last_seen, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' Insert or upsert many rows in one statement per chunk
#'
#' The package's write helpers used to send one row per `DBI` call, which
#' on a local SQLite file costs nothing and against a networked MariaDB
#' costs a full round trip each - the difference between a run that takes
#' seconds and one that takes minutes.
#'
#' Chunked `dbBind()` is not the fix, despite looking like it: RMariaDB
#' binds a multi-row parameter list with `while (bind_next_row())
#' { execute(); }`, one `mysql_stmt_execute()` per row, so it saves the
#' parse but still pays every round trip. What does collapse them is one
#' statement carrying every row's placeholders - `VALUES (?, ?), (?, ?),
#' ...` with a single flat parameter list, which both drivers execute
#' once.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param table Table name.
#' @param cols Column names, in the order `values` supplies them.
#' @param values A list of equal-length column vectors (a data frame is
#'   fine), giving the rows to write.
#' @param key_cols The columns whose uniqueness decides insert vs. update.
#'   `NULL` (the default) means a plain insert with no conflict handling -
#'   use it only when the caller has already established the rows are new.
#' @param update_cols The columns to overwrite when a row already exists.
#' @return Invisibly, the number of rows written.
#' @keywords internal
#' @noRd
episodic_db_write_many <- function(
    con,
    table,
    cols,
    values,
    key_cols = NULL,
    update_cols = NULL) {
  n <- length(values[[1]])
  if (n == 0) {
    return(invisible(0L))
  }
  tuple <- paste0("(", paste(rep("?", length(cols)), collapse = ", "), ")")

  # Both dialects spell "insert, or update what is already there" their own
  # way, and neither understands the other's. Same split as
  # episodic_db_last_insert_id() and episodic_db_pragmas() already make.
  tail <- ""
  if (!is.null(key_cols)) {
    sqlite <- inherits(con, "SQLiteConnection")
    tail <- if (length(update_cols) > 0) {
      if (sqlite) {
        paste0(
          " ON CONFLICT (",
          paste(key_cols, collapse = ", "),
          ") DO UPDATE SET ",
          paste(
            sprintf("%s = excluded.%s", update_cols, update_cols),
            collapse = ", "
          )
        )
      } else {
        paste0(
          " ON DUPLICATE KEY UPDATE ",
          paste(
            sprintf("%s = VALUES(%s)", update_cols, update_cols),
            collapse = ", "
          )
        )
      }
    } else if (sqlite) {
      # Nothing to update: the row already says what it would have said.
      paste0(" ON CONFLICT (", paste(key_cols, collapse = ", "), ") DO NOTHING")
    } else {
      # MariaDB has no DO NOTHING; assigning a key column to itself is the
      # idiomatic no-op, and unlike INSERT IGNORE it does not also swallow
      # unrelated errors.
      sprintf(" ON DUPLICATE KEY UPDATE %s = %s", key_cols[1], key_cols[1])
    }
  }

  # Chunked on the placeholder count, not the row count: a wide table hits
  # the driver's parameter ceiling far sooner than a narrow one.
  per_chunk <- max(1L, as.integer(floor(5000 / length(cols))))
  chunks <- split(seq_len(n), ceiling(seq_len(n) / per_chunk))
  for (idx in chunks) {
    sql <- paste0(
      "INSERT INTO ",
      table,
      " (",
      paste(cols, collapse = ", "),
      ") VALUES ",
      paste(rep(tuple, length(idx)), collapse = ", "),
      tail
    )
    # One flat list, row-major: every element length 1, so the driver binds
    # and executes exactly once.
    params <- vector("list", length(idx) * length(cols))
    k <- 1L
    for (i in idx) {
      for (col in cols) {
        params[[k]] <- values[[col]][i]
        k <- k + 1L
      }
    }
    DBI::dbExecute(con, sql, params = params)
  }
  invisible(n)
}

episodic_db_chunk_size <- 500L

#' The subset of `keys` already present in `episodic_case.source_key`
#'
#' One `IN (...)` query per chunk instead of one `SELECT` per row - the
#' loop this replaced sent as many round trips to the database as there
#' were cases in the batch, which is what made a run's own case-insert
#' step the slow part of an import that was otherwise dominated by
#' network latency rather than by the data itself.
#' @keywords internal
#' @noRd
episodic_db_existing_source_keys <- function(con, keys) {
  keys <- unique(keys)
  if (length(keys) == 0) {
    return(character(0))
  }
  chunks <- split(keys, ceiling(seq_along(keys) / episodic_db_chunk_size))
  found <- lapply(chunks, function(chunk) {
    placeholders <- paste(rep("?", length(chunk)), collapse = ", ")
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT source_key FROM episodic_case WHERE source_key IN (%s)",
        placeholders
      ),
      params = as.list(chunk)
    )$source_key
  })
  unlist(found, use.names = FALSE)
}

#' @keywords internal
#' @noRd
episodic_db_case_insert_new <- function(con, cases, run_id) {
  if (nrow(cases) == 0) {
    return(0L)
  }

  existing <- episodic_db_existing_source_keys(con, cases$source_key)
  to_insert <- cases[!(cases$source_key %in% existing), , drop = FALSE]
  n_inserted <- nrow(to_insert)
  if (n_inserted == 0) {
    return(0L)
  }

  # Every row's placeholders in one statement. This used to bind a single
  # prepared statement in chunks, which reads like a batch but is not one:
  # RMariaDB executes a multi-row parameter list one row at a time, so the
  # only thing that saved was the parse, and 2,400 cases still cost 2,400
  # round trips.
  episodic_db_write_many(
    con,
    table = "episodic_case",
    cols = c(
      "source_key",
      "lab_number",
      "patient_key",
      "sample_date",
      "receipt_date",
      "pathogen",
      "care_line",
      "institution_id",
      "ward",
      "specialism",
      "pc",
      "sex",
      "age",
      "first_seen_run"
    ),
    values = list(
      source_key = to_insert$source_key,
      lab_number = to_insert$lab_number,
      patient_key = to_insert$patient_key,
      sample_date = episodic_sql_date(to_insert$sample_date),
      receipt_date = episodic_sql_date(to_insert$receipt_date),
      pathogen = to_insert$pathogen,
      care_line = to_insert$care_line,
      institution_id = to_insert$institution_id,
      ward = to_insert$ward,
      specialism = to_insert$specialism,
      pc = to_insert$pc,
      sex = to_insert$sex,
      age = to_insert$age,
      first_seen_run = rep(run_id, n_inserted)
    )
  )
  n_inserted
}

#' @param week_start A week-start date (chart-cache row for the multi-year
#'   trend panel, see `R/detect_farrington.R`).
#' @keywords internal
#' @noRd
episodic_db_stream_trend_upsert <- function(
    con,
    stream_id,
    week_start,
    n_cases,
    expected = NA,
    upperbound = NA) {
  params <- list(stream_id, week_start)
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_stream_trend WHERE stream_id = ? AND week_start = ?",
    params = params
  )
  if (nrow(existing) > 0) {
    params <- list(n_cases, expected, upperbound, stream_id, week_start)
    DBI::dbExecute(
      con,
      "UPDATE episodic_stream_trend SET n_cases = ?, expected = ?, upperbound = ?
       WHERE stream_id = ? AND week_start = ?",
      params = params
    )
  } else {
    params <- list(stream_id, week_start, n_cases, expected, upperbound)
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_stream_trend (stream_id, week_start, n_cases, expected, upperbound)
       VALUES (?, ?, ?, ?, ?)",
      params = params
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_detection_insert <- function(
    con,
    run_id,
    stream_id,
    detector,
    first_day,
    last_day,
    n_cases,
    expected = NA,
    upperbound = NA,
    params_json,
    cluster_id = NA) {
  params <- list(
    run_id,
    stream_id,
    cluster_id,
    detector,
    first_day,
    last_day,
    n_cases,
    expected,
    upperbound,
    params_json,
    episodic_now()
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_detection
      (run_id, stream_id, cluster_id, detector, first_day, last_day, n_cases, expected,
       upperbound, params, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_detection_set_cluster <- function(con, detection_id, cluster_id) {
  params <- list(cluster_id, detection_id)
  DBI::dbExecute(
    con,
    "UPDATE episodic_detection SET cluster_id = ? WHERE detection_id = ?",
    params = params
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_insert <- function(
    con,
    stream_id,
    first_day,
    last_day,
    n_cases,
    expected = NA,
    excess = NA,
    ratio = NA,
    priority_score,
    detector_agreement,
    run_id) {
  params <- list(
    stream_id,
    first_day,
    last_day,
    n_cases,
    expected,
    excess,
    ratio,
    priority_score,
    detector_agreement,
    episodic_now(),
    run_id
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_cluster
      (stream_id, first_day, last_day, n_cases, expected, excess, ratio, priority_score,
       detector_agreement, opened_at, last_detected_run, runs_since_detected,
       changed_since_assessment, suppressed_by, merged_into)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, 0, NULL, NULL)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_update <- function(
    con,
    cluster_id,
    first_day,
    last_day,
    n_cases,
    expected = NA,
    excess = NA,
    ratio = NA,
    priority_score,
    detector_agreement,
    run_id,
    changed_since_assessment = NULL) {
  if (is.null(changed_since_assessment)) {
    params <- list(
      first_day,
      last_day,
      n_cases,
      expected,
      excess,
      ratio,
      priority_score,
      detector_agreement,
      run_id,
      cluster_id
    )
    DBI::dbExecute(
      con,
      "UPDATE episodic_cluster SET first_day = ?, last_day = ?, n_cases = ?, expected = ?,
        excess = ?, ratio = ?, priority_score = ?, detector_agreement = ?,
        last_detected_run = ?, runs_since_detected = 0 WHERE cluster_id = ?",
      params = params
    )
  } else {
    params <- list(
      first_day,
      last_day,
      n_cases,
      expected,
      excess,
      ratio,
      priority_score,
      detector_agreement,
      run_id,
      as.integer(changed_since_assessment),
      cluster_id
    )
    DBI::dbExecute(
      con,
      "UPDATE episodic_cluster SET first_day = ?, last_day = ?, n_cases = ?, expected = ?,
        excess = ?, ratio = ?, priority_score = ?, detector_agreement = ?,
        last_detected_run = ?, runs_since_detected = 0, changed_since_assessment = ?
       WHERE cluster_id = ?",
      params = params
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_increment_runs_since_detected <- function(con, cluster_id) {
  params <- list(cluster_id)
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET runs_since_detected = runs_since_detected + 1 WHERE cluster_id = ?",
    params = params
  )
  invisible(NULL)
}

#' Attach a cluster to the one that suppresses it, or detach it
#'
#' `NA` clears the suppression, which every run does before deciding it
#' again: a suppression is a statement about how the current picture
#' looks, not a permanent property of the cluster.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The cluster being suppressed.
#' @param suppressed_by The cluster that suppresses it, or `NA` to clear.
#' @keywords internal
#' @noRd
episodic_db_cluster_set_suppressed_by <- function(
    con,
    cluster_id,
    suppressed_by) {
  params <- list(
    if (is.na(suppressed_by)) NA_integer_ else as.integer(suppressed_by),
    cluster_id
  )
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET suppressed_by = ? WHERE cluster_id = ?",
    params = params
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_set_merged_into <- function(con, cluster_id, merged_into) {
  params <- list(merged_into, cluster_id)
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET merged_into = ? WHERE cluster_id = ?",
    params = params
  )
  invisible(NULL)
}

#' Link many cases to one cluster in a single statement
#'
#' `(cluster_id, case_id)` is the table's primary key, so re-linking a case
#' the cluster already holds is a no-op the database can decide for itself.
#' Asking it first, once per case, was two round trips per link.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id One cluster id.
#' @param case_ids The case ids to link. May be empty.
#' @return Invisibly, `NULL`.
#' @keywords internal
#' @noRd
episodic_db_cluster_case_link_many <- function(con, cluster_id, case_ids) {
  case_ids <- unique(case_ids)
  if (length(case_ids) == 0) {
    return(invisible(NULL))
  }
  episodic_db_write_many(
    con,
    table = "episodic_cluster_case",
    cols = c("cluster_id", "case_id"),
    values = list(
      cluster_id = rep(cluster_id, length(case_ids)),
      case_id = case_ids
    ),
    key_cols = c("cluster_id", "case_id")
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_run_start <- function(
    con,
    host,
    account,
    run_date = Sys.Date(),
    attempt_no = 1L) {
  params <- list(
    host,
    account,
    episodic_now(),
    episodic_sql_date(run_date),
    attempt_no
  )
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_detection_run (host, account, started_at, run_date, status, attempt_no)
     VALUES (?, ?, ?, ?, 'running', ?)",
    params = params
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_run_finish <- function(
    con,
    run_id,
    status,
    n_streams = NA,
    n_detections = NA,
    n_signals_new = NA,
    n_signals_updated = NA,
    n_cases_supplied = NA,
    n_cases_deduplicated = NA,
    n_cases_inserted = NA,
    n_denominators_written = NA,
    n_activity_supplied = NA,
    n_activity_written = NA,
    n_activity_skipped = NA,
    code_version = NA,
    pkg_versions = NA,
    config_hash = NA,
    config_snapshot = NA,
    error_text = NA) {
  params <- list(
    episodic_now(),
    status,
    n_streams,
    n_detections,
    n_signals_new,
    n_signals_updated,
    n_cases_supplied,
    n_cases_deduplicated,
    n_cases_inserted,
    n_denominators_written,
    n_activity_supplied,
    n_activity_written,
    n_activity_skipped,
    code_version,
    pkg_versions,
    config_hash,
    config_snapshot,
    error_text,
    run_id
  )
  DBI::dbExecute(
    con,
    "UPDATE episodic_detection_run SET finished_at = ?, status = ?, n_streams = ?,
      n_detections = ?, n_signals_new = ?, n_signals_updated = ?,
      n_cases_supplied = ?, n_cases_deduplicated = ?, n_cases_inserted = ?,
      n_denominators_written = ?, n_activity_supplied = ?, n_activity_written = ?,
      n_activity_skipped = ?, code_version = ?,
      pkg_versions = ?, config_hash = ?, config_snapshot = ?, error_text = ?
     WHERE run_id = ?",
    params = params
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS0Z", tz = "UTC")
}

#' Coerce a `Date` column to `YYYY-MM-DD` text before it is bound as a SQL parameter
#'
#' RSQLite binds a `Date` parameter by its underlying double (days since
#' 1970-01-01), not by its printed form - a `TEXT`-affinity column such as
#' `sample_date` then stores that number as a *string of digits*
#' (`"20089"`) rather than an ISO date. Every reader downstream calls
#' `as.Date()` on what it gets back and only ever expects `"2025-01-01"`,
#' so that stored digit-string fails to parse with "character string is
#' not in a standard unambiguous format" the moment it is read back - the
#' case data contract explicitly allows `sample_date` etc. to arrive as
#' `Date` (see `episodic_validate_dates()`), so this is not an edge case.
#' Applied at every write site that accepts an operator-supplied date
#' column, so the fix holds regardless of whether the caller remembered
#' to convert.
#' @keywords internal
#' @noRd
episodic_sql_date <- function(x) {
  if (inherits(x, "Date")) {
    format(x, "%Y-%m-%d")
  } else {
    x
  }
}
