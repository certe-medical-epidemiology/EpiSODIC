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
# episodic_case, episodic_reporting_triangle, episodic_denominator,
# episodic_detection, episodic_cluster, episodic_cluster_case,
# episodic_detection_run and (for pre-renders) episodic_report_render. See
# R/db_app_write.R for the insert-only counterparts. Parameters
# throughout are one row's worth of columns for the table each function
# name identifies - see inst/sql/schema.sql for the exact column
# contracts (nullability, enums, defaults).

#' @keywords internal
#' @noRd
episodic_db_pathogen_config_load <- function(con, pathogen_config) {
  for (i in seq_len(nrow(pathogen_config))) {
    row <- pathogen_config[i, ]
    existing <- episodic_db_pathogen_config_get(con, row$pathogen)
    if (is.null(existing)) {
      DBI::dbExecute(
        con,
        "INSERT INTO episodic_pathogen_config
          (pathogen, episode_days, incub_min_days, incub_max_days, case_free_days,
           cooldown_days, rt_applicable, si_mean_days, si_sd_days, si_dist,
           mem_applicable, severity_weight, source_ref)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          row$pathogen,
          row$episode_days,
          row$incub_min_days,
          row$incub_max_days,
          row$case_free_days,
          row$cooldown_days,
          as.integer(row$rt_applicable),
          row$si_mean_days,
          row$si_sd_days,
          row$si_dist,
          as.integer(row$mem_applicable),
          row$severity_weight,
          row$source_ref
        )
      )
    } else {
      DBI::dbExecute(
        con,
        "UPDATE episodic_pathogen_config SET
          episode_days = ?, incub_min_days = ?, incub_max_days = ?, case_free_days = ?,
          cooldown_days = ?, rt_applicable = ?, si_mean_days = ?, si_sd_days = ?,
          si_dist = ?, mem_applicable = ?, severity_weight = ?, source_ref = ?
         WHERE pathogen = ?",
        params = list(
          row$episode_days,
          row$incub_min_days,
          row$incub_max_days,
          row$case_free_days,
          row$cooldown_days,
          as.integer(row$rt_applicable),
          row$si_mean_days,
          row$si_sd_days,
          row$si_dist,
          as.integer(row$mem_applicable),
          row$severity_weight,
          row$source_ref,
          row$pathogen
        )
      )
    }
  }
  invisible(NULL)
}

#' @param institution_key,display_name,institution_type,municipality,is_monitored
#'   Columns of `episodic_institution`.
#' @return The `institution_id` of the inserted or existing row.
#' @keywords internal
#' @noRd
episodic_db_institution_upsert <- function(
    con,
    institution_key,
    display_name,
    institution_type,
    care_line,
    municipality = NA,
    pc = NA,
    n_beds = NA,
    is_monitored = FALSE) {
  existing <- episodic_db_institution_get(con, institution_key)
  if (!is.null(existing)) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_institution SET display_name = ?, institution_type = ?, care_line = ?,
        municipality = ?, pc = ?, n_beds = ?, is_monitored = ? WHERE institution_key = ?",
      params = list(
        display_name,
        institution_type,
        care_line,
        municipality,
        pc,
        n_beds,
        as.integer(is_monitored),
        institution_key
      )
    )
    return(existing$institution_id)
  }
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_institution
      (institution_key, display_name, institution_type, care_line, municipality, pc,
       n_beds, is_monitored, is_active)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1)",
    params = list(
      institution_key,
      display_name,
      institution_type,
      care_line,
      municipality,
      pc,
      n_beds,
      as.integer(is_monitored)
    )
  )
  episodic_db_last_insert_id(con)
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
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_institution_activity WHERE institution_id = ? AND period_start = ?",
    params = list(institution_id, period_start)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_institution_activity SET period_end = ?, patient_days = ?, admissions = ?,
        n_beds = ?, source = ? WHERE institution_id = ? AND period_start = ?",
      params = list(
        period_end,
        patient_days,
        admissions,
        n_beds,
        source,
        institution_id,
        period_start
      )
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_institution_activity
        (institution_id, period_start, period_end, patient_days, admissions, n_beds, source)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(
        institution_id,
        period_start,
        period_end,
        patient_days,
        admissions,
        n_beds,
        source
      )
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
  existing <- episodic_db_stream_get(con, stream_key)
  if (!is.null(existing)) {
    first_seen <- min(existing$first_seen, observed_date)
    last_seen <- max(existing$last_seen, observed_date)
    DBI::dbExecute(
      con,
      "UPDATE episodic_stream SET first_seen = ?, last_seen = ? WHERE stream_key = ?",
      params = list(first_seen, last_seen, stream_key)
    )
    return(existing$stream_id)
  }
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_stream
      (stream_key, level, pathogen, care_line, region_code, institution_id,
       ward, denominator, severity_weight, is_active, first_seen, last_seen, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?, ?)",
    params = list(
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
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_case_insert_new <- function(con, cases, run_id) {
  n_inserted <- 0L
  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    existing <- DBI::dbGetQuery(
      con,
      "SELECT 1 FROM episodic_case WHERE source_key = ?",
      params = list(row$source_key)
    )
    if (nrow(existing) > 0) {
      next
    }
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_case
        (source_key, patient_key, sample_date, receipt_date, pathogen,
         care_line, institution_id, ward, specialism, pc, sex, age, first_seen_run)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        row$source_key,
        row$patient_key,
        row$sample_date,
        row$receipt_date,
        row$pathogen,
        row$care_line,
        row$institution_id,
        row$ward,
        row$specialism,
        row$pc,
        row$sex,
        row$age,
        run_id
      )
    )
    n_inserted <- n_inserted + 1L
  }
  n_inserted
}

#' @keywords internal
#' @noRd
episodic_db_reporting_triangle_upsert <- function(
    con,
    stream_id,
    sample_date,
    run_date,
    n_cases) {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_reporting_triangle WHERE stream_id = ? AND sample_date = ? AND run_date = ?",
    params = list(stream_id, sample_date, run_date)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_reporting_triangle SET n_cases = ?
       WHERE stream_id = ? AND sample_date = ? AND run_date = ?",
      params = list(n_cases, stream_id, sample_date, run_date)
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_reporting_triangle (stream_id, sample_date, run_date, n_cases)
       VALUES (?, ?, ?, ?)",
      params = list(stream_id, sample_date, run_date, n_cases)
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_denominator_upsert <- function(
    con,
    pathogen,
    sample_date,
    care_line,
    area_code = NA,
    n_tests) {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_denominator
     WHERE pathogen = ? AND sample_date = ? AND care_line = ?
       AND (area_code = ? OR (area_code IS NULL AND ? IS NULL))",
    params = list(pathogen, sample_date, care_line, area_code, area_code)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_denominator SET n_tests = ?
       WHERE pathogen = ? AND sample_date = ? AND care_line = ?
         AND (area_code = ? OR (area_code IS NULL AND ? IS NULL))",
      params = list(
        n_tests,
        pathogen,
        sample_date,
        care_line,
        area_code,
        area_code
      )
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_denominator (pathogen, sample_date, care_line, area_code, n_tests)
       VALUES (?, ?, ?, ?, ?)",
      params = list(pathogen, sample_date, care_line, area_code, n_tests)
    )
  }
  invisible(NULL)
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
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_stream_trend WHERE stream_id = ? AND week_start = ?",
    params = list(stream_id, week_start)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_stream_trend SET n_cases = ?, expected = ?, upperbound = ?
       WHERE stream_id = ? AND week_start = ?",
      params = list(n_cases, expected, upperbound, stream_id, week_start)
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_stream_trend (stream_id, week_start, n_cases, expected, upperbound)
       VALUES (?, ?, ?, ?, ?)",
      params = list(stream_id, week_start, n_cases, expected, upperbound)
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
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_detection
      (run_id, stream_id, cluster_id, detector, first_day, last_day, n_cases, expected,
       upperbound, params, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
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
  )
  episodic_db_last_insert_id(con)
}

#' @keywords internal
#' @noRd
episodic_db_detection_set_cluster <- function(con, detection_id, cluster_id) {
  DBI::dbExecute(
    con,
    "UPDATE episodic_detection SET cluster_id = ? WHERE detection_id = ?",
    params = list(cluster_id, detection_id)
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
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_cluster
      (stream_id, first_day, last_day, n_cases, expected, excess, ratio, priority_score,
       detector_agreement, opened_at, last_detected_run, runs_since_detected,
       changed_since_assessment, suppressed_by, merged_into)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, 0, NULL, NULL)",
    params = list(
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
    DBI::dbExecute(
      con,
      "UPDATE episodic_cluster SET first_day = ?, last_day = ?, n_cases = ?, expected = ?,
        excess = ?, ratio = ?, priority_score = ?, detector_agreement = ?,
        last_detected_run = ?, runs_since_detected = 0 WHERE cluster_id = ?",
      params = list(
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
    )
  } else {
    DBI::dbExecute(
      con,
      "UPDATE episodic_cluster SET first_day = ?, last_day = ?, n_cases = ?, expected = ?,
        excess = ?, ratio = ?, priority_score = ?, detector_agreement = ?,
        last_detected_run = ?, runs_since_detected = 0, changed_since_assessment = ?
       WHERE cluster_id = ?",
      params = list(
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
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_increment_runs_since_detected <- function(con, cluster_id) {
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET runs_since_detected = runs_since_detected + 1 WHERE cluster_id = ?",
    params = list(cluster_id)
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
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET suppressed_by = ? WHERE cluster_id = ?",
    params = list(
      if (is.na(suppressed_by)) NA_integer_ else as.integer(suppressed_by),
      cluster_id
    )
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_set_merged_into <- function(con, cluster_id, merged_into) {
  DBI::dbExecute(
    con,
    "UPDATE episodic_cluster SET merged_into = ? WHERE cluster_id = ?",
    params = list(merged_into, cluster_id)
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_cluster_case_link <- function(con, cluster_id, case_id) {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episodic_cluster_case WHERE cluster_id = ? AND case_id = ?",
    params = list(cluster_id, case_id)
  )
  if (nrow(existing) == 0) {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_cluster_case (cluster_id, case_id) VALUES (?, ?)",
      params = list(cluster_id, case_id)
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_db_run_start <- function(con, host, account, attempt_no = 1L) {
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_detection_run (host, account, started_at, status, attempt_no)
     VALUES (?, ?, ?, 'running', ?)",
    params = list(host, account, episodic_now(), attempt_no)
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
  DBI::dbExecute(
    con,
    "UPDATE episodic_detection_run SET finished_at = ?, status = ?, n_streams = ?,
      n_detections = ?, n_signals_new = ?, n_signals_updated = ?,
      n_cases_supplied = ?, n_cases_deduplicated = ?, n_cases_inserted = ?,
      n_denominators_written = ?, n_activity_supplied = ?, n_activity_written = ?,
      n_activity_skipped = ?, code_version = ?,
      pkg_versions = ?, config_hash = ?, config_snapshot = ?, error_text = ?
     WHERE run_id = ?",
    params = list(
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
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episodic_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS0Z", tz = "UTC")
}
