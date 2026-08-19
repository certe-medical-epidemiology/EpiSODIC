#' Cron-side writers
#'
#' The cron owns the facts (ARCHITECTURE.md section 5.0) and may upsert.
#' These functions are the only place in the package that write to
#' `episode_stream`, `episode_institution`, `episode_institution_activity`,
#' `episode_case`, `episode_reporting_triangle`, `episode_denominator`,
#' `episode_detection`, `episode_cluster`,
#' `episode_cluster_case`, `episode_detection_run` and (for pre-renders)
#' `episode_report_render`. See `R/db_app_write.R` for the insert-only
#' counterparts.
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen_config A data frame matching `inst/config/pathogen_config.csv`.
#' @param institution_id An `episode_institution` id.
#' @param period_start,period_end Activity period bounds (dates).
#' @param patient_days,admissions,n_beds Activity counts, or `NA`.
#' @param source Free-text provenance, or `NA`.
#' @param stream_key A 40-character `stream_key`.
#' @param level One of the five lattice levels.
#' @param pathogen The raw lab-provided pathogen string (see `QUESTIONS.md`
#'   item 22); used verbatim as the stream's identity, no taxonomy resolved.
#' @param care_line One of `"first"`, `"second"`, `"other"`, `"unknown"`, or `NA`.
#' @param region_code A region code, or `NA`.
#' @param pc4 A PC4 postcode, or `NA`.
#' @param ward A ward, for `pathogen_ward` streams, or `NA`.
#' @param denominator One of `"none"`, `"tests"`, `"population"`, `"patient_days"`.
#' @param severity_weight A severity weight, 0-1.
#' @param observed_date The date this stream was observed on, updates `first_seen`/`last_seen`.
#' @param cases A data frame of cases to insert (`episode_ingest_run()`'s deduplicated batch).
#' @param run_id A `run_id`.
#' @param stream_id A `stream_id`.
#' @param sample_date,run_date Reporting-triangle dates.
#' @param n_cases A case count.
#' @param n_tests A test count, for the optional positivity metadata table.
#' @param area_code An area code, for the optional positivity metadata table.
#' @param detector One of the detector enum values.
#' @param first_day,last_day A detection or cluster interval.
#' @param expected,upperbound Statistical detector output, or `NA`.
#' @param params_json JSON-serialised detector attributes.
#' @param cluster_id A `cluster_id`, or `NA`.
#' @param detection_id A `detection_id`.
#' @param excess,ratio Cluster statistics, or `NA`.
#' @param priority_score A priority score, 0-100.
#' @param detector_agreement Count of distinct detectors that fired.
#' @param changed_since_assessment Logical, or `NULL` to leave unchanged.
#' @param merged_into The surviving `cluster_id` this cluster merged into.
#' @param case_id A `case_id`.
#' @param host,account Recorded on `episode_detection_run`.
#' @param attempt_no The run's attempt number.
#' @param status One of `episode_detection_run.status`.
#' @param n_streams,n_detections,n_signals_new,n_signals_updated Run summary counts.
#' @param code_version The installed EpiSODE version.
#' @param pkg_versions JSON-serialised package versions.
#' @param config_hash,config_snapshot The resolved configuration's hash and snapshot.
#' @param error_text Error text for a failed run, or `NA`.
#' @name db_cron_write
NULL

#' @rdname db_cron_write
#' @export
episode_db_pathogen_config_load <- function(con, pathogen_config) {
  for (i in seq_len(nrow(pathogen_config))) {
    row <- pathogen_config[i, ]
    existing <- episode_db_pathogen_config_get(con, row$pathogen)
    if (is.null(existing)) {
      DBI::dbExecute(
        con,
        "INSERT INTO episode_pathogen_config
          (pathogen, episode_days, incub_min_days, incub_max_days, case_free_days,
           cooldown_days, rt_applicable, si_mean_days, si_sd_days, si_dist,
           mem_applicable, severity_weight, source_ref)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          row$pathogen, row$episode_days, row$incub_min_days, row$incub_max_days,
          row$case_free_days, row$cooldown_days, as.integer(row$rt_applicable),
          row$si_mean_days, row$si_sd_days, row$si_dist,
          as.integer(row$mem_applicable), row$severity_weight, row$source_ref
        )
      )
    } else {
      DBI::dbExecute(
        con,
        "UPDATE episode_pathogen_config SET
          episode_days = ?, incub_min_days = ?, incub_max_days = ?, case_free_days = ?,
          cooldown_days = ?, rt_applicable = ?, si_mean_days = ?, si_sd_days = ?,
          si_dist = ?, mem_applicable = ?, severity_weight = ?, source_ref = ?
         WHERE pathogen = ?",
        params = list(
          row$episode_days, row$incub_min_days, row$incub_max_days, row$case_free_days,
          row$cooldown_days, as.integer(row$rt_applicable), row$si_mean_days, row$si_sd_days,
          row$si_dist, as.integer(row$mem_applicable), row$severity_weight, row$source_ref,
          row$pathogen
        )
      )
    }
  }
  invisible(NULL)
}

#' @rdname db_cron_write
#' @param institution_key,display_name,institution_type,municipality,is_monitored
#'   Columns of `episode_institution`, see ARCHITECTURE.md section 5.4.1.
#' @return The `institution_id` of the inserted or existing row.
#' @export
episode_db_institution_upsert <- function(con, institution_key, display_name, institution_type,
                                           care_line, municipality = NA, pc4 = NA, n_beds = NA,
                                           is_monitored = FALSE) {
  existing <- episode_db_institution_get(con, institution_key)
  if (!is.null(existing)) {
    DBI::dbExecute(
      con,
      "UPDATE episode_institution SET display_name = ?, institution_type = ?, care_line = ?,
        municipality = ?, pc4 = ?, n_beds = ?, is_monitored = ? WHERE institution_key = ?",
      params = list(display_name, institution_type, care_line, municipality, pc4, n_beds,
                     as.integer(is_monitored), institution_key)
    )
    return(existing$institution_id)
  }
  DBI::dbExecute(
    con,
    "INSERT INTO episode_institution
      (institution_key, display_name, institution_type, care_line, municipality, pc4,
       n_beds, is_monitored, is_active)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1)",
    params = list(institution_key, display_name, institution_type, care_line, municipality,
                  pc4, n_beds, as.integer(is_monitored))
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_cron_write
#' @export
episode_db_institution_activity_upsert <- function(con, institution_id, period_start, period_end,
                                                     patient_days = NA, admissions = NA,
                                                     n_beds = NA, source = NA) {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episode_institution_activity WHERE institution_id = ? AND period_start = ?",
    params = list(institution_id, period_start)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episode_institution_activity SET period_end = ?, patient_days = ?, admissions = ?,
        n_beds = ?, source = ? WHERE institution_id = ? AND period_start = ?",
      params = list(period_end, patient_days, admissions, n_beds, source, institution_id, period_start)
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episode_institution_activity
        (institution_id, period_start, period_end, patient_days, admissions, n_beds, source)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(institution_id, period_start, period_end, patient_days, admissions, n_beds, source)
    )
  }
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_stream_upsert <- function(con, stream_key, level, pathogen,
                                      care_line = NA, region_code = NA, institution_id = NA,
                                      ward = NA, denominator = "none", severity_weight = 1.00,
                                      observed_date) {
  existing <- episode_db_stream_get(con, stream_key)
  if (!is.null(existing)) {
    first_seen <- min(existing$first_seen, observed_date)
    last_seen <- max(existing$last_seen, observed_date)
    DBI::dbExecute(
      con,
      "UPDATE episode_stream SET first_seen = ?, last_seen = ? WHERE stream_key = ?",
      params = list(first_seen, last_seen, stream_key)
    )
    return(existing$stream_id)
  }
  DBI::dbExecute(
    con,
    "INSERT INTO episode_stream
      (stream_key, level, pathogen, care_line, region_code, institution_id,
       ward, denominator, severity_weight, is_active, first_seen, last_seen, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?, ?)",
    params = list(stream_key, level, pathogen, care_line, region_code,
                  institution_id, ward, denominator, severity_weight, observed_date, observed_date,
                  episode_now())
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_cron_write
#' @export
episode_db_case_insert_new <- function(con, cases, run_id) {
  n_inserted <- 0L
  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    existing <- DBI::dbGetQuery(
      con, "SELECT 1 FROM episode_case WHERE source_key = ?", params = list(row$source_key)
    )
    if (nrow(existing) > 0) next
    DBI::dbExecute(
      con,
      "INSERT INTO episode_case
        (source_key, patient_key, sample_date, receipt_date, pathogen,
         care_line, institution_id, ward, specialism, pc4, sex, age, first_seen_run)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        row$source_key, row$patient_key, row$sample_date, row$receipt_date, row$pathogen,
        row$care_line, row$institution_id, row$ward,
        row$specialism, row$pc4, row$sex, row$age, run_id
      )
    )
    n_inserted <- n_inserted + 1L
  }
  n_inserted
}

#' @rdname db_cron_write
#' @export
episode_db_reporting_triangle_upsert <- function(con, stream_id, sample_date, run_date, n_cases) {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episode_reporting_triangle WHERE stream_id = ? AND sample_date = ? AND run_date = ?",
    params = list(stream_id, sample_date, run_date)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episode_reporting_triangle SET n_cases = ?
       WHERE stream_id = ? AND sample_date = ? AND run_date = ?",
      params = list(n_cases, stream_id, sample_date, run_date)
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episode_reporting_triangle (stream_id, sample_date, run_date, n_cases)
       VALUES (?, ?, ?, ?)",
      params = list(stream_id, sample_date, run_date, n_cases)
    )
  }
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_denominator_upsert <- function(con, pathogen, sample_date, care_line, area_code = NA,
                                           n_tests) {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT 1 FROM episode_denominator
     WHERE pathogen = ? AND sample_date = ? AND care_line = ? AND area_code IS ?",
    params = list(pathogen, sample_date, care_line, area_code)
  )
  if (nrow(existing) > 0) {
    DBI::dbExecute(
      con,
      "UPDATE episode_denominator SET n_tests = ?
       WHERE pathogen = ? AND sample_date = ? AND care_line = ? AND area_code IS ?",
      params = list(n_tests, pathogen, sample_date, care_line, area_code)
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO episode_denominator (pathogen, sample_date, care_line, area_code, n_tests)
       VALUES (?, ?, ?, ?, ?)",
      params = list(pathogen, sample_date, care_line, area_code, n_tests)
    )
  }
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_detection_insert <- function(con, run_id, stream_id, detector, first_day, last_day,
                                         n_cases, expected = NA, upperbound = NA, params_json,
                                         cluster_id = NA) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_detection
      (run_id, stream_id, cluster_id, detector, first_day, last_day, n_cases, expected,
       upperbound, params, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(run_id, stream_id, cluster_id, detector, first_day, last_day, n_cases,
                  expected, upperbound, params_json, episode_now())
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_cron_write
#' @export
episode_db_detection_set_cluster <- function(con, detection_id, cluster_id) {
  DBI::dbExecute(
    con, "UPDATE episode_detection SET cluster_id = ? WHERE detection_id = ?",
    params = list(cluster_id, detection_id)
  )
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_cluster_insert <- function(con, stream_id, first_day, last_day, n_cases,
                                       expected = NA, excess = NA, ratio = NA,
                                       priority_score, detector_agreement, run_id) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_cluster
      (stream_id, first_day, last_day, n_cases, expected, excess, ratio, priority_score,
       detector_agreement, opened_at, last_detected_run, runs_since_detected,
       changed_since_assessment, suppressed_by, merged_into)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, 0, NULL, NULL)",
    params = list(stream_id, first_day, last_day, n_cases, expected, excess, ratio,
                  priority_score, detector_agreement, episode_now(), run_id)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_cron_write
#' @export
episode_db_cluster_update <- function(con, cluster_id, first_day, last_day, n_cases,
                                       expected = NA, excess = NA, ratio = NA,
                                       priority_score, detector_agreement, run_id,
                                       changed_since_assessment = NULL) {
  if (is.null(changed_since_assessment)) {
    DBI::dbExecute(
      con,
      "UPDATE episode_cluster SET first_day = ?, last_day = ?, n_cases = ?, expected = ?,
        excess = ?, ratio = ?, priority_score = ?, detector_agreement = ?,
        last_detected_run = ?, runs_since_detected = 0 WHERE cluster_id = ?",
      params = list(first_day, last_day, n_cases, expected, excess, ratio, priority_score,
                    detector_agreement, run_id, cluster_id)
    )
  } else {
    DBI::dbExecute(
      con,
      "UPDATE episode_cluster SET first_day = ?, last_day = ?, n_cases = ?, expected = ?,
        excess = ?, ratio = ?, priority_score = ?, detector_agreement = ?,
        last_detected_run = ?, runs_since_detected = 0, changed_since_assessment = ?
       WHERE cluster_id = ?",
      params = list(first_day, last_day, n_cases, expected, excess, ratio, priority_score,
                    detector_agreement, run_id, as.integer(changed_since_assessment), cluster_id)
    )
  }
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_cluster_increment_runs_since_detected <- function(con, cluster_id) {
  DBI::dbExecute(
    con,
    "UPDATE episode_cluster SET runs_since_detected = runs_since_detected + 1 WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_cluster_set_merged_into <- function(con, cluster_id, merged_into) {
  DBI::dbExecute(
    con, "UPDATE episode_cluster SET merged_into = ? WHERE cluster_id = ?",
    params = list(merged_into, cluster_id)
  )
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_cluster_case_link <- function(con, cluster_id, case_id) {
  existing <- DBI::dbGetQuery(
    con, "SELECT 1 FROM episode_cluster_case WHERE cluster_id = ? AND case_id = ?",
    params = list(cluster_id, case_id)
  )
  if (nrow(existing) == 0) {
    DBI::dbExecute(
      con, "INSERT INTO episode_cluster_case (cluster_id, case_id) VALUES (?, ?)",
      params = list(cluster_id, case_id)
    )
  }
  invisible(NULL)
}

#' @rdname db_cron_write
#' @export
episode_db_run_start <- function(con, host, account, attempt_no = 1L) {
  DBI::dbExecute(
    con,
    "INSERT INTO episode_detection_run (host, account, started_at, status, attempt_no)
     VALUES (?, ?, ?, 'running', ?)",
    params = list(host, account, episode_now(), attempt_no)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

#' @rdname db_cron_write
#' @export
episode_db_run_finish <- function(con, run_id, status, n_streams = NA, n_detections = NA,
                                   n_signals_new = NA, n_signals_updated = NA,
                                   code_version = NA, pkg_versions = NA, config_hash = NA,
                                   config_snapshot = NA, error_text = NA) {
  DBI::dbExecute(
    con,
    "UPDATE episode_detection_run SET finished_at = ?, status = ?, n_streams = ?,
      n_detections = ?, n_signals_new = ?, n_signals_updated = ?, code_version = ?,
      pkg_versions = ?, config_hash = ?, config_snapshot = ?, error_text = ?
     WHERE run_id = ?",
    params = list(episode_now(), status, n_streams, n_detections, n_signals_new,
                  n_signals_updated, code_version, pkg_versions, config_hash, config_snapshot,
                  error_text, run_id)
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
episode_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS0Z", tz = "UTC")
}
