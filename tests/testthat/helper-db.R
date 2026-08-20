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

# Test helpers: an in-memory-ish (temp file) EpiSODIC database, and small
# fixtures used across several test files.

# Caller is responsible for DBI::dbDisconnect(con); each call gets its own
# fresh temp-file database, so leaving one open across a test does not leak
# into other tests.
episode_test_db <- function() {
  path <- tempfile(fileext = ".sqlite")
  episode_db_create(path)
}

episode_test_pathogen_config <- function() {
  path <- system.file("config", "pathogen_config.csv", package = "EpiSODIC")
  if (identical(path, "")) path <- file.path("inst", "config", "pathogen_config.csv")
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

episode_test_config <- function() {
  episode_config_resolve(NA)
}

# A single ward-level Norovirus cluster with 6 cases across 3 PCs, used by
# both the read-model tests and the dossier UI tests.
app_read_setup <- function() {
  con <- episode_test_db()
  institution_id <- episode_db_institution_upsert(
    con, institution_key = digest::digest("hosp-app-read", algo = "sha1", serialize = FALSE),
    display_name = "Test Hospital", institution_type = "hospital", care_line = "second",
    is_monitored = TRUE
  )
  episode_db_institution_activity_upsert(
    con, institution_id = institution_id, period_start = "2025-01-01", period_end = "2025-01-31",
    patient_days = 1000
  )
  pathogen_config <- data.frame(
    pathogen = "Norovirus", episode_days = 14, incub_min_days = 0.5, incub_max_days = 3,
    case_free_days = 14, cooldown_days = 14, rt_applicable = 1, si_mean_days = 3, si_sd_days = 1.5,
    si_dist = "gamma", mem_applicable = 0, severity_weight = 0.6, source_ref = NA,
    stringsAsFactors = FALSE
  )
  episode_db_pathogen_config_load(con, pathogen_config)

  stream_id <- episode_db_stream_upsert(
    con, stream_key = episode_stream_key("pathogen_ward", "Norovirus", institution_id = institution_id, ward = "B4"),
    level = "pathogen_ward", pathogen = "Norovirus", care_line = "second",
    institution_id = institution_id, ward = "B4", denominator = "patient_days",
    observed_date = "2025-01-15"
  )

  run_id <- episode_db_run_start(con, "host", "account")

  cases <- data.frame(
    source_key = sprintf("K%d", 1:6),
    patient_key = sprintf("P%d", 1:6),
    sample_date = c("2025-01-10", "2025-01-11", "2025-01-12", "2025-01-12", "2025-01-13", "2025-01-13"),
    receipt_date = c("2025-01-10", "2025-01-11", "2025-01-12", "2025-01-12", "2025-01-13", "2025-01-13"),
    pathogen = "Norovirus", care_line = "second", institution_id = institution_id, ward = "B4",
    specialism = "Interne", pc = c("9711", "9711", "9711", "9712", "9713", "9711"),
    sex = c("M", "F", "M", "F", "M", "F"), age = c(82, 79, 88, 45, 30, 76),
    first_seen_run = run_id, stringsAsFactors = FALSE
  )
  episode_db_case_insert_new(con, cases, run_id)

  cluster_id <- episode_db_cluster_insert(
    con, stream_id = stream_id, first_day = "2025-01-10", last_day = "2025-01-13", n_cases = 6,
    expected = 0.8, excess = 5.2, ratio = 7.5, priority_score = 91, detector_agreement = 1, run_id = run_id
  )
  all_cases <- DBI::dbGetQuery(con, "SELECT case_id FROM episode_case")
  for (case_id in all_cases$case_id) {
    episode_db_cluster_case_link(con, cluster_id, case_id)
  }
  detection_id <- episode_db_detection_insert(
    con, run_id = run_id, stream_id = stream_id, detector = "same_place",
    first_day = "2025-01-10", last_day = "2025-01-13", n_cases = 6, params_json = "{}"
  )
  episode_db_detection_set_cluster(con, detection_id, cluster_id)

  episode_db_run_finish(con, run_id, status = "success", n_streams = 1, n_detections = 1,
                         config_hash = strrep("a", 40), config_snapshot = '{"reconciliation":{"close_after_runs":14}}')

  list(con = con, institution_id = institution_id, stream_id = stream_id, cluster_id = cluster_id, run_id = run_id)
}
