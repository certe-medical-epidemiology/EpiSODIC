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

# Test helpers: an in-memory-ish (temp file) EpiSODIC database, and small
# fixtures used across several test files.

# Caller is responsible for DBI::dbDisconnect(con); each call gets its own
# fresh temp-file database, so leaving one open across a test does not leak
# into other tests.
episodic_test_db <- function() {
  path <- tempfile(fileext = ".sqlite")
  episodic_db_create(path)
}

# For a test that wants the database file but opens its own connection, or
# hands the path to something that does. episodic_db_create() returns a
# live connection, so calling it for its side effect alone orphans one,
# which RSQLite then complains about whenever it is garbage-collected -
# long after the test that caused it has finished.
episodic_test_db_path <- function() {
  path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(path))
  path
}

# Locate the package's own R/ sources, for the handful of invariants that
# are load-bearing but not observable at runtime (see
# test-insert_only.R and test-db_write_reentrancy.R). An installed
# package's own system.file("R", ...) is a real directory but holds only
# the compiled lazy-load database (EpiSODIC.rdb/.rdx), not individual .R
# source files - so existence alone is not enough to trust a candidate;
# each must actually contain recognisable sources.
episodic_test_r_source_dir <- function() {
  candidates <- c(system.file("R", package = "EpiSODIC"), "R", "../../R")
  for (d in candidates) {
    if (
      nzchar(d) && dir.exists(d) && file.exists(file.path(d, "db_app_write.R"))
    ) {
      return(d)
    }
  }
  NA_character_
}


# Seed a stream's completion curve by creating the runs and case arrivals
# that produce it. Replaces the old approach of writing rows straight into
# episodic_reporting_triangle, which no longer exists - the curve is now
# derived from episodic_case.first_seen_run and episodic_detection_run.
#
# `seen_by_lag` is a named list mapping lag (days after `sample_date`, as a
# character) to the cumulative number of cases visible at that lag. Cases
# are created so those cumulative totals come out exactly.
episodic_test_seed_completion <- function(
  con,
  stream_id,
  sample_date,
  seen_by_lag
) {
  # The cases have to satisfy the stream's own membership rule, or
  # episodic_db_cases_for_stream_id() will not see them and the curve
  # comes back empty.
  stream <- DBI::dbGetQuery(
    con,
    "SELECT pathogen, institution_id, ward FROM episodic_stream WHERE stream_id = ?",
    params = list(stream_id)
  )
  pathogen <- stream$pathogen[1]
  institution_id <- stream$institution_id[1]
  ward <- stream$ward[1]

  lags <- as.integer(names(seen_by_lag))
  totals <- as.integer(unlist(seen_by_lag))
  ord <- order(lags)
  lags <- lags[ord]
  totals <- totals[ord]
  arrivals <- diff(c(0L, totals)) # how many first appear at each lag

  n <- 0L
  for (i in seq_along(lags)) {
    run_date <- as.character(as.Date(sample_date) + lags[i])
    run_id <- episodic_db_run_start(con, "h", "a", run_date = run_date)
    episodic_db_run_finish(con, run_id, status = "success")
    for (k in seq_len(max(0L, arrivals[i]))) {
      n <- n + 1L
      key <- sprintf("SEED-%d-%d", stream_id, n)
      params <- list(
        key,
        key,
        key,
        sample_date,
        pathogen,
        institution_id,
        ward,
        run_id
      )
      DBI::dbExecute(
        con,
        "INSERT INTO episodic_case (source_key, lab_number, patient_key,
           sample_date, pathogen, care_line, institution_id, ward, first_seen_run)
         VALUES (?, ?, ?, ?, ?, 'second', ?, ?, ?)",
        params = params
      )
    }
  }
  invisible(n)
}

episodic_test_pathogen_config <- function() {
  path <- system.file("config", "episodic_default_pathogen_config.csv", package = "EpiSODIC")
  if (identical(path, "")) {
    path <- file.path("inst", "config", "episodic_default_pathogen_config.csv")
  }
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

episodic_test_config <- function() {
  episodic_config_resolve(NA)
}

# One institution, for a test that needs a case to belong somewhere.
# Institutions are written by the batch in episodic_institutions_resolve(),
# which is also where the source key is hashed - so a test names the key it
# would have supplied in the case data, not a hash of it, and cannot drift
# from how a run does it.
#
# is_monitored is derived there from the institution type, as in production.
# A test that needs to state it outright - because it hands the same value
# to a detector as well, or wants a combination a run would not produce -
# passes it, and gets one UPDATE for it.
episodic_test_institution <- function(
  con,
  key,
  display_name = "Test Hospital",
  institution_type = "hospital",
  care_line = "second",
  municipality = NA_character_,
  is_monitored = NULL
) {
  ids <- episodic_institutions_resolve(
    con,
    data.frame(
      institution_key = key,
      institution_display_name = display_name,
      institution_type = institution_type,
      care_line = care_line,
      municipality = municipality,
      stringsAsFactors = FALSE
    )
  )
  institution_id <- unname(ids[[1]])
  if (!is.null(is_monitored)) {
    params <- list(as.integer(as.logical(is_monitored)), institution_id)
    DBI::dbExecute(
      con,
      "UPDATE episodic_institution SET is_monitored = ? WHERE institution_id = ?",
      params = params
    )
  }
  institution_id
}

# A single ward-level Norovirus cluster with 6 cases across 3 PCs, used by
# both the read-model tests and the dossier UI tests.
app_read_setup <- function() {
  con <- episodic_test_db()
  institution_id <- episodic_test_institution(con, "hosp-app-read")
  episodic_db_institution_activity_upsert(
    con,
    institution_id = institution_id,
    period_start = "2025-01-01",
    period_end = "2025-01-31",
    patient_days = 1000
  )
  pathogen_config <- data.frame(
    pathogen = "Norovirus",
    episode_days = 14,
    incub_min_days = 0.5,
    incub_max_days = 3,
    case_free_days = 14,
    cooldown_days = 14,
    rt_applicable = 1,
    si_mean_days = 3,
    si_sd_days = 1.5,
    si_dist = "gamma",
    mem_applicable = 0,
    severity_weight = 0.6,
    source_ref = NA,
    stringsAsFactors = FALSE
  )
  episodic_db_pathogen_config_load(con, pathogen_config)

  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_ward",
      "Norovirus",
      institution_id = institution_id,
      ward = "B4"
    ),
    level = "pathogen_ward",
    pathogen = "Norovirus",
    care_line = "second",
    institution_id = institution_id,
    ward = "B4",
    denominator = "patient_days",
    observed_date = "2025-01-15"
  )

  run_id <- episodic_db_run_start(con, "host", "account")

  cases <- data.frame(
    source_key = sprintf("K%d", 1:6),
    lab_number = sprintf("LAB-K%d", 1:6),
    patient_key = sprintf("P%d", 1:6),
    sample_date = c(
      "2025-01-10",
      "2025-01-11",
      "2025-01-12",
      "2025-01-12",
      "2025-01-13",
      "2025-01-13"
    ),
    receipt_date = c(
      "2025-01-10",
      "2025-01-11",
      "2025-01-12",
      "2025-01-12",
      "2025-01-13",
      "2025-01-13"
    ),
    pathogen = "Norovirus",
    care_line = "second",
    institution_id = institution_id,
    ward = "B4",
    specialism = "Interne",
    pc = c("9711", "9711", "9711", "9712", "9713", "9711"),
    sex = c("M", "F", "M", "F", "M", "F"),
    age = c(82, 79, 88, 45, 30, 76),
    first_seen_run = run_id,
    stringsAsFactors = FALSE
  )
  episodic_db_case_insert_new(con, cases, run_id)

  cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = stream_id,
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    n_cases = 6,
    expected = 0.8,
    excess = 5.2,
    ratio = 7.5,
    priority_score = 91,
    detector_agreement = 1,
    run_id = run_id
  )
  all_cases <- DBI::dbGetQuery(con, "SELECT case_id FROM episodic_case")
  episodic_db_cluster_case_link_many(con, cluster_id, all_cases$case_id)
  detection_id <- episodic_db_detection_insert(
    con,
    run_id = run_id,
    stream_id = stream_id,
    detector = "same_place",
    first_day = "2025-01-10",
    last_day = "2025-01-13",
    n_cases = 6,
    params_json = "{}"
  )
  episodic_db_detection_set_cluster(con, detection_id, cluster_id)

  episodic_db_run_finish(
    con,
    run_id,
    status = "success",
    n_streams = 1,
    n_detections = 1,
    config_hash = strrep("a", 40),
    config_snapshot = '{"reconciliation":{"close_after_runs":14}}'
  )

  list(
    con = con,
    institution_id = institution_id,
    stream_id = stream_id,
    cluster_id = cluster_id,
    run_id = run_id
  )
}
