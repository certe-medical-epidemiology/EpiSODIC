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

# A line list of `per_day` cases a day from `from` to `to`, each reported
# 0 to 3 days after sampling, and whatever of it had been reported by
# `asof`: the truth a nowcast estimates, and what it is given.
nowcast_line_list <- function(from, to, per_day = 4, seed = 1) {
  set.seed(seed)
  days <- seq(as.Date(from), as.Date(to), by = "day")
  n <- stats::rpois(length(days), per_day)
  sample_date <- rep(days, n)
  data.frame(
    sample_date = sample_date,
    report_date = sample_date + sample(0:3, length(sample_date),
      replace = TRUE, prob = c(0.1, 0.4, 0.3, 0.2)
    )
  )
}

# A ward's cases, sampled daily and each loaded by the run on its
# reporting date, over daily runs: a stream with an open same_place
# cluster and a reporting history the nowcast can learn from.
#
# `pathogen_lines`, when given, is a pathogen configuration overlay (a
# header and rows) for the runs, so "Test pathogen" can be given the
# serial interval and offspring distribution of a transmissible one.
nowcast_cron_database <- function(config_lines = character(0),
                                  n_runs = 16L,
                                  path = tempfile(fileext = ".sqlite"),
                                  pathogen_lines = NULL) {
  pathogen_config_path <- NA
  if (!is.null(pathogen_lines)) {
    pathogen_config_path <- tempfile(fileext = ".csv")
    writeLines(pathogen_lines, pathogen_config_path)
  }
  config_path <- tempfile(fileext = ".yaml")
  writeLines(
    c(
      "forecast:",
      "  nowcast:",
      "    max_delay_days: 3",
      "    min_reported_cases: 10",
      "    n_samples: 100",
      config_lines
    ),
    config_path
  )
  truth <- nowcast_line_list("2025-01-01", "2025-01-31", per_day = 3, seed = 4)
  truth$source_key <- paste0("K", seq_len(nrow(truth)))
  as_cases <- function(rows) {
    data.frame(
      source_key = rows$source_key,
      lab_number = paste0("LAB-", rows$source_key),
      patient_key = paste0("P-", rows$source_key),
      sample_date = format(rows$sample_date),
      receipt_date = format(rows$sample_date),
      pathogen = "Test pathogen",
      care_line = "second",
      institution_key = "HOSP-01",
      institution_display_name = "Hospital",
      institution_type = "hospital",
      municipality = NA_character_,
      ward = "ICU",
      specialism = "Interne",
      pc = "9711",
      sex = "M",
      age = 40L,
      stringsAsFactors = FALSE
    )
  }
  # From the fourth day, so the first run already has cases to load.
  run_dates <- as.Date("2025-01-04") + seq_len(n_runs) - 1L
  log <- character(0)
  for (i in seq_along(run_dates)) {
    log <- c(log, testthat::capture_messages(episodic_run_cron(
      db_path = path,
      cases = as_cases(truth[truth$report_date <= run_dates[i], ]),
      episodic_config_path = config_path,
      pathogen_config_path = pathogen_config_path,
      run_date = run_dates[i]
    )))
  }
  list(
    path = path,
    config_path = c(config_path, if (!is.na(pathogen_config_path)) pathogen_config_path),
    log = log
  )
}

# "Test pathogen" as a transmissible pathogen with a norovirus-like serial
# interval, with or without an offspring distribution.
outbreak_end_pathogen_lines <- function(end_r = 0.8, end_k = 0.5) {
  c(
    paste0(
      "pathogen,episode_days,case_free_days,rt_applicable,si_mean_days,",
      "si_sd_days,si_dist,mem_mode,severity_weight,end_r,end_k"
    ),
    paste0(
      "Test pathogen,30,14,1,3.6,1.9,gamma,no,1,",
      if (is.na(end_r)) "" else end_r,
      ",",
      if (is.na(end_k)) "" else end_k
    )
  )
}
