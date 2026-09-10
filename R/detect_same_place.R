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

#' `same_place` detector
#'
#' Needs no baseline: *n* or more cases of the same pathogen at the same
#' place within *k* days. Inside hospitals the
#' rule runs on ward, the real transmission unit; everywhere else (long-term
#' care, out-of-hours services, general practice) it runs on institution,
#' since those places do not receive their own statistically-modelled
#' streams. The stream a hit belongs to is created here if it does not
#' already exist, via the same `episodic_db_stream_upsert()` used by lattice
#' enumeration, so `same_place` detections reconcile into the same
#' `episodic_stream`/`episodic_cluster` tables as every other detector.
#'
#' # The lookback window
#'
#' Only hits whose most recent case falls within
#' `config$same_place$lookback_days` of `run_date` are reported. Without
#' that bound this detector rescanned the entire case history on every
#' run and re-emitted every hit window it had ever found, which is wrong
#' in three separate ways and not merely wasteful: `episodic_detection`
#' grew by the whole historical hit count on every single run; each
#' re-emitted historical window matched its own long-settled cluster in
#' reconciliation, resetting `runs_since_detected` to zero, so
#' `reconciliation.close_after_runs` could never fire for any cluster
#' this detector had ever touched; and the run's own detection count -
#' the number the dashboard's activity screen reports - counted the
#' archive rather than the day. It is the same bound
#' `farrington.max_weeks_tested` places on the statistical detector, for
#' the same reason.
#'
#' The scan itself still runs over the full history up to `run_date`, so
#' a window is always assembled from every case that belongs to it; only
#' which windows are *reported* is bounded. Cases sampled after
#' `run_date` are not part of this run's history at all - see
#' `episodic_detector_cases_asof()`.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame of cases to scan, with `pathogen`,
#'   `institution_id`, `ward`, `sample_date`.
#' @param institutions A data frame from `episodic_db_institutions()`.
#' @param config The resolved configuration; uses `config$same_place`.
#' @param run_date The date to treat as "today", for the lookback window.
#' @return A data frame of detection records (`episodic_detection_record()`
#'   shape) plus a `stream_id` column, one row per hit, carrying what the
#'   lookback kept out of it (`episodic_detector_lookback_note()`).
#' @keywords internal
#' @noRd
episodic_detect_same_place <- function(con,
                                       cases,
                                       institutions,
                                       config,
                                       run_date = Sys.Date()) {
  if (!episodic_detector_enabled(config, "same_place")) {
    return(episodic_detection_none())
  }
  cases <- episodic_detector_cases_asof(cases, run_date)
  cases <- cases[!is.na(cases$institution_id), ]
  if (nrow(cases) == 0) {
    return(episodic_detection_none())
  }

  inst_type <- institutions$institution_type[match(
    cases$institution_id,
    institutions$institution_id
  )]
  is_hospital <- inst_type == "hospital" & !is.na(cases$ward)

  scans <- list()

  if (any(is_hospital, na.rm = TRUE)) {
    ward_cases <- cases[which(is_hospital), ]
    scans$ward <- episodic_same_place_scan(
      ward_cases,
      group_cols = c("pathogen", "institution_id", "ward"),
      config = config,
      stream_level = "pathogen_ward",
      con = con,
      run_date = run_date
    )
  }

  non_hospital <- cases[which(!is_hospital), ]
  if (nrow(non_hospital) > 0) {
    scans$institution <- episodic_same_place_scan(
      non_hospital,
      group_cols = c("pathogen", "institution_id"),
      config = config,
      stream_level = "pathogen_institution",
      con = con,
      run_date = run_date
    )
  }

  hits <- lapply(scans, function(s) s$records)
  hits <- hits[!vapply(hits, is.null, logical(1))]
  records <- if (length(hits) == 0) {
    episodic_detection_record(
      integer(0),
      character(0),
      character(0),
      character(0),
      integer(0)
    )
  } else {
    do.call(rbind, hits)
  }

  episodic_detector_lookback_note(
    records,
    dropped = sum(vapply(scans, function(s) s$dropped, integer(1))),
    cutoff = episodic_detector_lookback_cutoff(
      run_date,
      config$same_place$lookback_days
    ),
    lookback_days = config$same_place$lookback_days
  )
}

#' @keywords internal
#' @noRd
episodic_same_place_scan <- function(cases,
                                     group_cols,
                                     config,
                                     stream_level,
                                     con,
                                     run_date = Sys.Date()) {
  cutoff <- episodic_detector_lookback_cutoff(
    run_date,
    config$same_place$lookback_days
  )
  key_df <- cases[, group_cols, drop = FALSE]
  key_str <- do.call(paste, c(key_df, sep = "\r"))
  groups <- split(seq_len(nrow(cases)), key_str)

  records <- list()
  n_dropped <- 0L
  for (g in groups) {
    grp <- cases[g, ]
    pathogen <- grp$pathogen[1]
    rule <- episodic_same_place_rule(config, pathogen)
    dates <- sort(as.Date(grp$sample_date))

    windows <- episodic_same_place_hit_windows(
      dates,
      n = rule$n,
      k_days = rule$k_days
    )
    current <- episodic_detector_windows_within(windows, cutoff)
    n_dropped <- n_dropped + (length(windows) - length(current))
    windows <- current
    if (length(windows) == 0) {
      next
    }

    institution_id <- grp$institution_id[1]
    ward <- if ("ward" %in% group_cols) grp$ward[1] else NA
    # `same_place` is the detector responsible for exactly the streams
    # (LTC, out-of-hours, general practice) that never get one from
    # lattice enumeration - hardcoding NA here defeated the point of
    # `care_line` existing at the stream level at all, for both those and
    # its hospital ward streams. Taken from the group's own cases, same
    # as `episodic_lattice_upsert_group()` does.
    care_line <- if ("care_line" %in% names(grp)) grp$care_line[1] else NA

    stream_key <- episodic_stream_key(
      level = stream_level,
      pathogen = pathogen,
      care_line = care_line,
      region_code = NA,
      institution_id = institution_id,
      ward = if (stream_level == "pathogen_ward") ward else NA
    )
    stream_id <- episodic_db_stream_upsert(
      con,
      stream_key = stream_key,
      level = stream_level,
      pathogen = pathogen,
      care_line = care_line,
      region_code = NA,
      institution_id = institution_id,
      ward = if (stream_level == "pathogen_ward") ward else NA,
      denominator = "none",
      observed_date = as.character(max(dates))
    )

    for (w in windows) {
      records[[length(records) + 1]] <- cbind(
        episodic_detection_record(
          stream_id = stream_id,
          detector = "same_place",
          first_day = w$first_day,
          last_day = w$last_day,
          n_cases = w$n_cases,
          params = list(rule_n = rule$n, rule_k_days = rule$k_days, ward = ward)
        )
      )
    }
  }
  list(
    records = if (length(records) == 0) NULL else do.call(rbind, records),
    dropped = n_dropped
  )
}

#' @keywords internal
#' @noRd
episodic_same_place_rule <- function(config, pathogen) {
  sp <- config$same_place
  override <- sp$overrides[[pathogen]]
  if (!is.null(override)) {
    list(n = override$n_cases, k_days = override$k_days)
  } else {
    list(n = sp$default_n_cases, k_days = sp$default_k_days)
  }
}

#' Find maximal windows where >= n cases fall within k_days of each other
#'
#' Two passes: which cases are part of *any* qualifying k-day window, and
#' then how those cases divide into episodes. The second is on the gaps
#' in time between flagged cases, so two separate outbreaks at the same
#' place are two windows however far apart they are - see the comment
#' inside for what happened when it was on gaps in index instead.
#'
#' @param dates_sorted A sorted `Date` vector (may contain duplicates).
#' @param n,k_days The rule threshold.
#' @return A list of `list(first_day, last_day, n_cases)`, one per merged
#'   hit window, in date order.
#' @keywords internal
#' @noRd
episodic_same_place_hit_windows <- function(dates_sorted, n, k_days) {
  if (length(dates_sorted) < n) {
    return(list())
  }

  hit <- logical(length(dates_sorted))
  for (i in seq_along(dates_sorted)) {
    in_window <- dates_sorted >= dates_sorted[i] &
      dates_sorted <= dates_sorted[i] + k_days
    if (sum(in_window) >= n) hit[in_window] <- TRUE
  }
  if (!any(hit)) {
    return(list())
  }

  # Split the flagged cases into episodes on the gaps *in time* between
  # them, not on gaps in their index. Contiguous indices were the merge
  # unit here, on the reasoning that `dates_sorted` is sorted - but sorted
  # says nothing about proximity. A ward with a cluster in 2021 and
  # another in 2025 flags every one of those cases, and every one of them
  # is index-adjacent to the next, so the two merged into a single
  # "window" running from 2021 to 2025 with all six cases in it. That
  # candidate then reached reconciliation, where it overlapped and
  # absorbed everything else on the stream and had its case count
  # recomputed over the whole four years.
  #
  # Two flagged cases belong to the same episode when they are within
  # `k_days` of each other, transitively - the detector's own definition
  # of "at the same place within k days", applied to deciding where one
  # episode ends and the next begins as well as to deciding what counts
  # as a hit at all.
  hit_idx <- which(hit)
  gaps <- as.numeric(diff(dates_sorted[hit_idx]), units = "days")
  episode <- cumsum(c(TRUE, gaps > k_days))

  windows <- list()
  for (e in unique(episode)) {
    idx <- hit_idx[episode == e]
    windows[[length(windows) + 1]] <- list(
      first_day = as.character(min(dates_sorted[idx])),
      last_day = as.character(max(dates_sorted[idx])),
      n_cases = length(idx)
    )
  }
  windows
}
