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
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame of cases to scan, with `pathogen`,
#'   `institution_id`, `ward`, `sample_date`.
#' @param institutions A data frame from `episodic_db_institutions()`.
#' @param config The resolved configuration; uses `config$same_place`.
#' @return A data frame of detection records (`episodic_detection_record()`
#'   shape) plus a `stream_id` column, one row per hit.
#' @keywords internal
#' @noRd
episodic_detect_same_place <- function(con, cases, institutions, config) {
  cases <- cases[!is.na(cases$institution_id), ]
  if (nrow(cases) == 0) {
    return(episodic_detection_record(
      integer(0),
      character(0),
      character(0),
      character(0),
      integer(0)
    ))
  }

  inst_type <- institutions$institution_type[match(
    cases$institution_id,
    institutions$institution_id
  )]
  is_hospital <- inst_type == "hospital" & !is.na(cases$ward)

  hits <- list()

  if (any(is_hospital, na.rm = TRUE)) {
    ward_cases <- cases[which(is_hospital), ]
    hits$ward <- episodic_same_place_scan(
      ward_cases,
      group_cols = c("pathogen", "institution_id", "ward"),
      config = config,
      stream_level = "pathogen_ward",
      con = con
    )
  }

  non_hospital <- cases[which(!is_hospital), ]
  if (nrow(non_hospital) > 0) {
    hits$institution <- episodic_same_place_scan(
      non_hospital,
      group_cols = c("pathogen", "institution_id"),
      config = config,
      stream_level = "pathogen_institution",
      con = con
    )
  }

  hits <- hits[!vapply(hits, is.null, logical(1))]
  if (length(hits) == 0) {
    return(episodic_detection_record(
      integer(0),
      character(0),
      character(0),
      character(0),
      integer(0)
    ))
  }
  do.call(rbind, hits)
}

#' @keywords internal
#' @noRd
episodic_same_place_scan <- function(
  cases,
  group_cols,
  config,
  stream_level,
  con
) {
  key_df <- cases[, group_cols, drop = FALSE]
  key_str <- do.call(paste, c(key_df, sep = "\r"))
  groups <- split(seq_len(nrow(cases)), key_str)

  records <- list()
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
    if (length(windows) == 0) {
      next
    }

    institution_id <- grp$institution_id[1]
    ward <- if ("ward" %in% group_cols) grp$ward[1] else NA

    stream_key <- episodic_stream_key(
      level = stream_level,
      pathogen = pathogen,
      care_line = NA,
      region_code = NA,
      institution_id = institution_id,
      ward = if (stream_level == "pathogen_ward") ward else NA
    )
    stream_id <- episodic_db_stream_upsert(
      con,
      stream_key = stream_key,
      level = stream_level,
      pathogen = pathogen,
      care_line = NA,
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
  if (length(records) == 0) {
    return(NULL)
  }
  do.call(rbind, records)
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
#' @param dates_sorted A sorted `Date` vector (may contain duplicates).
#' @param n,k_days The rule threshold.
#' @return A list of `list(first_day, last_day, n_cases)`, one per maximal
#'   merged hit window.
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

  # Merge the TRUE positions of `hit` into contiguous runs (by index, since
  # dates_sorted is sorted, contiguous indices are the correct merge unit).
  runs <- rle(hit)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  is_true_run <- runs$values

  windows <- list()
  for (r in which(is_true_run)) {
    idx <- starts[r]:ends[r]
    windows[[length(windows) + 1]] <- list(
      first_day = as.character(min(dates_sorted[idx])),
      last_day = as.character(max(dates_sorted[idx])),
      n_cases = length(idx)
    )
  }
  windows
}
