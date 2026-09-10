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

#' `rare_trigger` detector
#'
#' A single case of a curated pathogen is notable on its own, not on
#' aberration statistics: running it through Farrington produces only
#' silence, since a baseline model has nothing to compare one rare case
#' against. This detector needs no baseline and no eligibility gate: any
#' occurrence at or above `min_cases` fires.
#'
#' Matching is against the raw `pathogen` string, case-insensitively, since
#' operators supply free text and a curated list
#' should not silently miss a hit over a capitalisation difference.
#'
#' Only cases sampled within `config$rare_trigger$lookback_days` of
#' `run_date` are considered. A single case is notable when it is
#' *news*; the same case is not news again every night for the rest of
#' the instance's life, which is what an unbounded rescan of the whole
#' case history made it - one detection row per historical rare case per
#' run, and a `runs_since_detected` reset that kept every cluster this
#' detector had ever opened permanently ineligible for
#' `reconciliation.close_after_runs`. See
#' `episodic_detector_lookback_cutoff()`.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame of cases to scan, with `pathogen`,
#'   `institution_id`, `sample_date`.
#' @param config The resolved configuration; uses `config$rare_trigger`.
#' @param run_date The date to treat as "today", for the lookback window.
#' @param backfill When `TRUE`, the lookback window does not apply: this
#'   run reports every matching case in the history it was given. See
#'   `episodic_detector_lookback_cutoff()`.
#' @return A data frame of detection records plus a `stream_id` column, one
#'   row per matching case (or per institution-day group when several
#'   matching cases share an institution and date), carrying what the
#'   lookback kept out of it (`episodic_detector_lookback_note()`).
#' @keywords internal
#' @noRd
episodic_detect_rare_trigger <- function(con,
                                         cases,
                                         config,
                                         run_date = Sys.Date(),
                                         backfill = FALSE) {
  empty <- episodic_detection_none()

  if (!episodic_detector_enabled(config, "rare_trigger")) {
    return(empty)
  }
  rt <- config$rare_trigger
  if (is.null(rt) || length(rt$pathogens) == 0) {
    return(empty)
  }

  cases <- episodic_detector_cases_asof(cases, run_date)
  if (nrow(cases) == 0) {
    return(empty)
  }
  matches <- tolower(cases$pathogen) %in% tolower(rt$pathogens)
  cutoff <- episodic_detector_lookback_cutoff(
    run_date,
    rt$lookback_days,
    backfill = backfill
  )
  current <- matches
  if (!is.null(cutoff)) {
    current <- matches & as.Date(cases$sample_date) >= cutoff
  }
  # Counted before the cases are cut down, so a run that reports nothing
  # can still say whether there was nothing to report or an archive of
  # it - see `episodic_detector_lookback_note()`.
  dropped <- sum(matches, na.rm = TRUE) - sum(current, na.rm = TRUE)
  note <- function(x) {
    episodic_detector_lookback_note(x, dropped, cutoff, rt$lookback_days)
  }
  hits <- cases[which(current), ]
  if (nrow(hits) == 0) {
    return(note(empty))
  }

  min_cases <- if (is.null(rt$min_cases)) 1L else rt$min_cases

  key_str <- paste(
    hits$pathogen,
    hits$institution_id,
    hits$sample_date,
    sep = "\r"
  )
  groups <- split(seq_len(nrow(hits)), key_str)

  records <- list()
  for (g in groups) {
    grp <- hits[g, ]
    if (nrow(grp) < min_cases) {
      next
    }

    pathogen <- grp$pathogen[1]
    institution_id <- grp$institution_id[1]

    # rare_trigger streams are institution-level (or regional, when the case
    # has no institution); it reuses episodic_db_stream_upsert() exactly as
    # same_place does, so it reconciles into the same tables as every other
    # detector.
    stream_key <- episodic_stream_key(
      level = "pathogen_institution",
      pathogen = pathogen,
      care_line = NA,
      region_code = NA,
      institution_id = institution_id
    )
    stream_id <- episodic_db_stream_upsert(
      con,
      stream_key = stream_key,
      level = "pathogen_institution",
      pathogen = pathogen,
      care_line = NA,
      region_code = NA,
      institution_id = institution_id,
      denominator = "none",
      observed_date = grp$sample_date[1]
    )

    records[[length(records) + 1]] <- episodic_detection_record(
      stream_id = stream_id,
      detector = "rare_trigger",
      first_day = grp$sample_date[1],
      last_day = grp$sample_date[1],
      n_cases = nrow(grp),
      params = list(pathogen = pathogen, min_cases = min_cases)
    )
  }
  if (length(records) == 0) {
    return(note(empty))
  }
  note(do.call(rbind, records))
}
