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

#' Baseline feedback: exclude confirmed-epidemic periods from detection
#'
#' Periods classified as a confirmed epidemic are excluded from the
#' baseline of subsequent Farrington runs for that stream, derived from
#' `episodic_assessment_event` so it updates
#' automatically when a verdict is revised. `farringtonFlexible()`
#' downweights past aberrations statistically, but a human verdict is
#' better evidence than a residual - without this exclusion, last winter's
#' outbreak silently raises this winter's threshold.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id A stream id.
#' @return A data frame with `first_day`, `last_day` (character, ISO
#'   dates), one row per cluster in this stream whose latest verdict is
#'   `confirmed_epidemic`. Zero rows if none.
#' @keywords internal
#' @noRd
episodic_baseline_excluded_windows <- function(con, stream_id) {
  episodic_baseline_excluded_windows_many(con, stream_id)[[1]]
}

#' The same, for many streams at once
#'
#' The Streams screen asks this of every stream on the page, and the cron
#' of every eligible stream. One cluster query per stream plus one
#' assessment-event query per cluster made that a per-row round trip
#' count; both readers are the same two queries however many streams are
#' asked about.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_ids The streams to answer for.
#' @return A list of data frames, one per id, in the order given.
#' @keywords internal
#' @noRd
episodic_baseline_excluded_windows_many <- function(con, stream_ids) {
  empty <- data.frame(
    first_day = character(0),
    last_day = character(0),
    stringsAsFactors = FALSE
  )
  if (length(stream_ids) == 0) {
    return(list())
  }
  clusters_all <- episodic_db_clusters_for_streams(con, stream_ids)
  if (nrow(clusters_all) == 0) {
    return(rep(list(empty), length(stream_ids)))
  }
  events_all <- episodic_db_assessment_events_batch(
    con,
    clusters_all$cluster_id
  )
  classified <- events_all[!is.na(events_all$verdict), ]
  # The latest verdict per cluster: the batch reader orders by
  # cluster_id, created_at, event_id, so that is each cluster's last row.
  latest <- classified[!duplicated(classified$cluster_id, fromLast = TRUE), ]
  confirmed <- latest$cluster_id[latest$verdict == "confirmed_epidemic"]

  keep <- clusters_all[clusters_all$cluster_id %in% confirmed, , drop = FALSE]
  lapply(stream_ids, function(sid) {
    rows <- keep[keep$stream_id == sid, , drop = FALSE]
    if (nrow(rows) == 0) {
      return(empty)
    }
    data.frame(
      first_day = rows$first_day,
      last_day = rows$last_day,
      stringsAsFactors = FALSE
    )
  })
}

#' Remove cases falling inside any excluded window
#'
#' @param cases_for_stream A data frame with `sample_date`.
#' @param excluded_windows `episodic_baseline_excluded_windows()`'s output.
#' @return `cases_for_stream`, minus any row whose `sample_date` falls
#'   within `[first_day, last_day]` of any excluded window. Unchanged if
#'   `excluded_windows` has zero rows.
#' @keywords internal
#' @noRd
episodic_baseline_exclude_cases <- function(cases_for_stream,
                                            excluded_windows) {
  if (nrow(excluded_windows) == 0 || nrow(cases_for_stream) == 0) {
    return(cases_for_stream)
  }
  dates <- as.Date(cases_for_stream$sample_date)
  excluded <- rep(FALSE, length(dates))
  for (i in seq_len(nrow(excluded_windows))) {
    excluded <- excluded |
      (dates >= as.Date(excluded_windows$first_day[i]) &
        dates <= as.Date(excluded_windows$last_day[i]))
  }
  cases_for_stream[!excluded, , drop = FALSE]
}
