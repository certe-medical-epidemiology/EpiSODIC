#' Wrappers around `certestats` detectors
#'
#' `certestats::detect_disease_clusters()` (detector `'clusters'`) and
#' `certestats::detect_farrington()` (detector `'farrington'`) are the
#' constraint-fixed detection functions (ARCHITECTURE.md section 2).
#' `certestats` is Certe-internal and cannot be installed or exercised in
#' this environment (standing brief section 2), so these wrappers are
#' guarded with `requireNamespace()` throughout and, when the package is
#' unavailable, return zero detections rather than failing the run - the
#' documented fallback path a stranger cloning the repository needs.
#'
#' Golden-file tests around these wrappers belong in
#' `tests/testthat/test-detect_certestats.R` once `certestats` is available
#' to generate the golden output against; see `QUESTIONS.md` items 10 and 16
#' and the standing brief section 6.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases_for_stream A data frame of a single stream's cases, with
#'   `sample_date`.
#' @param stream_id The stream these cases belong to.
#' @param population_offset Patient-day offset, see `QUESTIONS.md` item 10
#'   (unverified whether `certestats::detect_farrington()` exposes this).
#' @return A data frame of detection records (possibly zero rows if
#'   `certestats` is unavailable or found nothing).
#' @name detect_certestats
NULL

#' @rdname detect_certestats
#' @export
episode_detect_clusters <- function(con, cases_for_stream, stream_id) {
  if (!requireNamespace("certestats", quietly = TRUE)) {
    return(episode_detection_record(integer(0), character(0), character(0), character(0), integer(0)))
  }
  result <- certestats::detect_disease_clusters(cases_for_stream$sample_date)
  episode_certestats_clusters_to_records(result, stream_id)
}

#' @rdname detect_certestats
#' @export
episode_detect_farrington <- function(con, cases_for_stream, stream_id, population_offset = NULL) {
  if (!requireNamespace("certestats", quietly = TRUE)) {
    return(episode_detection_record(integer(0), character(0), character(0), character(0), integer(0)))
  }
  result <- certestats::detect_farrington(cases_for_stream$sample_date, populationOffset = population_offset)
  episode_certestats_farrington_to_records(result, stream_id)
}

#' @keywords internal
#' @noRd
episode_certestats_clusters_to_records <- function(result, stream_id) {
  if (is.null(result) || length(result) == 0) {
    return(episode_detection_record(integer(0), character(0), character(0), character(0), integer(0)))
  }
  do.call(rbind, lapply(seq_along(result), function(i) {
    cl <- result[[i]]
    episode_detection_record(
      stream_id = stream_id, detector = "clusters", first_day = cl$first_day,
      last_day = cl$last_day, n_cases = cl$n_cases, params = cl
    )
  }))
}

#' @keywords internal
#' @noRd
episode_certestats_farrington_to_records <- function(result, stream_id) {
  if (is.null(result) || length(result) == 0) {
    return(episode_detection_record(integer(0), character(0), character(0), character(0), integer(0)))
  }
  alarms <- result[result$alarm, ]
  if (nrow(alarms) == 0) {
    return(episode_detection_record(integer(0), character(0), character(0), character(0), integer(0)))
  }
  do.call(rbind, lapply(seq_len(nrow(alarms)), function(i) {
    row <- alarms[i, ]
    episode_detection_record(
      stream_id = stream_id, detector = "farrington", first_day = row$date,
      last_day = row$date, n_cases = row$observed, expected = row$expected,
      upperbound = row$upperbound, params = as.list(row)
    )
  }))
}
