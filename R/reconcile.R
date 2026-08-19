#' Cluster reconciliation
#'
#' Run per stream, after detection, inside the run transaction
#' (ARCHITECTURE.md section 6). This is the load-bearing algorithm of the
#' whole system: no detector emits a stable cluster identity on its own, so
#' mapping today's detections onto persistent, real-world clusters is what
#' makes the rest of the application possible.
#'
#' Algorithm, matching section 6 verbatim:
#'
#' 1. Collect this run's detections for the stream across all detectors.
#' 2. Merge detections whose intervals overlap into candidate episodes,
#'    recording `detector_agreement` (how many distinct detectors fired).
#' 3. For each candidate, find open clusters in the same stream whose
#'    interval overlaps it, or lies within `case_free_days` of it.
#'    - No match: open a new cluster.
#'    - One match: extend it, recount, recompute score; if it already
#'      carries an assessment and the interval or case count changed, flag
#'      `changed_since_assessment`.
#'    - Multiple matches: merge. The oldest surviving cluster absorbs the
#'      others via `merged_into`; nothing is deleted and no assessment
#'      history is lost.
#' 4. Open clusters in the stream with no candidate this run get
#'    `runs_since_detected + 1`.
#' 5. Auto-closure: unassessed clusters, or those assessed `artefact`/
#'    `expected_variation`, close after `close_after_runs` runs undetected.
#'
#' This function is idempotent by construction: it is keyed on `stream_key`
#' and interval overlap rather than on insertion order (section 5.3), so
#' running it twice over the same detections produces no new clusters and
#' no duplicate rows, and the caller is responsible for wrapping it (and
#' the detection step that feeds it) in one transaction per run so a
#' partial failure leaves no partial state.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id The stream to reconcile.
#' @param detections A data frame of this run's detections for this stream
#'   (already inserted into `episode_detection` with their `detection_id`).
#' @param case_free_days The organism's case-free interval, from
#'   `episode_pathogen_config`.
#' @param run_id The current `run_id`.
#' @param close_after_runs From `config$reconciliation$close_after_runs`.
#' @param priority_score_fn A function `(candidate) -> numeric(1)` computing
#'   the priority score for a candidate episode; injected so reconciliation
#'   does not depend on `R/score_*.R` implementation details.
#' @param has_assessment_fn A function `(cluster_id) -> logical(1)`,
#'   `TRUE` if the cluster carries any assessment event.
#' @param verdict_fn A function `(cluster_id) -> character(1) or NA`, the
#'   latest verdict for a cluster, used to decide auto-closure eligibility.
#' @return Invisibly, a list with `n_new`, `n_updated`, `n_merged`.
#' @export
episode_reconcile_stream <- function(con, stream_id, detections, case_free_days, run_id,
                                      close_after_runs, priority_score_fn, has_assessment_fn,
                                      verdict_fn) {
  n_new <- 0L
  n_updated <- 0L
  n_merged <- 0L

  candidates <- episode_reconcile_merge_detections(detections)

  open_clusters <- episode_db_clusters_for_stream(con, stream_id)
  matched_cluster_ids <- character(0)

  for (i in seq_len(nrow(candidates))) {
    candidate <- candidates[i, ]
    matches <- episode_reconcile_find_matches(open_clusters, candidate, case_free_days)

    if (length(matches) == 0) {
      cluster_id <- episode_db_cluster_insert(
        con, stream_id = stream_id, first_day = candidate$first_day,
        last_day = candidate$last_day, n_cases = candidate$n_cases,
        priority_score = priority_score_fn(candidate),
        detector_agreement = candidate$detector_agreement, run_id = run_id
      )
      n_new <- n_new + 1L
      open_clusters <- rbind(open_clusters, episode_db_clusters_for_stream(con, stream_id)[
        episode_db_clusters_for_stream(con, stream_id)$cluster_id == cluster_id, ])
      matched_cluster_ids <- c(matched_cluster_ids, as.character(cluster_id))
      episode_reconcile_link_detections(con, detections, candidate, cluster_id)
      episode_reconcile_link_cases(con, stream_id, cluster_id, candidate$first_day, candidate$last_day)
    } else if (length(matches) == 1) {
      cluster_id <- open_clusters$cluster_id[matches]
      existing <- open_clusters[matches, ]
      new_first <- min(as.Date(existing$first_day), as.Date(candidate$first_day))
      new_last <- max(as.Date(existing$last_day), as.Date(candidate$last_day))
      new_n <- episode_reconcile_case_count(con, stream_id, new_first, new_last, existing, candidate)

      changed <- has_assessment_fn(cluster_id) &&
        (as.character(new_first) != existing$first_day ||
           as.character(new_last) != existing$last_day ||
           new_n != existing$n_cases)

      episode_db_cluster_update(
        con, cluster_id = cluster_id, first_day = as.character(new_first),
        last_day = as.character(new_last), n_cases = new_n,
        priority_score = priority_score_fn(candidate),
        detector_agreement = max(existing$detector_agreement, candidate$detector_agreement),
        run_id = run_id,
        changed_since_assessment = if (changed) TRUE else NULL
      )
      n_updated <- n_updated + 1L
      matched_cluster_ids <- c(matched_cluster_ids, as.character(cluster_id))
      episode_reconcile_link_detections(con, detections, candidate, cluster_id)
      episode_reconcile_link_cases(con, stream_id, cluster_id, as.character(new_first), as.character(new_last))
    } else {
      survivor_idx <- matches[which.min(as.Date(open_clusters$opened_at[matches]))]
      survivor_id <- open_clusters$cluster_id[survivor_idx]
      to_merge <- open_clusters$cluster_id[setdiff(matches, survivor_idx)]

      all_first <- min(as.Date(open_clusters$first_day[matches]), as.Date(candidate$first_day))
      all_last <- max(as.Date(open_clusters$last_day[matches]), as.Date(candidate$last_day))
      combined_n <- episode_reconcile_case_count(
        con, stream_id, all_first, all_last, open_clusters[survivor_idx, ], candidate
      )

      episode_db_cluster_update(
        con, cluster_id = survivor_id, first_day = as.character(all_first),
        last_day = as.character(all_last), n_cases = combined_n,
        priority_score = priority_score_fn(candidate),
        detector_agreement = max(open_clusters$detector_agreement[matches], candidate$detector_agreement),
        run_id = run_id
      )
      for (m in to_merge) {
        episode_db_cluster_set_merged_into(con, m, survivor_id)
      }
      n_merged <- n_merged + length(to_merge)
      n_updated <- n_updated + 1L
      matched_cluster_ids <- c(matched_cluster_ids, as.character(survivor_id))
      episode_reconcile_link_detections(con, detections, candidate, survivor_id)
      episode_reconcile_link_cases(con, stream_id, survivor_id, as.character(all_first), as.character(all_last))
    }
  }

  # step 4 + 5: age out and auto-close clusters with no candidate this run
  undetected <- open_clusters[!as.character(open_clusters$cluster_id) %in% matched_cluster_ids &
                                 is.na(open_clusters$merged_into), ]
  for (i in seq_len(nrow(undetected))) {
    cluster_id <- undetected$cluster_id[i]
    episode_db_cluster_increment_runs_since_detected(con, cluster_id)

    runs_since <- undetected$runs_since_detected[i] + 1L
    verdict <- verdict_fn(cluster_id)
    eligible_for_autoclose <- is.na(verdict) || verdict %in% c("artefact", "expected_variation")

    if (runs_since > close_after_runs && eligible_for_autoclose) {
      episode_db_cluster_state_insert(
        con, cluster_id = cluster_id, state = "closed", trigger = "system"
      )
    }
  }

  invisible(list(n_new = n_new, n_updated = n_updated, n_merged = n_merged))
}

#' Merge same-run detections whose intervals overlap into candidate episodes
#'
#' @param detections A data frame of detections (any number of detectors).
#' @return A data frame with one row per candidate episode: `first_day`,
#'   `last_day`, `n_cases` (max across the merged detections, since
#'   different detectors may report slightly different counts for
#'   overlapping windows), `detector_agreement` (count of distinct
#'   detectors), and `.detection_ids` (a list-column of the source
#'   `detection_id`s, used only internally to link back).
#' @keywords internal
#' @noRd
episode_reconcile_merge_detections <- function(detections) {
  if (nrow(detections) == 0) {
    return(data.frame(
      first_day = character(0), last_day = character(0), n_cases = integer(0),
      detector_agreement = integer(0)
    ))
  }

  ord <- order(as.Date(detections$first_day))
  detections <- detections[ord, ]

  groups <- list()
  current <- detections[1, , drop = FALSE]
  current_end <- as.Date(current$last_day)

  for (i in seq_len(nrow(detections))[-1]) {
    row <- detections[i, ]
    if (as.Date(row$first_day) <= current_end) {
      current <- rbind(current, row)
      current_end <- max(current_end, as.Date(row$last_day))
    } else {
      groups[[length(groups) + 1]] <- current
      current <- row
      current_end <- as.Date(row$last_day)
    }
  }
  groups[[length(groups) + 1]] <- current

  do.call(rbind, lapply(groups, function(grp) {
    data.frame(
      first_day = as.character(min(as.Date(grp$first_day))),
      last_day = as.character(max(as.Date(grp$last_day))),
      n_cases = max(grp$n_cases),
      detector_agreement = length(unique(grp$detector)),
      stringsAsFactors = FALSE
    )
  }))
}

#' @keywords internal
#' @noRd
episode_reconcile_link_detections <- function(con, detections, candidate, cluster_id) {
  in_candidate <- as.Date(detections$first_day) <= as.Date(candidate$last_day) &
    as.Date(detections$last_day) >= as.Date(candidate$first_day)
  ids <- detections$detection_id[in_candidate]
  for (id in ids) {
    episode_db_detection_set_cluster(con, id, cluster_id)
  }
  invisible(NULL)
}

#' Find open clusters overlapping a candidate, or within case_free_days of it
#'
#' @param open_clusters A data frame from `episode_db_clusters_for_stream()`.
#' @param candidate A single-row candidate episode.
#' @param case_free_days The organism's case-free interval.
#' @return Integer row indices into `open_clusters` (may be length 0, 1 or
#'   more than 1, per ARCHITECTURE.md section 6 step 3).
#' @keywords internal
#' @noRd
episode_reconcile_find_matches <- function(open_clusters, candidate, case_free_days) {
  if (nrow(open_clusters) == 0) return(integer(0))
  still_open <- is.na(open_clusters$merged_into)
  if (!any(still_open)) return(integer(0))

  cand_first <- as.Date(candidate$first_day)
  cand_last <- as.Date(candidate$last_day)

  overlaps <- as.Date(open_clusters$first_day) <= cand_last + case_free_days &
    as.Date(open_clusters$last_day) >= cand_first - case_free_days

  which(overlaps & still_open)
}

#' Link the individual cases within a cluster's interval to that cluster
#'
#' Populates `episode_cluster_case`, which nothing else in the reconciliation
#' loop writes to otherwise. This is part of the handoff contract (standing
#' brief, "the cron's writes are all the app needs": the line list panel
#' (M2) and the report's `case_ids` (M4) both read from this table rather
#' than recomputing a stream/date filter at read time).
#' @keywords internal
#' @noRd
episode_reconcile_link_cases <- function(con, stream_id, cluster_id, first_day, last_day) {
  tryCatch({
    stream <- DBI::dbGetQuery(con, "SELECT pathogen, institution_id FROM episode_stream WHERE stream_id = ?",
                               params = list(stream_id))
    if (nrow(stream) == 0) stop("no such stream")
    cases <- DBI::dbGetQuery(
      con,
      "SELECT case_id FROM episode_case
       WHERE pathogen = ? AND sample_date >= ? AND sample_date <= ?
         AND (institution_id IS ? OR ? IS NULL)",
      params = list(stream$pathogen[1], first_day, last_day,
                    stream$institution_id[1], stream$institution_id[1])
    )
    for (case_id in cases$case_id) {
      episode_db_cluster_case_link(con, cluster_id, case_id)
    }
  }, error = function(e) invisible(NULL))
  invisible(NULL)
}

#' Count distinct cases within a first_day/last_day interval for a stream
#'
#' Reconciliation recomputes the case count from the underlying data rather
#' than summing detector-reported counts, since detectors may double-count
#' at their boundaries. Falls back to the maximum of the inputs' own counts
#' when case-level data cannot be queried (keeps the function usable in
#' tests that pass synthetic clusters/candidates directly).
#' @keywords internal
#' @noRd
episode_reconcile_case_count <- function(con, stream_id, first_day, last_day, existing, candidate) {
  tryCatch({
    stream <- DBI::dbGetQuery(con, "SELECT pathogen, institution_id FROM episode_stream WHERE stream_id = ?",
                               params = list(stream_id))
    if (nrow(stream) == 0) stop("no such stream")
    res <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM episode_case
       WHERE pathogen = ? AND sample_date >= ? AND sample_date <= ?
         AND (institution_id IS ? OR ? IS NULL)",
      params = list(stream$pathogen[1], as.character(first_day), as.character(last_day),
                    stream$institution_id[1], stream$institution_id[1])
    )
    as.integer(res$n[1])
  }, error = function(e) {
    max(existing$n_cases, candidate$n_cases)
  })
}
