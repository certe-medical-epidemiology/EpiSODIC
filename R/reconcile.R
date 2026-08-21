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

#' Cluster reconciliation
#'
#' Run per stream, after detection, inside the run transaction. This is
#' the load-bearing algorithm of the whole system: no detector emits a
#' stable cluster identity on its own, so mapping today's detections
#' onto persistent, real-world clusters is what makes the rest of the
#' application possible.
#'
#' Algorithm:
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
#'   latest verdict for a cluster, used to decide auto-closure eligibility
#'   and, together with `cooldown_days`, the cool-down escape hatch.
#' @param cooldown_days The organism's cool-down interval, from
#'   `episode_pathogen_config`. `NA`
#'   (default) disables the cool-down/escape-hatch check entirely, for
#'   callers (and existing tests) that predate it.
#' @param cooldown_reopen_ratio From `config$reconciliation$
#'   cooldown_reopen_ratio` (a "half again" growth threshold). Ignored
#'   when `cooldown_days` is `NA`.
#' @return Invisibly, a list with `n_new`, `n_updated`, `n_merged`.
#' @keywords internal
#' @noRd
episode_reconcile_stream <- function(con, stream_id, detections, case_free_days, run_id,
                                      close_after_runs, priority_score_fn, has_assessment_fn,
                                      verdict_fn, cooldown_days = NA, cooldown_reopen_ratio = NA) {
  n_new <- 0L
  n_updated <- 0L
  n_merged <- 0L

  candidates <- episode_reconcile_merge_detections(detections)

  open_clusters <- episode_db_clusters_for_stream(con, stream_id)
  matched_cluster_ids <- character(0)

  for (i in seq_len(nrow(candidates))) {
    candidate <- candidates[i, ]
    matches <- episode_reconcile_find_matches(open_clusters, candidate, case_free_days)

    cooldown_match <- if (length(matches) == 0 && !is.na(cooldown_days)) {
      episode_reconcile_find_cooldown_match(open_clusters, candidate, case_free_days,
                                             cooldown_days, cooldown_reopen_ratio, verdict_fn)
    } else {
      NULL
    }

    if (length(matches) == 0 && !is.null(cooldown_match)) {
      cluster_id <- cooldown_match$cluster_id
      existing <- open_clusters[open_clusters$cluster_id == cluster_id, ]
      new_first <- min(as.Date(existing$first_day), as.Date(candidate$first_day))
      new_last <- max(as.Date(existing$last_day), as.Date(candidate$last_day))
      new_n <- episode_reconcile_case_count(con, stream_id, new_first, new_last, existing, candidate)

      episode_db_cluster_update(
        con, cluster_id = cluster_id, first_day = as.character(new_first),
        last_day = as.character(new_last), n_cases = new_n,
        priority_score = priority_score_fn(candidate),
        detector_agreement = max(existing$detector_agreement, candidate$detector_agreement),
        run_id = run_id,
        # Only a genuine escape-hatch hit (excess materially exceeds what
        # the terminal verdict was based on) flags changed_since_assessment;
        # a candidate absorbed within cool-down that does NOT clear that
        # bar is exactly the "tail of a resolved outbreak" cool-down exists
        # to suppress - its cases are still
        # linked to the cluster (nothing is discarded), but no reassessment
        # prompt fires for it.
        changed_since_assessment = if (cooldown_match$reopen) TRUE else NULL
      )
      n_updated <- n_updated + 1L
      matched_cluster_ids <- c(matched_cluster_ids, as.character(cluster_id))
      episode_reconcile_link_detections(con, detections, candidate, cluster_id)
      episode_reconcile_link_cases(con, stream_id, cluster_id, as.character(new_first), as.character(new_last))
    } else if (length(matches) == 0) {
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
#' @return Integer row indices into `open_clusters` (may be length 0, 1,
#'   or more than 1).
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

#' The cool-down escape hatch
#'
#' Only called for a candidate with zero ordinary matches (i.e. outside
#' `case_free_days` of every open cluster in the stream, so it would
#' otherwise become a brand-new cluster). Looks one step further, to
#' `cooldown_days`, for a cluster whose latest verdict was terminal
#' (`artefact`/`expected_variation`) and, if found, absorbs the candidate
#' into it instead of opening a new one - "so that the tail of a resolved
#' outbreak does not immediately reopen as a fresh one". Whether that
#' absorption also reopens the cluster as Herbeoordeling nodig
#' (`reopen = TRUE`, surfaced via `changed_since_assessment` by the
#' caller) depends on whether the candidate's excess "exceeds the excess
#' on which that judgement was made by a material margin" - approximated
#' here as `candidate$n_cases` against the closed cluster's own `n_cases`
#' scaled by `cooldown_reopen_ratio`, since `n_cases` is always available
#' (unlike `ratio`, which only Farrington-detected clusters carry) and is
#' exactly the quantity the terminal verdict was actually judged against.
#'
#' @param open_clusters A data frame from `episode_db_clusters_for_stream()`.
#' @param candidate A single-row candidate episode.
#' @param case_free_days The organism's case-free interval - the inner
#'   bound of the cool-down window (a candidate this close would already
#'   have matched ordinarily; this function only ever sees ones that did not).
#' @param cooldown_days The organism's cool-down interval - the outer bound.
#' @param cooldown_reopen_ratio From `config$reconciliation$
#'   cooldown_reopen_ratio`. `NA` disables the reopen check (every
#'   cool-down absorption is then silent, never flagged).
#' @param verdict_fn A function `(cluster_id) -> character(1) or NA`.
#' @return `NULL` if no eligible cluster is found, else a list with
#'   `cluster_id` and `reopen` (logical).
#' @keywords internal
#' @noRd
episode_reconcile_find_cooldown_match <- function(open_clusters, candidate, case_free_days,
                                                    cooldown_days, cooldown_reopen_ratio, verdict_fn) {
  if (nrow(open_clusters) == 0) return(NULL)
  still_present <- is.na(open_clusters$merged_into)
  if (!any(still_present)) return(NULL)

  cand_first <- as.Date(candidate$first_day)
  cand_last <- as.Date(candidate$last_day)

  # Strictly beyond case_free_days (else episode_reconcile_find_matches()
  # would already have matched it) but within cooldown_days.
  in_cooldown_window <- as.Date(open_clusters$last_day) < cand_first - case_free_days &
    as.Date(open_clusters$last_day) >= cand_first - cooldown_days

  idx <- which(in_cooldown_window & still_present)
  if (length(idx) == 0) return(NULL)
  idx <- idx[which.max(as.Date(open_clusters$last_day[idx]))]  # nearest in time
  cluster_id <- open_clusters$cluster_id[idx]

  verdict <- verdict_fn(cluster_id)
  if (is.na(verdict) || !verdict %in% c("artefact", "expected_variation")) return(NULL)

  reopen <- FALSE
  if (!is.na(cooldown_reopen_ratio)) {
    existing_n <- open_clusters$n_cases[idx]
    if (!is.na(existing_n) && existing_n > 0 &&
        candidate$n_cases >= existing_n * cooldown_reopen_ratio) {
      reopen <- TRUE
    }
  }

  list(cluster_id = cluster_id, reopen = reopen)
}

#' Link the individual cases within a cluster's interval to that cluster
#'
#' Populates `episode_cluster_case`, which nothing else in the
#' reconciliation loop writes to otherwise. This is part of the handoff
#' contract between the cron and the app: the line list panel and the
#' report's `case_ids` both read from this table rather than
#' recomputing a stream/date filter at read time.
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
         AND (? IS NULL OR institution_id = ?)",
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
         AND (? IS NULL OR institution_id = ?)",
      params = list(stream$pathogen[1], as.character(first_day), as.character(last_day),
                    stream$institution_id[1], stream$institution_id[1])
    )
    as.integer(res$n[1])
  }, error = function(e) {
    max(existing$n_cases, candidate$n_cases)
  })
}
