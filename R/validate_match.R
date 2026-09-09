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

# Matching detected clusters against seeded outbreaks.
#
# Every function here is a pure function over data frames: it never
# touches a database, never runs a detector, and never reads the clock.
# That is deliberate. The rules by which a cluster counts as a hit are
# the part of a validation result a reviewer will argue with, so they are
# the part that has to be readable and testable on its own, without a
# detection run anywhere near it.

#' Case-set overlap between clusters and seeded outbreaks
#'
#' The heart of the matching rule. Interval overlap alone is not enough:
#' an endemic winter cluster spanning the same fortnight as a seeded
#' outbreak overlaps it perfectly in time and shares not one case with
#' it. So clusters and outbreaks are compared on the cases themselves.
#'
#' Three denominators come out of it, because three different questions
#' are being asked:
#'
#' - `precision` is the share of the *cluster's own* cases that belong to
#'   this outbreak. It says whether the dossier an epidemiologist opens
#'   is about the outbreak or about something else that happens to
#'   contain a few of its cases.
#' - `recall_full` is the share of the *whole* outbreak the cluster
#'   holds, counting cases that had not been sampled yet at this run. It
#'   is the end-state question: was the outbreak captured.
#' - `recall_asof` is the share of the outbreak *that existed by this
#'   run* the cluster holds. It is the prospective question: on the day,
#'   did the cluster hold most of what there was to hold. A cluster
#'   cannot contain a case nobody has taken yet, so measuring a
#'   prospective run against the full outbreak measures how fast the
#'   outbreak accrued rather than how fast it was found.
#'
#' @param cluster_cases One row per (run, cluster, case): columns
#'   `run_date`, `cluster_id`, `source_key`.
#' @param truth_cases One row per injected case: columns `outbreak_id`,
#'   `source_key`, `sample_date`.
#' @return One row per (`run_date`, `cluster_id`, `outbreak_id`) that
#'   share at least one case, with `n_overlap`, `n_cluster`,
#'   `n_outbreak_full`, `n_outbreak_asof`, `precision`, `recall_full` and
#'   `recall_asof`. Zero rows (with those columns) when nothing overlaps,
#'   which is the correct answer for a negative control and is not the
#'   same as no answer.
#' @keywords internal
#' @noRd
episodic_validation_overlap <- function(cluster_cases, truth_cases) {
  episodic_validation_require_columns(
    cluster_cases,
    c("run_date", "cluster_id", "source_key"),
    "cluster_cases"
  )
  episodic_validation_require_columns(
    truth_cases,
    c("outbreak_id", "source_key", "sample_date"),
    "truth_cases"
  )
  empty <- data.frame(
    run_date = as.Date(character(0)),
    cluster_id = integer(0),
    outbreak_id = character(0),
    n_overlap = integer(0),
    n_cluster = integer(0),
    n_outbreak_full = integer(0),
    n_outbreak_asof = integer(0),
    precision = numeric(0),
    recall_full = numeric(0),
    recall_asof = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(cluster_cases) == 0 || nrow(truth_cases) == 0) {
    return(empty)
  }

  run_dates <- as.Date(cluster_cases$run_date)
  truth_dates <- as.Date(truth_cases$sample_date)
  outbreak_size <- table(truth_cases$outbreak_id)

  # Cluster size is per (run, cluster): membership grows as runs absorb
  # new cases, and a precision computed against the final size would
  # judge an early run by cases it could not have had.
  cluster_key <- paste(run_dates, cluster_cases$cluster_id, sep = "\r")
  cluster_size <- table(cluster_key)

  labelled <- merge(
    data.frame(
      run_date = run_dates,
      cluster_id = cluster_cases$cluster_id,
      source_key = cluster_cases$source_key,
      cluster_key = cluster_key,
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_key = truth_cases$source_key,
      outbreak_id = truth_cases$outbreak_id,
      stringsAsFactors = FALSE
    ),
    by = "source_key"
  )
  if (nrow(labelled) == 0) {
    return(empty)
  }

  pairs <- stats::aggregate(
    list(n_overlap = labelled$source_key),
    by = list(
      run_date = labelled$run_date,
      cluster_id = labelled$cluster_id,
      outbreak_id = labelled$outbreak_id,
      cluster_key = labelled$cluster_key
    ),
    FUN = length
  )

  # How much of each outbreak had been sampled by each run. Counted per
  # (run, outbreak) rather than assumed: an outbreak with nothing sampled
  # yet has a denominator of zero, and a share of zero cases is not zero,
  # it is undefined - so it comes out NA rather than a number that would
  # read as "the cluster held none of it".
  asof <- vapply(
    seq_len(nrow(pairs)),
    function(i) {
      sum(
        truth_cases$outbreak_id == pairs$outbreak_id[i] &
          truth_dates <= pairs$run_date[i]
      )
    },
    integer(1)
  )

  n_cluster <- as.integer(cluster_size[pairs$cluster_key])
  n_full <- as.integer(outbreak_size[pairs$outbreak_id])
  out <- data.frame(
    run_date = as.Date(pairs$run_date, origin = "1970-01-01"),
    cluster_id = pairs$cluster_id,
    outbreak_id = pairs$outbreak_id,
    n_overlap = as.integer(pairs$n_overlap),
    n_cluster = n_cluster,
    n_outbreak_full = n_full,
    n_outbreak_asof = asof,
    precision = ifelse(n_cluster > 0, pairs$n_overlap / n_cluster, NA_real_),
    recall_full = ifelse(n_full > 0, pairs$n_overlap / n_full, NA_real_),
    recall_asof = ifelse(asof > 0, pairs$n_overlap / asof, NA_real_),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$run_date, out$cluster_id, out$outbreak_id), ]
  rownames(out) <- NULL
  out
}

#' One row per seeded outbreak: was it found, by what, and how late
#'
#' `detected` is decided on the final state, against the whole outbreak:
#' some cluster's final membership holds at least `min_recall` of the
#' outbreak's cases. Detection *time* is then asked of that same cluster
#' and of the runs it existed in, in two ways, because they answer
#' different questions and reporting only the flattering one would be a
#' choice rather than a measurement:
#'
#' - `opened_run` is the run that first raised the matched cluster at all,
#'   whatever it held then. This is the earliest date anything appeared
#'   on the board that turned out to be this outbreak, and it is the most
#'   generous of the three.
#' - `detected_run` is the first run at which the matched cluster held
#'   `min_recall` of the outbreak *as it stood that day*. This is the
#'   operational answer: the day a dossier existed that was mostly this
#'   outbreak.
#' - `captured_run` is the first run at which it held `min_recall` of the
#'   whole outbreak, cases still to come included. Necessarily on or
#'   after `detected_run`, and for a slowly accruing outbreak it can be
#'   much later, because it is partly a measure of how fast the outbreak
#'   grew.
#'
#' All three are reported. Picking one and calling it the delay would be
#' choosing a number rather than measuring one, and they can be days
#' apart.
#'
#' An outbreak nothing matched is not dropped, and its delay is not zero,
#' absent or imputed: `detected` is `FALSE`, every delay is `NA`, and
#' `censor_days` carries the time it was watched for, so time-to-
#' detection can be estimated with the misses in it as censored
#' observations rather than quietly left out of the median.
#'
#' @param overlap From `episodic_validation_overlap()`.
#' @param truth The `outbreaks` table from
#'   `episodic_synthetic_ground_truth()`.
#' @param truth_cases One row per injected case: `outbreak_id`,
#'   `source_key`, `sample_date`.
#' @param run_dates Every run date in the replay, in order.
#' @param min_recall,min_precision The matching thresholds.
#' @param cluster_opened The run each cluster first appeared in, as a
#'   `Date` vector named by `cluster_id`. `NULL` leaves `opened_run` and
#'   `delay_from_open` `NA`, which is what a caller with no cluster table
#'   in hand gets - not a zero, and not the first run date.
#' @return A data frame with one row per outbreak in `truth`.
#' @keywords internal
#' @noRd
episodic_validation_outbreak_rows <- function(overlap,
                                              truth,
                                              truth_cases,
                                              run_dates,
                                              min_recall = 0.5,
                                              min_precision = 0.5,
                                              cluster_opened = NULL) {
  run_dates <- sort(as.Date(run_dates))
  if (length(run_dates) == 0) {
    stop(
      "A validation replay with no run dates cannot say anything about ",
      "detection. Widen the evaluation window.",
      call. = FALSE
    )
  }
  last_run <- max(run_dates)
  first_run <- min(run_dates)
  final <- overlap[overlap$run_date == last_run, , drop = FALSE]

  rows <- lapply(seq_len(nrow(truth)), function(i) {
    ob <- truth[i, ]
    id <- ob$outbreak_id
    own <- truth_cases[truth_cases$outbreak_id == id, , drop = FALSE]
    own_dates <- sort(as.Date(own$sample_date))
    mine_final <- final[final$outbreak_id == id, , drop = FALSE]

    best <- if (nrow(mine_final) == 0) {
      NULL
    } else {
      mine_final[which.max(mine_final$recall_full), ]
    }
    detected <- !is.null(best) && best$recall_full >= min_recall
    cluster_id <- if (detected) best$cluster_id else NA_integer_

    mine_all <- overlap[overlap$outbreak_id == id, , drop = FALSE]
    detected_run <- episodic_validation_first_run(
      mine_all,
      cluster_id,
      "recall_asof",
      min_recall
    )
    captured_run <- episodic_validation_first_run(
      mine_all,
      cluster_id,
      "recall_full",
      min_recall
    )

    # Fragmentation. Two counts, because "how many dossiers was this
    # outbreak split across" and "how many dossiers did it touch at all"
    # are different operational costs, and the second is always at least
    # the first.
    touching <- unique(mine_final$cluster_id)
    attributable <- unique(
      mine_final$cluster_id[
        !is.na(mine_final$precision) & mine_final$precision >= min_precision
      ]
    )

    remaining <- if (is.na(detected_run)) {
      NA_integer_
    } else {
      sum(own_dates > detected_run)
    }
    opened_run <- if (is.na(cluster_id) || is.null(cluster_opened)) {
      as.Date(NA)
    } else {
      as.Date(unname(cluster_opened[as.character(cluster_id)]))
    }
    data.frame(
      outbreak_id = id,
      label = ob$label,
      pathogen = ob$pathogen,
      expected_channel = ob$expected_channel,
      expected_level = ob$expected_level,
      n_cases = nrow(own),
      first_day = ob$first_day,
      third_case_day = if (length(own_dates) >= 3) own_dates[3] else as.Date(NA),
      peak_day = ob$peak_day,
      last_day = ob$last_day,
      # An outbreak already over when the replay began was handed to the
      # first run whole, so its delay measures the replay's start date
      # and nothing else. Flagged rather than dropped: it still counts
      # towards sensitivity, and the flag is what keeps it out of the
      # timeliness figures.
      fully_prospective = ob$first_day >= first_run,
      detected = detected,
      cluster_id = cluster_id,
      recall_full = if (is.null(best)) NA_real_ else best$recall_full,
      precision = if (is.null(best)) NA_real_ else best$precision,
      opened_run = opened_run,
      detected_run = detected_run,
      captured_run = captured_run,
      delay_from_open = as.numeric(opened_run - ob$first_day),
      delay_from_first = as.numeric(detected_run - ob$first_day),
      delay_from_third = if (length(own_dates) >= 3) {
        as.numeric(detected_run - own_dates[3])
      } else {
        NA_real_
      },
      # Watched until its own last case, or until the replay stopped,
      # whichever came first: nothing can be said about days nobody ran.
      censor_days = as.numeric(min(ob$last_day, last_run) - ob$first_day),
      detected_before_peak = if (is.na(detected_run)) {
        NA
      } else {
        detected_run < ob$peak_day
      },
      n_cases_remaining = remaining,
      share_remaining = if (is.na(remaining)) {
        NA_real_
      } else {
        remaining / nrow(own)
      },
      n_clusters_attributable = length(attributable),
      n_clusters_touching = length(touching),
      stringsAsFactors = FALSE
    )
  })
  out <- if (length(rows) == 0) {
    episodic_validation_outbreak_rows_empty()
  } else {
    do.call(rbind, rows)
  }
  rownames(out) <- NULL
  out
}

#' The first run at which one cluster crossed a recall threshold
#'
#' @return A `Date`, or `NA` when the cluster is `NA` (nothing matched)
#'   or never crossed.
#' @keywords internal
#' @noRd
episodic_validation_first_run <- function(overlap_for_outbreak,
                                          cluster_id,
                                          column,
                                          threshold) {
  if (is.na(cluster_id) || nrow(overlap_for_outbreak) == 0) {
    return(as.Date(NA))
  }
  mine <- overlap_for_outbreak[
    overlap_for_outbreak$cluster_id == cluster_id, ,
    drop = FALSE
  ]
  value <- mine[[column]]
  crossed <- mine$run_date[!is.na(value) & value >= threshold]
  if (length(crossed) == 0) as.Date(NA) else min(crossed)
}

#' @keywords internal
#' @noRd
episodic_validation_outbreak_rows_empty <- function() {
  data.frame(
    outbreak_id = character(0),
    label = character(0),
    pathogen = character(0),
    expected_channel = character(0),
    expected_level = character(0),
    n_cases = integer(0),
    first_day = as.Date(character(0)),
    third_case_day = as.Date(character(0)),
    peak_day = as.Date(character(0)),
    last_day = as.Date(character(0)),
    fully_prospective = logical(0),
    detected = logical(0),
    cluster_id = integer(0),
    recall_full = numeric(0),
    precision = numeric(0),
    opened_run = as.Date(character(0)),
    detected_run = as.Date(character(0)),
    captured_run = as.Date(character(0)),
    delay_from_open = numeric(0),
    delay_from_first = numeric(0),
    delay_from_third = numeric(0),
    censor_days = numeric(0),
    detected_before_peak = logical(0),
    n_cases_remaining = integer(0),
    share_remaining = numeric(0),
    n_clusters_attributable = integer(0),
    n_clusters_touching = integer(0),
    stringsAsFactors = FALSE
  )
}

#' One row per cluster raised: true positive or false alarm
#'
#' A cluster is a true positive when at least `min_precision` of *its
#' own* cases belong to one seeded outbreak. Anything else is a false
#' alarm - including a cluster that holds every case of a seeded outbreak
#' and forty endemic ones besides, which is a real result and not a
#' technicality: a dossier that is one part outbreak to five parts
#' background is not a dossier about the outbreak.
#'
#' A cluster merged into another is not counted at all. It is not an
#' alarm an epidemiologist was ever shown; the survivor it merged into is
#' the one on the board.
#'
#' @param overlap From `episodic_validation_overlap()`, at any run.
#' @param clusters One row per cluster, with `cluster_id`, `n_cases`,
#'   `opened_run`, `suppressed`, `merged`, `priority_score`, `detectors`,
#'   `level` and `pathogen`.
#' @param min_precision The share of a cluster's own cases that must
#'   belong to one outbreak.
#' @return `clusters` with `outbreak_id`, `n_overlap`, `precision`,
#'   `recall_full` and `true_positive` added.
#' @keywords internal
#' @noRd
episodic_validation_cluster_rows <- function(overlap,
                                             clusters,
                                             min_precision = 0.5) {
  episodic_validation_require_columns(
    clusters,
    c("cluster_id", "n_cases", "merged"),
    "clusters"
  )
  n <- nrow(clusters)
  clusters$outbreak_id <- rep(NA_character_, n)
  clusters$n_overlap <- rep(0L, n)
  clusters$precision <- rep(0, n)
  clusters$recall_full <- rep(NA_real_, n)
  if (nrow(clusters) > 0 && nrow(overlap) > 0) {
    for (i in seq_len(nrow(clusters))) {
      mine <- overlap[overlap$cluster_id == clusters$cluster_id[i], ]
      if (nrow(mine) == 0) {
        next
      }
      best <- mine[which.max(mine$precision), ]
      clusters$outbreak_id[i] <- best$outbreak_id
      clusters$n_overlap[i] <- best$n_overlap
      clusters$precision[i] <- best$precision
      clusters$recall_full[i] <- best$recall_full
    }
  }
  # A cluster holding no seeded case at all has precision 0, and that is
  # a measurement: every one of its cases is background. Distinct from a
  # cluster with no cases at all, which cannot be scored and is NA.
  clusters$precision[clusters$n_cases == 0] <- NA_real_
  clusters$true_positive <- !is.na(clusters$precision) &
    clusters$precision >= min_precision
  clusters$counted <- !clusters$merged
  clusters
}

#' Refuse a data frame that is missing a column this needs
#' @keywords internal
#' @noRd
episodic_validation_require_columns <- function(x, columns, what) {
  if (!is.data.frame(x)) {
    stop("`", what, "` must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0) {
    stop(
      "`",
      what,
      "` is missing the column(s) ",
      paste0("`", missing, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
