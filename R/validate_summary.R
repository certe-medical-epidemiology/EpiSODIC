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

# Assembling the per-outbreak, per-cluster and per-run tables into the
# headline numbers. Pure, like the rest of the harness's arithmetic.

#' Every headline number, in one long table
#'
#' Long rather than wide on purpose: the metrics have different shapes -
#' proportions with binomial intervals, rates with Poisson ones, medians
#' with interquartile ranges - and forcing them into one wide row per
#' group would mean a column that means something different depending on
#' which row you are reading. One row per (metric, group) keeps each
#' number next to the denominator and the interval that belong to it,
#' which is also the shape a results table in a paper wants.
#'
#' @param outbreaks Per-(seed, outbreak) rows, from
#'   `episodic_validation_outbreak_rows()` with a `seed` column added.
#' @param clusters Per-(seed, cluster) rows, from
#'   `episodic_validation_cluster_rows()` with a `seed` column added.
#' @param runs Per-(seed, run) rows: `seed`, `run_date`, `n_streams`,
#'   `n_clusters_new`, `n_clusters_new_visible`.
#' @return A data frame in `episodic_validation_summary_empty()`'s shape.
#' @keywords internal
#' @noRd
episodic_validation_summarise <- function(outbreaks, clusters, runs) {
  seeds <- sort(unique(runs$seed))
  rows <- list()
  add <- function(row) {
    rows[[length(rows) + 1]] <<- row
    invisible(NULL)
  }

  per_seed <- function(df, fun) {
    vapply(
      seeds,
      function(s) as.numeric(fun(df[df$seed == s, , drop = FALSE])),
      numeric(1)
    )
  }
  counted <- clusters[clusters$counted, , drop = FALSE]
  visible <- counted[!counted$suppressed, , drop = FALSE]
  timeliness <- outbreaks[
    outbreaks$detected & outbreaks$fully_prospective, ,
    drop = FALSE
  ]

  # ------------------------------------------------------------------
  # 1. Detection performance
  add(episodic_validation_proportion_row(
    "sensitivity",
    "overall",
    "all outbreaks",
    per_seed(outbreaks, function(d) sum(d$detected)),
    per_seed(outbreaks, nrow)
  ))
  for (id in sort(unique(outbreaks$outbreak_id))) {
    mine <- outbreaks[outbreaks$outbreak_id == id, , drop = FALSE]
    add(episodic_validation_proportion_row(
      "sensitivity",
      "outbreak_shape",
      id,
      per_seed(mine, function(d) sum(d$detected)),
      per_seed(mine, nrow)
    ))
  }
  for (channel in sort(unique(outbreaks$expected_channel))) {
    mine <- outbreaks[outbreaks$expected_channel == channel, , drop = FALSE]
    add(episodic_validation_proportion_row(
      "sensitivity",
      "design_channel",
      channel,
      per_seed(mine, function(d) sum(d$detected)),
      per_seed(mine, nrow)
    ))
  }

  add(episodic_validation_proportion_row(
    "ppv",
    "overall",
    "all clusters raised",
    per_seed(counted, function(d) sum(d$true_positive)),
    per_seed(counted, nrow)
  ))
  add(episodic_validation_proportion_row(
    "ppv",
    "overall",
    "clusters left after lattice suppression",
    per_seed(visible, function(d) sum(d$true_positive)),
    per_seed(visible, nrow)
  ))
  for (detector in episodic_validation_detector_groups(counted$first_detector)) {
    mine <- counted[which(counted$first_detector == detector), , drop = FALSE]
    add(episodic_validation_proportion_row(
      "ppv",
      "first_detector",
      detector,
      per_seed(mine, function(d) sum(d$true_positive)),
      per_seed(mine, nrow)
    ))
  }
  for (detector in episodic_validation_detector_groups(counted$detectors, split = TRUE)) {
    mine <- counted[
      which(vapply(
        strsplit(counted$detectors, "+", fixed = TRUE),
        function(x) detector %in% x,
        logical(1)
      )), ,
      drop = FALSE
    ]
    add(episodic_validation_proportion_row(
      "ppv",
      "any_detector",
      detector,
      per_seed(mine, function(d) sum(d$true_positive)),
      per_seed(mine, nrow)
    ))
  }

  stream_weeks <- per_seed(runs, function(d) sum(d$n_streams))
  add(episodic_validation_rate_row(
    "false_alarms",
    "overall",
    "all clusters raised",
    per_seed(counted, function(d) sum(!d$true_positive)),
    stream_weeks,
    unit = "per stream-week"
  ))
  add(episodic_validation_rate_row(
    "false_alarms",
    "overall",
    "clusters left after lattice suppression",
    per_seed(visible, function(d) sum(!d$true_positive)),
    stream_weeks,
    unit = "per stream-week"
  ))
  add(episodic_validation_rate_row(
    "alarms",
    "overall",
    "all clusters raised",
    per_seed(counted, nrow),
    stream_weeks,
    unit = "per stream-week"
  ))

  add(episodic_validation_value_row(
    "case_recall",
    "overall",
    "best-matching cluster per outbreak",
    outbreaks$seed,
    outbreaks$recall_full,
    unit = "share of the outbreak"
  ))
  for (id in sort(unique(outbreaks$outbreak_id))) {
    mine <- outbreaks[outbreaks$outbreak_id == id, , drop = FALSE]
    add(episodic_validation_value_row(
      "case_recall",
      "outbreak_shape",
      id,
      mine$seed,
      mine$recall_full,
      unit = "share of the outbreak"
    ))
  }
  add(episodic_validation_value_row(
    "case_precision",
    "overall",
    "all clusters raised",
    counted$seed,
    counted$precision,
    unit = "share of the cluster"
  ))
  add(episodic_validation_value_row(
    "fragmentation",
    "overall",
    "clusters one detected outbreak was split across",
    outbreaks$seed[outbreaks$detected],
    outbreaks$n_clusters_attributable[outbreaks$detected],
    unit = "clusters"
  ))

  # Which channel found each shape. Not the same question as PPV per
  # detector, and not answerable by it: this is what actually got there
  # first for an outbreak that is known to be real, per seed, so a
  # detector that wins in half the realisations is visible as half.
  detected_outbreaks <- outbreaks[outbreaks$detected, , drop = FALSE]
  for (id in sort(unique(detected_outbreaks$outbreak_id))) {
    mine <- detected_outbreaks[
      detected_outbreaks$outbreak_id == id, ,
      drop = FALSE
    ]
    for (detector in episodic_validation_detector_groups(mine$first_detector)) {
      add(episodic_validation_proportion_row(
        "found_first_by",
        "outbreak_shape",
        paste(id, detector, sep = ": "),
        per_seed(mine, function(d) {
          sum(!is.na(d$first_detector) & d$first_detector == detector)
        }),
        per_seed(mine, function(d) sum(!is.na(d$first_detector)))
      ))
    }
  }

  # ------------------------------------------------------------------
  # 2. Timeliness
  # Three delays, because the day something first appeared on the board,
  # the day what appeared was mostly this outbreak, and the day it held
  # most of the whole outbreak are three different claims, and they can
  # be days apart. Reporting one of them as "the delay" would be a
  # choice rather than a measurement.
  add(episodic_validation_value_row(
    "delay_from_cluster_opening",
    "overall",
    "detected outbreaks that began inside the window",
    timeliness$seed,
    timeliness$delay_from_open,
    unit = "days"
  ))
  add(episodic_validation_value_row(
    "delay_from_first_case",
    "overall",
    "detected outbreaks that began inside the window",
    timeliness$seed,
    timeliness$delay_from_first,
    unit = "days"
  ))
  add(episodic_validation_value_row(
    "delay_from_third_case",
    "overall",
    "detected outbreaks that began inside the window",
    timeliness$seed,
    timeliness$delay_from_third,
    unit = "days"
  ))
  for (id in sort(unique(timeliness$outbreak_id))) {
    mine <- timeliness[timeliness$outbreak_id == id, , drop = FALSE]
    add(episodic_validation_value_row(
      "delay_from_first_case",
      "outbreak_shape",
      id,
      mine$seed,
      mine$delay_from_first,
      unit = "days"
    ))
  }
  add(episodic_validation_proportion_row(
    "never_detected",
    "overall",
    "all outbreaks",
    per_seed(outbreaks, function(d) sum(!d$detected)),
    per_seed(outbreaks, nrow)
  ))

  # The censored estimate, over every outbreak that began inside the
  # window - the misses included, as censored observations. Quoted
  # alongside `never_detected`, never on its own: a median delay without
  # the share never detected beside it is the number this whole section
  # exists to avoid.
  prospective <- outbreaks[outbreaks$fully_prospective, , drop = FALSE]
  km <- episodic_validation_km(
    ifelse(
      prospective$detected,
      prospective$delay_from_first,
      prospective$censor_days
    ),
    prospective$detected
  )
  add(episodic_validation_summary_single(
    "time_to_detection_km_median",
    "overall",
    "outbreaks that began inside the window, misses censored",
    estimate = episodic_validation_km_median(km),
    numerator = sum(prospective$detected),
    denominator = nrow(prospective),
    n_seeds = length(seeds),
    unit = "days"
  ))

  add(episodic_validation_proportion_row(
    "detected_before_peak",
    "overall",
    "detected outbreaks that began inside the window",
    per_seed(timeliness, function(d) sum(d$detected_before_peak, na.rm = TRUE)),
    per_seed(timeliness, function(d) sum(!is.na(d$detected_before_peak)))
  ))
  add(episodic_validation_value_row(
    "cases_still_to_come_at_detection",
    "overall",
    "detected outbreaks that began inside the window",
    timeliness$seed,
    timeliness$share_remaining,
    unit = "share of the outbreak"
  ))

  # ------------------------------------------------------------------
  # 3. Alarm burden
  add(episodic_validation_value_row(
    "clusters_raised_per_run",
    "overall",
    "before lattice suppression",
    runs$seed,
    runs$n_clusters_new,
    unit = "clusters per run"
  ))
  add(episodic_validation_value_row(
    "clusters_raised_per_run",
    "overall",
    "after lattice suppression",
    runs$seed,
    runs$n_clusters_new_visible,
    unit = "clusters per run"
  ))
  add(episodic_validation_value_row(
    "streams_watched_per_run",
    "overall",
    "all streams",
    runs$seed,
    runs$n_streams,
    unit = "streams"
  ))

  # ------------------------------------------------------------------
  # 5. Triage quality
  auc <- vapply(
    seeds,
    function(s) {
      mine <- counted[counted$seed == s, , drop = FALSE]
      episodic_validation_auc(mine$priority_score, mine$true_positive)
    },
    numeric(1)
  )
  add(episodic_validation_value_row(
    "priority_score_auc",
    "overall",
    "true positives against false alarms",
    seeds,
    auc,
    unit = "AUC"
  ))
  add(episodic_validation_value_row(
    "priority_score",
    "cluster_class",
    "true positive",
    counted$seed[counted$true_positive],
    counted$priority_score[counted$true_positive],
    unit = "score"
  ))
  add(episodic_validation_value_row(
    "priority_score",
    "cluster_class",
    "false alarm",
    counted$seed[!counted$true_positive],
    counted$priority_score[!counted$true_positive],
    unit = "score"
  ))

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' A summary row for a single already-computed number
#'
#' For a metric that is estimated once over everything rather than per
#' seed and then summarised - the Kaplan-Meier median, whose whole point
#' is that it pools the censored observations with the uncensored ones.
#' @keywords internal
#' @noRd
episodic_validation_summary_single <- function(metric,
                                               group_type,
                                               group,
                                               estimate,
                                               numerator,
                                               denominator,
                                               n_seeds,
                                               unit) {
  data.frame(
    metric = metric,
    group_type = group_type,
    group = group,
    n_seeds = n_seeds,
    median = NA_real_,
    q25 = NA_real_,
    q75 = NA_real_,
    numerator = numerator,
    denominator = denominator,
    estimate = estimate,
    ci_low = NA_real_,
    ci_high = NA_real_,
    interval = NA_character_,
    unit = unit,
    stringsAsFactors = FALSE
  )
}

#' The detector labels present in a column, in a stable order
#'
#' `split = TRUE` breaks the "+"-joined combinations apart, so a cluster
#' two detectors both fired on counts towards each of them.
#' @keywords internal
#' @noRd
episodic_validation_detector_groups <- function(x, split = FALSE) {
  x <- x[!is.na(x) & nzchar(x)]
  if (split) {
    x <- unlist(strsplit(x, "+", fixed = TRUE))
  }
  sort(unique(x))
}
