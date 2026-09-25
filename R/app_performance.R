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

#' @keywords internal
#' @noRd
episodic_ui_performance_screen <- function(performance,
                                           lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  by_dp <- performance$by_detector_pathogen
  dist <- performance$classification_distribution
  t <- performance$timeliness

  timeliness_row <- function(label, entry) {
    value <- if (entry$n == 0 || is.na(entry$median_days)) {
      episodic_tr("misc.dash", lang = lang)
    } else {
      episodic_tr(
        "performance.timeliness_value",
        days = episodic_format_number(entry$median_days, digits = 1, lang = lang),
        n = episodic_format_number(entry$n, lang = lang),
        lang = lang
      )
    }
    shiny::tags$tr(shiny::tags$td(label), shiny::tags$td(value))
  }

  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(
      class = "episodic-screen-title",
      episodic_tr("performance.title", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:8px;",
      episodic_tr("performance.note", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:16px;",
      episodic_tr("performance.outbreaks_only_note", lang = lang)
    ),
    shiny::tags$h2(
      style = "font-size:15px;font-weight:600;margin:20px 0 6px;",
      episodic_tr("performance.timeliness_title", lang = lang)
    ),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$tbody(
        timeliness_row(
          episodic_tr("performance.to_detection", lang = lang),
          t$to_detection
        ),
        timeliness_row(
          episodic_tr("performance.to_first_assessment", lang = lang),
          t$to_first_assessment
        ),
        timeliness_row(
          episodic_tr("performance.to_classification", lang = lang),
          t$to_classification
        )
      )
    ),
    shiny::tags$h2(
      style = "font-size:15px;font-weight:600;margin:20px 0 6px;",
      episodic_tr("performance.distribution_title", lang = lang)
    ),
    if (nrow(dist) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("performance.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("performance.col.verdict", lang = lang)),
          shiny::tags$th(episodic_tr("performance.col.n", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(dist)), function(i) {
          shiny::tags$tr(
            shiny::tags$td(dist$verdict_label[i]),
            shiny::tags$td(episodic_format_number(dist$n[i], lang = lang))
          )
        }))
      )
    },
    shiny::tags$h2(
      style = "font-size:15px;font-weight:600;margin:20px 0 6px;",
      episodic_tr("performance.ppv_title", lang = lang)
    ),
    if (nrow(by_dp) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("performance.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("performance.col.detector", lang = lang)),
          shiny::tags$th(episodic_tr("performance.col.pathogen", lang = lang)),
          shiny::tags$th(episodic_tr(
            "performance.col.n_detections",
            lang = lang
          )),
          shiny::tags$th(episodic_tr("performance.col.ppv", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(by_dp)), function(i) {
          row <- by_dp[i, ]
          shiny::tags$tr(
            shiny::tags$td(shiny::HTML(episodic_ui_code_join(row$detector))),
            shiny::tags$td(shiny::HTML(episodic_ui_italicise_taxon(
              row$pathogen
            ))),
            shiny::tags$td(episodic_format_number(
              row$n_detections,
              lang = lang
            )),
            shiny::tags$td(
              if (is.na(row$ppv)) {
                episodic_tr("misc.dash", lang = lang)
              } else {
                paste0(
                  episodic_format_number(
                    row$ppv * 100,
                    digits = 0,
                    lang = lang
                  ),
                  "%"
                )
              }
            )
          )
        }))
      )
    }
  )
}

#' The Performance screen: positive predictive value, timeliness, classification mix
#'
#' Positive predictive value per detector per pathogen, computed from
#' the stored verdicts; time from first case to detection; and time from
#' detection to first assessment. This is the evidence base for tuning
#' decisions - how the eligibility gate gets calibrated towards a
#' realistic monthly assessment volume - and is worth publishing in its
#' own right. Timeliness is read from the `episodic_cluster`/
#' `episodic_assessment_event` timestamps directly (`opened_at`,
#' `first_day`, each cluster's own first assessment/classification
#' event) rather than reconstructed from state transitions, since those
#' timestamps are already recorded on the two tables this screen reads.
#'
#' A terminal verdict decides "positive predictive": `possible_epidemic`
#' and `confirmed_epidemic` count as a true positive for every detector
#' that ever flagged the cluster; `artefact` and `expected_variation`
#' count as a false positive. `cluster_not_yet` (still undecided) and an
#' unassessed cluster are excluded from PPV entirely - they are not yet
#' a judgement either way, not a form of "wrong".
#'
#' This is a cheap read, same as the rest of `R/app_read.R`; the
#' aggregation happens in R over already-fetched rows, not in SQL, since
#' SQLite's own `GROUP BY` gains nothing at the volumes this system runs
#' at.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language, for verdict labels.
#' @return A list with `by_detector_pathogen` (a data frame: `detector`,
#'   `pathogen`, `n_detections`, `n_true_positive`, `n_false_positive`,
#'   `ppv`), `classification_distribution` (a data frame: `verdict`,
#'   `verdict_label`, `n`), and `timeliness` (a list of three
#'   `list(median_days, n)` entries: `to_detection`,
#'   `to_first_assessment`, `to_classification` - `median_days` is `NA`
#'   when `n` is `0`).
#' @keywords internal
#' @noRd
episodic_app_performance <- function(con,
                                     lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  detections <- episodic_db_get_query(
    con,
    "SELECT DISTINCT cluster_id, detector FROM episodic_detection WHERE cluster_id IS NOT NULL"
  )
  # Including suppressed ones: this screen measures how the detectors
  # behaved, not what the queue looked like, and a detection that fired is
  # a detection whichever level of the lattice ended up carrying it.
  # Outbreaks only: epidemic-scale clusters have a different assessment
  # vocabulary (declarations rather than classifications), so mixing the
  # two denominators would produce a number about neither.
  clusters <- episodic_db_clusters(con, include_suppressed = TRUE)
  clusters <- clusters[clusters$scale == "outbreak", , drop = FALSE]
  streams <- episodic_db_streams(con, active_only = FALSE)
  outbreak_ids <- clusters$cluster_id
  detections <- detections[detections$cluster_id %in% outbreak_ids, , drop = FALSE]
  events <- episodic_db_get_query(
    con,
    "SELECT cluster_id, created_at, verdict FROM episodic_assessment_event ORDER BY created_at"
  )
  events <- events[events$cluster_id %in% outbreak_ids, , drop = FALSE]
  verdicts <- events[!is.na(events$verdict), , drop = FALSE]
  # last row per cluster_id, keeping input order (created_at ascending)
  latest_verdict <- verdicts[
    !duplicated(verdicts$cluster_id, fromLast = TRUE),
    c("cluster_id", "verdict")
  ]

  list(
    by_detector_pathogen = episodic_performance_ppv(
      detections,
      clusters,
      streams,
      latest_verdict
    ),
    classification_distribution = episodic_performance_classification_distribution(
      latest_verdict,
      lang = lang
    ),
    timeliness = episodic_performance_timeliness(clusters, events, verdicts)
  )
}

#' The verdicts that decide "positive predictive"
#'
#' A terminal verdict either confirms a detection (true positive) or
#' rejects it (false positive). Every other verdict, and no verdict at
#' all, is not yet a judgement either way and counts as neither.
#'
#' @keywords internal
#' @noRd
episodic_performance_true_verdicts <- c("possible_epidemic", "confirmed_epidemic")

#' @keywords internal
#' @noRd
episodic_performance_false_verdicts <- c("artefact", "expected_variation")

#' The instance's overall positive predictive value, one per outbreak
#'
#' The figure the Instance screen's Performance card carries: of the
#' outbreaks a detector fired on that an epidemiologist has since judged,
#' the share judged real. It counts each outbreak once, on its latest
#' verdict, where the per-detector table counts every detector that
#' flagged it, and it restricts to the same population that table does
#' (outbreak-scale clusters with at least one detection, suppressed ones
#' included). It is two indexed reads rather than
#' `episodic_app_performance()`, so opening the Instance screen stays
#' close to free.
#'
#' @param con A [DBI::DBIConnection-class].
#' @return A single number in `[0, 1]`, or `NA_real_` when no such
#'   outbreak has a terminal verdict yet. That is "nothing to measure",
#'   never a PPV of zero.
#' @keywords internal
#' @noRd
episodic_app_overall_ppv <- function(con) {
  verdicts <- episodic_db_get_query(
    con,
    "SELECT e.cluster_id, e.verdict
       FROM episodic_assessment_event e
       JOIN episodic_cluster c ON c.cluster_id = e.cluster_id
      WHERE e.verdict IS NOT NULL
        AND c.scale = 'outbreak'
        AND EXISTS (
          SELECT 1 FROM episodic_detection d WHERE d.cluster_id = e.cluster_id
        )
      ORDER BY e.created_at"
  )
  latest <- verdicts$verdict[!duplicated(verdicts$cluster_id, fromLast = TRUE)]
  n_true <- sum(latest %in% episodic_performance_true_verdicts)
  n_false <- sum(latest %in% episodic_performance_false_verdicts)
  if (n_true + n_false == 0) {
    return(NA_real_)
  }
  n_true / (n_true + n_false)
}

#' @keywords internal
#' @noRd
episodic_performance_ppv <- function(detections,
                                     clusters,
                                     streams,
                                     latest_verdict) {
  empty <- data.frame(
    detector = character(0),
    pathogen = character(0),
    n_detections = integer(0),
    n_true_positive = integer(0),
    n_false_positive = integer(0),
    ppv = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(detections) == 0) {
    return(empty)
  }

  detections$stream_id <- clusters$stream_id[match(
    detections$cluster_id,
    clusters$cluster_id
  )]
  detections$pathogen <- streams$pathogen[match(
    detections$stream_id,
    streams$stream_id
  )]
  detections$verdict <- latest_verdict$verdict[match(
    detections$cluster_id,
    latest_verdict$cluster_id
  )]
  detections$is_true <- ifelse(
    !is.na(detections$verdict) &
      detections$verdict %in% episodic_performance_true_verdicts,
    1L,
    0L
  )
  detections$is_false <- ifelse(
    !is.na(detections$verdict) &
      detections$verdict %in% episodic_performance_false_verdicts,
    1L,
    0L
  )

  agg <- stats::aggregate(
    cbind(
      n_true_positive = is_true,
      n_false_positive = is_false,
      n_detections = 1L
    ) ~ detector + pathogen,
    data = detections,
    FUN = sum
  )
  agg$ppv <- ifelse(
    (agg$n_true_positive + agg$n_false_positive) > 0,
    agg$n_true_positive / (agg$n_true_positive + agg$n_false_positive),
    NA_real_
  )
  agg[
    order(agg$pathogen, agg$detector),
    c(
      "detector",
      "pathogen",
      "n_detections",
      "n_true_positive",
      "n_false_positive",
      "ppv"
    )
  ]
}

#' @keywords internal
#' @noRd
episodic_performance_classification_distribution <- function(latest_verdict,
                                                             lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (nrow(latest_verdict) == 0) {
    return(data.frame(
      verdict = character(0),
      verdict_label = character(0),
      n = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  tab <- table(latest_verdict$verdict)
  dist <- data.frame(
    verdict = names(tab),
    n = as.integer(tab),
    stringsAsFactors = FALSE
  )
  # No `level`: this counts verdicts across every cluster system-wide,
  # a mix of every level, so there is no single level to word the label
  # for - kept as the general "epidemic" wording rather than guessing.
  dist$verdict_label <- vapply(
    dist$verdict,
    episodic_verdict_label,
    character(1),
    lang = lang
  )
  dist[order(-dist$n), c("verdict", "verdict_label", "n")]
}

#' @keywords internal
#' @noRd
episodic_performance_timeliness <- function(clusters, events, verdicts) {
  summarise_days <- function(x) {
    x <- x[!is.na(x)]
    list(
      median_days = if (length(x) > 0) stats::median(x) else NA_real_,
      n = length(x)
    )
  }
  if (nrow(clusters) == 0) {
    empty <- summarise_days(numeric(0))
    return(list(
      to_detection = empty,
      to_first_assessment = empty,
      to_classification = empty
    ))
  }

  first_assessment <- events[
    !duplicated(events$cluster_id),
    c("cluster_id", "created_at")
  ]
  names(first_assessment)[2] <- "first_assessment_at"
  first_classification <- verdicts[
    !duplicated(verdicts$cluster_id),
    c("cluster_id", "created_at")
  ]
  names(first_classification)[2] <- "first_classification_at"

  t <- clusters[, c("cluster_id", "first_day", "opened_at")]
  # A cluster opened by a backfill run was opened on the day the archive
  # was imported, whatever its cases are dated, so `opened_at -
  # first_day` measures the age of the extract rather than how quickly
  # anything was noticed. Dropped from time-to-detection outright: a
  # median taken over the two mixed together is a number about neither.
  #
  # Only from this one measure. The two below run from `opened_at`
  # forwards and time an epidemiologist rather than a detector, which is
  # as honest for a backfilled cluster as for any other. And PPV needs no
  # exclusion at all - it already counts only clusters carrying a
  # verdict, and a backfilled cluster that a person did assess is
  # evidence like any other.
  #
  # Carried as a column rather than as an index, because `merge()` below
  # reorders rows and a position taken before it would point at the wrong
  # cluster after it. A database migrated from before the column existed
  # reads `NA` here, which `%in%` treats as "not a backfill" - true of
  # every cluster opened before any run could report an archive.
  t$opened_in_backfill <- clusters$opened_in_backfill[match(
    t$cluster_id,
    clusters$cluster_id
  )]

  t <- merge(t, first_assessment, by = "cluster_id", all.x = TRUE)
  t <- merge(t, first_classification, by = "cluster_id", all.x = TRUE)
  detected_live <- !(t$opened_in_backfill %in% 1)

  list(
    to_detection = summarise_days(as.numeric(
      as.Date(t$opened_at[detected_live]) - as.Date(t$first_day[detected_live])
    )),
    to_first_assessment = summarise_days(as.numeric(
      as.Date(t$first_assessment_at) - as.Date(t$opened_at)
    )),
    to_classification = summarise_days(as.numeric(
      as.Date(t$first_classification_at) - as.Date(t$opened_at)
    ))
  )
}
