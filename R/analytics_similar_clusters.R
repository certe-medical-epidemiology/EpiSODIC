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

#' Find the most similar closed clusters ("Vergelijkbare clusters")
#'
#' A panel on the dossier surfacing the three most similar assessed
#' clusters from the archive, matched on pathogen, level, size, season
#' and duration, each showing what was decided and why. At a typical
#' rate of ten assessed clusters a month, two hundred precedents
#' accumulate within two years, which makes the archive a decision aid
#' rather than a filing cabinet.
#'
#' Matching requires the same pathogen (an exact string match, as
#' everywhere else pathogen identity is compared in this codebase) and
#' only ever considers *closed* clusters (an open one is not yet a
#' precedent). Similarity within that pool is a documented scoring rule
#' across four dimensions, each in `[0, 1]`:
#'
#' - Level: +1 if the same lattice level, else 0.
#' - Size: `1 / (1 + |log((n1+1)/(n2+1))|)` - a smooth similarity that
#'   treats a doubling and a halving of case count symmetrically.
#' - Season: circular day-of-year distance between the two clusters'
#'   `first_day`, so a cluster starting in late December and one in early
#'   January score as seasonally close, not far apart.
#' - Duration: `1 / (1 + |dur1 - dur2| / 7)`, in days.
#'
#' The total is their unweighted sum, ranking candidates within the
#' same-pathogen pool.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The cluster to find precedents for (excluded from its
#'   own results).
#' @param lang Session language.
#' @param n Maximum number of results.
#' @return A data frame, one row per similar cluster, the `n` most similar
#'   first: `cluster_id`, `pathogen`, `level_label`, `place`, `first_day`,
#'   `last_day`, `n_cases`, `case_days`, `priority_score`, `verdict_label`,
#'   `closed_at`. Zero rows if there is no closed precedent for this
#'   pathogen. Similarity decides *which* rows come back; the panel that
#'   renders them re-sorts into `episodic_cluster_table_order()`, like
#'   every other cluster table.
#' @keywords internal
#' @noRd
episodic_app_similar_clusters <- function(
    con,
    cluster_id,
    lang = Sys.getenv("EPISODIC_LANGUAGE"),
    n = 3L) {
  empty <- data.frame(
    cluster_id = integer(0),
    pathogen = character(0),
    level_label = character(0),
    place = character(0),
    first_day = character(0),
    last_day = character(0),
    n_cases = integer(0),
    case_days = integer(0),
    priority_score = numeric(0),
    verdict_label = character(0),
    closed_at = character(0),
    stringsAsFactors = FALSE
  )

  target <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(target) == 0) {
    return(empty)
  }
  target <- target[1, ]
  target_stream <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_stream WHERE stream_id = ?",
    params = list(target$stream_id)
  )[1, ]

  clusters <- episodic_db_clusters(con, open_only = TRUE)
  clusters <- clusters[clusters$cluster_id != cluster_id, ]
  if (nrow(clusters) == 0) {
    return(empty)
  }

  streams <- episodic_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(
    clusters$stream_id,
    streams$stream_id
  )]
  clusters <- clusters[
    !is.na(clusters$pathogen) & clusters$pathogen == target_stream$pathogen,
  ]
  if (nrow(clusters) == 0) {
    return(empty)
  }

  # Every cluster of this pathogen, not just the handful that survive the
  # ranking below - so deriving state one cluster at a time meant five
  # queries each across the whole history before anything was filtered.
  # That is what made opening a dossier slow once a pathogen had accrued a
  # few hundred clusters; the batch form is three queries for the lot.
  clusters$state <- episodic_app_derive_states_batch(con, clusters)
  clusters <- clusters[clusters$state == "closed", ]
  if (nrow(clusters) == 0) {
    return(empty)
  }

  clusters$level <- streams$level[match(clusters$stream_id, streams$stream_id)]
  clusters$care_line <- streams$care_line[match(
    clusters$stream_id,
    streams$stream_id
  )]

  doy_dist <- function(d1, d2) {
    doy1 <- as.integer(format(as.Date(d1), "%j"))
    doy2 <- as.integer(format(as.Date(d2), "%j"))
    diff <- abs(doy1 - doy2)
    pmin(diff, 365 - diff)
  }
  dur <- function(first, last) as.numeric(as.Date(last) - as.Date(first))

  target_dur <- dur(target$first_day, target$last_day)
  clusters$score <-
    ifelse(clusters$level == target_stream$level, 1, 0) +
    1 / (1 + abs(log((target$n_cases + 1) / (clusters$n_cases + 1)))) +
    (1 - doy_dist(target$first_day, clusters$first_day) / 182.625) +
    1 / (1 + abs(target_dur - dur(clusters$first_day, clusters$last_day)) / 7)

  clusters <- clusters[order(-clusters$score), ]
  clusters <- utils::head(clusters, n)

  institutions <- DBI::dbGetQuery(con, "SELECT * FROM episodic_institution")
  clusters$level_label <- vapply(
    seq_len(nrow(clusters)),
    function(i) {
      episodic_app_level_label(
        clusters$level[i],
        clusters$care_line[i],
        lang = lang
      )
    },
    character(1)
  )
  clusters$place <- vapply(
    seq_len(nrow(clusters)),
    function(i) {
      stream <- streams[streams$stream_id == clusters$stream_id[i], ][1, ]
      institution <- if (!is.na(stream$institution_id)) {
        institutions[institutions$institution_id == stream$institution_id, ][
          1,
        ]
      } else {
        NULL
      }
      episodic_app_place_label(stream, institution, lang = lang)
    },
    character(1)
  )
  events_all <- episodic_db_assessment_events_batch(con, clusters$cluster_id)
  clusters$verdict_label <- vapply(
    seq_len(nrow(clusters)),
    function(i) {
      events <- events_all[events_all$cluster_id == clusters$cluster_id[i], ]
      classified <- events[!is.na(events$verdict), ]
      if (nrow(classified) == 0) {
        return(NA_character_)
      }
      episodic_verdict_label(
        classified$verdict[nrow(classified)],
        level = clusters$level[i],
        lang = lang
      )
    },
    character(1)
  )
  clusters$closed_at <- episodic_app_closed_at_from(
    episodic_db_cluster_states_batch(con, clusters$cluster_id),
    clusters$cluster_id
  )
  clusters <- episodic_db_attach_case_days(con, clusters)

  clusters[, c(
    "cluster_id",
    "pathogen",
    "level_label",
    "place",
    "first_day",
    "last_day",
    "n_cases",
    "case_days",
    "priority_score",
    "verdict_label",
    "closed_at"
  )]
}
