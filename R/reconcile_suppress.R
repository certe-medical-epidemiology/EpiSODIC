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

#' Collapse one outbreak seen at several levels into one dossier
#'
#' The lattice watches the same cases at up to five levels at once, so a
#' single ward outbreak is a signal on that ward, in that hospital, and -
#' if it is large enough - in the area, the province and the region. All
#' five are true. Only one of them is a dossier worth opening, and which
#' one depends on where the rise actually sits:
#'
#' - A child suppresses its parent when it accounts for most of the
#'   parent's cases. The rise is local; the wider view is a restatement.
#' - A parent suppresses its children when the rise is spread across
#'   several of them and no single one dominates. The rise is diffuse;
#'   separate dossiers per area would be the same outbreak, filed five
#'   times.
#'
#' Nothing is discarded. A suppressed cluster keeps its cases, its
#' history and its assessment, and is attached to the cluster that
#' suppressed it (`episodic_cluster.suppressed_by`), which is where the
#' dossier shows it.
#'
#' Recomputed from scratch every run: a rise that was local last week and
#' has spread this week must be able to change which cluster survives, and
#' a suppression nothing justifies any more has to lift.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param config The resolved configuration; uses `config$suppression`.
#' @return Invisibly, the number of clusters suppressed.
#' @keywords internal
#' @noRd
episodic_suppress_lattice <- function(con, config) {
  sup <- config$suppression
  child_dominance <- as.numeric(sup$child_dominance_threshold %||% 0.7)
  parent_diffuse <- as.numeric(sup$parent_diffuse_threshold %||% 0.5)
  min_children <- as.integer(sup$parent_min_flagged_children %||% 2L)

  clusters <- episodic_db_clusters_for_suppression(con)
  if (nrow(clusters) == 0) {
    return(invisible(0L))
  }

  # Every run starts from no suppression at all, so that last run's
  # verdict cannot outlive the picture that justified it. One statement,
  # not one per suppressed cluster - the set is exactly what the WHERE
  # clause selects, so there is nothing for a loop to add.
  if (any(!is.na(clusters$suppressed_by))) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_cluster SET suppressed_by = NULL WHERE suppressed_by IS NOT NULL"
    )
  }
  clusters$suppressed_by <- NA_integer_

  n_suppressed <- 0L
  for (pathogen in unique(clusters$pathogen)) {
    same_pathogen <- clusters[clusters$pathogen == pathogen, ]
    for (pair in episodic_suppression_pairs()) {
      parents <- same_pathogen[same_pathogen$level == pair$parent, ]
      children <- same_pathogen[same_pathogen$level == pair$child, ]
      if (nrow(parents) == 0 || nrow(children) == 0) {
        next
      }

      for (p in seq_len(nrow(parents))) {
        parent <- parents[p, ]
        overlapping <- children[
          episodic_suppression_overlaps(parent, children),
        ]
        if (nrow(overlapping) == 0) {
          next
        }

        shares <- vapply(
          seq_len(nrow(overlapping)),
          function(k) {
            episodic_suppression_share(con, parent, overlapping[k, ])
          },
          numeric(1)
        )

        dominant <- which(shares >= child_dominance)
        if (length(dominant) > 0) {
          # The largest child of a parent it dominates: one cluster
          # survives, and it is the one the cases are actually in.
          strongest <- dominant[which.max(shares[dominant])]
          winner <- overlapping$cluster_id[strongest]
          n_suppressed <- n_suppressed +
            episodic_suppression_apply(con, parent$cluster_id, winner)
          next
        }

        diffuse <- all(shares < parent_diffuse) &&
          length(shares) >= min_children
        if (diffuse) {
          for (child_id in overlapping$cluster_id) {
            n_suppressed <- n_suppressed +
              episodic_suppression_apply(con, child_id, parent$cluster_id)
          }
        }
      }
    }
  }

  invisible(n_suppressed)
}

#' Which level is contained in which
#'
#' Two chains, not one: a ward is part of a hospital, and an area is part
#' of a province is part of the region. A hospital is not part of an area
#' here - the geographic levels group on the *patient's* postcode, so a
#' hospital's cases are scattered across areas rather than sitting in one.
#' @keywords internal
#' @noRd
episodic_suppression_pairs <- function() {
  list(
    list(child = "pathogen_ward", parent = "pathogen_institution"),
    list(child = "pathogen_area", parent = "pathogen_province"),
    list(child = "pathogen_province", parent = "pathogen_region")
  )
}

#' Do two clusters describe the same episode in time?
#' @keywords internal
#' @noRd
episodic_suppression_overlaps <- function(parent, children) {
  as.Date(children$first_day) <= as.Date(parent$last_day) &
    as.Date(children$last_day) >= as.Date(parent$first_day)
}

#' How much of the parent is the child?
#'
#' Counted in cases the two actually share, not in the size of each: two
#' unrelated clusters of similar size in the same weeks would otherwise
#' read as one dominating the other.
#' @keywords internal
#' @noRd
episodic_suppression_share <- function(con, parent, child) {
  parent_cases <- episodic_db_cluster_cases(con, parent$cluster_id)$case_id
  if (length(parent_cases) == 0) {
    return(0)
  }
  child_cases <- episodic_db_cluster_cases(con, child$cluster_id)$case_id
  length(intersect(parent_cases, child_cases)) / length(parent_cases)
}

#' Record one suppression, unless it would suppress an assessed cluster
#'
#' A cluster somebody has already classified stays in the queue whatever
#' the lattice says about it: the board's own record of a decision is not
#' something a later run gets to hide.
#' @keywords internal
#' @noRd
episodic_suppression_apply <- function(con, cluster_id, suppressed_by) {
  if (nrow(episodic_db_assessment_events(con, cluster_id)) > 0) {
    return(0L)
  }
  episodic_db_cluster_set_suppressed_by(con, cluster_id, suppressed_by)
  1L
}
