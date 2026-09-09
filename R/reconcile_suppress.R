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
#' Chains are not formed. `suppressed_by` is followed exactly one step
#' by everything that reads it (`episodic_db_clusters_suppressed_by()`,
#' the dossier), so a cluster suppressed this run never goes on to
#' suppress another, and a cluster that has already absorbed one is
#' never itself suppressed. Without that, the geographic chain
#' (area -> province -> region) could put a cluster behind one that is
#' itself behind a third: the reader follows the link out of the queue
#' and lands somewhere that is also not in the queue, and the cluster at
#' the far end is shown on no dossier at all. First writer wins, and the
#' effect of the rule is always to leave a cluster visible rather than
#' to hide it - which is the direction a surveillance system should err
#' in.
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

  # Every cluster that has been suppressed this run, and every cluster
  # that has suppressed one. A cluster in either set is out of the
  # running for the other role - see this function's own documentation
  # for why chains are refused rather than resolved.
  suppressed_ids <- integer(0)
  suppressor_ids <- integer(0)
  free_to_suppress <- function(id) !(id %in% suppressed_ids)
  free_to_be_suppressed <- function(id) !(id %in% suppressor_ids)

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

        # Read once per parent, not once per child: the shares are all
        # measured against the same set, and a parent with twenty
        # overlapping children was re-fetching it twenty times.
        parent_cases <- episodic_db_cluster_cases(
          con,
          parent$cluster_id
        )$case_id
        # Nothing to measure a share against. Zero shares would otherwise
        # read as "spread thinly across the children", which is a
        # statement about the data this parent does not have.
        if (length(parent_cases) == 0) {
          next
        }

        shares <- vapply(
          seq_len(nrow(overlapping)),
          function(k) {
            episodic_suppression_share(
              con,
              parent_cases,
              overlapping$cluster_id[k]
            )
          },
          numeric(1)
        )

        dominant <- which(shares >= child_dominance)
        if (length(dominant) > 0) {
          # The largest child of a parent it dominates: one cluster
          # survives, and it is the one the cases are actually in.
          strongest <- dominant[which.max(shares[dominant])]
          winner <- overlapping$cluster_id[strongest]
          if (
            free_to_suppress(winner) &&
              free_to_be_suppressed(parent$cluster_id)
          ) {
            applied <- episodic_suppression_apply(
              con,
              parent$cluster_id,
              winner
            )
            if (applied > 0L) {
              suppressed_ids <- c(suppressed_ids, parent$cluster_id)
              suppressor_ids <- c(suppressor_ids, winner)
              n_suppressed <- n_suppressed + applied
            }
          }
          next
        }

        diffuse <- all(shares < parent_diffuse) &&
          length(shares) >= min_children
        if (diffuse && free_to_suppress(parent$cluster_id)) {
          for (child_id in overlapping$cluster_id) {
            if (!free_to_be_suppressed(child_id)) {
              next
            }
            applied <- episodic_suppression_apply(
              con,
              child_id,
              parent$cluster_id
            )
            if (applied > 0L) {
              suppressed_ids <- c(suppressed_ids, child_id)
              suppressor_ids <- c(suppressor_ids, parent$cluster_id)
              n_suppressed <- n_suppressed + applied
            }
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
#'
#' @param con A [DBI::DBIConnection-class].
#' @param parent_cases The parent's own `case_id`s, already fetched -
#'   the caller holds them because every child in a parent's group is
#'   measured against the same set.
#' @param child_id The child cluster to measure.
#' @return The share of the parent's cases the child also holds, 0 to 1.
#' @keywords internal
#' @noRd
episodic_suppression_share <- function(con, parent_cases, child_id) {
  if (length(parent_cases) == 0) {
    return(0)
  }
  child_cases <- episodic_db_cluster_cases(con, child_id)$case_id
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
