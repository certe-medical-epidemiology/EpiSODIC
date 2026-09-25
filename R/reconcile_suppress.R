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
#' Everything the comparison needs is read up front, in two batched
#' queries: which cases each candidate cluster holds, and which of them
#' somebody has assessed. The comparison itself then runs in memory.
#' Read per parent and per child instead, a backfill that opens
#' thousands of clusters asks the database tens of thousands of
#' questions, one round trip each, inside the run's transaction.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param config The resolved configuration; uses `config$suppression`.
#' @param progress_every Seconds between progress lines while the pass
#'   runs. A pass that finishes sooner writes none; one that does not
#'   says how far it has got, rather than leaving the log silent.
#' @return Invisibly, the number of clusters suppressed.
#' @keywords internal
#' @noRd
episodic_suppress_lattice <- function(con, config, progress_every = 30) {
  sup <- config$suppression
  child_dominance <- as.numeric(sup$child_dominance_threshold %||% 0.7)
  parent_diffuse <- as.numeric(sup$parent_diffuse_threshold %||% 0.5)
  min_children <- as.integer(sup$parent_min_flagged_children %||% 2L)

  clusters <- episodic_db_clusters_for_suppression(con)
  if (nrow(clusters) == 0) {
    episodic_trace("Suppressing lattice: no cluster to weigh")
    return(invisible(0L))
  }

  # Every run starts from no suppression at all, so that last run's
  # verdict cannot outlive the picture that justified it. One statement,
  # not one per suppressed cluster - the set is exactly what the WHERE
  # clause selects, so there is nothing for a loop to add.
  if (any(!is.na(clusters$suppressed_by))) {
    episodic_db_execute(
      con,
      "UPDATE episodic_cluster SET suppressed_by = NULL WHERE suppressed_by IS NOT NULL"
    )
  }
  clusters$suppressed_by <- NA_integer_

  pathogens <- unique(clusters$pathogen)
  episodic_trace(
    "Suppressing lattice: weighing ",
    nrow(clusters),
    " cluster(s) across ",
    length(pathogens),
    " pathogen(s)"
  )
  links <- episodic_db_cluster_case_ids_batch(con, clusters$cluster_id)
  case_ids_of <- split(links$case_id, links$cluster_id)
  cases_of <- function(id) {
    ids <- case_ids_of[[as.character(id)]]
    if (is.null(ids)) integer(0) else ids
  }
  assessed_ids <- episodic_db_assessed_cluster_ids(con, clusters$cluster_id)
  episodic_trace(
    "Suppressing lattice: ",
    nrow(links),
    " case link(s) read for ",
    length(case_ids_of),
    " cluster(s), ",
    length(assessed_ids),
    " assessed cluster(s) exempt"
  )

  # Every cluster that has been suppressed this run, and every cluster
  # that has suppressed one. A cluster in either set is out of the
  # running for the other role - see this function's own documentation
  # for why chains are refused rather than resolved.
  suppressed_ids <- integer(0)
  suppressor_ids <- integer(0)
  free_to_suppress <- function(id) !(id %in% suppressed_ids)
  free_to_be_suppressed <- function(id) !(id %in% suppressor_ids)

  n_suppressed <- 0L
  n_by_child <- 0L
  n_by_parent <- 0L
  last_progress <- Sys.time()
  for (i in seq_along(pathogens)) {
    pathogen <- pathogens[i]
    if (
      as.numeric(difftime(Sys.time(), last_progress, units = "secs")) >=
        progress_every
    ) {
      episodic_trace(
        "Suppressing lattice: pathogen ",
        i,
        " of ",
        length(pathogens),
        ", ",
        n_suppressed,
        " cluster(s) suppressed so far"
      )
      last_progress <- Sys.time()
    }
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

        parent_cases <- cases_of(parent$cluster_id)
        # Nothing to measure a share against. Zero shares would otherwise
        # read as "spread thinly across the children", which is a
        # statement about the data this parent does not have.
        if (length(parent_cases) == 0) {
          next
        }

        shares <- vapply(
          overlapping$cluster_id,
          function(child_id) {
            episodic_suppression_share(parent_cases, cases_of(child_id))
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
              winner,
              assessed_ids
            )
            if (applied > 0L) {
              suppressed_ids <- c(suppressed_ids, parent$cluster_id)
              suppressor_ids <- c(suppressor_ids, winner)
              n_suppressed <- n_suppressed + applied
              n_by_child <- n_by_child + applied
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
              parent$cluster_id,
              assessed_ids
            )
            if (applied > 0L) {
              suppressed_ids <- c(suppressed_ids, child_id)
              suppressor_ids <- c(suppressor_ids, parent$cluster_id)
              n_suppressed <- n_suppressed + applied
              n_by_parent <- n_by_parent + applied
            }
          }
        }
      }
    }
  }

  episodic_trace(
    "Suppressing lattice done: ",
    n_suppressed,
    " cluster(s) suppressed (",
    n_by_child,
    " parent(s) behind a dominant child, ",
    n_by_parent,
    " child(ren) behind a diffuse parent)"
  )
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
#' @param parent_cases The parent's own `case_id`s.
#' @param child_cases The child's own `case_id`s.
#' @return The share of the parent's cases the child also holds, 0 to 1;
#'   `NA` for a parent with no cases, which has no share to measure.
#' @keywords internal
#' @noRd
episodic_suppression_share <- function(parent_cases, child_cases) {
  if (length(parent_cases) == 0) {
    return(NA_real_)
  }
  length(intersect(parent_cases, child_cases)) / length(parent_cases)
}

#' Record one suppression, unless it would suppress an assessed cluster
#'
#' A cluster somebody has already classified stays in the queue whatever
#' the lattice says about it: the board's own record of a decision is not
#' something a later run gets to hide.
#'
#' @param assessed_ids The `cluster_id`s with at least one assessment
#'   event, read once for the whole pass.
#' @keywords internal
#' @noRd
episodic_suppression_apply <- function(con,
                                       cluster_id,
                                       suppressed_by,
                                       assessed_ids) {
  if (cluster_id %in% assessed_ids) {
    return(0L)
  }
  episodic_db_cluster_set_suppressed_by(con, cluster_id, suppressed_by)
  1L
}
