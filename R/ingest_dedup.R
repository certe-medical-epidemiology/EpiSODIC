#' Deduplicate isolates into one case per patient per episode
#'
#' One isolate per patient per episode, using episode lengths from
#' `episode_pathogen_config`. `AMR::as.mo()`
#' only resolves non-viral taxonomy, and this package now treats `pathogen`
#' as an arbitrary lab-provided string that can be anything (a virus, a
#' resistance phenotype like "ETEC", a genus), so `AMR` is not a dependency
#' at all (see `QUESTIONS.md` item 22). Episode grouping uses a direct
#' implementation of `AMR::get_episode()`'s own documented algorithm for the
#' default (non-combination) case: isolates for the same patient and
#' pathogen are sorted by sample date, and a new episode starts whenever the
#' gap since the previous isolate in the group exceeds `episode_days`.
#'
#' @param raw A data frame satisfying the ingestion interface
#'   (`R/ingest_interface.R`), already validated.
#' @param pathogen_config A data frame from `inst/config/pathogen_config.csv`
#'   (or `episode_db_pathogen_config()`), providing `episode_days` per
#'   `pathogen`. A pathogen not present in this table uses the schema
#'   default of 30 days.
#' @return `raw`, with one row per patient per episode (the earliest
#'   `sample_date` in each episode is kept, since sample date is the
#'   architecture's anchor).
#' @export
episode_dedup <- function(raw, pathogen_config) {
  if (nrow(raw) == 0) return(raw)

  raw$.episode_days <- pathogen_config$episode_days[
    match(raw$pathogen, pathogen_config$pathogen)
  ]
  raw$.episode_days[is.na(raw$.episode_days)] <- 30

  raw$.sample_date <- as.Date(raw$sample_date)
  groups <- split(seq_len(nrow(raw)), paste(raw$patient_key, raw$pathogen, sep = ""))

  keep <- logical(nrow(raw))
  for (idx in groups) {
    ord <- idx[order(raw$.sample_date[idx])]
    dates <- raw$.sample_date[ord]
    episode_days <- raw$.episode_days[ord][1]

    episode_id <- episode_assign_episode_ids(dates, episode_days)
    first_in_episode <- !duplicated(episode_id)
    keep[ord[first_in_episode]] <- TRUE
  }

  result <- raw[keep, setdiff(names(raw), c(".episode_days", ".sample_date")), drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Assign episode group ids to a sorted vector of dates
#'
#' A direct implementation of `AMR::get_episode()`'s documented algorithm
#' for the default case, so results match what `AMR::get_episode()` would
#' produce without requiring `AMR` to be installed.
#'
#' @param dates_sorted A `Date` vector, already sorted ascending.
#' @param episode_days Deduplication window in days.
#' @return An integer vector of episode group ids, same length as
#'   `dates_sorted`.
#' @keywords internal
#' @noRd
episode_assign_episode_ids <- function(dates_sorted, episode_days) {
  if (length(dates_sorted) == 0) return(integer(0))
  gaps <- c(0, diff(as.integer(dates_sorted)))
  cumsum(gaps > episode_days | seq_along(dates_sorted) == 1)
}
