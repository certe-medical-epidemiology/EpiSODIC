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

#' Deduplicate isolates into one case per patient per episode
#'
#' One isolate per patient per episode, using episode lengths from
#' `episode_pathogen_config`, via `AMR::get_episode()` (a hard dependency -
#' `pathogen` is still an arbitrary lab-provided string, never resolved
#' against `AMR::as.mo()` or any taxonomy, since a taxonomy cannot
#' represent everything a lab reports; see `R/ingest_interface.R`).
#' Isolates for the same patient and pathogen are sorted by sample date
#' internally by `get_episode()`, one call per patient/pathogen group so
#' each group's own `episode_days` window applies independently.
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
#' @references
#' Berends MS, Luz CF, Friedrich AW, Sinha BNM, Albers CJ, Glasner C
#' (2022). "AMR: An R Package for Working with Antimicrobial Resistance
#' Data." *Journal of Statistical Software*, 104(3), 1-31.
#' \doi{10.18637/jss.v104.i03} (the source of `get_episode()`, called
#' directly here).
#' @importFrom AMR get_episode
#' @keywords internal
#' @noRd
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

    episode_id <- get_episode(dates, episode_days = episode_days)
    first_in_episode <- !duplicated(episode_id)
    keep[ord[first_in_episode]] <- TRUE
  }

  result <- raw[keep, setdiff(names(raw), c(".episode_days", ".sample_date")), drop = FALSE]
  rownames(result) <- NULL
  result
}
