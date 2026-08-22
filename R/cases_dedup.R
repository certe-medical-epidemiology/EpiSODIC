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

#' Deduplicate isolates into one case per patient per episode
#'
#' One isolate per patient per episode, using episode lengths from
#' `episodic_pathogen_config`, via `AMR::get_episode()` (a hard dependency -
#' `pathogen` is still an arbitrary lab-provided string, never resolved
#' against `AMR::as.mo()` or any taxonomy, since a taxonomy cannot
#' represent everything a lab reports; see `R/cases.R`).
#' Isolates for the same patient and pathogen are sorted by sample date
#' internally by `get_episode()`, one call per patient/pathogen group so
#' each group's own `episode_days` window applies independently.
#'
#' @param cases A data frame satisfying the case data contract
#'   (`R/cases.R`), already validated.
#' @param pathogen_config A data frame from `inst/config/pathogen_config.csv`
#'   (or `episodic_db_pathogen_config()`), providing `episode_days` per
#'   `pathogen`. A pathogen not present in this table uses the schema
#'   default of 30 days.
#' @return `cases`, with one row per patient per episode (the earliest
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
episodic_cases_deduplicate <- function(cases, pathogen_config) {
  if (nrow(cases) == 0) return(cases)

  cases$.episode_days <- pathogen_config$episode_days[
    match(cases$pathogen, pathogen_config$pathogen)
  ]
  cases$.episode_days[is.na(cases$.episode_days)] <- 30

  cases$.sample_date <- as.Date(cases$sample_date)
  groups <- split(seq_len(nrow(cases)), paste(cases$patient_key, cases$pathogen, sep = ""))

  keep <- logical(nrow(cases))
  for (idx in groups) {
    ord <- idx[order(cases$.sample_date[idx])]
    dates <- cases$.sample_date[ord]
    episode_days <- cases$.episode_days[ord][1]

    episode_id <- get_episode(dates, episode_days = episode_days)
    first_in_episode <- !duplicated(episode_id)
    keep[ord[first_in_episode]] <- TRUE
  }

  result <- cases[keep, setdiff(names(cases), c(".episode_days", ".sample_date")), drop = FALSE]
  rownames(result) <- NULL
  result
}
