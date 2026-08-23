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

#' Deduplicate positives into one case per patient per episode
#'
#' One positive per patient per episode, using episode lengths from
#' `episodic_pathogen_config`, via `AMR::get_episode()` (a hard dependency -
#' `pathogen` is still an arbitrary lab-provided string, never resolved
#' against `AMR::as.mo()` or any taxonomy, since a taxonomy cannot
#' represent everything a lab reports; see `R/cases.R`).
#' Positives for the same patient and pathogen are sorted by sample date
#' internally by `get_episode()`, one call per patient/pathogen group so
#' each group's own `episode_days` window applies independently.
#'
#' @param cases A data frame satisfying the case data contract
#'   (`R/cases.R`), already validated.
#' @param pathogen_config A data frame from `inst/config/pathogen_config.csv`
#'   (or `episodic_db_pathogen_config()`), providing `episode_days` per
#'   `pathogen`. A pathogen not present in this table uses the schema
#'   default of 30 days.
#' @param existing A named character vector of already-stored episode
#'   anchor dates (`YYYY-MM-DD`), one per patient/pathogen combination
#'   already present in the database, from
#'   `episodic_db_last_case_dates()`. Names are `paste0(patient_key,
#'   pathogen)`, matching this function's own internal grouping key. Lets
#'   an operator send only a recent window of positives on each run - an
#'   incoming positive that falls within `episode_days` of that
#'   patient/pathogen's last stored episode is recognised as a
#'   continuation of it (and dropped, since the stored case already
#'   represents that episode) rather than being inserted as a spurious
#'   second case. Defaults to `NULL` (nothing known yet), under which
#'   this behaves exactly as before - every incoming positive is compared
#'   only against the rest of the current batch.
#' @return `cases`, with one row per patient per new episode (the
#'   earliest `sample_date` in each episode is kept, since sample date is
#'   the architecture's anchor), excluding any episode `existing` already
#'   covers.
#' @references
#' Berends MS, Luz CF, Friedrich AW, Sinha BNM, Albers CJ, Glasner C
#' (2022). "AMR: An R Package for Working with Antimicrobial Resistance
#' Data." *Journal of Statistical Software*, 104(3), 1-31.
#' \doi{10.18637/jss.v104.i03} (the source of `get_episode()`, called
#' directly here).
#' @importFrom AMR get_episode
#' @keywords internal
#' @noRd
episodic_cases_deduplicate <- function(cases, pathogen_config, existing = NULL) {
  if (nrow(cases) == 0) {
    return(cases)
  }

  cases$.episode_days <- pathogen_config$episode_days[
    match(cases$pathogen, pathogen_config$pathogen)
  ]
  cases$.episode_days[is.na(cases$.episode_days)] <- 30

  cases$.sample_date <- as.Date(cases$sample_date)
  group_key <- paste(cases$patient_key, cases$pathogen, sep = "")
  groups <- split(seq_len(nrow(cases)), group_key)

  keep <- logical(nrow(cases))
  for (idx in groups) {
    ord <- idx[order(cases$.sample_date[idx])]
    dates <- cases$.sample_date[ord]
    episode_days <- cases$.episode_days[ord][1]

    anchor <- if (is.null(existing)) {
      NA_character_
    } else {
      unname(existing[group_key[ord[1]]])
    }
    if (!is.na(anchor)) {
      # Prepend the already-stored episode's anchor date, so an incoming
      # positive close enough to it is recognised as the same episode
      # (already represented in the database) instead of a new one.
      episode_id_all <- get_episode(
        c(as.Date(anchor), dates),
        episode_days = episode_days
      )
      anchor_episode <- episode_id_all[1]
      episode_id <- episode_id_all[-1]
      first_in_episode <- !duplicated(episode_id) & episode_id != anchor_episode
    } else {
      episode_id <- get_episode(dates, episode_days = episode_days)
      first_in_episode <- !duplicated(episode_id)
    }
    keep[ord[first_in_episode]] <- TRUE
  }

  result <- cases[
    keep,
    setdiff(names(cases), c(".episode_days", ".sample_date")),
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}
