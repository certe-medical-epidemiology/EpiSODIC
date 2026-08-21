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

#' Enumerate lattice streams from case data
#'
#' Streams are enumerated automatically from the data, per run, so a
#' newly appearing pathogen creates its streams without configuration.
#' Care line is a filter, not a level, so it is folded into the L1/L2
#' streams (where it is available) but not into L3-L5.
#'
#' L3 (Gebied) is derived from the PC's first two digits, a coarse but
#' deterministic and contiguous grouping. L4 (Provincie) is derived from the PC
#' ranges used by the synthetic generator (9xxx Groningen, 8xxx Fryslan,
#' 7xxx Drenthe) and is therefore specific to the bundled demo data; a
#' real deployment needs the actual PC-to-province mapping for its own
#' region.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame of newly-ingested (or all) cases, with at least
#'   `pathogen`, `sample_date`, `care_line`, `institution_id`, `pc`.
#' @param institutions A data frame from `episode_db_institutions()`.
#' @return A data frame of `stream_id` values touched by this run, one row
#'   per (stream, level) combination created or refreshed.
#' @keywords internal
#' @noRd
episode_lattice_enumerate <- function(con, cases, institutions) {
  if (nrow(cases) == 0) return(data.frame(stream_id = integer(0)))

  touched <- list()

  # L1: pathogen x ward (hospitals only, ward not NA)
  l1 <- cases[!is.na(cases$ward) & !is.na(cases$institution_id), ]
  if (nrow(l1) > 0) {
    touched$l1 <- episode_lattice_upsert_group(
      con, l1, level = "pathogen_ward",
      group_cols = c("pathogen", "institution_id", "ward"),
      care_line_col = "care_line", institution_col = "institution_id", ward_col = "ward",
      denominator = "patient_days"
    )
  }

  # L2: pathogen x institution (monitored institutions only)
  monitored_ids <- institutions$institution_id[institutions$is_monitored == 1]
  l2 <- cases[!is.na(cases$institution_id) & cases$institution_id %in% monitored_ids, ]
  if (nrow(l2) > 0) {
    touched$l2 <- episode_lattice_upsert_group(
      con, l2, level = "pathogen_institution",
      group_cols = c("pathogen", "institution_id"),
      care_line_col = "care_line", institution_col = "institution_id",
      denominator = "patient_days"
    )
  }

  # L3: pathogen x gebied (coarse PC grouping: first 2 digits)
  l3 <- cases[!is.na(cases$pc), ]
  if (nrow(l3) > 0) {
    l3$.region_code <- paste0("GEBIED-", substr(l3$pc, 1, 2))
    touched$l3 <- episode_lattice_upsert_group(
      con, l3, level = "pathogen_area",
      group_cols = c("pathogen", ".region_code"),
      region_col = ".region_code", denominator = "population"
    )
  }

  # L4: pathogen x provincie
  l4 <- cases[!is.na(cases$pc), ]
  if (nrow(l4) > 0) {
    l4$.region_code <- episode_pc_to_province(l4$pc)
    l4 <- l4[!is.na(l4$.region_code), ]
    if (nrow(l4) > 0) {
      touched$l4 <- episode_lattice_upsert_group(
        con, l4, level = "pathogen_province",
        group_cols = c("pathogen", ".region_code"),
        region_col = ".region_code", denominator = "population"
      )
    }
  }

  # L5: pathogen x regio (whole catchment)
  l5 <- cases
  l5$.region_code <- "NOORD_NEDERLAND"
  touched$l5 <- episode_lattice_upsert_group(
    con, l5, level = "pathogen_region",
    group_cols = c("pathogen", ".region_code"),
    region_col = ".region_code", denominator = "population"
  )

  do.call(rbind, touched)
}

#' @keywords internal
#' @noRd
episode_pc_to_province <- function(pc) {
  digit1 <- substr(pc, 1, 1)
  dplyr::case_when(
    digit1 == "9" ~ "PROV_GRONINGEN",
    digit1 == "8" ~ "PROV_FRYSLAN",
    digit1 == "7" ~ "PROV_DRENTHE",
    TRUE ~ NA_character_
  )
}

#' @keywords internal
#' @noRd
episode_lattice_upsert_group <- function(con, cases, level, group_cols, care_line_col = NULL,
                                          institution_col = NULL, region_col = NULL,
                                          ward_col = NULL, denominator = "none") {
  key_df <- cases[, group_cols, drop = FALSE]
  key_str <- do.call(paste, c(key_df, sep = ""))
  groups <- split(seq_len(nrow(cases)), key_str)

  ids <- integer(length(groups))
  i <- 1L
  for (g in groups) {
    row <- cases[g[1], ]
    care_line <- if (!is.null(care_line_col)) row[[care_line_col]] else NA
    institution_id <- if (!is.null(institution_col)) row[[institution_col]] else NA
    region_code <- if (!is.null(region_col)) row[[region_col]] else NA
    ward <- if (!is.null(ward_col)) row[[ward_col]] else NA

    stream_key <- episode_stream_key(
      level = level, pathogen = row$pathogen, care_line = care_line,
      region_code = region_code, institution_id = institution_id, ward = ward
    )

    max_date <- max(cases$sample_date[g])
    ids[i] <- episode_db_stream_upsert(
      con, stream_key = stream_key, level = level, pathogen = row$pathogen,
      care_line = care_line, region_code = region_code, institution_id = institution_id,
      ward = ward, denominator = denominator, observed_date = as.character(max_date)
    )
    i <- i + 1L
  }
  data.frame(stream_id = ids)
}
