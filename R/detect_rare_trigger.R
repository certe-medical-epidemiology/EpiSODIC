#' `rare_trigger` detector
#'
#' The rare-but-serious path (ARCHITECTURE.md section 8, item 2): a single
#' case of a curated pathogen is notable on its own, not on aberration
#' statistics, and running it through Farrington "produces only silence"
#' (the architecture's own words) since a baseline model has nothing to
#' compare one rare case against. This detector needs no baseline and no
#' eligibility gate: any occurrence at or above `min_cases` fires.
#'
#' Matching is against the raw `pathogen` string, case-insensitively, since
#' operators supply free text (`QUESTIONS.md` item 22) and a curated list
#' should not silently miss a hit over a capitalisation difference.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame of cases to scan, with `pathogen`,
#'   `institution_id`, `sample_date`.
#' @param institutions A data frame from `episode_db_institutions()` (kept
#'   for signature parity with `episode_detect_same_place()`; not currently
#'   used, since `rare_trigger` fires regardless of institution type).
#' @param config The resolved configuration; uses `config$rare_trigger`.
#' @return A data frame of detection records plus a `stream_id` column, one
#'   row per matching case (or per institution-day group when several
#'   matching cases share an institution and date).
#' @export
episode_detect_rare_trigger <- function(con, cases, institutions, config) {
  empty <- episode_detection_record(integer(0), character(0), character(0), character(0), integer(0))

  rt <- config$rare_trigger
  if (is.null(rt) || length(rt$pathogens) == 0) return(empty)

  matches <- tolower(cases$pathogen) %in% tolower(rt$pathogens)
  hits <- cases[matches, ]
  if (nrow(hits) == 0) return(empty)

  min_cases <- if (is.null(rt$min_cases)) 1L else rt$min_cases

  key_str <- paste(hits$pathogen, hits$institution_id, hits$sample_date, sep = "\r")
  groups <- split(seq_len(nrow(hits)), key_str)

  records <- list()
  for (g in groups) {
    grp <- hits[g, ]
    if (nrow(grp) < min_cases) next

    pathogen <- grp$pathogen[1]
    institution_id <- grp$institution_id[1]

    # rare_trigger streams are institution-level (or regional, when the case
    # has no institution); it reuses episode_db_stream_upsert() exactly as
    # same_place does, so it reconciles into the same tables as every other
    # detector.
    stream_key <- episode_stream_key(
      level = "pathogen_institution", pathogen = pathogen, care_line = NA,
      region_code = NA, institution_id = institution_id
    )
    stream_id <- episode_db_stream_upsert(
      con, stream_key = stream_key, level = "pathogen_institution", pathogen = pathogen,
      care_line = NA, region_code = NA, institution_id = institution_id,
      denominator = "none", observed_date = grp$sample_date[1]
    )

    records[[length(records) + 1]] <- episode_detection_record(
      stream_id = stream_id, detector = "rare_trigger",
      first_day = grp$sample_date[1], last_day = grp$sample_date[1], n_cases = nrow(grp),
      params = list(pathogen = pathogen, min_cases = min_cases)
    )
  }
  if (length(records) == 0) return(empty)
  do.call(rbind, records)
}
