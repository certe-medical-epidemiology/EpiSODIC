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
#' deterministic and contiguous grouping. L4 (Provincie) defaults to the PC
#' ranges used by the synthetic generator (9xxx Groningen, 8xxx Fryslan,
#' 7xxx Drenthe), which is specific to the bundled demo data; a real
#' deployment outside that region supplies its own PC-to-province lookup
#' via `EPISODIC_PC_PROVINCE_MAP` (see `episodic_pc_to_province()`) -
#' without it, every postcode outside the demo's three provinces resolves
#' to no province at all, and L4 never has anything to detect on.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases A data frame of newly-loaded (or all) cases, with at least
#'   `pathogen`, `sample_date`, `care_line`, `institution_id`, `pc`.
#' @param institutions A data frame from `episodic_db_institutions()`.
#' @return A data frame of `stream_id` values touched by this run, one row
#'   per (stream, level) combination created or refreshed.
#' @keywords internal
#' @noRd
episodic_lattice_enumerate <- function(con, cases, institutions) {
  if (nrow(cases) == 0) {
    return(data.frame(stream_id = integer(0)))
  }

  touched <- list()

  # L1: pathogen x ward (hospitals only, ward not NA)
  l1 <- cases[!is.na(cases$ward) & !is.na(cases$institution_id), ]
  if (nrow(l1) > 0) {
    touched$l1 <- episodic_lattice_upsert_group(
      con,
      l1,
      level = "pathogen_ward",
      group_cols = c("pathogen", "institution_id", "ward"),
      care_line_col = "care_line",
      institution_col = "institution_id",
      ward_col = "ward",
      denominator = "patient_days"
    )
  }

  # L2: pathogen x institution (monitored institutions only)
  monitored_ids <- institutions$institution_id[institutions$is_monitored == 1]
  l2 <- cases[
    !is.na(cases$institution_id) & cases$institution_id %in% monitored_ids,
  ]
  if (nrow(l2) > 0) {
    touched$l2 <- episodic_lattice_upsert_group(
      con,
      l2,
      level = "pathogen_institution",
      group_cols = c("pathogen", "institution_id"),
      care_line_col = "care_line",
      institution_col = "institution_id",
      denominator = "patient_days"
    )
  }

  # L3: pathogen x gebied (coarse PC grouping: first 2 digits)
  l3 <- cases[!is.na(cases$pc), ]
  if (nrow(l3) > 0) {
    l3$.region_code <- episodic_case_region_code(l3, "pathogen_area")
    touched$l3 <- episodic_lattice_upsert_group(
      con,
      l3,
      level = "pathogen_area",
      group_cols = c("pathogen", ".region_code"),
      region_col = ".region_code",
      denominator = "population"
    )
  }

  # L4: pathogen x provincie
  l4 <- cases[!is.na(cases$pc), ]
  if (nrow(l4) > 0) {
    l4$.region_code <- episodic_case_region_code(l4, "pathogen_province")
    # A mapping that places no case at all is not a mapping: it is a
    # postcode column formatted one way being looked up against
    # postcodes formatted another ("9713" against "9713 AB", say). The
    # result is an L4 level that stays permanently empty however many
    # cases arrive, and a Pathogen-screen map that never fills in, so it
    # is said out loud on the run rather than left for somebody to
    # notice months later.
    if (all(is.na(l4$.region_code))) {
      episodic_trace(
        "no province could be resolved for any of the ",
        length(unique(l4$pc)),
        " postcode values in this run - province-level (L4) detection ",
        "has nothing to run on. ",
        if (nzchar(Sys.getenv("EPISODIC_PC_PROVINCE_MAP"))) {
          paste0(
            "EPISODIC_PC_PROVINCE_MAP is set to '",
            Sys.getenv("EPISODIC_PC_PROVINCE_MAP"),
            "'; its `pc` column has to hold the same values as your case ",
            "data's `pc` column, exactly (e.g. ",
            paste0(
              "\"",
              utils::head(unique(l4$pc), 3),
              "\"",
              collapse = ", "
            ),
            "), not a prefix of them."
          )
        } else {
          paste0(
            "EPISODIC_PC_PROVINCE_MAP is unset, so the shipped Northern ",
            "Netherlands demo ranges are in use and match only Dutch 7xxx-",
            "9xxx postcodes. Point it at your own pc/province_code CSV."
          )
        }
      )
    }
    l4 <- l4[!is.na(l4$.region_code), ]
    if (nrow(l4) > 0) {
      touched$l4 <- episodic_lattice_upsert_group(
        con,
        l4,
        level = "pathogen_province",
        group_cols = c("pathogen", ".region_code"),
        region_col = ".region_code",
        denominator = "population"
      )
    }
  }

  # L5: pathogen x regio (whole catchment)
  l5 <- cases
  l5$.region_code <- episodic_region_code_all
  touched$l5 <- episodic_lattice_upsert_group(
    con,
    l5,
    level = "pathogen_region",
    group_cols = c("pathogen", ".region_code"),
    region_col = ".region_code",
    denominator = "population"
  )

  do.call(rbind, touched)
}

#' The region code a case falls in, at one lattice level
#'
#' Both halves of the lattice need this: enumeration, to decide which
#' streams exist, and `episodic_cases_for_stream()`, to fetch a stream's
#' own cases back. Deriving it in one place is the point - when only
#' enumeration knew the rule, every area and province stream was handed
#' the whole region's cases and reported the same count as the region
#' itself.
#'
#' @param cases A data frame of cases, with `pc`.
#' @param level A stream level; anything without a geography returns `NA`.
#' @return A character vector, one region code per case, `NA` where the
#'   case has no `pc` to place it by.
#' @keywords internal
#' @noRd
episodic_case_region_code <- function(cases, level) {
  if (nrow(cases) == 0) {
    return(character(0))
  }
  has_pc <- !is.na(cases$pc)
  switch(level,
    pathogen_area = ifelse(
      has_pc,
      paste0("GEBIED-", substr(cases$pc, 1, 2)),
      NA_character_
    ),
    pathogen_province = ifelse(
      has_pc,
      episodic_pc_to_province(cases$pc),
      NA_character_
    ),
    pathogen_region = rep(episodic_region_code_all, nrow(cases)),
    rep(NA_character_, nrow(cases))
  )
}

#' The whole catchment, as one region code
#' @keywords internal
#' @noRd
episodic_region_code_all <- "NORTHERN_NETHERLANDS"

#' Map a postcode to its L4 province/region code
#'
#' Falls back to the PC ranges used by the bundled Northern Netherlands
#' demo data (9xxx Groningen, 8xxx Fryslan, 7xxx Drenthe) only when no
#' operator-supplied mapping is configured. Left unconfigured in a real
#' deployment outside that specific demo region, every `pc` resolves to
#' `NA` here, so no province ever gets an L4 stream and L4 detection -
#' and everything downstream that reads it, including the Pathogen
#' screen's map - stays permanently empty however many cases accumulate.
#'
#' @param pc A character vector of postcode values, matching the `pc`
#'   column of your case data.
#' @param path Path to a CSV with columns `pc` (matching your case
#'   data's `pc` values exactly - not a prefix) and `province_code`.
#'   Defaults to the `EPISODIC_PC_PROVINCE_MAP` environment variable; if
#'   unset, falls back to the shipped Northern Netherlands demo default.
#'   A path that *is* set but cannot be used is an error, not a fallback -
#'   see `episodic_pc_province_map_resolve()`.
#' @return A character vector the same length as `pc`: the province code,
#'   or `NA` where `pc` has no entry in the mapping.
#' @keywords internal
#' @noRd
episodic_pc_to_province <- function(
  pc,
  path = Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)
) {
  mapping <- episodic_pc_province_map_resolve(path)
  if (!is.null(mapping)) {
    return(unname(mapping[as.character(pc)]))
  }
  digit1 <- substr(pc, 1, 1)
  dplyr::case_when(
    digit1 == "9" ~ "PROV_GRONINGEN",
    digit1 == "8" ~ "PROV_FRYSLAN",
    digit1 == "7" ~ "PROV_DRENTHE",
    TRUE ~ NA_character_
  )
}

#' What is wrong with the configured PC-to-province mapping, if anything
#'
#' Unset and unusable are two different things, and only the first is a
#' fallback. `EPISODIC_PC_PROVINCE_MAP` left unset means "this instance
#' has not configured one", and the shipped Northern Netherlands demo
#' ranges stand in. A variable that *is* set and does not resolve to a
#' readable CSV with the documented columns is a configuration error:
#' quietly substituting the demo ranges there would hand an operator who
#' supplied their own mapping a lattice built on somebody else's
#' provinces, or none at all, with nothing anywhere to say why.
#'
#' Stated as a returned message rather than thrown, so that the one
#' description of the problem serves both sides. `episodic_run_cron()`
#' refuses on it before the run has written anything, the same as any
#' other structural problem with a feed; the dashboard shows it in the
#' geography panel, where the reader is the first to notice the
#' provinces have stopped appearing. What neither side does is carry on
#' as if no mapping had been configured.
#'
#' @param path Path to the CSV, or `NA`/`""` for "not configured".
#' @return `NA_character_` when the mapping is usable (or none is
#'   configured), otherwise a single sentence naming the file and the
#'   problem.
#' @keywords internal
#' @noRd
episodic_pc_province_map_problem <- function(
  path = Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)
) {
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  refuse <- function(...) {
    paste0(
      "EPISODIC_PC_PROVINCE_MAP is set to '",
      path,
      "', but ",
      ...,
      ". Point it at a CSV with `pc` and `province_code` columns, or ",
      "unset it to fall back to the shipped demo mapping."
    )
  }
  if (!file.exists(path)) {
    return(refuse("no file exists there"))
  }
  df <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      colClasses = "character",
      na.strings = c("", "NA")
    ),
    error = function(e) conditionMessage(e)
  )
  if (!is.data.frame(df)) {
    return(refuse("it could not be read as a CSV: ", df))
  }
  missing <- setdiff(c("pc", "province_code"), names(df))
  if (length(missing) > 0) {
    return(refuse(
      "it has no ",
      paste0("`", missing, "`", collapse = " and no "),
      " column (it has ",
      paste0("`", names(df), "`", collapse = ", "),
      ")"
    ))
  }
  if (nrow(df) == 0) {
    return(refuse("it holds no rows"))
  }
  duplicated_pc <- unique(df$pc[duplicated(df$pc)])
  if (length(duplicated_pc) > 0) {
    # Silently keeping the first would put a postcode in one province on
    # one run and another after somebody re-sorts the file.
    return(refuse(
      "its `pc` column repeats ",
      paste0("\"", utils::head(duplicated_pc, 5), "\"", collapse = ", "),
      if (length(duplicated_pc) > 5) ", among others" else "",
      ", so a postcode would map to more than one province"
    ))
  }
  NA_character_
}

#' Read an operator-supplied PC-to-province CSV
#'
#' Deliberately total: it never throws, because it sits under
#' `episodic_case_region_code()`, which both the cron and the dashboard
#' derive stream membership with, and a derivation that can fail
#' mid-render is a dashboard that goes blank instead of explaining
#' itself. Refusing on an unusable mapping is
#' `episodic_pc_province_map_problem()`'s job, and both sides call it -
#' `episodic_run_cron()` before writing anything, the dashboard where it
#' shows the geography panel.
#'
#' @param path Path to the CSV, or `NA`/`""` for "not configured".
#' @return A named character vector (postcode -> province code), or
#'   `NULL` when no mapping is configured or the configured one cannot be
#'   used.
#' @keywords internal
#' @noRd
episodic_pc_province_map_resolve <- function(path) {
  # Two ways to have no mapping, and both mean the demo ranges stand in
  # here: none configured at all, and one configured that cannot be used.
  # Only the second is something to say out loud, and saying it is
  # `episodic_pc_province_map_problem()`'s job, not this one's.
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  if (!is.na(episodic_pc_province_map_problem(path))) {
    return(NULL)
  }
  df <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = c("", "NA")
  )
  stats::setNames(df$province_code, df$pc)
}

#' @keywords internal
#' @noRd
episodic_lattice_upsert_group <- function(
  con,
  cases,
  level,
  group_cols,
  care_line_col = NULL,
  institution_col = NULL,
  region_col = NULL,
  ward_col = NULL,
  denominator = "none"
) {
  key_df <- cases[, group_cols, drop = FALSE]
  # Separated, not concatenated. Glued together, institution 1 on ward "2A"
  # and institution 12 on ward "A" are the same group, and the two wards
  # become one stream with one of them silently gone - the kind of thing a
  # surveillance system must never do quietly. A control character cannot
  # occur in a pathogen name, a ward or a region code, so it separates
  # without being mistakable for content.
  key_str <- do.call(paste, c(key_df, sep = "\r"))
  groups <- split(seq_len(nrow(cases)), key_str)

  # One row per group, assembled in R first. The database is then touched
  # three times for the whole level instead of three times per stream (a
  # SELECT on stream_key, an INSERT or UPDATE, and a LAST_INSERT_ID()),
  # which is what made enumerating a few hundred streams take seventeen
  # seconds against a networked database. Group order is preserved, so
  # newly created streams still get their ids in the order they always did.
  heads <- vapply(groups, function(g) g[1], integer(1))
  reps <- cases[heads, , drop = FALSE]
  pick <- function(col) if (is.null(col)) rep(NA, nrow(reps)) else reps[[col]]

  care_line <- pick(care_line_col)
  institution_id <- pick(institution_col)
  region_code <- pick(region_col)
  ward <- pick(ward_col)
  observed_date <- as.character(vapply(
    groups,
    function(g) max(cases$sample_date[g]),
    character(1)
  ))
  stream_key <- vapply(
    seq_len(nrow(reps)),
    function(i) {
      episodic_stream_key(
        level = level,
        pathogen = reps$pathogen[i],
        care_line = care_line[i],
        region_code = region_code[i],
        institution_id = institution_id[i],
        ward = ward[i]
      )
    },
    character(1)
  )

  on_file <- DBI::dbGetQuery(
    con,
    "SELECT stream_id, stream_key, first_seen, last_seen FROM episodic_stream"
  )
  known <- match(stream_key, on_file$stream_key)

  # Same widening as the single-row upsert did: an existing stream keeps
  # the earliest first_seen and the latest last_seen it has ever had.
  first_seen <- ifelse(
    is.na(known),
    observed_date,
    pmin(on_file$first_seen[known], observed_date)
  )
  last_seen <- ifelse(
    is.na(known),
    observed_date,
    pmax(on_file$last_seen[known], observed_date)
  )

  new <- is.na(known)
  if (any(new)) {
    episodic_db_write_many(
      con,
      table = "episodic_stream",
      cols = c(
        "stream_key",
        "level",
        "pathogen",
        "care_line",
        "region_code",
        "institution_id",
        "ward",
        "denominator",
        "severity_weight",
        "is_active",
        "first_seen",
        "last_seen",
        "created_at"
      ),
      values = list(
        stream_key = stream_key[new],
        level = rep(level, sum(new)),
        pathogen = reps$pathogen[new],
        care_line = care_line[new],
        region_code = region_code[new],
        institution_id = institution_id[new],
        ward = ward[new],
        denominator = rep(denominator, sum(new)),
        severity_weight = rep(1.00, sum(new)),
        is_active = rep(1L, sum(new)),
        first_seen = first_seen[new],
        last_seen = last_seen[new],
        created_at = rep(episodic_now(), sum(new))
      )
    )
  }
  # Existing streams get a plain UPDATE, and only the ones whose window
  # actually moved. An upsert cannot serve here - episodic_stream has NOT
  # NULL columns (level, pathogen, created_at ...) that an update-only
  # caller has no business restating - and it is not needed: first_seen
  # and last_seen change only when a stream gains a case outside the span
  # it already knew about, so on a routine re-run this loop writes almost
  # nothing.
  moved <- !new &
    (first_seen != on_file$first_seen[known] |
      last_seen != on_file$last_seen[known])
  for (i in which(moved)) {
    params <- list(first_seen[i], last_seen[i], stream_key[i])
    DBI::dbExecute(
      con,
      "UPDATE episodic_stream SET first_seen = ?, last_seen = ? WHERE stream_key = ?",
      params = params
    )
  }

  on_file <- DBI::dbGetQuery(
    con,
    "SELECT stream_id, stream_key FROM episodic_stream"
  )
  ids <- as.integer(on_file$stream_id[match(stream_key, on_file$stream_key)])
  data.frame(stream_id = ids)
}
