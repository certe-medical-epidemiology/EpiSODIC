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

#' Add One or More Manual (External) Clusters
#'
#' Adds clusters that were never detected by EpiSODIC's own pipeline -
#' output from another algorithm or system, fed in as already-parsed R
#' values (an operator's own code is responsible for reading whatever
#' JSON/CSV that other system produces). A manual cluster is never
#' connected to this instance's own case data: it gets a real
#' `episodic_stream` identity (so it appears in the dashboard's normal
#' filtering, sorting and geography panels, exactly like a detected
#' cluster), but its case-level detail, if you supply any, is stored
#' separately in `episodic_cluster_manual_case` and never touches
#' `episodic_case`. It is therefore also never picked up by
#' [episodic_run_cron()]'s reconciliation or automatic closure - a manual
#' cluster only closes when an epidemiologist records that verdict.
#'
#' Every argument is vectorised: pass a single value to add one cluster,
#' or vectors/lists of length `n` to add `n` clusters in one call, each
#' getting its own new cluster id. Arguments of length 1 are recycled
#' against the longest argument you supply; per-cluster case-level detail
#' (`case_dates`/`pc`/`sex`/`age`) is never recycled - if you supply it,
#' it must be a `list()` of exactly the target length, one element (a
#' vector) per cluster.
#'
#' The whole call is one transaction: either every requested cluster is
#' created, or (on any validation or database error) none are.
#'
#' @param db_path Path to an existing SQLite database, or a `mysql://`
#'   DSN (see [episodic_db_dsn_mariadb()]). Defaults to the
#'   `EPISODIC_DB` environment variable.
#' @param user_id The `episodic_app_user` id this batch is attributed to
#'   - required, since provenance ("who added this, and when") is exactly
#'   what a hand-added cluster most needs. Recorded as the author of the
#'   seed note when `note` is supplied.
#' @param pathogen Raw pathogen string, exactly as it would appear in
#'   `episodic_case$pathogen` - used verbatim, not matched against any
#'   fixed list.
#' @param level One of the five lattice levels: `"pathogen_ward"`,
#'   `"pathogen_institution"`, `"pathogen_area"`, `"pathogen_province"`,
#'   `"pathogen_region"` (see `vignette("architecture")`).
#' @param first_day,last_day The cluster's episode window (`Date` or
#'   `"YYYY-MM-DD"` text).
#' @param n_cases The case count. Optional if `case_dates` (or any other
#'   per-cluster case-level detail) is supplied for that cluster, in
#'   which case it defaults to that detail's own length; required
#'   otherwise, since there is then nothing else to derive it from.
#' @param care_line,region_code,institution_id,ward The remaining
#'   `episodic_stream` identity fields, exactly as
#'   `episodic_stream_key()` expects them. `institution_id` must already
#'   exist in `episodic_institution` (e.g. resolved earlier via
#'   `episodic_institutions_resolve()`) - this function does not create
#'   institutions.
#' @param expected,excess,ratio Optional pass-through metrics, for
#'   external algorithms that do produce a baseline comparison. `NA`
#'   (the default) when the source has no such concept - exactly as for
#'   a detected cluster whose detector cannot produce one.
#' @param detector_agreement How many independent sources agree on this
#'   cluster. Defaults to `1L` (a single external source).
#' @param priority_score Optional override. When `NA` (the default), it
#'   is computed with the same `episodic_priority_score()` used for
#'   detected clusters, so manual and detected clusters sort comparably -
#'   from `expected`/`excess`/`ratio`, `detector_agreement`, and, when
#'   per-cluster case-level detail is supplied, growth slope and spatial
#'   concentration computed from it exactly as the cron pipeline would.
#' @param case_dates,pc,sex,age Optional per-cluster case-level detail,
#'   each a `list()` of length `n` (one vector per cluster, all of equal
#'   length within a cluster) - `case_dates` a vector of sample dates,
#'   `pc` postcodes, `sex` `"M"`/`"F"`/`"U"`/`NA`, `age` in years. Feeds
#'   the dossier's epi curve, demography and geography panels for this
#'   cluster. Leave `NULL` (the default) for a source that only supplies
#'   an aggregate count - the cluster is still created, its panels just
#'   have nothing to draw.
#' @param note Optional markdown text to seed the cluster's note with
#'   (see the dossier's notes panel), attributed to `user_id`.
#' @return An integer vector of the newly created `cluster_id` values,
#'   one per cluster, in the same order as the arguments.
#' @examples
#' \dontrun{
#' episodic_add_manual_cluster(
#'   user_id = 1L,
#'   pathogen = "Measles virus",
#'   level = "pathogen_area",
#'   first_day = "2025-01-10",
#'   last_day = "2025-01-20",
#'   region_code = "GR",
#'   case_dates = list(as.Date(c("2025-01-10", "2025-01-14", "2025-01-20"))),
#'   pc = list(c("9711AA", "9711AB", "9712CD")),
#'   note = "Reported by municipal health service, outside our own lab data."
#' )
#' }
#' @export
episodic_add_manual_cluster <- function(
    db_path = Sys.getenv("EPISODIC_DB", unset = NA),
    user_id,
    pathogen,
    level,
    first_day,
    last_day,
    n_cases = NA,
    care_line = NA,
    region_code = NA,
    institution_id = NA,
    ward = NA,
    expected = NA,
    excess = NA,
    ratio = NA,
    detector_agreement = 1L,
    priority_score = NA,
    case_dates = NULL,
    pc = NULL,
    sex = NULL,
    age = NULL,
    note = NA) {
  stopifnot(
    "user_id is required" = !missing(user_id) && length(user_id) == 1 && !is.na(user_id)
  )

  spec <- episodic_manual_cluster_spec(
    pathogen = pathogen,
    level = level,
    first_day = first_day,
    last_day = last_day,
    n_cases = n_cases,
    care_line = care_line,
    region_code = region_code,
    institution_id = institution_id,
    ward = ward,
    expected = expected,
    excess = excess,
    ratio = ratio,
    detector_agreement = detector_agreement,
    priority_score = priority_score,
    case_dates = case_dates,
    pc = pc,
    sex = sex,
    age = age,
    note = note
  )

  con <- episodic_db_open(db_path)
  on.exit(DBI::dbDisconnect(con))

  weights <- episodic_config_resolve(con = con)$priority_score$weights

  DBI::dbBegin(con)
  cluster_ids <- tryCatch(
    {
      ids <- integer(spec$n)
      for (i in seq_len(spec$n)) {
        ids[i] <- episodic_add_one_manual_cluster(con, spec, i, user_id, weights)
      }
      ids
    },
    error = function(e) e
  )

  if (inherits(cluster_ids, "error")) {
    DBI::dbRollback(con)
    stop(conditionMessage(cluster_ids), call. = FALSE)
  }
  DBI::dbCommit(con)
  cluster_ids
}

#' Write cluster `i` of a validated manual-cluster batch
#' @keywords internal
#' @noRd
episodic_add_one_manual_cluster <- function(con, spec, i, user_id, weights) {
  stream_key <- episodic_stream_key(
    level = spec$level[i],
    pathogen = spec$pathogen[i],
    care_line = spec$care_line[i],
    region_code = spec$region_code[i],
    institution_id = spec$institution_id[i],
    ward = spec$ward[i]
  )
  pathogen_config <- episodic_db_pathogen_config_get(con, spec$pathogen[i])
  severity_weight <- if (!is.null(pathogen_config)) {
    pathogen_config$severity_weight
  } else {
    1
  }
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = stream_key,
    level = spec$level[i],
    pathogen = spec$pathogen[i],
    care_line = spec$care_line[i],
    region_code = spec$region_code[i],
    institution_id = spec$institution_id[i],
    ward = spec$ward[i],
    severity_weight = severity_weight,
    observed_date = spec$last_day[i]
  )

  cases_i <- spec$cases[[i]]
  score <- if (!is.na(spec$priority_score[i])) {
    spec$priority_score[i]
  } else {
    growth_slope <- if (!is.null(cases_i)) {
      episodic_growth_slope(cases_i, spec$last_day[i])
    } else {
      0
    }
    spatial_concentration <- if (!is.null(cases_i)) {
      episodic_spatial_concentration(cases_i)
    } else {
      0
    }
    episodic_priority_score(
      excess = spec$excess[i],
      ratio = spec$ratio[i],
      severity_weight = severity_weight,
      growth_slope = growth_slope,
      detector_agreement = spec$detector_agreement[i],
      n_detectors = 1,
      density_ratio = NA,
      spatial_concentration = spatial_concentration,
      weights = weights
    )
  }

  cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = stream_id,
    first_day = episodic_sql_date(spec$first_day[i]),
    last_day = episodic_sql_date(spec$last_day[i]),
    n_cases = spec$n_cases[i],
    expected = spec$expected[i],
    excess = spec$excess[i],
    ratio = spec$ratio[i],
    priority_score = score,
    detector_agreement = spec$detector_agreement[i],
    run_id = NA,
    origin = "manual"
  )

  if (!is.null(cases_i)) {
    episodic_db_cluster_manual_case_insert_many(con, cluster_id, cases_i)
  }
  if (!is.na(spec$note[i]) && nzchar(spec$note[i])) {
    episodic_db_cluster_note_insert(con, cluster_id, user_id, spec$note[i])
  }

  cluster_id
}

#' @keywords internal
#' @noRd
episodic_db_cluster_manual_case_insert_many <- function(con, cluster_id, cases) {
  n <- nrow(cases)
  if (n == 0) {
    return(invisible(NULL))
  }
  episodic_db_write_many(
    con,
    table = "episodic_cluster_manual_case",
    cols = c("cluster_id", "sample_date", "pc", "sex", "age"),
    values = list(
      cluster_id = rep(cluster_id, n),
      sample_date = episodic_sql_date(cases$sample_date),
      pc = cases$pc,
      sex = cases$sex,
      age = cases$age
    )
  )
  invisible(NULL)
}

#' Validate and recycle `episodic_add_manual_cluster()`'s arguments
#'
#' Fails loudly, before any database connection is opened, naming which
#' argument or which cluster (1-indexed, matching the caller's own
#' vectors) is at fault - never a partial, silently-wrong batch.
#' @return A list: every scalar-per-cluster argument recycled to a common
#'   length `n`, plus `cases` - a length-`n` list, each element either
#'   `NULL` or a data frame with `sample_date`/`pc`/`sex`/`age`.
#' @keywords internal
#' @noRd
episodic_manual_cluster_spec <- function(
    pathogen,
    level,
    first_day,
    last_day,
    n_cases,
    care_line,
    region_code,
    institution_id,
    ward,
    expected,
    excess,
    ratio,
    detector_agreement,
    priority_score,
    case_dates,
    pc,
    sex,
    age,
    note) {
  detail_list_lengths <- vapply(
    list(case_dates, pc, sex, age),
    function(x) if (is.null(x)) 1L else length(x),
    integer(1)
  )
  n <- max(
    length(pathogen), length(level), length(first_day), length(last_day),
    length(n_cases), length(care_line), length(region_code),
    length(institution_id), length(ward), length(expected), length(excess),
    length(ratio), length(detector_agreement), length(priority_score),
    length(note), detail_list_lengths
  )

  recycle <- function(x, name) {
    if (length(x) == n) {
      return(x)
    }
    if (length(x) == 1) {
      return(rep(x, n))
    }
    stop(
      "episodic_add_manual_cluster(): '", name, "' has length ", length(x),
      ", but the arguments imply ", n, " cluster(s) - every argument must ",
      "have length 1 (recycled) or length ", n, ".",
      call. = FALSE
    )
  }
  recycle_detail <- function(x, name) {
    if (is.null(x)) {
      return(vector("list", n))
    }
    if (length(x) != n) {
      stop(
        "episodic_add_manual_cluster(): '", name, "' has length ", length(x),
        ", but the arguments imply ", n, " cluster(s) - per-cluster case-",
        "level detail must have exactly length ", n, " (no recycling).",
        call. = FALSE
      )
    }
    x
  }

  pathogen <- as.character(recycle(pathogen, "pathogen"))
  level <- as.character(recycle(level, "level"))
  allowed_levels <- c(
    "pathogen_ward", "pathogen_institution", "pathogen_area",
    "pathogen_province", "pathogen_region"
  )
  if (any(!level %in% allowed_levels)) {
    stop(
      "episodic_add_manual_cluster(): 'level' must be one of ",
      paste(sprintf('"%s"', allowed_levels), collapse = ", "),
      " - got ", paste(sprintf('"%s"', unique(level[!level %in% allowed_levels])), collapse = ", "),
      call. = FALSE
    )
  }
  first_day <- as.Date(recycle(first_day, "first_day"))
  last_day <- as.Date(recycle(last_day, "last_day"))
  if (any(first_day > last_day)) {
    stop(
      "episodic_add_manual_cluster(): 'first_day' must not be after ",
      "'last_day' (cluster ", which(first_day > last_day)[1], ").",
      call. = FALSE
    )
  }
  n_cases <- suppressWarnings(as.integer(recycle(n_cases, "n_cases")))
  care_line <- as.character(recycle(care_line, "care_line"))
  region_code <- as.character(recycle(region_code, "region_code"))
  institution_id <- suppressWarnings(as.integer(recycle(institution_id, "institution_id")))
  ward <- as.character(recycle(ward, "ward"))
  expected <- as.numeric(recycle(expected, "expected"))
  excess <- as.numeric(recycle(excess, "excess"))
  ratio <- as.numeric(recycle(ratio, "ratio"))
  detector_agreement <- suppressWarnings(as.integer(recycle(detector_agreement, "detector_agreement")))
  priority_score <- as.numeric(recycle(priority_score, "priority_score"))
  note <- as.character(recycle(note, "note"))

  case_dates <- recycle_detail(case_dates, "case_dates")
  pc <- recycle_detail(pc, "pc")
  sex <- recycle_detail(sex, "sex")
  age <- recycle_detail(age, "age")

  cases <- vector("list", n)
  for (i in seq_len(n)) {
    has_detail <- !is.null(case_dates[[i]]) || !is.null(pc[[i]]) ||
      !is.null(sex[[i]]) || !is.null(age[[i]])
    if (!has_detail) {
      if (is.na(n_cases[i])) {
        stop(
          "episodic_add_manual_cluster(): cluster ", i, " has neither ",
          "'n_cases' nor any per-cluster case-level detail - at least one ",
          "is required.",
          call. = FALSE
        )
      }
      next
    }
    detail_lengths <- vapply(
      list(case_dates[[i]], pc[[i]], sex[[i]], age[[i]]),
      function(x) if (is.null(x)) NA_integer_ else length(x),
      integer(1)
    )
    if (is.null(case_dates[[i]])) {
      stop(
        "episodic_add_manual_cluster(): cluster ", i, " supplies case-level ",
        "detail (pc/sex/age) but no 'case_dates' - sample_date is required ",
        "for every row of episodic_cluster_manual_case.",
        call. = FALSE
      )
    }
    detail_lengths <- detail_lengths[!is.na(detail_lengths)]
    if (length(unique(detail_lengths)) > 1) {
      stop(
        "episodic_add_manual_cluster(): cluster ", i, "'s case-level ",
        "detail vectors (case_dates/pc/sex/age) must all have the same ",
        "length; got ", paste(detail_lengths, collapse = ", "), ".",
        call. = FALSE
      )
    }
    n_i <- detail_lengths[1]
    if (!is.na(n_cases[i]) && n_cases[i] != n_i) {
      stop(
        "episodic_add_manual_cluster(): cluster ", i, "'s n_cases (",
        n_cases[i], ") does not match the length of its case-level ",
        "detail (", n_i, ").",
        call. = FALSE
      )
    }
    n_cases[i] <- n_i
    sex_i <- if (!is.null(sex[[i]])) sex[[i]] else rep(NA_character_, n_i)
    if (any(!sex_i %in% c("M", "F", "U", NA))) {
      stop(
        "episodic_add_manual_cluster(): cluster ", i, "'s 'sex' must be ",
        '"M", "F", "U" or NA.',
        call. = FALSE
      )
    }
    cases[[i]] <- data.frame(
      sample_date = if (!is.null(case_dates[[i]])) as.Date(case_dates[[i]]) else as.Date(rep(NA, n_i)),
      pc = if (!is.null(pc[[i]])) as.character(pc[[i]]) else rep(NA_character_, n_i),
      sex = sex_i,
      age = if (!is.null(age[[i]])) as.integer(age[[i]]) else rep(NA_integer_, n_i),
      stringsAsFactors = FALSE
    )
  }

  list(
    n = n,
    pathogen = pathogen,
    level = level,
    first_day = first_day,
    last_day = last_day,
    n_cases = n_cases,
    care_line = care_line,
    region_code = region_code,
    institution_id = institution_id,
    ward = ward,
    expected = expected,
    excess = excess,
    ratio = ratio,
    detector_agreement = detector_agreement,
    priority_score = priority_score,
    note = note,
    cases = cases
  )
}
