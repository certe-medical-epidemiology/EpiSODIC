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

#' Load optional institution activity (patient-days)
#'
#' Writes operator-supplied weekly hospital activity to
#' `episodic_institution_activity`, keyed on `institution_key` (resolved to
#' `institution_id` here, mirroring `episodic_denominators_load()`'s
#' own optional-source pattern). Entirely optional: a site with nothing to
#' supply here simply never calls this, and L1/L2 Farrington detection
#' falls back to raw counts - unnormalised, not broken: patient-day
#' normalisation is a refinement, not a requirement.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param activity A data frame (or tibble) with `institution_key`, `period_start`,
#'   `period_end`, `patient_days` (nullable `admissions`, `n_beds`,
#'   `source`).
#' @return Invisibly, a list with `n_supplied`, `n_written` and
#'   `n_skipped`. Rows whose `institution_key` matches no known
#'   institution are skipped rather than raising an error - an operator's
#'   activity feed and case feed need not be perfectly synchronised - but
#'   they are counted, warned about, and recorded against the run, which
#'   finishes `partial` rather than `success`.
#'
#' Not exported: an operator supplies a source to [episodic_run_cron()] via
#' `institution_activity`; this is the internal write step run
#' against it.
#' @keywords internal
#' @noRd
episodic_institution_activity_load <- function(con, activity) {
  # Checked by episodic_run_cron() before the run starts as well, so a
  # problem with this feed reaches the operator the same way a problem
  # with the case and denominator feeds does.
  episodic_validate_institution_activity(activity)

  institutions <- episodic_db_institutions(con)
  n_written <- 0L
  skipped_keys <- character(0)
  for (i in seq_len(nrow(activity))) {
    row <- activity[i, ]
    institution_id <- institutions$institution_id[
      institutions$institution_key == row$institution_key
    ]
    if (length(institution_id) == 0) {
      skipped_keys <- c(skipped_keys, row$institution_key)
      next
    }
    episodic_db_institution_activity_upsert(
      con,
      institution_id = institution_id[1],
      period_start = row$period_start,
      period_end = row$period_end,
      patient_days = row$patient_days,
      admissions = row$admissions %||% NA,
      n_beds = row$n_beds %||% NA,
      source = row$source %||% NA
    )
    n_written <- n_written + 1L
  }

  # Skipping is deliberate - an activity feed and a case feed need not be
  # perfectly synchronised - but skipping in silence is not. An operator
  # whose two feeds key institutions differently would otherwise see a
  # green run and never learn that none of their patient-days landed.
  if (length(skipped_keys) > 0) {
    unmatched <- unique(skipped_keys)
    shown <- if (length(unmatched) > 5) unmatched[1:5] else unmatched
    warning(
      length(skipped_keys),
      " of ",
      nrow(activity),
      " institution activity row(s) ",
      "were skipped: their `institution_key` matches no institution in the case ",
      "data (",
      paste(shown, collapse = ", "),
      if (length(unmatched) > length(shown)) ", ..." else "",
      "). ",
      "Detection falls back to raw counts for those institutions.",
      call. = FALSE
    )
  }

  invisible(list(
    n_supplied = nrow(activity),
    n_written = n_written,
    n_skipped = length(skipped_keys)
  ))
}

#' Check the optional institution activity feed against its own contract
#'
#' Split out of the load step so [episodic_run_cron()] can run it before
#' the run writes anything, mirroring `episodic_validate_denominators()`.
#'
#' @param activity A data frame (or tibble) with `institution_key`,
#'   `period_start`, `period_end`, `patient_days` (nullable `admissions`,
#'   `n_beds`, `source`).
#' @return Invisibly, `NULL`. Throws otherwise.
#' @keywords internal
#' @noRd
episodic_validate_institution_activity <- function(activity) {
  episodic_validate_columns(
    activity,
    required = c(
      "institution_key",
      "period_start",
      "period_end",
      "patient_days"
    ),
    filled = c("institution_key", "period_start", "period_end"),
    what = "Institution activity data"
  )
  episodic_validate_dates(
    activity,
    "period_start",
    na_ok = FALSE,
    what = "Institution activity data"
  )
  episodic_validate_dates(
    activity,
    "period_end",
    na_ok = FALSE,
    what = "Institution activity data"
  )
  if (
    nrow(activity) > 0 &&
      !all(is.na(activity$patient_days)) &&
      !is.numeric(activity$patient_days)
  ) {
    stop(
      "Institution activity data has a non-numeric `patient_days` (",
      paste(class(activity$patient_days), collapse = "/"),
      ").",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check your hospital activity data before you hand it to EpiSODIC
#'
#' The same purpose as [episodic_check_cases()], for the optional
#' `institution_activity` feed: every check
#' [episodic_run_cron()] would refuse a run over, reported instead of
#' thrown, plus a couple of things worth a look. Nothing is written, no
#' database is needed.
#'
#' Unlike the case and denominator feeds, an `institution_key` that
#' matches no institution in your case data is not a problem here - it
#' is deliberately allowed, and only ever counted and warned about at
#' run time (see `episodic_institution_activity_load()`), since the two
#' feeds need not be perfectly synchronised and this function has no
#' case data to compare against in the first place.
#'
#' @param institution_activity Your hospital activity data: a data frame
#'   or `tibble` with `institution_key`, `period_start`, `period_end`,
#'   `patient_days` (nullable `admissions`, `n_beds`, `source`) - see
#'   [episodic_synthetic_institution_activity()] for the shape - or a
#'   zero-argument function returning one.
#' @return A data frame of findings with class `episodic_case_check`, the
#'   same shape [episodic_check_cases()] returns. Zero rows means the
#'   data set passed every check.
#' @seealso [episodic_check_cases()] for the case data feed,
#'   `episodic_check_denominators()` for the positivity feed.
#' @examples
#' institutions <- data.frame(
#'   institution_key = "HOSP-1", institution_type = "hospital", n_beds = 320
#' )
#' activity <- episodic_synthetic_institution_activity(
#'   institutions,
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' episodic_check_institution_activity(activity)
#' @export
episodic_check_institution_activity <- function(institution_activity) {
  title <- "EpiSODIC institution activity data check"
  what <- "Institution activity data"

  resolved <- tryCatch(
    episodic_resolve_data(institution_activity),
    error = function(e) e
  )
  if (inherits(resolved, "condition")) {
    return(episodic_check_report(
      list(episodic_check_finding(
        severity = "problem",
        issue = "not_a_data_set",
        message = paste0(
          what,
          " must be a data frame (or tibble), or a function returning ",
          "one, not ",
          paste(class(institution_activity), collapse = "/"),
          "."
        ),
        fix = paste0(
          "Hand over the extract itself, e.g. ",
          "episodic_check_institution_activity(my_activity)."
        )
      )),
      title = title,
      what = what
    ))
  }
  if (!is.data.frame(resolved)) {
    return(episodic_check_report(
      list(episodic_check_finding(
        severity = "problem",
        issue = "not_a_data_set",
        message = paste0(
          what,
          " must be a data frame (or tibble), not ",
          paste(class(resolved), collapse = "/"),
          "."
        ),
        fix = paste0(
          "A function passed as `institution_activity` must return the ",
          "data set itself, not something built from it."
        )
      )),
      title = title,
      what = what
    ))
  }

  found <- c(
    episodic_check_institution_activity_structure(resolved),
    episodic_check_institution_activity_values(resolved),
    episodic_check_institution_activity_advice(resolved)
  )
  episodic_check_report(
    found,
    info = list(n_rows = nrow(resolved), n_cols = ncol(resolved)),
    title = title,
    what = what
  )
}

#' @keywords internal
#' @noRd
episodic_check_institution_activity_columns <- c(
  "institution_key",
  "period_start",
  "period_end",
  "patient_days"
)

#' @keywords internal
#' @noRd
episodic_check_institution_activity_structure <- function(activity) {
  found <- list()
  missing_cols <- setdiff(
    episodic_check_institution_activity_columns,
    names(activity)
  )
  if (length(missing_cols) > 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "problem",
      issue = "missing_column",
      column = paste(missing_cols, collapse = ", "),
      values = missing_cols,
      message = paste0(
        "Institution activity data is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      fix = paste0(
        "Add the column(s): `institution_key`, `period_start`, ",
        "`period_end` and `patient_days` (nullable `admissions`, ",
        "`n_beds`, `source`)."
      )
    )
  }
  found
}

#' @keywords internal
#' @noRd
episodic_check_institution_activity_values <- function(activity) {
  found <- list()

  for (column in c("institution_key", "period_start", "period_end")) {
    if (!column %in% names(activity)) {
      next
    }
    idx <- which(is.na(activity[[column]]))
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "empty_required_value",
        column = column,
        n_rows = length(idx),
        rows = idx,
        message = paste0(
          "`",
          column,
          "` is empty in ",
          length(idx),
          " of ",
          nrow(activity),
          " rows, and must always be filled."
        ),
        fix = "Every activity row must identify the institution and the period it covers."
      )
    }
  }

  for (column in c("period_start", "period_end")) {
    if (!column %in% names(activity)) {
      next
    }
    bad <- episodic_check_unreadable_dates(activity[[column]])
    idx <- which(bad)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "unreadable_date",
        column = column,
        n_rows = length(idx),
        rows = idx,
        values = episodic_check_chr(activity[[column]])[idx],
        message = paste0(
          "`",
          column,
          "` has ",
          length(idx),
          " of ",
          nrow(activity),
          " rows that are not a Date and do not read as YYYY-MM-DD."
        ),
        fix = episodic_check_date_fix(activity[[column]], idx)
      )
    }
  }

  if (
    "patient_days" %in%
      names(activity) &&
      nrow(activity) > 0 &&
      !all(is.na(activity$patient_days)) &&
      !is.numeric(activity$patient_days)
  ) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "problem",
      issue = "patient_days_not_numeric",
      column = "patient_days",
      message = paste0(
        "`patient_days` is ",
        paste(class(activity$patient_days), collapse = "/"),
        ", not a number."
      ),
      fix = "Give the number of patient-days for this institution and period as a number."
    )
  }

  found
}

#' @keywords internal
#' @noRd
episodic_check_institution_activity_advice <- function(activity) {
  found <- list()
  n <- nrow(activity)
  if (n == 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "advice",
      issue = "no_rows",
      n_rows = 0L,
      message = "Institution activity data has no rows at all.",
      fix = "Detection falls back to raw, unnormalised counts for every institution until this feed has rows."
    )
    return(found)
  }

  if (all(c("period_start", "period_end") %in% names(activity))) {
    starts <- episodic_check_as_date(activity$period_start)
    ends <- episodic_check_as_date(activity$period_end)
    idx <- which(!is.na(starts) & !is.na(ends) & ends < starts)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "period_end_before_start",
        column = "period_end",
        n_rows = length(idx),
        rows = idx,
        message = paste0(
          "`period_end` is earlier than `period_start` in ",
          length(idx),
          " of ",
          n,
          " rows."
        ),
        fix = "A period cannot end before it starts - usually the two columns are swapped."
      )
    }
  }

  if (
    "patient_days" %in% names(activity) && is.numeric(activity$patient_days)
  ) {
    idx <- which(!is.na(activity$patient_days) & activity$patient_days < 0)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "patient_days_negative",
        column = "patient_days",
        n_rows = length(idx),
        rows = idx,
        values = as.character(activity$patient_days[idx]),
        message = paste0(
          "`patient_days` is negative in ",
          length(idx),
          " of ",
          n,
          " rows."
        ),
        fix = "A negative patient-day count is always a mistake upstream."
      )
    }
  }

  found
}

#' Add a hospital activity feed (patient-days)
#'
#' Raw case counts at a hospital can rise simply because the hospital is
#' busier, not because infection risk has increased. If you can supply
#' weekly patient-days (or admissions, or bed counts) per hospital,
#' EpiSODIC can normalise case counts against activity for a more reliable
#' signal. This feed is entirely optional: without it, detection falls back
#' to raw counts, which is a reasonable default, not a broken one.
#'
#' This function is a synthetic example showing the expected shape: weekly
#' patient-days per hospital, modelled as bed count times occupancy, with a
#' realistic winter peak. Use it as a template for your own data, which you
#' pass to [episodic_run_cron()] as `institution_activity` -
#' normally a data frame or `tibble`.
#'
#' @param institutions A data frame (or tibble) of institutions (as
#'   returned by your own institution registry), filtered internally to
#'   hospitals only.
#' @param start_date,end_date The period to generate weekly rows for.
#'   Defaults to the five years up to today, matching
#'   [episodic_synthetic_cases()].
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame with `institution_key`, `period_start`,
#'   `period_end`, `patient_days`, `n_beds`, `source`.
#' @examples
#' institutions <- data.frame(
#'   institution_key = "HOSP-1", institution_type = "hospital", n_beds = 320
#' )
#' activity <- episodic_synthetic_institution_activity(
#'   institutions,
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' head(activity)
#' @export
episodic_synthetic_institution_activity <- function(
  institutions,
  start_date = end_date - 5 * 365,
  end_date = Sys.Date(),
  seed = 1
) {
  set.seed(seed)
  hospitals <- institutions[institutions$institution_type == "hospital", ]
  if (nrow(hospitals) == 0) {
    return(data.frame(
      institution_key = character(0),
      period_start = character(0),
      period_end = character(0),
      patient_days = integer(0),
      n_beds = integer(0),
      source = character(0),
      stringsAsFactors = FALSE
    ))
  }

  week_starts <- seq(start_date, end_date, by = "week")
  rows <- lapply(seq_len(nrow(hospitals)), function(i) {
    h <- hospitals[i, ]
    doy <- as.integer(format(week_starts, "%j"))
    seasonal_occupancy <- 0.82 + 0.08 * cos(2 * pi * (doy - 15) / 365.25) # winter peak
    patient_days <- round(
      h$n_beds *
        pmin(pmax(seasonal_occupancy, 0.5), 1) *
        7 *
        stats::runif(length(week_starts), 0.95, 1.05)
    )
    data.frame(
      institution_key = h$institution_key,
      period_start = as.character(week_starts),
      period_end = as.character(week_starts + 6),
      patient_days = patient_days,
      n_beds = h$n_beds,
      source = "synthetic",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
