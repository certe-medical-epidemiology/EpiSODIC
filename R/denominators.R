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

#' Load optional positivity metadata
#'
#' Writes the operator-supplied, pre-aggregated denominator table (see
#' `vignette("data-format")`'s "Positivity metadata" section) to
#' `episodic_denominator`. Entirely optional: a site with nothing to supply
#' here simply never calls this, and positivity panels stay blank for its
#' streams. Deliberately not a raw per-test linelist, so volume stays a
#' handful of aggregate rows per pathogen/period/stratum rather than every
#' individual test result.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param denominators A data frame (or tibble) with columns `pathogen`,
#'   `sample_date`, `care_line`, `area_code` (nullable) and `n_tests`.
#' @return Invisibly, a list with `n_supplied` and `n_written`. On a feed
#'   the database has not seen these are equal; re-sending a feed unchanged
#'   writes nothing, and `n_written` says so.
#'
#' Not exported: an operator supplies a source to [episodic_run_cron()] via
#' `denominators`; this is the internal write step run against it.
#' @keywords internal
#' @noRd
episodic_denominators_load <- function(con, denominators) {
  # Checked by episodic_run_cron() before the run starts as well, so a
  # problem with this feed reaches the operator the same way a problem
  # with the case feed does.
  episodic_validate_denominators(denominators)

  # Same reading of a missing care line as the case feed: unknown.
  denominators$care_line[is.na(denominators$care_line)] <- "unknown"

  n_supplied <- nrow(denominators)
  if (n_supplied == 0) {
    return(invisible(list(n_supplied = 0L, n_written = 0L)))
  }
  denominators$sample_date <- episodic_sql_date(denominators$sample_date)

  # (pathogen, sample_date, care_line, area_code) is the table's unique key,
  # so one row per key goes in - the last, as with an institution keyed
  # twice. A feed naming the same day twice used to be absorbed by the loop
  # this replaced, whose second iteration updated what its first had
  # inserted; a batched insert collides on the unique index instead, and
  # takes the whole run down with it.
  keys <- episodic_denominator_key(denominators)
  keep <- !duplicated(keys, fromLast = TRUE)
  denominators <- denominators[keep, , drop = FALSE]
  keys <- keys[keep]

  # One read, one insert, and an update only where a count actually moved -
  # rather than a SELECT plus a write per row, which is a round trip apiece
  # and, for a year of daily testing volumes per pathogen, most of what a
  # run spent on this feed.
  #
  # The matching is done in R rather than by an upsert on the unique index,
  # and has to be: area_code is nullable, and NULL is not equal to NULL in
  # SQL, so no conflict would ever be detected for the rows that leave it
  # empty. Every such row would insert afresh on every run, for ever. The
  # loop this replaced hand-wrote `area_code IS NULL` matching for exactly
  # that reason; in R, NA is a value like any other.
  #
  # Bounded by the feed's own date span: a row outside it cannot match any
  # key in the batch, so there is no reason to carry years of history back
  # over the wire to look at. ISO 8601 dates compare lexicographically,
  # which is why the schema stores them as text.
  params <- list(min(denominators$sample_date), max(denominators$sample_date))
  on_file <- DBI::dbGetQuery(
    con,
    "SELECT denominator_id, pathogen, sample_date, care_line, area_code, n_tests
       FROM episodic_denominator
      WHERE sample_date >= ? AND sample_date <= ?",
    params = params
  )
  known <- match(keys, episodic_denominator_key(on_file))

  new <- is.na(known)
  if (any(new)) {
    episodic_db_write_many(
      con,
      table = "episodic_denominator",
      cols = c("pathogen", "sample_date", "care_line", "area_code", "n_tests"),
      values = list(
        pathogen = denominators$pathogen[new],
        sample_date = denominators$sample_date[new],
        care_line = denominators$care_line[new],
        area_code = denominators$area_code[new],
        n_tests = denominators$n_tests[new]
      )
    )
  }

  # An UPDATE cannot be batched into one statement the way an INSERT can,
  # but it does not need to be: a re-sent feed says what the table already
  # says, so this loop is empty on a routine run and carries only the days
  # whose count was revised.
  moved <- which(!new & on_file$n_tests[known] != denominators$n_tests)
  for (i in moved) {
    params <- list(denominators$n_tests[i], on_file$denominator_id[known[i]])
    DBI::dbExecute(
      con,
      "UPDATE episodic_denominator SET n_tests = ? WHERE denominator_id = ?",
      params = params
    )
  }

  invisible(list(
    n_supplied = n_supplied,
    n_written = sum(new) + length(moved)
  ))
}

#' The denominator table's unique key, as one string per row
#'
#' Both sides of the match - the incoming feed and what is on file - are
#' keyed through here, so they can only ever disagree together. Separated
#' by a control character, which cannot occur in a pathogen name or an area
#' code, and with a marker for `NA` that is likewise unwriteable: an absent
#' area code is a key value of its own, not an empty one.
#' @keywords internal
#' @noRd
episodic_denominator_key <- function(rows) {
  area_code <- as.character(rows$area_code)
  area_code[is.na(area_code)] <- "\v"
  paste(rows$pathogen, rows$sample_date, rows$care_line, area_code, sep = "\r")
}

#' Check the optional denominator feed against its own requirements
#'
#' Split out of the load step so [episodic_run_cron()] can run it before
#' the run writes anything: an operator who supplies a testing-volume feed
#' should learn what is wrong with it where they can fix it, not from a
#' rolled-back transaction.
#'
#' @param denominators A data frame (or tibble) with columns `pathogen`,
#'   `sample_date`, `care_line`, `area_code` (nullable) and `n_tests`.
#' @return Invisibly, `NULL`. Throws otherwise.
#' @keywords internal
#' @noRd
episodic_validate_denominators <- function(denominators) {
  episodic_validate_columns(
    denominators,
    required = c(
      "pathogen",
      "sample_date",
      "care_line",
      "area_code",
      "n_tests"
    ),
    filled = c("pathogen", "sample_date", "n_tests"),
    what = "Denominator data"
  )
  episodic_validate_allowed(
    denominators,
    "care_line",
    episodic_care_lines,
    na_ok = TRUE,
    what = "Denominator data"
  )
  episodic_validate_dates(
    denominators,
    "sample_date",
    na_ok = FALSE,
    what = "Denominator data"
  )
  if (nrow(denominators) > 0 && !is.numeric(denominators$n_tests)) {
    stop(
      "Denominator data has a non-numeric `n_tests` (",
      paste(class(denominators$n_tests), collapse = "/"),
      "). Give the number of tests performed for this pathogen, period ",
      "and stratum as a number.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check Your Positivity Data Before You Hand It to EpiSODIC
#'
#' The same purpose as [episodic_check_cases()], for the optional
#' `denominators` feed: every check `episodic_validate_denominators()`
#' would throw on, reported instead of thrown - what is wrong, how many
#' rows, which ones, and what to do about it - plus a couple of things
#' that are allowed but worth a look. Nothing is written, no database is
#' needed. Run this while building your extract step, or when a run's
#' positivity panels come out empty or look wrong.
#'
#' @param denominators Your positivity data: a data frame or `tibble`
#'   with `pathogen`, `sample_date`, `care_line`, `area_code` and
#'   `n_tests` (see [episodic_synthetic_denominators()] for the shape),
#'   or a zero-argument function returning one.
#' @return A data frame of findings with class `episodic_case_check`, the
#'   same shape [episodic_check_cases()] returns. Zero rows means the
#'   data set passed every check.
#' @seealso [episodic_check_cases()] for the case data feed,
#'   `episodic_check_institution_activity()` for the hospital activity
#'   feed.
#' @examples
#' denom <- episodic_synthetic_denominators(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' episodic_check_denominators(denom)
#' @export
episodic_check_denominators <- function(denominators) {
  title <- "EpiSODIC denominator data check"
  what <- "Denominator data"

  resolved <- tryCatch(
    episodic_resolve_data(denominators),
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
          paste(class(denominators), collapse = "/"),
          "."
        ),
        fix = paste0(
          "Hand over the extract itself, e.g. ",
          "episodic_check_denominators(my_denominators)."
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
          "A function passed as `denominators` must return the data set ",
          "itself, not something built from it."
        )
      )),
      title = title,
      what = what
    ))
  }

  found <- c(
    episodic_check_denominators_structure(resolved),
    episodic_check_denominators_values(resolved),
    episodic_check_denominators_advice(resolved)
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
episodic_check_denominators_columns <- c(
  "pathogen",
  "sample_date",
  "care_line",
  "area_code",
  "n_tests"
)

#' @keywords internal
#' @noRd
episodic_check_denominators_structure <- function(denominators) {
  found <- list()
  missing_cols <- setdiff(
    episodic_check_denominators_columns,
    names(denominators)
  )
  if (length(missing_cols) > 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "problem",
      issue = "missing_column",
      column = paste(missing_cols, collapse = ", "),
      values = missing_cols,
      message = paste0(
        "Denominator data is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      fix = paste0(
        "Add the column(s): `pathogen`, `sample_date`, `care_line`, ",
        "`area_code` and `n_tests` (see ?episodic_synthetic_denominators ",
        "for the shape). `care_line` and `area_code` may be all-NA."
      )
    )
  }
  found
}

#' @keywords internal
#' @noRd
episodic_check_denominators_values <- function(denominators) {
  found <- list()

  for (column in c("pathogen", "sample_date", "n_tests")) {
    if (!column %in% names(denominators)) {
      next
    }
    idx <- which(is.na(denominators[[column]]))
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
          nrow(denominators),
          " rows, and must always be filled."
        ),
        fix = "Every denominator row must name the pathogen, week and test count it counts."
      )
    }
  }

  if ("care_line" %in% names(denominators)) {
    values <- episodic_check_chr(denominators$care_line)
    idx <- which(!is.na(values) & !values %in% episodic_care_lines)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "value_outside_allowed_set",
        column = "care_line",
        n_rows = length(idx),
        rows = idx,
        values = values[idx],
        message = paste0(
          "`care_line` has ",
          length(idx),
          " of ",
          nrow(denominators),
          " rows with a value outside the allowed set (",
          paste0("\"", episodic_care_lines, "\"", collapse = ", "),
          ", or NA)."
        ),
        fix = "Map your own coding onto `episodic_care_lines`."
      )
    }
  }

  if ("sample_date" %in% names(denominators)) {
    bad <- episodic_check_unreadable_dates(denominators$sample_date)
    idx <- which(bad)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "unreadable_date",
        column = "sample_date",
        n_rows = length(idx),
        rows = idx,
        values = episodic_check_chr(denominators$sample_date)[idx],
        message = paste0(
          "`sample_date` has ",
          length(idx),
          " of ",
          nrow(denominators),
          " rows that are not a Date and do not read as YYYY-MM-DD."
        ),
        fix = episodic_check_date_fix(denominators$sample_date, idx)
      )
    }
  }

  if (
    "n_tests" %in%
      names(denominators) &&
      nrow(denominators) > 0 &&
      !is.numeric(denominators$n_tests)
  ) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "problem",
      issue = "n_tests_not_numeric",
      column = "n_tests",
      message = paste0(
        "`n_tests` is ",
        paste(class(denominators$n_tests), collapse = "/"),
        ", not a number."
      ),
      fix = "Give the number of tests performed for this pathogen, period and stratum as a number."
    )
  }

  found
}

#' @keywords internal
#' @noRd
episodic_check_denominators_advice <- function(denominators) {
  found <- list()
  if (nrow(denominators) == 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "advice",
      issue = "no_rows",
      n_rows = 0L,
      message = "Denominator data has no rows at all.",
      fix = "Positivity panels stay blank for every stream until this feed has rows."
    )
    return(found)
  }
  if ("n_tests" %in% names(denominators) && is.numeric(denominators$n_tests)) {
    idx <- which(!is.na(denominators$n_tests) & denominators$n_tests <= 0)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "n_tests_not_positive",
        column = "n_tests",
        n_rows = length(idx),
        rows = idx,
        values = as.character(denominators$n_tests[idx]),
        message = paste0(
          "`n_tests` is zero or negative in ",
          length(idx),
          " of ",
          nrow(denominators),
          " rows."
        ),
        fix = paste0(
          "A period with zero tests performed is unusual but not ",
          "impossible; a negative count is always a mistake upstream. ",
          "Either way, positivity cannot be computed for that row and it ",
          "is skipped."
        )
      )
    }
  }
  if ("pathogen" %in% names(denominators)) {
    found <- c(found, episodic_check_pathogen_advice(denominators))
  }
  found
}

#' Add a Testing-Volume (Positivity) Feed
#'
#' Case counts alone cannot distinguish a rise in infections from a rise in
#' testing. If you can supply how many tests were performed - even as a
#' weekly aggregate, not per-test detail - EpiSODIC can show a positivity
#' rate alongside the case count, which is often the more meaningful signal.
#' This feed is entirely optional: skip it and positivity panels simply stay
#' blank.
#'
#' This function is a synthetic example showing the expected shape: weekly
#' counts of a multiplex GI PCR panel that also reports Norovirus. Use it as
#' a template for your own data, which you pass to [episodic_run_cron()] as
#' `denominators` - a data frame or `tibble` with the same five
#' columns: `pathogen`, `sample_date` (week start), `care_line`,
#' `area_code` (may be `NA`), and `n_tests`.
#'
#' @param start_date,end_date The period to generate weekly rows for.
#'   Defaults to the five years up to today, matching
#'   [episodic_synthetic_cases()].
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame with `pathogen`, `sample_date` (week start),
#'   `care_line`, `area_code`, `n_tests`.
#' @examples
#' denom <- episodic_synthetic_denominators(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' head(denom)
#' @export
episodic_synthetic_denominators <- function(start_date = end_date - 5 * 365,
                                            end_date = Sys.Date(),
                                            seed = 1) {
  set.seed(seed)
  week_starts <- seq(start_date, end_date, by = "week")
  n <- length(week_starts)
  data.frame(
    pathogen = "Norovirus",
    sample_date = as.character(week_starts),
    care_line = "second",
    area_code = NA_character_,
    n_tests = stats::rpois(n, lambda = 40),
    stringsAsFactors = FALSE
  )
}
