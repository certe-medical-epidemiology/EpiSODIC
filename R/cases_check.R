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

#' Check Your Case Data Before You Hand It to EpiSODIC
#'
#' Runs every check the [episodic_case_data] contract implies over your
#' extract and reports *everything* it finds in one go - what is wrong,
#' how many rows are affected, which rows those are, what the offending
#' values look like, and what to do about each one. Nothing is written,
#' nothing is changed, and no database is needed: this is the call to make
#' while you are still building your extract step, and the first call to
#' make when a run did not produce what you expected.
#'
#' Use `episodic_check_cases()` plainly when you want to *look*, or
#' `episodic_check_cases(..., stop_on_problem = TRUE)` when you want a script
#' to stop. The latter is what [episodic_run_cron()] itself calls before a run.
#'
#' None of this applies to a cluster added with [episodic_add_manual_cluster()]:
#' it is never connected to your own case data, so it never goes through this
#' contract (or any other check here) at all.
#'
#' @section What it reports:
#'
#' Two kinds of finding, deliberately kept apart:
#'
#' \describe{
#'   \item{Problems}{Things EpiSODIC cannot work around: a missing or
#'     unexpected column, an empty value in a column that must be filled,
#'     a value outside a fixed set, a date that does not read as a date, a
#'     duplicated `source_key`. A run refuses to start while any of these
#'     stand.}
#'   \item{Advice}{Things that are *allowed* but rarely intended, and that
#'     quietly cost you signal: no `ward` on any hospital row (so
#'     ward-level detection has nothing to group on), one pathogen spelled
#'     two ways (so its cases are split over two streams), a
#'     `patient_key` that is unique per row (so deduplication and episode
#'     grouping cannot do anything), postcodes that the geography panel
#'     cannot place, sample dates in the future. A run proceeds
#'     regardless; these are for you to judge.}
#' }
#'
#' The value prints as a report; it is also a plain data frame, one row
#' per finding, so you can work with it programmatically (see the
#' examples).
#'
#' @param cases Your case data: a data frame or `tibble` in the shape
#'   [episodic_case_data] describes, or a zero-argument function returning
#'   one. Anything else is itself reported as a problem rather
#'   than throwing - the point of this function is that it always answers.
#' @param stop_on_problem A [logical] to indicate whether an error must be
#'   thrown if any problem is found. Default is `FALSE`.
#' @return A data frame of findings with class `episodic_case_check` and
#'   one row per finding, with columns `severity` (`"problem"` or
#'   `"advice"`), `issue`, `column`, `n_rows`, `rows`, `values`,
#'   `message` and `fix`. Zero rows means the data set passed every check.
#'   A summary of what was read (rows, columns, date range, counts) is
#'   attached as the `"summary"` attribute and shown when printing.
#' @seealso [episodic_case_data] for what each column means and which
#'   values it accepts.
#' @examples
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
#' )
#' episodic_check_cases(cases)
#'
#' # a typical extract mistake: dates written day-first, sex as words
#' broken <- cases
#' broken$sample_date <- format(as.Date(broken$sample_date), "%d-%m-%Y")
#' broken$sex <- ifelse(broken$sex == "M", "male", "female")
#' report <- episodic_check_cases(broken)
#' report
#'
#' # it is a data frame too
#' report$column[report$severity == "problem"]
#' @export
episodic_check_cases <- function(cases, stop_on_problem = FALSE) {
  if (isTRUE(stop_on_problem)) {
    episodic_validate_cases(cases)
  }

  resolved <- tryCatch(episodic_resolve_data(cases), error = function(e) e)
  if (inherits(resolved, "condition")) {
    return(episodic_check_report(list(episodic_check_finding(
      severity = "problem",
      issue = "not_a_data_set",
      message = paste0(
        "Case data must be a data frame (or tibble), or a function ",
        "returning one, not ",
        paste(class(cases), collapse = "/"),
        "."
      ),
      fix = paste0(
        "Hand over the extract itself, e.g. ",
        "episodic_check_cases(my_cases)."
      )
    ))))
  }
  if (!is.data.frame(resolved)) {
    return(episodic_check_report(list(episodic_check_finding(
      severity = "problem",
      issue = "not_a_data_set",
      message = paste0(
        "Case data must be a data frame (or tibble), not ",
        paste(class(resolved), collapse = "/"),
        "."
      ),
      fix = paste0(
        "A function passed as `cases` must return the data set itself, ",
        "not something built from it."
      )
    ))))
  }

  found <- c(
    episodic_check_structure(resolved),
    episodic_check_values(resolved),
    episodic_check_advice(resolved)
  )
  episodic_check_report(found, info = episodic_check_summary(resolved))
}

#' Columns: present, allow-listed, and one row per result
#' @keywords internal
#' @noRd
episodic_check_structure <- function(cases) {
  found <- list()

  missing_cols <- setdiff(episodic_case_columns, names(cases))
  if (length(missing_cols) > 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "problem",
      issue = "missing_column",
      column = paste(missing_cols, collapse = ", "),
      values = missing_cols,
      message = paste0(
        "Case data is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      fix = paste0(
        "Add the column(s). `episodic_case_columns` lists all ",
        length(episodic_case_columns),
        " in order. A column your laboratory does not record may be ",
        "all-NA, except source_key, lab_number, patient_key, sample_date, ",
        "pathogen, institution_key, institution_display_name and ",
        "institution_type."
      )
    )
  }

  extra_cols <- setdiff(names(cases), episodic_case_columns)
  if (length(extra_cols) > 0) {
    guesses <- episodic_check_column_guesses(extra_cols)
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "problem",
      issue = "unexpected_column",
      column = paste(extra_cols, collapse = ", "),
      values = extra_cols,
      message = paste0(
        "Case data contains column(s) outside the allow-list: ",
        paste(extra_cols, collapse = ", "),
        ". The case data interface is an explicit allow-list; a new ",
        "upstream column must not leak in silently."
      ),
      fix = paste0(
        if (nzchar(guesses)) paste0(guesses, " Otherwise drop") else "Drop",
        " the column(s) in your extract step, e.g. ",
        "cases <- cases[, episodic_case_columns]."
      )
    )
  }

  if ("source_key" %in% names(cases)) {
    key <- cases$source_key
    duplicate <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(duplicate)) {
      idx <- which(duplicate)
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "duplicate_source_key",
        column = "source_key",
        n_rows = length(idx),
        rows = idx,
        values = key[idx],
        message = paste0(
          "Case data contains duplicate source_key values, in ",
          length(idx),
          " of ",
          nrow(cases),
          " rows."
        ),
        fix = paste0(
          "source_key must identify one laboratory result, not one ",
          "specimen - it needs its own value on every row. If your ",
          "extract joins a table that multiplies rows, de-duplicate ",
          "before handing the data over. If the repeats are legitimate - ",
          "one culture yielding results under two pathogen names, or two ",
          "isolates of the same pathogen from one culture with different ",
          "antibiograms - build source_key from whatever combination of ",
          "your own columns is unique per row: many laboratory systems ",
          "need all of lab_number, patient_key, a test/panel code and a ",
          "strain/isolate number, e.g. paste(lab_number, patient_key, ",
          "test_code, isolate_number)."
        )
      )
    }
  }

  found
}

#' Values: filled, inside their allowed set, and readable as what they claim
#' @keywords internal
#' @noRd
episodic_check_values <- function(cases) {
  found <- list()

  for (column in intersect(episodic_case_columns_required, names(cases))) {
    values <- episodic_check_chr(cases[[column]])
    idx <- which(is.na(values) | !nzchar(trimws(values)))
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
          nrow(cases),
          " rows, and must always be filled."
        ),
        fix = episodic_check_required_fix(column)
      )
    }
  }

  allowed_sets <- list(
    care_line = list(
      values = episodic_care_lines,
      na_ok = TRUE,
      holder = "episodic_care_lines"
    ),
    institution_type = list(
      values = episodic_institution_types,
      na_ok = FALSE,
      holder = "episodic_institution_types"
    ),
    sex = list(
      values = episodic_sex_codes,
      na_ok = TRUE,
      holder = "episodic_sex_codes"
    )
  )
  for (column in intersect(names(allowed_sets), names(cases))) {
    spec <- allowed_sets[[column]]
    values <- episodic_check_chr(cases[[column]])
    bad <- !is.na(values) & !values %in% spec$values
    if (!isTRUE(spec$na_ok)) {
      bad <- bad | is.na(values)
    }
    idx <- which(bad)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "value_outside_allowed_set",
        column = column,
        n_rows = length(idx),
        rows = idx,
        values = values[idx],
        message = paste0(
          "`",
          column,
          "` has ",
          length(idx),
          " of ",
          nrow(cases),
          " rows with a value outside the allowed set (",
          paste0("\"", spec$values, "\"", collapse = ", "),
          if (isTRUE(spec$na_ok)) ", or NA" else "",
          ")."
        ),
        fix = paste0(
          episodic_check_case_hint(values[idx], spec$values),
          "Map your own coding onto the allowed values in your extract ",
          "step; they are in `",
          spec$holder,
          "`, so you need not copy the strings by hand."
        )
      )
    }
  }

  for (column in intersect(c("sample_date", "receipt_date"), names(cases))) {
    values <- cases[[column]]
    bad <- episodic_check_unreadable_dates(values)
    idx <- which(bad)
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "unreadable_date",
        column = column,
        n_rows = length(idx),
        rows = idx,
        values = episodic_check_chr(values)[idx],
        message = paste0(
          "`",
          column,
          "` has ",
          length(idx),
          " of ",
          nrow(cases),
          " rows that are not a Date and do not read as YYYY-MM-DD."
        ),
        fix = episodic_check_date_fix(values, idx)
      )
    }
  }

  if ("age" %in% names(cases)) {
    age <- cases$age
    if (!all(is.na(age)) && !is.numeric(age)) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "problem",
        issue = "age_not_numeric",
        column = "age",
        message = paste0(
          "`age` is ",
          paste(class(age), collapse = "/"),
          ", not a number."
        ),
        fix = paste0(
          "Give age in whole years at sampling, as a number - not an age ",
          "group such as \"65+\" or \"20-24\". The dashboard bands ages ",
          "itself."
        )
      )
    }
  }

  found
}

#' Things that are allowed, but cost you signal if they were not intended
#' @keywords internal
#' @noRd
episodic_check_advice <- function(cases) {
  found <- list()
  n <- nrow(cases)
  has <- function(...) all(c(...) %in% names(cases))

  if (n == 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "advice",
      issue = "no_rows",
      n_rows = 0L,
      message = "Case data has no rows at all.",
      fix = paste0(
        "A run over an empty extract completes, writes nothing, and ",
        "leaves the dashboard empty. Check the date filter and the ",
        "positives-only filter in your own query first."
      )
    )
    return(found)
  }

  factor_cols <- names(cases)[vapply(cases, is.factor, logical(1))]
  if (length(factor_cols) > 0) {
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "advice",
      issue = "factor_column",
      column = paste(factor_cols, collapse = ", "),
      values = factor_cols,
      message = paste0(
        "Column(s) held as a factor rather than text: ",
        paste(factor_cols, collapse = ", "),
        "."
      ),
      fix = paste0(
        "read.csv(stringsAsFactors = TRUE) and read.table() produce ",
        "factors. Convert with as.character() so a value never travels ",
        "as the number behind its level."
      )
    )
  }

  if (has("sample_date")) {
    dates <- episodic_check_as_date(cases$sample_date)
    ahead <- which(!is.na(dates) & dates > Sys.Date())
    if (length(ahead) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "sample_date_in_future",
        column = "sample_date",
        n_rows = length(ahead),
        rows = ahead,
        values = as.character(dates[ahead]),
        message = paste0(
          "`sample_date` lies in the future for ",
          length(ahead),
          " of ",
          n,
          " rows, up to ",
          max(dates[ahead]),
          "."
        ),
        fix = paste0(
          "Every detector anchors on sample_date, so future dates land ",
          "in weeks that have not happened yet and distort the current ",
          "week's counts. Usually a year typo, or a receipt date used as ",
          "a fallback."
        )
      )
    }
    if (has("receipt_date")) {
      received <- episodic_check_as_date(cases$receipt_date)
      early <- which(!is.na(dates) & !is.na(received) & received < dates)
      if (length(early) > 0) {
        found[[length(found) + 1]] <- episodic_check_finding(
          severity = "advice",
          issue = "receipt_before_sample",
          column = "receipt_date",
          n_rows = length(early),
          rows = early,
          message = paste0(
            "`receipt_date` is earlier than `sample_date` in ",
            length(early),
            " of ",
            n,
            " rows."
          ),
          fix = paste0(
            "The laboratory cannot receive a sample before it is taken. ",
            "Harmless to EpiSODIC (receipt_date is stored for provenance ",
            "only), but it usually means the two columns are swapped."
          )
        )
      }
    }
  }

  if (has("patient_key")) {
    # Two shapes of the same mistake - a sample identifier where a patient
    # identifier belongs. Identical columns say it outright; all-distinct
    # values only suggest it, and only once there are enough rows for
    # "every patient appears once" to be implausible rather than ordinary
    # for a rare pathogen.
    same_as_source <- has("source_key") &&
      identical(
        episodic_check_chr(cases$patient_key),
        episodic_check_chr(cases$source_key)
      )
    unique_per_row <- length(unique(cases$patient_key)) == n && n >= 20
    if (same_as_source || unique_per_row) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "patient_key_unique_per_row",
        column = "patient_key",
        n_rows = n,
        message = if (same_as_source) {
          "`patient_key` holds exactly the same values as `source_key`."
        } else {
          paste0(
            "`patient_key` is different on every one of the ",
            n,
            " rows."
          )
        },
        fix = paste0(
          "Deduplication and episode grouping key on patient_key: with a ",
          "value that never repeats, every positive becomes its own case ",
          "and repeat positives inflate the counts. Use a pseudonymised ",
          "patient identifier that is stable across results, not the ",
          "sample identifier."
        )
      )
    }
    looks_raw <- grepl("^[0-9]{8,10}$", episodic_check_chr(cases$patient_key))
    if (any(looks_raw, na.rm = TRUE)) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "patient_key_looks_identifying",
        column = "patient_key",
        n_rows = sum(looks_raw, na.rm = TRUE),
        message = paste0(
          "`patient_key` is a bare 8-10 digit number in ",
          sum(looks_raw, na.rm = TRUE),
          " of ",
          n,
          " rows."
        ),
        fix = paste0(
          "That is the shape of a BSN or hospital number. Pseudonymise ",
          "before the data reaches EpiSODIC - it only needs a value that ",
          "is the same for the same patient, never the number itself."
        )
      )
    }
  }

  if (has("pathogen")) {
    found <- c(found, episodic_check_pathogen_advice(cases))
  }
  if (has("institution_key", "institution_display_name", "institution_type")) {
    found <- c(found, episodic_check_institution_advice(cases))
  }
  if (has("pc")) {
    pc <- episodic_check_chr(cases$pc)
    odd <- which(!is.na(pc) & nzchar(pc) & !grepl("^[0-9]{4}$", pc))
    if (length(odd) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "pc_not_four_digits",
        column = "pc",
        n_rows = length(odd),
        rows = odd,
        values = pc[odd],
        message = paste0(
          "`pc` is not four digits in ",
          length(odd),
          " of ",
          n,
          " rows."
        ),
        fix = paste0(
          "The shipped Netherlands reference data keys on four-digit ",
          "postcode areas, as text so a leading zero survives (\"0",
          "123\"). Values it cannot place fall out of the geography ",
          "panel and out of area-level detection. Ignore this if you ",
          "supply your own EPISODIC_GEO_DATA with a different `pc`."
        )
      )
    }
    if (all(is.na(pc) | !nzchar(pc))) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "pc_never_filled",
        column = "pc",
        n_rows = n,
        message = "`pc` is empty on every row.",
        fix = paste0(
          "Allowed, but area-level (L3) detection, the geography panel ",
          "and the concentration measure that feeds the priority score ",
          "all read the patient's postcode. Without it they stay blank."
        )
      )
    }
  }
  if (has("age")) {
    age <- suppressWarnings(as.numeric(cases$age))
    odd <- which(!is.na(age) & (age < 0 | age > 120))
    if (length(odd) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "age_out_of_range",
        column = "age",
        n_rows = length(odd),
        rows = odd,
        values = age[odd],
        message = paste0(
          "`age` is below 0 or above 120 in ",
          length(odd),
          " of ",
          n,
          " rows."
        ),
        fix = paste0(
          "Age is in whole years at sampling. Ages in months or days, ",
          "and a placeholder such as 999, both land here."
        )
      )
    }
  }

  found
}

#' One pathogen spelled two ways is two streams, not one
#' @keywords internal
#' @noRd
episodic_check_pathogen_advice <- function(cases) {
  found <- list()
  pathogens <- unique(episodic_check_chr(cases$pathogen))
  pathogens <- pathogens[!is.na(pathogens) & nzchar(pathogens)]
  if (length(pathogens) == 0) {
    return(found)
  }

  normalised <- tolower(gsub("[[:space:]]+", " ", trimws(pathogens)))
  clashing <- unique(normalised[duplicated(normalised)])
  if (length(clashing) > 0) {
    groups <- vapply(
      clashing,
      function(key) {
        paste0(
          "\"",
          paste(pathogens[normalised == key], collapse = "\" / \""),
          "\""
        )
      },
      character(1)
    )
    found[[length(found) + 1]] <- episodic_check_finding(
      severity = "advice",
      issue = "pathogen_spelling_variants",
      column = "pathogen",
      n_rows = length(clashing),
      values = groups,
      message = paste0(
        length(clashing),
        if (length(clashing) == 1) {
          " pathogen name appears"
        } else {
          " pathogen names appear"
        },
        " in more than one spelling, differing only in capitalisation ",
        "or spacing."
      ),
      fix = paste0(
        "`pathogen` is used verbatim and never matched against a ",
        "taxonomy, so each spelling becomes its own surveillance stream ",
        "with its own history and its own baseline - splitting a cluster ",
        "in half. Settle on one spelling per pathogen in your extract ",
        "step, and keep it stable across runs."
      )
    )
  }

  configured <- episodic_check_configured_pathogens()
  if (!is.null(configured)) {
    unknown <- setdiff(pathogens, configured)
    if (length(unknown) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "pathogen_not_configured",
        column = "pathogen",
        n_rows = length(unknown),
        values = unknown,
        message = paste0(
          length(unknown),
          " of ",
          length(pathogens),
          if (length(unknown) == 1) {
            " pathogen names is not in the shipped pathogen configuration."
          } else {
            " pathogen names are not in the shipped pathogen configuration."
          }
        ),
        fix = paste0(
          "Not an error: these fall back to the schema defaults - a ",
          "30-day episode, 14 case-free days before a cluster closes, no ",
          "reproduction number and no MEM. Add them to ",
          "inst/config/episodic_default_pathogen_config.csv (or match the spelling used ",
          "there) to get pathogen-specific behaviour."
        )
      )
    }
  }

  found
}

#' An institution keyed two ways, or a hospital with no wards
#' @keywords internal
#' @noRd
episodic_check_institution_advice <- function(cases) {
  found <- list()
  keys <- episodic_check_chr(cases$institution_key)
  usable <- !is.na(keys) & nzchar(keys)
  if (!any(usable)) {
    return(found)
  }

  for (column in c("institution_display_name", "institution_type")) {
    values <- episodic_check_chr(cases[[column]])
    per_key <- tapply(values[usable], keys[usable], function(x) {
      length(unique(x[!is.na(x)]))
    })
    inconsistent <- names(per_key)[!is.na(per_key) & per_key > 1]
    if (length(inconsistent) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "institution_inconsistent",
        column = column,
        n_rows = length(inconsistent),
        values = inconsistent,
        message = paste0(
          length(inconsistent),
          if (length(inconsistent) == 1) {
            " institution_key value carries"
          } else {
            " institution_key values carry"
          },
          " more than one `",
          column,
          "`."
        ),
        fix = paste0(
          "Institutions are keyed on institution_key, and the last row ",
          "read wins for the rest. Two names (or two types) behind one ",
          "key make an institution's own history read inconsistently ",
          "from one run to the next."
        )
      )
    }
  }

  if ("ward" %in% names(cases)) {
    types <- episodic_check_chr(cases$institution_type)
    wards <- episodic_check_chr(cases$ward)
    clinical <- types %in% c("hospital", "ltc_institution")
    clinical_wards <- wards[clinical]
    if (
      any(clinical) &&
        all(is.na(clinical_wards) | !nzchar(clinical_wards))
    ) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "ward_never_filled",
        column = "ward",
        n_rows = sum(clinical),
        message = paste0(
          "`ward` is empty on all ",
          sum(clinical),
          " hospital and long-term care rows."
        ),
        fix = paste0(
          "Ward-level (L1) detection - the level that finds two cases on ",
          "one ward within days of each other - has nothing to group on ",
          "without it, so only institution level and above will run."
        )
      )
    }
    stray <- which(!clinical & !is.na(wards) & nzchar(wards))
    if (length(stray) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "ward_outside_institution",
        column = "ward",
        n_rows = length(stray),
        rows = stray,
        values = wards[stray],
        message = paste0(
          "`ward` is filled on ",
          length(stray),
          " row(s) whose institution_type is neither hospital nor ",
          "ltc_institution."
        ),
        fix = paste0(
          "Harmless - it is simply never read for those rows - but if ",
          "these are wards of a hospital, their institution_type is ",
          "wrong and they are not being watched at ward level."
        )
      )
    }
  }

  if ("municipality" %in% names(cases)) {
    types <- episodic_check_chr(cases$institution_type)
    municipality <- episodic_check_chr(cases$municipality)
    idx <- which(
      types == "gp_municipality" &
        (is.na(municipality) | !nzchar(municipality))
    )
    if (length(idx) > 0) {
      found[[length(found) + 1]] <- episodic_check_finding(
        severity = "advice",
        issue = "municipality_missing",
        column = "municipality",
        n_rows = length(idx),
        rows = idx,
        message = paste0(
          "`municipality` is empty on ",
          length(idx),
          " row(s) of type gp_municipality."
        ),
        fix = paste0(
          "A general practice is stored as the municipality it is in, ",
          "not as itself - deliberately, since a single-handed practice ",
          "is no transmission unit. With an empty municipality those ",
          "rows all collapse onto one nameless institution."
        )
      )
    }
  }

  found
}

#' Assemble one finding
#'
#' Every check produces the same shape, so the report is a plain data
#' frame an operator can filter, and the printed version is one code path.
#' @keywords internal
#' @noRd
episodic_check_finding <- function(
    severity,
    issue,
    column = NA_character_,
    n_rows = NA_integer_,
    rows = integer(0),
    values = character(0),
    message,
    fix = NA_character_) {
  data.frame(
    severity = severity,
    issue = issue,
    column = column,
    n_rows = as.integer(n_rows),
    rows = episodic_check_rows_text(rows),
    values = episodic_check_values_text(values),
    message = message,
    fix = fix,
    stringsAsFactors = FALSE
  )
}

#' @param title Heading shown by `print.episodic_case_check()`, and the
#'   "what" named in the thrown message if the report is ever turned into
#'   one. Defaults to the case data wording, since that is this
#'   function's original and most common caller;
#'   `episodic_check_denominators()` and
#'   `episodic_check_institution_activity()` pass their own.
#' @keywords internal
#' @noRd
episodic_check_report <- function(
    found,
    info = list(),
    title = "EpiSODIC case data check",
    what = "Case data") {
  problems <- if (length(found) == 0) {
    data.frame(
      severity = character(0),
      issue = character(0),
      column = character(0),
      n_rows = integer(0),
      rows = character(0),
      values = character(0),
      message = character(0),
      fix = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, found)
  }
  rownames(problems) <- NULL
  attr(problems, "summary") <- info
  attr(problems, "title") <- title
  attr(problems, "what") <- what
  class(problems) <- c("episodic_case_check", "data.frame")
  problems
}

#' What was read, shown above the findings
#'
#' An operator's first question when a run comes back empty is rarely
#' "which column is wrong" but "did it read what I think it read". The
#' header answers that before any finding does.
#' @keywords internal
#' @noRd
episodic_check_summary <- function(cases) {
  info <- list(n_rows = nrow(cases), n_cols = ncol(cases))
  if ("sample_date" %in% names(cases)) {
    dates <- episodic_check_as_date(cases$sample_date)
    if (any(!is.na(dates))) {
      info$date_min <- min(dates, na.rm = TRUE)
      info$date_max <- max(dates, na.rm = TRUE)
    }
  }
  distinct <- function(column) {
    if (!column %in% names(cases)) {
      return(NULL)
    }
    values <- episodic_check_chr(cases[[column]])
    length(unique(values[!is.na(values) & nzchar(values)]))
  }
  info$n_pathogens <- distinct("pathogen")
  info$n_institutions <- distinct("institution_key")
  info$n_patients <- distinct("patient_key")
  info
}

#' @keywords internal
#' @noRd
episodic_check_chr <- function(values) {
  as.character(values)
}

#' Parse only what the contract calls a date, so nothing reads as year 1
#' @keywords internal
#' @noRd
episodic_check_as_date <- function(values) {
  if (inherits(values, "Date")) {
    return(values)
  }
  text <- episodic_check_chr(values)
  parsed <- suppressWarnings(as.Date(text, format = "%Y-%m-%d"))
  parsed[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)] <- NA
  parsed
}

#' @keywords internal
#' @noRd
episodic_check_unreadable_dates <- function(values) {
  if (inherits(values, "Date")) {
    return(rep(FALSE, length(values)))
  }
  text <- episodic_check_chr(values)
  # Shape first, then reality: as.Date(format = "%Y-%m-%d") matches a
  # prefix and ignores the rest, so "01-01-2025" would otherwise come
  # back as the first of January in the year 1 rather than as a problem.
  bad <- !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text) |
    is.na(suppressWarnings(as.Date(text, format = "%Y-%m-%d")))
  # An empty value is the "must be filled" check's business, not this
  # one, so it is never reported twice.
  bad & !is.na(values)
}

#' Name the format the offending dates are actually in
#'
#' The single most common extract mistake, and the one an operator can act
#' on fastest if told which conversion to apply.
#' @keywords internal
#' @noRd
episodic_check_date_fix <- function(values, idx) {
  text <- episodic_check_chr(values)[idx]
  text <- text[!is.na(text)]
  hint <- if (inherits(values, "POSIXt")) {
    paste0(
      "These are date-times rather than dates: convert with as.Date(x), ",
      "minding the timezone."
    )
  } else if (any(grepl("^[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{4}$", text))) {
    paste0(
      "These look day-first (e.g. 31-12-2025): convert with ",
      "as.Date(x, format = \"%d-%m-%Y\")."
    )
  } else if (any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ]", text))) {
    paste0(
      "These carry a time component: as.Date(substr(x, 1, 10)) keeps the ",
      "date."
    )
  } else if (any(grepl("^[0-9]{5}$", text))) {
    paste0(
      "These look like Excel date serial numbers: ",
      "as.Date(as.numeric(x), origin = \"1899-12-30\")."
    )
  } else if (any(grepl("^[0-9]{8}$", text))) {
    paste0(
      "These look like YYYYMMDD: as.Date(x, format = \"%Y%m%d\")."
    )
  } else {
    NULL
  }
  paste0(
    if (!is.null(hint)) paste0(hint, " ") else "",
    "EpiSODIC accepts a Date column, or text in ISO 8601 YYYY-MM-DD ",
    "form, and nothing else - a date silently read the wrong way round ",
    "is worse than one that refuses to read."
  )
}

#' @keywords internal
#' @noRd
episodic_check_case_hint <- function(bad, allowed) {
  normalised <- trimws(tolower(unique(episodic_check_chr(bad))))
  normalised <- normalised[!is.na(normalised)]
  if (any(normalised %in% tolower(allowed))) {
    paste0(
      "Some of these match an allowed value apart from capitalisation ",
      "or surrounding spaces, and the comparison is exact. "
    )
  } else {
    ""
  }
}

#' @keywords internal
#' @noRd
episodic_check_required_fix <- function(column) {
  hints <- c(
    source_key = paste0(
      "source_key identifies the result in your own source system, so a ",
      "re-run of the same extract cannot create the case twice. Any ",
      "stable string will do, as long as it is unique per row - see ",
      "?episodic_case_data if a bare specimen number is not."
    ),
    lab_number = paste0(
      "lab_number is your laboratory's own specimen or culture number. ",
      "Unlike source_key it need not be unique - two rows may share one ",
      "when a single culture produced more than one reported result."
    ),
    patient_key = paste0(
      "patient_key is what deduplication and episode grouping key on. ",
      "Supply a pseudonymised identifier that is the same for the same ",
      "patient across results."
    ),
    sample_date = paste0(
      "sample_date is the anchor every detector, trend and report is ",
      "built against. If your system falls back to a receipt date when ",
      "it is unfilled, apply that fallback in your own extract step."
    ),
    pathogen = paste0(
      "pathogen is free text, used verbatim - fill it with the name your ",
      "laboratory reports, spelled the same way on every run."
    ),
    institution_key = paste0(
      "institution_key identifies the reporting institution; it is ",
      "hashed on the way in, so it never reaches the database as-is."
    ),
    institution_display_name = paste0(
      "institution_display_name is the name shown in the dashboard. Fall ",
      "back to the key itself rather than leaving it empty."
    ),
    institution_type = paste0(
      "institution_type decides how the institution is handled; use ",
      "\"other\" when none of the specific types fit."
    )
  )
  hint <- if (column %in% names(hints)) hints[[column]] else ""
  paste0(
    hint,
    if (nzchar(hint)) " " else "",
    "Rows are never dropped silently, so a run refuses to start rather ",
    "than load a case it cannot place."
  )
}

#' Columns your source system may well call something else
#'
#' Deliberately covers the Dutch names too: the extract step is usually
#' written against a Dutch laboratory system, and `afnamedatum` reaching
#' EpiSODIC as an unexpected column is a likelier mistake than a typo.
#' @keywords internal
#' @noRd
episodic_check_column_synonyms <- c(
  id = "source_key",
  sampleid = "source_key",
  specimenid = "source_key",
  isolateid = "source_key",
  labid = "lab_number",
  labnr = "lab_number",
  labnummer = "lab_number",
  labnumber = "lab_number",
  monsternummer = "lab_number",
  kweeknummer = "lab_number",
  culturenumber = "lab_number",
  accessionnumber = "lab_number",
  patientid = "patient_key",
  patient = "patient_key",
  patientnumber = "patient_key",
  patientnr = "patient_key",
  pseudoid = "patient_key",
  date = "sample_date",
  sampledate = "sample_date",
  samplingdate = "sample_date",
  specimendate = "sample_date",
  collectiondate = "sample_date",
  afnamedatum = "sample_date",
  datum = "sample_date",
  receiptdate = "receipt_date",
  ontvangstdatum = "receipt_date",
  organism = "pathogen",
  microorganism = "pathogen",
  mo = "pathogen",
  agent = "pathogen",
  micoorganism = "pathogen",
  verwekker = "pathogen",
  echelon = "care_line",
  lijn = "care_line",
  hospital = "institution_key",
  institution = "institution_key",
  institutionid = "institution_key",
  organisation = "institution_key",
  instelling = "institution_key",
  institutionname = "institution_display_name",
  hospitalname = "institution_display_name",
  gemeente = "municipality",
  city = "municipality",
  place = "municipality",
  department = "ward",
  afdeling = "ward",
  unit = "ward",
  specialty = "specialism",
  speciality = "specialism",
  specialisme = "specialism",
  postcode = "pc",
  postcodearea = "pc",
  pcarea = "pc",
  pc6 = "pc",
  postalcode = "pc",
  zip = "pc",
  zipcode = "pc",
  pc4 = "pc",
  gender = "sex",
  geslacht = "sex",
  ageyears = "age",
  leeftijd = "age"
)

#' @keywords internal
#' @noRd
episodic_check_column_guesses <- function(extra_cols) {
  guesses <- character(0)
  for (column in extra_cols) {
    key <- gsub("[^a-z0-9]", "", tolower(column))
    target <- if (key %in% names(episodic_check_column_synonyms)) {
      episodic_check_column_synonyms[[key]]
    } else {
      distances <- utils::adist(key, episodic_case_columns)[1, ]
      if (min(distances) <= 3) {
        episodic_case_columns[which.min(distances)]
      } else {
        NA_character_
      }
    }
    if (!is.na(target)) {
      guesses <- c(
        guesses,
        paste0("`", column, "` looks like `", target, "`")
      )
    }
  }
  if (length(guesses) == 0) {
    return("")
  }
  paste0(
    paste(guesses, collapse = "; "),
    if (length(guesses) == 1) " - rename it if so." else " - rename them if so."
  )
}

#' The pathogens the shipped configuration knows about, or NULL
#'
#' Read defensively: a missing or unreadable configuration file must cost
#' one piece of advice, never the whole report.
#' @keywords internal
#' @noRd
episodic_check_configured_pathogens <- function() {
  path <- system.file("config", "episodic_default_pathogen_config.csv", package = "EpiSODIC")
  if (identical(path, "")) {
    path <- file.path("inst", "config", "episodic_default_pathogen_config.csv")
  }
  if (!file.exists(path)) {
    return(NULL)
  }
  config <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(config) || !"pathogen" %in% names(config)) {
    return(NULL)
  }
  config$pathogen
}

#' @keywords internal
#' @noRd
episodic_check_rows_text <- function(rows, max_shown = 5) {
  if (length(rows) == 0) {
    return("")
  }
  shown <- utils::head(rows, max_shown)
  paste0(
    paste(shown, collapse = ", "),
    if (length(rows) > max_shown) {
      paste0(" (and ", length(rows) - max_shown, " more)")
    } else {
      ""
    }
  )
}

#' @keywords internal
#' @noRd
episodic_check_values_text <- function(values, max_shown = 5) {
  if (length(values) == 0) {
    return("")
  }
  distinct <- unique(episodic_check_chr(values))
  shown <- utils::head(distinct, max_shown)
  shown[is.na(shown)] <- "NA"
  paste0(
    paste(shown, collapse = ", "),
    if (length(distinct) > max_shown) {
      paste0(" (and ", length(distinct) - max_shown, " more)")
    } else {
      ""
    }
  )
}

#' Everything that must be fixed, as one error message
#'
#' Written to be read in a console, in a cron log, and in the dashboard's
#' activity screen - all three are places an operator meets a failed run.
#' @keywords internal
#' @noRd
episodic_check_failure_message <- function(problems, what = "Case data") {
  bullets <- vapply(
    seq_len(nrow(problems)),
    function(i) {
      paste0(
        "  ",
        i,
        ". ",
        problems$message[i],
        if (nzchar(problems$values[i])) {
          paste0(" Values: ", problems$values[i], ".")
        } else {
          ""
        },
        if (nzchar(problems$rows[i])) {
          paste0(" Rows: ", problems$rows[i], ".")
        } else {
          ""
        },
        if (!is.na(problems$fix[i])) {
          paste0("\n     Fix: ", problems$fix[i])
        } else {
          ""
        }
      )
    },
    character(1)
  )
  paste0(
    what,
    " cannot be used by EpiSODIC: ",
    nrow(problems),
    if (nrow(problems) == 1) " problem." else " problems.",
    "\n",
    paste(bullets, collapse = "\n"),
    "\n  Run episodic_check_cases(your_data) for the full report, ",
    "including the rows involved and what is merely worth a look. See ",
    episodic_check_case_data_link(),
    " for what each column means."
  )
}

#' A clickable `?episodic_case_data` reference for console output
#'
#' `cat()`/`stop()`/`message()` text is not markdown, so the only way to
#' get a link a terminal or the RStudio console can actually follow is
#' `cli`'s own hyperlink markup - a plain `"?episodic_case_data"` string
#' is just text to click nowhere. Degrades to that same plain text where
#' the output stream has no hyperlink support (a log file, `Rscript`),
#' since `cli::format_inline()` does that automatically.
#' @keywords internal
#' @noRd
episodic_check_case_data_link <- function() {
  cli::format_inline("{.help [?episodic_case_data](EpiSODIC::episodic_case_data)}")
}

#' @param x An `episodic_case_check` report, as returned by
#'   `episodic_check_cases()`.
#' @param ... Ignored, for compatibility with the print generic.
#' @export
#' @noRd
print.episodic_case_check <- function(x, ...) {
  info <- attr(x, "summary")
  width <- 76L
  wrap <- function(text, initial, prefix) {
    cat(
      paste(
        strwrap(text, width = width, initial = initial, prefix = prefix),
        collapse = "\n"
      ),
      "\n",
      sep = ""
    )
  }
  count <- function(n) formatC(n, format = "d", big.mark = ",")
  counted <- function(n, singular, plural) {
    paste0(count(n), " ", if (n == 1) singular else plural)
  }

  title <- attr(x, "title") %||% "EpiSODIC case data check"
  cat(
    "-- ",
    title,
    " ",
    strrep("-", max(0L, width - nchar(title) - 4L)),
    "\n",
    sep = ""
  )
  if (!is.null(info$n_rows)) {
    cat(
      "   ",
      counted(info$n_rows, "row", "rows"),
      ", ",
      counted(info$n_cols, "column", "columns"),
      "\n",
      sep = ""
    )
    if (!is.null(info$date_min)) {
      cat(
        "   sample_date from ",
        as.character(info$date_min),
        " to ",
        as.character(info$date_max),
        "\n",
        sep = ""
      )
    }
    parts <- c(
      if (!is.null(info$n_pathogens)) {
        counted(info$n_pathogens, "pathogen", "pathogens")
      },
      if (!is.null(info$n_institutions)) {
        counted(info$n_institutions, "institution", "institutions")
      },
      if (!is.null(info$n_patients)) {
        counted(info$n_patients, "patient", "patients")
      }
    )
    if (length(parts) > 0) {
      cat("   ", paste(parts, collapse = ", "), "\n", sep = "")
    }
  }
  cat("\n")

  show <- function(rows, header) {
    cat(header, "\n\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      # No column prefix: every message names its own column already,
      # and reads as a sentence an epidemiologist can act on.
      wrap(
        rows$message[i],
        initial = paste0("  ", i, ". "),
        prefix = "     "
      )
      if (nzchar(rows$values[i])) {
        wrap(
          rows$values[i],
          initial = "     values: ",
          prefix = "             "
        )
      }
      if (nzchar(rows$rows[i])) {
        wrap(
          rows$rows[i],
          initial = "     rows:   ",
          prefix = "             "
        )
      }
      if (!is.na(rows$fix[i])) {
        wrap(
          rows$fix[i],
          initial = "     fix:    ",
          prefix = "             "
        )
      }
      cat("\n")
    }
  }

  problems <- x[x$severity == "problem", , drop = FALSE]
  advice <- x[x$severity == "advice", , drop = FALSE]

  if (nrow(problems) > 0) {
    show(
      problems,
      paste0(
        "x ",
        nrow(problems),
        if (nrow(problems) == 1) " problem" else " problems",
        " - a detection run refuses to start until these are fixed:"
      )
    )
  }
  if (nrow(advice) > 0) {
    show(
      advice,
      paste0(
        "! ",
        nrow(advice),
        if (nrow(advice) == 1) " thing" else " things",
        " worth a look - a run proceeds regardless:"
      )
    )
  }
  what <- attr(x, "what") %||% "Case data"
  is_cases <- identical(what, "Case data")
  if (nrow(problems) == 0) {
    if (is_cases) {
      cat(
        "v This data set satisfies the case data contract, and is ready ",
        "for\n  episodic_run_cron(). See ",
        episodic_check_case_data_link(),
        " for what each\n  column means.\n",
        sep = ""
      )
    } else {
      cat(
        "v ",
        what,
        " satisfies its contract, and is ready for episodic_run_cron().\n",
        sep = ""
      )
    }
  } else {
    if (is_cases) {
      cat(
        "Nothing was changed here. Fix the problems above in your own ",
        "extract step,\nthen check again. See ",
        episodic_check_case_data_link(),
        " for what each column means\n  and which values it accepts.\n",
        sep = ""
      )
    } else {
      cat(
        "Nothing was changed here. Fix the problems above in your own ",
        "extract step,\nthen check again.\n",
        sep = ""
      )
    }
  }
  invisible(x)
}
