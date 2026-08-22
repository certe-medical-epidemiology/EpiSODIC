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

#' Connect your own laboratory data
#'
#' EpiSODIC does not connect to any laboratory or hospital system itself.
#' Instead, you extract and transform your own positive-result data
#' beforehand - with whatever tooling you already use - and hand the result
#' to [episodic_run_cron()] as a plain data frame (a `tibble` is equally
#' fine) in the shape described here. This keeps EpiSODIC decoupled from
#' any specific laboratory information system: whatever your source system
#' is, your own extract step is the only place that needs to know about it.
#'
#' A data set is the normal case, and the one to reach for. If producing
#' the data only makes sense at run time - a live database query, for
#' instance - you can pass a zero-argument function returning such a data
#' set instead; EpiSODIC accepts either (see [episodic_resolve_data()]).
#'
#' Only confirmed-positive results belong here - there is no outcome
#' column, so do not include negative results. If you also want a
#' denominator (tests performed, for a positivity rate), supply that
#' separately as pre-aggregated counts; see [episodic_synthetic_denominators()].
#'
#' @section Required columns:
#'
#' `episodic_case_columns` lists all fifteen, in order. The set is an
#' explicit allow-list: a column outside it, or one missing from it, is
#' rejected by [episodic_validate_cases()] rather than silently ignored.
#' "Required" means the column must be present; several may be `NA`
#' throughout if your laboratory does not record them.
#'
#' Three columns accept only a fixed set of values, given as
#' `episodic_care_lines`, `episodic_institution_types` and
#' `episodic_sex_codes` so you can map onto them in your extract step
#' rather than copying the strings out of this page.
#'
#' \describe{
#'   \item{`source_key`}{Character, required, no `NA`, unique within the
#'     data set. A stable identifier for this result in your own source
#'     system, so re-running the same extract later cannot create
#'     duplicate cases. Any string will do, as long as the same result
#'     always carries the same one.}
#'   \item{`patient_key`}{Character, required, no `NA`. A pseudonymised
#'     patient identifier, the same for the same patient across results.
#'     Deduplication and episode grouping key on it: without it every
#'     isolate becomes its own case. Never shown as-is in the dashboard,
#'     but do pseudonymise it before it reaches EpiSODIC - a BSN or
#'     hospital number must not be passed through.}
#'   \item{`sample_date`}{Date, or character in `YYYY-MM-DD` (ISO 8601)
#'     form. Required, no `NA`. The date the sample was taken, and the
#'     anchor every detector, trend and report is built against. If your
#'     system falls back to a receipt date when the sample date is
#'     unfilled, apply that fallback in your own extract step, not here.}
#'   \item{`receipt_date`}{Date, or character in `YYYY-MM-DD` form; `NA`
#'     allowed. The date the laboratory received the sample. Stored for
#'     provenance only - deliberately never used to measure reporting
#'     delay, since a lab's receipt-date field can itself be standing in
#'     for a missing sample date.}
#'   \item{`pathogen`}{Character, required, no `NA`. The pathogen name
#'     exactly as your laboratory reports it (e.g. `"Escherichia coli"`,
#'     `"Influenza A"`, `"Norovirus"`). Free text, deliberately
#'     unconstrained: used verbatim and never matched against a taxonomy,
#'     since detection has to work for anything a lab can report, viral or
#'     not. Spelling must be stable across runs - `"Influenza A"` and
#'     `"influenza a"` are two different pathogens as far as detection is
#'     concerned. Names matching `inst/config/pathogen_config.csv` pick up
#'     that pathogen's episode length, incubation window and serial
#'     interval; anything else falls back to the schema defaults (30-day
#'     episode, 14 case-free days, no Rt, no MEM). The same isolate may
#'     appear under more than one `pathogen` value where that is
#'     epidemiologically useful - an ETEC isolate reported as both
#'     `"Escherichia coli"` and `"ETEC"`, so each is monitored separately.}
#'   \item{`care_line`}{Character; `NA` allowed. One of `"first"`
#'     (primary care), `"second"` (secondary care), `"other"`, or
#'     `"unknown"` - the values in `episodic_care_lines`. Anything else is
#'     rejected. `NA` is read as `"unknown"` and stored that way, so you
#'     need not map missing values yourself: an empty `care_line`, an
#'     R `NA` and a database `NULL` all mean the same thing here, and the
#'     dashboard shows all three as "unknown".}
#'   \item{`institution_key`}{Character, required, no `NA`. A stable
#'     identifier for the reporting institution. Hashed (SHA-1) on the way
#'     in, so a later rename does not fracture the institution's history,
#'     and so the raw key never reaches the database.}
#'   \item{`institution_display_name`}{Character, required, no `NA`. The
#'     human-readable institution name shown in the dashboard.}
#'   \item{`institution_type`}{Character, required. Exactly one of
#'     `"hospital"`, `"ltc_institution"` (long-term care),
#'     `"gp_municipality"`, `"ooh_service"` (out-of-hours service), or
#'     `"other"` - the values in `episodic_institution_types`. Anything
#'     else is rejected. This decides how the institution is handled:
#'     `"hospital"` institutions
#'     are monitored as first-class entities and are the only ones
#'     eligible for patient-day normalisation, while a `"gp_municipality"`
#'     is collapsed to its municipality.}
#'   \item{`municipality`}{Character; `NA` allowed. The institution's
#'     municipality. Required in practice for `"gp_municipality"` rows,
#'     since that is what they are collapsed to.}
#'   \item{`ward`}{Character; `NA` allowed. The ward within the
#'     institution. Only meaningful for `"hospital"` and
#'     `"ltc_institution"` rows; leave `NA` otherwise. This is the unit L1
#'     (ward-level) detection watches, so an inconsistent spelling splits
#'     one ward into two streams.}
#'   \item{`specialism`}{Character; `NA` allowed. The requesting clinical
#'     specialism. Context for the assessor, not a detection input.}
#'   \item{`pc`}{Character; `NA` allowed. The *patient's* postcode area,
#'     not the institution's - it is what the geography panel and
#'     area-level (L3) detection use. Four digits as a string for the
#'     shipped Netherlands reference data (`"9713"`, leading zeros
#'     preserved, so store it as character and not as a number). With your
#'     own `EPISODIC_GEO_DATA`, it must match that file's `pc` column
#'     instead.}
#'   \item{`sex`}{Character; `NA` allowed. Exactly one of `"M"`, `"F"`, or
#'     `"U"` (unknown) when present - the values in `episodic_sex_codes`.
#'     Anything else is rejected, so map your own coding (`1`/`2`,
#'     `"male"`/`"female"`) in your extract step.}
#'   \item{`age`}{Integer; `NA` allowed. Age in whole years at sampling.
#'     Not age group - the dashboard bands it itself.}
#' }
#'
#' The only case data shipped with the package is what the synthetic
#' generator ([episodic_synthetic_cases()]) returns for the bundled
#' demo - a useful template for the shape your own data should have.
#'
#' @name episodic_case_data
NULL

#' @rdname episodic_case_data
#' @examples
#' episodic_case_columns
#' episodic_care_lines
#' episodic_institution_types
#' @export
episodic_case_columns <- c(
  "source_key", "patient_key", "sample_date", "receipt_date", "pathogen",
  "care_line", "institution_key",
  "institution_display_name", "institution_type", "municipality",
  "ward", "specialism", "pc", "sex", "age"
)

#' @rdname episodic_case_data
#' @export
episodic_care_lines <- c("first", "second", "other", "unknown")

#' @rdname episodic_case_data
#' @export
episodic_institution_types <- c(
  "hospital", "ltc_institution", "gp_municipality", "ooh_service", "other"
)

#' @rdname episodic_case_data
#' @export
episodic_sex_codes <- c("M", "F", "U")

#' Columns that must be present and must never be `NA`
#' @keywords internal
#' @noRd
episodic_case_columns_required <- c(
  "source_key", "patient_key", "sample_date", "pathogen",
  "institution_key", "institution_display_name", "institution_type"
)

#' Check that your case data has the right shape
#'
#' Call this on the data set you intend to hand to [episodic_run_cron()],
#' while preparing it, to get a clear error while you can still fix the
#' extract - rather than halfway through a scheduled run, where the same
#' problem surfaces as a rolled-back transaction and a failed run.
#'
#' It checks everything [episodic_case_data] documents: that the columns
#' are exactly [episodic_case_columns], that `source_key` is unique, that
#' the columns which may never be `NA` are filled, that `care_line`,
#' `institution_type` and `sex` hold only allowed values, that the two
#' date columns parse, and that `age` is numeric. It reports what your
#' data says, and does not change it: the `NA` that [episodic_run_cron()]
#' will read as `"unknown"` is left as `NA` in what comes back.
#'
#' @param cases Your case data: a data frame or `tibble` in the shape
#'   [episodic_case_data] describes, or a zero-argument function
#'   returning one (resolved with [episodic_resolve_data()] first, so you
#'   can check either form of `cases`).
#' @return The validated data set, invisibly. Throws an informative error
#'   naming the offending column, and the offending values, otherwise.
#' @examples
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
#' )
#' episodic_validate_cases(cases)
#' @export
episodic_validate_cases <- function(cases) {
  cases <- episodic_resolve_data(cases)
  episodic_validate_columns(
    cases,
    required = episodic_case_columns,
    filled = episodic_case_columns_required,
    what = "Case data"
  )

  extra_cols <- setdiff(names(cases), episodic_case_columns)
  if (length(extra_cols) > 0) {
    stop(
      "Case data contains column(s) outside the allow-list: ",
      paste(extra_cols, collapse = ", "),
      ". The case data interface is an explicit allow-list; a new ",
      "upstream column must not leak in silently.",
      call. = FALSE
    )
  }
  if (any(duplicated(cases$source_key))) {
    stop("Case data contains duplicate source_key values.", call. = FALSE)
  }

  episodic_validate_allowed(cases, "care_line", episodic_care_lines, na_ok = TRUE)
  episodic_validate_allowed(cases, "institution_type", episodic_institution_types, na_ok = FALSE)
  episodic_validate_allowed(cases, "sex", episodic_sex_codes, na_ok = TRUE)

  episodic_validate_dates(cases, "sample_date", na_ok = FALSE)
  episodic_validate_dates(cases, "receipt_date", na_ok = TRUE)

  if (!all(is.na(cases$age)) && !is.numeric(cases$age)) {
    stop(
      "Case data has a non-numeric `age` (", paste(class(cases$age), collapse = "/"),
      "). Give age in whole years, not as an age group.", call. = FALSE
    )
  }

  invisible(cases)
}

#' Reject a missing column, or an NA where the schema allows none
#'
#' Shared by all three feeds. `required` names the columns that must be
#' present; `filled` those that must additionally never be `NA`.
#' @keywords internal
#' @noRd
episodic_validate_columns <- function(data, required, filled, what) {
  if (!is.data.frame(data)) {
    stop(what, " must be a data frame (or tibble), not ",
         paste(class(data), collapse = "/"), ".", call. = FALSE)
  }
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(what, " is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  for (column in filled) {
    if (any(is.na(data[[column]]))) {
      stop(
        what, " has NA in `", column, "`, which must always be filled ",
        "(", sum(is.na(data[[column]])), " of ", nrow(data), " rows).",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Reject values outside a fixed set, naming the ones that offended
#'
#' Shared by all three feeds, so a bad `care_line` reads the same whether
#' it arrived on a case, a denominator or an activity row.
#' @keywords internal
#' @noRd
episodic_validate_allowed <- function(data, column, allowed, na_ok, what = "Case data") {
  values <- data[[column]]
  bad <- if (isTRUE(na_ok)) values[!is.na(values) & !values %in% allowed] else values[!values %in% allowed]
  if (length(bad) > 0) {
    stop(
      what, " has ", length(bad), " row(s) with a `", column,
      "` outside the allowed values (", paste(allowed, collapse = ", "),
      if (isTRUE(na_ok)) ", or NA" else "", "): ",
      paste(unique(bad), collapse = ", "), ".", call. = FALSE
    )
  }
  invisible(NULL)
}

#' Reject dates that will not parse, naming the ones that offended
#' @keywords internal
#' @noRd
episodic_validate_dates <- function(data, column, na_ok, what = "Case data") {
  values <- data[[column]]
  if (inherits(values, "Date")) return(invisible(NULL))
  parsed <- suppressWarnings(as.Date(as.character(values), format = "%Y-%m-%d"))
  bad <- values[is.na(parsed) & !(is.na(values) & isTRUE(na_ok))]
  if (length(bad) > 0) {
    shown <- unique(bad)
    if (length(shown) > 5) shown <- shown[1:5]
    stop(
      what, " has ", length(bad), " row(s) whose `", column,
      "` is not a Date and does not read as YYYY-MM-DD: ",
      paste(shown, collapse = ", "), ".", call. = FALSE
    )
  }
  invisible(NULL)
}
