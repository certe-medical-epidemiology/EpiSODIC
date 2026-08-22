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
#' set instead; EpiSODIC accepts either (see [episodic_resolve_source()]).
#'
#' `episodic_ingest_columns` lists the required columns, in order:
#' \describe{
#'   \item{`source_key`}{A unique identifier for this result, stable across
#'     repeated runs (used to detect duplicates).}
#'   \item{`patient_key`}{A pseudonymised patient identifier, consistent
#'     for the same patient across results.}
#'   \item{`sample_date`, `receipt_date`}{Dates the sample was taken and
#'     received by the laboratory.}
#'   \item{`pathogen`}{The pathogen name, exactly as your laboratory
#'     reports it (e.g. `"Escherichia coli"`, `"Influenza A"`). Used
#'     verbatim - never matched against a taxonomy - since detection must
#'     work for any pathogen a lab can report, viral or not. The same
#'     isolate may appear under more than one `pathogen` value if that is
#'     epidemiologically useful (e.g. an ETEC isolate reported as both
#'     `"Escherichia coli"` and `"ETEC"`, so each is monitored separately).}
#'   \item{`care_line`}{Typically `"hospital"` or `"primary_care"`.}
#'   \item{`institution_key`, `institution_display_name`, `institution_type`}{
#'     The reporting institution's identifier, display name, and type.}
#'   \item{`municipality`, `ward`, `specialism`, `pc`}{Further location and
#'     clinical context, where available.}
#'   \item{`sex`, `age`}{Patient demographics, where available.}
#' }
#'
#' Only confirmed-positive results belong in this data set - there is no
#' outcome column, so do not include negative results here. If you also
#' want a denominator (tests performed, for a positivity rate), supply
#' that separately as pre-aggregated counts; see `R/ingest_denominator.R`.
#'
#' The only case data shipped with the package is what the synthetic
#' generator ([episodic_ingest_source_synthetic()]) returns for the bundled
#' demo - a useful template for the shape your own data should have.
#'
#' @name episodic_ingest_interface
NULL

#' @rdname episodic_ingest_interface
#' @examples
#' episodic_ingest_columns
#' @export
episodic_ingest_columns <- c(
  "source_key", "patient_key", "sample_date", "receipt_date", "pathogen",
  "care_line", "institution_key",
  "institution_display_name", "institution_type", "municipality",
  "ward", "specialism", "pc", "sex", "age"
)

#' Check that your case data has the right shape
#'
#' Call this on the data set you intend to hand to [episodic_run_cron()],
#' while preparing it, to get a clear error if a column is missing,
#' misnamed, or duplicated - before a scheduled run finds out for you.
#' See [episodic_ingest_columns] for the required columns.
#'
#' @param raw Your case data: a data frame or `tibble` in the shape
#'   [episodic_ingest_columns] describes, or a zero-argument function
#'   returning one (resolved with [episodic_resolve_source()] first, so you
#'   can check either form of `ingest_source`).
#' @return The validated data set, invisibly. Throws an informative error
#'   otherwise (a missing required column, an unexpected extra column, or
#'   duplicate `source_key` values).
#' @examples
#' raw <- episodic_ingest_source_synthetic(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
#' )
#' episodic_ingest_validate_source(raw)
#' @export
episodic_ingest_validate_source <- function(raw) {
  raw <- episodic_resolve_source(raw)
  if (!is.data.frame(raw)) {
    stop(
      "Case data must be a data frame (or tibble), not ",
      paste(class(raw), collapse = "/"), ".", call. = FALSE
    )
  }
  missing_cols <- setdiff(episodic_ingest_columns, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "Case data is missing required column(s): ",
      paste(missing_cols, collapse = ", "), call. = FALSE
    )
  }
  extra_cols <- setdiff(names(raw), episodic_ingest_columns)
  if (length(extra_cols) > 0) {
    stop(
      "Case data contains column(s) outside the allow-list: ",
      paste(extra_cols, collapse = ", "),
      ". The ingestion interface is an explicit allow-list; a new ",
      "upstream column must not leak in silently.",
      call. = FALSE
    )
  }
  if (any(duplicated(raw$source_key))) {
    stop("Case data contains duplicate source_key values.", call. = FALSE)
  }
  invisible(raw)
}
