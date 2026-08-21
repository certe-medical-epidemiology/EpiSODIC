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
#' Instead, you write a small R function - an "ingestion source" - that
#' returns your own positive-result data as a plain data frame in the shape
#' described here, and pass that function to [episodic_run_cron()]. This
#' keeps EpiSODIC decoupled from any specific laboratory information system:
#' whatever your source system is, your function is the only place that
#' needs to know about it.
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
#' Only confirmed-positive results belong in this feed - there is no
#' outcome column, so do not include negative results here. If you also
#' want a denominator (tests performed, for a positivity rate), supply
#' that separately as pre-aggregated counts; see `R/ingest_denominator.R`.
#'
#' The only ingestion source shipped with the package is the synthetic
#' generator ([episodic_ingest_source_synthetic()]) used for the bundled
#' demo - a useful template to base your own on.
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

#' Check that your ingestion source has the right shape
#'
#' Call this on the data frame your own ingestion source function returns,
#' while developing it, to get a clear error if a column is missing,
#' misnamed, or duplicated - before handing it to [episodic_run_cron()].
#' See [episodic_ingest_columns] for the required columns.
#'
#' @param raw A data frame, as returned by your ingestion source function.
#' @return `raw`, invisibly, if it is valid. Throws an informative error
#'   otherwise (a missing required column, an unexpected extra column, or
#'   duplicate `source_key` values).
#' @examples
#' raw <- episodic_ingest_source_synthetic(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
#' )
#' episodic_ingest_validate_source(raw)
#' @export
episodic_ingest_validate_source <- function(raw) {
  missing_cols <- setdiff(episodic_ingest_columns, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "Ingestion source is missing required column(s): ",
      paste(missing_cols, collapse = ", "), call. = FALSE
    )
  }
  extra_cols <- setdiff(names(raw), episodic_ingest_columns)
  if (length(extra_cols) > 0) {
    stop(
      "Ingestion source returned column(s) outside the allow-list: ",
      paste(extra_cols, collapse = ", "),
      ". The ingestion interface is an explicit allow-list; a new ",
      "upstream column must not leak in silently.",
      call. = FALSE
    )
  }
  if (any(duplicated(raw$source_key))) {
    stop("Ingestion source returned duplicate source_key values.", call. = FALSE)
  }
  invisible(raw)
}
