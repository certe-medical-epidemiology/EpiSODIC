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

#' Ingestion interface
#'
#' Defines the contract that any raw case data source must satisfy before
#' `episodic_ingest_run()` (see `R/ingest_pipeline.R`) can turn it into rows
#' of `episodic_case`. This is the entire boundary between EpiSODIC and
#' whatever laboratory or hospital system an operator runs: EpiSODIC
#' never calls any data source itself (see `README.md`'s data format
#' section) - the operator's own cron script extracts and transforms
#' into exactly this shape, then calls [episodic_run_cron()] with a
#' function that returns it.
#' The only implementation shipped in this package is the synthetic
#' generator (`R/ingest_synthetic.R`), used for the bundled demo.
#'
#' **Mandatory, drives all detection: one row per confirmed-positive
#' isolate/result.** `pathogen` is a raw, lab-provided string, used
#' verbatim - never resolved against `AMR::as.mo()` or any other taxonomy,
#' since `AMR` only covers non-viral organisms and this system must detect
#' clusters of anything a lab reports. The same
#' underlying isolate can legitimately appear more than once under
#' different `pathogen` values when that is epidemiologically useful (an
#' ETEC isolate as both `"Escherichia coli"` and `"ETEC"`, so each is
#' watched on its own), and it is entirely the operator's transform step
#' that decides this, not EpiSODIC.
#'
#' **No `positive`/test-outcome column, and no raw negatives.** Positivity
#' is handled separately as small, optional, pre-aggregated metadata (see
#' `R/ingest_denominator.R`); the mandatory feed here is positives only.
#'
#' @name ingest_interface
NULL

#' @rdname ingest_interface
#' @examples
#' episodic_ingest_columns
#' @export
episodic_ingest_columns <- c(
  "source_key", "patient_key", "sample_date", "receipt_date", "pathogen",
  "care_line", "institution_key",
  "institution_display_name", "institution_type", "municipality",
  "ward", "specialism", "pc", "sex", "age"
)

#' Validate that a raw ingestion source data frame satisfies the interface
#'
#' @param raw A data frame as returned by an ingestion source function.
#' @return `raw`, invisibly, if valid. Errors otherwise.
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
