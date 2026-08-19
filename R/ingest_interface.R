#' Ingestion interface
#'
#' Defines the contract that any raw case data source must satisfy before
#' `episode_ingest()` (see `R/ingest_dedup.R`) can turn it into rows of
#' `episode_case` and `episode_reporting_triangle`. `certedb::get_diver_data()`
#' cannot be called in this environment; the only implementation shipped is
#' `episode_ingest_source_synthetic()` (see `R/ingest_synthetic.R`). A real
#' Diver-backed adapter is a future extension point that must return a data
#' frame with the same shape.
#'
#' The allow-listed columns below are exactly the fields ARCHITECTURE.md
#' sections 5.4 and 5.4.1 name. Nothing else may be requested from Diver
#' (standing brief, section 3: no patient data beyond this, no named
#' clinician fields, ever).
#' @name ingest_interface
NULL

#' @rdname ingest_interface
#' @export
episode_ingest_columns <- c(
  "source_key", "patient_key", "sample_date", "receipt_date", "mo_code",
  "determination", "material", "care_line", "institution_key",
  "institution_display_name", "institution_type", "municipality",
  "ward", "specialism", "pc4", "sex", "age"
)

#' Validate that a raw ingestion source data frame satisfies the interface
#'
#' @param raw A data frame as returned by an ingestion source function.
#' @return `raw`, invisibly, if valid. Errors otherwise.
#' @export
episode_ingest_validate_source <- function(raw) {
  missing_cols <- setdiff(episode_ingest_columns, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "Ingestion source is missing required column(s): ",
      paste(missing_cols, collapse = ", "), call. = FALSE
    )
  }
  extra_cols <- setdiff(names(raw), episode_ingest_columns)
  if (length(extra_cols) > 0) {
    stop(
      "Ingestion source returned column(s) outside the allow-list: ",
      paste(extra_cols, collapse = ", "),
      ". The ingestion interface is an explicit allow-list (ARCHITECTURE.md ",
      "section 5.4.1); a new upstream column must not leak in silently.",
      call. = FALSE
    )
  }
  if (any(duplicated(raw$source_key))) {
    stop("Ingestion source returned duplicate source_key values.", call. = FALSE)
  }
  invisible(raw)
}
