#' Compute a deterministic `stream_key` for a stream definition
#'
#' `stream_key` identifies a monitored slice of the data (ARCHITECTURE.md
#' section 5.1) and must hash to the same value for the same slice across
#' runs and across hosts, regardless of insertion order. The detector is
#' deliberately not part of the key, since both detectors watch the same
#' stream.
#'
#' Algorithm (see `QUESTIONS.md` item 14, not specified by the
#' architecture beyond "deterministic hash of the defining dimensions"):
#' SHA-1 of the dimension values, coerced to character, `NA` mapped to the
#' literal string `"NA"`, joined with `"|"` in a fixed field order.
#'
#' @param level One of the five lattice levels, e.g. `"pathogen_ward"`.
#' @param mo_code The pathogen code (`AMR::as.mo()` output or the
#'   placeholder equivalent, see `R/mo_lookup.R`).
#' @param care_line One of `"first"`, `"second"`, `"other"`, `"unknown"`, or
#'   `NA` if care line does not apply at this level.
#' @param region_code The region/area code, or `NA`.
#' @param institution_id The institution id for hospital streams, or `NA`.
#' @param ward The ward, for L1 (`pathogen_ward`) streams, or `NA`. Not a
#'   dimension in `ARCHITECTURE.md` section 5.1's `episode_stream` DDL; see
#'   `QUESTIONS.md` item 20 for why it was added.
#' @return A 40-character lowercase hex SHA-1 digest.
#' @export
episode_stream_key <- function(level, mo_code, care_line = NA, region_code = NA,
                                institution_id = NA, ward = NA) {
  stopifnot(length(level) == 1, length(mo_code) == 1)
  fields <- vapply(
    list(level, mo_code, care_line, region_code, institution_id, ward),
    function(x) if (is.na(x)) "NA" else as.character(x),
    character(1)
  )
  digest::digest(paste(fields, collapse = "|"), algo = "sha1", serialize = FALSE)
}
