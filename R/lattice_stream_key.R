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

#' Compute a deterministic `stream_key` for a stream definition
#'
#' `stream_key` identifies a monitored slice of the data and must hash to
#' the same value for the same slice across runs and across hosts,
#' regardless of insertion order. The detector is deliberately not part
#' of the key, since both detectors watch the same stream.
#'
#' Algorithm: SHA-1 of the dimension values, coerced to character, `NA`
#' mapped to the literal string `"NA"`, joined with `"|"` in a fixed
#' field order.
#'
#' @param level One of the five lattice levels, e.g. `"pathogen_ward"`.
#' @param pathogen The raw lab-provided pathogen string, used as-is (no
#'   taxonomy resolution).
#' @param care_line One of `"first"`, `"second"`, `"third"`, `"other"`, `"unknown"`, or
#'   `NA` if care line does not apply at this level.
#' @param region_code The region/area code, or `NA`.
#' @param institution_id The institution id for hospital streams, or `NA`.
#' @param ward The ward, for L1 (`pathogen_ward`) streams, or `NA`.
#' @return A 40-character lowercase hex SHA-1 digest.
#' @keywords internal
#' @noRd
episodic_stream_key <- function(
  level,
  pathogen,
  care_line = NA,
  region_code = NA,
  institution_id = NA,
  ward = NA
) {
  stopifnot(length(level) == 1, length(pathogen) == 1)
  fields <- vapply(
    list(level, pathogen, care_line, region_code, institution_id, ward),
    function(x) if (is.na(x)) "NA" else as.character(x),
    character(1)
  )
  digest::digest(
    paste(fields, collapse = "|"),
    algo = "sha1",
    serialize = FALSE
  )
}
