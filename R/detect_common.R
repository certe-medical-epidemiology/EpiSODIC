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

#' Common detection record
#'
#' Every detector, whatever its source, produces the same shape of record
#' so that reconciliation (`R/reconcile_*.R`) never needs to know which
#' detector fired. This matches the columns of `episodic_detection`.
#'
#' @param stream_id The stream this detection belongs to.
#' @param detector One of `'clusters'`, `'farrington'`, `'ears'`, `'mem'`,
#'   `'rare_trigger'`, `'same_place'`.
#' @param first_day,last_day The interval the detection covers.
#' @param n_cases Case count in the interval.
#' @param expected,upperbound Statistical detector output; `NA` for
#'   detectors without a baseline model (e.g. `same_place`).
#' @param params A list of detector-specific attributes, stored as JSON.
#' @return A one-row data frame in the shape reconciliation expects.
#' @keywords internal
#' @noRd
episodic_detection_record <- function(stream_id,
                                      detector,
                                      first_day,
                                      last_day,
                                      n_cases,
                                      expected = NA_real_,
                                      upperbound = NA_real_,
                                      params = list()) {
  if (length(n_cases) == 0) {
    return(data.frame(
      stream_id = integer(0),
      detector = character(0),
      first_day = character(0),
      last_day = character(0),
      n_cases = integer(0),
      expected = numeric(0),
      upperbound = numeric(0),
      params = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    stream_id = stream_id,
    detector = detector,
    first_day = as.character(first_day),
    last_day = as.character(last_day),
    n_cases = as.integer(n_cases),
    expected = as.numeric(expected),
    upperbound = as.numeric(upperbound),
    params = as.character(jsonlite::toJSON(params, auto_unbox = TRUE)),
    stringsAsFactors = FALSE
  )
}
