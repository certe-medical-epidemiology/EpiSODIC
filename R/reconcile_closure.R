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

#' Closure criterion
#'
#' Which criterion prompts closure differs by stream kind: a case-free
#' interval for most streams, two maximum incubation periods for a
#' confirmed epidemic, and the MEM post-epidemic threshold
#' (`episodic_mem_status()`) for seasonal streams flagged
#' `mem_applicable`.
#'
#' @param last_case_date The most recent case date in the cluster.
#' @param verdict The cluster's latest verdict, or `NA`.
#' @param case_free_days,incub_max_days From `episodic_pathogen_config`.
#' @param mem_applicable From `episodic_pathogen_config`.
#' @param mem_status `episodic_mem_status()`'s own output for this stream,
#'   or `NULL` if `mem_applicable` is `FALSE` or MEM could not be
#'   computed (no `mem` package, off-season, insufficient history) - in
#'   which case a `mem_applicable` stream never closes via this criterion
#'   at all, rather than falling back to case-free days (a seasonal
#'   pathogen's case-free interval is not epidemiologically meaningful:
#'   the whole point of MEM is that "has the epidemic started/ended" is a
#'   different question from "has it been quiet for N days").
#' @param today The date to evaluate the criterion as of.
#' @return `TRUE` if the closure criterion has fired.
#' @keywords internal
#' @noRd
episodic_closure_criterion_met <- function(last_case_date, verdict, case_free_days,
                                           incub_max_days = NA, mem_applicable = FALSE,
                                           mem_status = NULL, today = Sys.Date()) {
  if (isTRUE(mem_applicable)) {
    if (is.null(mem_status)) return(FALSE)
    return(isTRUE(mem_status$current_week_count <= mem_status$post_epidemic_threshold))
  }

  days_since <- as.integer(as.Date(today) - as.Date(last_case_date))

  threshold <- case_free_days
  if (!is.na(verdict) && verdict == "confirmed_epidemic" && !is.na(incub_max_days)) {
    threshold <- max(threshold, 2 * incub_max_days)
  }

  days_since >= threshold
}
