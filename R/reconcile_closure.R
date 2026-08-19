#' Closure criterion
#'
#' Which criterion prompts closure differs by stream kind (ARCHITECTURE.md
#' section 6.3): a case-free interval for most streams, two maximum
#' incubation periods for a confirmed epidemic, and the MEM post-epidemic
#' threshold for seasonal streams flagged `mem_applicable`. MEM is M5 scope
#' (ARCHITECTURE.md section 7.3, MILESTONES.md M5); this function
#' implements the case-free/incubation-period branch only and always
#' returns `FALSE` for `mem_applicable` streams, since M1 has no MEM
#' computation to evaluate against.
#'
#' @param last_case_date The most recent case date in the cluster.
#' @param verdict The cluster's latest verdict, or `NA`.
#' @param case_free_days,incub_max_days From `episode_pathogen_config`.
#' @param mem_applicable From `episode_pathogen_config`; MEM-based closure
#'   is not evaluated in M1, see above.
#' @param today The date to evaluate the criterion as of.
#' @return `TRUE` if the closure criterion has fired.
#' @export
episode_closure_criterion_met <- function(last_case_date, verdict, case_free_days,
                                           incub_max_days = NA, mem_applicable = FALSE,
                                           today = Sys.Date()) {
  if (isTRUE(mem_applicable)) {
    return(FALSE)  # M5 scope, see roxygen note above
  }

  days_since <- as.integer(as.Date(today) - as.Date(last_case_date))

  threshold <- case_free_days
  if (!is.na(verdict) && verdict == "confirmed_epidemic" && !is.na(incub_max_days)) {
    threshold <- max(threshold, 2 * incub_max_days)
  }

  days_since >= threshold
}
