#' Closure criterion
#'
#' Which criterion prompts closure differs by stream kind (ARCHITECTURE.md
#' section 6.3): a case-free interval for most streams, two maximum
#' incubation periods for a confirmed epidemic, and the MEM post-epidemic
#' threshold for seasonal streams flagged `mem_applicable`
#' (ARCHITECTURE.md section 7.3, `episode_mem_status()`).
#'
#' @param last_case_date The most recent case date in the cluster.
#' @param verdict The cluster's latest verdict, or `NA`.
#' @param case_free_days,incub_max_days From `episode_pathogen_config`.
#' @param mem_applicable From `episode_pathogen_config`.
#' @param mem_status `episode_mem_status()`'s own output for this stream,
#'   or `NULL` if `mem_applicable` is `FALSE` or MEM could not be
#'   computed (no `mem` package, off-season, insufficient history) - in
#'   which case a `mem_applicable` stream never closes via this criterion
#'   at all, rather than falling back to case-free days (a seasonal
#'   pathogen's case-free interval is not epidemiologically meaningful:
#'   the whole point of MEM is that "has the epidemic started/ended" is a
#'   different question from "has it been quiet for N days").
#' @param today The date to evaluate the criterion as of.
#' @return `TRUE` if the closure criterion has fired.
#' @export
episode_closure_criterion_met <- function(last_case_date, verdict, case_free_days,
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
