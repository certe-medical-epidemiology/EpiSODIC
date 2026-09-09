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

#' Re-Match a Validation Result at Different Thresholds
#'
#' `min_recall` and `min_precision` are choices, and a result that only
#' holds at one particular pair of them is not a result. This applies a
#' different pair to a replay that has already been run, so a sweep costs
#' arithmetic rather than another few hours of detection.
#'
#' It re-derives everything the thresholds touch and nothing else: which
#' outbreaks count as detected, when, how badly they fragmented, and
#' which clusters count as true positives. The clusters that were raised,
#' the runs that raised them and the cases they held are what they were -
#' those are measurements, and no threshold changes them.
#'
#' @param result An `episodic_validation` object, from
#'   [episodic_validate_detection()] or [episodic_validate_comparator()].
#' @param min_recall,min_precision The thresholds to apply instead.
#' @return An `episodic_validation` object of the same shape, with
#'   `meta$min_recall` and `meta$min_precision` updated and
#'   `meta$rethresholded_from` recording what it was re-matched from.
#' @seealso [episodic_validate_detection()]
#' @examples
#' \donttest{
#' result <- episodic_validate_detection()
#' strict <- episodic_validate_rethreshold(result, min_recall = 0.8)
#' strict$summary[strict$summary$metric == "sensitivity", ]
#' }
#' @export
episodic_validate_rethreshold <- function(result,
                                          min_recall = result$meta$min_recall,
                                          min_precision = result$meta$min_precision) {
  if (!inherits(result, "episodic_validation")) {
    stop(
      "`result` must come from episodic_validate_detection() or ",
      "episodic_validate_comparator().",
      call. = FALSE
    )
  }
  if (is.null(result$truth)) {
    stop(
      "This result carries no ground truth to re-match against. It was ",
      "produced by an older version of the harness; re-run it.",
      call. = FALSE
    )
  }
  episodic_validation_check_threshold(min_recall, "min_recall")
  episodic_validation_check_threshold(min_precision, "min_precision")

  seeds <- result$meta$seeds
  rematched <- lapply(seeds, function(seed) {
    mine <- function(x) x[x$seed == seed, , drop = FALSE]
    overlap <- mine(result$overlap)
    run_dates <- sort(unique(mine(result$runs)$run_date))
    clusters <- episodic_validation_cluster_rows(
      overlap[overlap$run_date == max(run_dates), , drop = FALSE],
      mine(result$clusters),
      min_precision = min_precision
    )
    outbreaks <- episodic_validation_outbreak_rows(
      overlap,
      mine(result$truth$outbreaks),
      mine(result$truth$cases),
      run_dates,
      min_recall = min_recall,
      min_precision = min_precision
    )
    outbreaks$seed <- rep(seed, nrow(outbreaks))
    list(outbreaks = outbreaks, clusters = clusters)
  })

  outbreaks <- do.call(rbind, lapply(rematched, function(x) x$outbreaks))
  clusters <- do.call(rbind, lapply(rematched, function(x) x$clusters))
  rownames(outbreaks) <- NULL
  rownames(clusters) <- NULL

  out <- result
  out$outbreaks <- outbreaks
  out$clusters <- clusters
  out$summary <- episodic_validation_summarise(outbreaks, clusters, result$runs)
  out$meta$rethresholded_from <- c(
    min_recall = result$meta$min_recall,
    min_precision = result$meta$min_precision
  )
  out$meta$min_recall <- min_recall
  out$meta$min_precision <- min_precision
  out
}
