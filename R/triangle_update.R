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

#' Empirical completion curve for a stream
#'
#' The proportion of eventually-reported cases for a given sample date
#' that were already visible D days later, averaged across historical
#' sample dates. Used to shade the "incompleteness zone" on the epi
#' curve and to size the rolling detection window - its own empirical
#' completion curve decides how many trailing days are under-ascertained
#' by construction.
#'
#' Derived, not stored. This used to read `episodic_reporting_triangle`,
#' a table the cron rewrote in full on every run: one row per stream per
#' distinct sample date per run, so a nightly run added tens of thousands
#' of rows whether or not a single case had arrived, and the table grew
#' without bound. It was also redundant. `episodic_case.first_seen_run`
#' records the run that first saw each case and
#' `episodic_detection_run.run_date` dates every run, so "how many cases
#' with sample date D were visible at run R" is a `cumsum()` over data
#' the database already holds exactly. Deriving it is cheaper to write
#' (nothing), cheaper to store (nothing), and strictly more faithful than
#' a cache that could only ever record the runs that happened to fire.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id The stream to compute completeness for.
#' @param max_lag_days Maximum reporting lag (in days) to compute.
#' @return A data frame with columns `lag_days` and `completeness`
#'   (0-1, the median share of the final count visible at that lag).
#' @keywords internal
#' @noRd
episodic_triangle_completeness <- function(con, stream_id, max_lag_days = 21) {
  empty <- data.frame(lag_days = integer(0), completeness = numeric(0))

  cases <- episodic_db_cases_for_stream_id(
    con,
    stream_id,
    columns = c("sample_date", "first_seen_run")
  )
  if (nrow(cases) == 0) {
    return(empty)
  }
  # Only runs that committed. A failed run rolls its body back, so it
  # never made a case visible to anyone, and counting it would invent a
  # lag at which nothing had yet been reported.
  runs <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT run_id, run_date FROM episodic_detection_run
        WHERE status IN (%s) ORDER BY run_id",
      paste(rep("?", length(episodic_run_statuses_complete)), collapse = ", ")
    ),
    params = as.list(episodic_run_statuses_complete)
  )
  if (nrow(runs) == 0) {
    return(empty)
  }

  # n_cases visible at each run, per sample date: the running total of
  # cases first seen at or before that run. Built as a matrix rather than
  # a join so the whole curve costs two queries regardless of how many
  # runs and sample dates a stream has accumulated.
  sample_dates <- sort(unique(cases$sample_date))
  seen_at <- match(cases$first_seen_run, runs$run_id)
  keep <- !is.na(seen_at)
  if (!any(keep)) {
    return(empty)
  }
  # Cross-tabulated, not index-assigned: several cases routinely share a
  # sample date and the run that first saw them, and `m[idx] <- m[idx] + 1`
  # over repeated index pairs keeps only the last write.
  counts <- matrix(
    as.integer(table(
      factor(cases$sample_date[keep], levels = sample_dates),
      factor(seen_at[keep], levels = seq_len(nrow(runs)))
    )),
    nrow = length(sample_dates),
    ncol = nrow(runs)
  )
  # A case stays visible once it has appeared, so each row accumulates.
  # Done column by column rather than with t(apply(., 1, cumsum)), which
  # silently transposes when there is only one run and drops a dimension
  # when there is only one sample date.
  visible <- counts
  for (j in seq_len(ncol(counts))[-1]) {
    visible[, j] <- visible[, j - 1L] + counts[, j]
  }

  lag <- outer(
    as.numeric(as.Date(sample_dates)),
    as.numeric(as.Date(runs$run_date)),
    function(sd, rd) rd - sd
  )
  # The final count is what the stream ends up reporting for that sample
  # date, which is the last run's total - the same quantity the stored
  # triangle expressed as the maximum over its rows.
  final_n <- visible[, ncol(visible)]

  ok <- visible > 0 & lag >= 0 & lag <= max_lag_days & final_n > 0
  if (!any(ok)) {
    return(empty)
  }
  share <- (visible / final_n)[ok]
  by_lag <- stats::aggregate(
    share ~ lag_days,
    data.frame(lag_days = as.integer(lag[ok]), share = share),
    stats::median
  )
  names(by_lag) <- c("lag_days", "completeness")
  by_lag[order(by_lag$lag_days), ]
}
