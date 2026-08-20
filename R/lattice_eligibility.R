#' Eligibility gate
#'
#' The first layer of volume control (ARCHITECTURE.md section 8, item 1): a
#' stream needs sufficient baseline history, a minimum median weekly count,
#' and non-zero counts in a reasonable share of baseline weeks before
#' statistical detection is attempted on it. Streams failing this gate
#' fall through to EARS C2 or the rare-pathogen path instead
#' (`R/detect_*.R` handles those fall-through detectors); only the gate
#' itself lives here.
#'
#' The numeric thresholds are not specified anywhere in the architecture
#' beyond the three named criteria; see `QUESTIONS.md` item 16 for the
#' provisional defaults adopted and shipped in `inst/config/default.yaml`.
#'
#' @param cases_for_stream A data frame of cases belonging to one stream,
#'   with a `sample_date` column.
#' @param as_of The date detection is being run as of; baseline weeks are
#'   counted back from here.
#' @param config The resolved configuration (`episode_config_resolve()`);
#'   uses `config$eligibility`.
#' @return `TRUE` if the stream is eligible for statistical detection,
#'   `FALSE` otherwise.
#' @export
episode_eligibility_gate <- function(cases_for_stream, as_of, config) {
  gate <- config$eligibility

  dates <- as.Date(cases_for_stream$sample_date)
  baseline_start <- as.Date(as_of) - gate$min_baseline_weeks * 7

  if (length(dates) == 0 || min(dates) > baseline_start) {
    return(FALSE)
  }

  weeks <- seq(baseline_start, as.Date(as_of), by = "week")
  week_bin <- cut(dates[dates >= baseline_start & dates <= as.Date(as_of)], breaks = weeks, right = FALSE)
  weekly_counts <- table(week_bin)

  if (length(weekly_counts) == 0) return(FALSE)

  median_weekly <- stats::median(as.numeric(weekly_counts))
  nonzero_share <- mean(as.numeric(weekly_counts) > 0)

  median_weekly >= gate$min_median_weekly_count && nonzero_share >= gate$min_nonzero_week_share
}
