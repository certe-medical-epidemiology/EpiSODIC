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

#' `mem` seasonal detector
#'
#' The Moving Epidemic Method (`mem` package) supplies pre-epidemic and
#' post-epidemic thresholds from historical seasons. Eligibility is
#' derived from each stream's own data: the season anchor, the seasonal
#' concentration statistic, and the minimum history are all computed
#' rather than configured per pathogen.
#'
#' Whether MEM fits a stream at all is governed by `mem_mode` on the
#' pathogen configuration: `'auto'` (shipped default) fits only when the
#' stream's own data show a season, `'yes'` fits whenever the data
#' physically allow it regardless of the seasonality statistic, and
#' `'no'` never fits. `config$mem$levels` controls which lattice levels
#' MEM runs at (shipped: `pathogen_province` and `pathogen_region`).
#'
#' @param cases_for_stream A data frame of a single stream's cases,
#'   with `sample_date`.
#' @param stream_id The stream these cases belong to.
#' @param run_date The date to treat as "today".
#' @param config The resolved configuration; uses `config$mem`.
#' @param mem_mode One of `"auto"`, `"yes"`, `"no"`.
#' @param stream_label Optional label for refusal log messages.
#' @param mem_fit The function that fits MEM to a matrix of completed
#'   seasons; see `episodic_mem_status()`. By default `episodic_mem_fit()`
#'   with the configured `mem.typical_curve`.
#' @return A data frame of detection records (zero or one row).
#' @references
#' Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R,
#' Nunes B (2013). "Influenza Surveillance in Europe: Establishing Epidemic
#' Thresholds by the Moving Epidemic Method." *Influenza and Other
#' Respiratory Viruses*, 7(4), 546-558.
#' \doi{10.1111/j.1750-2659.2012.00422.x} (the Moving Epidemic Method,
#' implemented by the `mem` package and called directly here).
#' @keywords internal
#' @noRd
episodic_detect_mem <- function(cases_for_stream,
                                stream_id,
                                run_date = Sys.Date(),
                                config = episodic_config_resolve(),
                                mem_mode = "auto",
                                stream_label = NULL,
                                mem_fit = function(historical) {
                                  episodic_mem_fit(
                                    historical,
                                    typical_curve = episodic_mem_typical_curve(config)
                                  )
                                }) {
  empty <- episodic_detection_none()

  if (!episodic_detector_enabled(config, "mem")) {
    return(empty)
  }
  if (!requireNamespace("mem", quietly = TRUE)) {
    return(empty)
  }
  if (is.null(cases_for_stream) || nrow(cases_for_stream) == 0) {
    return(empty)
  }

  tag <- stream_label %||% stream_id

  if (identical(mem_mode, "no")) {
    episodic_trace(
      "MEM: declined stream ", tag, ", disabled by mem_mode",
      severity = "warn"
    )
    return(empty)
  }

  anchor <- episodic_mem_season_anchor(cases_for_stream, config)
  if (is.null(anchor)) {
    episodic_trace(
      "MEM: declined stream ", tag, ", insufficient history for climatology",
      severity = "warn"
    )
    return(empty)
  }

  seasonality_stat <- NA_real_
  if (identical(mem_mode, "auto")) {
    seas <- episodic_mem_seasonality(cases_for_stream, config)
    if (is.null(seas)) {
      episodic_trace(
        "MEM: declined stream ", tag,
        ", insufficient history for seasonality test",
        severity = "warn"
      )
      return(empty)
    }
    if (!isTRUE(seas$seasonal)) {
      episodic_trace(
        "MEM: declined stream ", tag,
        ", not seasonal (statistic = ", round(seas$statistic, 3), ")",
        severity = "warn"
      )
      return(empty)
    }
    seasonality_stat <- seas$statistic
  }

  status <- episodic_mem_status(
    cases_for_stream,
    run_date,
    config,
    anchor,
    fit = mem_fit
  )
  if (is.null(status) || !isTRUE(status$epidemic_started)) {
    return(empty)
  }

  intensity <- status$intensity_thresholds
  if (is.null(intensity)) {
    episodic_trace(
      "MEM: stream ", tag, " has no intensity bands, ",
      status$intensity_unavailable %||% "for a reason the fit did not give",
      severity = "warn"
    )
  }
  typical <- status$typical
  if (is.null(typical)) {
    episodic_trace(
      "MEM: stream ", tag, " has no typical season to compare with, ",
      status$typical_unavailable %||% "for a reason the fit did not give",
      severity = "warn"
    )
  }
  episodic_detection_record(
    stream_id = stream_id,
    detector = "mem",
    first_day = as.character(status$week_start),
    last_day = as.character(status$week_end),
    n_cases = status$current_week_count,
    expected = NA_real_,
    upperbound = status$pre_epidemic_threshold,
    params = list(
      anchor_week = anchor$anchor_week,
      seasonality_statistic = seasonality_stat,
      seasons_used = paste(status$seasons_used, collapse = ", "),
      season = status$season,
      post_epidemic_threshold = status$post_epidemic_threshold,
      intensity_medium = if (!is.null(intensity)) intensity[["medium"]] else NA,
      intensity_high = if (!is.null(intensity)) intensity[["high"]] else NA,
      intensity_very_high = if (!is.null(intensity)) intensity[["very_high"]] else NA,
      typical_statistic = if (!is.null(typical)) typical$statistic else NA,
      typical_start = if (!is.null(typical)) typical$start else NA,
      typical_length = if (!is.null(typical)) typical$length else NA,
      typical_length_lower = if (!is.null(typical)) typical$length_lower else NA,
      typical_length_upper = if (!is.null(typical)) typical$length_upper else NA,
      typical_lower = if (!is.null(typical)) I(typical$lower) else NA,
      typical_middle = if (!is.null(typical)) I(typical$middle) else NA,
      typical_upper = if (!is.null(typical)) I(typical$upper) else NA
    )
  )
}

# -- Anchor derivation ------------------------------------------------

#' Derive the season anchor from a stream's own case history
#'
#' Builds a 52-week climatology (median count per ISO week pooled across
#' complete years), smooths it circularly, and places the anchor at the
#' first week of the longest contiguous trough run.
#'
#' @param cases A data frame with `sample_date`.
#' @param config The resolved configuration. Reads
#'   `config$mem$min_climatology_years`,
#'   `config$mem$climatology_smooth_weeks` and
#'   `config$mem$trough_percentile`.
#' @return A list with `anchor_week` (integer 1-52), `trough_run`
#'   (integer vector of ISO week numbers in the trough), and
#'   `climatology` (numeric length-52, the smoothed profile, indexed by
#'   ISO week 1 to 52). `NULL` when the case history spans fewer than
#'   `min_climatology_years` complete years.
#' @keywords internal
#' @noRd
episodic_mem_season_anchor <- function(cases,
                                       config = episodic_config_resolve()) {
  days <- episodic_mem_day_counts(cases)
  if (is.null(days)) {
    return(NULL)
  }

  min_years <- as.integer(config$mem$min_climatology_years %||% 3L)
  smooth_weeks <- as.integer(config$mem$climatology_smooth_weeks %||% 5L)
  trough_pctl <- as.numeric(config$mem$trough_percentile %||% 0.25)

  complete_years <- episodic_mem_complete_years(days)
  if (length(complete_years) < min_years) {
    return(NULL)
  }

  mat <- episodic_mem_year_week_counts(days, complete_years)
  climatology <- vapply(1:52, function(w) {
    stats::median(mat[w, ])
  }, numeric(1))

  half_w <- smooth_weeks %/% 2L
  smoothed <- vapply(1:52, function(i) {
    indices <- ((i - 1L - half_w):(i - 1L + half_w)) %% 52L + 1L
    mean(climatology[indices])
  }, numeric(1))

  threshold <- stats::quantile(smoothed, probs = trough_pctl, names = FALSE)
  is_trough <- smoothed <= threshold

  runs <- episodic_mem_circular_runs(which(is_trough), 52L)
  if (length(runs) == 0) {
    return(NULL)
  }

  best <- NULL
  for (run in runs) {
    if (is.null(best) ||
      length(run) > length(best) ||
      (length(run) == length(best) &&
        min(smoothed[run]) < min(smoothed[best]))) {
      best <- run
    }
  }

  list(
    anchor_week = as.integer(best[1]),
    trough_run = as.integer(best),
    climatology = smoothed
  )
}

#' Find contiguous circular runs in a set of positions
#'
#' @param positions Integer vector of positions (1..n) that qualify.
#' @param n The cycle length (52 for ISO weeks).
#' @return A list of integer vectors, each a contiguous circular run.
#' @keywords internal
#' @noRd
episodic_mem_circular_runs <- function(positions, n) {
  if (length(positions) == 0) {
    return(list())
  }
  positions <- sort(unique(as.integer(positions)))
  if (length(positions) == n) {
    return(list(positions))
  }

  runs <- list()
  current_run <- positions[1]

  for (i in seq_along(positions)[-1]) {
    if (positions[i] == positions[i - 1L] + 1L) {
      current_run <- c(current_run, positions[i])
    } else {
      runs <- c(runs, list(current_run))
      current_run <- positions[i]
    }
  }
  runs <- c(runs, list(current_run))

  if (length(runs) > 1L) {
    last <- runs[[length(runs)]]
    first <- runs[[1]]
    if (last[length(last)] == n && first[1] == 1L) {
      runs <- c(list(c(last, first)), runs[-c(1, length(runs))])
    }
  }

  runs
}

#' A stream's cases as counts per distinct sample date
#'
#' Everything the anchor and the seasonality statistic compute is a sum
#' over cases of something that depends on the case's date alone - its
#' ISO year, its ISO week, whether that year is complete - so it can be
#' computed once per distinct date and weighted by how many cases fell on
#' it. Five years of history is under two thousand dates however many
#' cases a region holds, and formatting a date's ISO week is the costly
#' step.
#'
#' @param cases A data frame with `sample_date`.
#' @return `NULL` when no case has a readable date, otherwise a list with
#'   `date` (sorted distinct `Date`s), `n` (integer cases per date),
#'   `iso_year` and `iso_week` (integer, the week capped at 52, as the
#'   climatology and the season matrix fold week 53 into week 52).
#' @keywords internal
#' @noRd
episodic_mem_day_counts <- function(cases) {
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(NULL)
  }
  date <- sort(unique(dates))
  list(
    date = date,
    n = tabulate(match(dates, date), nbins = length(date)),
    iso_year = as.integer(format(date, "%G")),
    iso_week = pmin(as.integer(format(date, "%V")), 52L)
  )
}

#' The ISO years a stream's case history spans end to end
#'
#' @param days `episodic_mem_day_counts()`'s output.
#' @return The sorted ISO years whose week 1 Monday is on or after the
#'   first case and whose week 52 Sunday is on or before the last.
#' @keywords internal
#' @noRd
episodic_mem_complete_years <- function(days) {
  data_start <- days$date[1]
  data_end <- days$date[length(days$date)]
  all_years <- sort(unique(days$iso_year))
  complete <- vapply(all_years, function(yr) {
    data_start <= episodic_iso_week_start(yr, 1L) &&
      data_end >= episodic_iso_week_start(yr, 52L) + 6L
  }, logical(1))
  all_years[complete]
}

#' Case counts per ISO week (rows 1-52) and ISO year (columns)
#'
#' @param days `episodic_mem_day_counts()`'s output.
#' @param years The ISO years to tabulate, in column order.
#' @return An integer matrix, 52 rows by `length(years)` columns named by
#'   year, zero where no case fell.
#' @keywords internal
#' @noRd
episodic_mem_year_week_counts <- function(days, years) {
  mat <- matrix(
    0L,
    nrow = 52L,
    ncol = length(years),
    dimnames = list(NULL, as.character(years))
  )
  column <- match(days$iso_year, years)
  keep <- !is.na(column)
  if (!any(keep)) {
    return(mat)
  }
  cell <- days$iso_week[keep] + 52L * (column[keep] - 1L)
  summed <- rowsum(days$n[keep], cell, reorder = FALSE)
  mat[as.integer(rownames(summed))] <- as.integer(summed[, 1])
  mat
}

# -- Seasonality derivation -------------------------------------------

#' Derive whether a stream's pathogen shows a season
#'
#' Detrends the weekly series (divides by a centred 52-week moving
#' average) and measures the share of each year's detrended activity
#' falling in the peak `seasonality_peak_weeks` consecutive weeks,
#' averaged across complete years. A flat series with a strong upward
#' trend scores near `peak_weeks / 52` after detrending; a genuinely
#' seasonal series scores well above it.
#'
#' @param cases A data frame with `sample_date`.
#' @param config The resolved configuration. Reads
#'   `config$mem$min_climatology_years`,
#'   `config$mem$seasonality_peak_weeks` and
#'   `config$mem$seasonality_min_peak_share`.
#' @return A list with `statistic` (the averaged peak share) and
#'   `seasonal` (logical). `NULL` when the case history spans fewer
#'   than `min_climatology_years` complete years.
#' @keywords internal
#' @noRd
episodic_mem_seasonality <- function(cases,
                                     config = episodic_config_resolve()) {
  days <- episodic_mem_day_counts(cases)
  if (is.null(days)) {
    return(NULL)
  }

  min_years <- as.integer(config$mem$min_climatology_years %||% 3L)
  peak_weeks <- as.integer(config$mem$seasonality_peak_weeks %||% 8L)
  min_share <- as.numeric(config$mem$seasonality_min_peak_share %||% 0.40)

  complete_years <- episodic_mem_complete_years(days)
  if (length(complete_years) < min_years) {
    return(NULL)
  }

  n_years <- length(complete_years)
  mat <- episodic_mem_year_week_counts(days, complete_years)

  ts_values <- as.numeric(as.vector(mat))
  n_total <- length(ts_values)

  # Centred 52-week moving average for detrending
  half <- 26L
  detrended <- rep(NA_real_, n_total)
  for (i in seq_len(n_total)) {
    lo <- max(1L, i - half)
    hi <- min(n_total, i + half - 1L)
    ma <- mean(ts_values[lo:hi])
    detrended[i] <- if (ma > 0) ts_values[i] / ma else NA_real_
  }

  detrended_mat <- matrix(detrended, nrow = 52L, ncol = n_years)

  shares <- vapply(seq_len(n_years), function(j) {
    ratios <- detrended_mat[, j]
    ratios[is.na(ratios)] <- 0
    total <- sum(ratios)
    if (total <= 0) {
      return(NA_real_)
    }
    extended <- c(ratios, ratios[seq_len(peak_weeks - 1L)])
    best_sum <- -Inf
    for (start in seq_len(52L)) {
      s <- sum(extended[start:(start + peak_weeks - 1L)])
      if (s > best_sum) best_sum <- s
    }
    best_sum / total
  }, numeric(1))

  shares <- shares[!is.na(shares)]
  if (length(shares) == 0) {
    return(NULL)
  }

  statistic <- mean(shares)
  list(
    statistic = statistic,
    seasonal = statistic >= min_share
  )
}

# -- MEM status --------------------------------------------------------

#' Compute this stream's current MEM status
#'
#' Used by `episodic_detect_mem()` (fires on `epidemic_started`) and by
#' `episodic_epidemic_closure()` (fires when the evaluated count has
#' fallen back at or below `post_epidemic_threshold`, or at or below the
#' pre-epidemic threshold in a week inside the pathogen's derived
#' trough).
#'
#' Which week to evaluate: the last *complete* epidemiological week,
#' never the one in progress. The week containing the run date is
#' partial by construction, on a Tuesday it holds two days of cases,
#' and its count is too low by the same direction every time. Evaluating
#' last week costs a week of latency and buys a count that means what
#' the threshold means.
#'
#' Which seasons may serve as history: only seasons the case data spans
#' end to end. A season column exists as soon as one case falls in it,
#' so a site whose data begins mid-season carries a season that is
#' structural zeros for its first half; fed to `mem::memmodel()` it
#' drags the threshold down.
#'
#' @param cases A data frame with `sample_date`, all of a stream's
#'   known cases.
#' @param run_date The date to treat as "today".
#' @param config The resolved configuration.
#' @param anchor The result of `episodic_mem_season_anchor()`: a list
#'   with `anchor_week`, `trough_run` and `climatology`.
#' @param fit The function that fits MEM to the matrix of completed
#'   seasons, `episodic_mem_fit()` or one returning exactly what it would
#'   for the same matrix (`episodic_mem_fit_cached()`).
#' @return `NULL` when MEM cannot compute (no `mem` package, no data,
#'   or too few fully-observed prior seasons). Otherwise a list:
#'   `in_trough` (logical), `epidemic_started` (logical),
#'   `current_week_count`, `pre_epidemic_threshold`,
#'   `post_epidemic_threshold`, `intensity_thresholds`,
#'   `intensity_unavailable` (`NULL`, or why `intensity_thresholds` is),
#'   `intensity_level`, `typical` and `typical_unavailable` (as
#'   `episodic_mem_fit()` returns them), `anchor_week`, `season`,
#'   `seasons_used`, `week_start`, `week_end`.
#' @keywords internal
#' @noRd
episodic_mem_status <- function(cases,
                                run_date = Sys.Date(),
                                config = episodic_config_resolve(),
                                anchor = NULL,
                                fit = function(historical) {
                                  episodic_mem_fit(
                                    historical,
                                    typical_curve = episodic_mem_typical_curve(config)
                                  )
                                }) {
  if (is.null(anchor)) {
    return(NULL)
  }
  min_seasons <- as.integer(config$mem$min_seasons %||% 2L)
  anchor_week <- anchor$anchor_week
  evaluated <- episodic_mem_evaluation_week(run_date, anchor_week)

  if (!requireNamespace("mem", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(cases) || nrow(cases) == 0) {
    return(NULL)
  }

  built <- episodic_mem_seasonal_matrix(cases, anchor_week)
  if (is.null(built) || is.null(built$matrix)) {
    return(NULL)
  }

  prior_seasons <- setdiff(colnames(built$matrix), evaluated$season)
  prior_seasons <- episodic_mem_observed_seasons(
    prior_seasons, cases, anchor_week
  )
  if (length(prior_seasons) < min_seasons) {
    return(NULL)
  }

  historical <- built$matrix[, prior_seasons, drop = FALSE]
  fitted <- fit(historical)
  if (is.null(fitted)) {
    return(NULL)
  }

  pre_threshold <- fitted$pre_epidemic
  post_threshold <- fitted$post_epidemic
  intensity <- fitted$intensity

  week_label <- evaluated$week_label
  if (!week_label %in% rownames(built$matrix)) {
    return(NULL)
  }
  current_count <- if (evaluated$season %in% colnames(built$matrix)) {
    built$matrix[week_label, evaluated$season]
  } else {
    0L
  }

  eval_iso_week <- as.integer(week_label)
  in_trough <- eval_iso_week %in% anchor$trough_run

  list(
    in_trough = in_trough,
    epidemic_started = isTRUE(current_count > pre_threshold),
    current_week_count = as.integer(current_count),
    pre_epidemic_threshold = as.numeric(pre_threshold),
    post_epidemic_threshold = as.numeric(post_threshold),
    intensity_thresholds = intensity,
    intensity_unavailable = fitted$intensity_unavailable,
    intensity_level = episodic_mem_intensity_level(
      current_count,
      pre_threshold,
      intensity
    ),
    typical = fitted$typical,
    typical_unavailable = fitted$typical_unavailable,
    anchor_week = anchor_week,
    season = evaluated$season,
    seasons_used = prior_seasons,
    week_start = evaluated$week_start,
    week_end = evaluated$week_start + 6
  )
}

#' The statistics MEM's typical curve can summarise past seasons by
#'
#' Each name is a value of `mem.typical_curve`; each value the
#' `i.type.curve` of `mem::memmodel()` it selects, with the interval that
#' goes with it: the median with the Hettmansperger-Sheather/Nyblom
#' interval (normal approximation for few seasons), and the geometric and
#' arithmetic means with their confidence intervals. The median with a
#' bootstrap interval (`i.type.curve = 4`) is not offered: it draws at
#' random, so the same seasons would give a different curve on each fit
#' and the fit cache could not reuse one; it takes some thirty seconds a
#' fit; and it fails outright on two seasons.
#' @keywords internal
#' @noRd
episodic_mem_typical_curves <- c(median = 3L, geometric_mean = 2L, arithmetic_mean = 1L)

#' The typical-curve statistic of a run, checked
#'
#' @param config The resolved configuration; reads `mem.typical_curve`,
#'   with the shipped default for a hand-built configuration without it.
#' @return One of `names(episodic_mem_typical_curves)`.
#' @keywords internal
#' @noRd
episodic_mem_typical_curve <- function(config) {
  value <- config$mem$typical_curve
  if (is.null(value)) {
    value <- episodic_config_resolve_files(NA)$mem$typical_curve
  }
  if (
    !is.character(value) || length(value) != 1 ||
      !value %in% names(episodic_mem_typical_curves)
  ) {
    stop(
      "`mem.typical_curve` must be one of ",
      paste0("\"", names(episodic_mem_typical_curves), "\"", collapse = ", "),
      ", not ",
      paste(format(value), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  value
}

#' Fit MEM to a matrix of completed seasons
#'
#' The one place `mem::memmodel()` is called, reduced to the three things
#' anything downstream reads from its result. `episodic_mem_status()`
#' takes it as an argument so the cron can hand it
#' `episodic_mem_fit_cached()` instead, which returns what this would
#' have returned without fitting again when the matrix is unchanged.
#'
#' @param historical A weeks x seasons integer matrix, as built by
#'   `episodic_mem_seasonal_matrix()` and restricted to the seasons the
#'   thresholds are for.
#' @param typical_curve The statistic the typical curve summarises the
#'   past seasons by, one of `names(episodic_mem_typical_curves)`: it
#'   sets `i.type.curve` and nothing else, so the thresholds are the same
#'   whichever it is. The typical start and length are always medians
#'   (`i.type.other = 3`).
#' @return `NULL` when the model cannot be fitted, otherwise a list with
#'   `pre_epidemic` and `post_epidemic` (numeric), `intensity` (the
#'   thresholds from `episodic_mem_intensity_thresholds()`, possibly
#'   `NULL`), `intensity_unavailable` (`NULL`, or why `intensity` is),
#'   `typical` (the typical season from `episodic_mem_typical_season()`,
#'   possibly `NULL`) and `typical_unavailable` (`NULL`, or why `typical`
#'   is).
#' @keywords internal
#' @noRd
episodic_mem_fit <- function(historical, typical_curve = "median") {
  # Checked before the fit, whose errors are caught: an unknown statistic
  # is a configuration error, not a season MEM could not fit.
  curve_type <- episodic_mem_typical_curves[typical_curve]
  if (length(typical_curve) != 1 || is.na(curve_type)) {
    stop(
      "Unknown typical curve statistic \"",
      paste(typical_curve, collapse = ", "),
      "\"; one of ",
      paste0("\"", names(episodic_mem_typical_curves), "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  fit <- tryCatch(
    suppressWarnings(suppressMessages(mem::memmodel(
      as.data.frame(historical),
      i.type.curve = unname(curve_type),
      i.type.other = 3L,
      i.mem.info = FALSE
    ))),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }
  intensity <- episodic_mem_intensity_thresholds(fit)
  typical <- episodic_mem_typical_season(fit, nrow(historical), typical_curve)
  # Unnamed: `mem` names these after its own interval rows, and nothing
  # downstream reads the name - every consumer takes `as.numeric()` of
  # them - so the cache has no name to carry.
  list(
    pre_epidemic = unname(fit$pre.post.intervals["pre.i", 3]),
    post_epidemic = unname(fit$pre.post.intervals["post.i", 3]),
    intensity = intensity$thresholds,
    intensity_unavailable = intensity$unavailable,
    typical = typical$typical,
    typical_unavailable = typical$unavailable
  )
}

#' MEM's typical season: its curve, epidemic start and epidemic length
#'
#' `mem::memmodel()` aligns the past seasons on their epidemic periods
#' and summarises them as `typ.curve`, one row per week of the season in
#' the matrix's row order and three columns: the lower limit, the
#' typical value and the upper limit of its interval (`i.level.curve`,
#' shipped as 95%), by the statistic `i.type.curve` selects. The epidemic
#' period of that curve begins at the season week `mean.start` and lasts
#' `mean.length` weeks. Despite their names these are, with
#' `i.type.other = 3` as `episodic_mem_fit()` sets it, the rounded
#' median start and the median length of the past seasons' epidemics;
#' `ci.length` holds the interval of that length (`i.level.other`,
#' shipped as 95%) in its first row.
#'
#' It describes past seasons, not this one: it is what this season is
#' compared against, never a forecast of it. Anything but a finite curve
#' of the matrix's length, a start inside it and a finite positive
#' length is refused rather than repaired, as
#' `episodic_mem_intensity_thresholds()` refuses its thresholds.
#'
#' @param fit A `mem::memmodel()` result.
#' @param n_weeks The number of rows of the matrix it was fitted on.
#' @param statistic The statistic the curve was fitted with, one of
#'   `names(episodic_mem_typical_curves)`, carried with it.
#' @return A list with `typical`, `NULL` or a list of `statistic`,
#'   `start` (integer,
#'   the season week the typical epidemic starts at, counted from 1),
#'   `length`, `length_lower` and `length_upper` (numeric, in weeks) and
#'   `lower`, `middle` and `upper` (numeric of length `n_weeks`, the
#'   curve), and `unavailable`, `NULL` when `typical` is set and otherwise
#'   a sentence saying why not.
#' @keywords internal
#' @noRd
episodic_mem_typical_season <- function(fit, n_weeks, statistic = "median") {
  refuse <- function(why) list(typical = NULL, unavailable = why)
  curve <- tryCatch(fit$typ.curve, error = function(e) NULL)
  if (
    !is.matrix(curve) || !is.numeric(curve) || ncol(curve) != 3 ||
      nrow(curve) != n_weeks
  ) {
    return(refuse(sprintf(
      "mem::memmodel() returned a typical curve that is not %d weeks by 3 columns",
      as.integer(n_weeks)
    )))
  }
  if (!all(is.finite(curve))) {
    return(refuse("mem::memmodel() returned a typical curve with values that are not finite"))
  }
  start <- suppressWarnings(as.numeric(tryCatch(fit$mean.start, error = function(e) NULL)))
  if (
    length(start) != 1 || !is.finite(start) || start != round(start) ||
      start < 1 || start > n_weeks
  ) {
    return(refuse("mem::memmodel() returned a mean epidemic start that is not a week of the season"))
  }
  length_mean <- suppressWarnings(as.numeric(tryCatch(fit$mean.length, error = function(e) NULL)))
  length_ci <- suppressWarnings(as.numeric(tryCatch(fit$ci.length[1, c(1, 3)], error = function(e) NULL)))
  if (
    length(length_mean) != 1 || !is.finite(length_mean) || length_mean <= 0 ||
      length(length_ci) != 2 || !all(is.finite(length_ci))
  ) {
    return(refuse("mem::memmodel() returned an epidemic length that is not a finite positive number of weeks"))
  }
  list(
    typical = list(
      statistic = statistic,
      start = as.integer(start),
      length = length_mean,
      length_lower = length_ci[1],
      length_upper = length_ci[2],
      lower = unname(as.numeric(curve[, 1])),
      middle = unname(as.numeric(curve[, 2])),
      upper = unname(as.numeric(curve[, 3]))
    ),
    unavailable = NULL
  )
}

#' A MEM fit that reuses the stored one when its input is identical
#'
#' The completed seasons MEM is fitted on are the same from one run to
#' the next unless something about them changed: a late case in a past
#' season, a verdict, a configuration value, a new season completing.
#' Rather than guessing which of those happened, this hashes exactly what
#' `episodic_mem_fit()` would be given - the matrix, with its week and
#' season labels - together with the `mem` configuration, the pathogen's
#' `mem_mode` and the versions of `mem` and EpiSODIC, and reuses the
#' stored result only when that hash is the one it was stored under. A
#' reused result is therefore the result a fresh fit returns by
#' construction, with no assumption about which dates changed. Anything
#' else fits afresh and replaces the stored row
#' (`episodic_db_detector_cache_put()`).
#'
#' A fit `mem::memmodel()` refuses is not stored: nothing downstream
#' reads a failure, and it is fitted again next run.
#'
#' @param con A [DBI::DBIConnection-class], inside the run's transaction.
#' @param stream_id The stream the fit belongs to.
#' @param run_id The current run.
#' @param config The resolved configuration.
#' @param mem_mode The pathogen's `mem_mode`.
#' @param cached This stream's row of `episodic_db_detector_cache()`, or
#'   `NULL` when it has none.
#' @param tally An environment with integer `reused` and `fitted`, counted
#'   up here so the run can say how many fits it was spared.
#' @return A function of the historical matrix, returning what
#'   `episodic_mem_fit()` returns for it.
#' @keywords internal
#' @noRd
episodic_mem_fit_cached <- function(con,
                                    stream_id,
                                    run_id,
                                    config,
                                    mem_mode,
                                    cached = NULL,
                                    tally = NULL) {
  function(historical) {
    input_hash <- episodic_mem_fit_input_hash(historical, config, mem_mode)
    if (!is.null(cached) && identical(cached$input_hash, input_hash)) {
      decoded <- episodic_mem_fit_decode(cached$result)
      if (!is.null(decoded)) {
        if (!is.null(tally)) tally$reused <- tally$reused + 1L
        return(decoded)
      }
      episodic_trace(
        "MEM: the cached fit for stream ",
        stream_id,
        " could not be read back and is fitted again",
        severity = "warn"
      )
    }
    fitted <- episodic_mem_fit(
      historical,
      typical_curve = episodic_mem_typical_curve(config)
    )
    if (!is.null(tally)) tally$fitted <- tally$fitted + 1L
    if (!is.null(fitted)) {
      episodic_db_detector_cache_put(
        con,
        stream_id = stream_id,
        detector = "mem",
        input_hash = input_hash,
        result = episodic_mem_fit_encode(fitted),
        run_id = run_id
      )
    }
    fitted
  }
}

#' The hash a MEM fit is stored under
#'
#' @param historical The weeks x seasons matrix `episodic_mem_fit()` is
#'   given.
#' @param config The resolved configuration; `config$mem` enters the hash.
#' @param mem_mode The pathogen's `mem_mode`.
#' @return A SHA-1 hex string.
#' @keywords internal
#' @noRd
episodic_mem_fit_input_hash <- function(historical, config, mem_mode) {
  payload <- list(
    weeks = as.character(rownames(historical)),
    seasons = as.character(colnames(historical)),
    counts = as.integer(historical),
    mem = episodic_config_canonicalise(config$mem),
    mem_mode = as.character(mem_mode),
    # The shape of what is stored under the hash: a change to it is a
    # change of input, so rows of the previous shape are fitted again
    # rather than read back as a fit with fields missing.
    cache_format = 3L,
    mem_version = as.character(utils::packageVersion("mem")),
    episodic_version = as.character(utils::packageVersion("EpiSODIC"))
  )
  digest::digest(
    as.character(jsonlite::toJSON(
      payload,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = I(17)
    )),
    algo = "sha1",
    serialize = FALSE
  )
}

#' Encode a MEM fit for the cache, losslessly
#'
#' Each number is written in hexadecimal floating point (`sprintf("%a")`),
#' which `as.numeric()` reads back to the identical double, `NaN`, `NA`
#' and infinities included. A decimal rendering would round, and a
#' threshold read back one unit in the last place away from the one
#' fitted is a threshold a count can fall on the other side of.
#'
#' @param fitted `episodic_mem_fit()`'s output, not `NULL`.
#' @return A single JSON string.
#' @keywords internal
#' @noRd
episodic_mem_fit_encode <- function(fitted) {
  hex <- function(x) sprintf("%a", as.numeric(x))
  typical <- fitted$typical
  as.character(jsonlite::toJSON(
    list(
      pre_epidemic = hex(fitted$pre_epidemic),
      post_epidemic = hex(fitted$post_epidemic),
      intensity = if (is.null(fitted$intensity)) NULL else hex(fitted$intensity),
      intensity_unavailable = fitted$intensity_unavailable,
      typical = if (is.null(typical)) {
        NULL
      } else {
        list(
          statistic = typical$statistic,
          start = typical$start,
          length = hex(typical$length),
          length_lower = hex(typical$length_lower),
          length_upper = hex(typical$length_upper),
          lower = I(hex(typical$lower)),
          middle = I(hex(typical$middle)),
          upper = I(hex(typical$upper))
        )
      },
      typical_unavailable = fitted$typical_unavailable
    ),
    auto_unbox = TRUE,
    null = "null"
  ))
}

#' Decode a cached MEM fit
#'
#' @param result The stored string, from `episodic_mem_fit_encode()`.
#' @return `episodic_mem_fit()`'s output shape, or `NULL` when `result`
#'   is not a string that encoding produces.
#' @keywords internal
#' @noRd
episodic_mem_fit_decode <- function(result) {
  parsed <- tryCatch(
    jsonlite::fromJSON(as.character(result), simplifyVector = TRUE),
    error = function(e) NULL
  )
  scalar <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
  typical <- if (is.list(parsed)) parsed$typical else NULL
  typical_ok <- is.null(typical) || (
    is.list(typical) &&
      scalar(typical$statistic) &&
      typical$statistic %in% names(episodic_mem_typical_curves) &&
      is.numeric(typical$start) && length(typical$start) == 1 &&
      scalar(typical$length) && scalar(typical$length_lower) &&
      scalar(typical$length_upper) &&
      all(vapply(
        typical[c("lower", "middle", "upper")],
        function(x) is.character(x) && length(x) > 0,
        logical(1)
      )) &&
      length(unique(lengths(typical[c("lower", "middle", "upper")]))) == 1
  )
  # Exactly one of the two intensity fields is set: thresholds, or the
  # reason there are none; and likewise of the two typical-season
  # fields. A row carrying neither was written without the reason, and
  # is fitted again rather than read as a result that silently went
  # missing.
  if (
    !is.list(parsed) ||
      !scalar(parsed$pre_epidemic) ||
      !scalar(parsed$post_epidemic) ||
      !(is.null(parsed$intensity) ||
        (is.character(parsed$intensity) && length(parsed$intensity) == 3)) ||
      !(is.null(parsed$intensity_unavailable) ||
        scalar(parsed$intensity_unavailable)) ||
      is.null(parsed$intensity) == is.null(parsed$intensity_unavailable) ||
      !typical_ok ||
      !(is.null(parsed$typical_unavailable) ||
        scalar(parsed$typical_unavailable)) ||
      is.null(typical) == is.null(parsed$typical_unavailable)
  ) {
    return(NULL)
  }
  number <- function(x) suppressWarnings(as.numeric(x))
  list(
    pre_epidemic = number(parsed$pre_epidemic),
    post_epidemic = number(parsed$post_epidemic),
    intensity = if (is.null(parsed$intensity)) {
      NULL
    } else {
      stats::setNames(
        number(parsed$intensity),
        c("medium", "high", "very_high")
      )
    },
    intensity_unavailable = parsed$intensity_unavailable,
    typical = if (is.null(typical)) {
      NULL
    } else {
      list(
        statistic = typical$statistic,
        start = as.integer(typical$start),
        length = number(typical$length),
        length_lower = number(typical$length_lower),
        length_upper = number(typical$length_upper),
        lower = number(typical$lower),
        middle = number(typical$middle),
        upper = number(typical$upper)
      )
    },
    typical_unavailable = parsed$typical_unavailable
  )
}

#' MEM's medium/high/very high intensity thresholds
#'
#' `mem::memmodel()` returns them as `intensity.thresholds`: the upper
#' limits of its three intensity intervals (`i.level.intensity`, shipped
#' as 40%, 90% and 97.5%), in that order. `epidemic.thresholds` is a
#' different vector - the pre- and post-epidemic thresholds - and holds
#' two values, never three.
#'
#' Anything but three finite, non-decreasing values is refused rather
#' than repaired: sorting or trimming a vector of another shape would
#' produce bands that look like MEM's and are not. A refusal leaves the
#' intensity bands unavailable and says so; the pre- and post-epidemic
#' thresholds, which are read separately, are unaffected.
#'
#' @param fit A `mem::memmodel()` result.
#' @return A list with `thresholds`, a named numeric of length 3
#'   (`medium`, `high`, `very_high`) or `NULL`, and `unavailable`, `NULL`
#'   when `thresholds` is set and otherwise a sentence saying why not.
#' @keywords internal
#' @noRd
episodic_mem_intensity_thresholds <- function(fit) {
  raw <- tryCatch(fit$intensity.thresholds, error = function(e) NULL)
  values <- suppressWarnings(as.numeric(raw))
  if (length(values) != 3 || !all(is.finite(values))) {
    return(list(
      thresholds = NULL,
      unavailable = sprintf(
        "mem::memmodel() returned %d intensity threshold(s) rather than three finite values",
        length(raw)
      )
    ))
  }
  if (is.unsorted(values)) {
    return(list(
      thresholds = NULL,
      unavailable = "mem::memmodel() returned intensity thresholds that do not increase"
    ))
  }
  list(
    thresholds = stats::setNames(values, c("medium", "high", "very_high")),
    unavailable = NULL
  )
}

#' Which MEM intensity band a weekly count falls in
#'
#' @param count The evaluated week's case count.
#' @param pre_threshold The pre-epidemic threshold.
#' @param intensity `episodic_mem_intensity_thresholds()`'s output, or
#'   `NULL`.
#' @return One of `"baseline"`, `"low"`, `"medium"`, `"high"`,
#'   `"very_high"`, or `NA_character_` when intensity thresholds are
#'   unavailable.
#' @keywords internal
#' @noRd
episodic_mem_intensity_level <- function(count, pre_threshold, intensity) {
  if (is.null(intensity) || is.na(count)) {
    return(NA_character_)
  }
  if (!is.na(pre_threshold) && count <= pre_threshold) {
    return("baseline")
  }
  if (count >= intensity[["very_high"]]) {
    return("very_high")
  }
  if (count >= intensity[["high"]]) {
    return("high")
  }
  if (count >= intensity[["medium"]]) {
    return("medium")
  }
  "low"
}

# -- Season assignment -------------------------------------------------

#' The last fully-elapsed epidemiological week before the run date
#'
#' @param run_date The date to treat as "today".
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return `episodic_mem_season_week()`'s output for that week.
#' @keywords internal
#' @noRd
episodic_mem_evaluation_week <- function(run_date, anchor_week) {
  today <- as.Date(run_date)
  this_monday <- today - (as.integer(format(today, "%u")) - 1L)
  episodic_mem_season_week(this_monday - 7, anchor_week)
}

#' Keep only the seasons the case data spans end to end
#'
#' @param seasons Season labels (`"YYYY/YYYY"`).
#' @param cases A data frame with `sample_date`; its full date span
#'   defines the observation window.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return The subset of `seasons` falling entirely inside that window.
#' @keywords internal
#' @noRd
episodic_mem_observed_seasons <- function(seasons, cases, anchor_week) {
  if (length(seasons) == 0) {
    return(seasons)
  }
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(character(0))
  }
  observed_from <- min(dates)
  observed_to <- max(dates)

  keep <- vapply(
    seasons,
    function(season) {
      bounds <- episodic_mem_season_bounds(season, anchor_week)
      if (is.null(bounds)) {
        return(FALSE)
      }
      observed_from <= bounds$start && observed_to >= bounds$end
    },
    logical(1)
  )
  seasons[keep]
}

#' The calendar span of a surveillance season
#'
#' A season labelled `"Y1/Y2"` starts at `anchor_week` of year Y1 and
#' ends the day before `anchor_week` of year Y2.
#'
#' @param season A season label, `"YYYY/YYYY"`.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return A list with `start` and `end` (`Date`), or `NULL` if
#'   `season` is not a well-formed label.
#' @keywords internal
#' @noRd
episodic_mem_season_bounds <- function(season, anchor_week) {
  years <- suppressWarnings(as.integer(strsplit(
    as.character(season),
    "/",
    fixed = TRUE
  )[[1]]))
  if (length(years) != 2 || anyNA(years)) {
    return(NULL)
  }
  anchor_week <- as.integer(anchor_week)
  list(
    start = episodic_iso_week_start(years[1], anchor_week),
    end = episodic_iso_week_start(years[2], anchor_week) - 1L
  )
}

#' The season a date belongs to
#'
#' Every week of the year belongs to a season; there is no off-season.
#'
#' @param date A single `Date`.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return A season label, `"YYYY/YYYY"`.
#' @keywords internal
#' @noRd
episodic_season_containing <- function(date, anchor_week) {
  episodic_mem_season_week(date, anchor_week)$season
}

#' Step a season label back by `n` seasons
#'
#' @param season A season label, `"YYYY/YYYY"`.
#' @param n How many seasons back.
#' @return A season label, or `NULL` if `season` is malformed.
#' @keywords internal
#' @noRd
episodic_season_shift <- function(season, n = 1L) {
  years <- suppressWarnings(as.integer(strsplit(
    as.character(season),
    "/",
    fixed = TRUE
  )[[1]]))
  if (length(years) != 2 || anyNA(years)) {
    return(NULL)
  }
  sprintf("%d/%d", years[1] - as.integer(n), years[2] - as.integer(n))
}

#' MEM thresholds for one season, fitted only on the seasons before it
#'
#' `episodic_mem_status()` answers "where are we right now"; this
#' answers "what were the thresholds for the season being looked at",
#' which is what the Pathogen screen draws a season's weekly curve
#' against.
#'
#' The season itself is excluded from its own fit. Including it would be
#' look-ahead: a severe season would raise the very thresholds used to
#' call it severe.
#'
#' @param cases A data frame with `sample_date`.
#' @param season The season label the thresholds are for.
#' @param config The resolved configuration.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return A list with `pre_epidemic`, `post_epidemic`, `intensity`,
#'   `intensity_unavailable` and `seasons_used`, or `NULL` when `mem` is
#'   unavailable or too little
#'   earlier history exists.
#' @keywords internal
#' @noRd
episodic_mem_thresholds_for_season <- function(cases,
                                               season,
                                               config = episodic_config_resolve(),
                                               anchor_week = NULL) {
  if (is.null(anchor_week)) {
    return(NULL)
  }
  min_seasons <- as.integer(config$mem$min_seasons %||% 2L)
  if (!requireNamespace("mem", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(cases) || nrow(cases) == 0) {
    return(NULL)
  }

  built <- episodic_mem_seasonal_matrix(cases, anchor_week)
  if (is.null(built) || is.null(built$matrix)) {
    return(NULL)
  }

  earlier <- colnames(built$matrix)[
    colnames(built$matrix) < as.character(season)
  ]
  earlier <- episodic_mem_observed_seasons(earlier, cases, anchor_week)
  if (length(earlier) < min_seasons) {
    return(NULL)
  }

  fitted <- episodic_mem_fit(
    built$matrix[, earlier, drop = FALSE],
    typical_curve = episodic_mem_typical_curve(config)
  )
  if (is.null(fitted)) {
    return(NULL)
  }

  list(
    pre_epidemic = as.numeric(fitted$pre_epidemic),
    post_epidemic = as.numeric(fitted$post_epidemic),
    intensity = fitted$intensity,
    intensity_unavailable = fitted$intensity_unavailable,
    seasons_used = earlier
  )
}

#' The Monday of a given ISO week
#'
#' ISO 8601 week 1 is the week containing 4 January, so its Monday is
#' the Monday of that week and every later week is a multiple of seven
#' days on from it. Written out rather than round-tripped through
#' `format()` codes because `%G`/`%V` parse inconsistently across
#' platforms.
#'
#' @param iso_year,iso_week ISO year and week number.
#' @return A `Date`.
#' @keywords internal
#' @noRd
episodic_iso_week_start <- function(iso_year, iso_week) {
  jan4 <- as.Date(sprintf("%d-01-04", as.integer(iso_year)))
  week1_monday <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
  week1_monday + 7L * (as.integer(iso_week) - 1L)
}

# -- Matrix and week assignment ----------------------------------------

#' The week order within a season, from anchor round to anchor-1
#'
#' @param anchor_week Integer 1-52.
#' @return Character vector of length 52.
#' @keywords internal
#' @noRd
episodic_mem_week_order <- function(anchor_week) {
  anchor_week <- as.integer(anchor_week)
  if (anchor_week == 1L) {
    as.character(1:52)
  } else {
    as.character(c(anchor_week:52, 1:(anchor_week - 1L)))
  }
}

#' Build a `mem`-shaped season x week case-count matrix
#'
#' A 52-row matrix, one row per ISO week from the anchor round to the
#' week before it, one column per season labelled `"YYYY/YYYY"`, every
#' cell an integer count, zero-filled. Week 53 is folded into week 52,
#' a documented simplification already in place.
#'
#' @param cases A data frame with `sample_date`.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return A list with `matrix` (weeks x seasons, `NA`-free), or `NULL`
#'   if `cases` has no data.
#' @keywords internal
#' @noRd
episodic_mem_seasonal_matrix <- function(cases, anchor_week) {
  dates <- as.Date(cases$sample_date)
  if (length(dates) == 0) {
    return(NULL)
  }
  if (anyNA(dates)) {
    stop(
      "episodic_mem_seasonal_matrix(): ",
      sum(is.na(dates)),
      " case(s) have a sample_date that is not a date",
      call. = FALSE
    )
  }
  # Assigned once per distinct date and weighted by its case count: the
  # season and week a case falls in depend on its date alone.
  date <- sort(unique(dates))
  n <- tabulate(match(dates, date), nbins = length(date))
  assigned <- episodic_mem_season_weeks(date, anchor_week)

  week_order <- episodic_mem_week_order(anchor_week)
  season_levels <- sort(unique(assigned$season))

  mat <- matrix(
    0L,
    nrow = 52L,
    ncol = length(season_levels),
    dimnames = list(week_order, season_levels)
  )
  cell <- match(assigned$week_label, week_order) +
    52L * (match(assigned$season, season_levels) - 1L)
  summed <- rowsum(n, cell, reorder = FALSE)
  mat[as.integer(rownames(summed))] <- as.integer(summed[, 1])

  list(matrix = mat)
}

#' Which season and week a date falls in
#'
#' Every week of the year belongs to exactly one season; there is no
#' off-season. Given anchor week `a`, the season labelled `"Y1/Y2"`
#' contains ISO weeks `a` through 52 of year Y1 and weeks 1 through
#' `a-1` of year Y2.
#'
#' @param date A single `Date`.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return A list: `season` (`"YYYY/YYYY"`, never `NA`), `week_label`
#'   (character, the ISO week number as a string), `week_start` (the
#'   `Date` of that ISO week's Monday).
#' @keywords internal
#' @noRd
episodic_mem_season_week <- function(date, anchor_week) {
  date <- as.Date(date)
  iso_week <- as.integer(format(date, "%V"))
  iso_year <- as.integer(format(date, "%G"))
  week_start <- date - (as.integer(format(date, "%u")) - 1)

  week_capped <- min(iso_week, 52L)
  anchor_week <- as.integer(anchor_week)

  if (week_capped >= anchor_week) {
    season <- sprintf("%d/%d", iso_year, iso_year + 1L)
  } else {
    season <- sprintf("%d/%d", iso_year - 1L, iso_year)
  }

  list(
    season = season,
    week_label = as.character(week_capped),
    week_start = week_start
  )
}

#' Which season and week each of many dates falls in
#'
#' `episodic_mem_season_week()` for a vector of dates at once, by the
#' same rule.
#'
#' @param dates A `Date` vector without `NA`.
#' @param anchor_week Integer 1-52, the ISO week the season starts at.
#' @return A data frame with one row per date: `season` and `week_label`
#'   (character), `week_start` (`Date`).
#' @keywords internal
#' @noRd
episodic_mem_season_weeks <- function(dates, anchor_week) {
  dates <- as.Date(dates)
  iso_week <- as.integer(format(dates, "%V"))
  iso_year <- as.integer(format(dates, "%G"))
  week_capped <- pmin(iso_week, 52L)
  start_year <- ifelse(
    week_capped >= as.integer(anchor_week),
    iso_year,
    iso_year - 1L
  )
  data.frame(
    season = sprintf("%d/%d", start_year, start_year + 1L),
    week_label = as.character(week_capped),
    week_start = dates - (as.integer(format(dates, "%u")) - 1),
    stringsAsFactors = FALSE
  )
}

# -- Epidemic weeks -----------------------------------------------------

#' The MEM thresholds a seasonal epidemic was opened against
#'
#' An attribute of the epidemic, fixed when the run that opened it fitted
#' them on the seasons before its own, and stored on its
#' `episodic_epidemic_season` row. Read from there rather than fitted
#' again: a season's thresholds are set at its start, and a refit would
#' move them whenever a late report reached an earlier season.
#'
#' @param season An `episodic_epidemic_season` row, or `NULL`.
#' @return `NULL` for `NULL`, otherwise a list with `pre_epidemic`,
#'   `post_epidemic`, `intensity` (a named numeric of length 3, or `NULL`
#'   unless all three are stored, finite and non-decreasing) and
#'   `seasons_used`, the shape `episodic_mem_thresholds_for_season()`
#'   returns.
#' @keywords internal
#' @noRd
episodic_epidemic_stored_thresholds <- function(season) {
  if (is.null(season) || nrow(as.data.frame(season)) == 0) {
    return(NULL)
  }
  intensity <- suppressWarnings(as.numeric(c(
    season$intensity_medium %||% NA,
    season$intensity_high %||% NA,
    season$intensity_very_high %||% NA
  )))
  intensity <- if (all(is.finite(intensity)) && !is.unsorted(intensity)) {
    stats::setNames(intensity, c("medium", "high", "very_high"))
  } else {
    NULL
  }
  seasons_used <- as.character(season$seasons_used %||% NA)
  list(
    pre_epidemic = suppressWarnings(as.numeric(season$pre_epidemic_threshold %||% NA)),
    post_epidemic = suppressWarnings(as.numeric(season$post_epidemic_threshold %||% NA)),
    intensity = intensity,
    seasons_used = if (is.na(seasons_used)) {
      character(0)
    } else {
      trimws(strsplit(seasons_used, ",", fixed = TRUE)[[1]])
    }
  )
}

#' A seasonal epidemic's weekly counts and intensity bands, as of a run
#'
#' One row per week from the epidemic's onset week to the week the run
#' evaluates - the last fully elapsed one (`episodic_mem_evaluation_week()`),
#' the same week MEM detects and closes on - each counted from the
#' stream's cases and banded against the thresholds the epidemic was
#' opened against (`episodic_epidemic_stored_thresholds()`).
#'
#' @param cases_for_stream A data frame with `sample_date`, the stream's
#'   cases.
#' @param satellite A one-row data frame from
#'   `episodic_db_open_seasonal_epidemics()`.
#' @param run_date The date to treat as "today".
#' @return A data frame with `week_start` (character), `n_cases` and
#'   `intensity_level` (`NA` where the season has no intensity
#'   thresholds); no rows when the onset week is unknown or the run
#'   evaluates a week before it.
#' @keywords internal
#' @noRd
episodic_epidemic_weeks <- function(cases_for_stream, satellite, run_date) {
  empty <- data.frame(
    week_start = character(0),
    n_cases = integer(0),
    intensity_level = character(0),
    stringsAsFactors = FALSE
  )
  onset <- suppressWarnings(as.Date(satellite$onset_week_start %||% NA))
  if (length(onset) != 1 || is.na(onset)) {
    return(empty)
  }
  evaluated <- episodic_mem_evaluation_week(
    as.Date(run_date),
    as.integer(satellite$anchor_week)
  )$week_start
  if (evaluated < onset) {
    return(empty)
  }
  weeks <- seq(onset, evaluated, by = 7)
  dates <- as.Date(cases_for_stream$sample_date)
  dates <- dates[!is.na(dates) & dates >= onset & dates <= evaluated + 6]
  counts <- tabulate(
    as.integer(dates - onset) %/% 7L + 1L,
    nbins = length(weeks)
  )
  thresholds <- episodic_epidemic_stored_thresholds(satellite)
  data.frame(
    week_start = as.character(weeks),
    n_cases = as.integer(counts),
    intensity_level = vapply(
      counts,
      function(n) {
        episodic_mem_intensity_level(n, thresholds$pre_epidemic, thresholds$intensity)
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' An epidemic's typical past season, from its MEM detection
#'
#' Turns the typical season a MEM detection carries in its parameters
#' (`episodic_detect_mem()`) into what `episodic_epidemic_season` and
#' `episodic_epidemic_typical_week` store: the curve by week offset from
#' the typical epidemic start, the ISO week that start falls in, and how
#' many weeks after it this epidemic's onset came.
#'
#' @param mem_params The detection's parameters, parsed from JSON with
#'   `simplifyVector = FALSE`.
#' @param onset_week_start The Monday of the epidemic's onset week.
#' @param anchor_week The season's anchor week.
#' @return `NULL` when the detection carries no typical season, otherwise
#'   a list with `statistic` (the typical curve's), `onset_week` (integer
#'   ISO week), `onset_shift_weeks`
#'   (integer; negative when this epidemic started earlier), `length`,
#'   `length_lower`, `length_upper` and `weeks`, a data frame of
#'   `week_offset`, `lower`, `middle` and `upper`.
#' @keywords internal
#' @noRd
episodic_epidemic_typical_from_params <- function(mem_params,
                                                  onset_week_start,
                                                  anchor_week) {
  start <- suppressWarnings(as.integer(mem_params$typical_start %||% NA))
  if (length(start) != 1 || is.na(start)) {
    return(NULL)
  }
  curve <- lapply(
    mem_params[c("typical_lower", "typical_middle", "typical_upper")],
    function(x) suppressWarnings(as.numeric(unlist(x)))
  )
  week_order <- episodic_mem_week_order(anchor_week)
  n_weeks <- length(curve[[1]])
  if (
    any(lengths(curve) != n_weeks) || n_weeks != length(week_order) ||
      !all(is.finite(unlist(curve))) || start < 1 || start > n_weeks
  ) {
    stop(
      "A MEM detection carries a typical season that does not match its ",
      length(week_order),
      "-week season.",
      call. = FALSE
    )
  }
  statistic <- as.character(mem_params$typical_statistic %||% NA)
  if (!statistic %in% names(episodic_mem_typical_curves)) {
    stop(
      "A MEM detection carries a typical season without the statistic ",
      "its curve summarises past seasons by.",
      call. = FALSE
    )
  }
  onset <- episodic_mem_season_week(as.Date(onset_week_start), anchor_week)
  onset_index <- match(onset$week_label, week_order)
  list(
    statistic = statistic,
    onset_week = as.integer(week_order[start]),
    onset_shift_weeks = as.integer(onset_index - start),
    length = as.numeric(mem_params$typical_length),
    length_lower = as.numeric(mem_params$typical_length_lower),
    length_upper = as.numeric(mem_params$typical_length_upper),
    weeks = data.frame(
      week_offset = seq_len(n_weeks) - start,
      lower = curve$typical_lower,
      middle = curve$typical_middle,
      upper = curve$typical_upper
    )
  )
}

#' The most severe of a set of intensity bands
#'
#' @param levels Values of `episodic_mem_intensity_level()`, `NA`
#'   allowed.
#' @return The highest band present, or `NA_character_` when none is.
#' @keywords internal
#' @noRd
episodic_mem_intensity_max <- function(levels) {
  steps <- c("baseline", "low", "medium", "high", "very_high")
  ranks <- match(levels, steps)
  if (all(is.na(ranks))) {
    return(NA_character_)
  }
  steps[max(ranks, na.rm = TRUE)]
}

# -- Epidemic closure ---------------------------------------------------

#' Check whether an open seasonal epidemic should close
#'
#' The primary criterion is the post-epidemic threshold stored on the
#' satellite: the epidemic closes when the evaluated week's count falls
#' at or below it. The backstop is the derived trough: when the count
#' has fallen back to non-epidemic activity - at or below the
#' pre-epidemic threshold, MEM's own line between the two - but never
#' crosses the post-epidemic threshold cleanly on a noisy series, the
#' epidemic closes once the evaluated week is inside the pathogen's own
#' trough. These are two different pieces of evidence, and the audit
#' trail records which one fired.
#'
#' The trough alone is a calendar, not a measurement: an epidemic
#' running out of season - influenza in late summer - is in the trough
#' weeks while its count is still rising, and closing it there would
#' close it in the run that opened it. Without a pre-epidemic threshold
#' to tell a fallen count from a rising one, the backstop does not fire
#' and the post-epidemic threshold alone decides.
#'
#' @param cases_for_stream A data frame with `sample_date`, the stream's
#'   full case history (needed to derive the anchor and count the
#'   evaluated week).
#' @param run_date The date to treat as "today".
#' @param config The resolved configuration.
#' @param satellite A one-row data frame from
#'   `episodic_db_open_seasonal_epidemics()`, carrying
#'   `pre_epidemic_threshold`, `post_epidemic_threshold` and
#'   `anchor_week`.
#' @return `NULL` if the epidemic should stay open, otherwise a list
#'   with `ended_week_start` (Date) and `ended_reason` (character).
#' @keywords internal
#' @noRd
episodic_epidemic_closure <- function(cases_for_stream,
                                      run_date,
                                      config,
                                      satellite) {
  anchor_week <- as.integer(satellite$anchor_week)
  anchor <- episodic_mem_season_anchor(cases_for_stream, config)
  if (is.null(anchor)) {
    return(NULL)
  }

  evaluated <- episodic_mem_evaluation_week(run_date, anchor_week)
  week_start <- evaluated$week_start
  week_end <- week_start + 6

  dates <- as.Date(cases_for_stream$sample_date)
  week_count <- sum(
    dates >= week_start & dates <= week_end,
    na.rm = TRUE
  )

  post_threshold <- as.numeric(satellite$post_epidemic_threshold)
  if (!is.na(post_threshold) && week_count <= post_threshold) {
    return(list(
      ended_week_start = as.character(week_start),
      ended_reason = "post_epidemic_threshold"
    ))
  }

  pre_threshold <- as.numeric(satellite$pre_epidemic_threshold %||% NA_real_)
  eval_iso_week <- as.integer(evaluated$week_label)
  if (
    eval_iso_week %in% anchor$trough_run &&
      !is.na(pre_threshold) &&
      week_count <= pre_threshold
  ) {
    return(list(
      ended_week_start = as.character(week_start),
      ended_reason = "trough"
    ))
  }

  NULL
}
