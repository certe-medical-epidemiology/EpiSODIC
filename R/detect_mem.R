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
                                stream_label = NULL) {
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

  status <- episodic_mem_status(cases_for_stream, run_date, config, anchor)
  if (is.null(status) || !isTRUE(status$epidemic_started)) {
    return(empty)
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
      post_epidemic_threshold = status$post_epidemic_threshold
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
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(NULL)
  }

  min_years <- as.integer(config$mem$min_climatology_years %||% 3L)
  smooth_weeks <- as.integer(config$mem$climatology_smooth_weeks %||% 5L)
  trough_pctl <- as.numeric(config$mem$trough_percentile %||% 0.25)

  data_start <- min(dates)
  data_end <- max(dates)
  all_years <- sort(unique(as.integer(format(dates, "%G"))))
  complete <- vapply(all_years, function(yr) {
    data_start <= episodic_iso_week_start(yr, 1L) &&
      data_end >= episodic_iso_week_start(yr, 52L) + 6L
  }, logical(1))
  complete_years <- all_years[complete]

  if (length(complete_years) < min_years) {
    return(NULL)
  }

  iso_weeks <- pmin(as.integer(format(dates, "%V")), 52L)
  iso_years <- as.integer(format(dates, "%G"))
  in_complete <- iso_years %in% complete_years

  counts_df <- stats::aggregate(
    list(n = rep(1L, sum(in_complete))),
    by = list(
      year = iso_years[in_complete],
      week = iso_weeks[in_complete]
    ),
    FUN = sum
  )

  climatology <- vapply(1:52, function(w) {
    year_counts <- vapply(complete_years, function(yr) {
      idx <- counts_df$year == yr & counts_df$week == w
      if (any(idx)) counts_df$n[idx] else 0L
    }, integer(1))
    stats::median(year_counts)
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
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(NULL)
  }

  min_years <- as.integer(config$mem$min_climatology_years %||% 3L)
  peak_weeks <- as.integer(config$mem$seasonality_peak_weeks %||% 8L)
  min_share <- as.numeric(config$mem$seasonality_min_peak_share %||% 0.40)

  iso_weeks <- pmin(as.integer(format(dates, "%V")), 52L)
  iso_years <- as.integer(format(dates, "%G"))

  data_start <- min(dates)
  data_end <- max(dates)
  all_years <- sort(unique(iso_years))
  complete <- vapply(all_years, function(yr) {
    data_start <= episodic_iso_week_start(yr, 1L) &&
      data_end >= episodic_iso_week_start(yr, 52L) + 6L
  }, logical(1))
  complete_years <- all_years[complete]

  if (length(complete_years) < min_years) {
    return(NULL)
  }

  in_complete <- iso_years %in% complete_years
  counts_df <- stats::aggregate(
    list(n = rep(1L, sum(in_complete))),
    by = list(
      year = iso_years[in_complete],
      week = iso_weeks[in_complete]
    ),
    FUN = sum
  )

  n_years <- length(complete_years)
  mat <- matrix(0L,
    nrow = 52L, ncol = n_years,
    dimnames = list(NULL, as.character(complete_years))
  )
  for (i in seq_len(nrow(counts_df))) {
    w <- counts_df$week[i]
    y <- as.character(counts_df$year[i])
    mat[w, y] <- counts_df$n[i]
  }

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
#' Shared by `episodic_detect_mem()` (fires on `epidemic_started`) and
#' the epidemic closure criterion (fires when the evaluated count has
#' fallen back under `post_epidemic_threshold`).
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
#' @return `NULL` when MEM cannot compute (no `mem` package, no data,
#'   or too few fully-observed prior seasons). Otherwise a list:
#'   `in_trough` (logical), `epidemic_started` (logical),
#'   `current_week_count`, `pre_epidemic_threshold`,
#'   `post_epidemic_threshold`, `intensity_thresholds`,
#'   `intensity_level`, `anchor_week`, `season`, `seasons_used`,
#'   `week_start`, `week_end`.
#' @keywords internal
#' @noRd
episodic_mem_status <- function(cases,
                                run_date = Sys.Date(),
                                config = episodic_config_resolve(),
                                anchor = NULL) {
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
  fit <- tryCatch(
    suppressWarnings(suppressMessages(mem::memmodel(
      as.data.frame(historical),
      i.mem.info = FALSE
    ))),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }

  pre_threshold <- fit$pre.post.intervals["pre.i", 3]
  post_threshold <- fit$pre.post.intervals["post.i", 3]
  intensity <- episodic_mem_intensity_thresholds(fit)

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
    intensity_level = episodic_mem_intensity_level(
      current_count,
      pre_threshold,
      intensity
    ),
    anchor_week = anchor_week,
    season = evaluated$season,
    seasons_used = prior_seasons,
    week_start = evaluated$week_start,
    week_end = evaluated$week_start + 6
  )
}

#' MEM's medium/high/very high intensity thresholds, if this `mem` build
#' reports them in the shape expected
#'
#' Read defensively rather than indexed straight: `mem::memmodel()`'s
#' return shape for the intensity thresholds is not part of a stable
#' documented interface the way `pre.post.intervals` is, and a shape
#' this does not recognise must leave the intensity bands unavailable
#' rather than take the whole seasonal panel down with it.
#'
#' @param fit A `mem::memmodel()` result.
#' @return A named numeric of length 3 (`medium`, `high`, `very_high`),
#'   or `NULL`.
#' @keywords internal
#' @noRd
episodic_mem_intensity_thresholds <- function(fit) {
  raw <- tryCatch(fit$epidemic.thresholds, error = function(e) NULL)
  values <- suppressWarnings(as.numeric(raw))
  values <- values[is.finite(values)]
  if (length(values) < 3) {
    return(NULL)
  }
  values <- sort(values)[seq_len(3)]
  stats::setNames(values, c("medium", "high", "very_high"))
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
#' @return A list with `pre_epidemic`, `post_epidemic`, `intensity` and
#'   `seasons_used`, or `NULL` when `mem` is unavailable or too little
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

  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      mem::memmodel(
        as.data.frame(built$matrix[, earlier, drop = FALSE]),
        i.mem.info = FALSE
      )
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }

  list(
    pre_epidemic = as.numeric(fit$pre.post.intervals["pre.i", 3]),
    post_epidemic = as.numeric(fit$pre.post.intervals["post.i", 3]),
    intensity = episodic_mem_intensity_thresholds(fit),
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
  assigned <- lapply(dates, episodic_mem_season_week,
    anchor_week = anchor_week
  )
  seasons <- vapply(assigned, function(a) a$season, character(1))
  weeks <- vapply(assigned, function(a) a$week_label, character(1))

  week_order <- episodic_mem_week_order(anchor_week)
  season_levels <- sort(unique(seasons))

  mat <- matrix(
    0L,
    nrow = 52L,
    ncol = length(season_levels),
    dimnames = list(week_order, season_levels)
  )
  tab <- table(
    factor(weeks, levels = week_order),
    factor(seasons, levels = season_levels)
  )
  mat[rownames(tab), colnames(tab)] <- tab

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
