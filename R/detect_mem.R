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
#' The Moving Epidemic Method (`mem` package - not an R base/CRAN
#' near-namesake of anything else in this codebase) supplies pre-epidemic
#' and post-epidemic thresholds from historical seasons, for pathogens
#' flagged `mem_applicable` (Influenza A/B, RSV in the shipped
#' `pathogen_config.csv`). Farrington answers whether counts exceed a
#' statistical expectation; MEM answers a different, more clinically
#' relevant question for a seasonal pathogen: has the epidemic started.
#'
#' Runs only against `pathogen_region` (L5) streams: MEM needs a stable,
#' population-level weekly series across several historical seasons to
#' fit at all, which no ward or single institution has enough volume to
#' support, and seasonal surveillance is inherently a population-level
#' question, not an institutional one. `same_place` and
#' Farrington continue to run at every other level exactly as before;
#' this is additive, not a replacement.
#'
#' Requires at least `config$mem$min_seasons` complete prior seasons in the
#' historical matrix before firing at all, the same "need real baseline
#' history first" posture Farrington already takes for its own `b`
#' parameter - and, like `b`, a configured value rather than a literal, so
#' an instance can raise it without touching the code.
#'
#' @param cases_for_stream A data frame of a single (L5) stream's cases,
#'   with `sample_date`.
#' @param stream_id The stream these cases belong to.
#' @param run_date The date to treat as "today".
#' @param config The resolved configuration; uses `config$mem`.
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
episodic_detect_mem <- function(
  cases_for_stream,
  stream_id,
  run_date = Sys.Date(),
  config = episodic_config_resolve()
) {
  empty <- episodic_detection_record(
    integer(0),
    character(0),
    character(0),
    character(0),
    integer(0)
  )
  if (!requireNamespace("mem", quietly = TRUE)) {
    return(empty)
  }
  if (is.null(cases_for_stream) || nrow(cases_for_stream) == 0) {
    return(empty)
  }

  status <- episodic_mem_status(cases_for_stream, run_date, config)
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
    params = list(post_epidemic_threshold = status$post_epidemic_threshold)
  )
}

#' Compute this stream's current MEM status
#'
#' Shared by `episodic_detect_mem()` (fires on `epidemic_started`) and the
#' seasonal closure criterion (fires when the evaluated count has fallen
#' back under `post_epidemic_threshold`), so both read the same fitted
#' model rather than risking two slightly different ones.
#'
#' Three things decide whether this can say anything at all, in order:
#'
#' 1. **Which week to evaluate.** The last *complete* epidemiological
#'    week, never the one in progress. The week containing the run date
#'    is partial by construction - on a Tuesday it holds two days of
#'    cases - and on top of that its cases have not finished being
#'    reported. Comparing that count against a threshold fitted on whole
#'    weeks is comparing unlike quantities, and always in the same
#'    direction: the count is too low, so an epidemic start is declared
#'    late and a post-epidemic closure declared early. Evaluating last
#'    week costs a week of latency and buys a count that means what the
#'    threshold means.
#'
#' 2. **Whether it is in season at all.** Outside the week 40-20
#'    surveillance window there is no seasonal question to answer, and
#'    this returns `in_season = FALSE` rather than `NULL`. The
#'    distinction matters at the other end: a `mem_applicable` cluster
#'    used to have no closure route whatsoever between May and
#'    September, because "MEM could not be computed" and "the season is
#'    over" were the same answer - so every confirmed influenza epidemic
#'    stayed open on the rail right through the summer. The calendar
#'    answers "has the epidemic ended" perfectly well in July.
#'
#' 3. **Which seasons may serve as history.** Only seasons the case data
#'    actually spans end to end. A season column exists as soon as one
#'    case falls in it, so a site whose data begins in January carries a
#'    season that is three-quarters structural zeros; fed to
#'    `mem::memmodel()` as if it were an observed season, it drags the
#'    pre-epidemic threshold down and makes the next winter's epidemic
#'    start fire early. Half a season is not a quiet season.
#'
#' @param cases A data frame with `sample_date`, all of a stream's known
#'   cases (not just one cluster's).
#' @param run_date The date to treat as "today".
#' @param config The resolved configuration; `config$mem$min_seasons` is
#'   the minimum number of fully-observed prior seasons required.
#' @return `NULL` if `mem` is not installed, there is no case data, or
#'   fewer than that many fully-observed prior seasons exist.
#'   Otherwise a list: `in_season` (logical), `epidemic_started`
#'   (logical), `current_week_count`, `pre_epidemic_threshold`,
#'   `post_epidemic_threshold`, `week_start`, `week_end` (the evaluated
#'   week's `Date` bounds). Out of season, only `in_season`,
#'   `epidemic_started` (always `FALSE`) and the week bounds are
#'   meaningful; the counts and thresholds are `NA`.
#' @keywords internal
#' @noRd
episodic_mem_status <- function(
  cases,
  run_date = Sys.Date(),
  config = episodic_config_resolve()
) {
  min_seasons <- as.integer(config$mem$min_seasons %||% 2L)
  evaluated <- episodic_mem_evaluation_week(run_date)

  # Deliberately settled before the `mem` and case-data guards below:
  # whether the surveillance season has lapsed is a fact about the
  # calendar, and stays knowable when nothing else here is.
  if (is.na(evaluated$season)) {
    return(list(
      in_season = FALSE,
      epidemic_started = FALSE,
      current_week_count = NA_integer_,
      pre_epidemic_threshold = NA_real_,
      post_epidemic_threshold = NA_real_,
      week_start = evaluated$week_start,
      week_end = evaluated$week_start + 6
    ))
  }

  if (!requireNamespace("mem", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(cases) || nrow(cases) == 0) {
    return(NULL)
  }

  built <- episodic_mem_seasonal_matrix(cases)
  if (is.null(built) || is.null(built$matrix)) {
    return(NULL)
  }

  prior_seasons <- setdiff(colnames(built$matrix), evaluated$season)
  prior_seasons <- episodic_mem_observed_seasons(prior_seasons, cases)
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

  # "Threhold is the upper limit of the confidence interval" (?mem::memmodel).
  pre_threshold <- fit$pre.post.intervals["pre.i", 3]
  post_threshold <- fit$pre.post.intervals["post.i", 3]
  intensity <- episodic_mem_intensity_thresholds(fit)

  week_label <- evaluated$week_label
  if (!week_label %in% rownames(built$matrix)) {
    return(NULL)
  }
  # A season with no cases yet has no column of its own; that is a count
  # of zero, not an unknown.
  current_count <- if (evaluated$season %in% colnames(built$matrix)) {
    built$matrix[week_label, evaluated$season]
  } else {
    0L
  }

  list(
    in_season = TRUE,
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
    season = evaluated$season,
    seasons_used = prior_seasons,
    week_start = evaluated$week_start,
    week_end = evaluated$week_start + 6
  )
}

#' MEM's medium/high/very high intensity thresholds, if this `mem` build
#' reports them in the shape expected
#'
#' Detection only ever needed the pre-epidemic threshold, so these were
#' never read; displaying seasonal activity needs them, since "the
#' epidemic has started" and "the epidemic is severe" are different
#' statements and an epidemiologist assessing a season wants both.
#'
#' Read defensively rather than indexed straight: `mem::memmodel()`'s
#' return shape for the intensity thresholds is not part of a stable
#' documented contract the way `pre.post.intervals` is, and a shape this
#' does not recognise must leave the intensity bands unavailable rather
#' than take the whole seasonal panel down with it - the same posture
#' every other optional panel in this codebase takes.
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

#' The last fully-elapsed epidemiological week before the run date
#'
#' @param run_date The date to treat as "today".
#' @return `episodic_mem_season_week()`'s output for that week.
#' @keywords internal
#' @noRd
episodic_mem_evaluation_week <- function(run_date) {
  today <- as.Date(run_date)
  this_monday <- today - (as.integer(format(today, "%u")) - 1L)
  episodic_mem_season_week(this_monday - 7)
}

#' Keep only the seasons the case data spans end to end
#'
#' @param seasons Season labels (`"YYYY/YYYY"`).
#' @param cases A data frame with `sample_date`; its full date span (not
#'   only its in-season dates) is what defines the observation window.
#' @return The subset of `seasons` falling entirely inside that window.
#' @keywords internal
#' @noRd
episodic_mem_observed_seasons <- function(seasons, cases) {
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
      bounds <- episodic_mem_season_bounds(season)
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
#' Week 40 of the first year through week 20 of the second, the same
#' convention `episodic_mem_season_week()` assigns dates by.
#'
#' @param season A season label, `"YYYY/YYYY"`.
#' @return A list with `start` and `end` (`Date`), or `NULL` if `season`
#'   is not a well-formed label.
#' @keywords internal
#' @noRd
episodic_mem_season_bounds <- function(season) {
  years <- suppressWarnings(as.integer(strsplit(
    as.character(season),
    "/",
    fixed = TRUE
  )[[1]]))
  if (length(years) != 2 || anyNA(years)) {
    return(NULL)
  }
  list(
    start = episodic_iso_week_start(years[1], 40L),
    end = episodic_iso_week_start(years[2], 20L) + 6L
  )
}

#' The surveillance season a date belongs to, in season or out
#'
#' `episodic_mem_season_week()` deliberately answers `NA` off-season,
#' because MEM has nothing to say there. The activity screen has a
#' different question - "which season's chart am I looking at" - and in
#' July the answer is the season that just finished, not "none".
#'
#' @param date A single `Date`.
#' @return A season label, `"YYYY/YYYY"`.
#' @keywords internal
#' @noRd
episodic_season_containing <- function(date) {
  date <- as.Date(date)
  in_season <- episodic_mem_season_week(date)
  if (!is.na(in_season$season)) {
    return(in_season$season)
  }
  # The off-season window (weeks 21-39) always sits inside one calendar
  # year, so the season that just ended is unambiguous.
  year <- as.integer(format(date, "%Y"))
  sprintf("%d/%d", year - 1L, year)
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
#' `episodic_mem_status()` answers "where are we right now"; this answers
#' "what were the thresholds for the season being looked at", which is
#' what the activity screen draws a season's weekly curve against.
#'
#' The season itself is excluded from its own fit. Including it would be
#' look-ahead: a severe season would raise the very thresholds used to
#' call it severe, flattening exactly the signal an epidemiologist opened
#' the chart to see.
#'
#' @param cases A data frame with `sample_date`.
#' @param season The season label the thresholds are for.
#' @param config The resolved configuration; `config$mem$min_seasons` is
#'   the minimum number of fully-observed earlier seasons required - the
#'   same dial `episodic_mem_status()` reads, so raising it cannot leave
#'   the detector silent while this screen keeps drawing thresholds fitted
#'   on fewer seasons than the detector is willing to trust.
#' @return A list with `pre_epidemic`, `post_epidemic`, `intensity` (see
#'   `episodic_mem_intensity_thresholds()`) and `seasons_used`, or `NULL`
#'   when `mem` is unavailable or too little earlier history exists.
#' @keywords internal
#' @noRd
episodic_mem_thresholds_for_season <- function(
  cases,
  season,
  config = episodic_config_resolve()
) {
  min_seasons <- as.integer(config$mem$min_seasons %||% 2L)
  if (!requireNamespace("mem", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(cases) || nrow(cases) == 0) {
    return(NULL)
  }

  built <- episodic_mem_seasonal_matrix(cases)
  if (is.null(built) || is.null(built$matrix)) {
    return(NULL)
  }

  # "YYYY/YYYY" labels sort chronologically as plain strings, so a
  # string comparison is a chronological one here.
  earlier <- colnames(built$matrix)[
    colnames(built$matrix) < as.character(season)
  ]
  earlier <- episodic_mem_observed_seasons(earlier, cases)
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
#' ISO 8601 week 1 is the week containing 4 January, so its Monday is the
#' Monday of that week and every later week is a multiple of seven days
#' on from it. Written out rather than round-tripped through `format()`
#' codes because `%G`/`%V` parse inconsistently across platforms.
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

#' Build a `mem`-shaped season x week case-count matrix
#'
#' Surveillance seasons run ISO week 40 to week 20 of the following
#' calendar year, the standard northern-hemisphere influenza-season
#' convention, matching `mem`'s own documented convention and its
#' bundled `flucyl` example dataset's row/column shape (33 weeks: 40-52,
#' then 1-20; one column per season, named `"YYYY/YYYY"`). Week 53 (some
#' years have one) is folded into week 52 - a documented simplification:
#' `mem`'s own guidance describes handling it as a distinct column,
#' which this does not attempt.
#'
#' @param cases A data frame with `sample_date`.
#' @return `NULL` if `cases` has no in-season data at all, else a list
#'   with `matrix` (weeks x seasons, `NA`-free, zero-filled).
#' @keywords internal
#' @noRd
episodic_mem_seasonal_matrix <- function(cases) {
  dates <- as.Date(cases$sample_date)
  assigned <- lapply(dates, episodic_mem_season_week)
  seasons <- vapply(assigned, function(a) a$season, character(1))
  weeks <- vapply(assigned, function(a) a$week_label, character(1))

  in_season <- !is.na(seasons)
  if (!any(in_season)) {
    return(NULL)
  }
  seasons <- seasons[in_season]
  weeks <- weeks[in_season]

  week_order <- c(as.character(40:52), as.character(1:20))
  season_levels <- sort(unique(seasons))

  mat <- matrix(
    0L,
    nrow = length(week_order),
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

#' Which surveillance season/week a date falls in
#'
#' @param date A single `Date`.
#' @return A list: `season` (`"YYYY/YYYY"`, or `NA` if `date` falls in the
#'   May-September off-season window), `week_label` (character, one of
#'   `"40"`..`"52"`, `"1"`..`"20"`), `week_start` (the `Date` of that ISO
#'   week's Monday).
#' @keywords internal
#' @noRd
episodic_mem_season_week <- function(date) {
  date <- as.Date(date)
  iso_week <- as.integer(format(date, "%V"))
  iso_year <- as.integer(format(date, "%G"))
  week_start <- date - (as.integer(format(date, "%u")) - 1)

  if (iso_week >= 40) {
    week_capped <- min(iso_week, 52L) # fold week 53 into 52
    list(
      season = sprintf("%d/%d", iso_year, iso_year + 1L),
      week_label = as.character(week_capped),
      week_start = week_start
    )
  } else if (iso_week <= 20) {
    list(
      season = sprintf("%d/%d", iso_year - 1L, iso_year),
      week_label = as.character(iso_week),
      week_start = week_start
    )
  } else {
    list(
      season = NA_character_,
      week_label = NA_character_,
      week_start = week_start
    )
  }
}
