#' `mem` seasonal detector
#'
#' The Moving Epidemic Method (`mem` package - not an R base/CRAN
#' near-namesake of anything else in this codebase) supplies pre-epidemic
#' and post-epidemic thresholds from historical seasons, for organisms
#' flagged `mem_applicable` (Influenza A/B, RSV in the shipped
#' `pathogen_config.csv`). Farrington answers whether counts exceed a
#' statistical expectation; MEM answers a different, more clinically
#' relevant question for a seasonal pathogen - has the epidemic started
#' (ARCHITECTURE.md section 7.3).
#'
#' Runs only against `pathogen_region` (L5) streams: MEM needs a stable,
#' population-level weekly series across several historical seasons to
#' fit at all, which no ward or single institution has enough volume to
#' support, and ARCHITECTURE.md itself frames seasonal surveillance as a
#' population-level question, not an institutional one. `same_place` and
#' Farrington continue to run at every other level exactly as before;
#' this is additive, not a replacement.
#'
#' Requires at least two complete prior seasons in the historical matrix
#' before firing at all (`min_seasons`), the same "need real baseline
#' history first" posture Farrington already takes for its own `b`
#' parameter.
#'
#' @param cases_for_stream A data frame of a single (L5) stream's cases,
#'   with `sample_date`.
#' @param stream_id The stream these cases belong to.
#' @param run_date The date to treat as "today".
#' @param min_seasons Minimum number of complete prior seasons required.
#' @return A data frame of detection records (zero or one row).
#' @export
episode_detect_mem <- function(cases_for_stream, stream_id, run_date = Sys.Date(), min_seasons = 2L) {
  empty <- episode_detection_record(integer(0), character(0), character(0), character(0), integer(0))
  if (!requireNamespace("mem", quietly = TRUE)) return(empty)
  if (is.null(cases_for_stream) || nrow(cases_for_stream) == 0) return(empty)

  status <- episode_mem_status(cases_for_stream, run_date, min_seasons)
  if (is.null(status) || !isTRUE(status$epidemic_started)) return(empty)

  episode_detection_record(
    stream_id = stream_id, detector = "mem",
    first_day = as.character(status$week_start), last_day = as.character(status$week_end),
    n_cases = status$current_week_count,
    expected = NA_real_, upperbound = status$pre_epidemic_threshold,
    params = list(post_epidemic_threshold = status$post_epidemic_threshold)
  )
}

#' Compute this stream's current MEM status
#'
#' Shared by [episode_detect_mem()] (fires on `epidemic_started`) and the
#' seasonal closure criterion (fires when the current count has fallen
#' back under `post_epidemic_threshold` - ARCHITECTURE.md sections 6.3 and
#' 7.3), so both read the same fitted model rather than risking two
#' slightly different ones.
#'
#' @param cases A data frame with `sample_date`, all of a stream's known
#'   cases (not just one cluster's).
#' @param run_date The date to treat as "today".
#' @param min_seasons Minimum number of complete prior seasons required.
#' @return `NULL` if `mem` is not installed, there is no case data, the
#'   current date falls outside the surveillance season window, or fewer
#'   than `min_seasons` complete prior seasons exist. Otherwise a list:
#'   `epidemic_started` (logical), `current_week_count`,
#'   `pre_epidemic_threshold`, `post_epidemic_threshold`, `week_start`,
#'   `week_end` (the current epi week's `Date` bounds).
#' @export
episode_mem_status <- function(cases, run_date = Sys.Date(), min_seasons = 2L) {
  if (!requireNamespace("mem", quietly = TRUE)) return(NULL)
  if (is.null(cases) || nrow(cases) == 0) return(NULL)

  today <- as.Date(run_date)
  current <- episode_mem_season_week(today)
  if (is.na(current$season)) return(NULL)  # off-season (May-September window)

  built <- episode_mem_seasonal_matrix(cases)
  if (is.null(built) || is.null(built$matrix)) return(NULL)

  prior_seasons <- setdiff(colnames(built$matrix), current$season)
  if (length(prior_seasons) < min_seasons) return(NULL)

  historical <- built$matrix[, prior_seasons, drop = FALSE]
  fit <- tryCatch(
    suppressWarnings(suppressMessages(mem::memmodel(as.data.frame(historical), i.mem.info = FALSE))),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  # "Threhold is the upper limit of the confidence interval" (?mem::memmodel).
  pre_threshold <- fit$pre.post.intervals["pre.i", 3]
  post_threshold <- fit$pre.post.intervals["post.i", 3]

  week_label <- current$week_label
  if (!week_label %in% rownames(built$matrix) || !current$season %in% colnames(built$matrix)) return(NULL)
  current_count <- built$matrix[week_label, current$season]

  list(
    epidemic_started = isTRUE(current_count > pre_threshold),
    current_week_count = as.integer(current_count),
    pre_epidemic_threshold = as.numeric(pre_threshold),
    post_epidemic_threshold = as.numeric(post_threshold),
    week_start = current$week_start, week_end = current$week_start + 6
  )
}

#' Build a `mem`-shaped season x week case-count matrix
#'
#' Surveillance seasons run ISO week 40 to week 20 of the following
#' calendar year (ARCHITECTURE.md section 7.3's "northern hemisphere...
#' week 40 to 20"), matching `mem`'s own documented convention and its
#' bundled `flucyl` example dataset's row/column shape (33 weeks: 40-52,
#' then 1-20; one column per season, named `"YYYY/YYYY"`). Week 53 (some
#' years have one) is folded into week 52 - a documented simplification,
#' not a silent one (QUESTIONS.md): `mem`'s own guidance ("accommodate
#' week 53") describes handling it as a distinct column, which this does
#' not attempt.
#'
#' @param cases A data frame with `sample_date`.
#' @return `NULL` if `cases` has no in-season data at all, else a list
#'   with `matrix` (weeks x seasons, `NA`-free, zero-filled).
#' @keywords internal
#' @noRd
episode_mem_seasonal_matrix <- function(cases) {
  dates <- as.Date(cases$sample_date)
  assigned <- lapply(dates, episode_mem_season_week)
  seasons <- vapply(assigned, function(a) a$season, character(1))
  weeks <- vapply(assigned, function(a) a$week_label, character(1))

  in_season <- !is.na(seasons)
  if (!any(in_season)) return(NULL)
  seasons <- seasons[in_season]
  weeks <- weeks[in_season]

  week_order <- c(as.character(40:52), as.character(1:20))
  season_levels <- sort(unique(seasons))

  mat <- matrix(0L, nrow = length(week_order), ncol = length(season_levels),
                 dimnames = list(week_order, season_levels))
  tab <- table(factor(weeks, levels = week_order), factor(seasons, levels = season_levels))
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
episode_mem_season_week <- function(date) {
  date <- as.Date(date)
  iso_week <- as.integer(format(date, "%V"))
  iso_year <- as.integer(format(date, "%G"))
  week_start <- date - (as.integer(format(date, "%u")) - 1)

  if (iso_week >= 40) {
    week_capped <- min(iso_week, 52L)  # fold week 53 into 52
    list(season = sprintf("%d/%d", iso_year, iso_year + 1L), week_label = as.character(week_capped),
         week_start = week_start)
  } else if (iso_week <= 20) {
    list(season = sprintf("%d/%d", iso_year - 1L, iso_year), week_label = as.character(iso_week),
         week_start = week_start)
  } else {
    list(season = NA_character_, week_label = NA_character_, week_start = week_start)
  }
}
