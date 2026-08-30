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

# The Pathogen screen's read model: one pathogen, one date range, read
# at the epidemiological level rather than the operational one.
#
# Every other screen in EpiSODIC is organised around a cluster - a
# discrete thing someone has to make a decision about. That is the right
# unit for triage and the wrong unit for surveillance. "Is influenza A
# unusual this season, and where in the season are we" is not a question
# about any one cluster, and cannot be answered by reading several
# cluster dossiers in turn: the answer lives in the whole pathogen's
# incidence across the whole catchment.
#
# Two things in the codebase already computed the population-level view
# and then discarded it. MEM fits pre- and post-epidemic thresholds for
# every seasonal pathogen on every run and uses them only to decide
# whether a detector fires - the thresholds themselves, and where the
# current week sits against them, were never shown to anyone. And Rt was
# computed per cluster only, where the renewal equation is being fed a
# subset of the transmission process it assumes it is seeing whole. This
# screen is where both belong.

#' Which pathogens the Pathogen screen can be pointed at
#'
#' @param con A [DBI::DBIConnection-class].
#' @return A data frame with `pathogen`, `n_cases`, `first_day`,
#'   `last_day`, ordered by case count descending.
#' @keywords internal
#' @noRd
episodic_app_pathogen_options <- function(con) {
  out <- DBI::dbGetQuery(
    con,
    "SELECT pathogen, COUNT(*) AS n_cases, MIN(sample_date) AS first_day, MAX(sample_date) AS last_day
       FROM episodic_case GROUP BY pathogen ORDER BY COUNT(*) DESC"
  )
  if (nrow(out) == 0) {
    return(data.frame(
      pathogen = character(0),
      n_cases = integer(0),
      first_day = character(0),
      last_day = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out
}

#' The date-range presets the Pathogen screen offers
#'
#' Respiratory surveillance is organised in seasons, not calendar years,
#' so a season is a first-class preset rather than something to be
#' reconstructed from two date pickers every time. Multi-year is a
#' preset for the same reason: "how does this season compare with the
#' last five" is a routine question, not an advanced one.
#'
#' @keywords internal
#' @noRd
episodic_pathogen_period_ids <- c(
  "year_current",
  "season_current",
  "season_previous",
  "last_12m",
  "last_5y",
  "all",
  "custom"
)

#' Coerce one user-supplied date, however malformed, to a single `Date`
#'
#' A blank or absent date picker gives `NULL`, and `as.Date(NULL)` is a
#' zero-length `Date` rather than `NA` - which would then make every
#' downstream `if (is.na(x))` fail on a zero-length condition instead of
#' falling back to a default period.
#'
#' @param x Anything a date picker might hand over.
#' @return A single `Date`, possibly `NA`.
#' @keywords internal
#' @noRd
episodic_as_single_date <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.Date(NA))
  }
  parsed <- suppressWarnings(tryCatch(as.Date(x[1]), error = function(e) {
    as.Date(NA)
  }))
  if (length(parsed) == 0) as.Date(NA) else parsed[1]
}

#' Resolve a period selection into concrete dates
#'
#' @param period One of `episodic_pathogen_period_ids`.
#' @param from,to Explicit bounds, used when `period` is `"custom"` (or
#'   when a preset cannot be resolved).
#' @param asof The date the data is current as of.
#' @param data_from The earliest case date available, for `"all"`.
#' @return A list with `id`, `from`, `to` (`Date`), `season` (the season
#'   label when the period is a season, else `NA`), and `previous` (a
#'   `list(from, to, label)` describing the comparable earlier period, or
#'   `NULL`).
#' @keywords internal
#' @noRd
episodic_app_resolve_period <- function(
  period = "season_current",
  from = NULL,
  to = NULL,
  asof = Sys.Date(),
  data_from = NULL
) {
  asof <- as.Date(asof)
  period <- if (is.null(period) || !period %in% episodic_pathogen_period_ids) {
    "season_current"
  } else {
    period
  }

  season_window <- function(season) {
    bounds <- episodic_mem_season_bounds(season)
    if (is.null(bounds)) {
      return(NULL)
    }
    previous <- episodic_season_shift(season, 1L)
    previous_bounds <- if (is.null(previous)) {
      NULL
    } else {
      episodic_mem_season_bounds(previous)
    }
    list(
      id = period,
      from = bounds$start,
      to = min(bounds$end, asof),
      season = season,
      previous = if (is.null(previous_bounds)) {
        NULL
      } else {
        list(
          from = previous_bounds$start,
          to = previous_bounds$end,
          label = previous
        )
      }
    )
  }

  resolved <- switch(period,
    year_current = list(
      id = period,
      from = as.Date(sprintf("%d-01-01", as.integer(format(asof, "%Y")))),
      to = min(
        as.Date(sprintf("%d-12-31", as.integer(format(asof, "%Y")))),
        asof
      ),
      season = NA_character_
    ),
    season_current = season_window(episodic_season_containing(asof)),
    season_previous = season_window(episodic_season_shift(
      episodic_season_containing(asof),
      1L
    )),
    last_12m = list(
      id = period,
      from = asof - 364,
      to = asof,
      season = NA_character_
    ),
    # 5 x 52 weeks rather than 5 x 365 days, so the window starts on the
    # same weekday it ends on and the weekly bins line up.
    last_5y = list(
      id = period,
      from = asof - (5 * 52 * 7 - 1),
      to = asof,
      season = NA_character_
    ),
    all = list(
      id = period,
      from = as.Date(data_from %||% (asof - 3650)),
      to = asof,
      season = NA_character_
    ),
    custom = list(
      id = period,
      from = episodic_as_single_date(from),
      to = episodic_as_single_date(to),
      season = NA_character_
    )
  )

  if (
    is.null(resolved) ||
      is.na(resolved$from) ||
      is.na(resolved$to) ||
      resolved$to < resolved$from
  ) {
    resolved <- list(
      id = "last_12m",
      from = asof - 364,
      to = asof,
      season = NA_character_
    )
  }

  if (is.null(resolved$previous)) {
    # For everything that is not a named season, the comparison period is
    # the equally long window ending the day before this one starts.
    span <- as.integer(resolved$to - resolved$from)
    resolved$previous <- list(
      from = resolved$from - span - 1,
      to = resolved$from - 1,
      label = NA_character_
    )
  }
  resolved
}

#' Everything the Pathogen screen shows for one pathogen over one period
#'
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen The pathogen to describe; defaults to the commonest.
#' @param period,from,to See `episodic_app_resolve_period()`.
#' @param lang Session language, for labels.
#' @return A list; see the source for which sections are populated and
#'   from where. Sections that cannot be computed are `NULL`, and their
#'   panels skip themselves - the same contract
#'   `episodic_cluster_object()` uses.
#' @keywords internal
#' @noRd
episodic_app_pathogen_screen <- function(
  con,
  pathogen = NULL,
  period = "year_current",
  from = NULL,
  to = NULL,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  options <- episodic_app_pathogen_options(con)
  asof <- episodic_app_data_asof(con)

  if (nrow(options) == 0) {
    return(list(
      pathogens = options,
      pathogen = NULL,
      asof = asof,
      period = episodic_app_resolve_period(period, from, to, asof)
    ))
  }
  if (is.null(pathogen) || !pathogen %in% options$pathogen) {
    pathogen <- options$pathogen[1]
  }

  data_from <- as.Date(options$first_day[options$pathogen == pathogen][1])
  resolved <- episodic_app_resolve_period(
    period,
    from,
    to,
    asof,
    data_from = data_from
  )

  all_cases <- episodic_db_cases_for_pathogen(con, pathogen)
  all_cases$sample_date <- as.Date(all_cases$sample_date)
  all_cases <- all_cases[!is.na(all_cases$sample_date), , drop = FALSE]
  window_cases <- all_cases[
    all_cases$sample_date >= resolved$from &
      all_cases$sample_date <= resolved$to, ,
    drop = FALSE
  ]

  pc <- episodic_db_pathogen_config_get(con, pathogen)
  region_stream_id <- episodic_app_pathogen_region_stream(con, pathogen)
  incomplete_days <- if (is.null(region_stream_id)) {
    0L
  } else {
    episodic_app_completeness(con, region_stream_id)$incomplete_days %||% 0L
  }

  seasonal <- !is.null(pc) && isTRUE(as.logical(pc$mem_applicable))

  list(
    pathogens = options,
    pathogen = pathogen,
    asof = asof,
    period = resolved,
    config = pc,
    seasonal = seasonal,
    incomplete_days = incomplete_days,
    summary = episodic_app_pathogen_summary(all_cases, window_cases, resolved),
    weekly = episodic_app_pathogen_weekly(
      window_cases,
      resolved,
      incomplete_days,
      asof
    ),
    mem = if (seasonal) {
      episodic_app_pathogen_mem(all_cases, resolved, asof)
    } else {
      NULL
    },
    overlay = episodic_app_pathogen_overlay(
      all_cases,
      resolved,
      seasonal = seasonal,
      asof = asof
    ),
    rt = episodic_app_pathogen_rt(
      all_cases,
      pc,
      resolved,
      incomplete_days,
      asof
    ),
    rt_unavailable_reason = episodic_rt_unavailable_reason(pc),
    denominator = episodic_app_pathogen_denominator(
      con,
      pathogen,
      all_cases,
      resolved
    ),
    demography = episodic_app_pathogen_demography(all_cases, window_cases),
    care_lines = episodic_app_pathogen_breakdown(
      window_cases,
      "care_line",
      lang = lang
    ),
    concentration = episodic_app_concentration(window_cases),
    institutions = episodic_app_pathogen_institutions(
      con,
      window_cases,
      lang = lang
    ),
    clusters = episodic_app_pathogen_clusters(
      con,
      pathogen,
      resolved,
      lang = lang
    )
  )
}

#' The `pathogen_region` (L5) stream for an pathogen, if one exists
#'
#' L5 is "this pathogen across the whole catchment", which is exactly the
#' population the Pathogen screen describes - so its reporting-completion
#' curve is the right one to read the screen's own trailing days against.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen The pathogen.
#' @return A stream id, or `NULL`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_region_stream <- function(con, pathogen) {
  found <- DBI::dbGetQuery(
    con,
    "SELECT stream_id FROM episodic_stream WHERE pathogen = ? AND level = 'pathogen_region'
      ORDER BY stream_id LIMIT 1",
    params = list(pathogen)
  )
  if (nrow(found) == 0) NULL else found$stream_id[1]
}

#' Headline counts for the selected pathogen and period
#'
#' @param all_cases Every case of the pathogen.
#' @param window_cases Those inside the period.
#' @param resolved The resolved period.
#' @return A list of scalars.
#' @keywords internal
#' @noRd
episodic_app_pathogen_summary <- function(all_cases, window_cases, resolved) {
  previous <- resolved$previous
  n_previous <- if (is.null(previous)) {
    NA_integer_
  } else {
    sum(
      all_cases$sample_date >= previous$from &
        all_cases$sample_date <= previous$to
    )
  }

  weekly <- if (nrow(window_cases) == 0) {
    data.frame(week_start = as.Date(character(0)), n_cases = integer(0))
  } else {
    episodic_app_weekly_counts(
      window_cases$sample_date,
      resolved$from,
      resolved$to
    )
  }
  peak_idx <- if (nrow(weekly) == 0 || all(weekly$n_cases == 0)) {
    NA_integer_
  } else {
    which.max(weekly$n_cases)
  }

  list(
    n_cases = nrow(window_cases),
    n_patients = length(unique(window_cases$patient_key)),
    n_weeks = nrow(weekly),
    peak_week = if (is.na(peak_idx)) NA else weekly$week_start[peak_idx],
    peak_n = if (is.na(peak_idx)) NA_integer_ else weekly$n_cases[peak_idx],
    median_weekly = if (nrow(weekly) == 0) {
      NA_real_
    } else {
      stats::median(weekly$n_cases)
    },
    n_previous = n_previous,
    previous_label = previous$label %||% NA_character_,
    # A percentage change is only meaningful against a non-zero base;
    # "up from nothing" is a sentence, not a number.
    change_pct = if (is.na(n_previous) || n_previous == 0) {
      NA_real_
    } else {
      round(100 * (nrow(window_cases) - n_previous) / n_previous)
    }
  )
}

#' Weekly case counts over a date range, zero-filled
#'
#' @param dates Case dates.
#' @param from,to The range bounds.
#' @return A data frame with `week_start` and `n_cases`, one row per ISO
#'   week in the range including empty ones.
#' @keywords internal
#' @noRd
episodic_app_weekly_counts <- function(dates, from, to) {
  floor_to_monday <- function(d) d - (as.integer(format(d, "%u")) - 1L)
  first_week <- floor_to_monday(as.Date(from))
  last_week <- floor_to_monday(as.Date(to))
  if (last_week < first_week) {
    last_week <- first_week
  }
  week_start <- seq(first_week, last_week, by = "week")
  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  # Indexed with `[` rather than iterated over directly: `vapply()` over a
  # Date vector hands the function bare numbers, and comparing those
  # against Dates only works by accident of the shared storage type.
  counts <- vapply(
    seq_along(week_start),
    function(i) {
      ws <- week_start[i]
      sum(dates >= ws & dates < ws + 7)
    },
    integer(1)
  )
  data.frame(week_start = week_start, n_cases = counts)
}

#' The weekly curve, with the still-filling weeks flagged
#'
#' Flagged at week level rather than day level, matching the chart's own
#' aggregation: a week is incomplete if any of its days falls inside the
#' stream's under-ascertained window.
#'
#' @param window_cases Cases inside the period.
#' @param resolved The resolved period.
#' @param incomplete_days From `episodic_app_completeness()`.
#' @param asof The date the data is current as of.
#' @return A data frame with `week_start`, `n_cases`, `incomplete`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_weekly <- function(
  window_cases,
  resolved,
  incomplete_days = 0L,
  asof = Sys.Date()
) {
  weekly <- episodic_app_weekly_counts(
    window_cases$sample_date,
    resolved$from,
    resolved$to
  )
  cutoff <- as.Date(asof) - as.integer(incomplete_days)
  weekly$incomplete <- weekly$week_start + 6 > cutoff
  weekly
}

#' Where this season sits against its MEM thresholds
#'
#' @param all_cases Every case of the pathogen.
#' @param resolved The resolved period.
#' @param asof The date the data is current as of.
#' @return A list with `season`, `thresholds`
#'   (`episodic_mem_thresholds_for_season()`), `status`
#'   (`episodic_mem_status()`, only when the period contains the present)
#'   and `peak_level` (the highest intensity band any week of the period
#'   reached), or `NULL` when nothing can be fitted.
#' @keywords internal
#' @noRd
episodic_app_pathogen_mem <- function(all_cases, resolved, asof = Sys.Date()) {
  season <- resolved$season
  if (is.na(season)) {
    season <- episodic_season_containing(resolved$to)
  }
  thresholds <- episodic_mem_thresholds_for_season(all_cases, season)
  if (is.null(thresholds)) {
    return(NULL)
  }

  # The live status is only meaningful when the period being looked at
  # actually reaches the present; on a historical season "the current
  # week" is not part of the picture at all.
  status <- if (resolved$to >= as.Date(asof) - 7) {
    episodic_mem_status(all_cases, run_date = asof)
  } else {
    NULL
  }

  weekly <- episodic_app_weekly_counts(
    all_cases$sample_date,
    resolved$from,
    resolved$to
  )
  peak_n <- if (nrow(weekly) == 0) NA_integer_ else max(weekly$n_cases)
  list(
    season = season,
    thresholds = thresholds,
    status = status,
    peak_n = peak_n,
    peak_level = episodic_mem_intensity_level(
      peak_n,
      thresholds$pre_epidemic,
      thresholds$intensity
    )
  )
}

#' This period's curve against the equivalent periods of earlier years
#'
#' The season-over-season overlay an epidemiologist actually draws by
#' hand: every year's weekly curve on one x axis of week-within-period,
#' so "early", "late", "bigger", "smaller" can be read directly instead
#' of inferred from two separate charts.
#'
#' Seasonal pathogens are overlaid by surveillance season (week 40 to
#' week 20) and everything else by calendar year, because a season
#' boundary drawn through October is meaningless for a food-borne
#' pathogen whose incidence peaks in August - it would cut every summer
#' peak in half and scatter it across two lines.
#'
#' @param all_cases Every case of the pathogen.
#' @param resolved The resolved period.
#' @param seasonal Whether to group by surveillance season or by calendar
#'   year.
#' @param max_periods How many earlier periods to draw.
#' @param asof The date the data is current as of. The period in progress
#'   is cut off after the week containing it, rather than being drawn
#'   flat along zero for the rest of the year. Every group is zero-filled
#'   across the whole period - which is right for a finished one, where a
#'   quiet week really did have no cases - but for the period still
#'   running it draws a long horizontal line at zero through weeks that
#'   have not happened yet, saying "no cases" where the truth is "not
#'   observed". On a chart whose whole purpose is comparing this period's
#'   shape against earlier ones, that line is the most visually
#'   prominent thing on it and it is not data.
#' @return A list with `kind`, `current` (the label of the period the
#'   selection falls in) and `rows` (a data frame: `group`, `week_index`,
#'   `week_label`, `n_cases`, `NA` beyond what has been observed), or
#'   `NULL` when fewer than two periods have data.
#' @keywords internal
#' @noRd
episodic_app_pathogen_overlay <- function(
  all_cases,
  resolved,
  seasonal = FALSE,
  max_periods = 6L,
  asof = NULL
) {
  if (nrow(all_cases) == 0) {
    return(NULL)
  }
  dates <- all_cases$sample_date

  if (isTRUE(seasonal)) {
    # `dates[i]` rather than iterating the vector directly: `[` keeps the
    # Date class, where handing elements straight to lapply is at the
    # mercy of how a classed atomic vector is subset.
    assigned <- lapply(seq_along(dates), function(i) {
      episodic_mem_season_week(dates[i])
    })
    group <- vapply(assigned, function(a) a$season, character(1))
    week_label <- vapply(assigned, function(a) a$week_label, character(1))
    week_order <- c(as.character(40:52), as.character(1:20))
    keep <- !is.na(group)
    current <- episodic_season_containing(resolved$to)
  } else {
    iso_year <- as.integer(format(dates, "%G"))
    iso_week <- pmin(as.integer(format(dates, "%V")), 52L)
    group <- as.character(iso_year)
    week_label <- as.character(iso_week)
    week_order <- as.character(1:52)
    keep <- rep(TRUE, length(dates))
    current <- as.character(as.integer(format(as.Date(resolved$to), "%G")))
  }

  group <- group[keep]
  week_label <- week_label[keep]
  if (length(group) == 0) {
    return(NULL)
  }

  groups <- sort(unique(group), decreasing = TRUE)
  # Keep the period being looked at plus the most recent ones before it,
  # so an overlay never quietly omits the very line the user selected.
  groups <- unique(c(current[current %in% groups], groups))
  groups <- utils::head(groups, max_periods)
  if (length(groups) < 2) {
    return(NULL)
  }

  rows <- do.call(
    rbind,
    lapply(groups, function(g) {
      counts <- vapply(
        week_order,
        function(w) sum(group == g & week_label == w),
        integer(1)
      )
      data.frame(
        group = g,
        week_index = seq_along(week_order),
        week_label = week_order,
        n_cases = as.integer(counts),
        stringsAsFactors = FALSE
      )
    })
  )
  rows <- episodic_app_overlay_truncate(
    rows,
    week_order,
    groups,
    seasonal = seasonal,
    asof = asof
  )

  list(
    kind = if (isTRUE(seasonal)) "season" else "year",
    current = current,
    groups = groups,
    rows = rows,
    # So the chart can crop its own axis to the same weeks the weekly
    # incidence panel is showing, see episodic_app_overlay_period_range().
    period_range = episodic_app_overlay_period_range(
      resolved,
      seasonal,
      current,
      week_order
    )
  )
}

#' The x-axis span (week index bounds) the selected period covers
#'
#' The overlay chart plots every group (season or year) across the whole
#' week_order axis, because comparing shapes needs the full run of
#' weeks - but the weekly incidence panel right above it only ever draws
#' the selected period. Left alone, picking a short custom range moves
#' one chart's x axis without moving the other, and the two stop lining
#' up. Cropping the overlay to the same span fixes that for any period
#' that falls inside a single season/year, which covers every preset
#' except the multi-year ones ("last 5 years", "all") - there the two
#' panels are answering different questions on purpose (this season's
#' shape vs. several years of it) and forcing one range onto the other
#' would misrepresent the overlay rather than align it.
#'
#' @param resolved The resolved period (`episodic_app_resolve_period()`).
#' @param seasonal Whether the overlay groups by surveillance season or
#'   calendar year.
#' @param current The group label the overlay is centred on.
#' @param week_order The axis labels, in plotted order.
#' @return An integer `c(from, to)` pair of `week_index` bounds, or
#'   `NULL` when the period does not resolve to a single span on this
#'   axis.
#' @keywords internal
#' @noRd
episodic_app_overlay_period_range <- function(
  resolved,
  seasonal,
  current,
  week_order
) {
  if (is.null(resolved$from) || is.null(resolved$to)) {
    return(NULL)
  }
  if (is.na(resolved$from) || is.na(resolved$to)) {
    return(NULL)
  }
  week_of <- function(d) {
    if (isTRUE(seasonal)) {
      wk <- episodic_mem_season_week(d)
      list(group = wk$season, label = wk$week_label)
    } else {
      list(
        group = format(as.Date(d), "%G"),
        label = as.character(pmin(as.integer(format(as.Date(d), "%V")), 52L))
      )
    }
  }
  from <- week_of(resolved$from)
  to <- week_of(resolved$to)
  if (
    is.na(from$group) || is.na(to$group) ||
      !identical(from$group, current) || !identical(to$group, current)
  ) {
    return(NULL)
  }
  idx_from <- match(from$label, week_order)
  idx_to <- match(to$label, week_order)
  if (is.na(idx_from) || is.na(idx_to) || idx_from > idx_to) {
    return(NULL)
  }
  c(idx_from, idx_to)
}

#' Blank the weeks of the period in progress that have not happened yet
#'
#' @param rows The zero-filled overlay rows.
#' @param week_order The week labels, in the order they are plotted.
#' @param groups The periods being drawn.
#' @param seasonal Whether periods are surveillance seasons or calendar
#'   years.
#' @param asof The date the data is current as of, or `NULL` to leave the
#'   rows alone.
#' @return `rows`, with `n_cases` set to `NA` past the current week of
#'   whichever period is still running. A finished period is untouched:
#'   its quiet weeks are observations, not gaps.
#' @keywords internal
#' @noRd
episodic_app_overlay_truncate <- function(
  rows,
  week_order,
  groups,
  seasonal = FALSE,
  asof = NULL
) {
  if (is.null(asof)) {
    return(rows)
  }
  asof <- as.Date(asof)
  if (is.na(asof)) {
    return(rows)
  }

  if (isTRUE(seasonal)) {
    current <- episodic_mem_season_week(asof)
    running <- current$season
    week <- current$week_label
  } else {
    running <- format(asof, "%G")
    week <- as.character(min(as.integer(format(asof, "%V")), 52L))
  }
  # Off-season, `episodic_mem_season_week()` reports no week at all, and
  # that is the right answer here too: the season it follows is over, so
  # every one of its weeks was genuinely observed.
  cutoff <- match(week, week_order)
  if (is.na(running) || is.na(cutoff) || !running %in% groups) {
    return(rows)
  }

  beyond <- rows$group == running & rows$week_index > cutoff
  rows$n_cases[beyond] <- NA_integer_
  rows
}

#' Pathogen-level Rt across the whole catchment
#'
#' This is the estimate that means something. Rt is a property of a
#' transmission process, and for influenza A the transmission process is
#' the region's influenza A - not the fourteen cases that happened to
#' reconcile into one ward cluster. Estimating it on a cluster's own
#' cases feeds `EpiEstim` a systematically incomplete incidence series:
#' the infections that seeded the cluster are outside it by construction,
#' so the renewal denominator is too small and Rt reads high. The
#' per-cluster panel is still worth having - "is this ward outbreak still
#' growing" is a real question - but it is the narrower of the two, and
#' it was the only one on offer.
#'
#' The series is deliberately built from a lead-in *before* the selected
#' period and then clipped back to it, so that the first weeks shown are
#' conditioned on real infection history rather than on the accident of
#' where the user set the date picker. Without that, picking "current
#' season" would put the season's opening weeks inside
#' `episodic_compute_rt()`'s own start-of-series burn-in and either drop
#' them or, worse, show them inflated.
#'
#' @param all_cases Every case of the pathogen.
#' @param pc The pathogen config row.
#' @param resolved The resolved period.
#' @param incomplete_days,asof Passed through to `episodic_compute_rt()`.
#' @param lead_in_days How much history before the period to condition on.
#' @return `episodic_compute_rt()`'s data frame clipped to the period, or
#'   `NULL`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_rt <- function(
  all_cases,
  pc,
  resolved,
  incomplete_days = 0L,
  asof = Sys.Date(),
  lead_in_days = 90L
) {
  if (is.null(pc) || nrow(all_cases) == 0) {
    return(NULL)
  }
  lead_in <- all_cases[
    all_cases$sample_date >= resolved$from - lead_in_days &
      all_cases$sample_date <= resolved$to, ,
    drop = FALSE
  ]
  if (nrow(lead_in) == 0) {
    return(NULL)
  }

  rt <- episodic_compute_rt(
    lead_in,
    pc,
    incomplete_days = incomplete_days,
    asof = asof
  )
  if (is.null(rt)) {
    return(NULL)
  }
  rt <- rt[rt$window_end >= resolved$from, , drop = FALSE]
  if (nrow(rt) == 0) {
    return(NULL)
  }
  rt
}

#' Region-wide tests and positivity over the period
#'
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen The pathogen.
#' @param all_cases Every case of the pathogen.
#' @param resolved The resolved period.
#' @return A data frame with `week_start`, `n_tests`, `n_cases`,
#'   `positivity`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_denominator <- function(
  con,
  pathogen,
  all_cases,
  resolved
) {
  denom <- episodic_db_denominator_for_pathogen(con, pathogen)
  if (nrow(denom) == 0) {
    return(NULL)
  }

  denom <- stats::aggregate(n_tests ~ sample_date, denom, sum)
  denom$week_start <- as.Date(denom$sample_date)
  denom <- denom[
    !is.na(denom$week_start) &
      denom$week_start <= resolved$to &
      denom$week_start + 6 >= resolved$from, ,
    drop = FALSE
  ]
  if (nrow(denom) < 2) {
    return(NULL)
  }
  denom <- denom[order(denom$week_start), ]

  denom$n_cases <- vapply(
    seq_len(nrow(denom)),
    function(i) {
      ws <- denom$week_start[i]
      sum(all_cases$sample_date >= ws & all_cases$sample_date < ws + 7)
    },
    integer(1)
  )
  denom$positivity <- ifelse(
    denom$n_tests > 0,
    denom$n_cases / denom$n_tests,
    NA
  )
  denom[, c("week_start", "n_tests", "n_cases", "positivity")]
}

#' Age and sex over the period, against the pathogen's own long-run
#' distribution
#'
#' A shifted age distribution is one of the earliest readable signs that
#' something has changed about an pathogen's epidemiology - a new
#' subtype, a waning vaccination cohort, a changed testing policy - and
#' it shows at population level long before any single cluster is big
#' enough to reveal it.
#'
#' @param all_cases Every case of the pathogen.
#' @param window_cases Those inside the period.
#' @return A list with `bands` (for `episodic_ui_pyramid()`),
#'   `median_age`, `baseline_median_age`, and `n_unknown_age`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_demography <- function(all_cases, window_cases) {
  if (nrow(window_cases) == 0 || all(is.na(window_cases$age))) {
    return(NULL)
  }
  # The baseline excludes the period being described, so the comparison
  # is against other periods rather than partly against itself.
  baseline <- all_cases[
    !(all_cases$case_id %in% window_cases$case_id), ,
    drop = FALSE
  ]
  baseline_ages <- baseline$age[!is.na(baseline$age)]

  list(
    bands = episodic_app_demography_bars(window_cases),
    median_age = stats::median(window_cases$age, na.rm = TRUE),
    baseline_median_age = if (length(baseline_ages) < 5) {
      NA_real_
    } else {
      stats::median(baseline_ages)
    },
    n_unknown_age = sum(is.na(window_cases$age))
  )
}

#' A labelled count breakdown of one case column
#'
#' @param window_cases Cases inside the period.
#' @param column The column to tabulate.
#' @param lang Session language; `care_line` values are translated.
#' @return A data frame with `label` and `n`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_breakdown <- function(
  window_cases,
  column,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (nrow(window_cases) == 0 || all(is.na(window_cases[[column]]))) {
    return(NULL)
  }
  tab <- sort(table(window_cases[[column]]), decreasing = TRUE)
  labels <- names(tab)
  if (identical(column, "care_line")) {
    labels <- vapply(
      labels,
      function(v) episodic_tr(paste0("careline.", v), lang = lang),
      character(1)
    )
  }
  data.frame(
    label = as.character(labels),
    n = as.integer(tab),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Which institutions the period's cases came from
#'
#' @param con A [DBI::DBIConnection-class].
#' @param window_cases Cases inside the period.
#' @param top How many to keep.
#' @param lang Session language, for the unknown-institution label.
#' @return A data frame with `label` and `n`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_institutions <- function(
  con,
  window_cases,
  top = 10L,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  ids <- window_cases$institution_id[!is.na(window_cases$institution_id)]
  if (length(ids) == 0) {
    return(NULL)
  }
  institutions <- episodic_db_institutions(con)
  display <- institutions$display_name[match(ids, institutions$institution_id)]
  display[is.na(display)] <- episodic_tr("misc.unknown", lang = lang)
  tab <- utils::head(sort(table(display), decreasing = TRUE), top)
  data.frame(
    label = names(tab),
    n = as.integer(tab),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Clusters of this pathogen that overlap the period
#'
#' The bridge back from the epidemiological view to the operational one:
#' having decided that a season is unusual, the next question is always
#' which signals were raised during it and what was made of them.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen The pathogen.
#' @param resolved The resolved period.
#' @param lang Session language.
#' @return A data frame with `cluster_id`, `level_label`, `place`,
#'   `first_day`, `last_day`, `n_cases`, `verdict_label`, `state_label`.
#' @keywords internal
#' @noRd
episodic_app_pathogen_clusters <- function(
  con,
  pathogen,
  resolved,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  empty <- data.frame(
    cluster_id = integer(0),
    level_label = character(0),
    place = character(0),
    first_day = character(0),
    last_day = character(0),
    n_cases = integer(0),
    verdict_label = character(0),
    state_label = character(0),
    stringsAsFactors = FALSE
  )
  clusters <- DBI::dbGetQuery(
    con,
    "SELECT c.cluster_id, c.stream_id, c.first_day, c.last_day, c.n_cases,
            c.priority_score, c.changed_since_assessment
       FROM episodic_cluster c
       JOIN episodic_stream s ON s.stream_id = c.stream_id
      WHERE s.pathogen = ? AND c.merged_into IS NULL
        AND c.last_day >= ? AND c.first_day <= ?
      ORDER BY c.first_day DESC",
    params = list(
      pathogen,
      as.character(resolved$from),
      as.character(resolved$to)
    )
  )
  if (nrow(clusters) == 0) {
    return(empty)
  }

  streams <- episodic_db_streams(con, active_only = FALSE)
  institutions <- episodic_db_institutions(con)

  # Both read once for the whole screen rather than once per cluster. Per
  # cluster this used to be an assessment-event query plus
  # `episodic_app_derive_state_for_cluster()`'s own five - one of which
  # re-fetched the very events already in hand - so a pathogen with a
  # hundred clusters spent hundreds of round trips here. Against a local
  # SQLite file that is invisible; against a database over the network it
  # is the whole render, and it is what made this screen take tens of
  # seconds to open.
  clusters$pathogen <- pathogen
  events_all <- episodic_db_assessment_events_batch(con, clusters$cluster_id)
  states <- episodic_app_derive_states_batch(con, clusters)

  rows <- lapply(seq_len(nrow(clusters)), function(i) {
    row <- clusters[i, ]
    stream <- streams[streams$stream_id == row$stream_id, ][1, ]
    institution <- if (!is.na(stream$institution_id)) {
      inst <- institutions[
        institutions$institution_id == stream$institution_id,
      ]
      if (nrow(inst) == 0) NULL else inst[1, ]
    } else {
      NULL
    }
    events <- events_all[events_all$cluster_id == row$cluster_id, ]
    verdicts <- events$verdict[!is.na(events$verdict)]
    verdict <- if (length(verdicts) == 0) {
      NA_character_
    } else {
      verdicts[length(verdicts)]
    }
    state <- states[i]

    data.frame(
      cluster_id = row$cluster_id,
      level_label = episodic_app_level_label(
        stream$level,
        stream$care_line,
        lang = lang
      ),
      place = episodic_app_place_label(stream, institution, lang = lang),
      first_day = row$first_day,
      last_day = row$last_day,
      n_cases = row$n_cases,
      verdict_label = if (is.na(verdict)) {
        episodic_tr("misc.dash", lang = lang)
      } else {
        episodic_verdict_label(verdict, level = stream$level, lang = lang)
      },
      state_label = episodic_tr(paste0("state.", state), lang = lang),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
