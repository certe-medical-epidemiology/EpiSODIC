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

#' App read models
#'
#' Every function here is a cheap read against an already-populated
#' database. Nothing here recomputes a statistical model; anything
#' expensive (Farrington, reconciliation, priority scoring) has already
#' happened in the cron and is simply looked up. This is the layer that
#' turns raw tables into the shapes the Shiny UI and the interpretation engine
#' consume.
#' @name app_read
NULL

#' Open clusters for the rail
#'
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language, for level/state labels.
#' @return A data frame, one row per open cluster, ordered by
#'   `priority_score` descending.
#' @keywords internal
#' @noRd
episode_app_open_clusters <- function(con, lang = "nl") {
  clusters <- episode_db_clusters(con, open_only = TRUE)
  if (nrow(clusters) == 0) {
    return(clusters[, c("cluster_id", "priority_score"), drop = FALSE])
  }
  streams <- episode_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(clusters$stream_id, streams$stream_id)]
  clusters$level <- streams$level[match(clusters$stream_id, streams$stream_id)]

  clusters$state <- vapply(clusters$cluster_id, function(id) {
    episode_app_derive_state_for_cluster(con, id)
  }, character(1))
  clusters$state_label <- vapply(clusters$state, function(s) episode_tr(paste0("state.", s), lang = lang), character(1))
  clusters$level_label <- vapply(clusters$level, function(lv) episode_tr(paste0("level.", lv), lang = lang), character(1))

  open <- clusters[clusters$state != "closed", ]
  open[order(-open$priority_score), ]
}

#' Derive the state of a single cluster
#'
#' Thin wrapper around `episode_derive_state()` that fetches the inputs
#' from the database: the cluster's assessment events, its
#' `changed_since_assessment` flag, the closure criterion, and whether it
#' has been explicitly closed.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return One of `episode_derive_state()`'s state strings.
#' @keywords internal
#' @noRd
episode_app_derive_state_for_cluster <- function(con, cluster_id) {
  events <- episode_db_assessment_events(con, cluster_id)
  cluster <- DBI::dbGetQuery(con, "SELECT * FROM episode_cluster WHERE cluster_id = ?",
                              params = list(cluster_id))
  if (nrow(cluster) == 0) return("new")
  cluster <- cluster[1, ]

  closure_met <- FALSE
  if (nrow(events) > 0 && any(!is.na(events$verdict))) {
    latest_verdict <- events$verdict[!is.na(events$verdict)]
    latest_verdict <- latest_verdict[length(latest_verdict)]
    stream <- DBI::dbGetQuery(con, "SELECT pathogen FROM episode_stream WHERE stream_id = ?",
                               params = list(cluster$stream_id))
    pc <- episode_db_pathogen_config_get(con, stream$pathogen[1])
    if (!is.null(pc)) {
      mem_status <- NULL
      if (isTRUE(as.logical(pc$mem_applicable))) {
        stream_cases <- episode_db_cases_for_pathogen(con, stream$pathogen[1])
        mem_status <- episode_mem_status(stream_cases)
      }
      closure_met <- episode_closure_criterion_met(
        cluster$last_day, latest_verdict, case_free_days = pc$case_free_days,
        incub_max_days = pc$incub_max_days, mem_applicable = as.logical(pc$mem_applicable),
        mem_status = mem_status
      )
    }
  }

  episode_derive_state(
    events, changed_since_assessment = as.logical(cluster$changed_since_assessment),
    closure_criterion_met = closure_met,
    explicitly_closed = episode_app_explicitly_closed(con, cluster_id, events)
  )
}

#' Whether a person explicitly closed a cluster
#'
#' A non-terminal verdict (`cluster_not_yet`, `possible_epidemic`,
#' `confirmed_epidemic`) can be closed by an assessor's decision alone,
#' without the classification itself changing - "an epidemic closes when
#' a person says so, whether or not any criterion has fired". That act is
#' recorded as an `episode_cluster_state` row (`trigger = "closure"`), not
#' as a new assessment event (the classification's own rationale already
#' exists; closure records that it is over, not what it was). It counts
#' as still in effect only if nothing has happened since - in practice, no
#' newer assessment event exists.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param events This cluster's assessment events, as returned by
#'   `episode_db_assessment_events()` (avoids a redundant query when the
#'   caller already has them).
#' @return A single logical.
#' @keywords internal
#' @noRd
episode_app_explicitly_closed <- function(con, cluster_id, events) {
  states <- episode_db_cluster_states(con, cluster_id)
  closures <- states[states$trigger %in% c("closure", "system") & states$state == "closed", ]
  if (nrow(closures) == 0) return(FALSE)
  latest_closure_at <- closures$entered_at[nrow(closures)]
  if (nrow(events) == 0) return(TRUE)
  latest_event_at <- events$created_at[nrow(events)]
  latest_closure_at >= latest_event_at
}

#' Build the cluster object consumed by the interpretation engine and the dossier
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param lang Session language.
#' @return A list; see the source for exactly which fields are populated
#'   and from where. Optional sections (`concentration`, `denominator`,
#'   `demography`) are `NULL` when there is not enough data to compute
#'   them, which is also what makes the corresponding interpretation slots and
#'   dossier panels skip themselves.
#' @keywords internal
#' @noRd
episode_cluster_object <- function(con, cluster_id, lang = "nl") {
  cluster <- DBI::dbGetQuery(con, "SELECT * FROM episode_cluster WHERE cluster_id = ?",
                              params = list(cluster_id))
  if (nrow(cluster) == 0) stop("No such cluster: ", cluster_id, call. = FALSE)
  cluster <- cluster[1, ]

  stream <- DBI::dbGetQuery(con, "SELECT * FROM episode_stream WHERE stream_id = ?",
                             params = list(cluster$stream_id))[1, ]
  institution <- if (!is.na(stream$institution_id)) {
    DBI::dbGetQuery(con, "SELECT * FROM episode_institution WHERE institution_id = ?",
                     params = list(stream$institution_id))[1, ]
  } else {
    NULL
  }
  pc <- episode_db_pathogen_config_get(con, stream$pathogen)

  cases <- episode_db_cluster_cases(con, cluster_id)
  detections <- DBI::dbGetQuery(
    con, "SELECT DISTINCT detector FROM episode_detection WHERE cluster_id = ?",
    params = list(cluster_id)
  )$detector

  place <- episode_app_place_label(stream, institution, lang = lang)
  completeness <- episode_app_completeness(con, stream$stream_id)

  list(
    id = cluster$cluster_id,
    stream_id = cluster$stream_id,
    pathogen = stream$pathogen,
    level = stream$level,
    care_line = stream$care_line,
    place = place,
    detectors = detections,
    first_day = cluster$first_day,
    last_day = cluster$last_day,
    opened_at = cluster$opened_at,
    n_cases = cluster$n_cases,
    expected = cluster$expected,
    ratio = cluster$ratio,
    priority_score = cluster$priority_score,
    changed_since_assessment = as.logical(cluster$changed_since_assessment),
    density = episode_app_density(con, stream, cases),
    doubling_days = episode_app_doubling_time(cases),
    concentration = episode_app_concentration(cases, stream$level),
    denominator = episode_app_denominator_summary(con, stream$pathogen, cases),
    demography = episode_app_demography_shift(con, stream$stream_id, cases),
    completeness = completeness,
    unique_patients = length(unique(cases$patient_key)),
    n_isolates = nrow(cases),
    case_free = list(
      since = if (nrow(cases) > 0) as.integer(Sys.Date() - max(as.Date(cases$sample_date))) else NA_integer_,
      need = if (!is.null(pc)) pc$case_free_days else NA_integer_
    ),
    rt_applicable = if (!is.null(pc)) as.logical(pc$rt_applicable) else FALSE,
    case_free_days = if (!is.null(pc)) pc$case_free_days else NA_integer_,
    mem_applicable = if (!is.null(pc)) as.logical(pc$mem_applicable) else FALSE,
    curve_shape = if (!is.null(pc)) episode_classify_curve_shape(cases, pc$incub_max_days) else NA_character_,
    rt = if (!is.null(pc)) episode_compute_rt(cases, pc, incomplete_days = completeness$incomplete_days %||% 0L) else NULL,
    rt_unavailable_reason = episode_rt_unavailable_reason(pc)
  )
}

#' Why the Rt panel has nothing to show, when `rt_applicable` is `TRUE`
#'
#' `episode_compute_rt()` deliberately collapses several distinct causes
#' into one `NULL` (its own docs: "no off-the-shelf message ... is a
#' substitute for simply not showing the panel"), which is right for
#' `episode_cluster_object()` itself but leaves the *displayed* empty
#' state unable to distinguish "this organism's config is missing a
#' serial interval" (a data-entry gap worth fixing) from "there simply
#' are not enough cases in this cluster yet" (expected, no action
#' needed) from "the `EpiEstim` package is not installed" (an
#' environment gap, not a data one). Cheap to determine directly from
#' `pc` without re-running the computation.
#'
#' @param pc A single-row pathogen config, or `NULL`.
#' @return One of `"no_serial_interval"`, `"epiestim_missing"`,
#'   `"insufficient_history"`, or `NA` if Rt is not applicable at all.
#' @keywords internal
#' @noRd
episode_rt_unavailable_reason <- function(pc) {
  if (is.null(pc) || !isTRUE(as.logical(pc$rt_applicable))) return(NA_character_)
  if (is.na(pc$si_mean_days) || is.na(pc$si_sd_days)) return("no_serial_interval")
  if (!requireNamespace("EpiEstim", quietly = TRUE)) return("epiestim_missing")
  "insufficient_history"
}

#' @keywords internal
#' @noRd
episode_app_place_label <- function(stream, institution, lang = "nl") {
  care_line_suffix <- if (!is.na(stream$care_line)) {
    paste0(" \u00b7 ", episode_tr(paste0("careline.", stream$care_line), lang = lang))
  } else {
    ""
  }
  if (stream$level == "pathogen_ward" && !is.null(institution)) {
    return(paste0(institution$display_name, " \u00b7 afdeling ", stream$ward))
  }
  if (stream$level == "pathogen_institution" && !is.null(institution)) {
    return(institution$display_name)
  }
  region_label <- episode_app_format_region(stream$region_code)
  paste0(region_label, care_line_suffix)
}

#' Cosmetically format an internal `region_code`
#'
#' `region_code` values (`"GEBIED-97"`, `"PROV_GRONINGEN"`,
#' `"NOORD_NEDERLAND"`) are internal synthetic-demo constructs, not real
#' place names; a real deployment resolves proper Gebied/Provincie naming
#' from its own operator-supplied region reference data (see
#' `R/lattice_enumerate.R`). This only tidies punctuation so the demo is
#' legible in the meantime.
#' @keywords internal
#' @noRd
episode_app_format_region <- function(region_code) {
  if (is.na(region_code)) return("")
  label <- gsub("[_-]", " ", region_code)
  tools::toTitleCase(tolower(label))
}

#' @keywords internal
#' @noRd
episode_app_density <- function(con, stream, cases) {
  if (is.na(stream$institution_id) || nrow(cases) == 0) return(NULL)
  activity <- episode_db_institution_activity(con, stream$institution_id)
  if (nrow(activity) == 0) return(NULL)
  latest <- activity[nrow(activity), ]
  if (is.na(latest$patient_days) || latest$patient_days == 0) return(NULL)

  # Historical baseline: this stream's own long-run density, all known
  # cases against the sum of patient-days over the same calendar span -
  # not a fitted model (that is farringtonFlexible's own populationOffset
  # baseline), just the descriptive rate the cluster's own density is
  # being read against.
  all_cases <- DBI::dbGetQuery(
    con, "SELECT sample_date FROM episode_case WHERE pathogen = ? AND institution_id = ?",
    params = list(stream$pathogen, stream$institution_id)
  )
  baseline <- NA_real_
  if (nrow(all_cases) >= 5) {
    span_start <- min(as.Date(all_cases$sample_date))
    span_activity <- activity[as.Date(activity$period_end) >= span_start, ]
    total_patient_days <- sum(span_activity$patient_days, na.rm = TRUE)
    if (total_patient_days > 0) {
      baseline <- round(nrow(all_cases) / total_patient_days * 1000, 2)
    }
  }

  list(
    value = round(nrow(cases) / latest$patient_days * 1000, 2),
    baseline = baseline
  )
}

#' Simple doubling time from the case date distribution
#'
#' A cheap linear regression of log(cumulative cases) against day, over
#' the last 14 days of the cluster - not a fitted epidemic model (that
#' is Rt), just a descriptive rate.
#' @keywords internal
#' @noRd
episode_app_doubling_time <- function(cases) {
  if (nrow(cases) < 3) return(NA_real_)
  dates <- sort(as.Date(cases$sample_date))
  recent <- dates[dates >= max(dates) - 14]
  if (length(unique(recent)) < 2) return(NA_real_)
  days <- as.numeric(recent - min(recent))
  cum_n <- seq_along(recent)
  fit <- tryCatch(stats::lm(log(cum_n) ~ days), error = function(e) NULL)
  if (is.null(fit) || is.na(stats::coef(fit)[2]) || stats::coef(fit)[2] <= 0) return(NA_real_)
  round(log(2) / stats::coef(fit)[2], 1)
}

#' Where the cluster concentrates geographically (by PC4)
#'
#' Deliberately always PC4, not ward/institution: for a ward-level (L1)
#' cluster, every case already shares that ward by construction (the
#' stream itself is scoped to it), so grouping by ward there would be
#' tautological (100% "concentration" by definition, saying nothing).
#' PC4 concentration is the one dimension that is informative at every
#' lattice level.
#' @keywords internal
#' @noRd
episode_app_concentration <- function(cases, level) {
  if (nrow(cases) == 0 || all(is.na(cases$pc4))) return(NULL)
  tab <- table(cases$pc4)
  tab <- tab[order(-tab)]
  list(
    dominant_label = names(tab)[1],
    dominant_n = as.integer(tab[1]),
    dominant_share = as.numeric(tab[1]) / nrow(cases),
    total = nrow(cases),
    rows = data.frame(label = names(tab), n = as.integer(tab), row.names = NULL)
  )
}

#' Positivity summary from the optional denominator table
#' @keywords internal
#' @noRd
episode_app_denominator_summary <- function(con, pathogen, cases) {
  denom <- episode_db_denominator_for_pathogen(con, pathogen)
  if (nrow(denom) == 0 || nrow(cases) == 0) return(NULL)

  series <- episode_app_denominator_series(con, pathogen, cases)
  if (nrow(series) < 2) return(NULL)

  list(
    n_tests_first = series$n_tests[1], n_tests_last = series$n_tests[nrow(series)],
    positivity_first = series$positivity[1], positivity_last = series$positivity[nrow(series)],
    series = series
  )
}

#' Weekly (n_tests, n_cases, positivity) series aligned for charting
#' @keywords internal
#' @noRd
episode_app_denominator_series <- function(con, pathogen, cases) {
  denom <- episode_db_denominator_for_pathogen(con, pathogen)
  if (nrow(denom) == 0) {
    return(data.frame(week_start = as.Date(character(0)), n_tests = integer(0),
                       n_cases = integer(0), positivity = numeric(0)))
  }
  denom <- stats::aggregate(n_tests ~ sample_date, denom, sum)
  denom$week_start <- as.Date(denom$sample_date)
  case_dates <- as.Date(cases$sample_date)
  denom$n_cases <- vapply(denom$week_start, function(ws) sum(case_dates >= ws & case_dates < ws + 7), integer(1))
  denom$positivity <- ifelse(denom$n_tests > 0, denom$n_cases / denom$n_tests, NA)
  denom <- denom[order(denom$week_start), ]
  denom[, c("week_start", "n_tests", "n_cases", "positivity")]
}

#' Whether the cluster's age distribution has shifted from the stream baseline
#' @keywords internal
#' @noRd
episode_app_demography_shift <- function(con, stream_id, cases) {
  if (nrow(cases) == 0 || all(is.na(cases$age))) return(NULL)

  bands <- c("0-19", "20-39", "40-59", "60-79", "80+")
  band_of <- function(age) {
    cut(age, breaks = c(-1, 19, 39, 59, 79, Inf), labels = bands)
  }

  cluster_band <- band_of(cases$age)
  cluster_dominant <- names(sort(-table(cluster_band)))[1]

  stream_pathogen <- DBI::dbGetQuery(con, "SELECT pathogen FROM episode_stream WHERE stream_id = ?",
                                      params = list(stream_id))$pathogen[1]
  all_cases <- DBI::dbGetQuery(
    con, "SELECT age FROM episode_case WHERE pathogen = ?",
    params = list(stream_pathogen)
  )
  if (nrow(all_cases) < 5 || all(is.na(all_cases$age))) {
    return(list(shifted = FALSE, dominant_band = as.character(cluster_dominant), baseline_band = NA,
                bands = episode_app_demography_bars(cases)))
  }
  baseline_band_tab <- band_of(all_cases$age)
  baseline_dominant <- names(sort(-table(baseline_band_tab)))[1]

  list(
    shifted = !identical(cluster_dominant, baseline_dominant),
    dominant_band = as.character(cluster_dominant),
    baseline_band = as.character(baseline_dominant),
    bands = episode_app_demography_bars(cases)
  )
}

#' Age/sex pyramid bars: cluster counts (no baseline overlay)
#' @keywords internal
#' @noRd
episode_app_demography_bars <- function(cases) {
  bands <- c("0-19", "20-39", "40-59", "60-79", "80+")
  band_of <- function(age) cut(age, breaks = c(-1, 19, 39, 59, 79, Inf), labels = bands)
  cases$band <- band_of(cases$age)
  out <- data.frame(band = bands, stringsAsFactors = FALSE)
  out$m <- vapply(bands, function(b) sum(cases$band == b & cases$sex == "M", na.rm = TRUE), integer(1))
  out$v <- vapply(bands, function(b) sum(cases$band == b & cases$sex == "F", na.rm = TRUE), integer(1))
  out
}

#' Reporting-triangle-derived incomplete window for the epi curve shading
#' @keywords internal
#' @noRd
episode_app_completeness <- function(con, stream_id) {
  completeness <- episode_triangle_completeness(con, stream_id)
  if (nrow(completeness) == 0) return(list(incomplete_days = 0L))
  incomplete <- completeness[completeness$completeness < 0.95, ]
  list(incomplete_days = if (nrow(incomplete) == 0) 0L else max(incomplete$lag_days))
}

#' Daily case counts for the epi curve panel, with an incomplete flag
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return A data frame with `sample_date`, `n_cases`, `incomplete`.
#' @keywords internal
#' @noRd
episode_app_epi_curve <- function(con, cluster_id) {
  cases <- episode_db_cluster_cases(con, cluster_id)
  if (nrow(cases) == 0) {
    return(data.frame(sample_date = as.Date(character(0)), n_cases = integer(0), incomplete = logical(0)))
  }
  cluster <- DBI::dbGetQuery(con, "SELECT stream_id FROM episode_cluster WHERE cluster_id = ?",
                              params = list(cluster_id))
  incomplete_days <- episode_app_completeness(con, cluster$stream_id[1])$incomplete_days

  dates <- as.Date(cases$sample_date)
  all_days <- seq(min(dates), max(dates), by = "day")
  counts <- vapply(all_days, function(d) sum(dates == d), integer(1))
  data.frame(
    sample_date = all_days, n_cases = counts,
    incomplete = all_days > (max(all_days) - incomplete_days)
  )
}

#' Multi-year trend data for a stream (the cron-persisted chart cache)
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id A stream id.
#' @return `episode_db_stream_trend()`'s output, capped to the last 156
#'   weeks (matching `episode_farrington_trend()`'s own backfill cap).
#' @keywords internal
#' @noRd
episode_app_trend <- function(con, stream_id) {
  trend <- episode_db_stream_trend(con, stream_id)
  if (nrow(trend) == 0) return(trend)
  trend <- trend[order(trend$week_start), ]
  utils::tail(trend, 156)
}

#' Line list rows for the dossier's line list panel
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return A data frame with exactly the fields the line list is allowed
#'   to show.
#' @keywords internal
#' @noRd
episode_app_linelist <- function(con, cluster_id) {
  cases <- episode_db_cluster_cases(con, cluster_id)
  if (nrow(cases) == 0) return(cases)
  cases[order(cases$sample_date), c(
    "source_key", "sample_date", "sex", "age", "pc4", "ward", "specialism"
  )]
}

#' Detection settings for the dossier's settings panel
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return A named list of display-ready values.
#' @keywords internal
#' @noRd
episode_app_detection_settings <- function(con, cluster_id) {
  cluster_obj <- episode_cluster_object(con, cluster_id)
  run <- episode_db_latest_run(con, status = "success")
  list(
    detectors = cluster_obj$detectors,
    rt_applicable = cluster_obj$rt_applicable,
    aggregation = "week",
    population_offset = if (!is.null(cluster_obj$density)) "patient_days" else NULL,
    case_free_days = cluster_obj$case_free_days,
    last_run_when = if (!is.null(run)) run$finished_at else NA,
    last_run_host = if (!is.null(run)) run$host else NA,
    pkg_versions = if (!is.null(run) && !is.na(run$pkg_versions)) run$pkg_versions else NA
  )
}

#' Read-only Streams screen data
#'
#' Displays the configuration from the latest run's `config_snapshot`, not
#' from the file.
#'
#' Paginated, and deliberately at the read-model level rather than only
#' in the UI: `baseline_excluded` is one DB round trip per stream (via
#' `episode_baseline_excluded_windows()`, itself one round trip per
#' cluster in that stream), so computing it for every stream regardless
#' of what is actually shown made this screen slow to load once a real
#' instance's stream count grew past a few dozen - the exact bug report
#' this was written to fix. Slicing to `page` before that loop runs means
#' the cost is bounded by `page_size`, not by the total stream count.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param page 1-based page number.
#' @param page_size Streams per page.
#' @return A list with `streams` (a data frame, one page's worth),
#'   `total` (the total stream count across all pages), `page`,
#'   `page_size`, `n_pages`, and `config_snapshot` (the parsed JSON from
#'   the latest successful run, or `NULL`).
#' @keywords internal
#' @noRd
episode_app_streams_screen <- function(con, page = 1L, page_size = 50L) {
  streams_all <- episode_db_streams(con, active_only = FALSE)
  run <- episode_db_latest_run(con, status = "success")
  config_snapshot <- if (!is.null(run) && !is.na(run$config_snapshot)) {
    jsonlite::fromJSON(run$config_snapshot)
  } else {
    NULL
  }

  total <- nrow(streams_all)
  n_pages <- max(1L, ceiling(total / page_size))
  page <- min(max(1L, page), n_pages)
  from <- (page - 1L) * page_size + 1L
  to <- min(total, page * page_size)
  streams <- if (total == 0) streams_all else streams_all[from:to, , drop = FALSE]

  # Excluded windows are listed on the Streams screen so a baseline is
  # never quietly different from what an assessor expects. Computed only
  # for this page's streams.
  if (nrow(streams) > 0) {
    streams$baseline_excluded <- lapply(streams$stream_id, function(sid) {
      episode_baseline_excluded_windows(con, sid)
    })
  }
  list(streams = streams, total = total, page = page, page_size = page_size,
       n_pages = n_pages, config_snapshot = config_snapshot, run = run)
}

#' Status strip data: last run status and reporting completeness
#'
#' Always visible: a silently failed detection run is the system's main
#' operational risk, and must never be something an operator has to go
#' looking for.
#'
#' @param con A [DBI::DBIConnection-class].
#' @return A list describing the latest run.
#' @keywords internal
#' @noRd
episode_app_status <- function(con) {
  run <- episode_db_latest_run(con)
  if (is.null(run)) return(list(status = "none"))
  n_clusters <- nrow(episode_db_clusters(con, open_only = TRUE))
  list(
    status = run$status, finished_at = run$finished_at, n_streams = run$n_streams,
    n_detections = run$n_detections, n_clusters_open = n_clusters
  )
}
