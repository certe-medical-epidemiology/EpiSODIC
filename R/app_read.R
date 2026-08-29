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

# App read models: every function here is a cheap read against an
# already-populated database. Nothing here recomputes a statistical
# model; anything expensive (Farrington, reconciliation, priority
# scoring) has already happened in the cron and is simply looked up.
# This is the layer that turns raw tables into the shapes the Shiny UI
# and the interpretation engine consume.

#' Open clusters for the rail
#'
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language, for level/state labels.
#' @return A data frame, one row per open cluster, ordered by `last_day`
#'   descending (newest last case day first).
#' @keywords internal
#' @noRd
episodic_app_open_clusters <- function(
  con,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  clusters <- episodic_db_clusters(con, open_only = TRUE)
  if (nrow(clusters) == 0) {
    return(clusters[, c("cluster_id", "priority_score"), drop = FALSE])
  }
  streams <- episodic_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(
    clusters$stream_id,
    streams$stream_id
  )]
  clusters$level <- streams$level[match(clusters$stream_id, streams$stream_id)]
  clusters$care_line <- streams$care_line[match(
    clusters$stream_id,
    streams$stream_id
  )]

  clusters$state <- episodic_app_derive_states_batch(con, clusters)
  clusters$state_label <- vapply(
    clusters$state,
    function(s) episodic_tr(paste0("state.", s), lang = lang),
    character(1)
  )
  # Plain here - the rail shows the care line as its own chip beside the
  # pathogen name rather than folded into this text (see episodic_ui_rail()).
  clusters$level_label <- vapply(
    clusters$level,
    function(lv) episodic_tr(paste0("level.", lv), lang = lang),
    character(1)
  )

  open <- clusters[clusters$state != "closed", ]
  # Newest last case day first - the rail is a triage queue, and a cluster
  # that just gained a case is more likely to need attention right now
  # than one that scored higher on priority but has gone quiet.
  open[order(as.Date(open$last_day), decreasing = TRUE), ]
}

#' Derive state for many clusters at once
#'
#' What the rail actually needs from `episodic_app_derive_state_for_cluster()`,
#' but without its one-query-per-cluster cost: `episodic_db_assessment_events()`,
#' `episodic_db_cluster_states()` and (for a `mem_applicable` pathogen)
#' `episodic_db_cases_for_pathogen()` are each fetched once per distinct
#' cluster/pathogen here instead of once per cluster, which is what made
#' the rail's own render cost scale with the number of open clusters
#' rather than being flat.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param clusters A data frame with (at least) `cluster_id`, `pathogen`,
#'   `last_day` and `changed_since_assessment` columns - `episodic_db_clusters()`'s
#'   own shape, with `pathogen` joined in from `episodic_db_streams()`, is
#'   exactly this.
#' @return A character vector of states, one per row of `clusters`, in the
#'   same order.
#' @keywords internal
#' @noRd
episodic_app_derive_states_batch <- function(con, clusters) {
  if (nrow(clusters) == 0) {
    return(character(0))
  }

  ids <- clusters$cluster_id
  events_all <- episodic_db_assessment_events_batch(con, ids)
  states_all <- episodic_db_cluster_states_batch(con, ids)
  pathogen_config <- episodic_db_pathogen_config(con)

  # mem_status is only worth computing for pathogens that are both
  # mem_applicable and actually have a non-terminal latest verdict among
  # these clusters - episodic_db_cases_for_pathogen() reads every case for
  # the pathogen, so it is memoised per pathogen rather than per cluster.
  mem_cache <- new.env(parent = emptyenv())
  mem_status_for <- function(pathogen) {
    if (!exists(pathogen, envir = mem_cache, inherits = FALSE)) {
      status <- episodic_mem_status(episodic_db_cases_for_pathogen(
        con,
        pathogen
      ))
      assign(pathogen, status, envir = mem_cache)
    }
    get(pathogen, envir = mem_cache, inherits = FALSE)
  }

  vapply(
    seq_len(nrow(clusters)),
    function(i) {
      row <- clusters[i, ]
      events <- events_all[events_all$cluster_id == row$cluster_id, ]
      states <- states_all[states_all$cluster_id == row$cluster_id, ]

      closure_met <- FALSE
      if (nrow(events) > 0 && any(!is.na(events$verdict))) {
        latest_verdict <- events$verdict[!is.na(events$verdict)]
        latest_verdict <- latest_verdict[length(latest_verdict)]
        pc <- pathogen_config[pathogen_config$pathogen == row$pathogen, ]
        if (nrow(pc) > 0) {
          pc <- pc[1, ]
          mem_status <- NULL
          if (isTRUE(as.logical(pc$mem_applicable))) {
            mem_status <- mem_status_for(row$pathogen)
          }
          closure_met <- episodic_closure_criterion_met(
            row$last_day,
            latest_verdict,
            case_free_days = pc$case_free_days,
            incub_max_days = pc$incub_max_days,
            mem_applicable = as.logical(pc$mem_applicable),
            mem_status = mem_status
          )
        }
      }

      episodic_derive_state(
        events,
        changed_since_assessment = as.logical(row$changed_since_assessment),
        closure_criterion_met = closure_met,
        explicitly_closed = episodic_app_explicitly_closed_from(
          states,
          events
        )
      )
    },
    character(1)
  )
}

#' Derive the state of a single cluster
#'
#' Thin wrapper around `episodic_derive_state()` that fetches the inputs
#' from the database: the cluster's assessment events, its
#' `changed_since_assessment` flag, the closure criterion, and whether it
#' has been explicitly closed.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return One of `episodic_derive_state()`'s state strings.
#' @keywords internal
#' @noRd
episodic_app_derive_state_for_cluster <- function(con, cluster_id) {
  events <- episodic_db_assessment_events(con, cluster_id)
  cluster <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(cluster) == 0) {
    return("new")
  }
  cluster <- cluster[1, ]

  closure_met <- FALSE
  if (nrow(events) > 0 && any(!is.na(events$verdict))) {
    latest_verdict <- events$verdict[!is.na(events$verdict)]
    latest_verdict <- latest_verdict[length(latest_verdict)]
    stream <- DBI::dbGetQuery(
      con,
      "SELECT pathogen FROM episodic_stream WHERE stream_id = ?",
      params = list(cluster$stream_id)
    )
    pc <- episodic_db_pathogen_config_get(con, stream$pathogen[1])
    if (!is.null(pc)) {
      mem_status <- NULL
      if (isTRUE(as.logical(pc$mem_applicable))) {
        stream_cases <- episodic_db_cases_for_pathogen(con, stream$pathogen[1])
        mem_status <- episodic_mem_status(stream_cases)
      }
      closure_met <- episodic_closure_criterion_met(
        cluster$last_day,
        latest_verdict,
        case_free_days = pc$case_free_days,
        incub_max_days = pc$incub_max_days,
        mem_applicable = as.logical(pc$mem_applicable),
        mem_status = mem_status
      )
    }
  }

  episodic_derive_state(
    events,
    changed_since_assessment = as.logical(cluster$changed_since_assessment),
    closure_criterion_met = closure_met,
    explicitly_closed = episodic_app_explicitly_closed(con, cluster_id, events)
  )
}

#' Whether a person explicitly closed a cluster
#'
#' A non-terminal verdict (`cluster_not_yet`, `possible_epidemic`,
#' `confirmed_epidemic`) can be closed by an epidemiologist's decision alone,
#' without the classification itself changing - "an epidemic closes when
#' a person says so, whether or not any criterion has fired". That act is
#' recorded as an `episodic_cluster_state` row (`trigger = "closure"`), not
#' as a new assessment event (the classification's own rationale already
#' exists; closure records that it is over, not what it was). It counts
#' as still in effect only if nothing has happened since - in practice, no
#' newer assessment event exists.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param events This cluster's assessment events, as returned by
#'   `episodic_db_assessment_events()` (avoids a redundant query when the
#'   caller already has them).
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_app_explicitly_closed <- function(con, cluster_id, events) {
  episodic_app_explicitly_closed_from(
    episodic_db_cluster_states(con, cluster_id),
    events
  )
}

#' The actual logic behind `episodic_app_explicitly_closed()`, given states
#'
#' Split out so a caller deriving many clusters' state at once (the rail's
#' `episodic_app_derive_states_batch()`) can pass in states it already
#' fetched in bulk, rather than one `episodic_db_cluster_states()` query
#' per cluster.
#' @param states This cluster's rows from `episodic_db_cluster_states()`.
#' @param events This cluster's assessment events.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_app_explicitly_closed_from <- function(states, events) {
  closures <- states[
    states$trigger %in% c("closure", "system") & states$state == "closed",
  ]
  if (nrow(closures) == 0) {
    return(FALSE)
  }
  latest_closure_at <- closures$entered_at[nrow(closures)]
  if (nrow(events) == 0) {
    return(TRUE)
  }
  latest_event_at <- events$created_at[nrow(events)]
  latest_closure_at >= latest_event_at
}

#' When each of many clusters was last closed, from states fetched in bulk
#'
#' The archive and the similar-clusters panel both show a "closed on"
#' column, and both used to fill it with one
#' `episodic_db_cluster_states()` query per row. Given
#' `episodic_db_cluster_states_batch()`'s output - already ordered by
#' `cluster_id, entered_at, state_id`, so a cluster's last closure is its
#' last row - the same answer is a `match()`.
#'
#' @param states_all Rows from `episodic_db_cluster_states_batch()`.
#' @param cluster_ids The cluster ids to answer for, in the order wanted.
#' @return A character vector of `entered_at` values, one per id, `NA` for
#'   a cluster with no recorded closure.
#' @keywords internal
#' @noRd
episodic_app_closed_at_from <- function(states_all, cluster_ids) {
  closures <- states_all[states_all$state == "closed", ]
  if (nrow(closures) == 0) {
    return(rep(NA_character_, length(cluster_ids)))
  }
  last <- closures[!duplicated(closures$cluster_id, fromLast = TRUE), ]
  as.character(last$entered_at[match(cluster_ids, last$cluster_id)])
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
episodic_cluster_object <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  cluster <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(cluster) == 0) {
    stop("No such cluster: ", cluster_id, call. = FALSE)
  }
  cluster <- cluster[1, ]

  stream <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_stream WHERE stream_id = ?",
    params = list(cluster$stream_id)
  )[1, ]
  institution <- if (!is.na(stream$institution_id)) {
    DBI::dbGetQuery(
      con,
      "SELECT * FROM episodic_institution WHERE institution_id = ?",
      params = list(stream$institution_id)
    )[1, ]
  } else {
    NULL
  }
  pc <- episodic_db_pathogen_config_get(con, stream$pathogen)

  cases <- episodic_db_cluster_cases(con, cluster_id)
  detections <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT detector FROM episodic_detection WHERE cluster_id = ?",
    params = list(cluster_id)
  )$detector

  place <- episodic_app_place_label(stream, institution, lang = lang)
  completeness <- episodic_app_completeness(con, stream$stream_id)
  asof <- episodic_app_data_asof(con)

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
    density = episodic_app_density(con, stream, cases),
    doubling_days = episodic_app_doubling_time(
      cases,
      incomplete_days = completeness$incomplete_days %||% 0L,
      asof = asof
    ),
    concentration = episodic_app_concentration(cases, stream$level),
    denominator = episodic_app_denominator_summary(con, stream$pathogen, cases),
    asof = asof,
    demography = episodic_app_demography_shift(con, stream$stream_id, cases),
    completeness = completeness,
    unique_patients = length(unique(cases$patient_key)),
    n_positives = nrow(cases),
    case_free = list(
      since = if (nrow(cases) > 0) {
        as.integer(Sys.Date() - max(as.Date(cases$sample_date)))
      } else {
        NA_integer_
      },
      need = if (!is.null(pc)) pc$case_free_days else NA_integer_
    ),
    rt_applicable = if (!is.null(pc)) as.logical(pc$rt_applicable) else FALSE,
    case_free_days = if (!is.null(pc)) pc$case_free_days else NA_integer_,
    mem_applicable = if (!is.null(pc)) as.logical(pc$mem_applicable) else FALSE,
    curve_shape = if (!is.null(pc)) {
      episodic_classify_curve_shape(cases, pc$incub_max_days)
    } else {
      NA_character_
    },
    rt = if (!is.null(pc)) {
      episodic_compute_rt(
        cases,
        pc,
        incomplete_days = completeness$incomplete_days %||% 0L,
        asof = asof
      )
    } else {
      NULL
    },
    rt_unavailable_reason = episodic_rt_unavailable_reason(pc)
  )
}

#' Why the Rt panel has nothing to show, when `rt_applicable` is `TRUE`
#'
#' `episodic_compute_rt()` deliberately collapses several distinct causes
#' into one `NULL` (its own docs: "no off-the-shelf message ... is a
#' substitute for simply not showing the panel"), which is right for
#' `episodic_cluster_object()` itself but leaves the *displayed* empty
#' state unable to distinguish "this pathogen's config is missing a
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
episodic_rt_unavailable_reason <- function(pc) {
  if (is.null(pc) || !isTRUE(as.logical(pc$rt_applicable))) {
    return(NA_character_)
  }
  if (is.na(pc$si_mean_days) || is.na(pc$si_sd_days)) {
    return("no_serial_interval")
  }
  if (!requireNamespace("EpiEstim", quietly = TRUE)) {
    return("epiestim_missing")
  }
  "insufficient_history"
}

#' The level chip label, prefixed with the care line where known
#'
#' `"L2 \u00b7 instelling"` on its own tells an epidemiologist nothing
#' about whether the institution is a GP practice or a hospital ward;
#' prepending the care line (`"1e lijn"`/`"2e lijn"`) answers that in the
#' same glance the level already earns. `NA` (no care line recorded for
#' the stream) leaves the label unprefixed rather than printing
#' "unknown".
#'
#' @param level A stream level, e.g. `"pathogen_institution"`.
#' @param care_line A stream `care_line` (`"first"`, `"second"`,
#'   `"other"`, `"unknown"`, or `NA`).
#' @param lang Session language.
#' @return A single string.
#' @keywords internal
#' @noRd
episodic_app_level_label <- function(
  level,
  care_line = NA,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  label <- episodic_tr(paste0("level.", level), lang = lang)
  if (is.na(care_line)) {
    return(label)
  }
  paste0(
    episodic_tr(paste0("careline.short.", care_line), lang = lang),
    " \u00b7 ",
    label
  )
}

#' @keywords internal
#' @noRd
episodic_app_place_label <- function(
  stream,
  institution,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  care_line_suffix <- if (!is.na(stream$care_line)) {
    paste0(
      " \u00b7 ",
      episodic_tr(paste0("careline.", stream$care_line), lang = lang)
    )
  } else {
    ""
  }
  if (stream$level == "pathogen_ward" && !is.null(institution)) {
    return(paste0(institution$display_name, " \u00b7 afdeling ", stream$ward))
  }
  if (stream$level == "pathogen_institution" && !is.null(institution)) {
    return(institution$display_name)
  }
  region_label <- episodic_app_format_region(stream$region_code)
  paste0(region_label, care_line_suffix)
}

#' Cosmetically format an internal `region_code`
#'
#' `region_code` values (`"GEBIED-97"`, `"PROV_GRONINGEN"`,
#' `"NORTHERN_NETHERLANDS"`) are internal synthetic-demo constructs, not real
#' place names; a real deployment resolves proper Gebied/Provincie naming
#' from its own operator-supplied region reference data (see
#' `R/lattice_enumerate.R`). This only tidies punctuation so the demo is
#' legible in the meantime.
#' @keywords internal
#' @noRd
episodic_app_format_region <- function(region_code) {
  if (is.na(region_code)) {
    return("")
  }
  label <- gsub("[_-]", " ", region_code)
  tools::toTitleCase(tolower(label))
}

#' @param debug If `TRUE`, print the exact SQL and bound parameters for
#'   every query this function issues, via `episodic_trace_query()`, and
#'   print `message()`s of its own progress via `episodic_trace()`. Only
#'   ever passed by `episodic_run_cron()`'s own `debug` argument; every
#'   other caller (the dossier's own density stat) leaves this `FALSE`.
#' @keywords internal
#' @noRd
episodic_app_density <- function(con, stream, cases, debug = FALSE) {
  if (is.na(stream$institution_id) || nrow(cases) == 0) {
    return(NULL)
  }
  episodic_trace_query(
    debug,
    "SELECT * FROM episodic_institution_activity WHERE institution_id = ? ORDER BY period_start",
    list(stream$institution_id)
  )
  activity <- episodic_db_institution_activity(con, stream$institution_id)
  episodic_trace_debug(debug, "debug:         institution_activity query done")
  if (nrow(activity) == 0) {
    return(NULL)
  }
  latest <- activity[nrow(activity), ]
  if (is.na(latest$patient_days) || latest$patient_days == 0) {
    return(NULL)
  }

  # Historical baseline: this stream's own long-run density, all known
  # cases against the sum of patient-days over the same calendar span -
  # not a fitted model (that is farringtonFlexible's own populationOffset
  # baseline), just the descriptive rate the cluster's own density is
  # being read against.
  episodic_trace_query(
    debug,
    "SELECT sample_date FROM episodic_case WHERE pathogen = ? AND institution_id = ?",
    list(stream$pathogen, stream$institution_id)
  )
  all_cases <- DBI::dbGetQuery(
    con,
    "SELECT sample_date FROM episodic_case WHERE pathogen = ? AND institution_id = ?",
    params = list(stream$pathogen, stream$institution_id)
  )
  episodic_trace_debug(debug, "debug:         case-history query done")
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

#' Simple doubling time from the daily case counts
#'
#' A Poisson regression of *daily* case counts on day, over the last 14
#' complete days of the cluster - not a fitted epidemic model (that is
#' Rt), just a descriptive rate. Doubling time is \eqn{\log 2} over the
#' fitted log-linear growth rate, and is `NA` unless that rate is
#' positive.
#'
#' The obvious cheaper thing - regressing log(*cumulative*) cases on day
#' - is what this used to do, and it is wrong in a way that matters:
#' under constant, non-growing incidence the cumulative count still
#' climbs linearly, so \eqn{\log(\mathrm{cum})} climbs like \eqn{\log t},
#' whose OLS slope is positive for any flat series. Every cluster with
#' three or more cases therefore reported a finite doubling time,
#' including ones that were not growing at all - the single stat on the
#' dossier an epidemiologist is most likely to read as "this is accelerating".
#' Fitting the daily counts instead makes a flat series return a slope of
#' about zero, and a declining one a negative slope, both of which yield
#' `NA` and no stat tile.
#'
#' Zero-case days inside the window are counted as zeros rather than
#' being absent, since a run of empty days is exactly the evidence that
#' an outbreak is not growing.
#'
#' @param cases A data frame of the cluster's cases, with `sample_date`.
#' @param incomplete_days From `episodic_app_completeness()`. The most
#'   recent days are under-ascertained by construction (reporting lag),
#'   and including them biases the fitted slope *downwards* - a growing
#'   outbreak reads as flattening purely because its last few days have
#'   not finished being reported. Trimmed for the same reason
#'   `episodic_compute_rt()` withholds its trailing windows.
#' @param asof The date the data is current as of (the latest successful
#'   run). Only days within `incomplete_days` of this are trimmed: a
#'   cluster whose last case was months ago has no incomplete tail to
#'   trim, and trimming its final days regardless would silently discard
#'   real observations.
#' @param window_days How many trailing days to fit over.
#' @return A single numeric (days), or `NA_real_` - including whenever
#'   the fitted doubling time is longer than the fitted window itself,
#'   which is a statement about the window rather than about the
#'   outbreak.
#' @keywords internal
#' @noRd
episodic_app_doubling_time <- function(
  cases,
  incomplete_days = 0L,
  asof = Sys.Date(),
  window_days = 14L
) {
  if (nrow(cases) < 3) {
    return(NA_real_)
  }
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]
  if (length(dates) < 3) {
    return(NA_real_)
  }

  last_complete <- min(max(dates), as.Date(asof) - as.integer(incomplete_days))
  if (is.na(last_complete) || last_complete < min(dates)) {
    return(NA_real_)
  }
  first_day <- max(min(dates), last_complete - window_days + 1)

  all_days <- seq(first_day, last_complete, by = "day")
  if (length(all_days) < 3) {
    return(NA_real_)
  }
  counts <- vapply(all_days, function(d) sum(dates == d), integer(1))
  if (sum(counts) < 3 || sum(counts > 0) < 2) {
    return(NA_real_)
  }

  day_index <- as.numeric(all_days - first_day)
  fit <- tryCatch(
    suppressWarnings(stats::glm(counts ~ day_index, family = stats::poisson())),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NA_real_)
  }
  slope <- unname(stats::coef(fit)[2])
  if (is.na(slope) || slope <= 0) {
    return(NA_real_)
  }

  doubling <- log(2) / slope
  # A doubling time longer than the window it was fitted over is not a
  # measurement of doubling - the data simply does not contain a
  # doubling. This is also what keeps a perfectly flat series honest: its
  # fitted slope is zero only up to floating-point noise, and dividing by
  # a slope of 1e-17 would otherwise report a doubling time of some
  # astronomical number of days rather than "not applicable".
  if (!is.finite(doubling) || doubling > length(all_days)) {
    return(NA_real_)
  }
  round(doubling, 1)
}

#' Where the cluster concentrates geographically (by PC)
#'
#' Deliberately always PC, not ward/institution: for a ward-level (L1)
#' cluster, every case already shares that ward by construction (the
#' stream itself is scoped to it), so grouping by ward there would be
#' tautological (100% "concentration" by definition, saying nothing).
#' PC concentration is the one dimension that is informative at every
#' lattice level.
#'
#' `dominant_share` is a share of the cases whose PC is actually known,
#' not of every case in the cluster. Dividing by `nrow(cases)` - as this
#' used to - silently diluted the measure by however many cases had no
#' postcode: a cluster of ten cases, six of them in one PC and four with
#' no PC recorded, read as 60% concentrated when what was actually
#' observed was 100%. That share drives the concentration fragments in
#' the interpretation engine and the spatial component of the priority
#' score, so under-recorded postcodes were quietly pushing genuinely
#' localised clusters down the queue. A missing postcode is absence of
#' evidence about localisation, not evidence of dispersal.
#'
#' @param cases A data frame of the cluster's cases, with `pc`.
#' @param level The stream's lattice level (unused; kept so callers stay
#'   explicit about the level this is being computed at).
#' @return A list, or `NULL` when no case carries a PC. `total` is the
#'   number of cases with a known PC - the denominator `dominant_share`
#'   is a share of - and `n_unknown_pc` how many were set aside.
#' @keywords internal
#' @noRd
episodic_app_concentration <- function(cases, level) {
  if (nrow(cases) == 0 || all(is.na(cases$pc))) {
    return(NULL)
  }
  known <- cases$pc[!is.na(cases$pc)]
  tab <- table(known)
  tab <- tab[order(-tab)]
  list(
    dominant_label = names(tab)[1],
    dominant_n = as.integer(tab[1]),
    dominant_share = as.numeric(tab[1]) / length(known),
    total = length(known),
    n_unknown_pc = nrow(cases) - length(known),
    rows = data.frame(label = names(tab), n = as.integer(tab), row.names = NULL)
  )
}

#' Positivity summary from the optional denominator table
#'
#' `positivity_first`/`positivity_last` are the two ends of the *windowed*
#' series (see `episodic_app_denominator_series()`), which is what makes
#' them comparable at all. Read over the whole recorded history, as they
#' used to be, "first" was the earliest week the operator ever supplied a
#' denominator for and "last" was the most recent one - so the
#' interpretation engine's `denominator.rising_positivity` fragment was
#' comparing a week two years before a cluster began against a week
#' possibly long after it ended, and reporting the difference as evidence
#' about that cluster.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen The stream's pathogen.
#' @param cases The cluster's own cases; used only to place the window.
#' @keywords internal
#' @noRd
episodic_app_denominator_summary <- function(con, pathogen, cases) {
  denom <- episodic_db_denominator_for_pathogen(con, pathogen)
  if (nrow(denom) == 0 || nrow(cases) == 0) {
    return(NULL)
  }

  series <- episodic_app_denominator_series(con, pathogen, cases)
  if (nrow(series) < 2) {
    return(NULL)
  }

  list(
    n_tests_first = series$n_tests[1],
    n_tests_last = series$n_tests[nrow(series)],
    positivity_first = series$positivity[1],
    positivity_last = series$positivity[nrow(series)],
    series = series
  )
}

#' Weekly (n_tests, n_cases, positivity) series aligned for charting
#'
#' Positivity is *this pathogen's* confirmed cases over *this pathogen's*
#' tests, both counted region-wide over the same week. It used to be the
#' cluster's own case count over the region-wide test count, which is not
#' a positivity rate at all: numerator and denominator were drawn from
#' different populations, so the line tracked how big the cluster was
#' rather than how much of the testing was coming back positive, and sat
#' near zero for any cluster smaller than the region. That mattered
#' beyond the chart - the panel's whole stated purpose is telling a real
#' rise apart from a denominator effect ("if the bars rise but the line
#' stays flat, the increase is a denominator effect"), and a line
#' computed this way cannot answer that question.
#'
#' The cluster's own weekly counts stay available as `n_cluster_cases`,
#' for context alongside the rate rather than as part of it.
#'
#' Both counts are restricted to a window ending at the cluster's last
#' case week, so the panel describes the period the cluster actually
#' occupies instead of every week the operator has ever supplied a
#' denominator for.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param pathogen The stream's pathogen.
#' @param cases The cluster's own cases, with `sample_date`.
#' @param weeks How many weeks of context to keep, ending at the week of
#'   the cluster's last case.
#' @return A data frame with `week_start`, `n_tests`, `n_cases` (region
#'   wide, the positivity numerator), `n_cluster_cases`, and
#'   `positivity`.
#' @keywords internal
#' @noRd
episodic_app_denominator_series <- function(con, pathogen, cases, weeks = 26L) {
  empty <- data.frame(
    week_start = as.Date(character(0)),
    n_tests = integer(0),
    n_cases = integer(0),
    n_cluster_cases = integer(0),
    positivity = numeric(0)
  )
  denom <- episodic_db_denominator_for_pathogen(con, pathogen)
  if (nrow(denom) == 0) {
    return(empty)
  }

  denom <- stats::aggregate(n_tests ~ sample_date, denom, sum)
  denom$week_start <- as.Date(denom$sample_date)
  denom <- denom[!is.na(denom$week_start), , drop = FALSE]
  if (nrow(denom) == 0) {
    return(empty)
  }
  denom <- denom[order(denom$week_start), ]

  cluster_dates <- as.Date(cases$sample_date)
  cluster_dates <- cluster_dates[!is.na(cluster_dates)]
  if (length(cluster_dates) > 0) {
    window_end <- max(cluster_dates)
    window_start <- window_end - 7 * (as.integer(weeks) - 1L)
    denom <- denom[
      denom$week_start <= window_end & denom$week_start + 6 >= window_start, ,
      drop = FALSE
    ]
    if (nrow(denom) == 0) {
      return(empty)
    }
  }

  pathogen_dates <- as.Date(
    episodic_db_cases_for_pathogen(con, pathogen)$sample_date
  )
  pathogen_dates <- pathogen_dates[!is.na(pathogen_dates)]

  denom$n_cases <- vapply(
    denom$week_start,
    function(ws) {
      sum(pathogen_dates >= ws & pathogen_dates < ws + 7)
    },
    integer(1)
  )
  denom$n_cluster_cases <- vapply(
    denom$week_start,
    function(ws) {
      sum(cluster_dates >= ws & cluster_dates < ws + 7)
    },
    integer(1)
  )
  denom$positivity <- ifelse(
    denom$n_tests > 0,
    denom$n_cases / denom$n_tests,
    NA
  )
  denom[, c(
    "week_start",
    "n_tests",
    "n_cases",
    "n_cluster_cases",
    "positivity"
  )]
}

#' Whether the cluster's age distribution has shifted from the stream baseline
#' @keywords internal
#' @noRd
episodic_app_demography_shift <- function(con, stream_id, cases) {
  if (nrow(cases) == 0 || all(is.na(cases$age))) {
    return(NULL)
  }

  bands <- c("0-19", "20-39", "40-59", "60-79", "80+")
  band_of <- function(age) {
    cut(age, breaks = c(-1, 19, 39, 59, 79, Inf), labels = bands)
  }

  cluster_band <- band_of(cases$age)
  cluster_dominant <- names(sort(-table(cluster_band)))[1]

  stream_pathogen <- DBI::dbGetQuery(
    con,
    "SELECT pathogen FROM episodic_stream WHERE stream_id = ?",
    params = list(stream_id)
  )$pathogen[1]
  # Cases belonging to any cluster in this stream are excluded from the
  # baseline, so the comparison is against the endemic background rather
  # than against a history that already contains this cluster. Leaving
  # them in makes it partly circular, and increasingly so the rarer the
  # pathogen: for a pathogen whose recorded history is largely this one
  # cluster, the cluster dominates its own baseline and can therefore
  # never be found to have shifted away from it - exactly the situation
  # (a rare pathogen, a big cluster) where a demographic shift is most
  # worth surfacing. Same principle as the baseline exclusion Farrington
  # already applies (`episodic_baseline_excluded_windows()`): a detected
  # aberration must not become part of what counts as normal.
  all_cases <- DBI::dbGetQuery(
    con,
    "SELECT age FROM episodic_case WHERE pathogen = ? AND case_id NOT IN
            (SELECT case_id FROM episodic_cluster_case WHERE cluster_id IN
               (SELECT cluster_id FROM episodic_cluster WHERE stream_id = ?))",
    params = list(stream_pathogen, stream_id)
  )
  if (nrow(all_cases) < 5 || all(is.na(all_cases$age))) {
    return(list(
      shifted = FALSE,
      dominant_band = as.character(cluster_dominant),
      baseline_band = NA,
      bands = episodic_app_demography_bars(cases)
    ))
  }
  baseline_band_tab <- band_of(all_cases$age)
  baseline_dominant <- names(sort(-table(baseline_band_tab)))[1]

  list(
    shifted = !identical(cluster_dominant, baseline_dominant),
    dominant_band = as.character(cluster_dominant),
    baseline_band = as.character(baseline_dominant),
    bands = episodic_app_demography_bars(cases)
  )
}

#' Age/sex pyramid bars: cluster counts (no baseline overlay)
#' @keywords internal
#' @noRd
episodic_app_demography_bars <- function(cases) {
  bands <- c("0-19", "20-39", "40-59", "60-79", "80+")
  band_of <- function(age) {
    cut(age, breaks = c(-1, 19, 39, 59, 79, Inf), labels = bands)
  }
  cases$band <- band_of(cases$age)
  out <- data.frame(band = bands, stringsAsFactors = FALSE)
  out$m <- vapply(
    bands,
    function(b) sum(cases$band == b & cases$sex == "M", na.rm = TRUE),
    integer(1)
  )
  out$v <- vapply(
    bands,
    function(b) sum(cases$band == b & cases$sex == "F", na.rm = TRUE),
    integer(1)
  )
  out
}

#' Reporting-triangle-derived incomplete window for the epi curve shading
#'
#' `incomplete_days` is a *count of trailing days*, not a lag index: it
#' is the first lag at which the stream reaches 95% completeness, so that
#' exactly the days at lags `0 .. incomplete_days - 1` are the
#' under-ascertained ones.
#'
#' Two things were wrong with taking `max(lag_days)` over every lag below
#' 95%, as this used to. It was one day short even on a well-behaved
#' completion curve (the largest incomplete lag is `incomplete_days - 1`,
#' not `incomplete_days`). And `episodic_triangle_completeness()` returns
#' a *median* share per lag, which over a modest number of historical
#' sample dates is not monotone in practice: a single dip at, say, lag 11
#' in an otherwise fully-reported curve dragged the shaded zone out to
#' eleven days, greying out - and, via `episodic_compute_rt()`,
#' withholding Rt over - a week and a half of complete data. Reading the
#' run from the front treats a late dip as the noise it is, while a
#' genuinely slow-reporting stream, incomplete at every early lag, is
#' still shaded in full.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id The stream to summarise.
#' @return A list with `incomplete_days`.
#' @keywords internal
#' @noRd
episodic_app_completeness <- function(con, stream_id) {
  completeness <- episodic_triangle_completeness(con, stream_id)
  if (nrow(completeness) == 0) {
    return(list(incomplete_days = 0L))
  }
  completeness <- completeness[order(completeness$lag_days), ]

  complete_enough <- which(completeness$completeness >= 0.95)
  if (length(complete_enough) == 0) {
    # Never reaches 95% within max_lag_days: every observed lag is
    # under-ascertained, so shade all of them.
    return(list(incomplete_days = as.integer(max(completeness$lag_days)) + 1L))
  }
  list(incomplete_days = as.integer(completeness$lag_days[complete_enough[1]]))
}

#' Whether a cluster can still be shown in the dossier pane
#'
#' True for any cluster that exists and has not been merged into another
#' one - open or closed. Closed is not a reason to refuse: the archive is
#' full of clusters worth re-reading, and the Pathogen screen links to
#' them by id.
#'
#' Merged is a reason. `episodic_reconcile_stream()` folds overlapping
#' clusters into the oldest survivor and records `merged_into` on the
#' others; their cases are now counted under the survivor, so their
#' dossier would show a case list that no longer belongs to them.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id, or `NULL`.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_app_cluster_viewable <- function(con, cluster_id) {
  if (is.null(cluster_id) || length(cluster_id) != 1 || is.na(cluster_id)) {
    return(FALSE)
  }
  found <- DBI::dbGetQuery(
    con,
    "SELECT merged_into FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  nrow(found) == 1 && is.na(found$merged_into[1])
}

#' The date the database's case data is current as of
#'
#' Every "how recent is this" judgement in the app - which trailing days
#' of an epi curve are still filling up, which Rt windows to withhold,
#' which days of a cluster are too fresh to fit a growth rate over -
#' has to be measured against the last time cases were actually
#' loaded, not against the last day the cluster in question happened to
#' have a case.
#'
#' The distinction is not cosmetic. Anchoring on a cluster's own last
#' case day means a cluster that ended in March gets its final days
#' treated as under-reported forever, greying out its epi curve tail and
#' withholding its last Rt estimates, months after every one of those
#' cases was fully reported. Reporting lag is a property of *now*, not of
#' the cluster.
#'
#' @param con A [DBI::DBIConnection-class].
#' @return A `Date`: the latest successful run's finish date, falling
#'   back to today's date when no run has been recorded yet.
#' @keywords internal
#' @noRd
episodic_app_data_asof <- function(con) {
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  if (is.null(run) || is.na(run$finished_at)) {
    return(Sys.Date())
  }
  parsed <- tryCatch(
    as.Date(substr(run$finished_at, 1, 10)),
    error = function(e) NA
  )
  if (is.na(parsed)) Sys.Date() else parsed
}

#' Daily case counts for the epi curve panel, with an incomplete flag
#'
#' A day is flagged `incomplete` when it falls in the last
#' `incomplete_days` days before the date the data is current as of
#' (`episodic_app_data_asof()`).
#'
#' That anchor is the fix: the window used to be measured back from the
#' cluster's own last case day, so a cluster that stopped generating
#' cases weeks ago still had its final days drawn at reduced opacity -
#' permanently implying "more cases may still arrive here" about a tail
#' that had finished reporting long before. Reporting lag is a property
#' of now, not of the cluster.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return A data frame with `sample_date`, `n_cases`, `incomplete`.
#' @keywords internal
#' @noRd
episodic_app_epi_curve <- function(con, cluster_id) {
  cases <- episodic_db_cluster_cases(con, cluster_id)
  if (nrow(cases) == 0) {
    return(data.frame(
      sample_date = as.Date(character(0)),
      n_cases = integer(0),
      incomplete = logical(0)
    ))
  }
  cluster <- DBI::dbGetQuery(
    con,
    "SELECT stream_id FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  incomplete_days <- episodic_app_completeness(
    con,
    cluster$stream_id[1]
  )$incomplete_days
  asof <- episodic_app_data_asof(con)

  dates <- as.Date(cases$sample_date)
  all_days <- seq(min(dates), max(dates), by = "day")
  counts <- vapply(all_days, function(d) sum(dates == d), integer(1))
  data.frame(
    sample_date = all_days,
    n_cases = counts,
    incomplete = all_days > (asof - incomplete_days)
  )
}

#' Multi-year trend data for a stream (the cron-persisted chart cache)
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id A stream id.
#' @return `episodic_db_stream_trend()`'s output, capped to the last 156
#'   weeks (matching `episodic_farrington_trend()`'s own backfill cap).
#' @keywords internal
#' @noRd
episodic_app_trend <- function(con, stream_id) {
  trend <- episodic_db_stream_trend(con, stream_id)
  if (nrow(trend) == 0) {
    return(trend)
  }
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
episodic_app_linelist <- function(con, cluster_id) {
  cases <- episodic_db_cluster_cases(con, cluster_id)
  if (nrow(cases) == 0) {
    return(cases)
  }
  cases[
    order(cases$sample_date),
    c(
      "patient_key",
      "lab_number",
      "sample_date",
      "sex",
      "age",
      "pc",
      "ward",
      "specialism"
    )
  ]
}

#' Detection settings for the dossier's settings panel
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return A named list of display-ready values.
#' @keywords internal
#' @noRd
episodic_app_detection_settings <- function(con, cluster_id) {
  cluster_obj <- episodic_cluster_object(con, cluster_id)
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  list(
    detectors = cluster_obj$detectors,
    rt_applicable = cluster_obj$rt_applicable,
    aggregation = "week",
    population_offset = if (!is.null(cluster_obj$density)) {
      "patient_days"
    } else {
      NULL
    },
    case_free_days = cluster_obj$case_free_days,
    last_run_when = if (!is.null(run)) run$finished_at else NA,
    last_run_host = if (!is.null(run)) run$host else NA,
    pkg_versions = if (!is.null(run) && !is.na(run$pkg_versions)) {
      run$pkg_versions
    } else {
      NA
    }
  )
}

#' Read-only Streams screen data
#'
#' Displays the configuration from the latest run's `config_snapshot`, not
#' from the file.
#'
#' Paginated, and deliberately at the read-model level rather than only
#' in the UI: `baseline_excluded` is one DB round trip per stream (via
#' `episodic_baseline_excluded_windows()`, itself one round trip per
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
episodic_app_streams_screen <- function(con, page = 1L, page_size = 50L) {
  streams_all <- episodic_db_streams(con, active_only = FALSE)
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
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
  streams <- if (total == 0) {
    streams_all
  } else {
    streams_all[from:to, , drop = FALSE]
  }

  # Excluded windows are listed on the Streams screen so a baseline is
  # never quietly different from what an epidemiologist expects. Computed only
  # for this page's streams.
  if (nrow(streams) > 0) {
    streams$baseline_excluded <- episodic_baseline_excluded_windows_many(
      con,
      streams$stream_id
    )
    # Whether Farrington can actually run on this stream. It needs
    # (b + 1) * 52 weeks of history and returns nothing at all below that,
    # which is indistinguishable on screen from having looked and found
    # nothing - so a stream nobody's statistical detector is watching
    # looked exactly like a quiet one. Read off first_seen and the run's
    # own date, so it costs no query: both are already in hand.
    b <- config_snapshot$farrington$b
    asof <- if (!is.null(run) && !is.na(run$run_date)) {
      as.Date(run$run_date)
    } else {
      Sys.Date()
    }
    streams$farrington_weeks_need <- if (is.null(b)) {
      NA_integer_
    } else {
      as.integer((b + 1) * 52)
    }
    streams$farrington_weeks_have <- as.integer(
      as.integer(asof - as.Date(streams$first_seen)) %/% 7L + 1L
    )
    streams$farrington_ready <-
      !is.na(streams$farrington_weeks_need) &
        streams$farrington_weeks_have >= streams$farrington_weeks_need
  }
  list(
    streams = streams,
    total = total,
    page = page,
    page_size = page_size,
    n_pages = n_pages,
    config_snapshot = config_snapshot,
    run = run
  )
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
episodic_app_status <- function(con) {
  run <- episodic_db_latest_run(con)
  if (is.null(run)) {
    return(list(status = "none"))
  }
  n_clusters <- nrow(episodic_db_clusters(con, open_only = TRUE))
  list(
    status = run$status,
    finished_at = run$finished_at,
    n_streams = run$n_streams,
    n_detections = run$n_detections,
    n_clusters_open = n_clusters,
    # Why it failed, not only that it did: for an operator connecting
    # their own extract, the reason is the whole message - and the
    # dashboard is where they are looking when they notice.
    error_text = run$error_text %||% NA_character_
  )
}
