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
episodic_app_open_clusters <- function(con,
                                       lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  clusters <- episodic_db_clusters_not_closed(con, "outbreak")
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

  open <- clusters[clusters$state != "closed" & clusters$scale == "outbreak", ]
  # Newest last case day first - the rail is a triage queue, and a cluster
  # that just gained a case is more likely to need attention right now
  # than one that scored higher on priority but has gone quiet.
  open[order(as.Date(open$last_day), decreasing = TRUE), ]
}

#' Derive state for many clusters at once
#'
#' What the rail actually needs from `episodic_app_derive_state_for_cluster()`,
#' but without its one-query-per-cluster cost: `episodic_db_assessment_events()`
#' and `episodic_db_cluster_states()` are each fetched once for every
#' cluster in one batched query here, instead of once per cluster, which
#' is what made the rail's own render cost scale with the number of open
#' clusters rather than being flat.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param clusters A data frame with (at least) `cluster_id` and
#'   `changed_since_assessment` columns - `episodic_db_clusters()`'s own
#'   shape is exactly this.
#' @return A character vector of states, one per row of `clusters`, in the
#'   same order.
#' @keywords internal
#' @noRd
episodic_app_derive_states_batch <- function(con, clusters) {
  if (nrow(clusters) == 0) {
    return(character(0))
  }

  ids <- clusters$cluster_id
  # Grouped once with split() rather than filtered per cluster: a
  # per-cluster `events_all[events_all$cluster_id == id, ]` scans every
  # row for every cluster, which on the Archive - every cluster the
  # instance holds - grows with the square of their number.
  # Keyed through as.integer(): as.character() of a double id past
  # 99999 is "1e+05", which would group nothing if the two sides of the
  # match arrived as different numeric types.
  id_key <- function(x) as.character(as.integer(x))
  groups <- unique(id_key(ids))
  by_cluster <- function(rows) {
    split(rows, factor(id_key(rows$cluster_id), levels = groups))
  }
  events_by <- by_cluster(episodic_db_assessment_events_batch(con, ids))
  states_by <- by_cluster(episodic_db_cluster_states_batch(con, ids))
  changed <- as.logical(clusters$changed_since_assessment)
  keys <- id_key(ids)

  vapply(
    seq_along(ids),
    function(i) {
      events <- events_by[[keys[i]]]
      episodic_derive_state(
        events,
        changed_since_assessment = changed[i],
        explicitly_closed = episodic_app_explicitly_closed_from(
          states_by[[keys[i]]],
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
#' `changed_since_assessment` flag, and whether it has been explicitly
#' closed.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @return One of `episodic_derive_state()`'s state strings.
#' @keywords internal
#' @noRd
episodic_app_derive_state_for_cluster <- function(con, cluster_id) {
  events <- episodic_db_assessment_events(con, cluster_id)
  cluster <- episodic_db_get_query(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(cluster) == 0) {
    return("new")
  }
  cluster <- cluster[1, ]

  episodic_derive_state(
    events,
    changed_since_assessment = as.logical(cluster$changed_since_assessment),
    explicitly_closed = episodic_app_explicitly_closed(con, cluster_id, events)
  )
}

#' Whether a person explicitly closed a cluster
#'
#' Any verdict, including artefact/expected variation, is closed only by an
#' epidemiologist's deliberate decision, never by the classification
#' itself - "a cluster closes when a person says so", full stop. That act
#' is recorded as an `episodic_cluster_state` row (`trigger = "closure"`),
#' not as a new assessment event (the classification's own rationale
#' already exists; closure records that it is over, not what it was). It
#' counts as still in effect only if nothing has happened since - in
#' practice, no newer assessment event exists.
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
#' column, and one `episodic_db_cluster_states()` query per row is a
#' round trip per row. Given `episodic_db_cluster_states_batch()`'s
#' output - already ordered by `cluster_id, entered_at, state_id`, so a
#' cluster's last closure is its last row - the same answer is a
#' `match()`.
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

#' Who closed each of many clusters, from states fetched in bulk
#'
#' The Archive companion to `episodic_app_closed_at_from()` - same "last
#' `state == 'closed'` row per cluster" logic, resolved to a display label
#' (`episodic_app_actor_label()`: a person's full name, or the system
#' label for an automatic closure) instead of a timestamp.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param states_all Rows from `episodic_db_cluster_states_batch()`.
#' @param cluster_ids The cluster ids to answer for, in the order wanted.
#' @param lang Session language.
#' @return A character vector of actor labels, one per id, `NA` for a
#'   cluster with no recorded closure.
#' @keywords internal
#' @noRd
episodic_app_closed_by_from <- function(con,
                                        states_all,
                                        cluster_ids,
                                        lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  closures <- states_all[states_all$state == "closed", ]
  if (nrow(closures) == 0) {
    return(rep(NA_character_, length(cluster_ids)))
  }
  last <- closures[!duplicated(closures$cluster_id, fromLast = TRUE), ]
  idx <- match(cluster_ids, last$cluster_id)
  # One label per distinct closer - a person or the system - rather than
  # one lookup per cluster: an archive of thousands of clusters was
  # closed by a handful of people.
  closers <- unique(last$user_id)
  labels <- vapply(
    closers,
    function(user_id) episodic_app_actor_label(con, user_id, lang = lang),
    character(1)
  )
  out <- rep(NA_character_, length(cluster_ids))
  found <- !is.na(idx)
  out[found] <- unname(labels[match(last$user_id[idx[found]], closers)])
  out
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
episodic_cluster_object <- function(con,
                                    cluster_id,
                                    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  cluster <- episodic_db_get_query(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(cluster) == 0) {
    stop("No such cluster: ", cluster_id, call. = FALSE)
  }
  cluster <- cluster[1, ]

  stream <- episodic_db_get_query(
    con,
    "SELECT * FROM episodic_stream WHERE stream_id = ?",
    params = list(cluster$stream_id)
  )[1, ]
  institution <- if (!is.na(stream$institution_id)) {
    episodic_db_get_query(
      con,
      "SELECT * FROM episodic_institution WHERE institution_id = ?",
      params = list(stream$institution_id)
    )[1, ]
  } else {
    NULL
  }
  pc <- episodic_db_pathogen_config_get(con, stream$pathogen)

  # origin = "manual": case-level detail (if any) lives in
  # episodic_cluster_manual_case, never in episodic_case - see
  # episodic_add_manual_cluster(). Every downstream function fed `cases`
  # here only ever reads sample_date/pc/sex/age, so the two sources are
  # interchangeable from this point on; the one exception is
  # patient_key (unique_patients below), which a manual cluster has none
  # of by design.
  is_manual <- identical(cluster$origin, "manual")
  cases <- if (is_manual) {
    episodic_db_cluster_manual_cases(con, cluster_id)
  } else {
    episodic_db_cluster_cases(con, cluster_id)
  }
  detections <- episodic_db_get_query(
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
      incomplete_days = completeness$incomplete_days,
      asof = asof
    ),
    concentration = episodic_app_concentration(cases),
    denominator = episodic_app_denominator_summary(con, stream$pathogen, cases),
    asof = asof,
    demography = episodic_app_demography_shift(con, stream$stream_id, cases),
    completeness = completeness,
    # No patient_key to dedup on for a manual cluster - each row is
    # assumed to already be one distinct case, exactly as reported.
    unique_patients = if (is_manual) nrow(cases) else length(unique(cases$patient_key)),
    n_positives = nrow(cases),
    origin = cluster$origin,
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
    mem_mode = if (!is.null(pc)) as.character(pc$mem_mode) else "auto",
    curve_shape = if (!is.null(pc)) {
      episodic_classify_curve_shape(cases, pc$incub_max_days)
    } else {
      NA_character_
    },
    rt = if (!is.null(pc)) {
      episodic_compute_rt(
        cases,
        pc,
        incomplete_days = completeness$incomplete_days,
        asof = asof
      )
    } else {
      NULL
    },
    rt_unavailable_reason = episodic_rt_unavailable_reason(
      pc,
      incomplete_days = completeness$incomplete_days
    )
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
#' @param incomplete_days From `episodic_app_completeness()`. `NA` there
#'   means the reporting delay was never measured, which is its own
#'   reason: `episodic_compute_rt()` withholds every window rather than
#'   publish estimates it cannot tell apart from still-filling ones, and
#'   saying "not enough case history" instead would send an
#'   epidemiologist looking for cases that are already there.
#' @return One of `"completeness_unknown"`, `"no_serial_interval"`,
#'   `"epiestim_missing"`, `"insufficient_history"`, or `NA` if Rt is not
#'   applicable at all.
#' @keywords internal
#' @noRd
episodic_rt_unavailable_reason <- function(pc, incomplete_days = 0L) {
  if (is.null(pc) || !isTRUE(as.logical(pc$rt_applicable))) {
    return(NA_character_)
  }
  if (is.na(pc$si_mean_days) || is.na(pc$si_sd_days)) {
    return("no_serial_interval")
  }
  if (length(incomplete_days) != 1 || is.na(incomplete_days)) {
    return("completeness_unknown")
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
episodic_app_level_label <- function(level,
                                     care_line = NA,
                                     lang = Sys.getenv("EPISODIC_LANGUAGE")) {
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
episodic_app_place_label <- function(stream,
                                     institution,
                                     lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  care_line_suffix <- if (!is.na(stream$care_line)) {
    paste0(
      " \u00b7 ",
      episodic_tr(paste0("careline.", stream$care_line), lang = lang)
    )
  } else {
    ""
  }
  if (stream$level == "pathogen_ward" && !is.null(institution)) {
    return(paste0(
      institution$display_name,
      " \u00b7 ",
      episodic_tr("panel.places.title_ward", lang = lang),
      ": ",
      stream$ward
    ))
  }
  if (stream$level == "pathogen_institution" && !is.null(institution)) {
    return(institution$display_name)
  }
  region_label <- episodic_app_format_region(stream$region_code)
  paste0(region_label, care_line_suffix)
}

#' A stream's `region_code`, verbatim
#'
#' At `pathogen_area` level, `region_code` is built from
#' `config$geography` (see `episodic_geography_config()`); at
#' `pathogen_province` level it is whatever `province_code` the
#' operator's own `EPISODIC_PC_PROVINCE_MAP` resolved a postcode to (see
#' `episodic_pc_to_province()`). Either way it is a name the operator
#' chose, so it is shown exactly as configured -
#' cosmetic reformatting would risk mangling a real name (a hyphen in
#' "Noord-Holland" is part of the name, not a code separator to strip)
#' and would leave the operator unable to see, character for character,
#' whether their mapping is producing what they expect.
#'
#' @param region_code A stream's `region_code`, or `NA`.
#' @keywords internal
#' @noRd
episodic_app_format_region <- function(region_code) {
  if (is.na(region_code) || !nzchar(region_code)) {
    return("")
  }
  region_code
}

#' @keywords internal
#' @noRd
episodic_app_density <- function(con, stream, cases) {
  if (is.na(stream$institution_id) || nrow(cases) == 0) {
    return(NULL)
  }
  activity <- episodic_db_institution_activity(con, stream$institution_id)
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
  all_cases <- episodic_db_get_query(
    con,
    "SELECT sample_date FROM episodic_case WHERE pathogen = ? AND institution_id = ?",
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

#' Simple doubling time from the daily case counts
#'
#' A Poisson regression of *daily* case counts on day, over the last 14
#' complete days of the cluster - not a fitted epidemic model (that is
#' Rt), just a descriptive rate. Doubling time is \eqn{\log 2} over the
#' fitted log-linear growth rate, and is `NA` unless that rate is
#' positive.
#'
#' Daily counts, not the obvious cheaper thing: regressing
#' log(*cumulative*) cases on day is wrong in a way that matters. Under
#' constant, non-growing incidence the cumulative count still climbs
#' linearly, so \eqn{\log(\mathrm{cum})} climbs like \eqn{\log t},
#' whose OLS slope is positive for any flat series - every cluster with
#' three or more cases would report a finite doubling time, including
#' ones that are not growing at all, on the single stat an
#' epidemiologist is most likely to read as "this is accelerating".
#' Fitting the daily counts makes a flat series return a slope of about
#' zero, and a declining one a negative slope, both of which yield `NA`
#' and no stat tile.
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
episodic_app_doubling_time <- function(cases,
                                       incomplete_days = 0L,
                                       asof = Sys.Date(),
                                       window_days = 14L) {
  # `NA` is "the reporting delay was never measured", so there is no
  # trailing window to trim and no way to know how much of the slope is
  # reporting lag. A doubling time fitted anyway would be biased
  # downwards by an unknown amount, which is worse than no tile at all.
  if (length(incomplete_days) != 1 || is.na(incomplete_days)) {
    return(NA_real_)
  }
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
#' not of every case in the cluster. Dividing by `nrow(cases)` instead
#' dilutes the measure by however many cases have no postcode: a cluster
#' of ten cases, six of them in one PC and four with no PC recorded,
#' would read as 60% concentrated where what was observed is 100%. That
#' share drives the concentration fragments in the interpretation engine
#' and the spatial component of the priority score, so under-recorded
#' postcodes would push genuinely localised clusters down the queue. A
#' missing postcode is absence of evidence about localisation, not
#' evidence of dispersal.
#'
#' @param cases A data frame of the cluster's cases, with `pc`.
#' @return A list, or `NULL` when no case carries a PC. `total` is the
#'   number of cases with a known PC - the denominator `dominant_share`
#'   is a share of - and `n_unknown_pc` how many were set aside. `rows`
#'   carries `label` (the PC verbatim, which is what
#'   `episodic_geo_join()` joins the map geometry on, so it must stay
#'   the bare value), `n`, and `province` - the coarser unit that PC
#'   resolves to, or `NA` where it resolves to none.
#'   `province_error` holds why no province could be resolved for any of
#'   them, when the reason is a configuration problem worth showing the
#'   reader rather than absence of a mapping.
#' @keywords internal
#' @noRd
episodic_app_concentration <- function(cases) {
  if (nrow(cases) == 0 || all(is.na(cases$pc))) {
    return(NULL)
  }
  known <- cases$pc[!is.na(cases$pc)]
  tab <- table(known)
  tab <- tab[order(-tab)]
  provinces <- episodic_app_pc_provinces(names(tab))
  list(
    dominant_label = names(tab)[1],
    dominant_n = as.integer(tab[1]),
    dominant_share = as.numeric(tab[1]) / length(known),
    total = length(known),
    n_unknown_pc = nrow(cases) - length(known),
    province_error = provinces$error,
    rows = data.frame(
      label = names(tab),
      n = as.integer(tab),
      province = provinces$province,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )
}

#' The province each PC falls in, for display next to it
#'
#' The same lookup the lattice's L4 level is built on
#' (`episodic_pc_to_province()`), so a postcode is shown under the
#' province it is actually detected under rather than under a second,
#' cosmetic notion of where it is.
#'
#' A misconfigured `EPISODIC_PC_PROVINCE_MAP` stops a detection run
#' (`episodic_run_cron()`). The dashboard cannot take that literally - a
#' dossier that refuses to render says less about the problem than a
#' dossier that renders and names it - so the same message is read here
#' and handed back to be shown in the panel. Read, not swallowed: it
#' reaches the reader either way.
#'
#' @param pc A character vector of postcode values.
#' @return A list with `province` (a character vector the length of `pc`)
#'   and `error` (a single string, or `NA`).
#' @keywords internal
#' @noRd
episodic_app_pc_provinces <- function(pc) {
  problem <- episodic_pc_province_map_problem()
  if (!is.na(problem)) {
    return(list(
      province = rep(NA_character_, length(pc)),
      error = problem
    ))
  }
  list(province = episodic_pc_to_province(pc), error = NA_character_)
}

#' Positivity summary from the optional denominator table
#'
#' `positivity_first`/`positivity_last` are the two ends of the *windowed*
#' series (see `episodic_app_denominator_series()`), which is what makes
#' them comparable at all. Read over the whole recorded history instead,
#' "first" would be the earliest week the operator ever supplied a
#' denominator for and "last" the most recent one - and the
#' interpretation engine's `denominator.rising_positivity` fragment
#' would be comparing a week two years before a cluster began against a
#' week possibly long after it ended, reporting the difference as
#' evidence about that cluster.
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

#' How many dates fall in each seven-day window
#'
#' One sort and two binary searches per window, rather than a full pass
#' over every date for every window: the same counts, at a cost that
#' stops scaling with the product of the two.
#'
#' `week_starts` need not be regularly spaced or disjoint - each window
#' is counted on its own terms, which is what keeps this a drop-in for
#' the per-window `sum(dates >= ws & dates < ws + 7)` it replaces.
#'
#' @param dates A `Date` vector, `NA`s already removed.
#' @param week_starts A `Date` vector of window starts; each window is
#'   `[start, start + 7)`.
#' @return An integer vector, one count per `week_starts` entry.
#' @keywords internal
#' @noRd
episodic_week_counts <- function(dates, week_starts) {
  if (length(week_starts) == 0) {
    return(integer(0))
  }
  if (length(dates) == 0) {
    return(rep(0L, length(week_starts)))
  }
  sorted <- sort(as.numeric(dates))
  starts <- as.numeric(week_starts)
  # `Date` values are whole days, so "before the window ends" is "on or
  # before its last day" and no half-open boundary has to be expressed
  # in floating point.
  as.integer(
    findInterval(starts + 6, sorted) - findInterval(starts - 1, sorted)
  )
}

#' The positivity feed's test counts, summed per ISO week
#'
#' The feed's `sample_date` is a period start, and nothing requires the
#' operator's periods to be weeks starting on a Monday, or weeks at all -
#' a feed can arrive daily, or weekly from whatever day the laboratory's
#' own reporting week begins. Taken as-is, two period starts in the same
#' ISO week become two bars labelled with the same week number, and a
#' daily feed becomes seven overlapping "weeks" each counting a week of
#' cases against one day of tests. Summed to the Monday of the ISO week
#' each period starts in, every series has exactly one row per week, on
#' the same weeks the case curves are drawn on, and the cases counted
#' against it are the cases of that same week.
#'
#' @param denom Rows of `episodic_db_denominator_for_pathogen()`.
#' @return A data frame with `week_start` (`Date`, a Monday) and
#'   `n_tests`, one row per week, in week order; no rows when no period
#'   start parses as a date.
#' @keywords internal
#' @noRd
episodic_app_denominator_weekly <- function(denom) {
  # With an explicit format, so a period start that is not a date is an
  # NA that is left out, rather than an error that takes the panel down.
  week_start <- episodic_week_start(
    as.Date(as.character(denom$sample_date), format = "%Y-%m-%d")
  )
  keep <- !is.na(week_start) & !is.na(denom$n_tests)
  if (!any(keep)) {
    return(data.frame(week_start = as.Date(character(0)), n_tests = numeric(0)))
  }
  weekly <- stats::aggregate(
    list(n_tests = denom$n_tests[keep]),
    list(week_start = week_start[keep]),
    sum
  )
  weekly <- weekly[order(weekly$week_start), , drop = FALSE]
  rownames(weekly) <- NULL
  weekly
}

#' Weekly (n_tests, n_cases, positivity) series aligned for charting
#'
#' Positivity is *this pathogen's* confirmed cases over *this pathogen's*
#' tests, both counted region-wide over the same week. The cluster's own
#' case count over the region-wide test count is not a positivity rate
#' at all: numerator and denominator come from different populations, so
#' the line would track how big the cluster is rather than how much of
#' the testing is coming back positive, and sit near zero for any
#' cluster smaller than the region. The panel's whole stated purpose is
#' telling a real rise apart from a denominator effect ("if the bars
#' rise but the line stays flat, the increase is a denominator
#' effect"), and a line computed that way cannot answer that.
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

  denom <- episodic_app_denominator_weekly(denom)
  if (nrow(denom) == 0) {
    return(empty)
  }

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

  # Bounded by the weeks actually plotted, and by `sample_date` in SQL
  # rather than by discarding rows in R afterwards: a case outside
  # [first week start, last week end] cannot fall in any of these
  # buckets, so reading a pathogen's whole recorded history to count
  # half a year of it is work that grows with the archive and answers
  # nothing.
  window_from <- min(denom$week_start)
  window_to <- max(denom$week_start) + 6
  pathogen_dates <- as.Date(
    episodic_db_cases_for_pathogen(
      con,
      pathogen,
      columns = "sample_date",
      from = window_from,
      to = window_to
    )$sample_date
  )
  pathogen_dates <- pathogen_dates[!is.na(pathogen_dates)]

  denom$n_cases <- episodic_week_counts(pathogen_dates, denom$week_start)
  denom$n_cluster_cases <- episodic_week_counts(
    cluster_dates,
    denom$week_start
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

  stream_pathogen <- episodic_db_get_query(
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
  #
  # Expressed as NOT EXISTS rather than a nested NOT IN: the two select
  # the same rows, but the correlated form is answered per candidate
  # case through `idx_episodic_cluster_case_case`, where the nested one
  # materialises every case id of every cluster in the stream first.
  all_cases <- episodic_db_get_query(
    con,
    "SELECT c.age
       FROM episodic_case c
      WHERE c.pathogen = ?
        AND NOT EXISTS (
          SELECT 1
            FROM episodic_cluster_case cc
            JOIN episodic_cluster cl ON cl.cluster_id = cc.cluster_id
           WHERE cc.case_id = c.case_id
             AND cl.stream_id = ?
        )",
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
#' The leading run of incomplete lags, not `max(lag_days)` over every lag
#' below 95%. That reading is one day long even on a well-behaved
#' completion curve (the largest incomplete lag is `incomplete_days - 1`,
#' not `incomplete_days`), and `episodic_triangle_completeness()` returns
#' a *median* share per lag, which over a modest number of historical
#' sample dates is not monotone in practice: a single dip at, say, lag 11
#' in an otherwise fully-reported curve would drag the shaded zone out to
#' eleven days, greying out - and, via `episodic_compute_rt()`,
#' withholding Rt over - a week and a half of complete data. Reading the
#' run from the front treats a late dip as the noise it is, while a
#' genuinely slow-reporting stream, incomplete at every early lag, is
#' still shaded in full.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param stream_id The stream to summarise.
#' @return A list with `incomplete_days` - `NA_integer_` when there is no
#'   completion curve to read at all, which is not the same thing as a
#'   reporting delay of zero and is handled as its own case by every
#'   caller.
#' @keywords internal
#' @noRd
episodic_app_completeness <- function(con, stream_id) {
  completeness <- episodic_triangle_completeness(con, stream_id)
  if (nrow(completeness) == 0) {
    # `NA`, not `0L`. There is no completion curve for this stream at
    # all - no case of it has ever been seen by a run that committed -
    # so the reporting delay was not measured, which is a different
    # statement from a reporting delay of zero. Saying zero here told
    # the epi curve that its last days were final, the doubling-time
    # fit that it could use them, and `episodic_compute_rt()` that it
    # could publish its trailing windows: three plausible-looking wrong
    # answers derived from a measurement nobody took. Every caller
    # handles `NA` explicitly.
    return(list(incomplete_days = NA_integer_))
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
  found <- episodic_db_get_query(
    con,
    "SELECT merged_into FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  nrow(found) == 1 && is.na(found$merged_into[1])
}

#' The scale of a cluster
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A single cluster id.
#' @return `"outbreak"` or `"epidemic"`, or `"outbreak"` when the
#'   cluster does not exist.
#' @keywords internal
#' @noRd
episodic_app_cluster_scale <- function(con, cluster_id) {
  found <- episodic_db_get_query(
    con,
    "SELECT scale FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(found) == 0) "outbreak" else found$scale[1]
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
#' Read from `run_date`, not from `finished_at`, for two reasons.
#'
#' `finished_at` is a UTC instant (`episodic_now()`), and the date
#' extracted from it is therefore a UTC date compared against a local
#' `Sys.Date()` everywhere else. For an instance in CEST that makes
#' `asof` a day behind for the two hours after local midnight, and for
#' one in UTC+12 for half of every day - so the incompleteness window,
#' the epi curve's shading and `episodic_compute_rt()`'s cut-off all
#' shift by a day depending on the hour.
#'
#' `run_date` is a plain local date the run was told to treat as today,
#' with no timezone in it at all. It is also the right answer for a
#' backfill: a run replayed today as of 2024-06-30 has data current as
#' of 2024-06-30, and measuring its reporting lag from this morning
#' would grey out months of a curve that was fully reported long ago.
#'
#' @param con A [DBI::DBIConnection-class].
#' @return A `Date`: the latest complete run's `run_date`, falling back
#'   to today's date when no run has been recorded yet.
#' @keywords internal
#' @noRd
episodic_app_data_asof <- function(con) {
  run <- episodic_db_latest_run(con, status = episodic_run_statuses_complete)
  if (is.null(run) || is.na(run$run_date)) {
    return(Sys.Date())
  }
  parsed <- tryCatch(
    as.Date(substr(run$run_date, 1, 10)),
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
#' That anchor matters: measured back from the cluster's own last case
#' day instead, a cluster that stopped generating cases weeks ago would
#' have its final days drawn at reduced opacity for ever, permanently
#' implying "more cases may still arrive here" about a tail that
#' finished reporting long ago. Reporting lag is a property of now, not
#' of the cluster.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param completeness The stream's completion curve from
#'   `episodic_app_completeness()`, or `NULL` to read it here. The
#'   dossier already holds one on its cluster object, and deriving it
#'   costs a reporting triangle over the stream's whole case history -
#'   so the panel passes that rather than building a second identical
#'   one for the same stream in the same render.
#' @return A data frame with `sample_date`, `n_cases`, `incomplete`.
#' @keywords internal
#' @noRd
episodic_app_epi_curve <- function(con, cluster_id, completeness = NULL) {
  cluster <- episodic_db_get_query(
    con,
    "SELECT stream_id, origin FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  is_manual <- identical(cluster$origin[1], "manual")
  # origin = "manual": case-level detail, if any, lives in
  # episodic_cluster_manual_case - see episodic_add_manual_cluster().
  cases <- if (is_manual) {
    episodic_db_cluster_manual_cases(con, cluster_id)
  } else {
    episodic_db_cluster_cases(con, cluster_id)
  }
  if (nrow(cases) == 0) {
    return(data.frame(
      sample_date = as.Date(character(0)),
      n_cases = integer(0),
      incomplete = logical(0)
    ))
  }
  # Reporting-delay completeness is a cron-computed property of a stream's
  # real case data; a manual cluster's case detail (if supplied at all) is
  # reported as final by the external system, never incomplete.
  incomplete_days <- if (is_manual) {
    0L
  } else if (is.null(completeness)) {
    episodic_app_completeness(con, cluster$stream_id[1])$incomplete_days
  } else {
    completeness$incomplete_days
  }
  asof <- episodic_app_data_asof(con)

  dates <- as.Date(cases$sample_date)
  all_days <- seq(min(dates), max(dates), by = "day")
  counts <- vapply(all_days, function(d) sum(dates == d), integer(1))
  # An unmeasured reporting delay (`NA`, see `episodic_app_completeness()`)
  # gets the same treatment as a stream that never reaches 95%: every day
  # shaded. "We cannot tell whether these days are final" and "these days
  # are not final" call for the same caution, and the alternative -
  # shading nothing - is the claim that the curve is complete, made from
  # no measurement at all. The panel says why in words
  # (`panel.epicurve.note_unknown`), so a fully shaded curve is never
  # left to be puzzled over.
  incomplete <- if (is.na(incomplete_days)) {
    rep(TRUE, length(all_days))
  } else {
    all_days > (asof - incomplete_days)
  }
  data.frame(
    sample_date = all_days,
    n_cases = counts,
    incomplete = incomplete
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
#' The run it reports is the one that last detected this cluster
#' (`episodic_cluster.last_detected_run`), not the instance's latest run:
#' the panel describes how this cluster was found, and on an instance
#' whose runs have moved to a newer build since, the latest run's
#' package versions would be a statement about a detection that did not
#' produce it. A cluster no run detected (`origin = 'manual'`) has no
#' such run, and every run field is `NA`.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param obj The cluster object from `episodic_cluster_object()`, or
#'   `NULL` to build one. Nothing read from it here is
#'   language-dependent (which detectors fired, whether Rt applies,
#'   whether a density baseline exists, the case-free requirement), so
#'   an object built in any session language is the right answer for
#'   this panel.
#' @return A named list of display-ready values.
#' @keywords internal
#' @noRd
episodic_app_detection_settings <- function(con, cluster_id, obj = NULL) {
  cluster_obj <- if (is.null(obj)) {
    episodic_cluster_object(con, cluster_id)
  } else {
    obj
  }
  run_id <- episodic_db_get_query(
    con,
    "SELECT last_detected_run FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )$last_detected_run
  run <- if (length(run_id) == 1 && !is.na(run_id)) {
    episodic_db_run(con, run_id)
  }
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
#' of what is shown makes the screen slow to load once an instance's
#' stream count passes a few dozen. Slicing to `page` before that loop
#' runs bounds the cost by `page_size` rather than by the total stream
#' count.
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

# -- Epidemic screen read layer -------------------------------------------

#' Open epidemics for the epidemic rail
#'
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language, for level/state labels.
#' @return A data frame, one row per open epidemic, ordered by `last_day`
#'   descending.
#' @keywords internal
#' @noRd
episodic_app_open_epidemics <- function(con,
                                        lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  clusters <- episodic_db_clusters_not_closed(con, "epidemic")
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
  clusters$level_label <- vapply(
    clusters$level,
    function(lv) episodic_tr(paste0("level.", lv), lang = lang),
    character(1)
  )

  open <- clusters[clusters$state != "closed" & clusters$scale == "epidemic", ]
  open[order(as.Date(open$last_day), decreasing = TRUE), ]
}

#' The epidemic dossier object
#'
#' Builds the data a single epidemic dossier needs, organised around the
#' questions an epidemiologist brings to an epidemic rather than to an
#' outbreak: where in its course it is (the latest complete week against
#' the one before, the peak so far, the MEM intensity now and at the
#' peak, the latest Rt), how it compares with earlier seasons, where it
#' is, who it affects, whether the rise is real or a rise in testing,
#' which institutions carry it, and which outbreaks ran during it.
#'
#' Two populations, deliberately. The epidemic's own cases - the ones
#' linked to the cluster - describe the epidemic itself: its geography,
#' its age and sex, its care lines, its institutions. The stream's whole
#' history - every case the lattice counts for this pathogen in this
#' area, before and during the epidemic - is what the epidemic is read
#' against: the lead-in weeks of the curve, the MEM thresholds, the
#' earlier seasons of the overlay, the renewal model behind Rt, and the
#' age baseline. For a province epidemic the second is the province's
#' history, not the catchment's; a province's epidemic measured against
#' the whole catchment's thresholds and age distribution would be
#' compared with a population it is only part of.
#'
#' Positivity is the exception, and says so on the dossier: the testing
#' volume feed has no geographic stratum the lattice can match to a
#' province, so it is the catchment's.
#'
#' A quantity that cannot be computed is `NULL` (or `NA` in a scalar
#' slot), never zero: the latest complete week does not exist while
#' every week is still filling, a change on the week before has no base
#' when that week had no cases, and an intensity has no meaning without
#' fitted thresholds.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param lang Session language.
#' @param lead_in_weeks How many weeks before the epidemic's first day
#'   the curve shows, so the rise into it is on the chart.
#' @param tail_weeks How many weeks after a closed epidemic's last case
#'   the curve shows, so its fall is on the chart.
#' @return A list with elements named for the dossier panels.
#' @keywords internal
#' @noRd
episodic_epidemic_object <- function(con,
                                     cluster_id,
                                     lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                     lead_in_weeks = 8L,
                                     tail_weeks = 4L) {
  cluster <- episodic_db_get_query(
    con,
    "SELECT * FROM episodic_cluster WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  if (nrow(cluster) == 0) {
    stop("No such cluster: ", cluster_id, call. = FALSE)
  }
  cluster <- cluster[1, ]

  stream <- episodic_db_get_query(
    con,
    "SELECT * FROM episodic_stream WHERE stream_id = ?",
    params = list(cluster$stream_id)
  )[1, ]
  pc <- episodic_db_pathogen_config_get(con, stream$pathogen)

  cases <- episodic_db_cluster_cases(con, cluster_id)
  cases$sample_date <- as.Date(cases$sample_date)
  detections <- episodic_db_get_query(
    con,
    "SELECT DISTINCT detector FROM episodic_detection WHERE cluster_id = ?",
    params = list(cluster_id)
  )$detector

  # The stream's whole history, read once: the curve's lead-in, the MEM
  # thresholds, the overlay, Rt and the age baseline all need it, and on
  # an instance with years of history it is the largest read the dossier
  # makes.
  history <- episodic_db_cases_for_stream_id(
    con,
    cluster$stream_id,
    columns = c(
      "case_id",
      "patient_key",
      "sample_date",
      "age",
      "sex",
      "care_line",
      "institution_id"
    )
  )
  history$sample_date <- as.Date(history$sample_date)
  history <- history[!is.na(history$sample_date), , drop = FALSE]

  # Every case linked to the epidemic is, by construction, a case of its
  # stream. When the history read here does not hold them all, the
  # dashboard is deciding stream membership under a different geography
  # from the detection run that linked them - an L5 stream's code, or
  # the province mapping, differs between the two configurations - and
  # every panel read against the history would be drawn from a partial
  # population without a word. Counted, so the dossier says so, and the
  # curve falls back to the cases the epidemic is known to hold.
  history_missing <- sum(!(cases$case_id %in% history$case_id))
  history_problem <- if (history_missing > 0) {
    dashboard_code <- episodic_geography_config()$region_code
    list(
      n_read = nrow(cases) - history_missing,
      n_linked = nrow(cases),
      stream_code = stream$region_code,
      dashboard_code = if (
        identical(stream$level, "pathogen_region") &&
          !identical(stream$region_code, dashboard_code)
      ) {
        dashboard_code
      } else {
        NA_character_
      }
    )
  }

  season <- episodic_db_epidemic_season(con, cluster_id)
  place <- episodic_app_place_label(stream, NULL, lang = lang)
  asof <- episodic_app_data_asof(con)
  incomplete_days <- episodic_app_completeness(
    con,
    cluster$stream_id
  )$incomplete_days

  anchor_week <- if (!is.null(season)) {
    as.integer(season$anchor_week)
  } else if (!is.null(pc) && nrow(history) > 0) {
    anchor <- episodic_mem_season_anchor(history)
    if (!is.null(anchor)) anchor$anchor_week else NULL
  } else {
    NULL
  }

  # A closed epidemic is read over its own course, with a short tail to
  # show the fall: drawn to today, a season two years back is a curve
  # of two years of zeros, and its "latest complete week" a week in
  # which it had long since ended. An open one is read to today.
  closed <- identical(
    episodic_app_derive_state_for_cluster(con, cluster_id),
    "closed"
  )
  from <- as.Date(cluster$first_day)
  to <- if (closed) {
    min(as.Date(cluster$last_day) + 7L * as.integer(tail_weeks), asof)
  } else {
    max(as.Date(cluster$last_day), asof)
  }
  # `episodic_app_pathogen_weekly()` rather than the bare weekly counts:
  # it carries the `incomplete` flag, which is what shades the weeks
  # still filling. A curve drawn without it tells the reader that this
  # week's dip is a fall in incidence, when what it is is the post.
  weekly <- episodic_app_pathogen_weekly(
    data.frame(
      sample_date = if (is.null(history_problem)) {
        history$sample_date
      } else {
        cases$sample_date
      }
    ),
    list(from = from - 7L * as.integer(lead_in_weeks), to = to),
    incomplete_days,
    asof
  )

  thresholds <- if (!is.null(anchor_week) && !is.null(season)) {
    episodic_mem_thresholds_for_season(
      history,
      season$season_label,
      anchor_week = anchor_week
    )
  } else {
    NULL
  }

  resolved <- list(from = from, to = to)
  denominator <- episodic_app_pathogen_denominator(
    con,
    stream$pathogen,
    # From the Monday of the earliest week the panel can show, so the
    # first week's positivity counts all seven of its days.
    episodic_db_cases_for_pathogen(
      con,
      stream$pathogen,
      columns = "sample_date",
      from = episodic_week_start(from - 28),
      to = to + 6
    ),
    list(from = from - 28, to = to)
  )

  overlay <- episodic_app_pathogen_overlay(
    history,
    resolved,
    seasonal = !is.null(season) && !is.null(anchor_week),
    asof = asof,
    anchor_week = anchor_week
  )
  # Every season's full course, not cropped to this epidemic's weeks:
  # the overlay is here to show where in an ordinary season this one
  # started and peaked, which is exactly what cropping to its own weeks
  # would cut away.
  if (!is.null(overlay)) {
    overlay$period_range <- NULL
  }

  rt <- episodic_app_pathogen_rt(history, pc, resolved, incomplete_days, asof)

  institutions <- episodic_epidemic_institutions(con, cases, lang = lang)

  during_outbreaks <- episodic_db_outbreaks_during_epidemic(con, cluster_id)
  if (nrow(during_outbreaks) > 0) {
    inst_lookup <- episodic_db_institutions(con)
    during_streams <- episodic_db_get_query_in(
      con,
      "SELECT * FROM episodic_stream WHERE stream_id IN (%s)",
      during_outbreaks$stream_id
    )
    during_outbreaks$place <- vapply(
      during_outbreaks$stream_id,
      function(stream_id) {
        s <- during_streams[match(stream_id, during_streams$stream_id), ]
        inst <- if (!is.na(s$institution_id)) {
          inst_lookup[match(s$institution_id, inst_lookup$institution_id), ]
        } else {
          NULL
        }
        episodic_app_place_label(s, inst, lang = lang)
      },
      character(1)
    )
    during_outbreaks$level_label <- vapply(
      during_outbreaks$level,
      function(lv) episodic_tr(paste0("level.", lv), lang = lang),
      character(1)
    )
    during_outbreaks$state <- episodic_app_derive_states_batch(
      con,
      during_outbreaks
    )
    during_outbreaks$state_label <- vapply(
      during_outbreaks$state,
      function(s) episodic_tr(paste0("state.", s), lang = lang),
      character(1)
    )
    during_outbreaks <- episodic_db_attach_case_days(con, during_outbreaks)
  }

  concentration <- if (nrow(institutions) > 0) {
    list(
      top_institution = institutions$display_name[1],
      top_share = institutions$n_cases[1] / sum(institutions$n_cases),
      n_institutions = nrow(institutions)
    )
  } else {
    NULL
  }

  list(
    id = cluster$cluster_id,
    stream_id = cluster$stream_id,
    pathogen = stream$pathogen,
    level = stream$level,
    care_line = stream$care_line,
    scale = cluster$scale,
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
    origin = cluster$origin,
    asof = asof,
    incomplete_days = incomplete_days,
    history_problem = history_problem,
    season = season,
    anchor_week = anchor_week,
    weekly = weekly,
    thresholds = thresholds,
    closed = closed,
    course = episodic_epidemic_course(weekly, from, thresholds),
    overlay = overlay,
    rt = rt,
    rt_applicable = !is.null(pc) && isTRUE(as.logical(pc$rt_applicable)),
    case_free_days = if (!is.null(pc)) pc$case_free_days else NA_integer_,
    rt_unavailable_reason = episodic_rt_unavailable_reason(
      pc,
      incomplete_days = incomplete_days
    ),
    denominator = denominator,
    denominator_catchment_only = !identical(stream$level, "pathogen_region"),
    concentration_geo = episodic_app_concentration(cases),
    demography = episodic_app_pathogen_demography(history, cases),
    care_lines = episodic_app_pathogen_breakdown(cases, "care_line", lang = lang),
    institutions = institutions,
    concentration = concentration,
    during_outbreaks = during_outbreaks
  )
}

#' Where an epidemic stands in its own course
#'
#' Read off the weekly curve the dossier draws, so the numbers and the
#' chart cannot disagree. The latest complete week is the most recent
#' week the reporting delay says is no longer filling (`incomplete` is
#' `FALSE`); where every week is still filling, or the delay was never
#' measured, there is no such week and every figure that depends on it
#' is `NA` - a week still arriving compared against a finished one is a
#' fall that is really the post.
#'
#' @param weekly `episodic_app_pathogen_weekly()`'s output.
#' @param first_day The epidemic's first day; the peak is looked for from
#'   its week on, not in the lead-in weeks.
#' @param thresholds `episodic_mem_thresholds_for_season()`'s output, or
#'   `NULL`.
#' @return A list with `latest_week`, `latest_n`, `previous_n`,
#'   `change_pct` (`NA` against a week with no cases), `peak_week`,
#'   `peak_n`, `latest_level` and `peak_level` (`NA` without thresholds).
#' @keywords internal
#' @noRd
episodic_epidemic_course <- function(weekly, first_day, thresholds = NULL) {
  weekly <- weekly[order(weekly$week_start), , drop = FALSE]
  within <- weekly[
    weekly$week_start >= episodic_week_start(as.Date(first_day)), ,
    drop = FALSE
  ]
  complete <- which(!(weekly$incomplete %in% TRUE))
  latest <- if (length(complete) == 0) NA_integer_ else max(complete)

  latest_n <- if (is.na(latest)) NA_integer_ else weekly$n_cases[latest]
  previous_n <- if (is.na(latest) || latest < 2) {
    NA_integer_
  } else {
    weekly$n_cases[latest - 1]
  }
  change_pct <- if (is.na(previous_n) || previous_n == 0) {
    NA_real_
  } else {
    100 * (latest_n - previous_n) / previous_n
  }

  peak <- if (nrow(within) == 0 || all(within$n_cases == 0)) {
    NA_integer_
  } else {
    which.max(within$n_cases)
  }
  peak_n <- if (is.na(peak)) NA_integer_ else within$n_cases[peak]

  level_of <- function(n) {
    if (is.null(thresholds) || is.na(n)) {
      return(NA_character_)
    }
    episodic_mem_intensity_level(
      n,
      thresholds$pre_epidemic,
      thresholds$intensity
    )
  }

  list(
    latest_week = if (is.na(latest)) as.Date(NA) else weekly$week_start[latest],
    latest_n = latest_n,
    previous_n = previous_n,
    change_pct = change_pct,
    peak_week = if (is.na(peak)) as.Date(NA) else within$week_start[peak],
    peak_n = peak_n,
    latest_level = level_of(latest_n),
    peak_level = level_of(peak_n)
  )
}

#' The institutions an epidemic's own cases came from
#'
#' Counted from the cases linked to the epidemic, not from every case of
#' the pathogen in its weeks: a province epidemic's institutions are the
#' ones in that province. A case with no institution recorded is counted
#' under none, rather than under an invented one.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases The epidemic's own cases, with `institution_id`.
#' @param lang Session language, for the name of an institution the
#'   reference table does not hold.
#' @return A data frame with `institution_id`, `display_name` and
#'   `n_cases`, largest first; no rows when no case names an institution.
#' @keywords internal
#' @noRd
episodic_epidemic_institutions <- function(con,
                                           cases,
                                           lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  empty <- data.frame(
    institution_id = integer(0),
    display_name = character(0),
    n_cases = integer(0),
    stringsAsFactors = FALSE
  )
  ids <- cases$institution_id[!is.na(cases$institution_id)]
  if (length(ids) == 0) {
    return(empty)
  }
  tab <- table(ids)
  institutions <- episodic_db_institutions(con)
  out <- data.frame(
    institution_id = as.integer(names(tab)),
    n_cases = as.integer(tab),
    stringsAsFactors = FALSE
  )
  out$display_name <- institutions$display_name[
    match(out$institution_id, institutions$institution_id)
  ]
  out$display_name[is.na(out$display_name)] <- episodic_tr(
    "misc.unknown",
    lang = lang
  )
  out <- out[order(-out$n_cases, out$display_name), , drop = FALSE]
  rownames(out) <- NULL
  out[, c("institution_id", "display_name", "n_cases")]
}
