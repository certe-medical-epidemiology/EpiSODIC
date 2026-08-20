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

#' Run one detection cycle
#'
#' The cron entry point (ARCHITECTURE.md section 3). Ingests, enumerates
#' streams, detects, reconciles and persists, all
#' inside one transaction, so a partially failed run leaves no partial
#' state and a retry is always safe.
#'
#' Configuration is read from `EPISODE_CONFIG`;
#' the resolved configuration's hash and full snapshot are written to
#' `episode_detection_run` so that any result is explainable from the
#' database alone (ARCHITECTURE.md section 7.4).
#'
#' `certedb`/Diver access is deliberately never called from inside this
#' package (`QUESTIONS.md` item 22): `get_diver_data()` is the operator's
#' own step, run before `episode_run_cron()`, transforming Diver's columns
#' into `episode_ingest_columns`. See `README.md` for the raw data contract.
#'
#' @param db_path Path to the SQLite database file. Created if it does not
#'   exist.
#' @param ingest_source_fn A zero-argument function returning a data frame
#'   satisfying the ingestion interface, or that data frame itself (an
#'   operator who has already extracted and transformed their data has no
#'   reason to wrap it in a function just to satisfy this parameter - see
#'   [episode_resolve_source()]). Defaults to the bundled synthetic
#'   generator, the only implementation shipped in this environment.
#' @param denominator_source_fn An optional zero-argument function
#'   returning the pre-aggregated positivity metadata data frame
#'   (`pathogen`, `sample_date`, `care_line`, `area_code`, `n_tests`), that
#'   data frame itself, or `NULL` (the default) if the operator has none to
#'   supply. See `QUESTIONS.md` item 22.
#' @param institution_activity_source_fn An optional one-argument function
#'   (`institutions`, the current `episode_db_institutions()` data frame,
#'   so a real implementation can key its own hospital system's export by
#'   the same institutions this run already knows about) returning weekly
#'   patient-days (`institution_key`, `period_start`, `period_end`,
#'   `patient_days`); that data frame itself (the `institutions` argument
#'   is simply not passed in that case); or `NULL` (the default) if the
#'   operator has none to supply. Powers L2 patient-day normalisation
#'   (ARCHITECTURE.md section 7.1); without it, L1/L2 Farrington detection
#'   uses raw counts, unnormalised, exactly as before this was added.
#' @param episode_config_path Passed to [episode_config_resolve()].
#' @param host,account Recorded on `episode_detection_run`; default to the
#'   process's own host/account (ARCHITECTURE.md section 12: this is not an
#'   identity source for assessors, only for the run record).
#' @param run_date The date to treat as "today" for closure/eligibility
#'   calculations; defaults to the system date, injectable for tests.
#' @return Invisibly, the `run_id` of the completed run.
#' @examples
#' \donttest{
#' db_path <- tempfile(fileext = ".sqlite")
#' run_id <- episode_run_cron(
#'   db_path,
#'   ingest_source_fn = function() episode_ingest_source_synthetic(
#'     start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#'   )
#' )
#' file.remove(db_path)
#' }
#' @export
episode_run_cron <- function(db_path,
                              ingest_source_fn = episode_ingest_source_synthetic,
                              denominator_source_fn = NULL,
                              institution_activity_source_fn = NULL,
                              episode_config_path = Sys.getenv("EPISODE_CONFIG", unset = NA),
                              host = Sys.info()[["nodename"]],
                              account = Sys.info()[["user"]],
                              run_date = Sys.Date()) {
  config <- episode_config_resolve(episode_config_path)
  hashed <- episode_config_hash(config)

  con <- if (file.exists(db_path)) episode_db_connect(db_path) else episode_db_create(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  run_id <- episode_db_run_start(con, host = host, account = account)

  result <- tryCatch({
    DBI::dbBegin(con)
    stats <- episode_run_cron_body(con, run_id, config, ingest_source_fn, denominator_source_fn,
                                    institution_activity_source_fn, run_date)
    DBI::dbCommit(con)
    stats
  }, error = function(e) {
    DBI::dbRollback(con)
    list(status = "failed", error_text = conditionMessage(e), n_streams = NA_integer_,
         n_detections = NA_integer_, n_signals_new = NA_integer_, n_signals_updated = NA_integer_)
  })

  pkg_versions <- jsonlite::toJSON(episode_pkg_versions(), auto_unbox = TRUE)

  episode_db_run_finish(
    con, run_id,
    status = if (is.null(result$status)) "success" else result$status,
    n_streams = result$n_streams, n_detections = result$n_detections,
    n_signals_new = result$n_signals_new, n_signals_updated = result$n_signals_updated,
    code_version = as.character(utils::packageVersion("EpiSODIC")),
    pkg_versions = as.character(pkg_versions),
    config_hash = hashed$hash, config_snapshot = hashed$snapshot,
    error_text = result$error_text
  )

  invisible(run_id)
}

#' Resolve a `*_source_fn` argument to the data frame it names
#'
#' Every `episode_run_cron()` data source (`ingest_source_fn`,
#' `denominator_source_fn`, `institution_activity_source_fn`) accepts
#' either a function that produces the data frame, or that data frame
#' itself - a function is only useful when producing the data has to
#' happen at run time (a live query, a freshly-generated synthetic set);
#' an operator who already has the data sitting in a variable has no
#' reason to wrap it in `function() my_df`.
#'
#' @param x A function, a data frame, or `NULL`.
#' @param ... Passed to `x` if it is a function; ignored otherwise (a data
#'   frame that is already the answer does not need `institutions` handed
#'   to it, for instance).
#' @return `NULL` if `x` is `NULL`; `x` itself if it is a data frame; the
#'   result of calling `x` otherwise.
#' @examples
#' df <- data.frame(x = 1:3)
#' identical(episode_resolve_source(df), df)
#' identical(episode_resolve_source(function() df), df)
#' is.null(episode_resolve_source(NULL))
#' @export
episode_resolve_source <- function(x, ...) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.function(x)) return(x(...))
  stop("must be a function or a data frame, not ", paste(class(x), collapse = "/"), call. = FALSE)
}

#' @keywords internal
#' @noRd
episode_run_cron_body <- function(con, run_id, config, ingest_source_fn, denominator_source_fn,
                                   institution_activity_source_fn, run_date) {
  pathogen_config_path <- system.file("config", "pathogen_config.csv", package = "EpiSODIC")
  if (identical(pathogen_config_path, "")) {
    pathogen_config_path <- file.path("inst", "config", "pathogen_config.csv")
  }
  pathogen_config <- utils::read.csv(pathogen_config_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  episode_db_pathogen_config_load(con, pathogen_config)

  raw <- episode_resolve_source(ingest_source_fn)
  episode_ingest_run(con, raw, pathogen_config, run_id)

  denominator <- episode_resolve_source(denominator_source_fn)
  if (!is.null(denominator)) {
    episode_denominator_ingest_run(con, denominator)
  }

  cases_all <- episode_db_cases(con)
  institutions <- episode_db_institutions(con)

  activity <- episode_resolve_source(institution_activity_source_fn, institutions)
  if (!is.null(activity)) {
    episode_institution_activity_ingest_run(con, activity)
  }

  episode_lattice_enumerate(con, cases_all, institutions)

  n_detections_total <- 0L
  n_new_total <- 0L
  n_updated_total <- 0L

  same_place_detections <- episode_detect_same_place(con, cases_all, institutions, config)
  rare_trigger_detections <- episode_detect_rare_trigger(con, cases_all, institutions, config)

  streams <- episode_db_streams(con)  # refresh: same_place/rare_trigger may have created streams

  for (i in seq_len(nrow(streams))) {
    stream <- streams[i, ]
    stream_cases <- episode_cases_for_stream(cases_all, stream)

    episode_triangle_update(con, stream$stream_id, stream_cases, as.character(run_date))

    stream_detections <- rbind(
      same_place_detections[same_place_detections$stream_id == stream$stream_id, ],
      rare_trigger_detections[rare_trigger_detections$stream_id == stream$stream_id, ]
    )

    # ARCHITECTURE.md section 7.3: MEM runs on pathogen_region (L5) streams
    # only, for organisms flagged mem_applicable - see episode_detect_mem()'s
    # own docs for why L5 rather than every level.
    pc_mem <- pathogen_config[pathogen_config$pathogen == stream$pathogen, ]
    if (nrow(pc_mem) > 0 && isTRUE(as.logical(pc_mem$mem_applicable[1])) &&
        identical(stream$level, "pathogen_region")) {
      stream_detections <- rbind(stream_detections, episode_detect_mem(stream_cases, stream$stream_id, run_date))
    }

    if (nrow(stream_cases) > 0 &&
        episode_eligibility_gate(stream_cases, run_date, config)) {
      # ARCHITECTURE.md section 7.6: a period this stream's own history
      # shows was a confirmed epidemic must not silently raise next
      # winter's baseline. Excluded from the cases fed to Farrington only
      # (same_place/rare_trigger detect on raw counts and do not baseline
      # at all, so they are unaffected).
      excluded_windows <- episode_baseline_excluded_windows(con, stream$stream_id)
      farrington_cases <- episode_baseline_exclude_cases(stream_cases, excluded_windows)

      # ARCHITECTURE.md section 7.1: patient-day normalisation at L2. Both
      # calls below build identical weekly bins from the same
      # (farrington_cases, run_date), so one population vector serves both.
      weekly_weeks <- episode_weekly_bins(as.Date(farrington_cases$sample_date), run_date)$week_start
      population <- episode_farrington_population_vector(con, stream$institution_id, stream$level, weekly_weeks)

      stream_detections <- rbind(
        stream_detections,
        episode_detect_farrington(farrington_cases, stream$stream_id, config, run_date, population = population)
      )

      # trend cache for the multi-year trend panel (M2); see
      # episode_farrington_trend()'s own docs for the backfill-once,
      # top-up-thereafter strategy.
      n_existing_trend <- nrow(episode_db_stream_trend(con, stream$stream_id))
      trend <- episode_farrington_trend(farrington_cases, config, run_date, n_weeks_existing = n_existing_trend,
                                         population = population)
      for (k in seq_len(nrow(trend))) {
        episode_db_stream_trend_upsert(
          con, stream_id = stream$stream_id, week_start = as.character(trend$week_start[k]),
          n_cases = trend$n_cases[k], expected = trend$expected[k], upperbound = trend$upperbound[k]
        )
      }
    }

    if (nrow(stream_detections) == 0) next

    detection_ids <- integer(nrow(stream_detections))
    for (j in seq_len(nrow(stream_detections))) {
      d <- stream_detections[j, ]
      detection_ids[j] <- episode_db_detection_insert(
        con, run_id = run_id, stream_id = stream$stream_id, detector = d$detector,
        first_day = d$first_day, last_day = d$last_day, n_cases = d$n_cases,
        expected = d$expected, upperbound = d$upperbound, params_json = as.character(d$params)
      )
    }
    stream_detections$detection_id <- detection_ids
    n_detections_total <- n_detections_total + nrow(stream_detections)

    pc <- pathogen_config[pathogen_config$pathogen == stream$pathogen, ]
    case_free_days <- if (nrow(pc) > 0) pc$case_free_days[1] else config$reconciliation$case_free_days_default
    cooldown_days <- if (nrow(pc) > 0) pc$cooldown_days[1] else NA

    weights <- config$priority_score$weights
    reconcile_result <- episode_reconcile_stream(
      con, stream_id = stream$stream_id, detections = stream_detections,
      case_free_days = case_free_days, run_id = run_id,
      close_after_runs = config$reconciliation$close_after_runs,
      cooldown_days = cooldown_days,
      cooldown_reopen_ratio = config$reconciliation$cooldown_reopen_ratio %||% NA,
      priority_score_fn = function(candidate) {
        episode_priority_score(
          excess = NA, ratio = candidate$n_cases / max(candidate$n_cases, 1),
          severity_weight = if (nrow(pc) > 0) pc$severity_weight[1] else 1,
          detector_agreement = candidate$detector_agreement, n_detectors = 4,  # farrington, same_place, rare_trigger, mem
          weights = weights
        )
      },
      has_assessment_fn = function(cluster_id) {
        nrow(episode_db_assessment_events(con, cluster_id)) > 0
      },
      verdict_fn = function(cluster_id) {
        events <- episode_db_assessment_events(con, cluster_id)
        classified <- events[!is.na(events$verdict), ]
        if (nrow(classified) == 0) NA_character_ else classified$verdict[nrow(classified)]
      }
    )
    n_new_total <- n_new_total + reconcile_result$n_new
    n_updated_total <- n_updated_total + reconcile_result$n_updated
  }

  list(
    status = "success", n_streams = nrow(streams), n_detections = n_detections_total,
    n_signals_new = n_new_total, n_signals_updated = n_updated_total, error_text = NA
  )
}

#' Filter a data frame of cases down to those belonging to one stream
#'
#' L1/L2 streams filter on `pathogen` and `institution_id` (and `ward` for
#' L1). L3-L5 streams filter on `pathogen` and sample date only, an
#' approximation documented in `QUESTIONS.md` alongside the L3/L4 region
#' derivation itself (`R/lattice_enumerate.R`); a real operator-supplied
#' PC-to-region join would tighten this.
#'
#' @param cases All currently known cases.
#' @param stream A single-row stream (from `episode_db_streams()`).
#' @return The subset of `cases` belonging to `stream`.
#' @keywords internal
#' @noRd
episode_cases_for_stream <- function(cases, stream) {
  matches <- cases$pathogen == stream$pathogen
  if (!is.na(stream$institution_id)) {
    matches <- matches & !is.na(cases$institution_id) & cases$institution_id == stream$institution_id
  }
  if (!is.na(stream$ward)) {
    matches <- matches & !is.na(cases$ward) & cases$ward == stream$ward
  }
  cases[matches, ]
}

#' @keywords internal
#' @noRd
episode_pkg_versions <- function() {
  pkgs <- c("EpiSODIC", "surveillance", "EpiEstim")
  versions <- lapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA
  })
  stats::setNames(versions, pkgs)
}
