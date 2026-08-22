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

#' Run one surveillance detection cycle
#'
#' This is the function you schedule to run regularly (e.g. daily, via
#' cron): it pulls in new laboratory data, checks every monitored stream
#' for statistical aberrations with the configured detectors, reconciles
#' the results into cluster dossiers for the board to assess, and records
#' everything in the database. A run either completes in full or leaves no
#' trace at all - it runs inside a single database transaction, so a
#' failed run is always safe to simply retry.
#'
#' EpiSODIC never connects to your laboratory system directly. You extract
#' and transform your own data beforehand, and hand it over as a plain data
#' frame or `tibble`: `cases` for the laboratory results themselves (see
#' [episodic_case_data] for the required columns and their allowed
#' values), and optionally `denominators` and `institution_activity` for
#' testing volume and hospital activity. A data set is the normal case; if
#' producing the data only makes sense at run time (a live database query,
#' for instance), a zero-argument function returning one is accepted just
#' as well - see [episodic_resolve_data()].
#'
#' The exact detection settings used are recorded with the run (see
#' [episodic_config_hash()]), so any past result can always be traced back
#' to the configuration that produced it.
#'
#' @param db_path Path to the EpiSODIC database: a SQLite file (created
#'   automatically if it does not exist yet) or a MariaDB/MySQL DSN (see
#'   [episodic_db_dsn_mariadb()]).
#' @param cases Your laboratory data: a data frame or `tibble` in the
#'   shape [episodic_case_data] describes, or a zero-argument function
#'   that returns one. Defaults to the bundled synthetic generator, useful
#'   for demos and testing but not real surveillance.
#' @param denominators Optional: your testing-volume data, in the same
#'   form as `cases` - normally a data set, a function if it has to be
#'   produced at run time (see [episodic_synthetic_denominators()] for the
#'   expected shape). Leave as `NULL` (the default) if you have none to
#'   supply - positivity panels simply stay blank.
#' @param institution_activity Optional: your hospital patient-days
#'   data (see [episodic_synthetic_institution_activity()] for the
#'   expected shape), normally as a data set, or as a function taking the
#'   current institutions table. Leave as `NULL` (the default) if you have
#'   none - detection falls back to raw case counts.
#' @param episodic_config_path Passed to [episodic_config_resolve()].
#' @param host,account Recorded with the run for audit purposes; default
#'   to the current machine and account.
#' @param run_date The date to treat as "today". Defaults to the system
#'   date; mainly useful to override in tests.
#' @return Invisibly, the `run_id` of the completed run.
#' @examples
#' \donttest{
#' db_path <- tempfile(fileext = ".sqlite")
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' run_id <- episodic_run_cron(db_path, cases = cases)
#' file.remove(db_path)
#' }
#' @export
episodic_run_cron <- function(db_path,
                              cases = episodic_synthetic_cases,
                              denominators = NULL,
                              institution_activity = NULL,
                              episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
                              host = Sys.info()[["nodename"]],
                              account = Sys.info()[["user"]],
                              run_date = Sys.Date()) {
  config <- episodic_config_resolve(episodic_config_path)
  hashed <- episodic_config_hash(config)

  con <- if (episodic_db_exists(db_path)) episodic_db_connect(db_path) else episodic_db_create(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  run_id <- episodic_db_run_start(con, host = host, account = account)

  result <- tryCatch({
    DBI::dbBegin(con)
    stats <- episodic_run_cron_body(con, run_id, config, cases, denominators,
                                    institution_activity, run_date)
    DBI::dbCommit(con)
    stats
  }, error = function(e) {
    DBI::dbRollback(con)
    list(status = "failed", error_text = conditionMessage(e), n_streams = NA_integer_,
         n_detections = NA_integer_, n_signals_new = NA_integer_, n_signals_updated = NA_integer_)
  })

  pkg_versions <- jsonlite::toJSON(episodic_pkg_versions(), auto_unbox = TRUE)

  episodic_db_run_finish(
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

#' Resolve a data source argument to a data frame
#'
#' A small helper behind [episodic_run_cron()]'s `cases`,
#' `denominators`, and `institution_activity` arguments, each
#' of which accepts a data frame or `tibble` directly - the normal case -
#' or, if producing the data only makes sense at run time (a live database
#' query, for instance), a zero-argument function that returns one.
#'
#' @param x A data frame or `tibble`, a function returning one, or `NULL`.
#' @param ... Passed to `x` if it is a function; ignored otherwise.
#' @return `NULL` if `x` is `NULL`; `x` itself if it is a data frame (a
#'   `tibble` included); the result of calling `x` otherwise.
#' @examples
#' df <- data.frame(x = 1:3)
#' identical(episodic_resolve_data(df), df)
#' identical(episodic_resolve_data(function() df), df)
#' is.null(episodic_resolve_data(NULL))
#' @export
episodic_resolve_data <- function(x, ...) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.function(x)) return(x(...))
  stop("data source must be a data frame (or tibble), or a function returning one, not ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' @keywords internal
#' @noRd
episodic_run_cron_body <- function(con, run_id, config, cases, denominators,
                                   institution_activity, run_date) {
  pathogen_config_path <- system.file("config", "pathogen_config.csv", package = "EpiSODIC")
  if (identical(pathogen_config_path, "")) {
    pathogen_config_path <- file.path("inst", "config", "pathogen_config.csv")
  }
  pathogen_config <- utils::read.csv(pathogen_config_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  episodic_db_pathogen_config_load(con, pathogen_config)

  cases <- episodic_resolve_data(cases)
  episodic_cases_load(con, cases, pathogen_config, run_id)

  denominators <- episodic_resolve_data(denominators)
  if (!is.null(denominators)) {
    episodic_denominators_load(con, denominators)
  }

  cases_all <- episodic_db_cases(con)
  institutions <- episodic_db_institutions(con)

  institution_activity <- episodic_resolve_data(institution_activity, institutions)
  if (!is.null(institution_activity)) {
    episodic_institution_activity_load(con, institution_activity)
  }

  episodic_lattice_enumerate(con, cases_all, institutions)

  n_detections_total <- 0L
  n_new_total <- 0L
  n_updated_total <- 0L

  same_place_detections <- episodic_detect_same_place(con, cases_all, institutions, config)
  rare_trigger_detections <- episodic_detect_rare_trigger(con, cases_all, institutions, config)

  streams <- episodic_db_streams(con)  # refresh: same_place/rare_trigger may have created streams

  for (i in seq_len(nrow(streams))) {
    stream <- streams[i, ]
    stream_cases <- episodic_cases_for_stream(cases_all, stream)

    episodic_triangle_update(con, stream$stream_id, stream_cases, as.character(run_date))

    stream_detections <- rbind(
      same_place_detections[same_place_detections$stream_id == stream$stream_id, ],
      rare_trigger_detections[rare_trigger_detections$stream_id == stream$stream_id, ]
    )

    # MEM runs on pathogen_region (L5) streams only, for pathogens
    # flagged mem_applicable - see episodic_detect_mem()'s own docs for
    # why L5 rather than every level.
    pc_mem <- pathogen_config[pathogen_config$pathogen == stream$pathogen, ]
    if (nrow(pc_mem) > 0 && isTRUE(as.logical(pc_mem$mem_applicable[1])) &&
        identical(stream$level, "pathogen_region")) {
      stream_detections <- rbind(stream_detections, episodic_detect_mem(stream_cases, stream$stream_id, run_date))
    }

    if (nrow(stream_cases) > 0 &&
        episodic_eligibility_gate(stream_cases, run_date, config)) {
      # A period this stream's own history shows was a confirmed
      # epidemic must not silently raise next winter's baseline.
      # Excluded from the cases fed to Farrington only
      # (same_place/rare_trigger detect on raw counts and do not baseline
      # at all, so they are unaffected).
      excluded_windows <- episodic_baseline_excluded_windows(con, stream$stream_id)
      farrington_cases <- episodic_baseline_exclude_cases(stream_cases, excluded_windows)

      # Patient-day normalisation at L2. Both
      # calls below build identical weekly bins from the same
      # (farrington_cases, run_date), so one population vector serves both.
      weekly_weeks <- episodic_weekly_bins(as.Date(farrington_cases$sample_date), run_date)$week_start
      population <- episodic_farrington_population_vector(con, stream$institution_id, stream$level, weekly_weeks)

      stream_detections <- rbind(
        stream_detections,
        episodic_detect_farrington(farrington_cases, stream$stream_id, config, run_date, population = population)
      )

      # trend cache for the multi-year trend panel; see
      # episodic_farrington_trend()'s own docs for the backfill-once,
      # top-up-thereafter strategy.
      n_existing_trend <- nrow(episodic_db_stream_trend(con, stream$stream_id))
      trend <- episodic_farrington_trend(farrington_cases, config, run_date, n_weeks_existing = n_existing_trend,
                                         population = population)
      for (k in seq_len(nrow(trend))) {
        episodic_db_stream_trend_upsert(
          con, stream_id = stream$stream_id, week_start = as.character(trend$week_start[k]),
          n_cases = trend$n_cases[k], expected = trend$expected[k], upperbound = trend$upperbound[k]
        )
      }
    }

    if (nrow(stream_detections) == 0) next

    detection_ids <- integer(nrow(stream_detections))
    for (j in seq_len(nrow(stream_detections))) {
      d <- stream_detections[j, ]
      detection_ids[j] <- episodic_db_detection_insert(
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
    reconcile_result <- episodic_reconcile_stream(
      con, stream_id = stream$stream_id, detections = stream_detections,
      case_free_days = case_free_days, run_id = run_id,
      close_after_runs = config$reconciliation$close_after_runs,
      cooldown_days = cooldown_days,
      cooldown_reopen_ratio = config$reconciliation$cooldown_reopen_ratio %||% NA,
      # Five of the seven priority components are properties of the
      # candidate episode and its cases, so they are computed here, where
      # both are in hand. They used to be left at their defaults - most
      # damagingly `ratio = n_cases / max(n_cases, 1)`, which is
      # identically 1 for every candidate - which collapsed the ranking
      # that orders the whole assessment queue down to severity weight
      # and detector agreement alone.
      priority_score_fn = function(candidate) {
        metrics <- episodic_reconcile_candidate_metrics(candidate)
        candidate_cases <- episodic_cases_in_window(stream_cases, candidate$first_day, candidate$last_day)
        # Same descriptive rate the dossier's own density stat shows, so
        # the ranking and the displayed evidence cannot drift apart.
        density <- episodic_app_density(con, stream, candidate_cases)
        density_ratio <- if (is.null(density) || is.na(density$baseline) || density$baseline <= 0) {
          NA_real_
        } else {
          density$value / density$baseline
        }
        episodic_priority_score(
          excess = metrics$excess, ratio = metrics$ratio,
          severity_weight = if (nrow(pc) > 0) pc$severity_weight[1] else 1,
          growth_slope = episodic_growth_slope(stream_cases, candidate$last_day),
          detector_agreement = candidate$detector_agreement, n_detectors = 4,  # farrington, same_place, rare_trigger, mem
          density_ratio = density_ratio,
          spatial_concentration = episodic_spatial_concentration(candidate_cases),
          weights = weights
        )
      },
      has_assessment_fn = function(cluster_id) {
        nrow(episodic_db_assessment_events(con, cluster_id)) > 0
      },
      verdict_fn = function(cluster_id) {
        events <- episodic_db_assessment_events(con, cluster_id)
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
#' approximation of the L3/L4 region derivation itself
#' (`R/lattice_enumerate.R`); a real operator-supplied PC-to-region join
#' would tighten this.
#'
#' @param cases All currently known cases.
#' @param stream A single-row stream (from `episodic_db_streams()`).
#' @return The subset of `cases` belonging to `stream`.
#' @keywords internal
#' @noRd
episodic_cases_for_stream <- function(cases, stream) {
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
episodic_pkg_versions <- function() {
  pkgs <- c("EpiSODIC", "surveillance", "EpiEstim")
  versions <- lapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA
  })
  stats::setNames(versions, pkgs)
}
