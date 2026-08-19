#' Run one detection cycle
#'
#' The cron entry point (ARCHITECTURE.md section 3, MILESTONES.md M1 step
#' 10). Ingests, enumerates streams, detects, reconciles and persists, all
#' inside one transaction, so a partially failed run leaves no partial
#' state and a retry is safe (standing brief hard rule, reconciliation
#' properties list in `MILESTONES.md`).
#'
#' Configuration is read from `EPISODE_CONFIG` (standing brief hard rule 2);
#' the resolved configuration's hash and full snapshot are written to
#' `episode_detection_run` so that any result is explainable from the
#' database alone (ARCHITECTURE.md section 7.4).
#'
#' @param db_path Path to the SQLite database file. Created if it does not
#'   exist.
#' @param ingest_source_fn A zero- or one-argument function returning a
#'   data frame satisfying the ingestion interface. Defaults to the bundled
#'   synthetic generator, the only implementation shipped in this
#'   environment (`certedb::get_diver_data()` cannot be called here; see
#'   `QUESTIONS.md` item 11).
#' @param episode_config_path Passed to [episode_config_resolve()].
#' @param host,account Recorded on `episode_detection_run`; default to the
#'   process's own host/account (ARCHITECTURE.md section 12: this is not an
#'   identity source for assessors, only for the run record).
#' @param run_date The date to treat as "today" for closure/eligibility
#'   calculations; defaults to the system date, injectable for tests.
#' @return Invisibly, the `run_id` of the completed run.
#' @export
episode_run_cron <- function(db_path,
                              ingest_source_fn = episode_ingest_source_synthetic,
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
    stats <- episode_run_cron_body(con, run_id, config, ingest_source_fn, run_date)
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
    code_version = as.character(utils::packageVersion("EpiSODE")),
    pkg_versions = as.character(pkg_versions),
    config_hash = hashed$hash, config_snapshot = hashed$snapshot,
    error_text = result$error_text
  )

  invisible(run_id)
}

#' @keywords internal
#' @noRd
episode_run_cron_body <- function(con, run_id, config, ingest_source_fn, run_date) {
  pathogen_config_path <- system.file("config", "pathogen_config.csv", package = "EpiSODE")
  if (identical(pathogen_config_path, "")) {
    pathogen_config_path <- file.path("inst", "config", "pathogen_config.csv")
  }
  pathogen_config <- utils::read.csv(pathogen_config_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  episode_db_pathogen_config_load(con, pathogen_config)

  raw <- ingest_source_fn()
  episode_ingest_run(con, raw, pathogen_config, run_id)

  cases_all <- episode_db_cases(con)
  institutions <- episode_db_institutions(con)

  episode_lattice_enumerate(con, cases_all, institutions)

  streams <- episode_db_streams(con)
  n_detections_total <- 0L
  n_new_total <- 0L
  n_updated_total <- 0L

  same_place_detections <- episode_detect_same_place(con, cases_all, institutions, config)

  streams <- episode_db_streams(con)  # refresh: same_place may have created streams

  for (i in seq_len(nrow(streams))) {
    stream <- streams[i, ]
    stream_cases <- episode_cases_for_stream(cases_all, stream)

    episode_triangle_update(con, stream$stream_id, stream_cases, as.character(run_date))

    stream_detections <- same_place_detections[same_place_detections$stream_id == stream$stream_id, ]

    if (nrow(stream_cases) > 0 &&
        episode_eligibility_gate(stream_cases, run_date, config)) {
      stream_detections <- rbind(
        stream_detections,
        episode_detect_clusters(con, stream_cases, stream$stream_id),
        episode_detect_farrington(con, stream_cases, stream$stream_id)
      )
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

    pc <- pathogen_config[pathogen_config$mo_code == stream$mo_code, ]
    case_free_days <- if (nrow(pc) > 0) pc$case_free_days[1] else config$reconciliation$case_free_days_default

    weights <- config$priority_score$weights
    reconcile_result <- episode_reconcile_stream(
      con, stream_id = stream$stream_id, detections = stream_detections,
      case_free_days = case_free_days, run_id = run_id,
      close_after_runs = config$reconciliation$close_after_runs,
      priority_score_fn = function(candidate) {
        episode_priority_score(
          excess = NA, ratio = candidate$n_cases / max(candidate$n_cases, 1),
          severity_weight = if (nrow(pc) > 0) pc$severity_weight[1] else 1,
          detector_agreement = candidate$detector_agreement, n_detectors = 6,
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
#' L1/L2 streams filter on `mo_code` and `institution_id` (and `ward` for
#' L1). L3-L5 streams filter on `mo_code` and sample date only, an
#' approximation documented in `QUESTIONS.md` alongside the L3/L4 region
#' derivation itself (`R/lattice_enumerate.R`); a real PC4-to-region join
#' (via `certegis`, M5) would tighten this.
#'
#' @param cases All currently known cases.
#' @param stream A single-row stream (from `episode_db_streams()`).
#' @return The subset of `cases` belonging to `stream`.
#' @export
episode_cases_for_stream <- function(cases, stream) {
  matches <- cases$mo_code == stream$mo_code
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
  pkgs <- c("EpiSODE", "certestats", "AMR", "EpiEstim", "surveillance")
  versions <- lapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA
  })
  stats::setNames(versions, pkgs)
}
