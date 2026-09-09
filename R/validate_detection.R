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

#' Measure Detection Against Known Truth
#'
#' The dashboard's Performance screen measures this
#' instance against its own epidemiologists' verdicts. That is the right
#' operational metric and a circular one for a paper: it reports what the
#' board thought of what the system showed them, and an outbreak the
#' detectors miss never becomes a cluster for anyone to have an opinion
#' about, so sensitivity cannot be measured that way at all.
#'
#' This measures the detectors against outbreaks that are known to be
#' there because they were put there. It generates synthetic history with
#' [episodic_synthetic_cases()], replays it week by week through
#' [episodic_run_cron()] against a throwaway database, and compares the
#' clusters that came out against the ground truth that went in.
#'
#' @section How a cluster is matched to an outbreak:
#'
#' On the cases themselves, in both directions, never on overlapping
#' dates. An endemic winter cluster spanning the same fortnight as a
#' seeded outbreak overlaps it perfectly in time and shares not one case
#' with it.
#'
#' - An **outbreak is detected** when some cluster's final membership
#'   holds at least `min_recall` of the outbreak's cases.
#' - A **cluster is a true positive** when at least `min_precision` of
#'   its own cases belong to one seeded outbreak. Anything else is a
#'   false alarm.
#'
#' Both thresholds are arguments because both are choices, and a result
#' that only holds at one particular pair of values is not a result.
#' Sweep them and see ([episodic_validate_rethreshold()] does it without
#' replaying anything; `data-raw/validation/` does it over a grid).
#'
#' @section Three delays, not one:
#'
#' The day something first appeared on the board that turned out to be
#' this outbreak (`delay_from_open`), the day what was on the board was
#' mostly this outbreak (`delay_from_first`), and the day it held most of
#' the whole outbreak, cases still to come included, are three different
#' claims. They can be days apart. All three are reported, because
#' picking one and calling it "the delay" would be choosing a number
#' rather than measuring one.
#'
#' @section Why it replays week by week:
#'
#' A single run over the whole history says nothing about timeliness: it
#' sees the outbreak's last case at the same moment as its first. So the
#' replay steps `run_date` forward one week at a time and hands each run
#' only the cases sampled on or before that date - what a laboratory
#' extract taken that day would actually have contained.
#'
#' Two consequences worth knowing before reading any number out of this.
#' Farrington needs `(farrington$b + 1) * 52` weeks of history and MEM
#' needs two seasons, so an evaluation window that starts before the
#' baseline requirement is met measures the warm-up rather than the
#' method: keep `history_years` at 4 or more if the statistical detectors
#' are meant to be running. And all six seeded outbreaks are anchored to
#' the end of the generated window unless `outbreak_offsets` disperses
#' them, so a prospective evaluation wants them dispersed.
#'
#' @section Cost:
#'
#' Every run refits Farrington for every eligible stream, so the cost is
#' roughly (seeds x weeks) full detection runs. The defaults are
#' deliberately small - one seed, a short window, a single year of
#' history - so this returns in seconds and can be run from an example.
#' They are a demonstration of the machinery, not a study: at one year of
#' history the statistical detectors have no baseline to fit against and
#' report nothing, and one seed is one realisation. The study lives in
#' `data-raw/validation/`, is measured in minutes to hours, and is not
#' run by the test suite, by `R CMD check`, or by anything a user
#' installs.
#'
#' @param seeds RNG seeds to replicate over. Seeds are the replicates:
#'   one realisation of a stochastic generator is an anecdote, so the
#'   summary reports medians and interquartile ranges across them.
#' @param end_date The last day of generated history, and the last run
#'   the replay makes. Defaults to today, which means two runs on
#'   different days are not comparable; pin it for anything whose numbers
#'   are going to be quoted.
#' @param history_years Years of endemic history before the evaluation
#'   window, for the detectors to build a baseline from.
#' @param evaluation_weeks Weekly runs to replay.
#' @param min_recall,min_precision The matching thresholds described
#'   above.
#' @param outbreaks,outbreak_offsets Passed to
#'   [episodic_synthetic_cases()]. `outbreaks = FALSE` generates history
#'   with nothing seeded in it, which is the negative control: every
#'   alarm raised on it is a false one.
#' @param detectors Which detectors to switch on, for a drop-one
#'   analysis. Any of `"farrington"`, `"mem"`, `"same_place"` and
#'   `"rare_trigger"`; the rest are switched off in the instance
#'   configuration this writes, so `config_hash` records which ran.
#' @param config Further instance configuration to write, as a nested
#'   list (e.g. `list(farrington = list(alpha = 0.01))`), merged over the
#'   shipped defaults and validated the same way an operator's file is.
#'   It may not set any detector's `enabled` key - that is what
#'   `detectors` is for, and two mechanisms for one setting is one too
#'   many.
#' @param quiet If `TRUE` (the default), the per-run progress
#'   [episodic_run_cron()] writes is suppressed; a replay is hundreds of
#'   lines of it. Warnings and errors are not suppressed.
#' @return An `episodic_validation` object: a list of
#'   \describe{
#'     \item{`outbreaks`}{One row per (seed, seeded outbreak): whether it
#'       was detected, by which cluster and which detector, how much of
#'       it that cluster held, how late (three ways - see below), and how
#'       much of it was still to come at that moment.}
#'     \item{`clusters`}{One row per (seed, cluster raised): its case-set
#'       precision, whether it counts as a true positive, its priority
#'       score, and which detectors fired on it.}
#'     \item{`runs`}{One row per (seed, run): streams watched, clusters
#'       raised, and how many of those survived lattice suppression.}
#'     \item{`overlap`}{Every (run, cluster, outbreak) that shared a
#'       case, which is the raw material both tables above are derived
#'       from.}
#'     \item{`truth`}{What was injected: the outbreak table and the
#'       case-level membership, per seed. Kept with the result so the
#'       matching thresholds can be varied afterwards
#'       ([episodic_validate_rethreshold()]) without replaying anything.}
#'     \item{`time_to_detection`}{The Kaplan-Meier curve of days to
#'       detection over the outbreaks that began inside the evaluation
#'       window, with the ones nothing found kept in as right-censored
#'       observations rather than dropped.}
#'     \item{`summary`}{The headline numbers, one row per metric and
#'       group.}
#'     \item{`meta`}{The package version, the resolved `config_hash`, the
#'       thresholds, the seeds, and the denominators every figure rests
#'       on.}
#'   }
#' @seealso [episodic_synthetic_ground_truth()] for what is being
#'   measured against, and [episodic_run_cron()] for the detection run
#'   this replays.
#' @examples
#' \donttest{
#' result <- episodic_validate_detection()
#' result$summary[result$summary$metric == "sensitivity", ]
#' }
#' @export
episodic_validate_detection <- function(seeds = 1,
                                        end_date = Sys.Date(),
                                        history_years = 1,
                                        evaluation_weeks = 4,
                                        min_recall = 0.5,
                                        min_precision = 0.5,
                                        outbreaks = TRUE,
                                        outbreak_offsets = NULL,
                                        detectors = episodic_validation_detectors(),
                                        config = NULL,
                                        quiet = TRUE) {
  seeds <- episodic_validation_check_seeds(seeds)
  episodic_validation_check_threshold(min_recall, "min_recall")
  episodic_validation_check_threshold(min_precision, "min_precision")
  if (!is.numeric(evaluation_weeks) || length(evaluation_weeks) != 1 ||
    is.na(evaluation_weeks) || evaluation_weeks < 1) {
    stop("`evaluation_weeks` must be a single number of at least 1.", call. = FALSE)
  }
  if (!is.numeric(history_years) || length(history_years) != 1 ||
    is.na(history_years) || history_years <= 0) {
    stop("`history_years` must be a single positive number of years.", call. = FALSE)
  }

  config_path <- episodic_validation_config_file(detectors, config)
  on.exit(unlink(dirname(config_path), recursive = TRUE), add = TRUE)
  resolved <- episodic_config_resolve(config_path)
  config_hash <- episodic_config_hash(resolved)$hash

  last_run <- episodic_validation_last_run_date(end_date)
  run_dates <- seq(
    last_run - 7 * (as.integer(evaluation_weeks) - 1L),
    last_run,
    by = 7
  )
  generate_from <- min(run_dates) - round(history_years * 365)

  replicates <- lapply(seeds, function(seed) {
    episodic_validation_replicate(
      seed = seed,
      generate_from = generate_from,
      generate_to = last_run,
      run_dates = run_dates,
      outbreaks = outbreaks,
      outbreak_offsets = outbreak_offsets,
      config_path = config_path,
      min_recall = min_recall,
      min_precision = min_precision,
      quiet = quiet
    )
  })

  bind <- function(name) {
    parts <- lapply(replicates, function(r) r[[name]])
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    out
  }
  outbreak_rows <- bind("outbreaks")
  cluster_rows <- bind("clusters")
  run_rows <- bind("runs")

  structure(
    list(
      outbreaks = outbreak_rows,
      clusters = cluster_rows,
      runs = run_rows,
      overlap = bind("overlap"),
      truth = list(
        outbreaks = bind("truth_outbreaks"),
        cases = bind("truth_cases")
      ),
      time_to_detection = episodic_validation_km_from(outbreak_rows),
      summary = episodic_validation_summarise(
        outbreak_rows,
        cluster_rows,
        run_rows
      ),
      meta = list(
        package_version = as.character(utils::packageVersion("EpiSODIC")),
        config_hash = config_hash,
        detectors = sort(detectors),
        seeds = seeds,
        n_seeds = length(seeds),
        run_dates = run_dates,
        n_runs = length(run_dates),
        generated_from = generate_from,
        generated_to = last_run,
        history_years = history_years,
        min_recall = min_recall,
        min_precision = min_precision,
        outbreaks_injected = outbreaks,
        outbreak_offsets = outbreak_offsets,
        n_stream_weeks = sum(run_rows$n_streams),
        generated_at = Sys.time()
      )
    ),
    class = "episodic_validation"
  )
}

#' @rdname episodic_validate_detection
#' @param x An `episodic_validation` object.
#' @param ... Ignored.
#' @export
print.episodic_validation <- function(x, ...) {
  meta <- x$meta
  cat("EpiSODIC detection validation\n")
  cat(
    "  EpiSODIC ", meta$package_version,
    ", config ", substr(meta$config_hash, 1, 12),
    ", detectors: ", paste(meta$detectors, collapse = ", "), "\n",
    sep = ""
  )
  cat(
    "  ", meta$n_seeds, " seed(s) x ", meta$n_runs, " weekly run(s), ",
    format(min(meta$run_dates)), " to ", format(max(meta$run_dates)),
    "\n",
    sep = ""
  )
  cat(
    "  ", nrow(x$outbreaks), " seeded outbreak(s), ",
    sum(x$clusters$counted), " cluster(s) raised, ",
    meta$n_stream_weeks, " stream-week(s)\n",
    sep = ""
  )
  cat(
    "  matching: recall >= ", meta$min_recall,
    ", precision >= ", meta$min_precision, "\n\n",
    sep = ""
  )
  headline <- x$summary[
    x$summary$group_type == "overall" &
      x$summary$metric %in% c(
        "sensitivity",
        "ppv",
        "false_alarms",
        "delay_from_first_case",
        "time_to_detection_km_median"
      ),
    c("metric", "group", "estimate", "ci_low", "ci_high", "unit")
  ]
  print(headline, row.names = FALSE, digits = 3)
  invisible(x)
}

#' The censored time-to-detection curve for a set of outbreak rows
#'
#' Over the outbreaks that began inside the evaluation window only: one
#' that was already over when the replay started was handed to the first
#' run whole, so its delay measures the replay's start date.
#' @param outbreaks Per-outbreak rows.
#' @return `episodic_validation_km()`'s data frame.
#' @keywords internal
#' @noRd
episodic_validation_km_from <- function(outbreaks) {
  prospective <- outbreaks[outbreaks$fully_prospective, , drop = FALSE]
  episodic_validation_km(
    ifelse(
      prospective$detected,
      prospective$delay_from_first,
      prospective$censor_days
    ),
    prospective$detected
  )
}

#' The four detectors, as configuration section names
#' @keywords internal
#' @noRd
episodic_validation_detectors <- function() {
  c("farrington", "mem", "same_place", "rare_trigger")
}

#' One replicate: one seed, its own database, its own weekly replay
#'
#' Every replicate gets a throwaway SQLite database of its own from
#' `tempfile()` and removes it afterwards. It never touches a live
#' instance, and it deliberately does not go through `episodic_demo()`,
#' which creates a user account, sets four environment variables and
#' writes configuration files beside the database.
#' @keywords internal
#' @noRd
episodic_validation_replicate <- function(seed,
                                          generate_from,
                                          generate_to,
                                          run_dates,
                                          outbreaks,
                                          outbreak_offsets,
                                          config_path,
                                          min_recall,
                                          min_precision,
                                          quiet) {
  cases <- episodic_synthetic_cases(
    start_date = generate_from,
    end_date = generate_to,
    seed = seed,
    outbreaks = outbreaks,
    outbreak_offsets = outbreak_offsets
  )
  truth <- episodic_synthetic_ground_truth(cases)
  truth_cases <- merge(
    truth$cases,
    cases[, c("source_key", "sample_date")],
    by = "source_key"
  )

  db_path <- tempfile(pattern = "episodic-validation-", fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)

  sample_dates <- as.Date(cases$sample_date)
  membership <- list()
  cluster_snapshots <- list()
  run_rows <- list()

  for (run_date in as.list(run_dates)) {
    extract <- cases[sample_dates <= run_date, , drop = FALSE]
    run <- function() {
      episodic_run_cron(
        cases = extract,
        db_path = db_path,
        episodic_config_path = config_path,
        run_date = run_date,
        host = "validation",
        account = "validation"
      )
    }
    run_id <- if (isTRUE(quiet)) suppressMessages(run()) else run()

    captured <- episodic_validation_capture(db_path, run_id, run_date)
    membership[[length(membership) + 1]] <- captured$membership
    cluster_snapshots[[length(cluster_snapshots) + 1]] <- captured$clusters
    run_rows[[length(run_rows) + 1]] <- captured$run
  }

  membership <- episodic_validation_rbind(
    membership,
    c("run_date", "cluster_id", "source_key")
  )
  snapshots <- episodic_validation_rbind(
    cluster_snapshots,
    c(
      "run_date",
      "cluster_id",
      "n_cases",
      "priority_score",
      "suppressed",
      "merged"
    )
  )
  runs <- do.call(rbind, run_rows)

  overlap <- episodic_validation_overlap(membership, truth_cases)
  clusters <- episodic_validation_cluster_rows(
    overlap[overlap$run_date == max(run_dates), , drop = FALSE],
    episodic_validation_cluster_table(db_path, snapshots),
    min_precision = min_precision
  )
  outbreak_rows <- episodic_validation_outbreak_rows(
    overlap,
    truth$outbreaks,
    truth_cases,
    run_dates,
    min_recall = min_recall,
    min_precision = min_precision,
    clusters = clusters
  )

  # How many clusters each run put in front of an epidemiologist, before
  # and after lattice suppression. Counted from first appearance rather
  # than from the run's own detections: a cluster is new to the board the
  # week it opens, and is not new again the week after.
  opened <- clusters$opened_run
  new_at <- function(d) !is.na(opened) & opened == d & clusters$counted
  runs$n_clusters_new <- vapply(
    runs$run_date,
    function(d) sum(new_at(d)),
    integer(1)
  )
  runs$n_clusters_new_visible <- vapply(
    runs$run_date,
    function(d) sum(new_at(d) & !clusters$suppressed),
    integer(1)
  )

  # rep() rather than plain assignment: a replicate that raised no
  # clusters at all - which is what a negative control usually is - has
  # zero-row frames here, and `frame$seed <- seed` refuses those.
  outbreak_rows$seed <- rep(seed, nrow(outbreak_rows))
  clusters$seed <- rep(seed, nrow(clusters))
  runs$seed <- rep(seed, nrow(runs))
  overlap$seed <- rep(seed, nrow(overlap))
  truth$outbreaks$seed <- rep(seed, nrow(truth$outbreaks))
  truth_cases$seed <- rep(seed, nrow(truth_cases))
  list(
    outbreaks = outbreak_rows,
    clusters = clusters,
    runs = runs,
    overlap = overlap,
    truth_outbreaks = truth$outbreaks,
    truth_cases = truth_cases
  )
}

#' What one completed run leaves behind, read back
#' @keywords internal
#' @noRd
episodic_validation_capture <- function(db_path, run_id, run_date) {
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  run <- DBI::dbGetQuery(
    con,
    "SELECT run_id, status, n_streams, n_detections
       FROM episodic_detection_run WHERE run_id = ?",
    params = list(run_id)
  )
  if (nrow(run) != 1 || !run$status[1] %in% c("success", "partial")) {
    stop(
      "A validation replay run finished with status '",
      if (nrow(run) == 1) run$status[1] else "<no run row>",
      "' on ",
      format(run_date),
      ". The replay stops here rather than summarising a partial replay ",
      "as though it were a complete one.",
      call. = FALSE
    )
  }
  clusters <- DBI::dbGetQuery(
    con,
    "SELECT cluster_id, n_cases, priority_score, suppressed_by, merged_into
       FROM episodic_cluster WHERE origin = 'detected'"
  )
  membership <- DBI::dbGetQuery(
    con,
    "SELECT cc.cluster_id AS cluster_id, c.source_key AS source_key
       FROM episodic_cluster_case cc
       JOIN episodic_case c ON c.case_id = cc.case_id"
  )
  list(
    run = data.frame(
      run_date = run_date,
      run_id = run$run_id[1],
      n_streams = as.integer(run$n_streams[1]),
      n_detections = as.integer(run$n_detections[1]),
      stringsAsFactors = FALSE
    ),
    clusters = episodic_validation_stamp(
      run_date,
      data.frame(
        cluster_id = clusters$cluster_id,
        n_cases = clusters$n_cases,
        priority_score = clusters$priority_score,
        suppressed = !is.na(clusters$suppressed_by),
        merged = !is.na(clusters$merged_into),
        stringsAsFactors = FALSE
      )
    ),
    membership = episodic_validation_stamp(
      run_date,
      data.frame(
        cluster_id = membership$cluster_id,
        source_key = membership$source_key,
        stringsAsFactors = FALSE
      )
    )
  )
}

#' Put the run's date on every row of a snapshot, including none of them
#'
#' `data.frame(run_date = a_date, cluster_id = integer(0))` is an error
#' about differing numbers of rows, and a run that raised no clusters at
#' all is the ordinary state of a negative control rather than a
#' mistake.
#' @keywords internal
#' @noRd
episodic_validation_stamp <- function(run_date, frame) {
  frame$run_date <- rep(as.Date(run_date), nrow(frame))
  frame[, c("run_date", setdiff(names(frame), "run_date")), drop = FALSE]
}

#' Every cluster the replay raised, in its final state
#'
#' `opened_run` is the first run the cluster was seen in, not the first
#' run a detection was linked to it: a cluster is on the board from the
#' moment it exists.
#' @keywords internal
#' @noRd
episodic_validation_cluster_table <- function(db_path, snapshots) {
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  clusters <- DBI::dbGetQuery(
    con,
    "SELECT c.cluster_id, c.n_cases, c.priority_score, c.detector_agreement,
            c.suppressed_by, c.merged_into, s.level, s.pathogen
       FROM episodic_cluster c
       JOIN episodic_stream s ON s.stream_id = c.stream_id
      WHERE c.origin = 'detected'"
  )
  detections <- DBI::dbGetQuery(
    con,
    "SELECT d.cluster_id, d.detector, r.run_date
       FROM episodic_detection d
       JOIN episodic_detection_run r ON r.run_id = d.run_id
      WHERE d.cluster_id IS NOT NULL"
  )
  first_detector <- rep(NA_character_, nrow(clusters))
  all_detectors <- rep(NA_character_, nrow(clusters))
  for (i in seq_len(nrow(clusters))) {
    mine <- detections[detections$cluster_id == clusters$cluster_id[i], ]
    if (nrow(mine) == 0) {
      next
    }
    earliest <- mine[mine$run_date == min(mine$run_date), ]
    first_detector[i] <- paste(sort(unique(earliest$detector)), collapse = "+")
    all_detectors[i] <- paste(sort(unique(mine$detector)), collapse = "+")
  }
  opened <- vapply(
    clusters$cluster_id,
    function(id) {
      seen <- snapshots$run_date[snapshots$cluster_id == id]
      if (length(seen) == 0) NA_real_ else as.numeric(min(seen))
    },
    numeric(1)
  )
  data.frame(
    cluster_id = clusters$cluster_id,
    level = clusters$level,
    pathogen = clusters$pathogen,
    n_cases = clusters$n_cases,
    priority_score = clusters$priority_score,
    detector_agreement = clusters$detector_agreement,
    suppressed = !is.na(clusters$suppressed_by),
    merged = !is.na(clusters$merged_into),
    opened_run = as.Date(opened, origin = "1970-01-01"),
    first_detector = first_detector,
    detectors = all_detectors,
    stringsAsFactors = FALSE
  )
}

#' `rbind()` a list of frames, with an empty one of the right shape
#'
#' `do.call(rbind, list())` is `NULL`, and a `NULL` here would travel a
#' long way before anything noticed it was not a table with no rows in
#' it.
#' @keywords internal
#' @noRd
episodic_validation_rbind <- function(parts, columns) {
  parts <- parts[vapply(parts, function(p) nrow(p) > 0, logical(1))]
  if (length(parts) == 0) {
    empty <- as.data.frame(
      stats::setNames(rep(list(logical(0)), length(columns)), columns)
    )
    return(empty)
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' The instance configuration one validation run is made under
#'
#' Written into a temporary directory of its own and removed afterwards,
#' so a harness run leaves nothing behind and cannot pick up an
#' operator's `EPISODIC_CONFIG` by accident: the path is handed to
#' [episodic_run_cron()] directly.
#'
#' It goes through `episodic_config_validate()` like any operator's file,
#' so a misspelled key in `config` stops the study before it has spent an
#' hour producing numbers under settings that were never in force.
#' @keywords internal
#' @noRd
episodic_validation_config_file <- function(detectors, config) {
  known <- episodic_validation_detectors()
  if (!is.character(detectors) || anyNA(detectors) || length(detectors) == 0) {
    stop(
      "`detectors` must name at least one of ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unknown <- setdiff(detectors, known)
  if (length(unknown) > 0) {
    stop(
      "`detectors` names no such detector: ",
      paste(unknown, collapse = ", "),
      ". Known detectors are ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.null(config) && !is.list(config)) {
    stop("`config` must be a nested list, or NULL.", call. = FALSE)
  }
  sets_enabled <- vapply(
    known,
    function(d) !is.null(config[[d]]) && "enabled" %in% names(config[[d]]),
    logical(1)
  )
  if (any(sets_enabled)) {
    stop(
      "`config` sets `",
      paste0(known[sets_enabled], ".enabled", collapse = "`, `"),
      "`. Switch detectors on and off with `detectors`, which is the one ",
      "place this harness reads them from.",
      call. = FALSE
    )
  }

  settings <- if (is.null(config)) list() else config
  for (detector in known) {
    settings[[detector]] <- c(
      settings[[detector]],
      list(enabled = detector %in% detectors)
    )
  }
  directory <- tempfile(pattern = "episodic-validation-config-")
  dir.create(directory)
  path <- file.path(directory, "episodic.yaml")
  writeLines(yaml::as.yaml(settings), path)
  path
}

#' The last run date of a replay: the end of the last complete week
#'
#' The statistical detectors test whole weeks, so a replay that stepped
#' to an arbitrary weekday would hand the last run a partial one. Which
#' week counts as complete is `episodic_last_complete_week_start()`'s
#' answer and not a second one taken here: two definitions of the same
#' boundary would put the replay's run dates a week away from the weeks
#' Farrington actually tested at them.
#' @keywords internal
#' @noRd
episodic_validation_last_run_date <- function(end_date) {
  end_date <- as.Date(end_date)
  if (length(end_date) != 1 || is.na(end_date)) {
    stop("`end_date` must be a single date.", call. = FALSE)
  }
  episodic_last_complete_week_start(end_date) + 6
}

#' @keywords internal
#' @noRd
episodic_validation_check_seeds <- function(seeds) {
  if (!is.numeric(seeds) || length(seeds) == 0 || anyNA(seeds)) {
    stop("`seeds` must be one or more RNG seeds, without NA.", call. = FALSE)
  }
  if (anyDuplicated(seeds) > 0) {
    stop(
      "`seeds` repeats ",
      paste(unique(seeds[duplicated(seeds)]), collapse = ", "),
      ". A repeated seed is the same realisation twice, which would ",
      "narrow every interval this reports without adding a replicate.",
      call. = FALSE
    )
  }
  as.numeric(seeds)
}

#' @keywords internal
#' @noRd
episodic_validation_check_threshold <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
    value <= 0 || value > 1) {
    stop(
      "`",
      name,
      "` must be a single share above 0 and at most 1.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
