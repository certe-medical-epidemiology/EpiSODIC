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

#' Measure a Trivial Rule Against the Same Known Truth
#'
#' A sensitivity of 0.8 and one false alarm per two thousand
#' stream-weeks mean very little on their own. They mean a great deal
#' next to what a rule anyone could write in an afternoon gets on the
#' same data. This runs such a rule over exactly the same generated
#' history, week by week, and reports it through the same matching and
#' the same metrics as [episodic_validate_detection()], so the two sit in
#' one table.
#'
#' Two rules are available:
#'
#' \describe{
#'   \item{`"same_place"`}{Any `n_cases` cases of one pathogen at one
#'     place within `k_days`. A place is a ward inside a hospital and the
#'     institution itself everywhere else, which is what
#'     `episodic_detect_same_place()` watches. Cases at one place more
#'     than `case_free_days` apart belong to separate alarms.}
#'   \item{`"shewhart"`}{A plain control limit on weekly counts: the
#'     whole catchment's count for one pathogen in the week just
#'     completed, against the mean plus `sd_multiplier` standard
#'     deviations of the `baseline_weeks` weeks before it. Consecutive
#'     alarming weeks are one alarm, not one each.}
#' }
#'
#' Neither reconciles, scores, suppresses or ages anything, which is the
#' point: they are the floor a four-detector design has to clear to be
#' worth its complexity. Their false-alarm denominator is their own -
#' places watched, or pathogens watched - and not EpiSODIC's stream
#' count, so read `denominator` before comparing two rates.
#'
#' @inheritParams episodic_validate_detection
#' @param method Which rule to run.
#' @param n_cases,k_days,case_free_days The `"same_place"` rule's
#'   parameters: how many cases, within how many days, and how long a
#'   quiet gap ends one alarm and starts the next.
#' @param baseline_weeks,sd_multiplier The `"shewhart"` rule's
#'   parameters.
#' @return An `episodic_validation` object, in the same shape
#'   [episodic_validate_detection()] returns, with `meta$method` naming
#'   the rule and `meta$config_hash` absent - a rule this simple has no
#'   configuration to hash.
#' @seealso [episodic_validate_detection()]
#' @examples
#' \donttest{
#' naive <- episodic_validate_comparator()
#' naive$summary[naive$summary$metric == "sensitivity", ]
#' }
#' @export
episodic_validate_comparator <- function(method = c("same_place", "shewhart"),
                                         seeds = 1,
                                         end_date = Sys.Date(),
                                         history_years = 1,
                                         evaluation_weeks = 4,
                                         min_recall = 0.5,
                                         min_precision = 0.5,
                                         outbreaks = TRUE,
                                         outbreak_offsets = NULL,
                                         n_cases = 3,
                                         k_days = 7,
                                         case_free_days = 14,
                                         baseline_weeks = 52,
                                         sd_multiplier = 2,
                                         quiet = TRUE) {
  method <- match.arg(method)
  seeds <- episodic_validation_check_seeds(seeds)
  episodic_validation_check_threshold(min_recall, "min_recall")
  episodic_validation_check_threshold(min_precision, "min_precision")

  last_run <- episodic_validation_last_run_date(end_date)
  run_dates <- seq(
    last_run - 7 * (as.integer(evaluation_weeks) - 1L),
    last_run,
    by = 7
  )
  generate_from <- min(run_dates) - round(history_years * 365)
  params <- list(
    n_cases = n_cases,
    k_days = k_days,
    case_free_days = case_free_days,
    baseline_weeks = baseline_weeks,
    sd_multiplier = sd_multiplier
  )

  replicates <- lapply(seeds, function(seed) {
    episodic_validation_comparator_replicate(
      method = method,
      params = params,
      seed = seed,
      generate_from = generate_from,
      generate_to = last_run,
      run_dates = run_dates,
      outbreaks = outbreaks,
      outbreak_offsets = outbreak_offsets,
      min_recall = min_recall,
      min_precision = min_precision
    )
  })

  bind <- function(name) {
    out <- do.call(rbind, lapply(replicates, function(r) r[[name]]))
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
        method = method,
        parameters = params,
        detectors = method,
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

#' One comparator replicate, replayed the same way a real one is
#'
#' No database and no detection run: the rule is arithmetic over the
#' extract, so each week's extract is filtered out of the same generated
#' case set and handed straight to the rule.
#' @keywords internal
#' @noRd
episodic_validation_comparator_replicate <- function(method,
                                                     params,
                                                     seed,
                                                     generate_from,
                                                     generate_to,
                                                     run_dates,
                                                     outbreaks,
                                                     outbreak_offsets,
                                                     min_recall,
                                                     min_precision) {
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
  cases$sample_date <- as.Date(cases$sample_date)
  cases$place_key <- episodic_validation_place_key(cases)

  membership <- list()
  run_rows <- list()
  registry <- character(0)
  raised <- list()

  for (run_date in as.list(run_dates)) {
    extract <- cases[cases$sample_date <= run_date, , drop = FALSE]
    alarms <- if (identical(method, "same_place")) {
      episodic_validation_rule_same_place(extract, run_date, params)
    } else {
      episodic_validation_rule_shewhart(extract, run_date, params)
    }
    # Alarm keys are text, and everything downstream expects the integer
    # cluster ids a database hands out. The registry is that: first come,
    # first numbered, and stable across runs so an alarm keeps its
    # identity as it grows.
    new_keys <- setdiff(unique(alarms$alarm_key), registry)
    registry <- c(registry, new_keys)

    # An alarm, once raised, stays on the board with the cases it holds,
    # and is still there at the end of the replay. That is what a cluster
    # does, and a comparator judged on a different rule is not a
    # comparator. The Shewhart rule in particular only speaks while the
    # week it is testing exceeds its limit, so without this its alarms
    # vanished from the snapshot as soon as the rise passed - and the
    # final-state matching then scored a rule that had held an outbreak
    # at recall 1.00 as having missed it entirely.
    for (key in unique(alarms$alarm_key)) {
      raised[[key]] <- union(
        raised[[key]],
        alarms$source_key[alarms$alarm_key == key]
      )
    }
    standing <- if (length(raised) == 0) {
      data.frame(
        cluster_id = integer(0),
        source_key = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        cluster_id = rep(
          match(names(raised), registry),
          lengths(raised)
        ),
        source_key = unlist(raised, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    }
    membership[[length(membership) + 1]] <- episodic_validation_stamp(
      run_date,
      standing
    )
    run_rows[[length(run_rows) + 1]] <- data.frame(
      run_date = run_date,
      run_id = NA_integer_,
      n_streams = episodic_validation_comparator_groups(extract, method),
      n_detections = length(unique(alarms$alarm_key)),
      stringsAsFactors = FALSE
    )
  }

  membership <- episodic_validation_rbind(
    membership,
    c("run_date", "cluster_id", "source_key")
  )
  runs <- do.call(rbind, run_rows)
  overlap <- episodic_validation_overlap(membership, truth_cases)

  final <- membership[membership$run_date == max(run_dates), , drop = FALSE]
  opened <- vapply(
    seq_along(registry),
    function(id) {
      seen <- membership$run_date[membership$cluster_id == id]
      if (length(seen) == 0) NA_real_ else as.numeric(min(seen))
    },
    numeric(1)
  )
  table_rows <- data.frame(
    cluster_id = seq_along(registry),
    level = NA_character_,
    pathogen = NA_character_,
    n_cases = vapply(
      seq_along(registry),
      function(id) sum(final$cluster_id == id),
      integer(1)
    ),
    # A rule this simple ranks nothing, so there is no score to judge
    # its triage by. NA, not zero: it did not score these badly, it did
    # not score them at all.
    priority_score = NA_real_,
    detector_agreement = NA_integer_,
    suppressed = FALSE,
    merged = FALSE,
    opened_run = as.Date(opened, origin = "1970-01-01"),
    first_detector = method,
    detectors = method,
    stringsAsFactors = FALSE
  )
  clusters <- episodic_validation_cluster_rows(
    overlap[overlap$run_date == max(run_dates), , drop = FALSE],
    table_rows,
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

  new_at <- function(d) {
    !is.na(clusters$opened_run) & clusters$opened_run == d & clusters$counted
  }
  runs$n_clusters_new <- vapply(
    runs$run_date,
    function(d) sum(new_at(d)),
    integer(1)
  )
  runs$n_clusters_new_visible <- runs$n_clusters_new

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

#' The place a case counts as being at
#'
#' A ward inside a hospital, the institution itself everywhere else -
#' long-term care, general practice and out-of-hours services have no
#' wards, and the institution is the transmission unit there anyway.
#' @keywords internal
#' @noRd
episodic_validation_place_key <- function(cases) {
  ward <- ifelse(
    cases$institution_type == "hospital" & !is.na(cases$ward),
    cases$ward,
    ""
  )
  paste(cases$institution_key, ward, sep = "\r")
}

#' How many places, or pathogens, the rule was watching at this run
#' @keywords internal
#' @noRd
episodic_validation_comparator_groups <- function(extract, method) {
  if (nrow(extract) == 0) {
    return(0L)
  }
  if (identical(method, "same_place")) {
    length(unique(paste(extract$pathogen, extract$place_key, sep = "\r")))
  } else {
    length(unique(extract$pathogen))
  }
}

#' "Any N cases of one pathogen at one place within K days"
#'
#' @param extract The cases sampled on or before `run_date`.
#' @param run_date The date the run treats as today.
#' @param params `n_cases`, `k_days`, `case_free_days`.
#' @return One row per (alarm, case): `alarm_key`, `source_key`.
#' @keywords internal
#' @noRd
episodic_validation_rule_same_place <- function(extract, run_date, params) {
  empty <- data.frame(
    alarm_key = character(0),
    source_key = character(0),
    stringsAsFactors = FALSE
  )
  if (nrow(extract) == 0) {
    return(empty)
  }
  groups <- split(
    seq_len(nrow(extract)),
    paste(extract$pathogen, extract$place_key, sep = "\r")
  )
  rows <- lapply(names(groups), function(key) {
    idx <- groups[[key]]
    idx <- idx[order(extract$sample_date[idx])]
    days <- extract$sample_date[idx]
    # A quiet gap ends one alarm and starts the next, the same way
    # reconciliation's case-free rule does. Without it every case a place
    # has ever had is one endless alarm.
    episode <- cumsum(c(TRUE, diff(days) > params$case_free_days))
    parts <- lapply(unique(episode), function(e) {
      members <- idx[episode == e]
      dates <- extract$sample_date[members]
      fires <- any(vapply(
        seq_along(dates),
        function(i) sum(dates >= dates[i] & dates <= dates[i] + params$k_days),
        integer(1)
      ) >= params$n_cases)
      if (!fires) {
        return(NULL)
      }
      data.frame(
        alarm_key = paste(key, min(dates), sep = "\r"),
        source_key = extract$source_key[members],
        stringsAsFactors = FALSE
      )
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) empty else do.call(rbind, rows)
}

#' A 2-SD control limit on the catchment's weekly counts
#'
#' Tests the week just completed, as the statistical detectors do, and
#' against the same kind of baseline: the weeks immediately before it.
#' Consecutive alarming weeks belong to one alarm, so a six-week rise is
#' one alarm rather than six.
#' @keywords internal
#' @noRd
episodic_validation_rule_shewhart <- function(extract, run_date, params) {
  empty <- data.frame(
    alarm_key = character(0),
    source_key = character(0),
    stringsAsFactors = FALSE
  )
  if (nrow(extract) == 0) {
    return(empty)
  }
  week_start <- episodic_week_start(extract$sample_date)
  tested_week <- episodic_last_complete_week_start(run_date)
  rows <- lapply(unique(extract$pathogen), function(pathogen) {
    mine <- extract$pathogen == pathogen
    weeks <- seq(
      tested_week - 7L * params$baseline_weeks,
      tested_week,
      by = 7
    )
    counts <- vapply(
      weeks,
      function(w) sum(mine & week_start == w),
      integer(1)
    )
    baseline <- counts[-length(counts)]
    if (length(baseline) < params$baseline_weeks) {
      return(NULL)
    }
    limit <- mean(baseline) + params$sd_multiplier * stats::sd(baseline)
    if (!is.finite(limit) || counts[length(counts)] <= limit) {
      return(NULL)
    }
    # How far back the current run of alarming weeks goes. Walked
    # backwards week by week, each against its own baseline, so the
    # alarm's identity is the week the rise started and does not change
    # as the rise continues.
    #
    # Bounded at the first week the extract holds. The walk does stop on
    # its own once it reaches weeks with no cases in them - an all-zero
    # baseline gives a limit of zero and a count of zero does not exceed
    # it - but relying on that is relying on the data, and a rule this
    # simple should not be able to walk off the start of its own extract.
    earliest <- min(week_start)
    start <- tested_week
    repeat {
      previous <- start - 7L
      if (previous < earliest) {
        break
      }
      window <- seq(previous - 7L * params$baseline_weeks, previous, by = 7)
      window_counts <- vapply(
        window,
        function(w) sum(mine & week_start == w),
        integer(1)
      )
      previous_baseline <- window_counts[-length(window_counts)]
      if (length(previous_baseline) < params$baseline_weeks) {
        break
      }
      previous_limit <- mean(previous_baseline) +
        params$sd_multiplier * stats::sd(previous_baseline)
      if (!is.finite(previous_limit) ||
        window_counts[length(window_counts)] <= previous_limit) {
        break
      }
      start <- previous
    }
    members <- which(mine & week_start >= start & week_start <= tested_week)
    if (length(members) == 0) {
      return(NULL)
    }
    data.frame(
      alarm_key = paste(pathogen, start, sep = "\r"),
      source_key = extract$source_key[members],
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) empty else do.call(rbind, rows)
}
