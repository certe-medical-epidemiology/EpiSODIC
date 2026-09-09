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

# The detection validation study.
#
# Runs everything the paper quotes and writes it to
# data-raw/validation/results/ as CSV, with the package version and the
# resolved config_hash that produced each number.
#
# This directory is in .Rbuildignore: nothing here ships in the built
# package, nothing here runs under R CMD check, and nothing here is
# reachable from an installed EpiSODIC. That is deliberate. The full
# study is measured in hours; episodic_validate_detection()'s own
# defaults are measured in seconds, and those are what the examples and
# the test suite use.
#
# Run it from the package root:
#
#   Rscript data-raw/validation/run_study.R
#
# or, for a five-minute smoke test that exercises every scenario at a
# fraction of the size:
#
#   EPISODIC_VALIDATION_QUICK=true Rscript data-raw/validation/run_study.R
#
# It writes nothing outside data-raw/validation/results/ and touches no
# database but the throwaway SQLite files each replicate creates and
# removes for itself.

suppressMessages(pkgload::load_all(quiet = TRUE))

quick <- tolower(Sys.getenv("EPISODIC_VALIDATION_QUICK")) %in% c("true", "1", "yes")

study <- list(
  # Pinned, not Sys.Date(): the generator anchors its seasons and its
  # outbreaks to the end of the window, so a study run on a different day
  # is a different study. Move it deliberately, and re-run everything.
  end_date = as.Date("2026-06-28"),
  # Seeds are the replicates. One realisation of a stochastic generator
  # is an anecdote.
  seeds = if (quick) 1:2 else 1:20,
  # Farrington needs (b + 1) * 52 weeks and MEM two seasons, so an
  # evaluation window starting before that measures the warm-up rather
  # than the method.
  history_years = if (quick) 4 else 4,
  evaluation_weeks = if (quick) 6 else 26,
  # Fewer seeds for the sweeps: they multiply the number of replays by
  # the number of operating points, and their purpose is the shape of a
  # curve rather than a precise point on it.
  sweep_seeds = if (quick) 1 else 1:5,
  min_recall = 0.5,
  min_precision = 0.5
)

results_dir <- file.path("data-raw", "validation", "results")
raw_dir <- file.path(results_dir, "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

started <- Sys.time()
say <- function(...) {
  cat(
    format(Sys.time(), "%H:%M:%S"),
    sprintf("[%6.1f min] ", as.numeric(Sys.time() - started, units = "mins")),
    ..., "\n",
    sep = ""
  )
}

# --------------------------------------------------------------------
# Dispersing the outbreaks across the evaluation window
#
# All six are anchored to the end of the generated window by default, so
# they sit in its last few months. A prospective evaluation of a 26-week
# window in which every outbreak starts in the last eight of them is
# measuring those eight weeks. `outbreak_offsets` moves each one back,
# and the offsets are computed rather than written down: each outbreak's
# first case falls a different number of days before its own anchor, and
# that number is a property of the generator, not something a script
# should be carrying a copy of.
# --------------------------------------------------------------------
window_days <- 7 * (study$evaluation_weeks - 1)
probe <- episodic_synthetic_cases(
  start_date = study$end_date - 6 * 365,
  end_date = study$end_date,
  seed = min(study$seeds)
)
spans <- episodic_synthetic_ground_truth(probe)$outbreaks
spans$lead <- as.numeric(study$end_date - spans$first_day)
spans <- spans[order(-spans$lead), ]

# Evenly spaced nominal start days, longest-leading outbreak first, with
# a margin at each end so a seed whose draws run a little longer than the
# probe's still lands inside the window, and never earlier than the
# outbreak's own span allows.
margin <- 7
fits <- window_days - margin >= max(spans$lead) + margin
if (fits) {
  starts <- pmax(
    seq(window_days - margin, margin, length.out = nrow(spans)),
    spans$lead + margin
  )
  offsets <- round(starts - spans$lead)
  names(offsets) <- spans$outbreak_id
  say("outbreak offsets (days back from end_date): ", paste(
    names(offsets), offsets,
    sep = "=", collapse = ", "
  ))
} else if (quick) {
  # The smoke test's window is far too short to disperse anything: the
  # propagated outbreak alone spans about 110 days by construction. It
  # runs anchored, which exercises every scenario and measures nothing.
  offsets <- NULL
  say(
    "NOTE: quick mode - the ", window_days, "-day window cannot hold the ",
    max(spans$lead), "-day span of '", spans$outbreak_id[1], "', so every ",
    "outbreak stays anchored to end_date. The timeliness figures this ",
    "produces are meaningless; it is a smoke test."
  )
} else {
  stop(
    "The evaluation window is ", window_days, " days, and '",
    spans$outbreak_id[1], "' alone spans ", max(spans$lead),
    " days, so the outbreaks cannot be dispersed inside it. Raise ",
    "evaluation_weeks to at least ",
    ceiling((max(spans$lead) + 2 * margin) / 7) + 1,
    ".",
    call. = FALSE
  )
}

# --------------------------------------------------------------------
# The scenarios
# --------------------------------------------------------------------
run_scenario <- function(name, ...) {
  say("scenario '", name, "' starting")
  t0 <- Sys.time()
  result <- episodic_validate_detection(
    end_date = study$end_date,
    history_years = study$history_years,
    evaluation_weeks = study$evaluation_weeks,
    min_recall = study$min_recall,
    min_precision = study$min_precision,
    outbreak_offsets = offsets,
    ...
  )
  say(
    "scenario '", name, "' done in ",
    round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min"
  )
  saveRDS(result, file.path(raw_dir, paste0(name, ".rds")))
  result
}

run_comparator <- function(name, method, ...) {
  say("comparator '", name, "' starting")
  result <- episodic_validate_comparator(
    method = method,
    seeds = study$seeds,
    end_date = study$end_date,
    history_years = study$history_years,
    evaluation_weeks = study$evaluation_weeks,
    min_recall = study$min_recall,
    min_precision = study$min_precision,
    outbreak_offsets = offsets,
    ...
  )
  saveRDS(result, file.path(raw_dir, paste0(name, ".rds")))
  result
}

scenarios <- list()

scenarios$main <- run_scenario("main", seeds = study$seeds)

# The negative control: the same endemic history with nothing seeded in
# it. On seeded data every "false" alarm might be a real cluster the
# generator produced by chance, so this is the only clean specificity
# measurement available.
scenarios$negative_control <- run_scenario(
  "negative_control",
  seeds = study$seeds,
  outbreaks = FALSE
)

# Drop-one. Which detector fired first answers "who got there first",
# not "what does each channel add": two detectors may both find an
# outbreak, and the loser looks worthless until you remove the winner.
for (dropped in episodic_validation_detectors()) {
  scenarios[[paste0("drop_", dropped)]] <- run_scenario(
    paste0("drop_", dropped),
    seeds = study$seeds,
    detectors = setdiff(episodic_validation_detectors(), dropped)
  )
}

scenarios$comparator_same_place <- run_comparator(
  "comparator_same_place",
  "same_place"
)
scenarios$comparator_shewhart <- run_comparator(
  "comparator_shewhart",
  "shewhart"
)

# --------------------------------------------------------------------
# The operating-point sweep: sensitivity against false alarms per
# stream-week, which is how this literature presents an aberration
# detector. A sweep for the paper, presented as a curve. It is not a
# licence to move the shipped defaults.
# --------------------------------------------------------------------
#
# The shipped configuration is one point, run once, and belongs to both
# curves: it is farrington alpha 0.05 and same_place 3-in-14 at the same
# time. Listing it under each heading would run the identical
# configuration twice and put two identical points on the plot.
operating_points <- list(
  list(
    key = "shipped",
    dimension = "shipped",
    label = "shipped defaults (alpha 0.05, same_place 3 in 14)",
    config = NULL
  ),
  list(
    key = "farrington_alpha_001",
    dimension = "farrington.alpha",
    label = "farrington alpha 0.01",
    config = list(farrington = list(alpha = 0.01))
  ),
  list(
    key = "farrington_alpha_010",
    dimension = "farrington.alpha",
    label = "farrington alpha 0.10",
    config = list(farrington = list(alpha = 0.10))
  ),
  list(
    key = "same_place_2_in_14",
    dimension = "same_place",
    label = "same_place 2 in 14",
    config = list(same_place = list(default_n_cases = 2, default_k_days = 14))
  ),
  list(
    key = "same_place_4_in_14",
    dimension = "same_place",
    label = "same_place 4 in 14",
    config = list(same_place = list(default_n_cases = 4, default_k_days = 14))
  ),
  list(
    key = "same_place_3_in_7",
    dimension = "same_place",
    label = "same_place 3 in 7",
    config = list(same_place = list(default_n_cases = 3, default_k_days = 7))
  )
)
sweep <- list()
for (point in operating_points) {
  key <- paste0("sweep_", point$key)
  sweep[[key]] <- run_scenario(
    key,
    seeds = study$sweep_seeds,
    config = point$config
  )
  attr(sweep[[key]], "label") <- point$label
  attr(sweep[[key]], "dimension") <- point$dimension
}

# --------------------------------------------------------------------
# Writing it out
# --------------------------------------------------------------------
# Every row says what produced it and what it rests on: the package
# version and resolved config_hash, so a number in the paper traces back
# to the configuration that computed it, and the seeds, runs and
# stream-weeks behind it, so a rate is never read without its denominator
# in the same file.
stamp <- function(df, scenario, result) {
  n <- nrow(df)
  cbind(
    scenario = rep(scenario, n),
    package_version = rep(result$meta$package_version, n),
    config_hash = rep(result$meta$config_hash %||% NA_character_, n),
    n_seeds_total = rep(result$meta$n_seeds, n),
    n_runs = rep(result$meta$n_runs, n),
    n_stream_weeks = rep(result$meta$n_stream_weeks, n),
    df,
    stringsAsFactors = FALSE
  )
}

collect <- function(named_results, part) {
  do.call(rbind, lapply(names(named_results), function(name) {
    stamp(named_results[[name]][[part]], name, named_results[[name]])
  }))
}

all_results <- c(scenarios, sweep)
write_csv <- function(df, file) {
  utils::write.csv(df, file.path(results_dir, file), row.names = FALSE)
  say("wrote ", file, " (", nrow(df), " rows)")
}

write_csv(collect(all_results, "summary"), "summary.csv")
write_csv(collect(all_results, "outbreaks"), "outbreaks.csv")
write_csv(collect(all_results, "clusters"), "clusters.csv")
write_csv(collect(all_results, "runs"), "runs.csv")

# Time to detection, with the outbreaks nothing found kept in as censored
# observations. A median over the detected ones alone is biased downward,
# and the bias is worst exactly where the method is weakest.
km_rows <- do.call(rbind, lapply(names(all_results), function(name) {
  km <- all_results[[name]]$time_to_detection
  if (nrow(km) == 0) {
    return(NULL)
  }
  cbind(scenario = name, km, stringsAsFactors = FALSE)
}))
write_csv(km_rows, "time_to_detection_km.csv")

# Does the priority score rank true outbreaks above false alarms? The
# score exists to decide what an epidemiologist looks at first, so its
# ranking is a claim the system makes whether or not anyone measures it.
counted <- scenarios$main$clusters[scenarios$main$clusters$counted, ]
write_csv(
  episodic_validation_calibration(counted$priority_score, counted$true_positive),
  "priority_score_calibration.csv"
)

# Sensitivity to the matching thresholds: the headline numbers should not
# depend on the particular pair chosen. Re-matched from the replay that
# has already been run, so the sweep costs arithmetic.
threshold_grid <- expand.grid(
  min_recall = c(0.2, 0.35, 0.5, 0.65, 0.8),
  min_precision = c(0.2, 0.35, 0.5, 0.65, 0.8)
)
threshold_rows <- do.call(rbind, lapply(seq_len(nrow(threshold_grid)), function(i) {
  rematched <- episodic_validate_rethreshold(
    scenarios$main,
    min_recall = threshold_grid$min_recall[i],
    min_precision = threshold_grid$min_precision[i]
  )
  headline <- rematched$summary[
    rematched$summary$group_type %in% c("overall", "outbreak_shape"),
  ]
  cbind(
    min_recall = threshold_grid$min_recall[i],
    min_precision = threshold_grid$min_precision[i],
    headline,
    stringsAsFactors = FALSE
  )
}))
write_csv(threshold_rows, "threshold_sensitivity.csv")

# What each detector actually adds. Which one fired first says who got
# there first, not what would be lost without it: two detectors may both
# find an outbreak, and the loser looks worthless until the winner is
# removed. This is that comparison, stated directly rather than left to
# be read out of two rows of summary.csv.
headline <- function(result, metric, group, column = "estimate") {
  row <- result$summary[
    result$summary$metric == metric & result$summary$group == group,
  ]
  if (nrow(row) == 0) NA_real_ else row[[column]][1]
}
drop_one <- do.call(rbind, lapply(episodic_validation_detectors(), function(dropped) {
  scenario <- scenarios[[paste0("drop_", dropped)]]
  base_sensitivity <- headline(scenarios$main, "sensitivity", "all outbreaks")
  base_alarms <- headline(scenarios$main, "false_alarms", "all clusters raised")
  without_sensitivity <- headline(scenario, "sensitivity", "all outbreaks")
  without_alarms <- headline(scenario, "false_alarms", "all clusters raised")
  found_only_by_it <- vapply(
    sort(unique(scenarios$main$outbreaks$outbreak_id)),
    function(id) {
      with_it <- scenarios$main$outbreaks
      without <- scenario$outbreaks
      sum(with_it$detected[with_it$outbreak_id == id]) -
        sum(without$detected[without$outbreak_id == id])
    },
    numeric(1)
  )
  data.frame(
    detector_removed = dropped,
    config_hash = scenario$meta$config_hash,
    sensitivity_with = base_sensitivity,
    sensitivity_without = without_sensitivity,
    sensitivity_lost = base_sensitivity - without_sensitivity,
    false_alarms_with = base_alarms,
    false_alarms_without = without_alarms,
    false_alarms_saved = base_alarms - without_alarms,
    # The rates above are per stream-week, and the two scenarios do not
    # watch the same number of streams: same_place and rare_trigger
    # create streams of their own, so removing one shrinks the
    # denominator as well as the numerator, and the difference of two
    # rates can come out the wrong sign. The counts and their
    # denominators are here so that never has to be guessed at.
    false_alarm_count_with = headline(
      scenarios$main, "false_alarms", "all clusters raised", "numerator"
    ),
    false_alarm_count_without = headline(
      scenario, "false_alarms", "all clusters raised", "numerator"
    ),
    stream_weeks_with = scenarios$main$meta$n_stream_weeks,
    stream_weeks_without = scenario$meta$n_stream_weeks,
    # Which shapes stop being found at all when it goes, counted in
    # seed-detections rather than in outbreaks, so a channel that only
    # matters in some realisations is still visible.
    detections_lost_by_shape = paste(
      names(found_only_by_it)[found_only_by_it > 0],
      found_only_by_it[found_only_by_it > 0],
      sep = "=",
      collapse = " "
    ),
    stringsAsFactors = FALSE
  )
}))
write_csv(drop_one, "drop_one.csv")

# The operating-point curve itself, one row per point.
curve <- do.call(rbind, lapply(names(sweep), function(name) {
  result <- sweep[[name]]
  data.frame(
    point = name,
    dimension = attr(result, "dimension"),
    label = attr(result, "label"),
    config_hash = result$meta$config_hash,
    n_seeds = result$meta$n_seeds,
    sensitivity = headline(result, "sensitivity", "all outbreaks"),
    ppv = headline(result, "ppv", "all clusters raised"),
    false_alarms_per_stream_week = headline(
      result,
      "false_alarms",
      "all clusters raised"
    ),
    stream_weeks = result$meta$n_stream_weeks,
    median_delay_days = headline(
      result,
      "delay_from_first_case",
      "detected outbreaks that began inside the window"
    ),
    stringsAsFactors = FALSE
  )
}))
write_csv(curve, "operating_points.csv")

write_csv(
  data.frame(
    key = c(
      "package_version",
      "end_date",
      "seeds",
      "history_years",
      "evaluation_weeks",
      "sweep_seeds",
      "min_recall",
      "min_precision",
      "outbreak_offsets",
      "run_dates",
      "stream_weeks_main",
      "quick_mode",
      "generated_at",
      "elapsed_minutes"
    ),
    value = c(
      scenarios$main$meta$package_version,
      format(study$end_date),
      paste(study$seeds, collapse = " "),
      study$history_years,
      study$evaluation_weeks,
      paste(study$sweep_seeds, collapse = " "),
      study$min_recall,
      study$min_precision,
      paste(names(offsets), offsets, sep = "=", collapse = " "),
      paste(range(format(scenarios$main$meta$run_dates)), collapse = " to "),
      scenarios$main$meta$n_stream_weeks,
      quick,
      format(Sys.time()),
      round(as.numeric(Sys.time() - started, units = "mins"), 1)
    ),
    stringsAsFactors = FALSE
  ),
  "study_meta.csv"
)

# Outbreaks that were already over when the replay began have a delay
# that measures the replay's start date and nothing else. They are kept
# in the sensitivity figures and excluded from the timeliness ones; if
# any turn up, say so here rather than leaving it to be noticed in the
# CSV.
retrospective <- scenarios$main$outbreaks[
  !scenarios$main$outbreaks$fully_prospective,
]
if (nrow(retrospective) > 0) {
  say(
    "NOTE: ", nrow(retrospective), " of ",
    nrow(scenarios$main$outbreaks),
    " seeded outbreaks began before the first run and are excluded from ",
    "the timeliness figures: ",
    paste(sort(unique(retrospective$outbreak_id)), collapse = ", ")
  )
}

say("done. Results in ", results_dir)
print(scenarios$main)
