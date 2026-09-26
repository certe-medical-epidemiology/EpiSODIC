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

# Every faster route through a detection run is held here against the
# direct definition of what it computes, on randomised input, and must
# return an identical() result - not an equal-within-tolerance one. The
# reference implementations below are the definitions written out the
# plain way, one case or one week at a time.

# -- References --------------------------------------------------------

throughput_ref_hit_windows <- function(dates_sorted, n, k_days) {
  if (length(dates_sorted) < n) {
    return(list())
  }
  hit <- logical(length(dates_sorted))
  for (i in seq_along(dates_sorted)) {
    in_window <- dates_sorted >= dates_sorted[i] &
      dates_sorted <= dates_sorted[i] + k_days
    if (sum(in_window) >= n) hit[in_window] <- TRUE
  }
  if (!any(hit)) {
    return(list())
  }
  hit_idx <- which(hit)
  gaps <- as.numeric(diff(dates_sorted[hit_idx]), units = "days")
  episode <- cumsum(c(TRUE, gaps > k_days))
  windows <- list()
  for (e in unique(episode)) {
    idx <- hit_idx[episode == e]
    windows[[length(windows) + 1]] <- list(
      first_day = as.character(min(dates_sorted[idx])),
      last_day = as.character(max(dates_sorted[idx])),
      n_cases = length(idx)
    )
  }
  windows
}

throughput_ref_weekly_bins <- function(dates, run_date) {
  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(list(week_start = as.Date(character(0)), counts = integer(0)))
  }
  first_week <- episodic_week_start(min(dates))
  last_week <- episodic_last_complete_week_start(run_date)
  if (last_week < first_week) {
    return(list(week_start = as.Date(character(0)), counts = integer(0)))
  }
  week_start <- seq(first_week, last_week, by = "week")
  counts <- vapply(
    week_start,
    function(ws) sum(dates >= ws & dates < ws + 7),
    integer(1)
  )
  list(week_start = week_start, counts = counts)
}

# ISO year and week of every case, and the years spanned end to end,
# computed per case.
throughput_ref_years <- function(dates) {
  data_start <- min(dates)
  data_end <- max(dates)
  all_years <- sort(unique(as.integer(format(dates, "%G"))))
  complete <- vapply(all_years, function(yr) {
    data_start <= episodic_iso_week_start(yr, 1L) &&
      data_end >= episodic_iso_week_start(yr, 52L) + 6L
  }, logical(1))
  all_years[complete]
}

throughput_ref_year_week <- function(dates, years) {
  iso_weeks <- pmin(as.integer(format(dates, "%V")), 52L)
  iso_years <- as.integer(format(dates, "%G"))
  mat <- matrix(0L, 52L, length(years), dimnames = list(NULL, as.character(years)))
  for (i in seq_along(dates)) {
    column <- match(iso_years[i], years)
    if (!is.na(column)) {
      mat[iso_weeks[i], column] <- mat[iso_weeks[i], column] + 1L
    }
  }
  mat
}

throughput_ref_anchor <- function(cases, config) {
  dates <- as.Date(cases$sample_date)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(NULL)
  }
  min_years <- as.integer(config$mem$min_climatology_years %||% 3L)
  smooth_weeks <- as.integer(config$mem$climatology_smooth_weeks %||% 5L)
  trough_pctl <- as.numeric(config$mem$trough_percentile %||% 0.25)
  complete_years <- throughput_ref_years(dates)
  if (length(complete_years) < min_years) {
    return(NULL)
  }
  mat <- throughput_ref_year_week(dates, complete_years)
  climatology <- vapply(1:52, function(w) {
    stats::median(as.integer(mat[w, ]))
  }, numeric(1))
  half_w <- smooth_weeks %/% 2L
  smoothed <- vapply(1:52, function(i) {
    mean(climatology[((i - 1L - half_w):(i - 1L + half_w)) %% 52L + 1L])
  }, numeric(1))
  threshold <- stats::quantile(smoothed, probs = trough_pctl, names = FALSE)
  runs <- episodic_mem_circular_runs(which(smoothed <= threshold), 52L)
  if (length(runs) == 0) {
    return(NULL)
  }
  best <- NULL
  for (run in runs) {
    if (is.null(best) ||
      length(run) > length(best) ||
      (length(run) == length(best) &&
        min(smoothed[run]) < min(smoothed[best]))) {
      best <- run
    }
  }
  list(
    anchor_week = as.integer(best[1]),
    trough_run = as.integer(best),
    climatology = smoothed
  )
}

throughput_ref_seasonal_matrix <- function(cases, anchor_week) {
  dates <- as.Date(cases$sample_date)
  assigned <- lapply(dates, episodic_mem_season_week, anchor_week = anchor_week)
  seasons <- vapply(assigned, function(a) a$season, character(1))
  weeks <- vapply(assigned, function(a) a$week_label, character(1))
  week_order <- episodic_mem_week_order(anchor_week)
  season_levels <- sort(unique(seasons))
  mat <- matrix(
    0L,
    nrow = 52L,
    ncol = length(season_levels),
    dimnames = list(week_order, season_levels)
  )
  tab <- table(
    factor(weeks, levels = week_order),
    factor(seasons, levels = season_levels)
  )
  mat[rownames(tab), colnames(tab)] <- tab
  list(matrix = mat)
}

# A seasonal series: a winter peak on a flat floor, over `years` years.
throughput_seasonal_cases <- function(seed, years = 5, peak = 40) {
  set.seed(seed)
  days <- seq(as.Date("2019-07-01"), by = "day", length.out = years * 365)
  week <- as.integer(format(days, "%V"))
  rate <- 0.4 + peak / 7 * exp(-((pmin(abs(week - 4), 52 - abs(week - 4)))^2) / 18)
  n <- stats::rpois(length(days), rate)
  data.frame(
    sample_date = as.character(rep(days, n)),
    stringsAsFactors = FALSE
  )
}

# -- same_place ----------------------------------------------------------

test_that("the same_place scan finds exactly the windows of the pairwise definition", {
  set.seed(20260926)
  for (r in seq_len(1500)) {
    n_dates <- sample(0:60, 1)
    dates <- sort(as.Date("2024-01-01") + sample(0:sample(1:400, 1), n_dates, replace = TRUE))
    n <- sample(1:6, 1)
    k_days <- sample(0:30, 1)
    expect_identical(
      episodic_same_place_hit_windows(dates, n, k_days),
      throughput_ref_hit_windows(dates, n, k_days),
      info = paste("draw", r)
    )
  }
})

test_that("the same_place scan handles one date, all-equal dates and wide gaps", {
  one <- as.Date("2025-03-01")
  expect_identical(
    episodic_same_place_hit_windows(one, 1, 0),
    throughput_ref_hit_windows(one, 1, 0)
  )
  same <- rep(as.Date("2025-03-01"), 5)
  expect_identical(
    episodic_same_place_hit_windows(same, 5, 0),
    throughput_ref_hit_windows(same, 5, 0)
  )
  apart <- as.Date(c("2021-01-01", "2021-01-02", "2021-01-03", "2025-06-01", "2025-06-02", "2025-06-04"))
  windows <- episodic_same_place_hit_windows(apart, 3, 7)
  expect_identical(windows, throughput_ref_hit_windows(apart, 3, 7))
  expect_length(windows, 2)
})

# -- Farrington ------------------------------------------------------------

test_that("weekly bins count exactly what a per-week comparison counts", {
  set.seed(7)
  for (r in seq_len(300)) {
    dates <- as.Date("2021-01-01") + sample(0:1500, sample(0:400, 1), replace = TRUE)
    if (length(dates) > 0 && stats::runif(1) < 0.2) {
      dates[sample(length(dates), 1)] <- NA
    }
    run_date <- as.Date("2021-01-01") + sample(-10:1700, 1)
    expect_identical(
      episodic_weekly_bins(dates, run_date),
      throughput_ref_weekly_bins(dates, run_date),
      info = paste("draw", r)
    )
  }
})

test_that("a population offset reaches farringtonFlexible() in a shape sts() accepts", {
  dates <- as.Date("2020-01-06") + rep(0:(52 * 4), each = 1) * 7
  weekly <- episodic_weekly_bins(dates, as.Date("2024-02-01"))
  population <- rep(1000, length(weekly$counts))
  fc <- episodic_config_resolve(NA)$farrington
  fc$b <- 2
  result <- episodic_farrington_fit(
    weekly,
    range_idx = length(weekly$counts),
    fc = fc,
    population = population
  )
  expect_s4_class(result, "sts")
  # The result holds the tested week only.
  expect_equal(as.numeric(surveillance::population(result)), 1000)
})

throughput_farrington_series <- function(seed) {
  set.seed(seed)
  days <- seq(as.Date("2019-01-07"), as.Date("2024-03-03"), by = "day")
  week <- as.integer(format(days, "%V"))
  n <- stats::rpois(length(days), 0.6 + 0.5 * cos(2 * pi * week / 52))
  n[sample(length(days), 20)] <- n[sample(length(days), 20)] + 6L
  data.frame(sample_date = as.character(rep(days, n)), stringsAsFactors = FALSE)
}

test_that("a week's Farrington fit is the same whichever weeks it is fitted with", {
  fc <- episodic_config_resolve(NA)$farrington
  for (seed in 1:3) {
    cases <- throughput_farrington_series(seed)
    weekly <- episodic_weekly_bins(as.Date(cases$sample_date), as.Date("2024-03-03"))
    last <- length(weekly$counts)
    first <- (fc$b + 1) * 52
    memo <- episodic_farrington_fit_memo()
    for (range_idx in list(last, seq(last - 3, last), seq(first, last), seq(last - 20, last - 10), last - 5)) {
      expect_identical(
        memo(weekly, range_idx, fc),
        {
          direct <- episodic_farrington_fit_weeks(weekly, range_idx, fc)
          rownames(direct) <- NULL
          direct
        },
        info = paste("seed", seed, "weeks", min(range_idx), "to", max(range_idx))
      )
    }
  }
})

test_that("the detector and the trend cache write the same with a shared fit as with their own", {
  config <- episodic_config_resolve(NA)
  run_date <- as.Date("2024-03-03")
  for (seed in 1:3) {
    cases <- throughput_farrington_series(seed)
    for (n_weeks in c(1L, 4L)) {
      for (existing in c(0L, 10L)) {
        memo <- episodic_farrington_fit_memo()
        expect_identical(
          episodic_detect_farrington(cases, 1L, config, run_date, n_weeks = n_weeks, fit = memo),
          episodic_detect_farrington(cases, 1L, config, run_date, n_weeks = n_weeks)
        )
        expect_identical(
          episodic_farrington_trend(cases, config, run_date, n_weeks_existing = existing, fit = memo),
          episodic_farrington_trend(cases, config, run_date, n_weeks_existing = existing)
        )
      }
    }
  }
})

test_that("a shared Farrington fit refuses a series it was not fitted on", {
  fc <- episodic_config_resolve(NA)$farrington
  cases <- throughput_farrington_series(1)
  weekly <- episodic_weekly_bins(as.Date(cases$sample_date), as.Date("2024-03-03"))
  memo <- episodic_farrington_fit_memo()
  memo(weekly, length(weekly$counts), fc)
  other <- weekly
  other$counts[1] <- other$counts[1] + 1L
  expect_error(memo(other, length(other$counts), fc), "different series")
})

test_that("the batched activity read gives every institution the vector the per-institution read gives", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  ids <- c(
    episodic_test_institution(con, "hosp-a"),
    episodic_test_institution(con, "hosp-b"),
    episodic_test_institution(con, "hosp-c")
  )
  for (k in 1:2) {
    for (m in 1:6) {
      start <- seq(as.Date("2024-01-01"), by = "month", length.out = 7)
      DBI::dbExecute(
        con,
        "INSERT INTO episodic_institution_activity
           (institution_id, period_start, period_end, patient_days)
         VALUES (?, ?, ?, ?)",
        params = list(
          ids[k],
          as.character(start[m]),
          as.character(start[m + 1] - 1),
          if (m == 3) NA_integer_ else as.integer(100 * k + m)
        )
      )
    }
  }
  weeks <- seq(as.Date("2023-12-04"), by = "week", length.out = 40)
  activity <- episodic_db_institution_activity_all(con)
  for (id in ids) {
    expect_identical(
      episodic_farrington_population_from_activity(
        activity[[as.character(id)]],
        weeks
      ),
      episodic_farrington_population_vector(
        con,
        id,
        "pathogen_institution",
        weeks
      ),
      info = paste("institution", id)
    )
  }
  expect_null(activity[[as.character(ids[3])]])
})

test_that("trend row counts are read for every stream at once", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  streams <- vapply(1:3, function(k) {
    episodic_db_stream_upsert(
      con,
      stream_key = digest::digest(paste0("key-", k), algo = "sha1"),
      level = "pathogen_region",
      pathogen = "P",
      care_line = NA,
      region_code = paste0("R", k),
      institution_id = NA,
      ward = NA,
      denominator = "none",
      observed_date = "2025-01-01"
    )
  }, integer(1))
  for (w in 1:4) {
    episodic_db_stream_trend_upsert(con, streams[1], as.character(as.Date("2025-01-06") + 7 * w), 1L)
  }
  episodic_db_stream_trend_upsert(con, streams[2], "2025-01-06", 1L)
  counts <- episodic_db_stream_trend_counts(con)
  for (id in streams[1:2]) {
    expect_identical(
      unname(counts[as.character(id)]),
      nrow(episodic_db_stream_trend(con, id))
    )
  }
  expect_true(is.na(counts[as.character(streams[3])]))
})

# -- MEM -------------------------------------------------------------------

test_that("the MEM anchor, seasonality and season matrix equal their per-case definitions", {
  config <- episodic_config_resolve(NA)
  for (seed in 1:12) {
    cases <- if (seed %% 3 == 0) {
      set.seed(seed)
      data.frame(sample_date = as.character(
        as.Date("2019-06-01") + sample(0:sample(300:2400, 1), sample(1:2500, 1), replace = TRUE)
      ))
    } else {
      throughput_seasonal_cases(seed, years = 3 + seed %% 4)
    }
    expect_identical(
      episodic_mem_season_anchor(cases, config),
      throughput_ref_anchor(cases, config),
      info = paste("seed", seed)
    )
    anchor_week <- 1 + (seed * 7) %% 52
    expect_identical(
      episodic_mem_seasonal_matrix(cases, anchor_week),
      throughput_ref_seasonal_matrix(cases, anchor_week),
      info = paste("seed", seed)
    )
  }
})

test_that("the seasonality statistic is computed on the per-case year-week counts", {
  # The statistic itself is unchanged code; what is new is the matrix it
  # is computed from, which must equal the per-case tabulation.
  for (seed in 1:6) {
    cases <- throughput_seasonal_cases(seed, years = 4 + seed %% 3)
    dates <- as.Date(cases$sample_date)
    days <- episodic_mem_day_counts(cases)
    years <- episodic_mem_complete_years(days)
    expect_identical(years, throughput_ref_years(dates))
    expect_identical(
      episodic_mem_year_week_counts(days, years),
      throughput_ref_year_week(dates, years)
    )
  }
})

test_that("episodic_mem_season_weeks() agrees with episodic_mem_season_week() date by date", {
  dates <- seq(as.Date("2019-12-20"), as.Date("2021-01-15"), by = "day")
  for (anchor_week in c(1L, 2L, 27L, 40L, 52L)) {
    many <- episodic_mem_season_weeks(dates, anchor_week)
    for (i in seq_along(dates)) {
      one <- episodic_mem_season_week(dates[i], anchor_week)
      expect_identical(many$season[i], one$season)
      expect_identical(many$week_label[i], one$week_label)
      expect_identical(many$week_start[i], one$week_start)
    }
  }
})

test_that("a season matrix refuses a case whose date cannot be read, rather than dropping it", {
  expect_error(
    episodic_mem_seasonal_matrix(data.frame(sample_date = c("2024-01-01", NA)), 30L),
    "not a date"
  )
})

test_that("a cached MEM fit decodes to exactly the fresh fit", {
  skip_if_not_installed("mem")
  cases <- throughput_seasonal_cases(3)
  built <- episodic_mem_seasonal_matrix(cases, 30L)
  historical <- built$matrix[, 1:4, drop = FALSE]
  fresh <- episodic_mem_fit(historical)
  expect_false(is.null(fresh))
  expect_identical(episodic_mem_fit_decode(episodic_mem_fit_encode(fresh)), fresh)

  no_intensity <- list(pre_epidemic = NaN, post_epidemic = NA_real_, intensity = NULL)
  expect_identical(
    episodic_mem_fit_decode(episodic_mem_fit_encode(no_intensity)),
    no_intensity
  )
  expect_null(episodic_mem_fit_decode("{\"pre_epidemic\": 1}"))
  expect_null(episodic_mem_fit_decode("not json"))
})

test_that("a MEM fit is reused only for an identical input, and any changed input refits", {
  skip_if_not_installed("mem")
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  run_id <- episodic_db_run_start(con, "h", "a", run_date = "2025-01-01")
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = digest::digest("mem-cache", algo = "sha1"),
    level = "pathogen_region",
    pathogen = "P",
    care_line = NA,
    region_code = "R",
    institution_id = NA,
    ward = NA,
    denominator = "none",
    observed_date = "2025-01-01"
  )
  config <- episodic_config_resolve(NA)
  historical <- episodic_mem_seasonal_matrix(throughput_seasonal_cases(5), 30L)$matrix[, 1:4]

  fit_through_cache <- function(matrix, config, mem_mode = "auto") {
    tally <- new.env(parent = emptyenv())
    tally$reused <- 0L
    tally$fitted <- 0L
    cache <- episodic_db_detector_cache(con, "mem")
    cached <- cache[cache$stream_id == stream_id, , drop = FALSE]
    fit <- episodic_mem_fit_cached(
      con,
      stream_id = stream_id,
      run_id = run_id,
      config = config,
      mem_mode = mem_mode,
      cached = if (nrow(cached) > 0) cached[1, ] else NULL,
      tally = tally
    )(matrix)
    list(fit = fit, reused = tally$reused, fitted = tally$fitted)
  }

  first <- fit_through_cache(historical, config)
  expect_identical(first$fitted, 1L)
  expect_identical(first$fit, episodic_mem_fit(historical))

  again <- fit_through_cache(historical, config)
  expect_identical(again$reused, 1L)
  expect_identical(again$fitted, 0L)
  expect_identical(again$fit, first$fit)

  # A late case in a past season is a different matrix.
  late <- historical
  late[10, 2] <- late[10, 2] + 1L
  changed <- fit_through_cache(late, config)
  expect_identical(changed$fitted, 1L)
  expect_identical(changed$fit, episodic_mem_fit(late))

  # So is a configuration value, and a mem_mode, even with the matrix unchanged.
  fit_through_cache(historical, config)
  other_config <- config
  other_config$mem$trough_percentile <- 0.3
  expect_identical(fit_through_cache(historical, other_config)$fitted, 1L)
  expect_identical(fit_through_cache(historical, other_config, "yes")$fitted, 1L)
  expect_identical(fit_through_cache(historical, other_config, "yes")$reused, 1L)

  # However many times the input changed, the stream holds one row.
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_detector_cache")$n,
    1L
  )
})

test_that("a cached MEM fit that cannot be read back is fitted again and replaced", {
  skip_if_not_installed("mem")
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  run_id <- episodic_db_run_start(con, "h", "a", run_date = "2025-01-01")
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = digest::digest("mem-corrupt", algo = "sha1"),
    level = "pathogen_region",
    pathogen = "P",
    care_line = NA,
    region_code = "R",
    institution_id = NA,
    ward = NA,
    denominator = "none",
    observed_date = "2025-01-01"
  )
  config <- episodic_config_resolve(NA)
  historical <- episodic_mem_seasonal_matrix(throughput_seasonal_cases(8), 30L)$matrix[, 1:4]
  hash <- episodic_mem_fit_input_hash(historical, config, "auto")
  episodic_db_detector_cache_put(con, stream_id, "mem", hash, "{broken", run_id)
  cached <- episodic_db_detector_cache(con, "mem")
  expect_message(
    fitted <- episodic_mem_fit_cached(
      con, stream_id, run_id, config, "auto",
      cached = cached[1, ]
    )(historical),
    "could not be read back"
  )
  expect_identical(fitted, episodic_mem_fit(historical))
  stored <- episodic_db_detector_cache(con, "mem")
  expect_identical(episodic_mem_fit_decode(stored$result), fitted)
})

test_that("episodic_mem_status() with a cached fit equals it with a fresh fit", {
  skip_if_not_installed("mem")
  config <- episodic_config_resolve(NA)
  cases <- throughput_seasonal_cases(11, years = 6)
  anchor <- episodic_mem_season_anchor(cases, config)
  expect_false(is.null(anchor))
  run_date <- as.Date("2025-02-12")
  fresh <- episodic_mem_status(cases, run_date, config, anchor)
  stored <- NULL
  via_cache <- episodic_mem_status(
    cases,
    run_date,
    config,
    anchor,
    fit = function(historical) {
      stored <<- episodic_mem_fit_encode(episodic_mem_fit(historical))
      episodic_mem_fit_decode(stored)
    }
  )
  expect_identical(via_cache, fresh)
})

# -- Per-stream case filtering -----------------------------------------------

test_that("the stream case index hands every stream exactly what the per-stream filter does", {
  map <- tempfile(fileext = ".csv")
  on.exit(unlink(map))
  utils::write.csv(
    data.frame(
      pc = sprintf("%04d", 7000:9999),
      province_code = c("7" = "PROV_A", "8" = "PROV_B", "9" = "PROV_C")[substr(sprintf("%04d", 7000:9999), 1, 1)]
    ),
    map,
    row.names = FALSE,
    quote = FALSE
  )
  withr::local_envvar(EPISODIC_PC_PROVINCE_MAP = map)

  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 4
  )
  run_id <- episodic_db_run_start(con, "h", "a", run_date = "2024-12-31")
  suppressMessages(episodic_cases_load(
    con,
    cases,
    episodic_test_pathogen_config(),
    run_id
  ))
  cases_all <- episodic_db_cases(con)
  # Cases no stream can own must not reach one through the index either.
  cases_all$pc[seq(1, nrow(cases_all), by = 17)] <- NA
  cases_all$ward[seq(2, nrow(cases_all), by = 13)] <- NA
  config <- episodic_config_resolve(NA)
  geography <- episodic_geography_config(config)
  episodic_lattice_enumerate(con, cases_all, episodic_db_institutions(con), config)
  streams <- episodic_db_streams(con)
  expect_true(all(c(
    "pathogen_ward", "pathogen_institution", "pathogen_area",
    "pathogen_province", "pathogen_region"
  ) %in% streams$level))

  # Plus streams no case belongs to.
  extra <- streams[c(1, 1, 1), ]
  extra$pathogen[1] <- "No such pathogen"
  extra$institution_id[2] <- 999999L
  extra$level[3] <- "pathogen_area"
  extra$institution_id[3] <- NA
  extra$ward[3] <- NA
  extra$region_code[3] <- "AREA-00"
  streams <- rbind(streams, extra)

  index <- episodic_stream_case_index(cases_all, geography)
  for (i in seq_len(nrow(streams))) {
    expect_identical(
      index(streams[i, ]),
      episodic_cases_for_stream(cases_all, streams[i, ], geography),
      info = paste(streams$level[i], streams$pathogen[i], streams$region_code[i])
    )
  }
})

# -- Case loading ------------------------------------------------------------

test_that("stored episode anchors are found for more patients than one statement can name", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  run_id <- episodic_db_run_start(con, "h", "a", run_date = "2025-01-01")
  n <- 1234L
  keys <- sprintf("P%05d", seq_len(n))
  DBI::dbWriteTable(
    con,
    "episodic_case",
    data.frame(
      source_key = keys,
      lab_number = keys,
      patient_key = keys,
      sample_date = as.character(as.Date("2024-01-01") + seq_len(n) %% 300),
      pathogen = c("A", "B")[seq_len(n) %% 2 + 1],
      care_line = "second",
      first_seen_run = run_id,
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )
  found <- episodic_db_last_case_dates(con, c(keys, "nobody"), c("A", "B"))
  expect_length(found, n)
  expect_identical(
    unname(found[episodic_case_group_key(keys[1:3], c("B", "A", "B"))]),
    as.character(as.Date("2024-01-01") + 1:3 %% 300)
  )
})

# -- Stage timing --------------------------------------------------------------

test_that("the stage timer sums each stage and refuses a stage it does not know", {
  stage <- episodic_stage_timer(c("a", "b"))
  stage$time("a", x <- 1 + 1)
  expect_identical(x, 2)
  stage$start("b")
  stage$stop("b")
  expect_named(stage$seconds(), c("a", "b"))
  expect_true(all(stage$seconds() >= 0))
  expect_match(stage$line(), "^a [0-9.]+s, b [0-9.]+s, total [0-9.]+s$")
  expect_error(stage$time("c", NULL), "unknown stage 'c'")
  expect_error(stage$stop("a"), "stopped without being started")
})

# -- The whole run -------------------------------------------------------------

# The synthetic history is not seasonal enough for MEM's own test to pass
# it, so a run that has to fit MEM says `mem_mode = yes` for every
# pathogen.
throughput_mem_yes_config <- function() {
  path <- tempfile(fileext = ".csv")
  pathogen_config <- episodic_test_pathogen_config()
  pathogen_config$mem_mode <- "yes"
  utils::write.csv(pathogen_config, path, row.names = FALSE)
  path
}

throughput_table_counts <- function(con) {
  tables <- setdiff(
    sort(DBI::dbListTables(con)),
    "sqlite_sequence"
  )
  vapply(tables, function(t) {
    as.integer(DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", t))$n)
  }, integer(1))
}

test_that("a run logs its stream loop time by stage", {
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path))
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-08-31"),
    seed = 3
  )
  log <- testthat::capture_messages(episodic_run_cron(
    db_path = path,
    cases = cases,
    run_date = as.Date("2024-08-31")
  ))
  line <- grep("Stream loop time by stage", log, value = TRUE)
  expect_length(line, 1)
  for (stage in c("cases", "mem", "eligibility", "farrington", "trend", "detections", "reconciliation")) {
    expect_match(line, paste0(stage, " [0-9.]+s"))
  }
})

test_that("no table but the run log grows when the same input is run again and again", {
  skip_on_cran()
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path))
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2021-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 6
  )
  pathogen_config <- throughput_mem_yes_config()
  on.exit(unlink(pathogen_config), add = TRUE)
  run <- function() {
    suppressMessages(episodic_run_cron(
      db_path = path,
      cases = cases,
      pathogen_config_path = pathogen_config,
      run_date = as.Date("2024-12-31")
    ))
  }
  run()
  run()
  con <- episodic_db_connect(path)
  after_two <- throughput_table_counts(con)
  DBI::dbDisconnect(con)
  for (k in 1:3) run()
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  after_five <- throughput_table_counts(con)

  expect_gt(after_five[["episodic_case"]], 0L)
  expect_gt(after_five[["episodic_stream_trend"]], 0L)
  expect_gt(after_five[["episodic_detector_cache"]], 0L)
  # One row per run, and one per detector firing per run: the run log.
  run_log <- c("episodic_detection_run", "episodic_detection")
  expect_identical(
    after_five[["episodic_detection_run"]] - after_two[["episodic_detection_run"]],
    3L
  )
  grown <- setdiff(names(after_five)[after_five != after_two], run_log)
  expect_identical(grown, character(0))
  # The cache holds at most one row per stream and detector.
  expect_lte(
    after_five[["episodic_detector_cache"]],
    after_five[["episodic_stream"]]
  )
})

test_that("a run that reuses cached MEM fits writes what a run fitting afresh writes", {
  skip_on_cran()
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2021-01-01"),
    end_date = as.Date("2025-02-16"),
    seed = 2
  )
  pathogen_config <- throughput_mem_yes_config()
  base <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(base, pathogen_config)))
  suppressMessages(episodic_run_cron(
    db_path = base,
    cases = cases[as.Date(cases$sample_date) <= as.Date("2025-02-09"), ],
    pathogen_config_path = pathogen_config,
    run_date = as.Date("2025-02-09")
  ))
  with_cache <- tempfile(fileext = ".sqlite")
  without_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(with_cache, without_cache)), add = TRUE)
  file.copy(base, with_cache)
  file.copy(base, without_cache)
  con <- episodic_db_connect(without_cache)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_detector_cache")$n, 0L)
  DBI::dbExecute(con, "DELETE FROM episodic_detector_cache")
  DBI::dbDisconnect(con)

  log <- testthat::capture_messages(episodic_run_cron(
    db_path = with_cache,
    cases = cases,
    pathogen_config_path = pathogen_config,
    run_date = as.Date("2025-02-16")
  ))
  expect_match(grep("MEM fits:", log, value = TRUE), "[1-9][0-9]* reused")
  suppressMessages(episodic_run_cron(
    db_path = without_cache,
    cases = cases,
    pathogen_config_path = pathogen_config,
    run_date = as.Date("2025-02-16")
  ))

  read_back <- function(path, sql) {
    con <- episodic_db_connect(path)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, sql)
  }
  for (sql in c(
    "SELECT run_id, stream_id, cluster_id, detector, first_day, last_day, n_cases, expected, upperbound, params
       FROM episodic_detection ORDER BY detection_id",
    "SELECT cluster_id, stream_id, first_day, last_day, n_cases, expected, excess, ratio, priority_score, merged_into
       FROM episodic_cluster ORDER BY cluster_id",
    "SELECT cluster_id, case_id FROM episodic_cluster_case ORDER BY cluster_id, case_id",
    "SELECT * FROM episodic_epidemic_season ORDER BY cluster_id",
    "SELECT * FROM episodic_stream_trend ORDER BY stream_id, week_start",
    "SELECT stream_id, detector, input_hash, result FROM episodic_detector_cache ORDER BY stream_id"
  )) {
    expect_identical(read_back(with_cache, sql), read_back(without_cache, sql), info = sql)
  }
})
