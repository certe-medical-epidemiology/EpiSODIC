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

# Tests for the two-scale epidemic reform: scale routing, satellite
# writes, epidemic closure, continuity, during-links, and cross-scale
# suppression.

# -- Synthetic case data (shared with test-detect_mem.R) ---------------

epidemic_synthetic_seasons <- function(n_seasons = 5,
                                       seed = 42,
                                       peak_month = 1) {
  set.seed(seed)
  dates <- c()
  for (yr in seq_len(n_seasons) + 2018) {
    all_days <- seq(
      as.Date(sprintf("%d-01-01", yr)),
      as.Date(sprintf("%d-12-31", yr)),
      by = "day"
    )
    months <- as.integer(format(all_days, "%m"))
    lambda <- 2 + 15 * exp(-((months - peak_month)^2) / 4)
    n <- stats::rpois(length(all_days), lambda / 10)
    dates <- c(dates, rep(all_days, n))
  }
  data.frame(sample_date = as.character(as.Date(dates, origin = "1970-01-01")))
}

# -- Fixtures ----------------------------------------------------------

epidemic_setup <- function() {
  con <- episodic_test_db()
  config <- episodic_config_resolve(NA)
  run_id <- episodic_db_run_start(con, "h", "a")

  stream <- function(level, region_code = NA, institution_id = NA, ward = NA) {
    episodic_db_stream_upsert(
      con,
      stream_key = episodic_stream_key(
        level,
        "RSV",
        region_code = region_code,
        institution_id = institution_id,
        ward = ward
      ),
      level = level,
      pathogen = "RSV",
      region_code = region_code,
      institution_id = institution_id,
      ward = ward,
      observed_date = "2026-01-15"
    )
  }

  province_stream_id <- stream("pathogen_province", region_code = "GR")
  region_stream_id <- stream("pathogen_region", region_code = "NORTH")

  key <- digest::digest("hosp", algo = "sha1", serialize = FALSE)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_institution
      (institution_key, display_name, institution_type, care_line, is_monitored, is_active)
     VALUES (?, 'Hospital A', 'hospital', 'second', 1, 1)",
    params = list(key)
  )
  inst_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]

  ward_stream_id <- stream("pathogen_ward", institution_id = inst_id, ward = "ICU")
  inst_stream_id <- stream("pathogen_institution", institution_id = inst_id)
  area_stream_id <- stream("pathogen_area", region_code = "GEBIED-97")

  list(
    con = con,
    config = config,
    run_id = run_id,
    inst_id = inst_id,
    ward_stream_id = ward_stream_id,
    inst_stream_id = inst_stream_id,
    area_stream_id = area_stream_id,
    province_stream_id = province_stream_id,
    region_stream_id = region_stream_id
  )
}

# -- Scale routing -----------------------------------------------------

test_that("scale is 'epidemic' for L4/L5, 'outbreak' for L1-L3", {
  config <- episodic_config_resolve(NA)
  expect_equal(episodic_scale_for_level("pathogen_ward", config), "outbreak")
  expect_equal(episodic_scale_for_level("pathogen_institution", config), "outbreak")
  expect_equal(episodic_scale_for_level("pathogen_area", config), "outbreak")
  expect_equal(episodic_scale_for_level("pathogen_province", config), "epidemic")
  expect_equal(episodic_scale_for_level("pathogen_region", config), "epidemic")
})

test_that("cluster insert stores the scale column", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  ob_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$area_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 5,
    priority_score = 50,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "outbreak"
  )
  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$province_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 20,
    priority_score = 60,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )

  clusters <- DBI::dbGetQuery(
    env$con,
    "SELECT cluster_id, scale FROM episodic_cluster ORDER BY cluster_id"
  )
  expect_equal(clusters$scale[clusters$cluster_id == ob_id], "outbreak")
  expect_equal(clusters$scale[clusters$cluster_id == epi_id], "epidemic")
})

# -- Satellite write ---------------------------------------------------

test_that("epidemic season satellite is written and read back", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$province_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 20,
    priority_score = 60,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  episodic_db_epidemic_season_insert(
    env$con,
    cluster_id = epi_id,
    season_label = "2025/2026",
    anchor_week = 30L,
    post_epidemic_threshold = 5.0,
    intensity_medium = 10.0,
    intensity_high = 20.0,
    intensity_very_high = 30.0
  )

  sat <- episodic_db_epidemic_season(env$con, epi_id)
  expect_false(is.null(sat))
  expect_equal(sat$season_label, "2025/2026")
  expect_equal(sat$anchor_week, 30L)
  expect_equal(sat$post_epidemic_threshold, 5.0)
  expect_true(is.na(sat$ended_week_start))
})

# -- Epidemic closure --------------------------------------------------

test_that("seasonal epidemic closes on post_epidemic_threshold", {
  config <- episodic_config_resolve(NA)
  cases <- epidemic_synthetic_seasons(n_seasons = 5, peak_month = 1)
  anchor <- episodic_mem_season_anchor(cases, config)
  skip_if(is.null(anchor), "synthetic data produced no anchor")

  satellite <- data.frame(
    anchor_week = anchor$anchor_week,
    post_epidemic_threshold = 999
  )
  result <- episodic_epidemic_closure(
    cases,
    run_date = as.Date("2024-06-15"),
    config = config,
    satellite = satellite
  )
  expect_false(is.null(result))
  expect_equal(result$ended_reason, "post_epidemic_threshold")
  expect_true(!is.na(result$ended_week_start))
})

test_that("seasonal epidemic closes on trough backstop", {
  config <- episodic_config_resolve(NA)
  cases <- epidemic_synthetic_seasons(n_seasons = 5, peak_month = 1)
  anchor <- episodic_mem_season_anchor(cases, config)
  skip_if(is.null(anchor), "synthetic data produced no anchor")

  trough_week <- anchor$trough_run[1]
  trough_start <- episodic_iso_week_start(2024L, trough_week)
  run_date <- trough_start + 8

  satellite <- data.frame(
    anchor_week = anchor$anchor_week,
    post_epidemic_threshold = NA_real_
  )
  result <- episodic_epidemic_closure(
    cases,
    run_date = run_date,
    config = config,
    satellite = satellite
  )
  expect_false(is.null(result))
  expect_equal(result$ended_reason, "trough")
})

test_that("seasonal epidemic stays open when neither criterion is met", {
  config <- episodic_config_resolve(NA)
  cases <- epidemic_synthetic_seasons(n_seasons = 5, peak_month = 1)
  anchor <- episodic_mem_season_anchor(cases, config)
  skip_if(is.null(anchor), "synthetic data produced no anchor")

  non_trough <- setdiff(1:52, anchor$trough_run)
  skip_if(length(non_trough) == 0, "no non-trough weeks")

  # Pick a non-trough week in 2022 (within the synthetic data range) that
  # has cases, so the count stays above the threshold.
  found_week <- NULL
  for (w in non_trough) {
    ws <- episodic_iso_week_start(2022L, w)
    we <- ws + 6
    dates <- as.Date(cases$sample_date)
    n <- sum(dates >= ws & dates <= we, na.rm = TRUE)
    if (n > 0) {
      found_week <- w
      break
    }
  }
  skip_if(is.null(found_week), "no non-trough week with cases found")

  target_start <- episodic_iso_week_start(2022L, found_week)
  run_date <- target_start + 8

  satellite <- data.frame(
    anchor_week = anchor$anchor_week,
    post_epidemic_threshold = 0
  )
  result <- episodic_epidemic_closure(
    cases,
    run_date = run_date,
    config = config,
    satellite = satellite
  )
  expect_null(result)
})

test_that("epidemic closure updates the satellite and cluster state", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$province_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 20,
    priority_score = 60,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  episodic_db_epidemic_season_insert(
    env$con,
    cluster_id = epi_id,
    season_label = "2025/2026",
    anchor_week = 30L,
    post_epidemic_threshold = 5.0
  )
  episodic_db_epidemic_season_update_ended(
    env$con,
    cluster_id = epi_id,
    ended_week_start = "2026-03-02",
    ended_reason = "post_epidemic_threshold"
  )
  episodic_db_cluster_state_insert(
    env$con,
    cluster_id = epi_id,
    state = "closed",
    trigger = "system"
  )

  sat <- episodic_db_epidemic_season(env$con, epi_id)
  expect_equal(sat$ended_week_start, "2026-03-02")
  expect_equal(sat$ended_reason, "post_epidemic_threshold")

  open_seasonal <- episodic_db_open_seasonal_epidemics(env$con)
  expect_equal(nrow(open_seasonal), 0)
})

# -- Continuity across the season cut ----------------------------------

test_that("reconciliation matches by date overlap, not season label", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$province_stream_id,
    first_day = "2025-09-01",
    last_day = "2026-01-15",
    n_cases = 50,
    priority_score = 70,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  episodic_db_epidemic_season_insert(
    env$con,
    cluster_id = epi_id,
    season_label = "2025/2026",
    anchor_week = 30L,
    post_epidemic_threshold = 5.0
  )

  open_clusters <- episodic_db_clusters_for_stream(
    env$con,
    env$province_stream_id
  )
  candidate <- data.frame(
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 10,
    expected = NA_real_,
    upperbound = NA_real_,
    detector_agreement = 1L,
    .detection_ids = I(list(integer(0)))
  )
  matches <- episodic_reconcile_find_matches(
    open_clusters,
    candidate,
    case_free_days = 14
  )
  expect_true(length(matches) > 0)
  expect_equal(open_clusters$cluster_id[matches[1]], epi_id)
})

# -- "During" links ----------------------------------------------------

test_that("outbreak links to epidemic with same pathogen, time overlap, geographic nesting", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$region_stream_id,
    first_day = "2026-01-01",
    last_day = "2026-02-15",
    n_cases = 100,
    priority_score = 70,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  ob_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$inst_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 5,
    priority_score = 40,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "outbreak"
  )

  cases <- data.frame(
    sample_date = "2026-01-15",
    pathogen = "RSV",
    institution_id = env$inst_id,
    ward = NA_character_,
    pc = "9713AB",
    care_line = "second",
    stringsAsFactors = FALSE
  )
  geography <- episodic_geography_config(env$config)

  n <- episodic_epidemic_link_outbreaks(
    env$con,
    cases,
    geography,
    env$run_id
  )
  expect_equal(n, 1L)

  links <- episodic_db_cluster_links(env$con, ob_id)
  expect_equal(nrow(links), 1)
  expect_equal(links$outbreak_cluster_id, ob_id)
  expect_equal(links$epidemic_cluster_id, epi_id)
})

test_that("an outbreak can be during two epidemics simultaneously", {
  pc_csv <- tempfile(fileext = ".csv")
  writeLines("pc,province_code\n9713AB,GR", pc_csv)
  withr::local_envvar(EPISODIC_PC_PROVINCE_MAP = pc_csv)

  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_province <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$province_stream_id,
    first_day = "2026-01-01",
    last_day = "2026-02-15",
    n_cases = 50,
    priority_score = 60,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  epi_region <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$region_stream_id,
    first_day = "2026-01-01",
    last_day = "2026-02-15",
    n_cases = 100,
    priority_score = 70,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  ob_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$inst_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 5,
    priority_score = 40,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "outbreak"
  )

  cases <- data.frame(
    sample_date = "2026-01-15",
    pathogen = "RSV",
    institution_id = env$inst_id,
    ward = NA_character_,
    pc = "9713AB",
    care_line = "second",
    stringsAsFactors = FALSE
  )
  geography <- episodic_geography_config(env$config)

  n <- episodic_epidemic_link_outbreaks(
    env$con,
    cases,
    geography,
    env$run_id
  )
  expect_equal(n, 2L)

  links <- episodic_db_cluster_links(env$con, ob_id)
  expect_equal(nrow(links), 2)
  expect_true(all(c(epi_province, epi_region) %in% links$epidemic_cluster_id))
})

test_that("links survive a second run (idempotent)", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$region_stream_id,
    first_day = "2026-01-01",
    last_day = "2026-02-15",
    n_cases = 100,
    priority_score = 70,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  ob_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$inst_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 5,
    priority_score = 40,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "outbreak"
  )

  cases <- data.frame(
    sample_date = "2026-01-15",
    pathogen = "RSV",
    institution_id = env$inst_id,
    ward = NA_character_,
    pc = "9713AB",
    care_line = "second",
    stringsAsFactors = FALSE
  )
  geography <- episodic_geography_config(env$config)

  episodic_epidemic_link_outbreaks(env$con, cases, geography, env$run_id)
  run_id_2 <- episodic_db_run_start(env$con, "h", "a")
  episodic_epidemic_link_outbreaks(env$con, cases, geography, run_id_2)

  links <- episodic_db_cluster_links(env$con, ob_id)
  expect_equal(nrow(links), 1)
})

# -- Geographic nesting ------------------------------------------------

test_that("geography_nests returns TRUE for L5 epidemic unconditionally", {
  outbreak <- data.frame(
    cluster_id = 1L,
    pathogen = "RSV",
    level = "pathogen_ward",
    institution_id = 1L,
    ward = "ICU",
    region_code = NA_character_,
    stringsAsFactors = FALSE
  )
  epidemic <- data.frame(
    cluster_id = 2L,
    pathogen = "RSV",
    level = "pathogen_region",
    region_code = "NORTH",
    institution_id = NA_integer_,
    stringsAsFactors = FALSE
  )
  cases <- data.frame(sample_date = character(0), pathogen = character(0))
  geography <- episodic_geography_config(episodic_config_resolve(NA))

  expect_true(episodic_geography_nests(outbreak, epidemic, cases, geography))
})

# -- Cross-scale suppression -------------------------------------------

test_that("suppression fires across the outbreak/epidemic scale boundary", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  config <- episodic_config_resolve(NA)
  run_id <- episodic_db_run_start(con, "h", "a")

  area_stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_area",
      "RSV",
      region_code = "GEBIED-97"
    ),
    level = "pathogen_area",
    pathogen = "RSV",
    region_code = "GEBIED-97",
    observed_date = "2026-01-15"
  )
  province_stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_province",
      "RSV",
      region_code = "GR"
    ),
    level = "pathogen_province",
    pathogen = "RSV",
    region_code = "GR",
    observed_date = "2026-01-15"
  )

  province_cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = province_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 10,
    priority_score = 60,
    detector_agreement = 1,
    run_id = run_id,
    scale = "epidemic"
  )
  area_cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = area_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 10,
    priority_score = 50,
    detector_agreement = 1,
    run_id = run_id,
    scale = "outbreak"
  )

  case_ids <- vapply(1:10, function(i) {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_case
        (source_key, lab_number, patient_key, sample_date, pathogen, care_line, first_seen_run)
       VALUES (?, ?, ?, '2026-01-15', 'RSV', 'second', ?)",
      params = list(
        sprintf("C%d", i),
        sprintf("C%d", i),
        sprintf("P%d", i),
        run_id
      )
    )
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  }, numeric(1))

  episodic_db_cluster_case_link_many(con, province_cluster_id, case_ids)
  episodic_db_cluster_case_link_many(con, area_cluster_id, case_ids)

  episodic_suppress_lattice(con, config)

  clusters <- DBI::dbGetQuery(
    con,
    "SELECT cluster_id, scale, suppressed_by FROM episodic_cluster"
  )
  province_row <- clusters[clusters$cluster_id == province_cluster_id, ]
  area_row <- clusters[clusters$cluster_id == area_cluster_id, ]
  suppressed <- !is.na(province_row$suppressed_by) || !is.na(area_row$suppressed_by)
  expect_true(suppressed)
})
