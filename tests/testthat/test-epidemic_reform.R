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

test_that("closed outbreaks and closed epidemics are linked too", {
  # A first run opens and closes a whole history in one go; the "during"
  # relation is about pathogen, time and place, and holds whatever state
  # either cluster is in.
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))

  epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$region_stream_id,
    first_day = "2024-09-16",
    last_day = "2024-09-29",
    n_cases = 181,
    priority_score = 70,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  ob_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$inst_stream_id,
    first_day = "2024-09-18",
    last_day = "2024-09-25",
    n_cases = 5,
    priority_score = 40,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "outbreak"
  )
  for (id in c(epi_id, ob_id)) {
    episodic_db_cluster_state_insert(env$con, id, state = "closed", trigger = "system")
  }

  cases <- data.frame(
    sample_date = "2024-09-20",
    pathogen = "RSV",
    institution_id = env$inst_id,
    ward = NA_character_,
    pc = "9713AB",
    care_line = "second",
    stringsAsFactors = FALSE
  )
  geography <- episodic_geography_config(env$config)
  n <- episodic_epidemic_link_outbreaks(env$con, cases, geography, env$run_id)
  expect_equal(n, 1L)
  expect_equal(episodic_db_outbreaks_during_epidemic(env$con, epi_id)$cluster_id, ob_id)

  # A pair already linked is not written again.
  expect_equal(
    episodic_epidemic_link_outbreaks(env$con, cases, geography, env$run_id),
    0L
  )
})

test_that("an outbreak outside a province epidemic's province is not linked to it", {
  pc_csv <- tempfile(fileext = ".csv")
  writeLines("pc,province_code\n9713AB,DR", pc_csv)
  withr::local_envvar(EPISODIC_PC_PROVINCE_MAP = pc_csv)

  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))
  epi_id <- episodic_db_cluster_insert(
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
  episodic_db_cluster_insert(
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
  expect_equal(
    episodic_epidemic_link_outbreaks(env$con, cases, geography, env$run_id),
    0L
  )
  expect_equal(nrow(episodic_db_outbreaks_during_epidemic(env$con, epi_id)), 0)
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

# -- M3: Read path and Epidemics screen -----------------------------------

test_that("open_clusters returns only outbreaks, open_epidemics returns only epidemics", {
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

  outbreaks <- episodic_app_open_clusters(env$con, lang = "en")
  epidemics <- episodic_app_open_epidemics(env$con, lang = "en")

  expect_true(ob_id %in% outbreaks$cluster_id)
  expect_false(epi_id %in% outbreaks$cluster_id)

  expect_true(epi_id %in% epidemics$cluster_id)
  expect_false(ob_id %in% epidemics$cluster_id)
})

test_that("epidemic_object assembles dossier data for a seasonal epidemic", {
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

  obj <- episodic_epidemic_object(env$con, epi_id, lang = "en")
  expect_equal(obj$id, epi_id)
  expect_false(is.null(obj$season))
  expect_equal(obj$season$season_label, "2025/2026")
  expect_true(is.data.frame(obj$during_outbreaks))
})

test_that("epidemic_object has no season for a non-seasonal epidemic", {
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

  obj <- episodic_epidemic_object(env$con, epi_id, lang = "en")
  expect_null(obj$season)
})

test_that("epidemic UI renders without error for a seasonal epidemic", {
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

  epidemics <- episodic_app_open_epidemics(env$con, lang = "en")
  html <- as.character(episodic_ui_epidemic_rail(epidemics, epi_id, lang = "en"))
  expect_true(grepl("E-", html, fixed = TRUE))
  expect_false(grepl("\\[\\[", html))
  # The Outbreaks rail's own markup, so the two screens cannot drift
  # apart visually, and its own opener attribute, so a click on one is
  # never read as a click on the other.
  expect_true(grepl("episodic-rail-item", html, fixed = TRUE))
  expect_true(grepl("episodic-rail-item-open", html, fixed = TRUE))
  expect_true(grepl("data-episodic-epidemic", html, fixed = TRUE))
  expect_false(grepl("data-episodic-outbreak=", html, fixed = TRUE))
  # The selected row is marked for the first render; every render after
  # it is marked client-side from the same attribute.
  expect_true(grepl('aria-current="true"', html, fixed = TRUE))
})

test_that("the epidemic dossier uses the stat grid the stylesheet draws", {
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

  obj <- episodic_epidemic_object(env$con, epi_id, lang = "en")
  html <- as.character(episodic_ui_epidemic_stat_grid(obj, lang = "en"))
  expect_true(grepl('class="episodic-statgrid"', html, fixed = TRUE))
})

test_that("the epidemic dossier resolves its object before querying", {
  # The server hands the dossier `epidemic_object()` unevaluated, and
  # building that object queries `con`. Forced from inside another
  # query's parameters, those queries run while RMariaDB holds that
  # statement prepared, which closes it and kills the R process on
  # binding. SQLite tolerates the nesting, so what is asserted here is
  # the ordering itself: nothing forces the object while a DBI call is
  # on the stack.
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
  built <- episodic_epidemic_object(env$con, epi_id, lang = "en")

  forced_inside_query <- NA
  delayedAssign("lazy_obj", {
    heads <- vapply(
      sys.calls(),
      function(cl) deparse(cl[[1]])[1],
      character(1)
    )
    forced_inside_query <- any(grepl(
      "^(DBI::)?db(GetQuery|SendQuery|SendStatement|Execute|Bind)$",
      heads
    ))
    built
  })

  episodic_ui_epidemic_dossier(env$con, obj = lazy_obj, lang = "en")
  expect_false(is.na(forced_inside_query))
  expect_false(forced_inside_query)
})

test_that("an epidemic's weekly curve carries the incomplete flag", {
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

  obj <- episodic_epidemic_object(env$con, epi_id, lang = "en")
  # Without it the curve cannot say which weeks are still filling, and
  # `episodic_ui_pathogen_curve_chart()` refuses the frame outright.
  expect_true("incomplete" %in% names(obj$weekly))
  expect_type(obj$weekly$incomplete, "logical")
  expect_equal(length(obj$weekly$incomplete), nrow(obj$weekly))
})

test_that("epidemic rail shows empty state when no epidemics exist", {
  empty <- data.frame(
    cluster_id = integer(0),
    priority_score = numeric(0),
    stringsAsFactors = FALSE
  )
  html <- as.character(episodic_ui_epidemic_rail(empty, NULL, lang = "en"))
  expect_true(grepl("No open epidemics", html, fixed = TRUE))
})

test_that("epidemic during panel says 'none detected' when no outbreaks are linked", {
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

  obj <- episodic_epidemic_object(env$con, epi_id, lang = "en")
  html <- as.character(episodic_ui_epidemic_during_panel(obj, lang = "en"))
  expect_true(grepl("none detected", html, fixed = TRUE))
})

test_that("the during panel names each linked row by its own scale, not always an outbreak", {
  # episodic_db_outbreaks_during_epidemic() only ever links outbreak-scale
  # clusters today, but the panel takes its `level` column and derives the
  # prefix from it (episodic_object_ref()) rather than assuming "outbreak"
  # - both share one cluster_id sequence (see episodic_object_ref()), so a
  # row at an epidemic-scale level has to read E-{id}, not O-{id}.
  row <- function(cluster_id, level) {
    data.frame(
      cluster_id = cluster_id,
      stream_id = 1L,
      first_day = "2026-01-10",
      last_day = "2026-01-20",
      n_cases = 5L,
      case_days = 3L,
      priority_score = 50,
      pathogen = "Norovirus",
      level = level,
      level_label = "Level",
      institution_id = NA_character_,
      ward = NA_character_,
      place = "Ward B",
      state_label = "New",
      stringsAsFactors = FALSE
    )
  }

  outbreak_html <- as.character(episodic_ui_epidemic_during_panel(
    list(during_outbreaks = row(11L, "pathogen_ward")),
    lang = "en"
  ))
  expect_true(grepl("O-11", outbreak_html, fixed = TRUE))
  expect_false(grepl("E-11", outbreak_html, fixed = TRUE))

  epidemic_html <- as.character(episodic_ui_epidemic_during_panel(
    list(during_outbreaks = row(12L, "pathogen_region")),
    lang = "en"
  ))
  expect_true(grepl("E-12", epidemic_html, fixed = TRUE))
  expect_false(grepl("O-12", epidemic_html, fixed = TRUE))
})

test_that("the epidemic assessment rail offers declarations only where there is a season", {
  env <- epidemic_setup()
  on.exit(DBI::dbDisconnect(env$con))
  user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "epidemiologist",
    stringsAsFactors = FALSE
  )

  seasonal_id <- episodic_db_cluster_insert(
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
    cluster_id = seasonal_id,
    season_label = "2025/2026",
    anchor_week = 30L,
    post_epidemic_threshold = 5.0
  )
  aseasonal_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$region_stream_id,
    first_day = "2026-01-10",
    last_day = "2026-01-20",
    n_cases = 30,
    priority_score = 65,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )

  seasonal <- as.character(episodic_ui_epidemic_assessment_rail(
    env$con,
    seasonal_id,
    lang = "en",
    current_user = user
  ))
  aseasonal <- as.character(episodic_ui_epidemic_assessment_rail(
    env$con,
    aseasonal_id,
    lang = "en",
    current_user = user
  ))

  for (verdict in c("season_started", "season_not_yet", "season_ended")) {
    expect_true(grepl(verdict, seasonal, fixed = TRUE))
    # A Legionella epidemic has no season to declare started; offering
    # the act anyway would record a declaration about nothing.
    expect_false(grepl(verdict, aseasonal, fixed = TRUE))
  }
  # The ordinary classification is offered either way: an epidemic can
  # be an artefact at any scale.
  for (html in list(seasonal, aseasonal)) {
    expect_true(grepl("artefact", html, fixed = TRUE))
    expect_true(grepl("confirmed_epidemic", html, fixed = TRUE))
    expect_true(grepl(
      'window.episodicAssessForms["epidemic_assess"] = {',
      html,
      fixed = TRUE
    ))
    # Never the Outbreaks form's ids: both forms are in the page at once.
    expect_false(grepl('id="assess_verdict"', html, fixed = TRUE))
  }
})

test_that("a viewer sees the epidemic timeline and no form", {
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

  html <- as.character(episodic_ui_epidemic_assessment_rail(
    env$con,
    epi_id,
    lang = "en",
    current_user = NULL
  ))
  expect_true(grepl("episodic-timeline", html, fixed = TRUE))
  expect_false(grepl("episodicSubmitAssessment", html, fixed = TRUE))
})

test_that("epidemic verdict colours include declaration verdicts", {
  expect_type(episodic_ui_verdict_colour("season_started"), "character")
  expect_type(episodic_ui_verdict_colour("season_not_yet"), "character")
  expect_type(episodic_ui_verdict_colour("season_ended"), "character")
})

test_that("open_epidemics returns the expected columns for the rail", {
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

  epidemics <- episodic_app_open_epidemics(env$con, lang = "en")
  expect_true(all(
    c("cluster_id", "pathogen", "level_label", "state", "state_label") %in%
      names(epidemics)
  ))
})

test_that("db_outbreaks_during_epidemic returns linked outbreaks", {
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

  during <- episodic_db_outbreaks_during_epidemic(env$con, epi_id)
  expect_equal(nrow(during), 1)
  expect_true("cluster_id" %in% names(during))
  expect_true("pathogen" %in% names(during))
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

# -- The "during" relation, both directions -----------------------------

test_that("an outbreak knows the epidemics it ran during", {
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

  during <- episodic_db_epidemics_during_for_outbreak(env$con, ob_id)
  expect_equal(nrow(during), 1)
  expect_equal(during$cluster_id[1], epi_id)
  expect_equal(during$scale[1], "epidemic")

  # The lattice watches the same rise at province and region level, so
  # an outbreak is linked to both. Only the one that stands as a dossier
  # is read back: a chip pointing at a cluster suppression folded away
  # is a link to a dossier the app will not open. The link row stays,
  # because a later run can revisit the suppression.
  province_epi_id <- episodic_db_cluster_insert(
    env$con,
    stream_id = env$province_stream_id,
    first_day = "2026-01-01",
    last_day = "2026-02-15",
    n_cases = 60,
    priority_score = 65,
    detector_agreement = 1,
    run_id = env$run_id,
    scale = "epidemic"
  )
  DBI::dbExecute(
    env$con,
    "INSERT INTO episodic_cluster_link
       (outbreak_cluster_id, epidemic_cluster_id, created_at, run_id)
     VALUES (?, ?, '2026-02-16T00:00:00Z', ?)",
    params = list(ob_id, province_epi_id, env$run_id)
  )
  expect_equal(
    nrow(episodic_db_epidemics_during_for_outbreak(env$con, ob_id)),
    2
  )
  DBI::dbExecute(
    env$con,
    "UPDATE episodic_cluster SET suppressed_by = ? WHERE cluster_id = ?",
    params = list(epi_id, province_epi_id)
  )
  still <- episodic_db_epidemics_during_for_outbreak(env$con, ob_id)
  expect_equal(nrow(still), 1)
  expect_equal(still$cluster_id[1], epi_id)
  expect_equal(nrow(episodic_db_cluster_links(env$con, ob_id)), 2)

  during <- episodic_db_epidemics_during_for_outbreak(env$con, ob_id)

  # And the chip that carries it, which opens the Epidemics screen
  # rather than the Outbreaks one.
  html <- as.character(episodic_ui_during_chips(during, lang = "en"))
  expect_true(grepl(
    episodic_tr(
      "dossier.during_badge",
      ref = episodic_tr("dossier.epidemic_ref", id = epi_id, lang = "en"),
      lang = "en"
    ),
    html,
    fixed = TRUE
  ))
  expect_true(grepl("data-episodic-epidemic", html, fixed = TRUE))
  expect_false(grepl("data-episodic-outbreak", html, fixed = TRUE))
})

test_that("during chips are empty when the outbreak ran during nothing", {
  expect_null(episodic_ui_during_chips(NULL, lang = "en"))
  expect_null(episodic_ui_during_chips(
    data.frame(cluster_id = integer(0), level = character(0)),
    lang = "en"
  ))
})

test_that("the case-sharing relation stops at the scale boundary", {
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
  DBI::dbExecute(
    env$con,
    "INSERT INTO episodic_case (source_key, lab_number, patient_key,
       sample_date, pathogen, care_line, institution_id, first_seen_run)
     VALUES ('C-1', 'C-1', 'C-1', '2026-01-15', 'RSV', 'second', ?, ?)",
    params = list(env$inst_id, env$run_id)
  )
  case_id <- DBI::dbGetQuery(env$con, "SELECT last_insert_rowid() AS id")$id[1]
  for (cluster_id in c(epi_id, ob_id)) {
    DBI::dbExecute(
      env$con,
      "INSERT INTO episodic_cluster_case (cluster_id, case_id) VALUES (?, ?)",
      params = list(cluster_id, case_id)
    )
  }

  # Every case in the catchment is in the regional epidemic by
  # construction, so "shares cases with" relates every outbreak to it
  # and says nothing. The during link is what carries that relation.
  linked <- episodic_db_clusters_linked_to(env$con, ob_id)
  expect_equal(nrow(linked), 0)
})
