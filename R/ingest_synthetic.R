#' Synthetic ingestion source
#'
#' The default (and, in this environment, only) implementation of the
#' ingestion interface (`R/ingest_interface.R`). Generates several years of
#' seasonal baseline case data across a synthetic set of institutions,
#' pathogens, PC4 areas and care lines, then injects two outbreaks of known
#' shape so the detectors have something to visibly fire on: one point
#' source (`add_outbreak_point_source`, a ward-level cluster tightly bunched
#' in time) and one propagated (`add_outbreak_propagated`, a community
#' cluster with generation-interval-spaced case waves). This is what
#' `MILESTONES.md` M1 step 5 asks for.
#'
#' No Diver column name is invented here; every field in the returned data
#' frame is entirely synthetic and matches `episode_ingest_columns`.
#'
#' @param start_date First sample date to generate, a `Date`.
#' @param end_date Last sample date to generate, a `Date`.
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame satisfying `episode_ingest_validate_source()`.
#' @export
episode_ingest_source_synthetic <- function(start_date = as.Date("2021-01-01"),
                                             end_date = as.Date("2025-12-31"),
                                             seed = 1) {
  set.seed(seed)

  institutions <- episode_synthetic_institutions()
  pc4_pool <- episode_synthetic_pc4_pool()
  organisms <- episode_synthetic_organism_profiles()

  dates <- seq(start_date, end_date, by = "day")

  baseline <- episode_synthetic_baseline_cases(dates, institutions, pc4_pool, organisms)

  point_source <- episode_synthetic_outbreak_point_source(institutions, end_date)
  propagated <- episode_synthetic_outbreak_propagated(pc4_pool, end_date)

  cases <- rbind(baseline, point_source, propagated)
  cases$source_key <- sprintf("SYN-%08d", seq_len(nrow(cases)))

  cases <- cases[order(cases$sample_date), ]
  rownames(cases) <- NULL

  episode_ingest_validate_source(cases)
}

#' @keywords internal
#' @noRd
episode_synthetic_institutions <- function() {
  hospitals <- data.frame(
    institution_key = sprintf("HOSP-%02d", 1:8),
    institution_display_name = paste("Ziekenhuis", LETTERS[1:8]),
    institution_type = "hospital",
    care_line = "second",
    municipality = NA_character_,
    n_beds = c(850, 620, 410, 380, 300, 260, 240, 200),
    is_monitored = TRUE,
    stringsAsFactors = FALSE
  )
  ltc <- data.frame(
    institution_key = sprintf("LTC-%02d", 1:12),
    institution_display_name = paste("Zorgcentrum", 1:12),
    institution_type = "ltc_institution",
    care_line = "second",
    municipality = NA_character_,
    n_beds = sample(40:180, 12),
    is_monitored = FALSE,
    stringsAsFactors = FALSE
  )
  gp <- data.frame(
    institution_key = sprintf("GP-%02d", seq_along(episode_synthetic_municipalities())),
    institution_display_name = episode_synthetic_municipalities(),
    institution_type = "gp_municipality",
    care_line = "first",
    municipality = episode_synthetic_municipalities(),
    n_beds = NA_integer_,
    is_monitored = FALSE,
    stringsAsFactors = FALSE
  )
  rbind(hospitals, ltc, gp)
}

#' @keywords internal
#' @noRd
episode_synthetic_municipalities <- function() {
  c("Groningen", "Leeuwarden", "Assen", "Emmen", "Hoogezand-Sappemeer",
    "Winschoten", "Delfzijl", "Drachten", "Heerenveen", "Meppel")
}

#' @keywords internal
#' @noRd
episode_synthetic_pc4_pool <- function() {
  c(paste0("9", sprintf("%03d", sample(0:999, 40))),   # Groningen province
    paste0("8", sprintf("%03d", sample(0:999, 40))),   # Fryslan
    paste0("7", sprintf("%03d", sample(100:999, 30)))) # Drenthe
}

#' @keywords internal
#' @noRd
episode_synthetic_organism_profiles <- function() {
  # amplitude/phase describe a sinusoidal seasonal baseline; phase_day is the
  # day-of-year of peak incidence. mean_daily is the region-wide baseline
  # mean before seasonality is applied.
  data.frame(
    mo_code = c("V_NOROVIRUS", "V_INFLUENZA_A", "B_CAMPYLOBACTER", "B_SALMONELLA",
                "V_RSV", "B_CLOSTRIDIOIDES_DIFFICILE", "B_MRSA", "P_GIARDIA"),
    mean_daily = c(1.2, 1.0, 0.9, 0.4, 0.7, 0.5, 0.15, 0.2),
    amplitude = c(0.7, 0.9, 0.4, 0.3, 0.9, 0.1, 0.05, 0.1),
    phase_day = c(15, 15, 200, 210, 350, 180, 180, 200),  # ~mid-Jan, mid-Jul etc.
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
episode_synthetic_baseline_cases <- function(dates, institutions, pc4_pool, organisms) {
  rows <- list()
  for (i in seq_len(nrow(organisms))) {
    org <- organisms[i, ]
    doy <- as.integer(format(dates, "%j"))
    seasonal_mean <- org$mean_daily * (1 + org$amplitude * cos(2 * pi * (doy - org$phase_day) / 365.25))
    n_per_day <- stats::rpois(length(dates), lambda = pmax(seasonal_mean, 0.01))
    n_total <- sum(n_per_day)
    if (n_total == 0) next
    case_dates <- rep(dates, times = n_per_day)
    inst_idx <- sample(seq_len(nrow(institutions)), n_total, replace = TRUE)
    inst <- institutions[inst_idx, ]
    rows[[i]] <- data.frame(
      patient_key = sprintf("PT-%s-%06d", org$mo_code, sample.int(1e7, n_total)),
      sample_date = as.character(case_dates),
      receipt_date = as.character(case_dates + sample(0:3, n_total, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.05))),
      mo_code = org$mo_code,
      determination = paste0("DET-", substr(org$mo_code, 1, 3)),
      material = sample(c("faeces", "urine", "sputum", "wound", "blood"), n_total, replace = TRUE),
      care_line = inst$care_line,
      institution_key = inst$institution_key,
      institution_display_name = inst$institution_display_name,
      institution_type = inst$institution_type,
      municipality = inst$municipality,
      ward = ifelse(inst$institution_type == "hospital",
                     sample(c("Interne", "Chirurgie", "Longziekten", "Geriatrie", "IC"), n_total, replace = TRUE),
                     NA_character_),
      specialism = ifelse(inst$institution_type == "hospital",
                           sample(c("Interne geneeskunde", "Chirurgie", "Longziekten", "Klinische geriatrie"), n_total, replace = TRUE),
                           NA_character_),
      pc4 = sample(pc4_pool, n_total, replace = TRUE),
      sex = sample(c("M", "F"), n_total, replace = TRUE),
      age = pmin(pmax(round(stats::rnorm(n_total, mean = 45, sd = 25)), 0), 100),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Inject a point-source outbreak: one ward, tightly bunched in time
#' @keywords internal
#' @noRd
episode_synthetic_outbreak_point_source <- function(institutions, end_date, n_cases = 14) {
  hospital <- institutions[institutions$institution_type == "hospital", ][1, ]
  exposure_date <- end_date - 40
  # norovirus incubation is 0.5-3 days; all cases cluster within a few days
  onset_offsets <- round(stats::rgamma(n_cases, shape = 4, rate = 2.5))
  case_dates <- exposure_date + onset_offsets

  data.frame(
    patient_key = sprintf("PT-OUTBREAK-PS-%03d", seq_len(n_cases)),
    sample_date = as.character(case_dates),
    receipt_date = as.character(case_dates + 1),
    mo_code = "V_NOROVIRUS",
    determination = "DET-V_N",
    material = "faeces",
    care_line = "second",
    institution_key = hospital$institution_key,
    institution_display_name = hospital$institution_display_name,
    institution_type = "hospital",
    municipality = NA_character_,
    ward = "Geriatrie",
    specialism = "Klinische geriatrie",
    pc4 = sample(episode_synthetic_pc4_pool(), n_cases, replace = TRUE),
    sex = sample(c("M", "F"), n_cases, replace = TRUE),
    age = pmin(pmax(round(stats::rnorm(n_cases, mean = 78, sd = 8)), 60), 100),
    stringsAsFactors = FALSE
  )
}

#' Inject a propagated outbreak: community spread with generation-interval waves
#' @keywords internal
#' @noRd
episode_synthetic_outbreak_propagated <- function(pc4_pool, end_date, n_generations = 4,
                                                    cases_per_generation = c(2, 4, 6, 3)) {
  epi_pc4 <- sample(pc4_pool, 1)
  start_date <- end_date - 90
  serial_interval <- 20  # B_BORDETELLA_PERTUSSIS-like, days

  rows <- list()
  gen_start <- start_date
  for (g in seq_len(n_generations)) {
    n <- cases_per_generation[g]
    case_dates <- gen_start + round(stats::rgamma(n, shape = 2, rate = 2 / serial_interval))
    rows[[g]] <- data.frame(
      patient_key = sprintf("PT-OUTBREAK-PROP-G%d-%03d", g, seq_len(n)),
      sample_date = as.character(case_dates),
      receipt_date = as.character(case_dates + 1),
      mo_code = "B_BORDETELLA_PERTUSSIS",
      determination = "DET-B_B",
      material = "sputum",
      care_line = "first",
      institution_key = "GP-01",
      institution_display_name = "Groningen",
      institution_type = "gp_municipality",
      municipality = "Groningen",
      ward = NA_character_,
      specialism = NA_character_,
      pc4 = epi_pc4,
      sex = sample(c("M", "F"), n, replace = TRUE),
      age = pmin(pmax(round(stats::rnorm(n, mean = 8, sd = 4)), 0), 18),
      stringsAsFactors = FALSE
    )
    gen_start <- gen_start + serial_interval
  }
  do.call(rbind, rows)
}
