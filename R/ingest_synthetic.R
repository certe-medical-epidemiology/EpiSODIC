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

#' Synthetic ingestion source
#'
#' The default (and only) implementation of the ingestion interface
#' shipped with this package (`R/ingest_interface.R`). Generates several
#' years of seasonal baseline case data across a synthetic set of institutions,
#' pathogens, PC areas and care lines, then injects two outbreaks of known
#' shape so the detectors have something to visibly fire on: one point
#' source (`add_outbreak_point_source`, a ward-level cluster tightly bunched
#' in time) and one propagated (`add_outbreak_propagated`, a community
#' cluster with generation-interval-spaced case waves).
#'
#' No Diver column name is invented here; every field in the returned data
#' frame is entirely synthetic and matches `episode_ingest_columns`.
#'
#' @param start_date First sample date to generate, a `Date`.
#' @param end_date Last sample date to generate, a `Date`.
#' @param seed RNG seed, for reproducible demo data.
#' @return A data frame satisfying `episode_ingest_validate_source()`.
#' @examples
#' raw <- episode_ingest_source_synthetic(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' nrow(raw)
#' head(raw)
#' @export
episode_ingest_source_synthetic <- function(start_date = as.Date("2021-01-01"),
                                             end_date = as.Date("2025-12-31"),
                                             seed = 1) {
  set.seed(seed)

  institutions <- episode_synthetic_institutions()
  pc_pool <- episode_synthetic_pc_pool()
  organisms <- episode_synthetic_organism_profiles()

  dates <- seq(start_date, end_date, by = "day")

  baseline <- episode_synthetic_baseline_cases(dates, institutions, pc_pool, organisms)

  point_source <- episode_synthetic_outbreak_point_source(institutions, end_date)
  propagated <- episode_synthetic_outbreak_propagated(pc_pool, end_date)

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
episode_synthetic_pc_pool <- function() {
  c(paste0("9", sprintf("%03d", sample(0:999, 40))),   # Groningen province
    paste0("8", sprintf("%03d", sample(0:999, 40))),   # Fryslan
    paste0("7", sprintf("%03d", sample(100:999, 30)))) # Drenthe
}

#' @keywords internal
#' @noRd
episode_synthetic_organism_profiles <- function() {
  # amplitude/phase describe a sinusoidal seasonal baseline; phase_day is the
  # day-of-year of peak incidence. mean_daily is the region-wide baseline
  # mean before seasonality is applied. pathogen values match
  # inst/config/pathogen_config.csv exactly, as raw lab-provided strings.
  data.frame(
    pathogen = c("Norovirus", "Influenza A", "Campylobacter", "Salmonella",
                 "RSV", "Clostridioides difficile", "MRSA", "Giardia lamblia"),
    mean_daily = c(1.2, 1.0, 0.9, 0.4, 0.7, 0.5, 0.15, 0.2),
    amplitude = c(0.7, 0.9, 0.4, 0.3, 0.9, 0.1, 0.05, 0.1),
    phase_day = c(15, 15, 200, 210, 350, 180, 180, 200),  # ~mid-Jan, mid-Jul etc.
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
episode_synthetic_baseline_cases <- function(dates, institutions, pc_pool, organisms) {
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
      patient_key = sprintf("PT-%s-%06d", make.names(org$pathogen), sample.int(1e7, n_total)),
      sample_date = as.character(case_dates),
      receipt_date = as.character(case_dates + sample(0:3, n_total, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.05))),
      pathogen = org$pathogen,
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
      pc = sample(pc_pool, n_total, replace = TRUE),
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
    pathogen = "Norovirus",
    care_line = "second",
    institution_key = hospital$institution_key,
    institution_display_name = hospital$institution_display_name,
    institution_type = "hospital",
    municipality = NA_character_,
    ward = "Geriatrie",
    specialism = "Klinische geriatrie",
    pc = sample(episode_synthetic_pc_pool(), n_cases, replace = TRUE),
    sex = sample(c("M", "F"), n_cases, replace = TRUE),
    age = pmin(pmax(round(stats::rnorm(n_cases, mean = 78, sd = 8)), 60), 100),
    stringsAsFactors = FALSE
  )
}

#' Synthetic data with realistic signal volume for one pathogen, for calibration testing
#'
#' `episode_ingest_source_synthetic()` injects exactly two outbreaks
#' total across a multi-year window - enough to prove the detectors and
#' reconciliation work, nowhere near enough to tune anything against.
#' The eligibility gate's own calibration target is concrete - roughly
#' ten assessed clusters a month, system-wide (`episode_eligibility_gate()`);
#' there is no way to tune
#' towards a target using two data points. This is not a substitute for
#' real signal volume - it is still fabricated data, and no amount of
#' realism turns it into "real data" - but it is an honest stand-in: many
#' independent `same_place`-shaped case bumps for one named pathogen,
#' spread across LTC institutions and hospitals over the whole window,
#' at a volume an operator can actually run the Prestatie screen and the
#' eligibility gate against while deciding how to tune them for a real
#' instance. Every cluster this produces is clearly synthetic in origin
#' (`PT-VOL-*` patient keys) so it is never mistaken for anything else.
#'
#' @param start_date,end_date The window to generate over.
#' @param pathogen Which organism to generate elevated volume for.
#'   Defaults to *Clostridioides difficile*, a worked example of an
#'   endemic organism that produces frequent `same_place` clusters at a
#'   busy institution.
#' @param n_bumps_per_month Average number of independent case bumps
#'   generated per calendar month (Poisson-distributed), each becoming
#'   its own candidate cluster once reconciled. Tune this up or down to
#'   see how detection volume responds - that response is the entire
#'   point of this function.
#' @param seed RNG seed, for reproducible runs.
#' @return A data frame satisfying `episode_ingest_validate_source()`,
#'   including everything `episode_ingest_source_synthetic()` produces
#'   (background baseline, the two standard demo outbreaks) plus the
#'   extra volume.
#' @examples
#' raw <- episode_ingest_source_synthetic_calibration(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-06-30"),
#'   n_bumps_per_month = 4
#' )
#' sum(startsWith(raw$patient_key, "PT-VOL-"))
#' @export
episode_ingest_source_synthetic_calibration <- function(start_date = as.Date("2021-01-01"),
                                                          end_date = as.Date("2025-12-31"),
                                                          pathogen = "Clostridioides difficile",
                                                          n_bumps_per_month = 3,
                                                          seed = 1) {
  set.seed(seed)

  institutions <- episode_synthetic_institutions()
  pc_pool <- episode_synthetic_pc_pool()
  organisms <- episode_synthetic_organism_profiles()
  dates <- seq(start_date, end_date, by = "day")

  baseline <- episode_synthetic_baseline_cases(dates, institutions, pc_pool, organisms)
  point_source <- episode_synthetic_outbreak_point_source(institutions, end_date)
  propagated <- episode_synthetic_outbreak_propagated(pc_pool, end_date)
  volume <- episode_synthetic_outbreak_volume(institutions, start_date, end_date,
                                               pathogen = pathogen, n_bumps_per_month = n_bumps_per_month)

  cases <- rbind(baseline, point_source, propagated, volume)
  cases$source_key <- sprintf("SYN-%08d", seq_len(nrow(cases)))

  cases <- cases[order(cases$sample_date), ]
  rownames(cases) <- NULL

  episode_ingest_validate_source(cases)
}

#' Inject many independent same-place case bumps for one pathogen
#'
#' The volume generator behind [episode_ingest_source_synthetic_calibration()].
#' Each bump is shaped like the `same_place` detector's own trigger
#' condition (several cases at one institution within a short window) so
#' it reliably becomes its own cluster on reconciliation, rather than
#' relying on Farrington to notice an elevated baseline.
#'
#' @param institutions From `episode_synthetic_institutions()`.
#' @param start_date,end_date The window to spread bumps across.
#' @param pathogen The organism name to stamp every generated case with.
#' @param n_bumps_per_month Average bumps per calendar month
#'   (`stats::rpois()`).
#' @param cases_per_bump A `c(min, max)` range; the actual count per bump
#'   is drawn uniformly from it.
#' @return A data frame in the same shape as this file's other
#'   generators, or `NULL` if no bumps were generated at all (possible,
#'   not likely, with a very short window or low `n_bumps_per_month`).
#' @keywords internal
#' @noRd
episode_synthetic_outbreak_volume <- function(institutions, start_date, end_date,
                                               pathogen = "Clostridioides difficile",
                                               n_bumps_per_month = 3, cases_per_bump = c(3, 9)) {
  eligible <- institutions[institutions$institution_type %in% c("ltc_institution", "hospital"), ]
  months <- seq(as.Date(format(start_date, "%Y-%m-01")), end_date, by = "month")

  rows <- list()
  bump_id <- 0L
  for (m in months) {
    month_start <- as.Date(m, origin = "1970-01-01")
    n_bumps <- stats::rpois(1, n_bumps_per_month)
    for (b in seq_len(n_bumps)) {
      inst <- eligible[sample(nrow(eligible), 1), ]
      n_cases <- sample(cases_per_bump[1]:cases_per_bump[2], 1)
      onset_offsets <- round(stats::rgamma(n_cases, shape = 3, rate = 1.5))
      case_dates <- month_start + sample(0:27, 1) + onset_offsets
      case_dates <- case_dates[case_dates >= start_date & case_dates <= end_date]
      n_cases <- length(case_dates)
      if (n_cases == 0) next

      bump_id <- bump_id + 1L
      is_hospital <- identical(inst$institution_type, "hospital")
      rows[[length(rows) + 1]] <- data.frame(
        patient_key = sprintf("PT-VOL-%04d-%03d", bump_id, seq_len(n_cases)),
        sample_date = as.character(case_dates),
        receipt_date = as.character(case_dates + 1),
        pathogen = pathogen,
        care_line = inst$care_line,
        institution_key = inst$institution_key,
        institution_display_name = inst$institution_display_name,
        institution_type = inst$institution_type,
        municipality = inst$municipality,
        ward = if (is_hospital) sample(c("Interne", "Chirurgie", "Longziekten", "Geriatrie", "IC"), n_cases, replace = TRUE) else NA_character_,
        specialism = if (is_hospital) sample(c("Interne geneeskunde", "Chirurgie", "Longziekten", "Klinische geriatrie"), n_cases, replace = TRUE) else NA_character_,
        pc = sample(episode_synthetic_pc_pool(), n_cases, replace = TRUE),
        sex = sample(c("M", "F"), n_cases, replace = TRUE),
        age = pmin(pmax(round(stats::rnorm(n_cases, mean = 75, sd = 12)), 40), 100),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

#' Inject a propagated outbreak: community spread with generation-interval waves
#' @keywords internal
#' @noRd
episode_synthetic_outbreak_propagated <- function(pc_pool, end_date, n_generations = 4,
                                                    cases_per_generation = c(2, 4, 6, 3)) {
  epi_pc <- sample(pc_pool, 1)
  start_date <- end_date - 90
  serial_interval <- 20  # Bordetella pertussis-like, days

  rows <- list()
  gen_start <- start_date
  for (g in seq_len(n_generations)) {
    n <- cases_per_generation[g]
    case_dates <- gen_start + round(stats::rgamma(n, shape = 2, rate = 2 / serial_interval))
    rows[[g]] <- data.frame(
      patient_key = sprintf("PT-OUTBREAK-PROP-G%d-%03d", g, seq_len(n)),
      sample_date = as.character(case_dates),
      receipt_date = as.character(case_dates + 1),
      pathogen = "Bordetella pertussis",
      care_line = "first",
      institution_key = "GP-01",
      institution_display_name = "Groningen",
      institution_type = "gp_municipality",
      municipality = "Groningen",
      ward = NA_character_,
      specialism = NA_character_,
      pc = epi_pc,
      sex = sample(c("M", "F"), n, replace = TRUE),
      age = pmin(pmax(round(stats::rnorm(n, mean = 8, sd = 4)), 0), 18),
      stringsAsFactors = FALSE
    )
    gen_start <- gen_start + serial_interval
  }
  do.call(rbind, rows)
}
