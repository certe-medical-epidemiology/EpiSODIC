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

#' Generate Synthetic Outbreak Data
#'
#' Produces several years of laboratory surveillance data for a fictional
#' northern-Netherlands region - eight hospitals, twenty long-term care
#' institutions and the region's municipalities - and injects six
#' outbreaks for the detectors to find. This is what powers
#' [episodic_demo()] and the package's test suite, and it doubles as a
#' worked example of the shape your own data should have (see
#' [episodic_case_data]).
#'
#' @section What it puts in:
#'
#' A seasonal Poisson baseline for eight endemic pathogens, deliberately
#' thin per place: a demo whose baseline keeps tripping the rule-based
#' detectors by coincidence buries the outbreaks it is meant to
#' demonstrate. Patients recur - roughly one case in six is a repeat
#' positive from a patient already in the data - so deduplication and
#' episode grouping have something to do.
#'
#' On top of that, six outbreaks, sized from a single case to a regional
#' wave, each shaped for a different detector:
#'
#' \tabular{lll}{
#'   \strong{Outbreak} \tab \strong{Shape} \tab \strong{Found by} \cr
#'   Invasive meningococcal disease \tab one case \tab `rare_trigger` \cr
#'   Ward cluster \tab 3 cases, one ward, 12 days \tab `same_place` \cr
#'   Nursing home outbreak \tab 8 cases, one institution, 9 days \tab `same_place` \cr
#'   Point source \tab 14 cases, one ward, days apart \tab `same_place` \cr
#'   Propagated \tab 15 cases, four generations, 90 days \tab `same_place`, Rt \cr
#'   Regional wave \tab 144 cases, diffuse, 6 rising weeks \tab Farrington, MEM \cr
#' }
#'
#' The regional wave is deliberately spread one case to a place: no
#' institution sees enough of it for a rule-based detector to notice, and
#' only the statistical baseline comparison finds it. That contrast - a
#' diffuse signal no amount of local vigilance would catch - is half the
#' reason the statistical detectors exist.
#'
#' Outbreaks are anchored to `end_date` and clipped to the window you ask
#' for, so a short window returns a partial one rather than cases outside
#' the range you asked for.
#'
#' @section What was injected, as data:
#'
#' The result carries its own ground truth: which outbreaks were injected,
#' where and when each of them ran, and exactly which cases belong to
#' which. Read it with [episodic_synthetic_ground_truth()]. Injected cases
#' also carry a `PT-OUTBREAK-*` patient key, but that is a convenience for
#' a human reading a line list and not an interface: anything measuring
#' detection against what was injected takes the ground truth, never a
#' pattern match on a key.
#'
#' @param end_date Last sample date to generate. Defaults to today, so a
#'   demo built from this always shows current surveillance rather than
#'   whatever year the package was released in.
#' @param start_date First sample date to generate. Defaults to five years
#'   before `end_date` - Farrington needs four of them before it will
#'   compare anything against anything.
#' @param seed RNG seed, for reproducible demo data.
#' @param outbreaks Which outbreaks to inject: `TRUE` for all six (the
#'   default), `FALSE` for none, or a character vector of outbreak
#'   identifiers (`"RARE"`, `"WARD"`, `"LTC"`, `"PS"`, `"PROP"`,
#'   `"WAVE"`). A history with none injected is what a negative control
#'   needs: every alarm raised on it is a false one, which is the only
#'   clean way to measure an alarm rate. Each outbreak draws from the
#'   random stream whether or not it is kept, so a given seed always
#'   produces the same cases for a given outbreak regardless of which
#'   others were asked for.
#' @param outbreak_offsets Days to move outbreaks further back from
#'   `end_date`. All six are anchored to `end_date`, so by default they
#'   sit in the last few months of whatever window is generated - fine
#'   for a demo, wrong for a prospective evaluation, which needs them
#'   dispersed across the window instead. Either a single number applied
#'   to all of them, or a named vector giving days per outbreak
#'   identifier (any not named stays at 0). `NULL`, the default, leaves
#'   every outbreak anchored to `end_date`.
#' @return A data frame satisfying [episodic_check_cases()], carrying the
#'   ground truth of what was injected as an attribute - see
#'   [episodic_synthetic_ground_truth()].
#' @seealso [episodic_synthetic_ground_truth()] for what was injected,
#'   [episodic_check_cases()] to see what the requirements make of it,
#'   and [episodic_synthetic_cases_calibration()] for many more clusters
#'   than a demo wants, to tune a configuration against.
#' @examples
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' nrow(cases)
#' head(cases)
#' @export
episodic_synthetic_cases <- function(start_date = end_date - 5 * 365,
                                     end_date = Sys.Date(),
                                     seed = 1,
                                     outbreaks = TRUE,
                                     outbreak_offsets = NULL) {
  set.seed(seed)

  institutions <- episodic_synthetic_institutions()
  pc_pool <- episodic_synthetic_pc_pool()
  pathogens <- episodic_synthetic_pathogen_profiles()

  dates <- seq(start_date, end_date, by = "day")

  baseline <- episodic_synthetic_baseline_cases(
    dates,
    institutions,
    pc_pool,
    pathogens
  )
  baseline$outbreak_id <- NA_character_
  injected <- episodic_synthetic_outbreaks(
    institutions,
    pc_pool,
    start_date,
    end_date,
    which = outbreaks,
    offsets = outbreak_offsets
  )

  cases <- rbind(baseline, injected)
  cases$source_key <- sprintf("SYN-%08d", seq_len(nrow(cases)))
  cases$lab_number <- sprintf("LABSYN-%08d", seq_len(nrow(cases)))

  cases <- cases[order(cases$sample_date), ]
  rownames(cases) <- NULL

  truth <- episodic_synthetic_truth(cases)
  cases$outbreak_id <- NULL

  cases <- episodic_validate_cases(cases)
  attr(cases, "episodic_ground_truth") <- truth
  invisible(cases)
}

#' The fictional region: who reports, and how much of the total they see
#'
#' `weight` is the share of baseline cases a place draws, before
#' seasonality: hospitals and the larger municipalities see more than a
#' nursing home does. It is also what keeps the baseline thin enough per
#' place that coincidence alone does not fire `same_place` - which is what
#' made an earlier version of this generator produce hundreds of
#' three-case clusters and bury the six real ones.
#' @keywords internal
#' @noRd
episodic_synthetic_institutions <- function() {
  hospitals <- data.frame(
    institution_key = sprintf("HOSP-%02d", 1:8),
    institution_display_name = paste("Hospital", LETTERS[1:8]),
    institution_type = "hospital",
    care_line = "second",
    municipality = NA_character_,
    n_beds = c(850, 620, 410, 380, 300, 260, 240, 200),
    # A ward per 35 beds or so: without it, the biggest hospital's cases
    # would pile onto the same ten wards as the smallest one's and start
    # tripping `same_place` by coincidence.
    n_wards = round(c(850, 620, 410, 380, 300, 260, 240, 200) / 35),
    weight = 60 * c(850, 620, 410, 380, 300, 260, 240, 200) / 3260,
    is_monitored = TRUE,
    stringsAsFactors = FALSE
  )
  ltc <- data.frame(
    institution_key = sprintf("LTC-%02d", 1:20),
    institution_display_name = paste("Zorgcentrum", 1:20),
    institution_type = "ltc_institution",
    care_line = "first",
    municipality = NA_character_,
    n_beds = sample(40:180, 20),
    n_wards = NA_integer_,
    weight = 10 / 20,
    is_monitored = FALSE,
    stringsAsFactors = FALSE
  )
  municipalities <- episodic_synthetic_municipalities()
  gp <- data.frame(
    institution_key = sprintf("GP-%02d", seq_along(municipalities)),
    institution_display_name = municipalities,
    institution_type = "gp_municipality",
    care_line = "first",
    municipality = municipalities,
    n_beds = NA_integer_,
    n_wards = NA_integer_,
    # A gentle spread rather than a true population one: the largest
    # municipality drawing a hundred times what the smallest does would
    # make that one municipality its own noise source.
    weight = 30 *
      rep(c(1.5, 1.0, 0.6), length.out = length(municipalities)) /
      (37.2 * length(municipalities) / 36),
    is_monitored = FALSE,
    stringsAsFactors = FALSE
  )
  rbind(hospitals, ltc, gp)
}

#' @keywords internal
#' @noRd
episodic_synthetic_municipalities <- function() {
  c(
    # Groningen
    "Groningen",
    "Eemsdelta",
    "Het Hogeland",
    "Westerkwartier",
    "Midden-Groningen",
    "Oldambt",
    "Veendam",
    "Stadskanaal",
    "Pekela",
    "Westerwolde",
    # Fryslan
    "Leeuwarden",
    "Smallingerland",
    "Heerenveen",
    "S\u00fadwest-Frysl\u00e2n",
    "De Fryske Marren",
    "Achtkarspelen",
    "Tytsjerksteradiel",
    "Noardeast-Frysl\u00e2n",
    "Dantumadiel",
    "Waadhoeke",
    "Harlingen",
    "Opsterland",
    "Weststellingwerf",
    "Ooststellingwerf",
    # Drenthe
    "Assen",
    "Emmen",
    "Hoogeveen",
    "Meppel",
    "Coevorden",
    "Borger-Odoorn",
    "Aa en Hunze",
    "Noordenveld",
    "Tynaarlo",
    "Westerveld",
    "De Wolden",
    "Midden-Drenthe"
  )
}

#' @keywords internal
#' @noRd
episodic_synthetic_wards <- function(n = NULL) {
  wards <- c(
    "Internal Medicine",
    "Surgery",
    "Pulmonology",
    "Geriatrics",
    "ICU",
    "Cardiology",
    "Neurology",
    "Oncology",
    "Urology",
    "Rehabilitation",
    "Orthopaedics",
    "ENT",
    "Gynaecology",
    "Paediatrics",
    "Neonatology",
    "Nephrology",
    "Gastroenterology",
    "Rheumatology",
    "Dermatology",
    "Haematology",
    "Vascular Surgery",
    "Neurosurgery",
    "Trauma Surgery",
    "Psychiatry",
    "Obstetrics"
  )
  if (is.null(n)) {
    return(wards)
  }
  wards[seq_len(min(n, length(wards)))]
}

#' @keywords internal
#' @noRd
episodic_synthetic_specialism <- function(ward) {
  lookup <- c(
    "Internal Medicine" = "Internal Medicine",
    "Surgery" = "Surgery",
    "Pulmonology" = "Pulmonology",
    "Geriatrics" = "Clinical Geriatrics",
    "ICU" = "Intensive Care",
    "Cardiology" = "Cardiology",
    "Neurology" = "Neurology",
    "Oncology" = "Internal Medicine",
    "Urology" = "Urology",
    "Rehabilitation" = "Rehabilitation Medicine",
    "Orthopaedics" = "Orthopaedics",
    "ENT" = "Ear, Nose and Throat",
    "Gynaecology" = "Gynaecology",
    "Paediatrics" = "Paediatrics",
    "Neonatology" = "Paediatrics",
    "Nephrology" = "Internal Medicine",
    "Gastroenterology" = "Gastroenterology and Hepatology",
    "Rheumatology" = "Rheumatology",
    "Dermatology" = "Dermatology",
    "Haematology" = "Internal Medicine",
    "Vascular Surgery" = "Surgery",
    "Neurosurgery" = "Neurosurgery",
    "Trauma Surgery" = "Surgery",
    "Psychiatry" = "Psychiatry",
    "Obstetrics" = "Obstetrics"
  )
  out <- unname(lookup[ward])
  out[is.na(ward)] <- NA_character_
  out
}

#' @keywords internal
#' @noRd
episodic_synthetic_pc_pool <- function() {
  c(
    paste0("9", sprintf("%03d", sample(0:999, 40))), # Groningen province
    paste0("8", sprintf("%03d", sample(0:999, 40))), # Fryslan
    paste0("7", sprintf("%03d", sample(100:999, 30)))
  ) # Drenthe
}

#' The endemic baseline: eight pathogens, each with its own season
#'
#' `mean_daily` is the region-wide baseline mean before seasonality, and
#' is deliberately modest: spread over a hundred-odd reporting places, it
#' leaves each one far below what the rule-based detectors trigger on, so
#' the clusters in the demo are the injected ones rather than coincidence.
#' amplitude/phase_day describe a sinusoidal season, phase_day being the
#' day-of-year of peak incidence. `pathogen` values match
#' `inst/config/episodic_default_pathogen_config.csv` exactly, as raw lab-provided strings.
#' @keywords internal
#' @noRd
episodic_synthetic_pathogen_profiles <- function() {
  data.frame(
    pathogen = c(
      "Norovirus",
      "Influenza A",
      "Campylobacter",
      "Salmonella",
      "RSV",
      "Clostridioides difficile",
      "MRSA",
      "Giardia lamblia"
    ),
    mean_daily = c(0.47, 0.42, 0.38, 0.17, 0.30, 0.21, 0.07, 0.09),
    amplitude = c(0.5, 0.7, 0.4, 0.3, 0.7, 0.1, 0.05, 0.1),
    phase_day = c(15, 15, 200, 210, 350, 180, 180, 200), # ~mid-Jan, mid-Jul etc.
    stringsAsFactors = FALSE
  )
}

#' One case row, however it was generated
#'
#' Every generator here - baseline, each outbreak, the calibration volume -
#' produces the same fourteen columns in the same order, so they rbind
#' without anyone having to keep six copies of the column list in step.
#'
#' @param patient_key,sample_date,pathogen,pc,sex,age Per-case values.
#' @param institution A data frame of institution rows, one per case (or a
#'   single row, recycled).
#' @param ward Ward per case, `NA` outside hospitals.
#' @return A data frame
#' @keywords internal
#' @noRd
episodic_synthetic_case_rows <- function(patient_key,
                                         sample_date,
                                         pathogen,
                                         institution,
                                         ward,
                                         pc,
                                         sex,
                                         age) {
  sample_date <- as.Date(sample_date)
  n <- length(sample_date)
  data.frame(
    patient_key = patient_key,
    sample_date = as.character(sample_date),
    receipt_date = as.character(
      sample_date +
        sample(0:3, n, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.05))
    ),
    pathogen = pathogen,
    care_line = institution$care_line,
    institution_key = institution$institution_key,
    institution_display_name = institution$institution_display_name,
    institution_type = institution$institution_type,
    municipality = institution$municipality,
    ward = ward,
    specialism = episodic_synthetic_specialism(ward),
    pc = pc,
    sex = sex,
    age = age,
    stringsAsFactors = FALSE
  )
}

#' The end of the last complete week
#'
#' Surveillance reads its own weeks: a partial current week is a real
#' phenomenon, but it is a poor thing to demonstrate a detector on, since
#' what the statistical detectors test is exactly one week - the run's
#' current one - and how much of it exists depends on which day you
#' happened to run the demo.
#'
#' @param today The date to work back from.
#' @return The `Date` of the most recent Sunday before `today`.
#' @keywords internal
#' @noRd
episodic_synthetic_week_end <- function(today = Sys.Date()) {
  today - as.integer(format(today, "%u"))
}

#' A ward for each case, from the wards its own hospital has
#'
#' Non-hospital rows get `NA`: long-term care, out-of-hours services and
#' general practice have no wards in the schema, and `same_place` watches
#' the institution for them instead.
#' @keywords internal
#' @noRd
episodic_synthetic_ward_draw <- function(institution) {
  n_wards <- institution$n_wards
  is_hospital <- institution$institution_type == "hospital" & !is.na(n_wards)
  ward <- rep(NA_character_, nrow(institution))
  if (!any(is_hospital)) {
    return(ward)
  }
  wards <- episodic_synthetic_wards()
  drawn <- ceiling(stats::runif(sum(is_hospital)) * n_wards[is_hospital])
  ward[is_hospital] <- wards[pmin(pmax(drawn, 1L), length(wards))]
  ward
}

#' Patient keys that repeat, because patients do
#'
#' Drawn from a pool three times the number of cases, which leaves about
#' one case in six a repeat positive from a patient already in the data.
#' Without that, every positive is its own patient, deduplication has
#' nothing to collapse, and a demo shows none of it.
#' @keywords internal
#' @noRd
episodic_synthetic_patient_keys <- function(pathogen, n) {
  sprintf(
    "PT-%s-%06d",
    make.names(pathogen),
    sample.int(max(3L * n, 10L), n, replace = TRUE)
  )
}

#' The endemic background every detector is measured against
#' @keywords internal
#' @noRd
episodic_synthetic_baseline_cases <- function(dates,
                                              institutions,
                                              pc_pool,
                                              pathogens) {
  rows <- list()
  for (i in seq_len(nrow(pathogens))) {
    org <- pathogens[i, ]
    doy <- as.integer(format(dates, "%j"))
    seasonal_mean <- org$mean_daily *
      (1 + org$amplitude * cos(2 * pi * (doy - org$phase_day) / 365.25))
    n_per_day <- stats::rpois(length(dates), lambda = pmax(seasonal_mean, 0.01))
    n_total <- sum(n_per_day)
    if (n_total == 0) {
      next
    }
    case_dates <- rep(dates, times = n_per_day)
    inst <- institutions[
      sample(
        seq_len(nrow(institutions)),
        n_total,
        replace = TRUE,
        prob = institutions$weight
      ),
    ]
    ward <- episodic_synthetic_ward_draw(inst)
    frame <- episodic_synthetic_case_rows(
      patient_key = episodic_synthetic_patient_keys(org$pathogen, n_total),
      sample_date = case_dates,
      pathogen = org$pathogen,
      institution = inst,
      ward = ward,
      pc = sample(pc_pool, n_total, replace = TRUE),
      sex = sample(c("M", "F"), n_total, replace = TRUE),
      age = pmin(
        pmax(round(stats::rnorm(n_total, mean = 45, sd = 25)), 0),
        100
      )
    )
    # A patient who turns up twice turns up in the same place, is the same
    # sex and much the same age both times. Copying the first occurrence
    # is cruder than modelling it and produces data that reads right.
    first <- match(frame$patient_key, frame$patient_key)
    for (column in c(
      "care_line",
      "institution_key",
      "institution_display_name",
      "institution_type",
      "municipality",
      "ward",
      "specialism",
      "pc",
      "sex",
      "age"
    )) {
      frame[[column]] <- frame[[column]][first]
    }
    rows[[length(rows) + 1]] <- frame
  }
  do.call(rbind, rows)
}

#' What each of the six injected outbreaks is, and what it is for
#'
#' One row per outbreak, and the single place the identifiers, the
#' generator behind each of them and the channel each shape was built to
#' exercise are written down. `episodic_synthetic_outbreaks()` deals from
#' it, `episodic_synthetic_truth()` labels from it, and nothing else in
#' the package needs to know the six exist.
#'
#' `expected_channel` and `expected_level` are the design intent recorded
#' by the generator that produces each shape, not a prediction: which
#' detector actually fires first, and at which lattice level, is a
#' measurement, and `episodic_validate_detection()` makes it.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_catalogue <- function() {
  data.frame(
    outbreak_id = c("RARE", "WARD", "LTC", "PS", "PROP", "WAVE"),
    label = c(
      "Invasive meningococcal disease",
      "Ward cluster",
      "Nursing home outbreak",
      "Point source",
      "Propagated",
      "Regional wave"
    ),
    expected_channel = c(
      "rare_trigger",
      "same_place",
      "same_place",
      "same_place",
      "same_place",
      "farrington"
    ),
    expected_level = c(
      "pathogen_ward",
      "pathogen_ward",
      "pathogen_institution",
      "pathogen_ward",
      "pathogen_institution",
      "pathogen_region"
    ),
    stringsAsFactors = FALSE
  )
}

#' Which outbreaks an `outbreaks =` argument asks for
#'
#' Always returned in catalogue order, so the generators run in the same
#' order whatever was asked for.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_ids <- function(which) {
  known <- episodic_synthetic_outbreak_catalogue()$outbreak_id
  if (isTRUE(which)) {
    return(known)
  }
  if (isFALSE(which)) {
    return(character(0))
  }
  if (!is.character(which) || anyNA(which)) {
    stop(
      "`outbreaks` must be TRUE (inject all of them), FALSE (inject none) ",
      "or a character vector naming outbreaks to inject (",
      paste(known, collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  unknown <- setdiff(which, known)
  if (length(unknown) > 0) {
    stop(
      "`outbreaks` names no such outbreak: ",
      paste(unknown, collapse = ", "),
      ". Known outbreaks are ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  known[known %in% which]
}

#' Days back from `end_date` to anchor each outbreak at
#'
#' @return A named numeric vector, one element per element of `ids`.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_offsets_resolve <- function(offsets, ids) {
  resolved <- stats::setNames(rep(0, length(ids)), ids)
  if (is.null(offsets)) {
    return(resolved)
  }
  if (!is.numeric(offsets) || anyNA(offsets) || length(offsets) == 0) {
    stop(
      "`outbreak_offsets` must be a numeric vector of days without NA, or ",
      "NULL to anchor every outbreak to `end_date`.",
      call. = FALSE
    )
  }
  if (any(offsets < 0)) {
    stop(
      "`outbreak_offsets` are days back from `end_date`, so they cannot be ",
      "negative: no outbreak can be injected after the last day generated.",
      call. = FALSE
    )
  }
  if (is.null(names(offsets))) {
    if (length(offsets) != 1) {
      stop(
        "An unnamed `outbreak_offsets` is one number applied to every ",
        "outbreak, so it must be length 1. Name its elements (",
        paste(episodic_synthetic_outbreak_catalogue()$outbreak_id, collapse = ", "),
        ") to set an offset per outbreak.",
        call. = FALSE
      )
    }
    resolved[] <- offsets
    return(resolved)
  }
  known <- episodic_synthetic_outbreak_catalogue()$outbreak_id
  unknown <- setdiff(names(offsets), known)
  if (length(unknown) > 0) {
    stop(
      "`outbreak_offsets` names no such outbreak: ",
      paste(unknown, collapse = ", "),
      ". Known outbreaks are ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  named <- intersect(names(offsets), ids)
  resolved[named] <- offsets[named]
  resolved
}

#' The injected outbreaks, clipped to the window asked for
#'
#' Anchored to `end_date` (less each outbreak's own offset) so a demo
#' opens on recent signal, and filtered to `[start_date, end_date]`
#' afterwards: a generator asked for January must not return December.
#'
#' Every generator is called whether or not its outbreak was asked for,
#' and the unwanted ones are dropped afterwards. That costs a handful of
#' random draws and buys the property a validation harness needs: one
#' seed produces one set of cases per outbreak, and asking for fewer
#' outbreaks does not silently reshape the ones that remain.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreaks <- function(institutions,
                                         pc_pool,
                                         start_date,
                                         end_date,
                                         which = TRUE,
                                         offsets = NULL) {
  ids <- episodic_synthetic_outbreak_ids(which)
  offsets <- episodic_synthetic_outbreak_offsets_resolve(offsets, ids)
  generators <- list(
    RARE = episodic_synthetic_outbreak_rare_case,
    WARD = episodic_synthetic_outbreak_ward_cluster,
    LTC = episodic_synthetic_outbreak_ltc,
    PS = episodic_synthetic_outbreak_point_source,
    PROP = episodic_synthetic_outbreak_propagated,
    WAVE = episodic_synthetic_outbreak_regional_wave
  )
  parts <- list()
  for (id in names(generators)) {
    anchor <- if (id %in% ids) end_date - offsets[[id]] else end_date
    part <- generators[[id]](institutions, pc_pool, anchor)
    if (!(id %in% ids) || is.null(part) || nrow(part) == 0) {
      next
    }
    part$outbreak_id <- id
    parts[[length(parts) + 1]] <- part
  }
  if (length(parts) == 0) {
    return(NULL)
  }
  out <- do.call(rbind, parts)
  in_window <- as.Date(out$sample_date) >= start_date &
    as.Date(out$sample_date) <= end_date
  out <- out[in_window, ]
  if (nrow(out) == 0) NULL else out
}

#' Read Back What Was Injected Into Synthetic Data
#'
#' [episodic_synthetic_cases()] injects outbreaks into an otherwise
#' endemic history. This reads back exactly what it injected: one row per
#' outbreak saying where and when it ran and how large it was, and the
#' case-level membership that goes with it. It is what makes a detection
#' result measurable against known truth rather than against an
#' epidemiologist's verdict on it - see [episodic_validate_detection()].
#'
#' Injected cases also carry a recognisable `PT-OUTBREAK-*` patient key,
#' and it is tempting to recover membership by matching on it. Do not:
#' that couples whatever does the matching to a naming convention nobody
#' has undertaken to keep, and it fails silently, as a smaller set of
#' outbreak cases rather than an error, the first time the convention
#' changes.
#'
#' @param cases A data frame from [episodic_synthetic_cases()].
#' @return A list of two data frames:
#'   \describe{
#'     \item{`outbreaks`}{One row per injected outbreak: `outbreak_id`,
#'       `label`, `pathogen`, `institution_key` and `ward` (both `NA`
#'       where the outbreak was not confined to one), `first_day`,
#'       `peak_day`, `last_day`, `n_cases`, and the `expected_channel`
#'       and `expected_level` the shape was designed to exercise.}
#'     \item{`cases`}{One row per injected case: `outbreak_id` and the
#'       `source_key` it carries in the case data, which is what joins it
#'       to `episodic_case` once a run has loaded it.}
#'   }
#'   Both are empty (zero rows, same columns) when nothing was injected.
#' @seealso [episodic_synthetic_cases()], [episodic_validate_detection()]
#' @examples
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' truth <- episodic_synthetic_ground_truth(cases)
#' truth$outbreaks[, c("outbreak_id", "pathogen", "n_cases")]
#' @export
episodic_synthetic_ground_truth <- function(cases) {
  truth <- attr(cases, "episodic_ground_truth", exact = TRUE)
  if (is.null(truth)) {
    stop(
      "This data carries no ground truth. It has to come from ",
      "episodic_synthetic_cases(), and the attribute has to survive ",
      "whatever happened to it since - subsetting rows keeps it, but ",
      "selecting columns, dplyr verbs and most round trips through a file ",
      "do not.",
      call. = FALSE
    )
  }
  truth
}

#' Describe the injected outbreaks present in a generated case set
#'
#' Built from the cases that survived clipping to the requested window,
#' never from what the generators were asked for, so a partially clipped
#' outbreak is described by the part that is actually there.
#' @param cases The assembled case data, still carrying its
#'   `outbreak_id` column and with `source_key` already assigned.
#' @return The list `episodic_synthetic_ground_truth()` returns.
#' @keywords internal
#' @noRd
episodic_synthetic_truth <- function(cases) {
  catalogue <- episodic_synthetic_outbreak_catalogue()
  injected <- cases[!is.na(cases$outbreak_id), , drop = FALSE]
  ids <- catalogue$outbreak_id[
    catalogue$outbreak_id %in% unique(injected$outbreak_id)
  ]
  rows <- lapply(ids, function(id) {
    part <- injected[injected$outbreak_id == id, , drop = FALSE]
    meta <- catalogue[catalogue$outbreak_id == id, ]
    if (length(unique(part$pathogen)) != 1) {
      stop(
        "Injected outbreak '",
        id,
        "' spans more than one pathogen (",
        paste(sort(unique(part$pathogen)), collapse = ", "),
        "), which no generator here is meant to produce.",
        call. = FALSE
      )
    }
    days <- as.Date(part$sample_date)
    per_day <- table(days)
    data.frame(
      outbreak_id = id,
      label = meta$label,
      pathogen = part$pathogen[1],
      institution_key = episodic_synthetic_truth_single(part$institution_key),
      ward = episodic_synthetic_truth_single(part$ward),
      first_day = min(days),
      peak_day = as.Date(names(per_day)[which.max(per_day)]),
      last_day = max(days),
      n_cases = nrow(part),
      expected_channel = meta$expected_channel,
      expected_level = meta$expected_level,
      stringsAsFactors = FALSE
    )
  })
  outbreaks <- if (length(rows) == 0) {
    episodic_synthetic_truth_empty()
  } else {
    do.call(rbind, rows)
  }
  rownames(outbreaks) <- NULL
  list(
    outbreaks = outbreaks,
    cases = data.frame(
      outbreak_id = injected$outbreak_id,
      source_key = injected$source_key,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )
}

#' The one value a column holds, or `NA` when it holds several
#'
#' An outbreak confined to one ward has a ward; the regional wave, spread
#' across every place in the region, has none. `NA` there says "this
#' outbreak was not at one place", which is a different statement from
#' any particular place, and the difference matters to anything matching
#' clusters against it.
#' @keywords internal
#' @noRd
episodic_synthetic_truth_single <- function(x) {
  values <- unique(x)
  if (length(values) == 1) as.character(values) else NA_character_
}

#' A zero-row ground-truth table with the columns a full one has
#'
#' Returned when nothing was injected. Nothing injected is a real state -
#' it is what a negative control is - so it gets an empty table of the
#' right shape rather than `NULL`, and every caller can bind and subset
#' it without a special case.
#' @keywords internal
#' @noRd
episodic_synthetic_truth_empty <- function() {
  data.frame(
    outbreak_id = character(0),
    label = character(0),
    pathogen = character(0),
    institution_key = character(0),
    ward = character(0),
    first_day = as.Date(character(0)),
    peak_day = as.Date(character(0)),
    last_day = as.Date(character(0)),
    n_cases = integer(0),
    expected_channel = character(0),
    expected_level = character(0),
    stringsAsFactors = FALSE
  )
}

#' One case of an always-notable pathogen: the `rare_trigger` channel
#'
#' Invasive meningococcal disease has no meaningful baseline to compare a
#' case against - one is a public health event on its own - and it is the
#' smallest thing the board will ever be asked to look at.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_rare_case <- function(institutions,
                                                  pc_pool,
                                                  end_date) {
  hospital <- institutions[institutions$institution_type == "hospital", ][2, ]
  episodic_synthetic_case_rows(
    patient_key = "PT-OUTBREAK-RARE-001",
    sample_date = end_date - 12,
    pathogen = "Neisseria meningitidis",
    institution = hospital,
    ward = "Neurologie",
    pc = sample(pc_pool, 1),
    sex = "M",
    age = 17L
  )
}

#' Three cases on one ward: exactly what `same_place` is set to notice
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_ward_cluster <- function(institutions,
                                                     pc_pool,
                                                     end_date,
                                                     n_cases = 3) {
  hospital <- institutions[institutions$institution_type == "hospital", ][3, ]
  case_dates <- end_date - 75 + c(0, 5, 11)[seq_len(n_cases)]
  episodic_synthetic_case_rows(
    patient_key = sprintf("PT-OUTBREAK-WARD-%03d", seq_len(n_cases)),
    sample_date = case_dates,
    pathogen = "Clostridioides difficile",
    institution = hospital,
    ward = "Chirurgie",
    pc = sample(pc_pool, n_cases, replace = TRUE),
    sex = sample(c("M", "F"), n_cases, replace = TRUE),
    age = pmin(pmax(round(stats::rnorm(n_cases, mean = 74, sd = 9)), 55), 98)
  )
}

#' A nursing home norovirus outbreak: one institution, one fortnight
#'
#' Long-term care has no wards in the schema, so `same_place` watches the
#' institution - which is the transmission unit there anyway.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_ltc <- function(institutions,
                                            pc_pool,
                                            end_date,
                                            n_cases = 8) {
  home <- institutions[institutions$institution_type == "ltc_institution", ][
    3,
  ]
  case_dates <- end_date - 33 + round(stats::rgamma(n_cases, 3, rate = 0.9))
  episodic_synthetic_case_rows(
    patient_key = sprintf("PT-OUTBREAK-LTC-%03d", seq_len(n_cases)),
    sample_date = case_dates,
    pathogen = "Norovirus",
    institution = home,
    ward = NA_character_,
    pc = sample(pc_pool, n_cases, replace = TRUE),
    sex = sample(c("M", "F"), n_cases, replace = TRUE),
    age = pmin(pmax(round(stats::rnorm(n_cases, mean = 84, sd = 7)), 65), 101)
  )
}

#' A point-source outbreak: one ward, tightly bunched in time
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_point_source <- function(institutions,
                                                     pc_pool,
                                                     end_date,
                                                     n_cases = 14) {
  hospital <- institutions[institutions$institution_type == "hospital", ][1, ]
  exposure_date <- end_date - 40
  # norovirus incubation is 0.5-3 days; all cases cluster within a few days
  case_dates <- exposure_date +
    round(stats::rgamma(n_cases, shape = 4, rate = 2.5))
  episodic_synthetic_case_rows(
    patient_key = sprintf("PT-OUTBREAK-PS-%03d", seq_len(n_cases)),
    sample_date = case_dates,
    pathogen = "Norovirus",
    institution = hospital,
    ward = "Pulmonology",
    pc = sample(pc_pool, n_cases, replace = TRUE),
    sex = sample(c("M", "F"), n_cases, replace = TRUE),
    age = pmin(pmax(round(stats::rnorm(n_cases, mean = 78, sd = 8)), 60), 100)
  )
}

#' A propagated outbreak: community spread in generation-interval waves
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_propagated <- function(institutions,
                                                   pc_pool,
                                                   end_date,
                                                   n_generations = 4,
                                                   cases_per_generation = c(2, 4, 6, 3)) {
  municipality <- institutions[
    institutions$institution_type == "gp_municipality",
  ][1, ]
  epi_pc <- sample(pc_pool, 1)
  serial_interval <- 20 # Bordetella pertussis-like, days
  # Far enough back that the last generation still lands inside the
  # window: an outbreak whose tail is clipped off is a different shape
  # from the one this is meant to demonstrate.
  first_date <- end_date - (n_generations + 1.5) * serial_interval

  rows <- list()
  gen_start <- first_date
  for (g in seq_len(n_generations)) {
    n <- cases_per_generation[g]
    # Capped at two serial intervals: a case further out than that
    # belongs to the next generation, not a long tail of this one.
    case_dates <- gen_start +
      pmin(
        round(stats::rgamma(n, shape = 2, rate = 2 / serial_interval)),
        2 * serial_interval
      )
    rows[[g]] <- episodic_synthetic_case_rows(
      patient_key = sprintf("PT-OUTBREAK-PROP-G%d-%03d", g, seq_len(n)),
      sample_date = case_dates,
      pathogen = "Bordetella pertussis",
      institution = municipality,
      ward = NA_character_,
      pc = epi_pc,
      sex = sample(c("M", "F"), n, replace = TRUE),
      age = pmin(pmax(round(stats::rnorm(n, mean = 8, sd = 4)), 0), 18)
    )
    gen_start <- gen_start + serial_interval
  }
  do.call(rbind, rows)
}

#' A regional wave: too diffuse for any rule, obvious to a baseline model
#'
#' Six weeks of out-of-proportion influenza A spread across the whole
#' region, and deliberately spread thin: each place sees one case in a
#' week, so no institution and no ward ever reaches `same_place`'s
#' threshold. Only the statistical comparison against the same weeks of
#' previous years finds it - which is the point of having one.
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_regional_wave <- function(institutions,
                                                      pc_pool,
                                                      end_date,
                                                      cases_per_week = c(10, 15, 21, 27, 33, 38)) {
  places <- episodic_synthetic_places(institutions)
  # A random tour of every place in the region, dealt out week by week:
  # walking it means a place is revisited only after the whole tour, weeks
  # later, so no place ever accumulates enough of the wave for a
  # rule-based detector to see it. That is the property being demonstrated.
  tour <- sample(nrow(places))
  n_weeks <- length(cases_per_week)
  rows <- list()
  dealt <- 0L
  for (w in seq_len(n_weeks)) {
    # Ending on end_date, and rising into it: `episodic_detect_farrington()`
    # tests the run's current week and no other, so a wave that has already
    # peaked and subsided is one no statistical detector here will report.
    week_start <- end_date - 7 * (n_weeks - w) - 6
    n <- min(cases_per_week[w], nrow(places))
    idx <- tour[(dealt + seq_len(n) - 1L) %% nrow(places) + 1L]
    dealt <- dealt + n
    place <- places[idx, ]
    rows[[w]] <- episodic_synthetic_case_rows(
      patient_key = sprintf("PT-OUTBREAK-WAVE-W%d-%03d", w, seq_len(n)),
      sample_date = week_start + sample(0:6, n, replace = TRUE),
      pathogen = "Influenza A",
      institution = place,
      ward = place$ward,
      pc = sample(pc_pool, n, replace = TRUE),
      sex = sample(c("M", "F"), n, replace = TRUE),
      age = pmin(pmax(round(stats::rnorm(n, mean = 42, sd = 26)), 0), 100)
    )
  }
  do.call(rbind, rows)
}

#' Every place the region has, one row each
#'
#' What `same_place` treats as a place: a ward inside a hospital, the
#' institution itself everywhere else. Used to spread the regional wave
#' thinly enough that no single place sees a cluster of it.
#' @keywords internal
#' @noRd
episodic_synthetic_places <- function(institutions) {
  hospitals <- institutions[institutions$institution_type == "hospital", ]
  per_hospital <- lapply(seq_len(nrow(hospitals)), function(i) {
    wards <- episodic_synthetic_wards(hospitals$n_wards[i])
    place <- hospitals[rep(i, length(wards)), ]
    place$ward <- wards
    place
  })
  others <- institutions[institutions$institution_type != "hospital", ]
  others$ward <- NA_character_
  rbind(do.call(rbind, per_hospital), others)
}

#' Generate Synthetic Data at Tunable Cluster Volume
#'
#' [episodic_synthetic_cases()] injects six outbreaks in total - enough to
#' show every detector working, and few enough that a demo dashboard reads
#' like a real morning's work. That is the wrong shape for tuning a
#' configuration against (e.g. deciding how many dossiers your board can
#' realistically review per month). This function fills that gap: on top
#' of the same baseline and the same six outbreaks, it adds many
#' independent case clusters for one chosen pathogen, at a rate you
#' control, so you can see how detection volume responds as you adjust
#' `n_bumps_per_month` or your own configuration. Every case it adds
#' carries a `PT-VOL-*` patient key so it is always identifiable as
#' synthetic tuning data, never mistaken for anything else.
#'
#' @param start_date,end_date The window to generate over; defaults as in
#'   [episodic_synthetic_cases()].
#' @param pathogen Which pathogen to generate the extra clusters for.
#'   Defaults to *Clostridioides difficile*, a plausible example of an
#'   endemic pathogen that produces frequent clusters at a busy institution.
#' @param n_bumps_per_month Average number of independent case clusters
#'   generated per calendar month. Raise or lower this to see how detection
#'   volume responds.
#' @param seed RNG seed, for reproducible runs.
#' @return A data frame satisfying [episodic_check_cases()],
#'   including everything [episodic_synthetic_cases()] produces
#'   (background baseline, the six demo outbreaks) plus the extra volume.
#' @examples
#' cases <- episodic_synthetic_cases_calibration(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-06-30"),
#'   n_bumps_per_month = 4
#' )
#' sum(startsWith(cases$patient_key, "PT-VOL-"))
#' @export
episodic_synthetic_cases_calibration <- function(start_date = end_date - 5 * 365,
                                                 end_date = Sys.Date(),
                                                 pathogen = "Clostridioides difficile",
                                                 n_bumps_per_month = 3,
                                                 seed = 1) {
  set.seed(seed)

  institutions <- episodic_synthetic_institutions()
  pc_pool <- episodic_synthetic_pc_pool()
  pathogens <- episodic_synthetic_pathogen_profiles()
  dates <- seq(start_date, end_date, by = "day")

  baseline <- episodic_synthetic_baseline_cases(
    dates,
    institutions,
    pc_pool,
    pathogens
  )
  injected <- episodic_synthetic_outbreaks(
    institutions,
    pc_pool,
    start_date,
    end_date
  )
  volume <- episodic_synthetic_outbreak_volume(
    institutions,
    pc_pool,
    start_date,
    end_date,
    pathogen = pathogen,
    n_bumps_per_month = n_bumps_per_month
  )

  # No ground truth is attached here, deliberately. The extra bumps are
  # real seeded case clusters that nothing labels, so a ground-truth
  # table covering only the six injected outbreaks would have a
  # validation harness count every bump it found as a false alarm. Tune a
  # configuration with this; measure detection with
  # episodic_synthetic_cases().
  if (!is.null(injected)) {
    injected$outbreak_id <- NULL
  }
  cases <- rbind(baseline, injected, volume)
  cases$source_key <- sprintf("SYN-%08d", seq_len(nrow(cases)))
  cases$lab_number <- sprintf("LABSYN-%08d", seq_len(nrow(cases)))

  cases <- cases[order(cases$sample_date), ]
  rownames(cases) <- NULL

  episodic_validate_cases(cases)
}

#' Inject many independent same-place case bumps for one pathogen
#'
#' The volume generator behind [episodic_synthetic_cases_calibration()].
#' Each bump is shaped like the `same_place` detector's own trigger
#' condition (several cases at one institution within a short window) so
#' it reliably becomes its own cluster on reconciliation, rather than
#' relying on Farrington to notice an elevated baseline.
#'
#' @param institutions From `episodic_synthetic_institutions()`.
#' @param pc_pool From `episodic_synthetic_pc_pool()`.
#' @param start_date,end_date The window to spread bumps across.
#' @param pathogen The pathogen name to stamp every generated case with.
#' @param n_bumps_per_month Average bumps per calendar month
#'   (`stats::rpois()`).
#' @param cases_per_bump A `c(min, max)` range; the actual count per bump
#'   is drawn uniformly from it.
#' @return A data frame in the same shape as this file's other
#'   generators, or `NULL` if no bumps were generated at all (possible,
#'   not likely, with a very short window or low `n_bumps_per_month`).
#' @keywords internal
#' @noRd
episodic_synthetic_outbreak_volume <- function(institutions,
                                               pc_pool,
                                               start_date,
                                               end_date,
                                               pathogen = "Clostridioides difficile",
                                               n_bumps_per_month = 3,
                                               cases_per_bump = c(3, 9)) {
  eligible <- institutions[
    institutions$institution_type %in% c("ltc_institution", "hospital"),
  ]
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
      case_dates <- case_dates[
        case_dates >= start_date & case_dates <= end_date
      ]
      n_cases <- length(case_dates)
      if (n_cases == 0) {
        next
      }

      bump_id <- bump_id + 1L
      rows[[length(rows) + 1]] <- episodic_synthetic_case_rows(
        patient_key = sprintf("PT-VOL-%04d-%03d", bump_id, seq_len(n_cases)),
        sample_date = case_dates,
        pathogen = pathogen,
        institution = inst,
        ward = if (identical(inst$institution_type, "hospital")) {
          sample(episodic_synthetic_wards(), n_cases, replace = TRUE)
        } else {
          NA_character_
        },
        pc = sample(pc_pool, n_cases, replace = TRUE),
        sex = sample(c("M", "F"), n_cases, replace = TRUE),
        age = pmin(
          pmax(round(stats::rnorm(n_cases, mean = 75, sd = 12)), 40),
          100
        )
      )
    }
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}
