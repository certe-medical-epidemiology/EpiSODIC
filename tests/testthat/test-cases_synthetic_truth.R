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

# What the generator says it injected has to be what it injected. A
# harness that measures detection against this is only as honest as this
# is.

window <- list(
  start_date = as.Date("2025-01-01"),
  end_date = as.Date("2025-12-31")
)

test_that("the ground truth accounts for every injected case, and only those", {
  cases <- episodic_synthetic_cases(
    start_date = window$start_date,
    end_date = window$end_date
  )
  truth <- episodic_synthetic_ground_truth(cases)

  expect_setequal(
    truth$cases$source_key,
    cases$source_key[startsWith(cases$patient_key, "PT-OUTBREAK-")]
  )
  expect_equal(sum(truth$outbreaks$n_cases), nrow(truth$cases))
  expect_false(anyDuplicated(truth$cases$source_key) > 0)
})

test_that("every outbreak's row describes its own cases", {
  cases <- episodic_synthetic_cases(
    start_date = window$start_date,
    end_date = window$end_date
  )
  truth <- episodic_synthetic_ground_truth(cases)
  rownames(cases) <- cases$source_key

  for (i in seq_len(nrow(truth$outbreaks))) {
    row <- truth$outbreaks[i, ]
    keys <- truth$cases$source_key[truth$cases$outbreak_id == row$outbreak_id]
    mine <- cases[keys, ]
    expect_equal(nrow(mine), row$n_cases)
    expect_equal(min(as.Date(mine$sample_date)), row$first_day)
    expect_equal(max(as.Date(mine$sample_date)), row$last_day)
    expect_equal(unique(mine$pathogen), row$pathogen)
    if (!is.na(row$institution_key)) {
      expect_equal(unique(mine$institution_key), row$institution_key)
    }
  }
})

test_that("an outbreak spread over many places names none of them", {
  cases <- episodic_synthetic_cases(
    start_date = window$start_date,
    end_date = window$end_date
  )
  truth <- episodic_synthetic_ground_truth(cases)
  wave <- truth$outbreaks[truth$outbreaks$outbreak_id == "WAVE", ]

  # NA because it was everywhere, which is a different statement from
  # any one institution - and the whole point of the regional wave.
  expect_true(is.na(wave$institution_key))
  expect_true(is.na(wave$ward))
})

test_that("outbreaks = FALSE injects nothing and says so", {
  cases <- episodic_synthetic_cases(
    start_date = window$start_date,
    end_date = window$end_date,
    outbreaks = FALSE
  )
  truth <- episodic_synthetic_ground_truth(cases)

  expect_equal(sum(startsWith(cases$patient_key, "PT-OUTBREAK-")), 0)
  expect_equal(nrow(truth$outbreaks), 0)
  expect_equal(nrow(truth$cases), 0)
  # Empty, with the columns a full one has, so a caller needs no special
  # case for a negative control.
  expect_true(all(
    c("outbreak_id", "first_day", "n_cases") %in% names(truth$outbreaks)
  ))
})

test_that("the baseline is the same whether or not outbreaks are injected", {
  with_outbreaks <- episodic_synthetic_cases(
    start_date = window$start_date,
    end_date = window$end_date
  )
  without <- episodic_synthetic_cases(
    start_date = window$start_date,
    end_date = window$end_date,
    outbreaks = FALSE
  )
  injected <- episodic_synthetic_ground_truth(with_outbreaks)$cases$source_key
  background <- with_outbreaks[!with_outbreaks$source_key %in% injected, ]

  # A negative control is only a control if it is the same history with
  # the outbreaks taken out, not a different history.
  expect_equal(nrow(background), nrow(without))
  expect_equal(background$patient_key, without$patient_key)
  expect_equal(background$sample_date, without$sample_date)
})

test_that("asking for some outbreaks does not reshape the others", {
  all_six <- episodic_synthetic_ground_truth(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date
    )
  )$outbreaks
  two <- episodic_synthetic_ground_truth(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreaks = c("LTC", "PS")
    )
  )$outbreaks

  expect_equal(two$outbreak_id, c("LTC", "PS"))
  expect_equal(
    two[, c("first_day", "last_day", "n_cases")],
    all_six[all_six$outbreak_id %in% c("LTC", "PS"), c(
      "first_day",
      "last_day",
      "n_cases"
    )],
    ignore_attr = TRUE
  )
})

test_that("offsets move an outbreak back and leave the rest alone", {
  anchored <- episodic_synthetic_ground_truth(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date
    )
  )$outbreaks
  moved <- episodic_synthetic_ground_truth(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreak_offsets = c(WARD = 100)
    )
  )$outbreaks

  ward_before <- anchored[anchored$outbreak_id == "WARD", ]
  ward_after <- moved[moved$outbreak_id == "WARD", ]
  expect_equal(
    as.numeric(ward_before$first_day - ward_after$first_day),
    100
  )
  others <- anchored$outbreak_id != "WARD"
  expect_equal(anchored$first_day[others], moved$first_day[others])
})

test_that("a single offset applies to every outbreak", {
  anchored <- episodic_synthetic_ground_truth(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date
    )
  )$outbreaks
  moved <- episodic_synthetic_ground_truth(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreak_offsets = 30
    )
  )$outbreaks
  expect_equal(as.numeric(anchored$first_day - moved$first_day), rep(30, 6))
})

test_that("a misnamed outbreak or offset is refused, not ignored", {
  expect_error(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreaks = "WAVES"
    ),
    "no such outbreak"
  )
  expect_error(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreak_offsets = c(WAVES = 10)
    ),
    "no such outbreak"
  )
  expect_error(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreak_offsets = c(WAVE = -10)
    ),
    "cannot be negative"
  )
  expect_error(
    episodic_synthetic_cases(
      start_date = window$start_date,
      end_date = window$end_date,
      outbreak_offsets = c(10, 20)
    ),
    "must be length 1"
  )
})

test_that("data carrying no ground truth says so rather than guessing", {
  expect_error(
    episodic_synthetic_ground_truth(data.frame(source_key = "x")),
    "carries no ground truth"
  )
  # The calibration generator deliberately attaches none: its extra
  # bumps are unlabelled case clusters, and a truth table covering only
  # the six injected outbreaks would have every bump counted as a false
  # alarm.
  calibration <- episodic_synthetic_cases_calibration(
    start_date = window$start_date,
    end_date = as.Date("2025-03-31"),
    n_bumps_per_month = 1
  )
  expect_error(
    episodic_synthetic_ground_truth(calibration),
    "carries no ground truth"
  )
})
