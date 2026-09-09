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

# The matching rules, tested without running a single detection: they are
# pure functions over data frames, and they are the part of a validation
# result a reviewer argues with.

cluster_cases <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) {
    data.frame(
      run_date = as.Date(r[[1]]),
      cluster_id = r[[2]],
      source_key = r[[3]],
      stringsAsFactors = FALSE
    )
  }))
}

truth_cases <- data.frame(
  outbreak_id = c("A", "A", "A", "A", "B"),
  source_key = c("a1", "a2", "a3", "a4", "b1"),
  sample_date = as.Date(c(
    "2025-01-01",
    "2025-01-02",
    "2025-01-10",
    "2025-01-11",
    "2025-01-05"
  )),
  stringsAsFactors = FALSE
)

test_that("overlap is computed on cases, in both directions", {
  membership <- cluster_cases(
    list("2025-01-12", 1L, c("a1", "a2", "a3", "x1"))
  )
  overlap <- episodic_validation_overlap(membership, truth_cases)

  expect_equal(nrow(overlap), 1)
  expect_equal(overlap$outbreak_id, "A")
  expect_equal(overlap$n_overlap, 3L)
  expect_equal(overlap$n_cluster, 4L)
  # 3 of the cluster's 4 cases are the outbreak's, and 3 of the
  # outbreak's 4 cases are in the cluster.
  expect_equal(overlap$precision, 0.75)
  expect_equal(overlap$recall_full, 0.75)
})

test_that("recall as of a run counts only the cases that existed then", {
  membership <- cluster_cases(
    list("2025-01-03", 1L, c("a1", "a2")),
    list("2025-01-12", 1L, c("a1", "a2", "a3", "a4"))
  )
  overlap <- episodic_validation_overlap(membership, truth_cases)
  early <- overlap[overlap$run_date == as.Date("2025-01-03"), ]

  # Two of the outbreak's four cases had been sampled by 3 January, and
  # the cluster held both: complete as of that day, half of it in the end.
  expect_equal(early$n_outbreak_asof, 2L)
  expect_equal(early$recall_asof, 1)
  expect_equal(early$recall_full, 0.5)
})

test_that("a cluster overlapping nothing produces no overlap rows", {
  membership <- cluster_cases(list("2025-01-12", 1L, c("x1", "x2")))
  overlap <- episodic_validation_overlap(membership, truth_cases)

  expect_equal(nrow(overlap), 0)
  expect_named(
    overlap,
    c(
      "run_date",
      "cluster_id",
      "outbreak_id",
      "n_overlap",
      "n_cluster",
      "n_outbreak_full",
      "n_outbreak_asof",
      "precision",
      "recall_full",
      "recall_asof"
    )
  )
})

test_that("overlap refuses input missing a column it needs", {
  expect_error(
    episodic_validation_overlap(
      data.frame(cluster_id = 1L, source_key = "a1"),
      truth_cases
    ),
    "run_date"
  )
  expect_error(
    episodic_validation_overlap(
      cluster_cases(list("2025-01-12", 1L, "a1")),
      data.frame(outbreak_id = "A", source_key = "a1")
    ),
    "sample_date"
  )
})

test_that("an outbreak is detected when a cluster holds enough of it", {
  membership <- cluster_cases(
    list("2025-01-05", 1L, c("a1", "a2")),
    list("2025-01-12", 1L, c("a1", "a2", "a3"))
  )
  truth <- data.frame(
    outbreak_id = "A",
    label = "Test",
    pathogen = "Norovirus",
    institution_key = "H1",
    ward = "W1",
    first_day = as.Date("2025-01-01"),
    peak_day = as.Date("2025-01-02"),
    last_day = as.Date("2025-01-11"),
    n_cases = 4L,
    expected_channel = "same_place",
    expected_level = "pathogen_ward",
    stringsAsFactors = FALSE
  )
  own <- truth_cases[truth_cases$outbreak_id == "A", ]
  runs <- as.Date(c("2025-01-05", "2025-01-12"))

  rows <- episodic_validation_outbreak_rows(
    episodic_validation_overlap(membership, truth_cases),
    truth,
    own,
    runs,
    min_recall = 0.6
  )

  expect_true(rows$detected)
  expect_equal(rows$cluster_id, 1L)
  expect_equal(rows$recall_full, 0.75)
  # As of 5 January the cluster held both cases there were, all of what
  # existed; it did not hold 60% of the whole outbreak until the 12th.
  # Which of those two dates counts as "found it" is exactly the choice
  # this pair of columns refuses to make silently.
  expect_equal(rows$detected_run, as.Date("2025-01-05"))
  expect_equal(rows$captured_run, as.Date("2025-01-12"))
  expect_equal(rows$delay_from_first, 4)
  expect_equal(rows$n_cases_remaining, 2L)
  expect_equal(rows$share_remaining, 0.5)
  # No cluster table was passed, so when the cluster first appeared is
  # not known here. NA, not the first run date.
  expect_true(is.na(rows$opened_run))
  expect_true(is.na(rows$delay_from_open))
})

test_that("the delay is also measured from when the cluster first appeared", {
  membership <- cluster_cases(
    # The cluster exists from 5 January holding three endemic cases, and
    # only becomes this outbreak's dossier a week later.
    list("2025-01-05", 1L, c("x1", "x2", "x3")),
    list("2025-01-12", 1L, c("a1", "a2", "a3"))
  )
  truth <- data.frame(
    outbreak_id = "A",
    label = "Test",
    pathogen = "Norovirus",
    institution_key = "H1",
    ward = "W1",
    first_day = as.Date("2025-01-01"),
    peak_day = as.Date("2025-01-02"),
    last_day = as.Date("2025-01-11"),
    n_cases = 4L,
    expected_channel = "same_place",
    expected_level = "pathogen_ward",
    stringsAsFactors = FALSE
  )
  rows <- episodic_validation_outbreak_rows(
    episodic_validation_overlap(membership, truth_cases),
    truth,
    truth_cases[truth_cases$outbreak_id == "A", ],
    as.Date(c("2025-01-05", "2025-01-12")),
    min_recall = 0.5,
    cluster_opened = stats::setNames(as.Date("2025-01-05"), "1")
  )

  expect_equal(rows$opened_run, as.Date("2025-01-05"))
  expect_equal(rows$delay_from_open, 4)
  # Something was on the board four days in; it was not mostly this
  # outbreak until eleven. Both are true, and they are what the two
  # columns are for.
  expect_equal(rows$detected_run, as.Date("2025-01-12"))
  expect_equal(rows$delay_from_first, 11)
})

test_that("an outbreak nothing found is censored, not dropped", {
  truth <- data.frame(
    outbreak_id = "A",
    label = "Test",
    pathogen = "Norovirus",
    institution_key = "H1",
    ward = "W1",
    first_day = as.Date("2025-01-01"),
    peak_day = as.Date("2025-01-02"),
    last_day = as.Date("2025-01-11"),
    n_cases = 4L,
    expected_channel = "same_place",
    expected_level = "pathogen_ward",
    stringsAsFactors = FALSE
  )
  own <- truth_cases[truth_cases$outbreak_id == "A", ]
  rows <- episodic_validation_outbreak_rows(
    episodic_validation_overlap(
      cluster_cases(list("2025-01-12", 1L, c("x1", "x2"))),
      truth_cases
    ),
    truth,
    own,
    as.Date(c("2025-01-05", "2025-01-12"))
  )

  expect_equal(nrow(rows), 1)
  expect_false(rows$detected)
  expect_true(is.na(rows$delay_from_first))
  expect_true(is.na(rows$detected_run))
  # Watched from its first case to its last: ten days, and not zero.
  expect_equal(rows$censor_days, 10)
})

test_that("an outbreak that began before the replay is not fully prospective", {
  truth <- data.frame(
    outbreak_id = "A",
    label = "Test",
    pathogen = "Norovirus",
    institution_key = "H1",
    ward = "W1",
    first_day = as.Date("2025-01-01"),
    peak_day = as.Date("2025-01-02"),
    last_day = as.Date("2025-01-11"),
    n_cases = 4L,
    expected_channel = "same_place",
    expected_level = "pathogen_ward",
    stringsAsFactors = FALSE
  )
  own <- truth_cases[truth_cases$outbreak_id == "A", ]
  rows <- episodic_validation_outbreak_rows(
    episodic_validation_overlap(
      cluster_cases(list("2025-01-12", 1L, c("a1", "a2", "a3"))),
      truth_cases
    ),
    truth,
    own,
    as.Date(c("2025-01-05", "2025-01-12"))
  )
  expect_false(rows$fully_prospective)
})

test_that("fragmentation counts the clusters an outbreak was split across", {
  membership <- cluster_cases(
    list("2025-01-12", 1L, c("a1", "a2")),
    list("2025-01-12", 2L, c("a3", "a4")),
    # A large endemic cluster that happens to hold one of its cases is
    # touched, not attributable.
    list("2025-01-12", 3L, c("a1", "x1", "x2", "x3", "x4"))
  )
  truth <- data.frame(
    outbreak_id = "A",
    label = "Test",
    pathogen = "Norovirus",
    institution_key = "H1",
    ward = "W1",
    first_day = as.Date("2025-01-01"),
    peak_day = as.Date("2025-01-02"),
    last_day = as.Date("2025-01-11"),
    n_cases = 4L,
    expected_channel = "same_place",
    expected_level = "pathogen_ward",
    stringsAsFactors = FALSE
  )
  rows <- episodic_validation_outbreak_rows(
    episodic_validation_overlap(membership, truth_cases),
    truth,
    truth_cases[truth_cases$outbreak_id == "A", ],
    as.Date("2025-01-12"),
    min_recall = 0.5,
    min_precision = 0.5
  )
  expect_equal(rows$n_clusters_attributable, 2L)
  expect_equal(rows$n_clusters_touching, 3L)
})

test_that("a cluster is a true positive on its own precision", {
  membership <- cluster_cases(
    list("2025-01-12", 1L, c("a1", "a2", "a3")),
    list("2025-01-12", 2L, c("a4", "x1", "x2", "x3")),
    list("2025-01-12", 3L, c("y1", "y2"))
  )
  clusters <- data.frame(
    cluster_id = 1:3,
    n_cases = c(3L, 4L, 2L),
    merged = FALSE,
    stringsAsFactors = FALSE
  )
  scored <- episodic_validation_cluster_rows(
    episodic_validation_overlap(membership, truth_cases),
    clusters,
    min_precision = 0.5
  )

  expect_equal(scored$true_positive, c(TRUE, FALSE, FALSE))
  expect_equal(scored$precision, c(1, 0.25, 0))
  expect_equal(scored$outbreak_id, c("A", "A", NA))
})

test_that("a merged cluster is not counted as an alarm", {
  clusters <- data.frame(
    cluster_id = 1:2,
    n_cases = c(3L, 3L),
    merged = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  scored <- episodic_validation_cluster_rows(
    episodic_validation_overlap(
      cluster_cases(list("2025-01-12", 1L, c("a1", "a2", "a3"))),
      truth_cases
    ),
    clusters
  )
  expect_equal(scored$counted, c(TRUE, FALSE))
})
