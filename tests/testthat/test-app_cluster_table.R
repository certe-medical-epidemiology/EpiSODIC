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

cluster_table_frame <- function() {
  data.frame(
    cluster_id = c(11L, 12L, 13L),
    n_cases = c(4L, 9L, 2L),
    case_days = c(3L, 6L, 2L),
    first_day = c("2025-01-02", "2025-02-01", "2025-03-01"),
    last_day = c("2025-01-06", "2025-03-20", "2025-03-20"),
    priority_score = c(50, 61.4, 88.6),
    stringsAsFactors = FALSE
  )
}

test_that("the duration is an inclusive day count, and NA where a date does not parse", {
  expect_equal(
    episodic_cluster_duration_days("2025-01-06", "2025-01-06"),
    1L
  )
  expect_equal(
    episodic_cluster_duration_days("2025-01-02", "2025-01-06"),
    5L
  )
  expect_equal(
    episodic_cluster_duration_days(
      c("2025-01-02", NA),
      c("2025-01-06", "2025-01-06")
    ),
    c(5L, NA_integer_)
  )
})

test_that("every cluster table is sorted on the last case day, descending", {
  clusters <- cluster_table_frame()
  ord <- episodic_cluster_table_order(clusters)
  # 12 and 13 share a last day, so priority breaks the tie: 13 (88.6)
  # leads 12 (61.4), and 11 (last seen in January) comes last.
  expect_equal(clusters$cluster_id[ord], c(13L, 12L, 11L))

  # a row with no parseable last day sorts last rather than anywhere
  clusters$last_day[1] <- NA_character_
  expect_equal(
    clusters$cluster_id[episodic_cluster_table_order(clusters)],
    c(13L, 12L, 11L)
  )

  # fully determined: the same data always renders in the same order
  shuffled <- clusters[c(3, 1, 2), ]
  expect_equal(
    shuffled$cluster_id[episodic_cluster_table_order(shuffled)],
    clusters$cluster_id[episodic_cluster_table_order(clusters)]
  )

  expect_equal(episodic_cluster_table_order(clusters[0, ]), integer(0))
})

test_that("the table carries the whole spine, in order, whatever the screen", {
  html <- as.character(episodic_ui_cluster_table(
    cluster_table_frame(),
    lang = "en"
  ))
  positions <- vapply(
    episodic_cluster_table_spine,
    function(key) {
      as.integer(regexpr(
        episodic_tr(paste0("column.", key), lang = "en"),
        html,
        fixed = TRUE
      ))
    },
    integer(1)
  )
  expect_false(any(positions < 0))
  expect_equal(positions, sort(positions))

  # and the rows themselves, sorted, with every value the spine promises
  expect_true(grepl("episodic-cell-id", html, fixed = TRUE))
  expect_lt(
    regexpr(episodic_tr("dossier.cluster_ref", id = 13L, lang = "en"), html),
    regexpr(episodic_tr("dossier.cluster_ref", id = 11L, lang = "en"), html)
  )
  # first case and last case as one range, not two separate date columns
  expect_true(grepl(
    episodic_format_date_range("2025-01-02", "2025-01-06", lang = "en"),
    html,
    fixed = TRUE
  ))
  expect_true(grepl("<td>89</td>", html, fixed = TRUE))
  expect_true(grepl("<td>5 days</td>", html, fixed = TRUE))
  expect_true(grepl("<td>6</td>", html, fixed = TRUE)) # cluster 12's case_days
})

test_that("context columns follow the id and outcome columns close the row", {
  clusters <- cluster_table_frame()
  clusters$place <- "Ward A"
  clusters$state_label <- "closed"
  html <- as.character(episodic_ui_cluster_table(
    clusters,
    context = list(episodic_ui_cluster_col_place(lang = "en")),
    outcome = list(episodic_ui_cluster_col(
      episodic_tr("column.state", lang = "en"),
      function(row) row$state_label
    )),
    lang = "en"
  ))
  expect_lt(
    regexpr(episodic_tr("column.cluster", lang = "en"), html, fixed = TRUE),
    regexpr(episodic_tr("column.place", lang = "en"), html, fixed = TRUE)
  )
  expect_lt(
    regexpr(episodic_tr("column.place", lang = "en"), html, fixed = TRUE),
    regexpr(episodic_tr("column.cases", lang = "en"), html, fixed = TRUE)
  )
  # outcome sits right after "case days", ahead of duration/priority - see
  # the file header on episodic_cluster_table_spine_outcome_after
  expect_lt(
    regexpr(episodic_tr("column.case_days", lang = "en"), html, fixed = TRUE),
    regexpr(episodic_tr("column.state", lang = "en"), html, fixed = TRUE)
  )
  expect_lt(
    regexpr(episodic_tr("column.state", lang = "en"), html, fixed = TRUE),
    regexpr(episodic_tr("column.priority", lang = "en"), html, fixed = TRUE)
  )
  expect_true(grepl("Ward A", html, fixed = TRUE))
})

test_that("a cluster table refuses to render a column it was not given", {
  clusters <- cluster_table_frame()
  expect_error(
    episodic_ui_cluster_table(clusters[, c("cluster_id", "n_cases")]),
    "priority_score"
  )
  expect_error(
    episodic_ui_cluster_table(
      clusters,
      context = list(list(label = "x", render = function(row) "y"))
    ),
    "episodic_ui_cluster_col"
  )
  expect_error(
    episodic_ui_cluster_col("x", "not a function"),
    "must be a function"
  )
})

test_that("a row opens its dossier, by click and by keyboard", {
  row <- as.character(episodic_ui_cluster_row(
    42L,
    shiny::tags$td("a cell"),
    lang = "en"
  ))
  # episodicOpenCluster() (see R/app_ui.R) both sets the `open_cluster`
  # Shiny input and moves the rail's own highlight - a row must call it
  # rather than setInputValue directly, or the rail goes stale on click.
  expect_true(grepl("episodicOpenCluster(42)", row, fixed = TRUE))
  expect_true(grepl("42", row, fixed = TRUE))
  expect_true(grepl("tabindex", row, fixed = TRUE))
  expect_true(grepl("onkeydown", row, fixed = TRUE))
  expect_true(grepl("episodic-id-link", row, fixed = TRUE))
  expect_true(grepl("a cell", row, fixed = TRUE))
  # the id cell comes first, before whatever cells the caller passed
  expect_lt(
    regexpr("episodic-cell-id", row, fixed = TRUE),
    regexpr("a cell", row, fixed = TRUE)
  )
})

test_that("a cluster that no longer stands on its own says so instead of dead-linking", {
  row <- as.character(episodic_ui_cluster_row(
    42L,
    shiny::tags$td("a cell"),
    unlinked_reason = "it was suppressed",
    lang = "en"
  ))
  # no click target at all, rather than one that goes nowhere
  expect_false(grepl("episodicOpenCluster", row, fixed = TRUE))
  expect_false(grepl("episodic-row-link", row, fixed = TRUE))
  expect_false(grepl("tabindex", row, fixed = TRUE))
  # the id is still there, marked, and hovering it explains why
  expect_true(grepl("episodic-id-unlinked", row, fixed = TRUE))
  expect_true(grepl("it was suppressed", row, fixed = TRUE))
  expect_true(grepl("42", row, fixed = TRUE))
})

test_that("the table takes its unlinked rows from the data frame's own column", {
  clusters <- cluster_table_frame()
  clusters$unlinked_reason <- c(NA_character_, "absorbed elsewhere", NA)
  html <- as.character(episodic_ui_cluster_table(clusters, lang = "en"))
  expect_true(grepl("episodic-id-unlinked", html, fixed = TRUE))
  expect_true(grepl("absorbed elsewhere", html, fixed = TRUE))
  # exactly one of the three rows is unlinked
  expect_equal(
    length(gregexpr("episodic-id-unlinked", html, fixed = TRUE)[[1]]),
    1
  )
  expect_equal(
    length(gregexpr("episodic-row-link", html, fixed = TRUE)[[1]]),
    2
  )
})

test_that("a missing priority or unparseable date renders as a dash, not as a number", {
  clusters <- cluster_table_frame()
  clusters$priority_score[1] <- NA_real_
  clusters$last_day[1] <- NA_character_
  html <- as.character(episodic_ui_cluster_table(clusters, lang = "en"))
  expect_true(grepl(episodic_tr("misc.dash", lang = "en"), html, fixed = TRUE))
})

test_that("the cluster table has no missing translations in any shipped language", {
  for (lang in c("en", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    clusters <- cluster_table_frame()
    clusters$unlinked_reason <- c(
      NA_character_,
      episodic_tr("cluster.unlinked.suppressed", ref = "#7", lang = lang),
      NA
    )
    clusters$place <- "Ward A"
    html <- as.character(episodic_ui_cluster_table(
      clusters,
      context = list(episodic_ui_cluster_col_place(lang = lang)),
      lang = lang
    ))
    expect_false(grepl("[[", html, fixed = TRUE), info = lang)
  }
})

test_that("a ?cluster= deep link is parsed, and anything else is ignored", {
  expect_equal(episodic_app_url_cluster_id("?cluster=123"), 123L)
  expect_equal(episodic_app_url_cluster_id("?view=archive&cluster=7"), 7L)
  expect_null(episodic_app_url_cluster_id(NULL))
  expect_null(episodic_app_url_cluster_id(""))
  expect_null(episodic_app_url_cluster_id("?cluster="))
  expect_null(episodic_app_url_cluster_id("?cluster=0"))
  expect_null(episodic_app_url_cluster_id("?cluster=-3"))
  expect_null(episodic_app_url_cluster_id("?cluster=12x"))
  expect_null(episodic_app_url_cluster_id("?cluster=abc"))
  expect_null(episodic_app_url_cluster_id("?other=1"))
})

test_that("a day value that does not parse yields NA rather than taking the screen down", {
  # as.Date() on a character vector errors on an unparseable value, so a
  # single malformed day would otherwise blow up the whole table.
  expect_true(is.na(episodic_cluster_duration_days("garbage", "2025-01-06")))
  expect_equal(
    episodic_cluster_duration_days(
      as.Date("2025-01-02"),
      as.Date("2025-01-06")
    ),
    5L
  )
  clusters <- data.frame(
    cluster_id = c(1L, 2L),
    n_cases = c(1L, 1L),
    case_days = c(1L, 1L),
    first_day = c("2025-01-01", "2025-03-01"),
    last_day = c("nonsense", "2025-03-20"),
    priority_score = c(90, 10),
    stringsAsFactors = FALSE
  )
  expect_equal(
    clusters$cluster_id[episodic_cluster_table_order(clusters)],
    c(2L, 1L)
  )
  expect_silent(episodic_ui_cluster_table(clusters, lang = "en"))
})

test_that("case_days counts distinct dates with a case, not the case count or the calendar span", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))

  run_id <- episodic_db_run_start(con, "test", "test", Sys.Date())
  stream_id <- episodic_db_stream_upsert(
    con,
    stream_key = episodic_stream_key(
      "pathogen_region",
      "Test pathogen",
      region_code = "NL"
    ),
    level = "pathogen_region",
    pathogen = "Test pathogen",
    region_code = "NL",
    observed_date = "2025-01-01"
  )
  cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = stream_id,
    first_day = "2025-01-01",
    last_day = "2025-01-10",
    n_cases = 4L,
    priority_score = 50,
    detector_agreement = 1L,
    run_id = run_id
  )
  # 4 cases, but only 2 distinct sample dates - a sharp two-day peak, not
  # a spread across the whole 10-day window this cluster runs.
  sample_dates <- c("2025-01-01", "2025-01-01", "2025-01-02", "2025-01-02")
  case_ids <- vapply(seq_along(sample_dates), function(i) {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_case
        (source_key, lab_number, patient_key, sample_date, pathogen, first_seen_run)
       VALUES (?, ?, ?, ?, ?, ?)",
      params = list(
        paste0("case-", i),
        paste0("lab-", i),
        paste0("pt-", i),
        sample_dates[i],
        "Test pathogen",
        run_id
      )
    )
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  }, integer(1))
  episodic_db_cluster_case_link_many(con, cluster_id, case_ids)

  batch <- episodic_db_case_days_batch(con, cluster_id)
  expect_equal(batch$case_days[batch$cluster_id == cluster_id], 2L)

  clusters <- data.frame(cluster_id = cluster_id, stringsAsFactors = FALSE)
  attached <- episodic_db_attach_case_days(con, clusters)
  expect_equal(attached$case_days, 2L)

  # a cluster id the batch query never saw (no linked cases at all) gets
  # 0, not NA - NA would print as a dash next to a cluster that plainly
  # has cases
  clusters2 <- data.frame(cluster_id = c(cluster_id, 999999L))
  attached2 <- episodic_db_attach_case_days(con, clusters2)
  expect_equal(attached2$case_days, c(2L, 0L))

  expect_equal(nrow(episodic_db_case_days_batch(con, integer(0))), 0)
})
