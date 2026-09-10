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

test_that("episodic_report_suppress_small_counts() replaces 0 < n < threshold with '<threshold', leaves 0 and large counts alone", {
  df <- data.frame(label = c("a", "b", "c", "d"), n = c(0, 3, 12, NA))
  out <- episodic_report_suppress_small_counts(df, "n", threshold = 5)
  expect_equal(out$n, c("0", "<5", "12", NA))
})

test_that("episodic_report_suppress_small_counts() is a no-op when threshold is NULL or <= 1", {
  df <- data.frame(label = "a", n = 3)
  expect_equal(
    episodic_report_suppress_small_counts(df, "n", threshold = NULL),
    df
  )
  expect_equal(
    episodic_report_suppress_small_counts(df, "n", threshold = 1),
    df
  )
})

test_that("episodic_report_suppress_small_counts() handles a zero-row data frame", {
  df <- data.frame(label = character(0), n = integer(0))
  expect_equal(
    nrow(episodic_report_suppress_small_counts(df, "n", threshold = 5)),
    0
  )
})

test_that("episodic_quarto_available() is FALSE without the CLI, and episodic_report_render() gives a clear error", {
  skip_if(
    episodic_quarto_available(),
    "quarto CLI is actually available in this environment"
  )
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_error(
    episodic_report_render(env$con, env$cluster_id, output_dir = tempfile()),
    "Quarto CLI"
  )
})

test_that("episodic_report_qmd_path() uses the shipped template when nothing is configured", {
  expect_true(file.exists(episodic_report_qmd_path(NA)))
  expect_true(basename(episodic_report_qmd_path(NA)) == "episodic_default_report.qmd")
  expect_true(file.exists(episodic_report_qmd_path("")))
})

test_that("episodic_report_qmd_path() refuses a configured path that does not exist", {
  # Not a fallback: rendering the shipped template instead would send an
  # organisation's outbreak reports out under EpiSODIC's own layout
  # rather than theirs, which nobody notices until the report has left
  # the building.
  expect_error(
    episodic_report_qmd_path("/no/such/file.qmd"),
    "no file exists there"
  )
})

test_that("episodic_report_qmd_path() honours an operator-supplied path that actually exists", {
  custom <- tempfile(fileext = ".qmd")
  writeLines("---\ntitle: custom\n---\n", custom)
  expect_equal(episodic_report_qmd_path(custom), custom)
})

test_that("episodic_report_output_dir() derives a sibling directory from a SQLite db_path when unset", {
  db_path <- file.path(tempdir(), "episodic_test.sqlite")
  config <- list(report = list(output_dir = NULL))
  expect_equal(
    episodic_report_output_dir(config, db_path, "reports"),
    file.path(dirname(db_path), "reports")
  )
  config2 <- list(report = list(output_dir = NA))
  expect_equal(
    episodic_report_output_dir(config2, db_path, "config_exports"),
    file.path(dirname(db_path), "config_exports")
  )
})

test_that("episodic_report_output_dir() refuses a MariaDB DSN when report.output_dir is unset", {
  dsn <- "mysql://episodic:episodic@127.0.0.1:3306/episodic_test"
  config <- list(report = list(output_dir = NULL))
  expect_error(
    episodic_report_output_dir(config, dsn, "reports"),
    "report.output_dir",
    fixed = TRUE
  )
  # the DSN's own text, credentials included, must never reach a
  # filesystem path - so it must not even appear in the error message
  err <- tryCatch(
    episodic_report_output_dir(config, dsn, "reports"),
    error = function(e) e
  )
  expect_false(grepl("episodic:episodic", conditionMessage(err), fixed = TRUE))
})

test_that("episodic_report_output_dir() honours a configured output_dir for either dialect", {
  base <- tempfile("episodic_output_dir_")
  config <- list(report = list(output_dir = base))

  sqlite_dir <- episodic_report_output_dir(config, "/some/db.sqlite", "reports")
  expect_equal(sqlite_dir, file.path(base, "reports"))
  expect_true(dir.exists(sqlite_dir))

  dsn <- "mysql://episodic:episodic@127.0.0.1:3306/episodic_test"
  dsn_dir <- episodic_report_output_dir(config, dsn, "config_exports")
  expect_equal(dsn_dir, file.path(base, "config_exports"))
  expect_true(dir.exists(dsn_dir))

  # a resolved directory never contains the DSN's credentials, whichever
  # branch produced it
  expect_false(grepl("@", dsn_dir, fixed = TRUE))
  expect_false(grepl("://", dsn_dir, fixed = TRUE))
})

test_that("episodic_report_output_dir() refuses a configured output_dir that cannot be created", {
  # a file, not a directory, so dir.create() underneath it fails
  blocker <- tempfile("episodic_output_dir_blocker_")
  writeLines("not a directory", blocker)
  config <- list(report = list(output_dir = file.path(blocker, "nested")))
  expect_error(
    episodic_report_output_dir(config, "/some/db.sqlite", "reports"),
    "report.output_dir",
    fixed = TRUE
  )
})

test_that("a claimed version number is one above the highest already taken, gaps included", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_equal(episodic_db_report_version_next(env$con, env$cluster_id), 1L)

  # two prior renders including a gap, as episodic_db_report_render_insert()
  # would leave them after a render that failed between claim and insert
  episodic_db_report_render_insert(
    env$con,
    env$cluster_id,
    user_id = NA,
    file_path = "a.html",
    file_sha256 = strrep("a", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = 1
  )
  episodic_db_report_render_insert(
    env$con,
    env$cluster_id,
    user_id = NA,
    file_path = "b.html",
    file_sha256 = strrep("b", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = 3
  )
  # A render that was never claimed (an older database, brought forward)
  # still holds its number.
  expect_equal(episodic_db_report_version_next(env$con, env$cluster_id), 4L)
  expect_equal(episodic_db_report_version_claim(env$con, env$cluster_id), 4L)
  expect_equal(episodic_db_report_version_claim(env$con, env$cluster_id), 5L)
})

test_that("two renders of one cluster that overlap are handed different version numbers", {
  # The real ordering, without needing two processes: the second claim
  # happens while the first render is still going, i.e. before any
  # episodic_report_render row exists for it at all. Deriving the
  # version from episodic_report_render after the render is what gave
  # both of them the same number.
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  first <- episodic_db_report_version_claim(env$con, env$cluster_id)
  second <- episodic_db_report_version_claim(env$con, env$cluster_id)
  expect_equal(c(first, second), c(1L, 2L))

  # ... and each render then records its own row under its own number.
  episodic_db_report_render_insert(
    env$con,
    env$cluster_id,
    user_id = NA,
    file_path = sprintf("cluster-%d-v%d.html", env$cluster_id, second),
    file_sha256 = strrep("b", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = second
  )
  episodic_db_report_render_insert(
    env$con,
    env$cluster_id,
    user_id = NA,
    file_path = sprintf("cluster-%d-v%d.html", env$cluster_id, first),
    file_sha256 = strrep("a", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = first
  )
  reports <- episodic_db_reports_for_cluster(env$con, env$cluster_id)
  expect_equal(sort(reports$version_no), c(1L, 2L))
  expect_equal(length(unique(reports$file_path)), 2L)
})

test_that("the database refuses a second render row under a version number already used", {
  # The backstop behind the claim register: a regression that went back
  # to computing the version in R fails loudly here instead of quietly
  # recording a file_sha256 for bytes that have been overwritten.
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  episodic_db_report_render_insert(
    env$con,
    env$cluster_id,
    user_id = NA,
    file_path = "a.html",
    file_sha256 = strrep("a", 64),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = 1
  )
  expect_error(
    episodic_db_report_render_insert(
      env$con,
      env$cluster_id,
      user_id = NA,
      file_path = "a-again.html",
      file_sha256 = strrep("c", 64),
      params_json = "{}",
      case_ids_json = "[]",
      version_no = 1
    ),
    "UNIQUE"
  )
})

test_that("a claim that collides is retried, and anything else is raised as it is", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # Every attempt is refused: the claim gives up loudly rather than
  # rendering under a number it never got.
  local_mocked_bindings(
    episodic_db_report_version_next = function(con, cluster_id) 1L
  )
  expect_equal(episodic_db_report_version_claim(env$con, env$cluster_id), 1L)
  expect_error(
    episodic_db_report_version_claim(env$con, env$cluster_id, max_attempts = 3L),
    "after 3 attempts"
  )
})

test_that("a claim does not retry an error that is not a collision", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  local_mocked_bindings(
    episodic_db_report_version_next = function(con, cluster_id) {
      stop("the database is on fire", call. = FALSE)
    }
  )
  expect_error(
    episodic_db_report_version_claim(env$con, env$cluster_id),
    "on fire"
  )
})

test_that("episodic_report_latest_render() breaks a version tie on the later row", {
  # Nothing can write a tie any more, but a database migrated from
  # before the unique index may already hold one, and which of the two
  # is "the previous report" must not depend on row order.
  existing <- data.frame(
    report_id = c(7L, 9L, 3L),
    version_no = c(4L, 4L, 2L),
    rendered_at = c("2026-01-02", "2026-01-03", "2026-01-01"),
    stringsAsFactors = FALSE
  )
  expect_equal(episodic_report_latest_render(existing)$report_id, 9L)
  expect_null(episodic_report_latest_render(existing[0, ]))
  expect_null(episodic_report_latest_render(NULL))
  expect_error(
    episodic_report_latest_render(data.frame(version_no = 1L)),
    "report_id"
  )
})

test_that("episodic_report_snapshot() pulls the headline fields from a cluster object", {
  obj <- list(
    n_cases = 5, expected = 1.2, ratio = 4.1, priority_score = 62,
    last_day = "2026-01-10", pathogen = "irrelevant"
  )
  snap <- episodic_report_snapshot(obj)
  expect_equal(
    snap,
    list(n_cases = 5, expected = 1.2, ratio = 4.1, priority_score = 62, last_day = "2026-01-10")
  )
})

test_that("episodic_report_diff() is NULL for the first-ever render", {
  existing <- data.frame(
    report_id = integer(0), version_no = integer(0),
    rendered_at = character(0),
    params = character(0), case_ids = character(0)
  )
  snapshot <- list(n_cases = 1, expected = NA_real_, ratio = NA_real_, priority_score = 10, last_day = "2026-01-01")
  expect_null(episodic_report_diff(existing, snapshot, case_ids = 1L))
})

test_that("episodic_report_diff() is NULL when the previous render predates the snapshot feature", {
  existing <- data.frame(
    report_id = 1L,
    version_no = 1L,
    rendered_at = "2026-01-01T00:00:00Z",
    params = jsonlite::toJSON(list(cluster_id = 1L), auto_unbox = TRUE),
    case_ids = jsonlite::toJSON(c(1L, 2L)),
    stringsAsFactors = FALSE
  )
  snapshot <- list(n_cases = 3, expected = 1, ratio = 3, priority_score = 20, last_day = "2026-01-05")
  expect_null(episodic_report_diff(existing, snapshot, case_ids = c(1L, 2L, 3L)))
})

test_that("episodic_report_diff() computes case, priority, ratio and period deltas", {
  existing <- data.frame(
    report_id = 1L,
    version_no = 1L,
    rendered_at = "2026-09-01T07:00:00Z",
    params = jsonlite::toJSON(list(snapshot = list(
      n_cases = 3, expected = 1.2, ratio = 2.5, priority_score = 40, last_day = "2026-08-30"
    )), auto_unbox = TRUE, null = "null", na = "null"),
    case_ids = jsonlite::toJSON(c(1L, 2L, 3L)),
    stringsAsFactors = FALSE
  )
  snapshot <- list(n_cases = 5, expected = 1.5, ratio = 3.3, priority_score = 55, last_day = "2026-09-05")
  diff <- episodic_report_diff(existing, snapshot, case_ids = c(1L, 2L, 3L, 4L, 5L))

  expect_equal(diff$previous_version_no, 1L)
  expect_equal(diff$n_new_cases, 2L)
  expect_equal(diff$n_cases_delta, 2)
  expect_equal(diff$priority_score_delta, 15)
  expect_equal(diff$ratio_delta, 0.8)
  expect_true(diff$period_extended)
})

test_that("episodic_report_diff() reports zero new cases and no period extension when nothing changed", {
  existing <- data.frame(
    report_id = 1L,
    version_no = 2L,
    rendered_at = "2026-09-01T07:00:00Z",
    params = jsonlite::toJSON(list(snapshot = list(
      n_cases = 5, expected = 1.5, ratio = 3.3, priority_score = 55, last_day = "2026-09-05"
    )), auto_unbox = TRUE, null = "null", na = "null"),
    case_ids = jsonlite::toJSON(c(1L, 2L, 3L, 4L, 5L)),
    stringsAsFactors = FALSE
  )
  snapshot <- list(n_cases = 5, expected = 1.5, ratio = 3.3, priority_score = 55, last_day = "2026-09-05")
  diff <- episodic_report_diff(existing, snapshot, case_ids = c(1L, 2L, 3L, 4L, 5L))

  expect_equal(diff$n_new_cases, 0L)
  expect_equal(diff$n_cases_delta, 0)
  expect_equal(diff$priority_score_delta, 0)
  expect_false(diff$period_extended)
})

test_that("episodic_report_diff() handles a ratio that was or is NA without erroring", {
  existing <- data.frame(
    report_id = 1L,
    version_no = 1L,
    rendered_at = "2026-09-01T07:00:00Z",
    params = jsonlite::toJSON(list(snapshot = list(
      n_cases = 1, expected = NA_real_, ratio = NA_real_, priority_score = 10, last_day = "2026-09-01"
    )), auto_unbox = TRUE, null = "null", na = "null"),
    case_ids = jsonlite::toJSON(1L),
    stringsAsFactors = FALSE
  )
  snapshot <- list(n_cases = 2, expected = 1, ratio = 2, priority_score = 20, last_day = "2026-09-02")
  diff <- episodic_report_diff(existing, snapshot, case_ids = c(1L, 2L))
  expect_true(is.na(diff$ratio_delta))
})

test_that("episodic_report_render() stores a diffable snapshot that a later render can read back", {
  skip_if_not(
    episodic_test_can_render_report(),
    "needs the quarto CLI and an installed EpiSODIC (the template library()s it)"
  )
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  output_dir <- tempfile()

  first <- episodic_report_render(env$con, env$cluster_id, output_dir = output_dir)
  second <- episodic_report_render(env$con, env$cluster_id, output_dir = output_dir)
  expect_equal(second$version_no, first$version_no + 1L)

  reports <- episodic_db_reports_for_cluster(env$con, env$cluster_id)
  latest_params <- jsonlite::fromJSON(reports$params[reports$version_no == second$version_no])
  expect_false(is.null(latest_params$snapshot))
  expect_false(is.null(latest_params$diff))
  expect_equal(latest_params$diff$previous_version_no, first$version_no)
})
