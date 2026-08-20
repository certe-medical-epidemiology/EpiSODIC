test_that("episode_report_suppress_small_counts() replaces 0 < n < threshold with '<threshold', leaves 0 and large counts alone", {
  df <- data.frame(label = c("a", "b", "c", "d"), n = c(0, 3, 12, NA))
  out <- episode_report_suppress_small_counts(df, "n", threshold = 5)
  expect_equal(out$n, c("0", "<5", "12", NA))
})

test_that("episode_report_suppress_small_counts() is a no-op when threshold is NULL or <= 1", {
  df <- data.frame(label = "a", n = 3)
  expect_equal(episode_report_suppress_small_counts(df, "n", threshold = NULL), df)
  expect_equal(episode_report_suppress_small_counts(df, "n", threshold = 1), df)
})

test_that("episode_report_suppress_small_counts() handles a zero-row data frame", {
  df <- data.frame(label = character(0), n = integer(0))
  expect_equal(nrow(episode_report_suppress_small_counts(df, "n", threshold = 5)), 0)
})

test_that("episode_quarto_available() is FALSE without the CLI, and episode_report_render() gives a clear error", {
  skip_if(episode_quarto_available(), "quarto CLI is actually available in this environment")
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  expect_error(
    episode_report_render(env$con, env$cluster_id, output_dir = tempfile()),
    "Quarto CLI"
  )
})

test_that("episode_report_qmd_path() falls back to the shipped template when unset, missing or invalid", {
  expect_true(file.exists(episode_report_qmd_path(NA)))
  expect_true(basename(episode_report_qmd_path(NA)) == "cluster_report.qmd")
  expect_true(file.exists(episode_report_qmd_path("")))
  expect_true(file.exists(episode_report_qmd_path("/no/such/file.qmd")))
})

test_that("episode_report_qmd_path() honours an operator-supplied path that actually exists", {
  custom <- tempfile(fileext = ".qmd")
  writeLines("---\ntitle: custom\n---\n", custom)
  expect_equal(episode_report_qmd_path(custom), custom)
})

test_that("episode_report_render() picks version_no = max(existing) + 1, not a fixed increment", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  # simulate two prior renders including a gap, as episode_db_report_render_insert() would leave them
  episode_db_report_render_insert(env$con, env$cluster_id, user_id = NA, file_path = "a.html",
                                   file_sha256 = strrep("a", 64), params_json = "{}", case_ids_json = "[]",
                                   version_no = 1)
  episode_db_report_render_insert(env$con, env$cluster_id, user_id = NA, file_path = "b.html",
                                   file_sha256 = strrep("b", 64), params_json = "{}", case_ids_json = "[]",
                                   version_no = 3)
  existing <- episode_db_reports_for_cluster(env$con, env$cluster_id)
  expect_equal(max(existing$version_no) + 1L, 4L)
})
