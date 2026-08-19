episode_test_r_source_dir <- function() {
  # An installed package's own system.file("R", ...) is a real directory
  # but holds only the compiled lazy-load database (EpiSODE.rdb/.rdx), not
  # individual .R source files - so existence alone is not enough to
  # trust a candidate; each must actually contain recognisable sources.
  candidates <- c(system.file("R", package = "EpiSODE"), "R", "../../R")
  for (d in candidates) {
    if (nzchar(d) && dir.exists(d) && file.exists(file.path(d, "db_app_write.R"))) return(d)
  }
  NA_character_
}

test_that("the app's write surface issues no UPDATE or DELETE SQL statements", {
  # MILESTONES.md M3's own "done when" line: "Verify by inspection that the
  # app issues no UPDATE or DELETE statements at all." Scoped to the files
  # a live Shiny session can actually reach (everything prefixed app_,
  # auth.R, run_app.R) - NOT R/db_cron_write.R or the cron pipeline, which
  # legitimately UPDATEs the facts it owns (ARCHITECTURE.md section 5.0:
  # "the cron owns the facts, the app owns the judgements"; only the app
  # side of that split is insert-only).
  r_dir <- episode_test_r_source_dir()
  skip_if(is.na(r_dir), "R/ source directory not found (e.g. checking an installed, non-source package)")

  app_files <- list.files(r_dir, pattern = "^(app_|auth\\.R$|run_app\\.R$)", full.names = TRUE)
  expect_true(length(app_files) >= 10)  # sanity: the glob actually matched something

  offenders <- character(0)
  for (f in app_files) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep("\\bUPDATE\\s+episode_|\\bDELETE\\s+FROM\\s+episode_", lines, ignore.case = TRUE, perl = TRUE)
    if (length(hits) > 0) {
      offenders <- c(offenders, sprintf("%s:%d: %s", basename(f), hits, trimws(lines[hits])))
    }
  }
  expect_equal(offenders, character(0))
})

test_that("db_app_write.R specifically (the app's single-table writers) contains only INSERTs", {
  r_dir <- episode_test_r_source_dir()
  skip_if(is.na(r_dir), "R/ source directory not found (e.g. checking an installed, non-source package)")

  lines <- readLines(file.path(r_dir, "db_app_write.R"), warn = FALSE)
  code_lines <- lines[!grepl("^\\s*#", lines)]  # drop roxygen/comment lines
  sql_lines <- grep("dbExecute|INSERT|UPDATE|DELETE", code_lines, value = TRUE, ignore.case = TRUE)
  expect_true(length(sql_lines) > 0)  # sanity
  expect_false(any(grepl("\\bUPDATE\\b|\\bDELETE\\b", sql_lines, ignore.case = TRUE)))
})
