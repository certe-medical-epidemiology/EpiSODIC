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

config_write_file <- function(lines) {
  path <- tempfile(fileext = ".yaml")
  writeLines(lines, path)
  withr::defer(unlink(c(path, paste0(path, ".lock"))), envir = parent.frame())
  path
}

operator_config <- c(
  "# Operator configuration",
  "same_place:",
  "  # Tighter than the default for this catchment",
  "  default_k_days: 7   # agreed with infection prevention",
  "  overrides:",
  "    MRSA: {n_cases: 2, k_days: 14}",
  "",
  "# Reports",
  "report:",
  "  small_count_threshold: 5",
  "notifications: ~"
)

test_that("setting one key leaves every other line as the operator wrote it", {
  path <- config_write_file(operator_config)
  episodic_config_set(c("same_place", "default_k_days"), 10, path)
  lines <- readLines(path)
  expect_identical(lines[-4], operator_config[-4])
  expect_identical(lines[4], "  default_k_days: 10   # agreed with infection prevention")
  expect_identical(episodic_config_resolve(path)$same_place$default_k_days, 10L)
})

test_that("a key missing from its section is added at the end of that section", {
  path <- config_write_file(operator_config)
  episodic_config_set(c("report", "output_dir"), "/srv/reports", path)
  lines <- readLines(path)
  expect_identical(lines[10:11], c("  small_count_threshold: 5", "  output_dir: /srv/reports"))
  expect_identical(lines[12], "notifications: ~")
})

test_that("a missing section is appended, and a null section becomes a mapping", {
  path <- config_write_file(operator_config)
  episodic_config_set(c("plugins", "enabled", "share-regional"), list(permissions = list("cluster_summary")), path)
  episodic_config_set(c("notifications", "enabled"), TRUE, path)
  config <- yaml::read_yaml(path)
  expect_identical(config$plugins$enabled$`share-regional`$permissions, "cluster_summary")
  expect_true(config$notifications$enabled)
  expect_identical(readLines(path)[1:10], operator_config[1:10])
  expect_true(any(readLines(path) == "  enabled: true"))
})

test_that("a whole section is replaced, and ~, lists and booleans read back as written", {
  path <- config_write_file(operator_config)
  value <- list(enabled = FALSE, dashboard_url = NULL, channels = list(ntfy = list(enabled = TRUE, topic = "yes")))
  episodic_config_set("notifications", value, path)
  config <- yaml::read_yaml(path)
  expect_false(config$notifications$enabled)
  expect_true("dashboard_url" %in% names(config$notifications))
  expect_null(config$notifications$dashboard_url)
  expect_identical(config$notifications$channels$ntfy$topic, "yes")
  expect_false(any(grepl(": (yes|no)$", readLines(path))))
})

test_that("a key inside a flow mapping is set by rewriting that mapping from its parse", {
  path <- config_write_file(operator_config)
  episodic_config_set(c("same_place", "overrides", "MRSA", "k_days"), 7, path)
  config <- yaml::read_yaml(path)
  expect_identical(config$same_place$overrides$MRSA, list(n_cases = 2L, k_days = 7L))
  expect_identical(readLines(path)[1:5], operator_config[1:5])
})

test_that("a value is refused unless the edited file reads back as the original with that one key changed", {
  path <- config_write_file(c("same_place: &sp", "  default_k_days: 7", "farrington: *sp"))
  before <- readLines(path)
  expect_error(episodic_config_set(c("same_place", "default_k_days"), 9, path), "cannot follow safely")
  expect_identical(readLines(path), before)
})

test_that("a setting that fails validation is not written", {
  path <- config_write_file(operator_config)
  expect_error(episodic_config_set(c("same_place", "no_such_key"), 1, path), "not a configuration key")
  expect_identical(readLines(path), operator_config)
})

test_that("values YAML cannot carry back are refused before the file is touched", {
  path <- config_write_file(operator_config)
  expect_error(episodic_config_set(c("report", "output_dir"), NA_character_, path), "missing value")
  expect_error(episodic_config_set(c("report", "small_count_threshold"), Inf, path), "infinite")
  expect_error(episodic_config_set(character(0), 1, path), "must name")
  expect_identical(readLines(path), operator_config)
})

test_that("without a writable configuration file nothing is written and the reason is given", {
  expect_error(episodic_config_set("notifications", NULL, NA), "EPISODIC_CONFIG is not set")
  expect_error(episodic_config_set("notifications", NULL, tempfile(fileext = ".yaml")), "no file exists there")
  skip_on_os("windows")
  path <- config_write_file(operator_config)
  Sys.chmod(path, "0444")
  skip_if(file.access(path, mode = 2) == 0, "running as an account that can write read-only files")
  expect_error(episodic_config_set("notifications", NULL, path), "not writable")
})

test_that("a held lock stops the write rather than racing it", {
  path <- config_write_file(operator_config)
  dir.create(paste0(path, ".lock"))
  expect_error(
    episodic_config_lock(path, wait_seconds = 0.2),
    "being written by another process"
  )
  expect_identical(readLines(path), operator_config)
})

test_that("Windows line endings and the file's permissions are kept", {
  path <- config_write_file(character(0))
  writeBin(charToRaw("report:\r\n  small_count_threshold: 5\r\n"), path)
  skip_on_os("windows")
  Sys.chmod(path, "0600")
  episodic_config_set(c("report", "small_count_threshold"), 3, path)
  text <- rawToChar(readBin(path, "raw", 1000))
  expect_identical(text, "report:\r\n  small_count_threshold: 3\r\n")
  expect_identical(format(file.info(path)$mode), "600")
})

test_that("an empty configuration file takes its first setting", {
  path <- config_write_file(character(0))
  episodic_config_set(c("report", "output_dir"), "/srv/reports", path)
  expect_identical(yaml::read_yaml(path), list(report = list(output_dir = "/srv/reports")))
})
