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

# The suite, the check and the formatter. See README.md in this folder.
# Never reached from the package: data-raw is in .Rbuildignore, and this
# takes minutes.
#
# Exits 0 if everything passed and 1 if anything did not. That exit code
# is the contract with whatever schedules it - a timer needs to know pass
# or fail without reading the log.

options(warn = 1)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# The package root, whether this was started from it or from this
# folder. `--file=` is how Rscript names the script it is running; there
# is no `sys.frame()$ofile` outside `source()`.
episodic_script_dir <- function() {
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", arg))))
  }
  getwd()
}
root <- normalizePath(file.path(episodic_script_dir(), "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(getwd())
}
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run this from the package root, or from data-raw/verification/.")
}
setwd(root)

out_dir <- file.path(root, "data-raw", "verification", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (pkg in c("devtools", "styler")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is needed. See README.md in this folder.")
  }
}

results <- list()
record <- function(name, ok, detail = "") {
  results[[length(results) + 1L]] <<- list(
    name = name,
    ok = isTRUE(ok),
    detail = detail
  )
  message(sprintf("[%s] %s%s",
    if (isTRUE(ok)) "pass" else "FAIL",
    name,
    if (nzchar(detail)) paste0(" - ", detail) else ""
  ))
}

# --------------------------------------------------------------------- #
# 1. The suite                                                          #
# --------------------------------------------------------------------- #

message("\n== devtools::test() ==")
test_results <- tryCatch(
  as.data.frame(devtools::test(stop_on_failure = FALSE)),
  error = function(e) {
    record("suite", FALSE, conditionMessage(e))
    NULL
  }
)
if (!is.null(test_results)) {
  # Warnings count. A warning in this suite is a deprecation or a
  # coercion nobody has looked at yet, and the first run where it turns
  # into an error is a run in production.
  failed <- sum(test_results$failed) + sum(test_results$error)
  warned <- sum(test_results$warning)
  record(
    "suite",
    failed == 0 && warned == 0,
    sprintf(
      "%d failed, %d errored, %d warned, %d passed",
      sum(test_results$failed),
      sum(test_results$error),
      warned,
      sum(test_results$passed)
    )
  )
  utils::write.csv(
    test_results,
    file.path(out_dir, "test-results.csv"),
    row.names = FALSE
  )
}

# --------------------------------------------------------------------- #
# 2. R CMD check                                                        #
# --------------------------------------------------------------------- #

message("\n== R CMD build && R CMD check --as-cran ==")
check <- tryCatch(
  devtools::check(
    document = FALSE,
    args = c("--as-cran", "--no-manual"),
    error_on = "never",
    quiet = FALSE,
    check_dir = out_dir
  ),
  error = function(e) {
    record("R CMD check", FALSE, conditionMessage(e))
    NULL
  }
)
if (!is.null(check)) {
  # One NOTE is expected: the package uses Authors@R, which R CMD check
  # reports on unless the tarball was built first. It is not a problem
  # and never has been - see CLAUDE.md - so it is named here rather than
  # allowed by counting.
  notes <- check$notes
  expected <- grepl("Author|Maintainer", notes)
  unexpected <- notes[!expected]
  record(
    "R CMD check",
    length(check$errors) == 0 &&
      length(check$warnings) == 0 &&
      length(unexpected) == 0,
    sprintf(
      "%d errors, %d warnings, %d unexpected notes",
      length(check$errors),
      length(check$warnings),
      length(unexpected)
    )
  )
  writeLines(
    c(check$errors, check$warnings, notes),
    file.path(out_dir, "check-output.txt")
  )
}

# --------------------------------------------------------------------- #
# 3. Formatting                                                         #
# --------------------------------------------------------------------- #

message("\n== styler::style_pkg(dry = 'on') ==")
styled <- tryCatch(
  styler::style_pkg(dry = "on"),
  error = function(e) {
    record("formatting", FALSE, conditionMessage(e))
    NULL
  }
)
if (!is.null(styled)) {
  changed <- styled$file[styled$changed]
  record(
    "formatting",
    length(changed) == 0,
    if (length(changed) == 0) {
      "no file would change"
    } else {
      paste(length(changed), "file(s) would change:", paste(changed, collapse = ", "))
    }
  )
}

# --------------------------------------------------------------------- #
# 4. Static assertions R CMD check does not make                        #
# --------------------------------------------------------------------- #

message("\n== project rules ==")

r_files <- list.files("R", pattern = "[.]R$", full.names = TRUE)

# Every R file carries the Certe banner.
missing_banner <- Filter(
  function(f) !grepl("An R package by Certe", readLines(f, n = 3)[2] %||% ""),
  r_files
)
record(
  "GPL banner on every R file",
  length(missing_banner) == 0,
  paste(basename(missing_banner), collapse = ", ")
)

# The stylesheet mirrors for Arabic. A physical left/right property is a
# component that stays put while everything around it moves.
css <- paste(readLines("inst/app/www/episodic.css", warn = FALSE), collapse = "\n")
# `(?s)` so the lazy match crosses the newlines a block comment spans.
declarations <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
physical <- unlist(regmatches(
  declarations,
  gregexpr("(margin|padding|border)-(left|right)\\s*:", declarations)
))
record(
  "no physical left/right properties in the stylesheet",
  length(physical) == 0,
  paste(unique(physical), collapse = ", ")
)

# Every exported topic is in the pkgdown index, or the site build fails
# with "topics missing from index".
if (requireNamespace("pkgdown", quietly = TRUE)) {
  indexed <- tryCatch(
    {
      yml <- yaml::read_yaml("_pkgdown.yml")
      unlist(lapply(yml$reference, function(s) s$contents))
    },
    error = function(e) character(0)
  )
  exported <- tryCatch(
    {
      ns <- readLines("NAMESPACE", warn = FALSE)
      gsub("^export\\((.*)\\)$", "\\1", ns[grepl("^export\\(", ns)])
    },
    error = function(e) character(0)
  )
  missing_topics <- setdiff(exported, gsub("[`\"]", "", indexed))
  record(
    "every exported topic is in _pkgdown.yml",
    length(missing_topics) == 0,
    paste(missing_topics, collapse = ", ")
  )
}

# --------------------------------------------------------------------- #

message("\n== summary ==")
for (r in results) {
  message(sprintf("  %-4s %s", if (r$ok) "ok" else "FAIL", r$name))
}
failed <- Filter(function(r) !r$ok, results)
if (length(failed) > 0) {
  message(sprintf("\n%d check(s) failed.", length(failed)))
  quit(status = 1L, save = "no")
}
message("\nAll checks passed.")
quit(status = 0L, save = "no")
