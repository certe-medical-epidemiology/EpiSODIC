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

# The demo runs on synthetic northern-Netherlands data, and its geography
# (a catchment code, three province codes, Dutch municipalities and a
# Dutch postcode map) is supplied the way any instance supplies its own:
# through configuration written by `episodic_demo()`. Nothing outside the
# demo and the synthetic generator may name it, or an instance anywhere
# else carries Dutch places on its streams, dashboard and reports.

# Every string constant in an expression, nested functions included.
string_constants <- function(expr) {
  if (is.character(expr)) {
    expr
  } else if (is.call(expr) || is.expression(expr) || is.pairlist(expr)) {
    unlist(lapply(as.list(expr), string_constants))
  } else if (is.function(expr)) {
    c(string_constants(formals(expr)), string_constants(body(expr)))
  } else {
    character(0)
  }
}

demo_geography <- paste0(
  "NORTHERN_NETHERLANDS|PROV_(DRENTHE|FRYSLAN|GRONINGEN)|",
  "geo_postcodes4_nl"
)

test_that("only the demo and the synthetic generator name the demo's geography", {
  ns <- asNamespace("EpiSODIC")
  owners <- "^episodic_(demo|synthetic)|^episodic_geo_source_default"
  offenders <- character(0)
  names <- grep(owners, ls(ns, all.names = TRUE), value = TRUE, invert = TRUE)
  for (name in names) {
    obj <- get(name, envir = ns)
    if (!is.function(obj)) {
      next
    }
    hits <- grep(demo_geography, string_constants(obj), value = TRUE)
    if (length(hits) > 0) {
      offenders <- c(offenders, sprintf("%s(): %s", name, hits[1]))
    }
  }
  expect_equal(offenders, character(0))

  # ...and the demo still does, so the walk cannot pass by having
  # nothing to find.
  expect_true(any(grepl(
    demo_geography,
    string_constants(ns$episodic_demo_config_yaml)
  )))
})

test_that("the shipped configuration names no place", {
  config <- yaml::read_yaml(system.file(
    "config", "episodic_default_config.yaml",
    package = "EpiSODIC"
  ))
  expect_identical(config$geography$region_code, "REGION")
  shipped <- readLines(system.file(
    "config", "episodic_default_config.yaml",
    package = "EpiSODIC"
  ))
  expect_false(any(grepl(demo_geography, shipped)))
})
