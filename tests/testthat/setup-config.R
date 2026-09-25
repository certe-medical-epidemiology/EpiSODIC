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

# The suite's own instance configuration.
#
# Pinned explicitly rather than left to the shipped default, because
# nearly every server test here reads a rendered output *before* signing
# in, and behind the login wall the server deliberately renders nothing
# at all - a test suite that inherited whatever `access.require_login`
# ships as would break the moment that default changed. Rather than
# sprinkle a sign-in through tests that are not about signing in, the
# suite pins the wall down here and tests it deliberately where it
# belongs (test-app_require_login.R, which sets both values explicitly
# and saves and restores whatever it finds, so it composes with this).
#
# It also pins the geography, so the region and area codes the lattice
# tests assert on are the ones this file names rather than whatever the
# shipped defaults happen to say.
#
# Set here, in a setup file, rather than in a helper: testthat runs
# setup-*.R before any test file and undoes what it set through
# `teardown_env()` once the run ends, while `devtools::load_all()`
# sources every helper-*.R into the interactive session as well. A
# helper setting the variable would replace an operator's own
# EPISODIC_CONFIG in their console, and every dashboard or detection
# run started from that session would then read the suite's
# configuration - its geography and its open access - instead of theirs.
local({
  path <- tempfile("episodic-test-config-", fileext = ".yaml")
  writeLines(
    c(
      "access:",
      "  require_login: false",
      "geography:",
      "  region_code: TEST_REGION",
      "  area_code_prefix: \"AREA-\"",
      "  area_pc_characters: 2"
    ),
    path
  )
  withr::local_envvar(
    EPISODIC_CONFIG = path,
    .local_envir = testthat::teardown_env()
  )
  withr::defer(unlink(path), envir = testthat::teardown_env())
})
