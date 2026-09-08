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
# EpiSODIC ships with `access.require_login: true`, which is the right
# default for something deployed at laboratories worldwide but the wrong
# one for a test suite: nearly every server test here reads a rendered
# output *before* signing in, and behind the login wall the server
# deliberately renders nothing at all. Rather than sprinkle a sign-in
# through tests that are not about signing in, the suite pins the wall
# down here and tests it deliberately where it belongs
# (test-app_require_login.R, which sets both values explicitly and saves
# and restores whatever it finds, so it composes with this).
#
# It also pins the geography, so the region and area codes the lattice
# tests assert on are the ones this file names rather than whatever the
# shipped defaults happen to say.
#
# Set here, in a helper, so it is in force for every test file:
# testthat sources helper-*.R before any test-*.R.
episodic_test_config_path <- local({
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
  Sys.setenv(EPISODIC_CONFIG = path)
  path
})

# The region and area codes the suite's own configuration produces, so a
# test asserts against the setting rather than against a literal that
# would have to be edited in two places.
episodic_test_region_code <- function() {
  episodic_geography_config()$region_code
}

episodic_test_area_code <- function(pc) {
  geography <- episodic_geography_config()
  paste0(
    geography$area_code_prefix,
    substr(pc, 1, geography$area_pc_characters)
  )
}
