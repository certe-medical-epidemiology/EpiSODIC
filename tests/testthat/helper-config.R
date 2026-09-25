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
