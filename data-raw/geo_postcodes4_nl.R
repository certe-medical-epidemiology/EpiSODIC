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

# Provenance script for inst/extdata/geo_postcodes4_nl.rds - not run at
# install or check time (data-raw/ is .Rbuildignore'd), kept only so the
# shipped default can be regenerated or audited.
#
# Source: certe-medical-epidemiology/certegis's `geo_postcodes4` dataset
# (data/geo_postcodes4.rda), GPL-2 licensed, same licence as this package.
# certegis itself is no longer a dependency of EpiSODIC: the geography
# panel's data contract (R/geo_data.R) is deliberately generic - an sf
# object with `pc`/`geometry` columns - so any operator, anywhere, can
# point EPISODIC_GEO_DATA at their own equivalent file. `pc` matches
# `episode_case.pc`'s own generic values (postcodes, zip codes,
# municipality codes, anything); it is not itself Dutch-postcode-specific
# despite the shipped default being Dutch four-digit postcodes.
# This script only produces that shipped Netherlands default, trimmed to
# the two columns the contract actually needs (certegis's own copy also
# carries population and area, which this package has no use for and
# should not redistribute beyond what the choropleth needs).
#
# Regenerate by cloning certe-medical-epidemiology/certegis and running:
#
geo <- certegis::geo_postcodes4
geo <- geo[, c("postcode", "geometry"), drop = FALSE]
names(geo)[names(geo) == "postcode"] <- "pc"
geo$pc <- as.character(geo$pc)
saveRDS(geo, "inst/extdata/geo_postcodes4_nl.rds", compress = "xz")
