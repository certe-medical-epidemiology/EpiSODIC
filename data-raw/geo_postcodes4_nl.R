# Provenance script for inst/extdata/geo_postcodes4_nl.rds - not run at
# install or check time (data-raw/ is .Rbuildignore'd), kept only so the
# shipped default can be regenerated or audited.
#
# Source: certe-medical-epidemiology/certegis's `geo_postcodes4` dataset
# (data/geo_postcodes4.rda), GPL-2 licensed, same licence as this package.
# certegis itself is no longer a dependency of EpiSODE (QUESTIONS.md): the
# geography panel's data contract (R/geo_data.R) is deliberately generic -
# an sf object with `pc`/`geometry` columns - so any operator, anywhere,
# can point EPISODE_GEO_DATA at their own equivalent file. `pc` matches
# `episode_case.pc4`'s own generic values (postcodes, zip codes,
# municipality codes, anything - see QUESTIONS.md); it is not itself
# Dutch-postcode-specific despite the shipped default being Dutch PC4s.
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
