#' Geographic reference data for the choropleth panel
#'
#' `certegis` was dropped as this feature's dependency: it is Certe's own
#' package, useful only to a Dutch operator, and there is no reason
#' geography should be Netherlands-only when the rest of EpiSODIC is not
#' (`pathogen`, institution types, and every other domain concept in this
#' codebase are already operator-defined, unconstrained values -
#' `QUESTIONS.md` item 22). This is the same shape of solution the
#' package already uses elsewhere for optional, instance-specific data:
#' a shipped default (here, geometry for the Netherlands' PC4 postcodes,
#' `data-raw/geo_postcodes4_nl.R` documents its provenance) that any
#' operator can override by pointing `EPISODE_GEO_DATA` at their own file
#' - one environment variable, the same pattern `EPISODE_CONFIG` and
#' `EPISODE_PALETTE_CONFIG` already establish.
#'
#' The contract is minimal and country-agnostic: an `sf` object with a
#' `pc` column (matching whatever an operator's own `episode_case.pc4`
#' values are - postcodes, zip codes, municipality codes, anything; this
#' package never validates or interprets that column beyond joining it,
#' and it is deliberately named `pc`, not `pc4`, so nothing about the
#' contract implies a 4-digit Dutch postcode) and a `geometry` column.
#' `sf`/GDAL/GEOS/PROJ are a real system-level dependency beyond what
#' CRAN alone can supply, so this whole feature is guarded end to end: no
#' `sf` installed means the geography panel falls back to the existing PC
#' bar breakdown, exactly as before this existed.
#'
#' A second, entirely independent piece of geographic data is supported
#' on top of this: `EPISODE_GEO_DATA_OVERLAY`
#' ([episode_geo_overlay_resolve()]), an outline layer (region boundaries
#' - provinces, municipalities, whatever an operator wants for
#' orientation) drawn with colour but no fill on top of the choropleth.
#' It has no `pc` contract at all, since it carries no case counts to
#' join.
#' @name geo_data
NULL

#' Resolve the optional region-outline overlay
#'
#' A second, independent geographic layer, drawn as outlines (no fill,
#' a thicker line) on top of the PC choropleth - for boundaries an
#' operator wants visible for orientation (provinces, municipalities,
#' catchment areas) but that carry no case counts of their own, so
#' `episode_geo_join()`'s `pc`-keyed contract does not apply here: the
#' overlay needs nothing but a `geometry` column. Unlike
#' [episode_geo_source_resolve()], there is no shipped default -
#' region boundaries are far more jurisdiction-specific than postcode
#' geometry, and guessing at a "sensible default" (which country's
#' provinces?) would be arbitrary in a way the PC4 default is not
#' (`EPISODE_GEO_DATA` is Netherlands-only *labelled as such*, not
#' pretending to be universal). No `EPISODE_GEO_DATA_OVERLAY` set (or an
#' invalid file) simply means no overlay layer, same as no `sf` at all.
#'
#' @param path Path to an `.rds` file holding an `sf` object with a
#'   `geometry` column. Defaults to the `EPISODE_GEO_DATA_OVERLAY`
#'   environment variable.
#' @return An `sf` object, or `NULL` if `sf` is not installed, the
#'   variable is unset, or the file is missing/invalid.
#' @examples
#' # NULL when unset (or when the sf package is not installed)
#' episode_geo_overlay_resolve(path = NA)
#' @export
episode_geo_overlay_resolve <- function(path = Sys.getenv("EPISODE_GEO_DATA_OVERLAY", unset = NA)) {
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)

  overlay <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(overlay) || !"geometry" %in% names(overlay)) return(NULL)
  overlay
}

#' Resolve the geographic reference dataset to use
#'
#' @param path Path to an `.rds` file holding an `sf` object with `pc`
#'   and `geometry` columns. Defaults to the `EPISODE_GEO_DATA`
#'   environment variable; if unset (or the file does not exist), falls
#'   back to the shipped Netherlands PC4 default.
#' @return An `sf` object, or `NULL` if `sf` is not installed.
#' @examples
#' # falls back to the shipped Netherlands PC4 default when sf is
#' # installed, or NULL when it is not
#' geo <- episode_geo_source_resolve(path = NA)
#' @export
episode_geo_source_resolve <- function(path = Sys.getenv("EPISODE_GEO_DATA", unset = NA)) {
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)

  if (!is.na(path) && nzchar(path) && file.exists(path)) {
    geo <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(geo) && all(c("pc", "geometry") %in% names(geo))) return(geo)
  }
  episode_geo_source_default()
}

#' The shipped Netherlands PC4 default
#'
#' @return An `sf` object with `pc`, `geometry`, or `NULL` if `sf` is not
#'   installed.
#' @keywords internal
#' @noRd
episode_geo_source_default <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
  path <- system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODIC")
  if (identical(path, "")) path <- file.path("inst", "extdata", "geo_postcodes4_nl.rds")
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

#' Join case counts onto geographic reference data
#'
#' @param rows A data frame with `label` (PC-equivalent codes) and `n`
#'   (case counts), from `episode_app_concentration()$rows`.
#' @param geo_data An `sf` object with `pc`/`geometry`, or `NULL` to
#'   resolve one via [episode_geo_source_resolve()].
#' @return An `sf` object (a right join: every `geo_data` row is kept,
#'   `n` is `NA` where `rows` has no matching `pc`), or `NULL` if no
#'   geographic data is available at all.
#' @keywords internal
#' @noRd
episode_geo_join <- function(rows, geo_data = NULL) {
  geo_data <- geo_data %||% episode_geo_source_resolve()
  if (is.null(geo_data) || nrow(rows) == 0) return(NULL)

  counts <- data.frame(pc = as.character(rows$label), n = rows$n, stringsAsFactors = FALSE)
  geo_data$pc <- as.character(geo_data$pc)
  merged <- merge(geo_data, counts, by = "pc", all.x = TRUE)
  sf::st_as_sf(merged)
}
