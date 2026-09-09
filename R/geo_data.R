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

#' Show Clusters on a Map
#'
#' The dashboard can plot cluster case counts on a choropleth map by postcode
#' (or any other geographic unit you use), provided the optional `sf`
#' package is installed and geographic reference data is available. Without
#' either, the dashboard still works fine - it just falls back to a bar
#' chart of case counts by area instead of a map.
#'
#' EpiSODIC ships with Dutch four-digit postcode geometry as a working
#' default, but is not tied to the Netherlands or to postcodes: point the
#' `EPISODIC_GEO_DATA` environment variable at your own `.rds` file (an
#' `sf` object with a `pc` column matching your case data's area codes, and
#' a `geometry` column) to map your own region instead.
#'
#' You can optionally add a second, purely visual layer of outlines - e.g.
#' province or municipality borders - drawn on top of the choropleth for
#' orientation, via `EPISODIC_GEO_DATA_OVERLAY` and
#' [episodic_geo_overlay_resolve()]. This layer carries no case counts, so
#' it only needs a `geometry` column.
#' @name episodic_geo
NULL

#' @rdname episodic_geo
#' @param path Path to an `.rds` file holding an `sf` object with a
#'   `geometry` column, for the optional outline layer. Defaults to the
#'   `EPISODIC_GEO_DATA_OVERLAY` environment variable.
#' @return An `sf` object, or `NULL` if `sf` is not installed, no path is
#'   set, or the file is missing or invalid.
#' @examples
#' # NULL when unset (or when the sf package is not installed)
#' episodic_geo_overlay_resolve(path = NA)
#' @export
episodic_geo_overlay_resolve <- function(path = Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }

  overlay <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(overlay) || !"geometry" %in% names(overlay)) {
    return(NULL)
  }
  overlay
}

#' @rdname episodic_geo
#' @param path Path to an `.rds` file holding an `sf` object with `pc`
#'   and `geometry` columns. Defaults to the `EPISODIC_GEO_DATA`
#'   environment variable. With none set, there is no map and the
#'   dashboard shows its bar-chart fallback instead.
#' @section Why there is no default map:
#' This used to fall back to the Netherlands PC4 geometry bundled with
#' the package whenever `EPISODIC_GEO_DATA` was unset, and even after
#' warning about an operator file it could not use. Postcode-like codes
#' are four digits in a great many countries, so a laboratory anywhere
#' else got a silent, confident map of the Netherlands with its own case
#' counts joined onto whichever Dutch postcodes happened to share a
#' number - a plausible-looking wrong answer, which is worse than no
#' answer. There is now no default: with nothing configured the map is
#' simply absent, and the dashboard falls back to a bar chart of case
#' counts by area, which is correct everywhere.
#'
#' `episodic_demo()` sets `EPISODIC_GEO_DATA` to the bundled Netherlands
#' geometry itself, so the demo still shows a map.
#' @examples
#' # NULL when unset (or when the sf package is not installed)
#' episodic_geo_source_resolve(path = NA)
#' @export
episodic_geo_source_resolve <- function(path = Sys.getenv("EPISODIC_GEO_DATA", unset = NA)) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  if (!file.exists(path)) {
    warning(
      "EPISODIC_GEO_DATA points at '",
      path,
      "', but no file exists there - the map falls back to a bar chart.",
      call. = FALSE
    )
    return(NULL)
  }
  geo <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(geo) || !all(c("pc", "geometry") %in% names(geo))) {
    warning(
      "The geographic reference data at '",
      path,
      "' could not be read as an 'sf' object with 'pc' and 'geometry' ",
      "columns - the map falls back to a bar chart.",
      call. = FALSE
    )
    return(NULL)
  }
  geo
}

#' The Netherlands PC4 geometry bundled with the package
#'
#' Shipped so that `episodic_demo()` has a map to draw, and so that the
#' package's own tests have a real `sf` object to exercise the map code
#' against. Deliberately **not** a fallback for
#' `episodic_geo_source_resolve()`: an instance that has not configured
#' its own geography gets no map, never somebody else's country's.
#'
#' @return An `sf` object with `pc` and `geometry`, or `NULL` if `sf` is
#'   not installed or the file is missing.
#' @keywords internal
#' @noRd
episodic_geo_source_default <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }
  path <- episodic_geo_source_default_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

#' Where the bundled Netherlands PC4 geometry lives
#' @keywords internal
#' @noRd
episodic_geo_source_default_path <- function() {
  path <- system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODIC")
  if (identical(path, "")) {
    path <- file.path("inst", "extdata", "geo_postcodes4_nl.rds")
  }
  path
}

#' Join case counts onto geographic reference data
#'
#' @param rows A data frame with `label` (PC-equivalent codes) and `n`
#'   (case counts), from `episodic_app_concentration()$rows`.
#' @param geo_data An `sf` object with `pc`/`geometry`, or `NULL` to
#'   resolve one via [episodic_geo_source_resolve()].
#' @return An `sf` object (a right join: every `geo_data` row is kept,
#'   `n` is `NA` where `rows` has no matching `pc`), or `NULL` if no
#'   geographic data is available at all.
#' @keywords internal
#' @noRd
episodic_geo_join <- function(rows, geo_data = NULL) {
  geo_data <- geo_data %||% episodic_geo_source_resolve()
  if (is.null(geo_data) || nrow(rows) == 0) {
    return(NULL)
  }

  counts <- data.frame(
    pc = as.character(rows$label),
    n = rows$n,
    stringsAsFactors = FALSE
  )
  geo_data$pc <- as.character(geo_data$pc)
  merged <- merge(geo_data, counts, by = "pc", all.x = TRUE)
  sf::st_as_sf(merged)
}
