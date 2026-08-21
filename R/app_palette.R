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

#' The dashboard's colour palette
#'
#' Returns the colours used throughout the EpiSODIC dashboard and charts, as
#' a named list of hex codes. Useful if you want to match your own plots or
#' reports to the house style, or check what colour a given status uses.
#'
#' The palette ships with an organisation-neutral default
#' (`inst/config/palette.yaml`). To use your own institute's colours instead,
#' point the `EPISODIC_PALETTE_CONFIG` environment variable at a YAML file
#' that overrides only the roles you want to change - anything you don't set
#' keeps its shipped default. This is independent of [episodic_config_resolve()]
#' on purpose: colours never affect the `config_hash` recorded with a
#' detection run, since they have no bearing on reproducibility.
#'
#' @return A named list of hex colour strings. The greyscale neutrals are
#'   `ink` (default text), `muted` (secondary text), `faint` (tertiary text),
#'   `border`, `bg_subtle`, `bg`, and `surface`. The semantic roles are
#'   `primary`, `secondary`, `tertiary`, `success`, `warning`, and `danger`,
#'   each with `_dark`/`_light`/`_tint` variants where used.
#' @examples
#' pal <- episodic_palette()
#' pal$primary
#' pal$danger
#' @export
episodic_palette <- function() {
  episodic_palette_config_resolve()
}

#' Resolve the shipped default palette, with an optional instance override
#'
#' @param palette_config_path Path to an instance palette file, overlaid
#'   key-by-key on the shipped defaults. Defaults to the
#'   `EPISODIC_PALETTE_CONFIG` environment variable.
#' @return A named list of role-named hex colours (see [episodic_palette()]).
#' @keywords internal
#' @noRd
episodic_palette_config_resolve <- function(palette_config_path = Sys.getenv("EPISODIC_PALETTE_CONFIG", unset = NA)) {
  defaults_path <- system.file("config", "palette.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "palette.yaml")
  }
  base <- yaml::read_yaml(defaults_path)

  if (!is.na(palette_config_path) && nzchar(palette_config_path) && file.exists(palette_config_path)) {
    instance_palette <- yaml::read_yaml(palette_config_path)
    base <- episodic_config_merge(base, instance_palette)
  }

  base
}

#' The five-colour brand bar used under the app header
#' @keywords internal
#' @noRd
episodic_brand_bar <- function() {
  p <- episodic_palette()
  c(p$warning, p$success, p$danger, p$primary, p$secondary)
}
