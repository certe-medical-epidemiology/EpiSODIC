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

#' The application colour palette
#'
#' Read from `inst/config/palette.yaml`, the same defaults-then-instance-
#' override pattern [episode_config_resolve()] uses for detection
#' configuration (deliberately a *separate* file and env var,
#' `EPISODE_PALETTE_CONFIG`: colours must never affect `config_hash`,
#' which is about detection reproducibility, not display). Any
#' organisation runs EpiSODIC in its own house colours by pointing
#' `EPISODE_PALETTE_CONFIG` at a YAML file overriding whichever roles it
#' wants to rebrand - a department that wants its own colours supplies
#' its own file, exactly as it supplies its own report template
#' (`EPISODE_QUARTO_REPORT`) or geographic reference data
#' (`EPISODE_GEO_DATA`); this package ships only the generic mechanism
#' and an organisation-neutral default, never a specific organisation's
#' house style.
#'
#' @return A named list of hex colour strings: the neutrals (`ink`
#'   default text, `muted` secondary text, `faint` tertiary, `border`,
#'   `bg_subtle`, `bg`, `surface` - a true grey scale, independent of
#'   whichever hue is `primary`) and the semantic roles `primary`
#'   (+`_dark`/`_light`/`_tint`), `secondary` (+`_dark`), `tertiary`
#'   (+`_dark`), `success` (+`_dark`), `warning` (+`_dark`), `danger`
#'   (+`_dark`).
#' @examples
#' pal <- episode_palette()
#' pal$primary
#' @export
episode_palette <- function() {
  episode_palette_config_resolve()
}

#' Resolve the shipped default palette, with an optional instance override
#'
#' @param palette_config_path Path to an instance palette file, overlaid
#'   key-by-key on the shipped defaults. Defaults to the
#'   `EPISODE_PALETTE_CONFIG` environment variable.
#' @return A named list of role-named hex colours (see [episode_palette()]).
#' @keywords internal
#' @noRd
episode_palette_config_resolve <- function(palette_config_path = Sys.getenv("EPISODE_PALETTE_CONFIG", unset = NA)) {
  defaults_path <- system.file("config", "palette.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "palette.yaml")
  }
  base <- yaml::read_yaml(defaults_path)

  if (!is.na(palette_config_path) && nzchar(palette_config_path) && file.exists(palette_config_path)) {
    instance_palette <- yaml::read_yaml(palette_config_path)
    base <- episode_config_merge(base, instance_palette)
  }

  base
}

#' The five-colour brand bar used under the app header
#' @keywords internal
#' @noRd
episode_brand_bar <- function() {
  p <- episode_palette()
  c(p$warning, p$success, p$danger, p$primary, p$secondary)
}
