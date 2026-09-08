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

#' The Dashboard's Colour Palette and Typography
#'
#' Returns the colours and typography used throughout the EpiSODIC dashboard
#' and charts, as a named list. Useful if you want to match your own plots or
#' reports to the house style, or check what colour a given status uses.
#'
#' The palette ships with an organisation-neutral default
#' (`r doc_system_file("inst/config/episodic_default_style.yaml")`). To use your own institute's colours or fonts instead,
#' point the `EPISODIC_STYLE` environment variable at a YAML file
#' that overrides only the roles you want to change - anything you do not set
#' keeps its shipped default.
#'
#' This is independent of `episodic_config_resolve()`
#' on purpose: colours and typography never affect the `config_hash` recorded
#' with a detection run, since they have no bearing on reproducibility.
#'
#' @section Default font and colours:
#' These are all the default values, and all can be changed using a custom YAML file.
#'
#' `r doc_palette()`
#'
#' Of note:
#'
#' * `primary_dark` is the background colour of the navigation bar.
#' * `font` is a CSS font-family stack, and `font_size_base` is the app's base font size.
#'   * Every other font size in the dashboard is set in `rem` relative to it, so changing `font_size_base` scales the whole app's type proportionally (useful when swapping in a font that reads naturally smaller or larger than the default at the same pixel size).
#'   * Changing `font` only changes the CSS declaration; if it names a webfont rather than a system font, delivering that font (a self-hosted `@font-face` or a link to its provider) is the operator's own concern.
#'
#' @return A named list. The greyscale neutrals are
#'   `ink` (default text), `muted` (secondary text), `faint` (tertiary text),
#'   `border`, `bg_subtle`, `bg`, and `surface`. The semantic roles are
#'   `primary`, `secondary`, `tertiary`, `success`, `warning`, and `danger`,
#'   each with `_dark`/`_light`/`_tint` variants where used. `font` and
#'   `font_size_base` hold the app's typography, not a colour.
#' @examples
#' pal <- episodic_palette()
#' pal$primary
#' pal$danger
#' pal$font
#' pal$font_size_base
#' @export
episodic_palette <- function() {
  episodic_palette_config_resolve()
}

# A dossier render alone calls episodic_palette() seven times, charts nine -
# every one of them re-reading and re-parsing the same YAML file(s) from
# disk, on every render, for a value that cannot change within a running
# session (it is a function of the env var and shipped/instance files, none
# of which are touched while the app is up). Cached per resolved path, not
# globally, so a caller that does pass a different `palette_config_path`
# still gets the right file, just not re-read every time.
episodic_palette_cache <- new.env(parent = emptyenv())

#' Resolve the shipped default palette, with an optional instance override
#'
#' @param palette_config_path Path to an instance palette file, overlaid
#'   key-by-key on the shipped defaults. Defaults to the
#'   `EPISODIC_STYLE` environment variable.
#' @return A named list of role-named hex colours (see [episodic_palette()]).
#' @keywords internal
#' @noRd
episodic_palette_config_resolve <- function(palette_config_path = Sys.getenv("EPISODIC_STYLE", unset = NA)) {
  # `[[` on an environment requires a non-empty name - "" (from an unset
  # env var) errors with "zero-length variable name" - hence the sentinel
  # rather than caching under palette_config_path itself.
  cache_key <- if (is.na(palette_config_path) || !nzchar(palette_config_path)) {
    "._default"
  } else {
    palette_config_path
  }
  cached <- episodic_palette_cache[[cache_key]]
  if (!is.null(cached)) {
    return(cached)
  }

  defaults_path <- system.file("config", "episodic_default_style.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "episodic_default_style.yaml")
  }
  base <- yaml::read_yaml(defaults_path)

  if (!is.na(palette_config_path) && nzchar(palette_config_path)) {
    # Set but unusable is a configuration error, not a fallback - the
    # same rule EPISODIC_CONFIG and EPISODIC_PC_PROVINCE_MAP follow. An
    # operator who pointed EPISODIC_STYLE at a mistyped path used to get
    # the shipped palette with nothing said, and reasonably concluded
    # their own house colours simply had not been applied properly.
    if (!file.exists(palette_config_path)) {
      stop(
        "EPISODIC_STYLE points at '",
        palette_config_path,
        "', but no file exists there. Correct the path, or unset it to ",
        "use the shipped palette.",
        call. = FALSE
      )
    }
    instance_palette <- tryCatch(
      yaml::read_yaml(palette_config_path),
      error = function(e) e
    )
    if (inherits(instance_palette, "condition")) {
      stop(
        "EPISODIC_STYLE points at '",
        palette_config_path,
        "', which could not be read as YAML: ",
        conditionMessage(instance_palette),
        call. = FALSE
      )
    }
    base <- episodic_config_merge(base, instance_palette %||% list())
  }

  episodic_palette_cache[[cache_key]] <- base
  base
}

#' The five-colour brand bar used under the app header
#' @keywords internal
#' @noRd
episodic_brand_bar <- function() {
  p <- episodic_palette()
  c(p$warning, p$success, p$danger, p$primary, p$secondary)
}
