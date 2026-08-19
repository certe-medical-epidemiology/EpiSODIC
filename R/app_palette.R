#' The application colour palette
#'
#' Uses `certestyle::certe.colours` when installed (ARCHITECTURE.md
#' section 10.1, MILESTONES.md M2). Since `certestyle` is Certe-internal
#' and therefore only in `Suggests` (standing brief section 2), a
#' shipped-default palette covers everyone else - not hardcoded in R, but
#' read from `inst/config/palette.yaml`, the same defaults-then-instance-
#' override pattern [episode_config_resolve()] uses for detection
#' configuration (deliberately a *separate* file and env var,
#' `EPISODE_PALETTE_CONFIG`: colours must never affect `config_hash`,
#' which is about detection reproducibility, not display). Any
#' organisation can therefore run EpiSODE in its own house colours,
#' certestyle installed or not.
#'
#' @return A named list of hex colour strings: the neutrals (`ink`
#'   default text, `muted` secondary text, `faint` tertiary, `border`,
#'   `bg_subtle`, `bg`, `surface` - a true grey scale, independent of
#'   whichever hue is `primary`) and the semantic roles `primary`
#'   (+`_dark`/`_light`/`_tint`), `secondary` (+`_dark`), `tertiary`
#'   (+`_dark`), `success` (+`_dark`), `warning` (+`_dark`), `danger`
#'   (+`_dark`).
#' @export
episode_palette <- function() {
  if (requireNamespace("certestyle", quietly = TRUE)) {
    cc <- tryCatch(certestyle::certe.colours, error = function(e) NULL)
    if (!is.null(cc)) return(episode_palette_from_certestyle(cc))
  }
  episode_palette_fallback()
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
  defaults_path <- system.file("config", "palette.yaml", package = "EpiSODE")
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

#' @keywords internal
#' @noRd
episode_palette_fallback <- function() {
  episode_palette_config_resolve()
}

#' @keywords internal
#' @noRd
episode_palette_from_certestyle <- function(cc) {
  # certestyle::certe.colours exposes its own (Dutch) hue names, one real
  # hue per role so none of Certe's own colours get merged with another;
  # mapped explicitly here rather than by name-matching, since we do not
  # control that external package's naming. Any hue it does not provide
  # falls back to our own shipped default for that role. cc may be a
  # named character vector rather than a list (certestyle's actual return
  # type), so `$` is not safe here - a small name-based getter handles both.
  cc_get <- function(name) {
    if (!is.null(names(cc)) && name %in% names(cc)) unname(cc[[name]]) else NULL
  }
  defaults <- episode_palette_config_resolve()
  list(
    ink = defaults$ink, muted = defaults$muted, faint = defaults$faint,
    border = defaults$border, bg_subtle = defaults$bg_subtle, bg = defaults$bg, surface = defaults$surface,
    primary = cc_get("blauw") %||% defaults$primary, primary_dark = cc_get("blauw0") %||% defaults$primary_dark,
    primary_light = cc_get("blauw2") %||% defaults$primary_light, primary_tint = cc_get("blauw5") %||% defaults$primary_tint,
    secondary = cc_get("lila") %||% defaults$secondary, secondary_dark = cc_get("lila0") %||% defaults$secondary_dark,
    tertiary = cc_get("bruin") %||% defaults$tertiary, tertiary_dark = cc_get("bruin0") %||% defaults$tertiary_dark,
    success = cc_get("groen") %||% defaults$success, success_dark = cc_get("groen0") %||% defaults$success_dark,
    warning = cc_get("geel") %||% defaults$warning, warning_dark = cc_get("geel0") %||% defaults$warning_dark,
    danger = cc_get("roze") %||% defaults$danger, danger_dark = cc_get("roze0") %||% defaults$danger_dark
  )
}

#' The five-colour brand bar used under the app header
#' @export
episode_brand_bar <- function() {
  p <- episode_palette()
  c(p$warning, p$success, p$danger, p$primary, p$secondary)
}
