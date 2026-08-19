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
#' @return A named list of hex colour strings: the base hues (`blue`,
#'   `green`, `pink`, `yellow`, `lilac`, `brown`, each with `_dark`/
#'   `_light`/`_pale` steps where the UI needs them) plus the semantic
#'   roles used throughout the app - `ink` (default text), `muted`
#'   (secondary text/labels), `faint` (tertiary), `rule`/`track`/`paper`/
#'   `surface` (structural greys), and `petrol`/`carmine`/`pink`/`olive`/
#'   `yellow`/`lilac` (matching `episode-mockup.jsx`'s `C` object).
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
#' @return A named list of the base hues (English keys).
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

#' Derive the semantic role names the app and CSS actually use, from a set
#' of base hues
#'
#' A single source of truth: whatever supplies the base hues (the shipped
#' defaults, an instance override, or `certestyle`), every semantic role is
#' computed from it here rather than duplicated as a separate literal, so
#' an override always propagates consistently.
#'
#' @param base A named list of base hues, see `episode_palette_config_resolve()`.
#' @return A named list of semantic-role hex colours.
#' @keywords internal
#' @noRd
episode_palette_semantic <- function(base) {
  list(
    ink = base$blue_dark, muted = base$blue, faint = base$blue_pale,
    rule = base$blue_paler, track = base$blue_palest, paper = base$blue_faintest, surface = base$white,
    petrol = base$blue, petrolD = base$blue_dark, petrolL = base$blue_light,
    carmine = base$pink_dark, pink = base$pink, pinkTint = base$pink_palest,
    olive = base$green, oliveD = base$green_dark,
    yellow = base$yellow, yellowD = base$brown_dark,
    lilac = base$lilac,
    band = base$blue_paler, hatch = base$blue_paler, tint = base$blue_palest, soft = base$blue_faintest
  )
}

#' @keywords internal
#' @noRd
episode_palette_fallback <- function() {
  base <- episode_palette_config_resolve()
  c(base, episode_palette_semantic(base))
}

#' @keywords internal
#' @noRd
episode_palette_from_certestyle <- function(cc) {
  # certestyle::certe.colours exposes its own (Dutch) hue names; mapped
  # explicitly here rather than by name-matching, since we do not control
  # that external package's naming. Any hue it does not provide falls back
  # to our own shipped default for that hue.
  defaults <- episode_palette_config_resolve()
  base <- list(
    blue = cc$blauw %||% defaults$blue, blue_dark = cc$blauw0 %||% defaults$blue_dark,
    blue_light = cc$blauw2 %||% defaults$blue_light, blue_pale = cc$blauw3 %||% defaults$blue_pale,
    blue_paler = cc$blauw4 %||% defaults$blue_paler, blue_palest = cc$blauw5 %||% defaults$blue_palest,
    blue_faintest = cc$blauw6 %||% defaults$blue_faintest,
    green = cc$groen %||% defaults$green, green_dark = cc$groen0 %||% defaults$green_dark,
    green_pale = cc$groen3 %||% defaults$green_pale, green_palest = cc$groen5 %||% defaults$green_palest,
    pink = cc$roze %||% defaults$pink, pink_dark = cc$roze0 %||% defaults$pink_dark,
    pink_pale = cc$roze3 %||% defaults$pink_pale, pink_palest = cc$roze5 %||% defaults$pink_palest,
    yellow = cc$geel %||% defaults$yellow, yellow_dark = cc$geel0 %||% defaults$yellow_dark,
    yellow_pale = cc$geel3 %||% defaults$yellow_pale, yellow_palest = cc$geel5 %||% defaults$yellow_palest,
    lilac = cc$lila %||% defaults$lilac, lilac_dark = cc$lila0 %||% defaults$lilac_dark,
    lilac_pale = cc$lila3 %||% defaults$lilac_pale,
    brown = cc$bruin %||% defaults$brown, brown_dark = cc$bruin0 %||% defaults$brown_dark,
    white = defaults$white
  )
  c(base, episode_palette_semantic(base))
}

#' The five-colour brand bar used under the app header
#' @export
episode_brand_bar <- function() {
  p <- episode_palette()
  c(p$yellow, p$olive, p$pink, p$blue, p$lilac)
}
