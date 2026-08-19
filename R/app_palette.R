#' Certe colour palette
#'
#' Uses `certestyle::certe.colours` when installed (ARCHITECTURE.md
#' section 10.1, MILESTONES.md M2), with darker variants for text where
#' the saturated colour fails contrast, exactly as M2 specifies. Since
#' `certestyle` is Certe-internal and therefore only in `Suggests`
#' (standing brief section 2), a fallback copy of the exact same hex
#' values ships here too, so the app looks identical whether or not
#' `certestyle` is installed - a stranger cloning the repository gets the
#' real Certe look, not a generic Bootstrap theme.
#'
#' @return A named list of hex colour strings. See the source for the
#'   full set; the semantic roles used throughout the app are `ink`
#'   (default text), `muted` (secondary text/labels), `faint` (tertiary),
#'   `rule`/`track`/`paper`/`surface` (structural greys), and the named
#'   hues (`petrol`, `carmine`, `pink`, `olive`, `yellow`, `lilac`).
#' @export
episode_palette <- function() {
  if (requireNamespace("certestyle", quietly = TRUE)) {
    cc <- tryCatch(certestyle::certe.colours, error = function(e) NULL)
    if (!is.null(cc)) return(episode_palette_from_certestyle(cc))
  }
  episode_palette_fallback()
}

#' @keywords internal
#' @noRd
episode_palette_fallback <- function() {
  list(
    blauw = "#4A647D", blauw0 = "#3A4D5D", blauw2 = "#69849C", blauw3 = "#97AABB",
    blauw4 = "#C5D0DB", blauw5 = "#E2E7EC", blauw6 = "#F6F7F8",
    groen = "#93984C", groen0 = "#5A5D33", groen3 = "#C9CCA5", groen5 = "#EEEFE4",
    roze = "#B4527F", roze0 = "#7F3C5B", roze3 = "#D5ACBF", roze5 = "#F2E6EB",
    geel = "#E4D559", geel0 = "#D4C230", geel3 = "#ECE6B1", geel5 = "#F9F7E8",
    lila = "#CEB9D6", lila0 = "#BEA5C7", lila3 = "#E6DDE9",
    bruin = "#998961", bruin0 = "#675D45",
    # semantic roles, matching episode-mockup.jsx's `C` object exactly
    ink = "#3A4D5D", muted = "#4A647D", faint = "#97AABB",
    rule = "#C5D0DB", track = "#E2E7EC", paper = "#F6F7F8", surface = "#FFFFFF",
    petrol = "#4A647D", petrolD = "#3A4D5D", petrolL = "#69849C",
    carmine = "#7F3C5B", pink = "#B4527F", pinkTint = "#F2E6EB",
    olive = "#93984C", oliveD = "#5A5D33",
    yellow = "#E4D559", yellowD = "#675D45",
    lilac = "#CEB9D6",
    band = "#C5D0DB", hatch = "#C5D0DB", tint = "#E2E7EC", soft = "#F6F7F8"
  )
}

#' @keywords internal
#' @noRd
episode_palette_from_certestyle <- function(cc) {
  # certestyle::certe.colours is expected to provide the base named hues;
  # semantic roles and darker text variants are derived the same way the
  # fallback defines them, so behaviour is identical either way.
  base <- episode_palette_fallback()
  known <- intersect(names(base), names(cc))
  for (name in known) base[[name]] <- cc[[name]]
  base
}

#' The five-colour brand bar used under the app header
#' @export
episode_brand_bar <- function() {
  p <- episode_palette()
  c(p$geel, p$olive, p$roze %||% p$pink, p$blauw %||% p$petrol, p$lila %||% p$lilac)
}
