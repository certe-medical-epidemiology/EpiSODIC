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

#' The application shell
#'
#' Custom header, nav and brand bar rather than a stock `bslib::page_navbar`,
#' to match the intended house-style layout precisely. `bslib` supplies
#' the Bootstrap reset and font-loading helper only; all
#' visual design comes from `inst/app/www/episodic.css` and the palette
#' injected as CSS custom properties.
#'
#' @param lang Session language: `"nl"` (default), `"en"`, `"es"`, `"fr"`,
#'   `"de"`, `"zh"`, `"hi"`, or `"ar"`.
#' @return The page returned by [bslib::page_fluid()].
#' @keywords internal
#' @noRd
episodic_app_ui <- function(lang = "nl") {
  pal <- episodic_palette()

  bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = episodic_tr("app.title", lang = lang),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet",
                        href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&display=swap"),
      shiny::tags$link(rel = "stylesheet", href = "www/episodic.css"),
      shiny::tags$style(episodic_app_palette_css(pal))
    ),
    shiny::tags$div(
      class = "episodic-header",
      shiny::tags$div(
        style = "display:flex;align-items:center;",
        shiny::tags$span(
          class = "episodic-brand", title = episodic_tr("app.full_name", lang = lang),
          "EpiSODIC",
          shiny::tags$span(class = "episodic-brand-version", paste0("v", utils::packageVersion("EpiSODIC")))
        ),
        shiny::tags$div(
          class = "episodic-nav",
          episodic_ui_nav_link("clusters", episodic_tr("nav.clusters", lang = lang), active = TRUE),
          # Between the operational views and the configuration ones: it
          # is the same surveillance data read at a different altitude,
          # not a settings screen.
          episodic_ui_nav_link("pathogen", episodic_tr("nav.pathogen", lang = lang)),
          episodic_ui_nav_link("streams", episodic_tr("nav.streams", lang = lang)),
          episodic_ui_nav_link("archive", episodic_tr("nav.archive", lang = lang)),
          episodic_ui_nav_link("activity", episodic_tr("nav.activity", lang = lang)),
          episodic_ui_nav_link("performance", episodic_tr("nav.performance", lang = lang)),
          episodic_ui_nav_link("info", episodic_tr("nav.info", lang = lang))
        ),
        shiny::tags$span(class = "episodic-demodata", episodic_tr("app.demodata", lang = lang))
      ),
      shiny::tags$div(
        style = "display:flex;align-items:center;gap:14px;",
        shiny::uiOutput("status_strip", inline = TRUE),
        shiny::uiOutput("auth_control", inline = TRUE)
      )
    ),
    shiny::tags$div(
      class = "episodic-brandbar",
      lapply(episodic_brand_bar(), function(colour) shiny::tags$div(style = sprintf("background:%s;", colour)))
    ),
    shiny::uiOutput("main_view")
  )
}

#' One top-navigation link
#'
#' The stylesheet has always had an `.active` rule for these, but nothing
#' ever applied the class, so the nav gave no indication of which screen
#' you were on. Handled client-side at click time rather than by
#' re-rendering the header from the server, the same approach
#' `episodic_ui_rail()` takes for its own selection highlight and for the
#' same reason: the header is not otherwise reactive, and making it so to
#' move one CSS class would rebuild the sign-in control and status strip
#' on every navigation.
#'
#' @param view The view id this link switches to.
#' @param label The link's visible text.
#' @param active Whether this link starts out highlighted - true for the
#'   view the app opens on.
#' @keywords internal
#' @noRd
episodic_ui_nav_link <- function(view, label, active = FALSE) {
  shiny::tags$a(
    href = "#",
    class = if (isTRUE(active)) "episodic-nav-link active" else "episodic-nav-link",
    `data-view` = view,
    onclick = sprintf(
      paste0(
        "document.querySelectorAll('.episodic-nav-link').forEach(function(a){a.classList.remove('active');}); ",
        "this.classList.add('active'); ",
        "Shiny.setInputValue('nav_view', '%s', {priority: 'event'}); return false;"
      ),
      view
    ),
    label
  )
}

#' @keywords internal
#' @noRd
episodic_app_palette_css <- function(pal) {
  # CSS custom property names conventionally use hyphens, not underscores
  # (episodic_palette()'s own list names, e.g. "primary_tint", follow R's
  # convention instead); translated here so the stylesheet's var(--episodic-
  # primary-tint) references match what actually gets defined.
  css_names <- gsub("_", "-", names(pal), fixed = TRUE)
  vars <- vapply(seq_along(pal), function(i) sprintf("--episodic-%s: %s;", css_names[i], pal[[i]]), character(1))
  paste0(":root {\n", paste(vars, collapse = "\n"), "\n}")
}
