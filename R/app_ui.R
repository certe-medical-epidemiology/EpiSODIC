#' The application shell
#'
#' Custom header, nav and brand bar rather than a stock `bslib::page_navbar`,
#' to match `episode-mockup.jsx`'s layout precisely (MILESTONES.md M2).
#' `bslib` supplies the Bootstrap reset and font-loading helper only; all
#' visual design comes from `inst/app/www/episode.css` and the palette
#' injected as CSS custom properties.
#'
#' @param lang Session language, `"nl"` (default) or `"en"`.
#' @return The page returned by [bslib::page_fluid()].
#' @keywords internal
#' @noRd
episode_app_ui <- function(lang = "nl") {
  pal <- episode_palette()

  bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = episode_tr("app.title", lang = lang),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet",
                        href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&display=swap"),
      shiny::tags$link(rel = "stylesheet", href = "www/episode.css"),
      shiny::tags$style(episode_app_palette_css(pal))
    ),
    shiny::tags$div(
      class = "episode-header",
      shiny::tags$div(
        style = "display:flex;align-items:center;",
        shiny::tags$span(class = "episode-brand",
                          "Epi", shiny::tags$span(class = "episode-brand-accent", "SODE")),
        shiny::tags$div(
          class = "episode-nav",
          episode_ui_nav_link("clusters", episode_tr("nav.clusters", lang = lang)),
          episode_ui_nav_link("streams", episode_tr("nav.streams", lang = lang))
        ),
        shiny::tags$span(class = "episode-demodata", episode_tr("app.demodata", lang = lang))
      ),
      shiny::uiOutput("status_strip", inline = TRUE)
    ),
    shiny::tags$div(
      class = "episode-brandbar",
      lapply(episode_brand_bar(), function(colour) shiny::tags$div(style = sprintf("background:%s;", colour)))
    ),
    shiny::uiOutput("main_view")
  )
}

#' @keywords internal
#' @noRd
episode_ui_nav_link <- function(view, label) {
  shiny::tags$a(
    href = "#", class = "episode-nav-link", `data-view` = view,
    onclick = sprintf("Shiny.setInputValue('nav_view', '%s', {priority: 'event'}); return false;", view),
    label
  )
}

#' @keywords internal
#' @noRd
episode_app_palette_css <- function(pal) {
  vars <- vapply(names(pal), function(n) sprintf("--episode-%s: %s;", n, pal[[n]]), character(1))
  paste0(":root {\n", paste(vars, collapse = "\n"), "\n}")
}
