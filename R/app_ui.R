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
#' @param lang Session language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @return The page returned by [bslib::page_fluid()].
#' @keywords internal
#' @noRd
episodic_app_ui <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()

  bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = episodic_tr("app.title", lang = lang),
    shiny::tags$head(
      # Only fetched when the resolved palette still uses the shipped
      # default font - the moment an instance overrides `font` in its
      # EPISODIC_STYLE, this Google Fonts request for a face
      # nobody asked for would otherwise keep firing on every page load.
      # Serving a substitute font is then the operator's own concern
      # (a system font needs no webfont link at all; a different webfont
      # is loaded the same way - self-hosted, or linked from its own
      # provider - by shipping a custom `www/episodic.css` alongside it).
      if (grepl("IBM Plex Sans", pal$font, fixed = TRUE)) {
        shiny::tags$link(
          rel = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&display=swap"
        )
      },
      shiny::tags$link(rel = "stylesheet", href = "www/episodic.css"),
      shiny::tags$style(episodic_app_palette_css(pal))
    ),
    shiny::tags$div(
      class = "episodic-header",
      shiny::tags$div(
        style = "display:flex;align-items:center;",
        shiny::tags$span(
          class = "episodic-brand",
          title = episodic_tr("app.full_name", lang = lang),
          "EpiSODIC"
        ),
        # Rendered from the server's own view(), not written once here:
        # the highlight has to follow every way the view can change, and
        # not every one of them is a click on these links. The Pathogen
        # screen's cluster table switches views from a table row, and a
        # nav that only updated itself on its own clicks was left
        # pointing at the screen you had just left.
        shiny::uiOutput(
          "nav_links",
          container = shiny::tags$div,
          class = "episodic-nav"
        )
      ),
      shiny::tags$div(
        style = "display:flex;align-items:center;gap:14px;",
        shiny::uiOutput("status_strip", inline = TRUE),
        shiny::uiOutput("auth_control", inline = TRUE)
      )
    ),
    shiny::tags$div(
      class = "episodic-brandbar",
      lapply(episodic_brand_bar(), function(colour) {
        shiny::tags$div(style = sprintf("background:%s;", colour))
      })
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
#' The top navigation links, with the current view marked
#'
#' @param active_view The view id currently on screen.
#' @param lang Session language.
#' @param is_admin Whether the signed-in account may see the Settings
#'   screen - `FALSE`/`NULL` (the default: nobody signed in, or a
#'   non-admin) omits the link entirely, so a non-admin never sees a link
#'   to a screen that server-side re-checks the same flag and refuses to
#'   render regardless (see `episodic_app_server_settings()`).
#' @return A `shiny::tagList` of links.
#' @keywords internal
#' @noRd
episodic_ui_nav_links <- function(
    active_view = "clusters",
    lang = Sys.getenv("EPISODIC_LANGUAGE"),
    is_admin = FALSE) {
  views <- c(
    "clusters",
    # Between the operational views and the configuration ones: it is the
    # same surveillance data read at a different altitude, not a settings
    # screen.
    "pathogen",
    "streams",
    "archive",
    "activity",
    "performance",
    "info"
  )
  if (isTRUE(is_admin)) {
    views <- c(views, "settings")
  }
  shiny::tagList(lapply(views, function(v) {
    episodic_ui_nav_link(
      v,
      episodic_tr(paste0("nav.", v), lang = lang),
      active = identical(v, active_view)
    )
  }))
}

#' @keywords internal
#' @noRd
episodic_ui_nav_link <- function(view, label, active = FALSE) {
  shiny::tags$a(
    href = "#",
    class = if (isTRUE(active)) {
      "episodic-nav-link active"
    } else {
      "episodic-nav-link"
    },
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
  vars <- vapply(
    seq_along(pal),
    function(i) sprintf("--episodic-%s: %s;", css_names[i], pal[[i]]),
    character(1)
  )
  paste0(":root {\n", paste(vars, collapse = "\n"), "\n}")
}
