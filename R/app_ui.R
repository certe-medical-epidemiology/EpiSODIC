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
  resolved_lang <- episodic_lang(lang)

  bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = episodic_tr("app.title", lang = lang),
    shiny::tags$head(
      # `lang` is what a screen reader picks its voice and pronunciation
      # rules from, and `dir` is what makes a right-to-left language read
      # right to left. The page carried neither, while Arabic has been
      # one of the eight shipped languages all along - so an Arabic
      # dashboard rendered left to right, with its navigation, tables,
      # chart labels and assessment form all mirrored the wrong way
      # round, and every language was announced to assistive technology
      # as whatever the browser guessed.
      #
      # Written onto <html> from script rather than passed to
      # `bslib::page_fluid()`: the page function's own handling of these
      # attributes differs across bslib versions, and this works on all
      # of them. See `episodic_lang_dir()` and the "Right-to-left"
      # section of episodic.css.
      shiny::tags$script(shiny::HTML(sprintf(
        paste0(
          "document.documentElement.setAttribute('lang', '%s');",
          "document.documentElement.setAttribute('dir', '%s');"
        ),
        resolved_lang,
        episodic_lang_dir(resolved_lang)
      ))),
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
      shiny::tags$style(episodic_app_palette_css(pal)),
      # The one place that opens a cluster from outside the rail itself -
      # a cluster table row, a "linked to #N" chip - has to be able to
      # move the rail's own "active" highlight, which output$rail_pane
      # deliberately does not re-render on every selection (see its own
      # comment in app_server.R, about not losing scroll position).
      # Defined globally, once, rather than per-caller: every place that
      # used to inline `Shiny.setInputValue('open_cluster', ...)`
      # (episodic_ui_cluster_row(), episodic_ui_chip_link()) now calls
      # this instead, so the rail highlight and the dossier selection can
      # never drift apart again. A no-op when the target cluster is not
      # in the rail's current list (a closed cluster, or an id opened
      # while the rail is not on screen) - there is simply nothing to
      # highlight yet. Also switches the mobile pane to the dossier and
      # refreshes the pane switcher's cluster label (see episodicSelectPane()/
      # episodicSyncPaneBar() below) - a no-op above 1200px, since neither
      # function finds the elements it looks for there.
      shiny::tags$script(shiny::HTML(paste0(
        "function episodicOpenCluster(id){",
        "Shiny.setInputValue('open_cluster', id, {priority: 'event'});",
        "document.querySelectorAll('.episodic-rail-item').forEach(function(el){",
        "el.classList.toggle('active', el.dataset.clusterId === String(id));",
        "});",
        "episodicSelectPane('dossier');",
        "episodicSyncPaneBar();",
        "}"
      ))),
      # Client-side pane switching for the 768-1199px and <768px tiers
      # (see the "Responsive layout" section at the end of episodic.css).
      # A single data-pane attribute on .episodic-body carries which pane
      # is on top/visible; the CSS below 1200px gives it meaning, and the
      # attribute is otherwise inert (no rule above 1200px reads it), so
      # this function is harmless to call from a desktop session too.
      # Deliberately not a Shiny input: switching panes must never be a
      # server round trip (it would re-render output$rail_pane on every
      # tap and lose its scroll position, see that output's own comment
      # in app_server.R).
      shiny::tags$script(shiny::HTML(paste0(
        "function episodicSelectPane(pane){",
        "var body = document.querySelector('.episodic-body'); ",
        "if(!body){return;} ",
        "body.dataset.pane = pane; ",
        "document.querySelectorAll('.episodic-pane-tab').forEach(function(btn){",
        "btn.classList.toggle('active', btn.dataset.paneTarget === pane);",
        "});",
        "}"
      ))),
      # The phone-tier segmented control (see episodic_ui_pane_switcher() in
      # R/app_widgets.R) also names the cluster the other two segments refer
      # to. Copied client-side from the rail's own already-rendered active
      # item rather than read from a new server output, so no output added
      # here needs its own episodic_app_access_granted() gate - it is
      # already gated once, upstream, on output$rail_pane. The checkbox and
      # care-line chip are stripped from the clone: they belong to the rail
      # row, not to a "which cluster is this" label.
      shiny::tags$script(shiny::HTML(paste0(
        "function episodicSyncPaneBar(){",
        "var label = document.getElementById('episodic-pane-switcher-label'); ",
        "if(!label){return;} ",
        "var active = document.querySelector('.episodic-rail-item.active .episodic-rail-pathogen'); ",
        "if(!active){label.innerHTML = ''; return;} ",
        "var clone = active.cloneNode(true); ",
        "clone.querySelectorAll('input, .episodic-chip').forEach(function(el){el.remove();}); ",
        "label.innerHTML = clone.innerHTML;",
        "}"
      ))),
      # Keeps the pane-switcher label in step with output$rail_pane even
      # when the active item changes without a click of its own - the
      # initial render (the cron's first-cluster default selection) and
      # any later re-render of the rail's cluster list (sign-in/out,
      # a new detection run). shiny:value fires for every output on every
      # render, including the first, so this alone also covers page load.
      shiny::tags$script(shiny::HTML(
        "$(document).on('shiny:value', function(ev){ if(ev.name === 'rail_pane'){ episodicSyncPaneBar(); } });"
      )),
      # Collapses the header nav below 1200px (see .episodic-nav-toggle in
      # episodic.css). episodic_ui_nav_link()'s own onclick closes it again
      # on the next navigation, the same way it already clears the other
      # links' "active" class.
      shiny::tags$script(shiny::HTML(paste0(
        "function episodicToggleNav(btn){",
        "var nav = document.querySelector('.episodic-nav'); ",
        "if(!nav){return;} ",
        "var open = nav.classList.toggle('open'); ",
        "btn.setAttribute('aria-expanded', open ? 'true' : 'false');",
        "}"
      )))
    ),
    shiny::tags$div(
      class = "episodic-shell",
      shiny::tags$div(
        class = "episodic-header",
        shiny::tags$div(
          class = "episodic-header-left",
          shiny::tags$span(
            class = "episodic-brand",
            title = episodic_tr("app.full_name", lang = lang),
            "EpiSODIC"
          ),
          # Icon-only hamburger: hidden entirely at >=1200px, where the
          # nav is always shown inline and there is nothing to expand.
          shiny::tags$button(
            type = "button",
            class = "episodic-nav-toggle",
            `aria-label` = episodic_tr("nav.menu_label", lang = lang),
            `aria-expanded` = "false",
            onclick = "episodicToggleNav(this);",
            "\u2630"
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
          class = "episodic-header-right",
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
episodic_ui_nav_links <- function(active_view = "clusters",
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
        # Closes the mobile dropdown behind .episodic-nav-toggle again -
        # a no-op above 1200px, where .episodic-nav never gains the
        # "open" class in the first place.
        "var nav = document.querySelector('.episodic-nav'); ",
        "if(nav){nav.classList.remove('open'); var t = document.querySelector('.episodic-nav-toggle'); if(t){t.setAttribute('aria-expanded', 'false');}} ",
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
