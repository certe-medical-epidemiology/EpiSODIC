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

#' Every screen the dashboard has
#'
#' The order is the order the screens are written into the page, which is
#' also their tab order. Which one is *shown* is `data-view` on
#' `.episodic-shell`; every screen is in the page all the time.
#'
#' A view id is also the value `input$nav_view` carries, and any client
#' can set any input, so `episodic_app_server_factory()` checks an
#' incoming value against this vector rather than trusting it - an id
#' that is not one of these would otherwise leave the shell showing no
#' screen at all.
#'
#' @return A character vector of view ids.
#' @keywords internal
#' @noRd
episodic_app_views <- function() {
  c(
    "outbreaks",
    "epidemics",
    "pathogens",
    "instance",
    "archive",
    "streams",
    "activity",
    "performance",
    "info",
    "settings"
  )
}

#' The navigation link a screen lights up
#'
#' Five links carry ten screens. The four surveillance screens are
#' their own group; the five that describe the instance rather than a
#' cluster are reached from the Instance screen and light its link, so a
#' reader on the Performance screen can still see where they are.
#'
#' This mapping is stated once, here, and reaches the browser twice: as
#' `data-nav` on whatever element navigates (so a click needs no mapping
#' of its own) and in the message the server sends when the view changes
#' by any other route. `episodic-nav.js` never computes it.
#'
#' @param view A view id from `episodic_app_views()`.
#' @return A single nav group id.
#' @keywords internal
#' @noRd
episodic_app_nav_group <- function(view) {
  if (view %in% c("outbreaks", "epidemics", "pathogens")) {
    return(view)
  }
  "instance"
}

#' The application shell
#'
#' Custom header, nav and brand bar rather than a stock
#' `bslib::page_navbar`, to match the intended house-style layout
#' precisely. `bslib` supplies the Bootstrap reset and font-loading
#' helper only; all visual design comes from `inst/app/www/episodic.css`
#' and the palette injected as CSS custom properties.
#'
#' Every screen is written into the page here, once, and shown or hidden
#' by `data-view` on `.episodic-shell`. That is what makes navigation
#' cost nothing: a screen is built the first time it is shown, because
#' Shiny suspends the outputs inside a hidden one, and coming back to it
#' re-runs nothing unless its data actually changed (`Observer$resume()`
#' schedules a re-execution only for an observer invalidated while it was
#' suspended). The alternative, one `renderUI` switching on the view,
#' tears the current screen out of the page and rebuilds the destination
#' from the database on every single navigation, and rebuilds the one you
#' came from again on the way back.
#'
#' This needs `shiny (>= 1.14.0)`, which is where output visibility
#' started being tracked with `ResizeObserver`/`IntersectionObserver`
#' instead of jQuery `shown`/`hidden` events. That is what lets a
#' stylesheet alone decide what is visible: on an older Shiny, a
#' CSS-only show/hide is invisible to the suspension machinery and every
#' hidden screen would keep recomputing.
#'
#' @param lang Session language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`, or a regional variant of
#'   one (`"en-US"`, `"es-419"`). Defaults to the `EPISODIC_LANGUAGE`
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
      # right to left. Written onto <html> from script rather than passed
      # to `bslib::page_fluid()`: the page function's own handling of
      # these attributes differs across bslib versions, and this works on
      # all of them.
      #
      # Inline, and first in the head, rather than in episodic-nav.js:
      # this is the one thing that must be true before the first paint,
      # and a deferred script would show an Arabic reader a
      # left-to-right layout for as long as it took to load. See
      # `episodic_lang_dir()` and the "Right-to-left" section of
      # episodic.css.
      #
      # The same block also appends `viewport-fit=cover` to the viewport
      # meta tag Shiny's own bootstrap dependency injects (it ships
      # without one), rather than adding a second `<meta name="viewport">`
      # of our own: two such tags in one document leave which one wins to
      # the browser, where a plain edit of the existing element does not.
      # `viewport-fit=cover` is what lets the page draw under the
      # notch/status-bar safe area at all - without it, every
      # `env(safe-area-inset-*)` in episodic.css (see the mobile pane
      # switcher's bottom padding) resolves to zero on an iPhone, not the
      # real inset.
      shiny::tags$script(shiny::HTML(sprintf(
        paste0(
          "document.documentElement.setAttribute('lang', '%s');",
          "document.documentElement.setAttribute('dir', '%s');",
          "var vp = document.querySelector('meta[name=\"viewport\"]');",
          "if (vp && vp.content.indexOf('viewport-fit') === -1) {",
          "vp.content += ', viewport-fit=cover';",
          "}"
        ),
        resolved_lang,
        episodic_lang_dir(resolved_lang)
      ))),
      # Tints the browser's own chrome (Safari's toolbar, Android
      # Chrome's) to the header's colour, so it reads as part of the app
      # rather than a white bar Safari drew on top of it. Paired with
      # `html`'s own background in episodic.css: that is what colours the
      # rubber-band overscroll above the header, which this meta tag does
      # not reach.
      shiny::tags$meta(name = "theme-color", content = pal$primary_dark),
      # Only fetched when the resolved palette still uses the shipped
      # default font - the moment an instance overrides `font` in its
      # EPISODIC_STYLE, this Google Fonts request for a face
      # nobody asked for would otherwise keep firing on every page load.
      # Serving a substitute font is then the operator's own concern
      # (a system font needs no webfont link at all; a different webfont
      # is loaded the same way - self-hosted, or linked from its own
      # provider - by shipping a custom `www/episodic.css` alongside it).
      #
      # `preconnect` to the font host beside it, because the stylesheet
      # and the font file it names are on different origins: without it
      # the DNS lookup and TLS handshake for the second only begin once
      # the first has been parsed, which is a round trip added to first
      # paint on every cold load.
      if (grepl("IBM Plex Sans", pal$font, fixed = TRUE)) {
        shiny::tagList(
          shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
          shiny::tags$link(
            rel = "preconnect",
            href = "https://fonts.gstatic.com",
            crossorigin = ""
          ),
          shiny::tags$link(
            rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&display=swap"
          )
        )
      },
      shiny::tags$link(
        rel = "stylesheet",
        href = paste0(
          "www/episodic.css?v=",
          episodic_app_asset_version("episodic.css")
        )
      ),
      shiny::tags$style(episodic_app_palette_css(pal)),
      # All navigation behaviour, in one cached file rather than in a
      # handful of <script> blocks rebuilt into the page on every render
      # and an `onclick` attribute on every element. See the file's own
      # header for the rule it holds to.
      shiny::tags$script(src = paste0(
        "www/episodic-nav.js?v=",
        episodic_app_asset_version("episodic-nav.js")
      ))
    ),
    shiny::tags$div(
      class = "episodic-shell",
      # The whole of the dashboard's navigation state, and the only
      # place it is held. `episodic-nav.js` writes these four; the
      # stylesheet reads the first two; nothing else stores a copy.
      # Shiny never replaces this element, which is the entire reason
      # they live here rather than on anything a `renderUI` produces.
      #
      # The two selections are separate because the two screens are: a
      # reader who opens an outbreak from an epidemic's dossier and
      # navigates back should find the epidemic they left still marked.
      `data-view` = "outbreaks",
      `data-nav` = "outbreaks",
      `data-outbreak` = "",
      `data-epidemic` = "",
      # Brand, navigation and status are siblings rather than the
      # navigation being nested inside a left-hand half: below 768px the
      # bar takes a row of its own beneath the other two, and `order`
      # only arranges elements against their own siblings.
      shiny::tags$header(
        class = "episodic-header",
        shiny::tags$span(
          class = "episodic-brand",
          title = episodic_tr("app.full_name", lang = lang),
          "EpiSODIC"
        ),
        # Rendered from the server rather than written once here only so
        # that it can be withheld from a visitor who has not signed in -
        # the navigation is a map of what there is to read. The links
        # are the same for every reader and the highlight is not in them
        # (see `episodic_ui_nav_link()`), so this renders once per access
        # state and a navigation never touches it.
        shiny::uiOutput(
          "nav_links",
          container = shiny::tags$nav,
          class = "episodic-nav",
          `aria-label` = episodic_tr("nav.menu_label", lang = lang)
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
      # Shown in place of every screen on an instance that requires a
      # sign-in. Its own output rather than a branch inside each screen:
      # the screens already return NULL to a visitor who may see
      # nothing, so this is the only thing left to draw.
      shiny::uiOutput("locked_screen"),
      shiny::tags$div(
        class = "episodic-screens",
        episodic_ui_screen(
          "outbreaks",
          shiny::tags$div(
            class = "episodic-body",
            # Shown only in the 768-1199px tier, where the rail slides
            # in over the dossier and assessment rather than sharing a
            # row with them: the rail is a list you consult and leave,
            # the assessment is what you write into while reading the
            # dossier, so the rail is what gets shed at that width.
            # Below 768px the segmented control's own first segment does
            # the same job, and above 1200px there is nothing to reveal.
            shiny::tags$button(
              type = "button",
              class = "episodic-rail-toggle",
              `data-episodic-pane` = "rail",
              episodic_tr("rail.title", lang = lang)
            ),
            shiny::tags$div(
              class = "episodic-pane-backdrop",
              `data-episodic-pane` = "dossier"
            ),
            # The three panes are the outputs themselves rather than
            # wrappers around elements carrying the geometry: an
            # `shiny::uiOutput()` renders a div of its own, and that div
            # is what `.episodic-body`'s flex layout actually lays out.
            # With `width`/`flex`/`overflow-y` one level further in, the
            # dossier's `flex: 1` addresses nothing and none of the
            # three can scroll inside itself. Carrying the geometry here
            # also gives each pane a real box for the recalculating
            # spinner, so a pane being rebuilt says so in place instead
            # of emptying.
            shiny::uiOutput(
              "rail_pane",
              container = shiny::tags$div,
              class = "episodic-pane episodic-pane-rail",
              # Which rail this is. Both surveillance screens carry one,
              # each holding its own selection, and `episodic-nav.js`
              # marks the current row within one of them rather than
              # across the document.
              `data-episodic-rail` = "outbreaks"
            ),
            shiny::uiOutput(
              "dossier_pane",
              container = shiny::tags$div,
              class = "episodic-pane episodic-pane-dossier"
            ),
            shiny::uiOutput(
              "assessment_pane",
              container = shiny::tags$div,
              class = "episodic-pane episodic-pane-assessment"
            ),
            episodic_ui_pane_switcher(lang = lang)
          )
        ),
        # The same three panes, for the coarse scale. Laid out here
        # rather than inside a screen-wide `uiOutput()`, for the reason
        # the Outbreaks screen is: an output that renders the whole body
        # replaces the rail's DOM, and with it its scroll position and
        # the row `episodic-nav.js` marked, every time the list of open
        # epidemics changes.
        episodic_ui_screen(
          "epidemics",
          shiny::tags$div(
            class = "episodic-body episodic-body-epidemics",
            shiny::tags$button(
              type = "button",
              class = "episodic-rail-toggle",
              `data-episodic-pane` = "rail",
              episodic_tr("nav.epidemics", lang = lang)
            ),
            shiny::tags$div(
              class = "episodic-pane-backdrop",
              `data-episodic-pane` = "dossier"
            ),
            shiny::uiOutput(
              "epidemic_rail_pane",
              container = shiny::tags$div,
              class = "episodic-pane episodic-pane-rail",
              `data-episodic-rail` = "epidemics"
            ),
            shiny::uiOutput(
              "epidemic_dossier_pane",
              container = shiny::tags$div,
              class = "episodic-pane episodic-pane-dossier"
            ),
            shiny::uiOutput(
              "epidemic_assessment_pane",
              container = shiny::tags$div,
              class = "episodic-pane episodic-pane-assessment"
            ),
            episodic_ui_pane_switcher(scale = "epidemic", lang = lang)
          )
        ),
        episodic_ui_screen("pathogens", shiny::uiOutput("pathogen_screen")),
        episodic_ui_screen("instance", shiny::uiOutput("instance_screen")),
        episodic_ui_screen("archive", shiny::uiOutput("archive_screen")),
        episodic_ui_screen("streams", shiny::uiOutput("streams_screen")),
        episodic_ui_screen("activity", shiny::uiOutput("activity_screen")),
        episodic_ui_screen(
          "performance",
          shiny::uiOutput("performance_screen")
        ),
        episodic_ui_screen("info", shiny::uiOutput("info_screen")),
        episodic_ui_screen("settings", shiny::uiOutput("settings_screen"))
      )
    )
  )
}

#' One screen of the dashboard
#'
#' A plain container carrying the view id it belongs to. The stylesheet
#' shows the one matching `.episodic-shell`'s `data-view` and hides the
#' rest; nothing here knows which of those it is.
#'
#' @param view The view id this screen is shown for.
#' @param ... The screen's content, normally a single `shiny::uiOutput()`.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_screen <- function(view, ...) {
  shiny::tags$div(class = "episodic-screen", `data-screen` = view, ...)
}

#' The top navigation links
#'
#' Four links, the same for every reader. Which one is lit is not in this
#' markup at all - see `episodic_ui_nav_link()` - so this renders once
#' per access state and never on a navigation.
#'
#' @param lang Session language.
#' @return A `shiny::tagList` of links.
#' @keywords internal
#' @noRd
episodic_ui_nav_links <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::tagList(lapply(
    c("outbreaks", "epidemics", "pathogens", "instance"),
    function(v) {
      episodic_ui_nav_link(v, episodic_tr(paste0("nav.", v), lang = lang))
    }
  ))
}

#' One top-navigation link
#'
#' It carries no highlight of its own. Which link is lit is derived by
#' the stylesheet from `data-nav` on `.episodic-shell`, so there is no
#' class here for a re-render to lose and no second writer to disagree
#' with the first. The click is picked up by `episodic-nav.js`'s one
#' delegated listener, which is why there is no `onclick` either: an
#' attribute of a few bytes rather than a line of JavaScript re-sent and
#' re-parsed with every render of the bar.
#'
#' @param view The view id this link switches to.
#' @param label The link's visible text.
#' @keywords internal
#' @noRd
episodic_ui_nav_link <- function(view, label) {
  shiny::tags$a(
    href = "#",
    class = "episodic-nav-link",
    `data-view` = view,
    `data-episodic-nav` = view,
    `data-nav` = episodic_app_nav_group(view),
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

#' Cache-busting query string for one file under `inst/app/www`
#'
#' `www/episodic.css` and `www/episodic-nav.js` are the same URL across
#' every request against the same running instance, so a browser that
#' already cached one keeps serving those bytes until something about
#' the URL itself changes. A version query string ties that to a
#' release, but a package reinstall is not the only time these files'
#' actual bytes change - an edit during development does too, with the
#' installed version left exactly where it was - so the key here is the
#' file's own modification time instead, read from wherever
#' `episodic_run_app()`'s `addResourcePath()` call points `www/` at:
#' the installed copy normally, or the source tree itself under
#' `devtools::load_all()`. Either way, this is the same file whose bytes
#' a browser would actually fetch, so the key changes exactly when they
#' do - on every edit in development, and on every reinstall for a
#' release, with no version bump required either way.
#'
#' @param filename A file name directly under `inst/app/www`.
#' @return A short cache-busting string, or the installed package
#'   version if the file cannot be found (should not happen in a working
#'   install; a missing asset is not this function's problem to raise).
#' @keywords internal
#' @noRd
episodic_app_asset_version <- function(filename) {
  path <- system.file("app", "www", filename, package = "EpiSODIC")
  if (!nzchar(path) || !file.exists(path)) {
    return(as.character(utils::packageVersion("EpiSODIC")))
  }
  as.character(as.integer(file.mtime(path)))
}
