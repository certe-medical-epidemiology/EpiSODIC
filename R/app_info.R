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

#' The package's own DESCRIPTION metadata, for the Info screen's about block
#'
#' Read live rather than hardcoded, so the about block never drifts from
#' the shipped `DESCRIPTION` the way a copy-pasted string would the next
#' time the version, description or URL changes there. `Title` is not
#' included: `episodic_tr("app.full_name")` already carries the same text,
#' pre-cleaned of `DESCRIPTION`'s line-wrap whitespace and already
#' translated, so the UI uses that instead of this function's own `Title`.
#'
#' @return A list with `version`, `description`, `license` and `url`
#'   (the first entry of `DESCRIPTION`'s comma-separated `URL` field, or
#'   `NA` if it has none), or `NULL` if package metadata cannot be read at
#'   all (not installed and not loaded via a tool that shims it).
#' @keywords internal
#' @noRd
episodic_app_package_meta <- function() {
  desc <- tryCatch(
    utils::packageDescription("EpiSODIC"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.list(desc)) {
    return(NULL)
  }
  urls <- trimws(strsplit(desc$URL %||% "", ",", fixed = TRUE)[[1]])
  urls <- urls[nzchar(urls)]
  list(
    version = as.character(utils::packageVersion("EpiSODIC")),
    # DESCRIPTION wraps long fields across lines with continuation
    # indentation; collapsed to single spaces so it reads as one paragraph.
    description = gsub("\\s+", " ", trimws(desc$Description %||% "")),
    license = desc$License %||% NA_character_,
    url = if (length(urls) > 0) urls[1] else NA_character_
  )
}

#' The "Info" screen
#'
#' Static reference material - what the detection algorithms actually do,
#' what the cluster states mean, who can see and do what - so an
#' epidemiologist reading "gedetecteerd door `same_place`" on a dossier
#' has somewhere in the app itself to look the term up. Content is
#' hardcoded (not read from the database), so unlike every other screen
#' this one needs no `con` argument.
#'
#' @param lang Session language: `"nl"`, `"en"`, `"es"`, `"fr"`, `"de"`,
#'   `"zh"`, `"hi"`, or `"ar"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_info_screen <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  meta <- episodic_app_package_meta()
  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$div(
      class = "episodic-info-about",
      shiny::img(src = "www/logo.svg"),
      shiny::tags$div(
        shiny::tags$div(
          class = "episodic-info-about-name",
          episodic_tr("app.title", lang = lang),
          if (!is.null(meta)) {
            shiny::tags$span(
              class = "episodic-info-about-version",
              paste0("v", meta$version)
            )
          }
        ),
        if (!is.null(meta) && nzchar(meta$description)) {
          shiny::tags$p(
            class = "episodic-info-about-desc",
            shiny::HTML(gsub("<doi:(.*?)>", '<a href="https://doi.org/\\1" target="_blank">[link]</a>', meta$description))
          )
        },
        shiny::tags$div(
          class = "episodic-info-about-links",
          shiny::tags$p("Developed by Matthijs Berends et al, 2026.")
        ),
        if (!is.null(meta)) {
          shiny::tags$div(
            class = "episodic-info-about-links",
            if (!is.na(meta$url)) {
              shiny::tags$a(
                href = meta$url,
                target = "_blank",
                rel = "noopener noreferrer",
                episodic_tr("info.about.website", lang = lang)
              )
            },
            if (!is.na(meta$license)) {
              shiny::tags$span(
                episodic_tr(
                  "info.about.license",
                  license = meta$license,
                  lang = lang
                )
              )
            }
          )
        }
      )
    ),
    shiny::tags$h1(
      style = "font-size:22px;font-weight:600;margin-bottom:4px;",
      episodic_tr("info.title", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:16px;",
      episodic_tr("info.intro", lang = lang)
    ),
    episodic_ui_panel(
      episodic_tr("info.algorithms.title", lang = lang),
      note = shiny::HTML(episodic_tr("info.algorithms.note", lang = lang)),
      episodic_ui_info_algorithms_table(lang = lang)
    ),
    episodic_ui_panel(
      episodic_tr("info.states.title", lang = lang),
      episodic_ui_info_states_table(lang = lang)
    ),
    episodic_ui_panel(
      episodic_tr("info.access.title", lang = lang),
      shiny::tags$p(
        class = "episodic-panel-note",
        episodic_tr("info.access.body", lang = lang)
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_info_algorithms_table <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  rows <- list(
    list(
      name = "farringtonFlexible",
      type = episodic_tr("info.algorithms.type.baseline", lang = lang),
      key = "info.algorithms.farrington"
    ),
    list(
      name = "same_place",
      type = episodic_tr("info.algorithms.type.rule", lang = lang),
      key = "info.algorithms.same_place"
    ),
    list(
      name = "rare_trigger",
      type = episodic_tr("info.algorithms.type.rule", lang = lang),
      key = "info.algorithms.rare_trigger"
    ),
    # `mem` has been firing detections all along without appearing here,
    # so an epidemiologist reading "detected by mem" on a dossier had nowhere
    # in the app to look the term up - the exact gap this screen exists
    # to close.
    list(
      name = "mem",
      type = episodic_tr("info.algorithms.type.seasonal", lang = lang),
      key = "info.algorithms.mem"
    )
  )
  shiny::tags$table(
    class = "episodic-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(episodic_tr("info.algorithms.col.name", lang = lang)),
      shiny::tags$th(episodic_tr("info.algorithms.col.type", lang = lang)),
      shiny::tags$th(episodic_tr("info.algorithms.col.how", lang = lang))
    )),
    shiny::tags$tbody(
      lapply(rows, function(r) {
        shiny::tags$tr(
          shiny::tags$td(shiny::HTML(episodic_ui_code_join(r$name))),
          shiny::tags$td(r$type),
          shiny::tags$td(shiny::HTML(episodic_tr(r$key, lang = lang)))
        )
      })
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_info_states_table <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  states <- c(
    "new",
    "assessing",
    "monitoring",
    "closable",
    "reassess",
    "closed"
  )
  shiny::tags$table(
    class = "episodic-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(episodic_tr("info.states.col.state", lang = lang)),
      shiny::tags$th(episodic_tr("info.states.col.meaning", lang = lang))
    )),
    shiny::tags$tbody(
      lapply(states, function(s) {
        shiny::tags$tr(
          shiny::tags$td(episodic_ui_chip(
            episodic_tr(paste0("state.", s), lang = lang),
            episodic_ui_state_colour(s),
            filled = TRUE
          )),
          shiny::tags$td(episodic_tr(paste0("info.states.", s), lang = lang))
        )
      })
    )
  )
}
