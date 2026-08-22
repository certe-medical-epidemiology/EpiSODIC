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
  shiny::tags$div(
    class = "episodic-streams-screen",
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
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
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
    # so an assessor reading "detected by mem" on a dossier had nowhere
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
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
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
            episodic_ui_state_colour(s)
          )),
          shiny::tags$td(episodic_tr(paste0("info.states.", s), lang = lang))
        )
      })
    )
  )
}
