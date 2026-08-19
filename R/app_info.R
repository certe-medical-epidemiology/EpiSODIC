#' The "Info" screen
#'
#' Static reference material - what the detection algorithms actually do,
#' what the cluster states mean, who can see and do what - so an
#' epidemiologist reading "gedetecteerd door `same_place`" on a dossier has
#' somewhere to look the term up without reading `ARCHITECTURE.md` itself.
#' Content is hardcoded (not read from the database), so unlike every other
#' screen this one needs no `con` argument.
#'
#' @param lang Session language, `"nl"` (default) or `"en"`.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episode_ui_info_screen <- function(lang = "nl") {
  shiny::tags$div(
    class = "episode-streams-screen",
    shiny::tags$h1(style = "font-size:22px;font-weight:600;margin-bottom:4px;", episode_tr("info.title", lang = lang)),
    shiny::tags$p(style = "font-size:12.5px;color:var(--episode-muted);margin-bottom:16px;", episode_tr("info.intro", lang = lang)),
    episode_ui_panel(
      episode_tr("info.algorithms.title", lang = lang),
      note = shiny::HTML(episode_tr("info.algorithms.note", lang = lang)),
      episode_ui_info_algorithms_table(lang = lang)
    ),
    episode_ui_panel(
      episode_tr("info.states.title", lang = lang),
      episode_ui_info_states_table(lang = lang)
    ),
    episode_ui_panel(
      episode_tr("info.access.title", lang = lang),
      shiny::tags$p(class = "episode-panel-note", episode_tr("info.access.body", lang = lang))
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_info_algorithms_table <- function(lang = "nl") {
  rows <- list(
    list(name = "farringtonFlexible", type = episode_tr("info.algorithms.type.baseline", lang = lang),
         key = "info.algorithms.farrington"),
    list(name = "same_place", type = episode_tr("info.algorithms.type.rule", lang = lang),
         key = "info.algorithms.same_place"),
    list(name = "rare_trigger", type = episode_tr("info.algorithms.type.rule", lang = lang),
         key = "info.algorithms.rare_trigger")
  )
  shiny::tags$table(
    class = "episode-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(episode_tr("info.algorithms.col.name", lang = lang)),
      shiny::tags$th(episode_tr("info.algorithms.col.type", lang = lang)),
      shiny::tags$th(episode_tr("info.algorithms.col.how", lang = lang))
    )),
    shiny::tags$tbody(
      lapply(rows, function(r) {
        shiny::tags$tr(
          shiny::tags$td(shiny::HTML(episode_ui_code_join(r$name))),
          shiny::tags$td(r$type),
          shiny::tags$td(episode_tr(r$key, lang = lang))
        )
      })
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_info_states_table <- function(lang = "nl") {
  states <- c("new", "assessing", "monitoring", "closable", "reassess", "closed")
  shiny::tags$table(
    class = "episode-table",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(episode_tr("info.states.col.state", lang = lang)),
      shiny::tags$th(episode_tr("info.states.col.meaning", lang = lang))
    )),
    shiny::tags$tbody(
      lapply(states, function(s) {
        shiny::tags$tr(
          shiny::tags$td(episode_ui_chip(episode_tr(paste0("state.", s), lang = lang), episode_ui_state_colour(s))),
          shiny::tags$td(episode_tr(paste0("info.states.", s), lang = lang))
        )
      })
    )
  )
}
