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

# Archive and Activity screens: read-only, reached from the top
# navigation. Neither needs a sign-in - last winter's assessment is
# exactly as useful a precedent for an anonymous viewer as it is for a
# signed-in assessor.

#' The Archive screen
#'
#' @param archive A data frame from `episode_app_archive()`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episode_ui_archive_screen <- function(archive, lang = "nl") {
  shiny::tags$div(
    class = "episode-streams-screen",
    shiny::tags$h1(style = "font-size:22px;font-weight:600;margin-bottom:4px;", episode_tr("archive.title", lang = lang)),
    shiny::tags$p(style = "font-size:12.5px;color:var(--episode-muted);margin-bottom:16px;", episode_tr("archive.note", lang = lang)),
    shiny::tags$input(
      type = "text", class = "episode-search-input", id = "archive_search_input",
      placeholder = episode_tr("archive.search_placeholder", lang = lang),
      oninput = "Shiny.setInputValue('archive_search', this.value, {priority: 'event'})"
    ),
    if (nrow(archive) == 0) {
      shiny::tags$p(class = "episode-panel-empty", episode_tr("archive.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episode-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episode_tr("archive.col.pathogen", lang = lang)),
          shiny::tags$th(episode_tr("archive.col.level", lang = lang)),
          shiny::tags$th(episode_tr("archive.col.place", lang = lang)),
          shiny::tags$th(episode_tr("archive.col.cases", lang = lang)),
          shiny::tags$th(episode_tr("archive.col.priority", lang = lang)),
          shiny::tags$th(episode_tr("archive.col.closed_at", lang = lang))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(archive)), function(i) {
            row <- archive[i, ]
            shiny::tags$tr(
              shiny::tags$td(shiny::HTML(episode_ui_italicise_taxon(row$pathogen))),
              shiny::tags$td(row$level_label),
              shiny::tags$td(row$place),
              shiny::tags$td(row$n_cases),
              shiny::tags$td(round(row$priority_score, 0)),
              shiny::tags$td(if (is.na(row$closed_at)) episode_tr("misc.unknown", lang = lang) else episode_ui_format_datetime(row$closed_at, fmt = "%d-%m-%Y"))
            )
          })
        )
      )
    }
  )
}

#' The Activity screen
#'
#' @param activity A data frame from `episode_app_activity_log()`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episode_ui_activity_screen <- function(activity, lang = "nl") {
  shiny::tags$div(
    class = "episode-streams-screen",
    shiny::tags$h1(style = "font-size:22px;font-weight:600;margin-bottom:4px;", episode_tr("activity.title", lang = lang)),
    shiny::tags$p(style = "font-size:12.5px;color:var(--episode-muted);margin-bottom:16px;", episode_tr("activity.note", lang = lang)),
    if (nrow(activity) == 0) {
      shiny::tags$p(class = "episode-panel-empty", episode_tr("activity.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episode-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episode_tr("activity.col.time", lang = lang)),
          shiny::tags$th(episode_tr("activity.col.actor", lang = lang)),
          shiny::tags$th(episode_tr("activity.col.action", lang = lang)),
          shiny::tags$th(episode_tr("activity.col.target", lang = lang))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(activity)), function(i) {
            row <- activity[i, ]
            shiny::tags$tr(
              class = if (isTRUE(row$is_system)) "episode-activity-row system" else "episode-activity-row",
              shiny::tags$td(episode_ui_format_datetime(row$at, fmt = "%d-%m-%Y %H:%M")),
              shiny::tags$td(row$actor),
              shiny::tags$td(row$action),
              shiny::tags$td(if (is.na(row$target)) episode_tr("misc.dash", lang = lang) else row$target)
            )
          })
        )
      )
    }
  )
}
