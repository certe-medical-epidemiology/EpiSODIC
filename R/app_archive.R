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

# Archive and Activity screens: read-only, reached from the top
# navigation. Neither needs a sign-in - last winter's assessment is
# exactly as useful a precedent for an anonymous viewer as it is for a
# signed-in assessor.

#' The Archive screen
#'
#' @param archive A data frame from `episodic_app_archive()`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_archive_screen <- function(archive, lang = "nl") {
  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(
      style = "font-size:22px;font-weight:600;margin-bottom:4px;",
      episodic_tr("archive.title", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:16px;",
      episodic_tr("archive.note", lang = lang)
    ),
    shiny::tags$input(
      type = "text",
      class = "episodic-search-input",
      id = "archive_search_input",
      placeholder = episodic_tr("archive.search_placeholder", lang = lang),
      oninput = "Shiny.setInputValue('archive_search', this.value, {priority: 'event'})"
    ),
    if (nrow(archive) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("archive.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("column.cluster", lang = lang)),
          shiny::tags$th(episodic_tr("archive.col.pathogen", lang = lang)),
          shiny::tags$th(episodic_tr("archive.col.level", lang = lang)),
          shiny::tags$th(episodic_tr("archive.col.place", lang = lang)),
          shiny::tags$th(episodic_tr("archive.col.cases", lang = lang)),
          shiny::tags$th(episodic_tr("archive.col.priority", lang = lang)),
          shiny::tags$th(episodic_tr("archive.col.closed_at", lang = lang))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(archive)), function(i) {
            row <- archive[i, ]
            # Reachable, not just listed. Last winter's assessment is only
            # a useful precedent if you can open it and read the reasoning,
            # and until now the archive named clusters it gave no way in to.
            episodic_ui_cluster_link_row(
              row$cluster_id,
              lang = lang,
              shiny::tags$td(shiny::HTML(episodic_ui_italicise_taxon(
                row$pathogen
              ))),
              shiny::tags$td(row$level_label),
              shiny::tags$td(row$place),
              shiny::tags$td(row$n_cases),
              shiny::tags$td(round(row$priority_score, 0)),
              shiny::tags$td(
                if (is.na(row$closed_at)) {
                  episodic_tr("misc.unknown", lang = lang)
                } else {
                  episodic_ui_format_datetime(row$closed_at, fmt = "%d-%m-%Y")
                }
              )
            )
          })
        )
      )
    }
  )
}

#' The Activity screen
#'
#' @param activity A data frame from `episodic_app_activity_log()`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_activity_screen <- function(activity, lang = "nl") {
  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(
      style = "font-size:22px;font-weight:600;margin-bottom:4px;",
      episodic_tr("activity.title", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:16px;",
      episodic_tr("activity.note", lang = lang)
    ),
    if (nrow(activity) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("activity.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("activity.col.time", lang = lang)),
          shiny::tags$th(episodic_tr("activity.col.actor", lang = lang)),
          shiny::tags$th(episodic_tr("activity.col.action", lang = lang)),
          shiny::tags$th(episodic_tr("activity.col.target", lang = lang))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(activity)), function(i) {
            row <- activity[i, ]
            shiny::tags$tr(
              class = if (isTRUE(row$is_system)) {
                "episodic-activity-row system"
              } else {
                "episodic-activity-row"
              },
              shiny::tags$td(episodic_ui_format_datetime(
                row$at,
                fmt = "%d-%m-%Y %H:%M"
              )),
              shiny::tags$td(row$actor),
              shiny::tags$td(
                row$action,
                # What a run took in, under the run's own line: an operator
                # reading the log should not have to query the database to
                # find out whether last night's extract arrived in full.
                if (!is.na(row$detail)) {
                  shiny::tags$div(
                    style = "font-size:11.5px;color:var(--episodic-muted);margin-top:2px;",
                    row$detail
                  )
                },
                # And everything else the run recorded - the per-feed
                # counts, the provenance, and the whole failure message -
                # one click away, so the reason a dashboard is empty never
                # lives only in a console somebody else was looking at.
                if (!is.null(row$run_id) && !is.na(row$run_id)) {
                  shiny::tags$button(
                    class = "episodic-btn episodic-run-detail-btn",
                    onclick = sprintf(
                      "Shiny.setInputValue('activity_run_detail', %d, {priority: 'event'})",
                      as.integer(row$run_id)
                    ),
                    episodic_tr("activity.detail_button", lang = lang)
                  )
                }
              ),
              shiny::tags$td(
                if (is.na(row$target)) {
                  episodic_tr("misc.dash", lang = lang)
                } else {
                  row$target
                }
              )
            )
          })
        )
      )
    }
  )
}

#' Everything one detection run recorded, as a modal
#'
#' The Activity screen shows a run as one line; this is the rest of it.
#' For a failed run the whole recorded message is here verbatim - the same
#' text [episodic_check_cases()] would have printed - because somebody
#' looking at an empty dashboard needs to read it, and may well not be the
#' person who scheduled the run.
#'
#' @param run One row of `episodic_detection_run`.
#' @param lang Session language.
#' @return A `shiny::modalDialog`.
#' @keywords internal
#' @noRd
episodic_ui_run_modal <- function(run, lang = "nl") {
  heading <- function(key) {
    shiny::tags$div(
      class = "episodic-run-modal-heading",
      episodic_tr(key, lang = lang)
    )
  }
  moment <- function(at) {
    episodic_ui_format_datetime(at, fmt = "%d-%m-%Y %H:%M")
  }
  unknown <- episodic_tr("misc.unknown", lang = lang)
  load_summary <- episodic_app_run_load_summary(run, lang = lang)
  failed <- identical(run$status, "failed") &&
    !is.null(run$error_text) &&
    !is.na(run$error_text) &&
    nzchar(run$error_text)

  shiny::modalDialog(
    title = episodic_tr(
      "activity.run_modal_title",
      id = run$run_id,
      lang = lang
    ),
    easyClose = TRUE,
    size = "l",
    footer = shiny::tags$button(
      class = "episodic-btn",
      `data-dismiss` = "modal",
      episodic_tr("misc.close", lang = lang)
    ),
    shiny::tags$div(
      class = "episodic-run-modal-when",
      episodic_tr(paste0("activity.action_run_", run$status), lang = lang),
      " \u00b7 ",
      episodic_tr(
        "activity.run_modal_when",
        started = moment(run$started_at),
        finished = moment(run$finished_at),
        lang = lang
      )
    ),
    heading("activity.run_modal_load"),
    shiny::tags$p(
      if (is.na(load_summary)) {
        episodic_tr("activity.run_modal_no_load", lang = lang)
      } else {
        load_summary
      }
    ),
    if (!is.null(run$n_streams) && !is.na(run$n_streams)) {
      shiny::tagList(
        heading("activity.run_modal_detection"),
        shiny::tags$p(episodic_tr(
          "activity.run_modal_detection_line",
          streams = run$n_streams,
          detections = run$n_detections %||% 0,
          new = run$n_signals_new %||% 0,
          updated = run$n_signals_updated %||% 0,
          lang = lang
        ))
      )
    },
    if (isTRUE(failed)) {
      shiny::tagList(
        heading("activity.run_modal_error"),
        # Pre-formatted, deliberately: the recorded message is written to
        # be read as it was written, one numbered problem per line.
        shiny::tags$pre(class = "episodic-run-modal-error", run$error_text),
        shiny::tags$p(
          class = "episodic-run-modal-hint",
          episodic_tr("status.run_failed_hint", lang = lang)
        )
      )
    },
    shiny::tags$div(
      class = "episodic-run-modal-provenance",
      episodic_tr(
        "activity.run_modal_provenance",
        host = run$host,
        account = run$account,
        version = run$code_version %||% unknown,
        hash = substr(run$config_hash %||% unknown, 1, 12),
        lang = lang
      )
    )
  )
}
