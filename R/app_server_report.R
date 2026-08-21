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

#' Wire the dossier's report-render-on-demand button
#'
#' Re-checks `current_user()` server-side before rendering, same as every
#' other write action (the DOM/onclick is not a trust boundary). Reports
#' are written to `<directory containing db_path>/reports/`, so a report
#' lands next to the database rather than needing its own configuration
#' knob.
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param db_path Path to the SQLite database (used only to derive the
#'   sibling `reports/` directory).
#' @param lang Session language.
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @param selected_cluster_id A `shiny::reactiveVal`, re-triggered after a
#'   successful render so the dossier redraws with the new version listed.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episode_app_server_report <- function(input, output, session, con, db_path, lang = "nl",
                                        current_user, selected_cluster_id) {
  render_error <- shiny::reactiveVal(NULL)

  output$report_render_error <- shiny::renderUI({
    msg <- render_error()
    if (is.null(msg)) return(NULL)
    shiny::tagList(
      shiny::tags$div(class = "episode-form-error", msg),
      # The render button is disabled and the "generating" text shown
      # client-side, at click time (episode_ui_report_panel()) - a
      # synchronous server-side render blocks the whole session, so
      # nothing pushed from an observer can appear before it finishes.
      # On success the dossier pane re-renders wholesale, which already
      # clears both; on error it does not, so reset them here instead.
      shiny::tags$script(shiny::HTML(paste0(
        "(function(){var b=document.getElementById('report-render-button'); if(b) b.disabled=false; ",
        "var p=document.getElementById('report-render-pending'); if(p) p.style.display='none';})();"
      )))
    )
  })

  shiny::observeEvent(input$report_render_submit, {
    user <- current_user()
    shiny::req(user)
    render_error(NULL)

    result <- tryCatch(
      episode_report_render(con, cluster_id = input$report_render_submit,
                             output_dir = file.path(dirname(db_path), "reports"),
                             user_id = user$user_id, lang = lang),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      render_error(rlang::cnd_message(result, inherit = TRUE))
    } else {
      id <- selected_cluster_id()
      selected_cluster_id(NULL)
      selected_cluster_id(id)
    }
  })

  invisible(NULL)
}
