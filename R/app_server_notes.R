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

#' Wire the dossier's cluster-notes save button and History modal
#'
#' Two independent observers:
#'
#' * `note_save_submit` re-resolves `current_user()` with
#'   `episodic_auth_refresh_user()` and requires only that *someone* is
#'   signed in - unlike
#'   `episodic_app_server_report()`/`episodic_app_server_assessment_actions()`,
#'   this is deliberately not further gated on
#'   `episodic_user_is_epidemiologist()`: the notes panel is open to any
#'   role. The DOM/onclick is still not a trust boundary, so a session
#'   whose account was deactivated mid-session is still refused
#'   server-side.
#' * `note_history_open` shows the change-history modal
#'   (`episodic_ui_notes_history_modal()`); reading history needs no
#'   sign-in, since the live note itself is visible without one, but is
#'   still gated on `access_granted()` for the same reason the run-detail
#'   modal in `app_server.R` is - the onclick is not a trust boundary
#'   either.
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @param notes_version A `shiny::reactiveVal` bumped on a successful save
#'   to invalidate `output$notes_pane` (see `app_server.R`) - unlike the
#'   `selected_cluster_id` toggle this used to piggyback on, it leaves the
#'   rest of the dossier, several panels of which are plots, untouched.
#' @param access_granted A `shiny::reactive` as returned by
#'   `episodic_app_access_granted()`.
#' @param lang Session language.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episodic_app_server_notes <- function(input,
                                      output,
                                      session,
                                      con,
                                      current_user,
                                      notes_version,
                                      access_granted,
                                      lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::observeEvent(input$note_save_submit, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(!is.null(user))

    payload <- input$note_save_submit
    note_text <- if (is.null(payload$note_text)) "" else payload$note_text
    episodic_db_cluster_note_insert(
      con,
      cluster_id = as.integer(payload$cluster_id),
      user_id = user$user_id,
      note_text = note_text
    )

    notes_version(notes_version() + 1L)
  })

  shiny::observeEvent(input$note_history_open, {
    shiny::req(access_granted())
    shiny::showModal(episodic_ui_notes_history_modal(
      con,
      as.integer(input$note_history_open),
      lang = lang
    ))
  })

  invisible(NULL)
}
