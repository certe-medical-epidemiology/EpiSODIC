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
#'   to invalidate `output$notes_pane` (see `app_server.R`) and nothing
#'   else - a toggle of `selected_cluster_id` would redraw the whole
#'   dossier, several panels of which are plots.
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
    cluster_id <- as.integer(payload$cluster_id)
    note_text <- trimws(
      if (is.null(payload$note_text)) "" else as.character(payload$note_text)
    )
    if (!episodic_note_is_new(con, cluster_id, note_text)) {
      return(invisible(NULL))
    }
    episodic_db_cluster_note_insert(
      con,
      cluster_id = cluster_id,
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

#' Whether a submitted note says something the record does not already
#'
#' `episodic_cluster_note` is append-only, so every save is a version in
#' the history modal and in the Activity log. A save that repeats the
#' note already on file therefore does not record that nothing changed -
#' it records a change that did not happen, over the name and timestamp
#' of whoever pressed the button, and the next reader cannot tell the
#' two apart. Pressing Save on an unedited panel is the ordinary way
#' that arises.
#'
#' Two things are refused, then: text identical to the most recent note
#' (compared after `trimws()`, so re-indenting is not a version), and an
#' empty note on a cluster that has never had one, which would open a
#' history with a blank first entry. Emptying a note that does say
#' something is a deliberate act and is recorded like any other.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The cluster the note belongs to.
#' @param note_text The submitted text, already trimmed.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_note_is_new <- function(con, cluster_id, note_text) {
  current <- episodic_db_cluster_note_current(con, cluster_id)
  if (nrow(current) == 0) {
    return(nzchar(note_text))
  }
  !identical(note_text, trimws(as.character(current$note_text[1])))
}
