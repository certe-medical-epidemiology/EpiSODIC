#' Classification, closure and mute write actions
#'
#' Wires the assessment form's submit buttons. Every handler re-checks
#' `current_user()` server-side before writing anything - the UI only
#' renders these controls for a signed-in session, but a forged client
#' event must not be able to write regardless (the DOM and `onclick`
#' handlers are not a trust boundary).
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language.
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @param selected_cluster_id A `shiny::reactiveVal` holding the rail's
#'   current selection, used to force the dossier/assessment panes to
#'   refresh after a write (the underlying cluster's state may have
#'   changed).
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episode_app_server_assessment_actions <- function(input, output, session, con, lang = "nl",
                                                    current_user, selected_cluster_id) {
  refresh <- function() {
    # Re-trigger the dossier/assessment renderers by "reselecting" the
    # same cluster - both read from the database, not from this value's
    # identity, so this is a cheap way to invalidate them after a write.
    id <- selected_cluster_id()
    selected_cluster_id(NULL)
    selected_cluster_id(id)
  }

  shiny::observeEvent(input$assess_submit, {
    user <- current_user()
    shiny::req(user)
    payload <- input$assess_submit
    rationale <- trimws(payload$rationale %||% "")
    if (!nzchar(rationale)) {
      return(invisible(NULL))  # mandatory rationale (ARCHITECTURE.md section 5.6); client also enforces this
    }
    episode_app_submit_assessment(
      con, cluster_id = payload$cluster_id, user_id = user$user_id,
      verdict = if (nzchar(payload$verdict %||% "")) payload$verdict else NA,
      rationale = rationale,
      # wpg_notifiable/ggd_informed are left at episode_app_submit_assessment()'s
      # own NA default - the form no longer collects them (Wpg and GGD are
      # Netherlands-specific, out of scope for a general-purpose deployment).
      snooze_until = if (nzchar(payload$snooze %||% "")) payload$snooze else NA
    )
    refresh()
  })

  shiny::observeEvent(input$assess_close, {
    user <- current_user()
    shiny::req(user)
    episode_app_submit_closure(con, cluster_id = input$assess_close, user_id = user$user_id)
    refresh()
  })

  shiny::observeEvent(input$assess_mute_submit, {
    user <- current_user()
    shiny::req(user)
    payload <- input$assess_mute_submit
    if (!nzchar(payload$muted_from %||% "") || !nzchar(payload$muted_until %||% "")) {
      return(invisible(NULL))
    }
    episode_db_stream_mute_insert(
      con, stream_id = payload$stream_id, muted_from = payload$muted_from,
      muted_until = payload$muted_until, reason = payload$reason, user_id = user$user_id
    )
    refresh()
  })

  invisible(NULL)
}
