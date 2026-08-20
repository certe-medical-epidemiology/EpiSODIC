#' Classification, closure and mute write actions
#'
#' Wires the assessment form's submit buttons, including
#' `bulk_assess_submit` (the rail's multi-select bar - "often they are
#' artefacts", one classification and rationale applied to every checked
#' cluster in a loop over [episode_app_submit_assessment()], each getting
#' its own `episode_assessment_event` row exactly as a one-at-a-time
#' classification would). Every handler re-checks
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
#' @param db_touch A zero-argument function that bumps the session's
#'   shared `db_version` counter, so read models that do not depend on
#'   `selected_cluster_id` (the rail's own open-cluster count, the
#'   Archief screen) also notice a write happened. Without this, closing
#'   or classifying a cluster left both silently stale until something
#'   unrelated (a `view()` change) happened to force a recompute.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episode_app_server_assessment_actions <- function(input, output, session, con, lang = "nl",
                                                    current_user, selected_cluster_id, db_touch) {
  refresh <- function() {
    # Re-trigger the dossier/assessment renderers by "reselecting" the
    # same cluster - both read from the database, not from this value's
    # identity, so this is a cheap way to invalidate them after a write.
    id <- selected_cluster_id()
    selected_cluster_id(NULL)
    selected_cluster_id(id)
    db_touch()
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

  shiny::observeEvent(input$bulk_assess_submit, {
    user <- current_user()
    shiny::req(user)
    payload <- input$bulk_assess_submit
    rationale <- trimws(payload$rationale %||% "")
    cluster_ids <- payload$cluster_ids
    if (!nzchar(rationale) || length(cluster_ids) == 0) {
      return(invisible(NULL))  # mandatory rationale, same rule as the single-cluster form; client also enforces both
    }
    verdict <- if (nzchar(payload$verdict %||% "")) payload$verdict else NA
    for (cluster_id in cluster_ids) {
      episode_app_submit_assessment(con, cluster_id = cluster_id, user_id = user$user_id,
                                     verdict = verdict, rationale = rationale)
    }
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
