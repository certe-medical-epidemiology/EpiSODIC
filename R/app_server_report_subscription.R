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

#' Wire the dossier's scheduled-report form (set and cancel)
#'
#' Same shape as `episodic_app_server_report()`: re-resolves
#' `current_user()` and requires `episodic_user_is_epidemiologist()`
#' server-side (the onclick is not a trust boundary), then a plain
#' `episodic_db_report_subscription_event_insert()` - never an update,
#' matching every other app write. `channel` is re-validated against the
#' currently enabled, email-capable channels rather than trusted from the
#' client: a channel disabled after the form was drawn must not be
#' schedulable just because the `<select>` still lists it.
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @param selected_cluster_id A `shiny::reactiveVal`, re-triggered on a
#'   successful save/cancel so the dossier redraws with the new schedule
#'   shown - the same toggle trick `episodic_app_server_report()` uses.
#' @param lang Session language.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episodic_app_server_report_subscription <- function(input,
                                                    output,
                                                    session,
                                                    con,
                                                    current_user,
                                                    selected_cluster_id,
                                                    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  schedule_error <- shiny::reactiveVal(NULL)

  output$report_schedule_error <- shiny::renderUI({
    msg <- schedule_error()
    if (is.null(msg)) {
      return(NULL)
    }
    shiny::tags$div(class = "episodic-form-error", msg)
  })

  redraw <- function() {
    id <- selected_cluster_id()
    selected_cluster_id(NULL)
    selected_cluster_id(id)
  }

  shiny::observeEvent(input$report_schedule_save, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_epidemiologist(user))
    schedule_error(NULL)

    payload <- input$report_schedule_save
    cluster_id <- as.integer(payload$cluster_id)

    interval_days <- suppressWarnings(as.integer(payload$interval_days))
    parsed <- episodic_report_subscription_parse_recipients(payload$recipients)
    config <- episodic_config_resolve(con = con)
    available_channels <- episodic_report_subscription_available_channels(config)
    channel <- payload$channel

    problem <- if (is.na(interval_days) || interval_days < 1) {
      episodic_tr("panel.report.schedule_interval_invalid", lang = lang)
    } else if (length(parsed$invalid) > 0) {
      episodic_tr(
        "panel.report.schedule_invalid_recipients",
        list = paste(parsed$invalid, collapse = ", "),
        lang = lang
      )
    } else if (length(parsed$valid) == 0) {
      episodic_tr("panel.report.schedule_recipients_required", lang = lang)
    } else if (!(channel %in% available_channels)) {
      episodic_tr("panel.report.schedule_no_channel", lang = lang)
    } else {
      NULL
    }

    if (!is.null(problem)) {
      schedule_error(problem)
      return(invisible(NULL))
    }

    episodic_db_report_subscription_event_insert(
      con,
      cluster_id = cluster_id,
      user_id = user$user_id,
      action = "set",
      interval_days = interval_days,
      recipients_json = episodic_report_subscription_recipients_to_json(parsed$valid),
      channel = channel,
      include_linelist = isTRUE(payload$include_linelist)
    )
    redraw()
  })

  shiny::observeEvent(input$report_schedule_cancel, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_epidemiologist(user))
    schedule_error(NULL)

    episodic_db_report_subscription_event_insert(
      con,
      cluster_id = as.integer(input$report_schedule_cancel),
      user_id = user$user_id,
      action = "cancel"
    )
    redraw()
  })

  invisible(NULL)
}
