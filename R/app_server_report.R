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
#' Re-resolves `current_user()` with `episodic_auth_refresh_user()` and
#' re-checks `episodic_user_is_epidemiologist()` server-side before
#' rendering, same as every other write action (the DOM/onclick is not a
#' trust boundary, and an account deactivated or demoted mid-session must
#' not keep writing on its already-open session). The report is written
#' to the directory `episodic_report_output_dir()` resolves - by default
#' `<directory containing db_path>/reports/`, next to the database,
#' unless `report.output_dir` is configured.
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param db_path Path to the database, or a MariaDB DSN - passed to
#'   `episodic_report_output_dir()` to resolve the `reports/` directory.
#' @param lang Session language.
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @param selected_cluster_id A `shiny::reactiveVal`, re-triggered after a
#'   successful render so the dossier redraws with the new version listed.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episodic_app_server_report <- function(input,
                                       output,
                                       session,
                                       con,
                                       db_path,
                                       lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                       current_user,
                                       selected_cluster_id) {
  render_error <- shiny::reactiveVal(NULL)

  output$report_render_error <- shiny::renderUI({
    msg <- render_error()
    if (is.null(msg)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::tags$div(class = "episodic-form-error", msg),
      # The render button is disabled and the "generating" text shown
      # client-side, at click time (episodic_ui_report_panel()) - a
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
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_epidemiologist(user))
    render_error(NULL)

    result <- tryCatch(
      {
        output_dir <- episodic_report_output_dir(
          episodic_config_resolve(con = con),
          db_path,
          "reports"
        )
        episodic_report_render(
          con,
          cluster_id = input$report_render_submit,
          output_dir = output_dir,
          user_id = user$user_id,
          lang = lang
        )
      },
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

#' A link builder for the dossier's rendered report versions
#'
#' Rendered reports live outside the app's static resources, wherever
#' `episodic_report_output_dir()` put them, and carry the case line list
#' by default. They are therefore served through this session rather than
#' through `addResourcePath()`: a static path would hand any rendered
#' report to anyone who knows or guesses its URL, with no sign-in behind
#' it. Each request is answered by `episodic_app_report_response()`,
#' which re-checks the session's account at the time of the request.
#'
#' The data object is registered on the first call rather than when the
#' session starts, so a session that never shows a rendered report never
#' opens the route.
#'
#' @param session The Shiny session.
#' @param con A [DBI::DBIConnection-class].
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @param lang Session language.
#' @return A function of one `report_id`, returning the URL (relative to
#'   the app) that serves that report version.
#' @keywords internal
#' @noRd
episodic_app_report_href <- function(session,
                                     con,
                                     current_user,
                                     lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  base_url <- NULL
  function(report_id) {
    if (is.null(base_url)) {
      base_url <<- session$registerDataObj(
        "episodic_report",
        NULL,
        function(data, req) {
          query <- shiny::parseQueryString(req$QUERY_STRING %||% "")
          episodic_app_report_response(
            con,
            user = shiny::isolate(current_user()),
            report_id = query$report_id,
            lang = lang
          )
        }
      )
    }
    paste0(base_url, "&report_id=", as.integer(report_id))
  }
}

#' The HTTP response for one rendered report version
#'
#' Serves the report only to a signed-in account that is still active,
#' the same condition the dossier's line list is shown on, since a
#' rendered report carries that line list by default. The file is served
#' only when its bytes still hash to the `file_sha256` recorded at render
#' time: a report whose file has been replaced or altered since is
#' refused, never shown under a version number it no longer matches.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user The session's account row, or `NULL`.
#' @param report_id The requested report id, as a string from the query.
#' @param lang Session language.
#' @return A `shiny::httpResponse()`.
#' @keywords internal
#' @noRd
episodic_app_report_response <- function(con,
                                         user,
                                         report_id,
                                         lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  refuse <- function(status, key) {
    shiny::httpResponse(
      status,
      "text/plain; charset=UTF-8",
      episodic_tr(key, lang = lang)
    )
  }
  if (is.null(episodic_auth_refresh_user(con, user))) {
    return(refuse(403L, "panel.report.forbidden"))
  }
  if (length(report_id) != 1 || is.na(report_id) ||
    !grepl("^[0-9]+$", report_id)) {
    return(refuse(404L, "panel.report.not_found"))
  }
  row <- episodic_db_report_render_by_id(con, as.integer(report_id))
  if (is.null(row)) {
    return(refuse(404L, "panel.report.not_found"))
  }
  path <- row$file_path[1]
  if (!file.exists(path)) {
    return(refuse(410L, "panel.report.file_missing"))
  }
  bytes <- readBin(path, what = "raw", n = file.size(path))
  sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  if (!identical(sha256, row$file_sha256[1])) {
    return(refuse(409L, "panel.report.file_changed"))
  }
  shiny::httpResponse(200L, "text/html; charset=UTF-8", bytes)
}
