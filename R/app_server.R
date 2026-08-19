#' The application server
#'
#' @param db_path Path to the SQLite database.
#' @param lang Session language, `"nl"` (default) or `"en"`.
#' @return A Shiny server function.
#' @keywords internal
#' @noRd
episode_app_server_factory <- function(db_path, lang = "nl") {
  function(input, output, session) {
    con <- episode_db_connect(db_path)
    session$onSessionEnded(function() {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })

    view <- shiny::reactiveVal("clusters")
    shiny::observeEvent(input$nav_view, view(input$nav_view))

    open_clusters <- shiny::reactive({
      shiny::req(view() == "clusters")
      episode_app_open_clusters(con, lang = lang)
    })

    selected_cluster_id <- shiny::reactiveVal(NULL)
    shiny::observeEvent(open_clusters(), {
      current <- selected_cluster_id()
      ids <- open_clusters()$cluster_id
      if (length(ids) > 0 && (is.null(current) || !(current %in% ids))) {
        selected_cluster_id(ids[1])
      }
    })
    shiny::observeEvent(input$rail_select, selected_cluster_id(input$rail_select))

    output$status_strip <- shiny::renderUI({
      episode_ui_status_strip(episode_app_status(con), lang = lang)
    })

    output$main_view <- shiny::renderUI({
      if (view() == "streams") {
        episode_ui_streams_screen(episode_app_streams_screen(con), lang = lang)
      } else {
        shiny::tags$div(
          class = "episode-body",
          episode_ui_rail(open_clusters(), selected_cluster_id(), lang = lang),
          shiny::uiOutput("dossier_pane"),
          shiny::uiOutput("assessment_pane")
        )
      }
    })

    output$dossier_pane <- shiny::renderUI({
      cluster_id <- selected_cluster_id()
      if (is.null(cluster_id)) {
        return(shiny::tags$div(class = "episode-dossier",
                                shiny::tags$p(episode_tr("rail.empty", lang = lang))))
      }
      episode_ui_dossier(con, cluster_id, lang = lang)
    })

    output$assessment_pane <- shiny::renderUI({
      cluster_id <- selected_cluster_id()
      if (is.null(cluster_id)) return(NULL)
      episode_ui_assessment_rail(con, cluster_id, lang = lang)
    })
  }
}

#' @keywords internal
#' @noRd
episode_ui_status_strip <- function(status, lang = "nl") {
  if (identical(status$status, "none")) {
    return(shiny::tags$div(class = "episode-status-strip", episode_tr("status.no_run", lang = lang)))
  }
  pal <- episode_palette()
  if (identical(status$status, "success")) {
    dot_colour <- pal$yellow
    text <- episode_tr(
      "status.run_ok", lang = lang, time = episode_ui_format_datetime(status$finished_at),
      streams_phrase = episode_count_phrase(status$n_streams %||% 0, episode_tr("unit.stream", lang = lang), episode_tr("unit.streams", lang = lang)),
      clusters_phrase = episode_count_phrase(status$n_clusters_open %||% 0, episode_tr("unit.cluster", lang = lang), episode_tr("unit.clusters", lang = lang))
    )
  } else {
    dot_colour <- pal$carmine
    text <- episode_tr("status.run_failed", lang = lang, time = episode_ui_format_datetime(status$finished_at))
  }
  shiny::tags$div(
    class = "episode-status-strip",
    shiny::tags$span(shiny::tags$span(class = "episode-status-dot", style = sprintf("background:%s;", dot_colour)), text)
  )
}

#' @keywords internal
#' @noRd
episode_ui_format_datetime <- function(iso) {
  if (is.null(iso) || is.na(iso)) return(episode_tr("misc.unknown"))
  parsed <- tryCatch(as.POSIXct(iso, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"), error = function(e) NA)
  if (is.na(parsed)) return(iso)
  format(parsed, "%H:%M")
}

#' @keywords internal
#' @noRd
episode_ui_rail <- function(open, selected_id, lang = "nl") {
  shiny::tags$div(
    class = "episode-rail",
    shiny::tags$div(
      class = "episode-rail-header",
      shiny::tags$div(class = "episode-rail-title", episode_tr("rail.title", lang = lang)),
      shiny::tags$div(class = "episode-rail-count",
                       episode_count_phrase(nrow(open), episode_tr("unit.cluster", lang = lang),
                                             episode_tr("unit.clusters", lang = lang)),
                       " ", episode_tr("rail.count_suffix", lang = lang))
    ),
    if (nrow(open) == 0) {
      shiny::tags$div(style = "padding:14px;font-size:12.5px;color:var(--episode-muted);", episode_tr("rail.empty", lang = lang))
    } else {
      lapply(seq_len(nrow(open)), function(i) {
        row <- open[i, ]
        active <- identical(row$cluster_id, selected_id)
        shiny::tags$div(
          class = paste("episode-rail-item", if (active) "active" else ""),
          onclick = sprintf("Shiny.setInputValue('rail_select', %d, {priority: 'event'})", row$cluster_id),
          shiny::tags$div(class = "episode-rail-pathogen", row$pathogen),
          shiny::tags$div(class = "episode-rail-meta", row$level_label),
          shiny::tags$div(class = "episode-rail-meta",
                           episode_count_phrase(row$n_cases, episode_tr("unit.case", lang = lang), episode_tr("unit.cases", lang = lang)),
                           sprintf(" \u00b7 ratio %s", round(row$ratio, 1))),
          shiny::tags$div(class = "episode-rail-state", episode_ui_state_dot(row$state), row$state_label)
        )
      })
    }
  )
}
