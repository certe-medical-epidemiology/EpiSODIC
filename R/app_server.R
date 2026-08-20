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

    # Bumped by every write action (episode_app_server_assessment_actions()'s
    # refresh()) so read models with no other reason to invalidate - the
    # rail's own open-cluster list, the Archief screen - notice a
    # classification, closure or mute happened even while the user never
    # leaves the view that triggered it.
    db_version <- shiny::reactiveVal(0)
    db_touch <- function() db_version(db_version() + 1)

    open_clusters <- shiny::reactive({
      db_version()
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

    streams_page <- shiny::reactiveVal(1L)
    shiny::observeEvent(input$streams_page_select, streams_page(input$streams_page_select))

    current_user <- episode_app_server_auth(input, output, session, con, lang = lang)

    output$auth_control <- shiny::renderUI({
      episode_ui_auth_control(current_user(), lang = lang)
    })

    output$status_strip <- shiny::renderUI({
      episode_ui_status_strip(episode_app_status(con), lang = lang)
    })

    output$main_view <- shiny::renderUI({
      if (view() == "streams") {
        episode_ui_streams_screen(episode_app_streams_screen(con, page = streams_page()), lang = lang)
      } else if (view() == "archive") {
        shiny::uiOutput("archive_screen")
      } else if (view() == "activity") {
        episode_ui_activity_screen(episode_app_activity_log(con, lang = lang), lang = lang)
      } else if (view() == "performance") {
        episode_ui_performance_screen(episode_app_performance(con, lang = lang), lang = lang)
      } else if (view() == "info") {
        episode_ui_info_screen(lang = lang)
      } else {
        shiny::tags$div(
          class = "episode-body",
          shiny::uiOutput("rail_pane"),
          shiny::uiOutput("dossier_pane"),
          shiny::uiOutput("assessment_pane")
        )
      }
    })

    # Deliberately not dependent on selected_cluster_id(): the rail's own
    # HTML only needs to change when the open-cluster list itself changes,
    # not on every click. If it re-rendered on selection too, the whole
    # rail's DOM (and with it, its scroll position) would be replaced on
    # every click - episode_ui_rail()'s onclick handles the "active"
    # highlight itself, client-side, instead.
    output$rail_pane <- shiny::renderUI({
      # current_user() deliberately not isolated (unlike selected_cluster_id()
      # above it): the bulk-select checkboxes and action bar are gated on
      # being signed in, so they need to appear/disappear immediately on
      # sign-in/out, matching dossier_pane's own choice below for the same
      # reason.
      episode_ui_rail(open_clusters(), shiny::isolate(selected_cluster_id()), lang = lang, current_user = current_user())
    })

    output$dossier_pane <- shiny::renderUI({
      cluster_id <- selected_cluster_id()
      current_user()  # re-render on sign in/out (line list lock, classification form)
      if (is.null(cluster_id)) {
        return(shiny::tags$div(class = "episode-dossier",
                                shiny::tags$p(episode_tr("rail.empty", lang = lang))))
      }
      episode_ui_dossier(con, cluster_id, lang = lang, current_user = current_user())
    })

    output$assessment_pane <- shiny::renderUI({
      cluster_id <- selected_cluster_id()
      user <- current_user()
      if (is.null(cluster_id)) return(NULL)
      episode_ui_assessment_rail(con, cluster_id, lang = lang, current_user = user)
    })

    archive_query <- shiny::reactiveVal("")
    shiny::observeEvent(input$archive_search, archive_query(input$archive_search))
    output$archive_screen <- shiny::renderUI({
      db_version()
      episode_ui_archive_screen(episode_app_archive(con, query = archive_query(), lang = lang), lang = lang)
    })

    episode_app_server_assessment_actions(input, output, session, con, lang = lang,
                                           current_user = current_user, selected_cluster_id = selected_cluster_id,
                                           db_touch = db_touch)
    episode_app_server_report(input, output, session, con, db_path = db_path, lang = lang,
                               current_user = current_user, selected_cluster_id = selected_cluster_id)
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
    dot_colour <- pal$warning
    text <- episode_tr(
      "status.run_ok", lang = lang, time = episode_ui_format_datetime(status$finished_at),
      streams_phrase = episode_count_phrase(status$n_streams %||% 0, episode_tr("unit.stream", lang = lang), episode_tr("unit.streams", lang = lang)),
      clusters_phrase = episode_count_phrase(status$n_clusters_open %||% 0, episode_tr("unit.cluster", lang = lang), episode_tr("unit.clusters", lang = lang))
    )
  } else {
    dot_colour <- pal$danger_dark
    text <- episode_tr("status.run_failed", lang = lang, time = episode_ui_format_datetime(status$finished_at))
  }
  shiny::tags$div(
    class = "episode-status-strip",
    shiny::tags$span(shiny::tags$span(class = "episode-status-dot", style = sprintf("background:%s;", dot_colour)), text)
  )
}

#' Format a UTC-stored ISO timestamp for display, converted to local time
#'
#' Every timestamp in the database is stored as UTC (`episode_now()`),
#' deliberately - a single unambiguous instant, independent of server or
#' viewer timezone. Display is the other half of that deal: this always
#' converts to `Sys.timezone()` before formatting, so a viewer never sees
#' a bare UTC/"Z" timestamp. `Sys.timezone()` rather than
#' `as.POSIXlt(Sys.time())$zone`, since it returns the IANA zone name
#' `format()`'s own `tz` argument expects and handles DST transitions
#' correctly for any instant, not just "now" - `as.POSIXlt(x)$zone` gives
#' only the abbreviation for one already-resolved instant.
#'
#' Note that `as.POSIXct(iso, tz = "UTC")` alone is not enough: the
#' resulting object's own `tzone` attribute stays `"UTC"`, and
#' `format()` without an explicit `tz` argument formats in *that* stored
#' zone, not the system's - this silently produced UTC times labelled as
#' if they were local (QUESTIONS.md).
#'
#' @param iso An ISO-8601 UTC string (`episode_now()`'s format), or `NA`.
#' @param fmt A `format()`/`strftime()` format string.
#' @param tz Target IANA timezone. Defaults to `Sys.timezone()`; exposed as
#'   an argument (rather than hardcoded) mainly so tests can pass a fixed
#'   zone - `Sys.timezone()` caches its result for the R session, so
#'   `Sys.setenv(TZ = ...)` after that first call has no effect on it.
#' @return A character string in local time, or `episode_tr("misc.unknown")`
#'   for `NA`/`NULL`, or `iso` itself if it does not parse.
#' @keywords internal
#' @noRd
episode_ui_format_datetime <- function(iso, fmt = "%H:%M", tz = Sys.timezone()) {
  if (is.null(iso) || is.na(iso)) return(episode_tr("misc.unknown"))
  parsed <- tryCatch(as.POSIXct(iso, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"), error = function(e) NA)
  if (is.na(parsed)) return(iso)
  format(parsed, fmt, tz = tz)
}

#' The open-cluster rail, with an optional bulk-assessment bar
#'
#' "Often they are artefacts": a signed-in user can check several
#' clusters and classify all of them in one submit, rather than opening
#' each dossier in turn for what is frequently the same verdict
#' (`artefact`) and a shared rationale. Selection itself is tracked
#' entirely client-side (a checkbox's own `checked` state, read from the
#' DOM at submit time via `episodeBulkUpdate()`/the submit button's
#' onclick) rather than in a Shiny reactive - the rail's own render is
#' deliberately decoupled from `selected_cluster_id()` so a click does
#' not replace the whole list and lose scroll position (see
#' `output$rail_pane`'s own comment); round-tripping every checkbox
#' toggle through the server would reintroduce exactly that problem.
#' `episode_app_server_assessment_actions()`'s `bulk_assess_submit`
#' observer is the write side.
#'
#' @param open A data frame from [episode_app_open_clusters()].
#' @param selected_id The rail's current single-cluster selection (for
#'   the "active" highlight), or `NULL`.
#' @param lang Session language.
#' @param current_user The session's signed-in user row, or `NULL` -
#'   bulk selection is a write action, so checkboxes and the action bar
#'   only render for a signed-in session, same gate as the single-cluster
#'   assessment form.
#' @keywords internal
#' @noRd
episode_ui_rail <- function(open, selected_id, lang = "nl", current_user = NULL) {
  pal <- episode_palette()
  verdicts <- c("artefact", "expected_variation", "cluster_not_yet", "possible_epidemic", "confirmed_epidemic")
  verdict_options <- c(
    list(list(value = "", label = episode_tr("assessment.verdict_none", lang = lang), colour = pal$muted)),
    lapply(verdicts, function(v) list(value = v, label = episode_tr(paste0("verdict.", v), lang = lang),
                                       colour = episode_ui_verdict_colour(v)))
  )

  bulk_bar <- if (!is.null(current_user)) {
    shiny::tags$div(
      id = "episode-bulk-bar", style = "display:none;", class = "episode-panel-body",
      shiny::tags$div(style = "font-size:12.5px;font-weight:600;margin-bottom:6px;",
                       shiny::tags$span(id = "episode-bulk-count"), " ", episode_tr("rail.bulk_selected", lang = lang)),
      shiny::tags$div(class = "episode-form-group",
                       shiny::tags$label(class = "episode-form-label", episode_tr("assessment.verdict_label", lang = lang)),
                       episode_ui_picker("bulk_assess_verdict", verdict_options)),
      shiny::tags$div(class = "episode-form-group",
                       shiny::tags$label(class = "episode-form-label", episode_tr("assessment.rationale_label", lang = lang)),
                       shiny::tags$textarea(id = "bulk_assess_rationale", rows = 2,
                                             placeholder = episode_tr("assessment.rationale_placeholder", lang = lang))),
      shiny::tags$div(
        style = "display:flex;gap:8px;",
        shiny::tags$button(
          class = "episode-btn", type = "button",
          onclick = paste0(
            "var ids=Array.from(document.querySelectorAll('.episode-rail-select:checked')).map(function(el){return parseInt(el.value);}); ",
            "var rationale=document.getElementById('bulk_assess_rationale').value; ",
            "if(!rationale.trim()){return;} ",
            "Shiny.setInputValue('bulk_assess_submit', {cluster_ids: ids, verdict: document.getElementById('bulk_assess_verdict').value, rationale: rationale}, {priority: 'event'});"
          ),
          episode_tr("rail.bulk_apply", lang = lang)
        ),
        shiny::tags$button(
          class = "episode-btn", type = "button",
          onclick = paste0(
            "document.querySelectorAll('.episode-rail-select').forEach(function(el){el.checked=false;}); episodeBulkUpdate();"
          ),
          episode_tr("rail.bulk_clear", lang = lang)
        )
      )
    )
  }

  shiny::tags$div(
    class = "episode-rail",
    if (!is.null(current_user)) {
      shiny::tags$script(shiny::HTML(paste0(
        "function episodeBulkUpdate(){var n=document.querySelectorAll('.episode-rail-select:checked').length; ",
        "var bar=document.getElementById('episode-bulk-bar'); if(!bar){return;} ",
        "bar.style.display = n>0 ? 'block' : 'none'; ",
        "var c=document.getElementById('episode-bulk-count'); if(c){c.textContent=n;}}"
      )))
    },
    shiny::tags$div(
      class = "episode-rail-header",
      shiny::tags$div(class = "episode-rail-title", episode_tr("rail.title", lang = lang)),
      shiny::tags$div(class = "episode-rail-count",
                       episode_count_phrase(nrow(open), episode_tr("unit.cluster", lang = lang),
                                             episode_tr("unit.clusters", lang = lang)),
                       " ", episode_tr("rail.count_suffix", lang = lang))
    ),
    bulk_bar,
    if (nrow(open) == 0) {
      shiny::tags$div(style = "padding:14px;font-size:12.5px;color:var(--episode-muted);", episode_tr("rail.empty", lang = lang))
    } else {
      lapply(seq_len(nrow(open)), function(i) {
        row <- open[i, ]
        active <- identical(row$cluster_id, selected_id)
        shiny::tags$div(
          class = paste("episode-rail-item", if (active) "active" else ""),
          onclick = sprintf(
            "document.querySelectorAll('.episode-rail-item').forEach(function(el){el.classList.remove('active');}); this.classList.add('active'); Shiny.setInputValue('rail_select', %d, {priority: 'event'})",
            row$cluster_id
          ),
          if (!is.null(current_user)) {
            shiny::tags$input(
              type = "checkbox", class = "episode-rail-select", value = row$cluster_id,
              onclick = "event.stopPropagation(); episodeBulkUpdate();",
              onchange = "episodeBulkUpdate();"
            )
          },
          shiny::tags$div(class = "episode-rail-pathogen", shiny::HTML(episode_ui_italicise_taxon(row$pathogen))),
          shiny::tags$div(class = "episode-rail-meta", row$level_label),
          shiny::tags$div(class = "episode-rail-meta", episode_format_date_range(row$first_day, row$last_day, lang = lang)),
          shiny::tags$div(class = "episode-rail-meta", paste(c(
            episode_count_phrase(row$n_cases, episode_tr("unit.case", lang = lang), episode_tr("unit.cases", lang = lang)),
            if (!is.na(row$ratio)) sprintf("ratio %s", round(row$ratio, 1))
          ), collapse = " \u00b7 ")),
          shiny::tags$div(class = "episode-rail-state", episode_ui_state_dot(row$state), row$state_label)
        )
      })
    }
  )
}
