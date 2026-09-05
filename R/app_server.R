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

#' The application server
#'
#' @param db_path Path to the SQLite database.
#' @param lang Session language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @return A Shiny server function.
#' @keywords internal
#' @noRd
episodic_app_server_factory <- function(db_path,
                                        lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  function(input, output, session) {
    con <- episodic_db_connect(db_path)
    session$onSessionEnded(function() {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })

    # Read once per session rather than per render: the access policy is
    # a property of the deployment, and re-reading the YAML on every
    # reactive invalidation would let it change under a session halfway
    # through.
    require_login <- episodic_app_require_login()

    # Set up before anything that reads it, because `access_granted()` is
    # the gate every output below gets its data past, and a gate defined
    # after the things it guards is a gate somebody eventually forgets to
    # close.
    current_user <- episodic_app_server_auth(
      input,
      output,
      session,
      con,
      lang = lang,
      require_login = require_login
    )

    # The one place the anonymous-access policy is decided. Everything
    # that would put surveillance data on the page is behind it, and what
    # it gates is whether that data is *computed and sent at all* - never
    # whether it is hidden once sent. See
    # `episodic_app_access_granted()`.
    access_granted <- shiny::reactive({
      episodic_app_access_granted(require_login, current_user())
    })

    view <- shiny::reactiveVal("clusters")
    shiny::observeEvent(input$nav_view, view(input$nav_view))

    # Bumped by every write action (episodic_app_server_assessment_actions()'s
    # refresh()) so read models with no other reason to invalidate - the
    # rail's own open-cluster list, the Archief screen - notice a
    # classification, closure or mute happened even while the user never
    # leaves the view that triggered it.
    db_version <- shiny::reactiveVal(0)
    db_touch <- function() db_version(db_version() + 1)

    open_clusters <- shiny::reactive({
      db_version()
      shiny::req(access_granted())
      shiny::req(view() == "clusters")
      episodic_app_open_clusters(con, lang = lang)
    })

    selected_cluster_id <- shiny::reactiveVal(NULL)
    # Bumped by `episodic_app_server_notes()` on a successful save, purely
    # to invalidate `output$notes_pane` below - the rest of the dossier
    # (several panels of which are plots) has no reason to redraw just
    # because a note changed, unlike a cluster selection change.
    notes_version <- shiny::reactiveVal(0L)
    # Fills the dossier pane on first load, and moves on when whatever was
    # selected has genuinely gone. It used to reset the selection whenever
    # the selected cluster was not in the *open* list, which is a
    # different and too-broad condition: the Pathogen screen links to
    # clusters by id and most of the ones it lists are closed, so that
    # rule would have silently redirected every such link to the top of
    # the rail. A cluster that closes while you are reading it also has no
    # business disappearing out from under you - the state chip says it
    # closed, which is the answer you were looking for.
    #
    # Merged-away clusters are the real exception: their cases now belong
    # to the surviving cluster, so their dossier is stale rather than
    # merely closed, and that is what still forces a re-selection.
    shiny::observeEvent(open_clusters(), {
      ids <- open_clusters()$cluster_id
      if (length(ids) == 0) {
        return()
      }
      if (!episodic_app_cluster_viewable(con, selected_cluster_id())) {
        selected_cluster_id(ids[1])
      }
    })
    shiny::observeEvent(
      input$rail_select,
      selected_cluster_id(input$rail_select)
    )

    # Deep link from any cluster table (see `R/app_cluster_table.R`).
    # Setting the selection before the view means the dossier pane has
    # its cluster ready by the time the clusters view renders, and the
    # observer above will leave it alone whichever order the two land in.
    shiny::observeEvent(input$open_cluster, {
      selected_cluster_id(as.integer(input$open_cluster))
      view("clusters")
    })

    # The same deep link from outside the app: `?cluster=123` opens that
    # cluster's dossier on load. It is what makes the id in a
    # notification a link somebody can follow (see
    # `episodic_notify_cluster_url()`) rather than a reference they have
    # to go and find in the queue. An id that names nothing viewable is
    # ignored rather than blanking the screen - a link from an old email
    # to a cluster since merged away should land on the queue, not on an
    # error.
    session$onFlushed(
      function() {
        # onFlushed's callback runs outside any reactive consumer, but
        # clientData's fields are themselves reactive values - reading
        # url_search here needs isolate() or it throws "Can't access
        # reactive value ... outside of reactive consumer". isolate() is
        # also the right call, not observe(): this reads the query string
        # once on initial load (once = TRUE) and must not re-fire if it
        # changes later.
        requested <- shiny::isolate(episodic_app_url_cluster_id(
          session$clientData$url_search
        ))
        if (is.null(requested)) {
          return()
        }
        if (!episodic_app_cluster_viewable(con, requested)) {
          return()
        }
        selected_cluster_id(requested)
        view("clusters")
      },
      once = TRUE
    )

    streams_page <- shiny::reactiveVal(1L)
    shiny::observeEvent(
      input$streams_page_select,
      streams_page(input$streams_page_select)
    )

    # Pathogen screen selection. Held here rather than derived from the
    # inputs at render time so that switching away to a cluster dossier
    # and back does not silently reset the pathogen and period an
    # epidemiologist was part-way through reading.
    pathogen_selected <- shiny::reactiveVal(NULL)
    pathogen_period <- shiny::reactiveVal("year_current")
    pathogen_range <- shiny::reactiveVal(list(from = NULL, to = NULL))
    shiny::observeEvent(
      input$pathogen_select,
      pathogen_selected(input$pathogen_select)
    )
    shiny::observeEvent(input$pathogen_period, {
      pathogen_period(input$pathogen_period)
      if (!identical(input$pathogen_period, "custom")) {
        pathogen_range(list(from = NULL, to = NULL))
      }
    })
    shiny::observeEvent(input$pathogen_custom_range, {
      # Picking dates *is* the request to use them, so this selects the
      # custom period rather than requiring a separate click on it.
      pathogen_range(list(
        from = input$pathogen_custom_range$from,
        to = input$pathogen_custom_range$to
      ))
      pathogen_period("custom")
    })

    output$auth_control <- shiny::renderUI({
      episodic_ui_auth_control(current_user(), lang = lang)
    })

    # The navigation is a map of what there is to read, and the status
    # strip carries the last run's outcome and completeness. Neither is a
    # cluster, and both are withheld from a visitor who may see nothing.
    output$nav_links <- shiny::renderUI({
      if (!access_granted()) {
        return(NULL)
      }
      episodic_ui_nav_links(
        view(),
        lang = lang,
        is_admin = episodic_user_is_admin(current_user())
      )
    })

    output$status_strip <- shiny::renderUI({
      if (!access_granted()) {
        return(NULL)
      }
      episodic_ui_status_strip(episodic_app_status(con), lang = lang)
    })

    output$main_view <- shiny::renderUI({
      # Every screen below reads the database. On an instance that
      # requires a sign-in, an anonymous session never gets past here, so
      # none of those reads happens and nothing they would return is
      # serialised into the page.
      if (!access_granted()) {
        return(episodic_ui_locked_screen(lang = lang))
      }
      if (view() == "streams") {
        episodic_ui_streams_screen(
          episodic_app_streams_screen(con, page = streams_page()),
          lang = lang
        )
      } else if (view() == "archive") {
        shiny::uiOutput("archive_screen")
      } else if (view() == "activity") {
        episodic_ui_activity_screen(
          episodic_app_activity_log(con, lang = lang),
          lang = lang
        )
      } else if (view() == "pathogen") {
        range <- pathogen_range()
        episodic_ui_pathogen_screen(
          episodic_app_pathogen_screen(
            con,
            pathogen = pathogen_selected(),
            period = pathogen_period(),
            from = range$from,
            to = range$to,
            lang = lang
          ),
          lang = lang
        )
      } else if (view() == "performance") {
        episodic_ui_performance_screen(
          episodic_app_performance(con, lang = lang),
          lang = lang
        )
      } else if (view() == "info") {
        episodic_ui_info_screen(
          con,
          current_user = current_user(),
          lang = lang
        )
      } else if (view() == "settings") {
        shiny::uiOutput("settings_screen")
      } else {
        shiny::tags$div(
          class = "episodic-body",
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
    # every click - episodic_ui_rail()'s onclick handles the "active"
    # highlight itself, client-side, instead.
    output$rail_pane <- shiny::renderUI({
      # Gated in its own right, not only through `main_view` not placing
      # it: an output nothing binds is an output nothing computes, but
      # that is a property of how it happens to be reached today, and
      # this is a leak if it ever stops being true.
      if (!access_granted()) {
        return(NULL)
      }
      # current_user() deliberately not isolated (unlike selected_cluster_id()
      # above it): the bulk-select checkboxes and action bar are gated on
      # being signed in, so they need to appear/disappear immediately on
      # sign-in/out, matching dossier_pane's own choice below for the same
      # reason.
      episodic_ui_rail(
        open_clusters(),
        shiny::isolate(selected_cluster_id()),
        lang = lang,
        current_user = current_user()
      )
    })

    output$dossier_pane <- shiny::renderUI({
      if (!access_granted()) {
        return(NULL)
      }
      cluster_id <- selected_cluster_id()
      current_user() # re-render on sign in/out (line list lock, classification form)
      if (is.null(cluster_id)) {
        return(shiny::tags$div(
          class = "episodic-dossier",
          shiny::tags$p(episodic_tr("rail.empty", lang = lang))
        ))
      }
      episodic_ui_dossier(
        con,
        cluster_id,
        lang = lang,
        current_user = current_user()
      )
    })

    output$notes_pane <- shiny::renderUI({
      if (!access_granted()) {
        return(NULL)
      }
      cluster_id <- selected_cluster_id()
      notes_version() # invalidate on save, without touching dossier_pane
      user <- current_user()
      if (is.null(cluster_id)) {
        return(NULL)
      }
      episodic_ui_notes_panel(con, cluster_id, user, lang = lang)
    })

    output$assessment_pane <- shiny::renderUI({
      if (!access_granted()) {
        return(NULL)
      }
      cluster_id <- selected_cluster_id()
      user <- current_user()
      if (is.null(cluster_id)) {
        return(NULL)
      }
      episodic_ui_assessment_rail(
        con,
        cluster_id,
        lang = lang,
        current_user = user
      )
    })

    archive_query <- shiny::reactiveVal("")
    shiny::observeEvent(
      input$archive_search,
      archive_query(input$archive_search)
    )
    archive_levels <- shiny::reactiveVal(character(0))
    shiny::observeEvent(input$archive_level_filter, {
      v <- input$archive_level_filter
      archive_levels(
        if (!nzchar(v)) character(0) else strsplit(v, ",", fixed = TRUE)[[1]]
      )
    })
    output$archive_screen <- shiny::renderUI({
      if (!access_granted()) {
        return(NULL)
      }
      db_version()
      episodic_ui_archive_screen(
        episodic_app_archive(
          con,
          query = archive_query(),
          level = archive_levels(),
          lang = lang
        ),
        selected_levels = archive_levels(),
        lang = lang
      )
    })

    # The Activity screen's per-run detail. Everything shown here is
    # already in the database; the point is that an epidemiologist reading
    # the log can see why a run failed without asking whoever schedules
    # them - and read the whole message, not the line the table had room
    # for.
    shiny::observeEvent(input$activity_run_detail, {
      # An observer, unlike an output, runs whenever the input arrives -
      # and any client can set any input. Without this the run detail,
      # error text and per-feed counts included, is one console line away
      # on an instance that shows an anonymous visitor nothing.
      shiny::req(access_granted())
      run <- episodic_db_run(con, as.integer(input$activity_run_detail))
      if (is.null(run)) {
        return(NULL)
      }
      shiny::showModal(episodic_ui_run_modal(con, run, lang = lang))
    })

    episodic_app_server_assessment_actions(
      input,
      output,
      session,
      con,
      current_user = current_user,
      selected_cluster_id = selected_cluster_id,
      db_touch = db_touch
    )
    episodic_app_server_report(
      input,
      output,
      session,
      con,
      db_path = db_path,
      lang = lang,
      current_user = current_user,
      selected_cluster_id = selected_cluster_id
    )
    episodic_app_server_notes(
      input,
      output,
      session,
      con,
      current_user = current_user,
      notes_version = notes_version,
      access_granted = access_granted,
      lang = lang
    )
    episodic_app_server_settings(
      input,
      output,
      session,
      con,
      db_path = db_path,
      lang = lang,
      current_user = current_user
    )
  }
}

#' @keywords internal
#' @noRd
episodic_ui_status_strip <- function(status,
                                     lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (identical(status$status, "none")) {
    return(shiny::tags$div(
      class = "episodic-status-strip",
      episodic_tr("status.no_run", lang = lang)
    ))
  }
  pal <- episodic_palette()
  if (status$status %in% episodic_run_statuses_complete) {
    # A partial run completed and is safe to read from; it is flagged
    # rather than greened over, because rows of an optional feed were
    # skipped and somebody should go and look at why.
    dot_colour <- if (identical(status$status, "partial")) {
      pal$warning
    } else {
      pal$success
    }
    text <- episodic_tr(
      if (identical(status$status, "partial")) {
        "status.run_partial"
      } else {
        "status.run_ok"
      },
      lang = lang,
      time = episodic_ui_format_datetime(status$finished_at),
      streams_phrase = episodic_count_phrase(
        status$n_streams %||% 0,
        episodic_tr("unit.stream", lang = lang),
        episodic_tr("unit.streams", lang = lang)
      ),
      clusters_phrase = episodic_count_phrase(
        status$n_clusters_open %||% 0,
        episodic_tr("unit.cluster", lang = lang),
        episodic_tr("unit.clusters", lang = lang)
      )
    )
  } else {
    dot_colour <- pal$danger
    # A failed run is almost always an operator's own data, and the
    # reason for it is already recorded. Showing only "failed" sends
    # somebody looking through logs for a message the dashboard is
    # holding.
    reason <- episodic_ui_first_line(status$error_text)
    text <- episodic_tr(
      "status.run_failed",
      lang = lang,
      time = episodic_ui_format_datetime(status$finished_at)
    )
    # Appended rather than substituted into the template: the message is
    # the operator's own error text, and it belongs in the strip as it
    # was recorded.
    if (!is.null(reason)) {
      text <- paste0(text, " \u00b7 ", reason)
    }
  }
  shiny::tags$div(
    class = "episodic-status-strip",
    shiny::tags$span(
      shiny::tags$span(
        class = "episodic-status-dot",
        style = sprintf("background:%s;", dot_colour)
      ),
      text,
      if (!status$status %in% c("none", episodic_run_statuses_complete)) {
        shiny::tags$span(
          class = "episodic-status-hint",
          style = "margin-left:8px;opacity:0.85;",
          episodic_tr("status.run_failed_hint", lang = lang)
        )
      }
    )
  )
}

#' The first line of a recorded error, short enough for one strip
#'
#' A validation failure is deliberately a long, multi-line message: it
#' names every offending column and how to fix each. Its first line is the
#' summary, and the whole message is on the activity screen.
#' @keywords internal
#' @noRd
episodic_ui_first_line <- function(text, max_chars = 160L) {
  if (
    is.null(text) || length(text) == 0 || is.na(text[1]) || !nzchar(text[1])
  ) {
    return(NULL)
  }
  first <- strsplit(as.character(text[1]), "\n", fixed = FALSE)[[1]][1]
  first <- trimws(first)
  if (is.na(first) || !nzchar(first)) {
    return(NULL)
  }
  if (nchar(first) > max_chars) {
    first <- paste0(substr(first, 1, max_chars - 1L), "\u2026")
  }
  first
}

#' Format a UTC-stored ISO timestamp for display, converted to local time
#'
#' Every timestamp in the database is stored as UTC (`episodic_now()`),
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
#' zone, not the system's - a common source of times being silently
#' displayed as if they were local when they are not.
#'
#' @param iso An ISO-8601 UTC string (`episodic_now()`'s format), or `NA`.
#' @param fmt A `format()`/`strftime()` format string.
#' @param tz Target IANA timezone. Defaults to `Sys.timezone()`; exposed as
#'   an argument (rather than hardcoded) mainly so tests can pass a fixed
#'   zone - `Sys.timezone()` caches its result for the R session, so
#'   `Sys.setenv(TZ = ...)` after that first call has no effect on it.
#' @return A character string in local time, or `episodic_tr("misc.unknown")`
#'   for `NA`/`NULL`, or `iso` itself if it does not parse.
#' @keywords internal
#' @noRd
episodic_ui_format_datetime <- function(iso,
                                        fmt = "%H:%M",
                                        tz = Sys.timezone()) {
  if (is.null(iso) || is.na(iso)) {
    return(episodic_tr("misc.unknown"))
  }
  parsed <- tryCatch(
    as.POSIXct(iso, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    error = function(e) NA
  )
  if (is.na(parsed)) {
    return(iso)
  }
  format(parsed, fmt, tz = tz)
}

#' The cluster id a `?cluster=` deep link asks for
#'
#' Parsed rather than trusted: the query string is whatever a reader's
#' browser was pointed at, so anything that is not a single positive
#' integer is no request at all. Refusing to guess is the point - a
#' malformed link must leave the app where it would have been, never
#' select some other cluster.
#'
#' @param search A URL query string including its leading `"?"`
#'   (`session$clientData$url_search`), or `NULL`.
#' @return A single integer, or `NULL` when no usable `cluster` parameter
#'   is present.
#' @keywords internal
#' @noRd
episodic_app_url_cluster_id <- function(search) {
  if (is.null(search) || length(search) != 1 || is.na(search)) {
    return(NULL)
  }
  parsed <- shiny::parseQueryString(search)
  value <- parsed$cluster
  if (is.null(value) || length(value) != 1 || !grepl("^[0-9]+$", value)) {
    return(NULL)
  }
  id <- suppressWarnings(as.integer(value))
  if (is.na(id) || id <= 0L) {
    return(NULL)
  }
  id
}

#' The open-cluster rail, with an optional bulk-assessment bar
#'
#' "Often they are artefacts": a signed-in user can check several
#' clusters and classify all of them in one submit, rather than opening
#' each dossier in turn for what is frequently the same verdict
#' (`artefact`) and a shared rationale. Selection itself is tracked
#' entirely client-side (a checkbox's own `checked` state, read from the
#' DOM at submit time via `episodicBulkUpdate()`/the submit button's
#' onclick) rather than in a Shiny reactive - the rail's own render is
#' deliberately decoupled from `selected_cluster_id()` so a click does
#' not replace the whole list and lose scroll position (see
#' `output$rail_pane`'s own comment); round-tripping every checkbox
#' toggle through the server would reintroduce exactly that problem.
#' `episodic_app_server_assessment_actions()`'s `bulk_assess_submit`
#' observer is the write side.
#'
#' @param open A data frame from `episodic_app_open_clusters()`.
#' @param selected_id The rail's current single-cluster selection (for
#'   the "active" highlight), or `NULL`.
#' @param lang Session language.
#' @param current_user The session's signed-in user row, or `NULL` -
#'   bulk selection is a write action, so checkboxes and the action bar
#'   only render for a signed-in epidemiologist, same gate as the single-cluster
#'   assessment form. A signed-in viewer sees the rail exactly as a
#'   signed-out visitor does.
#' @keywords internal
#' @noRd
episodic_ui_rail <- function(open,
                             selected_id,
                             lang = Sys.getenv("EPISODIC_LANGUAGE"),
                             current_user = NULL) {
  pal <- episodic_palette()
  verdicts <- c(
    "artefact",
    "expected_variation",
    "cluster_not_yet",
    "possible_epidemic",
    "confirmed_epidemic"
  )
  verdict_options <- c(
    list(list(
      value = "",
      label = episodic_tr("assessment.verdict_none", lang = lang),
      colour = pal$muted
    )),
    lapply(verdicts, function(v) {
      list(
        value = v,
        # No `level`: this one dropdown applies to whatever mix of
        # clusters is checked in the rail, which can span several
        # levels at once - so it deliberately keeps the general
        # "epidemic" wording rather than guessing one.
        label = episodic_verdict_label(v, lang = lang),
        colour = episodic_ui_verdict_colour(v)
      )
    })
  )

  bulk_bar <- if (episodic_user_is_epidemiologist(current_user)) {
    shiny::tags$div(
      id = "episodic-bulk-bar",
      style = "display:none;",
      class = "episodic-panel-body",
      shiny::tags$div(
        style = "font-size:12.5px;font-weight:600;margin-bottom:6px;",
        shiny::tags$span(id = "episodic-bulk-count"),
        " ",
        episodic_tr("rail.bulk_selected", lang = lang)
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("assessment.verdict_label", lang = lang)
        ),
        episodic_ui_picker("bulk_assess_verdict", verdict_options)
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("assessment.rationale_label", lang = lang)
        ),
        shiny::tags$textarea(
          id = "bulk_assess_rationale",
          rows = 2,
          placeholder = episodic_tr(
            "assessment.rationale_placeholder",
            lang = lang
          )
        )
      ),
      shiny::tags$div(
        style = "display:flex;gap:8px;",
        shiny::tags$button(
          class = "episodic-btn",
          type = "button",
          onclick = paste0(
            "var ids=Array.from(document.querySelectorAll('.episodic-rail-select:checked')).map(function(el){return parseInt(el.value);}); ",
            "if(ids.length===0){return;} ",
            "var rationale=document.getElementById('bulk_assess_rationale').value; ",
            "Shiny.setInputValue('bulk_assess_submit', {cluster_ids: ids, verdict: document.getElementById('bulk_assess_verdict').value, rationale: rationale}, {priority: 'event'});"
          ),
          episodic_tr("rail.bulk_apply", lang = lang)
        ),
        shiny::tags$button(
          class = "episodic-btn",
          type = "button",
          onclick = paste0(
            "document.querySelectorAll('.episodic-rail-select').forEach(function(el){el.checked=false;}); episodicBulkUpdate();"
          ),
          episodic_tr("rail.bulk_clear", lang = lang)
        )
      )
    )
  }

  shiny::tags$div(
    class = "episodic-rail",
    if (episodic_user_is_epidemiologist(current_user)) {
      shiny::tags$script(shiny::HTML(paste0(
        "function episodicBulkUpdate(){var n=document.querySelectorAll('.episodic-rail-select:checked').length; ",
        "var bar=document.getElementById('episodic-bulk-bar'); if(!bar){return;} ",
        "bar.style.display = n>0 ? 'block' : 'none'; ",
        "var c=document.getElementById('episodic-bulk-count'); if(c){c.textContent=n;}}"
      )))
    },
    shiny::tags$div(
      class = "episodic-rail-header",
      shiny::tags$div(
        class = "episodic-rail-title",
        episodic_tr("rail.title", lang = lang)
      ),
      shiny::tags$div(
        class = "episodic-rail-count",
        episodic_count_phrase(
          nrow(open),
          episodic_tr("unit.cluster", lang = lang),
          episodic_tr("unit.clusters", lang = lang)
        ),
        " ",
        episodic_tr("rail.count_suffix", lang = lang)
      )
    ),
    bulk_bar,
    if (nrow(open) == 0) {
      shiny::tags$div(
        style = "padding:14px;font-size:12.5px;color:var(--episodic-muted);",
        episodic_tr("rail.empty", lang = lang)
      )
    } else {
      lapply(seq_len(nrow(open)), function(i) {
        row <- open[i, ]
        # `care_line`/`priority_score` are absent from some older test
        # fixtures that predate them; a bare `row$care_line` on such a
        # frame returns NULL rather than NA, and `is.na(NULL)` errors
        # ("argument is of length zero") rather than returning FALSE.
        row$care_line <- row$care_line %||% NA_character_
        row$priority_score <- row$priority_score %||% NA_real_
        active <- identical(row$cluster_id, selected_id)
        shiny::tags$div(
          class = paste("episodic-rail-item", if (active) "active" else ""),
          # The id every episodicOpenCluster() call (a cluster table row, a
          # linked-cluster chip) reads back to find and highlight this item
          # when the selection changes from outside the rail itself.
          `data-cluster-id` = row$cluster_id,
          onclick = sprintf(
            "document.querySelectorAll('.episodic-rail-item').forEach(function(el){el.classList.remove('active');}); this.classList.add('active'); Shiny.setInputValue('rail_select', %d, {priority: 'event'})",
            row$cluster_id
          ),
          shiny::tags$div(
            class = "episodic-rail-pathogen",
            if (episodic_user_is_epidemiologist(current_user)) {
              shiny::tags$input(
                type = "checkbox",
                class = "episodic-rail-select",
                value = row$cluster_id,
                onclick = "event.stopPropagation(); episodicBulkUpdate();",
                onchange = "episodicBulkUpdate();"
              )
            },
            shiny::HTML(episodic_ui_italicise_taxon(row$pathogen)),
            shiny::tags$span(
              class = "episodic-rail-id",
              episodic_tr(
                "dossier.cluster_ref",
                id = row$cluster_id,
                lang = lang
              )
            ),
            if (!is.na(row$care_line)) {
              care_line_colour <- episodic_ui_care_line_colour(row$care_line)
              if (!is.null(care_line_colour)) {
                episodic_ui_chip(
                  episodic_tr(
                    paste0("careline.short.", row$care_line),
                    lang = lang
                  ),
                  care_line_colour,
                  filled = TRUE
                )
              }
            }
          ),
          shiny::tags$div(class = "episodic-rail-meta", row$level_label),
          shiny::tags$div(
            class = "episodic-rail-meta",
            episodic_format_date_range(
              row$first_day,
              row$last_day,
              lang = lang
            )
          ),
          shiny::tags$div(
            class = "episodic-rail-meta",
            paste(
              c(
                episodic_count_phrase(
                  row$n_cases,
                  episodic_tr("unit.case", lang = lang),
                  episodic_tr("unit.cases", lang = lang)
                ),
                if (!is.na(row$priority_score)) {
                  episodic_tr(
                    "rail.priority",
                    score = trimws(format(round(row$priority_score, 0))),
                    lang = lang
                  )
                }
              ),
              collapse = " \u00b7 "
            )
          ),
          shiny::tags$div(
            class = "episodic-rail-state",
            episodic_ui_state_dot(row$state),
            row$state_label
          )
        )
      })
    }
  )
}
