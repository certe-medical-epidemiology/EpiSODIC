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

#' The dossier pane
#'
#' Assembles the full cluster dossier: header, stat grid, status
#' trajectory band, interpretation, then the analytical panels in a fixed
#' order (evidence before context, context before administration).
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param lang Session language.
#' @param current_user The session's signed-in user row, or `NULL` for an
#'   anonymous viewer. Read access is anonymous throughout; the line
#'   list is the one panel gated on it.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episode_ui_dossier <- function(con, cluster_id, lang = "nl", current_user = NULL) {
  obj <- episode_cluster_object(con, cluster_id, lang = lang)
  state <- episode_app_derive_state_for_cluster(con, cluster_id)
  timeline <- episode_app_assessment_timeline(con, cluster_id, lang = lang)
  pal <- episode_palette()

  shiny::tags$div(
    class = "episode-dossier",
    episode_ui_dossier_header(obj, state, lang = lang),
    episode_ui_stat_grid(obj, lang = lang),
    episode_ui_trajectory(obj, timeline, lang = lang),
    episode_ui_interpretation_panel(obj, lang = lang),
    episode_ui_epicurve_panel(con, cluster_id, obj, lang = lang),
    episode_ui_rt_panel(obj, lang = lang),
    episode_ui_trend_panel(con, obj, lang = lang),
    episode_ui_denominator_panel(obj, lang = lang),
    shiny::tags$div(
      style = "display:flex;gap:16px;align-items:flex-start;",
      shiny::tags$div(style = "flex:1;", episode_ui_demography_panel(obj, lang = lang)),
      shiny::tags$div(style = "flex:1;", episode_ui_geo_panel(obj, lang = lang))
    ),
    episode_ui_places_panel(con, cluster_id, obj, lang = lang),
    episode_ui_resistance_panel(lang = lang),
    episode_ui_similar_clusters_panel(con, cluster_id, lang = lang),
    episode_ui_report_panel(con, cluster_id, current_user, lang = lang),
    if (is.null(current_user)) {
      episode_ui_linelist_locked_panel(lang = lang)
    } else {
      episode_ui_linelist_panel(con, cluster_id, obj, lang = lang)
    },
    episode_ui_settings_panel(con, cluster_id, lang = lang)
  )
}

#' The line list panel's locked state for anonymous viewers
#'
#' Rendered as a locked panel explaining that signing in reveals it,
#' not as an absent tab - so it stays in the same position in the panel
#' order either way.
#' @keywords internal
#' @noRd
episode_ui_linelist_locked_panel <- function(lang = "nl") {
  episode_ui_panel(
    episode_tr("linelist.locked_title", lang = lang),
    shiny::tags$div(
      class = "episode-locked-panel",
      shiny::tags$span(class = "episode-lock-icon", "\U0001F512"),
      shiny::tags$p(episode_tr("linelist.locked_message", lang = lang))
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_dossier_header <- function(obj, state, lang = "nl") {
  pal <- episode_palette()
  shiny::tagList(
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
      shiny::tags$h1(class = "episode-dossier-title", shiny::HTML(episode_ui_italicise_taxon(obj$pathogen))),
      episode_ui_chip(episode_tr(paste0("level.", obj$level), lang = lang), pal$primary),
      episode_ui_chip(episode_tr(paste0("state.", state), lang = lang), episode_ui_state_colour(state)),
      if (isTRUE(obj$changed_since_assessment)) episode_ui_chip(episode_tr("dossier.changed_badge", lang = lang), pal$tertiary_dark)
    ),
    shiny::tags$div(
      class = "episode-dossier-meta",
      style = "display:flex;gap:8px;flex-wrap:wrap;",
      shiny::tags$span(obj$place),
      shiny::tags$span(style = "color:var(--episode-faint);", "\u00b7"),
      shiny::tags$span(episode_tr("dossier.meta.cluster_id", id = obj$id, lang = lang)),
      shiny::tags$span(style = "color:var(--episode-faint);", "\u00b7"),
      shiny::tags$span(episode_tr("dossier.meta.first_last", first = obj$first_day, last = obj$last_day, lang = lang)),
      shiny::tags$span(style = "color:var(--episode-faint);", "\u00b7"),
      shiny::tags$span(shiny::HTML(episode_tr("dossier.meta.detected_by",
                                               detectors = episode_ui_code_join(obj$detectors, sep = " en "), lang = lang)))
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_stat_grid <- function(obj, lang = "nl") {
  pal <- episode_palette()
  stats <- list(
    episode_ui_stat(episode_tr("dossier.stat.observed", lang = lang), obj$n_cases,
                     episode_tr("dossier.stat.observed_sub", expected = round(obj$expected %||% NA, 1), lang = lang),
                     colour = pal$danger_dark)
  )
  if (!is.na(obj$ratio)) {
    stats <- c(stats, list(episode_ui_stat(episode_tr("dossier.stat.ratio", lang = lang), round(obj$ratio, 1),
                                            episode_tr("dossier.stat.ratio_sub", lang = lang))))
  }
  if (!is.null(obj$density)) {
    stats <- c(stats, list(episode_ui_stat(episode_tr("dossier.stat.density", lang = lang), obj$density$value,
                                            episode_tr("dossier.stat.density_sub", baseline = obj$density$baseline %||% episode_tr("misc.unknown", lang = lang), lang = lang))))
  }
  if (!is.na(obj$doubling_days)) {
    stats <- c(stats, list(episode_ui_stat(episode_tr("dossier.stat.doubling", lang = lang), paste(obj$doubling_days, "d"),
                                            episode_tr("dossier.stat.doubling_sub", lang = lang))))
  }
  isolate_phrase <- episode_count_phrase(obj$n_isolates, episode_tr("unit.isolate", lang = lang), episode_tr("unit.isolates", lang = lang))
  stats <- c(stats, list(episode_ui_stat(episode_tr("dossier.stat.unique_patients", lang = lang), obj$unique_patients,
                                          episode_tr("dossier.stat.unique_patients_sub", isolates_phrase = isolate_phrase, lang = lang))))
  if (!is.na(obj$case_free$need)) {
    stats <- c(stats, list(episode_ui_stat(
      episode_tr("dossier.stat.case_free", lang = lang),
      episode_tr("dossier.stat.case_free_value", since = obj$case_free$since, lang = lang),
      episode_tr("dossier.stat.case_free_sub", need = obj$case_free$need, lang = lang)
    )))
  }
  stats <- c(stats, list(episode_ui_stat(episode_tr("dossier.stat.priority", lang = lang), round(obj$priority_score, 0),
                                          episode_tr("dossier.stat.priority_sub", lang = lang))))

  shiny::tags$div(class = "episode-statgrid", stats)
}

#' The status trajectory band: one segment per classification held, not
#' per derived state
#'
#' Earlier version rendered a single bar labelled with the current
#' derived state (new/assessing/monitoring/...), which cannot answer the
#' one question this band exists for: was this ever thought to be a
#' possible epidemic before being confirmed, or was it artefact from the
#' start? Segments are therefore built from verdict-*setting* events only
#' (`episode_app_assessment_timeline()`'s `verdict` column, ignoring
#' note-only assessments that carry no classification and closures,
#' neither of which change what the cluster was judged to be) - the time
#' before the first one is its own segment, always labelled
#' `statusverloop.unassessed` rather than the generic "new" state label,
#' since "New" is easily misread as "recently created" rather than what
#' it actually means here: nobody has looked at it yet.
#'
#' Segments are equal-width, not time-proportional - a near-simultaneous
#' pair of events would otherwise render as an unreadable sliver: each
#' segment's own start date is shown instead, which carries the same
#' information without that failure mode.
#'
#' @param obj A list from `episode_cluster_object()`.
#' @param timeline A data frame from `episode_app_assessment_timeline()`.
#' @param lang Session language.
#' @keywords internal
#' @noRd
episode_ui_trajectory <- function(obj, timeline, lang = "nl") {
  verdict_events <- timeline[timeline$kind == "assessment" & !is.na(timeline$verdict), , drop = FALSE]
  starts <- c(obj$opened_at, verdict_events$at)
  labels <- c(episode_tr("statusverloop.unassessed", lang = lang), verdict_events$verdict_label)
  colours <- c(episode_palette()$muted, vapply(verdict_events$verdict, episode_ui_verdict_colour, character(1)))

  shiny::tags$section(
    class = "episode-trajectory",
    shiny::tags$div(class = "episode-trajectory-title", episode_tr("statusverloop.title", lang = lang)),
    shiny::tags$div(
      class = "episode-trajectory-track",
      lapply(seq_along(starts), function(i) {
        shiny::tags$div(
          style = "flex:1;",
          shiny::tags$div(class = "episode-trajectory-seg-bar", style = sprintf("background:%s;", colours[i])),
          shiny::tags$div(class = "episode-trajectory-seg-label", labels[i]),
          shiny::tags$div(class = "episode-trajectory-seg-time", episode_ui_format_datetime(starts[i], fmt = "%d-%m-%Y"))
        )
      })
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_interpretation_panel <- function(obj, lang = "nl") {
  generated <- episode_interpretation_generate(obj, lang = lang)
  paragraphs <- generated$text[!startsWith(generated$fired, "recommendation.")]
  recommendation <- generated$text[startsWith(generated$fired, "recommendation.")]
  pal <- episode_palette()

  episode_ui_panel(
    episode_tr("panel.interpretation.title", lang = lang), aside = episode_tr("panel.interpretation.aside", lang = lang),
    shiny::tags$div(
      class = "episode-interpretation",
      if (length(paragraphs) == 0) {
        shiny::tags$p(class = "episode-panel-empty", episode_tr("panel.interpretation.empty", lang = lang))
      } else {
        lapply(paragraphs, shiny::tags$p)
      },
      if (length(recommendation) > 0) {
        shiny::tags$div(class = "episode-advies", recommendation[1])
      }
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_epicurve_panel <- function(con, cluster_id, obj, lang = "nl") {
  curve <- episode_app_epi_curve(con, cluster_id)
  incomplete_days <- obj$completeness$incomplete_days %||% 0
  days_phrase <- episode_count_phrase(incomplete_days, episode_tr("unit.day", lang = lang), episode_tr("unit.days", lang = lang))
  note <- if (incomplete_days > 0) episode_tr("panel.epicurve.note", days_phrase = days_phrase, lang = lang) else NULL

  episode_ui_panel(
    episode_tr("panel.epicurve.title", lang = lang), note = note,
    shiny::renderPlot(episode_ui_epi_curve_chart(curve, lang = lang), height = 210)
  )
}

#' @keywords internal
#' @noRd
episode_ui_trend_panel <- function(con, obj, lang = "nl") {
  trend <- episode_app_trend(con, obj$stream_id)
  if (nrow(trend) < 4) {
    return(episode_ui_panel_empty(episode_tr("panel.trend.title", lang = lang), shiny::HTML(episode_tr("panel.trend.unavailable", lang = lang))))
  }
  episode_ui_panel(
    episode_tr("panel.trend.title", lang = lang),
    aside = episode_tr("panel.trend.aside", weeks = nrow(trend), lang = lang),
    note = shiny::HTML(episode_tr("panel.trend.note", lang = lang)),
    shiny::renderPlot(episode_ui_trend_chart(trend, lang = lang), height = 230)
  )
}

#' @keywords internal
#' @noRd
episode_ui_rt_panel <- function(obj, lang = "nl") {
  # Suppressed entirely when rt_applicable is false - not even an
  # empty-state panel, unlike a section that is merely awaiting more data.
  if (!isTRUE(obj$rt_applicable)) return(NULL)
  if (is.null(obj$rt) || nrow(obj$rt) == 0) {
    msg <- episode_tr(paste0("panel.rt.unavailable.", obj$rt_unavailable_reason %||% "insufficient_history"), lang = lang)
    return(episode_ui_panel_empty(episode_tr("panel.rt.title", lang = lang), msg))
  }
  episode_ui_panel(
    episode_tr("panel.rt.title", lang = lang),
    note = episode_tr("panel.rt.note", lang = lang),
    shiny::renderPlot(episode_ui_rt_chart(obj$rt, lang = lang), height = 200)
  )
}

#' @keywords internal
#' @noRd
episode_ui_similar_clusters_panel <- function(con, cluster_id, lang = "nl") {
  similar <- episode_app_similar_clusters(con, cluster_id, lang = lang)
  if (nrow(similar) == 0) {
    return(episode_ui_panel_empty(episode_tr("panel.similar.title", lang = lang), episode_tr("panel.similar.unavailable", lang = lang)))
  }
  episode_ui_panel(
    episode_tr("panel.similar.title", lang = lang),
    shiny::tags$table(
      class = "episode-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(episode_tr("panel.similar.col.place", lang = lang)),
        shiny::tags$th(episode_tr("panel.similar.col.cases", lang = lang)),
        shiny::tags$th(episode_tr("panel.similar.col.verdict", lang = lang)),
        shiny::tags$th(episode_tr("panel.similar.col.closed_at", lang = lang))
      )),
      shiny::tags$tbody(
        lapply(seq_len(nrow(similar)), function(i) {
          row <- similar[i, ]
          shiny::tags$tr(
            shiny::tags$td(paste0(row$level_label, " \u00b7 ", row$place)),
            shiny::tags$td(row$n_cases),
            shiny::tags$td(if (is.na(row$verdict_label)) episode_tr("misc.dash", lang = lang) else row$verdict_label),
            shiny::tags$td(if (is.na(row$closed_at)) episode_tr("misc.unknown", lang = lang) else episode_ui_format_datetime(row$closed_at, fmt = "%d-%m-%Y"))
          )
        })
      )
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_denominator_panel <- function(obj, lang = "nl") {
  if (is.null(obj$denominator) || is.null(obj$denominator$series) || nrow(obj$denominator$series) < 2) {
    return(episode_ui_panel_empty(episode_tr("panel.denominator.title", lang = lang), episode_tr("panel.denominator.unavailable", lang = lang)))
  }
  episode_ui_panel(
    episode_tr("panel.denominator.title", lang = lang), aside = episode_tr("panel.denominator.aside", lang = lang),
    note = episode_tr("panel.denominator.note", lang = lang),
    shiny::renderPlot(episode_ui_denominator_chart(obj$denominator$series, lang = lang), height = 200)
  )
}

#' @keywords internal
#' @noRd
episode_ui_demography_panel <- function(obj, lang = "nl") {
  if (is.null(obj$demography) || is.null(obj$demography$bands)) {
    return(episode_ui_panel_empty(episode_tr("panel.demography.title", lang = lang), episode_tr("misc.none", lang = lang)))
  }
  episode_ui_panel(
    episode_tr("panel.demography.title", lang = lang), note = episode_tr("panel.demography.note", lang = lang),
    episode_ui_pyramid(obj$demography$bands, lang = lang)
  )
}

#' @keywords internal
#' @noRd
episode_ui_geo_panel <- function(obj, lang = "nl") {
  if (is.null(obj$concentration)) {
    return(episode_ui_panel_empty(episode_tr("panel.geo.title", lang = lang), episode_tr("panel.geo.empty", lang = lang),
                                   aside = episode_tr("panel.geo.aside", lang = lang)))
  }
  map_chart <- episode_ui_geo_map_chart(obj$concentration$rows)
  episode_ui_panel(
    episode_tr("panel.geo.title", lang = lang), aside = episode_tr("panel.geo.aside", lang = lang),
    if (is.null(map_chart)) {
      episode_ui_bars(utils::head(obj$concentration$rows, 8), unit = episode_tr("panel.geo.unit", lang = lang))
    } else {
      shiny::renderPlot(map_chart, height = 260)
    }
  )
}

#' @keywords internal
#' @noRd
episode_ui_places_panel <- function(con, cluster_id, obj, lang = "nl") {
  cases <- episode_db_cluster_cases(con, cluster_id)
  is_hospital <- obj$level == "pathogen_ward" || (nrow(cases) > 0 && !all(is.na(cases$ward)))
  title <- episode_tr(if (is_hospital) "panel.places.title_ward" else "panel.places.title_institution", lang = lang)

  group_col <- if (is_hospital) "ward" else "specialism"
  if (nrow(cases) == 0 || all(is.na(cases[[group_col]]))) {
    return(episode_ui_panel_empty(title, episode_tr("misc.none", lang = lang)))
  }
  tab <- sort(table(cases[[group_col]]), decreasing = TRUE)
  rows <- data.frame(label = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
  episode_ui_panel(title, episode_ui_bars(rows))
}

#' @keywords internal
#' @noRd
episode_ui_resistance_panel <- function(lang = "nl") {
  # Susceptibility data is not part of the ingestion contract, so this
  # panel is always a placeholder.
  episode_ui_panel_empty(episode_tr("panel.resistance.title", lang = lang), episode_tr("panel.resistance.unavailable", lang = lang))
}

#' @keywords internal
#' @noRd
episode_ui_linelist_panel <- function(con, cluster_id, obj, lang = "nl") {
  ll <- episode_app_linelist(con, cluster_id)
  cols <- c("source_key", "sample_date", "sex", "age", "pc", "ward", "specialism")
  labels <- vapply(cols, function(c) episode_tr(paste0("panel.linelist.col.", switch(c,
    source_key = "case", sample_date = "date", sex = "sex", age = "age", pc = "pc",
    ward = "ward", specialism = "specialism"
  )), lang = lang), character(1))

  episode_ui_panel(
    episode_tr("panel.linelist.title", lang = lang),
    aside = episode_tr("panel.linelist.aside", shown = nrow(ll), total = obj$n_cases, lang = lang),
    shiny::tags$table(
      class = "episode-table",
      shiny::tags$thead(shiny::tags$tr(lapply(labels, shiny::tags$th))),
      shiny::tags$tbody(
        lapply(seq_len(nrow(ll)), function(i) {
          row <- ll[i, ]
          shiny::tags$tr(lapply(cols, function(c) shiny::tags$td(as.character(row[[c]] %||% episode_tr("misc.dash", lang = lang)))))
        })
      )
    )
  )
}

#' The report panel: existing versions plus a render-on-demand button
#'
#' "The cron pre-renders for every cluster with a verdict of
#' possible_epidemic or above; assessors can re-render on demand,
#' producing a new version". The button only
#' renders for a signed-in user, matching every other write action; the
#' version list itself is visible to anyone, since a rendered report's
#' existence is not sensitive the way its line-list *contents* are.
#' @keywords internal
#' @noRd
episode_ui_report_panel <- function(con, cluster_id, current_user, lang = "nl") {
  reports <- episode_db_reports_for_cluster(con, cluster_id)
  episode_ui_panel(
    episode_tr("panel.report.title", lang = lang),
    if (nrow(reports) == 0) {
      shiny::tags$p(class = "episode-panel-empty", episode_tr("panel.report.empty", lang = lang))
    } else {
      shiny::tags$ul(
        style = "font-size:12.5px;padding-left:18px;",
        lapply(rev(seq_len(nrow(reports))), function(i) {
          row <- reports[i, ]
          shiny::tags$li(episode_tr("panel.report.version_line", version = row$version_no,
                                     when = episode_ui_format_datetime(row$rendered_at, fmt = "%d-%m-%Y %H:%M"),
                                     lang = lang))
        })
      )
    },
    if (!is.null(current_user)) {
      shiny::tagList(
        shiny::uiOutput("report_render_error"),
        shiny::tags$button(
          id = "report-render-button",
          class = "episode-btn",
          # Disabling the button and showing the "generating" text happens
          # here, client-side, at click time - the render itself is a
          # long, synchronous server call that blocks the whole session,
          # so nothing pushed from a Shiny observer could appear on
          # screen before it finishes. episode_app_server_report() clears
          # both again once it is done (a fresh dossier pane on success,
          # a small reset script alongside the error message on failure).
          onclick = sprintf(
            "this.disabled=true; document.getElementById('report-render-pending').style.display='block'; Shiny.setInputValue('report_render_submit', %d, {priority: 'event'})",
            cluster_id
          ),
          episode_tr("panel.report.render_button", lang = lang)
        ),
        shiny::tags$p(
          id = "report-render-pending", style = "display:none;font-size:12.5px;color:var(--episode-muted);margin-top:6px;",
          episode_tr("panel.report.pending", lang = lang)
        )
      )
    }
  )
}

#' @keywords internal
#' @noRd
episode_ui_settings_panel <- function(con, cluster_id, lang = "nl") {
  settings <- episode_app_detection_settings(con, cluster_id)
  # list(), not c(): a shiny::HTML() value (the detectors row) loses its
  # "html" class and gets escaped as literal text if combined with a
  # plain string via c() - list() keeps each element intact.
  rows <- list(
    list(episode_tr("panel.settings.detectors", lang = lang), shiny::HTML(episode_ui_code_join(settings$detectors))),
    list(episode_tr("panel.settings.aggregation", lang = lang), episode_tr("panel.settings.aggregation_value", lang = lang)),
    list(episode_tr("panel.settings.population_offset", lang = lang),
      episode_tr(if (!is.null(settings$population_offset)) "panel.settings.population_offset_patient_days" else "panel.settings.population_offset_none", lang = lang)),
    list(episode_tr("panel.settings.case_free_days", lang = lang), as.character(settings$case_free_days)),
    list(episode_tr("panel.settings.last_run", lang = lang),
      episode_tr("panel.settings.last_run_value",
                 when = if (is.null(settings$last_run_when) || is.na(settings$last_run_when)) episode_tr("misc.unknown", lang = lang) else episode_ui_format_datetime(settings$last_run_when, fmt = "%d-%m-%Y %H:%M"),
                 host = settings$last_run_host %||% episode_tr("misc.unknown", lang = lang), lang = lang)),
    list(episode_tr("panel.settings.pkg_versions", lang = lang), settings$pkg_versions %||% episode_tr("misc.unknown", lang = lang))
  )
  episode_ui_panel(
    episode_tr("panel.settings.title", lang = lang),
    shiny::tags$dl(class = "episode-settings",
                    lapply(rows, function(r) shiny::tags$div(shiny::tags$dt(r[[1]]), shiny::tags$dd(r[[2]]))))
  )
}

#' The right-hand assessment rail
#'
#' The timeline ("Verloop") is always visible, to anonymous viewers too,
#' as an append-only record of every assessment. The classification
#' form, closure and mute actions render only for a signed-in user -
#' signing in is required to classify, never to read.
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param lang Session language.
#' @param current_user The session's signed-in user row, or `NULL`.
#' @keywords internal
#' @noRd
episode_ui_assessment_rail <- function(con, cluster_id, lang = "nl", current_user = NULL) {
  obj <- episode_cluster_object(con, cluster_id, lang = lang)
  timeline <- episode_app_assessment_timeline(con, cluster_id, lang = lang)

  shiny::tags$div(
    class = "episode-assessment-rail",
    shiny::tags$div(
      class = "episode-verloop",
      shiny::tags$div(class = "episode-verloop-title", episode_tr("verloop.title", lang = lang)),
      if (nrow(timeline) == 0) {
        shiny::tags$p(class = "episode-verloop-empty",
                      shiny::HTML(episode_tr("verloop.not_assessed", first = obj$first_day,
                                              detectors = episode_ui_code_join(obj$detectors, sep = " en "), lang = lang)))
      } else {
        lapply(rev(seq_len(nrow(timeline))), function(i) episode_ui_timeline_entry(timeline[i, ], lang = lang))
      }
    ),
    if (!is.null(current_user)) episode_ui_assessment_form(cluster_id, obj, lang = lang)
  )
}

#' One row of the assessment timeline
#' @keywords internal
#' @noRd
episode_ui_timeline_entry <- function(row, lang = "nl") {
  shiny::tags$div(
    class = "episode-timeline-entry",
    shiny::tags$div(class = "episode-timeline-meta",
                     sprintf("%s \u00b7 %s", episode_ui_format_datetime(row$at), row$actor)),
    if (row$kind == "closure") {
      shiny::tags$div(episode_tr("activity.action_closed", lang = lang))
    } else {
      shiny::tagList(
        if (!is.na(row$verdict_label)) {
          shiny::tags$div(class = "episode-timeline-verdict", row$verdict_label)
        },
        if (!is.na(row$rationale) && nzchar(row$rationale)) {
          shiny::tags$div(class = "episode-timeline-rationale", row$rationale)
        }
      )
    }
  )
}

#' The classification form, closure and mute actions for a signed-in user
#' @keywords internal
#' @noRd
episode_ui_assessment_form <- function(cluster_id, obj, lang = "nl") {
  pal <- episode_palette()
  # Ordered mild/terminal to severe - artefact and expected_variation
  # are both terminal (close immediately), the rest escalate.
  verdicts <- c("artefact", "expected_variation", "cluster_not_yet",
                "possible_epidemic", "confirmed_epidemic")
  mute_reasons <- c("seasonal", "screening_campaign", "method_change", "known_source", "other")

  verdict_options <- c(
    list(list(value = "", label = episode_tr("assessment.verdict_none", lang = lang), colour = pal$muted)),
    lapply(verdicts, function(v) list(
      value = v, label = episode_tr(paste0("verdict.", v), lang = lang),
      hint = episode_tr(paste0("verdict.", v, ".hint"), lang = lang),
      colour = episode_ui_verdict_colour(v)
    ))
  )
  mute_options <- lapply(mute_reasons, function(r) list(
    value = r, label = episode_tr(paste0("assessment.mute_reason.", r), lang = lang), colour = pal$secondary
  ))

  shiny::tags$div(
    class = "episode-panel-body", style = "border-top:1px solid var(--episode-rule);padding:16px;",
    shiny::tags$div(class = "episode-form-group",
                     shiny::tags$label(class = "episode-form-label", episode_tr("assessment.verdict_label", lang = lang)),
                     episode_ui_picker("assess_verdict", verdict_options)),
    shiny::tags$div(class = "episode-form-group",
                     shiny::tags$label(class = "episode-form-label", episode_tr("assessment.rationale_label", lang = lang)),
                     shiny::tags$textarea(id = "assess_rationale", rows = 3,
                                           placeholder = episode_tr("assessment.rationale_placeholder", lang = lang))),
    shiny::tags$div(class = "episode-form-group",
                     shiny::tags$label(class = "episode-form-label", episode_tr("assessment.snooze_label", lang = lang)),
                     shiny::tags$input(type = "date", id = "assess_snooze")),
    shiny::tags$div(id = "assess_error"),
    shiny::tags$div(
      class = "episode-form-actions",
      shiny::tags$button(
        class = "episode-btn episode-btn-primary",
        onclick = sprintf(
          "Shiny.setInputValue('assess_submit', {cluster_id: %d, verdict: document.getElementById('assess_verdict').value, rationale: document.getElementById('assess_rationale').value, snooze: document.getElementById('assess_snooze').value}, {priority: 'event'})",
          cluster_id
        ),
        episode_tr("assessment.submit", lang = lang)
      ),
      shiny::tags$button(
        class = "episode-btn", `data-confirm` = episode_tr("assessment.close_confirm", lang = lang),
        onclick = sprintf(
          "if(confirm(this.dataset.confirm)){ Shiny.setInputValue('assess_close', %d, {priority: 'event'}) }",
          cluster_id
        ),
        episode_tr("assessment.close_button", lang = lang)
      )
    ),
    shiny::tags$hr(),
    shiny::tags$p(class = "episode-form-hint", episode_tr("assessment.mute_intro", lang = lang)),
    shiny::tags$div(
      class = "episode-form-group",
      shiny::tags$label(class = "episode-form-label", episode_tr("assessment.mute_title", lang = lang)),
      episode_ui_picker("mute_reason", mute_options, selected = mute_reasons[1])
    ),
    shiny::tags$div(style = "display:flex;gap:8px;",
                     shiny::tags$div(class = "episode-form-group", style = "flex:1;",
                                      shiny::tags$label(class = "episode-form-label", episode_tr("assessment.mute_from_label", lang = lang)),
                                      shiny::tags$input(type = "date", id = "mute_from", value = as.character(Sys.Date()))),
                     shiny::tags$div(class = "episode-form-group", style = "flex:1;",
                                      shiny::tags$label(class = "episode-form-label", episode_tr("assessment.mute_until_label", lang = lang)),
                                      shiny::tags$input(type = "date", id = "mute_until"))),
    shiny::tags$button(
      class = "episode-btn",
      onclick = sprintf(
        "Shiny.setInputValue('assess_mute_submit', {stream_id: %d, reason: document.getElementById('mute_reason').value, muted_from: document.getElementById('mute_from').value, muted_until: document.getElementById('mute_until').value}, {priority: 'event'})",
        obj$stream_id
      ),
      episode_tr("assessment.mute_submit", lang = lang)
    )
  )
}

#' The read-only Streams screen
#' @keywords internal
#' @noRd
episode_ui_streams_screen <- function(screen, lang = "nl") {
  streams <- screen$streams
  pager <- if (!is.null(screen$n_pages) && screen$n_pages > 1) {
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;margin-bottom:12px;font-size:12.5px;",
      shiny::tags$button(
        class = "episode-btn", disabled = if (screen$page <= 1) NA else NULL,
        onclick = sprintf("Shiny.setInputValue('streams_page_select', %d, {priority: 'event'})", screen$page - 1L),
        episode_tr("streams.page_prev", lang = lang)
      ),
      shiny::tags$span(episode_tr("streams.page_of", page = screen$page, n_pages = screen$n_pages, lang = lang)),
      shiny::tags$button(
        class = "episode-btn", disabled = if (screen$page >= screen$n_pages) NA else NULL,
        onclick = sprintf("Shiny.setInputValue('streams_page_select', %d, {priority: 'event'})", screen$page + 1L),
        episode_tr("streams.page_next", lang = lang)
      )
    )
  }
  shiny::tags$div(
    class = "episode-streams-screen",
    shiny::tags$h1(style = "font-size:22px;font-weight:600;margin-bottom:4px;", episode_tr("streams.title", lang = lang)),
    shiny::tags$p(style = "font-size:12.5px;color:var(--episode-muted);margin-bottom:16px;", episode_tr("streams.note", lang = lang)),
    pager,
    if (nrow(streams) == 0) {
      shiny::tags$p(class = "episode-panel-empty", episode_tr("rail.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episode-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episode_tr("streams.col.stream", lang = lang)),
          shiny::tags$th(episode_tr("streams.col.level", lang = lang)),
          shiny::tags$th(episode_tr("streams.col.denominator", lang = lang)),
          shiny::tags$th(episode_tr("streams.col.first_seen", lang = lang)),
          shiny::tags$th(episode_tr("streams.col.last_seen", lang = lang)),
          shiny::tags$th(episode_tr("streams.col.baseline_excluded", lang = lang))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(streams)), function(i) {
            row <- streams[i, ]
            excluded <- if (!is.null(streams$baseline_excluded)) streams$baseline_excluded[[i]] else NULL
            excluded_text <- if (is.null(excluded) || nrow(excluded) == 0) {
              episode_tr("misc.dash", lang = lang)
            } else {
              paste(sprintf("%s \u2013 %s", excluded$first_day, excluded$last_day), collapse = "; ")
            }
            shiny::tags$tr(
              shiny::tags$td(shiny::HTML(episode_ui_italicise_taxon(row$pathogen))),
              shiny::tags$td(episode_tr(paste0("level.", row$level), lang = lang)),
              shiny::tags$td(row$denominator), shiny::tags$td(row$first_seen), shiny::tags$td(row$last_seen),
              shiny::tags$td(excluded_text)
            )
          })
        )
      )
    },
    if (!is.null(screen$config_snapshot)) {
      episode_ui_panel(
        episode_tr("streams.config.title", lang = lang),
        shiny::tags$pre(style = "font-size:11.5px;white-space:pre-wrap;",
                         jsonlite::toJSON(screen$config_snapshot, auto_unbox = TRUE, pretty = TRUE))
      )
    }
  )
}
