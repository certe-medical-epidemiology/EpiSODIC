#' The dossier pane
#'
#' Assembles the full cluster dossier: header, stat grid, status
#' trajectory band, Duiding, then the analytical panels in the order
#' `episode-mockup.jsx` and MILESTONES.md M2 specify.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param lang Session language.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episode_ui_dossier <- function(con, cluster_id, lang = "nl") {
  obj <- episode_cluster_object(con, cluster_id, lang = lang)
  state <- episode_app_derive_state_for_cluster(con, cluster_id)
  pal <- episode_palette()

  shiny::tags$div(
    class = "episode-dossier",
    episode_ui_dossier_header(obj, state, lang = lang),
    episode_ui_stat_grid(obj, lang = lang),
    episode_ui_trajectory(obj, state, lang = lang),
    episode_ui_duiding_panel(obj, lang = lang),
    episode_ui_epicurve_panel(con, cluster_id, obj, lang = lang),
    episode_ui_trend_panel(con, obj, lang = lang),
    episode_ui_denominator_panel(obj, lang = lang),
    shiny::tags$div(
      style = "display:flex;gap:16px;align-items:flex-start;",
      shiny::tags$div(style = "flex:1;", episode_ui_demography_panel(obj, lang = lang)),
      shiny::tags$div(style = "flex:1;", episode_ui_geo_panel(obj, lang = lang))
    ),
    episode_ui_places_panel(con, cluster_id, obj, lang = lang),
    episode_ui_resistance_panel(lang = lang),
    episode_ui_linelist_panel(con, cluster_id, obj, lang = lang),
    episode_ui_settings_panel(con, cluster_id, lang = lang)
  )
}

#' @keywords internal
#' @noRd
episode_ui_dossier_header <- function(obj, state, lang = "nl") {
  pal <- episode_palette()
  shiny::tagList(
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
      shiny::tags$h1(class = "episode-dossier-title", obj$pathogen),
      episode_ui_chip(episode_tr(paste0("level.", obj$level), lang = lang), pal$petrol),
      episode_ui_chip(episode_tr(paste0("state.", state), lang = lang), episode_ui_state_colour(state)),
      if (isTRUE(obj$changed_since_assessment)) episode_ui_chip(episode_tr("dossier.changed_badge", lang = lang), pal$yellowD)
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
      shiny::tags$span(episode_tr("dossier.meta.detected_by", detectors = paste(obj$detectors, collapse = " en "), lang = lang))
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
                     colour = pal$carmine)
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
      episode_tr("dossier.stat.case_free_value", since = obj$case_free$since, need = obj$case_free$need, lang = lang)
    )))
  }
  stats <- c(stats, list(episode_ui_stat(episode_tr("dossier.stat.priority", lang = lang), round(obj$priority_score, 0),
                                          episode_tr("dossier.stat.priority_sub", lang = lang))))

  shiny::tags$div(class = "episode-statgrid", stats)
}

#' @keywords internal
#' @noRd
episode_ui_trajectory <- function(obj, state, lang = "nl") {
  colour <- episode_ui_state_colour(state)
  shiny::tags$section(
    class = "episode-trajectory",
    shiny::tags$div(class = "episode-trajectory-title", episode_tr("statusverloop.title", lang = lang)),
    shiny::tags$div(
      class = "episode-trajectory-track",
      shiny::tags$div(
        style = "flex:1;",
        shiny::tags$div(class = "episode-trajectory-seg-bar", style = sprintf("background:%s;", colour)),
        shiny::tags$div(class = "episode-trajectory-seg-label", episode_tr(paste0("state.", state), lang = lang)),
        shiny::tags$div(class = "episode-trajectory-seg-time", episode_tr("statusverloop.now", lang = lang))
      )
    )
  )
}

#' @keywords internal
#' @noRd
episode_ui_duiding_panel <- function(obj, lang = "nl") {
  generated <- episode_duiding_generate(obj, lang = lang)
  paragraphs <- generated$text[!startsWith(generated$fired, "recommendation.")]
  recommendation <- generated$text[startsWith(generated$fired, "recommendation.")]
  pal <- episode_palette()

  episode_ui_panel(
    episode_tr("panel.duiding.title", lang = lang), aside = episode_tr("panel.duiding.aside", lang = lang),
    shiny::tags$div(
      class = "episode-duiding",
      if (length(paragraphs) == 0) {
        shiny::tags$p(class = "episode-panel-empty", episode_tr("panel.duiding.empty", lang = lang))
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
    return(episode_ui_panel_empty(episode_tr("panel.trend.title", lang = lang), episode_tr("panel.trend.unavailable", lang = lang)))
  }
  episode_ui_panel(
    episode_tr("panel.trend.title", lang = lang),
    aside = episode_tr("panel.trend.aside", weeks = nrow(trend), lang = lang),
    note = episode_tr("panel.trend.note", lang = lang),
    shiny::renderPlot(episode_ui_trend_chart(trend, lang = lang), height = 230)
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
  episode_ui_panel(
    episode_tr("panel.geo.title", lang = lang), aside = episode_tr("panel.geo.aside", lang = lang),
    episode_ui_bars(utils::head(obj$concentration$rows, 8), unit = episode_tr("panel.geo.unit", lang = lang))
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
  # No M1 ingestion path carries susceptibility data yet (ARCHITECTURE.md
  # never lists it as a captured case field); always a placeholder for now,
  # see QUESTIONS.md.
  episode_ui_panel_empty(episode_tr("panel.resistance.title", lang = lang), episode_tr("panel.resistance.unavailable", lang = lang))
}

#' @keywords internal
#' @noRd
episode_ui_linelist_panel <- function(con, cluster_id, obj, lang = "nl") {
  ll <- episode_app_linelist(con, cluster_id)
  cols <- c("source_key", "sample_date", "sex", "age", "pc4", "ward", "specialism")
  labels <- vapply(cols, function(c) episode_tr(paste0("panel.linelist.col.", switch(c,
    source_key = "case", sample_date = "date", sex = "sex", age = "age", pc4 = "pc4",
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

#' @keywords internal
#' @noRd
episode_ui_settings_panel <- function(con, cluster_id, lang = "nl") {
  settings <- episode_app_detection_settings(con, cluster_id)
  rows <- list(
    c(episode_tr("panel.settings.detectors", lang = lang), paste(settings$detectors, collapse = ", ")),
    c(episode_tr("panel.settings.aggregation", lang = lang), episode_tr("panel.settings.aggregation_value", lang = lang)),
    c(episode_tr("panel.settings.population_offset", lang = lang),
      episode_tr(if (!is.null(settings$population_offset)) "panel.settings.population_offset_patient_days" else "panel.settings.population_offset_none", lang = lang)),
    c(episode_tr("panel.settings.case_free_days", lang = lang), as.character(settings$case_free_days)),
    c(episode_tr("panel.settings.last_run", lang = lang),
      episode_tr("panel.settings.last_run_value", when = settings$last_run_when %||% episode_tr("misc.unknown", lang = lang),
                 host = settings$last_run_host %||% episode_tr("misc.unknown", lang = lang), lang = lang)),
    c(episode_tr("panel.settings.pkg_versions", lang = lang), settings$pkg_versions %||% episode_tr("misc.unknown", lang = lang))
  )
  episode_ui_panel(
    episode_tr("panel.settings.title", lang = lang),
    shiny::tags$dl(class = "episode-settings",
                    lapply(rows, function(r) shiny::tags$div(shiny::tags$dt(r[1]), shiny::tags$dd(r[2]))))
  )
}

#' The right-hand assessment rail (read-only in M2: no login, no writes)
#'
#' Shows only the timeline ("Verloop"), matching what an anonymous M2
#' reader is entitled to see. The classification form itself is M3 scope
#' (ARCHITECTURE.md section 12: "Login only to classify").
#' @keywords internal
#' @noRd
episode_ui_assessment_rail <- function(con, cluster_id, lang = "nl") {
  obj <- episode_cluster_object(con, cluster_id, lang = lang)
  shiny::tags$div(
    class = "episode-assessment-rail",
    shiny::tags$div(
      class = "episode-verloop",
      shiny::tags$div(class = "episode-verloop-title", episode_tr("verloop.title", lang = lang)),
      shiny::tags$p(class = "episode-verloop-empty",
                    episode_tr("verloop.not_assessed", first = obj$first_day,
                               detectors = paste(obj$detectors, collapse = " en "), lang = lang))
    )
  )
}

#' The read-only Streams screen
#' @keywords internal
#' @noRd
episode_ui_streams_screen <- function(screen, lang = "nl") {
  streams <- screen$streams
  shiny::tags$div(
    class = "episode-streams-screen",
    shiny::tags$h1(style = "font-size:22px;font-weight:600;margin-bottom:4px;", episode_tr("streams.title", lang = lang)),
    shiny::tags$p(style = "font-size:12.5px;color:var(--episode-muted);margin-bottom:16px;", episode_tr("streams.note", lang = lang)),
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
          shiny::tags$th(episode_tr("streams.col.last_seen", lang = lang))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(streams)), function(i) {
            row <- streams[i, ]
            shiny::tags$tr(
              shiny::tags$td(row$pathogen), shiny::tags$td(episode_tr(paste0("level.", row$level), lang = lang)),
              shiny::tags$td(row$denominator), shiny::tags$td(row$first_seen), shiny::tags$td(row$last_seen)
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
