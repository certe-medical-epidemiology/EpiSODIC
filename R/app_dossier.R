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
episodic_ui_dossier <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  current_user = NULL
) {
  obj <- episodic_cluster_object(con, cluster_id, lang = lang)
  state <- episodic_app_derive_state_for_cluster(con, cluster_id)
  timeline <- episodic_app_assessment_timeline(con, cluster_id, lang = lang)
  pal <- episodic_palette()

  shiny::tags$div(
    class = "episodic-dossier",
    episodic_ui_dossier_header(obj, state, lang = lang),
    episodic_ui_stat_grid(obj, lang = lang),
    episodic_ui_trajectory(obj, timeline, lang = lang),
    episodic_ui_interpretation_panel(obj, lang = lang),
    episodic_ui_epicurve_panel(con, cluster_id, obj, lang = lang),
    episodic_ui_rt_panel(obj, lang = lang),
    episodic_ui_trend_panel(con, obj, lang = lang),
    episodic_ui_denominator_panel(obj, lang = lang),
    episodic_ui_demography_panel(obj, lang = lang),
    # Geography gets the full width of the dossier pane rather than
    # sharing a row with the demography pyramid. Half a pane is enough
    # for a bar breakdown but not for a map: at that size the postcode
    # labels the panel exists to convey are not readable, which is the
    # whole question it is meant to answer.
    episodic_ui_geo_panel(obj, lang = lang),
    episodic_ui_places_panel(con, cluster_id, obj, lang = lang),
    episodic_ui_resistance_panel(lang = lang),
    episodic_ui_suppressed_panel(con, cluster_id, lang = lang),
    episodic_ui_similar_clusters_panel(con, cluster_id, lang = lang),
    episodic_ui_report_panel(con, cluster_id, current_user, lang = lang),
    if (is.null(current_user)) {
      episodic_ui_linelist_locked_panel(lang = lang)
    } else {
      episodic_ui_linelist_panel(con, cluster_id, obj, lang = lang)
    },
    episodic_ui_settings_panel(con, cluster_id, lang = lang)
  )
}

#' The line list panel's locked state for anonymous viewers
#'
#' Rendered as a locked panel explaining that signing in reveals it,
#' not as an absent tab - so it stays in the same position in the panel
#' order either way.
#' @keywords internal
#' @noRd
episodic_ui_linelist_locked_panel <- function(
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  episodic_ui_panel(
    episodic_tr("linelist.locked_title", lang = lang),
    shiny::tags$div(
      class = "episodic-locked-panel",
      shiny::tags$span(class = "episodic-lock-icon", "\U0001F512"),
      shiny::tags$p(episodic_tr("linelist.locked_message", lang = lang))
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_dossier_header <- function(
  obj,
  state,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  shiny::tagList(
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
      # The cluster id sits with the name rather than down in the meta
      # line: it is what an assessor quotes in an email, reads out on the
      # phone and searches the archive by, so it belongs where the eye
      # lands first. Muted and upright so it reads as a label on the
      # name, not as part of the taxon.
      shiny::tags$h1(
        class = "episodic-dossier-title",
        shiny::HTML(episodic_ui_italicise_taxon(obj$pathogen)),
        shiny::tags$span(
          class = "episodic-dossier-id",
          episodic_tr("dossier.cluster_ref", id = obj$id, lang = lang)
        )
      ),
      episodic_ui_chip(
        episodic_tr(paste0("level.", obj$level), lang = lang),
        pal$primary
      ),
      episodic_ui_chip(
        episodic_tr(paste0("state.", state), lang = lang),
        episodic_ui_state_colour(state)
      ),
      if (isTRUE(obj$changed_since_assessment)) {
        episodic_ui_chip(
          episodic_tr("dossier.changed_badge", lang = lang),
          pal$tertiary_dark
        )
      }
    ),
    shiny::tags$div(
      class = "episodic-dossier-meta",
      style = "display:flex;gap:8px;flex-wrap:wrap;",
      shiny::tags$span(obj$place),
      shiny::tags$span(style = "color:var(--episodic-faint);", "\u00b7"),
      shiny::tags$span(episodic_tr(
        "dossier.meta.first_last",
        first = obj$first_day,
        last = obj$last_day,
        lang = lang
      )),
      shiny::tags$span(style = "color:var(--episodic-faint);", "\u00b7"),
      shiny::tags$span(shiny::HTML(episodic_tr(
        "dossier.meta.detected_by",
        detectors = episodic_ui_code_join(obj$detectors, sep = " en "),
        lang = lang
      )))
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_stat_grid <- function(obj, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  # `expected` is NA for detectors that fit no baseline at all
  # (same_place, rare_trigger), which is a different statement from an
  # expectation of zero - say "unknown" rather than printing "expected
  # NA" under the case count.
  expected_label <- if (is.null(obj$expected) || is.na(obj$expected)) {
    episodic_tr("misc.unknown", lang = lang)
  } else {
    round(obj$expected, 1)
  }
  stats <- list(
    episodic_ui_stat(
      episodic_tr("dossier.stat.observed", lang = lang),
      obj$n_cases,
      episodic_tr(
        "dossier.stat.observed_sub",
        expected = expected_label,
        lang = lang
      ),
      colour = pal$danger_dark
    )
  )
  if (!is.null(obj$ratio) && !is.na(obj$ratio)) {
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("dossier.stat.ratio", lang = lang),
        round(obj$ratio, 1),
        episodic_tr("dossier.stat.ratio_sub", lang = lang)
      ))
    )
  }
  if (!is.null(obj$density)) {
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("dossier.stat.density", lang = lang),
        obj$density$value,
        episodic_tr(
          "dossier.stat.density_sub",
          baseline = obj$density$baseline %||%
            episodic_tr("misc.unknown", lang = lang),
          lang = lang
        )
      ))
    )
  }
  if (!is.na(obj$doubling_days)) {
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("dossier.stat.doubling", lang = lang),
        paste(obj$doubling_days, "d"),
        episodic_tr("dossier.stat.doubling_sub", lang = lang)
      ))
    )
  }
  isolate_phrase <- episodic_count_phrase(
    obj$n_isolates,
    episodic_tr("unit.isolate", lang = lang),
    episodic_tr("unit.isolates", lang = lang)
  )
  stats <- c(
    stats,
    list(episodic_ui_stat(
      episodic_tr("dossier.stat.unique_patients", lang = lang),
      obj$unique_patients,
      episodic_tr(
        "dossier.stat.unique_patients_sub",
        isolates_phrase = isolate_phrase,
        lang = lang
      )
    ))
  )
  if (!is.na(obj$case_free$need)) {
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("dossier.stat.case_free", lang = lang),
        episodic_tr(
          "dossier.stat.case_free_value",
          since = obj$case_free$since,
          lang = lang
        ),
        episodic_tr(
          "dossier.stat.case_free_sub",
          need = obj$case_free$need,
          lang = lang
        )
      ))
    )
  }
  stats <- c(
    stats,
    list(episodic_ui_stat(
      episodic_tr("dossier.stat.priority", lang = lang),
      round(obj$priority_score, 0),
      episodic_tr("dossier.stat.priority_sub", lang = lang)
    ))
  )

  shiny::tags$div(class = "episodic-statgrid", stats)
}

#' The status trajectory band: one segment per classification held, not
#' per derived state
#'
#' Earlier version rendered a single bar labelled with the current
#' derived state (new/assessing/monitoring/...), which cannot answer the
#' one question this band exists for: was this ever thought to be a
#' possible epidemic before being confirmed, or was it artefact from the
#' start? Segments are therefore built from verdict-*setting* events only
#' (`episodic_app_assessment_timeline()`'s `verdict` column, ignoring
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
#' @param obj A list from `episodic_cluster_object()`.
#' @param timeline A data frame from `episodic_app_assessment_timeline()`.
#' @param lang Session language.
#' @keywords internal
#' @noRd
episodic_ui_trajectory <- function(
  obj,
  timeline,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  verdict_events <- timeline[
    timeline$kind == "assessment" & !is.na(timeline$verdict),
    ,
    drop = FALSE
  ]
  starts <- c(obj$opened_at, verdict_events$at)
  labels <- c(
    episodic_tr("statusverloop.unassessed", lang = lang),
    verdict_events$verdict_label
  )
  colours <- c(
    episodic_palette()$muted,
    vapply(verdict_events$verdict, episodic_ui_verdict_colour, character(1))
  )

  shiny::tags$section(
    class = "episodic-trajectory",
    shiny::tags$div(
      class = "episodic-trajectory-title",
      episodic_tr("statusverloop.title", lang = lang)
    ),
    shiny::tags$div(
      class = "episodic-trajectory-track",
      lapply(seq_along(starts), function(i) {
        shiny::tags$div(
          style = "flex:1;",
          shiny::tags$div(
            class = "episodic-trajectory-seg-bar",
            style = sprintf("background:%s;", colours[i])
          ),
          shiny::tags$div(class = "episodic-trajectory-seg-label", labels[i]),
          shiny::tags$div(
            class = "episodic-trajectory-seg-time",
            episodic_ui_format_datetime(starts[i], fmt = "%d-%m-%Y")
          )
        )
      })
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_interpretation_panel <- function(
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  generated <- episodic_interpretation_generate(obj, lang = lang)
  paragraphs <- generated$text[!startsWith(generated$fired, "recommendation.")]
  recommendation <- generated$text[startsWith(
    generated$fired,
    "recommendation."
  )]
  pal <- episodic_palette()

  episodic_ui_panel(
    episodic_tr("panel.interpretation.title", lang = lang),
    aside = episodic_tr("panel.interpretation.aside", lang = lang),
    shiny::tags$div(
      class = "episodic-interpretation",
      if (length(paragraphs) == 0) {
        shiny::tags$p(
          class = "episodic-panel-empty",
          episodic_tr("panel.interpretation.empty", lang = lang)
        )
      } else {
        lapply(paragraphs, shiny::tags$p)
      },
      if (length(recommendation) > 0) {
        shiny::tags$div(class = "episodic-advies", recommendation[1])
      }
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_epicurve_panel <- function(
  con,
  cluster_id,
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  curve <- episodic_app_epi_curve(con, cluster_id)
  incomplete_days <- obj$completeness$incomplete_days %||% 0
  days_phrase <- episodic_count_phrase(
    incomplete_days,
    episodic_tr("unit.day", lang = lang),
    episodic_tr("unit.days", lang = lang)
  )
  note <- if (incomplete_days > 0) {
    episodic_tr("panel.epicurve.note", days_phrase = days_phrase, lang = lang)
  } else {
    NULL
  }

  episodic_ui_panel(
    episodic_tr("panel.epicurve.title", lang = lang),
    note = note,
    shiny::renderPlot(
      episodic_ui_epi_curve_chart(curve, lang = lang),
      height = 210
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_trend_panel <- function(
  con,
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  trend <- episodic_app_trend(con, obj$stream_id)
  if (nrow(trend) < 4) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.trend.title", lang = lang),
      shiny::HTML(episodic_tr("panel.trend.unavailable", lang = lang))
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.trend.title", lang = lang),
    aside = episodic_tr("panel.trend.aside", weeks = nrow(trend), lang = lang),
    note = shiny::HTML(episodic_tr("panel.trend.note", lang = lang)),
    shiny::renderPlot(episodic_ui_trend_chart(trend, lang = lang), height = 230)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_rt_panel <- function(obj, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  # Suppressed entirely when rt_applicable is false - not even an
  # empty-state panel, unlike a section that is merely awaiting more data.
  if (!isTRUE(obj$rt_applicable)) {
    return(NULL)
  }
  if (is.null(obj$rt) || nrow(obj$rt) == 0) {
    msg <- episodic_tr(
      paste0(
        "panel.rt.unavailable.",
        obj$rt_unavailable_reason %||% "insufficient_history"
      ),
      lang = lang
    )
    return(episodic_ui_panel_empty(
      episodic_tr("panel.rt.title", lang = lang),
      msg
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.rt.title", lang = lang),
    note = episodic_tr("panel.rt.note", lang = lang),
    shiny::renderPlot(episodic_ui_rt_chart(obj$rt, lang = lang), height = 200)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_similar_clusters_panel <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  similar <- episodic_app_similar_clusters(con, cluster_id, lang = lang)
  if (nrow(similar) == 0) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.similar.title", lang = lang),
      episodic_tr("panel.similar.unavailable", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.similar.title", lang = lang),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(episodic_tr("panel.similar.col.place", lang = lang)),
        shiny::tags$th(episodic_tr("panel.similar.col.cases", lang = lang)),
        shiny::tags$th(episodic_tr("panel.similar.col.verdict", lang = lang)),
        shiny::tags$th(episodic_tr("panel.similar.col.closed_at", lang = lang))
      )),
      shiny::tags$tbody(
        lapply(seq_len(nrow(similar)), function(i) {
          row <- similar[i, ]
          shiny::tags$tr(
            shiny::tags$td(paste0(row$level_label, " \u00b7 ", row$place)),
            shiny::tags$td(row$n_cases),
            shiny::tags$td(
              if (is.na(row$verdict_label)) {
                episodic_tr("misc.dash", lang = lang)
              } else {
                row$verdict_label
              }
            ),
            shiny::tags$td(
              if (is.na(row$closed_at)) {
                episodic_tr("misc.unknown", lang = lang)
              } else {
                episodic_ui_format_datetime(row$closed_at, fmt = "%d-%m-%Y")
              }
            )
          )
        })
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_denominator_panel <- function(
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (
    is.null(obj$denominator) ||
      is.null(obj$denominator$series) ||
      nrow(obj$denominator$series) < 2
  ) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.denominator.title", lang = lang),
      episodic_tr("panel.denominator.unavailable", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.denominator.title", lang = lang),
    aside = episodic_tr("panel.denominator.aside", lang = lang),
    note = episodic_tr("panel.denominator.note", lang = lang),
    shiny::renderPlot(
      episodic_ui_denominator_chart(obj$denominator$series, lang = lang),
      height = 200
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_demography_panel <- function(
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (is.null(obj$demography) || is.null(obj$demography$bands)) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.demography.title", lang = lang),
      episodic_tr("misc.none", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.demography.title", lang = lang),
    note = episodic_tr("panel.demography.note", lang = lang),
    episodic_ui_pyramid(obj$demography$bands, lang = lang)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_geo_panel <- function(obj, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.null(obj$concentration)) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.geo.title", lang = lang),
      episodic_tr("panel.geo.empty", lang = lang),
      aside = episodic_tr("panel.geo.aside", lang = lang)
    ))
  }
  map_chart <- episodic_ui_geo_map_chart(obj$concentration$rows)
  n_unknown <- obj$concentration$n_unknown_pc %||% 0
  note <- if (n_unknown > 0) {
    # Stated rather than silently dropped: the concentration share, and
    # with it the spatial term of the priority score, is computed over
    # the cases with a known PC only.
    episodic_tr(
      "panel.geo.unknown_pc",
      n = n_unknown,
      total = obj$n_cases,
      lang = lang
    )
  }
  episodic_ui_panel(
    episodic_tr("panel.geo.title", lang = lang),
    aside = episodic_tr("panel.geo.aside", lang = lang),
    note = note,
    if (is.null(map_chart)) {
      episodic_ui_bars(
        utils::head(obj$concentration$rows, 8),
        unit = episodic_tr("panel.geo.unit", lang = lang)
      )
    } else {
      shiny::tagList(
        shiny::renderPlot(map_chart, height = 430),
        # The map is cropped to the cases, so the areas around the edge
        # are context, not absence of cases elsewhere - say so.
        shiny::tags$p(
          class = "episodic-panel-note",
          episodic_tr("panel.geo.map_note", lang = lang)
        ),
        episodic_ui_bars(utils::head(obj$concentration$rows, 8))
      )
    }
  )
}

#' @keywords internal
#' @noRd
episodic_ui_places_panel <- function(
  con,
  cluster_id,
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  cases <- episodic_db_cluster_cases(con, cluster_id)
  is_hospital <- obj$level == "pathogen_ward" ||
    (nrow(cases) > 0 && !all(is.na(cases$ward)))
  title <- episodic_tr(
    if (is_hospital) {
      "panel.places.title_ward"
    } else {
      "panel.places.title_institution"
    },
    lang = lang
  )

  group_col <- if (is_hospital) "ward" else "specialism"
  if (nrow(cases) == 0 || all(is.na(cases[[group_col]]))) {
    return(episodic_ui_panel_empty(
      title,
      episodic_tr("misc.none", lang = lang)
    ))
  }
  tab <- sort(table(cases[[group_col]]), decreasing = TRUE)
  rows <- data.frame(
    label = names(tab),
    n = as.integer(tab),
    stringsAsFactors = FALSE
  )
  episodic_ui_panel(title, episodic_ui_bars(rows))
}

#' What this cluster suppressed: the same outbreak, seen wider or narrower
#'
#' The lattice watches these cases at several levels at once, and only one
#' of those levels becomes a dossier. This panel is where the others go,
#' so that "one cluster" never has to mean "we threw the other views
#' away" - an assessor can see that the ward outbreak in front of them is
#' also what the hospital-level stream was flagging.
#' @keywords internal
#' @noRd
episodic_ui_suppressed_panel <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  title <- episodic_tr("panel.suppressed.title", lang = lang)
  suppressed <- episodic_db_clusters_suppressed_by(con, cluster_id)
  if (nrow(suppressed) == 0) {
    return(NULL)
  }

  rows <- lapply(seq_len(nrow(suppressed)), function(i) {
    row <- suppressed[i, ]
    shiny::tags$tr(
      shiny::tags$td(episodic_tr(
        paste0("level.", row$level),
        lang = lang
      )),
      shiny::tags$td(episodic_count_phrase(
        row$n_cases,
        episodic_tr("unit.case", lang = lang),
        episodic_tr("unit.cases", lang = lang)
      )),
      shiny::tags$td(episodic_format_date_range(
        row$first_day,
        row$last_day,
        lang = lang
      ))
    )
  })

  episodic_ui_panel(
    title,
    shiny::tagList(
      shiny::tags$p(
        class = "episodic-panel-note",
        episodic_tr("panel.suppressed.note", lang = lang)
      ),
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(
            episodic_tr("panel.suppressed.col.level", lang = lang)
          ),
          shiny::tags$th(
            episodic_tr("panel.suppressed.col.size", lang = lang)
          ),
          shiny::tags$th(
            episodic_tr("panel.suppressed.col.period", lang = lang)
          )
        )),
        shiny::tags$tbody(rows)
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_resistance_panel <- function(
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  # Susceptibility data is not part of the case data contract, so this
  # panel is always a placeholder.
  episodic_ui_panel_empty(
    episodic_tr("panel.resistance.title", lang = lang),
    episodic_tr("panel.resistance.unavailable", lang = lang)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_linelist_panel <- function(
  con,
  cluster_id,
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  ll <- episodic_app_linelist(con, cluster_id)
  cols <- c(
    "source_key",
    "sample_date",
    "sex",
    "age",
    "pc",
    "ward",
    "specialism"
  )
  labels <- vapply(
    cols,
    function(c) {
      episodic_tr(
        paste0(
          "panel.linelist.col.",
          switch(
            c,
            source_key = "case",
            sample_date = "date",
            sex = "sex",
            age = "age",
            pc = "pc",
            ward = "ward",
            specialism = "specialism"
          )
        ),
        lang = lang
      )
    },
    character(1)
  )

  episodic_ui_panel(
    episodic_tr("panel.linelist.title", lang = lang),
    aside = episodic_tr(
      "panel.linelist.aside",
      shown = nrow(ll),
      total = obj$n_cases,
      lang = lang
    ),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$thead(shiny::tags$tr(lapply(labels, shiny::tags$th))),
      shiny::tags$tbody(
        lapply(seq_len(nrow(ll)), function(i) {
          row <- ll[i, ]
          shiny::tags$tr(lapply(cols, function(c) {
            shiny::tags$td(as.character(
              row[[c]] %||% episodic_tr("misc.dash", lang = lang)
            ))
          }))
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
episodic_ui_report_panel <- function(
  con,
  cluster_id,
  current_user,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  reports <- episodic_db_reports_for_cluster(con, cluster_id)
  episodic_ui_panel(
    episodic_tr("panel.report.title", lang = lang),
    if (nrow(reports) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("panel.report.empty", lang = lang)
      )
    } else {
      shiny::tags$ul(
        style = "font-size:12.5px;padding-left:18px;",
        lapply(rev(seq_len(nrow(reports))), function(i) {
          row <- reports[i, ]
          shiny::tags$li(episodic_tr(
            "panel.report.version_line",
            version = row$version_no,
            when = episodic_ui_format_datetime(
              row$rendered_at,
              fmt = "%d-%m-%Y %H:%M"
            ),
            lang = lang
          ))
        })
      )
    },
    if (!is.null(current_user)) {
      shiny::tagList(
        shiny::uiOutput("report_render_error"),
        shiny::tags$button(
          id = "report-render-button",
          class = "episodic-btn",
          # Disabling the button and showing the "generating" text happens
          # here, client-side, at click time - the render itself is a
          # long, synchronous server call that blocks the whole session,
          # so nothing pushed from a Shiny observer could appear on
          # screen before it finishes. episodic_app_server_report() clears
          # both again once it is done (a fresh dossier pane on success,
          # a small reset script alongside the error message on failure).
          onclick = sprintf(
            "this.disabled=true; document.getElementById('report-render-pending').style.display='block'; Shiny.setInputValue('report_render_submit', %d, {priority: 'event'})",
            cluster_id
          ),
          episodic_tr("panel.report.render_button", lang = lang)
        ),
        shiny::tags$p(
          id = "report-render-pending",
          style = "display:none;font-size:12.5px;color:var(--episodic-muted);margin-top:6px;",
          episodic_tr("panel.report.pending", lang = lang)
        )
      )
    }
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_panel <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  settings <- episodic_app_detection_settings(con, cluster_id)
  # list(), not c(): a shiny::HTML() value (the detectors row) loses its
  # "html" class and gets escaped as literal text if combined with a
  # plain string via c() - list() keeps each element intact.
  rows <- list(
    list(
      episodic_tr("panel.settings.detectors", lang = lang),
      shiny::HTML(episodic_ui_code_join(settings$detectors))
    ),
    list(
      episodic_tr("panel.settings.aggregation", lang = lang),
      episodic_tr("panel.settings.aggregation_value", lang = lang)
    ),
    list(
      episodic_tr("panel.settings.population_offset", lang = lang),
      episodic_tr(
        if (!is.null(settings$population_offset)) {
          "panel.settings.population_offset_patient_days"
        } else {
          "panel.settings.population_offset_none"
        },
        lang = lang
      )
    ),
    list(
      episodic_tr("panel.settings.case_free_days", lang = lang),
      as.character(settings$case_free_days)
    ),
    list(
      episodic_tr("panel.settings.last_run", lang = lang),
      episodic_tr(
        "panel.settings.last_run_value",
        when = if (
          is.null(settings$last_run_when) || is.na(settings$last_run_when)
        ) {
          episodic_tr("misc.unknown", lang = lang)
        } else {
          episodic_ui_format_datetime(
            settings$last_run_when,
            fmt = "%d-%m-%Y %H:%M"
          )
        },
        host = settings$last_run_host %||%
          episodic_tr("misc.unknown", lang = lang),
        lang = lang
      )
    ),
    list(
      episodic_tr("panel.settings.pkg_versions", lang = lang),
      settings$pkg_versions %||% episodic_tr("misc.unknown", lang = lang)
    )
  )
  episodic_ui_panel(
    episodic_tr("panel.settings.title", lang = lang),
    shiny::tags$dl(
      class = "episodic-settings",
      lapply(rows, function(r) {
        shiny::tags$div(shiny::tags$dt(r[[1]]), shiny::tags$dd(r[[2]]))
      })
    )
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
episodic_ui_assessment_rail <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  current_user = NULL
) {
  obj <- episodic_cluster_object(con, cluster_id, lang = lang)
  timeline <- episodic_app_assessment_timeline(con, cluster_id, lang = lang)

  shiny::tags$div(
    class = "episodic-assessment-rail",
    shiny::tags$div(
      class = "episodic-verloop",
      shiny::tags$div(
        class = "episodic-verloop-title",
        episodic_tr("verloop.title", lang = lang)
      ),
      if (nrow(timeline) == 0) {
        shiny::tags$p(
          class = "episodic-verloop-empty",
          shiny::HTML(episodic_tr(
            "verloop.not_assessed",
            first = obj$first_day,
            detectors = episodic_ui_code_join(obj$detectors, sep = " en "),
            lang = lang
          ))
        )
      } else {
        lapply(rev(seq_len(nrow(timeline))), function(i) {
          episodic_ui_timeline_entry(timeline[i, ], lang = lang)
        })
      }
    ),
    if (!is.null(current_user)) {
      episodic_ui_assessment_form(cluster_id, obj, lang = lang)
    }
  )
}

#' One row of the assessment timeline
#' @keywords internal
#' @noRd
episodic_ui_timeline_entry <- function(
  row,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  shiny::tags$div(
    class = "episodic-timeline-entry",
    shiny::tags$div(
      class = "episodic-timeline-meta",
      sprintf("%s \u00b7 %s", episodic_ui_format_datetime(row$at), row$actor)
    ),
    if (row$kind == "closure") {
      shiny::tags$div(episodic_tr("activity.action_closed", lang = lang))
    } else {
      shiny::tagList(
        if (!is.na(row$verdict_label)) {
          shiny::tags$div(
            class = "episodic-timeline-verdict",
            row$verdict_label
          )
        },
        if (!is.na(row$rationale) && nzchar(row$rationale)) {
          shiny::tags$div(class = "episodic-timeline-rationale", row$rationale)
        }
      )
    }
  )
}

#' The classification form, closure and mute actions for a signed-in user
#' @keywords internal
#' @noRd
episodic_ui_assessment_form <- function(
  cluster_id,
  obj,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  # Ordered mild/terminal to severe - artefact and expected_variation
  # are both terminal (close immediately), the rest escalate.
  verdicts <- c(
    "artefact",
    "expected_variation",
    "cluster_not_yet",
    "possible_epidemic",
    "confirmed_epidemic"
  )
  mute_reasons <- c(
    "seasonal",
    "screening_campaign",
    "method_change",
    "known_source",
    "other"
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
        label = episodic_tr(paste0("verdict.", v), lang = lang),
        hint = episodic_tr(paste0("verdict.", v, ".hint"), lang = lang),
        colour = episodic_ui_verdict_colour(v)
      )
    })
  )
  mute_options <- lapply(mute_reasons, function(r) {
    list(
      value = r,
      label = episodic_tr(paste0("assessment.mute_reason.", r), lang = lang),
      colour = pal$secondary
    )
  })

  shiny::tags$div(
    class = "episodic-panel-body",
    style = "border-top:1px solid var(--episodic-rule);padding:16px;",
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("assessment.verdict_label", lang = lang)
      ),
      episodic_ui_picker("assess_verdict", verdict_options)
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("assessment.rationale_label", lang = lang)
      ),
      shiny::tags$textarea(
        id = "assess_rationale",
        rows = 3,
        placeholder = episodic_tr(
          "assessment.rationale_placeholder",
          lang = lang
        )
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("assessment.snooze_label", lang = lang)
      ),
      shiny::tags$input(type = "date", id = "assess_snooze")
    ),
    shiny::tags$div(id = "assess_error"),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        class = "episodic-btn episodic-btn-primary",
        onclick = sprintf(
          "Shiny.setInputValue('assess_submit', {cluster_id: %d, verdict: document.getElementById('assess_verdict').value, rationale: document.getElementById('assess_rationale').value, snooze: document.getElementById('assess_snooze').value}, {priority: 'event'})",
          cluster_id
        ),
        episodic_tr("assessment.submit", lang = lang)
      ),
      shiny::tags$button(
        class = "episodic-btn",
        `data-confirm` = episodic_tr("assessment.close_confirm", lang = lang),
        onclick = sprintf(
          "if(confirm(this.dataset.confirm)){ Shiny.setInputValue('assess_close', %d, {priority: 'event'}) }",
          cluster_id
        ),
        episodic_tr("assessment.close_button", lang = lang)
      )
    ),
    shiny::tags$hr(),
    shiny::tags$p(
      class = "episodic-form-hint",
      episodic_tr("assessment.mute_intro", lang = lang)
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("assessment.mute_title", lang = lang)
      ),
      episodic_ui_picker(
        "mute_reason",
        mute_options,
        selected = mute_reasons[1]
      )
    ),
    shiny::tags$div(
      style = "display:flex;gap:8px;",
      shiny::tags$div(
        class = "episodic-form-group",
        style = "flex:1;",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("assessment.mute_from_label", lang = lang)
        ),
        shiny::tags$input(
          type = "date",
          id = "mute_from",
          value = as.character(Sys.Date())
        )
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        style = "flex:1;",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("assessment.mute_until_label", lang = lang)
        ),
        shiny::tags$input(type = "date", id = "mute_until")
      )
    ),
    shiny::tags$button(
      class = "episodic-btn",
      onclick = sprintf(
        "Shiny.setInputValue('assess_mute_submit', {stream_id: %d, reason: document.getElementById('mute_reason').value, muted_from: document.getElementById('mute_from').value, muted_until: document.getElementById('mute_until').value}, {priority: 'event'})",
        obj$stream_id
      ),
      episodic_tr("assessment.mute_submit", lang = lang)
    )
  )
}

#' The read-only Streams screen
#' @keywords internal
#' @noRd
episodic_ui_streams_screen <- function(
  screen,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  streams <- screen$streams
  pager <- if (!is.null(screen$n_pages) && screen$n_pages > 1) {
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;margin-bottom:12px;font-size:12.5px;",
      shiny::tags$button(
        class = "episodic-btn",
        disabled = if (screen$page <= 1) NA else NULL,
        onclick = sprintf(
          "Shiny.setInputValue('streams_page_select', %d, {priority: 'event'})",
          screen$page - 1L
        ),
        episodic_tr("streams.page_prev", lang = lang)
      ),
      shiny::tags$span(episodic_tr(
        "streams.page_of",
        page = screen$page,
        n_pages = screen$n_pages,
        lang = lang
      )),
      shiny::tags$button(
        class = "episodic-btn",
        disabled = if (screen$page >= screen$n_pages) NA else NULL,
        onclick = sprintf(
          "Shiny.setInputValue('streams_page_select', %d, {priority: 'event'})",
          screen$page + 1L
        ),
        episodic_tr("streams.page_next", lang = lang)
      )
    )
  }
  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(
      style = "font-size:22px;font-weight:600;margin-bottom:4px;",
      episodic_tr("streams.title", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:16px;",
      episodic_tr("streams.note", lang = lang)
    ),
    pager,
    if (nrow(streams) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("rail.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("streams.col.stream", lang = lang)),
          shiny::tags$th(episodic_tr("streams.col.level", lang = lang)),
          shiny::tags$th(episodic_tr("streams.col.denominator", lang = lang)),
          shiny::tags$th(episodic_tr("streams.col.first_seen", lang = lang)),
          shiny::tags$th(episodic_tr("streams.col.last_seen", lang = lang)),
          shiny::tags$th(episodic_tr(
            "streams.col.baseline_excluded",
            lang = lang
          ))
        )),
        shiny::tags$tbody(
          lapply(seq_len(nrow(streams)), function(i) {
            row <- streams[i, ]
            excluded <- if (!is.null(streams$baseline_excluded)) {
              streams$baseline_excluded[[i]]
            } else {
              NULL
            }
            excluded_text <- if (is.null(excluded) || nrow(excluded) == 0) {
              episodic_tr("misc.dash", lang = lang)
            } else {
              paste(
                sprintf("%s \u2013 %s", excluded$first_day, excluded$last_day),
                collapse = "; "
              )
            }
            shiny::tags$tr(
              shiny::tags$td(shiny::HTML(episodic_ui_italicise_taxon(
                row$pathogen
              ))),
              shiny::tags$td(episodic_tr(
                paste0("level.", row$level),
                lang = lang
              )),
              shiny::tags$td(row$denominator),
              shiny::tags$td(row$first_seen),
              shiny::tags$td(row$last_seen),
              shiny::tags$td(excluded_text)
            )
          })
        )
      )
    },
    if (!is.null(screen$config_snapshot)) {
      episodic_ui_panel(
        episodic_tr("streams.config.title", lang = lang),
        shiny::tags$pre(
          style = "font-size:11.5px;white-space:pre-wrap;",
          jsonlite::toJSON(
            screen$config_snapshot,
            auto_unbox = TRUE,
            pretty = TRUE
          )
        )
      )
    }
  )
}
