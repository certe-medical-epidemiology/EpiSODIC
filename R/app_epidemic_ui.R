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

# The Epidemics screen: a rail of open epidemics, a dossier with
# seasonal evidence, and a declaration form for seasonal verdicts.

#' The Epidemics screen
#'
#' Lists open epidemics with their pathogen, level and state. Follows
#' the same rail pattern the Clusters screen uses: the list is the
#' navigation, the dossier is the destination.
#'
#' @param epidemics A data frame from `episodic_app_open_epidemics()`.
#' @param selected_id The currently selected epidemic cluster id, or `NULL`.
#' @param lang Session language.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episodic_ui_epidemics_screen <- function(epidemics,
                                         selected_id = NULL,
                                         lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::tags$div(
    class = "episodic-body episodic-body-epidemics",
    shiny::tags$div(
      class = "episodic-pane episodic-pane-rail",
      episodic_ui_epidemic_rail(epidemics, selected_id, lang = lang)
    ),
    shiny::uiOutput(
      "epidemic_dossier_pane",
      container = shiny::tags$div,
      class = "episodic-pane episodic-pane-dossier"
    ),
    shiny::uiOutput(
      "epidemic_assessment_pane",
      container = shiny::tags$div,
      class = "episodic-pane episodic-pane-assessment"
    )
  )
}

#' The epidemic rail
#'
#' @param epidemics Data frame of open epidemics.
#' @param selected_id Currently selected epidemic id.
#' @param lang Session language.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_rail <- function(epidemics,
                                      selected_id = NULL,
                                      lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  shiny::tagList(
    shiny::tags$div(
      class = "episodic-rail-header",
      shiny::tags$div(
        class = "episodic-rail-title",
        episodic_tr("epidemics.rail_title", lang = lang)
      ),
      shiny::tags$div(
        class = "episodic-rail-count",
        episodic_count_phrase(
          nrow(epidemics),
          episodic_tr("unit.epidemic", lang = lang),
          episodic_tr("unit.epidemics", lang = lang),
          lang = lang
        ),
        " ",
        episodic_tr("rail.count_suffix", lang = lang)
      )
    ),
    if (nrow(epidemics) == 0) {
      shiny::tags$p(
        class = "episodic-rail-empty",
        episodic_tr("epidemics.rail_empty", lang = lang)
      )
    } else {
      shiny::tags$div(
        class = "episodic-rail-list",
        lapply(seq_len(nrow(epidemics)), function(i) {
          row <- epidemics[i, ]
          episodic_ui_epidemic_rail_row(
            row,
            selected = identical(as.integer(row$cluster_id), as.integer(selected_id)),
            lang = lang
          )
        })
      )
    }
  )
}

#' One row in the epidemic rail
#' @keywords internal
#' @noRd
episodic_ui_epidemic_rail_row <- function(row,
                                          selected = FALSE,
                                          lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  shiny::tags$div(
    class = paste0(
      "episodic-rail-row",
      if (selected) " episodic-rail-row-selected"
    ),
    `data-cluster-id` = row$cluster_id,
    onclick = sprintf(
      "Shiny.setInputValue('epidemic_select', %d, {priority: 'event'})",
      as.integer(row$cluster_id)
    ),
    shiny::tags$div(
      class = "episodic-rail-row-title",
      shiny::HTML(episodic_ui_italicise_taxon(row$pathogen)),
      shiny::tags$span(
        class = "episodic-dossier-id",
        episodic_tr("dossier.epidemic_ref", id = row$cluster_id, lang = lang)
      )
    ),
    shiny::tags$div(
      class = "episodic-rail-row-meta",
      style = "display:flex;gap:6px;flex-wrap:wrap;align-items:center;",
      episodic_ui_chip(
        row$level_label,
        pal$primary
      ),
      episodic_ui_chip(
        row$state_label,
        episodic_ui_state_colour(row$state),
        filled = TRUE
      )
    )
  )
}

#' The epidemic dossier
#'
#' Evidence-centric: seasonal curve with thresholds, tests and positivity,
#' contributing institutions, linked outbreaks. No patient-level line list
#' (at L5 that is the whole catchment).
#'
#' @param con A [DBI::DBIConnection-class].
#' @param obj The epidemic object from `episodic_epidemic_object()`.
#' @param lang Session language.
#' @param current_user The session's signed-in user row, or `NULL`.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_dossier <- function(con,
                                         obj,
                                         lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                         current_user = NULL) {
  state <- episodic_app_derive_state_for_cluster(con, obj$id)
  timeline <- episodic_app_assessment_timeline(
    con,
    obj$id,
    lang = lang,
    level = obj$level
  )
  pal <- episodic_palette()

  shiny::tagList(
    episodic_ui_epidemic_header(obj, state, lang = lang),
    episodic_ui_epidemic_stat_grid(obj, lang = lang),
    episodic_ui_epidemic_curve_panel(obj, lang = lang),
    episodic_ui_epidemic_denominator_panel(obj, lang = lang),
    episodic_ui_epidemic_institutions_panel(obj, lang = lang),
    episodic_ui_epidemic_during_panel(con, obj, lang = lang)
  )
}

#' The epidemic dossier header
#' @keywords internal
#' @noRd
episodic_ui_epidemic_header <- function(obj,
                                        state,
                                        lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  season_label <- if (!is.null(obj$season)) {
    obj$season$season_label
  } else {
    NULL
  }

  shiny::tagList(
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
      shiny::tags$h1(
        class = "episodic-dossier-title",
        shiny::HTML(episodic_ui_italicise_taxon(obj$pathogen)),
        shiny::tags$span(
          class = "episodic-dossier-id",
          episodic_tr("dossier.epidemic_ref", id = obj$id, lang = lang)
        )
      ),
      episodic_ui_chip(
        episodic_app_level_label(obj$level, obj$care_line, lang = lang),
        pal$primary
      ),
      episodic_ui_chip(
        episodic_tr(paste0("state.", state), lang = lang),
        episodic_ui_state_colour(state),
        filled = TRUE
      ),
      if (!is.null(season_label)) {
        episodic_ui_chip(
          episodic_tr(
            "epidemics.season_chip",
            season = season_label,
            lang = lang
          ),
          pal$secondary
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
        first = episodic_format_date(obj$first_day, lang = lang),
        last = episodic_format_date(obj$last_day, lang = lang),
        lang = lang
      )),
      if (length(obj$detectors) > 0) {
        shiny::tagList(
          shiny::tags$span(style = "color:var(--episodic-faint);", "\u00b7"),
          shiny::tags$span(shiny::HTML(episodic_tr(
            "dossier.meta.detected_by",
            detectors = episodic_ui_code_join(
              obj$detectors,
              sep = episodic_tr("misc.list_separator_and", lang = lang)
            ),
            lang = lang
          )))
        )
      }
    )
  )
}

#' The epidemic stat grid
#' @keywords internal
#' @noRd
episodic_ui_epidemic_stat_grid <- function(obj,
                                           lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  stats <- list(
    episodic_ui_stat(
      episodic_tr("column.cases", lang = lang),
      episodic_format_number(obj$n_cases, lang = lang)
    )
  )

  if (!is.null(obj$concentration)) {
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("epidemics.stat.institutions", lang = lang),
      episodic_format_number(obj$concentration$n_institutions, lang = lang),
      episodic_tr(
        "epidemics.stat.top_share",
        name = obj$concentration$top_institution,
        pct = episodic_format_number(
          obj$concentration$top_share * 100,
          digits = 0,
          lang = lang
        ),
        lang = lang
      )
    )))
  }

  if (!is.null(obj$season)) {
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("epidemics.stat.season", lang = lang),
      obj$season$season_label,
      episodic_tr(
        "epidemics.stat.anchor",
        week = episodic_format_number(
          as.integer(obj$season$anchor_week),
          lang = lang
        ),
        lang = lang
      )
    )))
  }

  n_during <- nrow(obj$during_outbreaks)
  stats <- c(stats, list(episodic_ui_stat(
    episodic_tr("epidemics.stat.outbreaks_during", lang = lang),
    if (n_during == 0) {
      episodic_tr("epidemics.none_detected", lang = lang)
    } else {
      episodic_format_number(n_during, lang = lang)
    }
  )))

  shiny::tags$div(class = "episodic-stat-grid", stats)
}

#' The seasonal curve panel with MEM thresholds
#' @keywords internal
#' @noRd
episodic_ui_epidemic_curve_panel <- function(obj,
                                             lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.null(obj$weekly) || nrow(obj$weekly) == 0 || sum(obj$weekly$n_cases) == 0) {
    return(episodic_ui_panel_empty(
      episodic_tr("epidemics.panel.curve.title", lang = lang),
      episodic_tr("epidemics.panel.curve.empty", lang = lang)
    ))
  }
  graphics_issue <- episodic_graphics_probe()
  if (!is.null(graphics_issue)) {
    return(episodic_ui_panel_empty(
      episodic_tr("epidemics.panel.curve.title", lang = lang),
      episodic_graphics_error_message(graphics_issue, lang = lang)
    ))
  }

  note <- if (!is.null(obj$thresholds)) {
    episodic_tr(
      "epidemics.panel.curve.threshold_note",
      lang = lang,
      seasons = length(obj$thresholds$seasons_used),
      pre = episodic_format_number(
        obj$thresholds$pre_epidemic,
        digits = 1,
        lang = lang
      ),
      post = episodic_format_number(
        obj$thresholds$post_epidemic,
        digits = 1,
        lang = lang
      )
    )
  } else if (!is.null(obj$season)) {
    episodic_tr("epidemics.panel.curve.no_thresholds", lang = lang)
  } else {
    episodic_tr("epidemics.panel.curve.not_seasonal", lang = lang)
  }

  episodic_ui_panel(
    episodic_tr("epidemics.panel.curve.title", lang = lang),
    note = shiny::HTML(note),
    shiny::renderPlot(
      episodic_ui_pathogen_curve_chart(obj$weekly, obj$thresholds, lang = lang),
      height = 300
    )
  )
}

#' Tests performed and positivity
#' @keywords internal
#' @noRd
episodic_ui_epidemic_denominator_panel <- function(obj,
                                                   lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.null(obj$denominator)) {
    return(episodic_ui_panel_empty(
      episodic_tr("epidemics.panel.denominator.title", lang = lang),
      episodic_tr("epidemics.panel.denominator.empty", lang = lang)
    ))
  }
  graphics_issue <- episodic_graphics_probe()
  if (!is.null(graphics_issue)) {
    return(episodic_ui_panel_empty(
      episodic_tr("epidemics.panel.denominator.title", lang = lang),
      episodic_graphics_error_message(graphics_issue, lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("epidemics.panel.denominator.title", lang = lang),
    note = shiny::HTML(episodic_tr(
      "epidemics.panel.denominator.note",
      lang = lang
    )),
    shiny::renderPlot(
      episodic_ui_denominator_chart(obj$denominator, lang = lang),
      height = 260
    )
  )
}

#' Contributing institutions
#' @keywords internal
#' @noRd
episodic_ui_epidemic_institutions_panel <- function(obj,
                                                    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  inst <- obj$institutions
  if (is.null(inst) || nrow(inst) == 0) {
    return(episodic_ui_panel_empty(
      episodic_tr("epidemics.panel.institutions.title", lang = lang),
      episodic_tr("epidemics.panel.institutions.empty", lang = lang)
    ))
  }

  total <- sum(inst$n_cases)
  rows <- lapply(seq_len(nrow(inst)), function(i) {
    share <- if (total > 0) inst$n_cases[i] / total else 0
    shiny::tags$tr(
      shiny::tags$td(inst$display_name[i]),
      shiny::tags$td(episodic_format_number(inst$n_cases[i], lang = lang)),
      shiny::tags$td(episodic_format_number(share * 100, digits = 1, lang = lang))
    )
  })

  episodic_ui_panel(
    episodic_tr("epidemics.panel.institutions.title", lang = lang),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(episodic_tr("epidemics.panel.institutions.col_name", lang = lang)),
        shiny::tags$th(episodic_tr("column.cases", lang = lang)),
        shiny::tags$th(episodic_tr("epidemics.panel.institutions.col_share", lang = lang))
      )),
      shiny::tags$tbody(rows)
    )
  )
}

#' Outbreaks occurring during this epidemic
#' @keywords internal
#' @noRd
episodic_ui_epidemic_during_panel <- function(con,
                                              obj,
                                              lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  during <- obj$during_outbreaks
  if (is.null(during) || nrow(during) == 0) {
    return(episodic_ui_panel(
      episodic_tr("epidemics.panel.during.title", lang = lang),
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("epidemics.none_detected", lang = lang)
      )
    ))
  }

  rows <- lapply(seq_len(nrow(during)), function(i) {
    row <- during[i, ]
    level_label <- episodic_tr(paste0("level.", row$level), lang = lang)
    shiny::tags$tr(
      shiny::tags$td(
        shiny::tags$a(
          href = "#",
          class = "episodic-cluster-link",
          onclick = sprintf(
            "Shiny.setInputValue('open_cluster', %d, {priority: 'event'}); return false;",
            as.integer(row$cluster_id)
          ),
          episodic_tr("dossier.outbreak_ref", id = row$cluster_id, lang = lang)
        )
      ),
      shiny::tags$td(shiny::HTML(episodic_ui_italicise_taxon(row$pathogen))),
      shiny::tags$td(level_label),
      shiny::tags$td(
        if (!is.null(row$place)) row$place else episodic_tr("misc.dash", lang = lang)
      ),
      shiny::tags$td(episodic_format_number(row$n_cases, lang = lang)),
      shiny::tags$td(episodic_format_date_range(
        row$first_day,
        row$last_day,
        lang = lang
      ))
    )
  })

  episodic_ui_panel(
    episodic_tr("epidemics.panel.during.title", lang = lang),
    shiny::tags$div(
      style = "overflow-x:auto;",
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("column.id", lang = lang)),
          shiny::tags$th(episodic_tr("column.pathogen", lang = lang)),
          shiny::tags$th(episodic_tr("column.level", lang = lang)),
          shiny::tags$th(episodic_tr("column.place", lang = lang)),
          shiny::tags$th(episodic_tr("column.cases", lang = lang)),
          shiny::tags$th(episodic_tr("column.period", lang = lang))
        )),
        shiny::tags$tbody(rows)
      )
    )
  )
}

#' The declaration form for seasonal verdicts
#'
#' An epidemiologist records whether the season has started, has not started
#' yet, or has ended. Reuses the `episodic_assessment_event` table with the
#' declaration verdicts (`season_started`, `season_not_yet`, `season_ended`).
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The epidemic cluster's id.
#' @param obj The epidemic object.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_declaration_form <- function(con,
                                                  cluster_id,
                                                  obj,
                                                  lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  verdicts <- c("season_started", "season_not_yet", "season_ended")
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

  shiny::tags$div(
    class = "episodic-panel-body",
    style = "border-top:1px solid var(--episodic-rule);padding:16px;",
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("epidemics.declaration_label", lang = lang)
      ),
      shiny::tags$div(
        onclick = "episodicDeclarationChanged()",
        episodic_ui_picker("epidemic_declare_verdict", verdict_options)
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("assessment.rationale_label", lang = lang)
      ),
      shiny::tags$textarea(
        id = "epidemic_declare_rationale",
        rows = 3,
        disabled = "disabled",
        placeholder = episodic_tr(
          "epidemics.declaration_rationale_placeholder",
          lang = lang
        )
      )
    ),
    shiny::tags$div(id = "epidemic_declare_error"),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        id = "epidemic_declare_btn",
        class = "episodic-btn episodic-btn-primary",
        disabled = "disabled",
        onclick = sprintf("episodicSubmitDeclaration(%d)", as.integer(cluster_id)),
        episodic_tr("epidemics.declaration_submit", lang = lang)
      )
    ),
    shiny::tags$script(shiny::HTML(sprintf(
      "function episodicDeclarationChanged() {
  var v = document.getElementById('epidemic_declare_verdict').value;
  var rationale = document.getElementById('epidemic_declare_rationale');
  var btn = document.getElementById('epidemic_declare_btn');
  var has = !!v;
  rationale.disabled = !has;
  btn.disabled = !has;
}
function episodicSubmitDeclaration(clusterId) {
  var v = document.getElementById('epidemic_declare_verdict').value;
  var r = document.getElementById('epidemic_declare_rationale').value;
  if (!v) return;
  var label = %s;
  if (!confirm(label[v] + '\\n\\n' + (r || '(%s)'))) return;
  Shiny.setInputValue('epidemic_declare_submit', {
    cluster_id: clusterId, verdict: v, rationale: r
  }, {priority: 'event'});
}",
      jsonlite::toJSON(stats::setNames(
        vapply(
          verdicts,
          function(v) episodic_tr(paste0("verdict.", v), lang = lang),
          character(1)
        ),
        verdicts
      ), auto_unbox = TRUE),
      episodic_tr("epidemics.declaration_no_rationale", lang = lang)
    )))
  )
}

#' The epidemic assessment rail
#'
#' Timeline of assessment events plus the declaration form for
#' epidemiologists. Viewers see the timeline only.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The epidemic cluster's id.
#' @param obj The epidemic object.
#' @param lang Session language.
#' @param current_user The session's signed-in user, or `NULL`.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_assessment_rail <- function(con,
                                                 cluster_id,
                                                 lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                                 current_user = NULL,
                                                 obj = NULL) {
  if (is.null(obj)) {
    obj <- episodic_epidemic_object(con, cluster_id, lang = lang)
  }
  timeline <- episodic_app_assessment_timeline(
    con,
    cluster_id,
    lang = lang,
    level = obj$level
  )

  shiny::tagList(
    shiny::tags$div(
      class = "episodic-timeline",
      shiny::tags$div(
        class = "episodic-timeline-title",
        episodic_tr("timeline.title", lang = lang)
      ),
      if (nrow(timeline) == 0) {
        shiny::tags$p(
          class = "episodic-timeline-empty",
          shiny::HTML(episodic_tr(
            "timeline.not_assessed",
            first = episodic_format_date(obj$first_day, lang = lang),
            detectors = episodic_ui_code_join(
              obj$detectors,
              sep = episodic_tr("misc.list_separator_and", lang = lang)
            ),
            lang = lang
          ))
        )
      } else {
        lapply(rev(seq_len(nrow(timeline))), function(i) {
          episodic_ui_timeline_entry(timeline[i, ], lang = lang)
        })
      }
    ),
    if (episodic_user_is_epidemiologist(current_user) && !is.null(obj$season)) {
      episodic_ui_epidemic_declaration_form(con, cluster_id, obj, lang = lang)
    }
  )
}
