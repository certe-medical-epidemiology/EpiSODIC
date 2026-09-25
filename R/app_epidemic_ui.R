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
# seasonal evidence, and the assessment pane an epidemiologist declares
# the season in.
#
# The screen's own three-pane body is in `episodic_app_ui()`, beside the
# Outbreaks screen's, rather than behind an output that renders all of
# it: a rail whose DOM is replaced whenever the list of open epidemics
# changes loses its scroll position and the row `episodic-nav.js`
# marked, on every run and every write.

#' The epidemic rail
#'
#' The Outbreaks rail's own markup and classes, with two differences
#' that follow from what an epidemic is. There is no bulk-assessment
#' bar: a declaration is a decision about one season of one pathogen,
#' with policy consequences, and applying one to several at once is not
#' an act this screen should make easy. And there is no open-by-number
#' box, because every epidemic the instance holds open is in this list -
#' at region and province level there are a handful, not a queue.
#'
#' @param epidemics A data frame from `episodic_app_open_epidemics()`.
#' @param selected_id The currently selected epidemic cluster id, or
#'   `NULL`.
#' @param lang Session language.
#' @return A `shiny::tagList` of the rail's contents. Its own box is
#'   `output$epidemic_rail_pane`'s container - see `episodic_app_ui()`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_rail <- function(epidemics,
                                      selected_id = NULL,
                                      lang = Sys.getenv("EPISODIC_LANGUAGE")) {
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
      shiny::tags$div(
        style = "padding:14px;font-size:12.5px;color:var(--episodic-muted);",
        episodic_tr("epidemics.rail_empty", lang = lang)
      )
    } else {
      lapply(seq_len(nrow(epidemics)), function(i) {
        row <- epidemics[i, ]
        episodic_ui_epidemic_rail_row(
          row,
          selected = identical(
            as.integer(row$cluster_id),
            as.integer(selected_id)
          ),
          lang = lang
        )
      })
    }
  )
}

#' One row in the epidemic rail
#'
#' `data-episodic-epidemic` rather than `data-episodic-outbreak`: both
#' carry a cluster id, but one selects within this screen and the other
#' opens the Outbreaks screen, and `episodic-nav.js` has to tell them
#' apart. `aria-current` is written server-side here for the first
#' render and by `episodic-nav.js` after that, exactly as the Outbreaks
#' rail does it.
#'
#' @param row One row of `episodic_app_open_epidemics()`.
#' @param selected Whether this row is the current selection.
#' @param lang Session language.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_rail_row <- function(row,
                                          selected = FALSE,
                                          lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  # Absent from fixtures that predate them; a bare `row$care_line` on
  # such a frame returns NULL rather than NA, and `is.na(NULL)` errors
  # rather than returning FALSE.
  row$care_line <- row$care_line %||% NA_character_
  row$priority_score <- row$priority_score %||% NA_real_

  shiny::tags$div(
    class = "episodic-rail-item",
    `aria-current` = if (selected) "true",
    `data-cluster-id` = row$cluster_id,
    shiny::tags$button(
      type = "button",
      class = "episodic-rail-item-open",
      `data-episodic-epidemic` = row$cluster_id,
      shiny::tags$div(
        class = "episodic-rail-pathogen",
        shiny::HTML(episodic_ui_italicise_taxon(row$pathogen)),
        shiny::tags$span(
          class = "episodic-rail-id",
          episodic_tr("dossier.epidemic_ref", id = row$cluster_id, lang = lang)
        ),
        if (!is.na(row$care_line)) {
          care_line_colour <- episodic_ui_care_line_colour(row$care_line)
          if (!is.null(care_line_colour)) {
            episodic_ui_chip(
              episodic_tr(paste0("careline.short.", row$care_line), lang = lang),
              care_line_colour,
              filled = TRUE
            )
          }
        }
      ),
      shiny::tags$div(class = "episodic-rail-meta", row$level_label),
      shiny::tags$div(
        class = "episodic-rail-meta",
        episodic_format_date_range(row$first_day, row$last_day, lang = lang)
      ),
      shiny::tags$div(
        class = "episodic-rail-meta",
        paste(
          c(
            episodic_count_phrase(
              row$n_cases,
              episodic_tr("unit.case", lang = lang),
              episodic_tr("unit.cases", lang = lang),
              lang = lang
            ),
            if (!is.na(row$priority_score)) {
              episodic_tr(
                "rail.priority",
                score = episodic_format_number(
                  row$priority_score,
                  digits = 0,
                  lang = lang
                ),
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
  )
}

#' The epidemic dossier
#'
#' Evidence-centric: seasonal curve with thresholds, tests and
#' positivity, contributing institutions, and the outbreaks that ran
#' during it. No patient-level line list: at region level that is the
#' whole catchment, which is a case register rather than a dossier.
#'
#' The notes panel is behind its own `uiOutput()`, for the reason the
#' Outbreaks dossier's is - saving a note re-renders that one panel and
#' leaves the plots beside it alone.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param obj The epidemic object from `episodic_epidemic_object()`.
#' @param lang Session language.
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_dossier <- function(con,
                                         obj,
                                         lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  # Resolved up front, as every other function taking `obj` does: the
  # server passes `epidemic_object()` unevaluated, and building it here,
  # before the dossier's own reads, keeps the two sets of queries apart
  # (`R/db_query.R` says what interleaving them costs on MariaDB).
  force(obj)
  state <- episodic_app_derive_state_for_cluster(con, obj$id)
  shiny::tagList(
    episodic_ui_epidemic_header(obj, state, lang = lang),
    episodic_ui_epidemic_stat_grid(obj, lang = lang),
    shiny::uiOutput("epidemic_notes_pane"),
    episodic_ui_epidemic_curve_panel(obj, lang = lang),
    episodic_ui_epidemic_denominator_panel(obj, lang = lang),
    episodic_ui_epidemic_institutions_panel(obj, lang = lang),
    episodic_ui_epidemic_during_panel(obj, lang = lang)
  )
}

#' The epidemic dossier header
#'
#' The Outbreaks dossier's header, with the season in place of the
#' origin and linked-cluster badges: at this scale the case-sharing
#' relation those badges carry is vacuous, since every case in the
#' catchment is in the regional cluster by construction.
#'
#' @param obj The epidemic object.
#' @param state The derived state, from
#'   `episodic_app_derive_state_for_cluster()`.
#' @param lang Session language.
#' @return A `shiny::tagList`.
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

  shiny::tags$div(class = "episodic-statgrid", stats)
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
      episodic_ui_pathogen_curve_chart(
        obj$weekly,
        obj$thresholds,
        lang = lang,
        accent = episodic_nav_accent("epidemics")
      ),
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
#'
#' @param obj The epidemic object.
#' @param lang Session language.
#' @return A `shiny::tags$section`.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_during_panel <- function(obj,
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
      # The app's one way of linking to an outbreak, rather than an
      # `onclick` of this panel's own: `episodic_ui_cluster_link()`
      # carries the keyboard contract with it, and going through
      # `episodic-nav.js` is what moves the screen as well as the
      # selection.
      shiny::tags$td(
        episodic_ui_cluster_link(
          episodic_object_ref(row$cluster_id, row$level, lang = lang),
          cluster_id = row$cluster_id,
          lang = lang
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

#' The epidemic assessment rail
#'
#' The Outbreaks screen's assessment rail, for the coarse scale: the
#' timeline of what has been recorded, and for a signed-in epidemiologist
#' the same form, carrying its own element ids.
#'
#' The form's verdict list is what differs. An epidemic is assessed like
#' any other signal - it can be an artefact, expected variation, or a
#' confirmed epidemic - and a *seasonal* epidemic can additionally be
#' declared started, not yet started, or ended. Those three are the act
#' the Moving Epidemic Method exists to inform and does not itself
#' perform: the crossing is a measurement, the declaration is a decision
#' with an author, a timestamp and a consequence for screening policy.
#' They are offered only where there is a season to declare, which a
#' `Legionella` epidemic has none of.
#'
#' Muting is offered here for the same reason it is on an outbreak: a
#' regional stream in a month everyone already knows about is one an
#' epidemiologist may legitimately silence.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The epidemic cluster's id.
#' @param obj The epidemic object from `episodic_epidemic_object()`, or
#'   `NULL` to build one.
#' @param lang Session language.
#' @param current_user The session's signed-in user, or `NULL`.
#' @return A `shiny::tagList` of the assessment rail's contents. Its own
#'   box is `output$epidemic_assessment_pane`'s container - see
#'   `episodic_app_ui()`.
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
  declarations <- if (is.null(obj$season)) {
    character(0)
  } else {
    c("season_started", "season_not_yet", "season_ended")
  }

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
    if (episodic_user_is_epidemiologist(current_user)) {
      episodic_ui_assessment_form(
        con,
        cluster_id,
        obj,
        lang = lang,
        prefix = "epidemic_assess",
        extra_verdicts = declarations,
        # Declaring a season over is terminal for this epidemic in the
        # way an artefact is for an outbreak, so it pre-ticks the
        # closure box - a suggestion, not a closure. The cron closes a
        # seasonal epidemic from the post-epidemic threshold on its own;
        # this is the epidemiologist saying so first.
        close_suggested = c("artefact", "expected_variation", "season_ended")
      )
    }
  )
}
