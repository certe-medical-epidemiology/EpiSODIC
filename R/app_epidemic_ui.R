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
          episodic_object_ref(row$cluster_id, "epidemic", lang = lang)
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
#' Ordered by the questions an epidemiologist brings to an epidemic, in
#' the order they are asked:
#'
#' 1. Where is it in its course? The stat grid: the latest complete week
#'    against the one before, the peak so far, the MEM intensity now and
#'    at the peak, the latest Rt, the season.
#' 2. What does its curve look like? Weekly cases with the MEM thresholds
#'    drawn on, the weeks before it began included so the rise is on the
#'    chart.
#' 3. Is this season unusual? Every earlier season on one axis.
#' 4. Is it still growing? Rt, on the area's whole incidence.
#' 5. Is the rise real? Tests and positivity.
#' 6. Where is it? The choropleth and the postcode bars.
#' 7. Who does it affect, and where are they found? Age and sex against
#'    the area's own baseline, beside the care lines.
#' 8. Which institutions carry it, and which outbreaks ran during it?
#'
#' No patient-level line list: at region level that is the whole
#' catchment, which is a case register rather than a dossier.
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
    episodic_ui_epidemic_history_problem(obj, lang = lang),
    episodic_ui_epidemic_stat_grid(obj, lang = lang),
    shiny::uiOutput("epidemic_notes_pane"),
    episodic_ui_epidemic_curve_panel(obj, lang = lang),
    episodic_ui_epidemic_overlay_panel(obj, lang = lang),
    # Both full width: each is a weekly series over months, and at half
    # width its week axis has no room for the labels it needs.
    episodic_ui_epidemic_rt_panel(obj, lang = lang),
    episodic_ui_epidemic_denominator_panel(obj, lang = lang),
    episodic_ui_epidemic_geo_panel(obj, lang = lang),
    shiny::tags$div(
      class = "episodic-split-row",
      shiny::tags$div(
        class = "episodic-split-col",
        episodic_ui_epidemic_demography_panel(obj, lang = lang)
      ),
      shiny::tags$div(
        class = "episodic-split-col",
        episodic_ui_epidemic_care_line_panel(obj, lang = lang)
      )
    ),
    episodic_ui_epidemic_institutions_panel(obj, lang = lang),
    episodic_ui_epidemic_during_panel(obj, lang = lang),
    # The epidemic object carries every field the panel reads (the
    # detectors, whether Rt applies, the case-free requirement, and no
    # patient-day density), so it is handed over rather than an outbreak
    # object being built for an epidemic.
    episodic_ui_settings_panel(
      con,
      obj$id,
      lang = lang,
      obj = obj,
      scale = "epidemic"
    )
  )
}

#' Say so when the stream's history is missing cases the epidemic holds
#'
#' See `episodic_epidemic_object()`: the curve then falls back to the
#' epidemic's own cases, and the panels read against the history - the
#' comparison with earlier seasons, Rt, the age baseline - are drawn
#' from part of the population, or not at all. Where the cause is
#' visible (an L5 stream's code that is not the dashboard's), both codes
#' are named, since that is what the operator has to reconcile.
#'
#' @param obj The epidemic object.
#' @param lang Session language.
#' @return A `shiny::tags$div`, or `NULL` when the history is whole.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_history_problem <- function(obj,
                                                 lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  problem <- obj$history_problem
  if (is.null(problem)) {
    return(NULL)
  }
  shiny::tags$div(
    class = "episodic-dossier-problem",
    role = "alert",
    shiny::tags$p(episodic_tr(
      "epidemics.history_problem",
      read = episodic_format_number(problem$n_read, lang = lang),
      linked = episodic_format_number(problem$n_linked, lang = lang),
      lang = lang
    )),
    if (!is.na(problem$dashboard_code)) {
      shiny::tags$p(episodic_tr(
        "epidemics.history_problem_region",
        stream_code = problem$stream_code,
        dashboard_code = problem$dashboard_code,
        lang = lang
      ))
    }
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
  level_now <- if (isTRUE(obj$closed)) {
    NA_character_
  } else {
    obj$course$latest_level %||% NA_character_
  }

  shiny::tagList(
    shiny::tags$div(
      style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
      shiny::tags$h1(
        class = "episodic-dossier-title",
        shiny::HTML(episodic_ui_italicise_taxon(obj$pathogen)),
        shiny::tags$span(
          class = "episodic-dossier-id",
          episodic_object_ref(obj$id, "epidemic", lang = lang)
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
      },
      # The intensity band this week sits in, where MEM could fit one:
      # the single word an epidemiologist is most often asked for about
      # a seasonal epidemic, and the one the header can carry.
      if (!is.na(level_now)) {
        episodic_ui_chip(
          episodic_tr(paste0("pathogen.intensity.", level_now), lang = lang),
          episodic_ui_intensity_colour(level_now),
          filled = TRUE
        )
      }
    ),
    shiny::tags$div(
      class = "episodic-dossier-meta",
      style = "display:flex;gap:8px;flex-wrap:wrap;",
      shiny::tags$span(obj$place),
      shiny::tags$span(style = "color:var(--episodic-faint);", "·"),
      shiny::tags$span(episodic_tr(
        "dossier.meta.first_last",
        first = episodic_format_date(obj$first_day, lang = lang),
        last = episodic_format_date(obj$last_day, lang = lang),
        lang = lang
      )),
      if (length(obj$detectors) > 0) {
        shiny::tagList(
          shiny::tags$span(style = "color:var(--episodic-faint);", "·"),
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

#' The epidemic stat grid: where the epidemic stands
#'
#' A tile is shown only for a figure that could be computed. There is no
#' "latest complete week" while every week is still filling, no change
#' on the week before when that week had no cases (the tile says so in
#' words instead of printing a percentage of nothing), no intensity
#' without fitted thresholds and no Rt where none could be estimated.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_stat_grid <- function(obj,
                                           lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  course <- obj$course
  week_date <- function(d) episodic_format_date(d, lang = lang)

  # The weeks the epidemic spans, first case to last: counted to today
  # instead, a season two years back would read as two years long.
  n_weeks <- if (
    !is.null(obj$first_day) && !is.na(obj$first_day) &&
      !is.null(obj$last_day) && !is.na(obj$last_day)
  ) {
    as.integer(
      (episodic_week_start(as.Date(obj$last_day)) -
        episodic_week_start(as.Date(obj$first_day))) / 7
    ) + 1L
  } else {
    NA_integer_
  }
  # "Now" has no meaning for an epidemic that is over: its latest week
  # and its current intensity are left out, and its intensity is read
  # at the peak instead.
  running <- !isTRUE(obj$closed)
  stats <- list(
    episodic_ui_stat(
      episodic_tr("column.cases", lang = lang),
      episodic_format_number(obj$n_cases, lang = lang),
      if (!is.na(n_weeks)) {
        episodic_tr(
          "epidemics.stat.cases_sub",
          date = week_date(obj$first_day),
          weeks = episodic_count_phrase(
            n_weeks,
            episodic_tr("unit.week", lang = lang),
            episodic_tr("unit.weeks", lang = lang),
            lang = lang
          ),
          lang = lang
        )
      }
    )
  )

  if (running && !is.null(course) && !is.na(course$latest_n)) {
    sub <- if (is.na(course$previous_n)) {
      episodic_tr(
        "epidemics.stat.peak_sub",
        week = week_date(course$latest_week),
        lang = lang
      )
    } else if (is.na(course$change_pct)) {
      episodic_tr(
        "epidemics.stat.latest_week_sub_no_base",
        week = week_date(course$latest_week),
        lang = lang
      )
    } else {
      episodic_tr(
        "epidemics.stat.latest_week_sub",
        week = week_date(course$latest_week),
        change = sprintf(
          "%s%s%%",
          if (course$change_pct > 0) "+" else "",
          episodic_format_number(course$change_pct, digits = 0, lang = lang)
        ),
        lang = lang
      )
    }
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("epidemics.stat.latest_week", lang = lang),
      episodic_format_number(course$latest_n, lang = lang),
      sub,
      colour = if (!is.na(course$change_pct) && course$change_pct > 0) {
        pal$danger
      } else if (!is.na(course$change_pct) && course$change_pct < 0) {
        pal$success
      } else {
        NULL
      }
    )))
  }

  if (!is.null(course) && !is.na(course$peak_n)) {
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("epidemics.stat.peak", lang = lang),
      episodic_format_number(course$peak_n, lang = lang),
      episodic_tr(
        "epidemics.stat.peak_sub",
        week = week_date(course$peak_week),
        lang = lang
      )
    )))
  }

  if (!running && !is.null(course) && !is.na(course$peak_level)) {
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("pathogen.stat.intensity", lang = lang),
      episodic_tr(
        paste0("pathogen.intensity.", course$peak_level),
        lang = lang
      ),
      episodic_tr("pathogen.stat.intensity_sub", lang = lang),
      colour = episodic_ui_intensity_colour(course$peak_level)
    )))
  }
  if (running && !is.null(course) && !is.na(course$latest_level)) {
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("epidemics.stat.intensity", lang = lang),
      episodic_tr(
        paste0("pathogen.intensity.", course$latest_level),
        lang = lang
      ),
      if (!is.na(course$peak_level)) {
        episodic_tr(
          "epidemics.stat.intensity_sub",
          level = episodic_tr(
            paste0("pathogen.intensity.", course$peak_level),
            lang = lang
          ),
          lang = lang
        )
      },
      colour = episodic_ui_intensity_colour(course$latest_level)
    )))
  }

  rt_latest <- if (!is.null(obj$rt) && nrow(obj$rt) > 0) {
    obj$rt[nrow(obj$rt), , drop = FALSE]
  }
  if (!is.null(rt_latest) && !is.na(rt_latest$mean)) {
    stats <- c(stats, list(episodic_ui_stat(
      episodic_tr("epidemics.stat.rt", lang = lang),
      episodic_format_number(rt_latest$mean, digits = 2, fixed = TRUE, lang = lang),
      if (!is.na(rt_latest$lower) && !is.na(rt_latest$upper)) {
        episodic_tr(
          "epidemics.stat.rt_sub",
          lower = episodic_format_number(
            rt_latest$lower,
            digits = 2,
            fixed = TRUE,
            lang = lang
          ),
          upper = episodic_format_number(
            rt_latest$upper,
            digits = 2,
            fixed = TRUE,
            lang = lang
          ),
          date = week_date(rt_latest$window_end),
          lang = lang
        )
      },
      # Coloured only when the interval is on one side of 1: an interval
      # straddling it has not said which way the epidemic is going.
      colour = if (!is.na(rt_latest$lower) && rt_latest$lower > 1) {
        pal$danger
      } else if (!is.na(rt_latest$upper) && rt_latest$upper < 1) {
        pal$success
      } else {
        NULL
      }
    )))
  }

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
    aside = episodic_tr(
      "pathogen.panel.curve.aside",
      weeks = episodic_format_number(nrow(obj$weekly), lang = lang),
      lang = lang
    ),
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

#' This epidemic's season against the earlier ones
#' @keywords internal
#' @noRd
episodic_ui_epidemic_overlay_panel <- function(obj,
                                               lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  overlay <- obj$overlay
  if (is.null(overlay)) {
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.overlay.title", lang = lang),
      episodic_tr("pathogen.panel.overlay.empty", lang = lang)
    ))
  }
  graphics_issue <- episodic_graphics_probe()
  if (!is.null(graphics_issue)) {
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.overlay.title", lang = lang),
      episodic_graphics_error_message(graphics_issue, lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("pathogen.panel.overlay.title", lang = lang),
    aside = episodic_tr(
      paste0("pathogen.panel.overlay.kind.", overlay$kind),
      lang = lang
    ),
    note = episodic_tr("epidemics.panel.overlay.note", lang = lang),
    shiny::renderPlot(
      episodic_ui_pathogen_overlay_chart(overlay, lang = lang),
      height = 280
    )
  )
}

#' Rt on the epidemic area's whole incidence
#' @keywords internal
#' @noRd
episodic_ui_epidemic_rt_panel <- function(obj,
                                          lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  # Suppressed entirely where Rt does not apply to the pathogen, as on
  # the Outbreaks dossier: a panel explaining that a food-borne pathogen
  # has no reproduction number is not something to read on every visit.
  if (!isTRUE(obj$rt_applicable)) {
    return(NULL)
  }
  if (is.null(obj$rt) || nrow(obj$rt) == 0) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.rt.title", lang = lang),
      episodic_tr(
        paste0(
          "panel.rt.unavailable.",
          if (is.na(obj$rt_unavailable_reason)) {
            "insufficient_history"
          } else {
            obj$rt_unavailable_reason
          }
        ),
        lang = lang
      )
    ))
  }
  graphics_issue <- episodic_graphics_probe()
  if (!is.null(graphics_issue)) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.rt.title", lang = lang),
      episodic_graphics_error_message(graphics_issue, lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.rt.title", lang = lang),
    note = episodic_tr("epidemics.panel.rt.note", lang = lang),
    shiny::renderPlot(
      episodic_ui_rt_chart(
        obj$rt,
        lang = lang,
        accent = episodic_nav_accent("epidemics")
      ),
      height = 240
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
  note <- episodic_tr("epidemics.panel.denominator.note", lang = lang)
  if (isTRUE(obj$denominator_catchment_only)) {
    note <- paste(
      note,
      episodic_tr("epidemics.panel.denominator.catchment_only", lang = lang)
    )
  }
  episodic_ui_panel(
    episodic_tr("epidemics.panel.denominator.title", lang = lang),
    aside = episodic_tr("panel.denominator.aside", lang = lang),
    note = shiny::HTML(note),
    shiny::renderPlot(
      episodic_ui_denominator_chart(obj$denominator, lang = lang),
      height = 240
    )
  )
}

#' Where the epidemic's cases are: the choropleth
#'
#' The Outbreaks dossier's geography panel on the epidemic's own cases:
#' one map cropped to them, and the postcode bars.
#' @keywords internal
#' @noRd
episodic_ui_epidemic_geo_panel <- function(obj,
                                           lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  geo <- obj$concentration_geo
  episodic_ui_geo_panel(
    list(
      concentration = geo,
      n_cases = if (is.null(geo)) {
        obj$n_cases
      } else {
        geo$total + geo$n_unknown_pc
      }
    ),
    lang = lang,
    accent = episodic_nav_accent("epidemics"),
    # One map, unlabelled: an epidemic's cases span its region or
    # province, so the cropped frame is already the whole of it, and
    # its areas are too many to label legibly - the bars carry the
    # counts.
    context_map = FALSE,
    label_areas = FALSE
  )
}

#' Age and sex, against the area's own baseline
#' @keywords internal
#' @noRd
episodic_ui_epidemic_demography_panel <- function(obj,
                                                  lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  demo <- obj$demography
  if (is.null(demo)) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.demography.title", lang = lang),
      episodic_tr("misc.none", lang = lang)
    ))
  }
  note <- if (is.na(demo$baseline_median_age)) {
    episodic_tr("panel.demography.note", lang = lang)
  } else {
    episodic_tr(
      "epidemics.panel.demography.note",
      median = episodic_format_number(demo$median_age, digits = 0, lang = lang),
      baseline = episodic_format_number(
        demo$baseline_median_age,
        digits = 0,
        lang = lang
      ),
      lang = lang
    )
  }
  episodic_ui_panel(
    episodic_tr("panel.demography.title", lang = lang),
    note = note,
    episodic_ui_pyramid(demo$bands, lang = lang)
  )
}

#' Which care lines the epidemic's cases were found in
#' @keywords internal
#' @noRd
episodic_ui_epidemic_care_line_panel <- function(obj,
                                                 lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.null(obj$care_lines)) {
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.care_line.title", lang = lang),
      episodic_tr("misc.none", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("pathogen.panel.care_line.title", lang = lang),
    note = episodic_tr("pathogen.panel.care_line.note", lang = lang),
    episodic_ui_bars(obj$care_lines, lang = lang)
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
    shiny::tags$tr(
      shiny::tags$td(inst$display_name[i]),
      shiny::tags$td(episodic_format_number(inst$n_cases[i], lang = lang)),
      shiny::tags$td(episodic_format_number(
        inst$n_cases[i] / total * 100,
        digits = 1,
        lang = lang
      ))
    )
  })

  episodic_ui_panel(
    episodic_tr("epidemics.panel.institutions.title", lang = lang),
    shiny::tags$div(
      style = "overflow-x:auto;",
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
  )
}

#' Outbreaks occurring during this epidemic
#'
#' The app's own cluster table, so a row reads, hovers and opens exactly
#' as it does on the Outbreaks and Pathogens screens, and carries the
#' outbreak's state: which of the outbreaks that ran during the epidemic
#' are still open is the question this panel is read for.
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

  episodic_ui_panel(
    episodic_tr("epidemics.panel.during.title", lang = lang),
    aside = episodic_count_phrase(
      nrow(during),
      episodic_tr("unit.outbreak", lang = lang),
      episodic_tr("unit.outbreaks", lang = lang),
      lang = lang
    ),
    shiny::tags$div(
      style = "overflow-x:auto;",
      episodic_ui_cluster_table(
        during,
        context = list(
          episodic_ui_cluster_col_level(lang = lang),
          episodic_ui_cluster_col_place(lang = lang)
        ),
        outcome = list(
          episodic_ui_cluster_col(
            episodic_tr("column.state", lang = lang),
            function(row) {
              if (is.null(row$state_label) || is.na(row$state_label)) {
                episodic_tr("misc.dash", lang = lang)
              } else {
                row$state_label
              }
            }
          )
        ),
        lang = lang
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
