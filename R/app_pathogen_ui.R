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

#' The Pathogen screen
#'
#' Pathogen picker, period picker, then the panels in the order the
#' question is actually asked: how much, where in the season, how fast,
#' who, where, and finally which signals were raised during it.
#'
#' @param screen A list from `episodic_app_pathogen_screen()`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_pathogen_screen <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (nrow(screen$pathogens) == 0 || is.null(screen$pathogen)) {
    return(shiny::tags$div(
      class = "episodic-streams-screen",
      shiny::tags$h1(
        style = "font-size:22px;font-weight:600;margin-bottom:4px;",
        episodic_tr("pathogen.title", lang = lang)
      ),
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("pathogen.empty", lang = lang)
      )
    ))
  }

  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(
      style = "font-size:22px;font-weight:600;margin-bottom:4px;",
      episodic_tr("pathogen.title", lang = lang)
    ),
    shiny::tags$p(
      style = "font-size:12.5px;color:var(--episodic-muted);margin-bottom:16px;",
      episodic_tr("pathogen.note", lang = lang)
    ),
    episodic_ui_pathogen_controls(screen, lang = lang),
    episodic_ui_pathogen_stats(screen, lang = lang),
    episodic_ui_pathogen_curve_panel(screen, lang = lang),
    episodic_ui_pathogen_overlay_panel(screen, lang = lang),
    episodic_ui_pathogen_rt_panel(screen, lang = lang),
    episodic_ui_pathogen_denominator_panel(screen, lang = lang),
    episodic_ui_pathogen_demography_panel(screen, lang = lang),
    episodic_ui_pathogen_geo_panel(screen, lang = lang),
    episodic_ui_pathogen_breakdown_panels(screen, lang = lang),
    episodic_ui_pathogen_clusters_panel(screen, lang = lang),
    episodic_ui_pathogen_config_panel(screen, lang = lang)
  )
}

#' Pathogen and period selectors
#'
#' The period presets carry the epidemiological calendar (a surveillance
#' season is one click, not two date entries), with explicit from/to
#' behind a `custom` option for everything the presets do not cover.
#'
#' @param screen A list from `episodic_app_pathogen_screen()`.
#' @param lang Session language.
#' @keywords internal
#' @noRd
episodic_ui_pathogen_controls <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  period <- screen$period

  # "Norovirus (2197)" reads as an identifier; it is a case count, and an
  # all-time one, which is the number that decides whether this pathogen
  # has enough history for the screen to say anything at all. Spelled out
  # with its unit so it cannot be mistaken for an id.
  case_words <- c(
    episodic_tr("unit.case", lang = lang),
    episodic_tr("unit.cases", lang = lang)
  )
  pathogen_options <- lapply(seq_len(nrow(screen$pathogens)), function(i) {
    row <- screen$pathogens[i, ]
    shiny::tags$option(
      value = row$pathogen,
      selected = if (identical(row$pathogen, screen$pathogen)) "selected",
      episodic_tr(
        "pathogen.select_option",
        lang = lang,
        pathogen = row$pathogen,
        cases = episodic_count_phrase(row$n_cases, case_words[1], case_words[2])
      )
    )
  })

  period_buttons <- lapply(episodic_pathogen_period_ids, function(id) {
    active <- identical(id, period$id)
    shiny::tags$button(
      type = "button",
      class = if (active) {
        "episodic-picker-btn active"
      } else {
        "episodic-picker-btn"
      },
      style = if (active) {
        sprintf(
          "background:%s;border-color:%s;color:#fff;",
          pal$primary,
          pal$primary
        )
      } else {
        ""
      },
      title = if (id %in% c("season_current", "season_previous")) {
        episodic_tr("pathogen.period.season_note", lang = lang)
      },
      onclick = sprintf(
        "Shiny.setInputValue('pathogen_period', '%s', {priority: 'event'}); return false;",
        id
      ),
      episodic_tr(paste0("pathogen.period.", id), lang = lang)
    )
  })

  shiny::tags$div(
    class = "episodic-pathogen-controls",
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("pathogen.select_label", lang = lang)
      ),
      shiny::tags$select(
        id = "pathogen_select",
        class = "episodic-select",
        onchange = "Shiny.setInputValue('pathogen_select', this.value, {priority: 'event'})",
        pathogen_options
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("pathogen.period_label", lang = lang)
      ),
      shiny::tags$div(
        class = "episodic-picker episodic-picker-inline",
        period_buttons
      ),
      shiny::tags$div(
        style = "font-size:11px;color:var(--episodic-faint);margin-top:4px;",
        episodic_tr("pathogen.period.season_note", lang = lang)
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("pathogen.custom_label", lang = lang)
      ),
      shiny::tags$div(
        style = "display:flex;gap:8px;align-items:center;",
        shiny::tags$input(
          type = "date",
          class = "episodic-date-input",
          id = "pathogen_from",
          value = as.character(period$from)
        ),
        shiny::tags$span(style = "color:var(--episodic-faint);", "\u2013"),
        shiny::tags$input(
          type = "date",
          class = "episodic-date-input",
          id = "pathogen_to",
          value = as.character(period$to)
        ),
        shiny::tags$button(
          type = "button",
          class = "episodic-btn",
          onclick = paste0(
            "Shiny.setInputValue('pathogen_custom_range', ",
            "{from: document.getElementById('pathogen_from').value, ",
            "to: document.getElementById('pathogen_to').value}, {priority: 'event'});"
          ),
          episodic_tr("pathogen.custom_apply", lang = lang)
        )
      )
    ),
    shiny::tags$div(
      class = "episodic-pathogen-period-label",
      episodic_tr(
        "pathogen.showing",
        lang = lang,
        from = episodic_format_date(period$from, lang = lang),
        to = episodic_format_date(period$to, lang = lang),
        asof = episodic_format_date(screen$asof, lang = lang)
      )
    )
  )
}

#' Headline numbers for the selected pathogen and period
#' @keywords internal
#' @noRd
episodic_ui_pathogen_stats <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  s <- screen$summary

  stats <- list(
    episodic_ui_stat(
      episodic_tr("pathogen.stat.cases", lang = lang),
      s$n_cases,
      episodic_tr("pathogen.stat.cases_sub", weeks = s$n_weeks, lang = lang)
    ),
    episodic_ui_stat(
      episodic_tr("pathogen.stat.patients", lang = lang),
      s$n_patients,
      episodic_tr("pathogen.stat.patients_sub", lang = lang)
    )
  )
  if (!is.na(s$peak_n)) {
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("pathogen.stat.peak", lang = lang),
        s$peak_n,
        episodic_tr(
          "pathogen.stat.peak_sub",
          week = episodic_format_date(s$peak_week, lang = lang),
          lang = lang
        )
      ))
    )
  }
  if (!is.na(s$n_previous)) {
    change <- if (is.na(s$change_pct)) {
      episodic_tr("misc.dash", lang = lang)
    } else {
      sprintf("%+d%%", as.integer(s$change_pct))
    }
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("pathogen.stat.change", lang = lang),
        change,
        episodic_tr("pathogen.stat.change_sub", n = s$n_previous, lang = lang),
        colour = if (!is.na(s$change_pct) && s$change_pct > 0) {
          pal$danger
        } else if (!is.na(s$change_pct) && s$change_pct < 0) {
          pal$success
        } else {
          pal$muted
        }
      ))
    )
  }
  if (!is.null(screen$mem) && !is.na(screen$mem$peak_level)) {
    stats <- c(
      stats,
      list(episodic_ui_stat(
        episodic_tr("pathogen.stat.intensity", lang = lang),
        episodic_tr(
          paste0("pathogen.intensity.", screen$mem$peak_level),
          lang = lang
        ),
        episodic_tr("pathogen.stat.intensity_sub", lang = lang),
        colour = episodic_ui_intensity_colour(screen$mem$peak_level)
      ))
    )
  }
  shiny::tags$div(class = "episodic-statgrid", stats)
}

#' Colour for a MEM intensity band
#'
#' Baseline through very high on the palette's own warning/danger ramp,
#' so intensity reads as a severity scale rather than as five unrelated
#' categories.
#'
#' @param level One of `episodic_mem_intensity_level()`'s values.
#' @keywords internal
#' @noRd
episodic_ui_intensity_colour <- function(level) {
  pal <- episodic_palette()
  switch(as.character(level),
    baseline = pal$muted,
    low = pal$success,
    medium = pal$warning_dark,
    high = pal$danger,
    very_high = pal$danger_dark,
    pal$muted
  )
}

#' The weekly curve, with MEM thresholds where they exist
#' @keywords internal
#' @noRd
episodic_ui_pathogen_curve_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  # An all-zero curve is not a chart worth drawing: a period with no
  # cases of this pathogen is a sentence, not a row of empty bars.
  if (
    is.null(screen$weekly) ||
      nrow(screen$weekly) == 0 ||
      sum(screen$weekly$n_cases) == 0
  ) {
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.curve.title", lang = lang),
      episodic_tr("pathogen.panel.curve.empty", lang = lang)
    ))
  }
  thresholds <- screen$mem$thresholds
  note <- if (!is.null(thresholds)) {
    episodic_tr(
      "pathogen.panel.curve.mem_note",
      lang = lang,
      seasons = length(thresholds$seasons_used),
      pre = round(thresholds$pre_epidemic, 1),
      post = round(thresholds$post_epidemic, 1)
    )
  } else if (isTRUE(screen$seasonal)) {
    episodic_tr("pathogen.panel.curve.mem_unavailable", lang = lang)
  } else {
    episodic_tr("pathogen.panel.curve.note", lang = lang)
  }

  episodic_ui_panel(
    episodic_tr("pathogen.panel.curve.title", lang = lang),
    aside = episodic_tr(
      "pathogen.panel.curve.aside",
      weeks = nrow(screen$weekly),
      lang = lang
    ),
    note = shiny::HTML(note),
    shiny::renderPlot(
      episodic_ui_pathogen_curve_chart(screen$weekly, thresholds, lang = lang),
      height = 300
    )
  )
}

#' The season-over-season (or year-over-year) overlay
#' @keywords internal
#' @noRd
episodic_ui_pathogen_overlay_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  overlay <- screen$overlay
  if (is.null(overlay)) {
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.overlay.title", lang = lang),
      episodic_tr("pathogen.panel.overlay.empty", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("pathogen.panel.overlay.title", lang = lang),
    aside = episodic_tr(
      paste0("pathogen.panel.overlay.kind.", overlay$kind),
      lang = lang
    ),
    note = episodic_tr("pathogen.panel.overlay.note", lang = lang),
    shiny::renderPlot(
      episodic_ui_pathogen_overlay_chart(overlay, lang = lang),
      height = 300
    )
  )
}

#' Pathogen-level Rt
#' @keywords internal
#' @noRd
episodic_ui_pathogen_rt_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.null(screen$rt)) {
    reason <- screen$rt_unavailable_reason
    msg <- if (is.na(reason)) {
      episodic_tr("pathogen.panel.rt.not_applicable", lang = lang)
    } else {
      episodic_tr(paste0("panel.rt.unavailable.", reason), lang = lang)
    }
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.rt.title", lang = lang),
      msg
    ))
  }
  episodic_ui_panel(
    episodic_tr("pathogen.panel.rt.title", lang = lang),
    note = episodic_tr("pathogen.panel.rt.note", lang = lang),
    shiny::renderPlot(
      episodic_ui_rt_chart(screen$rt, lang = lang),
      height = 260
    )
  )
}

#' Tests and positivity across the catchment
#' @keywords internal
#' @noRd
episodic_ui_pathogen_denominator_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.null(screen$denominator)) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.denominator.title", lang = lang),
      episodic_tr("panel.denominator.unavailable", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("panel.denominator.title", lang = lang),
    aside = episodic_tr("panel.denominator.aside", lang = lang),
    note = episodic_tr("pathogen.panel.denominator.note", lang = lang),
    shiny::renderPlot(
      episodic_ui_denominator_chart(screen$denominator, lang = lang),
      height = 260
    )
  )
}

#' Age and sex over the period
#' @keywords internal
#' @noRd
episodic_ui_pathogen_demography_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  demo <- screen$demography
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
      "pathogen.panel.demography.note",
      lang = lang,
      median = round(demo$median_age, 0),
      baseline = round(demo$baseline_median_age, 0)
    )
  }
  episodic_ui_panel(
    episodic_tr("panel.demography.title", lang = lang),
    note = note,
    episodic_ui_pyramid(demo$bands, lang = lang)
  )
}

#' Where the period's cases were, full width
#' @keywords internal
#' @noRd
episodic_ui_pathogen_geo_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  concentration <- screen$concentration
  if (is.null(concentration)) {
    return(episodic_ui_panel_empty(
      episodic_tr("panel.geo.title", lang = lang),
      episodic_tr("panel.geo.empty", lang = lang)
    ))
  }
  map_chart <- episodic_ui_geo_map_chart(concentration$rows)
  episodic_ui_panel(
    episodic_tr("panel.geo.title", lang = lang),
    aside = episodic_tr("panel.geo.aside", lang = lang),
    if (is.null(map_chart)) {
      episodic_ui_bars(
        utils::head(concentration$rows, 12),
        unit = episodic_tr("panel.geo.unit", lang = lang)
      )
    } else {
      shiny::tagList(
        shiny::tags$div(
          class = "episodic-geo-single",
          shiny::renderPlot(
            map_chart,
            height = episodic_ui_map_render_height(map_chart)
          )
        ),
        shiny::tags$p(
          class = "episodic-panel-note",
          episodic_tr("panel.geo.map_note", lang = lang)
        ),
        episodic_ui_bars(utils::head(concentration$rows, 12))
      )
    }
  )
}

#' Care line and institution breakdowns, side by side
#' @keywords internal
#' @noRd
episodic_ui_pathogen_breakdown_panels <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  care_lines <- screen$care_lines
  institutions <- screen$institutions
  shiny::tags$div(
    style = "display:flex;gap:16px;align-items:flex-start;",
    shiny::tags$div(
      style = "flex:1;",
      if (is.null(care_lines)) {
        episodic_ui_panel_empty(
          episodic_tr("pathogen.panel.care_line.title", lang = lang),
          episodic_tr("misc.none", lang = lang)
        )
      } else {
        episodic_ui_panel(
          episodic_tr("pathogen.panel.care_line.title", lang = lang),
          note = episodic_tr("pathogen.panel.care_line.note", lang = lang),
          episodic_ui_bars(care_lines)
        )
      }
    ),
    shiny::tags$div(
      style = "flex:1;",
      if (is.null(institutions)) {
        episodic_ui_panel_empty(
          episodic_tr("pathogen.panel.institutions.title", lang = lang),
          episodic_tr("misc.none", lang = lang)
        )
      } else {
        episodic_ui_panel(
          episodic_tr("pathogen.panel.institutions.title", lang = lang),
          episodic_ui_bars(institutions)
        )
      }
    )
  )
}

#' The signals raised for this pathogen during the period
#' @keywords internal
#' @noRd
episodic_ui_pathogen_clusters_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  clusters <- screen$clusters
  if (is.null(clusters) || nrow(clusters) == 0) {
    return(episodic_ui_panel_empty(
      episodic_tr("pathogen.panel.clusters.title", lang = lang),
      episodic_tr("pathogen.panel.clusters.empty", lang = lang)
    ))
  }
  episodic_ui_panel(
    episodic_tr("pathogen.panel.clusters.title", lang = lang),
    aside = episodic_count_phrase(
      nrow(clusters),
      episodic_tr("unit.cluster", lang = lang),
      episodic_tr("unit.clusters", lang = lang)
    ),
    note = episodic_tr("pathogen.panel.clusters.note", lang = lang),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$thead(shiny::tags$tr(
        # First column: the id is the handle everything else refers to -
        # what gets quoted in an email or searched for in the archive -
        # so it leads the row rather than being absent from a table of
        # clusters entirely.
        shiny::tags$th(episodic_tr("column.cluster", lang = lang)),
        shiny::tags$th(episodic_tr(
          "pathogen.panel.clusters.col.period",
          lang = lang
        )),
        shiny::tags$th(episodic_tr("archive.col.level", lang = lang)),
        shiny::tags$th(episodic_tr("archive.col.place", lang = lang)),
        shiny::tags$th(episodic_tr("archive.col.cases", lang = lang)),
        shiny::tags$th(episodic_tr("panel.similar.col.verdict", lang = lang)),
        shiny::tags$th(episodic_tr(
          "pathogen.panel.clusters.col.state",
          lang = lang
        ))
      )),
      shiny::tags$tbody(lapply(seq_len(nrow(clusters)), function(i) {
        row <- clusters[i, ]
        episodic_ui_cluster_link_row(
          row$cluster_id,
          lang = lang,
          shiny::tags$td(episodic_format_date_range(
            row$first_day,
            row$last_day,
            lang = lang
          )),
          shiny::tags$td(row$level_label),
          shiny::tags$td(row$place),
          shiny::tags$td(row$n_cases),
          shiny::tags$td(row$verdict_label),
          shiny::tags$td(row$state_label)
        )
      }))
    )
  )
}

#' The pathogen's own row from the loaded pathogen configuration
#'
#' What this instance is actually working from for this pathogen: the
#' episode window that decides one case per episode
#' (`episodic_cases_deduplicate()`), the case-free/cool-down days that
#' decide when a cluster is closable, whether Rt and a seasonal baseline
#' apply, and what feeds them. All of it lives in
#' `inst/config/pathogen_config.csv` (or an instance's own override of
#' it) and is otherwise invisible from this screen - readable here
#' rather than only in the source file.
#' @keywords internal
#' @noRd
episodic_ui_pathogen_config_panel <- function(
    screen,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pc <- screen$config
  if (is.null(pc)) {
    return(episodic_ui_panel(
      paste(
        episodic_tr("pathogen.panel.config.title", lang = lang),
        screen$pathogen
      ),
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr(
          "pathogen.panel.config.none",
          pathogen = screen$pathogen,
          lang = lang
        )
      )
    ))
  }

  dash <- episodic_tr("misc.dash", lang = lang)
  yes_no <- function(x) {
    episodic_tr(
      if (isTRUE(as.logical(x))) "misc.yes" else "misc.no",
      lang = lang
    )
  }
  day_phrase <- function(n) {
    if (is.na(n)) {
      return(dash)
    }
    episodic_count_phrase(
      n,
      episodic_tr("unit.day", lang = lang),
      episodic_tr("unit.days", lang = lang)
    )
  }

  rows <- list(list(
    label = episodic_tr("pathogen.panel.config.episode.label", lang = lang),
    value = day_phrase(pc$episode_days),
    meaning = episodic_tr("pathogen.panel.config.episode.meaning", lang = lang)
  ))

  if (!is.na(pc$incub_min_days) || !is.na(pc$incub_max_days)) {
    rows <- c(
      rows,
      list(list(
        label = episodic_tr(
          "pathogen.panel.config.incubation.label",
          lang = lang
        ),
        value = episodic_tr(
          "pathogen.panel.config.incubation.value",
          min = pc$incub_min_days %||% dash,
          max = pc$incub_max_days %||% dash,
          lang = lang
        ),
        meaning = episodic_tr(
          "pathogen.panel.config.incubation.meaning",
          lang = lang
        )
      ))
    )
  }

  rows <- c(
    rows,
    list(
      list(
        label = episodic_tr(
          "pathogen.panel.config.case_free.label",
          lang = lang
        ),
        value = day_phrase(pc$case_free_days),
        meaning = episodic_tr(
          "pathogen.panel.config.case_free.meaning",
          lang = lang
        )
      ),
      list(
        label = episodic_tr(
          "pathogen.panel.config.cooldown.label",
          lang = lang
        ),
        value = day_phrase(pc$cooldown_days),
        meaning = episodic_tr(
          "pathogen.panel.config.cooldown.meaning",
          lang = lang
        )
      ),
      list(
        label = episodic_tr("pathogen.panel.config.rt.label", lang = lang),
        value = yes_no(pc$rt_applicable),
        meaning = episodic_tr("pathogen.panel.config.rt.meaning", lang = lang)
      )
    )
  )

  if (isTRUE(as.logical(pc$rt_applicable)) && !is.na(pc$si_mean_days)) {
    rows <- c(
      rows,
      list(list(
        label = episodic_tr("pathogen.panel.config.si.label", lang = lang),
        value = episodic_tr(
          "pathogen.panel.config.si.value",
          mean = pc$si_mean_days,
          sd = pc$si_sd_days %||% dash,
          dist = pc$si_dist %||% dash,
          lang = lang
        ),
        meaning = episodic_tr("pathogen.panel.config.si.meaning", lang = lang)
      ))
    )
  }

  rows <- c(
    rows,
    list(
      list(
        label = episodic_tr(
          "pathogen.panel.config.seasonal.label",
          lang = lang
        ),
        value = yes_no(pc$mem_applicable),
        meaning = episodic_tr(
          "pathogen.panel.config.seasonal.meaning",
          lang = lang
        )
      ),
      list(
        label = episodic_tr(
          "pathogen.panel.config.severity.label",
          lang = lang
        ),
        value = as.character(pc$severity_weight %||% dash),
        meaning = episodic_tr(
          "pathogen.panel.config.severity.meaning",
          lang = lang
        )
      )
    )
  )

  if (!is.na(pc$source_ref) && nzchar(trimws(pc$source_ref %||% ""))) {
    rows <- c(
      rows,
      list(list(
        label = episodic_tr("pathogen.panel.config.source.label", lang = lang),
        # The reference itself is often a long citation string, so it goes
        # in the wide "What it does" column rather than "Value" - the
        # narrower column that every other row's short number or yes/no
        # has to keep sized for.
        value = dash,
        meaning = pc$source_ref
      ))
    )
  }

  episodic_ui_panel(
    paste(
      episodic_tr("pathogen.panel.config.title", lang = lang),
      screen$pathogen
    ),
    shiny::tags$p(
      style = "font-size:12.5px;margin:0 0 12px;",
      episodic_tr(
        "pathogen.panel.config.note",
        pathogen = screen$pathogen,
        lang = lang
      )
    ),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(episodic_tr(
          "pathogen.panel.config.col.parameter",
          lang = lang
        )),
        shiny::tags$th(episodic_tr(
          "pathogen.panel.config.col.value",
          lang = lang
        )),
        shiny::tags$th(episodic_tr(
          "pathogen.panel.config.col.meaning",
          lang = lang
        ))
      )),
      shiny::tags$tbody(
        lapply(rows, function(r) {
          shiny::tags$tr(
            shiny::tags$td(r$label),
            shiny::tags$td(r$value),
            shiny::tags$td(r$meaning)
          )
        })
      )
    )
  )
}
