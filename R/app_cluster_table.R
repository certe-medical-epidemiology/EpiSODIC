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

# A table of clusters is the backbone of assessing the registry, and it
# appears on nearly every screen: the Archive, the Pathogen screen, two
# panels of the dossier, the outbreak report and the notification email.
# Written out per screen, those tables drifted - different columns,
# different orders, an id in some and not others - so an epidemiologist
# had to re-learn the table at every stop. Everything below exists so
# there is one cluster table, defined once: the same spine of columns in
# the same order, sorted the same way, with the same row behaviour.
#
# The spine is deliberately fixed and cannot be switched off by a caller:
#
#   cluster id | <context> | cases | first case | last case | duration |
#   priority | <outcome>
#
# `context` (what and where: pathogen, level, place, relation) and
# `outcome` (what was decided: classification, state, closure date) are
# the only per-screen variation, because those are the parts that
# genuinely differ between an archive and a dossier panel. Everything a
# reader compares between screens - how many cases, over which days, for
# how long, at what priority - is the same column in the same place
# everywhere.

#' The number of days a cluster ran, inclusive of both end days
#'
#' A cluster whose first and last case fell on the same day ran for one
#' day, not zero. Vectorised, and `NA` for any row whose dates do not
#' parse rather than a nonsense number.
#'
#' @param first_day,last_day Character or `Date` vectors of equal length.
#' @return An integer vector.
#' @keywords internal
#' @noRd
episodic_cluster_duration_days <- function(first_day, last_day) {
  first <- suppressWarnings(as.Date(first_day))
  last <- suppressWarnings(as.Date(last_day))
  as.integer(last - first) + 1L
}

#' The order every cluster table is read in
#'
#' Most recent last case day first - the one ordering an epidemiologist
#' scanning a registry actually wants, since a cluster's last case day is
#' what says whether it is still running. Priority breaks a tie (two
#' clusters last seen the same day: the more urgent one leads), and the
#' cluster id breaks that in turn, so the order is fully determined and a
#' table never reshuffles between two renders of the same data. Rows with
#' no parseable last day sort last rather than being dropped or landing
#' arbitrarily.
#'
#' @param clusters A data frame with `last_day`, `priority_score` and
#'   `cluster_id`.
#' @return An integer vector of row positions, for `clusters[ord, ]`.
#' @keywords internal
#' @noRd
episodic_cluster_table_order <- function(clusters) {
  if (nrow(clusters) == 0) {
    return(integer(0))
  }
  last <- as.numeric(suppressWarnings(as.Date(clusters$last_day)))
  priority <- as.numeric(clusters$priority_score)
  priority[is.na(priority)] <- -Inf
  id <- as.numeric(clusters$cluster_id)
  order(-last, -priority, -id, na.last = TRUE)
}

#' The columns every cluster table carries, in order
#'
#' Named for the `column.*` translation keys they render, and shared with
#' the notification email so a column that exists on screen exists in the
#' message too.
#' @keywords internal
#' @noRd
episodic_cluster_table_spine <- c(
  "cluster",
  "cases",
  "first_day",
  "last_day",
  "duration",
  "priority"
)

#' The columns a cluster table needs to be given
#'
#' `duration_days` is deliberately absent: it is derived from the two day
#' columns rather than carried, so a caller cannot supply one that
#' disagrees with the dates next to it.
#' @keywords internal
#' @noRd
episodic_cluster_table_required <- c(
  "cluster_id",
  "n_cases",
  "first_day",
  "last_day",
  "priority_score"
)

#' One extra column of a cluster table
#'
#' @param label The column header, already translated.
#' @param render A function taking one row of the cluster data frame and
#'   returning the cell's content (a string, or a `shiny::tags` element).
#' @return A list of class `episodic_cluster_col`.
#' @keywords internal
#' @noRd
episodic_ui_cluster_col <- function(label, render) {
  if (!is.function(render)) {
    stop("A cluster table column's `render` must be a function.", call. = FALSE)
  }
  structure(
    list(label = label, render = render),
    class = "episodic_cluster_col"
  )
}

#' A cluster table row: the id, then its cells
#'
#' The id leads every row on every screen - it is what an epidemiologist
#' quotes in an email, reads out on the phone and searches the archive by
#' - and the whole row is the click target that opens the dossier. A
#' `<tr>` has no focus or activation behaviour of its own, hence
#' `tabindex` and the key handler; the server side is
#' `input$open_cluster`, which sets the selection and switches to the
#' Clusters screen.
#'
#' A row whose cluster no longer stands as a dossier of its own - one the
#' lattice suppression pass folded into another - is not silently
#' rendered as a dead link. It keeps its id, in the theme's warning
#' colour, and hovering it says why the row does not open: it used to be
#' a cluster in its own right and no longer is.
#'
#' @param cluster_id The cluster the row is about.
#' @param ... The remaining cells, in order, after the id cell.
#' @param unlinked_reason Why this row does not open a dossier, already
#'   translated, or `NA` (the default) for a row that does.
#' @param lang Session language.
#' @return A `shiny::tags$tr`.
#' @keywords internal
#' @noRd
episodic_ui_cluster_row <- function(
    cluster_id,
    ...,
    unlinked_reason = NA_character_,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  ref <- episodic_tr("dossier.cluster_ref", id = cluster_id, lang = lang)
  unlinked <- length(unlinked_reason) == 1 &&
    !is.na(unlinked_reason) &&
    nzchar(unlinked_reason)

  if (unlinked) {
    return(shiny::tags$tr(
      shiny::tags$td(
        class = "episodic-cell-id",
        shiny::tags$span(
          class = "episodic-id-unlinked",
          title = unlinked_reason,
          ref
        )
      ),
      ...
    ))
  }

  open_js <- sprintf(
    "Shiny.setInputValue('open_cluster', %d, {priority: 'event'});",
    as.integer(cluster_id)
  )
  shiny::tags$tr(
    class = "episodic-row-link",
    tabindex = "0",
    title = episodic_tr("cluster.open_hint", lang = lang),
    onclick = open_js,
    onkeydown = sprintf(
      "if(event.key==='Enter'||event.key===' '){event.preventDefault();%s}",
      open_js
    ),
    shiny::tags$td(
      class = "episodic-cell-id",
      shiny::tags$span(class = "episodic-id-link", ref)
    ),
    ...
  )
}

#' The cluster table
#'
#' Every table of clusters in the dashboard. See the file header for what
#' the spine is and why it is not negotiable per screen.
#'
#' @param clusters A data frame with at least
#'   `episodic_cluster_table_required`. An `unlinked_reason` column, where
#'   present, marks the rows that do not open a dossier and says why (see
#'   `episodic_ui_cluster_row()`).
#' @param context Extra columns placed directly after the id - what and
#'   where the cluster is. A list of `episodic_ui_cluster_col()`.
#' @param outcome Extra columns placed at the end - what was decided about
#'   it. A list of `episodic_ui_cluster_col()`.
#' @param lang Session language.
#' @return A `shiny::tags$table`.
#' @keywords internal
#' @noRd
episodic_ui_cluster_table <- function(
    clusters,
    context = list(),
    outcome = list(),
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  # A cluster table that quietly rendered a blank column would be a table
  # an epidemiologist reads as "no cases" or "no priority" rather than as
  # "this screen forgot to fetch it", so a missing column stops here.
  absent <- setdiff(episodic_cluster_table_required, names(clusters))
  if (length(absent) > 0) {
    stop(
      "A cluster table needs column",
      if (length(absent) > 1) "s" else "",
      " ",
      paste0("`", absent, "`", collapse = ", "),
      ", which the supplied data frame does not have.",
      call. = FALSE
    )
  }
  for (col in c(context, outcome)) {
    if (!inherits(col, "episodic_cluster_col")) {
      stop(
        "Every cluster table column must come from `episodic_ui_cluster_col()`.",
        call. = FALSE
      )
    }
  }

  clusters <- clusters[episodic_cluster_table_order(clusters), , drop = FALSE]
  duration <- episodic_cluster_duration_days(
    clusters$first_day,
    clusters$last_day
  )
  dash <- episodic_tr("misc.dash", lang = lang)
  reasons <- clusters$unlinked_reason %||%
    rep(NA_character_, nrow(clusters))

  spine_labels <- vapply(
    episodic_cluster_table_spine,
    function(key) episodic_tr(paste0("column.", key), lang = lang),
    character(1)
  )
  headers <- c(
    spine_labels[1],
    vapply(context, function(col) col$label, character(1)),
    spine_labels[-1],
    vapply(outcome, function(col) col$label, character(1))
  )

  body <- lapply(seq_len(nrow(clusters)), function(i) {
    row <- clusters[i, , drop = FALSE]
    cells <- c(
      lapply(context, function(col) shiny::tags$td(col$render(row))),
      list(
        shiny::tags$td(row$n_cases),
        shiny::tags$td(episodic_format_date(row$first_day, lang = lang)),
        shiny::tags$td(episodic_format_date(row$last_day, lang = lang)),
        shiny::tags$td(if (is.na(duration[i])) {
          dash
        } else {
          episodic_count_phrase(
            duration[i],
            episodic_tr("unit.day", lang = lang),
            episodic_tr("unit.days", lang = lang)
          )
        }),
        shiny::tags$td(if (is.na(row$priority_score)) {
          dash
        } else {
          round(row$priority_score, 0)
        })
      ),
      lapply(outcome, function(col) shiny::tags$td(col$render(row)))
    )
    do.call(
      episodic_ui_cluster_row,
      c(
        list(row$cluster_id),
        cells,
        list(unlinked_reason = reasons[i], lang = lang)
      ),
      # The arguments are already values, not expressions; quoting keeps
      # `do.call()` from deparsing whole tag trees into the call it builds.
      quote = TRUE
    )
  })

  shiny::tags$table(
    class = "episodic-table",
    shiny::tags$thead(shiny::tags$tr(lapply(headers, shiny::tags$th))),
    shiny::tags$tbody(body)
  )
}

#' The context columns cluster tables share
#'
#' Pathogen, level and place are the same three questions on every screen
#' that asks them, so they are built once here rather than re-declared
#' per panel. Each takes the column its screen happens to hold the value
#' in, since a screen that already computed a level label should not
#' recompute it.
#' @keywords internal
#' @noRd
episodic_ui_cluster_col_pathogen <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_ui_cluster_col(
    episodic_tr("column.pathogen", lang = lang),
    function(row) shiny::HTML(episodic_ui_italicise_taxon(row$pathogen))
  )
}

#' @keywords internal
#' @noRd
episodic_ui_cluster_col_level <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_ui_cluster_col(
    episodic_tr("column.level", lang = lang),
    function(row) row$level_label
  )
}

#' @keywords internal
#' @noRd
episodic_ui_cluster_col_place <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_ui_cluster_col(
    episodic_tr("column.place", lang = lang),
    function(row) row$place
  )
}

#' @keywords internal
#' @noRd
episodic_ui_cluster_col_closed_at <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_ui_cluster_col(
    episodic_tr("column.closed_at", lang = lang),
    function(row) {
      if (is.na(row$closed_at)) {
        episodic_tr("misc.unknown", lang = lang)
      } else {
        episodic_format_date(row$closed_at, lang = lang)
      }
    }
  )
}

#' @keywords internal
#' @noRd
episodic_ui_cluster_col_verdict <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  episodic_ui_cluster_col(
    episodic_tr("column.verdict", lang = lang),
    function(row) {
      if (is.na(row$verdict_label)) {
        episodic_tr("misc.dash", lang = lang)
      } else {
        row$verdict_label
      }
    }
  )
}
