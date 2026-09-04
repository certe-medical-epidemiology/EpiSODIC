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
#   cluster id | <context> | first case | last case | cases | case days |
#   <outcome> | duration | priority
#
# `context` (what and where: pathogen, level, place, relation) and
# `outcome` (what was decided: classification, state, closure date) are
# the only per-screen variation, because those are the parts that
# genuinely differ between an archive and a dossier panel. Everything a
# reader compares between screens - how many cases, over how many of
# those days, over which calendar span, for how long, at what priority -
# is the same column in the same place everywhere. `outcome` sits right
# after `case days` rather than at the very end: it is what was decided
# about the signal the columns before it describe, so it reads as the
# conclusion to that evidence rather than as an afterthought tacked on
# past `duration`/`priority`, which describe the signal's shape, not its
# handling.
#
# `case days` (`episodic_db_attach_case_days()`) is distinct from
# `duration`: duration is the calendar span from first case to last case
# inclusive, so a cluster can run 90 days with cases on only 10 of them
# (a sharp peak) or on 39 of them (a flat, sustained rise) - the same
# duration and case count, two different epidemic shapes.

#' A cluster's stored day column, as dates
#'
#' `as.Date()` on a character vector *errors* on a value it cannot parse
#' rather than returning `NA`, which would take a whole screen down over
#' one malformed row. Every day column in this database is ISO
#' `YYYY-MM-DD` (`episodic_sql_date()`), so naming that format is both
#' the right reading and the one that yields `NA` for anything else.
#'
#' @param x A character or `Date` vector.
#' @return A `Date` vector the same length as `x`.
#' @keywords internal
#' @noRd
episodic_cluster_day <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(as.character(x), format = "%Y-%m-%d")
}

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
  as.integer(
    episodic_cluster_day(last_day) - episodic_cluster_day(first_day)
  ) +
    1L
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
  last <- as.numeric(episodic_cluster_day(clusters$last_day))
  priority <- as.numeric(clusters$priority_score)
  priority[is.na(priority)] <- -Inf
  id <- as.numeric(clusters$cluster_id)
  order(-last, -priority, -id, na.last = TRUE)
}

#' The columns every cluster table carries, in order
#'
#' Named for the `column.*` translation keys they render, and shared with
#' the notification email so a column that exists on screen exists in the
#' message too. `<outcome>` (see the file header) is spliced in right
#' after `episodic_cluster_table_spine_outcome_after`, not appended to
#' this vector - a caller never sees that split, only the one rendered
#' order it produces.
#' @keywords internal
#' @noRd
episodic_cluster_table_spine <- c(
  "cluster",
  "first_day",
  "last_day",
  "cases",
  "case_days",
  "duration",
  "priority"
)

#' @keywords internal
#' @noRd
episodic_cluster_table_spine_outcome_after <- "case_days"

#' The columns a cluster table needs to be given
#'
#' `duration_days` is deliberately absent: it is derived from the two day
#' columns rather than carried, so a caller cannot supply one that
#' disagrees with the dates next to it. `case_days` cannot be derived the
#' same way - it needs the case-level link table, not just the two day
#' columns - so unlike duration it *is* required input; a caller builds
#' it with [episodic_db_attach_case_days()].
#' @keywords internal
#' @noRd
episodic_cluster_table_required <- c(
  "cluster_id",
  "n_cases",
  "case_days",
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

  open_js <- sprintf("episodicOpenCluster(%d);", as.integer(cluster_id))
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
#' @param outcome Extra columns placed right after "case days" - what was
#'   decided about the cluster the columns before it describe. A list of
#'   `episodic_ui_cluster_col()`.
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

  col_label <- function(key) episodic_tr(paste0("column.", key), lang = lang)

  # Each spine key becomes one header cell, except "first_day" and
  # "last_day": together they become the period triple - two split
  # columns plus one combined "Case period" column, all three always in
  # the DOM. inst/app/www/episodic.css shows exactly one set via @media,
  # collapsing to the range once the table has too many columns to keep
  # both dates legible; a static server-rendered table cannot pick the
  # breakpoint itself, so both renderings ship and CSS chooses.
  spine_header <- function(key) {
    if (identical(key, "first_day")) {
      return(list(
        shiny::tags$th(class = "episodic-col-period-split", col_label("first_day")),
        shiny::tags$th(class = "episodic-col-period-split", col_label("last_day")),
        shiny::tags$th(class = "episodic-col-period-combined", col_label("period"))
      ))
    }
    if (identical(key, "last_day")) {
      return(list()) # folded into the "first_day" triple above
    }
    list(shiny::tags$th(col_label(key)))
  }

  spine_cell <- function(key, row, i) {
    if (identical(key, "first_day")) {
      return(list(
        shiny::tags$td(
          class = "episodic-cell-period-split",
          episodic_format_date(row$first_day, lang = lang)
        ),
        shiny::tags$td(
          class = "episodic-cell-period-split",
          episodic_format_date(row$last_day, lang = lang)
        ),
        shiny::tags$td(
          class = "episodic-cell-period-combined",
          episodic_format_date_range(row$first_day, row$last_day, lang = lang)
        )
      ))
    }
    if (identical(key, "last_day")) {
      return(list()) # folded into the "first_day" triple above
    }
    if (identical(key, "cases")) {
      return(list(shiny::tags$td(row$n_cases)))
    }
    if (identical(key, "case_days")) {
      return(list(shiny::tags$td(row$case_days)))
    }
    if (identical(key, "duration")) {
      return(list(shiny::tags$td(if (is.na(duration[i])) {
        dash
      } else {
        episodic_count_phrase(
          duration[i],
          episodic_tr("unit.day", lang = lang),
          episodic_tr("unit.days", lang = lang)
        )
      })))
    }
    # "priority"
    list(shiny::tags$td(if (is.na(row$priority_score)) {
      dash
    } else {
      round(row$priority_score, 0)
    }))
  }

  # The spine minus "cluster" (already the row's own leading id cell, via
  # episodic_ui_cluster_row()), spliced with `outcome` right after
  # episodic_cluster_table_spine_outcome_after - the one place the
  # rendered order actually departs from the spine vector's own order,
  # and the reason that marker exists rather than a caller having to
  # know where to insert `outcome` by re-reading this function. `per_key`
  # is `spine_header`/`spine_cell` (partially applied per row for cells),
  # `outcome_tags` the already-built outcome `<th>`/`<td>` list for this
  # splice point.
  rest <- episodic_cluster_table_spine[-1]
  splice_after_outcome <- function(per_key, outcome_tags) {
    # do.call(c, ...) rather than unlist(..., recursive = FALSE): each
    # per-key result and outcome_tags is itself a list of shiny tag
    # objects (which are themselves lists), and c() concatenates lists
    # one level deep without touching what is inside each element - the
    # same idiom this file already uses for context/outcome elsewhere.
    # unlist() risks looking one level too far into a tag's own internals.
    do.call(c, lapply(rest, function(key) {
      if (identical(key, episodic_cluster_table_spine_outcome_after)) {
        c(per_key(key), outcome_tags)
      } else {
        per_key(key)
      }
    }))
  }

  headers <- c(
    list(shiny::tags$th(col_label("cluster"))),
    lapply(context, function(col) shiny::tags$th(col$label)),
    splice_after_outcome(
      spine_header,
      lapply(outcome, function(col) shiny::tags$th(col$label))
    )
  )

  body <- lapply(seq_len(nrow(clusters)), function(i) {
    row <- clusters[i, , drop = FALSE]
    cells <- c(
      lapply(context, function(col) shiny::tags$td(col$render(row))),
      splice_after_outcome(
        function(key) spine_cell(key, row, i),
        lapply(outcome, function(col) shiny::tags$td(col$render(row)))
      )
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
    shiny::tags$thead(shiny::tags$tr(headers)),
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
