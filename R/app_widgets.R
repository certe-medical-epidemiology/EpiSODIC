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

# Small reusable UI building blocks: thin shiny::tags wrappers for the
# interface's small recurring primitives (a chip, a panel, a stat tile, a
# bar, a pyramid), server-side rendered rather than a client-side
# component library. Styling lives in inst/app/www/episodic.css; these
# functions only assign class names and content.

#' Format text for outbreak reports and the dashboard
#'
#' Small HTML formatting helpers used when building dossier text, both in
#' the dashboard and in the Quarto outbreak report template
#' (`inst/report/cluster_report.qmd`). Both are exported so that a custom
#' report template of your own (set via `EPISODIC_QUARTO_REPORT`) can use
#' the same formatting as the shipped one. Input text is always HTML-escaped
#' first, so the result is safe to pass on to [shiny::HTML()].
#'
#' `episodic_ui_italicise_taxon()` italicises pathogen names that `AMR`
#' recognises as a taxonomic binomial (e.g. *Escherichia coli*), following
#' standard microbiological convention. Names it does not recognise (e.g.
#' "Influenza A", a virus type rather than a species) are left as-is.
#'
#' `episodic_ui_code_join()` renders a vector of detector names (e.g.
#' `"farrington"`, `"same_place"`) as inline `<code>`, joined into one
#' string - useful when listing which detectors flagged a cluster.
#'
#' @param pathogen A character vector of pathogen display names.
#' @return A character vector (or, for `episodic_ui_code_join()`, a single
#'   string), safe to pass to [shiny::HTML()].
#' @examples
#' episodic_ui_italicise_taxon(c("Escherichia coli", "Influenza A"))
#' episodic_ui_code_join(c("farrington", "same_place"))
#' @export
episodic_ui_italicise_taxon <- function(pathogen) {
  escaped <- gsub("&", "&amp;", pathogen, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  is_taxon <- pathogen %in% AMR::microorganisms$fullname
  escaped[is_taxon] <- paste0("<i>", escaped[is_taxon], "</i>")
  escaped
}

#' @rdname episodic_ui_italicise_taxon
#' @param detectors A character vector of detector names.
#' @param sep Separator between entries.
#' @export
episodic_ui_code_join <- function(detectors, sep = ", ") {
  escaped <- gsub("&", "&amp;", detectors, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  paste(sprintf("<code>%s</code>", escaped), collapse = sep)
}

#' A vertical list of colour-coded buttons standing in for a `<select>`
#'
#' Used for the classification and mute-reason pickers on the assessment
#' form: a labelled, coloured button per option, filled once selected, so
#' the option and its meaning are both visible at once.
#'
#' @param input_id The id of the hidden input the selected value is
#'   written to.
#' @param options A list of `list(value, label, colour, hint = NULL)`.
#' @param selected The initially-selected value, or `NULL`/`""` for none.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_picker <- function(input_id, options, selected = NULL) {
  selected <- selected %||% ""
  shiny::tags$div(
    class = "episodic-picker",
    shiny::tags$input(type = "hidden", id = input_id, value = selected),
    lapply(options, function(opt) {
      active <- identical(opt$value, selected) && nzchar(selected)
      shiny::tags$button(
        type = "button",
        class = if (active) {
          "episodic-picker-btn active"
        } else {
          "episodic-picker-btn"
        },
        style = if (active) {
          sprintf("background:%s;border-color:%s;", opt$colour, opt$colour)
        } else {
          ""
        },
        `data-value` = opt$value,
        `data-colour` = opt$colour,
        `data-input` = input_id,
        onclick = sprintf(
          "document.getElementById('%s').value=this.dataset.value; document.querySelectorAll('[data-input=\"%s\"]').forEach(function(b){b.classList.remove('active');b.style.background='';b.style.borderColor='';}); this.classList.add('active'); this.style.background=this.dataset.colour; this.style.borderColor=this.dataset.colour;",
          input_id,
          input_id
        ),
        opt$label,
        if (!is.null(opt$hint)) {
          shiny::tags$div(class = "episodic-picker-hint", opt$hint)
        }
      )
    })
  )
}

#' A row of toggle chips for a multi-value filter (e.g. "L4 and L5 only")
#'
#' Unlike `episodic_ui_picker()`, more than one chip can be active at
#' once, and there is no client-side state to keep in sync: each chip's
#' `onclick` already carries the *next* full selection, computed here at
#' render time from `selected`, so a click just reports that set to
#' Shiny and lets the next render (server-driven, same as
#' `archive_search`) redraw every chip's active state correctly - no
#' JavaScript has to track which chips are on.
#'
#' @param input_id The Shiny input the comma-joined selection is written
#'   to (`""` when nothing is selected, i.e. no filter).
#' @param options A list of `list(value, label)`.
#' @param selected Character vector of currently-selected `value`s.
#' @param all_label Label for the leading "clear the filter" chip.
#' @param lang Session language, for the active-chip colour only.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_multi_picker <- function(
  input_id,
  options,
  selected = character(0),
  all_label = "All"
) {
  pal <- episodic_palette()
  active_style <- sprintf(
    "background:%s;border-color:%s;color:#fff;",
    pal$primary,
    pal$primary
  )
  all_active <- length(selected) == 0
  chips <- c(
    list(shiny::tags$button(
      type = "button",
      class = if (all_active) {
        "episodic-picker-btn active"
      } else {
        "episodic-picker-btn"
      },
      style = if (all_active) active_style else "",
      onclick = sprintf(
        "Shiny.setInputValue('%s', '', {priority: 'event'});",
        input_id
      ),
      all_label
    )),
    lapply(options, function(opt) {
      active <- opt$value %in% selected
      next_selected <- if (active) {
        setdiff(selected, opt$value)
      } else {
        c(selected, opt$value)
      }
      shiny::tags$button(
        type = "button",
        class = if (active) {
          "episodic-picker-btn active"
        } else {
          "episodic-picker-btn"
        },
        style = if (active) active_style else "",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
          input_id,
          paste(next_selected, collapse = ",")
        ),
        opt$label
      )
    })
  )
  shiny::tags$div(class = "episodic-picker episodic-picker-inline", chips)
}

#' @param text Chip text.
#' @param colour A hex colour.
#' @param filled If `TRUE`, filled background; otherwise an outline chip.
#' @keywords internal
#' @noRd
episodic_ui_chip <- function(text, colour, filled = FALSE) {
  style <- if (filled) {
    sprintf("color:#fff;background:%s;", colour)
  } else {
    sprintf("color:%s;border:1px solid %s66;", colour, colour)
  }
  class <- paste(
    "episodic-chip",
    if (filled) "episodic-chip-filled" else "episodic-chip-outline"
  )
  shiny::tags$span(class = class, style = style, text)
}

#' A chip that opens another cluster
#'
#' Same shape as `episodic_ui_chip()`, but it goes somewhere: used in the
#' dossier header to say that these cases are also in another cluster, and
#' to take the epidemiologist straight to it. Focusable and operable from the
#' keyboard, like the cluster rows elsewhere - a chip that only responds
#' to a mouse is a link a keyboard user cannot follow.
#'
#' @param text Chip label.
#' @param colour Chip colour, from [episodic_palette()].
#' @param cluster_id The cluster to open.
#' @param lang Session language.
#' @return A `shiny::tags$span`.
#' @keywords internal
#' @noRd
episodic_ui_chip_link <- function(
  text,
  colour,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  open_js <- sprintf(
    "Shiny.setInputValue('open_cluster', %d, {priority: 'event'});",
    as.integer(cluster_id)
  )
  shiny::tags$span(
    class = "episodic-chip episodic-chip-outline episodic-chip-link",
    style = sprintf("color:%s;border:1px solid %s66;", colour, colour),
    tabindex = "0",
    role = "link",
    title = episodic_tr("cluster.open_hint", lang = lang),
    onclick = open_js,
    onkeydown = sprintf(
      "if(event.key==='Enter'||event.key===' '){event.preventDefault();%s}",
      open_js
    ),
    text
  )
}

#' @param title Panel title.
#' @param aside Optional right-aligned header text.
#' @param note Optional footnote paragraph.
#' @param ... Panel body content.
#' @keywords internal
#' @noRd
episodic_ui_panel <- function(title, ..., aside = NULL, note = NULL) {
  shiny::tags$section(
    class = "episodic-panel",
    shiny::tags$div(
      class = "episodic-panel-header",
      shiny::tags$h2(class = "episodic-panel-title", title),
      if (!is.null(aside)) {
        shiny::tags$span(class = "episodic-panel-aside", aside)
      }
    ),
    shiny::tags$div(
      class = "episodic-panel-body",
      ...,
      if (!is.null(note)) shiny::tags$p(class = "episodic-panel-note", note)
    )
  )
}

#' @param message Empty-state text shown instead of a body.
#' @keywords internal
#' @noRd
episodic_ui_panel_empty <- function(title, message, aside = NULL) {
  episodic_ui_panel(
    title,
    aside = aside,
    shiny::tags$p(class = "episodic-panel-empty", message)
  )
}

#' @param label Stat label (uppercase caption).
#' @param value Stat value (large number/text).
#' @param sub Optional sub-label.
#' @param colour Optional value colour.
#' @keywords internal
#' @noRd
episodic_ui_stat <- function(label, value, sub = NULL, colour = NULL) {
  shiny::tags$div(
    shiny::tags$div(class = "episodic-stat-label", label),
    shiny::tags$div(
      class = "episodic-stat-value",
      style = if (!is.null(colour)) sprintf("color:%s;", colour),
      value
    ),
    if (!is.null(sub)) shiny::tags$div(class = "episodic-stat-sub", sub)
  )
}

#' A CSS `width`/`height` percentage, immune to `options(OutDec = ",")`
#'
#' `sprintf("%s", <double>)` coerces through `format()`, which honours
#' `OutDec` - so under a Dutch session (`OutDec = ","`) it renders
#' `"95,4545...\%"`, invalid CSS that browsers silently drop, leaving the
#' element at its default (usually full) width. `%f` conversions go
#' through C-level float formatting instead and never consult `OutDec`,
#' so this is a real fix rather than a locale workaround. Only for this
#' kind of machine-read value: numbers shown to the user should keep
#' their comma, which is why this isn't applied more broadly.
#' @keywords internal
#' @noRd
episodic_css_pct <- function(x) sprintf("%.4f%%", x)

#' @param rows A data frame with `label` and `n` columns.
#' @param unit Optional footnote under the bars.
#' @param colour Bar fill colour.
#' @keywords internal
#' @noRd
episodic_ui_bars <- function(rows, unit = NULL, colour = NULL) {
  if (nrow(rows) == 0) {
    return(shiny::tags$p(class = "episodic-panel-empty", "..."))
  }
  pal <- episodic_palette()
  colour <- colour %||% pal$primary
  max_n <- max(rows$n, 1)
  bars <- lapply(seq_len(nrow(rows)), function(i) {
    shiny::tags$div(
      class = "episodic-bar-row",
      shiny::tags$div(
        class = "episodic-bar-label",
        title = rows$label[i],
        rows$label[i]
      ),
      shiny::tags$div(
        class = "episodic-bar-track",
        shiny::tags$div(
          class = "episodic-bar-fill",
          style = sprintf(
            "width:%s;background:%s;",
            episodic_css_pct(100 * rows$n[i] / max_n),
            colour
          )
        )
      ),
      shiny::tags$div(class = "episodic-bar-value", rows$n[i])
    )
  })
  shiny::tagList(
    bars,
    if (!is.null(unit)) {
      shiny::tags$div(
        style = "font-size:11px;color:var(--episodic-faint);margin-top:6px;",
        unit
      )
    }
  )
}

#' @param demo A data frame with `band`, `m`, `v` (male/female counts), one
#'   row per age band in ascending order (youngest first).
#' @param lang Session language, for the axis labels.
#' @keywords internal
#' @noRd
episodic_ui_pyramid <- function(demo, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (nrow(demo) == 0 || sum(demo$m, demo$v) == 0) {
    return(shiny::tags$p(class = "episodic-panel-empty", "..."))
  }
  max_n <- max(c(demo$m, demo$v), 1)
  # Rendered oldest-band-first (top) to youngest-band-last (bottom), the
  # conventional age-sex pyramid orientation - `demo` itself stays
  # youngest-first, since that is the natural order for the underlying data.
  demo <- demo[rev(seq_len(nrow(demo))), ]
  rows <- lapply(seq_len(nrow(demo)), function(i) {
    shiny::tags$div(
      class = "episodic-pyramid-row",
      shiny::tags$div(
        class = "episodic-pyramid-side-left",
        shiny::tags$div(
          class = "episodic-pyramid-bar-m",
          style = sprintf(
            "width:%s;",
            episodic_css_pct(100 * demo$m[i] / max_n)
          )
        )
      ),
      shiny::tags$div(class = "episodic-pyramid-band", demo$band[i]),
      shiny::tags$div(
        class = "episodic-pyramid-side-right",
        shiny::tags$div(
          class = "episodic-pyramid-bar-f",
          style = sprintf(
            "width:%s;",
            episodic_css_pct(100 * demo$v[i] / max_n)
          )
        )
      )
    )
  })
  shiny::tagList(
    rows,
    shiny::tags$div(
      style = "display:flex;justify-content:space-between;font-size:11px;color:var(--episodic-muted);margin-top:4px;",
      shiny::tags$span(episodic_tr("panel.demography.male", lang = lang)),
      shiny::tags$span(episodic_tr("panel.demography.female", lang = lang))
    )
  )
}

#' A table row that opens a cluster's dossier
#'
#' Used by every table that lists clusters - the Pathogen screen's
#' clusters panel and the Archive - so the id column, the click target
#' and the keyboard handling stay identical between them rather than
#' being written twice and drifting.
#'
#' The whole row is the target and the id is the visible affordance: a
#' row is a generous thing to hit, but nothing about a bare table row
#' says it can be clicked. `tabindex` and the key handler are there
#' because a `<tr>` has no focus or activation behaviour of its own; the
#' server side is `input$open_cluster`, which sets the selection and
#' switches to the clusters view.
#'
#' @param cluster_id The cluster the row opens.
#' @param lang Session language.
#' @param ... The remaining cells, in order, after the id cell.
#' @return A `shiny::tags$tr`.
#' @keywords internal
#' @noRd
episodic_ui_cluster_link_row <- function(
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  ...
) {
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
      shiny::tags$span(
        class = "episodic-id-link",
        episodic_tr("dossier.cluster_ref", id = cluster_id, lang = lang)
      )
    ),
    ...
  )
}

#' @param state One of `episodic_derive_state()`'s state strings.
#' @keywords internal
#' @noRd
episodic_ui_state_dot <- function(state) {
  colour <- episodic_ui_state_colour(state)
  shiny::tags$span(
    class = "episodic-state-dot",
    style = sprintf("background:%s;", colour)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_state_colour <- function(state) {
  pal <- episodic_palette()
  switch(state,
    new = pal$primary_light,
    assessing = pal$primary,
    monitoring = pal$danger,
    closable = pal$warning_dark,
    closed = pal$success_dark,
    reassess = pal$tertiary_dark,
    pal$muted
  )
}

#' The rail care-line chip's colour
#'
#' Deliberately not primary/secondary/tertiary in line order: first line
#' takes primary since it is the one an epidemiologist sees most often,
#' second takes tertiary and third takes secondary. `"other"`/`"unknown"`
#' (and anything else) get `NULL` - no chip, rather than a chip that
#' says nothing.
#'
#' @param care_line A stream `care_line` value, or `NA`.
#' @return A hex colour, or `NULL`.
#' @keywords internal
#' @noRd
episodic_ui_care_line_colour <- function(care_line) {
  pal <- episodic_palette()
  switch(care_line,
    first = pal$primary,
    second = pal$tertiary,
    third = pal$secondary,
    NULL
  )
}

#' @param verdict One of `episodic_ui_assessment_form()`'s verdict keys
#'   (`"artefact"`, `"expected_variation"`, `"cluster_not_yet"`,
#'   `"possible_epidemic"`, `"confirmed_epidemic"`).
#' @keywords internal
#' @noRd
episodic_ui_verdict_colour <- function(verdict) {
  pal <- episodic_palette()
  switch(verdict,
    artefact = pal$muted,
    expected_variation = pal$muted,
    cluster_not_yet = pal$success_dark,
    possible_epidemic = pal$warning_dark,
    confirmed_epidemic = pal$danger,
    pal$muted
  )
}

#' Stream levels bounded and local enough to call a cluster an "outbreak"
#' rather than an "epidemic"
#'
#' A ward or an institution is a contained setting - the standard field
#' term for a confirmed cluster there is "outbreak" (a norovirus outbreak
#' on a ward, a Legionella outbreak tied to one care home), same as this
#' codebase's own commentary already says informally throughout
#' (`R/reconcile.R`, `R/run_cron.R`, ...). From area level up, a cluster
#' is a population-level statistical excess over baseline - what
#' "epidemic" means, and already the Moving Epidemic Method's own
#' vocabulary elsewhere on the same screens ("epidemic start/end
#' threshold"). `pathogen_area` is not the fuzzy middle case it looks
#' like: `episodic_case_region_code()` defines it as a multi-town
#' postcode district, not a single contained setting, so it sits with
#' province/region rather than with ward/institution.
#' @keywords internal
#' @noRd
episodic_verdict_outbreak_levels <- c("pathogen_ward", "pathogen_institution")

#' A verdict's display label, worded for the cluster's own scale
#'
#' The stored verdict value (`"possible_epidemic"`, ...) never changes -
#' `episodic_ui_verdict_colour()`, the state machine and the performance
#' stats all still key off it. Only the wording shown to a reader shifts:
#' `"cluster_not_yet"`/`"possible_epidemic"`/`"confirmed_epidemic"` read
#' as "... outbreak" rather than "... epidemic" when the cluster sits at
#' ward or institution level (see `episodic_verdict_outbreak_levels`).
#' `"artefact"`/`"expected_variation"` never mention either word, so
#' `level` makes no difference to them.
#'
#' @param verdict One of the five verdict keys, or `NA`.
#' @param level The cluster's stream level (e.g. `"pathogen_ward"`), or
#'   `NULL` when no single level applies - a bulk action across a mixed
#'   selection, or a system-wide distribution summed over every level.
#'   Both deliberately keep the general "epidemic" wording rather than
#'   guessing a level that is not actually singular.
#' @param lang Session language.
#' @return A single string, or `NA_character_` if `verdict` is `NA`.
#' @keywords internal
#' @noRd
episodic_verdict_label <- function(
  verdict,
  level = NULL,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (is.na(verdict)) {
    return(NA_character_)
  }
  key <- paste0("verdict.", verdict)
  has_outbreak_variant <- verdict %in%
    c(
      "cluster_not_yet",
      "possible_epidemic",
      "confirmed_epidemic"
    )
  if (
    has_outbreak_variant &&
      !is.null(level) &&
      !is.na(level) &&
      level %in% episodic_verdict_outbreak_levels
  ) {
    key <- paste0(key, ".outbreak")
  }
  episodic_tr(key, lang = lang)
}
