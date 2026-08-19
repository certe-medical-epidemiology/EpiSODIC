#' Small reusable UI building blocks
#'
#' Thin `shiny::tags` wrappers mirroring `episode-mockup.jsx`'s primitive
#' components (`Chip`, `Panel`, `Stat`, `Bars`, `Pyramid`): same structure
#' and Dutch wording, none of the React idioms (MILESTONES.md M2). Styling
#' lives in `inst/app/www/episode.css`; these functions only assign class
#' names and content.
#' @name app_widgets
NULL

#' Italicise pathogen names `AMR` recognises as a taxonomic binomial
#'
#' Wraps names found in `AMR::microorganisms$fullname` in `<i>...</i>`, for
#' display via [shiny::HTML()] (e.g. *Escherichia coli*); names `AMR`
#' does not recognise as a species (e.g. "Influenza A", a virus type
#' rather than a binomial) pass through unitalicised, exactly as intended -
#' `pathogen` is deliberately unconstrained free text (`QUESTIONS.md`
#' item 22c: `AMR` was dropped from detection so viruses aren't excluded).
#' `AMR` is Suggests-only (Certe-internal-adjacent but CRAN, still an
#' optional dependency for this display-only nicety); a no-op when it is
#' not installed. Text is HTML-escaped before any tag is added, so this is
#' always safe to pass to [shiny::HTML()].
#'
#' @param pathogen A character vector of pathogen display names.
#' @return A character vector, safe to pass to [shiny::HTML()].
#' @keywords internal
#' @noRd
episode_ui_italicise_taxon <- function(pathogen) {
  escaped <- gsub("&", "&amp;", pathogen, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  if (!requireNamespace("AMR", quietly = TRUE)) return(escaped)
  is_taxon <- pathogen %in% AMR::microorganisms$fullname
  escaped[is_taxon] <- paste0("<i>", escaped[is_taxon], "</i>")
  escaped
}

#' Render a list of detector names as inline code, joined by a separator
#'
#' A detector name (`farrington`, `same_place`, `rare_trigger`, ...) is an
#' identifier from the codebase, not prose - rendered in `<code>` so it
#' reads as one, wherever it appears in a sentence or table (dossier meta
#' line, settings panel, timeline). Text is HTML-escaped before any tag is
#' added, so the result is always safe to pass to [shiny::HTML()].
#'
#' @param detectors A character vector of detector names.
#' @param sep Separator between entries.
#' @return A single character string, safe to pass to [shiny::HTML()].
#' @keywords internal
#' @noRd
episode_ui_code_join <- function(detectors, sep = ", ") {
  escaped <- gsub("&", "&amp;", detectors, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  paste(sprintf("<code>%s</code>", escaped), collapse = sep)
}

#' A vertical list of colour-coded buttons standing in for a `<select>`
#'
#' Used for the classification and mute-reason pickers on the assessment
#' form (`episode_ui_assessment_form()`) - matching `episode-mockup.jsx`'s
#' own design (a labelled, coloured button per option, filled once
#' selected) rather than a plain dropdown, so the option and its meaning
#' are both visible at once instead of hidden behind a click.
#'
#' Selection is plain inline `onclick` JS (consistent with the rest of this
#' app's forms, e.g. `episode_ui_nav_link()`), writing into a hidden
#' `input_id` element that the submit button's own onclick reads - no new
#' JS file, no new dependency.
#'
#' @param input_id The id of the hidden input the selected value is
#'   written to.
#' @param options A list of `list(value, label, colour, hint = NULL)`.
#' @param selected The initially-selected value, or `NULL`/`""` for none.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episode_ui_picker <- function(input_id, options, selected = NULL) {
  selected <- selected %||% ""
  shiny::tags$div(
    class = "episode-picker",
    shiny::tags$input(type = "hidden", id = input_id, value = selected),
    lapply(options, function(opt) {
      active <- identical(opt$value, selected) && nzchar(selected)
      shiny::tags$button(
        type = "button",
        class = if (active) "episode-picker-btn active" else "episode-picker-btn",
        style = if (active) sprintf("background:%s;border-color:%s;", opt$colour, opt$colour) else "",
        `data-value` = opt$value, `data-colour` = opt$colour, `data-input` = input_id,
        onclick = sprintf(
          "document.getElementById('%s').value=this.dataset.value; document.querySelectorAll('[data-input=\"%s\"]').forEach(function(b){b.classList.remove('active');b.style.background='';b.style.borderColor='';}); this.classList.add('active'); this.style.background=this.dataset.colour; this.style.borderColor=this.dataset.colour;",
          input_id, input_id
        ),
        opt$label,
        if (!is.null(opt$hint)) shiny::tags$div(class = "episode-picker-hint", opt$hint)
      )
    })
  )
}

#' @rdname app_widgets
#' @param text Chip text.
#' @param colour A hex colour.
#' @param filled If `TRUE`, filled background; otherwise an outline chip.
#' @export
episode_ui_chip <- function(text, colour, filled = FALSE) {
  style <- if (filled) {
    sprintf("color:#fff;background:%s;", colour)
  } else {
    sprintf("color:%s;border:1px solid %s66;", colour, colour)
  }
  class <- paste("episode-chip", if (filled) "episode-chip-filled" else "episode-chip-outline")
  shiny::tags$span(class = class, style = style, text)
}

#' @rdname app_widgets
#' @param title Panel title.
#' @param aside Optional right-aligned header text.
#' @param note Optional footnote paragraph.
#' @param ... Panel body content.
#' @export
episode_ui_panel <- function(title, ..., aside = NULL, note = NULL) {
  shiny::tags$section(
    class = "episode-panel",
    shiny::tags$div(
      class = "episode-panel-header",
      shiny::tags$h2(class = "episode-panel-title", title),
      if (!is.null(aside)) shiny::tags$span(class = "episode-panel-aside", aside)
    ),
    shiny::tags$div(
      class = "episode-panel-body",
      ...,
      if (!is.null(note)) shiny::tags$p(class = "episode-panel-note", note)
    )
  )
}

#' @rdname app_widgets
#' @param message Empty-state text shown instead of a body.
#' @export
episode_ui_panel_empty <- function(title, message, aside = NULL) {
  episode_ui_panel(title, aside = aside, shiny::tags$p(class = "episode-panel-empty", message))
}

#' @rdname app_widgets
#' @param label Stat label (uppercase caption).
#' @param value Stat value (large number/text).
#' @param sub Optional sub-label.
#' @param colour Optional value colour.
#' @export
episode_ui_stat <- function(label, value, sub = NULL, colour = NULL) {
  shiny::tags$div(
    shiny::tags$div(class = "episode-stat-label", label),
    shiny::tags$div(class = "episode-stat-value", style = if (!is.null(colour)) sprintf("color:%s;", colour), value),
    if (!is.null(sub)) shiny::tags$div(class = "episode-stat-sub", sub)
  )
}

#' @rdname app_widgets
#' @param rows A data frame with `label` and `n` columns.
#' @param unit Optional footnote under the bars.
#' @param colour Bar fill colour.
#' @export
episode_ui_bars <- function(rows, unit = NULL, colour = NULL) {
  if (nrow(rows) == 0) return(shiny::tags$p(class = "episode-panel-empty", "..."))
  pal <- episode_palette()
  colour <- colour %||% pal$primary
  max_n <- max(rows$n, 1)
  bars <- lapply(seq_len(nrow(rows)), function(i) {
    shiny::tags$div(
      class = "episode-bar-row",
      shiny::tags$div(class = "episode-bar-label", rows$label[i]),
      shiny::tags$div(
        class = "episode-bar-track",
        shiny::tags$div(class = "episode-bar-fill",
                         style = sprintf("width:%s%%;background:%s;", 100 * rows$n[i] / max_n, colour))
      ),
      shiny::tags$div(class = "episode-bar-value", rows$n[i])
    )
  })
  shiny::tagList(bars, if (!is.null(unit)) shiny::tags$div(style = "font-size:11px;color:var(--episode-faint);margin-top:6px;", unit))
}

#' @rdname app_widgets
#' @param demo A data frame with `band`, `m`, `v` (male/female counts).
#' @param lang Session language, for the axis labels.
#' @export
episode_ui_pyramid <- function(demo, lang = "nl") {
  if (nrow(demo) == 0 || sum(demo$m, demo$v) == 0) {
    return(shiny::tags$p(class = "episode-panel-empty", "..."))
  }
  max_n <- max(c(demo$m, demo$v), 1)
  rows <- lapply(seq_len(nrow(demo)), function(i) {
    shiny::tags$div(
      class = "episode-pyramid-row",
      shiny::tags$div(class = "episode-pyramid-side-left",
                       shiny::tags$div(class = "episode-pyramid-bar-m",
                                        style = sprintf("width:%s%%;", 100 * demo$m[i] / max_n))),
      shiny::tags$div(class = "episode-pyramid-band", demo$band[i]),
      shiny::tags$div(class = "episode-pyramid-side-right",
                       shiny::tags$div(class = "episode-pyramid-bar-f",
                                        style = sprintf("width:%s%%;", 100 * demo$v[i] / max_n)))
    )
  })
  shiny::tagList(
    rows,
    shiny::tags$div(
      style = "display:flex;justify-content:space-between;font-size:11px;color:var(--episode-muted);margin-top:4px;",
      shiny::tags$span(episode_tr("panel.demography.male", lang = lang)),
      shiny::tags$span(episode_tr("panel.demography.female", lang = lang))
    )
  )
}

#' @rdname app_widgets
#' @param state One of [episode_derive_state()]'s state strings.
#' @export
episode_ui_state_dot <- function(state) {
  colour <- episode_ui_state_colour(state)
  shiny::tags$span(class = "episode-state-dot", style = sprintf("background:%s;", colour))
}

#' @rdname app_widgets
#' @export
episode_ui_state_colour <- function(state) {
  pal <- episode_palette()
  switch(state,
    new = pal$primary, assessing = pal$primary, monitoring = pal$danger,
    closable = pal$warning_dark, closed = pal$success_dark, reassess = pal$tertiary_dark,
    pal$muted
  )
}

#' @rdname app_widgets
#' @param verdict One of `episode_ui_assessment_form()`'s verdict keys
#'   (`"artefact"`, `"expected_variation"`, `"cluster_not_yet"`,
#'   `"possible_epidemic"`, `"confirmed_epidemic"`).
#' @export
episode_ui_verdict_colour <- function(verdict) {
  pal <- episode_palette()
  switch(verdict,
    artefact = pal$muted, expected_variation = pal$muted,
    cluster_not_yet = pal$success_dark, possible_epidemic = pal$warning_dark,
    confirmed_epidemic = pal$danger,
    pal$muted
  )
}
