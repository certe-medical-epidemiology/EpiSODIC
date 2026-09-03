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

# What the instance's own reference files actually delivered.
#
# An operator points EPISODIC_PC_PROVINCE_MAP at a CSV, opens the
# dashboard, and sees postcodes with no province beside them. Nothing on
# any screen tells them whether the file was read, whether it was read
# and rejected, or whether it was read fine and simply matches none of
# their postcodes - three different problems with three different fixes,
# and no way to tell them apart from the outside. Same for the map
# geometry, the overlay, and every other EPISODIC_* file.
#
# This is that answer, per variable: is it configured, was it usable, and
# what did it actually put into the app. Resolved live at render time
# rather than recorded at startup, so it reflects what the app would use
# right now.

#' The status a reference file resolved to
#'
#' Five outcomes, deliberately distinct: `"in_use"` (the operator's own
#' file, read and used), `"default"` (nothing configured, so the shipped
#' default stands in), `"unset"` (nothing configured and there is no
#' default - the feature is simply off), `"problem"` (configured and
#' unusable, which is the case that must never look like any of the
#' others), and `"unavailable"` (configured or not, an optional package
#' the feature needs is missing).
#' @keywords internal
#' @noRd
episodic_reference_statuses <- c(
  "in_use",
  "default",
  "unset",
  "problem",
  "unavailable"
)

#' @param status One of `episodic_reference_statuses`.
#' @keywords internal
#' @noRd
episodic_reference_status_colour <- function(status) {
  pal <- episodic_palette()
  switch(status,
    in_use = pal$success_dark,
    default = pal$muted,
    unset = pal$muted,
    problem = pal$danger,
    unavailable = pal$warning_dark,
    pal$muted
  )
}

#' One row of the reference-data table
#' @keywords internal
#' @noRd
episodic_reference_row <- function(variable, status, detail, path = NA) {
  list(
    variable = variable,
    status = status,
    detail = detail,
    # Shown only to a signed-in reader: the Info screen is public, and a
    # filesystem path says more about the machine than a visitor needs.
    path = if (length(path) == 1 && !is.na(path) && nzchar(path)) {
      path
    } else {
      NA_character_
    }
  )
}

#' What every `EPISODIC_*` reference file delivered to this app
#'
#' @param con A [DBI::DBIConnection-class], or `NULL` to skip the checks
#'   that need case data (the postcode-coverage line).
#' @param lang Session language.
#' @return A list of rows from `episodic_reference_row()`.
#' @keywords internal
#' @noRd
episodic_app_reference_data <- function(
    con = NULL,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  tr <- function(key, ...) episodic_tr(key, ..., lang = lang)
  env <- function(name) {
    value <- Sys.getenv(name, unset = NA)
    if (length(value) != 1 || is.na(value) || !nzchar(value)) NA_character_ else value
  }

  c(
    list(episodic_app_reference_pc_province(con, lang = lang)),
    list(episodic_app_reference_geo(lang = lang)),
    list(episodic_app_reference_geo_overlay(lang = lang)),
    list(episodic_reference_row(
      "EPISODIC_LANGUAGE",
      if (is.na(env("EPISODIC_LANGUAGE"))) "default" else "in_use",
      tr("info.reference.language", language = episodic_lang(lang))
    )),
    list(episodic_reference_row(
      "EPISODIC_CONFIG",
      if (is.na(env("EPISODIC_CONFIG"))) "default" else "in_use",
      tr(if (is.na(env("EPISODIC_CONFIG"))) {
        "info.reference.file.default"
      } else {
        "info.reference.file.custom"
      }),
      path = env("EPISODIC_CONFIG")
    )),
    list(episodic_reference_row(
      "EPISODIC_PALETTE_CONFIG",
      if (is.na(env("EPISODIC_PALETTE_CONFIG"))) "default" else "in_use",
      tr(if (is.na(env("EPISODIC_PALETTE_CONFIG"))) {
        "info.reference.file.default"
      } else {
        "info.reference.file.custom"
      }),
      path = env("EPISODIC_PALETTE_CONFIG")
    )),
    list(episodic_reference_row(
      "EPISODIC_QUARTO_REPORT",
      if (is.na(env("EPISODIC_QUARTO_REPORT"))) "default" else "in_use",
      tr(if (is.na(env("EPISODIC_QUARTO_REPORT"))) {
        "info.reference.file.default"
      } else {
        "info.reference.file.custom"
      }),
      path = env("EPISODIC_QUARTO_REPORT")
    ))
  )
}

#' The postcode-to-province mapping's own row
#'
#' The one that prompted this panel. Three separable failures, and the
#' row says which: not configured at all (the shipped demo ranges stand
#' in, and outside the northern Netherlands they match nothing);
#' configured but unusable (`episodic_pc_province_map_problem()` says
#' exactly why); or configured, usable, and matching none of the
#' postcodes actually in the case data - a formatting mismatch
#' (`"9713"` against `"9713 AB"`), which looks identical from the
#' outside to the mapping never having been read at all.
#' @keywords internal
#' @noRd
episodic_app_reference_pc_province <- function(
    con = NULL,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  tr <- function(key, ...) episodic_tr(key, ..., lang = lang)
  path <- Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)
  problem <- episodic_pc_province_map_problem(path)
  if (!is.na(problem)) {
    return(episodic_reference_row(
      "EPISODIC_PC_PROVINCE_MAP",
      "problem",
      problem,
      path = path
    ))
  }

  mapping <- episodic_pc_province_map_resolve(path)
  detail <- if (is.null(mapping)) {
    tr("info.reference.pc_province.default")
  } else {
    provinces <- unique(mapping[!is.na(mapping)])
    tr(
      "info.reference.pc_province.summary",
      entries = length(mapping),
      provinces = length(provinces),
      examples = paste(utils::head(sort(provinces), 4), collapse = ", ")
    )
  }

  # The number that actually answers "is my CSV being used": how many of
  # the postcodes in this database's own cases resolve to a province.
  coverage <- episodic_app_reference_pc_coverage(con)
  if (!is.null(coverage) && coverage$total > 0) {
    detail <- paste(
      detail,
      tr(
        if (coverage$matched == 0) {
          "info.reference.pc_province.coverage_none"
        } else {
          "info.reference.pc_province.coverage"
        },
        matched = coverage$matched,
        total = coverage$total
      )
    )
  }

  episodic_reference_row(
    "EPISODIC_PC_PROVINCE_MAP",
    if (is.null(mapping)) "default" else "in_use",
    detail,
    path = path
  )
}

#' How many of the case data's own postcodes resolve to a province
#'
#' Distinct values, not rows: the question is whether the mapping covers
#' the postcodes this laboratory actually reports, and one postcode
#' carrying a thousand cases should not drown out a hundred that carry
#' one each.
#'
#' @param con A [DBI::DBIConnection-class], or `NULL`.
#' @return A list with `matched` and `total`, or `NULL` when there is no
#'   connection or no case data to measure against.
#' @keywords internal
#' @noRd
episodic_app_reference_pc_coverage <- function(con) {
  if (is.null(con)) {
    return(NULL)
  }
  pc <- tryCatch(
    DBI::dbGetQuery(
      con,
      "SELECT DISTINCT pc FROM episodic_case WHERE pc IS NOT NULL"
    )$pc,
    error = function(e) NULL
  )
  if (is.null(pc) || length(pc) == 0) {
    return(NULL)
  }
  resolved <- episodic_pc_to_province(pc)
  list(matched = sum(!is.na(resolved)), total = length(pc))
}

#' The choropleth geometry's own row
#' @keywords internal
#' @noRd
episodic_app_reference_geo <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  tr <- function(key, ...) episodic_tr(key, ..., lang = lang)
  path <- Sys.getenv("EPISODIC_GEO_DATA", unset = NA)
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(episodic_reference_row(
      "EPISODIC_GEO_DATA",
      "unavailable",
      tr("info.reference.geo.no_sf"),
      path = path
    ))
  }
  # A file that is set but rejected warns rather than throwing (see
  # `episodic_geo_source_resolve()`), and that warning is the whole
  # explanation for a map an operator expected to be theirs and is not.
  warned <- NULL
  geo <- withCallingHandlers(
    episodic_geo_source_resolve(),
    warning = function(w) {
      warned <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  if (!is.na(path) && nzchar(path) && !is.null(warned)) {
    return(episodic_reference_row(
      "EPISODIC_GEO_DATA",
      "problem",
      warned,
      path = path
    ))
  }
  if (is.null(geo)) {
    return(episodic_reference_row(
      "EPISODIC_GEO_DATA",
      "unavailable",
      tr("info.reference.geo.no_sf"),
      path = path
    ))
  }
  used_own <- !is.na(path) && nzchar(path) && file.exists(path)
  episodic_reference_row(
    "EPISODIC_GEO_DATA",
    if (used_own) "in_use" else "default",
    tr("info.reference.areas", n = nrow(geo)),
    path = path
  )
}

#' The optional region-outline overlay's own row
#' @keywords internal
#' @noRd
episodic_app_reference_geo_overlay <- function(
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  tr <- function(key, ...) episodic_tr(key, ..., lang = lang)
  path <- Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
  if (is.na(path) || !nzchar(path)) {
    return(episodic_reference_row(
      "EPISODIC_GEO_DATA_OVERLAY",
      "unset",
      tr("info.reference.overlay.unset")
    ))
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(episodic_reference_row(
      "EPISODIC_GEO_DATA_OVERLAY",
      "unavailable",
      tr("info.reference.geo.no_sf"),
      path = path
    ))
  }
  overlay <- episodic_geo_overlay_resolve(path)
  if (is.null(overlay)) {
    # No default exists for this one, so a configured file that does not
    # resolve means the layer is simply absent - which on screen looks
    # exactly like not having configured one.
    return(episodic_reference_row(
      "EPISODIC_GEO_DATA_OVERLAY",
      "problem",
      tr("info.reference.overlay.unusable"),
      path = path
    ))
  }
  episodic_reference_row(
    "EPISODIC_GEO_DATA_OVERLAY",
    "in_use",
    tr("info.reference.shapes", n = nrow(overlay)),
    path = path
  )
}

#' The Info screen's reference-data panel
#'
#' @param con A [DBI::DBIConnection-class], or `NULL`.
#' @param current_user The session's signed-in user row, or `NULL`. Only
#'   the resolved file paths are gated on it; every status and count is
#'   public, since those are what say whether the dashboard is showing
#'   what the operator configured.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_info_reference_panel <- function(
    con = NULL,
    current_user = NULL,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  rows <- episodic_app_reference_data(con, lang = lang)
  episodic_ui_panel(
    episodic_tr("info.reference.title", lang = lang),
    note = episodic_tr("info.reference.note", lang = lang),
    shiny::tags$table(
      class = "episodic-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(episodic_tr(
          "info.reference.col.variable",
          lang = lang
        )),
        shiny::tags$th(episodic_tr("info.reference.col.status", lang = lang)),
        shiny::tags$th(episodic_tr(
          "info.reference.col.delivered",
          lang = lang
        ))
      )),
      shiny::tags$tbody(lapply(rows, function(row) {
        shiny::tags$tr(
          shiny::tags$td(shiny::HTML(episodic_ui_code_join(row$variable))),
          shiny::tags$td(episodic_ui_chip(
            episodic_tr(
              paste0("info.reference.status.", row$status),
              lang = lang
            ),
            episodic_reference_status_colour(row$status),
            filled = TRUE
          )),
          shiny::tags$td(
            row$detail,
            if (!is.null(current_user) && !is.na(row$path)) {
              shiny::tags$div(
                class = "episodic-reference-path",
                episodic_tr(
                  "info.reference.path",
                  path = row$path,
                  lang = lang
                )
              )
            }
          )
        )
      }))
    )
  )
}
