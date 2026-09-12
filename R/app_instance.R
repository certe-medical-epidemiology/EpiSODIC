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

#' The Instance screen's own data
#'
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language.
#' @return A list of the counts and the schema version the cards show.
#' @keywords internal
#' @noRd
episodic_app_instance <- function(con, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  list(
    counts = episodic_db_instance_counts(con),
    schema_version = episodic_schema_version
  )
}

#' The five screens that describe the instance rather than a cluster
#'
#' The navigation bar carries the three screens an epidemiologist moves
#' between while assessing a signal. These five are about the system:
#' what it watches, what it did, how well it detects, what it is, and how
#' it is configured. They are read occasionally rather than daily, and a
#' flat bar of eight peers over items of such different frequency is what
#' makes a navigation impossible to fit on a narrow screen without
#' hiding some of it.
#'
#' So they are a screen rather than a menu. Reaching one is an ordinary
#' view change through the same mechanism every other screen uses: no
#' collapsed state to get stuck open, no second interaction to learn, and
#' nothing that has to be dismissed before the page can be read.
#'
#' @param instance The list from `episodic_app_instance()`.
#' @param current_user The session's signed-in user row, or `NULL`. The
#'   Settings card is omitted for anyone who is not an admin, the same
#'   gate `episodic_app_server_settings()` re-checks server-side before
#'   it renders that screen at all.
#' @param lang Session language.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_instance_screen <- function(instance,
                                        current_user = NULL,
                                        lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  counts <- instance$counts
  cards <- list(
    list(
      view = "streams",
      meta = episodic_count_phrase(
        counts$streams,
        episodic_tr("unit.stream", lang = lang),
        episodic_tr("unit.streams", lang = lang),
        lang = lang
      )
    ),
    list(
      view = "activity",
      meta = episodic_count_phrase(
        counts$runs,
        episodic_tr("unit.run", lang = lang),
        episodic_tr("unit.runs", lang = lang),
        lang = lang
      )
    ),
    # No number: see `episodic_db_instance_counts()` for why this one
    # card deliberately says nothing about how well the instance detects.
    list(view = "performance", meta = NULL),
    list(
      view = "info",
      # A schema version is an identifier, not a quantity, so it is
      # written as it is rather than grouped.
      meta = episodic_tr(
        "instance.schema_version",
        version = as.character(instance$schema_version),
        lang = lang
      )
    )
  )
  if (isTRUE(episodic_user_is_admin(current_user))) {
    cards <- c(
      cards,
      list(list(
        view = "settings",
        meta = episodic_count_phrase(
          counts$users,
          episodic_tr("unit.account", lang = lang),
          episodic_tr("unit.accounts", lang = lang),
          lang = lang
        )
      ))
    )
  }

  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(
      class = "episodic-screen-title",
      episodic_tr("nav.instance", lang = lang)
    ),
    shiny::tags$p(
      class = "episodic-screen-lead",
      episodic_tr("instance.lead", lang = lang)
    ),
    shiny::tags$div(
      class = "episodic-instance-cards",
      lapply(cards, function(card) {
        episodic_ui_instance_card(
          card$view,
          title = episodic_tr(paste0("nav.", card$view), lang = lang),
          description = episodic_tr(
            paste0("instance.card.", card$view),
            lang = lang
          ),
          meta = card$meta
        )
      })
    )
  )
}

#' One card on the Instance screen
#'
#' A link, not a button: it goes to a screen. It carries the same two
#' attributes every other thing that navigates carries, so
#' `episodic-nav.js`'s one delegated listener handles it with no code of
#' its own, and `data-nav` keeps the Instance link in the bar lit once
#' the reader is on the screen the card opened.
#'
#' @param view The view id the card opens.
#' @param title The screen's name, as the navigation would have said it.
#' @param description One line on what the screen is for.
#' @param meta An optional short count, or `NULL` for a card that
#'   deliberately carries no number.
#' @return A `shiny::tags$a`.
#' @keywords internal
#' @noRd
episodic_ui_instance_card <- function(view, title, description, meta = NULL) {
  shiny::tags$a(
    href = "#",
    class = "episodic-instance-card",
    `data-episodic-nav` = view,
    `data-nav` = episodic_app_nav_group(view),
    shiny::tags$span(class = "episodic-instance-card-title", title),
    shiny::tags$span(class = "episodic-instance-card-desc", description),
    if (!is.null(meta)) {
      shiny::tags$span(class = "episodic-instance-card-meta", meta)
    }
  )
}

#' Which cluster the phone-tier pane switcher's segments refer to
#'
#' Built from the server's own selection rather than copied out of the
#' rail: a cluster opened from the Pathogen screen, from a `?cluster=`
#' link or from the open-by-number box is often not a row in the rail at
#' all, and a label read off a row that is not there is blank - which
#' says "nothing is open" about a cluster that is.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The cluster currently open.
#' @param lang Session language.
#' @return A `shiny::tagList`, or `NULL` when the id names no cluster.
#' @keywords internal
#' @noRd
episodic_ui_pane_label <- function(con,
                                   cluster_id,
                                   lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  label <- episodic_db_cluster_label(con, cluster_id)
  if (is.null(label)) {
    return(NULL)
  }
  shiny::tagList(
    shiny::HTML(episodic_ui_italicise_taxon(label$pathogen[1])),
    " ",
    shiny::tags$span(
      class = "episodic-rail-id",
      episodic_tr("dossier.cluster_ref", id = cluster_id, lang = lang)
    )
  )
}
