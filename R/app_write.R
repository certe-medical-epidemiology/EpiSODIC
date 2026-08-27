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

# App write orchestration: R/db_app_write.R exposes one insert per
# table. The functions here combine those into the actual actions a
# signed-in epidemiologist takes, adding the one piece of bookkeeping a raw
# insert cannot do on its own: recording every state transition. Still
# insert-only throughout - nothing here issues UPDATE or DELETE.

#' Submit a classification
#'
#' Inserts the assessment event, then re-derives state; if it changed,
#' appends an `episodic_cluster_state` row (`trigger = "assessment"`)
#' recording the transition and which event caused it.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param user_id The signed-in epidemiologist's `user_id`.
#' @param verdict One of the five classification values, or `NA` (a
#'   rationale-only note with no classification yet).
#' @param rationale Optional free-text rationale; `NA` or `""` records no
#'   rationale rather than blocking the assessment.
#' @param wpg_notifiable,ggd_informed Logical or `NA`.
#' @param ggd_note Free text, or `NA`.
#' @param snooze_until A date, or `NA`.
#' @param supersedes An earlier `event_id` this event supersedes, or `NA`.
#' @return Invisibly, the new `event_id`.
#' @keywords internal
#' @noRd
episodic_app_submit_assessment <- function(
  con,
  cluster_id,
  user_id,
  verdict = NA,
  rationale = "",
  wpg_notifiable = NA,
  ggd_informed = NA,
  ggd_note = NA,
  snooze_until = NA,
  supersedes = NA
) {
  state_before <- episodic_app_derive_state_for_cluster(con, cluster_id)

  event_id <- episodic_db_assessment_event_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    verdict = verdict,
    rationale = rationale,
    wpg_notifiable = wpg_notifiable,
    ggd_informed = ggd_informed,
    ggd_note = ggd_note,
    snooze_until = snooze_until,
    supersedes = supersedes
  )

  state_after <- episodic_app_derive_state_for_cluster(con, cluster_id)
  if (!identical(state_before, state_after)) {
    episodic_db_cluster_state_insert(
      con,
      cluster_id = cluster_id,
      state = state_after,
      trigger = "assessment",
      event_id = event_id,
      user_id = user_id
    )
  }

  invisible(event_id)
}

#' Explicitly close a cluster
#'
#' Closure is an act, not a classification:
#' it needs no new rationale, since the classification that is being
#' closed already carries its own. Always available on a non-terminal
#' classification, whether or not the closure criterion has fired.
#'
#' @inheritParams episodic_app_submit_assessment
#' @return Invisibly, the new `episodic_cluster_state` row's `state_id`.
#' @keywords internal
#' @noRd
episodic_app_submit_closure <- function(con, cluster_id, user_id) {
  invisible(episodic_db_cluster_state_insert(
    con,
    cluster_id = cluster_id,
    state = "closed",
    trigger = "closure",
    user_id = user_id
  ))
}
