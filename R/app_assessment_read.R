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

# Read models: assessment timeline, archive, activity. Cheap reads only,
# same as R/app_read.R.

#' The assessment rail's timeline ("Verloop")
#'
#' Every classification, closure and mute affecting this cluster, in one
#' chronological feed - "assessments rendered as an append-only timeline,
#' never overwritten".
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param lang Session language.
#' @return A data frame ordered by `at` ascending, columns `at`, `actor`
#'   (a `full_name`, or the interpretation label for a system actor),
#'   `kind` (`"assessment"` or `"closure"`), `verdict` (the raw key, `NA`
#'   unless `kind == "assessment"` and a verdict was set - for colour
#'   lookups via `episodic_ui_verdict_colour()`, where the translated
#'   `verdict_label` cannot be used back as a key), `verdict_label`
#'   (translated), `rationale`.
#' @keywords internal
#' @noRd
episodic_app_assessment_timeline <- function(
  con,
  cluster_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  level = NA
) {
  events <- episodic_db_assessment_events(con, cluster_id)
  states <- episodic_db_cluster_states(con, cluster_id)
  closures <- states[states$trigger == "closure", ]

  rows <- list()
  if (nrow(events) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = events$created_at,
      actor = vapply(
        events$user_id,
        episodic_app_actor_label,
        character(1),
        con = con,
        lang = lang
      ),
      kind = "assessment",
      verdict = events$verdict,
      verdict_label = vapply(
        events$verdict,
        episodic_verdict_label,
        character(1),
        level = level,
        lang = lang
      ),
      rationale = events$rationale,
      stringsAsFactors = FALSE
    )
  }
  if (nrow(closures) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = closures$entered_at,
      actor = vapply(
        closures$user_id,
        episodic_app_actor_label,
        character(1),
        con = con,
        lang = lang
      ),
      kind = "closure",
      verdict = NA_character_,
      verdict_label = NA_character_,
      rationale = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(
      at = character(0),
      actor = character(0),
      kind = character(0),
      verdict = character(0),
      verdict_label = character(0),
      rationale = character(0),
      stringsAsFactors = FALSE
    ))
  }
  timeline <- do.call(rbind, rows)
  timeline[order(timeline$at), ]
}

#' A user_id (possibly `NA`, meaning the system) as a display label
#' @keywords internal
#' @noRd
episodic_app_actor_label <- function(
  con,
  user_id,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (is.na(user_id)) {
    return(episodic_tr("activity.actor_system", lang = lang))
  }
  user <- episodic_db_user_by_id(con, user_id)
  if (is.null(user)) {
    return(episodic_tr("misc.unknown", lang = lang))
  }
  user$full_name
}

#' The Archive screen: closed clusters, searchable
#'
#' Last winter's assessment is the best prior for this winter's cluster.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param query Free-text search over pathogen and place (case-insensitive
#'   substring), or `NULL`/`""` for no filter.
#' @param level A character vector of stream levels to include (e.g.
#'   `c("pathogen_province", "pathogen_region")`), or `NULL`/empty for
#'   every level.
#' @param lang Session language.
#' @return A data frame, one row per closed cluster, in
#'   `episodic_cluster_table_order()` - most recent last case day first.
#' @keywords internal
#' @noRd
episodic_app_archive <- function(
  con,
  query = NULL,
  level = NULL,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  empty <- data.frame(
    cluster_id = integer(0),
    pathogen = character(0),
    level = character(0),
    level_label = character(0),
    place = character(0),
    first_day = character(0),
    last_day = character(0),
    duration_days = integer(0),
    n_cases = integer(0),
    case_days = integer(0),
    priority_score = numeric(0),
    closed_at = character(0),
    stringsAsFactors = FALSE
  )
  clusters <- episodic_db_clusters(con, open_only = TRUE)
  if (nrow(clusters) == 0) {
    return(empty)
  }
  streams <- episodic_db_streams(con, active_only = FALSE)
  institutions <- DBI::dbGetQuery(con, "SELECT * FROM episodic_institution")

  # Derived for the whole archive in three queries rather than five per
  # cluster. `episodic_app_derive_state_for_cluster()` is the readable form
  # but it re-reads the cluster, its stream, the pathogen config, its events
  # and its states one row at a time; on a networked database that is what
  # made opening the archive take tens of seconds. The batch form needs
  # `pathogen` alongside `last_day` and `changed_since_assessment`, so the
  # stream join moves up here from below.
  clusters$pathogen <- streams$pathogen[match(
    clusters$stream_id,
    streams$stream_id
  )]
  clusters$state <- episodic_app_derive_states_batch(con, clusters)
  closed <- clusters[clusters$state == "closed", ]
  if (nrow(closed) == 0) {
    return(empty)
  }

  closed$level <- streams$level[match(closed$stream_id, streams$stream_id)]
  closed$care_line <- streams$care_line[match(
    closed$stream_id,
    streams$stream_id
  )]
  closed$level_label <- vapply(
    seq_len(nrow(closed)),
    function(i) {
      episodic_app_level_label(
        closed$level[i],
        closed$care_line[i],
        lang = lang
      )
    },
    character(1)
  )
  closed$place <- vapply(
    seq_len(nrow(closed)),
    function(i) {
      stream <- streams[streams$stream_id == closed$stream_id[i], ][1, ]
      institution <- if (!is.na(stream$institution_id)) {
        institutions[institutions$institution_id == stream$institution_id, ][
          1,
        ]
      } else {
        NULL
      }
      episodic_app_place_label(stream, institution, lang = lang)
    },
    character(1)
  )
  closed$closed_at <- episodic_app_closed_at_from(
    episodic_db_cluster_states_batch(con, closed$cluster_id),
    closed$cluster_id
  )

  if (!is.null(query) && nzchar(query)) {
    hit <- grepl(query, closed$pathogen, ignore.case = TRUE) |
      grepl(query, closed$place, ignore.case = TRUE)
    closed <- closed[hit, ]
  }
  if (!is.null(level) && length(level) > 0) {
    closed <- closed[closed$level %in% level, ]
  }
  if (nrow(closed) == 0) {
    return(empty)
  }

  closed$duration_days <- episodic_cluster_duration_days(
    closed$first_day,
    closed$last_day
  )
  closed <- episodic_db_attach_case_days(con, closed)

  # Sorted the way every cluster table in the app is sorted - most recent
  # last case day first - rather than on closed_at: an operator scanning
  # the archive reads it as a timeline of outbreaks, and wants the most
  # recently active on top regardless of the order clusters happened to
  # be closed in. `episodic_ui_cluster_table()` applies the same order
  # again when it renders; doing it here as well means any other caller
  # gets the rows in the order the screen shows them.
  closed <- closed[episodic_cluster_table_order(closed), ]
  closed[, c(
    "cluster_id",
    "pathogen",
    "level",
    "level_label",
    "place",
    "first_day",
    "last_day",
    "duration_days",
    "n_cases",
    "case_days",
    "priority_score",
    "closed_at"
  )]
}

#' What a detection run actually took in, as one line
#'
#' A run that says only "succeeded" cannot answer the question an
#' operator actually has the morning after: did last night's extract
#' arrive in full? This renders the load counters recorded against the
#' run, and names skipped rows explicitly - that is the number a green
#' run must never be able to hide.
#'
#' @param run One row of `episodic_db_runs()`.
#' @param lang Session language.
#' @return A single string, or `NA_character_` for a run recorded before
#'   the counters existed, or one that failed before loading anything.
#' @keywords internal
#' @noRd
episodic_app_run_detail <- function(
  run,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  # A failed run has no load summary, but it does have a reason, and this
  # screen is where somebody goes to find out why the dashboard is empty.
  # Its first line only, since a validation failure names every offending
  # column and would otherwise fill the table; the whole message is one
  # click away, in the run detail. Untranslated either way - it is the
  # operator's own error text.
  if (identical(run$status, "failed")) {
    return(episodic_ui_first_line(run$error_text) %||% NA_character_)
  }

  episodic_app_run_load_summary(run, lang = lang)
}

#' What a run's feeds delivered, as one line
#'
#' Split from `episodic_app_run_detail()` so the run detail modal can show
#' the load summary and the failure reason as two different things, rather
#' than one standing in for the other.
#'
#' @param run One row of `episodic_detection_run`.
#' @param lang Session language.
#' @return A single string, or `NA_character_` when the run recorded no
#'   counts at all.
#' @keywords internal
#' @noRd
episodic_app_run_load_summary <- function(
  run,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  # NULL when reading a database written before the counters existed;
  # NA when the run failed before it loaded anything. Neither has a
  # summary to show, and neither should read as "zero cases arrived".
  if (is.null(run$n_cases_supplied) || is.na(run$n_cases_supplied)) {
    return(NA_character_)
  }

  detail <- episodic_tr(
    "activity.detail_run",
    inserted = run$n_cases_inserted,
    supplied = run$n_cases_supplied,
    deduplicated = run$n_cases_deduplicated,
    lang = lang
  )
  if (isTRUE(run$n_activity_skipped > 0)) {
    detail <- paste(
      detail,
      episodic_tr(
        "activity.detail_run_skipped",
        skipped = run$n_activity_skipped,
        lang = lang
      ),
      sep = " \u00b7 "
    )
  }
  detail
}

#' The Activity screen: every recorded action, with system runs visually distinct
#'
#' Name, timestamp, action, target, with system-authored runs visually
#' distinct from human actions.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param limit Maximum number of rows to return, most recent first.
#' @param lang Session language.
#' @return A data frame with `at`, `actor`, `action`, `target`, `detail`,
#'   `is_system` and `run_id`. `detail` is the run load summary on run
#'   rows (or, for a failed run, why it failed), and `NA` on human ones;
#'   `run_id` is filled on run rows only, so the screen can offer the
#'   full run detail for those.
#' @keywords internal
#' @noRd
episodic_app_activity_log <- function(
  con,
  limit = 200,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  # Including suppressed ones: this is the record of what happened, and a
  # cluster somebody acted on has to keep resolving to its own name here
  # however the lattice later decided to file it.
  clusters <- episodic_db_clusters(con, include_suppressed = TRUE)
  streams <- episodic_db_streams(con, active_only = FALSE)
  cluster_target <- function(cluster_id) {
    stream_id <- clusters$stream_id[clusters$cluster_id == cluster_id]
    pathogen <- streams$pathogen[streams$stream_id == stream_id][1]
    episodic_tr(
      "activity.target_cluster",
      pathogen = pathogen %||% "?",
      id = cluster_id,
      lang = lang
    )
  }

  rows <- list()

  events <- DBI::dbGetQuery(con, "SELECT * FROM episodic_assessment_event")
  if (nrow(events) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = events$created_at,
      actor = vapply(
        events$user_id,
        episodic_app_actor_label,
        character(1),
        con = con,
        lang = lang
      ),
      action = ifelse(
        is.na(events$verdict),
        episodic_tr("activity.action_note", lang = lang),
        episodic_tr("activity.action_classified", lang = lang)
      ),
      target = vapply(events$cluster_id, cluster_target, character(1)),
      detail = NA_character_,
      is_system = FALSE,
      run_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  states <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_cluster_state WHERE `trigger` = 'closure'"
  )
  if (nrow(states) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = states$entered_at,
      actor = vapply(
        states$user_id,
        episodic_app_actor_label,
        character(1),
        con = con,
        lang = lang
      ),
      action = episodic_tr("activity.action_closed", lang = lang),
      target = vapply(states$cluster_id, cluster_target, character(1)),
      detail = NA_character_,
      is_system = FALSE,
      run_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  mutes <- DBI::dbGetQuery(con, "SELECT * FROM episodic_stream_mute")
  if (nrow(mutes) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = mutes$created_at,
      actor = vapply(
        mutes$user_id,
        episodic_app_actor_label,
        character(1),
        con = con,
        lang = lang
      ),
      action = episodic_tr("activity.action_muted", lang = lang),
      target = vapply(
        mutes$stream_id,
        function(sid) {
          pathogen <- streams$pathogen[streams$stream_id == sid][1]
          pathogen %||% "?"
        },
        character(1)
      ),
      detail = NA_character_,
      is_system = FALSE,
      run_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  logins <- DBI::dbGetQuery(
    con,
    "SELECT * FROM episodic_app_user_event WHERE event_type = 'login'"
  )
  if (nrow(logins) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = logins$created_at,
      actor = vapply(
        logins$user_id,
        episodic_app_actor_label,
        character(1),
        con = con,
        lang = lang
      ),
      action = episodic_tr("activity.action_login", lang = lang),
      target = NA_character_,
      detail = NA_character_,
      is_system = FALSE,
      run_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  runs <- episodic_db_runs(con, limit = limit)
  if (nrow(runs) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      at = ifelse(is.na(runs$finished_at), runs$started_at, runs$finished_at),
      actor = episodic_tr("activity.actor_system", lang = lang),
      action = vapply(
        runs$status,
        function(s) episodic_tr(paste0("activity.action_run_", s), lang = lang),
        character(1)
      ),
      target = runs$host,
      detail = vapply(
        seq_len(nrow(runs)),
        function(i) episodic_app_run_detail(runs[i, ], lang),
        character(1)
      ),
      is_system = TRUE,
      run_id = runs$run_id,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(
      at = character(0),
      actor = character(0),
      action = character(0),
      target = character(0),
      detail = character(0),
      is_system = logical(0),
      run_id = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  activity <- do.call(rbind, rows)
  activity <- activity[order(activity$at, decreasing = TRUE), ]
  utils::head(activity, limit)
}
