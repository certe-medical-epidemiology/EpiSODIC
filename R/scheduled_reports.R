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

#' The email channels a scheduled report can be sent through
#'
#' Deliberately a subset of `notifications.channels`: ntfy, Teams and
#' Slack take a topic/webhook, not a list of colleagues' addresses, so
#' they are not offered here regardless of whether they are enabled.
#' @keywords internal
#' @noRd
episodic_report_subscription_email_channels <- c("smtp", "sendmail", "microsoft365")

#' Which configured channels a new schedule may be pointed at
#' @param config The resolved configuration (including `notifications`).
#' @return A character vector of enabled, email-capable channel names -
#'   possibly empty, if none are configured.
#' @keywords internal
#' @noRd
episodic_report_subscription_available_channels <- function(config) {
  channels <- config$notifications$channels
  if (is.null(channels)) {
    return(character(0))
  }
  Filter(
    function(name) isTRUE(channels[[name]]$enabled),
    episodic_report_subscription_email_channels
  )
}

#' Whether a cluster's schedule should end after one final report
#'
#' A cluster a schedule points at always exists (the event carries a
#' foreign key to it), but it may since have closed, merged into another
#' cluster, or been suppressed behind one - each of those ends the
#' schedule, after one last report, rather than continuing to update
#' colleagues about a cluster that is no longer live. Pure and
#' side-effect-free, taking one already-fetched row rather than a
#' connection, so this is unit-testable without a database.
#' @param cluster_row One row from `episodic_db_clusters()`, with a
#'   `state` column attached (see `episodic_app_derive_states_batch()`).
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_scheduled_report_terminal <- function(cluster_row) {
  isTRUE(cluster_row$state[1] == "closed") ||
    !is.na(cluster_row$merged_into[1]) ||
    !is.na(cluster_row$suppressed_by[1])
}

#' Send every scheduled report that is due, after a cron run commits
#'
#' Called by [episodic_run_cron()] once the detection transaction has
#' committed and `episodic_notify()` has run, on the same "never block
#' the run" footing: one subscription's failure (a misconfigured
#' channel, an unreachable SMTP relay) is logged and skipped, never
#' allowed to stop another cluster's report from going out or to fail
#' the cron run itself. See [episodic_scheduled_reports] for the cadence
#' and closure rules this applies.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param config The resolved configuration, with the same
#'   `notifications` overlay `episodic_notify()` was called with (so a
#'   Settings-screen channel change reaches this without a redeploy).
#' @param run_id The current run ID, recorded on every send it makes.
#' @param run_date The run date, for judging cadence.
#' @param db_path Path to the database (or MariaDB DSN) - passed to
#'   `episodic_report_output_dir()` to resolve the `reports/` directory
#'   renders are written to, exactly like the dossier's on-demand render
#'   button does.
#' @param episodic_config_path Passed through to
#'   [episodic_report_render()] unchanged.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_scheduled_reports_dispatch <- function(con,
                                                config,
                                                run_id,
                                                run_date,
                                                db_path,
                                                episodic_config_path) {
  events_all <- episodic_db_report_subscription_events_all(con)
  current <- episodic_report_subscription_current_all(events_all)
  if (length(current) == 0) {
    return(invisible(NULL))
  }

  sends_all <- episodic_db_report_subscription_sends_all(con)
  cluster_ids <- vapply(current, function(x) x$cluster_id, integer(1))

  clusters <- episodic_db_clusters(con, include_suppressed = TRUE)
  clusters <- clusters[clusters$cluster_id %in% cluster_ids, , drop = FALSE]
  streams <- episodic_db_streams(con, active_only = FALSE)
  clusters$pathogen <- streams$pathogen[match(clusters$stream_id, streams$stream_id)]
  clusters$level <- streams$level[match(clusters$stream_id, streams$stream_id)]
  clusters$state <- episodic_app_derive_states_batch(con, clusters)

  for (subscription in current) {
    cluster_row <- clusters[clusters$cluster_id == subscription$cluster_id, ]
    terminal <- episodic_scheduled_report_terminal(cluster_row)

    already_finalised <- nrow(sends_all[
      sends_all$subscription_event_id == subscription$event_id &
        sends_all$final == 1, ,
      drop = FALSE
    ]) > 0

    if (terminal && already_finalised) {
      next
    }

    due <- if (terminal) {
      TRUE
    } else {
      episodic_report_subscription_due(subscription, sends_all, run_date)
    }
    if (!due) {
      next
    }

    tryCatch(
      episodic_scheduled_report_send_one(
        con,
        config,
        subscription,
        run_id = run_id,
        db_path = db_path,
        episodic_config_path = episodic_config_path,
        final = terminal
      ),
      error = function(e) {
        # episodic_scheduled_report_send_one() already catches and logs
        # every failure it can attribute to a specific step (rendering,
        # sending) as a 'failed' send row; this is the backstop for
        # anything that slipped past that - it must never propagate and
        # take the whole cron run down with it.
        episodic_trace(
          "Scheduled report dispatch for cluster ",
          subscription$cluster_id,
          " failed unexpectedly: ",
          conditionMessage(e)
        )
      }
    )
  }

  invisible(NULL)
}

#' Render and send one scheduled report, logging the attempt either way
#' @keywords internal
#' @noRd
episodic_scheduled_report_send_one <- function(con,
                                               config,
                                               subscription,
                                               run_id,
                                               db_path,
                                               episodic_config_path,
                                               final) {
  lang <- episodic_lang()
  result <- tryCatch(
    {
      channel_name <- subscription$channel
      channel_cfg <- config$notifications$channels[[channel_name]]
      if (is.null(channel_cfg) || !isTRUE(channel_cfg$enabled)) {
        stop(
          "Notification channel '",
          channel_name,
          "' is no longer enabled - cannot deliver this scheduled report.",
          call. = FALSE
        )
      }

      rendered <- episodic_report_render(
        con,
        cluster_id = subscription$cluster_id,
        output_dir = episodic_report_output_dir(config, db_path, "reports"),
        user_id = NA,
        include_linelist = subscription$include_linelist,
        episodic_config_path = episodic_config_path,
        lang = lang
      )

      message <- episodic_scheduled_report_message(
        con,
        subscription,
        final = final,
        attachment_path = rendered$file_path,
        lang = lang
      )
      channel_override <- channel_cfg
      channel_override$to <- subscription$recipients

      channel_fn <- switch(channel_name,
        smtp = episodic_notify_smtp,
        sendmail = episodic_notify_sendmail,
        microsoft365 = episodic_notify_microsoft365
      )
      episodic_trace(
        "Sending scheduled report for cluster ",
        subscription$cluster_id,
        " via ",
        channel_name,
        " to ",
        length(subscription$recipients),
        " recipient(s)"
      )
      channel_fn(channel_override, message)
      list(status = "sent", report_id = rendered$report_id, error_text = NA)
    },
    error = function(e) {
      episodic_trace(
        "Scheduled report for cluster ",
        subscription$cluster_id,
        " failed: ",
        conditionMessage(e)
      )
      list(status = "failed", report_id = NA, error_text = conditionMessage(e))
    }
  )

  episodic_db_report_subscription_send_insert(
    con,
    cluster_id = subscription$cluster_id,
    subscription_event_id = subscription$event_id,
    run_id = run_id,
    report_id = result$report_id,
    recipients_json = episodic_report_subscription_recipients_to_json(subscription$recipients),
    status = result$status,
    error_text = result$error_text,
    final = final
  )
  invisible(NULL)
}

#' Build the email a scheduled report is sent with
#'
#' Short and self-describing on purpose: the content colleagues actually
#' need is the attached report, not a second copy of it retyped into the
#' email body. What the body does add is the one thing the attachment
#' cannot show on its own - that this is a recurring, automated update,
#' at what cadence, and (the "banger" bit - see
#' `episodic_report_diff()`) a one-line summary of what changed since
#' the previous one, so a reader can tell at a glance whether opening the
#' attachment is urgent.
#' @param con A [DBI::DBIConnection-class].
#' @param subscription One entry from
#'   `episodic_report_subscription_current_all()`.
#' @param final Whether this is the closure/suppression send that ends
#'   the schedule.
#' @param attachment_path Path to the rendered report HTML.
#' @param lang Language for the email text.
#' @return A list with `title`, `html` and `attachment_path` - the shape
#'   every `episodic_notify_*()` channel function expects.
#' @keywords internal
#' @noRd
episodic_scheduled_report_message <- function(con, subscription, final, attachment_path, lang) {
  details <- episodic_notify_cluster_details(con, subscription$cluster_id)[1, ]
  location <- episodic_notify_location(details, lang = lang)
  period_str <- episodic_format_date_range(
    details$first_day,
    details$last_day,
    lang = lang
  )
  ref <- episodic_tr("dossier.cluster_ref", id = subscription$cluster_id, lang = lang)

  title <- episodic_tr(
    "scheduled_report.email_subject",
    ref = ref,
    pathogen = details$pathogen,
    location = location,
    lang = lang
  )

  lines <- character(0)
  lines <- c(lines, paste0(
    "<p>",
    episodic_tr(
      "scheduled_report.email_intro",
      pathogen = episodic_html_escape(details$pathogen),
      location = episodic_html_escape(location),
      period = episodic_html_escape(period_str),
      lang = lang
    ),
    "</p>"
  ))

  reports_so_far <- episodic_db_reports_for_cluster(con, subscription$cluster_id)
  latest_report <- reports_so_far[which.max(reports_so_far$version_no), ]
  params <- tryCatch(
    jsonlite::fromJSON(latest_report$params),
    error = function(e) NULL
  )
  diff <- params$diff
  if (!is.null(diff) && isTRUE(diff$n_new_cases > 0)) {
    lines <- c(lines, paste0(
      "<p>",
      episodic_tr(
        "scheduled_report.email_new_cases",
        cases_phrase = episodic_count_phrase(
          diff$n_new_cases,
          episodic_tr("unit.new_case", lang = lang),
          episodic_tr("unit.new_cases", lang = lang)
        ),
        lang = lang
      ),
      "</p>"
    ))
  }

  if (isTRUE(final)) {
    lines <- c(lines, paste0(
      "<p><strong>",
      episodic_tr("scheduled_report.email_final", lang = lang),
      "</strong></p>"
    ))
  } else {
    lines <- c(lines, paste0(
      "<p style='color:#666'>",
      episodic_tr(
        "scheduled_report.email_recurrence",
        interval = episodic_count_phrase(
          subscription$interval_days,
          episodic_tr("unit.day", lang = lang),
          episodic_tr("unit.days", lang = lang)
        ),
        lang = lang
      ),
      "</p>"
    ))
  }

  list(
    title = title,
    html = episodic_notify_html_wrap(
      title,
      paste(lines, collapse = "\n"),
      dashboard_url = NULL,
      lang = lang
    ),
    attachment_path = attachment_path
  )
}
