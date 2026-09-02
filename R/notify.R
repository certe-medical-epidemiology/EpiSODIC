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

#' How notifications work
#'
#' When [episodic_run_cron()] detects new clusters or fails, it can
#' notify your team through one or more channels: ntfy, SMTP, sendmail,
#' Microsoft 365, Teams (Power Automate Workflow webhook), or Slack.
#' Notifications are configured in the instance YAML configuration file
#' (the same file `EPISODIC_CONFIG` points at), under a `notifications`
#' key. The notification settings are deliberately excluded from the
#' detection `config_hash`, so changing them never alters reproducibility
#' and secrets never reach the `config_snapshot` stored on each run.
#'
#' Notifications are dispatched *after* the detection transaction has
#' committed: a failed notification never rolls back a successful run.
#' Errors during dispatch are logged via `episodic_trace()` and never
#' stop the run from finishing.
#'
#' @name episodic_notifications
NULL

#' Dispatch notifications after a cron run
#'
#' Called by [episodic_run_cron()] after the transaction commits. Checks
#' which triggers fired (new clusters, run failure), builds the message,
#' and sends it through every enabled channel.
#'
#' @param con A [DBI::DBIConnection-class], for querying cluster details.
#' @param config The resolved configuration (including `notifications`).
#' @param result The result list from `episodic_run_cron_body()`.
#' @param run_id The current run ID.
#' @param run_date The run date.
#' @param host The host name, for failure messages.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_notify <- function(con, config, result, run_id, run_date, host) {
  notif <- config$notifications
  if (is.null(notif) || !isTRUE(notif$enabled)) {
    return(invisible(NULL))
  }

  triggers <- notif$triggers
  channels <- notif$channels
  if (is.null(channels)) {
    return(invisible(NULL))
  }

  status <- result$status %||% "success"
  n_new <- result$n_signals_new %||% 0L
  new_cluster_ids <- result$new_cluster_ids %||% integer(0)

  trigger_new <- isTRUE(triggers$new_clusters) &&
    !identical(status, "failed") &&
    n_new > 0
  trigger_failure <- isTRUE(triggers$run_failure) &&
    identical(status, "failed")

  if (!trigger_new && !trigger_failure) {
    return(invisible(NULL))
  }

  dashboard_url <- notif$dashboard_url
  lang <- episodic_lang()

  if (trigger_new) {
    cluster_details <- episodic_notify_cluster_details(con, new_cluster_ids)
    message <- episodic_notify_build_new_clusters(
      cluster_details,
      n_new,
      run_date,
      dashboard_url,
      lang = lang
    )
    episodic_notify_dispatch(channels, message)
  }

  if (trigger_failure) {
    message <- episodic_notify_build_failure(
      result$error_text,
      run_date,
      host,
      lang = lang
    )
    episodic_notify_dispatch(channels, message)
  }

  invisible(NULL)
}

#' Fetch cluster + stream details for newly created clusters
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_ids Integer vector of cluster IDs.
#' @return A data frame with cluster and stream columns joined.
#' @keywords internal
#' @noRd
episodic_notify_cluster_details <- function(con, cluster_ids) {
  if (length(cluster_ids) == 0) {
    return(data.frame(
      cluster_id = integer(0),
      pathogen = character(0),
      level = character(0),
      institution_name = character(0),
      ward = character(0),
      region_code = character(0),
      n_cases = integer(0),
      expected = numeric(0),
      excess = numeric(0),
      ratio = numeric(0),
      priority_score = numeric(0),
      detector_agreement = integer(0),
      first_day = character(0),
      last_day = character(0),
      stringsAsFactors = FALSE
    ))
  }
  placeholders <- paste(rep("?", length(cluster_ids)), collapse = ", ")
  sql <- paste0(
    "SELECT c.cluster_id, s.pathogen, s.level,",
    " COALESCE(i.display_name, '') AS institution_name,",
    " s.ward, s.region_code,",
    " c.n_cases, c.expected, c.excess, c.ratio,",
    " c.priority_score, c.detector_agreement,",
    " c.first_day, c.last_day",
    " FROM episodic_cluster c",
    " INNER JOIN episodic_stream s ON s.stream_id = c.stream_id",
    " LEFT JOIN episodic_institution i ON i.institution_id = s.institution_id",
    " WHERE c.cluster_id IN (",
    placeholders,
    ")",
    " ORDER BY c.priority_score DESC"
  )
  params <- as.list(as.integer(cluster_ids))
  DBI::dbGetQuery(con, sql, params = params)
}

#' Describe where a cluster is, from stream fields
#' @keywords internal
#' @noRd
episodic_notify_location <- function(row, lang = episodic_lang()) {
  parts <- character(0)
  if (nzchar(row$institution_name %||% "")) {
    parts <- c(parts, row$institution_name)
  }
  if (!is.na(row$ward) && nzchar(row$ward)) {
    parts <- c(parts, row$ward)
  }
  if (!is.na(row$region_code) && nzchar(row$region_code)) {
    parts <- c(parts, row$region_code)
  }
  level_key <- paste0("notif.level.", row$level)
  level_label <- episodic_tr(level_key, lang = lang)
  if (identical(level_label, paste0("[[", level_key, "]]"))) {
    level_label <- row$level
  }
  if (length(parts) == 0) {
    return(level_label)
  }
  paste0(paste(parts, collapse = ", "), " (", level_label, ")")
}

#' Build a notification message for new clusters
#'
#' @param details Data frame from `episodic_notify_cluster_details()`.
#' @param n_new Total number of new signals.
#' @param run_date The run date.
#' @param dashboard_url Optional dashboard URL for deep links.
#' @param lang Language to render the message in. Defaults to
#'   `EPISODIC_LANGUAGE`, falling back to `"en"`, the same as the
#'   dashboard.
#' @return A list with `title`, `plain`, `html`, `teams_card`, `slack_blocks`.
#' @keywords internal
#' @noRd
episodic_notify_build_new_clusters <- function(
    details,
    n_new,
    run_date,
    dashboard_url = NULL,
    lang = episodic_lang()) {
  count_phrase <- episodic_count_phrase(
    n_new,
    episodic_tr("notif.new_cluster.singular", lang = lang),
    episodic_tr("notif.new_cluster.plural", lang = lang)
  )
  title <- episodic_tr(
    "notif.title_new_clusters",
    count_phrase = count_phrase,
    date = run_date,
    lang = lang
  )

  max_detail <- 10L
  show <- details[seq_len(min(nrow(details), max_detail)), , drop = FALSE]
  remainder <- max(0L, nrow(details) - max_detail)

  plain_lines <- character(0)
  html_rows <- character(0)
  teams_facts <- list()
  slack_lines <- character(0)

  for (i in seq_len(nrow(show))) {
    row <- show[i, ]
    location <- episodic_notify_location(row, lang = lang)
    expected_str <- if (is.na(row$expected)) "n/a" else round(row$expected, 1)
    ratio_str <- if (is.na(row$ratio)) "n/a" else round(row$ratio, 1)
    priority_str <- round(row$priority_score, 0)
    period_str <- episodic_format_date_range(
      row$first_day,
      row$last_day,
      lang = lang
    )
    line <- episodic_tr(
      "notif.cluster_line",
      pathogen = row$pathogen,
      location = location,
      cases = row$n_cases,
      expected = expected_str,
      ratio = ratio_str,
      priority = priority_str,
      period = period_str,
      lang = lang
    )
    summary_str <- episodic_tr(
      "notif.cluster_summary",
      cases = row$n_cases,
      expected = expected_str,
      priority = priority_str,
      lang = lang
    )
    plain_lines <- c(plain_lines, paste0("  - ", line))
    html_rows <- c(
      html_rows,
      paste0(
        "<tr>",
        "<td>",
        episodic_html_escape(row$pathogen),
        "</td>",
        "<td>",
        episodic_html_escape(location),
        "</td>",
        "<td style='text-align:center'>",
        row$n_cases,
        "</td>",
        "<td style='text-align:center'>",
        expected_str,
        "</td>",
        "<td style='text-align:center'>",
        ratio_str,
        "</td>",
        "<td style='text-align:center'>",
        priority_str,
        "</td>",
        "<td style='text-align:center'>",
        episodic_html_escape(period_str),
        "</td>",
        "</tr>"
      )
    )
    teams_facts <- c(
      teams_facts,
      list(list(
        title = paste0(row$pathogen, " \u00b7 ", location),
        value = summary_str
      ))
    )
    slack_lines <- c(
      slack_lines,
      paste0("*", row$pathogen, "* \u00b7 ", location, ": ", summary_str)
    )
  }

  if (remainder > 0) {
    more <- episodic_tr("notif.and_more", n = remainder, lang = lang)
    plain_lines <- c(plain_lines, paste0("  ", more))
    html_rows <- c(
      html_rows,
      paste0(
        "<tr><td colspan='7' style='font-style:italic'>",
        episodic_html_escape(more),
        "</td></tr>"
      )
    )
    slack_lines <- c(slack_lines, paste0("_", more, "_"))
  }

  plain <- paste(c(title, "", plain_lines), collapse = "\n")
  if (!is.null(dashboard_url) && nzchar(dashboard_url)) {
    plain <- paste0(plain, "\n\nDashboard: ", dashboard_url)
  }

  html <- episodic_notify_html_wrap(
    title,
    paste0(
      "<table style='border-collapse:collapse;width:100%'>",
      "<tr style='background:#f0f0f0'>",
      "<th style='text-align:left;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.pathogen", lang = lang)),
      "</th>",
      "<th style='text-align:left;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.location", lang = lang)),
      "</th>",
      "<th style='text-align:center;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.cases", lang = lang)),
      "</th>",
      "<th style='text-align:center;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.expected", lang = lang)),
      "</th>",
      "<th style='text-align:center;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.ratio", lang = lang)),
      "</th>",
      "<th style='text-align:center;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.priority", lang = lang)),
      "</th>",
      "<th style='text-align:center;padding:4px'>",
      episodic_html_escape(episodic_tr("notif.table.period", lang = lang)),
      "</th>",
      "</tr>",
      paste(html_rows, collapse = "\n"),
      "</table>"
    ),
    dashboard_url,
    lang = lang
  )

  teams_card <- episodic_notify_teams_card(
    title,
    teams_facts,
    dashboard_url,
    lang = lang
  )
  slack_text <- paste(
    c(paste0("*", title, "*"), "", slack_lines),
    collapse = "\n"
  )
  if (!is.null(dashboard_url) && nzchar(dashboard_url)) {
    slack_text <- paste0(
      slack_text,
      "\n\n<",
      dashboard_url,
      "|",
      episodic_tr("notif.open_dashboard", lang = lang),
      ">"
    )
  }

  list(
    title = title,
    plain = plain,
    html = html,
    teams_card = teams_card,
    slack_text = slack_text
  )
}

#' Build a notification message for a run failure
#'
#' @param error_text The recorded error text, or `NULL`.
#' @param run_date The run date.
#' @param host The host name.
#' @param lang Language to render the message in. Defaults to
#'   `EPISODIC_LANGUAGE`, falling back to `"en"`, the same as the
#'   dashboard.
#' @keywords internal
#' @noRd
episodic_notify_build_failure <- function(
    error_text,
    run_date,
    host,
    lang = episodic_lang()) {
  title <- episodic_tr("notif.title_failed", lang = lang)
  label_date <- episodic_tr("notif.label.date", lang = lang)
  label_host <- episodic_tr("notif.label.host", lang = lang)
  label_error <- episodic_tr("notif.label.error", lang = lang)
  error_str <- error_text %||% episodic_tr("notif.no_error_text", lang = lang)

  plain <- paste0(
    title,
    "\n\n",
    label_date,
    ": ",
    run_date,
    "\n",
    label_host,
    ": ",
    host,
    "\n",
    label_error,
    ": ",
    error_str
  )
  html <- episodic_notify_html_wrap(
    title,
    paste0(
      "<p><strong>", episodic_html_escape(label_date), ":</strong> ",
      run_date,
      "</p>",
      "<p><strong>", episodic_html_escape(label_host), ":</strong> ",
      episodic_html_escape(host),
      "</p>",
      "<p><strong>", episodic_html_escape(label_error), ":</strong> ",
      episodic_html_escape(error_str),
      "</p>"
    ),
    NULL,
    lang = lang
  )
  teams_card <- episodic_notify_teams_card(
    title,
    list(
      list(title = label_date, value = as.character(run_date)),
      list(title = label_host, value = host),
      list(title = label_error, value = error_str)
    ),
    NULL,
    lang = lang
  )
  slack_text <- paste0(
    "*",
    title,
    "*\n\n",
    label_date,
    ": ",
    run_date,
    "\n",
    label_host,
    ": ",
    host,
    "\n",
    label_error,
    ": `",
    error_str,
    "`"
  )

  list(
    title = title,
    plain = plain,
    html = html,
    teams_card = teams_card,
    slack_text = slack_text
  )
}

#' Escape HTML special characters
#' @keywords internal
#' @noRd
episodic_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Wrap HTML notification body in a minimal document
#' @keywords internal
#' @noRd
episodic_notify_html_wrap <- function(
    title,
    body_html,
    dashboard_url,
    lang = episodic_lang()) {
  footer <- ""
  if (!is.null(dashboard_url) && nzchar(dashboard_url)) {
    footer <- paste0(
      "<p style='margin-top:16px'><a href='",
      episodic_html_escape(dashboard_url),
      "'>",
      episodic_html_escape(episodic_tr("notif.open_dashboard", lang = lang)),
      "</a></p>"
    )
  }
  paste0(
    "<html><body style='font-family:sans-serif;font-size:14px'>",
    "<h2 style='margin:0 0 12px'>",
    episodic_html_escape(title),
    "</h2>",
    body_html,
    footer,
    "</body></html>"
  )
}

#' Build a Teams Adaptive Card JSON structure
#' @keywords internal
#' @noRd
episodic_notify_teams_card <- function(
    title,
    facts,
    dashboard_url,
    lang = episodic_lang()) {
  body <- list(
    list(
      type = "TextBlock",
      size = "Medium",
      weight = "Bolder",
      text = title
    ),
    list(
      type = "FactSet",
      facts = facts
    )
  )

  actions <- list()
  if (!is.null(dashboard_url) && nzchar(dashboard_url)) {
    actions <- list(list(
      type = "Action.OpenUrl",
      title = episodic_tr("notif.open_dashboard", lang = lang),
      url = dashboard_url
    ))
  }

  card <- list(
    type = "message",
    attachments = list(list(
      contentType = "application/vnd.microsoft.card.adaptive",
      content = list(
        `$schema` = "http://adaptivecards.io/schemas/adaptive-card.json",
        type = "AdaptiveCard",
        version = "1.4",
        body = body,
        actions = actions
      )
    ))
  )
  jsonlite::toJSON(card, auto_unbox = TRUE, null = "null")
}

#' Dispatch a message through all enabled channels
#' @keywords internal
#' @noRd
episodic_notify_dispatch <- function(channels, message) {
  channel_fns <- list(
    ntfy = episodic_notify_ntfy,
    smtp = episodic_notify_smtp,
    sendmail = episodic_notify_sendmail,
    microsoft365 = episodic_notify_microsoft365,
    teams = episodic_notify_teams,
    slack = episodic_notify_slack
  )

  for (name in names(channel_fns)) {
    ch <- channels[[name]]
    if (is.null(ch) || !isTRUE(ch$enabled)) {
      next
    }
    tryCatch(
      {
        episodic_trace("Sending notification via ", name)
        channel_fns[[name]](ch, message)
        episodic_trace("Notification sent via ", name)
      },
      error = function(e) {
        episodic_trace(
          "Notification via ",
          name,
          " failed: ",
          conditionMessage(e)
        )
      }
    )
  }
}

#' Validate notification configuration
#'
#' Checks that enabled channels have all required fields and that
#' optional dependencies are installed. Returns a character vector of
#' problems (empty if everything is valid).
#'
#' @param config The resolved configuration.
#' @return A character vector of problems, or `character(0)` if valid.
#' @keywords internal
#' @noRd
episodic_notify_validate_config <- function(config) {
  notif <- config$notifications
  if (is.null(notif) || !isTRUE(notif$enabled)) {
    return(character(0))
  }
  problems <- character(0)
  channels <- notif$channels
  if (is.null(channels)) {
    return("notifications.enabled is TRUE but no channels are configured")
  }

  if (isTRUE(channels$ntfy$enabled)) {
    if (
      is.null(channels$ntfy$server) || !nzchar(channels$ntfy$server %||% "")
    ) {
      problems <- c(problems, "ntfy: server is required")
    }
    if (is.null(channels$ntfy$topic) || !nzchar(channels$ntfy$topic %||% "")) {
      problems <- c(problems, "ntfy: topic is required")
    }
    if (!requireNamespace("httr2", quietly = TRUE)) {
      problems <- c(
        problems,
        "ntfy: package 'httr2' is required but not installed"
      )
    }
  }

  if (isTRUE(channels$smtp$enabled)) {
    if (is.null(channels$smtp$host) || !nzchar(channels$smtp$host %||% "")) {
      problems <- c(problems, "smtp: host is required")
    }
    if (is.null(channels$smtp$from) || !nzchar(channels$smtp$from %||% "")) {
      problems <- c(problems, "smtp: from is required")
    }
    to <- channels$smtp$to
    if (is.null(to) || length(to) == 0) {
      problems <- c(
        problems,
        "smtp: at least one recipient in 'to' is required"
      )
    }
    if (!requireNamespace("curl", quietly = TRUE)) {
      problems <- c(
        problems,
        "smtp: package 'curl' is required but not installed"
      )
    }
  }

  if (isTRUE(channels$sendmail$enabled)) {
    binary <- channels$sendmail$binary %||% "/usr/sbin/sendmail"
    if (!file.exists(binary)) {
      problems <- c(problems, paste0("sendmail: binary not found at ", binary))
    }
    if (
      is.null(channels$sendmail$from) || !nzchar(channels$sendmail$from %||% "")
    ) {
      problems <- c(problems, "sendmail: from is required")
    }
    to <- channels$sendmail$to
    if (is.null(to) || length(to) == 0) {
      problems <- c(
        problems,
        "sendmail: at least one recipient in 'to' is required"
      )
    }
  }

  if (isTRUE(channels$microsoft365$enabled)) {
    if (
      is.null(channels$microsoft365$tenant_id) ||
        !nzchar(channels$microsoft365$tenant_id %||% "")
    ) {
      problems <- c(problems, "microsoft365: tenant_id is required")
    }
    has_client_secret <-
      !is.null(channels$microsoft365$client_secret) &&
        nzchar(channels$microsoft365$client_secret %||% "")
    if (
      has_client_secret &&
        (is.null(channels$microsoft365$from) ||
          !nzchar(channels$microsoft365$from %||% ""))
    ) {
      problems <- c(
        problems,
        "microsoft365: from is required when client_secret is set (application permissions)"
      )
    }
    to <- channels$microsoft365$to
    if (is.null(to) || length(to) == 0) {
      problems <- c(
        problems,
        "microsoft365: at least one recipient in 'to' is required"
      )
    }
    if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
      problems <- c(
        problems,
        "microsoft365: package 'Microsoft365R' is required but not installed"
      )
    }
    if (!requireNamespace("AzureGraph", quietly = TRUE)) {
      problems <- c(
        problems,
        "microsoft365: package 'AzureGraph' is required but not installed"
      )
    }
  }

  if (isTRUE(channels$teams$enabled)) {
    if (
      is.null(channels$teams$webhook_url) ||
        !nzchar(channels$teams$webhook_url %||% "")
    ) {
      problems <- c(problems, "teams: webhook_url is required")
    }
    if (!requireNamespace("httr2", quietly = TRUE)) {
      problems <- c(
        problems,
        "teams: package 'httr2' is required but not installed"
      )
    }
  }

  if (isTRUE(channels$slack$enabled)) {
    if (
      is.null(channels$slack$webhook_url) ||
        !nzchar(channels$slack$webhook_url %||% "")
    ) {
      problems <- c(problems, "slack: webhook_url is required")
    }
    if (!requireNamespace("httr2", quietly = TRUE)) {
      problems <- c(
        problems,
        "slack: package 'httr2' is required but not installed"
      )
    }
  }

  problems
}

#' Send a test notification through all configured channels
#'
#' Validates the notification configuration and sends a test message
#' through every enabled channel. Run this interactively after setting up
#' your instance configuration to verify that notifications work end to
#' end.
#'
#' @param episodic_config_path Passed to [episodic_config_resolve()].
#' @return Invisibly, a named logical vector: `TRUE` for each channel
#'   that succeeded, `FALSE` for each that failed.
#' @examples
#' \dontrun{
#' # After configuring notifications in your EPISODIC_CONFIG YAML:
#' episodic_notify_test()
#' }
#' @export
episodic_notify_test <- function(
    episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  config <- episodic_config_resolve(episodic_config_path)
  notif <- config$notifications

  if (is.null(notif) || !isTRUE(notif$enabled)) {
    cli::cli_alert_warning(
      "Notifications are not enabled in the configuration."
    )
    return(invisible(logical(0)))
  }

  problems <- episodic_notify_validate_config(config)
  if (length(problems) > 0) {
    cli::cli_alert_danger("Configuration problems found:")
    for (p in problems) {
      cli::cli_alert_danger("  {p}")
    }
    return(invisible(logical(0)))
  }

  lang <- episodic_lang()
  title <- episodic_tr("notif.title_test", lang = lang)
  body_text <- episodic_tr("notif.test.body", lang = lang)
  sent_at_label <- episodic_tr("notif.test.sent_at", lang = lang)
  working_text <- episodic_tr("notif.test.working", lang = lang)
  status_label <- episodic_tr("notif.label.status", lang = lang)
  status_value <- episodic_tr("notif.test.status_value", lang = lang)
  sent_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  message <- list(
    title = title,
    plain = paste0(
      body_text,
      "\n\n",
      sent_at_label,
      ": ",
      sent_at,
      "\n",
      working_text
    ),
    html = episodic_notify_html_wrap(
      title,
      paste0(
        "<p>", episodic_html_escape(body_text), "</p>",
        "<p><strong>", episodic_html_escape(sent_at_label), ":</strong> ",
        sent_at,
        "</p>",
        "<p>", episodic_html_escape(working_text), "</p>"
      ),
      notif$dashboard_url,
      lang = lang
    ),
    teams_card = episodic_notify_teams_card(
      title,
      list(
        list(title = status_label, value = status_value),
        list(title = sent_at_label, value = sent_at)
      ),
      notif$dashboard_url,
      lang = lang
    ),
    slack_text = paste0(
      "*", title, "*\n\n",
      sent_at_label,
      ": ",
      sent_at,
      "\n",
      working_text
    )
  )

  channel_fns <- list(
    ntfy = episodic_notify_ntfy,
    smtp = episodic_notify_smtp,
    sendmail = episodic_notify_sendmail,
    microsoft365 = episodic_notify_microsoft365,
    teams = episodic_notify_teams,
    slack = episodic_notify_slack
  )

  results <- logical(0)
  for (name in names(channel_fns)) {
    ch <- notif$channels[[name]]
    if (is.null(ch) || !isTRUE(ch$enabled)) {
      next
    }
    ok <- tryCatch(
      {
        cli::cli_alert_info("Sending test via {name}...")
        channel_fns[[name]](ch, message)
        cli::cli_alert_success("{name}: sent successfully")
        TRUE
      },
      error = function(e) {
        cli::cli_alert_danger("{name}: {conditionMessage(e)}")
        FALSE
      }
    )
    results <- c(results, stats::setNames(ok, name))
  }

  invisible(results)
}

#' Set up Microsoft 365 authentication for notifications
#'
#' Runs an interactive Azure AD login and caches the refresh token for
#' subsequent unattended use by [episodic_run_cron()]. You only need to
#' run this once per server, unless the token expires. Requires the
#' `Microsoft365R` and `AzureGraph` packages.
#'
#' If your Azure AD app registration has a client secret configured for
#' application-level (daemon) access, put the `client_secret` in the
#' instance YAML instead and skip this function entirely: the cron will
#' use the client-credentials flow directly.
#'
#' If your staff already authenticate to Microsoft 365 through a shared
#' login on this server (for example by having called
#' `Microsoft365R::get_business_outlook()` interactively at some point, or
#' by running this function once), you can skip `client_id` entirely in
#' the instance YAML: `episodic_notify_microsoft365()` will reuse whatever
#' cached Azure AD token it finds for the configured `tenant_id` in
#' `AzureAuth::AzureR_dir()`, without any client ID, client secret, or new
#' interactive login.
#'
#' @param tenant_id Your Azure AD tenant ID.
#' @param client_id Your Azure AD app registration's client ID. If
#'   `NULL`, uses the Microsoft365R package default.
#' @param scopes Scopes to request. The default requests `Mail.Send`.
#' @return Invisible `TRUE` on success.
#' @examples
#' \dontrun{
#' episodic_setup_microsoft365(tenant_id = "your-tenant-id")
#' }
#' @export
episodic_setup_microsoft365 <- function(
    tenant_id,
    client_id = NULL,
    scopes = c("Mail.Send", "User.Read", "openid", "offline_access")) {
  rlang::check_installed("Microsoft365R")
  rlang::check_installed("AzureGraph")
  rlang::check_installed("AzureAuth")

  cli::cli_alert_info(
    "Starting interactive Azure AD login for tenant {tenant_id}..."
  )
  cli::cli_alert_info(
    "A browser window will open. Sign in with the account that will send notifications."
  )

  args <- list(tenant = tenant_id, scopes = scopes)
  if (!is.null(client_id)) {
    args$app <- client_id
  }
  gr <- do.call(AzureGraph::create_graph_login, args)

  cli::cli_alert_success("Login successful. Token cached for unattended use.")
  cli::cli_alert_info(
    "The cached token is stored in {AzureAuth::AzureR_dir()}"
  )

  invisible(TRUE)
}
