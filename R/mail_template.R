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

# Operator templates for the body of EpiSODIC's two routine emails: the
# alert for newly detected clusters and the message a scheduled report is
# attached to. Each is optional and each has its own environment
# variable, because the two carry different data - many clusters against
# one cluster and its report - and an operator changing one should not
# have to reimplement the other.
#
# A template changes only the HTML body sent through the email channels
# (smtp, sendmail, microsoft365). ntfy, Teams and Slack cannot show HTML
# and keep their own messages; the subject line stays the translated one;
# the run-failure email always uses the built-in body, since a message
# saying a run failed must not itself depend on a render succeeding.
#
# A template that cannot be used never costs an email: the built-in body
# goes out instead, and the run log says why with a danger line. An alert
# arriving in the house layout a day late is worth less than one arriving
# now in EpiSODIC's own.

#' The environment variable naming each kind of email template
#' @keywords internal
#' @noRd
episodic_mail_template_variables <- c(
  new_clusters = "EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS",
  report = "EPISODIC_MAIL_TEMPLATE_REPORT"
)

#' Check an email template kind
#' @keywords internal
#' @noRd
episodic_mail_template_kind <- function(kind) {
  if (
    !is.character(kind) ||
      length(kind) != 1 ||
      !kind %in% names(episodic_mail_template_variables)
  ) {
    stop(
      "`kind` must be one of ",
      paste0("'", names(episodic_mail_template_variables), "'", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  kind
}

#' The configured template path for one kind, or `NA` when none is set
#' @keywords internal
#' @noRd
episodic_mail_template_env <- function(kind) {
  kind <- episodic_mail_template_kind(kind)
  value <- Sys.getenv(episodic_mail_template_variables[[kind]], unset = NA)
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}

#' Why a configured email template cannot be used, or `NA` when it can
#'
#' `NA` too when no template is configured: the built-in body is then
#' what was asked for, not a problem.
#'
#' Only Quarto (`.qmd`) is accepted. Report rendering already needs the
#' Quarto CLI, which ships its own pandoc, so it adds nothing an instance
#' sending scheduled reports does not have; R Markdown would add a second
#' rendering engine with its own failure modes. Quarto would render an
#' `.Rmd` too, but reading `format:` where an `.Rmd` author wrote
#' `output:`, and silently dropping those options is the one outcome
#' worse than refusing the file.
#'
#' @param kind `"new_clusters"` or `"report"`.
#' @param template_path The configured path, or `NA`.
#' @return `NA_character_`, `"missing_file"`, `"not_qmd"` or
#'   `"no_quarto"`.
#' @keywords internal
#' @noRd
episodic_mail_template_problem <- function(kind,
                                           template_path = episodic_mail_template_env(kind)) {
  if (length(template_path) != 1 || is.na(template_path) || !nzchar(template_path)) {
    return(NA_character_)
  }
  if (!file.exists(template_path)) {
    return("missing_file")
  }
  if (!grepl("\\.qmd$", template_path, ignore.case = TRUE)) {
    return("not_qmd")
  }
  if (!episodic_quarto_available()) {
    return("no_quarto")
  }
  NA_character_
}

#' A template problem as a sentence
#' @param reason From `episodic_mail_template_problem()`.
#' @param lang Language to write it in.
#' @keywords internal
#' @noRd
episodic_mail_template_problem_text <- function(reason, lang = episodic_lang()) {
  episodic_tr(paste0("info.reference.mail_template.", reason), lang = lang)
}

#' Say, once per cron run, which configured email templates cannot be used
#'
#' Said at the start of the run, not only when an email is due, so an
#' operator reading the log of a quiet week still learns that the next
#' alert will not go out in their layout.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_mail_template_trace <- function() {
  for (kind in names(episodic_mail_template_variables)) {
    reason <- episodic_mail_template_problem(kind)
    if (!is.na(reason)) {
      episodic_trace(
        episodic_mail_template_variables[[kind]],
        ": ",
        episodic_mail_template_problem_text(reason, lang = episodic_language_fallback),
        severity = "warn"
      )
    }
  }
  invisible(NULL)
}

#' Inline styles for an email, from the instance palette
#'
#' Every email EpiSODIC sends, built-in or from a template, is styled from
#' the same palette the dashboard is (`episodic_palette()`), so an alert
#' carries the instance's own colours and font. Inline because an email
#' client drops a `<style>` block (Outlook) or restricts it (Gmail); an
#' inline `style` attribute is the one form every client honours.
#'
#' @param palette A palette, as returned by `episodic_palette()`.
#' @return A named list of CSS declaration strings (unescaped; put one in
#'   an attribute with `episodic_mail_style_attr()`): `body`, `heading`,
#'   `paragraph`, `table`, `head_row`, `th`, `td`, `link`, `muted`.
#' @keywords internal
#' @noRd
episodic_mail_style <- function(palette = episodic_palette()) {
  font <- paste0("font-family:", palette$font)
  list(
    body = paste0(
      font,
      ";font-size:", palette$font_size_base,
      ";color:", palette$ink,
      ";background-color:", palette$surface
    ),
    heading = paste0(font, ";margin:0 0 12px;color:", palette$primary_dark),
    # No colour of its own: a paragraph inherits the body's, so a caller
    # prefixing `muted` to it is not overridden by a later declaration.
    paragraph = "margin:0 0 8px",
    table = paste0(
      font,
      ";font-size:", palette$font_size_base,
      ";border-collapse:collapse;width:100%"
    ),
    head_row = paste0("background-color:", palette$bg_subtle),
    th = paste0(
      "padding:4px;color:", palette$ink,
      ";border-bottom:2px solid ", palette$border
    ),
    td = paste0("padding:4px;border-bottom:1px solid ", palette$border),
    link = paste0("color:", palette$primary),
    muted = paste0("color:", palette$muted)
  )
}

#' A `style` attribute holding CSS declarations, escaped for HTML
#'
#' Single-quoted, like every attribute in EpiSODIC's email HTML, and
#' escaped whole: a font stack carries double quotes (`"IBM Plex Sans"`),
#' and escaping keeps any quote in it from ending the attribute.
#' @param css A CSS declaration string.
#' @return A string starting with a space, ready to paste into a tag.
#' @keywords internal
#' @noRd
episodic_mail_style_attr <- function(css) {
  paste0(" style='", episodic_html_escape(css), "'")
}

#' The data a new-clusters email template receives
#'
#' @param details From `episodic_notify_cluster_details()`.
#' @param n_new Total number of new signals.
#' @param run_date The run date.
#' @param dashboard_url The configured dashboard URL, or `NULL`/`""`.
#' @param title The email's subject line.
#' @param default_html The built-in body, as a whole HTML document.
#' @param default_content The inside of `default_html`'s `<body>`.
#' @param lang Language the email is written in.
#' @param palette The palette the email is styled from.
#' @return A list; see `vignette("notifications")` for its documented
#'   shape. A value that was not measured is `NA` in `clusters` and `NULL`
#'   elsewhere, never zero.
#' @keywords internal
#' @noRd
episodic_mail_data_new_clusters <- function(details,
                                            n_new,
                                            run_date,
                                            dashboard_url,
                                            title,
                                            default_html,
                                            default_content,
                                            lang = episodic_lang(),
                                            palette = episodic_palette()) {
  details <- details[episodic_cluster_table_order(details), , drop = FALSE]
  has_dashboard <- !is.null(dashboard_url) && nzchar(dashboard_url)
  clusters <- data.frame(
    cluster_id = as.integer(details$cluster_id),
    ref = vapply(
      seq_len(nrow(details)),
      function(i) episodic_object_ref(details$cluster_id[i], details$level[i], lang = lang),
      character(1)
    ),
    url = vapply(
      details$cluster_id,
      function(id) episodic_notify_cluster_url(dashboard_url, id) %||% NA_character_,
      character(1)
    ),
    pathogen = details$pathogen,
    level = details$level,
    location = vapply(
      seq_len(nrow(details)),
      function(i) episodic_notify_location(details[i, , drop = FALSE], lang = lang),
      character(1)
    ),
    first_day = as.character(details$first_day),
    last_day = as.character(details$last_day),
    period = vapply(
      seq_len(nrow(details)),
      function(i) {
        episodic_format_date_range(details$first_day[i], details$last_day[i], lang = lang)
      },
      character(1)
    ),
    duration_days = episodic_cluster_duration_days(details$first_day, details$last_day),
    n_cases = as.integer(details$n_cases),
    case_days = as.integer(details$case_days),
    expected = as.numeric(details$expected),
    ratio = as.numeric(details$ratio),
    priority_score = as.numeric(details$priority_score),
    stringsAsFactors = FALSE
  )
  list(
    kind = "new_clusters",
    title = title,
    n_new = as.integer(n_new),
    run_date = as.character(run_date),
    dashboard_url = if (has_dashboard) dashboard_url else NULL,
    clusters = clusters,
    lang = episodic_lang(lang),
    palette = palette,
    mail_style = episodic_mail_style(palette),
    default_html = default_html,
    default_content = default_content,
    package_version = as.character(utils::packageVersion("EpiSODIC"))
  )
}

#' The data a scheduled-report email template receives
#'
#' @param details One row of `episodic_notify_cluster_details()`.
#' @param diff What changed since the previous report, or `NULL` when there
#'   is nothing to compare against.
#' @param final Whether this send ends the schedule.
#' @param interval_days The schedule's interval.
#' @param attachment_path Path to the attached report, or `NULL`.
#' @param title,default_html,default_content,lang,palette As for
#'   `episodic_mail_data_new_clusters()`.
#' @return A list; see `vignette("scheduled-reports")` for its documented
#'   shape.
#' @keywords internal
#' @noRd
episodic_mail_data_report <- function(details,
                                      diff,
                                      final,
                                      interval_days,
                                      attachment_path,
                                      title,
                                      default_html,
                                      default_content,
                                      lang = episodic_lang(),
                                      palette = episodic_palette()) {
  list(
    kind = "report",
    title = title,
    cluster_id = as.integer(details$cluster_id),
    ref = episodic_object_ref(details$cluster_id, details$level, lang = lang),
    pathogen = details$pathogen,
    level = details$level,
    location = episodic_notify_location(details, lang = lang),
    first_day = as.character(details$first_day),
    last_day = as.character(details$last_day),
    period = episodic_format_date_range(details$first_day, details$last_day, lang = lang),
    n_cases = as.integer(details$n_cases),
    diff = diff,
    final = isTRUE(final),
    interval_days = as.integer(interval_days),
    attachment_name = if (is.null(attachment_path)) NULL else basename(attachment_path),
    lang = episodic_lang(lang),
    palette = palette,
    mail_style = episodic_mail_style(palette),
    default_html = default_html,
    default_content = default_content,
    package_version = as.character(utils::packageVersion("EpiSODIC"))
  )
}

#' The HTML body an email goes out with
#'
#' The operator's template for `kind` when one is configured and renders,
#' and `data$default_html` - the built-in body - otherwise. A template that
#' cannot be used or fails to render is logged as a danger line naming the
#' variable and the reason, and the email still goes out.
#'
#' @param kind `"new_clusters"` or `"report"`.
#' @param data From `episodic_mail_data_new_clusters()` or
#'   `episodic_mail_data_report()`.
#' @param template_path The configured template, or `NA` for none.
#' @return A single HTML string.
#' @keywords internal
#' @noRd
episodic_mail_body <- function(kind,
                               data,
                               template_path = episodic_mail_template_env(kind)) {
  kind <- episodic_mail_template_kind(kind)
  if (length(template_path) != 1 || is.na(template_path) || !nzchar(template_path)) {
    return(data$default_html)
  }
  tryCatch(
    episodic_mail_render(kind, data, template_path),
    error = function(e) {
      episodic_trace(
        episodic_mail_template_variables[[kind]],
        " could not be used, so this email goes out with the built-in body: ",
        conditionMessage(e),
        severity = "danger"
      )
      data$default_html
    }
  )
}

#' Render an email template to HTML
#'
#' The same mechanics as `episodic_report_render()`: the template is copied
#' into a working directory of its own, the data is written beside it as
#' `mail_data.rds`, and Quarto renders it with `params$data_path` pointing
#' there. Fails loudly on every path; `episodic_mail_body()` decides what
#' a failure means for the email.
#'
#' @param kind `"new_clusters"` or `"report"`.
#' @param data The template's data.
#' @param template_path A configured template path.
#' @return A single HTML string.
#' @keywords internal
#' @noRd
episodic_mail_render <- function(kind, data, template_path) {
  kind <- episodic_mail_template_kind(kind)
  variable <- episodic_mail_template_variables[[kind]]
  reason <- episodic_mail_template_problem(kind, template_path)
  if (!is.na(reason)) {
    stop(
      variable,
      " ('",
      template_path,
      "'): ",
      episodic_mail_template_problem_text(reason, lang = episodic_language_fallback),
      call. = FALSE
    )
  }

  work_dir <- tempfile("episodic_mail_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  input <- file.path(work_dir, "episodic_mail.qmd")
  if (!file.copy(template_path, input)) {
    stop(
      "The email template '",
      template_path,
      "' could not be copied into the working directory for rendering.",
      call. = FALSE
    )
  }
  saveRDS(data, file.path(work_dir, "mail_data.rds"))

  tryCatch(
    quarto::quarto_render(
      input = input,
      execute_params = list(data_path = "mail_data.rds"),
      # quiet = FALSE for the same reason as in episodic_report_render():
      # only then does the error carry the CLI's own account of what
      # went wrong.
      output_file = "mail.html",
      quiet = FALSE,
      as_job = FALSE
    ),
    error = function(e) {
      stop(
        "Quarto failed to render the email template: ",
        rlang::cnd_message(e, inherit = TRUE),
        call. = FALSE
      )
    }
  )

  output <- file.path(work_dir, "mail.html")
  if (!file.exists(output)) {
    stop(
      "Quarto did not produce the expected output file: ",
      output,
      call. = FALSE
    )
  }
  html <- paste(
    readLines(output, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )
  if (!nzchar(trimws(html))) {
    stop("The email template rendered to an empty document.", call. = FALSE)
  }
  enc2utf8(html)
}

#' Preview an Email Template
#'
#' Renders the body of one of EpiSODIC's emails against example data and
#' writes it to an HTML file to open in a browser, so an email template can
#' be checked without waiting for a detection run to send one.
#'
#' Two emails can be given a template of their own, each with its own
#' environment variable pointing at a Quarto (`.qmd`) file:
#'
#' * `EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS` for the alert sent when a run
#'   detects new clusters;
#' * `EPISODIC_MAIL_TEMPLATE_REPORT` for the email a scheduled outbreak
#'   report is attached to.
#'
#' A template changes the HTML body sent through the email channels
#' (`smtp`, `sendmail`, `microsoft365`); the subject line, the ntfy, Teams
#' and Slack messages and the run-failure email are unchanged. Two
#' starting points ship with the package, in
#' `system.file("mail", package = "EpiSODIC")`. Every template receives the
#' instance palette (`episodic_palette()`) and the inline styles built from
#' it, so it can carry the dashboard's own colours and font. See
#' `vignette("notifications")` for the data a template receives.
#'
#' A template that cannot be used when an email is due never stops the
#' email: it goes out with the built-in body, and the run log says why.
#' This function, by contrast, stops with the error, which is what a
#' preview is for.
#'
#' @param kind `"new_clusters"` or `"report"`.
#' @param template_path Path to the `.qmd` template to preview. Defaults to
#'   the one the matching environment variable names; with neither, the
#'   built-in body is written, which is what the email would go out with.
#' @param output_file Where to write the HTML.
#' @param lang Language to render in. Defaults to `EPISODIC_LANGUAGE`.
#' @return The path to the written HTML file, invisibly.
#' @examples
#' \dontrun{
#' # the shipped starting point for the new-clusters alert
#' episodic_mail_template_preview(
#'   "new_clusters",
#'   template_path = system.file(
#'     "mail",
#'     "episodic_default_mail_new_clusters.qmd",
#'     package = "EpiSODIC"
#'   )
#' )
#' }
#' @export
episodic_mail_template_preview <- function(kind = c("new_clusters", "report"),
                                           template_path = NULL,
                                           output_file = tempfile(fileext = ".html"),
                                           lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  kind <- match.arg(kind)
  if (is.null(template_path)) {
    template_path <- episodic_mail_template_env(kind)
  }
  data <- episodic_mail_preview_data(kind, lang = lang)
  html <- if (is.na(template_path)) {
    data$default_html
  } else {
    episodic_mail_render(kind, data, template_path)
  }
  con <- file(output_file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  message(
    if (is.na(template_path)) {
      "No template configured for this email, so the built-in body was written to "
    } else {
      "The template's body was written to "
    },
    output_file
  )
  invisible(output_file)
}

#' Example data for a template preview
#'
#' Two made-up clusters, shaped exactly as
#' `episodic_notify_cluster_details()` returns real ones and passed through
#' the same builders a real send uses, so a template that renders here
#' renders against a real run too.
#' @keywords internal
#' @noRd
episodic_mail_preview_data <- function(kind, lang = episodic_lang()) {
  details <- data.frame(
    cluster_id = c(101L, 102L),
    pathogen = c("Klebsiella pneumoniae", "Norovirus"),
    level = c("pathogen_ward", "pathogen_institution"),
    institution_name = c("Example Hospital", "Example Hospital"),
    ward = c("Ward 3B", NA_character_),
    region_code = c(NA_character_, NA_character_),
    n_cases = c(4L, 11L),
    case_days = c(4L, 7L),
    expected = c(0.6, 3.2),
    excess = c(3.4, 7.8),
    ratio = c(6.7, 3.4),
    priority_score = c(64, 71),
    detector_agreement = c(1L, 2L),
    first_day = c("2025-01-06", "2025-01-02"),
    last_day = c("2025-01-14", "2025-01-15"),
    stringsAsFactors = FALSE
  )
  dashboard_url <- "https://episodic.example.org"
  if (identical(kind, "new_clusters")) {
    built <- episodic_notify_build_new_clusters(
      details,
      n_new = nrow(details),
      run_date = "2025-01-16",
      dashboard_url = dashboard_url,
      lang = lang
    )
    return(episodic_mail_data_new_clusters(
      details,
      n_new = nrow(details),
      run_date = "2025-01-16",
      dashboard_url = dashboard_url,
      title = built$title,
      default_html = built$html,
      default_content = built$html_content,
      lang = lang
    ))
  }
  row <- details[2, , drop = FALSE]
  diff <- list(
    previous_version_no = 1L,
    previous_rendered_at = "2025-01-09 08:00:00",
    n_new_cases = 3L,
    n_cases_delta = 3L,
    priority_score_delta = 5,
    ratio_delta = 0.4,
    period_extended = TRUE
  )
  built <- episodic_scheduled_report_build(
    row,
    diff = diff,
    final = FALSE,
    interval_days = 7L,
    lang = lang
  )
  episodic_mail_data_report(
    row,
    diff = diff,
    final = FALSE,
    interval_days = 7L,
    attachment_path = "outbreak-102-v2.html",
    title = built$title,
    default_html = built$html,
    default_content = built$html_content,
    lang = lang
  )
}
