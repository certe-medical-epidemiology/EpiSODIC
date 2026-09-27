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

mail_test_details <- function() {
  data.frame(
    cluster_id = c(1L, 2L),
    pathogen = c("MRSA", "Norovirus"),
    level = c("pathogen_institution", "pathogen_ward"),
    institution_name = c("Hospital A", "Hospital A"),
    ward = c(NA_character_, "3B"),
    region_code = c(NA_character_, NA_character_),
    n_cases = c(5L, 3L),
    case_days = c(4L, 3L),
    expected = c(1.2, NA_real_),
    excess = c(3.8, NA_real_),
    ratio = c(4.2, NA_real_),
    priority_score = c(72.3, 40),
    detector_agreement = c(2L, 1L),
    first_day = c("2026-08-01", "2026-08-10"),
    last_day = c("2026-08-15", "2026-08-12"),
    stringsAsFactors = FALSE
  )
}

test_that("an email template kind is one of the two, and names its own variable", {
  expect_equal(
    unname(episodic_mail_template_variables),
    c("EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS", "EPISODIC_MAIL_TEMPLATE_REPORT")
  )
  expect_error(episodic_mail_template_kind("failure"), "must be one of")
  expect_error(episodic_mail_template_kind(c("report", "report")), "must be one of")
})

test_that("an unset template is not a problem, and a set one is checked for existence, format and Quarto", {
  expect_true(is.na(episodic_mail_template_problem("report", NA_character_)))
  expect_true(is.na(episodic_mail_template_problem("report", "")))
  expect_equal(
    episodic_mail_template_problem("report", tempfile(fileext = ".qmd")),
    "missing_file"
  )

  rmd <- tempfile(fileext = ".Rmd")
  qmd <- tempfile(fileext = ".QMD")
  writeLines("---\n---", rmd)
  writeLines("---\n---", qmd)
  on.exit(unlink(c(rmd, qmd)))
  expect_equal(episodic_mail_template_problem("report", rmd), "not_qmd")

  local_mocked_bindings(episodic_quarto_available = function() FALSE)
  expect_equal(episodic_mail_template_problem("report", qmd), "no_quarto")
  local_mocked_bindings(episodic_quarto_available = function() TRUE)
  expect_true(is.na(episodic_mail_template_problem("report", qmd)))
})

test_that("every template problem is described in every shipped language", {
  for (lang in c("en", "en-US", "nl", "de", "fr", "es", "ar", "hi", "zh")) {
    for (reason in c("missing_file", "not_qmd", "no_quarto")) {
      text <- episodic_mail_template_problem_text(reason, lang = lang)
      expect_false(grepl("[[", text, fixed = TRUE), info = paste(lang, reason))
    }
  }
})

test_that("the email styles are built from the palette, not from fixed colours", {
  pal <- episodic_palette_shipped()
  pal$primary <- "#123456"
  pal$primary_dark <- "#0A0B0C"
  pal$ink <- "#010203"
  pal$font <- "\"Noto Sans\", Arial, sans-serif"
  pal$font_size_base <- "15px"
  style <- episodic_mail_style(pal)

  expect_match(style$body, "font-family:\"Noto Sans\", Arial, sans-serif", fixed = TRUE)
  expect_match(style$body, "font-size:15px", fixed = TRUE)
  expect_match(style$body, "color:#010203", fixed = TRUE)
  expect_match(style$heading, "#0A0B0C", fixed = TRUE)
  expect_match(style$link, "#123456", fixed = TRUE)

  # a font stack's double quotes cannot end the single-quoted attribute
  expect_equal(
    episodic_mail_style_attr("font-family:\"A B\", 'C'"),
    " style='font-family:&quot;A B&quot;, &#39;C&#39;'"
  )
})

test_that("the built-in emails carry the palette's colours and font, and no fixed ones", {
  pal <- episodic_palette_shipped()
  pal$primary <- "#123456"
  pal$bg_subtle <- "#ABCDEF"
  pal$font <- "Georgia, serif"
  style <- episodic_mail_style(pal)

  msg <- episodic_notify_build_new_clusters(
    mail_test_details(),
    2L,
    "2026-08-15",
    "https://episodic.example.org",
    lang = "en",
    style = style
  )
  expect_match(msg$html, "font-family:Georgia, serif", fixed = TRUE)
  expect_match(msg$html, "background-color:#ABCDEF", fixed = TRUE)
  expect_match(msg$html, "color:#123456", fixed = TRUE)
  expect_false(grepl("#f0f0f0", msg$html, fixed = TRUE))
  expect_false(grepl("font-family:sans-serif;font-size:14px", msg$html, fixed = TRUE))
  # the content sits inside the document, not beside it
  expect_true(grepl(msg$html_content, msg$html, fixed = TRUE))
  expect_false(grepl("<body", msg$html_content, fixed = TRUE))

  failure <- episodic_notify_build_failure("boom", "2026-08-15", "host", lang = "en", style = style)
  expect_match(failure$html, "font-family:Georgia, serif", fixed = TRUE)

  report <- episodic_scheduled_report_build(
    mail_test_details()[1, ],
    diff = list(n_new_cases = 2L),
    final = FALSE,
    interval_days = 7L,
    lang = "en",
    style = style
  )
  expect_match(report$html, "font-family:Georgia, serif", fixed = TRUE)
  expect_match(report$html, paste0("color:", pal$muted), fixed = TRUE)
  expect_false(grepl("#666", report$html, fixed = TRUE))
})

test_that("a new-clusters template receives every cluster, with an unmeasured value as NA rather than zero", {
  details <- mail_test_details()
  data <- episodic_mail_data_new_clusters(
    details,
    n_new = 2L,
    run_date = "2026-08-15",
    dashboard_url = NULL,
    title = "Title",
    default_html = "<html></html>",
    default_content = "<h2>Title</h2>",
    lang = "en"
  )
  expect_equal(data$kind, "new_clusters")
  expect_null(data$dashboard_url)
  expect_equal(nrow(data$clusters), 2L)
  expect_true(all(c(
    "cluster_id", "ref", "url", "pathogen", "level", "location", "first_day",
    "last_day", "period", "duration_days", "n_cases", "case_days", "expected",
    "ratio", "priority_score"
  ) %in% names(data$clusters)))
  # ordered as every cluster table is: most recent last case day first
  expect_equal(data$clusters$cluster_id, c(1L, 2L))
  expect_true(all(is.na(data$clusters$url)))
  norovirus <- data$clusters[data$clusters$cluster_id == 2L, ]
  expect_true(is.na(norovirus$expected))
  expect_true(is.na(norovirus$ratio))
  expect_equal(data$palette, episodic_palette())
  expect_equal(data$mail_style, episodic_mail_style(episodic_palette()))
  expect_equal(data$default_content, "<h2>Title</h2>")

  with_url <- episodic_mail_data_new_clusters(
    details, 2L, "2026-08-15", "https://episodic.example.org",
    "Title", "<html></html>", "",
    lang = "en"
  )
  expect_equal(with_url$dashboard_url, "https://episodic.example.org")
  expect_equal(with_url$clusters$url[1], "https://episodic.example.org?cluster=1")
})

test_that("a report template receives its cluster, and no diff when there is nothing to compare", {
  data <- episodic_mail_data_report(
    mail_test_details()[1, ],
    diff = NULL,
    final = TRUE,
    interval_days = 7L,
    attachment_path = file.path(tempdir(), "outbreak-1-v1.html"),
    title = "Title",
    default_html = "<html></html>",
    default_content = "",
    lang = "en"
  )
  expect_equal(data$kind, "report")
  expect_equal(data$cluster_id, 1L)
  expect_equal(data$ref, "O-1")
  expect_null(data$diff)
  expect_true(data$final)
  expect_equal(data$attachment_name, "outbreak-1-v1.html")
  expect_equal(data$palette, episodic_palette())
})

test_that("with no template configured, the built-in body goes out and nothing is rendered", {
  local_mocked_bindings(episodic_mail_render = function(...) stop("must not render"))
  expect_equal(
    episodic_mail_body("report", list(default_html = "<p>built-in</p>"), NA_character_),
    "<p>built-in</p>"
  )
})

test_that("a template that cannot be used still sends the email, with the built-in body and a danger line", {
  missing <- tempfile(fileext = ".qmd")
  expect_message(
    html <- episodic_mail_body(
      "new_clusters",
      list(default_html = "<p>built-in</p>"),
      missing
    ),
    "EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS could not be used"
  )
  expect_equal(html, "<p>built-in</p>")

  local_mocked_bindings(episodic_mail_render = function(...) stop("Quarto broke"))
  expect_message(
    html <- episodic_mail_body("report", list(default_html = "<p>built-in</p>"), missing),
    "Quarto broke"
  )
  expect_equal(html, "<p>built-in</p>")
})

test_that("a template that renders replaces the built-in body", {
  local_mocked_bindings(episodic_mail_render = function(kind, data, template_path) {
    paste0("<p>", kind, " from ", basename(template_path), "</p>")
  })
  expect_equal(
    episodic_mail_body("report", list(default_html = "<p>built-in</p>"), "/x/mine.qmd"),
    "<p>report from mine.qmd</p>"
  )
})

test_that("rendering refuses a template it cannot use, naming the variable", {
  expect_error(
    episodic_mail_render("report", list(), tempfile(fileext = ".qmd")),
    "EPISODIC_MAIL_TEMPLATE_REPORT .*does not exist"
  )
})

test_that("the new-clusters alert renders the template only for an email channel", {
  qmd <- tempfile(fileext = ".qmd")
  writeLines("---\n---", qmd)
  on.exit(unlink(qmd))
  withr::local_envvar(EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS = qmd)

  rendered <- new.env()
  rendered$n <- 0L
  sent <- new.env()
  local_mocked_bindings(
    episodic_notify_cluster_details = function(con, cluster_ids) mail_test_details(),
    episodic_quarto_available = function() TRUE,
    episodic_mail_render = function(kind, data, template_path) {
      rendered$n <- rendered$n + 1L
      "<p>from the template</p>"
    },
    episodic_notify_ntfy = function(channel, message) sent$ntfy <- message$html,
    episodic_notify_smtp = function(channel, message) sent$smtp <- message$html
  )
  result <- list(status = "success", n_signals_new = 2L, new_cluster_ids = 1:2)

  ntfy_only <- list(notifications = list(
    enabled = TRUE,
    triggers = list(new_clusters = TRUE),
    channels = list(ntfy = list(enabled = TRUE))
  ))
  episodic_notify(NULL, ntfy_only, result, 1L, "2026-08-15", "host")
  expect_equal(rendered$n, 0L)
  expect_false(grepl("from the template", sent$ntfy, fixed = TRUE))

  both <- ntfy_only
  both$notifications$channels$smtp <- list(enabled = TRUE)
  episodic_notify(NULL, both, result, 1L, "2026-08-15", "host")
  expect_equal(rendered$n, 1L)
  expect_equal(sent$smtp, "<p>from the template</p>")
  expect_false(grepl("from the template", sent$ntfy, fixed = TRUE))
})

test_that("a scheduled report's email uses the report template when one is configured", {
  qmd <- tempfile(fileext = ".qmd")
  writeLines("---\n---", qmd)
  on.exit(unlink(qmd))

  captured <- new.env()
  local_mocked_bindings(
    episodic_notify_cluster_details = function(con, cluster_ids) mail_test_details()[1, ],
    episodic_scheduled_report_latest_diff = function(con, cluster_id) NULL,
    episodic_mail_render = function(kind, data, template_path) {
      captured$data <- data
      "<p>report template</p>"
    }
  )
  subscription <- list(cluster_id = 1L, interval_days = 7L)

  withr::local_envvar(EPISODIC_MAIL_TEMPLATE_REPORT = NA)
  msg <- episodic_scheduled_report_message(NULL, subscription, FALSE, "outbreak-1-v1.html", "en")
  expect_match(msg$html, "<html><body", fixed = TRUE)
  expect_null(captured$data)

  withr::local_envvar(EPISODIC_MAIL_TEMPLATE_REPORT = qmd)
  msg <- episodic_scheduled_report_message(NULL, subscription, FALSE, "outbreak-1-v1.html", "en")
  expect_equal(msg$html, "<p>report template</p>")
  expect_equal(msg$attachment_path, "outbreak-1-v1.html")
  expect_equal(captured$data$kind, "report")
  expect_equal(captured$data$attachment_name, "outbreak-1-v1.html")
  expect_null(captured$data$diff)
})

test_that("the notification check reports an email template that cannot be used", {
  withr::local_envvar(EPISODIC_MAIL_TEMPLATE_REPORT = tempfile(fileext = ".qmd"))
  config <- list(notifications = list(
    enabled = TRUE,
    channels = list(ntfy = list(enabled = FALSE))
  ))
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("^EPISODIC_MAIL_TEMPLATE_REPORT: .*does not exist", problems)))
})

test_that("the cron run log names a configured template that cannot be used, as a warning line", {
  withr::local_envvar(
    EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS = tempfile(fileext = ".qmd"),
    EPISODIC_MAIL_TEMPLATE_REPORT = NA
  )
  expect_message(
    episodic_mail_template_trace(),
    "EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS: .*does not exist"
  )
  withr::local_envvar(EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS = NA)
  expect_silent(episodic_mail_template_trace())
})

test_that("a preview with no template writes the built-in body the email would go out with", {
  withr::local_envvar(
    EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS = NA,
    EPISODIC_MAIL_TEMPLATE_REPORT = NA
  )
  for (kind in c("new_clusters", "report")) {
    out <- tempfile(fileext = ".html")
    expect_message(
      path <- episodic_mail_template_preview(kind, output_file = out, lang = "en"),
      "built-in body"
    )
    expect_equal(path, out)
    html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_match(html, "<html><body", fixed = TRUE)
    expect_match(html, episodic_palette()$primary_dark, fixed = TRUE)
    unlink(out)
  }
})

test_that("both shipped templates render, in the instance palette", {
  skip_if_not(
    episodic_test_can_render_report(),
    "needs the quarto CLI and an installed EpiSODIC (the template reads it)"
  )
  for (kind in c("new_clusters", "report")) {
    template <- system.file(
      "mail",
      paste0("episodic_default_mail_", kind, ".qmd"),
      package = "EpiSODIC"
    )
    expect_true(nzchar(template), info = kind)
    out <- tempfile(fileext = ".html")
    suppressMessages(episodic_mail_template_preview(
      kind,
      template_path = template,
      output_file = out,
      lang = "en"
    ))
    html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_match(html, episodic_palette()$primary_dark, fixed = TRUE, info = kind)
    expect_false(grepl("<script", html, fixed = TRUE), info = kind)
    unlink(out)
  }
})
