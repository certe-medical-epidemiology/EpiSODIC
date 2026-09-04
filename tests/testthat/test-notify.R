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

test_that("episodic_notify() does nothing when notifications are not configured", {
  config <- episodic_config_resolve(NA)
  result <- list(
    status = "success",
    n_signals_new = 5L,
    new_cluster_ids = 1:5
  )
  expect_silent(
    episodic_notify(NULL, config, result, 1L, Sys.Date(), "testhost")
  )
})

test_that("episodic_notify() does nothing when enabled but no triggers fire", {
  config <- episodic_config_resolve(NA)
  config$notifications <- list(
    enabled = TRUE,
    triggers = list(new_clusters = TRUE, run_failure = TRUE),
    channels = list(ntfy = list(enabled = TRUE, server = "x", topic = "y"))
  )
  result <- list(
    status = "success",
    n_signals_new = 0L,
    new_cluster_ids = integer(0)
  )
  expect_silent(
    episodic_notify(NULL, config, result, 1L, Sys.Date(), "testhost")
  )
})

test_that("episodic_config_hash() is unchanged by notification config", {
  config <- episodic_config_resolve(NA)
  h1 <- episodic_config_hash(config)

  config$notifications <- list(
    enabled = TRUE,
    triggers = list(new_clusters = TRUE),
    channels = list(
      ntfy = list(
        enabled = TRUE,
        server = "https://ntfy.example",
        topic = "test"
      )
    )
  )
  h2 <- episodic_config_hash(config)
  expect_equal(h1$hash, h2$hash)
})

test_that("episodic_html_escape() escapes special characters", {
  expect_equal(episodic_html_escape("<b>"), "&lt;b&gt;")
  expect_equal(episodic_html_escape("a & b"), "a &amp; b")
  expect_equal(episodic_html_escape("x=\"y\""), "x=&quot;y&quot;")
  expect_equal(episodic_html_escape("plain"), "plain")
})

test_that("episodic_notify_location() describes streams", {
  row <- list(
    institution_name = "Hospital A",
    ward = "ICU",
    region_code = NA,
    level = "pathogen_ward"
  )
  expect_equal(episodic_notify_location(row), "Hospital A, ICU (ward)")

  row2 <- list(
    institution_name = "",
    ward = NA,
    region_code = "GR",
    level = "pathogen_area"
  )
  expect_equal(episodic_notify_location(row2), "GR (area)")

  row3 <- list(
    institution_name = "",
    ward = NA,
    region_code = NA,
    level = "pathogen_region"
  )
  expect_equal(episodic_notify_location(row3), "region")
})

test_that("episodic_notify_build_new_clusters() produces all message formats", {
  details <- data.frame(
    cluster_id = 1L,
    pathogen = "MRSA",
    level = "pathogen_institution",
    institution_name = "Hospital A",
    ward = NA_character_,
    region_code = NA_character_,
    n_cases = 5L,
    expected = 1.2,
    excess = 3.8,
    ratio = 4.2,
    priority_score = 72.3,
    detector_agreement = 2L,
    first_day = "2026-08-01",
    last_day = "2026-08-15",
    stringsAsFactors = FALSE
  )
  msg <- episodic_notify_build_new_clusters(details, 1L, "2026-08-15", NULL)

  expect_true(is.list(msg))
  expect_true(all(
    c("title", "plain", "html", "teams_card", "slack_text") %in% names(msg)
  ))
  expect_match(msg$title, "1 new cluster")
  expect_match(msg$plain, "MRSA")
  expect_match(msg$plain, "Hospital A")
  expect_match(msg$html, "<table")
  expect_match(msg$html, "MRSA")
  expect_match(msg$slack_text, "\\*MRSA\\*")
})

test_that("episodic_notify_build_new_clusters() caps at 10 and shows remainder", {
  details <- data.frame(
    cluster_id = 1:12,
    pathogen = paste0("pathogen_", 1:12),
    level = rep("pathogen_region", 12),
    institution_name = rep("", 12),
    ward = rep(NA_character_, 12),
    region_code = rep("NL", 12),
    n_cases = rep(3L, 12),
    expected = rep(1.0, 12),
    excess = rep(2.0, 12),
    ratio = rep(3.0, 12),
    priority_score = rep(50.0, 12),
    detector_agreement = rep(1L, 12),
    first_day = rep("2026-08-01", 12),
    last_day = rep("2026-08-15", 12),
    stringsAsFactors = FALSE
  )
  msg <- episodic_notify_build_new_clusters(details, 12L, "2026-08-15", NULL)
  expect_match(msg$plain, "and 2 more")
  expect_match(msg$title, "12 new clusters")
})

test_that("episodic_notify_build_failure() produces all message formats", {
  msg <- episodic_notify_build_failure("bad data", "2026-08-15", "myhost")

  expect_match(msg$title, "failed")
  expect_match(msg$plain, "bad data")
  expect_match(msg$plain, "myhost")
  expect_match(msg$html, "bad data")
  expect_match(msg$slack_text, "`bad data`")
})

test_that("episodic_notify_build_new_clusters() renders the Period column like the app", {
  details <- data.frame(
    cluster_id = 1L,
    pathogen = "MRSA",
    level = "pathogen_institution",
    institution_name = "Hospital A",
    ward = NA_character_,
    region_code = NA_character_,
    n_cases = 5L,
    expected = 1.2,
    excess = 3.8,
    ratio = 4.2,
    priority_score = 72.3,
    detector_agreement = 2L,
    first_day = "2026-08-01",
    last_day = "2026-08-15",
    stringsAsFactors = FALSE
  )
  msg <- episodic_notify_build_new_clusters(details, 1L, "2026-08-15", NULL)
  # The table carries the two case days as their own columns, formatted
  # the way the app formats a date; the one-line plain-text fallback is a
  # sentence, so it keeps the compact range.
  expect_match(
    msg$html,
    episodic_format_date("2026-08-01", lang = "en"),
    fixed = TRUE
  )
  expect_match(
    msg$html,
    episodic_format_date("2026-08-15", lang = "en"),
    fixed = TRUE
  )
  expect_match(msg$html, "15 days", fixed = TRUE)
  expect_match(
    msg$plain,
    episodic_format_date_range("2026-08-01", "2026-08-15", lang = "en"),
    fixed = TRUE
  )
})

test_that("episodic_notify_build_new_clusters() centre-aligns everything after the place", {
  details <- data.frame(
    cluster_id = 1L,
    pathogen = "MRSA",
    level = "pathogen_institution",
    institution_name = "Hospital A",
    ward = NA_character_,
    region_code = NA_character_,
    n_cases = 5L,
    expected = 1.2,
    excess = 3.8,
    ratio = 4.2,
    priority_score = 72.3,
    detector_agreement = 2L,
    first_day = "2026-08-01",
    last_day = "2026-08-15",
    stringsAsFactors = FALSE
  )
  msg <- episodic_notify_build_new_clusters(details, 1L, "2026-08-15", NULL)
  header <- sub(
    ".*(<tr style='background:#f0f0f0'>.*?</tr>).*",
    "\\1",
    msg$html,
    perl = TRUE
  )
  # cluster, pathogen and place read as text; cases, first case, last
  # case, duration, priority, expected and ratio are all numbers
  expect_equal(
    lengths(regmatches(header, gregexpr("text-align:left", header))),
    3
  )
  expect_equal(
    lengths(regmatches(header, gregexpr("text-align:center", header))),
    7
  )
  row <- sub(".*(<tr>.*?</tr>).*", "\\1", msg$html, perl = TRUE)
  expect_equal(lengths(regmatches(row, gregexpr("text-align:center", row))), 7)
})

test_that("episodic_notify_build_new_clusters() and episodic_notify_build_failure() honour lang", {
  details <- data.frame(
    cluster_id = 1L,
    pathogen = "MRSA",
    level = "pathogen_institution",
    institution_name = "Hospital A",
    ward = NA_character_,
    region_code = NA_character_,
    n_cases = 5L,
    expected = 1.2,
    excess = 3.8,
    ratio = 4.2,
    priority_score = 72.3,
    detector_agreement = 2L,
    first_day = "2026-08-01",
    last_day = "2026-08-15",
    stringsAsFactors = FALSE
  )
  msg_nl <- episodic_notify_build_new_clusters(
    details,
    1L,
    "2026-08-15",
    NULL,
    lang = "nl"
  )
  expect_match(msg_nl$title, "nieuw cluster")
  expect_match(msg_nl$title, "gedetecteerd")
  expect_match(msg_nl$html, "Verwekker")
  expect_match(msg_nl$html, "Prioriteit")

  msg_failed_nl <- episodic_notify_build_failure(
    "bad data",
    "2026-08-15",
    "myhost",
    lang = "nl"
  )
  expect_match(msg_failed_nl$title, "mislukt")
  expect_match(msg_failed_nl$plain, "Foutmelding")
})

test_that("episodic_notify_location() translates the level label", {
  row <- list(
    institution_name = "",
    ward = NA,
    region_code = NA,
    level = "pathogen_region"
  )
  expect_equal(episodic_notify_location(row, lang = "nl"), "regio")
})

test_that("episodic_notify_build_new_clusters() includes dashboard link when given", {
  details <- data.frame(
    cluster_id = 1L,
    pathogen = "MRSA",
    level = "pathogen_institution",
    institution_name = "Hospital A",
    ward = NA_character_,
    region_code = NA_character_,
    n_cases = 5L,
    expected = 1.2,
    excess = 3.8,
    ratio = 4.2,
    priority_score = 72.3,
    detector_agreement = 2L,
    first_day = "2026-08-01",
    last_day = "2026-08-15",
    stringsAsFactors = FALSE
  )
  msg <- episodic_notify_build_new_clusters(
    details,
    1L,
    "2026-08-15",
    "https://episodic.example.org"
  )
  expect_match(msg$plain, "https://episodic.example.org")
  expect_match(msg$html, "https://episodic.example.org")
  expect_match(msg$slack_text, "episodic.example.org")

  # and each cluster's id is the deep link into its own dossier, not
  # just a reference the reader then has to find in the queue
  expect_match(
    msg$html,
    "https://episodic.example.org?cluster=1",
    fixed = TRUE
  )
  expect_match(
    msg$plain,
    "https://episodic.example.org?cluster=1",
    fixed = TRUE
  )
  expect_match(
    msg$slack_text,
    "<https://episodic.example.org?cluster=1|#1>",
    fixed = TRUE
  )
})

test_that("episodic_notify_cluster_url() appends to whatever query the dashboard URL already has", {
  expect_null(episodic_notify_cluster_url(NULL, 1L))
  expect_null(episodic_notify_cluster_url("", 1L))
  expect_equal(
    episodic_notify_cluster_url("https://x.example.org", 7L),
    "https://x.example.org?cluster=7"
  )
  expect_equal(
    episodic_notify_cluster_url("https://x.example.org/?lang=nl", 7L),
    "https://x.example.org/?lang=nl&cluster=7"
  )
})

test_that("the notification names every cluster by id and shows them last case day first", {
  details <- data.frame(
    cluster_id = c(11L, 12L, 13L),
    pathogen = c("MRSA", "Norovirus", "VRE"),
    level = rep("pathogen_region", 3),
    institution_name = rep("", 3),
    ward = rep(NA_character_, 3),
    region_code = rep("NL", 3),
    n_cases = c(4L, 9L, 2L),
    expected = rep(1.0, 3),
    excess = rep(2.0, 3),
    ratio = rep(3.0, 3),
    priority_score = c(50, 61.4, 88.6),
    detector_agreement = rep(1L, 3),
    first_day = c("2026-01-02", "2026-02-01", "2026-03-01"),
    last_day = c("2026-01-06", "2026-03-20", "2026-03-20"),
    stringsAsFactors = FALSE
  )
  msg <- episodic_notify_build_new_clusters(details, 3L, "2026-03-21", NULL)

  # the id leads every row, exactly as it does on screen
  for (id in details$cluster_id) {
    expect_match(
      msg$html,
      episodic_tr("dossier.cluster_ref", id = id, lang = "en"),
      fixed = TRUE
    )
    expect_match(
      msg$plain,
      episodic_tr("dossier.cluster_ref", id = id, lang = "en"),
      fixed = TRUE
    )
  }

  # and the same order: 13 and 12 share a last day, so priority breaks
  # the tie, and 11 (last seen in January) comes last
  positions <- vapply(
    c(13L, 12L, 11L),
    function(id) {
      as.integer(regexpr(
        episodic_tr("dossier.cluster_ref", id = id, lang = "en"),
        msg$html,
        fixed = TRUE
      ))
    },
    integer(1)
  )
  expect_false(any(positions < 0))
  expect_equal(positions, sort(positions))
})

test_that("episodic_notify_validate_config() passes for disabled notifications", {
  config <- episodic_config_resolve(NA)
  expect_length(episodic_notify_validate_config(config), 0)
})

test_that("episodic_notify_validate_config() reports missing fields", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        ntfy = list(enabled = TRUE)
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("ntfy: server", problems)))
  expect_true(any(grepl("ntfy: topic", problems)))
})

test_that("episodic_notify_validate_config() reports missing SMTP fields", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        smtp = list(enabled = TRUE, host = "mail.example.com")
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("smtp: from", problems)))
  expect_true(any(grepl("smtp:.*recipient", problems)))
})

test_that("episodic_notify_validate_config() reports missing Teams webhook", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        teams = list(enabled = TRUE)
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("teams: webhook_url", problems)))
})

test_that("episodic_notify_validate_config() reports missing Slack webhook", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        slack = list(enabled = TRUE)
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("slack: webhook_url", problems)))
})

test_that("episodic_notify_validate_config() reports missing Microsoft 365 fields", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        microsoft365 = list(enabled = TRUE)
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("microsoft365: tenant_id", problems)))
  expect_true(any(grepl("microsoft365:.*recipient", problems)))
  # 'from' is only required for the client_secret (app-only) flow, not for
  # the delegated/cached-token flow
  expect_false(any(grepl("microsoft365: from", problems)))
})

test_that("episodic_notify_validate_config() requires 'from' only with client_secret", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        microsoft365 = list(
          enabled = TRUE,
          tenant_id = "contoso",
          client_secret = "s3cr3t",
          to = "team-lead@example.org"
        )
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_true(any(grepl("microsoft365: from", problems)))
})

test_that("episodic_notify_validate_config() does not require 'from' without client_secret", {
  config <- list(
    notifications = list(
      enabled = TRUE,
      channels = list(
        microsoft365 = list(
          enabled = TRUE,
          tenant_id = "contoso",
          to = "team-lead@example.org"
        )
      )
    )
  )
  problems <- episodic_notify_validate_config(config)
  expect_false(any(grepl("microsoft365: from", problems)))
})

test_that("episodic_notify_microsoft365_cached_token() errors loudly when nothing is cached", {
  testthat::skip_if_not_installed("AzureGraph")
  testthat::skip_if_not_installed("AzureAuth")
  local_mocked_bindings(
    list_graph_logins = function() list(),
    .package = "AzureGraph"
  )
  expect_error(
    episodic_notify_microsoft365_cached_token("contoso"),
    "no cached Azure AD token found for tenant 'contoso'"
  )
})

test_that("episodic_notify_microsoft365_cached_token() matches on a substring of the tenant", {
  testthat::skip_if_not_installed("AzureGraph")
  testthat::skip_if_not_installed("AzureAuth")
  fake_login <- list(tenant = "contoso.onmicrosoft.com", token = "the-token")
  local_mocked_bindings(
    list_graph_logins = function() list(contoso = list(hash1 = fake_login)),
    .package = "AzureGraph"
  )
  expect_identical(
    episodic_notify_microsoft365_cached_token("contoso"),
    "the-token"
  )
})

test_that("episodic_notify_microsoft365_cached_token() matches the tenant case-insensitively", {
  testthat::skip_if_not_installed("AzureGraph")
  testthat::skip_if_not_installed("AzureAuth")
  fake_login <- list(tenant = "Contoso.OnMicrosoft.com", token = "the-token")
  local_mocked_bindings(
    list_graph_logins = function() list(contoso = list(hash1 = fake_login)),
    .package = "AzureGraph"
  )
  expect_identical(
    episodic_notify_microsoft365_cached_token("CONTOSO"),
    "the-token"
  )
})

test_that("episodic_notify_microsoft365() opens 'from' as a shared mailbox under Option C", {
  testthat::skip_if_not_installed("Microsoft365R")
  testthat::skip_if_not_installed("AzureGraph")
  testthat::skip_if_not_installed("AzureAuth")

  fake_login <- list(tenant = "contoso.onmicrosoft.com", token = "the-token")
  local_mocked_bindings(
    list_graph_logins = function() list(contoso = list(hash1 = fake_login)),
    .package = "AzureGraph"
  )

  captured <- new.env()
  fake_outlook <- list(
    create_email = function(body, content_type, subject, to) {
      list(send = function() invisible(NULL))
    }
  )
  local_mocked_bindings(
    get_business_outlook = function(...) {
      captured$args <- list(...)
      fake_outlook
    },
    .package = "Microsoft365R"
  )

  episodic_notify_microsoft365(
    list(
      tenant_id = "contoso",
      to = "team-lead@example.org",
      from = "shared@example.org"
    ),
    list(title = "t", html = "<p>x</p>")
  )

  expect_equal(captured$args$shared_mbox_email, "shared@example.org")
  expect_equal(captured$args$tenant, "contoso")
})

test_that("episodic_notify_microsoft365() does not request a shared mailbox when 'from' is unset", {
  testthat::skip_if_not_installed("Microsoft365R")
  testthat::skip_if_not_installed("AzureGraph")
  testthat::skip_if_not_installed("AzureAuth")

  fake_login <- list(tenant = "contoso.onmicrosoft.com", token = "the-token")
  local_mocked_bindings(
    list_graph_logins = function() list(contoso = list(hash1 = fake_login)),
    .package = "AzureGraph"
  )

  captured <- new.env()
  fake_outlook <- list(
    create_email = function(body, content_type, subject, to) {
      list(send = function() invisible(NULL))
    }
  )
  local_mocked_bindings(
    get_business_outlook = function(...) {
      captured$args <- list(...)
      fake_outlook
    },
    .package = "Microsoft365R"
  )

  episodic_notify_microsoft365(
    list(tenant_id = "contoso", to = "team-lead@example.org"),
    list(title = "t", html = "<p>x</p>")
  )

  expect_false("shared_mbox_email" %in% names(captured$args))
})

test_that("episodic_notify_teams_card() produces valid JSON", {
  card <- episodic_notify_teams_card(
    "Test title",
    list(list(title = "Key", value = "Value")),
    "https://example.org"
  )
  parsed <- jsonlite::fromJSON(card, simplifyVector = FALSE)
  expect_equal(parsed$type, "message")
  expect_true(length(parsed$attachments) > 0)
  expect_equal(
    parsed$attachments[[1]]$content$type,
    "AdaptiveCard"
  )
})

test_that("episodic_notify_dispatch() catches channel errors without propagating", {
  channels <- list(
    fake = list(enabled = TRUE)
  )
  message <- list(title = "test", plain = "test")
  expect_silent(episodic_notify_dispatch(channels, message))
})

test_that("episodic_reconcile_stream() returns new_cluster_ids", {
  db_path <- tempfile(fileext = ".sqlite")
  con <- episodic_db_create(db_path)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db_path)
  })
  run_id <- episodic_db_run_start(con, "test", "test", Sys.Date())
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2025-01-01"),
    end_date = as.Date("2025-03-31")
  )
  episodic_cases_load(
    con,
    cases,
    utils::read.csv(
      system.file("config", "episodic_default_pathogen_config.csv", package = "EpiSODIC"),
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    ),
    run_id
  )
  episodic_lattice_enumerate(
    con,
    episodic_db_cases(con),
    episodic_db_institutions(con)
  )
  streams <- episodic_db_streams(con)
  if (nrow(streams) == 0) {
    skip("No streams created from synthetic data")
  }
  stream <- streams[1, ]
  detections <- episodic_detect_rare_trigger(
    con,
    episodic_db_cases(con),
    episodic_config_resolve(NA)
  )
  stream_det <- detections[detections$stream_id == stream$stream_id, ]
  if (nrow(stream_det) == 0) {
    stream_det <- episodic_detect_same_place(
      con,
      episodic_db_cases(con),
      episodic_db_institutions(con),
      episodic_config_resolve(NA)
    )
    stream_det <- stream_det[stream_det$stream_id == stream$stream_id, ]
  }
  if (nrow(stream_det) > 0) {
    for (j in seq_len(nrow(stream_det))) {
      d <- stream_det[j, ]
      stream_det$detection_id[j] <- episodic_db_detection_insert(
        con,
        run_id = run_id,
        stream_id = stream$stream_id,
        detector = d$detector,
        first_day = d$first_day,
        last_day = d$last_day,
        n_cases = d$n_cases,
        expected = d$expected,
        upperbound = d$upperbound,
        params_json = as.character(d$params)
      )
    }
  }
  result <- episodic_reconcile_stream(
    con,
    stream_id = stream$stream_id,
    detections = stream_det,
    case_free_days = 14,
    run_id = run_id,
    close_after_runs = 14,
    priority_score_fn = function(candidate) 50,
    has_assessment_fn = function(cluster_id) FALSE,
    verdict_fn = function(cluster_id) NA_character_
  )
  expect_true("new_cluster_ids" %in% names(result))
  expect_true(
    is.integer(result$new_cluster_ids) || is.numeric(result$new_cluster_ids)
  )
  expect_equal(length(result$new_cluster_ids), result$n_new)
})

test_that("episodic_notify_mime_message() builds a valid MIME header", {
  mime <- episodic_notify_mime_message(
    "from@example.org",
    c("to@example.org"),
    "Test Subject",
    "<p>Hello</p>"
  )
  expect_match(mime, "From: from@example.org")
  expect_match(mime, "To: to@example.org")
  expect_match(mime, "Subject: Test Subject")
  expect_match(mime, "Content-Type: text/html")
  expect_match(mime, "<p>Hello</p>")
})
