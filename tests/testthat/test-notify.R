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
      system.file("config", "pathogen_config.csv", package = "EpiSODIC"),
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
