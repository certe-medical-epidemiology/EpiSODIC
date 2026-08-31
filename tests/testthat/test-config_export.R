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

test_that("episodic_config_mask_secrets() replaces known secret keys but leaves everything else alone", {
  notif <- list(
    enabled = TRUE,
    channels = list(
      smtp = list(host = "smtp.example.org", password = "s3cret"),
      teams = list(webhook_url = "https://hooks.example/token"),
      microsoft365 = list(tenant_id = "abc", client_secret = "s3cret2")
    )
  )
  masked <- episodic_config_mask_secrets(notif)
  expect_equal(masked$channels$smtp$host, "smtp.example.org")
  expect_equal(masked$channels$smtp$password, "***")
  expect_equal(masked$channels$teams$webhook_url, "***")
  expect_equal(masked$channels$microsoft365$client_secret, "***")
  expect_equal(masked$channels$microsoft365$tenant_id, "abc")
})

test_that("episodic_config_mask_secrets() returns NULL for NULL input", {
  expect_null(episodic_config_mask_secrets(NULL))
})

test_that("episodic_config_export() writes a zip containing the resolved config, without a database", {
  skip_if_not_installed("zip")
  out_dir <- tempfile("episodic_export_")
  path <- episodic_config_export(db_path = NA, output_dir = out_dir)
  expect_true(file.exists(path))
  expect_true(grepl("\\.zip$", path))

  extract_dir <- tempfile("episodic_export_extract_")
  zip::unzip(path, exdir = extract_dir)
  expect_true(file.exists(file.path(extract_dir, "config_resolved.yaml")))
  expect_true(file.exists(file.path(extract_dir, "config_manifest.txt")))
  expect_true(file.exists(file.path(extract_dir, "pathogen_config.csv")))

  resolved <- yaml::read_yaml(file.path(extract_dir, "config_resolved.yaml"))
  expect_equal(resolved$reconciliation$close_after_runs, 14)
})

test_that("episodic_config_export() masks notification secrets by default, and can include them on request", {
  skip_if_not_installed("zip")
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  db_path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(db_path))
  con2 <- episodic_db_connect(db_path)
  user_id <- episodic_db_app_user_insert(
    con2,
    "admin1",
    "Admin One",
    "a@x.nl",
    sodium::password_store("pw12345678"),
    is_admin = TRUE
  )
  episodic_db_app_config_event_insert(
    con2,
    user_id,
    "notifications",
    jsonlite::toJSON(
      list(channels = list(teams = list(webhook_url = "https://hooks.example/real-secret"))),
      auto_unbox = TRUE
    )
  )
  DBI::dbDisconnect(con2)

  out_dir <- tempfile("episodic_export_")
  masked_path <- episodic_config_export(db_path = db_path, output_dir = out_dir)
  extract_masked <- tempfile("episodic_export_extract_")
  zip::unzip(masked_path, exdir = extract_masked)
  resolved_masked <- yaml::read_yaml(file.path(extract_masked, "config_resolved.yaml"))
  expect_equal(resolved_masked$notifications$channels$teams$webhook_url, "***")

  unmasked_path <- episodic_config_export(
    db_path = db_path,
    output_dir = out_dir,
    include_secrets = TRUE
  )
  extract_unmasked <- tempfile("episodic_export_extract_")
  zip::unzip(unmasked_path, exdir = extract_unmasked)
  resolved_unmasked <- yaml::read_yaml(file.path(extract_unmasked, "config_resolved.yaml"))
  expect_equal(
    resolved_unmasked$notifications$channels$teams$webhook_url,
    "https://hooks.example/real-secret"
  )
})
