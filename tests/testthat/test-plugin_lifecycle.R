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

plugin_source_file <- function(id, spec = plugin_test_spec()) {
  dir <- tempfile("plugin_source_")
  withr::defer(unlink(dir, recursive = TRUE), envir = parent.frame())
  plugin_test_write(dir, id, spec)
}

test_that("a plugin is installed from a local file, checked first, and never over another without overwrite", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  episodic_plugin_install(plugin_source_file("share-regional"), episodic_config_path = instance$config_path)
  expect_true(file.exists(file.path(instance$dir, "share-regional.yaml")))
  expect_identical(episodic_plugin_list(instance$config_path)$status, "disabled")

  expect_error(
    episodic_plugin_install(plugin_source_file("share-regional"), episodic_config_path = instance$config_path),
    "already installed"
  )
  newer <- plugin_source_file("share-regional", plugin_test_spec(version = "0.2.0"))
  episodic_plugin_install(newer, overwrite = TRUE, episodic_config_path = instance$config_path)
  expect_identical(episodic_plugin_list(instance$config_path)$version, "0.2.0")
})

test_that("a file that is not a valid plugin is refused and nothing is written", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  bad <- plugin_source_file("typo", plugin_test_spec(permisions = list("cluster_summary")))
  expect_error(episodic_plugin_install(bad, episodic_config_path = instance$config_path), "permisions is not a key")
  expect_false(file.exists(file.path(instance$dir, "typo.yaml")))
  wrong_name <- file.path(tempdir(), "Share.yaml")
  writeLines("name: x", wrong_name)
  withr::defer(unlink(wrong_name))
  expect_error(episodic_plugin_install(wrong_name, episodic_config_path = instance$config_path), "not a valid plugin file name")
})

test_that("a plugin whose requirements are not met yet is installed, and listed as unavailable", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  spec <- plugin_test_spec(requires = list(packages = list("no.such.package.episodic")))
  episodic_plugin_install(plugin_source_file("needs-package", spec), episodic_config_path = instance$config_path)
  listed <- episodic_plugin_list(instance$config_path)
  expect_identical(listed$status, "unavailable")
  expect_match(listed$problems, "no.such.package.episodic", fixed = TRUE)
  expect_error(episodic_plugin_enable("needs-package", instance$config_path), "cannot be enabled")
})

test_that("a plugin is fetched over https, from a GitHub page address by its raw address", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  bytes <- readBin(plugin_source_file("share-regional"), "raw", 1e5)
  fetched <- NULL
  local_mocked_bindings(episodic_import_export_endpoint_read = function(endpoint) {
    fetched <<- endpoint$location
    bytes
  })
  episodic_plugin_install(
    "https://github.com/org/plugins/blob/v1.0.0/share-regional.yaml",
    episodic_config_path = instance$config_path
  )
  expect_identical(fetched, "https://raw.githubusercontent.com/org/plugins/v1.0.0/share-regional.yaml")
  expect_true(file.exists(file.path(instance$dir, "share-regional.yaml")))
  expect_error(
    episodic_plugin_install("http://example.org/share-regional.yaml", episodic_config_path = instance$config_path),
    "https://"
  )
})

test_that("enabling writes exactly what the plugin declares into the configuration, and disabling removes it", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  spec <- plugin_test_spec(secrets = list("REGIONAL_TOKEN"))
  episodic_plugin_install(plugin_source_file("share-regional", spec), episodic_config_path = instance$config_path)
  episodic_plugin_enable("share-regional", instance$config_path)
  config <- yaml::read_yaml(instance$config_path)
  expect_identical(config$plugins$enabled$`share-regional`, list(permissions = "cluster_summary", secrets = "REGIONAL_TOKEN"))
  expect_identical(readLines(instance$config_path)[1], "# Test instance")
  expect_identical(episodic_plugin_list(instance$config_path)$status, "enabled")

  expect_true(episodic_plugin_disable("share-regional", instance$config_path))
  expect_false(episodic_plugin_disable("share-regional", instance$config_path))
  expect_length(yaml::read_yaml(instance$config_path)$plugins$enabled, 0)
  expect_true(any(readLines(instance$config_path) == "  enabled: {}"))
})

test_that("an update that asks for more than was granted needs approval before it runs again", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  episodic_plugin_install(plugin_source_file("share-regional"), episodic_config_path = instance$config_path)
  episodic_plugin_enable("share-regional", instance$config_path)
  grown <- plugin_source_file("share-regional", plugin_test_spec(secrets = list("REGIONAL_TOKEN")))
  episodic_plugin_install(grown, overwrite = TRUE, episodic_config_path = instance$config_path)
  expect_identical(episodic_plugin_list(instance$config_path)$status, "needs_approval")
  episodic_plugin_enable("share-regional", instance$config_path)
  expect_identical(episodic_plugin_list(instance$config_path)$status, "enabled")
})

test_that("uninstalling disables the plugin and removes its file", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  episodic_plugin_install(plugin_source_file("share-regional"), episodic_config_path = instance$config_path)
  episodic_plugin_enable("share-regional", instance$config_path)
  episodic_plugin_uninstall("share-regional", instance$config_path)
  expect_false(file.exists(file.path(instance$dir, "share-regional.yaml")))
  expect_identical(nrow(episodic_plugin_list(instance$config_path)), 0L)
})

test_that("a plugin receives only the parts of the data it holds permission for", {
  setup <- app_read_setup()
  withr::defer(DBI::dbDisconnect(setup$con))
  plugin <- list(id = "p", version = "1", trigger = "button")
  config <- episodic_config_resolve(NA)
  context <- list(cluster_id = setup$cluster_id, user = list(username = "epi", role = "epidemiologist"))

  summary_only <- episodic_plugin_payload(setup$con, plugin, "cluster_summary", context, config, "nl")
  expect_identical(summary_only$cluster$cluster_id, setup$cluster_id)
  expect_identical(summary_only$cluster$pathogen, "Norovirus")
  expect_identical(summary_only$cluster$label, episodic_object_ref(setup$cluster_id, "outbreak", lang = "nl"))
  expect_false("cases" %in% names(summary_only))
  expect_identical(summary_only$language, "nl")
  expect_identical(summary_only$user$username, "epi")

  with_cases <- episodic_plugin_payload(setup$con, plugin, c("cluster_summary", "cluster_cases"), context, config, "en")
  expect_identical(nrow(with_cases$cases), 6L)
  expect_true("patient_key" %in% names(with_cases$cases))

  cases_only <- episodic_plugin_payload(setup$con, plugin, "cluster_cases", context, config, "en")
  expect_false("cluster" %in% names(cases_only))
})

test_that("a headless run is recorded with the exact file, what it was sent and what came back", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  setup <- app_read_setup()
  db_path <- setup$con@dbname
  withr::defer(DBI::dbDisconnect(setup$con))
  spec <- plugin_test_spec(steps = list(plugin_test_step(paste(
    "p <- jsonlite::read_json(Sys.getenv('EPISODIC_PAYLOAD'))",
    "cat('message=Shared ', p$cluster$label, '\\nlink=https://regional.example/1\\n', sep = '', file = Sys.getenv('EPISODIC_RESULT'))",
    sep = "\n"
  ))))
  episodic_plugin_install(plugin_source_file("share-regional", spec), episodic_config_path = instance$config_path)
  expect_error(episodic_plugin_run("share-regional", setup$cluster_id, db_path = db_path, episodic_config_path = instance$config_path), "disabled")
  episodic_plugin_enable("share-regional", instance$config_path)
  expect_error(episodic_plugin_run("share-regional", db_path = db_path, episodic_config_path = instance$config_path), "cluster_id")

  outcome <- episodic_plugin_run("share-regional", setup$cluster_id, db_path = db_path, episodic_config_path = instance$config_path)
  expect_identical(outcome$status, "success")
  expect_identical(outcome$message, paste("Shared", episodic_object_ref(setup$cluster_id, "outbreak", lang = "en")))

  event <- DBI::dbGetQuery(setup$con, "SELECT * FROM episodic_plugin_run_event")
  expect_identical(nrow(event), 1L)
  expect_identical(event$plugin_id, "share-regional")
  expect_identical(event$initiated_by, "headless")
  expect_identical(event$cluster_id, setup$cluster_id)
  expect_identical(event$permissions, '["cluster_summary"]')
  expect_identical(event$link, "https://regional.example/1")
  expect_identical(event$plugin_sha256, episodic_plugin_read(file.path(instance$dir, "share-regional.yaml"))$sha256)
})

test_that("a headless run that does not succeed is recorded and raised as a warning", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  setup <- app_read_setup()
  withr::defer(DBI::dbDisconnect(setup$con))
  spec <- plugin_test_spec(steps = list(plugin_test_step("stop('platform unreachable')")))
  episodic_plugin_install(plugin_source_file("share-regional", spec), episodic_config_path = instance$config_path)
  episodic_plugin_enable("share-regional", instance$config_path)
  expect_warning(
    outcome <- episodic_plugin_run("share-regional", setup$cluster_id, db_path = setup$con@dbname, episodic_config_path = instance$config_path),
    "platform unreachable"
  )
  expect_identical(outcome$status, "failed")
  expect_identical(DBI::dbGetQuery(setup$con, "SELECT status FROM episodic_plugin_run_event")$status, "failed")
})

test_that("plugins enabled for after a run run once the run has committed, and cannot fail it", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  writes_status <- plugin_test_spec(
    trigger = "run_committed", placement = NULL, label = NULL, permissions = list("run_summary"),
    steps = list(plugin_test_step(paste(
      "p <- jsonlite::read_json(Sys.getenv('EPISODIC_PAYLOAD'))",
      "cat('message=run ', p$run$run_id, ' ', p$run$status, ' opened ', length(p$run$new_clusters), '\\n', sep = '', file = Sys.getenv('EPISODIC_RESULT'))",
      sep = "\n"
    )))
  )
  breaks <- plugin_test_spec(
    trigger = "run_committed", placement = NULL, label = NULL, permissions = list("run_summary"),
    steps = list(plugin_test_step("stop('broken plugin')"))
  )
  episodic_plugin_install(plugin_source_file("after-run", writes_status), episodic_config_path = instance$config_path)
  episodic_plugin_install(plugin_source_file("broken-after-run", breaks), episodic_config_path = instance$config_path)
  episodic_plugin_enable("after-run", instance$config_path)
  episodic_plugin_enable("broken-after-run", instance$config_path)

  path <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(path))
  log <- testthat::capture_messages(run_id <- episodic_run_cron(
    db_path = path,
    cases = function() episodic_synthetic_cases(start_date = as.Date("2024-06-01"), end_date = as.Date("2024-08-31"), seed = 3),
    run_date = as.Date("2024-08-31"),
    episodic_config_path = instance$config_path
  ))
  con <- episodic_db_connect(path)
  withr::defer(DBI::dbDisconnect(con), priority = "first")
  expect_identical(episodic_db_latest_run(con)$status, "success")
  events <- DBI::dbGetQuery(con, "SELECT * FROM episodic_plugin_run_event ORDER BY plugin_id")
  expect_identical(events$plugin_id, c("after-run", "broken-after-run"))
  expect_identical(events$initiated_by, c("cron", "cron"))
  expect_identical(events$status, c("success", "failed"))
  expect_identical(events$run_id, c(run_id, run_id))
  opened <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_cluster")$n
  expect_identical(events$message[1], paste0("run ", run_id, " success opened ", opened))
  expect_true(any(grepl("Plugin broken-after-run ended with status failed", log, fixed = TRUE)))
  expect_false(any(grepl("dispatch failed", log, fixed = TRUE)))
})
