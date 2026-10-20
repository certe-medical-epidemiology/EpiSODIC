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

test_that("the plugin folder is EPISODIC_PLUGIN_DIR, else beside the configuration file, else none", {
  withr::local_envvar(EPISODIC_PLUGIN_DIR = NA)
  expect_identical(episodic_plugin_dir("/srv/episodic/episodic.yaml"), file.path("/srv/episodic", "plugins"))
  expect_null(episodic_plugin_dir(NA))
  withr::local_envvar(EPISODIC_PLUGIN_DIR = "/opt/plugins")
  expect_identical(episodic_plugin_dir("/srv/episodic/episodic.yaml"), "/opt/plugins")
})

test_that("a well-formed plugin reads with everything it declares and no problems", {
  local_mocked_bindings(episodic_quarto_available = function() TRUE)
  instance <- plugin_test_instance()
  header <- plugin_test_header(
    permissions = list("cluster_summary", "cluster_cases"),
    env = list("REGIONAL_TOKEN"),
    destination = "https://regional.example",
    source = "https://example.org/share-regional.qmd",
    timeout = 30L,
    requires = list(packages = list("jsonlite"))
  )
  plugin <- episodic_plugin_read(plugin_test_write(instance$dir, "share-regional", header))
  expect_identical(plugin$problems, list())
  expect_identical(plugin$id, "share-regional")
  expect_identical(plugin$permissions, c("cluster_summary", "cluster_cases"))
  expect_identical(plugin$env, "REGIONAL_TOKEN")
  expect_identical(plugin$destination, "https://regional.example")
  expect_identical(plugin$timeout, 30L)
  expect_match(plugin$sha256, "^[0-9a-f]{64}$")
  expect_identical(episodic_plugin_label(plugin, lang = "nl"), "Delen")
  expect_identical(episodic_plugin_label(plugin, lang = "de"), "Share")
})

test_that("every problem with a plugin file is named, not only the first", {
  local_mocked_bindings(episodic_quarto_available = function() TRUE)
  instance <- plugin_test_instance()
  header <- plugin_test_header(
    api = 2L,
    placement = "report",
    permissions = list("cluster_cases", "run_summary"),
    env = list("EPISODIC_DB"),
    timeout = 9999L
  )
  header$engine <- "jupyter"
  codes <- plugin_test_codes(episodic_plugin_read(plugin_test_write(instance$dir, "Bad_Name", header)))
  expect_setequal(
    codes,
    c("bad_id", "bad_engine", "api_unsupported", "bad_field", "bad_permission", "episodic_env", "no_destination", "bad_timeout")
  )
})

test_that("a file without a header, or without an episodic section, is not a plugin", {
  instance <- plugin_test_instance()
  dir.create(instance$dir)
  writeLines("just text", file.path(instance$dir, "notes.qmd"))
  expect_identical(plugin_test_codes(episodic_plugin_read(file.path(instance$dir, "notes.qmd"))), c("no_header"))
  writeLines(c("---", "title: [unclosed", "---"), file.path(instance$dir, "broken.qmd"))
  expect_identical(plugin_test_codes(episodic_plugin_read(file.path(instance$dir, "broken.qmd"))), "bad_header")
  header <- plugin_test_header()
  header$episodic <- NULL
  expect_true("no_episodic" %in% plugin_test_codes(episodic_plugin_read(plugin_test_write(instance$dir, "plain", header))))
})

test_that("what a plugin requires is checked and never installed", {
  local_mocked_bindings(episodic_quarto_available = function() FALSE)
  instance <- plugin_test_instance()
  header <- plugin_test_header(requires = list(packages = list("no.such.package.episodic")))
  plugin <- episodic_plugin_read(plugin_test_write(instance$dir, "needs-things", header))
  expect_setequal(plugin_test_codes(plugin), c("no_quarto", "missing_package"))
  expect_false(requireNamespace("no.such.package.episodic", quietly = TRUE))
})

test_that("a plugin's status follows from its file and from what the configuration grants it", {
  local_mocked_bindings(episodic_quarto_available = function() TRUE)
  instance <- plugin_test_instance()
  plugin_test_write(instance$dir, "off")
  plugin_test_write(instance$dir, "on")
  plugin_test_write(instance$dir, "grew", plugin_test_header(env = list("REGIONAL_TOKEN")))
  plugin_test_write(instance$dir, "broken", plugin_test_header(trigger = "never"))
  episodic_config_set(c("plugins", "enabled", "on"), list(permissions = list("cluster_summary")), instance$config_path)
  episodic_config_set(c("plugins", "enabled", "grew"), list(permissions = list("cluster_summary")), instance$config_path)
  episodic_config_set(c("plugins", "enabled", "gone"), list(permissions = list("cluster_summary")), instance$config_path)

  plugins <- episodic_plugins(episodic_config_resolve(instance$config_path), instance$dir)
  status <- vapply(plugins, function(p) p$status, character(1))
  expect_identical(
    status,
    c(broken = "unavailable", gone = "missing", grew = "needs_approval", off = "disabled", on = "enabled")
  )
  expect_identical(plugins$grew$ungranted, "REGIONAL_TOKEN")
  expect_identical(names(episodic_plugins_runnable(plugins, "button", "outbreak")), "on")
  expect_length(episodic_plugins_runnable(plugins, "button", "epidemic"), 0)
})

test_that("episodic_plugin_list() lists every plugin with its status and its problems as text", {
  local_mocked_bindings(episodic_quarto_available = function() TRUE)
  instance <- plugin_test_instance()
  plugin_test_write(instance$dir, "share-regional")
  plugin_test_write(instance$dir, "old-api", plugin_test_header(api = 0L))
  listed <- episodic_plugin_list(instance$config_path)
  expect_identical(listed$id, c("old-api", "share-regional"))
  expect_identical(listed$status, c("unavailable", "disabled"))
  expect_match(listed$problems[1], "plugin API version 0", fixed = TRUE)
  expect_true(is.na(listed$problems[2]))
})

test_that("without a configuration file or plugin folder there are no plugins, and no error", {
  withr::local_envvar(EPISODIC_CONFIG = NA, EPISODIC_PLUGIN_DIR = NA)
  expect_identical(nrow(episodic_plugin_list(NA)), 0L)
})
