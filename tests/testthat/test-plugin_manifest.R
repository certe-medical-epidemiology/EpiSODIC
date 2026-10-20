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
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  spec <- plugin_test_spec(
    permissions = list("cluster_summary", "cluster_cases"),
    secrets = list("REGIONAL_TOKEN"),
    destination = "https://regional.example",
    source = "https://example.org/share-regional.yaml",
    timeout = 30L,
    requires = list(packages = list("jsonlite")),
    steps = list(plugin_test_step(env = list(MODE = "test", RETRIES = 3L, DRY = TRUE)))
  )
  plugin <- episodic_plugin_read(plugin_test_write(instance$dir, "share-regional", spec))
  expect_identical(plugin$problems, list())
  expect_identical(plugin$id, "share-regional")
  expect_identical(plugin$permissions, c("cluster_summary", "cluster_cases"))
  expect_identical(plugin$secrets, "REGIONAL_TOKEN")
  expect_identical(plugin$destination, "https://regional.example")
  expect_identical(plugin$timeout, 30L)
  expect_identical(plugin$steps[[1]]$env, c(MODE = "test", RETRIES = "3", DRY = "true"))
  expect_match(plugin$sha256, "^[0-9a-f]{64}$")
  expect_identical(episodic_plugin_label(plugin, lang = "nl"), "Delen")
  expect_identical(episodic_plugin_label(plugin, lang = "de"), "Share")
})

test_that("every problem with a plugin file is named, not only the first", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  spec <- plugin_test_spec(
    api = 2L,
    placement = "report",
    permissions = list("cluster_cases", "run_summary"),
    secrets = list("EPISODIC_DB"),
    timeout = 9999L,
    colour = "blue",
    version = NULL,
    steps = list(plugin_test_step(shell = "perl", when = "always"))
  )
  codes <- plugin_test_codes(episodic_plugin_read(plugin_test_write(instance$dir, "Bad_Name", spec)))
  expect_setequal(
    codes,
    c(
      "bad_id", "unknown_key", "missing_key", "api_unsupported", "bad_field",
      "bad_permission", "no_destination", "episodic_secret", "bad_timeout"
    )
  )
  expect_identical(sum(codes == "unknown_key"), 2L)
})

test_that("a hook cannot carry a button's placement or label", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  spec <- plugin_test_spec(trigger = "run_committed", permissions = list("run_summary"))
  codes <- plugin_test_codes(episodic_plugin_read(plugin_test_write(instance$dir, "hook", spec)))
  expect_identical(codes, c("button_only", "button_only"))
})

test_that("a file that is not YAML, or not a mapping, is not a plugin", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  dir.create(instance$dir)
  writeLines("name: [unclosed", file.path(instance$dir, "broken.yaml"))
  expect_identical(plugin_test_codes(episodic_plugin_read(file.path(instance$dir, "broken.yaml"))), "bad_yaml")
  writeLines(c("- a", "- b"), file.path(instance$dir, "list.yaml"))
  expect_identical(plugin_test_codes(episodic_plugin_read(file.path(instance$dir, "list.yaml"))), "not_mapping")
})

test_that("what a plugin requires is checked and never installed", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  local_mocked_bindings(episodic_plugin_shell_program = function(shell) if (shell == "bash") "" else "/usr/bin/true")
  spec <- plugin_test_spec(
    requires = list(packages = list("no.such.package.episodic"), commands = list("no-such-command-episodic")),
    steps = list(plugin_test_step(), plugin_test_step(run = "echo hi", shell = "bash"))
  )
  plugin <- episodic_plugin_read(plugin_test_write(instance$dir, "needs-things", spec))
  expect_identical(plugin_test_codes(plugin), c("missing_package", "missing_command", "missing_command"))
  expect_false(requireNamespace("no.such.package.episodic", quietly = TRUE))
})

test_that("a plugin's status follows from its file and from what the configuration grants it", {
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  plugin_test_write(instance$dir, "off")
  plugin_test_write(instance$dir, "on")
  plugin_test_write(instance$dir, "grew", plugin_test_spec(secrets = list("REGIONAL_TOKEN")))
  plugin_test_write(instance$dir, "broken", plugin_test_spec(trigger = "never"))
  grant <- list(permissions = list("cluster_summary"))
  for (id in c("on", "grew", "gone")) {
    episodic_config_set(c("plugins", "enabled", id), grant, instance$config_path)
  }

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
  instance <- plugin_test_instance()
  withr::local_envvar(instance$env)
  plugin_test_write(instance$dir, "share-regional")
  plugin_test_write(instance$dir, "old-api", plugin_test_spec(api = 0L))
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
