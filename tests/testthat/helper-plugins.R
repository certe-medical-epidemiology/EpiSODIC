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

# Fixtures for the plugin tests: a configuration file and plugin folder
# of the calling test's own, and plugin files written from a list.

plugin_test_instance <- function(env = parent.frame()) {
  root <- tempfile("episodic_plugins_")
  dir.create(root)
  config_path <- file.path(root, "episodic.yaml")
  writeLines(c("# Test instance", "plugins:", "  enabled: {}"), config_path)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  withr::local_envvar(EPISODIC_CONFIG = config_path, EPISODIC_PLUGIN_DIR = NA, .local_envir = env)
  list(root = root, config_path = config_path, dir = file.path(root, "plugins"))
}

plugin_test_spec <- function(...) {
  spec <- list(
    name = "Share with the regional platform",
    version = "0.1.0",
    api = 1L,
    trigger = "button",
    placement = "outbreak",
    label = list(en = "Share", nl = "Delen"),
    permissions = list("cluster_summary"),
    steps = list(plugin_test_step())
  )
  # Assigned one by one: utils::modifyList() leaves an unnamed list as it
  # was, and NULL removes a key.
  overrides <- list(...)
  for (name in names(overrides)) {
    if (is.null(overrides[[name]])) {
      spec[[name]] <- NULL
    } else {
      spec[name] <- list(overrides[[name]])
    }
  }
  spec
}

plugin_test_step <- function(run = "cat('message=Shared\\n', file = Sys.getenv('EPISODIC_RESULT'))", shell = "r", ...) {
  list(name = "Send", shell = shell, run = run, ...)
}

plugin_test_write <- function(dir, id, spec = plugin_test_spec()) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, paste0(id, ".yaml"))
  writeLines(sub("\n$", "", yaml::as.yaml(spec)), path)
  path
}

plugin_test_codes <- function(plugin) {
  vapply(plugin$problems, function(p) p$code, character(1))
}
