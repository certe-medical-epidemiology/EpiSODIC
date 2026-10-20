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

# A plugin is one Quarto file in the plugin folder. Its YAML header says
# what it is and what it needs; the configuration file says whether it
# may run and what it was granted. Nothing about plugins is kept in the
# database except the record of each run (`episodic_plugin_run_event`),
# which is history and never decides what loads. See vignette("plugins").

#' The plugin API version this EpiSODIC provides
#'
#' A plugin names the version it was written for in `episodic.api`; one
#' naming any other is refused rather than handed a payload it may
#' misread. The version changes only when the payload or the result
#' contract changes incompatibly.
#' @keywords internal
#' @noRd
episodic_plugin_api_version <- 1L

#' What a plugin can be triggered by, and where a button can appear
#' @keywords internal
#' @noRd
episodic_plugin_triggers <- c("button", "run_committed")
episodic_plugin_placements <- c("outbreak", "epidemic")

#' The permissions a plugin can ask for, per trigger
#'
#' Each names a part of the payload: without it, that part is absent.
#' `cluster_cases` is the line list of a cluster's cases, pseudonymised
#' as stored; it is the one permission that sends case-level data, and
#' asking for it requires a declared `destination`.
#' @keywords internal
#' @noRd
episodic_plugin_permissions <- list(
  button = c("cluster_summary", "cluster_cases"),
  run_committed = c("run_summary")
)

#' The engines a plugin's chunks can be written in
#' @keywords internal
#' @noRd
episodic_plugin_engines <- c("r", "python", "bash")

#' The longest a plugin may run, in seconds
#' @keywords internal
#' @noRd
episodic_plugin_timeout_max <- 600L
episodic_plugin_timeout_default <- 60L

#' The folder plugins live in
#'
#' `EPISODIC_PLUGIN_DIR` when set; otherwise a `plugins` folder beside the
#' configuration file. With neither there is no folder, and plugins are
#' unavailable rather than written into a temporary or package directory.
#'
#' @param episodic_config_path The configuration file.
#' @return The folder's path (which need not exist yet), or `NULL`.
#' @keywords internal
#' @noRd
episodic_plugin_dir <- function(episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  dir <- Sys.getenv("EPISODIC_PLUGIN_DIR", unset = NA)
  if (!is.na(dir) && nzchar(dir)) {
    return(dir)
  }
  if (!is.na(episodic_config_path) && nzchar(episodic_config_path)) {
    return(file.path(dirname(episodic_config_path), "plugins"))
  }
  NULL
}

#' Whether a plugin id is well formed
#'
#' The file name without `.qmd`: lowercase letters, digits and hyphens,
#' starting with a letter or digit, so it is safe as a file name, a
#' configuration key and an HTML id on every platform.
#' @keywords internal
#' @noRd
episodic_plugin_id_valid <- function(id) {
  is.character(id) && length(id) == 1 && !is.na(id) && grepl("^[a-z0-9][a-z0-9-]{0,62}$", id)
}

#' A problem with a plugin, as a reason code and its details
#' @keywords internal
#' @noRd
episodic_plugin_problem <- function(code, ...) {
  list(code = code, args = list(...))
}

#' A plugin problem as text, in a language
#' @keywords internal
#' @noRd
episodic_plugin_problem_text <- function(problem, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  do.call(episodic_tr, c(list(paste0("plugins.problem.", problem$code)), problem$args, list(lang = lang)))
}

#' Read the YAML header of a Quarto file
#'
#' @param path The file.
#' @return A list with `header` (the parsed header, or `NULL`) and
#'   `problem` (`NULL`, or why it could not be read).
#' @keywords internal
#' @noRd
episodic_plugin_read_header <- function(path) {
  lines <- tryCatch(readLines(path, encoding = "UTF-8", warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) {
    return(list(header = NULL, problem = episodic_plugin_problem("unreadable")))
  }
  lines <- sub("\r$", "", lines)
  if (length(lines) == 0 || !identical(trimws(lines[1]), "---")) {
    return(list(header = NULL, problem = episodic_plugin_problem("no_header")))
  }
  close <- which(trimws(lines[-1]) %in% c("---", "...")) + 1L
  if (length(close) == 0) {
    return(list(header = NULL, problem = episodic_plugin_problem("no_header")))
  }
  header <- tryCatch(
    yaml::yaml.load(paste(lines[2:(close[1] - 1L)], collapse = "\n")),
    error = function(e) e,
    warning = function(w) w
  )
  if (inherits(header, "condition")) {
    return(list(header = NULL, problem = episodic_plugin_problem("bad_header", error = conditionMessage(header))))
  }
  if (!is.list(header) || is.null(names(header))) {
    return(list(header = NULL, problem = episodic_plugin_problem("no_header")))
  }
  list(header = header, problem = NULL)
}

#' A per-language label, with English required
#' @keywords internal
#' @noRd
episodic_plugin_label_valid <- function(x) {
  is.list(x) && !is.null(names(x)) && all(nzchar(names(x))) &&
    all(vapply(x, function(v) is.character(v) && length(v) == 1 && !is.na(v) && nzchar(trimws(v)), logical(1))) &&
    "en" %in% names(x)
}

#' A string list from YAML: `NULL` stays empty, a scalar becomes one item
#' @keywords internal
#' @noRd
episodic_plugin_strings <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  if (is.list(x)) {
    if (!all(vapply(x, function(v) is.character(v) && length(v) == 1, logical(1)))) {
      return(NULL)
    }
    return(unlist(x, use.names = FALSE))
  }
  if (is.character(x)) x else NULL
}

#' Read and check one plugin file
#'
#' Every problem is collected, so the Plugins screen can name them all at
#' once. A plugin with any problem is unavailable: it is listed, with the
#' reasons, and can neither be enabled nor run.
#'
#' @param path The `.qmd` file.
#' @return A list describing the plugin: `id`, `path`, `sha256`, `title`,
#'   `version`, `source`, `trigger`, `placement`, `label`, `description`,
#'   `permissions`, `env`, `destination`, `packages`, `engines`, `timeout`,
#'   and `problems` (a list of `episodic_plugin_problem()`s).
#' @keywords internal
#' @noRd
episodic_plugin_read <- function(path) {
  id <- sub("\\.qmd$", "", basename(path))
  plugin <- list(
    id = id, path = path, sha256 = NA_character_, title = id, version = NA_character_,
    source = NA_character_, trigger = NA_character_, placement = NA_character_,
    label = NULL, description = NULL, permissions = character(0), env = character(0),
    destination = NA_character_, packages = character(0), engines = character(0),
    timeout = episodic_plugin_timeout_default, problems = list()
  )
  problems <- list()
  add <- function(code, ...) problems[[length(problems) + 1L]] <<- episodic_plugin_problem(code, ...)

  if (!episodic_plugin_id_valid(id)) {
    add("bad_id")
  }
  bytes <- tryCatch(readBin(path, "raw", file.info(path)$size), error = function(e) NULL)
  if (!is.null(bytes)) {
    plugin$sha256 <- sodium::bin2hex(sodium::sha256(bytes))
  }
  read <- episodic_plugin_read_header(path)
  if (!is.null(read$problem)) {
    plugin$problems <- c(problems, list(read$problem))
    return(plugin)
  }
  header <- read$header

  if (is.character(header$title) && length(header$title) == 1 && nzchar(header$title)) {
    plugin$title <- header$title
  }
  if (!is.null(header$version) && length(header$version) == 1 && nzchar(as.character(header$version))) {
    plugin$version <- as.character(header$version)
  } else {
    add("bad_field", field = "version")
  }
  if (!identical(header$engine, "knitr")) {
    add("bad_engine")
  }
  ep <- header$episodic
  if (!is.list(ep) || is.null(names(ep))) {
    add("no_episodic")
    plugin$problems <- problems
    return(plugin)
  }

  api <- suppressWarnings(as.integer(ep$api))
  if (length(api) != 1 || is.na(api)) {
    add("bad_field", field = "episodic.api")
  } else if (api != episodic_plugin_api_version) {
    add("api_unsupported", api = as.character(api), supported = as.character(episodic_plugin_api_version))
  }

  if (is.character(ep$trigger) && length(ep$trigger) == 1 && ep$trigger %in% episodic_plugin_triggers) {
    plugin$trigger <- ep$trigger
  } else {
    add("bad_field", field = "episodic.trigger")
  }

  if (identical(plugin$trigger, "button")) {
    if (is.character(ep$placement) && length(ep$placement) == 1 && ep$placement %in% episodic_plugin_placements) {
      plugin$placement <- ep$placement
    } else {
      add("bad_field", field = "episodic.placement")
    }
    if (episodic_plugin_label_valid(ep$label)) {
      plugin$label <- ep$label
    } else {
      add("bad_field", field = "episodic.label")
    }
  }
  if (!is.null(ep$description)) {
    if (episodic_plugin_label_valid(ep$description)) {
      plugin$description <- ep$description
    } else {
      add("bad_field", field = "episodic.description")
    }
  }

  permissions <- episodic_plugin_strings(ep$permissions)
  if (is.null(permissions) || anyDuplicated(permissions)) {
    add("bad_field", field = "episodic.permissions")
  } else {
    allowed <- if (is.na(plugin$trigger)) character(0) else episodic_plugin_permissions[[plugin$trigger]]
    unknown <- setdiff(permissions, allowed)
    if (length(unknown) > 0) {
      add("bad_permission", permission = paste(unknown, collapse = ", "))
    }
    plugin$permissions <- permissions
  }

  env <- episodic_plugin_strings(ep$env)
  if (is.null(env) || anyDuplicated(env) || !all(grepl("^[A-Za-z_][A-Za-z0-9_]*$", env))) {
    add("bad_field", field = "episodic.env")
  } else if (any(grepl("^EPISODIC_", env))) {
    add("episodic_env", variable = paste(env[grepl("^EPISODIC_", env)], collapse = ", "))
  } else {
    plugin$env <- env
  }

  if (!is.null(ep$destination)) {
    if (is.character(ep$destination) && length(ep$destination) == 1 && nzchar(trimws(ep$destination))) {
      plugin$destination <- trimws(ep$destination)
    } else {
      add("bad_field", field = "episodic.destination")
    }
  }
  if ("cluster_cases" %in% plugin$permissions && is.na(plugin$destination)) {
    add("no_destination")
  }

  if (!is.null(ep$source)) {
    if (is.character(ep$source) && length(ep$source) == 1 && grepl("^https://", ep$source)) {
      plugin$source <- ep$source
    } else {
      add("bad_field", field = "episodic.source")
    }
  }

  if (!is.null(ep$timeout)) {
    timeout <- suppressWarnings(as.integer(ep$timeout))
    if (length(timeout) != 1 || is.na(timeout) || timeout < 1 || timeout > episodic_plugin_timeout_max ||
      !identical(as.numeric(ep$timeout), as.numeric(timeout))) {
      add("bad_timeout", max = as.character(episodic_plugin_timeout_max))
    } else {
      plugin$timeout <- timeout
    }
  }

  requires <- ep$requires %||% list()
  if (!is.list(requires) || (length(requires) > 0 && is.null(names(requires))) ||
    length(setdiff(names(requires), c("packages", "engines"))) > 0) {
    add("bad_field", field = "episodic.requires")
  } else {
    packages <- episodic_plugin_strings(requires$packages)
    engines <- episodic_plugin_strings(requires$engines)
    if (is.null(packages)) {
      add("bad_field", field = "episodic.requires.packages")
    } else {
      plugin$packages <- packages
    }
    if (is.null(engines) || length(setdiff(engines, episodic_plugin_engines)) > 0) {
      add("bad_field", field = "episodic.requires.engines")
    } else {
      plugin$engines <- engines
    }
  }

  plugin$problems <- c(problems, episodic_plugin_requirements(plugin))
  plugin
}

#' Whether what a plugin requires is present on this server
#'
#' Checked, never installed: EpiSODIC installs no software on a plugin's
#' behalf. Packages are looked for in the library paths the plugin will
#' run with, which are this process's own.
#' @keywords internal
#' @noRd
episodic_plugin_requirements <- function(plugin) {
  problems <- list()
  add <- function(code, ...) problems[[length(problems) + 1L]] <<- episodic_plugin_problem(code, ...)
  if (!episodic_quarto_available()) {
    add("no_quarto")
  }
  for (package in plugin$packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      add("missing_package", package = package)
    }
  }
  if ("python" %in% plugin$engines) {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      add("missing_package", package = "reticulate")
    }
    if (!nzchar(Sys.which("python3")) && !nzchar(Sys.which("python"))) {
      add("missing_engine", engine = "Python")
    }
  }
  if ("bash" %in% plugin$engines && !nzchar(Sys.which("bash"))) {
    add("missing_engine", engine = "Bash")
  }
  problems
}

#' What the configuration grants an enabled plugin
#'
#' @param config The resolved configuration.
#' @param id The plugin's id.
#' @return `NULL` when the plugin is not enabled, else a list with
#'   `permissions` and `env`.
#' @keywords internal
#' @noRd
episodic_plugin_grant <- function(config, id) {
  enabled <- config$plugins$enabled
  if (!is.list(enabled) || !(id %in% names(enabled))) {
    return(NULL)
  }
  entry <- enabled[[id]]
  if (!is.list(entry)) {
    entry <- list()
  }
  list(
    permissions = episodic_plugin_strings(entry$permissions) %||% character(0),
    env = episodic_plugin_strings(entry$env) %||% character(0)
  )
}

#' Every plugin in the folder and every plugin the configuration enables
#'
#' A plugin's status is one of:
#' - `"enabled"`: listed in `plugins.enabled`, with everything it declares granted;
#' - `"disabled"`: in the folder, not listed;
#' - `"needs_approval"`: listed, but the file now declares a permission or
#'   variable it was not granted, so it does not run until enabled again;
#' - `"unavailable"`: the file has a problem;
#' - `"missing"`: listed in the configuration, but no file is there.
#'
#' @param config The resolved configuration.
#' @param dir The plugin folder, or `NULL`.
#' @return A list of plugin descriptions, each with `status` and, where
#'   it applies, `grant` and `ungranted`, ordered by id.
#' @keywords internal
#' @noRd
episodic_plugins <- function(config = episodic_config_resolve(), dir = episodic_plugin_dir()) {
  files <- if (!is.null(dir) && dir.exists(dir)) {
    sort(list.files(dir, pattern = "\\.qmd$", full.names = TRUE))
  } else {
    character(0)
  }
  plugins <- lapply(files, episodic_plugin_read)
  names(plugins) <- vapply(plugins, function(p) p$id, character(1))

  for (id in names(plugins)) {
    plugin <- plugins[[id]]
    grant <- episodic_plugin_grant(config, id)
    plugin$grant <- grant
    plugin$ungranted <- if (is.null(grant)) {
      character(0)
    } else {
      c(setdiff(plugin$permissions, grant$permissions), setdiff(plugin$env, grant$env))
    }
    plugin$status <- if (length(plugin$problems) > 0) {
      "unavailable"
    } else if (is.null(grant)) {
      "disabled"
    } else if (length(plugin$ungranted) > 0) {
      "needs_approval"
    } else {
      "enabled"
    }
    plugins[[id]] <- plugin
  }

  for (id in setdiff(names(config$plugins$enabled %||% list()), names(plugins))) {
    plugins[[id]] <- list(
      id = id, path = if (is.null(dir)) NA_character_ else file.path(dir, paste0(id, ".qmd")),
      title = id, version = NA_character_, status = "missing",
      problems = list(episodic_plugin_problem("file_missing")),
      grant = episodic_plugin_grant(config, id), ungranted = character(0)
    )
  }
  plugins[order(names(plugins))]
}

#' The plugins that may run now for one trigger and placement
#' @keywords internal
#' @noRd
episodic_plugins_runnable <- function(plugins, trigger, placement = NULL) {
  Filter(function(p) {
    identical(p$status, "enabled") && identical(p$trigger, trigger) &&
      (is.null(placement) || identical(p$placement, placement))
  }, plugins)
}

#' A plugin's label in a language, falling back to English
#' @keywords internal
#' @noRd
episodic_plugin_label <- function(plugin, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  label <- plugin$label
  if (is.null(label)) {
    return(plugin$title)
  }
  lang <- episodic_lang(lang)
  base <- sub("-.*$", "", lang)
  label[[lang]] %||% label[[base]] %||% label[["en"]]
}

#' List the Installed Plugins
#'
#' Lists every plugin in the plugin folder and every plugin the
#' configuration enables, with its status and, when it cannot run, why.
#' See `vignette("plugins")`.
#'
#' The plugin folder is `EPISODIC_PLUGIN_DIR`, or a `plugins` folder beside
#' the configuration file (`EPISODIC_CONFIG`) when that is unset.
#'
#' @param episodic_config_path The configuration file. Defaults to the
#'   `EPISODIC_CONFIG` environment variable.
#' @return A data frame with one row per plugin: `id`, `title`,
#'   `version`, `trigger`, `placement`, `status` (`"enabled"`,
#'   `"disabled"`, `"needs_approval"`, `"unavailable"` or `"missing"`),
#'   `permissions`, `env`, `destination` and `problems` (the reasons, as
#'   text, separated by `"; "`).
#' @seealso [episodic_plugin_install()], [episodic_plugin_enable()],
#'   [episodic_plugin_run()], `vignette("plugins")`
#' @examples
#' \dontrun{
#' episodic_plugin_list()
#' }
#' @export
episodic_plugin_list <- function(episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  plugins <- episodic_plugins(
    episodic_config_resolve(episodic_config_path),
    episodic_plugin_dir(episodic_config_path)
  )
  text <- function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_character_ else paste(x, collapse = ", ")
  data.frame(
    id = vapply(plugins, function(p) p$id, character(1)),
    title = vapply(plugins, function(p) p$title %||% p$id, character(1)),
    version = vapply(plugins, function(p) p$version %||% NA_character_, character(1)),
    trigger = vapply(plugins, function(p) p$trigger %||% NA_character_, character(1)),
    placement = vapply(plugins, function(p) p$placement %||% NA_character_, character(1)),
    status = vapply(plugins, function(p) p$status, character(1)),
    permissions = vapply(plugins, function(p) text(p$permissions), character(1)),
    env = vapply(plugins, function(p) text(p$env), character(1)),
    destination = vapply(plugins, function(p) p$destination %||% NA_character_, character(1)),
    problems = vapply(plugins, function(p) {
      if (length(p$problems) == 0) {
        NA_character_
      } else {
        paste(vapply(p$problems, episodic_plugin_problem_text, character(1), lang = "en"), collapse = "; ")
      }
    }, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
