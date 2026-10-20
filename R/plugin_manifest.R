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

# A plugin is one YAML file in the plugin folder: what it is, what it
# needs, and the steps it runs. The configuration file says whether it
# may run and what it was granted. Nothing about plugins is kept in the
# database except the record of each run (`episodic_plugin_run_event`),
# which is history and never decides what loads. See vignette("plugins").

#' The plugin API version this EpiSODIC provides
#'
#' A plugin names the version it was written for in `api`; one naming
#' any other is refused rather than handed a payload it may misread. The
#' version changes only when the file format, the payload or the result
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

#' The languages a step can be written in, and the program that runs each
#'
#' `r` runs with this R installation's own `Rscript`, so a plugin sees the
#' same packages EpiSODIC does.
#' @keywords internal
#' @noRd
episodic_plugin_shells <- c("r", "bash", "python")

#' The keys a plugin file may hold, and those it must
#' @keywords internal
#' @noRd
episodic_plugin_keys <- c(
  "name", "version", "api", "description", "source", "trigger", "placement",
  "label", "permissions", "destination", "secrets", "requires", "timeout", "steps"
)
episodic_plugin_keys_required <- c("name", "version", "api", "trigger", "steps")
episodic_plugin_step_keys <- c("name", "shell", "run", "env")

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
#' The file name without `.yaml`: lowercase letters, digits and hyphens,
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

#' One line of text: a string, not empty
#' @keywords internal
#' @noRd
episodic_plugin_is_text <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(x))
}

#' A per-language text, with English required
#' @keywords internal
#' @noRd
episodic_plugin_label_valid <- function(x) {
  is.list(x) && length(x) > 0 && !is.null(names(x)) && all(nzchar(names(x))) &&
    all(vapply(x, episodic_plugin_is_text, logical(1))) && "en" %in% names(x)
}

#' A string list from YAML: `NULL` stays empty, a scalar becomes one item
#' @keywords internal
#' @noRd
episodic_plugin_strings <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  if (is.list(x)) {
    if (!all(vapply(x, function(v) is.character(v) && length(v) == 1 && !is.na(v), logical(1)))) {
      return(NULL)
    }
    return(unlist(x, use.names = FALSE))
  }
  if (is.character(x) && !anyNA(x)) x else NULL
}

#' Whether a name is usable as an environment variable
#' @keywords internal
#' @noRd
episodic_plugin_env_name_valid <- function(x) {
  grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)
}

#' Read and check one plugin file
#'
#' Every problem is collected, so the Plugins screen can name them all at
#' once. A plugin with any problem is unavailable: it is listed, with the
#' reasons, and can neither be enabled nor run. Keys the format does not
#' define are problems too, so a misspelt key is named rather than
#' ignored.
#'
#' @param path The `.yaml` file.
#' @return A list describing the plugin: `id`, `path`, `sha256`, `name`,
#'   `version`, `description`, `source`, `trigger`, `placement`, `label`,
#'   `permissions`, `destination`, `secrets`, `packages`, `commands`,
#'   `timeout`, `steps`, and `problems` (a list of
#'   `episodic_plugin_problem()`s).
#' @keywords internal
#' @noRd
episodic_plugin_read <- function(path) {
  id <- sub("\\.yaml$", "", basename(path))
  plugin <- list(
    id = id, path = path, sha256 = NA_character_, name = id, version = NA_character_,
    description = NULL, source = NA_character_, trigger = NA_character_,
    placement = NA_character_, label = NULL, permissions = character(0),
    destination = NA_character_, secrets = character(0), packages = character(0),
    commands = character(0), timeout = episodic_plugin_timeout_default,
    steps = list(), problems = list()
  )
  problems <- list()
  add <- function(code, ...) problems[[length(problems) + 1L]] <<- episodic_plugin_problem(code, ...)
  done <- function() {
    plugin$problems <- problems
    plugin
  }

  if (!episodic_plugin_id_valid(id)) {
    add("bad_id")
  }
  bytes <- tryCatch(readBin(path, "raw", file.info(path)$size), error = function(e) NULL)
  if (is.null(bytes)) {
    add("unreadable")
    return(done())
  }
  plugin$sha256 <- sodium::bin2hex(sodium::sha256(bytes))
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  spec <- tryCatch(
    yaml::yaml.load(text),
    error = function(e) e,
    warning = function(w) w
  )
  if (inherits(spec, "condition")) {
    add("bad_yaml", error = conditionMessage(spec))
    return(done())
  }
  if (!is.list(spec) || length(spec) == 0 || is.null(names(spec))) {
    add("not_mapping")
    return(done())
  }

  for (key in setdiff(names(spec), episodic_plugin_keys)) {
    add("unknown_key", field = key)
  }
  for (key in setdiff(episodic_plugin_keys_required, names(spec))) {
    add("missing_key", field = key)
  }

  if (!is.null(spec$name)) {
    if (episodic_plugin_is_text(spec$name)) plugin$name <- trimws(spec$name) else add("bad_field", field = "name")
  }
  if (!is.null(spec$version)) {
    version <- if (is.numeric(spec$version) || is.character(spec$version)) as.character(spec$version) else NA_character_
    if (length(version) == 1 && !is.na(version) && nzchar(version)) plugin$version <- version else add("bad_field", field = "version")
  }
  if (!is.null(spec$api)) {
    api <- if (is.numeric(spec$api) && length(spec$api) == 1 && spec$api == round(spec$api)) as.integer(spec$api) else NA_integer_
    if (is.na(api)) {
      add("bad_field", field = "api")
    } else if (api != episodic_plugin_api_version) {
      add("api_unsupported", api = as.character(api), supported = as.character(episodic_plugin_api_version))
    }
  }
  if (!is.null(spec$description)) {
    if (episodic_plugin_label_valid(spec$description)) plugin$description <- spec$description else add("bad_field", field = "description")
  }
  if (!is.null(spec$source)) {
    if (episodic_plugin_is_text(spec$source) && grepl("^https://", spec$source)) {
      plugin$source <- spec$source
    } else {
      add("bad_field", field = "source")
    }
  }

  if (!is.null(spec$trigger)) {
    if (episodic_plugin_is_text(spec$trigger) && spec$trigger %in% episodic_plugin_triggers) {
      plugin$trigger <- spec$trigger
    } else {
      add("bad_field", field = "trigger")
    }
  }
  if (identical(plugin$trigger, "button")) {
    if (episodic_plugin_is_text(spec$placement) && spec$placement %in% episodic_plugin_placements) {
      plugin$placement <- spec$placement
    } else {
      add("bad_field", field = "placement")
    }
    if (episodic_plugin_label_valid(spec$label)) plugin$label <- spec$label else add("bad_field", field = "label")
  } else if (!is.na(plugin$trigger)) {
    for (key in intersect(c("placement", "label"), names(spec))) {
      add("button_only", field = key)
    }
  }

  permissions <- episodic_plugin_strings(spec$permissions)
  if (is.null(permissions) || anyDuplicated(permissions)) {
    add("bad_field", field = "permissions")
  } else {
    allowed <- if (is.na(plugin$trigger)) character(0) else episodic_plugin_permissions[[plugin$trigger]]
    unknown <- setdiff(permissions, allowed)
    if (length(unknown) > 0 && !is.na(plugin$trigger)) {
      add("bad_permission", permission = paste(unknown, collapse = ", "))
    }
    plugin$permissions <- permissions
  }

  if (!is.null(spec$destination)) {
    if (episodic_plugin_is_text(spec$destination)) plugin$destination <- trimws(spec$destination) else add("bad_field", field = "destination")
  }
  if ("cluster_cases" %in% plugin$permissions && is.na(plugin$destination)) {
    add("no_destination")
  }

  secrets <- episodic_plugin_strings(spec$secrets)
  if (is.null(secrets) || anyDuplicated(secrets) || !all(episodic_plugin_env_name_valid(secrets))) {
    add("bad_field", field = "secrets")
  } else if (any(grepl("^EPISODIC_", secrets))) {
    add("episodic_secret", variable = paste(secrets[grepl("^EPISODIC_", secrets)], collapse = ", "))
  } else {
    plugin$secrets <- secrets
  }

  if (!is.null(spec$timeout)) {
    timeout <- spec$timeout
    if (is.numeric(timeout) && length(timeout) == 1 && timeout == round(timeout) &&
      timeout >= 1 && timeout <= episodic_plugin_timeout_max) {
      plugin$timeout <- as.integer(timeout)
    } else {
      add("bad_timeout", max = as.character(episodic_plugin_timeout_max))
    }
  }

  requires <- spec$requires %||% list()
  if (!is.list(requires) || (length(requires) > 0 && is.null(names(requires)))) {
    add("bad_field", field = "requires")
  } else {
    for (key in setdiff(names(requires), c("packages", "commands"))) {
      add("unknown_key", field = paste0("requires.", key))
    }
    packages <- episodic_plugin_strings(requires$packages)
    commands <- episodic_plugin_strings(requires$commands)
    if (is.null(packages)) add("bad_field", field = "requires.packages") else plugin$packages <- packages
    if (is.null(commands)) add("bad_field", field = "requires.commands") else plugin$commands <- commands
  }

  if (!is.null(spec$steps)) {
    steps <- spec$steps
    if (!is.list(steps) || length(steps) == 0 || !is.null(names(steps))) {
      add("bad_field", field = "steps")
    } else {
      plugin$steps <- lapply(seq_along(steps), function(i) {
        step <- steps[[i]]
        where <- paste0("steps[", i, "]")
        if (!is.list(step) || is.null(names(step))) {
          add("bad_field", field = where)
          return(NULL)
        }
        for (key in setdiff(names(step), episodic_plugin_step_keys)) {
          add("unknown_key", field = paste0(where, ".", key))
        }
        if (!(episodic_plugin_is_text(step$shell) && step$shell %in% episodic_plugin_shells)) {
          add("bad_field", field = paste0(where, ".shell"))
        }
        if (!episodic_plugin_is_text(step$run)) {
          add("bad_field", field = paste0(where, ".run"))
        } else if (grepl("${{", step$run, fixed = TRUE)) {
          add("placeholder_in_run", field = paste0(where, ".run"))
        }
        if (!is.null(step$name) && !episodic_plugin_is_text(step$name)) {
          add("bad_field", field = paste0(where, ".name"))
        }
        env <- step$env %||% list()
        env_ok <- is.list(env) && (length(env) == 0 || (!is.null(names(env)) &&
          all(episodic_plugin_env_name_valid(names(env))) &&
          all(vapply(env, function(v) (is.character(v) || is.numeric(v) || is.logical(v)) && length(v) == 1 && !is.na(v), logical(1)))))
        if (!env_ok) {
          add("bad_field", field = paste0(where, ".env"))
        } else if (any(grepl("^EPISODIC_", names(env)))) {
          add("episodic_secret", variable = paste(grep("^EPISODIC_", names(env), value = TRUE), collapse = ", "))
        } else {
          for (name in names(env)) {
            for (problem in episodic_plugin_placeholder_problems(env[[name]], paste0(where, ".env.", name), plugin)) {
              problems[[length(problems) + 1L]] <<- problem
            }
          }
        }
        list(
          name = if (episodic_plugin_is_text(step$name)) trimws(step$name) else paste("Step", i),
          shell = step$shell,
          run = step$run,
          env = if (env_ok) vapply(env, function(v) if (is.logical(v)) tolower(as.character(v)) else as.character(v), character(1)) else character(0)
        )
      })
    }
  }

  if (length(problems) == 0) {
    problems <- episodic_plugin_requirements(plugin)
  }
  done()
}

#' The placeholders in a text: `${{ cluster.label }}` gives `cluster.label`
#' @keywords internal
#' @noRd
episodic_plugin_placeholders <- function(text) {
  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    return(character(0))
  }
  found <- regmatches(text, gregexpr("\\$\\{\\{[^}]*\\}\\}", text))[[1]]
  trimws(gsub("^\\$\\{\\{|\\}\\}$", "", found))
}

#' The payload parts a placeholder may start from, for a plugin
#'
#' What the payload of that plugin always holds, plus the parts its
#' permissions add; never `cases`, which is a list of cases rather than
#' one value.
#' @keywords internal
#' @noRd
episodic_plugin_placeholder_roots <- function(plugin) {
  roots <- c("api", "plugin", "trigger", "instance_id", "language", "dashboard_url")
  if (identical(plugin$trigger, "button")) {
    roots <- c(roots, "user")
  }
  if ("cluster_summary" %in% plugin$permissions) {
    roots <- c(roots, "cluster")
  }
  if ("run_summary" %in% plugin$permissions) {
    roots <- c(roots, "run")
  }
  roots
}

#' What is wrong with the placeholders in one step variable
#' @keywords internal
#' @noRd
episodic_plugin_placeholder_problems <- function(value, field, plugin) {
  problems <- list()
  for (path in episodic_plugin_placeholders(value)) {
    if (!grepl("^[a-z_]+(\\.[a-z0-9_]+)*$", path)) {
      problems[[length(problems) + 1L]] <- episodic_plugin_problem("bad_placeholder", field = field, placeholder = path)
    } else if (!(sub("\\..*$", "", path) %in% episodic_plugin_placeholder_roots(plugin))) {
      problems[[length(problems) + 1L]] <- episodic_plugin_problem("placeholder_part", field = field, part = sub("\\..*$", "", path))
    }
  }
  problems
}

#' The program that runs a step written in `shell`, or `""` if absent
#' @keywords internal
#' @noRd
episodic_plugin_shell_program <- function(shell) {
  switch(shell,
    r = {
      rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      if (file.exists(rscript)) rscript else ""
    },
    bash = unname(Sys.which("bash")),
    python = {
      python <- unname(Sys.which("python3"))
      if (nzchar(python)) python else unname(Sys.which("python"))
    }
  )
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
  if (!requireNamespace("processx", quietly = TRUE)) {
    add("missing_package", package = "processx")
  }
  for (package in plugin$packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      add("missing_package", package = package)
    }
  }
  shells <- unique(vapply(plugin$steps, function(s) s$shell, character(1)))
  for (shell in shells) {
    if (!nzchar(episodic_plugin_shell_program(shell))) {
      add("missing_command", command = c(r = "Rscript", bash = "bash", python = "python3")[[shell]])
    }
  }
  for (command in plugin$commands) {
    if (!nzchar(Sys.which(command))) {
      add("missing_command", command = command)
    }
  }
  problems
}

#' What the configuration grants an enabled plugin
#'
#' @param config The resolved configuration.
#' @param id The plugin's id.
#' @return `NULL` when the plugin is not enabled, else a list with
#'   `permissions` and `secrets`.
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
    secrets = episodic_plugin_strings(entry$secrets) %||% character(0)
  )
}

#' Every plugin in the folder and every plugin the configuration enables
#'
#' A plugin's status is one of:
#' - `"enabled"`: listed in `plugins.enabled`, with everything it declares granted;
#' - `"disabled"`: in the folder, not listed;
#' - `"needs_approval"`: listed, but the file now declares a permission or
#'   secret it was not granted, so it does not run until enabled again;
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
    sort(list.files(dir, pattern = "\\.yaml$", full.names = TRUE))
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
      c(setdiff(plugin$permissions, grant$permissions), setdiff(plugin$secrets, grant$secrets))
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
      id = id, path = if (is.null(dir)) NA_character_ else file.path(dir, paste0(id, ".yaml")),
      name = id, version = NA_character_, status = "missing",
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
    return(plugin$name)
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
#' @return A data frame with one row per plugin: `id`, `name`,
#'   `version`, `trigger`, `placement`, `status` (`"enabled"`,
#'   `"disabled"`, `"needs_approval"`, `"unavailable"` or `"missing"`),
#'   `permissions`, `secrets`, `destination` and `problems` (the reasons,
#'   as text in the `EPISODIC_LANGUAGE` language, separated by `"; "`).
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
    name = vapply(plugins, function(p) p$name %||% p$id, character(1)),
    version = vapply(plugins, function(p) p$version %||% NA_character_, character(1)),
    trigger = vapply(plugins, function(p) p$trigger %||% NA_character_, character(1)),
    placement = vapply(plugins, function(p) p$placement %||% NA_character_, character(1)),
    status = vapply(plugins, function(p) p$status, character(1)),
    permissions = vapply(plugins, function(p) text(p$permissions), character(1)),
    secrets = vapply(plugins, function(p) text(p$secrets), character(1)),
    destination = vapply(plugins, function(p) p$destination %||% NA_character_, character(1)),
    problems = vapply(plugins, function(p) {
      if (length(p$problems) == 0) {
        NA_character_
      } else {
        paste(vapply(p$problems, episodic_plugin_problem_text, character(1)), collapse = "; ")
      }
    }, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
