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

# Installing, enabling and running plugins. Installing writes a file into
# the plugin folder; enabling writes what the plugin is granted into the
# configuration file; running records what happened in
# episodic_plugin_run_event. The same functions serve the Plugins screen,
# the cron's hook and a script.

#' The largest plugin file accepted, in bytes
#' @keywords internal
#' @noRd
episodic_plugin_max_bytes <- 1024L * 1024L

#' Problems that mean a file is not a usable plugin at all
#'
#' As opposed to requirements this server does not meet yet (a package
#' or program to be installed), which leave a plugin installed but
#' unavailable until they are.
#' @keywords internal
#' @noRd
episodic_plugin_requirement_codes <- c("missing_package", "missing_command")

#' The plugin folder, or a stop saying why there is none
#' @keywords internal
#' @noRd
episodic_plugin_dir_required <- function(episodic_config_path) {
  dir <- episodic_plugin_dir(episodic_config_path)
  if (is.null(dir)) {
    stop(
      "There is no plugin folder: set EPISODIC_PLUGIN_DIR, or EPISODIC_CONFIG ",
      "so that plugins live in a 'plugins' folder beside the configuration file.",
      call. = FALSE
    )
  }
  dir
}

#' One plugin by id, or a stop naming what is wrong
#' @keywords internal
#' @noRd
episodic_plugin_get <- function(id, config, dir) {
  if (!episodic_plugin_id_valid(id)) {
    stop("'", id, "' is not a plugin id: lowercase letters, digits and hyphens.", call. = FALSE)
  }
  plugin <- episodic_plugins(config, dir)[[id]]
  if (is.null(plugin)) {
    stop("There is no plugin '", id, "' in the plugin folder (", dir, ").", call. = FALSE)
  }
  plugin
}

#' Install a Plugin
#'
#' Copies a plugin file into the plugin folder: from an `https://` address
#' (a file on GitHub may be given by its page address and is fetched from
#' its raw address), or from a local file. The file is checked first; one
#' that is not a valid plugin is refused and nothing is written. A plugin
#' whose requirements this server does not meet yet is installed and
#' listed as unavailable, with the reasons, until they are met.
#'
#' Installing never enables: see [episodic_plugin_enable()]. Installing a
#' newer version over an enabled plugin keeps it enabled, unless the new
#' version asks for permissions or secrets it was not granted, in which
#' case it does not run until enabled again.
#'
#' A plugin runs code on this server with the account EpiSODIC runs as.
#' Install only plugins from a source you trust, and read a plugin's
#' steps before enabling it. See `vignette("plugins")`.
#'
#' @param source An `https://` address or a local file path. The file name
#'   (without `.yaml`) is the plugin's id.
#' @param overwrite Whether to replace an installed plugin of the same id.
#' @param token_env For a private source: the name of the environment
#'   variable holding a bearer token. The token itself is never accepted
#'   as an argument.
#' @param episodic_config_path The configuration file. Defaults to the
#'   `EPISODIC_CONFIG` environment variable.
#' @return Invisibly, the plugin's id.
#' @seealso [episodic_plugin_list()], [episodic_plugin_enable()],
#'   [episodic_plugin_uninstall()]
#' @examples
#' \dontrun{
#' episodic_plugin_install("https://github.com/org/plugins/blob/v1.0.0/share-regional.yaml")
#' }
#' @export
episodic_plugin_install <- function(source,
                                    overwrite = FALSE,
                                    token_env = NULL,
                                    episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  if (!episodic_plugin_is_text(source)) {
    stop("`source` must be an https:// address or a file path.", call. = FALSE)
  }
  dir <- episodic_plugin_dir_required(episodic_config_path)
  remote <- grepl("^[A-Za-z][A-Za-z0-9+.-]*://", source)
  if (remote) {
    url <- sub("^https://github\\.com/([^/]+)/([^/]+)/blob/", "https://raw.githubusercontent.com/\\1/\\2/", source)
    if (!grepl("^https://", url)) {
      stop("A plugin is installed from an https:// address or a local file, not from '", source, "'.", call. = FALSE)
    }
    endpoint <- episodic_import_export_endpoint(
      "http_get", url,
      token_env = if (is.null(token_env)) NA_character_ else token_env,
      direction = "import"
    )
    bytes <- episodic_import_export_endpoint_read(endpoint)
    name <- basename(sub("[?#].*$", "", url))
  } else {
    if (!file.exists(source) || dir.exists(source)) {
      stop("No plugin file exists at '", source, "'.", call. = FALSE)
    }
    size <- file.info(source)$size
    if (size > episodic_plugin_max_bytes) {
      stop("'", source, "' is larger than a plugin file can be (", episodic_plugin_max_bytes, " bytes).", call. = FALSE)
    }
    bytes <- readBin(source, "raw", size)
    name <- basename(source)
  }
  if (length(bytes) > episodic_plugin_max_bytes) {
    stop("The file is larger than a plugin file can be (", episodic_plugin_max_bytes, " bytes).", call. = FALSE)
  }
  if (!grepl("\\.yaml$", name)) {
    stop("A plugin file's name ends in .yaml; '", name, "' does not.", call. = FALSE)
  }
  id <- sub("\\.yaml$", "", name)
  if (!episodic_plugin_id_valid(id)) {
    stop("'", name, "' is not a valid plugin file name: its id must be lowercase letters, digits and hyphens.", call. = FALSE)
  }
  target <- file.path(dir, name)
  if (file.exists(target) && !isTRUE(overwrite)) {
    stop("A plugin '", id, "' is already installed. Pass overwrite = TRUE to replace it.", call. = FALSE)
  }

  staging <- tempfile(fileext = ".yaml")
  on.exit(unlink(staging), add = TRUE)
  writeBin(bytes, staging)
  checked <- episodic_plugin_read(staging)
  checked$problems <- Filter(function(p) !(p$code %in% c("bad_id", episodic_plugin_requirement_codes)), checked$problems)
  if (length(checked$problems) > 0) {
    stop(
      "'", name, "' is not a valid plugin, so it was not installed: ",
      paste(vapply(checked$problems, episodic_plugin_problem_text, character(1)), collapse = " "),
      call. = FALSE
    )
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  episodic_config_write_atomic_new(target, bytes)
  invisible(id)
}

#' Write a file whole, replacing it if present, in one step
#' @keywords internal
#' @noRd
episodic_config_write_atomic_new <- function(path, bytes) {
  tmp <- tempfile(paste0(".", basename(path), "."), tmpdir = dirname(path))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  writeBin(bytes, tmp)
  if (!file.rename(tmp, path)) {
    stop("'", path, "' could not be written.", call. = FALSE)
  }
  invisible(path)
}

#' Enable or Disable a Plugin
#'
#' Enabling grants a plugin exactly what its file declares: its
#' permissions (which parts of the data it receives) and its secrets
#' (which environment variables it may read), and writes that grant into
#' the configuration file under `plugins.enabled`. A plugin that later
#' declares more than it was granted does not run until enabled again.
#' Disabling removes it from `plugins.enabled`; the file stays.
#'
#' The configuration file is the only record of which plugins are enabled,
#' and these functions write it the way the Plugins screen does, leaving
#' everything else in it as it is.
#'
#' @param id The plugin's id, as [episodic_plugin_list()] shows it.
#' @param episodic_config_path The configuration file. Defaults to the
#'   `EPISODIC_CONFIG` environment variable.
#' @return Invisibly, what was granted (`episodic_plugin_enable()`), or
#'   whether the plugin had been enabled (`episodic_plugin_disable()`).
#' @seealso [episodic_plugin_list()], [episodic_plugin_install()]
#' @examples
#' \dontrun{
#' episodic_plugin_enable("share-regional")
#' episodic_plugin_disable("share-regional")
#' }
#' @export
episodic_plugin_enable <- function(id, episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  dir <- episodic_plugin_dir_required(episodic_config_path)
  plugin <- episodic_plugin_get(id, episodic_config_resolve(episodic_config_path), dir)
  if (length(plugin$problems) > 0) {
    stop(
      "Plugin '", id, "' cannot be enabled: ",
      paste(vapply(plugin$problems, episodic_plugin_problem_text, character(1)), collapse = " "),
      call. = FALSE
    )
  }
  grant <- list(permissions = as.list(plugin$permissions), secrets = as.list(plugin$secrets))
  episodic_config_set(c("plugins", "enabled", id), grant, episodic_config_path)
  invisible(grant)
}

#' @rdname episodic_plugin_enable
#' @export
episodic_plugin_disable <- function(id, episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  if (!episodic_plugin_id_valid(id)) {
    stop("'", id, "' is not a plugin id: lowercase letters, digits and hyphens.", call. = FALSE)
  }
  enabled <- episodic_config_resolve(episodic_config_path)$plugins$enabled %||% list()
  if (!(id %in% names(enabled))) {
    return(invisible(FALSE))
  }
  remaining <- enabled[setdiff(names(enabled), id)]
  if (length(remaining) == 0) {
    remaining <- stats::setNames(list(), character(0))
  }
  episodic_config_set(c("plugins", "enabled"), remaining, episodic_config_path)
  invisible(TRUE)
}

#' Uninstall a Plugin
#'
#' Disables the plugin, if it was enabled, and removes its file from the
#' plugin folder. The record of its past runs stays.
#'
#' @inheritParams episodic_plugin_enable
#' @return Invisibly, `TRUE`.
#' @seealso [episodic_plugin_install()]
#' @examples
#' \dontrun{
#' episodic_plugin_uninstall("share-regional")
#' }
#' @export
episodic_plugin_uninstall <- function(id, episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  dir <- episodic_plugin_dir_required(episodic_config_path)
  episodic_plugin_disable(id, episodic_config_path)
  path <- file.path(dir, paste0(id, ".yaml"))
  if (file.exists(path) && !isTRUE(file.remove(path))) {
    stop("The plugin file '", path, "' could not be removed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' A cluster as a plugin receives it
#'
#' What the database holds about the cluster, never a derived screen
#' state: the dossier's figures, its stream, and the latest classification
#' an epidemiologist recorded. `label` is the cluster's reference as the
#' dashboard writes it in `lang` (`O-12`, `U-12` in Dutch).
#' @keywords internal
#' @noRd
episodic_plugin_cluster_summary <- function(con, cluster_id, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  row <- episodic_db_get_query(
    con,
    "SELECT c.cluster_id, c.scale, c.origin, c.first_day, c.last_day, c.n_cases,
            c.expected, c.excess, c.ratio, c.priority_score, c.detector_agreement,
            c.opened_at, s.pathogen, s.level, s.care_line, s.region_code, s.ward,
            i.display_name AS institution
       FROM episodic_cluster c
       JOIN episodic_stream s ON s.stream_id = c.stream_id
       LEFT JOIN episodic_institution i ON i.institution_id = s.institution_id
      WHERE c.cluster_id = ?",
    params = list(as.integer(cluster_id))
  )
  if (nrow(row) != 1) {
    stop("There is no cluster ", cluster_id, ".", call. = FALSE)
  }
  verdict <- episodic_db_get_query(
    con,
    "SELECT verdict FROM episodic_assessment_event
      WHERE cluster_id = ? AND verdict IS NOT NULL
      ORDER BY created_at DESC, event_id DESC LIMIT 1",
    params = list(as.integer(cluster_id))
  )
  summary <- as.list(row[1, ])
  summary$label <- episodic_object_ref(summary$cluster_id, summary$scale, lang = lang)
  summary$verdict <- if (nrow(verdict) == 1) verdict$verdict else NA_character_
  summary
}

#' A cluster's cases as a plugin receives them
#' @keywords internal
#' @noRd
episodic_plugin_cluster_cases <- function(con, cluster_id, origin) {
  if (identical(origin, "manual")) {
    episodic_db_cluster_manual_cases(con, cluster_id)
  } else {
    episodic_db_cluster_cases(con, cluster_id)
  }
}

#' The payload a plugin reads from EPISODIC_PAYLOAD
#'
#' Only what the plugin was granted: a part it holds no permission for is
#' absent, not empty.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param plugin The plugin.
#' @param permissions The permissions it holds.
#' @param context A list: `cluster_id` for a button, `run` (a list with
#'   `run_id` and the run's figures and `new_cluster_ids`) for the hook,
#'   `user` (`username`, `role`) when a person ran it.
#' @param config The resolved configuration.
#' @param lang The language of whoever ran it.
#' @return A list.
#' @keywords internal
#' @noRd
episodic_plugin_payload <- function(con, plugin, permissions, context, config, lang) {
  payload <- list(
    api = episodic_plugin_api_version,
    plugin = list(id = plugin$id, version = plugin$version),
    trigger = plugin$trigger,
    instance_id = episodic_db_instance_id(con),
    language = episodic_lang(lang),
    dashboard_url = config$notifications$dashboard_url %||% NA_character_,
    user = context$user
  )
  if (identical(plugin$trigger, "button")) {
    summary <- episodic_plugin_cluster_summary(con, context$cluster_id, lang = lang)
    if ("cluster_summary" %in% permissions) {
      payload$cluster <- summary
    }
    if ("cluster_cases" %in% permissions) {
      payload$cases <- episodic_plugin_cluster_cases(con, context$cluster_id, summary$origin)
    }
  }
  if (identical(plugin$trigger, "run_committed") && "run_summary" %in% permissions) {
    run <- context$run
    payload$run <- run[setdiff(names(run), "new_cluster_ids")]
    payload$run$new_clusters <- lapply(run$new_cluster_ids %||% integer(0), function(id) {
      episodic_plugin_cluster_summary(con, id, lang = lang)
    })
  }
  payload
}

#' Record a plugin run
#' @keywords internal
#' @noRd
episodic_db_plugin_run_record <- function(con, plugin, permissions, initiated_by, started_at,
                                          outcome, user_id = NA_integer_, cluster_id = NA_integer_,
                                          run_id = NA_integer_, host = Sys.info()[["nodename"]], account = Sys.info()[["user"]]) {
  initiated_by <- match.arg(initiated_by, c("dashboard", "cron", "headless"))
  na_text <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_character_ else as.character(x)
  episodic_db_execute(
    con,
    "INSERT INTO episodic_plugin_run_event
      (plugin_id, plugin_version, plugin_sha256, initiated_by, started_at, finished_at,
       user_id, host, account, cluster_id, run_id, permissions, destination,
       status, message, link, step, detail)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      plugin$id, na_text(plugin$version), plugin$sha256, initiated_by, started_at, episodic_now(),
      as.integer(user_id), host, account, as.integer(cluster_id), as.integer(run_id),
      as.character(jsonlite::toJSON(as.character(permissions))), na_text(plugin$destination),
      outcome$status, na_text(outcome$message), na_text(outcome$link), na_text(outcome$step), na_text(outcome$detail)
    )
  )
  episodic_db_last_insert_id(con)
}

#' Run one enabled plugin and record that it ran
#'
#' Builds the payload, runs the steps and records the outcome. A payload
#' that cannot be built is recorded as a failed run, so every attempt
#' leaves a row. Never raises for what the plugin does; raises only when
#' the run cannot be recorded, because a plugin that may have sent data
#' out must not leave no trace.
#'
#' @return The outcome, from `episodic_plugin_execute()`.
#' @keywords internal
#' @noRd
episodic_plugin_invoke <- function(con, plugin, context, initiated_by, config,
                                   lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                   user_id = NA_integer_, host = Sys.info()[["nodename"]], account = Sys.info()[["user"]]) {
  started_at <- episodic_now()
  permissions <- intersect(plugin$permissions, plugin$grant$permissions)
  payload <- tryCatch(
    episodic_plugin_payload(con, plugin, permissions, context, config, lang),
    error = function(e) e
  )
  outcome <- if (inherits(payload, "error")) {
    list(
      status = "failed", message = NA_character_, link = NA_character_, step = NA_character_,
      detail = paste("The payload could not be built:", conditionMessage(payload)), duration = 0
    )
  } else {
    episodic_plugin_execute(plugin, plugin$grant, payload, lang = lang)
  }
  episodic_db_plugin_run_record(
    con, plugin, permissions, initiated_by, started_at, outcome,
    user_id = user_id,
    cluster_id = context$cluster_id %||% NA_integer_,
    run_id = context$run$run_id %||% NA_integer_,
    host = host, account = account
  )
  outcome
}

#' Run a Plugin
#'
#' Runs an enabled plugin from a script, exactly as its button or hook
#' would, and records the run. A plugin triggered by a button needs the
#' cluster it acts on; one triggered after a detection run is given the
#' most recent committed run, or `run_id`.
#'
#' @param id The plugin's id.
#' @param cluster_id For a button plugin: the cluster, by its number (the
#'   `12` of `O-12`).
#' @param run_id For a run plugin: the run; `NULL` for the most recent
#'   run that committed.
#' @param db_path The database. Defaults to the `EPISODIC_DB` environment
#'   variable.
#' @param episodic_config_path The configuration file. Defaults to the
#'   `EPISODIC_CONFIG` environment variable.
#' @param lang The language the plugin should write its message in.
#' @return Invisibly, a list with `status` (`"success"`, `"error"`,
#'   `"failed"` or `"timeout"`), `message`, `link`, `step` and `detail`.
#'   A status other than `"success"` is also raised as a warning.
#' @seealso [episodic_plugin_list()], `vignette("plugins")`
#' @examples
#' \dontrun{
#' episodic_plugin_run("share-regional", cluster_id = 12)
#' }
#' @export
episodic_plugin_run <- function(id,
                                cluster_id = NULL,
                                run_id = NULL,
                                db_path = Sys.getenv("EPISODIC_DB", unset = NA),
                                episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
                                lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  config <- episodic_config_resolve(episodic_config_path)
  dir <- episodic_plugin_dir_required(episodic_config_path)
  plugin <- episodic_plugin_get(id, config, dir)
  if (!identical(plugin$status, "enabled")) {
    stop("Plugin '", id, "' is ", gsub("_", " ", plugin$status), ", so it cannot run.", call. = FALSE)
  }
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  context <- list(user = NULL)
  if (identical(plugin$trigger, "button")) {
    if (is.null(cluster_id) || length(cluster_id) != 1 || is.na(suppressWarnings(as.integer(cluster_id)))) {
      stop("Plugin '", id, "' acts on a cluster: give its number as `cluster_id`.", call. = FALSE)
    }
    context$cluster_id <- as.integer(cluster_id)
  } else {
    context$run <- episodic_plugin_run_context(con, run_id)
  }
  outcome <- episodic_plugin_invoke(con, plugin, context, "headless", config, lang = lang)
  if (!identical(outcome$status, "success")) {
    warning(
      "Plugin '", id, "' ended with status '", outcome$status, "'",
      if (!is.na(outcome$detail)) paste0(": ", outcome$detail) else if (!is.na(outcome$message)) paste0(": ", outcome$message) else ".",
      call. = FALSE
    )
  }
  invisible(outcome)
}

#' The run a run plugin is given when run from a script
#' @keywords internal
#' @noRd
episodic_plugin_run_context <- function(con, run_id = NULL) {
  run <- if (is.null(run_id)) {
    episodic_db_get_query(
      con,
      "SELECT * FROM episodic_detection_run WHERE status IN ('success', 'partial')
        ORDER BY run_id DESC LIMIT 1"
    )
  } else {
    episodic_db_get_query(con, "SELECT * FROM episodic_detection_run WHERE run_id = ?", params = list(as.integer(run_id)))
  }
  if (nrow(run) != 1) {
    stop(if (is.null(run_id)) "No detection run has committed yet." else paste0("There is no run ", run_id, "."), call. = FALSE)
  }
  new_ids <- episodic_db_get_query(
    con,
    "SELECT cluster_id FROM episodic_cluster WHERE last_detected_run = ? AND opened_at >= ?",
    params = list(run$run_id, run$started_at)
  )$cluster_id
  episodic_plugin_run_summary(run[1, ], new_ids)
}

#' A detection run as a run plugin receives it
#' @keywords internal
#' @noRd
episodic_plugin_run_summary <- function(run, new_cluster_ids) {
  list(
    run_id = as.integer(run$run_id),
    run_date = run$run_date,
    status = run$status,
    is_backfill = isTRUE(as.logical(run$is_backfill)),
    n_detections = run$n_detections,
    n_signals_new = run$n_signals_new,
    n_signals_updated = run$n_signals_updated,
    new_cluster_ids = as.integer(new_cluster_ids)
  )
}

#' Run the plugins enabled for after a detection run commits
#'
#' Called by `episodic_run_cron()` after its transaction has committed
#' and its notifications are sent. Each plugin's outcome is logged; an
#' error of any kind is caught and logged, never raised, so a plugin can
#' neither fail the run nor stop the plugins after it.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param config The run's resolved configuration.
#' @param run_id The run.
#' @param result The run's result (`n_detections`, `n_signals_new`,
#'   `n_signals_updated`, `new_cluster_ids`).
#' @param episodic_config_path The configuration file, for the plugin folder.
#' @param host,account Where the run ran.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_plugin_run_hooks <- function(con, config, run_id, result, episodic_config_path, host, account) {
  plugins <- tryCatch(
    episodic_plugins_runnable(episodic_plugins(config, episodic_plugin_dir(episodic_config_path)), "run_committed"),
    error = function(e) e
  )
  if (inherits(plugins, "error")) {
    episodic_trace("Plugins could not be read: ", conditionMessage(plugins), severity = "danger")
    return(invisible(NULL))
  }
  if (length(plugins) == 0) {
    return(invisible(NULL))
  }
  run <- tryCatch(
    episodic_plugin_run_summary(
      episodic_db_get_query(con, "SELECT * FROM episodic_detection_run WHERE run_id = ?", params = list(run_id))[1, ],
      result$new_cluster_ids %||% integer(0)
    ),
    error = function(e) e
  )
  if (inherits(run, "error")) {
    episodic_trace("Plugins after the run were not started: ", conditionMessage(run), severity = "danger")
    return(invisible(NULL))
  }
  for (plugin in plugins) {
    episodic_trace("Running plugin ", plugin$id)
    outcome <- tryCatch(
      episodic_plugin_invoke(con, plugin, list(run = run), "cron", config, host = host, account = account),
      error = function(e) e
    )
    if (inherits(outcome, "error")) {
      episodic_trace("Plugin ", plugin$id, " could not be run or recorded: ", conditionMessage(outcome), severity = "danger")
    } else if (identical(outcome$status, "success")) {
      episodic_trace("Plugin ", plugin$id, " finished", if (!is.na(outcome$message)) paste0(": ", outcome$message) else "")
    } else {
      episodic_trace(
        "Plugin ", plugin$id, " ended with status ", outcome$status,
        if (!is.na(outcome$detail)) paste0(": ", outcome$detail) else if (!is.na(outcome$message)) paste0(": ", outcome$message) else "",
        severity = "warn"
      )
    }
  }
  invisible(NULL)
}
