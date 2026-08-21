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

#' Resolve the EpiSODIC configuration
#'
#' Loads the shipped defaults from `inst/config/default.yaml`, then, if the
#' `EPISODIC_CONFIG` environment variable points at a readable file, loads it
#' and overlays it on top: any key it sets replaces the corresponding key in
#' the defaults. Detection settings are never read from anywhere else and
#' never from inside this package's own tree at runtime beyond the shipped
#' defaults.
#'
#' The result is what gets hashed into `config_hash` and stored verbatim as
#' `config_snapshot` on every detection run, so a run's exact parameters are
#' always recoverable from the database alone.
#'
#' @param episode_config_path Path to the instance configuration file.
#'   Defaults to the `EPISODIC_CONFIG` environment variable. If unset or the
#'   file does not exist, only the shipped defaults are used, which is the
#'   supported way to run the bundled demo.
#' @return A nested list, the resolved configuration.
#' @examples
#' config <- episode_config_resolve()
#' names(config)
#' config$eligibility$min_baseline_weeks
#' @export
episode_config_resolve <- function(episode_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  defaults_path <- system.file("config", "default.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "default.yaml")
  }
  config <- yaml::read_yaml(defaults_path)

  if (!is.na(episode_config_path) && nzchar(episode_config_path) && file.exists(episode_config_path)) {
    instance_config <- yaml::read_yaml(episode_config_path)
    config <- episode_config_merge(config, instance_config)
  }

  config
}

#' Recursively merge an instance configuration on top of the defaults
#'
#' Any key present in `override` replaces the corresponding key in `base`.
#' Nested lists are merged recursively rather than replaced wholesale, so an
#' instance can override a single weight in `priority_score.weights` without
#' having to restate every other weight.
#'
#' @param base The shipped defaults (or the result of a previous merge).
#' @param override The instance configuration to overlay.
#' @return The merged list.
#' @keywords internal
#' @noRd
episode_config_merge <- function(base, override) {
  for (key in names(override)) {
    if (is.list(override[[key]]) && is.list(base[[key]]) &&
        !is.null(names(override[[key]])) && !is.null(names(base[[key]]))) {
      base[[key]] <- episode_config_merge(base[[key]], override[[key]])
    } else {
      base[[key]] <- override[[key]]
    }
  }
  base
}

#' Compute the SHA-1 hash and canonical JSON snapshot of a resolved config
#'
#' The hash is taken over a canonicalised representation (keys sorted
#' recursively, then serialised as JSON) so that key ordering in the
#' source YAML never changes the hash. SHA-1 was chosen to match the
#' `CHAR(40)` width used for other `_key`/`_hash` columns in the schema.
#'
#' @param config A resolved configuration, as returned by
#'   [episode_config_resolve()].
#' @return A list with elements `hash` (a 40-character SHA-1 hex digest) and
#'   `snapshot` (the canonical JSON string).
#' @examples
#' hashed <- episode_config_hash(episode_config_resolve())
#' hashed$hash
#' @export
episode_config_hash <- function(config) {
  canonical <- episode_config_canonicalise(config)
  snapshot <- jsonlite::toJSON(canonical, auto_unbox = TRUE, null = "null")
  list(
    hash = digest::digest(snapshot, algo = "sha1", serialize = FALSE),
    snapshot = as.character(snapshot)
  )
}

#' @keywords internal
#' @noRd
episode_config_canonicalise <- function(x) {
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && !any(nms == "")) {
      x <- x[order(nms)]
      lapply(x, episode_config_canonicalise)
    } else {
      lapply(x, episode_config_canonicalise)
    }
  } else {
    x
  }
}
