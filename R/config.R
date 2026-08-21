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

#' Read the surveillance configuration
#'
#' EpiSODIC's detection behaviour - which detectors run, their thresholds,
#' how a dossier's priority score is weighted, and so on - is controlled by
#' a YAML configuration file, not by function arguments. This function reads
#' that configuration: it starts from the package's built-in defaults and,
#' if you have set the `EPISODIC_CONFIG` environment variable to point at
#' your own YAML file, overlays your settings on top. You only need to set
#' the keys you want to change; anything you leave out keeps its default.
#'
#' Running the bundled demo needs no configuration file at all - the
#' shipped defaults are enough on their own.
#'
#' Every detection run stores the exact configuration it used (see
#' [episodic_config_hash()]), so you can always trace a past result back to
#' the settings that produced it, even after you have since changed them.
#'
#' @param episodic_config_path Path to your own configuration file. Defaults
#'   to the `EPISODIC_CONFIG` environment variable; if that is unset or the
#'   file does not exist, only the built-in defaults are used.
#' @return A nested list with the resolved configuration, e.g.
#'   `config$eligibility$min_baseline_weeks` or `config$priority_score$weights`.
#' @examples
#' config <- episodic_config_resolve()
#' names(config)
#' config$eligibility$min_baseline_weeks
#' @export
episodic_config_resolve <- function(episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  defaults_path <- system.file("config", "default.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "default.yaml")
  }
  config <- yaml::read_yaml(defaults_path)

  if (!is.na(episodic_config_path) && nzchar(episodic_config_path) && file.exists(episodic_config_path)) {
    instance_config <- yaml::read_yaml(episodic_config_path)
    config <- episodic_config_merge(config, instance_config)
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
episodic_config_merge <- function(base, override) {
  for (key in names(override)) {
    if (is.list(override[[key]]) && is.list(base[[key]]) &&
        !is.null(names(override[[key]])) && !is.null(names(base[[key]]))) {
      base[[key]] <- episodic_config_merge(base[[key]], override[[key]])
    } else {
      base[[key]] <- override[[key]]
    }
  }
  base
}

#' Fingerprint a configuration for reproducibility
#'
#' Every detection run is stamped with a hash of the exact configuration
#' that produced it, so that two runs can be compared to see whether they
#' actually used the same settings, and any run's full configuration can be
#' recovered later even if the live configuration file has since changed.
#' The hash does not depend on the order of keys in your YAML file: it is
#' computed over a canonical (sorted, JSON) representation, so equivalent
#' configurations always produce the same hash.
#'
#' You will not normally call this directly - EpiSODIC's detection pipeline
#' calls it automatically - but it is useful for confirming that two
#' configuration files are equivalent, or for recovering a full historic
#' configuration from a stored hash and snapshot.
#'
#' @param config A resolved configuration, as returned by
#'   [episodic_config_resolve()].
#' @return A list with `hash` (a 40-character hex digest) and `snapshot`
#'   (the canonical configuration, as a JSON string).
#' @examples
#' hashed <- episodic_config_hash(episodic_config_resolve())
#' hashed$hash
#' @export
episodic_config_hash <- function(config) {
  canonical <- episodic_config_canonicalise(config)
  snapshot <- jsonlite::toJSON(canonical, auto_unbox = TRUE, null = "null")
  list(
    hash = digest::digest(snapshot, algo = "sha1", serialize = FALSE),
    snapshot = as.character(snapshot)
  )
}

#' @keywords internal
#' @noRd
episodic_config_canonicalise <- function(x) {
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && !any(nms == "")) {
      x <- x[order(nms)]
      lapply(x, episodic_config_canonicalise)
    } else {
      lapply(x, episodic_config_canonicalise)
    }
  } else {
    x
  }
}
