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
#' `episodic_config_hash()`), so you can always trace a past result back to
#' the settings that produced it, even after you have since changed them.
#'
#' An `is_admin` account can additionally override the `notifications`
#' section from the Settings screen, without touching the YAML file at
#' all - pass `con` to also apply the most recent such override (if any)
#' on top of the YAML-resolved configuration. The YAML overlay itself
#' remains fully supported either way: an admin override is an additional,
#' higher-precedence layer, not a replacement for it.
#'
#' @param episodic_config_path Path to your own configuration file. Defaults
#'   to the `EPISODIC_CONFIG` environment variable; if that is unset or the
#'   file does not exist, only the built-in defaults are used.
#' @param con An open [DBI::DBIConnection-class], to also overlay the most
#'   recent Settings-screen `notifications` override (if any) on top of
#'   the YAML-resolved configuration. `NULL` (the default) skips this -
#'   [episodic_run_cron()] and the Settings screen itself both pass their
#'   own connection; most other callers (tests, `episodic_config_hash()`
#'   snapshots) do not need one.
#' @return A nested list with the resolved configuration, e.g.
#'   `config$eligibility$min_baseline_weeks` or `config$priority_score$weights`.
#' @keywords internal
#' @noRd
episodic_config_resolve <- function(episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
                                    con = NULL) {
  defaults_path <- system.file("config", "episodic_default_config.yaml", package = "EpiSODIC")
  if (identical(defaults_path, "")) {
    defaults_path <- file.path("inst", "config", "episodic_default_config.yaml")
  }
  config <- yaml::read_yaml(defaults_path)

  if (!is.na(episodic_config_path) && nzchar(episodic_config_path)) {
    # Set but unusable is a configuration error, not a fallback. Ignoring
    # it - as this used to - runs the instance on the shipped defaults
    # while the operator believes their own thresholds, their own
    # `same_place` overrides and their own notification channels are in
    # force, and nothing anywhere says otherwise. A typo in a path is not
    # a rare event; a surveillance system silently running settings
    # nobody chose is not an acceptable consequence of one.
    if (!file.exists(episodic_config_path)) {
      stop(
        "EPISODIC_CONFIG points at '",
        episodic_config_path,
        "', but no file exists there. Correct the path, or unset it to ",
        "run on the shipped defaults deliberately.",
        call. = FALSE
      )
    }
    instance_config <- tryCatch(
      yaml::read_yaml(episodic_config_path),
      error = function(e) e
    )
    if (inherits(instance_config, "condition")) {
      stop(
        "EPISODIC_CONFIG points at '",
        episodic_config_path,
        "', which could not be read as YAML: ",
        conditionMessage(instance_config),
        call. = FALSE
      )
    }
    if (is.null(instance_config)) {
      instance_config <- list()
    }
    episodic_config_validate(instance_config, config, episodic_config_path)
    config <- episodic_config_merge(config, instance_config)
  }

  if (!is.null(con)) {
    latest <- episodic_db_app_config_latest(con, "notifications")
    if (!is.null(latest)) {
      overlay <- jsonlite::fromJSON(
        latest$config_json,
        simplifyVector = FALSE
      )
      config$notifications <- episodic_config_merge(
        config$notifications %||% list(),
        overlay
      )
    }
  }

  config
}

#' Configuration subtrees whose child keys an operator names themselves
#'
#' Everything else in an instance configuration must correspond to a key
#' the shipped defaults document, because that is what makes a typo
#' detectable at all. These three cannot: their children are pathogen
#' names, channel names and a list of pathogens respectively, none of
#' which EpiSODIC can enumerate in advance.
#' @keywords internal
#' @noRd
episodic_config_open_sections <- list(
  "notifications",
  c("same_place", "overrides"),
  c("rare_trigger", "pathogens")
)

#' Settings that mean something by an explicit YAML `null`
#'
#' Each of these is documented in the shipped defaults as "set to ~ to
#' disable", and each is read through `%||% NA` at its one call site, so
#' a null genuinely turns the check off rather than propagating an
#' unexpected `NULL` into arithmetic. Every other key is a value the
#' code needs a value for.
#' @keywords internal
#' @noRd
episodic_config_nullable_keys <- c(
  "reconciliation.cooldown_reopen_ratio",
  "reconciliation.stale_open_days",
  "effect_size_floor.min_excess_over_upperbound",
  "effect_size_floor.min_ratio_observed_expected",
  "same_place.lookback_days",
  "rare_trigger.lookback_days"
)

#' Refuse an instance configuration that cannot mean what it says
#'
#' A YAML overlay is merged key by key, which is exactly why a
#' misspelled key is invisible: `reconcilliation:` adds a section nobody
#' reads and leaves `reconciliation:` at its defaults, and the run
#' proceeds, and the operator's thresholds are simply not in force. The
#' same goes for a value of the wrong shape - `close_after_runs:
#' fourteen` reaches `runs_since > close_after_runs`, where R compares a
#' number against a string and quietly gets an answer.
#'
#' So every key an instance sets is checked against the shipped defaults
#' (which document the complete valid set), and every value it sets is
#' checked against the type and arity of the default it replaces. Both
#' are reported together, naming every offending key path, rather than
#' one per run.
#'
#' Deliberately strict about `null` too. YAML's `~` is a perfectly
#' reasonable thing to write, but only a handful of settings actually
#' mean anything by it - the ones documented as "set to ~ to disable",
#' listed in `episodic_config_nullable_keys`. Everywhere else it leaves
#' a caller holding `NULL` where it expects a number, and
#' `case_free_days_default: ~` in particular turns every
#' interval-overlap test in reconciliation into `integer(0)`, which
#' matches nothing and opens a fresh cluster for every candidate,
#' silently.
#'
#' @param instance The instance configuration, as read from YAML.
#' @param defaults The shipped defaults, before merging.
#' @param path The instance file's path, for the error message.
#' @return Invisibly `TRUE`; throws otherwise.
#' @keywords internal
#' @noRd
episodic_config_validate <- function(instance, defaults, path = "<instance config>") {
  problems <- character(0)

  is_open <- function(key_path) {
    any(vapply(
      episodic_config_open_sections,
      function(open) {
        length(key_path) >= length(open) &&
          identical(key_path[seq_along(open)], open)
      },
      logical(1)
    ))
  }

  type_of <- function(x) {
    if (is.null(x)) {
      "null"
    } else if (is.logical(x)) {
      "true/false"
    } else if (is.numeric(x)) {
      "a number"
    } else if (is.character(x)) {
      "text"
    } else if (is.list(x)) {
      "a section"
    } else {
      paste(class(x), collapse = "/")
    }
  }

  # A named list is a section; anything else (including an unnamed list,
  # which is how YAML delivers a sequence) is a value.
  is_section <- function(x) {
    is.list(x) && !is.null(names(x)) && !any(names(x) == "")
  }

  walk <- function(node, known, key_path) {
    for (key in names(node)) {
      here <- c(key_path, key)
      dotted <- paste(here, collapse = ".")
      if (is_open(here)) {
        next
      }
      if (!key %in% names(known)) {
        suggestion <- episodic_config_nearest_key(key, names(known))
        problems <<- c(problems, paste0(
          "`",
          dotted,
          "` is not a configuration key EpiSODIC knows",
          if (!is.na(suggestion)) {
            paste0(" (did you mean `", suggestion, "`?)")
          } else {
            ""
          }
        ))
        next
      }
      value <- node[[key]]
      expected <- known[[key]]
      if (is_section(expected)) {
        if (!is_section(value)) {
          problems <<- c(problems, paste0(
            "`",
            dotted,
            "` must be a section of settings, but is ",
            type_of(value)
          ))
        } else {
          walk(value, expected, here)
        }
        next
      }
      if (is.null(expected)) {
        # The default is null and carries no shape to check against.
        next
      }
      if (is.null(value)) {
        if (!dotted %in% episodic_config_nullable_keys) {
          problems <<- c(problems, paste0(
            "`",
            dotted,
            "` cannot be null; give it a value, or leave the key out ",
            "entirely to keep the default (",
            paste(format(expected), collapse = ", "),
            ")"
          ))
        }
        next
      }
      if (!identical(type_of(value), type_of(expected))) {
        problems <<- c(problems, paste0(
          "`",
          dotted,
          "` must be ",
          type_of(expected),
          ", but is ",
          type_of(value)
        ))
        next
      }
      if (length(expected) == 1 && length(value) != 1) {
        problems <<- c(problems, paste0(
          "`",
          dotted,
          "` must be a single value, but has ",
          length(value)
        ))
      }
    }
  }

  if (!is_section(instance) && length(instance) > 0) {
    stop(
      "The configuration at '",
      path,
      "' is not a set of named sections.",
      call. = FALSE
    )
  }
  walk(instance, defaults, character(0))

  if (length(problems) > 0) {
    stop(
      "The configuration at '",
      path,
      "' cannot be used:\n",
      paste0("  - ", problems, collapse = "\n"),
      "\nSee inst/config/episodic_default_config.yaml (or ",
      "vignette(\"deployment\")) for every key and its default.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' The known key most likely to be what a misspelling meant
#'
#' Only offered when it is close enough to be worth offering: a
#' suggestion that is merely the least-bad of an unrelated set is worse
#' than none.
#' @param key The unknown key.
#' @param known The keys valid at this level.
#' @return A single key, or `NA_character_`.
#' @keywords internal
#' @noRd
episodic_config_nearest_key <- function(key, known) {
  if (length(known) == 0) {
    return(NA_character_)
  }
  distances <- utils::adist(key, known, ignore.case = TRUE)[1, ]
  best <- which.min(distances)
  if (distances[best] > max(2, floor(nchar(key) / 3))) {
    return(NA_character_)
  }
  known[best]
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
    if (
      is.list(override[[key]]) &&
        is.list(base[[key]]) &&
        !is.null(names(override[[key]])) &&
        !is.null(names(base[[key]]))
    ) {
      base[[key]] <- episodic_config_merge(base[[key]], override[[key]])
    } else if (is.null(override[[key]])) {
      # `base[[key]] <- NULL` *removes* the key rather than setting it to
      # null, which is R's assignment semantics and not what YAML's `~`
      # says. Single-bracket assignment of a one-element list is the
      # spelling that keeps the key and gives it a null value, so a
      # setting documented as "set to ~ to disable" behaves as documented
      # rather than as absent - which happens to give the same answer
      # wherever the reader uses `%||%`, and a different one everywhere
      # else.
      base[key] <- list(NULL)
    } else {
      base[[key]] <- override[[key]]
    }
  }
  base
}

#' Configuration sections deliberately left out of `config_hash`
#'
#' Detection reproducibility is the guarantee the hash exists for: same
#' detection settings in, same hash out. A section that cannot change
#' what a run computes therefore must not change the hash either, or
#' every notification tweak and every access-policy change would make
#' historic runs look incomparable with current ones for no reason.
#'
#' `notifications` has the further reason that it holds secrets (SMTP
#' passwords, webhook URLs), and `config_snapshot` is stored in plain
#' text on every run row.
#'
#' `report` is here on the same test: the small-count suppression
#' threshold changes what a rendered report discloses, never what a
#' detection run computes, and each render records the threshold it
#' actually used in its own `params` anyway.
#'
#' `geography` is deliberately *not* here. `region_code` and the area
#' rule both feed `episodic_stream_key()`, so a change to either changes
#' every geographic stream's identity - which is exactly the kind of
#' difference the hash exists to make visible.
#' @keywords internal
#' @noRd
episodic_config_unhashed_sections <- c("notifications", "access", "report")

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
#' The `notifications` and `access` sections are excluded: both govern how
#' the instance is operated rather than what a run computes, so neither
#' can make two otherwise-identical runs compare as different.
#'
#' You will not normally call this directly - EpiSODIC's detection pipeline
#' calls it automatically - but it is useful for confirming that two
#' configuration files are equivalent, or for recovering a full historic
#' configuration from a stored hash and snapshot.
#'
#' @param config A resolved configuration, as returned by
#'   `episodic_config_resolve()`.
#' @return A list with `hash` (a 40-character hex digest) and `snapshot`
#'   (the canonical configuration, as a JSON string).
#' @keywords internal
#' @noRd
episodic_config_hash <- function(config) {
  config_for_hash <- config
  # Sections that govern how the instance is operated rather than what a
  # detection run computes. Turning on the login wall, or changing an
  # SMTP password, must not make two otherwise-identical runs compare as
  # having used different settings - and `notifications` additionally
  # holds secrets that have no business in `config_snapshot`.
  for (section in episodic_config_unhashed_sections) {
    config_for_hash[[section]] <- NULL
  }
  canonical <- episodic_config_canonicalise(config_for_hash)
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
