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

# The Settings screen (episodic_ui_settings_screen()/episodic_app_server_settings())
# is the in-app alternative to editing EPISODIC_CONFIG by hand and running
# episodic_add_user() at the console - see issue "Admin role and
# settings UI". Visible only to an is_admin account
# (episodic_user_is_admin()); everyone else never sees the "Settings" nav
# link at all (episodic_ui_nav_links() omits it), and every server-side
# handler here re-checks is_admin regardless, the same defence-in-depth
# every other write action in this app applies.
#
# Three things live here:
#  - notification channels/recipients/toggles: read/write, stored as an
#    episodic_app_config_event (append-only; episodic_config_resolve()
#    overlays the latest one on top of the YAML config)
#  - detection configuration: read-only, straight from
#    episodic_config_resolve() - changing it deliberately still requires
#    editing EPISODIC_CONFIG
#  - dashboard accounts: create, and change role/admin/active state, via
#    episodic_add_user()'s underlying insert and the
#    episodic_auth_set_*() event writers

#' The editable notification-channel field specification
#'
#' Drives both the Settings screen's form (`episodic_ui_settings_notif_channel()`)
#' and how submitted values are assembled back into a `notifications`
#' config list (`episodic_app_settings_build_notifications()`) - one
#' definition instead of the two staying in sync by hand. `key` is a
#' dotted path within the channel's own subtree (e.g. `"auth.password"`
#' for `notifications.channels.ntfy.auth.password`); `type` is one of
#' `"text"`, `"secret"`, `"number"`, `"checkbox"`, or `"textlist"` (a
#' comma-separated list of recipients, e.g. `to`).
#'
#' @return A named list, one entry per channel, each `list(fields = list(...))`.
#' @keywords internal
#' @noRd
episodic_settings_channel_specs <- function() {
  list(
    ntfy = list(
      fields = list(
        list(key = "server", type = "text"),
        list(key = "topic", type = "text"),
        list(key = "priority", type = "number"),
        list(key = "auth.username", type = "text"),
        list(key = "auth.password", type = "secret")
      )
    ),
    smtp = list(
      fields = list(
        list(key = "host", type = "text"),
        list(key = "port", type = "number"),
        list(key = "tls", type = "checkbox"),
        list(key = "from", type = "text"),
        list(key = "to", type = "textlist"),
        list(key = "username", type = "text"),
        list(key = "password", type = "secret")
      )
    ),
    sendmail = list(
      fields = list(
        list(key = "binary", type = "text"),
        list(key = "from", type = "text"),
        list(key = "to", type = "textlist")
      )
    ),
    microsoft365 = list(
      fields = list(
        list(key = "tenant_id", type = "text"),
        list(key = "client_id", type = "text"),
        list(key = "client_secret", type = "secret"),
        list(key = "from", type = "text"),
        list(key = "to", type = "textlist")
      )
    ),
    teams = list(
      fields = list(
        list(key = "webhook_url", type = "secret")
      )
    ),
    slack = list(
      fields = list(
        list(key = "webhook_url", type = "secret")
      )
    )
  )
}

#' The `input`/output element id for one channel field
#' @keywords internal
#' @noRd
episodic_settings_field_id <- function(channel, key) {
  paste0("stg_notif_", channel, "_", gsub(".", "_", key, fixed = TRUE))
}

#' Read a dotted-path value out of a (possibly nested) list
#'
#' `episodic_settings_get_path(list(auth = list(username = "a")), "auth.username")`
#' returns `"a"`; any missing intermediate step returns `NULL`.
#'
#' @param x A list.
#' @param path A dotted key path, e.g. `"auth.username"`.
#' @return The value at `path`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_settings_get_path <- function(x, path) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  for (p in parts) {
    if (!is.list(x) || is.null(x[[p]])) {
      return(NULL)
    }
    x <- x[[p]]
  }
  x
}

#' Write a dotted-path value into a (possibly nested) list
#'
#' The write-side counterpart of `episodic_settings_get_path()`: creates
#' intermediate sublists as needed, and assigning `NULL` removes the leaf
#' the same way `base[[key]] <- NULL` always does in R.
#'
#' @param x A list (modified and returned; lists are copy-on-modify in R,
#'   so this never mutates a caller's original in place).
#' @param path A dotted key path, e.g. `"auth.username"`.
#' @param value The value to set.
#' @return `x`, with `path` set to `value`.
#' @keywords internal
#' @noRd
episodic_settings_set_path <- function(x, path, value) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    x[[parts]] <- value
    return(x)
  }
  head <- parts[1]
  rest <- paste(parts[-1], collapse = ".")
  sub <- if (is.list(x[[head]])) x[[head]] else list()
  x[[head]] <- episodic_settings_set_path(sub, rest, value)
  x
}

#' Assemble a `notifications` config list from submitted Settings-screen inputs
#'
#' @param input The Shiny server function's `input`.
#' @param existing The `notifications` section currently in effect (from
#'   `episodic_config_resolve(con = con)$notifications`), used only so a
#'   `"secret"` field left blank on screen keeps its stored value instead
#'   of being wiped - the screen never round-trips a real secret value
#'   back into the browser (`episodic_ui_settings_notif_channel()` renders
#'   those inputs empty with a "leave blank to keep" hint), so "blank" is
#'   ambiguous between "no change" and "clear it", and this treats it as
#'   "no change".
#' @return A list suitable for `config$notifications`.
#' @keywords internal
#' @noRd
episodic_app_settings_build_notifications <- function(input, existing) {
  specs <- episodic_settings_channel_specs()
  channels <- list()
  for (ch in names(specs)) {
    channel <- list(
      enabled = isTRUE(input[[paste0("stg_notif_", ch, "_enabled")]])
    )
    existing_channel <- existing$channels[[ch]]
    for (f in specs[[ch]]$fields) {
      id <- episodic_settings_field_id(ch, f$key)
      raw <- input[[id]]
      value <- switch(f$type,
        checkbox = isTRUE(raw),
        number = {
          v <- suppressWarnings(as.numeric(raw))
          if (length(v) == 0 || is.na(v)) NULL else v
        },
        textlist = {
          v <- trimws(strsplit(raw %||% "", ",", fixed = TRUE)[[1]])
          v <- v[nzchar(v)]
          if (length(v) == 0) NULL else as.list(v)
        },
        secret = {
          v <- trimws(raw %||% "")
          if (nzchar(v)) {
            v
          } else {
            episodic_settings_get_path(existing_channel, f$key)
          }
        },
        {
          v <- trimws(raw %||% "")
          if (nzchar(v)) v else NULL
        }
      )
      channel <- episodic_settings_set_path(channel, f$key, value)
    }
    channels[[ch]] <- channel
  }

  dashboard_url <- trimws(input$stg_notif_dashboard_url %||% "")
  list(
    enabled = isTRUE(input$stg_notif_enabled),
    dashboard_url = if (nzchar(dashboard_url)) dashboard_url else NULL,
    triggers = list(
      new_clusters = isTRUE(input$stg_notif_trigger_new_clusters),
      run_failure = isTRUE(input$stg_notif_trigger_run_failure)
    ),
    channels = channels
  )
}

#' Wire the Settings screen's server-side logic
#'
#' Every handler re-resolves the signed-in account with
#' `episodic_auth_refresh_user()` and re-checks `episodic_user_is_admin()`
#' before writing anything - the same server-side trust boundary every
#' other write action in this app applies (the nav link and the forms
#' themselves are just what an admin normally sees, not what is actually
#' enforced), with the extra requirement here that an account another
#' admin has just revoked or demoted cannot keep using its own
#' already-open session to write either.
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param db_path Path to the database (for `episodic_config_export()`'s
#'   sibling `config_exports/` directory).
#' @param lang Session language.
#' @param current_user A `shiny::reactiveVal` holding the signed-in user's
#'   account row, or `NULL`.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episodic_app_server_settings <- function(input,
                                         output,
                                         session,
                                         con,
                                         db_path,
                                         lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                         current_user) {
  settings_message <- shiny::reactiveVal(NULL)
  settings_version <- shiny::reactiveVal(0)
  settings_touch <- function() settings_version(settings_version() + 1)

  output$settings_screen <- shiny::renderUI({
    settings_version()
    user <- current_user()
    if (!episodic_user_is_admin(user)) {
      return(shiny::tags$p(episodic_tr("settings.no_access", lang = lang)))
    }
    episodic_ui_settings_screen(con, lang = lang, message = settings_message())
  })

  shiny::observeEvent(input$settings_notif_save, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_admin(user))

    existing <- episodic_config_resolve(con = con)$notifications
    notif <- episodic_app_settings_build_notifications(input, existing)
    episodic_db_app_config_event_insert(
      con,
      user_id = user$user_id,
      section = "notifications",
      config_json = as.character(jsonlite::toJSON(
        notif,
        auto_unbox = TRUE,
        null = "null"
      ))
    )
    settings_message(list(
      type = "success",
      text = episodic_tr("settings.notif.saved", lang = lang)
    ))
    settings_touch()
  })

  shiny::observeEvent(input$settings_config_export, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_admin(user))
    result <- tryCatch(
      episodic_config_export(db_path = db_path),
      error = function(e) e
    )
    settings_message(
      if (inherits(result, "error")) {
        list(
          type = "error",
          text = episodic_tr(
            "settings.export.failed",
            error = conditionMessage(result),
            lang = lang
          )
        )
      } else {
        list(
          type = "success",
          text = episodic_tr(
            "settings.export.saved",
            path = result,
            lang = lang
          )
        )
      }
    )
    settings_touch()
  })

  shiny::observeEvent(input$settings_user_create, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_admin(user))
    rlang::check_installed("sodium")

    username <- trimws(input$stg_user_new_username %||% "")
    full_name <- trimws(input$stg_user_new_full_name %||% "")
    email <- trimws(input$stg_user_new_email %||% "")
    password <- input$stg_user_new_password %||% ""
    role <- input$stg_user_new_role %||% "epidemiologist"
    is_admin_new <- isTRUE(input$stg_user_new_is_admin)

    problems <- character(0)
    if (!nzchar(username)) {
      problems <- c(
        problems,
        episodic_tr("settings.users.err_username", lang = lang)
      )
    }
    if (!nzchar(full_name)) {
      problems <- c(
        problems,
        episodic_tr("settings.users.err_full_name", lang = lang)
      )
    }
    if (!nzchar(email)) {
      problems <- c(
        problems,
        episodic_tr("settings.users.err_email", lang = lang)
      )
    }
    if (nchar(password) < 8) {
      problems <- c(
        problems,
        episodic_tr("auth.password_too_short", lang = lang)
      )
    }
    # The role <select> only ever submits one of these two values, but the
    # Shiny input is client-controlled like any other - validated with a
    # clear message here rather than left to surface as an opaque CHECK
    # constraint violation from episodic_db_app_user_insert().
    if (!role %in% c("epidemiologist", "viewer")) {
      problems <- c(
        problems,
        episodic_tr("settings.users.err_role", lang = lang)
      )
    }
    if (
      length(problems) == 0 &&
        !is.null(episodic_db_user_by_username(con, username))
    ) {
      problems <- c(
        problems,
        episodic_tr("settings.users.err_duplicate", lang = lang)
      )
    }

    if (length(problems) > 0) {
      settings_message(list(
        type = "error",
        text = paste(problems, collapse = " ")
      ))
      return(invisible(NULL))
    }

    episodic_db_app_user_insert(
      con,
      username = username,
      full_name = full_name,
      email = email,
      password_hash = sodium::password_store(password),
      role = role,
      is_admin = is_admin_new
    )
    settings_message(list(
      type = "success",
      text = episodic_tr(
        "settings.users.created",
        username = username,
        lang = lang
      )
    ))
    settings_touch()
  })

  shiny::observeEvent(input$settings_user_role_change, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_admin(user))
    parts <- strsplit(input$settings_user_role_change, ":", fixed = TRUE)[[1]]
    episodic_auth_set_role(con, as.integer(parts[1]), user$user_id, parts[2])
    settings_touch()
  })

  shiny::observeEvent(input$settings_user_admin_change, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_admin(user))
    parts <- strsplit(input$settings_user_admin_change, ":", fixed = TRUE)[[1]]
    target_id <- as.integer(parts[1])
    new_is_admin <- identical(parts[2], "true")
    # An admin revoking their *own* is_admin flag would lock every admin
    # account out of Settings at once (nothing left in the UI to grant it
    # back - only episodic_auth_set_admin() at the console could recover),
    # so the one case this refuses is exactly that: self-revocation.
    # Revoking someone else's, even the last other admin's, is still
    # allowed - that is a deliberate access decision this account is
    # entitled to make.
    if (identical(target_id, user$user_id) && !new_is_admin) {
      settings_message(list(
        type = "error",
        text = episodic_tr("settings.users.err_self_lockout", lang = lang)
      ))
      return(invisible(NULL))
    }
    episodic_auth_set_admin(con, target_id, user$user_id, new_is_admin)
    settings_touch()
  })

  shiny::observeEvent(input$settings_user_active_change, {
    user <- episodic_auth_refresh_user(con, current_user())
    shiny::req(episodic_user_is_admin(user))
    parts <- strsplit(input$settings_user_active_change, ":", fixed = TRUE)[[1]]
    target_id <- as.integer(parts[1])
    new_is_active <- identical(parts[2], "true")
    if (identical(target_id, user$user_id) && !new_is_active) {
      settings_message(list(
        type = "error",
        text = episodic_tr("settings.users.err_self_lockout", lang = lang)
      ))
      return(invisible(NULL))
    }
    episodic_auth_set_active(con, target_id, user$user_id, new_is_active)
    settings_touch()
  })

  invisible(NULL)
}
