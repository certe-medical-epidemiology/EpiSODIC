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

#' The Settings screen
#'
#' Visible only to an `is_admin` account - `episodic_app_server_settings()`
#' shows `settings.no_access` instead for anyone else who somehow reaches
#' `view() == "settings"` (the nav link itself is already hidden from
#' them, per `episodic_ui_nav_links()`).
#'
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language.
#' @param message An optional `list(type = "success"|"error", text = ...)`
#'   banner from the most recent action, or `NULL`.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_settings_screen <- function(
  con,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  message = NULL
) {
  config <- episodic_config_resolve(con = con)
  users <- episodic_db_app_users(con)
  # role/is_admin/is_active are insert-only attributes (see R/auth.R); the
  # raw episodic_app_user row only carries each account's *original*
  # value, so every row is resolved against its event history before the
  # table renders, the same way episodic_auth_login() resolves the
  # signed-in user's own row.
  if (nrow(users) > 0) {
    users <- do.call(
      rbind,
      lapply(seq_len(nrow(users)), function(i) {
        episodic_auth_resolve_user(con, users[i, ])
      })
    )
  }
  audit <- episodic_db_app_config_events(
    con,
    section = "notifications",
    limit = 20
  )

  shiny::tags$div(
    class = "episodic-streams-screen",
    if (!is.null(message)) {
      shiny::tags$div(
        class = if (identical(message$type, "error")) {
          "episodic-form-error"
        } else {
          "episodic-form-hint"
        },
        style = "margin-bottom:14px;font-size:12.5px;",
        message$text
      )
    },
    episodic_ui_settings_users_panel(users, lang = lang),
    episodic_ui_settings_access_panel(config, lang = lang),
    episodic_ui_settings_detection_panel(config, lang = lang),
    episodic_ui_settings_notif_panel(config$notifications, lang = lang),
    episodic_ui_settings_export_panel(lang = lang),
    episodic_ui_settings_audit_panel(audit, lang = lang),
    shiny::tags$script(shiny::HTML(episodic_ui_settings_bind_script()))
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_notif_panel <- function(
  notif,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  notif <- notif %||% list()
  episodic_ui_panel(
    episodic_tr("settings.notif.title", lang = lang),
    note = episodic_tr("settings.notif.note", lang = lang),
    shiny::tags$div(
      class = "episodic-settings-notif-master",
      episodic_ui_settings_toggle(
        id = "stg_notif_enabled",
        label = episodic_tr("settings.notif.master_enabled", lang = lang),
        checked = isTRUE(notif$enabled),
        emphasise = TRUE
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("settings.notif.dashboard_url", lang = lang)
      ),
      shiny::tags$input(
        type = "text",
        id = "stg_notif_dashboard_url",
        class = "episodic-settings-input",
        value = notif$dashboard_url %||% ""
      )
    ),
    shiny::tags$div(
      class = "episodic-settings-notif-triggers",
      shiny::tags$label(
        class = "episodic-form-checkbox-label",
        shiny::tags$input(
          type = "checkbox",
          id = "stg_notif_trigger_new_clusters",
          class = "episodic-settings-input",
          checked = if (isTRUE(notif$triggers$new_clusters)) NA else NULL
        ),
        episodic_tr("settings.notif.trigger_new_clusters", lang = lang)
      ),
      shiny::tags$label(
        class = "episodic-form-checkbox-label",
        shiny::tags$input(
          type = "checkbox",
          id = "stg_notif_trigger_run_failure",
          class = "episodic-settings-input",
          checked = if (isTRUE(notif$triggers$run_failure)) NA else NULL
        ),
        episodic_tr("settings.notif.trigger_run_failure", lang = lang)
      )
    ),
    shiny::tags$div(
      class = "episodic-settings-channels-grid",
      lapply(names(episodic_settings_channel_specs()), function(ch) {
        episodic_ui_settings_notif_channel(ch, notif, lang = lang)
      })
    ),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('settings_notif_save', Math.random(), {priority: 'event'})",
        episodic_tr("settings.notif.save", lang = lang)
      )
    )
  )
}

#' A track+thumb toggle switch bound the same way as a checkbox
#'
#' Renders a native checkbox (still carrying `episodic-settings-input`,
#' so `episodic_ui_settings_bind_script()` binds it exactly like any
#' other field) visually replaced by a track and thumb, for the handful
#' of on/off controls - the notifications master switch and each
#' channel's own enabled state - that sit in a header row where a bare
#' checkbox reads as an afterthought next to a bold title.
#' @param id Input id.
#' @param label Visible label, placed after the switch.
#' @param checked Initial state.
#' @param emphasise Slightly larger label, for the panel's own master switch.
#' @keywords internal
#' @noRd
episodic_ui_settings_toggle <- function(id, label, checked = FALSE, emphasise = FALSE) {
  shiny::tags$label(
    class = "episodic-toggle",
    shiny::tags$input(
      type = "checkbox",
      id = id,
      class = "episodic-settings-input",
      checked = if (isTRUE(checked)) NA else NULL
    ),
    shiny::tags$span(class = "episodic-toggle-track"),
    shiny::tags$span(
      class = "episodic-toggle-label",
      style = if (emphasise) "font-size:13.5px;font-weight:600;",
      label
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_notif_channel <- function(
  channel,
  notif,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  current <- notif$channels[[channel]]

  shiny::tags$div(
    class = "episodic-settings-channel",
    shiny::tags$div(
      class = "episodic-settings-channel-title",
      shiny::tags$span(episodic_tr(
        paste0("settings.notif.channel.", channel),
        lang = lang
      )),
      episodic_ui_settings_toggle(
        id = paste0("stg_notif_", channel, "_enabled"),
        label = episodic_tr("settings.notif.enabled", lang = lang),
        checked = isTRUE(current$enabled)
      )
    ),
    shiny::tags$div(
      class = "episodic-settings-channel-grid",
      lapply(
        episodic_settings_channel_specs()[[channel]]$fields,
        function(f) episodic_ui_settings_field(channel, f, current, lang = lang)
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_field <- function(
  channel,
  field,
  current,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  id <- episodic_settings_field_id(channel, field$key)
  label <- episodic_tr(
    paste0("settings.notif.field.", gsub(".", "_", field$key, fixed = TRUE)),
    lang = lang
  )
  value <- episodic_settings_get_path(current, field$key)

  if (field$type == "checkbox") {
    return(shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-checkbox-label",
        shiny::tags$input(
          type = "checkbox",
          id = id,
          class = "episodic-settings-input",
          checked = if (isTRUE(value)) NA else NULL
        ),
        label
      )
    ))
  }

  if (field$type == "secret") {
    return(shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(class = "episodic-form-label", label),
      shiny::tags$input(
        type = "password",
        id = id,
        class = "episodic-settings-input",
        autocomplete = "new-password",
        placeholder = if (!is.null(value) && nzchar(value)) {
          episodic_tr("settings.notif.secret_set", lang = lang)
        } else {
          ""
        }
      ),
      shiny::tags$p(
        class = "episodic-form-hint",
        episodic_tr("settings.notif.secret_hint", lang = lang)
      )
    ))
  }

  display <- if (field$type == "textlist") {
    if (is.null(value)) "" else paste(unlist(value), collapse = ", ")
  } else if (is.null(value)) {
    ""
  } else {
    as.character(value)
  }

  shiny::tags$div(
    class = "episodic-form-group",
    shiny::tags$label(class = "episodic-form-label", label),
    shiny::tags$input(
      type = if (field$type == "number") "number" else "text",
      id = id,
      class = "episodic-settings-input",
      value = display
    )
  )
}

#' The read-only dashboard-access panel
#'
#' `access.require_login` is a YAML-only setting with no in-app override,
#' deliberately (see `episodic_app_require_login()`), so this panel reads
#' rather than edits: an admin can confirm which way the instance is set
#' without SSH access, and changing it still takes file access to the
#' machine.
#'
#' @param config A resolved configuration.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_settings_access_panel <- function(
  config,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  required <- episodic_app_require_login(config)
  pal <- episodic_palette()
  episodic_ui_panel(
    episodic_tr("settings.access.title", lang = lang),
    note = episodic_tr("settings.access.note", lang = lang),
    shiny::tags$div(
      class = "episodic-settings-kv",
      shiny::tags$span(
        class = "episodic-settings-kv-label",
        episodic_tr("settings.access.require_login", lang = lang)
      ),
      shiny::tags$span(
        class = "episodic-settings-kv-value",
        episodic_ui_chip(
          episodic_tr(
            if (required) "settings.access.closed" else "settings.access.open",
            lang = lang
          ),
          if (required) pal$success_dark else pal$muted,
          filled = TRUE
        )
      )
    ),
    shiny::tags$p(
      class = "episodic-panel-note",
      episodic_tr(
        if (required) {
          "settings.access.closed_detail"
        } else {
          "settings.access.open_detail"
        },
        lang = lang
      )
    )
  )
}

#' The read-only detection configuration grid
#' @keywords internal
#' @noRd
episodic_ui_settings_detection_panel <- function(
  config,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  # `notifications` has its own editable panel above; `access` has its
  # own read-only one. Neither is detection configuration, and listing
  # them here under that heading would say they were.
  sections <- config[setdiff(names(config), episodic_config_unhashed_sections)]
  episodic_ui_panel(
    episodic_tr("settings.detection.title", lang = lang),
    note = episodic_tr("settings.detection.note", lang = lang),
    lapply(names(sections), function(section) {
      rows <- episodic_settings_flatten(sections[[section]])
      shiny::tags$div(
        class = "episodic-settings-section",
        shiny::tags$h3(
          class = "episodic-settings-section-title",
          episodic_settings_prettify_label(section)
        ),
        shiny::tags$div(
          class = "episodic-settings-readonly-grid",
          lapply(names(rows), function(key) {
            shiny::tags$div(
              class = "episodic-settings-kv",
              shiny::tags$span(
                class = "episodic-settings-kv-label",
                episodic_settings_prettify_label(key)
              ),
              shiny::tags$span(class = "episodic-settings-kv-value", rows[[key]])
            )
          })
        )
      )
    })
  )
}

#' Turn a flattened config key into a readable label
#'
#' `"case_free_days_default"` becomes `"Case free days default"`,
#' `"overrides.norovirus.k_days"` becomes `"Overrides norovirus k days"`.
#' Single-letter Farrington parameters (`w`, `b`) round-trip unchanged
#' bar the capital, which is the correct reading of those names - they
#' are the method's own notation, not a UI shortcoming.
#' @keywords internal
#' @noRd
episodic_settings_prettify_label <- function(key) {
  words <- strsplit(gsub("[._]+", " ", key), " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
  paste(words, collapse = " ")
}

#' Flatten a nested config list into `"a.b.c" -> "display value"` pairs
#' @keywords internal
#' @noRd
episodic_settings_flatten <- function(x, prefix = "") {
  out <- list()
  for (nm in names(x)) {
    key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
    val <- x[[nm]]
    if (is.list(val) && !is.null(names(val))) {
      out <- c(out, episodic_settings_flatten(val, key))
    } else {
      out[[key]] <- paste(unlist(val), collapse = ", ")
    }
  }
  out
}

#' @keywords internal
#' @noRd
episodic_ui_settings_users_panel <- function(
  users,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  episodic_ui_panel(
    episodic_tr("settings.users.title", lang = lang),
    if (nrow(users) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("settings.users.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr(
            "settings.users.col.username",
            lang = lang
          )),
          shiny::tags$th(episodic_tr(
            "settings.users.col.full_name",
            lang = lang
          )),
          shiny::tags$th(episodic_tr("settings.users.col.email", lang = lang)),
          shiny::tags$th(episodic_tr("settings.users.col.role", lang = lang)),
          shiny::tags$th(episodic_tr("settings.users.col.admin", lang = lang)),
          shiny::tags$th(episodic_tr("settings.users.col.active", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(users)), function(i) {
          episodic_ui_settings_user_row(users[i, ], lang = lang)
        }))
      )
    },
    episodic_ui_settings_user_create_form(lang = lang)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_user_row <- function(
  user,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  shiny::tags$tr(
    class = "episodic-settings-user-row",
    shiny::tags$td(user$username),
    shiny::tags$td(user$full_name),
    shiny::tags$td(user$email),
    shiny::tags$td(shiny::tags$select(
      class = "episodic-settings-user-role",
      `data-user-id` = user$user_id,
      shiny::tags$option(
        value = "epidemiologist",
        selected = if (identical(user$role, "epidemiologist")) NA else NULL,
        episodic_tr("settings.users.role.epidemiologist", lang = lang)
      ),
      shiny::tags$option(
        value = "viewer",
        selected = if (identical(user$role, "viewer")) NA else NULL,
        episodic_tr("settings.users.role.viewer", lang = lang)
      )
    )),
    shiny::tags$td(shiny::tags$input(
      type = "checkbox",
      class = "episodic-settings-user-admin",
      `data-user-id` = user$user_id,
      checked = if (isTRUE(as.logical(user$is_admin))) NA else NULL
    )),
    shiny::tags$td(shiny::tags$input(
      type = "checkbox",
      class = "episodic-settings-user-active",
      `data-user-id` = user$user_id,
      checked = if (isTRUE(as.logical(user$is_active))) NA else NULL
    ))
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_user_create_form <- function(
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  shiny::tags$div(
    style = "margin-top:18px;border-top:1px solid var(--episodic-border);padding-top:14px;",
    shiny::tags$div(
      class = "episodic-settings-channel-grid",
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("auth.username", lang = lang)
        ),
        shiny::tags$input(
          type = "text",
          id = "stg_user_new_username",
          class = "episodic-settings-input",
          autocomplete = "off"
        )
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("settings.users.col.full_name", lang = lang)
        ),
        shiny::tags$input(
          type = "text",
          id = "stg_user_new_full_name",
          class = "episodic-settings-input"
        )
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("settings.users.col.email", lang = lang)
        ),
        shiny::tags$input(
          type = "email",
          id = "stg_user_new_email",
          class = "episodic-settings-input"
        )
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("settings.users.temp_password", lang = lang)
        ),
        shiny::tags$input(
          type = "password",
          id = "stg_user_new_password",
          class = "episodic-settings-input",
          autocomplete = "new-password"
        )
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-label",
          episodic_tr("settings.users.col.role", lang = lang)
        ),
        shiny::tags$select(
          id = "stg_user_new_role",
          class = "episodic-settings-input",
          shiny::tags$option(
            value = "epidemiologist",
            episodic_tr("settings.users.role.epidemiologist", lang = lang)
          ),
          shiny::tags$option(
            value = "viewer",
            episodic_tr("settings.users.role.viewer", lang = lang)
          )
        )
      ),
      shiny::tags$div(
        class = "episodic-form-group",
        shiny::tags$label(
          class = "episodic-form-checkbox-label",
          shiny::tags$input(
            type = "checkbox",
            id = "stg_user_new_is_admin",
            class = "episodic-settings-input"
          ),
          episodic_tr("settings.users.col.admin", lang = lang)
        )
      )
    ),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('settings_user_create', Math.random(), {priority: 'event'})",
        episodic_tr("settings.users.create", lang = lang)
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_export_panel <- function(
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  episodic_ui_panel(
    episodic_tr("settings.export.title", lang = lang),
    note = episodic_tr("settings.export.note", lang = lang),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        class = "episodic-btn",
        onclick = "Shiny.setInputValue('settings_config_export', Math.random(), {priority: 'event'})",
        episodic_tr("settings.export.button", lang = lang)
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_settings_audit_panel <- function(
  audit,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  episodic_ui_panel(
    episodic_tr("settings.audit.title", lang = lang),
    if (nrow(audit) == 0) {
      shiny::tags$p(
        class = "episodic-panel-empty",
        episodic_tr("settings.audit.empty", lang = lang)
      )
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("settings.audit.col.when", lang = lang)),
          shiny::tags$th(episodic_tr("settings.audit.col.who", lang = lang)),
          shiny::tags$th(episodic_tr("settings.audit.col.section", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(audit)), function(i) {
          row <- audit[i, ]
          shiny::tags$tr(
            shiny::tags$td(episodic_ui_format_datetime(
              row$created_at,
              fmt = "%d-%m-%Y %H:%M"
            )),
            shiny::tags$td(
              row$actor_username %||% episodic_tr("misc.unknown", lang = lang)
            ),
            shiny::tags$td(row$section)
          )
        }))
      )
    }
  )
}

#' The client-side script binding every dynamic Settings-screen input
#'
#' One script instead of one inline `onclick`/`.on()` call per field
#' (there are dozens, across six notification channels plus the user
#' table): every field carries a fixed CSS hook class
#' (`episodic-settings-input`, or one of the three `episodic-settings-user-*`
#' classes for the per-row role/admin/active controls, which additionally
#' carry the row's `user_id` as `data-user-id` since their target Shiny
#' input is shared across every row), and this binds all of them found in
#' the DOM at the time it runs. Safe to re-run on every re-render, unlike
#' the delegated (`$(document).on(...)`) style used elsewhere in this
#' package: `episodic_ui_settings_screen()` is rendered wholesale into a
#' single `uiOutput`, so a re-render always destroys the old elements
#' (and their listeners) before this script's `querySelectorAll` sees the
#' new ones - never both at once.
#' @keywords internal
#' @noRd
episodic_ui_settings_bind_script <- function() {
  "(function(){
    document.querySelectorAll('.episodic-settings-input').forEach(function(el){
      var send = function(){
        Shiny.setInputValue(el.id, el.type === 'checkbox' ? el.checked : el.value);
      };
      el.addEventListener('input', send);
      el.addEventListener('change', send);
    });
    document.querySelectorAll('.episodic-settings-user-role').forEach(function(el){
      el.addEventListener('change', function(){
        Shiny.setInputValue('settings_user_role_change', el.dataset.userId + ':' + el.value, {priority: 'event'});
      });
    });
    document.querySelectorAll('.episodic-settings-user-admin').forEach(function(el){
      el.addEventListener('change', function(){
        Shiny.setInputValue('settings_user_admin_change', el.dataset.userId + ':' + el.checked, {priority: 'event'});
      });
    });
    document.querySelectorAll('.episodic-settings-user-active').forEach(function(el){
      el.addEventListener('change', function(){
        Shiny.setInputValue('settings_user_active_change', el.dataset.userId + ':' + el.checked, {priority: 'event'});
      });
    });
  })();"
}
