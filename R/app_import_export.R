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

# The Import and export screen: importing from and exporting to partner
# instances (see
# vignette("import-export")). Reached from its card on the Instance screen,
# and only by an is_admin account, like Settings: the card is omitted
# for everyone else, the screen renders nothing for them, and every
# handler here re-resolves the signed-in account and re-checks is_admin
# before it writes or exports anything.
#
# What lives here:
#  - a download of an encrypted export, built in this process with the
#    signing key in its own EPISODIC_EXPORT_SIGNING_KEY;
#  - export schedules and import sources, recorded as app-owned events
#    and carried out by episodic_run_cron(), never by the dashboard;
#  - a connection test per schedule and source;
#  - the audit trail of every export and import.
#
# The app never writes cases: an import source is a record of where the
# next run fetches from, and the run is what takes the cases in.

#' The Import and export screen's data
#' @param con A [DBI::DBIConnection-class].
#' @return A list with `instance_id` (or the error reading it),
#'   `schedules`, `sources`, `exports`, `imports`, `users` (id to full
#'   name) and `signing_fingerprint` (of the key in this process's
#'   `EPISODIC_EXPORT_SIGNING_KEY`, or the error reading it).
#' @keywords internal
#' @noRd
episodic_app_import_export <- function(con) {
  users <- episodic_db_app_users(con)
  signing <- tryCatch(
    episodic_import_export_fingerprint(sodium::sig_pubkey(episodic_import_export_key_read(
      Sys.getenv("EPISODIC_EXPORT_SIGNING_KEY", unset = NA),
      "signing",
      "secret",
      "The signing key",
      env = "EPISODIC_EXPORT_SIGNING_KEY"
    ))),
    error = function(e) e
  )
  list(
    instance_id = tryCatch(episodic_db_instance_id(con), error = function(e) e),
    schedules = episodic_export_schedules_current(con),
    sources = episodic_import_sources_current(con),
    exports = episodic_db_export_events(con, limit = 100L),
    imports = episodic_db_import_events(con, limit = 100L),
    users = stats::setNames(users$full_name, users$user_id),
    signing_fingerprint = signing
  )
}

#' A labelled form field on the Import and export screen
#'
#' Every field carries `episodic-import-export-input`, which
#' `episodic_ui_import_export_bind_script()` binds; a field never touched
#' sends nothing, so the server reads each with the default shown here.
#' @param id The input id.
#' @param label The label text.
#' @param type An `<input>` type, or `"textarea"` or `"select"`.
#' @param value The initial value.
#' @param choices For a select: a named character vector, value to label.
#' @param hint An optional line under the field.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_import_export_field <- function(id,
                                            label,
                                            type = "text",
                                            value = "",
                                            choices = NULL,
                                            hint = NULL) {
  control <- switch(type,
    textarea = shiny::tags$textarea(
      id = id,
      class = "episodic-import-export-input",
      rows = 4,
      spellcheck = "false",
      value
    ),
    select = shiny::tags$select(
      id = id,
      class = "episodic-import-export-input",
      lapply(names(choices), function(v) {
        shiny::tags$option(
          value = v,
          selected = if (identical(v, value)) NA else NULL,
          choices[[v]]
        )
      })
    ),
    checkbox = NULL,
    shiny::tags$input(
      type = type,
      id = id,
      class = "episodic-import-export-input",
      value = value,
      autocomplete = "off"
    )
  )
  shiny::tags$div(
    class = "episodic-form-group",
    if (identical(type, "checkbox")) {
      shiny::tags$label(
        class = "episodic-form-checkbox-label",
        shiny::tags$input(
          type = "checkbox",
          id = id,
          class = "episodic-import-export-input",
          checked = if (isTRUE(value)) NA else NULL
        ),
        label
      )
    } else {
      shiny::tagList(
        shiny::tags$label(class = "episodic-form-label", `for` = id, label),
        control
      )
    },
    if (!is.null(hint)) {
      shiny::tags$div(class = "episodic-form-hint", hint)
    }
  )
}

#' The optional-column checkboxes of an export form
#' @param prefix The input id prefix.
#' @param selected The columns ticked.
#' @param lang Session language.
#' @keywords internal
#' @noRd
episodic_ui_import_export_fields <- function(prefix, selected, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::tags$div(
    class = "episodic-form-group",
    shiny::tags$span(class = "episodic-form-label", episodic_tr("import_export.form.fields", lang = lang)),
    shiny::tags$div(
      class = "episodic-settings-channel-grid",
      lapply(episodic_import_export_omittable_fields(), function(field) {
        shiny::tags$label(
          class = "episodic-form-checkbox-label",
          shiny::tags$input(
            type = "checkbox",
            id = paste0(prefix, field),
            class = "episodic-import-export-input",
            checked = if (field %in% selected) NA else NULL
          ),
          shiny::tags$code(field)
        )
      })
    ),
    shiny::tags$div(class = "episodic-form-hint", episodic_tr("import_export.form.fields_hint", lang = lang))
  )
}

#' The mode choice, public key first and preferred
#' @keywords internal
#' @noRd
episodic_ui_import_export_mode_choices <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  c(
    public_key = episodic_tr("import_export.mode.public_key", lang = lang),
    passphrase = episodic_tr("import_export.mode.passphrase", lang = lang)
  )
}

#' A row action button: test or cancel a schedule or source
#' @keywords internal
#' @noRd
episodic_ui_import_export_action <- function(action, name, label, primary = FALSE) {
  shiny::tags$button(
    type = "button",
    class = paste("episodic-btn", if (primary) "episodic-btn-primary" else NULL, "episodic-import-export-action"),
    `data-action` = action,
    `data-name` = name,
    label
  )
}

#' The Import and export screen
#' @param data `episodic_app_import_export()`'s result.
#' @param config A resolved configuration.
#' @param lang Session language.
#' @param message A list with `type` and `text`, or `NULL`.
#' @param test An `episodic_import_export_test` to show, or `NULL`.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_import_export_screen <- function(data,
                                             config,
                                             lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                             message = NULL,
                                             test = NULL) {
  default_fields <- unlist(config$export$fields) %||% character(0)
  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(class = "episodic-screen-title", episodic_tr("nav.import_export", lang = lang)),
    shiny::tags$p(class = "episodic-screen-lead", episodic_tr("import_export.lead", lang = lang)),
    if (!is.null(message)) {
      shiny::tags$div(
        class = if (identical(message$type, "error")) "episodic-form-error" else "episodic-form-hint",
        role = if (identical(message$type, "error")) "alert" else "status",
        style = "margin-bottom:14px;font-size:12.5px;",
        message$text
      )
    },
    episodic_ui_import_export_identity_panel(data, lang = lang),
    if (!is.null(test)) episodic_ui_import_export_test_panel(test, lang = lang),
    episodic_ui_import_export_download_panel(data, default_fields, lang = lang),
    episodic_ui_import_export_schedules_panel(data, default_fields, lang = lang),
    episodic_ui_import_export_sources_panel(data, lang = lang),
    episodic_ui_import_export_audit_panel(data, lang = lang),
    shiny::tags$script(shiny::HTML(episodic_ui_import_export_bind_script()))
  )
}

#' @keywords internal
#' @noRd
episodic_ui_import_export_identity_panel <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  id <- data$instance_id
  episodic_ui_panel(
    episodic_tr("import_export.identity.title", lang = lang),
    note = episodic_tr("import_export.identity.note", lang = lang),
    shiny::tags$div(
      class = "episodic-settings-kv",
      shiny::tags$span(class = "episodic-settings-kv-label", episodic_tr("info.about.instance", lang = lang)),
      shiny::tags$span(
        class = "episodic-settings-kv-value",
        if (inherits(id, "error")) {
          episodic_tr("info.about.instance_unreadable", error = conditionMessage(id), lang = lang)
        } else {
          shiny::tags$code(id)
        }
      )
    ),
    shiny::tags$div(
      class = "episodic-settings-kv",
      shiny::tags$span(class = "episodic-settings-kv-label", episodic_tr("import_export.identity.signing", lang = lang)),
      shiny::tags$span(
        class = "episodic-settings-kv-value",
        if (inherits(data$signing_fingerprint, "error")) {
          episodic_tr("import_export.identity.signing_unset", lang = lang)
        } else {
          shiny::tags$code(episodic_import_export_fingerprint_short(data$signing_fingerprint))
        }
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_import_export_download_panel <- function(data,
                                                     default_fields,
                                                     lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  today <- Sys.Date()
  episodic_ui_panel(
    episodic_tr("import_export.download.title", lang = lang),
    note = episodic_tr("import_export.download.note", lang = lang),
    shiny::tags$div(
      class = "episodic-settings-channel-grid",
      episodic_ui_import_export_field("impexp_dl_from", episodic_tr("import_export.form.from", lang = lang), "date", format(today - 27, "%Y-%m-%d")),
      episodic_ui_import_export_field("impexp_dl_to", episodic_tr("import_export.form.to", lang = lang), "date", format(today, "%Y-%m-%d")),
      episodic_ui_import_export_field(
        "impexp_dl_mode",
        episodic_tr("import_export.form.mode", lang = lang),
        "select",
        "public_key",
        choices = episodic_ui_import_export_mode_choices(lang = lang),
        hint = episodic_tr("import_export.form.mode_hint", lang = lang)
      ),
      episodic_ui_import_export_field("impexp_dl_pc", episodic_tr("import_export.form.pc_characters", lang = lang), "number", "")
    ),
    episodic_ui_import_export_field(
      "impexp_dl_recipients",
      episodic_tr("import_export.form.recipients", lang = lang),
      "textarea",
      hint = episodic_tr("import_export.form.recipients_hint", lang = lang)
    ),
    episodic_ui_import_export_field(
      "impexp_dl_passphrase",
      episodic_tr("import_export.form.passphrase", lang = lang),
      "password",
      hint = episodic_tr("import_export.form.passphrase_hint", lang = lang)
    ),
    episodic_ui_import_export_fields("impexp_dl_field_", default_fields, lang = lang),
    shiny::tags$div(
      class = "episodic-form-actions",
      if (inherits(data$signing_fingerprint, "error")) {
        shiny::tags$span(class = "episodic-form-error", episodic_tr("import_export.identity.signing_unset", lang = lang))
      } else {
        shiny::downloadButton(
          "impexp_download",
          episodic_tr("import_export.download.button", lang = lang),
          class = "episodic-btn episodic-btn-primary",
          icon = NULL
        )
      }
    )
  )
}

#' The last export or import recorded under a name, as a short status
#' @keywords internal
#' @noRd
episodic_ui_import_export_last <- function(rows, name_col, name, when_col, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  mine <- rows[!is.na(rows[[name_col]]) & rows[[name_col]] == name, , drop = FALSE]
  if (nrow(mine) == 0) {
    return(episodic_tr("import_export.never", lang = lang))
  }
  latest <- mine[1, ]
  ok <- latest$status %in% c("written", "imported")
  pal <- episodic_palette()
  shiny::tagList(
    episodic_ui_chip(
      episodic_tr(paste0("import_export.status.", latest$status), lang = lang),
      if (ok) pal$success_dark else pal$danger_dark,
      filled = TRUE
    ),
    " ",
    episodic_ui_format_stamp(latest[[when_col]], lang = lang)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_import_export_schedules_panel <- function(data,
                                                      default_fields,
                                                      lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  schedules <- data$schedules
  episodic_ui_panel(
    episodic_tr("import_export.schedules.title", lang = lang),
    note = episodic_tr("import_export.schedules.note", lang = lang),
    if (length(schedules) == 0) {
      shiny::tags$p(class = "episodic-panel-empty", episodic_tr("import_export.schedules.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("import_export.col.name", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.cadence", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.mode", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.destination", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.last", lang = lang)),
          shiny::tags$th("")
        )),
        shiny::tags$tbody(lapply(schedules, function(s) {
          shiny::tags$tr(
            shiny::tags$td(s$schedule_name),
            shiny::tags$td(episodic_tr(
              "import_export.cadence",
              interval = episodic_format_number(s$interval_days, lang = lang),
              window = episodic_format_number(s$window_days, lang = lang),
              lang = lang
            )),
            shiny::tags$td(
              episodic_tr(paste0("import_export.mode.", s$mode), lang = lang),
              if (identical(s$mode, "public_key")) {
                paste0(" (", episodic_format_number(length(jsonlite::fromJSON(s$recipients)), lang = lang), ")")
              }
            ),
            shiny::tags$td(shiny::tags$code(
              if (!is.na(s$destination)) s$destination else paste0("env:", s$destination_env)
            )),
            shiny::tags$td(episodic_ui_import_export_last(data$exports, "schedule_name", s$schedule_name, "exported_at", lang = lang)),
            shiny::tags$td(
              episodic_ui_import_export_action("schedule_test", s$schedule_name, episodic_tr("import_export.test.button", lang = lang)),
              " ",
              episodic_ui_import_export_action("schedule_cancel", s$schedule_name, episodic_tr("import_export.cancel.button", lang = lang))
            )
          )
        }))
      )
    },
    shiny::tags$h3(class = "episodic-settings-section-title", episodic_tr("import_export.schedules.form", lang = lang)),
    shiny::tags$div(
      class = "episodic-settings-channel-grid",
      episodic_ui_import_export_field("impexp_sc_name", episodic_tr("import_export.col.name", lang = lang), hint = episodic_tr("import_export.form.name_hint", lang = lang)),
      episodic_ui_import_export_field("impexp_sc_interval", episodic_tr("import_export.form.interval", lang = lang), "number", "7"),
      episodic_ui_import_export_field("impexp_sc_window", episodic_tr("import_export.form.window", lang = lang), "number", "28", hint = episodic_tr("import_export.form.window_hint", lang = lang)),
      episodic_ui_import_export_field("impexp_sc_mode", episodic_tr("import_export.form.mode", lang = lang), "select", "public_key", choices = episodic_ui_import_export_mode_choices(lang = lang)),
      episodic_ui_import_export_field("impexp_sc_signing_env", episodic_tr("import_export.form.signing_env", lang = lang), value = "EPISODIC_EXPORT_SIGNING_KEY"),
      episodic_ui_import_export_field("impexp_sc_passphrase_env", episodic_tr("import_export.form.passphrase_env", lang = lang)),
      episodic_ui_import_export_field("impexp_sc_pc", episodic_tr("import_export.form.pc_characters", lang = lang), "number", "")
    ),
    episodic_ui_import_export_field(
      "impexp_sc_recipients",
      episodic_tr("import_export.form.recipients", lang = lang),
      "textarea",
      hint = episodic_tr("import_export.form.recipients_hint", lang = lang)
    ),
    episodic_ui_import_export_fields("impexp_sc_field_", default_fields, lang = lang),
    episodic_ui_import_export_endpoint_fields("impexp_sc_", "export", lang = lang),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        type = "button",
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('impexp_schedule_save', Math.random(), {priority: 'event'})",
        episodic_tr("import_export.schedules.save", lang = lang)
      )
    )
  )
}

#' The location and credential fields of a schedule or source form
#' @param prefix The input id prefix.
#' @param direction `"export"` or `"import"`.
#' @param lang Session language.
#' @keywords internal
#' @noRd
episodic_ui_import_export_endpoint_fields <- function(prefix, direction, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  types <- episodic_import_export_endpoint_types[[direction]]
  shiny::tags$div(
    class = "episodic-settings-channel-grid",
    episodic_ui_import_export_field(
      paste0(prefix, "loc_type"),
      episodic_tr(paste0("import_export.form.location_type_", direction), lang = lang),
      "select",
      "file",
      choices = stats::setNames(
        vapply(types, function(t) episodic_tr(paste0("import_export.location.", t), lang = lang), character(1)),
        types
      )
    ),
    episodic_ui_import_export_field(
      paste0(prefix, "loc"),
      episodic_tr("import_export.form.location", lang = lang),
      hint = episodic_tr("import_export.form.location_hint", lang = lang)
    ),
    episodic_ui_import_export_field(
      paste0(prefix, "loc_env"),
      episodic_tr("import_export.form.location_env", lang = lang),
      hint = episodic_tr("import_export.form.location_env_hint", lang = lang)
    ),
    episodic_ui_import_export_field(paste0(prefix, "user_env"), episodic_tr("import_export.form.username_env", lang = lang)),
    episodic_ui_import_export_field(paste0(prefix, "pass_env"), episodic_tr("import_export.form.password_env", lang = lang)),
    episodic_ui_import_export_field(paste0(prefix, "token_env"), episodic_tr("import_export.form.token_env", lang = lang))
  )
}

#' @keywords internal
#' @noRd
episodic_ui_import_export_sources_panel <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  sources <- data$sources
  episodic_ui_panel(
    episodic_tr("import_export.sources.title", lang = lang),
    note = episodic_tr("import_export.sources.note", lang = lang),
    if (length(sources) == 0) {
      shiny::tags$p(class = "episodic-panel-empty", episodic_tr("import_export.sources.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("import_export.col.name", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.mode", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.sender", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.location", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.last", lang = lang)),
          shiny::tags$th("")
        )),
        shiny::tags$tbody(lapply(sources, function(s) {
          fingerprint <- tryCatch(
            episodic_import_export_fingerprint_short(episodic_import_export_fingerprint(
              episodic_import_export_key_parse(s$trusted_sender, "signing", "public", "key")
            )),
            error = function(e) NA_character_
          )
          shiny::tags$tr(
            shiny::tags$td(s$source_name),
            shiny::tags$td(episodic_tr(paste0("import_export.mode.", s$mode), lang = lang)),
            shiny::tags$td(shiny::tags$code(fingerprint)),
            shiny::tags$td(shiny::tags$code(
              if (!is.na(s$location)) s$location else paste0("env:", s$location_env)
            )),
            shiny::tags$td(episodic_ui_import_export_last(data$imports, "source_name", s$source_name, "imported_at", lang = lang)),
            shiny::tags$td(
              episodic_ui_import_export_action("source_test", s$source_name, episodic_tr("import_export.test.button", lang = lang)),
              " ",
              episodic_ui_import_export_action("source_cancel", s$source_name, episodic_tr("import_export.cancel.button", lang = lang))
            )
          )
        }))
      )
    },
    shiny::tags$h3(class = "episodic-settings-section-title", episodic_tr("import_export.sources.form", lang = lang)),
    shiny::tags$div(
      class = "episodic-settings-channel-grid",
      episodic_ui_import_export_field("impexp_so_name", episodic_tr("import_export.col.name", lang = lang), hint = episodic_tr("import_export.form.name_hint", lang = lang)),
      episodic_ui_import_export_field("impexp_so_mode", episodic_tr("import_export.form.mode", lang = lang), "select", "public_key", choices = episodic_ui_import_export_mode_choices(lang = lang)),
      episodic_ui_import_export_field("impexp_so_key_env", episodic_tr("import_export.form.key_env", lang = lang), value = "EPISODIC_IMPORT_KEY"),
      episodic_ui_import_export_field("impexp_so_passphrase_env", episodic_tr("import_export.form.passphrase_env", lang = lang))
    ),
    episodic_ui_import_export_field(
      "impexp_so_sender",
      episodic_tr("import_export.form.sender", lang = lang),
      "textarea",
      hint = episodic_tr("import_export.form.sender_hint", lang = lang)
    ),
    episodic_ui_import_export_endpoint_fields("impexp_so_", "import", lang = lang),
    shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        type = "button",
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('impexp_source_save', Math.random(), {priority: 'event'})",
        episodic_tr("import_export.sources.save", lang = lang)
      )
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_import_export_test_panel <- function(test, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  episodic_ui_panel(
    episodic_tr(
      if (test$ok) "import_export.test.passed" else "import_export.test.failed",
      name = test$name,
      lang = lang
    ),
    shiny::tags$ul(
      class = "episodic-import-export-test",
      lapply(test$lines, function(line) {
        shiny::tags$li(
          episodic_ui_chip(
            episodic_tr(
              if (isTRUE(line$ok)) "import_export.test.mark_ok" else if (identical(line$ok, FALSE)) "import_export.test.mark_fail" else "import_export.test.mark_info",
              lang = lang
            ),
            if (isTRUE(line$ok)) pal$success_dark else if (identical(line$ok, FALSE)) pal$danger_dark else pal$muted,
            filled = TRUE
          ),
          " ",
          episodic_import_export_test_text(line, lang = lang)
        )
      })
    )
  )
}

#' Who an export was made by, as the audit trail says it
#' @keywords internal
#' @noRd
episodic_ui_import_export_export_who <- function(row, users, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  switch(row$initiated_by,
    download = episodic_tr(
      "import_export.who.download",
      user = if (!is.na(row$user_id) && as.character(row$user_id) %in% names(users)) {
        users[[as.character(row$user_id)]]
      } else {
        episodic_tr("activity.actor_unknown", lang = lang)
      },
      lang = lang
    ),
    schedule = episodic_tr("import_export.who.schedule", name = row$schedule_name, lang = lang),
    episodic_tr("import_export.who.headless", account = row$account, host = row$host, lang = lang)
  )
}

#' The range an export or import covers, as dates in the reader's language
#' @keywords internal
#' @noRd
episodic_ui_import_export_range <- function(from, to, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (is.na(from) || is.na(to)) {
    return("")
  }
  episodic_format_date_range(from, to, lang = lang)
}

#' @keywords internal
#' @noRd
episodic_ui_import_export_audit_panel <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  exports <- data$exports
  imports <- data$imports
  pal <- episodic_palette()
  status_chip <- function(status) {
    episodic_ui_chip(
      episodic_tr(paste0("import_export.status.", status), lang = lang),
      if (status %in% c("written", "imported")) pal$success_dark else pal$danger_dark,
      filled = TRUE
    )
  }
  count <- function(n) if (is.na(n)) "" else episodic_format_number(n, lang = lang)
  short <- function(x) if (is.na(x)) "" else substr(x, 1L, 12L)
  episodic_ui_panel(
    episodic_tr("import_export.audit.title", lang = lang),
    note = episodic_tr("import_export.audit.note", lang = lang),
    shiny::tags$h3(class = "episodic-settings-section-title", episodic_tr("import_export.audit.exports", lang = lang)),
    if (nrow(exports) == 0) {
      shiny::tags$p(class = "episodic-panel-empty", episodic_tr("import_export.audit.exports_empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("import_export.col.when", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.who", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.status", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.range", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.cases", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.recipients", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.file", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.destination", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(exports)), function(i) {
          row <- exports[i, ]
          fingerprints <- if (is.na(row$recipient_fingerprints)) {
            character(0)
          } else {
            as.character(jsonlite::fromJSON(row$recipient_fingerprints))
          }
          shiny::tags$tr(
            shiny::tags$td(episodic_ui_format_stamp(row$exported_at, lang = lang)),
            shiny::tags$td(episodic_ui_import_export_export_who(row, data$users, lang = lang)),
            shiny::tags$td(
              status_chip(row$status),
              if (!is.na(row$error_text)) shiny::tags$div(class = "episodic-form-hint", row$error_text)
            ),
            shiny::tags$td(episodic_ui_import_export_range(row$date_from, row$date_to, lang = lang)),
            shiny::tags$td(count(row$n_cases)),
            shiny::tags$td(
              if (identical(row$mode, "passphrase")) {
                episodic_tr("import_export.mode.passphrase", lang = lang)
              } else {
                lapply(fingerprints, function(f) shiny::tags$div(shiny::tags$code(episodic_import_export_fingerprint_short(f))))
              }
            ),
            shiny::tags$td(shiny::tags$code(short(row$file_sha256))),
            shiny::tags$td(shiny::tags$code(if (is.na(row$destination)) "" else row$destination))
          )
        }))
      )
    },
    shiny::tags$h3(class = "episodic-settings-section-title", episodic_tr("import_export.audit.imports", lang = lang)),
    if (nrow(imports) == 0) {
      shiny::tags$p(class = "episodic-panel-empty", episodic_tr("import_export.audit.imports_empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("import_export.col.when", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.run", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.source", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.status", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.sender", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.range", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.cases", lang = lang)),
          shiny::tags$th(episodic_tr("import_export.col.file", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(imports)), function(i) {
          row <- imports[i, ]
          shiny::tags$tr(
            shiny::tags$td(episodic_ui_format_stamp(row$imported_at, lang = lang)),
            # A run number is an identifier, written as it is.
            shiny::tags$td(as.character(row$run_id)),
            shiny::tags$td(
              if (is.na(row$source_name)) episodic_tr("import_export.source.direct", lang = lang) else row$source_name
            ),
            shiny::tags$td(
              status_chip(row$status),
              if (!is.na(row$error_text)) shiny::tags$div(class = "episodic-form-hint", row$error_text)
            ),
            shiny::tags$td(
              if (!is.na(row$sender_instance_id)) shiny::tags$code(row$sender_instance_id),
              if (!is.na(row$signer_fingerprint)) {
                shiny::tags$div(shiny::tags$code(episodic_import_export_fingerprint_short(row$signer_fingerprint)))
              }
            ),
            shiny::tags$td(episodic_ui_import_export_range(row$date_from, row$date_to, lang = lang)),
            shiny::tags$td(count(row$n_cases)),
            shiny::tags$td(shiny::tags$code(short(row$file_sha256)))
          )
        }))
      )
    }
  )
}

#' The client-side script binding the Import and export screen's inputs
#'
#' The same approach as `episodic_ui_settings_bind_script()`, on a class
#' of its own so neither screen's script binds the other's fields. The
#' row actions share one input, carrying the action and the name; the
#' name travels as a data attribute, so a name with a quote in it is
#' text and never script.
#' @keywords internal
#' @noRd
episodic_ui_import_export_bind_script <- function() {
  "(function(){
    document.querySelectorAll('.episodic-import-export-input').forEach(function(el){
      var send = function(){
        Shiny.setInputValue(el.id, el.type === 'checkbox' ? el.checked : el.value);
      };
      el.addEventListener('input', send);
      el.addEventListener('change', send);
    });
    document.querySelectorAll('.episodic-import-export-action').forEach(function(el){
      el.addEventListener('click', function(){
        Shiny.setInputValue('impexp_action', {action: el.dataset.action, name: el.dataset.name, nonce: Math.random()}, {priority: 'event'});
      });
    });
  })();"
}

#' The public keys pasted into a recipients field, one per key line
#' @param text The field's text.
#' @return A character vector of key lines (possibly empty).
#' @keywords internal
#' @noRd
episodic_app_import_export_key_lines <- function(text) {
  if (is.null(text) || is.na(text)) {
    return(character(0))
  }
  lines <- trimws(unlist(strsplit(as.character(text), "\n", fixed = TRUE)))
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

#' Wire the Import and export screen's server-side logic
#'
#' Every handler re-resolves the signed-in account with
#' `episodic_auth_refresh_user()` and re-checks `episodic_user_is_admin()`
#' before it writes or exports, the same boundary the Settings screen
#' keeps: the card and the forms are what an admin sees, not what is
#' enforced.
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language.
#' @param current_user A `shiny::reactiveVal` holding the signed-in
#'   user's account row, or `NULL`.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episodic_app_server_import_export <- function(input,
                                              output,
                                              session,
                                              con,
                                              lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                              current_user) {
  notice <- shiny::reactiveVal(NULL)
  test_result <- shiny::reactiveVal(NULL)
  version <- shiny::reactiveVal(0)
  touch <- function() version(version() + 1)
  value <- function(id, default = NA_character_) {
    got <- input[[id]]
    if (is.null(got)) default else got
  }
  fields_ticked <- function(prefix, config) {
    default <- unlist(config$export$fields) %||% character(0)
    Filter(function(field) {
      ticked <- input[[paste0(prefix, field)]]
      if (is.null(ticked)) field %in% default else isTRUE(ticked)
    }, episodic_import_export_omittable_fields())
  }
  say <- function(type, key, ...) {
    notice(list(type = type, text = episodic_tr(key, ..., lang = lang)))
  }
  admin <- function() {
    user <- episodic_auth_refresh_user(con, current_user())
    if (episodic_user_is_admin(user)) user else NULL
  }

  output$import_export_screen <- shiny::renderUI({
    version()
    if (!episodic_user_is_admin(current_user())) {
      return(shiny::tags$p(episodic_tr("settings.no_access", lang = lang)))
    }
    episodic_ui_import_export_screen(
      episodic_app_import_export(con),
      episodic_config_resolve(con = con),
      lang = lang,
      message = notice(),
      test = test_result()
    )
  })

  output$impexp_download <- shiny::downloadHandler(
    # Neutral on purpose: a file kept in a public place must not say who
    # sent it or which period it covers, and its name is the first thing
    # anyone sees.
    filename = function() "episodic-export.episodic",
    contentType = "application/octet-stream",
    content = function(file) {
      user <- admin()
      if (is.null(user)) {
        stop("Only an admin account can export.", call. = FALSE)
      }
      config <- episodic_config_resolve(con = con)
      from <- value("impexp_dl_from", format(Sys.Date() - 27, "%Y-%m-%d"))
      to <- value("impexp_dl_to", format(Sys.Date(), "%Y-%m-%d"))
      mode <- value("impexp_dl_mode", "public_key")
      pc <- episodic_import_export_blank_na(value("impexp_dl_pc"))
      built <- NULL
      outcome <- tryCatch(
        {
          built <- episodic_import_export_build(
            con,
            from = from,
            to = to,
            recipients = if (identical(mode, "public_key")) episodic_app_import_export_key_lines(value("impexp_dl_recipients")) else NULL,
            passphrase = if (identical(mode, "passphrase")) value("impexp_dl_passphrase", "") else NULL,
            signing_key = Sys.getenv("EPISODIC_EXPORT_SIGNING_KEY", unset = NA),
            fields = fields_ticked("impexp_dl_field_", config),
            pc_characters = if (is.na(pc)) NULL else pc,
            config = config
          )
          writeBin(built$bytes, file)
          NULL
        },
        error = function(e) e
      )
      episodic_db_export_record(
        con,
        initiated_by = "download",
        status = if (is.null(outcome)) "written" else "failed",
        from = from,
        to = to,
        built = built,
        destination = "download",
        user_id = user$user_id,
        error_text = if (is.null(outcome)) NA_character_ else conditionMessage(outcome)
      )
      if (is.null(outcome)) {
        say("success", "import_export.download.done", n = episodic_format_number(built$n_cases, lang = lang))
      } else {
        say("error", "import_export.download.failed", error = conditionMessage(outcome))
      }
      touch()
      if (!is.null(outcome)) {
        stop(outcome)
      }
    }
  )

  shiny::observeEvent(input$impexp_schedule_save, {
    user <- admin()
    shiny::req(user)
    config <- episodic_config_resolve(con = con)
    result <- tryCatch(
      episodic_export_schedule_set(
        con,
        user_id = user$user_id,
        name = value("impexp_sc_name", ""),
        interval_days = value("impexp_sc_interval", "7"),
        window_days = value("impexp_sc_window", "28"),
        mode = value("impexp_sc_mode", "public_key"),
        recipients = episodic_app_import_export_key_lines(value("impexp_sc_recipients")),
        passphrase_env = value("impexp_sc_passphrase_env"),
        signing_key_env = value("impexp_sc_signing_env", "EPISODIC_EXPORT_SIGNING_KEY"),
        fields = fields_ticked("impexp_sc_field_", config),
        pc_characters = value("impexp_sc_pc"),
        destination_type = value("impexp_sc_loc_type", "file"),
        destination = value("impexp_sc_loc"),
        destination_env = value("impexp_sc_loc_env"),
        username_env = value("impexp_sc_user_env"),
        password_env = value("impexp_sc_pass_env"),
        token_env = value("impexp_sc_token_env"),
        config = config
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      say("error", "import_export.saved_failed", error = conditionMessage(result))
    } else {
      say("success", "import_export.schedules.saved", name = trimws(value("impexp_sc_name", "")))
    }
    touch()
  })

  shiny::observeEvent(input$impexp_source_save, {
    user <- admin()
    shiny::req(user)
    result <- tryCatch(
      episodic_import_source_set(
        con,
        user_id = user$user_id,
        name = value("impexp_so_name", ""),
        mode = value("impexp_so_mode", "public_key"),
        key_env = value("impexp_so_key_env", "EPISODIC_IMPORT_KEY"),
        passphrase_env = value("impexp_so_passphrase_env"),
        trusted_sender = value("impexp_so_sender", ""),
        location_type = value("impexp_so_loc_type", "file"),
        location = value("impexp_so_loc"),
        location_env = value("impexp_so_loc_env"),
        username_env = value("impexp_so_user_env"),
        password_env = value("impexp_so_pass_env"),
        token_env = value("impexp_so_token_env")
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      say("error", "import_export.saved_failed", error = conditionMessage(result))
    } else {
      say("success", "import_export.sources.saved", name = trimws(value("impexp_so_name", "")))
    }
    touch()
  })

  shiny::observeEvent(input$impexp_action, {
    user <- admin()
    shiny::req(user)
    action <- input$impexp_action$action
    name <- input$impexp_action$name
    shiny::req(is.character(action), is.character(name))
    kind <- if (startsWith(action, "schedule")) "schedule" else "source"
    if (action %in% c("schedule_test", "source_test")) {
      result <- tryCatch(
        episodic_import_export_test_named(con, name, kind, episodic_config_resolve(con = con)),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        test_result(NULL)
        say("error", "import_export.test.error", error = conditionMessage(result))
      } else {
        test_result(result)
        notice(NULL)
      }
    } else if (action %in% c("schedule_cancel", "source_cancel")) {
      result <- tryCatch(
        episodic_import_export_cancel(con, user$user_id, name, kind = kind),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        say("error", "import_export.saved_failed", error = conditionMessage(result))
      } else {
        say("success", "import_export.cancelled", name = name)
      }
    }
    touch()
  })

  invisible(NULL)
}
