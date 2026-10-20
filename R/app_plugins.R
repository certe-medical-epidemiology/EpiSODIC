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

# The Plugins screen and the plugin buttons on the dossiers (see
# vignette("plugins")).
#
# The screen is reached from its card on the Instance screen, and only by
# an is_admin account, like Settings: the card is omitted for everyone
# else, the screen renders nothing for them, and every handler re-resolves
# the signed-in account and re-checks is_admin before it changes a file.
# Installing writes into the plugin folder; enabling and disabling write
# the configuration file. Nothing here writes the database except the
# record of a plugin run.
#
# A plugin button appears on the outbreak or epidemic dossier for an
# epidemiologist, never for a viewer or a visitor who has not signed in,
# and the handler checks the role again. A plugin that receives a
# cluster's cases asks for confirmation first, naming where they go.

#' The Plugins screen's data
#' @param con A [DBI::DBIConnection-class].
#' @return A list with `config_path`, `dir`, `plugins`, `runs` and `users`
#'   (id to full name).
#' @keywords internal
#' @noRd
episodic_app_plugins <- function(con) {
  config_path <- Sys.getenv("EPISODIC_CONFIG", unset = NA)
  dir <- episodic_plugin_dir(config_path)
  users <- episodic_db_app_users(con)
  list(
    config_path = config_path,
    dir = dir,
    plugins = episodic_plugins(episodic_config_resolve(), dir),
    runs = episodic_db_plugin_runs(con, limit = 100L),
    users = stats::setNames(users$full_name, users$user_id)
  )
}

#' The most recent plugin runs, newest first
#' @keywords internal
#' @noRd
episodic_db_plugin_runs <- function(con, limit = 100L) {
  episodic_db_get_query(
    con,
    "SELECT r.*, c.scale AS cluster_scale
       FROM episodic_plugin_run_event r
       LEFT JOIN episodic_cluster c ON c.cluster_id = r.cluster_id
      ORDER BY r.started_at DESC, r.event_id DESC LIMIT ?",
    params = list(as.integer(limit))
  )
}

#' A row action on the Plugins screen
#' @keywords internal
#' @noRd
episodic_ui_plugins_action <- function(action, id, label, primary = FALSE) {
  shiny::tags$button(
    type = "button",
    class = paste("episodic-btn", if (primary) "episodic-btn-primary" else NULL, "episodic-plugins-action"),
    `data-action` = action,
    `data-plugin` = id,
    label
  )
}

#' Where a plugin acts, in words
#' @keywords internal
#' @noRd
episodic_plugin_trigger_text <- function(plugin, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (identical(plugin$trigger, "button") && !is.na(plugin$placement %||% NA)) {
    episodic_tr(paste0("plugins.trigger.button_", plugin$placement), lang = lang)
  } else if (identical(plugin$trigger, "run_committed")) {
    episodic_tr("plugins.trigger.run_committed", lang = lang)
  } else {
    episodic_tr("misc.unknown", lang = lang)
  }
}

#' What a plugin receives and reads, in words
#' @keywords internal
#' @noRd
episodic_ui_plugin_grants <- function(plugin, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  items <- list()
  for (permission in plugin$permissions %||% character(0)) {
    items[[length(items) + 1L]] <- shiny::tags$li(episodic_tr(paste0("plugins.permission.", permission), lang = lang))
  }
  if (length(plugin$secrets %||% character(0)) > 0) {
    items[[length(items) + 1L]] <- shiny::tags$li(
      episodic_tr("plugins.grants.secrets", variables = paste(plugin$secrets, collapse = ", "), lang = lang)
    )
  }
  if (!is.na(plugin$destination %||% NA)) {
    items[[length(items) + 1L]] <- shiny::tags$li(
      episodic_tr("plugins.grants.destination", destination = plugin$destination, lang = lang)
    )
  }
  if (length(items) == 0) {
    return(shiny::tags$span(class = "episodic-panel-empty", episodic_tr("plugins.grants.none", lang = lang)))
  }
  shiny::tags$ul(class = "episodic-plugins-grants", items)
}

#' The Plugins screen
#' @param data `episodic_app_plugins()`'s result.
#' @param lang Session language.
#' @param message A list with `type` and `text`, or `NULL`.
#' @return A `shiny::tags$div`.
#' @keywords internal
#' @noRd
episodic_ui_plugins_screen <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE"), message = NULL) {
  shiny::tags$div(
    class = "episodic-streams-screen",
    shiny::tags$h1(class = "episodic-screen-title", episodic_tr("nav.plugins", lang = lang)),
    shiny::tags$p(class = "episodic-screen-lead", episodic_tr("plugins.lead", lang = lang)),
    if (!is.null(message)) {
      shiny::tags$div(
        class = if (identical(message$type, "error")) "episodic-form-error" else "episodic-form-hint",
        role = if (identical(message$type, "error")) "alert" else "status",
        style = "margin-bottom:14px;font-size:12.5px;",
        message$text
      )
    },
    episodic_ui_plugins_folder_panel(data, lang = lang),
    episodic_ui_plugins_list_panel(data, lang = lang),
    if (!is.null(data$dir)) episodic_ui_plugins_install_panel(lang = lang),
    episodic_ui_plugins_runs_panel(data, lang = lang),
    shiny::tags$script(shiny::HTML(episodic_ui_plugins_bind_script()))
  )
}

#' @keywords internal
#' @noRd
episodic_ui_plugins_folder_panel <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  notices <- list()
  if (is.null(data$dir)) {
    notices <- c(notices, list(episodic_tr("plugins.folder.none", lang = lang)))
  }
  if (is.na(data$config_path) || !nzchar(data$config_path)) {
    notices <- c(notices, list(episodic_tr("plugins.config.no_file", lang = lang)))
  } else if (file.access(data$config_path, mode = 2) != 0) {
    notices <- c(notices, list(episodic_tr("plugins.config.not_writable", path = data$config_path, lang = lang)))
  }
  episodic_ui_panel(
    episodic_tr("plugins.folder.title", lang = lang),
    note = episodic_tr("plugins.folder.note", lang = lang),
    lapply(notices, function(text) {
      shiny::tags$div(class = "episodic-form-error", role = "alert", style = "margin-bottom:10px;", text)
    }),
    shiny::tags$div(
      class = "episodic-settings-kv",
      shiny::tags$span(class = "episodic-settings-kv-label", episodic_tr("plugins.folder.path", lang = lang)),
      shiny::tags$code(data$dir %||% episodic_tr("misc.none", lang = lang))
    ),
    shiny::tags$div(
      class = "episodic-settings-kv",
      shiny::tags$span(class = "episodic-settings-kv-label", episodic_tr("plugins.folder.config", lang = lang)),
      shiny::tags$code(if (is.na(data$config_path)) episodic_tr("misc.none", lang = lang) else data$config_path)
    )
  )
}

#' @keywords internal
#' @noRd
episodic_ui_plugins_list_panel <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  plugins <- data$plugins
  episodic_ui_panel(
    episodic_tr("plugins.list.title", lang = lang),
    if (length(plugins) == 0) {
      shiny::tags$p(class = "episodic-panel-empty", episodic_tr("plugins.list.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("plugins.col.plugin", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.trigger", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.status", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.grants", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.actions", lang = lang))
        )),
        shiny::tags$tbody(lapply(plugins, function(plugin) {
          episodic_ui_plugins_row(plugin, lang = lang)
        }))
      )
    }
  )
}

#' @keywords internal
#' @noRd
episodic_ui_plugins_row <- function(plugin, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  status <- plugin$status
  actions <- list()
  if (status %in% c("disabled", "needs_approval")) {
    actions <- c(actions, list(episodic_ui_plugins_action("enable", plugin$id, episodic_tr("plugins.action.enable", lang = lang), primary = TRUE)))
  }
  if (status %in% c("enabled", "needs_approval", "missing")) {
    actions <- c(actions, list(episodic_ui_plugins_action("disable", plugin$id, episodic_tr("plugins.action.disable", lang = lang))))
  }
  if (!is.na(plugin$source %||% NA)) {
    actions <- c(actions, list(episodic_ui_plugins_action("update", plugin$id, episodic_tr("plugins.action.update", lang = lang))))
  }
  if (!identical(status, "missing")) {
    actions <- c(actions, list(episodic_ui_plugins_action("uninstall", plugin$id, episodic_tr("plugins.action.uninstall", lang = lang))))
  }
  shiny::tags$tr(
    shiny::tags$td(
      shiny::tags$div(shiny::tags$strong(plugin$name %||% plugin$id)),
      shiny::tags$div(
        class = "episodic-form-hint",
        shiny::tags$code(plugin$id),
        if (!is.na(plugin$version %||% NA)) paste0(" \u00b7 ", plugin$version)
      )
    ),
    shiny::tags$td(episodic_plugin_trigger_text(plugin, lang = lang)),
    shiny::tags$td(
      shiny::tags$span(
        class = paste0("episodic-plugins-status episodic-plugins-status-", status),
        episodic_tr(paste0("plugins.status.", status), lang = lang)
      ),
      if (length(plugin$problems) > 0) {
        shiny::tags$ul(
          class = "episodic-plugins-problems",
          lapply(plugin$problems, function(p) shiny::tags$li(episodic_plugin_problem_text(p, lang = lang)))
        )
      },
      if (identical(status, "needs_approval")) {
        shiny::tags$div(
          class = "episodic-form-hint",
          episodic_tr("plugins.status.needs_approval_detail", items = paste(plugin$ungranted, collapse = ", "), lang = lang)
        )
      }
    ),
    shiny::tags$td(episodic_ui_plugin_grants(plugin, lang = lang)),
    shiny::tags$td(class = "episodic-plugins-actions", actions)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_plugins_install_panel <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  field <- function(id, label, hint = NULL, type = "text") {
    shiny::tags$div(
      class = "episodic-form-group",
      if (identical(type, "checkbox")) {
        shiny::tags$label(
          class = "episodic-form-checkbox-label",
          shiny::tags$input(type = "checkbox", id = id, class = "episodic-plugins-input"),
          label
        )
      } else {
        shiny::tagList(
          shiny::tags$label(class = "episodic-form-label", `for` = id, label),
          shiny::tags$input(type = "text", id = id, class = "episodic-plugins-input", autocomplete = "off", spellcheck = "false")
        )
      },
      if (!is.null(hint)) shiny::tags$div(class = "episodic-form-hint", hint)
    )
  }
  episodic_ui_panel(
    episodic_tr("plugins.install.title", lang = lang),
    note = episodic_tr("plugins.install.note", lang = lang),
    field("plugins_install_source", episodic_tr("plugins.install.source", lang = lang), episodic_tr("plugins.install.source_hint", lang = lang)),
    field("plugins_install_token_env", episodic_tr("plugins.install.token_env", lang = lang), episodic_tr("plugins.install.token_env_hint", lang = lang)),
    field("plugins_install_overwrite", episodic_tr("plugins.install.overwrite", lang = lang), type = "checkbox"),
    shiny::tags$button(
      type = "button",
      class = "episodic-btn episodic-btn-primary episodic-plugins-action",
      `data-action` = "install",
      `data-plugin` = "",
      episodic_tr("plugins.install.button", lang = lang)
    )
  )
}

#' Who ran a plugin, in words
#' @keywords internal
#' @noRd
episodic_ui_plugin_run_by <- function(row, users, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  switch(row$initiated_by,
    dashboard = {
      name <- if (is.na(row$user_id)) NA_character_ else unname(users[as.character(row$user_id)])
      if (is.na(name)) episodic_tr("misc.unknown", lang = lang) else name
    },
    cron = episodic_tr("plugins.by.cron", lang = lang),
    episodic_tr("plugins.by.headless", account = row$account, host = row$host, lang = lang)
  )
}

#' @keywords internal
#' @noRd
episodic_ui_plugins_runs_panel <- function(data, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  runs <- data$runs
  episodic_ui_panel(
    episodic_tr("plugins.runs.title", lang = lang),
    note = episodic_tr("plugins.runs.note", lang = lang),
    if (nrow(runs) == 0) {
      shiny::tags$p(class = "episodic-panel-empty", episodic_tr("plugins.runs.empty", lang = lang))
    } else {
      shiny::tags$table(
        class = "episodic-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(episodic_tr("plugins.col.when", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.plugin", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.by", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.subject", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.status", lang = lang)),
          shiny::tags$th(episodic_tr("plugins.col.outcome", lang = lang))
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(runs)), function(i) {
          row <- runs[i, ]
          subject <- if (!is.na(row$cluster_id) && !is.na(row$cluster_scale)) {
            episodic_object_ref(row$cluster_id, row$cluster_scale, lang = lang)
          } else if (!is.na(row$run_id)) {
            episodic_tr("plugins.subject.run", id = row$run_id, lang = lang)
          } else {
            episodic_tr("misc.dash", lang = lang)
          }
          outcome <- c(row$message, row$step, row$detail)
          outcome <- outcome[!is.na(outcome) & nzchar(outcome)]
          shiny::tags$tr(
            shiny::tags$td(episodic_ui_format_stamp(row$started_at, lang = lang)),
            shiny::tags$td(
              shiny::tags$code(row$plugin_id),
              if (!is.na(row$plugin_version)) paste0(" ", row$plugin_version)
            ),
            shiny::tags$td(episodic_ui_plugin_run_by(row, data$users, lang = lang)),
            shiny::tags$td(subject),
            shiny::tags$td(episodic_tr(paste0("plugins.run_status.", row$status), lang = lang)),
            shiny::tags$td(
              class = "episodic-plugins-outcome",
              if (length(outcome) > 0) paste(outcome, collapse = " \u00b7 "),
              if (!is.na(row$link)) {
                shiny::tags$div(shiny::tags$a(href = row$link, target = "_blank", rel = "noopener noreferrer", row$link))
              }
            )
          )
        }))
      )
    }
  )
}

#' The client-side script binding the Plugins screen's inputs and actions
#'
#' The same approach as `episodic_ui_import_export_bind_script()`, on a
#' class of its own. The plugin id travels as a data attribute, so it is
#' text and never script.
#' @keywords internal
#' @noRd
episodic_ui_plugins_bind_script <- function() {
  "(function(){
    document.querySelectorAll('.episodic-plugins-input').forEach(function(el){
      var send = function(){
        Shiny.setInputValue(el.id, el.type === 'checkbox' ? el.checked : el.value);
      };
      el.addEventListener('input', send);
      el.addEventListener('change', send);
    });
    document.querySelectorAll('.episodic-plugins-action').forEach(function(el){
      el.addEventListener('click', function(){
        Shiny.setInputValue('plugins_action', {action: el.dataset.action, plugin: el.dataset.plugin, nonce: Math.random()}, {priority: 'event'});
      });
    });
  })();"
}

#' The plugin buttons on a dossier
#'
#' A panel with one button per enabled plugin placed on this kind of
#' dossier, or nothing when there is none. The cluster travels with the
#' click, so a button can never act on a dossier other than the one it
#' was drawn on.
#'
#' @param plugins The runnable plugins for this placement.
#' @param cluster_id The dossier's cluster.
#' @param lang Session language.
#' @return A `shiny::tags$section`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_ui_plugin_buttons <- function(plugins, cluster_id, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  if (length(plugins) == 0 || is.null(cluster_id)) {
    return(NULL)
  }
  episodic_ui_panel(
    episodic_tr("plugins.panel.title", lang = lang),
    shiny::tags$div(
      class = "episodic-plugins-buttons",
      lapply(plugins, function(plugin) {
        shiny::tags$button(
          type = "button",
          class = "episodic-btn episodic-plugin-button",
          `data-plugin` = plugin$id,
          `data-cluster` = as.integer(cluster_id),
          episodic_plugin_label(plugin, lang = lang)
        )
      })
    ),
    shiny::tags$script(shiny::HTML(
      "(function(){
        document.querySelectorAll('.episodic-plugin-button').forEach(function(el){
          if (el.dataset.bound) return;
          el.dataset.bound = '1';
          el.addEventListener('click', function(){
            Shiny.setInputValue('plugin_button', {plugin: el.dataset.plugin, cluster: Number(el.dataset.cluster), nonce: Math.random()}, {priority: 'event'});
          });
        });
      })();"
    ))
  )
}

#' The confirmation before a plugin sends a cluster's cases out
#' @keywords internal
#' @noRd
episodic_ui_plugin_confirm_modal <- function(plugin, cluster_id, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::modalDialog(
    title = episodic_tr("plugins.confirm.title", name = episodic_plugin_label(plugin, lang = lang), lang = lang),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::tags$button(
        type = "button", class = "episodic-btn", `data-bs-dismiss` = "modal",
        episodic_tr("plugins.confirm.cancel", lang = lang)
      ),
      shiny::tags$button(
        type = "button",
        class = "episodic-btn episodic-btn-primary",
        `data-plugin` = plugin$id,
        `data-cluster` = as.integer(cluster_id),
        onclick = "Shiny.setInputValue('plugin_confirm', {plugin: this.dataset.plugin, cluster: Number(this.dataset.cluster), nonce: Math.random()}, {priority: 'event'})",
        episodic_tr("plugins.confirm.button", lang = lang)
      )
    ),
    shiny::tags$p(episodic_tr("plugins.confirm.cases", destination = plugin$destination, lang = lang)),
    episodic_ui_plugin_grants(plugin, lang = lang)
  )
}

#' The result of a plugin run, as the person who ran it sees it
#'
#' The plugin's message and link are shown as text and as a plain link:
#' nothing a plugin returns is rendered as markup. When it did not
#' succeed, the end of its error output is shown too, with the values of
#' its secrets masked, so whoever pushed the button can see what went
#' wrong.
#' @keywords internal
#' @noRd
episodic_ui_plugin_result_modal <- function(plugin, outcome, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  ok <- identical(outcome$status, "success")
  shiny::modalDialog(
    title = episodic_tr("plugins.result.title", name = episodic_plugin_label(plugin, lang = lang), lang = lang),
    easyClose = TRUE,
    footer = shiny::tags$button(
      type = "button", class = "episodic-btn", `data-bs-dismiss` = "modal",
      episodic_tr("misc.close", lang = lang)
    ),
    shiny::tags$p(
      class = if (ok) "episodic-form-hint" else "episodic-form-error",
      role = if (ok) "status" else "alert",
      episodic_tr(paste0("plugins.run_status.", outcome$status), lang = lang)
    ),
    if (!is.na(outcome$message)) shiny::tags$p(outcome$message),
    if (!is.na(outcome$link)) {
      shiny::tags$p(shiny::tags$a(
        href = outcome$link, target = "_blank", rel = "noopener noreferrer",
        episodic_tr("plugins.result.link", lang = lang)
      ))
    },
    if (!ok && !is.na(outcome$detail)) {
      shiny::tags$pre(class = "episodic-plugins-detail", outcome$detail)
    }
  )
}

#' Wire the Plugins screen and the dossier buttons
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language.
#' @param current_user A `shiny::reactiveVal` holding the signed-in
#'   user's account row, or `NULL`.
#' @param access_granted A reactive: whether this session may read
#'   surveillance data at all.
#' @param selected_cluster_id,selected_epidemic_id Reactives holding the
#'   outbreak and epidemic on screen.
#' @return Invisible `NULL`; called for its side effects.
#' @keywords internal
#' @noRd
episodic_app_server_plugins <- function(input,
                                        output,
                                        session,
                                        con,
                                        lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                        current_user,
                                        access_granted,
                                        selected_cluster_id,
                                        selected_epidemic_id) {
  notice <- shiny::reactiveVal(NULL)
  version <- shiny::reactiveVal(0)
  touch <- function() version(version() + 1)
  say <- function(type, key, ...) {
    notice(list(type = type, text = episodic_tr(key, ..., lang = lang)))
  }
  value <- function(id, default = "") {
    got <- input[[id]]
    if (is.null(got)) default else got
  }
  admin <- function() {
    user <- episodic_auth_refresh_user(con, current_user())
    if (episodic_user_is_admin(user)) user else NULL
  }
  config_path <- function() Sys.getenv("EPISODIC_CONFIG", unset = NA)
  current_plugins <- function() {
    episodic_plugins(episodic_config_resolve(), episodic_plugin_dir(config_path()))
  }

  output$plugins_screen <- shiny::renderUI({
    version()
    if (!episodic_user_is_admin(current_user())) {
      return(shiny::tags$p(episodic_tr("settings.no_access", lang = lang)))
    }
    data <- tryCatch(episodic_app_plugins(con), error = function(e) e)
    if (inherits(data, "error")) {
      return(shiny::tags$div(
        class = "episodic-form-error", role = "alert",
        episodic_tr("plugins.action.failed", error = conditionMessage(data), lang = lang)
      ))
    }
    episodic_ui_plugins_screen(data, lang = lang, message = notice())
  })

  shiny::observeEvent(input$plugins_action, {
    user <- admin()
    shiny::req(user)
    action <- input$plugins_action$action
    id <- input$plugins_action$plugin
    shiny::req(action %in% c("install", "enable", "confirm_enable", "disable", "update", "uninstall", "confirm_uninstall"))
    if (!identical(action, "install")) {
      shiny::req(episodic_plugin_id_valid(id))
    }
    result <- tryCatch(
      switch(action,
        install = {
          token_env <- trimws(value("plugins_install_token_env"))
          installed <- episodic_plugin_install(
            trimws(value("plugins_install_source")),
            overwrite = isTRUE(input$plugins_install_overwrite),
            token_env = if (nzchar(token_env)) token_env else NULL,
            episodic_config_path = config_path()
          )
          list(key = "plugins.install.done", name = installed)
        },
        enable = {
          plugin <- current_plugins()[[id]]
          shiny::req(plugin)
          shiny::showModal(episodic_ui_plugin_enable_modal(plugin, lang = lang))
          NULL
        },
        confirm_enable = {
          shiny::removeModal()
          episodic_plugin_enable(id, config_path())
          list(key = "plugins.enable.done", name = id)
        },
        disable = {
          episodic_plugin_disable(id, config_path())
          list(key = "plugins.disable.done", name = id)
        },
        update = {
          plugin <- current_plugins()[[id]]
          shiny::req(plugin, !is.na(plugin$source %||% NA))
          episodic_plugin_install(plugin$source, overwrite = TRUE, episodic_config_path = config_path())
          list(key = "plugins.update.done", name = id)
        },
        uninstall = {
          plugin <- current_plugins()[[id]]
          shiny::req(plugin)
          shiny::showModal(episodic_ui_plugin_uninstall_modal(plugin, lang = lang))
          NULL
        },
        confirm_uninstall = {
          shiny::removeModal()
          episodic_plugin_uninstall(id, config_path())
          list(key = "plugins.uninstall.done", name = id)
        }
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      say("error", "plugins.action.failed", error = conditionMessage(result))
    } else if (!is.null(result)) {
      say("success", result$key, name = result$name)
    }
    touch()
  })

  buttons_for <- function(placement, cluster_id) {
    user <- current_user()
    if (!isTRUE(access_granted()) || !episodic_user_is_epidemiologist(user) || is.null(cluster_id)) {
      return(NULL)
    }
    plugins <- tryCatch(
      episodic_plugins_runnable(current_plugins(), "button", placement),
      error = function(e) list()
    )
    episodic_ui_plugin_buttons(plugins, cluster_id, lang = lang)
  }
  output$plugin_buttons_outbreak <- shiny::renderUI({
    version()
    buttons_for("outbreak", selected_cluster_id())
  })
  output$plugin_buttons_epidemic <- shiny::renderUI({
    version()
    buttons_for("epidemic", selected_epidemic_id())
  })

  # Re-checked on the server: the button being on screen is what an
  # epidemiologist sees, not what is enforced.
  runnable_for <- function(request) {
    user <- episodic_auth_refresh_user(con, current_user())
    if (!isTRUE(access_granted()) || !episodic_user_is_epidemiologist(user)) {
      return(NULL)
    }
    id <- request$plugin
    cluster_id <- suppressWarnings(as.integer(request$cluster))
    if (!episodic_plugin_id_valid(id) || length(cluster_id) != 1 || is.na(cluster_id)) {
      return(NULL)
    }
    plugin <- current_plugins()[[id]]
    if (is.null(plugin) || !identical(plugin$status, "enabled") || !identical(plugin$trigger, "button")) {
      return(NULL)
    }
    shown <- if (identical(plugin$placement, "epidemic")) selected_epidemic_id() else selected_cluster_id()
    if (!identical(as.integer(shown), cluster_id)) {
      return(NULL)
    }
    list(user = user, plugin = plugin, cluster_id = cluster_id)
  }
  run_plugin <- function(target) {
    outcome <- tryCatch(
      episodic_plugin_invoke(
        con, target$plugin,
        list(cluster_id = target$cluster_id, user = list(username = target$user$username, role = target$user$role)),
        "dashboard", episodic_config_resolve(),
        lang = lang, user_id = target$user$user_id
      ),
      error = function(e) {
        list(
          status = "failed", message = NA_character_, link = NA_character_,
          step = NA_character_, detail = conditionMessage(e)
        )
      }
    )
    shiny::showModal(episodic_ui_plugin_result_modal(target$plugin, outcome, lang = lang))
    touch()
  }

  shiny::observeEvent(input$plugin_button, {
    target <- runnable_for(input$plugin_button)
    if (is.null(target)) {
      shiny::showModal(shiny::modalDialog(
        easyClose = TRUE,
        footer = shiny::tags$button(type = "button", class = "episodic-btn", `data-bs-dismiss` = "modal", episodic_tr("misc.close", lang = lang)),
        shiny::tags$p(class = "episodic-form-error", role = "alert", episodic_tr("plugins.run.refused", lang = lang))
      ))
      return()
    }
    if ("cluster_cases" %in% target$plugin$permissions) {
      shiny::showModal(episodic_ui_plugin_confirm_modal(target$plugin, target$cluster_id, lang = lang))
      return()
    }
    run_plugin(target)
  })

  shiny::observeEvent(input$plugin_confirm, {
    shiny::removeModal()
    target <- runnable_for(input$plugin_confirm)
    shiny::req(target)
    run_plugin(target)
  })

  invisible(NULL)
}

#' The confirmation before enabling a plugin, naming what it is granted
#' @keywords internal
#' @noRd
episodic_ui_plugin_enable_modal <- function(plugin, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::modalDialog(
    title = episodic_tr("plugins.enable.title", name = plugin$name %||% plugin$id, lang = lang),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::tags$button(type = "button", class = "episodic-btn", `data-bs-dismiss` = "modal", episodic_tr("plugins.confirm.cancel", lang = lang)),
      episodic_ui_plugins_modal_action("confirm_enable", plugin$id, episodic_tr("plugins.enable.confirm", lang = lang))
    ),
    shiny::tags$p(episodic_tr("plugins.enable.intro", lang = lang)),
    episodic_ui_plugin_grants(plugin, lang = lang),
    shiny::tags$p(
      class = "episodic-form-hint",
      episodic_tr("plugins.enable.steps", n = episodic_format_number(length(plugin$steps), lang = lang), lang = lang)
    )
  )
}

#' The confirmation before uninstalling a plugin
#' @keywords internal
#' @noRd
episodic_ui_plugin_uninstall_modal <- function(plugin, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::modalDialog(
    title = episodic_tr("plugins.uninstall.title", name = plugin$name %||% plugin$id, lang = lang),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::tags$button(type = "button", class = "episodic-btn", `data-bs-dismiss` = "modal", episodic_tr("plugins.confirm.cancel", lang = lang)),
      episodic_ui_plugins_modal_action("confirm_uninstall", plugin$id, episodic_tr("plugins.uninstall.confirm", lang = lang))
    ),
    shiny::tags$p(episodic_tr("plugins.uninstall.intro", lang = lang))
  )
}

#' A confirming button in a Plugins screen dialog
#'
#' Bound inline: a dialog is drawn outside the screen, after the screen's
#' own binding script has run.
#' @keywords internal
#' @noRd
episodic_ui_plugins_modal_action <- function(action, id, label) {
  shiny::tags$button(
    type = "button",
    class = "episodic-btn episodic-btn-primary",
    `data-action` = action,
    `data-plugin` = id,
    onclick = "Shiny.setInputValue('plugins_action', {action: this.dataset.action, plugin: this.dataset.plugin, nonce: Math.random()}, {priority: 'event'})",
    label
  )
}
