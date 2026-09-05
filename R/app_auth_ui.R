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

# Authentication UI: the app opens read-only (aggregate data only) for
# anyone who reaches it; signing in - as either role - unlocks patient-
# level detail, and only the "epidemiologist" role can additionally classify. A
# small header control and two modals (sign in; forced first-login
# password change) are the entire authentication surface here - no role
# shown in this control either (an is_admin account manages accounts and
# roles from the separate Settings screen instead, see
# R/app_settings.R/episodic_ui_settings_users_panel()).

#' The header sign-in/sign-out control
#'
#' @param current_user The session's signed-in user row, or `NULL`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_auth_control <- function(
  current_user,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (is.null(current_user)) {
    return(shiny::tags$a(
      class = "episodic-auth-link",
      href = "#",
      onclick = "Shiny.setInputValue('auth_show_login', Math.random(), {priority: 'event'}); return false;",
      episodic_tr("auth.signin", lang = lang)
    ))
  }
  shiny::tags$span(
    style = "display:flex;align-items:center;gap:10px;",
    shiny::tags$span(
      class = "episodic-auth-link",
      style = "cursor:default;",
      episodic_tr(
        "auth.signed_in_as",
        name = current_user$full_name,
        lang = lang
      )
    ),
    shiny::tags$a(
      class = "episodic-auth-link",
      href = "#",
      onclick = "Shiny.setInputValue('auth_signout', Math.random(), {priority: 'event'}); return false;",
      episodic_tr("auth.signout", lang = lang)
    )
  )
}

#' The sign-in modal
#'
#' @param error If `TRUE`, shows the generic credentials-rejected message.
#' @param dismissible Whether the reader may close the modal without
#'   signing in. `FALSE` on an instance with `access.require_login` set,
#'   where there is nothing behind the modal to go back to. It is a
#'   courtesy, not a control: the screen behind it is empty because the
#'   server rendered nothing, not because the modal is covering it.
#' @param lang Session language.
#' @return A `shiny::modalDialog`.
#' @keywords internal
#' @noRd
episodic_ui_login_modal <- function(
  error = FALSE,
  dismissible = TRUE,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  shiny::modalDialog(
    title = episodic_tr("auth.signin", lang = lang),
    easyClose = isTRUE(dismissible),
    footer = shiny::tags$div(
      class = "episodic-form-actions",
      if (isTRUE(dismissible)) {
        shiny::tags$button(
          class = "episodic-btn",
          type = "button",
          # Bootstrap 5 (episodic_app_ui()'s bslib::bs_theme(version = 5))
          # reads `data-bs-dismiss`, not Bootstrap 4's unprefixed
          # `data-dismiss`.
          `data-bs-dismiss` = "modal",
          onclick = "Shiny.setInputValue('auth_cancel_login', Math.random(), {priority:'event'})",
          episodic_tr("misc.close", lang = lang)
        )
      },
      shiny::tags$button(
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('auth_login_submit', Math.random(), {priority: 'event'})",
        episodic_tr("auth.submit", lang = lang)
      )
    ),
    if (!isTRUE(dismissible)) {
      shiny::tags$p(
        class = "episodic-panel-note",
        episodic_tr("auth.required_note", lang = lang)
      )
    },
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("auth.username", lang = lang)
      ),
      shiny::tags$input(
        type = "text",
        id = "auth_username",
        autocomplete = "username"
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("auth.password", lang = lang)
      ),
      shiny::tags$input(
        type = "password",
        id = "auth_password",
        autocomplete = "current-password",
        onkeydown = "if(event.key==='Enter'){Shiny.setInputValue('auth_login_submit', Math.random(), {priority:'event'});}"
      )
    ),
    if (isTRUE(error)) {
      shiny::tags$div(
        class = "episodic-form-error",
        episodic_tr("auth.error", lang = lang)
      )
    },
    shiny::tags$script(shiny::HTML(
      "$('#auth_username').on('input', function(){ Shiny.setInputValue('auth_username_val', this.value); });
       $('#auth_password').on('input', function(){ Shiny.setInputValue('auth_password_val', this.value); });
       $(document).one('shown.bs.modal', function(){ $('#auth_username').trigger('focus'); });"
    ))
  )
}

#' The forced first-login password-change modal
#'
#' @param error A translated error message to show, or `NULL`.
#' @param lang Session language.
#' @return A `shiny::modalDialog`.
#' @keywords internal
#' @noRd
episodic_ui_must_change_modal <- function(
  error = NULL,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  shiny::modalDialog(
    title = episodic_tr("auth.must_change_title", lang = lang),
    easyClose = FALSE,
    footer = shiny::tags$div(
      class = "episodic-form-actions",
      shiny::tags$button(
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('auth_change_password_submit', Math.random(), {priority: 'event'})",
        episodic_tr("auth.change_password", lang = lang)
      )
    ),
    shiny::tags$p(episodic_tr("auth.must_change_note", lang = lang)),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("auth.new_password", lang = lang)
      ),
      shiny::tags$input(
        type = "password",
        id = "auth_new_password",
        autocomplete = "new-password"
      )
    ),
    shiny::tags$div(
      class = "episodic-form-group",
      shiny::tags$label(
        class = "episodic-form-label",
        episodic_tr("auth.confirm_password", lang = lang)
      ),
      shiny::tags$input(
        type = "password",
        id = "auth_confirm_password",
        autocomplete = "new-password"
      )
    ),
    if (!is.null(error)) shiny::tags$div(class = "episodic-form-error", error),
    shiny::tags$script(shiny::HTML(
      "$('#auth_new_password').on('input', function(){ Shiny.setInputValue('auth_new_password_val', this.value); });
       $('#auth_confirm_password').on('input', function(){ Shiny.setInputValue('auth_confirm_password_val', this.value); });"
    ))
  )
}

#' What an anonymous visitor sees when the instance requires a sign-in
#'
#' The whole of the main view, for a session that has not signed in on an
#' instance with `access.require_login` set. It carries no surveillance
#' data of any kind, because on such a session the server never computes
#' any: this is not the data screen with something drawn over it.
#'
#' The sign-in modal is shown over this on connect. The panel exists
#' underneath it anyway, for the reader who dismisses the modal with the
#' browser's own controls or whose JavaScript never ran - they get an
#' explanation and a way back to the prompt, rather than a blank page
#' that reads as an outage.
#'
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episodic_ui_locked_screen <- function(lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  shiny::tags$div(
    class = "episodic-locked-screen",
    shiny::tags$div(
      class = "episodic-locked-card",
      shiny::tags$span(class = "episodic-lock-icon", "\U0001F512"),
      shiny::tags$h1(
        class = "episodic-locked-title",
        episodic_tr("auth.required_title", lang = lang)
      ),
      shiny::tags$p(
        class = "episodic-locked-body",
        episodic_tr("auth.required_body", lang = lang)
      ),
      shiny::tags$button(
        class = "episodic-btn episodic-btn-primary",
        onclick = "Shiny.setInputValue('auth_show_login', Math.random(), {priority: 'event'});",
        episodic_tr("auth.signin", lang = lang)
      )
    )
  )
}
