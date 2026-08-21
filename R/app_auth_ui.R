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

# Authentication UI: the app opens read-only for anyone who reaches it,
# login only to classify. A small header control and two modals (sign in;
# forced first-login password change) are the entire authentication
# surface - no separate screen, no account management UI (four accounts,
# provisioned outside the app).

#' The header sign-in/sign-out control
#'
#' @param current_user The session's signed-in user row, or `NULL`.
#' @param lang Session language.
#' @return A `shiny::tags` element.
#' @keywords internal
#' @noRd
episode_ui_auth_control <- function(current_user, lang = "nl") {
  if (is.null(current_user)) {
    return(shiny::tags$a(
      class = "episode-auth-link", href = "#",
      onclick = "Shiny.setInputValue('auth_show_login', Math.random(), {priority: 'event'}); return false;",
      episode_tr("auth.signin", lang = lang)
    ))
  }
  shiny::tags$span(
    style = "display:flex;align-items:center;gap:10px;",
    shiny::tags$span(class = "episode-auth-link", style = "cursor:default;",
                      episode_tr("auth.signed_in_as", name = current_user$full_name, lang = lang)),
    shiny::tags$a(
      class = "episode-auth-link", href = "#",
      onclick = "Shiny.setInputValue('auth_signout', Math.random(), {priority: 'event'}); return false;",
      episode_tr("auth.signout", lang = lang)
    )
  )
}

#' The sign-in modal
#'
#' @param error If `TRUE`, shows the generic credentials-rejected message.
#' @param lang Session language.
#' @return A `shiny::modalDialog`.
#' @keywords internal
#' @noRd
episode_ui_login_modal <- function(error = FALSE, lang = "nl") {
  shiny::modalDialog(
    title = episode_tr("auth.signin", lang = lang),
    easyClose = TRUE,
    footer = shiny::tags$div(
      class = "episode-form-actions",
      shiny::tags$button(class = "episode-btn", `data-dismiss` = "modal",
                          onclick = "Shiny.setInputValue('auth_cancel_login', Math.random(), {priority:'event'})",
                          episode_tr("misc.dash", lang = lang)),
      shiny::tags$button(
        class = "episode-btn episode-btn-primary",
        onclick = "Shiny.setInputValue('auth_login_submit', Math.random(), {priority: 'event'})",
        episode_tr("auth.submit", lang = lang)
      )
    ),
    shiny::tags$div(
      class = "episode-form-group",
      shiny::tags$label(class = "episode-form-label", episode_tr("auth.username", lang = lang)),
      shiny::tags$input(type = "text", id = "auth_username", autocomplete = "username")
    ),
    shiny::tags$div(
      class = "episode-form-group",
      shiny::tags$label(class = "episode-form-label", episode_tr("auth.password", lang = lang)),
      shiny::tags$input(type = "password", id = "auth_password", autocomplete = "current-password",
                         onkeydown = "if(event.key==='Enter'){Shiny.setInputValue('auth_login_submit', Math.random(), {priority:'event'});}")
    ),
    if (isTRUE(error)) shiny::tags$div(class = "episode-form-error", episode_tr("auth.error", lang = lang)),
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
episode_ui_must_change_modal <- function(error = NULL, lang = "nl") {
  shiny::modalDialog(
    title = episode_tr("auth.must_change_title", lang = lang),
    easyClose = FALSE,
    footer = shiny::tags$div(
      class = "episode-form-actions",
      shiny::tags$button(
        class = "episode-btn episode-btn-primary",
        onclick = "Shiny.setInputValue('auth_change_password_submit', Math.random(), {priority: 'event'})",
        episode_tr("auth.change_password", lang = lang)
      )
    ),
    shiny::tags$p(episode_tr("auth.must_change_note", lang = lang)),
    shiny::tags$div(
      class = "episode-form-group",
      shiny::tags$label(class = "episode-form-label", episode_tr("auth.new_password", lang = lang)),
      shiny::tags$input(type = "password", id = "auth_new_password", autocomplete = "new-password")
    ),
    shiny::tags$div(
      class = "episode-form-group",
      shiny::tags$label(class = "episode-form-label", episode_tr("auth.confirm_password", lang = lang)),
      shiny::tags$input(type = "password", id = "auth_confirm_password", autocomplete = "new-password")
    ),
    if (!is.null(error)) shiny::tags$div(class = "episode-form-error", error),
    shiny::tags$script(shiny::HTML(
      "$('#auth_new_password').on('input', function(){ Shiny.setInputValue('auth_new_password_val', this.value); });
       $('#auth_confirm_password').on('input', function(){ Shiny.setInputValue('auth_confirm_password_val', this.value); });"
    ))
  )
}
