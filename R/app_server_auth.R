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

#' Authentication server logic
#'
#' Wires the login modal, the forced first-login password change, and
#' sign-out. Returns the session's `current_user` reactive so the rest of
#' the server can read it (`NULL` for an anonymous viewer).
#'
#' @param input,output,session The Shiny server function's own arguments.
#' @param con A [DBI::DBIConnection-class].
#' @param lang Session language.
#' @return A `shiny::reactiveVal` holding the signed-in user's account row,
#'   or `NULL`.
#' @keywords internal
#' @noRd
episodic_app_server_auth <- function(
  input,
  output,
  session,
  con,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  current_user <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$auth_show_login, {
    shiny::showModal(episodic_ui_login_modal(lang = lang))
  })

  shiny::observeEvent(input$auth_cancel_login, shiny::removeModal())

  shiny::observeEvent(input$auth_login_submit, {
    username <- input$auth_username_val %||% ""
    password <- input$auth_password_val %||% ""
    result <- episodic_auth_login(con, username, password)
    if (!isTRUE(result$ok)) {
      shiny::showModal(episodic_ui_login_modal(error = TRUE, lang = lang))
      return(invisible(NULL))
    }
    current_user(result$user)
    if (isTRUE(result$must_change)) {
      shiny::showModal(episodic_ui_must_change_modal(lang = lang))
    } else {
      shiny::removeModal()
    }
  })

  shiny::observeEvent(input$auth_signout, current_user(NULL))

  shiny::observeEvent(input$auth_change_password_submit, {
    new_pw <- input$auth_new_password_val %||% ""
    confirm_pw <- input$auth_confirm_password_val %||% ""
    user <- current_user()
    shiny::req(user)

    if (nchar(new_pw) < 8) {
      shiny::showModal(episodic_ui_must_change_modal(
        error = episodic_tr("auth.password_too_short", lang = lang),
        lang = lang
      ))
      return(invisible(NULL))
    }
    if (!identical(new_pw, confirm_pw)) {
      shiny::showModal(episodic_ui_must_change_modal(
        error = episodic_tr("auth.password_mismatch", lang = lang),
        lang = lang
      ))
      return(invisible(NULL))
    }

    episodic_auth_change_password(con, user$user_id, new_pw)
    shiny::removeModal()
  })

  current_user
}
