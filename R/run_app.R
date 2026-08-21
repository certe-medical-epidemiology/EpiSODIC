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

#' Open the EpiSODIC dashboard
#'
#' Launches the Shiny dashboard against a database already populated by
#' [episodic_run_cron()]: an overview of monitored surveillance streams and
#' each cluster's dossier, with charts, its narrative interpretation, and
#' an assessment form. Anyone can browse the dashboard; signing in is only
#' required to record an assessment or classify a cluster.
#'
#' If you just want to explore EpiSODIC without setting anything up first,
#' use [episodic_demo()] instead - it populates a database with synthetic
#' data and calls this function for you.
#'
#' @param db_path Path to the EpiSODIC database: a SQLite file, or a
#'   MariaDB/MySQL DSN (see [episodic_db_dsn_mariadb()]). Defaults to the
#'   `EPISODIC_DB` environment variable.
#' @param lang Dashboard language, fixed for the whole running app - there
#'   is no in-app language switcher. One of `"nl"` (default), `"en"`,
#'   `"es"`, `"fr"`, `"de"`, `"zh"`, `"hi"`, or `"ar"`.
#' @param ... Passed on to [shiny::runApp()], e.g. `port` or `host`.
#' @return Invisible; called for its side effect of starting the app. This
#'   call blocks until the app is stopped.
#' @examples
#' \dontrun{
#' # opens a blocking, interactive dashboard session; see episodic_demo()
#' # for a one-call version that also creates a populated demo database
#' episodic_run_app("/path/to/episodic.sqlite")
#' }
#' @export
episodic_run_app <- function(db_path = Sys.getenv("EPISODIC_DB", unset = NA), lang = "nl", ...) {
  rlang::check_installed(c("shiny", "bslib"))
  if (is.na(db_path) || !nzchar(db_path)) {
    stop(
      "No database path given and EPISODIC_DB is not set. Pass db_path ",
      "explicitly, or set the EPISODIC_DB environment variable.",
      call. = FALSE
    )
  }
  if (episodic_db_dialect(db_path) == "sqlite" && !file.exists(db_path)) {
    stop("No database file found at '", db_path, "'.", call. = FALSE)
  }
  shiny::addResourcePath("www", system.file("app", "www", package = "EpiSODIC"))
  app <- shiny::shinyApp(
    ui = episodic_app_ui(lang = lang),
    server = episodic_app_server_factory(db_path, lang = lang)
  )
  shiny::runApp(app, ...)
}
