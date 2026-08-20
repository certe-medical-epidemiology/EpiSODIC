#' Run the EpiSODIC Shiny app
#'
#' Read-only for anonymous visitors (ARCHITECTURE.md section 12: signing in
#' is only required to classify a cluster). Serves the cluster dossier and
#' the Streams overview against a database already populated by
#' [episode_run_cron()].
#'
#' @param db_path Path to the SQLite database. Defaults to the
#'   `EPISODE_DB` environment variable.
#' @param lang Default session language, `"nl"` (default) or `"en"`.
#' @param ... Passed on to [shiny::runApp()] (e.g. `port`, `host`).
#' @return Invisible; called for its side effect of starting the app.
#' @examples
#' \dontrun{
#' # opens a blocking, interactive Shiny session - see episode_demo() for a
#' # one-call, populated version of this same example
#' episode_run_app("/path/to/episode.sqlite")
#' }
#' @export
episode_run_app <- function(db_path = Sys.getenv("EPISODE_DB", unset = NA), lang = "nl", ...) {
  rlang::check_installed(c("shiny", "bslib"))
  if (is.na(db_path) || !nzchar(db_path)) {
    stop(
      "No database path given and EPISODE_DB is not set. Pass db_path ",
      "explicitly, or set the EPISODE_DB environment variable.",
      call. = FALSE
    )
  }
  if (!file.exists(db_path)) {
    stop("No database file found at '", db_path, "'.", call. = FALSE)
  }
  shiny::addResourcePath("www", system.file("app", "www", package = "EpiSODIC"))
  app <- shiny::shinyApp(
    ui = episode_app_ui(lang = lang),
    server = episode_app_server_factory(db_path, lang = lang)
  )
  shiny::runApp(app, ...)
}
