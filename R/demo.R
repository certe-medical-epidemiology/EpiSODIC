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

#' Run the full synthetic demo in one call
#'
#' A stranger should be able to clone the repository and run the whole
#' system in under a minute. This is everything a first-time reader
#' would otherwise piece together by hand from the README (`Sys.setenv()`, `episode_run_cron()` against the
#' bundled synthetic generator, `episode_provision_user()`) wrapped into
#' one call - a fresh SQLite database, a completed detection run, one
#' assessor account ready to sign in with, and (unless `launch = FALSE`)
#' the app itself. No Diver access, no real credentials, no instance
#' configuration file required; every setting used here is a shipped
#' default.
#'
#' @param db_path Path to the SQLite database to create. Defaults to a
#'   session temp file, so repeated calls never collide with each other
#'   and nothing is left behind once the R session ends.
#' @param username,full_name,email,password Credentials for the demo
#'   assessor account this provisions, so signing in and classifying a
#'   cluster works immediately after `launch`. These are placeholder
#'   values, not real credentials - change them for anything beyond a
#'   local demo.
#' @param launch If `TRUE` (default), calls [episode_run_app()]
#'   afterwards (blocking, exactly as calling it directly would). Set to
#'   `FALSE` to only set up the database and return its path - useful
#'   for scripting, screenshots, or tests that need a populated demo
#'   database without an interactive session.
#' @param lang Passed to [episode_run_app()] when `launch = TRUE`.
#' @param ingest_source_fn,denominator_source_fn Passed straight through
#'   to [episode_run_cron()]. Default to the bundled synthetic generator
#'   over its own full default window (several years); exposed mainly so
#'   a smaller/faster window can be substituted - this package's own
#'   tests do exactly that, since a real demo's representative multi-year
#'   window is far more than a test needs to confirm the plumbing works.
#' @return Invisibly, `db_path`.
#' @examples
#' \dontrun{
#' # launches a blocking, interactive Shiny session against several years
#' # of freshly-generated synthetic data
#' episode_demo()
#' }
#'
#' \donttest{
#' # non-interactive: populate a database and stop there, e.g. for scripting
#' db_path <- episode_demo(
#'   launch = FALSE,
#'   ingest_source_fn = function() episode_ingest_source_synthetic(
#'     start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#'   ),
#'   denominator_source_fn = NULL
#' )
#' file.remove(db_path)
#' }
#' @export
episode_demo <- function(db_path = tempfile(fileext = ".sqlite"),
                          username = "demo", full_name = "Demo User",
                          email = "demo@example.org", password = "episode-demo",
                          launch = TRUE, lang = "nl",
                          ingest_source_fn = episode_ingest_source_synthetic,
                          denominator_source_fn = episode_denominator_source_synthetic) {
  Sys.setenv(
    EPISODIC_CONFIG = system.file("config", "default.yaml", package = "EpiSODIC"),
    EPISODIC_DB = db_path,
    EPISODIC_GEO_DATA = system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODIC")
  )

  episode_run_cron(
    db_path,
    ingest_source_fn = ingest_source_fn,
    denominator_source_fn = denominator_source_fn
  )

  episode_provision_user(
    db_path = db_path, username = username, full_name = full_name,
    email = email, password = password
  )
  message(sprintf("EpiSODIC demo account - username: %s, password: %s", username, password))

  if (isTRUE(launch)) {
    episode_run_app(db_path = db_path, lang = lang)
  }

  invisible(db_path)
}
