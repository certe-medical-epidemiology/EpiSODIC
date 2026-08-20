#' Run the full synthetic demo in one call
#'
#' MILESTONES.md M7's own done-when bar: "a stranger clones the
#' repository and runs the whole system in under a minute." This is
#' everything a first-time reader would otherwise piece together by hand
#' from the README (`Sys.setenv()`, `episode_run_cron()` against the
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
#' @export
episode_demo <- function(db_path = tempfile(fileext = ".sqlite"),
                          username = "demo", full_name = "Demo User",
                          email = "demo@example.org", password = "episode-demo",
                          launch = TRUE, lang = "nl",
                          ingest_source_fn = episode_ingest_source_synthetic,
                          denominator_source_fn = episode_denominator_source_synthetic) {
  Sys.setenv(
    EPISODE_CONFIG = system.file("config", "default.yaml", package = "EpiSODE"),
    EPISODE_DB = db_path,
    EPISODE_GEO_DATA = system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODE")
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
  message(sprintf("EpiSODE demo account - username: %s, password: %s", username, password))

  if (isTRUE(launch)) {
    episode_run_app(db_path = db_path, lang = lang)
  }

  invisible(db_path)
}
