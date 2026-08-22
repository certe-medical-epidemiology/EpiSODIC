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

#' Try EpiSODIC with synthetic outbreak data
#'
#' The fastest way to see what EpiSODIC does: this single call creates a
#' fresh database, generates several years of synthetic laboratory data,
#' runs detection over it, creates a demo assessor account, and opens the
#' dashboard - all without needing access to any real laboratory system or
#' an instance configuration file. Everything used here is a shipped
#' default, so it works right after installing the package.
#'
#' @param db_path Path to the SQLite database to create. Defaults to a
#'   temporary file, so repeated calls never collide and nothing is left
#'   behind once the R session ends.
#' @param username,full_name,email,password Credentials for the demo
#'   assessor account this creates, so you can sign in and classify a
#'   cluster right away. These are placeholder values - change them for
#'   anything beyond a local demo.
#' @param launch If `TRUE` (default), opens the dashboard afterwards (see
#'   [episodic_run_app()]); this call blocks until you close it. Set to
#'   `FALSE` to only build the demo database and return its path, e.g. for
#'   scripting or screenshots.
#' @param lang Dashboard language when `launch = TRUE`: `"nl"`, `"en"`,
#'   `"es"`, `"fr"`, `"de"`, `"zh"`, `"hi"`, or `"ar"`. Defaults to the
#'   `EPISODIC_LANGUAGE` environment variable, falling back to `"en"` if
#'   that is unset.
#' @param cases,denominators The data to generate the demo
#'   from - normally data frames (or tibbles), passed on unchanged to
#'   [episodic_run_cron()]. Default to several years of synthetic data;
#'   generate a narrower date range yourself (see
#'   [episodic_synthetic_cases()]) and pass it here for a quicker
#'   demo.
#' @return Invisibly, `db_path`.
#' @examples
#' \dontrun{
#' # launches a blocking, interactive Shiny session against several years
#' # of freshly-generated synthetic data
#' episodic_demo()
#' }
#'
#' \donttest{
#' # non-interactive: populate a database and stop there, e.g. for scripting
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' db_path <- episodic_demo(launch = FALSE, cases = cases, denominators = NULL)
#' file.remove(db_path)
#' }
#' @export
episodic_demo <- function(
  db_path = tempfile(fileext = ".sqlite"),
  username = "demo",
  full_name = "Demo User",
  email = "demo@example.org",
  password = "episodic-demo",
  launch = TRUE,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  cases = episodic_synthetic_cases,
  denominators = episodic_synthetic_denominators
) {
  Sys.setenv(
    EPISODIC_CONFIG = system.file(
      "config",
      "default.yaml",
      package = "EpiSODIC"
    ),
    EPISODIC_DB = db_path,
    EPISODIC_GEO_DATA = system.file(
      "extdata",
      "geo_postcodes4_nl.rds",
      package = "EpiSODIC"
    )
  )

  episodic_run_cron(
    db_path,
    cases = cases,
    denominators = denominators
  )

  episodic_provision_user(
    db_path = db_path,
    username = username,
    full_name = full_name,
    email = email,
    password = password
  )
  message(sprintf(
    "EpiSODIC demo account - username: %s, password: %s",
    username,
    password
  ))

  if (isTRUE(launch)) {
    episodic_run_app(db_path = db_path, lang = lang)
  }

  invisible(db_path)
}
