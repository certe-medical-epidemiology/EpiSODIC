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

#' Try EpiSODIC With Synthetic Outbreak Data
#'
#' The fastest way to see what EpiSODIC does: this single call creates a
#' fresh database, generates several years of synthetic laboratory data,
#' runs detection over it, creates a demo epidemiologist account, and opens the
#' dashboard - all without needing access to any real laboratory system or
#' an instance configuration file. Everything used here is a shipped
#' default, so it works right after installing the package.
#'
#' @param db_path Path to the SQLite database to create. Defaults to a
#'   temporary file, so repeated calls never collide and nothing is left
#'   behind once the R session ends.
#' @param username,full_name,email,password Credentials for the demo
#'   epidemiologist account this creates, so you can sign in and classify a
#'   cluster right away. These are placeholder values - change them for
#'   anything beyond a local demo.
#' @param launch If `TRUE` (default), opens the dashboard afterwards (see
#'   [episodic_run_app()]); this call blocks until you close it. Set to
#'   `FALSE` to only build the demo database and return its path, e.g. for
#'   scripting or screenshots.
#' @param run_date The date to run detection as of. Defaults to the end of
#'   the last complete week, so the week the statistical detectors test is
#'   a full one however far into the week you happen to run the demo -
#'   which is how surveillance reads its own weeks anyway.
#' @param lang Dashboard language when `launch = TRUE`: `"en"`, `"ar"`,
#'   `"nl"`, `"fr"`, `"de"`, `"hi"`, `"zh"`, or `"es"`. Defaults to the
#'   `EPISODIC_LANGUAGE` environment variable, falling back to `"en"` if
#'   that is unset.
#' @param cases,denominators The data to generate the demo
#'   from - normally data frames (or tibbles), passed on unchanged to
#'   [episodic_run_cron()]. Default to several years of synthetic data;
#'   generate a narrower date range yourself (see
#'   [episodic_synthetic_cases()]) and pass it here for a quicker
#'   demo. Trying the demo with your own extract is a good way to see
#'   EpiSODIC work end to end: `cases` is checked against the
#'   [episodic_case_data] requirements first, so data it cannot use stops here
#'   with an explanation of what to fix, rather than opening a dashboard
#'   with nothing in it. Run [episodic_check_cases()] on your extract
#'   yourself to see the same findings, plus the advisory ones.
#' @param ... Arguments passed on to [episodic_run_app()].
#' @return Invisibly, `db_path`.
#' @section What the demo writes beside the database:
#' EpiSODIC has no built-in geography - no default map, and no built-in
#' rule for turning a postcode into a province - because a default for
#' either would be a default for one country. The demo therefore
#' configures its own, exactly the way a real deployment does: two files
#' named after `db_path` (`<name>-config.yaml` and
#' `<name>-pc-province.csv`), plus the Netherlands postcode geometry
#' bundled with the package.
#'
#' They are left in place, so a database built with `launch = FALSE` can
#' be re-opened later with the same geography by setting `EPISODIC_DB`,
#' `EPISODIC_CONFIG`, `EPISODIC_PC_PROVINCE_MAP` and `EPISODIC_GEO_DATA`
#' back to them - the paths are printed when the demo finishes. Without
#' them the database still opens; it simply shows no map and no
#' provinces.
#' @inheritSection episodic_case_data Check your data before you run anything
#' @seealso [episodic_check_cases()] to see what EpiSODIC makes of your
#'   own extract first, and [episodic_case_data] for the shape it expects.
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
episodic_demo <- function(db_path = tempfile(fileext = ".sqlite"),
                          username = "demo",
                          full_name = "Demo User",
                          email = "demo@example.org",
                          password = "demo",
                          launch = TRUE,
                          run_date = episodic_synthetic_week_end(),
                          lang = Sys.getenv("EPISODIC_LANGUAGE"),
                          cases = function() episodic_synthetic_cases(end_date = run_date),
                          denominators = function() episodic_synthetic_denominators(end_date = run_date),
                          ...) {
  EPISODIC_CONFIG.old <- Sys.getenv("EPISODIC_CONFIG")
  EPISODIC_DB.old <- Sys.getenv("EPISODIC_DB")
  EPISODIC_GEO_DATA.old <- Sys.getenv("EPISODIC_GEO_DATA")
  EPISODIC_PC_PROVINCE_MAP.old <- Sys.getenv("EPISODIC_PC_PROVINCE_MAP")

  # The demo configures its own geography exactly the way a real
  # deployment does, through the documented environment variables, rather
  # than relying on EpiSODIC guessing. There is no guess left to rely on:
  # neither the map nor the province level has a built-in default,
  # because a default for either is a default for one *country*, and this
  # package is meant to run in any of them.
  #
  # Written beside the database rather than into a temporary file, and
  # not removed afterwards, so that `episodic_demo(launch = FALSE)`
  # leaves behind a demo somebody can actually re-open: the database on
  # its own, with these variables unset again, would show no map and no
  # provinces. `episodic_demo_files()` names them.
  demo <- episodic_demo_files(db_path)
  writeLines(episodic_demo_config_yaml(), demo$config)
  utils::write.csv(
    episodic_demo_pc_province_map(),
    demo$pc_province_map,
    row.names = FALSE,
    quote = FALSE
  )
  geo_path <- episodic_geo_source_default_path()
  Sys.setenv(
    EPISODIC_CONFIG = demo$config,
    EPISODIC_DB = db_path,
    EPISODIC_PC_PROVINCE_MAP = demo$pc_province_map
  )
  if (file.exists(geo_path)) {
    Sys.setenv(EPISODIC_GEO_DATA = geo_path)
  }
  on.exit(
    Sys.setenv(
      EPISODIC_CONFIG = EPISODIC_CONFIG.old,
      EPISODIC_DB = EPISODIC_DB.old,
      EPISODIC_GEO_DATA = EPISODIC_GEO_DATA.old,
      EPISODIC_PC_PROVINCE_MAP = EPISODIC_PC_PROVINCE_MAP.old
    )
  )

  message("Creating synthetic cases...", appendLF = FALSE)
  episodic_run_cron(
    cases = cases,
    db_path = db_path,
    denominators = denominators,
    run_date = run_date
  )
  message("OK")

  episodic_add_user(
    db_path = db_path,
    username = username,
    full_name = full_name,
    email = email,
    password = password,
    is_admin = TRUE,
    role = "epidemiologist",
    # The one account in a throwaway database, whose password is printed
    # two lines below - there is nothing for a forced change to protect.
    # This used to be arranged by `episodic_auth_must_change()` special-
    # casing the literal username "demo" against the literal password
    # "demo", which is a hardcoded credential in the sign-in path of a
    # package other people deploy.
    must_change = FALSE
  )
  message(paste0(
    strrep("=", 75),
    "\n\n  EpiSODIC demo account (admin) - username: ",
    username,
    ", password: ",
    password,
    "\n\n  To re-open this demo later, with its geography:\n",
    "    Sys.setenv(EPISODIC_DB = \"",
    db_path,
    "\",\n               EPISODIC_CONFIG = \"",
    demo$config,
    "\",\n               EPISODIC_PC_PROVINCE_MAP = \"",
    demo$pc_province_map,
    "\")\n    episodic_run_app()\n\n",
    strrep("=", 75)
  ))

  if (isTRUE(launch)) {
    episodic_run_app(db_path = db_path, lang = lang, ...)
  }

  invisible(db_path)
}

#' Where `episodic_demo()` writes the configuration it runs against
#'
#' Beside the database, named after it, so a demo built with
#' `launch = FALSE` can be re-opened later by pointing `EPISODIC_CONFIG`
#' and `EPISODIC_PC_PROVINCE_MAP` back at these two files.
#' @param db_path The demo database's path.
#' @return A list with `config` and `pc_province_map` paths.
#' @keywords internal
#' @noRd
episodic_demo_files <- function(db_path) {
  base <- tools::file_path_sans_ext(db_path)
  list(
    config = paste0(base, "-config.yaml"),
    pc_province_map = paste0(base, "-pc-province.csv")
  )
}

#' The instance configuration `episodic_demo()` runs against
#'
#' The shipped defaults plus the demo's own geography: the synthetic data
#' covers the northern Netherlands, and naming that in a config file is
#' how any instance names its own.
#' @return A character vector of YAML lines.
#' @keywords internal
#' @noRd
episodic_demo_config_yaml <- function() {
  c(
    "geography:",
    "  region_code: NORTHERN_NETHERLANDS",
    "  area_code_prefix: \"AREA-\"",
    "  area_pc_characters: 2"
  )
}

#' The postcode-to-province mapping `episodic_demo()` runs against
#'
#' The synthetic generator draws its postcodes from three Dutch
#' provinces (`episodic_synthetic_pc_pool()`), and EpiSODIC has no
#' built-in rule for turning a postcode into a province in any country -
#' so the demo supplies one, in exactly the CSV form
#' `EPISODIC_PC_PROVINCE_MAP` documents. Covering the whole 7000-9999
#' range rather than only the pool a given run happens to draw keeps it
#' correct for every run.
#'
#' @return A data frame with `pc` and `province_code` columns.
#' @keywords internal
#' @noRd
episodic_demo_pc_province_map <- function() {
  pc <- sprintf("%04d", 7000:9999)
  province <- c(
    "7" = "PROV_DRENTHE",
    "8" = "PROV_FRYSLAN",
    "9" = "PROV_GRONINGEN"
  )[substr(pc, 1, 1)]
  data.frame(
    pc = pc,
    province_code = unname(province),
    stringsAsFactors = FALSE
  )
}
