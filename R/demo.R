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
#'   behind once the R session ends. A database that already exists is
#'   refused, and a MariaDB/MySQL DSN is refused outright: this call
#'   generates synthetic cases and runs detection over them, which is
#'   contamination anywhere but a throwaway database.
#' @param overwrite If `TRUE`, an existing demo database at `db_path` and
#'   the two configuration files beside it are deleted and rebuilt.
#'   `FALSE` (the default) refuses instead. This deletes whatever is at
#'   that path, so it is deliberately not something the demo decides for
#'   you.
#' @param username,full_name,email,password Credentials for the demo
#'   epidemiologist account this creates, so you can sign in and classify a
#'   cluster right away. These are placeholder values - change them for
#'   anything beyond a local demo.
#' @param launch If `TRUE` (default), opens the dashboard afterwards (see
#'   [episodic_run_app()]); this call blocks until you close it. Set to
#'   `FALSE` to only build the demo database and return its path, e.g. for
#'   scripting or screenshots.
#' @param run_date The date to run detection as of. Left `NULL` (the
#'   default) it is chosen for you, and which way depends on whose data
#'   this is: for the bundled synthetic data, the end of the last
#'   complete week, so the week the statistical detectors test is a full
#'   one however far into the week you happen to run the demo; for a
#'   `cases` extract you supply yourself, the last day that extract
#'   covers, and the demo says so when it does it.
#'
#'   That second rule is the demo's alone. Detection is bounded to a
#'   lookback window around `run_date`, so a run dated today against an
#'   extract from last year correctly finds nothing - which is right for
#'   a scheduled run and useless for someone trying the system out on a
#'   historical export. [episodic_run_cron()] therefore keeps dating its
#'   runs from the system date, as a real surveillance run must.
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
                          run_date = NULL,
                          lang = Sys.getenv("EPISODIC_LANGUAGE"),
                          cases = NULL,
                          denominators = NULL,
                          overwrite = FALSE,
                          ...) {
  episodic_demo_check_db_path(db_path, overwrite = overwrite)

  # Resolved here rather than in the signature, because each of these
  # defaults depends on the others and R cannot express that: the
  # synthetic generators are anchored to `run_date`, while `run_date`
  # for somebody's own extract has to come from the extract.
  #
  # `missing()`, not a `NULL` sentinel: `denominators = NULL` is a
  # meaningful thing for a caller to write - it says "no denominator
  # feed at all", and the examples below do exactly that - so "not
  # supplied" and "supplied as NULL" have to stay distinguishable.
  # Captured up front, since `missing()` stops being reliable once the
  # formal it asks about has been assigned to.
  # `NULL` counts as "not supplied" for `cases` and `run_date`, where it
  # says nothing a caller could mean, and as a real value for
  # `denominators`, where it means "no denominator feed".
  supplied_cases <- !missing(cases) && !is.null(cases)
  supplied_run_date <- !missing(run_date) && !is.null(run_date)
  supplied_denominators <- !missing(denominators)

  # `cases` is resolved to a data frame once, and the data frame is what
  # goes to `episodic_run_cron()`. Resolving a generator function twice
  # would produce two different data sets, and date the run from one of
  # them while detecting on the other.
  if (supplied_cases) {
    cases <- episodic_resolve_data(cases)
    if (!supplied_run_date) {
      run_date <- episodic_demo_run_date(cases)
      message(
        "Running detection as of ",
        format(run_date),
        ", the last day your case data covers."
      )
    }
  }
  if (!supplied_run_date && !supplied_cases) {
    run_date <- episodic_synthetic_week_end()
  }
  if (!supplied_cases) {
    cases <- function() episodic_synthetic_cases(end_date = run_date)
  }
  if (!supplied_denominators) {
    denominators <- function() episodic_synthetic_denominators(end_date = run_date)
  }

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

#' Refuse to build a demo anywhere but a throwaway database
#'
#' `episodic_demo()` generates synthetic cases and runs detection over
#' them. Pointed at a database that already holds data, that is not a
#' demo: the synthetic cases land in `episodic_case` alongside the real
#' ones, reach every denominator, line list and patient search, and
#' cannot be told apart afterwards without knowing which run wrote them.
#' A MariaDB/MySQL DSN is refused whatever it contains, since the demo
#' writes two configuration files named after `db_path` and a DSN is a
#' server instance rather than a scratch file.
#'
#' @param db_path The path the demo was asked to build at.
#' @param overwrite Whether the caller explicitly asked for an existing
#'   demo database to be deleted and rebuilt.
#' @return Invisible `NULL`, or an error.
#' @keywords internal
#' @noRd
episodic_demo_check_db_path <- function(db_path, overwrite = FALSE) {
  usable <- is.character(db_path) && length(db_path) == 1 &&
    !is.na(db_path) && nzchar(db_path)
  if (!usable) {
    stop(
      "`db_path` must be a single non-empty path to an SQLite file.",
      call. = FALSE
    )
  }
  if (episodic_db_dialect(db_path) != "sqlite") {
    stop(
      "episodic_demo() builds a throwaway SQLite database and writes two ",
      "configuration files beside it, so it cannot be pointed at a ",
      "MariaDB/MySQL instance. Leave `db_path` at its default, or give a ",
      "path to a file that does not exist yet.",
      call. = FALSE
    )
  }
  if (!file.exists(db_path)) {
    return(invisible(NULL))
  }
  if (!isTRUE(overwrite)) {
    stop(
      "There is already a database at ",
      db_path,
      ". episodic_demo() generates synthetic cases and runs detection over ",
      "them, so building a demo here would mix synthetic data into whatever ",
      "is already stored - irreversibly, and invisibly, if this is a real ",
      "instance. Give a `db_path` that does not exist yet, or pass ",
      "overwrite = TRUE to delete this one and rebuild it.",
      call. = FALSE
    )
  }

  demo <- episodic_demo_files(db_path)
  existing <- c(db_path, demo$config, demo$pc_province_map)
  existing <- existing[file.exists(existing)]
  removed <- unlink(existing)
  if (removed != 0 || any(file.exists(existing))) {
    stop(
      "Could not remove the existing demo at ",
      db_path,
      " - delete it yourself, or give a `db_path` that does not exist yet.",
      call. = FALSE
    )
  }
  message("Removed the existing demo at ", db_path, ".")
  invisible(NULL)
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

#' The date to run a demo of somebody's own extract as of
#'
#' The last day the extract covers. Anything later is a run that looks
#' back at data older than its own detection window and finds nothing,
#' which is correct behaviour and a poor first impression - and the
#' reason `episodic_demo()` does this while `episodic_run_cron()`
#' deliberately does not.
#'
#' @param cases A resolved case data frame.
#' @return A single `Date`; the synthetic default's own anchor when the
#'   extract carries no usable `sample_date` at all, since there is
#'   nothing in it to date the run from.
#' @keywords internal
#' @noRd
episodic_demo_run_date <- function(cases) {
  dates <- suppressWarnings(as.Date(cases$sample_date))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(episodic_synthetic_week_end())
  }
  max(dates)
}
