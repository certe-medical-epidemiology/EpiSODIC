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

#' Export the resolved configuration as a zip file
#'
#' Bundles the fully-resolved configuration (shipped defaults, your
#' `EPISODIC_CONFIG` YAML overlay, and - if `db_path` points at a
#' database - any Settings-screen `notifications` override) together with
#' the pathogen configuration and, if set, the colour palette override,
#' into a single zip file. This is the Settings screen's "export
#' configuration" button; it is also just a plain function you can call
#' from the console to snapshot an instance's configuration for backup or
#' migration.
#'
#' @param db_path Path to the EpiSODIC database: an existing SQLite file,
#'   or a MariaDB/MySQL DSN (see [episodic_db_dsn_mariadb()]). Defaults to
#'   the `EPISODIC_DB` environment variable. Only used to read a
#'   Settings-screen `notifications` override, if one exists - `NA`/unset
#'   skips this and exports the YAML-resolved configuration only.
#' @param episodic_config_path Passed to [episodic_config_resolve()].
#' @param output_dir Directory to write the zip into. Defaults to a
#'   `config_exports/` directory next to `db_path` (mirroring where
#'   [episodic_report_render()] writes outbreak reports), or a temporary
#'   directory if `db_path` is not set.
#' @param include_secrets If `FALSE` (the default), notification secrets
#'   (SMTP/webhook/client passwords) are replaced with `"***"` in the
#'   exported configuration - the same masking the Settings screen applies
#'   on-screen. Pass `TRUE` only when you specifically intend the export to
#'   be usable to restore working notification channels elsewhere, and can
#'   handle the file with the same care as the secrets themselves.
#' @return Invisibly, the path to the written zip file.
#' @examples
#' \dontrun{
#' episodic_config_export(db_path = Sys.getenv("EPISODIC_DB"))
#' }
#' @export
episodic_config_export <- function(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
  output_dir = NULL,
  include_secrets = FALSE
) {
  rlang::check_installed("zip")

  con <- NULL
  if (!is.na(db_path) && nzchar(db_path) && episodic_db_exists(db_path)) {
    con <- episodic_db_connect(db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  config <- episodic_config_resolve(episodic_config_path, con = con)
  if (!isTRUE(include_secrets)) {
    config$notifications <- episodic_config_mask_secrets(config$notifications)
  }
  hashed <- episodic_config_hash(config)

  if (is.null(output_dir)) {
    output_dir <- if (!is.na(db_path) && nzchar(db_path)) {
      file.path(dirname(db_path), "config_exports")
    } else {
      tempdir()
    }
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  work_dir <- tempfile("episodic_config_export_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  yaml::write_yaml(config, file.path(work_dir, "config_resolved.yaml"))
  writeLines(
    c(
      paste0("config_hash: ", hashed$hash),
      paste0("exported_at: ", episodic_now()),
      paste0("secrets_included: ", isTRUE(include_secrets))
    ),
    file.path(work_dir, "config_manifest.txt")
  )

  pathogen_csv <- system.file(
    "config",
    "pathogen_config.csv",
    package = "EpiSODIC"
  )
  if (identical(pathogen_csv, "")) {
    pathogen_csv <- file.path("inst", "config", "pathogen_config.csv")
  }
  if (file.exists(pathogen_csv)) {
    file.copy(pathogen_csv, file.path(work_dir, "pathogen_config.csv"))
  }

  palette_path <- Sys.getenv("EPISODIC_PALETTE_CONFIG", unset = NA)
  if (
    !is.na(palette_path) && nzchar(palette_path) && file.exists(palette_path)
  ) {
    file.copy(palette_path, file.path(work_dir, "palette.yaml"))
  }

  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  zip_path <- file.path(output_dir, paste0("episodic_config_", stamp, ".zip"))

  # Registered with after = FALSE so it runs before the unlink(work_dir)
  # on.exit() above, however zip::zip() below turns out (error or not):
  # unlinking the process's *current* working directory is unreliable
  # across platforms, so the cwd must already be back to old_wd by the
  # time that runs.
  old_wd <- setwd(work_dir)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  zip::zip(zip_path, list.files("."))

  invisible(zip_path)
}

#' Replace known notification secrets with a mask, recursively
#'
#' @param x A (possibly nested) list, e.g. `config$notifications`.
#' @return The same structure with `password`, `client_secret`, and
#'   `webhook_url` values replaced by `"***"` wherever they occur, `NULL`
#'   left as `NULL`.
#' @keywords internal
#' @noRd
episodic_config_mask_secrets <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  secret_keys <- c("password", "client_secret", "webhook_url")
  if (is.list(x)) {
    nms <- names(x)
    for (i in seq_along(x)) {
      key <- if (!is.null(nms)) nms[i] else ""
      if (nzchar(key) && key %in% secret_keys && !is.null(x[[i]])) {
        x[[i]] <- "***"
      } else if (is.list(x[[i]])) {
        x[[i]] <- episodic_config_mask_secrets(x[[i]])
      }
    }
  }
  x
}
