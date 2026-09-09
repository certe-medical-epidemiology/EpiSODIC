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

#' Render an Outbreak Report for Clinical Colleagues
#'
#' Produces a self-contained HTML outbreak report for one cluster - the
#' document you send to a treating physician, an infection prevention
#' nurse, or a clinical colleague (e.g., a clinical microbiologist) who has
#' neither an EpiSODIC account nor R installed. The report includes the
#' epidemic curve, trend chart, the narrative summary shown in the
#' dashboard, and (optionally) the case line list.
#'
#' Every render is kept, versioned, and logged to the database, including
#' exactly which cases it contained - so what was sent out on a given date
#' stays a fully recoverable record, even if the underlying data changes
#' later.
#'
#' By default the report uses EpiSODIC's own report template. If your
#' organisation needs its own layout or branding, set the
#' `EPISODIC_QUARTO_REPORT` environment variable to your own `.qmd` file;
#' the default `r doc_system_file("inst/report/episodic_default_report.qmd")`
#' is a good starting point to copy and adapt.
#'
#' Rendering requires [Quarto](https://quarto.org) to be installed
#' separately (both the `quarto` R package and the Quarto command-line
#' tool) - this function raises an informative error if it is not found.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id The cluster to report on.
#' @param output_dir Directory the rendered HTML is written to. Created if
#'   it does not exist.
#' @param user_id The id of the user requesting the report, or `NA` for an
#'   automated (cron) render.
#' @param include_linelist If `TRUE` (default), the case line list is
#'   included in the report. Set to `FALSE` for a summary-only report, e.g.
#'   when sending outside your own organisation.
#' @param small_count_threshold Small counts in the geography/institution
#'   breakdown tables are suppressed (shown as `"<threshold"`) below this
#'   value, to avoid identifying individuals in a small population.
#'   Defaults to `config$report$small_count_threshold`.
#' @param episodic_config_path The config path.
#' @param lang Report language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @param qmd_path Path to the Quarto template to render. Defaults to the
#'   `EPISODIC_QUARTO_REPORT` environment variable, falling back to the
#'   shipped template if that is unset.
#' @return Invisibly, a list with `file_path` (where the HTML was written),
#'   `file_sha256`, `version_no`, and `report_id`.
#' @examples
#' \dontrun{
#' # needs both the quarto R package and the Quarto CLI installed, plus a
#' # database with at least one detected cluster - see episodic_demo() for
#' # a populated one, and the app's rail for a cluster_id to render
#' db_path <- episodic_demo(launch = FALSE)
#' con <- episodic_db_connect(db_path)
#' episodic_report_render(con, cluster_id = 1, output_dir = tempdir())
#' DBI::dbDisconnect(con)
#' }
#' @export
episodic_report_render <- function(con,
                                   cluster_id,
                                   output_dir,
                                   user_id = NA,
                                   include_linelist = TRUE,
                                   small_count_threshold = NULL,
                                   episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
                                   lang = Sys.getenv("EPISODIC_LANGUAGE"),
                                   qmd_path = Sys.getenv("EPISODIC_QUARTO_REPORT", unset = NA)) {
  if (!episodic_quarto_available()) {
    stop(
      "Rendering a report needs both the 'quarto' R package and the Quarto ",
      "CLI (https://quarto.org) installed - neither the report Rmd/HTML ",
      "content nor its self-contained-HTML packaging can happen without ",
      "the CLI, which the R package only wraps.",
      call. = FALSE
    )
  }

  config <- episodic_config_resolve(episodic_config_path)

  threshold <- small_count_threshold %||%
    config$report$small_count_threshold %||%
    5L

  obj <- episodic_cluster_object(con, cluster_id, lang = lang)
  epi_curve <- episodic_app_epi_curve(con, cluster_id)
  trend <- episodic_app_trend(con, obj$stream_id)
  linelist <- if (isTRUE(include_linelist)) {
    episodic_app_linelist(con, cluster_id)
  } else {
    NULL
  }
  timeline <- episodic_app_assessment_timeline(
    con,
    cluster_id,
    lang = lang,
    level = obj$level
  )
  similar <- episodic_app_similar_clusters(con, cluster_id, lang = lang)
  case_ids <- episodic_db_cluster_cases(con, cluster_id)$case_id

  if (!is.null(obj$concentration)) {
    obj$concentration$rows <- episodic_report_suppress_small_counts(
      obj$concentration$rows,
      "n",
      threshold
    )
  }

  qmd_path <- episodic_report_qmd_path(qmd_path)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  existing <- episodic_db_reports_for_cluster(con, cluster_id)
  version_no <- if (nrow(existing) == 0) 1L else max(existing$version_no) + 1L
  snapshot <- episodic_report_snapshot(obj)
  diff <- episodic_report_diff(existing, snapshot, case_ids)

  report_data <- list(
    obj = obj,
    epi_curve = epi_curve,
    trend = trend,
    linelist = linelist,
    timeline = timeline,
    similar = similar,
    diff = diff,
    small_count_threshold = threshold,
    rendered_at = episodic_now(),
    lang = lang,
    package_version = as.character(utils::packageVersion("EpiSODIC"))
  )

  work_dir <- tempfile("episodic_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  file.copy(qmd_path, file.path(work_dir, "episodic_default_report.qmd"))
  data_path <- file.path(work_dir, "report_data.rds")
  saveRDS(report_data, data_path)

  tryCatch(
    {
      quarto::quarto_render(
        input = file.path(work_dir, "episodic_default_report.qmd"),
        execute_params = list(data_path = "report_data.rds"),
        # quiet = FALSE (not TRUE): the quarto R package always captures the
        # CLI's stderr into the condition it raises on failure, but only
        # embeds it in the error *message* when the CLI itself was not told
        # to be quiet - with quiet = TRUE the caller only ever sees "rerun
        # with quiet = FALSE", never the actual underlying cause.
        output_file = "report.html",
        quiet = FALSE,
        as_job = FALSE
      )
    },
    error = function(e) {
      stop(
        "Quarto failed to render this report: ",
        rlang::cnd_message(e, inherit = TRUE),
        call. = FALSE
      )
    }
  )

  rendered_path <- file.path(work_dir, "report.html")
  if (!file.exists(rendered_path)) {
    stop(
      "Quarto did not produce the expected output file: ",
      rendered_path,
      call. = FALSE
    )
  }

  out_file <- file.path(
    output_dir,
    sprintf("cluster-%d-v%d.html", cluster_id, version_no)
  )
  file.copy(rendered_path, out_file, overwrite = TRUE)

  file_sha256 <- digest::digest(out_file, algo = "sha256", file = TRUE)
  params_json <- as.character(jsonlite::toJSON(
    list(
      cluster_id = cluster_id,
      include_linelist = include_linelist,
      small_count_threshold = threshold,
      lang = lang,
      snapshot = snapshot,
      diff = diff
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
  case_ids_json <- as.character(jsonlite::toJSON(case_ids))

  report_id <- episodic_db_report_render_insert(
    con,
    cluster_id = cluster_id,
    user_id = user_id,
    file_path = out_file,
    file_sha256 = file_sha256,
    params_json = params_json,
    case_ids_json = case_ids_json,
    version_no = version_no
  )

  invisible(list(
    file_path = out_file,
    file_sha256 = file_sha256,
    version_no = version_no,
    report_id = report_id
  ))
}

#' Whether report rendering is actually possible in this R session
#'
#' Both the `quarto` R package (Suggests) and the separate Quarto CLI
#' (https://quarto.org, not an R package at all) are required - the R
#' package is only a thin wrapper that shells out to the CLI binary, so
#' `requireNamespace("quarto")` alone is not sufficient.
#' [quarto::quarto_path()] returns `NULL` (not an error) when the CLI is
#' not found, which is what this checks.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_quarto_available <- function() {
  requireNamespace("quarto", quietly = TRUE) && !is.null(quarto::quarto_path())
}

#' Resolve the directory reports or config exports are written to
#'
#' `config$report$output_dir`, once set, always wins, and a value that
#' cannot be created is refused rather than silently falling back to
#' something else - a configured path pointing nowhere is an error, not
#' a fallback. Left unset, `db_path` is taken as a SQLite filesystem path
#' and `<subdir>` resolved next to it, which is the behaviour every call
#' site had before this existed.
#'
#' That fallback only means something for SQLite. For a MariaDB/MySQL
#' DSN (`mysql://user:password@host:port/db`), `db_path` is not a
#' filesystem path at all, and taking its "parent directory" turns the
#' DSN's own text, credentials included, into a directory name on disk -
#' the database password ends up on the filesystem, in backups, in `ls`
#' output. So with no `report.output_dir` configured and a DSN for
#' `db_path`, this refuses outright rather than deriving anything from
#' it.
#'
#' Used by every call site that used to compute
#' `file.path(dirname(db_path), <subdir>)` directly - the scheduled
#' report dispatcher, the dossier's on-demand render button, and
#' [episodic_config_export()] - so they cannot drift from each other or
#' from this rule. Pure aside from the `dir.create()` needed to validate
#' a configured path; takes a config list and a path string, not a
#' connection, so it is unit-testable without a database.
#'
#' @param config The resolved configuration.
#' @param db_path Path to the EpiSODIC database, or a MariaDB/MySQL DSN.
#' @param subdir The subdirectory to resolve within the base directory,
#'   e.g. `"reports"` or `"config_exports"`.
#' @return The resolved directory path, as a single string.
#' @keywords internal
#' @noRd
episodic_report_output_dir <- function(config, db_path, subdir) {
  configured <- config$report$output_dir
  if (!is.null(configured) && !is.na(configured) && nzchar(configured)) {
    output_dir <- file.path(configured, subdir)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    if (!dir.exists(output_dir)) {
      stop(
        "'report.output_dir' is set to '", configured, "', but '",
        output_dir, "' could not be created. Point it at a directory ",
        "that exists, or whose parent is writable.",
        call. = FALSE
      )
    }
    return(output_dir)
  }

  if (episodic_db_dialect(db_path) != "sqlite") {
    stop(
      "The database is a MariaDB/MySQL DSN, and 'report.output_dir' is ",
      "not configured, so there is no filesystem path to derive one ",
      "from. Deriving it from the DSN would put the database password ",
      "into a directory name on disk, so this refuses instead - set ",
      "'report.output_dir' in the instance configuration.",
      call. = FALSE
    )
  }

  file.path(dirname(db_path), subdir)
}

#' Resolve the Quarto report template to use
#'
#' An operator's own template, if `EPISODIC_QUARTO_REPORT` (or the
#' explicit `qmd_path` argument) names a file that actually exists;
#' otherwise the shipped `inst/report/episodic_default_report.qmd` default -
#' matching the shape `EPISODIC_CONFIG`/`EPISODIC_GEO_DATA`/... already
#' establish.
#' @param qmd_path A path, or `NA`.
#' @return A path to an existing `.qmd` file.
#' @keywords internal
#' @noRd
episodic_report_qmd_path <- function(qmd_path = Sys.getenv("EPISODIC_QUARTO_REPORT", unset = NA)) {
  if (!is.na(qmd_path) && nzchar(qmd_path)) {
    # Set but missing is a configuration error, not a fallback. Silently
    # rendering the shipped template instead sends an organisation's
    # outbreak reports out under EpiSODIC's own layout and branding
    # rather than theirs, which is exactly the kind of thing nobody
    # notices until the report has already left the building.
    if (!file.exists(qmd_path)) {
      stop(
        "EPISODIC_QUARTO_REPORT points at '",
        qmd_path,
        "', but no file exists there. Correct the path, or unset it to ",
        "render the shipped report template.",
        call. = FALSE
      )
    }
    return(qmd_path)
  }
  default_path <- system.file(
    "report",
    "episodic_default_report.qmd",
    package = "EpiSODIC"
  )
  if (identical(default_path, "")) {
    default_path <- file.path("inst", "report", "episodic_default_report.qmd")
  }
  default_path
}

#' Suppress small counts in a breakdown table
#'
#' Standard disclosure control for a report that may leave the department
#' : a cell with `0 < n < threshold` is replaced
#' with `"<threshold"` rather than the exact count, since a single-digit
#' count at a named place can be personally identifying in a small
#' population. Zero is left as `0` (absence is not disclosive) and `NA`
#' passes through unchanged.
#'
#' @param df A data frame with a count column.
#' @param count_col The name of the count column.
#' @param threshold Counts below this are suppressed. `NULL` or `<= 1`
#'   disables suppression (returns `df` unchanged).
#' @return `df`, with `count_col` coerced to character where suppression
#'   applied.
#' @keywords internal
#' @noRd
episodic_report_suppress_small_counts <- function(df, count_col, threshold) {
  if (is.null(df) || nrow(df) == 0 || is.null(threshold) || threshold <= 1) {
    return(df)
  }
  n <- df[[count_col]]
  small <- !is.na(n) & n > 0 & n < threshold
  out <- as.character(n)
  out[small] <- paste0("<", threshold)
  df[[count_col]] <- out
  df
}

#' Build the comparable "snapshot" of one report render
#'
#' Stored alongside every render's other parameters (inside `params`, as
#' `episodic_db_reports_for_cluster()` already returns it - no schema
#' change needed) so the *next* render of the same cluster can compute
#' what changed since this one, without re-deriving anything from case
#' data that may since have moved on (dedup, corrections). Deliberately
#' just the handful of headline numbers a reader compares report to
#' report - not a copy of `obj` itself, which carries far more than a
#' diff needs.
#' @param obj A cluster object, as returned by `episodic_cluster_object()`.
#' @return A list of scalars, JSON-serialisable.
#' @keywords internal
#' @noRd
episodic_report_snapshot <- function(obj) {
  list(
    n_cases = obj$n_cases,
    expected = obj$expected,
    ratio = obj$ratio,
    priority_score = obj$priority_score,
    last_day = obj$last_day
  )
}

#' What changed since the previous rendered version of this report
#'
#' `NULL` when there is no previous version, or when the previous
#' version predates this feature and therefore carries no `snapshot` in
#' its stored `params` - in both cases the report simply omits the
#' "changes since the previous report" section rather than showing a
#' diff against nothing.
#' @param existing The cluster's existing `episodic_report_render` rows,
#'   as returned by `episodic_db_reports_for_cluster()` (zero rows for
#'   the first-ever render).
#' @param snapshot This render's own snapshot, from
#'   `episodic_report_snapshot()`.
#' @param case_ids This render's case ids, for an exact new-case count
#'   even when a late-arriving result backdates into the earlier period.
#' @return A list with `previous_version_no`, `previous_rendered_at`,
#'   `n_new_cases`, `n_cases_delta`, `priority_score_delta`,
#'   `ratio_delta`, and `period_extended` (logical) - or `NULL`.
#' @keywords internal
#' @noRd
episodic_report_diff <- function(existing, snapshot, case_ids) {
  if (nrow(existing) == 0) {
    return(NULL)
  }
  previous_row <- existing[which.max(existing$version_no), ]
  previous_params <- tryCatch(
    jsonlite::fromJSON(previous_row$params),
    error = function(e) NULL
  )
  previous_snapshot <- previous_params$snapshot
  if (is.null(previous_snapshot)) {
    return(NULL)
  }
  previous_case_ids <- tryCatch(
    jsonlite::fromJSON(previous_row$case_ids),
    error = function(e) integer(0)
  )
  previous_ratio <- previous_snapshot$ratio %||% NA_real_

  list(
    previous_version_no = previous_row$version_no,
    previous_rendered_at = previous_row$rendered_at,
    n_new_cases = length(setdiff(case_ids, previous_case_ids)),
    n_cases_delta = snapshot$n_cases - previous_snapshot$n_cases,
    priority_score_delta = snapshot$priority_score - previous_snapshot$priority_score,
    ratio_delta = if (is.na(snapshot$ratio) || is.na(previous_ratio)) {
      NA_real_
    } else {
      snapshot$ratio - previous_ratio
    },
    period_extended = !identical(snapshot$last_day, previous_snapshot$last_day) &&
      as.Date(snapshot$last_day) > as.Date(previous_snapshot$last_day)
  )
}
