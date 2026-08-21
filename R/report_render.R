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

#' Render an outbreak report for clinical colleagues
#'
#' Produces a self-contained HTML outbreak report for one cluster - the
#' document you send to a treating physician, an infection prevention
#' team, or another clinical colleague who has neither an EpiSODIC account
#' nor R installed. The report includes the epidemic curve, trend chart,
#' the narrative summary shown in the dashboard, and (optionally) the case
#' line list.
#'
#' Every render is kept, versioned, and logged to the database, including
#' exactly which cases it contained - so what was sent out on a given date
#' stays a fully recoverable record, even if the underlying data changes
#' later.
#'
#' By default the report uses EpiSODIC's own report template. If your
#' organisation needs its own layout or branding, set the
#' `EPISODIC_QUARTO_REPORT` environment variable to your own `.qmd` file;
#' `inst/report/cluster_report.qmd` in the package source is a good
#' starting point to copy and adapt.
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
#' @param config The resolved configuration (see [episodic_config_resolve()]);
#'   only `config$report` is used.
#' @param lang Report language, `"nl"` (default) or `"en"`.
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
episodic_report_render <- function(con, cluster_id, output_dir, user_id = NA,
                                   include_linelist = TRUE, small_count_threshold = NULL,
                                   config = episodic_config_resolve(), lang = "nl",
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

  threshold <- small_count_threshold %||% config$report$small_count_threshold %||% 5L

  obj <- episodic_cluster_object(con, cluster_id, lang = lang)
  epi_curve <- episodic_app_epi_curve(con, cluster_id)
  trend <- episodic_app_trend(con, obj$stream_id)
  linelist <- if (isTRUE(include_linelist)) episodic_app_linelist(con, cluster_id) else NULL
  timeline <- episodic_app_assessment_timeline(con, cluster_id, lang = lang)
  similar <- episodic_app_similar_clusters(con, cluster_id, lang = lang)
  case_ids <- episodic_db_cluster_cases(con, cluster_id)$case_id

  if (!is.null(obj$concentration)) {
    obj$concentration$rows <- episodic_report_suppress_small_counts(obj$concentration$rows, "n", threshold)
  }

  report_data <- list(
    obj = obj, epi_curve = epi_curve, trend = trend, linelist = linelist, timeline = timeline,
    similar = similar, small_count_threshold = threshold, rendered_at = episodic_now(), lang = lang,
    package_version = as.character(utils::packageVersion("EpiSODIC"))
  )

  qmd_path <- episodic_report_qmd_path(qmd_path)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  existing <- episodic_db_reports_for_cluster(con, cluster_id)
  version_no <- if (nrow(existing) == 0) 1L else max(existing$version_no) + 1L

  work_dir <- tempfile("episodic_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  file.copy(qmd_path, file.path(work_dir, "cluster_report.qmd"))
  data_path <- file.path(work_dir, "report_data.rds")
  saveRDS(report_data, data_path)

  tryCatch({
    quarto::quarto_render(
      input = file.path(work_dir, "cluster_report.qmd"),
      execute_params = list(data_path = "report_data.rds"),
      # quiet = FALSE (not TRUE): the quarto R package always captures the
      # CLI's stderr into the condition it raises on failure, but only
      # embeds it in the error *message* when the CLI itself was not told
      # to be quiet - with quiet = TRUE the caller only ever sees "rerun
      # with quiet = FALSE", never the actual underlying cause.
      output_file = "report.html", quiet = FALSE, as_job = FALSE
    )
  }, error = function(e) {
    stop("Quarto failed to render this report: ", rlang::cnd_message(e, inherit = TRUE), call. = FALSE)
  })

  rendered_path <- file.path(work_dir, "report.html")
  if (!file.exists(rendered_path)) {
    stop("Quarto did not produce the expected output file: ", rendered_path, call. = FALSE)
  }

  out_file <- file.path(output_dir, sprintf("cluster-%d-v%d.html", cluster_id, version_no))
  file.copy(rendered_path, out_file, overwrite = TRUE)

  file_sha256 <- digest::digest(out_file, algo = "sha256", file = TRUE)
  params_json <- as.character(jsonlite::toJSON(
    list(cluster_id = cluster_id, include_linelist = include_linelist,
         small_count_threshold = threshold, lang = lang),
    auto_unbox = TRUE
  ))
  case_ids_json <- as.character(jsonlite::toJSON(case_ids))

  report_id <- episodic_db_report_render_insert(
    con, cluster_id = cluster_id, user_id = user_id, file_path = out_file,
    file_sha256 = file_sha256, params_json = params_json, case_ids_json = case_ids_json,
    version_no = version_no
  )

  invisible(list(file_path = out_file, file_sha256 = file_sha256, version_no = version_no,
                  report_id = report_id))
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

#' Resolve the Quarto report template to use
#'
#' An operator's own template, if `EPISODIC_QUARTO_REPORT` (or the
#' explicit `qmd_path` argument) names a file that actually exists;
#' otherwise the shipped `inst/report/cluster_report.qmd` default -
#' matching the shape `EPISODIC_CONFIG`/`EPISODIC_GEO_DATA`/... already
#' establish.
#' @param qmd_path A path, or `NA`.
#' @return A path to an existing `.qmd` file.
#' @keywords internal
#' @noRd
episodic_report_qmd_path <- function(qmd_path = Sys.getenv("EPISODIC_QUARTO_REPORT", unset = NA)) {
  if (!is.na(qmd_path) && nzchar(qmd_path) && file.exists(qmd_path)) {
    return(qmd_path)
  }
  default_path <- system.file("report", "cluster_report.qmd", package = "EpiSODIC")
  if (identical(default_path, "")) default_path <- file.path("inst", "report", "cluster_report.qmd")
  default_path
}

#' Suppress small counts in a breakdown table
#'
#' Standard disclosure control for a report that may leave the department
#': a cell with `0 < n < threshold` is replaced
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
  if (is.null(df) || nrow(df) == 0 || is.null(threshold) || threshold <= 1) return(df)
  n <- df[[count_col]]
  small <- !is.na(n) & n > 0 & n < threshold
  out <- as.character(n)
  out[small] <- paste0("<", threshold)
  df[[count_col]] <- out
  df
}
