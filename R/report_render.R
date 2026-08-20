#' Render an outbreak report for a cluster
#'
#' A parameterised Quarto template rendered to self-contained HTML
#' (ARCHITECTURE.md section 11). Sent as a file, not a
#' link into the app: the medical staff receiving it have neither R nor an
#' account, and a static artefact is also the defensible record of what was
#' communicated on that date. Every render is versioned and registered in
#' `episode_report_render` (`episode_db_report_render_insert()`) - never
#' overwritten, so what was handed to a microbiologist on a given morning
#' stays exactly recoverable, including which cases it contained
#' (`case_ids`).
#'
#' Data for the template is assembled here (from the same read models the
#' app itself uses: [episode_cluster_object()], [episode_app_epi_curve()],
#' [episode_app_linelist()]) and handed to Quarto as a single RDS side
#' channel rather than as `execute_params` values directly - a line list
#' data frame has no clean YAML/JSON representation, and passing one path
#' string keeps the template itself simple (`readRDS(params$data_path)`).
#'
#' Line-list inclusion is decided here, at render time, via
#' `include_linelist` - independent of whoever later opens the file, unlike
#' the live app's own line-list panel which is gated on the *viewer's*
#' current session (ARCHITECTURE.md section 9: "Hidden entirely for
#' anonymous viewers"). A rendered report has no viewer session at all.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cluster_id A cluster id.
#' @param output_dir Directory the rendered HTML is written to. Created if
#'   it does not exist.
#' @param user_id The rendering user's id, or `NA` for a cron pre-render
#'   (matches `episode_report_render.user_id`'s own documented meaning).
#' @param include_linelist If `TRUE` (default), the line list is embedded
#'   in the report.
#' @param small_count_threshold Cells below this count are suppressed in
#'   the geography/institution breakdown tables ("small-count suppression
#'   configurable for reports leaving the department", ARCHITECTURE.md
#'   section 9). Defaults to `config$report$small_count_threshold`.
#' @param config The resolved configuration; only `config$report` is used.
#' @param lang Report language, `"nl"` (default) or `"en"`.
#' @param qmd_path Path to the Quarto template to render. Defaults to the
#'   `EPISODE_QUARTO_REPORT` environment variable; if that is unset (or
#'   names a file that does not exist), falls back to the shipped
#'   `inst/report/cluster_report.qmd`. An operator's own template only
#'   needs to read `params$data_path` (an `.rds` path, `readRDS()`'d to
#'   the same list this function assembles: `obj`, `epi_curve`, `trend`,
#'   `linelist`, `timeline`, `similar`, `small_count_threshold`,
#'   `rendered_at`, `lang`, `package_version`) - see the shipped template
#'   for the exact shape and for `episode_tr(..., lang = d$lang)` usage.
#' @return Invisibly, a list with `file_path`, `file_sha256`, `version_no`,
#'   `report_id`.
#' @export
episode_report_render <- function(con, cluster_id, output_dir, user_id = NA,
                                   include_linelist = TRUE, small_count_threshold = NULL,
                                   config = episode_config_resolve(), lang = "nl",
                                   qmd_path = Sys.getenv("EPISODE_QUARTO_REPORT", unset = NA)) {
  if (!episode_quarto_available()) {
    stop(
      "Rendering a report needs both the 'quarto' R package and the Quarto ",
      "CLI (https://quarto.org) installed - neither the report Rmd/HTML ",
      "content nor its self-contained-HTML packaging can happen without ",
      "the CLI, which the R package only wraps.",
      call. = FALSE
    )
  }

  threshold <- small_count_threshold %||% config$report$small_count_threshold %||% 5L

  obj <- episode_cluster_object(con, cluster_id, lang = lang)
  epi_curve <- episode_app_epi_curve(con, cluster_id)
  trend <- episode_app_trend(con, obj$stream_id)
  linelist <- if (isTRUE(include_linelist)) episode_app_linelist(con, cluster_id) else NULL
  timeline <- episode_app_assessment_timeline(con, cluster_id, lang = lang)
  similar <- episode_app_similar_clusters(con, cluster_id, lang = lang)
  case_ids <- episode_db_cluster_cases(con, cluster_id)$case_id

  if (!is.null(obj$concentration)) {
    obj$concentration$rows <- episode_report_suppress_small_counts(obj$concentration$rows, "n", threshold)
  }

  report_data <- list(
    obj = obj, epi_curve = epi_curve, trend = trend, linelist = linelist, timeline = timeline,
    similar = similar, small_count_threshold = threshold, rendered_at = episode_now(), lang = lang,
    package_version = as.character(utils::packageVersion("EpiSODE"))
  )

  qmd_path <- episode_report_qmd_path(qmd_path)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  existing <- episode_db_reports_for_cluster(con, cluster_id)
  version_no <- if (nrow(existing) == 0) 1L else max(existing$version_no) + 1L

  work_dir <- tempfile("episode_report_")
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

  report_id <- episode_db_report_render_insert(
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
episode_quarto_available <- function() {
  requireNamespace("quarto", quietly = TRUE) && !is.null(quarto::quarto_path())
}

#' Resolve the Quarto report template to use
#'
#' An operator's own template, if `EPISODE_QUARTO_REPORT` (or the
#' explicit `qmd_path` argument) names a file that actually exists;
#' otherwise the shipped `inst/report/cluster_report.qmd` default -
#' matching the shape `EPISODE_CONFIG`/`EPISODE_GEO_DATA`/... already
#' establish.
#' @param qmd_path A path, or `NA`.
#' @return A path to an existing `.qmd` file.
#' @keywords internal
#' @noRd
episode_report_qmd_path <- function(qmd_path = Sys.getenv("EPISODE_QUARTO_REPORT", unset = NA)) {
  if (!is.na(qmd_path) && nzchar(qmd_path) && file.exists(qmd_path)) {
    return(qmd_path)
  }
  default_path <- system.file("report", "cluster_report.qmd", package = "EpiSODE")
  if (identical(default_path, "")) default_path <- file.path("inst", "report", "cluster_report.qmd")
  default_path
}

#' Suppress small counts in a breakdown table
#'
#' Standard disclosure control for a report that may leave the department
#' (ARCHITECTURE.md section 9): a cell with `0 < n < threshold` is replaced
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
episode_report_suppress_small_counts <- function(df, count_col, threshold) {
  if (is.null(df) || nrow(df) == 0 || is.null(threshold) || threshold <= 1) return(df)
  n <- df[[count_col]]
  small <- !is.na(n) & n > 0 & n < threshold
  out <- as.character(n)
  out[small] <- paste0("<", threshold)
  df[[count_col]] <- out
  df
}
