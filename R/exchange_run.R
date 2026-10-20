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

# How a detection run takes imported cases in: combined with the
# operator's own case data before anything is written, loaded in the same
# transaction, and recorded against the run that consumed them.

#' An `imports` argument as a list of imported case data frames
#' @param imports `NULL`, one `episodic_exchange_cases` data frame, or a
#'   list of them.
#' @return A list of `episodic_exchange_cases` data frames.
#' @keywords internal
#' @noRd
episodic_exchange_imports_normalise <- function(imports) {
  if (is.null(imports)) {
    return(list())
  }
  if (inherits(imports, "episodic_exchange_cases")) {
    return(list(imports))
  }
  if (is.list(imports) && !is.data.frame(imports) &&
    all(vapply(imports, inherits, logical(1), "episodic_exchange_cases"))) {
    return(unname(imports))
  }
  stop(
    "`imports` must be what episodic_exchange_import() returns, or a list ",
    "of those. A data frame of your own goes in `cases`; an imported one ",
    "keeps the class and the \"exchange\" attribute that say whose cases ",
    "they are.",
    call. = FALSE
  )
}

#' One import, as the run records it
#'
#' @param cases The imported cases, or `NULL` for a source that failed.
#' @param source_name,location The registered source, or `NA` for a file
#'   handed to the run directly.
#' @param error An `episodic_exchange_error` or other condition, for a
#'   source that failed, or `NULL`.
#' @return A list with `status`, `cases`, `meta`, `source_name`,
#'   `location`, `failure_reason` and `error_text`.
#' @keywords internal
#' @noRd
episodic_exchange_entry <- function(cases = NULL,
                                    source_name = NA_character_,
                                    location = NA_character_,
                                    error = NULL) {
  list(
    status = if (is.null(error)) "imported" else "failed",
    cases = cases,
    meta = if (is.null(cases)) NULL else attr(cases, "exchange"),
    source_name = source_name,
    location = location,
    failure_reason = if (is.null(error)) {
      NA_character_
    } else {
      as.character(error$reason %||% "error")
    },
    error_text = if (is.null(error)) NA_character_ else conditionMessage(error)
  )
}

#' Dates as `YYYY-MM-DD` text, whichever form a feed gave them in
#' @keywords internal
#' @noRd
episodic_exchange_date_text <- function(x) {
  if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
}

#' Combine the operator's cases with the imported ones
#'
#' Refused, before the run writes anything:
#'
#' \itemize{
#'   \item an import whose sender is this instance itself: its cases are
#'     already here under their own keys, and imported they would count
#'     twice;
#'   \item an import whose keys do not carry its sender's prefix, which
#'     means the data frame was altered after `episodic_exchange_import()`
#'     and its keys can no longer be told from anyone else's;
#'   \item a `source_key` shared by one of the operator's own cases and an
#'     imported one: the loader keys on it, so one of them would be
#'     skipped as already present.
#' }
#'
#' The same imported `source_key` in two files is the same result sent
#' twice, which the overlapping windows of scheduled exports do by
#' design; the first is kept, and the count of the others is reported.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cases The operator's own case data, already checked.
#' @param entries A list of `episodic_exchange_entry()` results.
#' @return A list with `cases` (the combined data frame, case data
#'   columns only), `origin_instance_id` (one per row, `NA` for the
#'   operator's own), `entries` and `n_repeated`.
#' @keywords internal
#' @noRd
episodic_exchange_combine <- function(con, cases, entries) {
  local <- as.data.frame(cases, stringsAsFactors = FALSE)[, episodic_case_columns, drop = FALSE]
  imported <- Filter(function(e) identical(e$status, "imported"), entries)
  if (length(imported) == 0) {
    return(list(
      cases = cases,
      origin_instance_id = NA_character_,
      entries = entries,
      n_repeated = 0L
    ))
  }
  own <- episodic_db_instance_id(con)
  parts <- list(local)
  origins <- list(rep(NA_character_, nrow(local)))
  for (entry in imported) {
    sender <- entry$meta$sender_instance_id
    if (!episodic_instance_id_valid(sender)) {
      stop(
        "An import carries no valid sender instance identifier; pass ",
        "`imports` exactly as episodic_exchange_import() returned them.",
        call. = FALSE
      )
    }
    if (identical(sender, own)) {
      stop(
        "An import comes from this instance itself (", own, "): its cases ",
        "are already here under their own keys, and imported they would ",
        "count twice.",
        call. = FALSE
      )
    }
    part <- entry$cases
    class(part) <- "data.frame"
    attr(part, "exchange") <- NULL
    episodic_check_cases(part, stop_on_problem = TRUE)
    prefix <- paste0(sender, ":")
    for (column in c("source_key", "patient_key", "lab_number")) {
      if (!all(startsWith(part[[column]], prefix))) {
        stop(
          "An import from instance ", sender, " has `", column, "` values ",
          "without its prefix \"", prefix, "\": the data frame was changed ",
          "after episodic_exchange_import(), and its keys can no longer be ",
          "told from this instance's own.",
          call. = FALSE
        )
      }
    }
    parts[[length(parts) + 1L]] <- part[, episodic_case_columns, drop = FALSE]
    origins[[length(origins) + 1L]] <- rep(sender, nrow(part))
  }
  parts <- lapply(parts, function(p) {
    p$sample_date <- episodic_exchange_date_text(p$sample_date)
    p$receipt_date <- episodic_exchange_date_text(p$receipt_date)
    p$age <- as.integer(p$age)
    p
  })
  combined <- do.call(rbind, parts)
  rownames(combined) <- NULL
  origin <- unlist(origins, use.names = FALSE)

  is_local <- is.na(origin)
  collision <- intersect(combined$source_key[is_local], combined$source_key[!is_local])
  if (length(collision) > 0) {
    stop(
      length(collision), " source_key value(s) occur both in your own case ",
      "data and in an import (", paste(utils::head(collision, 3), collapse = ", "),
      "). Imported keys carry their sender's instance identifier and a ",
      "colon as a prefix; a key of your own written that way cannot be told ",
      "from a partner's. Change those keys in your extract step.",
      call. = FALSE
    )
  }
  repeated <- !is_local & duplicated(combined$source_key)
  if (any(repeated)) {
    combined <- combined[!repeated, , drop = FALSE]
    origin <- origin[!repeated]
    rownames(combined) <- NULL
  }
  list(
    cases = combined,
    origin_instance_id = origin,
    entries = entries,
    n_repeated = sum(repeated)
  )
}

#' Record a run's imports
#'
#' One row per import the run was handed or fetched, the failed ones
#' included, written inside the run's transaction so the record commits
#' exactly when the cases do.
#' @param con A [DBI::DBIConnection-class].
#' @param run_id The run.
#' @param entries A list of `episodic_exchange_entry()` results.
#' @return Invisibly, the number of rows written.
#' @keywords internal
#' @noRd
episodic_db_exchange_import_record <- function(con, run_id, entries) {
  json <- function(x) {
    if (is.null(x)) NA_character_ else as.character(jsonlite::toJSON(as.character(x)))
  }
  for (entry in entries) {
    meta <- entry$meta
    episodic_db_execute(
      con,
      "INSERT INTO episodic_exchange_import_event
         (run_id, imported_at, source_name, location, status, failure_reason,
          error_text, mode, sender_instance_id, signer_fingerprint,
          file_sha256, date_from, date_to, file_created_at,
          sender_episodic_version, n_cases, omitted_fields, warnings)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        as.integer(run_id),
        episodic_now(),
        as.character(entry$source_name),
        as.character(entry$location),
        entry$status,
        as.character(entry$failure_reason),
        as.character(entry$error_text),
        meta$mode %||% NA_character_,
        meta$sender_instance_id %||% NA_character_,
        meta$signer_fingerprint %||% NA_character_,
        meta$file_sha256 %||% NA_character_,
        meta$from %||% NA_character_,
        meta$to %||% NA_character_,
        meta$created_at %||% NA_character_,
        meta$sender_episodic_version %||% NA_character_,
        if (is.null(meta)) NA_integer_ else as.integer(meta$n_cases),
        json(meta$omitted_fields),
        json(meta$warnings)
      )
    )
  }
  invisible(length(entries))
}
