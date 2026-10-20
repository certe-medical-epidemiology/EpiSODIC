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

# Exporting a date range of this instance's own cases to an exchange file.
#
# Three callers build the same file: episodic_exchange_export() from a
# script, the cron's scheduled exports (R/exchange_schedule.R) and the
# dashboard's download button (R/app_exchange.R). All three go through
# episodic_exchange_build() for the file and
# episodic_db_exchange_export_record() for its audit row, so what an
# export contains and what the audit trail says about it cannot differ by
# who asked for it.

#' Read a date argument, refusing anything that is not one date
#' @param x A `Date` or `"YYYY-MM-DD"`.
#' @param what The argument's name, for the error.
#' @return A `Date`.
#' @keywords internal
#' @noRd
episodic_exchange_date <- function(x, what) {
  if (inherits(x, "Date") && length(x) == 1L && !is.na(x)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  stop("`", what, "` must be one date (a Date, or \"YYYY-MM-DD\").", call. = FALSE)
}

#' Whether a value names an environment variable
#' @keywords internal
#' @noRd
episodic_exchange_env_name_valid <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)
}

#' The field set an export carries
#'
#' The required case columns always, plus the optional ones `fields`
#' names, or, when `fields` is `NULL`, the configured
#' `exchange.export_fields` (`~` there meaning none of them). A name that
#' is not an optional case column is refused: a typo in a field list must
#' not quietly export more, or less, than was asked for.
#'
#' @param fields A character vector of optional case columns, or `NULL`.
#' @param config A resolved configuration.
#' @return A list with `fields` (every column the export carries, in
#'   case data order) and `omitted` (the optional columns it leaves out).
#' @keywords internal
#' @noRd
episodic_exchange_fields <- function(fields, config) {
  chosen <- if (is.null(fields)) {
    unlist(config$exchange$export_fields)
  } else {
    fields
  }
  chosen <- as.character(chosen)
  omittable <- episodic_exchange_omittable_fields()
  required <- episodic_case_columns_required
  named_required <- intersect(chosen, required)
  unknown <- setdiff(chosen, c(omittable, required))
  if (length(unknown) > 0) {
    stop(
      "Not an optional case column, so not something an export can include ",
      "or leave out: ", paste(unknown, collapse = ", "), ". The optional ",
      "columns are ", paste(omittable, collapse = ", "), "; the required ",
      "ones (", paste(required, collapse = ", "), ") are always exported.",
      call. = FALSE
    )
  }
  if (length(named_required) > 0) {
    chosen <- setdiff(chosen, named_required)
  }
  included <- episodic_case_columns[episodic_case_columns %in% c(required, chosen)]
  list(fields = included, omitted = setdiff(omittable, chosen))
}

#' The postcode truncation an export applies, validated
#' @param pc_characters A whole number of at least 1, or `NULL` for none.
#' @param config A resolved configuration, for the default.
#' @return An integer, or `NULL`.
#' @keywords internal
#' @noRd
episodic_exchange_pc_characters <- function(pc_characters, config) {
  value <- pc_characters %||% config$exchange$pc_characters
  if (is.null(value)) {
    return(NULL)
  }
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || number < 1 || number != round(number)) {
    stop(
      "`pc_characters` (or exchange.pc_characters) must be a whole number ",
      "of at least 1, or left unset to export postcodes as stored, not ",
      paste(format(value), collapse = ", "), ".",
      call. = FALSE
    )
  }
  as.integer(number)
}

#' This instance's own cases in a sample-date range, as the case data format names them
#'
#' Cases imported from a partner are left out: forwarding one partner's
#' data to another is a decision for a data-sharing agreement, not a side
#' effect of exporting a range. `episodic_case.origin_instance_id` says
#' which those are. The institution columns come from
#' `episodic_institution`, so `institution_key` is the stored hash.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param from,to `Date`s, inclusive.
#' @return A data frame with `episodic_case_columns`, ordered by sample
#'   date.
#' @keywords internal
#' @noRd
episodic_db_exchange_cases <- function(con, from, to) {
  cases <- episodic_db_get_query(
    con,
    "SELECT c.source_key, c.lab_number, c.patient_key, c.sample_date,
            c.receipt_date, c.pathogen, c.care_line,
            i.institution_key, i.display_name AS institution_display_name,
            i.institution_type, i.municipality,
            c.ward, c.specialism, c.pc, c.sex, c.age
       FROM episodic_case c
       LEFT JOIN episodic_institution i ON i.institution_id = c.institution_id
      WHERE c.sample_date >= ? AND c.sample_date <= ?
        AND c.origin_instance_id IS NULL
      ORDER BY c.sample_date, c.case_id",
    params = list(format(from, "%Y-%m-%d"), format(to, "%Y-%m-%d"))
  )
  if (any(is.na(cases$institution_key))) {
    stop(
      sum(is.na(cases$institution_key)), " case(s) in this range have no ",
      "institution on file, and an exchange file carries the institution ",
      "of every case. The database is inconsistent; this export was not ",
      "written.",
      call. = FALSE
    )
  }
  cases[, episodic_case_columns, drop = FALSE]
}

#' Build an exchange file's bytes from the database
#'
#' Everything an export decides happens here: which cases, which columns,
#' how coarse the postcodes, to whom, signed by whom. Writing the bytes
#' somewhere and recording the export are the caller's.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param from,to The sample-date range, inclusive.
#' @param recipients Recipients' public encryption keys (texts or paths),
#'   or `NULL` for passphrase mode.
#' @param passphrase The passphrase, or `NULL` for public-key mode.
#' @param signing_key The sender's secret signing key (text or path).
#' @param fields,pc_characters See `episodic_exchange_export()`.
#' @param config A resolved configuration.
#' @return A list with `bytes`, `file_sha256`, `n_cases`, `mode`,
#'   `fields`, `omitted`, `pc_characters`, `recipient_fingerprints`,
#'   `signer_fingerprint`, `from` and `to`.
#' @keywords internal
#' @noRd
episodic_exchange_build <- function(con,
                                    from,
                                    to,
                                    recipients,
                                    passphrase,
                                    signing_key,
                                    fields,
                                    pc_characters,
                                    config) {
  from <- episodic_exchange_date(from, "from")
  to <- episodic_exchange_date(to, "to")
  if (from > to) {
    stop("`from` (", from, ") is after `to` (", to, ").", call. = FALSE)
  }
  has_recipients <- length(recipients) > 0
  has_passphrase <- !is.null(passphrase)
  if (has_recipients == has_passphrase) {
    stop(
      "An export is encrypted either to `recipients` (their public ",
      "encryption keys, the preferred mode) or with a `passphrase`, not ",
      "both and not neither.",
      call. = FALSE
    )
  }
  if (has_passphrase) {
    if (!is.character(passphrase) || length(passphrase) != 1L || is.na(passphrase)) {
      stop("`passphrase` must be a single string.", call. = FALSE)
    }
    if (nchar(passphrase, type = "chars") < episodic_exchange_passphrase_min_chars) {
      stop(
        "The passphrase has ", nchar(passphrase, type = "chars"), " ",
        "characters, and an export needs at least ",
        episodic_exchange_passphrase_min_chars, ": a file kept in a public ",
        "place can be guessed at offline for as long as anyone likes. ",
        "Prefer recipients' public keys where you can.",
        call. = FALSE
      )
    }
  }
  recipient_raw <- if (has_recipients) {
    lapply(seq_along(recipients), function(i) {
      episodic_exchange_key_read(
        recipients[[i]],
        "encryption",
        "public",
        paste0("Recipient key ", i)
      )
    })
  } else {
    NULL
  }
  if (has_recipients) {
    fingerprints <- vapply(recipient_raw, episodic_exchange_fingerprint, character(1))
    if (anyDuplicated(fingerprints)) {
      stop("The same recipient key is listed more than once.", call. = FALSE)
    }
  } else {
    fingerprints <- character(0)
  }
  signing_secret <- episodic_exchange_key_read(
    signing_key,
    "signing",
    "secret",
    "The signing key",
    env = "EPISODIC_EXCHANGE_SIGNING_KEY"
  )
  selection <- episodic_exchange_fields(fields, config)
  pc_n <- episodic_exchange_pc_characters(pc_characters, config)

  cases <- episodic_db_exchange_cases(con, from, to)
  if (!is.null(pc_n) && "pc" %in% selection$fields) {
    cases$pc <- ifelse(is.na(cases$pc), NA_character_, substr(cases$pc, 1L, pc_n))
  }
  cases$sample_date <- as.character(cases$sample_date)
  cases$receipt_date <- as.character(cases$receipt_date)
  cases$age <- as.integer(cases$age)
  cases <- cases[, selection$fields, drop = FALSE]

  types <- episodic_exchange_case_types()[selection$fields]
  manifest <- list(
    sender_instance_id = episodic_db_instance_id(con),
    episodic_version = as.character(utils::packageVersion("EpiSODIC")),
    schema_version = episodic_db_schema_version(con),
    from = format(from, "%Y-%m-%d"),
    to = format(to, "%Y-%m-%d"),
    created_at = episodic_now(),
    fields = I(selection$fields),
    omitted_fields = I(selection$omitted),
    pc_characters = pc_n,
    hashed_columns = I("institution_key"),
    own_cases_only = TRUE
  )
  body <- episodic_exchange_payload_build(manifest, list(cases = cases), list(cases = types))
  bytes <- episodic_exchange_seal(
    body,
    recipients = recipient_raw,
    passphrase = if (has_passphrase) passphrase else NULL,
    signing_secret = signing_secret
  )
  list(
    bytes = bytes,
    file_sha256 = sodium::bin2hex(sodium::sha256(bytes)),
    n_cases = nrow(cases),
    mode = if (has_recipients) "public_key" else "passphrase",
    fields = selection$fields,
    omitted = selection$omitted,
    pc_characters = pc_n,
    recipient_fingerprints = unname(fingerprints),
    signer_fingerprint = episodic_exchange_fingerprint(sodium::sig_pubkey(signing_secret)),
    from = from,
    to = to
  )
}

#' Write bytes to a file without ever leaving half a file behind
#'
#' Written beside the target and renamed into place, so a reader polling
#' the path (an import source on a shared drive) sees the previous file
#' or the new one, never one cut short.
#' @param bytes Raw.
#' @param file The target path.
#' @return Invisibly, `file`.
#' @keywords internal
#' @noRd
episodic_exchange_write_file <- function(bytes, file) {
  dir <- dirname(file)
  if (!dir.exists(dir)) {
    stop("The directory ", dir, " does not exist.", call. = FALSE)
  }
  partial <- tempfile(".episodic-exchange-", tmpdir = dir)
  on.exit(if (file.exists(partial)) unlink(partial), add = TRUE)
  writeBin(bytes, partial)
  if (!file.rename(partial, file)) {
    # A rename across file systems, or onto a file held open elsewhere,
    # can fail where a copy succeeds.
    if (!file.copy(partial, file, overwrite = TRUE)) {
      stop("Could not write ", file, ".", call. = FALSE)
    }
  }
  invisible(file)
}

#' Record an export in the audit trail
#'
#' The one writer of `episodic_exchange_export_event`, called for every
#' export whichever process ran it, and for a failed one as well. It only
#' inserts.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param initiated_by `"headless"`, `"schedule"` or `"download"`.
#' @param status `"written"` or `"failed"`.
#' @param from,to The range asked for.
#' @param built `episodic_exchange_build()`'s result, or `NULL` when the
#'   export failed before a file existed.
#' @param destination Where the file went: a path, a URL, or
#'   `"env:<VARIABLE>"`.
#' @param schedule_name,user_id,run_id Who or what asked for it, where
#'   applicable; `NA` otherwise.
#' @param error_text Why it failed, or `NA`.
#' @return The new `export_id`.
#' @keywords internal
#' @noRd
episodic_db_exchange_export_record <- function(con,
                                               initiated_by,
                                               status,
                                               from,
                                               to,
                                               built = NULL,
                                               destination = NA_character_,
                                               schedule_name = NA_character_,
                                               user_id = NA_integer_,
                                               run_id = NA_integer_,
                                               error_text = NA_character_) {
  json <- function(x) {
    if (is.null(x)) NA_character_ else as.character(jsonlite::toJSON(as.character(x)))
  }
  as_day <- function(x) {
    parsed <- tryCatch(episodic_exchange_date(x, "date"), error = function(e) NULL)
    if (is.null(parsed)) as.character(x)[1] else format(parsed, "%Y-%m-%d")
  }
  episodic_db_execute(
    con,
    "INSERT INTO episodic_exchange_export_event
       (exported_at, initiated_by, schedule_name, user_id, run_id, host,
        account, status, mode, date_from, date_to, n_cases, fields,
        omitted_fields, pc_characters, recipient_fingerprints,
        signer_fingerprint, file_sha256, file_bytes, destination, error_text)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      episodic_now(),
      initiated_by,
      as.character(schedule_name),
      as.integer(user_id),
      as.integer(run_id),
      Sys.info()[["nodename"]],
      Sys.info()[["user"]],
      status,
      if (is.null(built)) NA_character_ else built$mode,
      as_day(from),
      as_day(to),
      if (is.null(built)) NA_integer_ else as.integer(built$n_cases),
      json(built$fields),
      json(built$omitted),
      if (is.null(built$pc_characters)) NA_integer_ else as.integer(built$pc_characters),
      json(built$recipient_fingerprints),
      if (is.null(built)) NA_character_ else built$signer_fingerprint,
      if (is.null(built)) NA_character_ else built$file_sha256,
      if (is.null(built)) NA_integer_ else length(built$bytes),
      as.character(destination),
      as.character(error_text)
    )
  )
  episodic_db_last_insert_id(con)
}

#' Export a Date Range of Cases to an Encrypted Exchange File
#'
#' Writes this instance's own cases with a sample date from `from` to
#' `to` into one encrypted, signed file that another EpiSODIC instance
#' imports with [episodic_exchange_import()]. The file is safe to keep in
#' a public place (a public bucket, a shared drive, a Git repository):
#' without a recipient's key it reveals nothing beyond its own size, and
#' without your signing key nobody can forge or alter it. It works
#' headless, from `Rscript` or a scheduled job, with no dashboard.
#'
#' Two ways to encrypt, and the first is the one to use:
#'
#' \describe{
#'   \item{Public-key mode (`recipients`)}{The file is encrypted to each
#'     recipient's public encryption key, and only their secret keys open
#'     it. Several recipients can share one file. The recipients cannot
#'     forge a file in your name, and replacing one recipient's key
#'     involves nobody else.}
#'   \item{Passphrase mode (`passphrase`)}{The file is encrypted with a
#'     key derived from a passphrase (scrypt, with a fresh random salt per
#'     file). Everyone who holds the passphrase can read every file, so
#'     changing it means coordinating every party at once. At least 16
#'     characters.}
#' }
#'
#' In both modes the file is signed with your signing key, and an
#' importer refuses it unless your public signing key is among the
#' senders they have pinned.
#'
#' **What leaves the instance.** Cases are exported as this instance
#' stores them: one row per episode (see `vignette("exchange")`), with the
#' institution as its stored SHA-1 hash, never the raw key. Cases this
#' instance itself imported from a partner are never exported. Only the
#' optional columns in `fields` are included; the others are left out of
#' the file entirely, the file records which, and an importer receives
#' them as not recorded (`NA`), never as a default value.
#'
#' Every export is recorded in the database (`episodic_exchange_export_event`):
#' when, by which host and account, the range, the columns, the
#' recipients' key fingerprints, the signing key's fingerprint, the number
#' of cases and the SHA-256 of the file. A failed export is recorded too.
#'
#' @param db_path The EpiSODIC database: a SQLite file or a MariaDB/MySQL
#'   DSN (see [episodic_db_dsn_mariadb()]). Defaults to the `EPISODIC_DB`
#'   environment variable.
#' @param from,to The sample-date range, inclusive: `Date`s or
#'   `"YYYY-MM-DD"`.
#' @param file The path to write. An existing file is replaced, in one
#'   step, so a reader never sees half of it.
#' @param recipients For public-key mode: one or more recipients' public
#'   encryption keys, each as its text or as the path of its `.pub` file.
#' @param signing_key Your secret signing key, as its text or as the path
#'   of the file [episodic_exchange_keygen()] wrote. Defaults to the
#'   `EPISODIC_EXCHANGE_SIGNING_KEY` environment variable.
#' @param passphrase For passphrase mode: the passphrase. Give either
#'   `recipients` or `passphrase`, not both.
#' @param fields The optional case columns to include (any of
#'   `receipt_date`, `care_line`, `municipality`, `ward`, `specialism`,
#'   `pc`, `sex`, `age`). `NULL` (the default) uses the configured
#'   `exchange.export_fields`. The required columns are always included.
#' @param pc_characters Keep only the first this many characters of each
#'   postcode. `NULL` (the default) uses the configured
#'   `exchange.pc_characters`, which ships unset: postcodes as stored.
#' @param episodic_config_path The configuration path. Defaults to the
#'   `EPISODIC_CONFIG` environment variable.
#' @return Invisibly, a list with `file`, `file_sha256`, `n_cases`,
#'   `fields`, `omitted_fields` and `export_id` (the audit row).
#' @seealso [episodic_exchange_import()], [episodic_exchange_keygen()],
#'   `vignette("exchange")`
#' @examples
#' \donttest{
#' db_path <- tempfile(fileext = ".sqlite")
#' episodic_run_cron(
#'   cases = episodic_synthetic_cases(
#'     start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#'   ),
#'   db_path = db_path,
#'   run_date = as.Date("2025-03-31")
#' )
#' sender <- episodic_exchange_keygen(tempfile())
#' receiver <- episodic_exchange_keygen(tempfile())
#' file <- tempfile(fileext = ".episodic")
#' episodic_exchange_export(
#'   db_path = db_path,
#'   from = "2025-03-01",
#'   to = "2025-03-31",
#'   file = file,
#'   recipients = receiver$encryption_public,
#'   signing_key = sender$signing_secret
#' )
#' }
#' @export
episodic_exchange_export <- function(db_path = Sys.getenv("EPISODIC_DB", unset = NA),
                                     from,
                                     to,
                                     file,
                                     recipients = NULL,
                                     signing_key = Sys.getenv("EPISODIC_EXCHANGE_SIGNING_KEY", unset = NA),
                                     passphrase = NULL,
                                     fields = NULL,
                                     pc_characters = NULL,
                                     episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a single path.", call. = FALSE)
  }
  config <- episodic_config_resolve(episodic_config_path)
  con <- episodic_db_open(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  outcome <- tryCatch(
    {
      built <- episodic_exchange_build(
        con,
        from = from,
        to = to,
        recipients = recipients,
        passphrase = passphrase,
        signing_key = signing_key,
        fields = fields,
        pc_characters = pc_characters,
        config = config
      )
      episodic_exchange_write_file(built$bytes, file)
      built
    },
    error = function(e) e
  )
  if (inherits(outcome, "error")) {
    recorded <- tryCatch(
      episodic_db_exchange_export_record(
        con,
        initiated_by = "headless",
        status = "failed",
        from = from,
        to = to,
        destination = file,
        error_text = conditionMessage(outcome)
      ),
      error = function(e) e
    )
    if (inherits(recorded, "error")) {
      stop(
        conditionMessage(outcome), " (Recording the failed export failed ",
        "as well: ", conditionMessage(recorded), ")",
        call. = FALSE
      )
    }
    stop(outcome)
  }
  export_id <- episodic_db_exchange_export_record(
    con,
    initiated_by = "headless",
    status = "written",
    from = outcome$from,
    to = outcome$to,
    built = outcome,
    destination = file
  )
  invisible(list(
    file = file,
    file_sha256 = outcome$file_sha256,
    n_cases = outcome$n_cases,
    fields = outcome$fields,
    omitted_fields = outcome$omitted,
    export_id = export_id
  ))
}
