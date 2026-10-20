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

# Importing an exchange file as case data.
#
# An import never writes to the database. The case tables are cron-owned,
# so imported cases reach them the way every other case does: through
# episodic_run_cron(), as its `imports`, with the same validation,
# deduplication and audit trail. What this file adds is what only an
# import knows - whose cases these are, so that their keys can never be
# taken for this instance's own, and that their institution key is
# already the stored hash.

#' Read an exchange file's bytes
#' @param file A path, or the file's bytes as a raw vector.
#' @return A raw vector.
#' @keywords internal
#' @noRd
episodic_exchange_read_bytes <- function(file) {
  if (is.raw(file)) {
    return(file)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a single path, or the file's bytes as a raw vector.", call. = FALSE)
  }
  if (!file.exists(file)) {
    episodic_exchange_fail("no_file", "No exchange file exists at ", file, ".")
  }
  size <- file.info(file)$size
  readBin(file, what = "raw", n = size)
}

#' Prefix a sender's keys with its instance identifier
#'
#' `source_key` is unique only within one laboratory, and `patient_key`
#' and `lab_number` are one laboratory's own pseudonyms and numbers, so a
#' partner's `"12345"` and this instance's `"12345"` are different results,
#' patients and cultures. Unprefixed, the case loader would skip the
#' partner's result as already present, and episode deduplication would
#' merge two people into one patient and drop the second's cases: both a
#' silent loss. `:` cannot occur in an instance identifier, so the prefix
#' can always be split off again.
#' @param cases A case data frame.
#' @param instance_id The sender's instance identifier.
#' @return `cases` with the three columns prefixed.
#' @keywords internal
#' @noRd
episodic_exchange_prefix_keys <- function(cases, instance_id) {
  for (column in c("source_key", "patient_key", "lab_number")) {
    cases[[column]] <- paste0(instance_id, ":", cases[[column]])
  }
  cases
}

#' Where imported cases cannot be placed, per lattice level
#'
#' A run keeps a case it cannot place at a level: it simply joins no
#' stream there, and counts everywhere else. A local case whose postcode
#' is missing from `EPISODIC_PC_PROVINCE_MAP` is treated so, and an
#' imported one is treated the same way. What an import adds is the
#' warning: a partner's postcodes are the likeliest to fall outside this
#' instance's own reference data, and whether to expand that data is the
#' operator's call, which they can only make if they are told.
#'
#' @param cases The imported cases.
#' @param config A resolved configuration.
#' @param pc_characters The sender's postcode truncation, or `NULL`.
#' @param province_map_path The PC-to-province mapping path.
#' @return A character vector of warnings, one per level with cases it
#'   cannot place; empty when every case can be placed everywhere.
#' @keywords internal
#' @noRd
episodic_exchange_geography_warnings <- function(cases,
                                                 config,
                                                 pc_characters,
                                                 province_map_path = Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)) {
  n <- nrow(cases)
  if (n == 0) {
    return(character(0))
  }
  geography <- episodic_geography_config(config)
  warnings <- character(0)
  no_pc <- is.na(cases$pc)
  if (any(no_pc)) {
    warnings <- c(warnings, paste0(
      sum(no_pc), " of ", n, " imported case(s) carry no postcode, so they ",
      "are not placed at area (L3) or province (L4) level; they still count ",
      "at ward, institution and region level."
    ))
  }
  if (!is.null(pc_characters) && pc_characters < geography$area_pc_characters) {
    warnings <- c(warnings, paste0(
      "The sender cut postcodes to ", pc_characters, " character(s) and this ",
      "instance's areas use ", geography$area_pc_characters, ", so the ",
      sum(!no_pc), " imported case(s) with a postcode fall into area (L3) ",
      "streams of their own rather than into this instance's areas."
    ))
  }
  with_pc <- cases$pc[!no_pc]
  if (length(with_pc) > 0) {
    province <- episodic_pc_to_province(with_pc, path = province_map_path)
    unplaced <- sum(is.na(province))
    if (unplaced > 0) {
      configured <- !(length(province_map_path) != 1 || is.na(province_map_path) || !nzchar(province_map_path))
      warnings <- c(warnings, if (configured) {
        paste0(
          unplaced, " of ", length(with_pc), " imported case(s) with a ",
          "postcode have one EPISODIC_PC_PROVINCE_MAP does not list, so they ",
          "are not placed at province (L4) level. Add the partner's ",
          "postcodes to the mapping if they belong in your provinces."
        )
      } else {
        paste0(
          "No EPISODIC_PC_PROVINCE_MAP is configured, so none of the ",
          length(with_pc), " imported case(s) with a postcode is placed at ",
          "province (L4) level, exactly as for this instance's own cases."
        )
      })
    }
  }
  warnings
}

#' Import an Encrypted Exchange File as Case Data
#'
#' Opens a file written by [episodic_exchange_export()] at another
#' instance, verifies it in full, and returns its cases as a validated
#' data frame in the case data format. It does **not** write to the
#' database: hand the result to [episodic_run_cron()] as `imports`, and
#' the cases are loaded, deduplicated, detected on and recorded like
#' every other case, with the import itself recorded against that run.
#'
#' ```r
#' partner <- episodic_exchange_import(
#'   "partner.episodic",
#'   trusted_senders = "keys/partner_signing.pub"
#' )
#' episodic_run_cron(cases = my_cases, imports = partner)
#' ```
#'
#' Nothing in the file is returned unless all of it holds: it must open
#' with your key (or the passphrase), its signature must verify against
#' one of `trusted_senders`, and every table must match the SHA-256 its
#' manifest records. A wrong key, an untrusted or forged sender, a
#' tampered or truncated file, a format version this EpiSODIC does not
#' read, and a hash mismatch each stop with an error that says which; a
#' partially readable file is never partially imported. The error carries
#' class `episodic_exchange_error` and a `reason` code.
#'
#' What is returned:
#'
#' \itemize{
#'   \item `source_key`, `patient_key` and `lab_number` are prefixed with
#'     the sender's instance identifier and a colon, so the partner's keys
#'     can never be taken for yours.
#'   \item `institution_key` is the stored hash the sender's database
#'     holds; the file declares it so, and the run uses it as it is.
#'   \item A column the sender left out is `NA` on every row, which the
#'     case data format reads as "not recorded", never a default value.
#'   \item Cases whose postcode this instance's reference data cannot
#'     place at some level are kept, with a warning per level.
#' }
#'
#' The data frame carries class `episodic_exchange_cases` and an
#' `"exchange"` attribute describing the file (sender, signer
#' fingerprint, range, omitted columns, the file's SHA-256, the
#' warnings), which the run records.
#'
#' Imports are append-only: a case the sender later corrects or retracts
#' stays as it was first imported. See `vignette("exchange")` for this
#' and the other limits (patient linkage across laboratories, pathogen
#' naming, history imported after the first run).
#'
#' @param file The exchange file: a path, or its bytes as a raw vector.
#' @param key Your secret encryption key, as its text or as the path of
#'   the file [episodic_exchange_keygen()] wrote. Defaults to the
#'   `EPISODIC_EXCHANGE_KEY` environment variable. Not used for a
#'   passphrase file.
#' @param trusted_senders The public signing keys of the partners whose
#'   files you accept, each as its text or as the path of its `.pub`
#'   file. A file signed by any other key is refused.
#' @param passphrase For a file written in passphrase mode: the
#'   passphrase. Leave `NULL` for a file encrypted to your key.
#' @param episodic_config_path The configuration path, for the geography
#'   the warnings are measured against. Defaults to the `EPISODIC_CONFIG`
#'   environment variable.
#' @return A data frame of class `episodic_exchange_cases`, with exactly
#'   the case data columns, and the `"exchange"` attribute.
#' @seealso [episodic_exchange_export()], [episodic_run_cron()],
#'   `vignette("exchange")`
#' @examples
#' \dontrun{
#' partner <- episodic_exchange_import(
#'   "partner.episodic",
#'   trusted_senders = "keys/partner_signing.pub"
#' )
#' attr(partner, "exchange")$omitted_fields
#' episodic_run_cron(cases = my_cases, imports = partner)
#' }
#' @export
episodic_exchange_import <- function(file,
                                     key = Sys.getenv("EPISODIC_EXCHANGE_KEY", unset = NA),
                                     trusted_senders,
                                     passphrase = NULL,
                                     episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  if (missing(trusted_senders) || length(trusted_senders) == 0) {
    episodic_exchange_fail(
      "untrusted_sender",
      "`trusted_senders` is empty, so no file can be accepted: name the ",
      "public signing key of every partner whose files you import."
    )
  }
  trusted <- lapply(seq_along(trusted_senders), function(i) {
    episodic_exchange_key_read(
      trusted_senders[[i]],
      "signing",
      "public",
      paste0("Trusted sender key ", i)
    )
  })
  secret_key <- if (is.null(passphrase)) {
    episodic_exchange_key_read(
      key,
      "encryption",
      "secret",
      "The secret encryption key",
      env = "EPISODIC_EXCHANGE_KEY"
    )
  } else {
    NULL
  }
  config <- episodic_config_resolve(episodic_config_path)
  bytes <- episodic_exchange_read_bytes(file)
  opened <- episodic_exchange_open(
    bytes,
    secret_key = secret_key,
    passphrase = passphrase,
    trusted_senders = trusted
  )
  payload <- episodic_exchange_payload_read(
    opened$body,
    types = list(cases = episodic_exchange_case_types()),
    required = list(cases = episodic_case_columns_required)
  )
  manifest <- payload$manifest
  cases <- payload$tables$cases

  sender <- manifest$sender_instance_id
  if (!episodic_instance_id_valid(sender)) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file names no valid sender instance identifier, so ",
      "its keys cannot be told apart from anyone else's."
    )
  }
  hashed <- unlist(manifest$hashed_columns)
  if (!"institution_key" %in% hashed) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file does not declare its institution_key hashed. ",
      "Hashing it here would produce a key that matches no institution, ",
      "and using it unhashed would put a raw identifier in the database."
    )
  }
  if (!all(grepl("^[0-9a-f]{40}$", cases$institution_key))) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file declares its institution_key hashed, but not ",
      "every value is a SHA-1 hash."
    )
  }
  carried <- names(cases)
  omitted <- unlist(manifest$omitted_fields) %||% character(0)
  absent <- setdiff(episodic_case_columns, carried)
  if (!setequal(absent, omitted)) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file's manifest says it left out ",
      if (length(omitted)) paste(omitted, collapse = ", ") else "nothing",
      ", but its case table lacks ",
      if (length(absent)) paste(absent, collapse = ", ") else "nothing",
      "."
    )
  }
  # A column the sender left out is not recorded here either: NA of the
  # column's own type, which the case data format reads as "not
  # recorded", and nothing that could be read as a value.
  for (column in absent) {
    cases[[column]] <- if (identical(episodic_exchange_case_types()[[column]], "integer")) {
      rep(NA_integer_, nrow(cases))
    } else {
      rep(NA_character_, nrow(cases))
    }
  }
  cases <- cases[, episodic_case_columns, drop = FALSE]
  cases <- episodic_exchange_prefix_keys(cases, sender)

  # The same checks a run applies to any case data, so a file a run would
  # refuse is refused here, where the operator is looking.
  report <- episodic_check_cases(cases, stop_on_problem = TRUE)

  pc_characters <- manifest$pc_characters
  warnings <- episodic_exchange_geography_warnings(
    cases,
    config,
    pc_characters = if (is.null(pc_characters)) NULL else as.integer(pc_characters)
  )
  for (w in warnings) {
    warning(w, call. = FALSE)
  }

  structure(
    cases,
    class = c("episodic_exchange_cases", "data.frame"),
    exchange = list(
      sender_instance_id = sender,
      signer_fingerprint = opened$signer_fingerprint,
      mode = opened$mode,
      format_version = opened$format_version,
      payload_version = as.integer(manifest$payload_version),
      sender_episodic_version = as.character(manifest$episodic_version %||% NA_character_),
      sender_schema_version = as.integer(manifest$schema_version %||% NA_integer_),
      from = as.character(manifest$from %||% NA_character_),
      to = as.character(manifest$to %||% NA_character_),
      created_at = as.character(manifest$created_at %||% NA_character_),
      n_cases = nrow(cases),
      fields = carried,
      omitted_fields = omitted,
      pc_characters = if (is.null(pc_characters)) NULL else as.integer(pc_characters),
      file_sha256 = sodium::bin2hex(sodium::sha256(bytes)),
      file_bytes = length(bytes),
      advice = sum(report$severity == "advice"),
      warnings = warnings
    )
  )
}

#' Print an imported exchange file's cases
#'
#' Says whose cases they are, for which range, and what the sender left
#' out, before the first rows: the provenance is the part of an import a
#' bare data frame print would hide.
#' @param x An `episodic_exchange_cases` data frame.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
#' @export
print.episodic_exchange_cases <- function(x, ...) {
  meta <- attr(x, "exchange")
  cat(
    "<episodic_exchange_cases> ", nrow(x), " case(s) from instance ",
    meta$sender_instance_id, " (", meta$from, " to ", meta$to, ")\n",
    sep = ""
  )
  if (length(meta$omitted_fields) > 0) {
    cat("Left out by the sender: ", paste(meta$omitted_fields, collapse = ", "), "\n", sep = "")
  }
  plain <- x
  class(plain) <- "data.frame"
  attr(plain, "exchange") <- NULL
  print(utils::head(plain))
  invisible(x)
}
