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

# The payload inside an exchange file's envelope (R/exchange_crypto.R).
#
# A plain, documented format, deliberately not R's own serialisation:
# `unserialize()` on bytes from another party is a code-execution surface
# whatever signature checked them, and an RDS file is unreadable outside
# R. The payload is one UTF-8 JSON document, compressed as a zlib stream
# (`memCompress(type = "gzip")`) before it is encrypted. Its tables are
# carried as JSON text of their own, so the SHA-256 the manifest records
# for each is a hash of exact bytes rather than of a structure two JSON
# writers could print differently. `vignette("exchange")` documents every
# field.

#' The payload format version this build writes and reads
#' @keywords internal
#' @noRd
episodic_exchange_payload_version <- 1L

#' The columns of the `cases` table in payload version 1, and their types
#'
#' Exactly the case data columns (`episodic_case_columns`), so an import
#' hands `episodic_run_cron()` the shape any other feed has. `age` is the
#' one integer column; every other is text, dates as `YYYY-MM-DD`.
#' @keywords internal
#' @noRd
episodic_exchange_case_types <- function() {
  types <- stats::setNames(
    rep("character", length(episodic_case_columns)),
    episodic_case_columns
  )
  types[["age"]] <- "integer"
  types
}

#' The case columns an export may leave out
#'
#' Data minimisation per export: any column the case data format allows
#' to be `NA` (`episodic_case_columns_required` lists the others). A
#' required column cannot be left out, because a case without it is not a
#' case the receiving run can load.
#' @keywords internal
#' @noRd
episodic_exchange_omittable_fields <- function() {
  setdiff(episodic_case_columns, episodic_case_columns_required)
}

#' One table as the JSON text a payload carries
#'
#' An object of equal-length arrays, one per column, with `null` for a
#' value that is not there. Never `""` or `0` for one: absence of a value
#' is not a value.
#' @param df A data frame.
#' @param types A named character vector, column to `"character"` or
#'   `"integer"`.
#' @return A single string.
#' @keywords internal
#' @noRd
episodic_exchange_table_json <- function(df, types) {
  columns <- lapply(stats::setNames(names(types), names(types)), function(column) {
    values <- df[[column]]
    if (identical(types[[column]], "integer")) {
      as.integer(values)
    } else {
      as.character(values)
    }
  })
  as.character(jsonlite::toJSON(
    columns,
    auto_unbox = FALSE,
    na = "null",
    null = "null",
    digits = NA
  ))
}

#' SHA-256 of a string's UTF-8 bytes, in hexadecimal
#' @keywords internal
#' @noRd
episodic_exchange_sha256_text <- function(text) {
  sodium::bin2hex(sodium::sha256(charToRaw(enc2utf8(text))))
}

#' Build a compressed payload
#'
#' @param manifest A named list of the manifest fields (see
#'   `vignette("exchange")`), without `tables`. A field that is a list of
#'   values is wrapped in `I()`, so a list of one is still written as an
#'   array.
#' @param tables A named list of data frames.
#' @param types A named list, per table, of column types.
#' @return The compressed payload bytes.
#' @keywords internal
#' @noRd
episodic_exchange_payload_build <- function(manifest, tables, types) {
  table_entries <- lapply(stats::setNames(names(tables), names(tables)), function(name) {
    text <- episodic_exchange_table_json(tables[[name]], types[[name]])
    list(
      rows = nrow(tables[[name]]),
      columns = as.list(types[[name]]),
      sha256 = episodic_exchange_sha256_text(text),
      data = text
    )
  })
  document <- c(
    list(
      payload_format = "episodic-exchange-payload",
      payload_version = episodic_exchange_payload_version
    ),
    manifest,
    list(tables = table_entries)
  )
  json <- as.character(jsonlite::toJSON(
    document,
    auto_unbox = TRUE,
    na = "null",
    null = "null",
    digits = NA
  ))
  memCompress(charToRaw(enc2utf8(json)), type = "gzip")
}

#' Read a payload back, verifying every table before converting any
#'
#' @param body The compressed payload bytes, as `episodic_exchange_open()`
#'   returns them.
#' @param types A named list, per table this build reads, of the column
#'   types it accepts.
#' @param required A named list, per table, of the columns it must carry;
#'   any other accepted column may be absent, which is how an export
#'   leaves a field out.
#' @return A list with `manifest` (everything but the tables) and
#'   `tables` (a named list of data frames, each holding exactly the
#'   columns its file carried).
#' @keywords internal
#' @noRd
episodic_exchange_payload_read <- function(body, types, required = list()) {
  json <- tryCatch(memDecompress(body, type = "gzip"), error = function(e) NULL)
  if (is.null(json)) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file's payload does not decompress, although its ",
      "signature verified: the sender's EpiSODIC wrote something this one ",
      "cannot read."
    )
  }
  text <- rawToChar(json)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    episodic_exchange_fail("bad_payload", "This exchange file's payload is not valid UTF-8.")
  }
  document <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(document) || !identical(document$payload_format, "episodic-exchange-payload")) {
    episodic_exchange_fail("bad_payload", "This exchange file's payload is not an EpiSODIC exchange payload.")
  }
  version <- suppressWarnings(as.integer(document$payload_version))
  if (length(version) != 1L || is.na(version) || version < 1L) {
    episodic_exchange_fail("bad_payload", "This exchange file's payload names no valid payload version.")
  }
  if (version > episodic_exchange_payload_version) {
    episodic_exchange_fail(
      "unsupported_version",
      "This exchange file's payload is version ", version, ", written by a ",
      "newer EpiSODIC; this one reads payload version ",
      episodic_exchange_payload_version, " and older. Upgrade EpiSODIC to ",
      "import it."
    )
  }
  if (!is.list(document$tables)) {
    episodic_exchange_fail("bad_payload", "This exchange file's payload carries no tables.")
  }
  unknown <- setdiff(names(document$tables), names(types))
  if (length(unknown) > 0) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file carries table(s) this EpiSODIC does not read: ",
      paste(unknown, collapse = ", "), "."
    )
  }
  missing <- setdiff(names(types), names(document$tables))
  if (length(missing) > 0) {
    episodic_exchange_fail(
      "bad_payload",
      "This exchange file lacks the table(s) ", paste(missing, collapse = ", "), "."
    )
  }

  # Every table's hash is checked before any table is converted, so a
  # file with one damaged table is refused whole rather than read in part.
  for (name in names(types)) {
    entry <- document$tables[[name]]
    data <- entry$data
    if (!is.character(data) || length(data) != 1L) {
      episodic_exchange_fail("bad_payload", "Table `", name, "` in this exchange file carries no data.")
    }
    if (!identical(entry$sha256, episodic_exchange_sha256_text(data))) {
      episodic_exchange_fail(
        "hash_mismatch",
        "Table `", name, "` in this exchange file does not match the SHA-256 ",
        "its manifest records. Nothing in the file was read."
      )
    }
  }

  tables <- lapply(stats::setNames(names(types), names(types)), function(name) {
    entry <- document$tables[[name]]
    declared <- unlist(entry$columns)
    if (!is.character(declared) || is.null(names(declared)) || anyDuplicated(names(declared))) {
      episodic_exchange_fail("bad_payload", "Table `", name, "` in this exchange file declares no columns.")
    }
    unknown_columns <- setdiff(names(declared), names(types[[name]]))
    missing_columns <- setdiff(required[[name]], names(declared))
    expected <- types[[name]][intersect(names(types[[name]]), names(declared))]
    if (
      length(unknown_columns) > 0 ||
        length(missing_columns) > 0 ||
        !identical(declared[names(expected)], expected)
    ) {
      episodic_exchange_fail(
        "bad_payload",
        "Table `", name, "` in this exchange file does not have the columns ",
        "and types payload version ", version, " defines",
        if (length(unknown_columns) > 0) {
          paste0(" (unknown: ", paste(unknown_columns, collapse = ", "), ")")
        } else {
          ""
        },
        if (length(missing_columns) > 0) {
          paste0(" (missing: ", paste(missing_columns, collapse = ", "), ")")
        } else {
          ""
        },
        "."
      )
    }
    columns <- tryCatch(
      jsonlite::fromJSON(entry$data, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.list(columns) || !setequal(names(columns), names(expected))) {
      episodic_exchange_fail("bad_payload", "Table `", name, "` in this exchange file is malformed.")
    }
    rows <- suppressWarnings(as.integer(entry$rows))
    if (length(rows) != 1L || is.na(rows) || rows < 0L) {
      episodic_exchange_fail("bad_payload", "Table `", name, "` in this exchange file names no row count.")
    }
    out <- lapply(stats::setNames(names(expected), names(expected)), function(column) {
      values <- columns[[column]]
      if (length(values) != rows) {
        episodic_exchange_fail(
          "bad_payload",
          "Column `", column, "` of table `", name, "` holds ", length(values),
          " values for ", rows, " rows."
        )
      }
      episodic_exchange_column_values(values, expected[[column]], column, name)
    })
    df <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
    names(df) <- names(expected)
    df
  })

  document$tables <- NULL
  list(manifest = document, tables = tables)
}

#' One payload column as an R vector of its declared type
#'
#' A JSON `null` becomes `NA`. A value of the wrong type is refused: a
#' signed file from a trusted sender is still data, and a text where a
#' whole number belongs is not something to coerce quietly.
#' @keywords internal
#' @noRd
episodic_exchange_column_values <- function(values, type, column, table) {
  bad <- function() {
    episodic_exchange_fail(
      "bad_payload",
      "Column `", column, "` of table `", table, "` holds a value that is ",
      "not ", if (identical(type, "integer")) "a whole number" else "text", "."
    )
  }
  if (identical(type, "integer")) {
    vapply(values, function(v) {
      if (is.null(v)) {
        return(NA_integer_)
      }
      if (!is.numeric(v) || length(v) != 1L || v != round(v) || abs(v) > .Machine$integer.max) {
        bad()
      }
      as.integer(v)
    }, integer(1), USE.NAMES = FALSE)
  } else {
    vapply(values, function(v) {
      if (is.null(v)) {
        return(NA_character_)
      }
      if (!is.character(v) || length(v) != 1L) {
        bad()
      }
      v
    }, character(1), USE.NAMES = FALSE)
  }
}
