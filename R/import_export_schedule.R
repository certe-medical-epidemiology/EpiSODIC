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

# Export schedules and import sources: registered on the dashboard by an
# admin, carried out by episodic_run_cron().
#
# Both are app-owned and event-sourced (episodic_export_schedule_event,
# episodic_import_source_event), the pattern scheduled reports follow:
# the current schedule or source under a name is the latest row for it,
# and a "cancel" row ends it. Neither stores a secret; every key,
# passphrase and credential is the name of an environment variable,
# read by the process that uses it.
#
# A run fetches every registered source before detection, so the cases
# enter in the same transaction as the operator's own, and a source that
# fails marks the run partial with its reason rather than being skipped
# in silence. A run carries out every export that is due after its
# detection transaction has committed, and an export that fails is
# recorded and logged and never fails the run.

#' Refuse a schedule or source name that cannot be told apart or shown
#' @param name The name.
#' @return The name, trimmed.
#' @keywords internal
#' @noRd
episodic_import_export_name_check <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("A name is needed.", call. = FALSE)
  }
  name <- trimws(name)
  if (!nzchar(name) || nchar(name) > 80L || grepl("[[:cntrl:]]", name)) {
    stop("A name is one line of 1 to 80 characters.", call. = FALSE)
  }
  name
}

#' Read an optional value from a form or a call: `NA` when blank
#' @keywords internal
#' @noRd
episodic_import_export_blank_na <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) {
    NA_character_
  } else {
    trimws(as.character(x[1]))
  }
}

#' Refuse a missing or malformed environment variable name
#' @keywords internal
#' @noRd
episodic_import_export_env_required <- function(x, what) {
  x <- episodic_import_export_blank_na(x)
  if (is.na(x)) {
    stop(what, " is read from an environment variable; name it.", call. = FALSE)
  }
  if (!episodic_import_export_env_name_valid(x)) {
    stop("\"", x, "\" is not an environment variable name.", call. = FALSE)
  }
  x
}

#' A public key as the one line it is stored as, refusing any other kind
#' @param text A key text, possibly pasted with the comment lines of its
#'   `.pub` file.
#' @param kind `"encryption"` or `"signing"`.
#' @param what How the error names it.
#' @return The key line.
#' @keywords internal
#' @noRd
episodic_import_export_public_key_line <- function(text, kind, what) {
  lines <- trimws(unlist(strsplit(as.character(text), "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) != 1L) {
    stop(what, " must be one key: the line of its .pub file that starts with \"episodic-import-export-\".", call. = FALSE)
  }
  episodic_import_export_key_parse(lines, kind, "public", what)
  lines
}

#' Register or change an export schedule
#'
#' Validates everything a run will need before anything is written: a
#' schedule a run cannot carry out is refused here, where the admin is,
#' rather than failing every night in a log. The window must be at least
#' as long as the interval, so consecutive exports overlap rather than
#' leave days nobody exported; the overlap is harmless, because an import
#' takes a result in once however many files carry it.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user_id The admin account recording it.
#' @param name The schedule's name; setting an existing name changes it.
#' @param interval_days Export every this many days.
#' @param window_days Each export covers the sample dates of the last
#'   this many days up to the run date.
#' @param mode `"public_key"` or `"passphrase"`.
#' @param recipients For public-key mode, the recipients' public
#'   encryption keys as texts.
#' @param passphrase_env For passphrase mode, the variable holding the
#'   passphrase.
#' @param signing_key_env The variable holding the secret signing key (its
#'   text or its file's path).
#' @param fields The optional columns to export, or `NULL` for the
#'   configured `export.fields`.
#' @param pc_characters Postcode truncation, or `NA` for the configured.
#' @param destination_type,destination,destination_env,username_env,password_env,token_env
#'   Where the file goes; see `episodic_import_export_endpoint()`.
#' @param config A resolved configuration, to validate `fields` against.
#' @return The new `event_id`.
#' @keywords internal
#' @noRd
episodic_export_schedule_set <- function(con,
                                         user_id,
                                         name,
                                         interval_days,
                                         window_days,
                                         mode,
                                         recipients = character(0),
                                         passphrase_env = NA_character_,
                                         signing_key_env,
                                         fields = NULL,
                                         pc_characters = NA_integer_,
                                         destination_type,
                                         destination = NA_character_,
                                         destination_env = NA_character_,
                                         username_env = NA_character_,
                                         password_env = NA_character_,
                                         token_env = NA_character_,
                                         config = episodic_config_resolve()) {
  name <- episodic_import_export_name_check(name)
  whole <- function(x, what) {
    number <- suppressWarnings(as.numeric(x))
    if (length(number) != 1L || is.na(number) || number < 1 || number != round(number)) {
      stop(what, " must be a whole number of at least 1.", call. = FALSE)
    }
    as.integer(number)
  }
  interval_days <- whole(interval_days, "The interval in days")
  window_days <- whole(window_days, "The window in days")
  if (window_days < interval_days) {
    stop(
      "The window (", window_days, " days) is shorter than the interval (",
      interval_days, " days), so cases sampled between two exports would ",
      "never be sent. Make the window at least as long as the interval; ",
      "longer is better, since an overlap is taken in once.",
      call. = FALSE
    )
  }
  mode <- match.arg(mode, c("public_key", "passphrase"))
  recipients_json <- NA_character_
  if (identical(mode, "public_key")) {
    recipients <- recipients[!is.na(recipients) & nzchar(trimws(recipients))]
    if (length(recipients) == 0) {
      stop("Public-key mode needs at least one recipient's public encryption key.", call. = FALSE)
    }
    lines <- vapply(seq_along(recipients), function(i) {
      episodic_import_export_public_key_line(recipients[[i]], "encryption", paste0("Recipient key ", i))
    }, character(1))
    if (anyDuplicated(lines)) {
      stop("The same recipient key is listed more than once.", call. = FALSE)
    }
    recipients_json <- as.character(jsonlite::toJSON(lines))
    passphrase_env <- NA_character_
  } else {
    passphrase_env <- episodic_import_export_env_required(passphrase_env, "The passphrase")
  }
  signing_key_env <- episodic_import_export_env_required(signing_key_env, "The signing key")
  fields_json <- NA_character_
  if (!is.null(fields)) {
    selection <- episodic_export_fields(fields, config)
    fields_json <- as.character(jsonlite::toJSON(
      setdiff(selection$fields, episodic_case_columns_required)
    ))
  }
  pc <- episodic_import_export_blank_na(pc_characters)
  if (!is.na(pc)) {
    pc <- episodic_export_pc_characters(pc, config)
  }
  endpoint <- episodic_import_export_endpoint(
    destination_type,
    location = destination,
    location_env = destination_env,
    username_env = username_env,
    password_env = password_env,
    token_env = token_env,
    direction = "export"
  )
  episodic_db_export_schedule_event_insert(
    con,
    schedule_name = name,
    user_id = user_id,
    action = "set",
    values = list(
      interval_days = interval_days,
      window_days = window_days,
      mode = mode,
      recipients = recipients_json,
      passphrase_env = passphrase_env,
      signing_key_env = signing_key_env,
      fields = fields_json,
      pc_characters = if (is.na(pc)) NA_integer_ else as.integer(pc),
      destination_type = endpoint$type,
      destination = endpoint$location,
      destination_env = endpoint$location_env,
      username_env = endpoint$username_env,
      password_env = endpoint$password_env,
      token_env = endpoint$token_env
    )
  )
}

#' Register or change an import source
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user_id The admin account recording it.
#' @param name The source's name; setting an existing name changes it.
#' @param mode `"public_key"` or `"passphrase"`.
#' @param key_env For public-key mode, the variable holding this
#'   instance's secret encryption key (its text or its file's path).
#' @param passphrase_env For passphrase mode, the variable holding the
#'   passphrase.
#' @param trusted_sender The partner's public signing key, as its text.
#' @param location_type,location,location_env,username_env,password_env,token_env
#'   Where the file comes from; see `episodic_import_export_endpoint()`.
#' @return The new `event_id`.
#' @keywords internal
#' @noRd
episodic_import_source_set <- function(con,
                                       user_id,
                                       name,
                                       mode,
                                       key_env = NA_character_,
                                       passphrase_env = NA_character_,
                                       trusted_sender,
                                       location_type,
                                       location = NA_character_,
                                       location_env = NA_character_,
                                       username_env = NA_character_,
                                       password_env = NA_character_,
                                       token_env = NA_character_) {
  name <- episodic_import_export_name_check(name)
  mode <- match.arg(mode, c("public_key", "passphrase"))
  if (identical(mode, "public_key")) {
    key_env <- episodic_import_export_env_required(key_env, "The secret encryption key")
    passphrase_env <- NA_character_
  } else {
    passphrase_env <- episodic_import_export_env_required(passphrase_env, "The passphrase")
    key_env <- NA_character_
  }
  trusted_line <- episodic_import_export_public_key_line(trusted_sender, "signing", "The sender's public signing key")
  endpoint <- episodic_import_export_endpoint(
    location_type,
    location = location,
    location_env = location_env,
    username_env = username_env,
    password_env = password_env,
    token_env = token_env,
    direction = "import"
  )
  episodic_db_import_source_event_insert(
    con,
    source_name = name,
    user_id = user_id,
    action = "set",
    values = list(
      mode = mode,
      key_env = key_env,
      passphrase_env = passphrase_env,
      trusted_sender = trusted_line,
      location_type = endpoint$type,
      location = endpoint$location,
      location_env = endpoint$location_env,
      username_env = endpoint$username_env,
      password_env = endpoint$password_env,
      token_env = endpoint$token_env
    )
  )
}

#' End an export schedule or an import source
#' @param con A [DBI::DBIConnection-class].
#' @param user_id The admin account recording it.
#' @param name The name.
#' @param kind `"schedule"` or `"source"`.
#' @return The new `event_id`.
#' @keywords internal
#' @noRd
episodic_import_export_cancel <- function(con, user_id, name, kind = c("schedule", "source")) {
  kind <- match.arg(kind)
  current <- if (identical(kind, "schedule")) {
    episodic_export_schedules_current(con)
  } else {
    episodic_import_sources_current(con)
  }
  if (is.null(current[[name]])) {
    stop("There is no current ", kind, " named \"", name, "\".", call. = FALSE)
  }
  if (identical(kind, "schedule")) {
    episodic_db_export_schedule_event_insert(con, name, user_id, "cancel")
  } else {
    episodic_db_import_source_event_insert(con, name, user_id, "cancel")
  }
}

#' Every schedule or source event, oldest first
#' @keywords internal
#' @noRd
episodic_db_export_schedule_events <- function(con) {
  episodic_db_get_query(
    con,
    "SELECT * FROM episodic_export_schedule_event ORDER BY created_at, event_id"
  )
}

#' @keywords internal
#' @noRd
episodic_db_import_source_events <- function(con) {
  episodic_db_get_query(
    con,
    "SELECT * FROM episodic_import_source_event ORDER BY created_at, event_id"
  )
}

#' The latest event per name, kept where it is a "set"
#' @param events A data frame of events, oldest first.
#' @param name_col The name column.
#' @return A named list of one-row lists, keyed by name.
#' @keywords internal
#' @noRd
episodic_import_export_current <- function(events, name_col) {
  if (nrow(events) == 0) {
    return(list())
  }
  latest <- events[!duplicated(events[[name_col]], fromLast = TRUE), , drop = FALSE]
  latest <- latest[latest$action == "set", , drop = FALSE]
  if (nrow(latest) == 0) {
    return(list())
  }
  out <- lapply(seq_len(nrow(latest)), function(i) as.list(latest[i, , drop = FALSE]))
  stats::setNames(out, latest[[name_col]])
}

#' The current export schedules, keyed by name
#' @keywords internal
#' @noRd
episodic_export_schedules_current <- function(con) {
  episodic_import_export_current(episodic_db_export_schedule_events(con), "schedule_name")
}

#' The current import sources, keyed by name
#' @keywords internal
#' @noRd
episodic_import_sources_current <- function(con) {
  episodic_import_export_current(episodic_db_import_source_events(con), "source_name")
}

#' The endpoint a schedule or source names
#' @keywords internal
#' @noRd
episodic_export_schedule_endpoint <- function(schedule) {
  episodic_import_export_endpoint(
    schedule$destination_type,
    location = schedule$destination,
    location_env = schedule$destination_env,
    username_env = schedule$username_env,
    password_env = schedule$password_env,
    token_env = schedule$token_env,
    direction = "export"
  )
}

#' @keywords internal
#' @noRd
episodic_import_source_endpoint <- function(source) {
  episodic_import_export_endpoint(
    source$location_type,
    location = source$location,
    location_env = source$location_env,
    username_env = source$username_env,
    password_env = source$password_env,
    token_env = source$token_env,
    direction = "import"
  )
}

#' Whether an export schedule is due on a run date
#'
#' Due the first time under a schedule's current settings (no file
#' written against its `event_id` yet), or `interval_days` after the last
#' file written under them. A failed export does not push the next try
#' back, so a server that was down costs one run, not a full interval.
#' Pure, taking already-read rows, the way
#' `episodic_report_subscription_due()` is.
#' @param schedule One entry of `episodic_export_schedules_current()`.
#' @param exports `episodic_export_event` rows.
#' @param run_date The run's date.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_export_schedule_due <- function(schedule, exports, run_date) {
  written <- exports[
    !is.na(exports$schedule_event_id) &
      exports$schedule_event_id == schedule$event_id &
      exports$status == "written", ,
    drop = FALSE
  ]
  if (nrow(written) == 0) {
    return(TRUE)
  }
  last <- max(as.Date(substr(written$exported_at, 1, 10)))
  as.numeric(as.Date(run_date) - last) >= as.integer(schedule$interval_days)
}

#' What a schedule hands `episodic_import_export_build()`, read from the environment now
#' @keywords internal
#' @noRd
episodic_export_schedule_inputs <- function(schedule) {
  list(
    recipients = if (identical(schedule$mode, "public_key")) {
      as.character(jsonlite::fromJSON(schedule$recipients))
    } else {
      NULL
    },
    passphrase = if (identical(schedule$mode, "passphrase")) {
      episodic_import_export_env_value(schedule$passphrase_env, "The passphrase")
    } else {
      NULL
    },
    signing_key = episodic_import_export_env_value(schedule$signing_key_env, "The signing key"),
    fields = if (is.na(schedule$fields)) NULL else as.character(jsonlite::fromJSON(schedule$fields)),
    pc_characters = if (is.na(schedule$pc_characters)) NULL else as.integer(schedule$pc_characters)
  )
}

#' Carry out one export schedule now
#'
#' @param con A [DBI::DBIConnection-class].
#' @param schedule One entry of `episodic_export_schedules_current()`.
#' @param config A resolved configuration.
#' @param run_date The run's date: the window ends on it.
#' @param run_id The run, recorded on the export, or `NA`.
#' @return The new `export_id`. Raises after recording a failure.
#' @keywords internal
#' @noRd
episodic_export_schedule_run <- function(con, schedule, config, run_date, run_id = NA_integer_) {
  to <- as.Date(run_date)
  from <- to - as.integer(schedule$window_days) + 1L
  endpoint <- tryCatch(episodic_export_schedule_endpoint(schedule), error = function(e) e)
  label <- if (inherits(endpoint, "error")) NA_character_ else episodic_import_export_endpoint_label(endpoint)
  built <- NULL
  outcome <- tryCatch(
    {
      if (inherits(endpoint, "error")) {
        stop(endpoint)
      }
      inputs <- episodic_export_schedule_inputs(schedule)
      built <- episodic_import_export_build(
        con,
        from = from,
        to = to,
        recipients = inputs$recipients,
        passphrase = inputs$passphrase,
        signing_key = inputs$signing_key,
        fields = inputs$fields,
        pc_characters = inputs$pc_characters,
        config = config
      )
      episodic_import_export_endpoint_write(endpoint, built$bytes)
      NULL
    },
    error = function(e) e
  )
  export_id <- episodic_db_export_record(
    con,
    initiated_by = "schedule",
    status = if (is.null(outcome)) "written" else "failed",
    from = from,
    to = to,
    built = built,
    destination = label,
    schedule_name = schedule$schedule_name,
    schedule_event_id = schedule$event_id,
    run_id = run_id,
    error_text = if (is.null(outcome)) NA_character_ else conditionMessage(outcome)
  )
  if (!is.null(outcome)) {
    stop(outcome)
  }
  invisible(export_id)
}

#' Carry out every export schedule that is due, after a run commits
#'
#' On the footing scheduled reports are on: one schedule's failure is
#' recorded and logged and never stops another schedule or fails the run.
#' @param con A [DBI::DBIConnection-class].
#' @param config A resolved configuration.
#' @param run_id The run.
#' @param run_date The run's date.
#' @return Invisibly, the number of exports written.
#' @keywords internal
#' @noRd
episodic_export_schedules_dispatch <- function(con, config, run_id, run_date) {
  schedules <- episodic_export_schedules_current(con)
  if (length(schedules) == 0) {
    return(invisible(0L))
  }
  exports <- episodic_db_get_query(con, "SELECT * FROM episodic_export_event")
  written <- 0L
  for (schedule in schedules) {
    if (!episodic_export_schedule_due(schedule, exports, run_date)) {
      next
    }
    result <- tryCatch(
      episodic_export_schedule_run(con, schedule, config, run_date, run_id = run_id),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      episodic_trace(
        "Export schedule \"", schedule$schedule_name, "\" failed: ",
        conditionMessage(result),
        severity = "danger"
      )
    } else {
      written <- written + 1L
      episodic_trace("Export schedule \"", schedule$schedule_name, "\" written (export ", result, ")")
    }
  }
  invisible(written)
}

#' The secret a source opens its files with, read from the environment now
#' @keywords internal
#' @noRd
episodic_import_source_secret <- function(source) {
  if (identical(source$mode, "passphrase")) {
    list(
      secret_key = NULL,
      passphrase = episodic_import_export_env_value(source$passphrase_env, "The passphrase")
    )
  } else {
    list(
      secret_key = episodic_import_export_key_read(
        episodic_import_export_env_value(source$key_env, "The secret encryption key"),
        "encryption",
        "secret",
        paste0("The secret encryption key in ", source$key_env)
      ),
      passphrase = NULL
    )
  }
}

#' Fetch and import every registered source, before a run's transaction
#'
#' One entry per source, imported or failed: a failure is the source's
#' own, recorded with its reason and logged as a warning, and the run
#' goes on with the others and finishes `partial`.
#' @param con A [DBI::DBIConnection-class].
#' @param config A resolved configuration.
#' @return A list of `episodic_import_export_entry()` results.
#' @keywords internal
#' @noRd
episodic_import_sources_fetch <- function(con, config) {
  sources <- episodic_import_sources_current(con)
  lapply(sources, function(source) {
    label <- tryCatch(
      episodic_import_export_endpoint_label(episodic_import_source_endpoint(source)),
      error = function(e) NA_character_
    )
    imported <- tryCatch(
      {
        secret <- episodic_import_source_secret(source)
        bytes <- episodic_import_export_endpoint_read(episodic_import_source_endpoint(source))
        episodic_cases_import_bytes(
          bytes,
          secret_key = secret$secret_key,
          passphrase = secret$passphrase,
          trusted = list(episodic_import_export_key_parse(source$trusted_sender, "signing", "public", "The trusted sender key")),
          config = config
        )
      },
      error = function(e) e
    )
    if (inherits(imported, "error")) {
      episodic_trace(
        "Import source \"", source$source_name, "\" failed, its cases not ",
        "taken in this run: ", conditionMessage(imported),
        severity = "warn"
      )
      return(episodic_import_export_entry(
        source_name = source$source_name,
        location = label,
        error = imported
      ))
    }
    meta <- attr(imported, "import")
    episodic_trace(
      "Import source \"", source$source_name, "\": ", nrow(imported),
      " case(s) from instance ", meta$sender_instance_id, " (", meta$from,
      " to ", meta$to, ")"
    )
    for (w in meta$warnings) {
      episodic_trace("Import source \"", source$source_name, "\": ", w, severity = "warn")
    }
    episodic_import_export_entry(cases = imported, source_name = source$source_name, location = label)
  })
}

#' Test a registered import source the way the next run will use it
#'
#' Reads the key or passphrase variable, fetches the file and imports it
#' in full - decryption, signature, hashes - without writing anything, so
#' a green test means the next run takes the file in.
#' @param source One entry of `episodic_import_sources_current()`.
#' @param config A resolved configuration.
#' @return A list of `episodic_import_export_test_line()` results.
#' @keywords internal
#' @noRd
episodic_import_export_test_source <- function(source, config) {
  lines <- list(episodic_import_export_test_line("environment", NA, "import_export.test.environment_note"))
  add <- function(...) lines[[length(lines) + 1L]] <<- episodic_import_export_test_line(...)
  variable <- if (identical(source$mode, "passphrase")) source$passphrase_env else source$key_env
  secret <- tryCatch(episodic_import_source_secret(source), error = function(e) e)
  if (inherits(secret, "error")) {
    add("key", FALSE, "import_export.test.secret_failed", variable = variable, detail = conditionMessage(secret))
    return(lines)
  }
  if (is.null(secret$secret_key)) {
    add("key", TRUE, "import_export.test.passphrase_ok", variable = variable)
  } else {
    add(
      "key", TRUE, "import_export.test.key_ok",
      variable = variable,
      fingerprint = episodic_import_export_fingerprint_short(
        episodic_import_export_fingerprint(sodium::pubkey(secret$secret_key))
      )
    )
  }
  endpoint <- episodic_import_source_endpoint(source)
  reached <- episodic_import_export_endpoint_test(endpoint)
  lines <- c(lines, reached$lines)
  if (is.null(reached$bytes)) {
    return(lines)
  }
  imported <- tryCatch(
    episodic_cases_import_bytes(
      reached$bytes,
      secret_key = secret$secret_key,
      passphrase = secret$passphrase,
      trusted = list(episodic_import_export_key_parse(source$trusted_sender, "signing", "public", "The trusted sender key")),
      config = config
    ),
    error = function(e) e
  )
  if (inherits(imported, "error")) {
    add("import", FALSE, "import_export.test.import_failed", detail = conditionMessage(imported))
  } else {
    meta <- attr(imported, "import")
    add(
      "import", TRUE, "import_export.test.import_ok",
      n = nrow(imported), instance = meta$sender_instance_id, from = meta$from, to = meta$to
    )
    for (w in meta$warnings) {
      add("import", NA, "import_export.test.import_warning", detail = w)
    }
  }
  lines
}

#' Test a registered export schedule without exporting anything
#' @param schedule One entry of `episodic_export_schedules_current()`.
#' @return A list of `episodic_import_export_test_line()` results.
#' @keywords internal
#' @noRd
episodic_import_export_test_schedule <- function(schedule) {
  lines <- list(episodic_import_export_test_line("environment", NA, "import_export.test.environment_note"))
  add <- function(...) lines[[length(lines) + 1L]] <<- episodic_import_export_test_line(...)
  signing <- tryCatch(
    episodic_import_export_key_read(
      episodic_import_export_env_value(schedule$signing_key_env, "The signing key"),
      "signing",
      "secret",
      paste0("The signing key in ", schedule$signing_key_env)
    ),
    error = function(e) e
  )
  if (inherits(signing, "error")) {
    add("key", FALSE, "import_export.test.secret_failed", variable = schedule$signing_key_env, detail = conditionMessage(signing))
  } else {
    add(
      "key", TRUE, "import_export.test.signing_ok",
      variable = schedule$signing_key_env,
      fingerprint = episodic_import_export_fingerprint_short(
        episodic_import_export_fingerprint(sodium::sig_pubkey(signing))
      )
    )
  }
  if (identical(schedule$mode, "passphrase")) {
    value <- Sys.getenv(schedule$passphrase_env, unset = NA)
    if (is.na(value) || !nzchar(value)) {
      add("passphrase", FALSE, "import_export.test.env_missing", variable = schedule$passphrase_env)
    } else if (nchar(value, type = "chars") < episodic_import_export_passphrase_min_chars) {
      add("passphrase", FALSE, "import_export.test.passphrase_short", variable = schedule$passphrase_env, n = episodic_import_export_passphrase_min_chars)
    } else {
      add("passphrase", TRUE, "import_export.test.passphrase_ok", variable = schedule$passphrase_env)
    }
  } else {
    add("recipients", TRUE, "import_export.test.recipients", n = length(jsonlite::fromJSON(schedule$recipients)))
  }
  c(lines, episodic_import_export_endpoint_test(episodic_export_schedule_endpoint(schedule))$lines)
}

#' Test an Import Source or Export Schedule
#'
#' Checks a source or schedule registered on the dashboard's Instance
#' screen the way the next scheduled run will use it, and writes nothing.
#' For an import source it reads the key or passphrase variable, fetches
#' the file and imports it in full (decryption, signature, hashes), so a
#' passing test means the next run takes it in. For an export schedule it
#' reads the signing key and passphrase variables and checks the
#' destination without writing to it: a path's directory must exist and
#' be writable; an HTTP(S) destination is asked for `OPTIONS`, which shows
#' whether it answers and refuses the credentials outright, while whether
#' an upload is accepted is known only from the first real export.
#'
#' The environment variables are read in the process this runs in. The
#' scheduled run reads its own, from wherever it is started; a test from
#' a console or the dashboard proves that environment, not the cron's.
#'
#' @param name The source's or schedule's name.
#' @param kind `"source"` or `"schedule"`.
#' @param db_path The EpiSODIC database. Defaults to the `EPISODIC_DB`
#'   environment variable.
#' @param episodic_config_path The configuration path. Defaults to the
#'   `EPISODIC_CONFIG` environment variable.
#' @return An object of class `episodic_import_export_test`: a list with
#'   `name`, `kind`, `ok` (`TRUE` when no check failed) and `lines`, one
#'   per check. Printing it lists the checks.
#' @seealso `vignette("import-export")`
#' @examples
#' \dontrun{
#' episodic_import_export_test_connection("Partner lab", kind = "source")
#' }
#' @export
episodic_import_export_test_connection <- function(name,
                                                   kind = c("source", "schedule"),
                                                   db_path = Sys.getenv("EPISODIC_DB", unset = NA),
                                                   episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  kind <- match.arg(kind)
  con <- episodic_db_open(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  episodic_import_export_test_named(con, name, kind, episodic_config_resolve(episodic_config_path))
}

#' The work behind `episodic_import_export_test_connection()`, on an open connection
#' @keywords internal
#' @noRd
episodic_import_export_test_named <- function(con, name, kind, config) {
  current <- if (identical(kind, "source")) {
    episodic_import_sources_current(con)
  } else {
    episodic_export_schedules_current(con)
  }
  entry <- current[[name]]
  if (is.null(entry)) {
    stop("There is no current ", kind, " named \"", name, "\".", call. = FALSE)
  }
  lines <- if (identical(kind, "source")) {
    episodic_import_export_test_source(entry, config)
  } else {
    episodic_import_export_test_schedule(entry)
  }
  structure(
    list(
      name = name,
      kind = kind,
      ok = !any(vapply(lines, function(l) identical(l$ok, FALSE), logical(1))),
      lines = lines
    ),
    class = "episodic_import_export_test"
  )
}

#' One connection test line, in a language
#' @param line An `episodic_import_export_test_line()` result.
#' @param lang The language.
#' @return A single string.
#' @keywords internal
#' @noRd
episodic_import_export_test_text <- function(line, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  params <- lapply(line$params, function(v) {
    if (is.numeric(v)) episodic_format_number(v, lang = lang) else as.character(v)
  })
  do.call(episodic_tr, c(list(line$key), params, list(lang = lang)))
}

#' Print a connection test
#' @param x An `episodic_import_export_test`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
#' @export
print.episodic_import_export_test <- function(x, ...) {
  cat("Import/export ", x$kind, " \"", x$name, "\": ", if (x$ok) "passed" else "FAILED", "\n", sep = "")
  for (line in x$lines) {
    mark <- if (isTRUE(line$ok)) "[ok]  " else if (identical(line$ok, FALSE)) "[FAIL]" else "[info]"
    cat(mark, " ", episodic_import_export_test_text(line), "\n", sep = "")
  }
  invisible(x)
}

#' The export and import audit trail, most recent first
#' @param con A [DBI::DBIConnection-class].
#' @param limit The most rows to return.
#' @return A data frame.
#' @keywords internal
#' @noRd
episodic_db_export_events <- function(con, limit = 200L) {
  episodic_db_get_query(
    con,
    "SELECT * FROM episodic_export_event
      ORDER BY exported_at DESC, export_id DESC LIMIT ?",
    params = list(as.integer(limit))
  )
}

#' @keywords internal
#' @noRd
episodic_db_import_events <- function(con, limit = 200L) {
  episodic_db_get_query(
    con,
    "SELECT * FROM episodic_import_event
      ORDER BY imported_at DESC, import_id DESC LIMIT ?",
    params = list(as.integer(limit))
  )
}
