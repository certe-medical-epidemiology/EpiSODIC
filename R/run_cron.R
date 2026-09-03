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

#' Emit a timestamped progress line
#'
#' `episodic_run_cron()` writes one of these at every phase of a run, so
#' a scheduled cron job's own log - and a console watching an interactive
#' one - shows where the run currently is. Unconditional, not gated
#' behind `debug`: this is the trail that lets "the run got stuck" or "R
#' crashed" localise to a phase without needing to reproduce anything,
#' which is exactly the information a silent run (or a fatal crash that
#' leaves no R-level error at all) otherwise never gives up.
#' @keywords internal
#' @noRd
episodic_trace <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), " | ", ...)
}

#' The extra detail `debug = TRUE` adds on top of every `episodic_trace()` line
#'
#' Session and package versions, memory, and the resolved configuration -
#' the things worth having in hand when something goes wrong but too
#' verbose to print on every run by default.
#' @keywords internal
#' @noRd
episodic_trace_debug <- function(debug, ...) {
  if (isTRUE(debug)) {
    episodic_trace(...)
  }
  invisible(NULL)
}

#' A one-line memory snapshot, for `debug = TRUE` traces
#'
#' `gc()`'s own matrix shape (which columns it reports, and in what
#' unit) varies across R versions, so its printed form - not its
#' indexed values - is what gets reused here.
#' @keywords internal
#' @noRd
episodic_trace_memory <- function() {
  lines <- utils::capture.output(print(gc(verbose = FALSE)))
  paste(trimws(lines[nzchar(trimws(lines))]), collapse = " | ")
}

#' Print the exact SQL and bound parameter values about to be sent, for `debug = TRUE`
#'
#' The crash trace has now landed on three different calls across three
#' otherwise-identical runs (once at the `episodic_growth_slope()` /
#' `episodic_spatial_concentration()` boundary, once inside
#' `episodic_spatial_concentration()`, once inside
#' `episodic_app_density()`) - all downstream of a database round trip,
#' never in between two purely in-memory steps. Printing the literal SQL
#' text and every bound parameter's value, class and encoding
#' immediately before each such call, rather than only "done" once it
#' returns, is what finally shows whether one specific query - or one
#' specific parameter value - is what a run never gets past, instead of
#' inferring it from where the trace happens to stop.
#' @keywords internal
#' @noRd
episodic_trace_query <- function(debug, sql, params = list()) {
  if (!isTRUE(debug)) {
    return(invisible(NULL))
  }
  param_detail <- if (length(params) == 0) {
    "(none)"
  } else {
    paste(
      vapply(
        params,
        function(p) {
          sprintf(
            "%s<%s>",
            paste(utils::capture.output(print(p)), collapse = " "),
            paste(class(p), collapse = "/")
          )
        },
        character(1)
      ),
      collapse = ", "
    )
  }
  episodic_trace("debug:       SQL: ", sql, " | params: ", param_detail)
}

#' Dump a `pc` vector's own encoding, for `debug = TRUE` traces
#'
#' Printed right before `episodic_spatial_concentration()` runs, since a
#' MariaDB-only crash has been isolated to exactly that call on exactly
#' this input - reproducibly, on data forced through a `gc()` right
#' beforehand that itself completed cleanly, which rules out a merely
#' *delayed* symptom of damage from an earlier call and points at
#' something about this specific `pc` data instead. A text value fetched
#' from a database can carry a `CHARSXP` that claims one encoding
#' (`UTF-8`, `latin1`, native) while actually holding bytes for another
#' - a client/server character-set mismatch is a well-known way for this
#' to happen with MariaDB specifically - and R's own string hashing
#' (which `table()`, called inside `episodic_spatial_concentration()`,
#' relies on) is not guaranteed to be safe against that. Cheap: a
#' candidate's `pc` vector is a handful of values at most, never the
#' full stream.
#' @keywords internal
#' @noRd
episodic_trace_pc_dump <- function(debug, pc) {
  if (!isTRUE(debug)) {
    return(invisible(NULL))
  }
  if (length(pc) == 0) {
    episodic_trace("debug:       pc: (no values)")
    return(invisible(NULL))
  }
  detail <- vapply(
    pc,
    function(x) {
      if (is.na(x)) {
        return("NA")
      }
      sprintf(
        "%s [enc=%s valid=%s nchar=%d bytes=%s]",
        x,
        Encoding(x),
        tryCatch(validEnc(x), error = function(e) NA),
        tryCatch(nchar(x, type = "bytes"), error = function(e) NA_integer_),
        paste(
          tryCatch(as.integer(charToRaw(x)), error = function(e) NA_integer_),
          collapse = ","
        )
      )
    },
    character(1)
  )
  episodic_trace("debug:       pc: ", paste(detail, collapse = " | "))
}

#' Everything `debug = TRUE` prints once, at the start of a run
#'
#' A crash that leaves no R-level error at all is the case this exists
#' for: `sessionInfo()`, the versions of the packages a fatal error is
#' most likely to originate in (the DB driver, `surveillance`,
#' `EpiEstim`), and the database dialect in play, printed once so
#' whoever reads the log afterwards does not have to ask the operator
#' what they were running.
#' @param db_path The `db_path` argument as given to `episodic_run_cron()`.
#' @keywords internal
#' @noRd
episodic_trace_session_info <- function(db_path) {
  episodic_trace("---- debug: session info ----")
  message(paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
  episodic_trace(
    "debug: database dialect = ",
    tryCatch(episodic_db_dialect(db_path), error = function(e) NA_character_)
  )
  episodic_trace("debug: package versions:")
  versions <- episodic_pkg_versions_extended()
  for (nm in names(versions)) {
    message("    ", nm, ": ", versions[[nm]])
  }
  episodic_trace("debug: memory at start: ", episodic_trace_memory())
  episodic_trace("---- debug: end session info ----")
}

#' Package versions worth knowing when a run crashes without an R-level error
#'
#' Wider than `episodic_pkg_versions()` (which is what gets recorded on
#' the run row): this is printed, not stored, so it can include the
#' database driver and every compiled-code dependency a fatal error
#' (rather than a catchable one) is most likely to actually originate
#' in.
#' @keywords internal
#' @noRd
episodic_pkg_versions_extended <- function() {
  pkgs <- c(
    "EpiSODIC",
    "DBI",
    "RSQLite",
    "RMariaDB",
    "surveillance",
    "EpiEstim",
    "sf",
    "MASS"
  )
  stats::setNames(
    vapply(
      pkgs,
      function(p) {
        if (requireNamespace(p, quietly = TRUE)) {
          as.character(utils::packageVersion(p))
        } else {
          "not installed"
        }
      },
      character(1)
    ),
    pkgs
  )
}

#' Run one surveillance detection cycle
#'
#' This is the function you schedule to run regularly (e.g. daily, via
#' cron): it pulls in new laboratory data, checks every monitored stream
#' for statistical aberrations with the configured detectors, reconciles
#' the results into cluster dossiers for the board to assess, and records
#' everything in the database. A run either completes in full or leaves no
#' trace at all - it runs inside a single database transaction, so a
#' failed run is always safe to simply retry.
#'
#' EpiSODIC never connects to your laboratory system directly. You extract
#' and transform your own data beforehand, and hand it over as a plain data
#' frame or `tibble`: `cases` for the laboratory results themselves (see
#' [episodic_case_data] for the required columns and their allowed
#' values), and optionally `denominators` and `institution_activity` for
#' testing volume and hospital activity. A data set is the normal case; if
#' producing the data only makes sense at run time (a live database query,
#' for instance), a zero-argument function returning one is accepted just
#' as well - see [episodic_resolve_data()].
#'
#' The exact detection settings used are recorded with the run (see
#' [episodic_config_hash()]), so any past result can always be traced back
#' to the configuration that produced it.
#'
#' So is what each feed delivered. Before the run writes anything, your
#' case data goes through [episodic_check_cases()]. Structural problems -
#' a missing column, a value outside the allowed set, a date that does not
#' read as a date - stop the run with an error naming every offending
#' column, its values and the rows they are in, and are recorded on the
#' run as well, so the reason is visible both where the run was started
#' and in the dashboard's activity screen. Advisory findings are mentioned
#' once and the run proceeds. Run [episodic_check_cases()] on your extract
#' yourself to see all of it without starting a run at all. A run that
#' fails later, for any other reason, records the reason and warns rather
#' than returning quietly. Rows
#' that are merely unmatched are counted rather than dropped in silence:
#' institution activity whose `institution_key` matches no known
#' institution is skipped with a warning, its count recorded, and the run
#' finishes `"partial"` instead of `"success"`. Both are complete runs the
#' dashboard reads from; `"partial"` says go and look at why rows were
#' skipped. `episodic_detection_run` carries the counts (`n_cases_supplied`,
#' `n_cases_inserted`, `n_activity_skipped`, and the rest).
#'
#' @param cases Your laboratory data: a data frame or `tibble` in the
#'   shape [episodic_case_data] describes, or a zero-argument function
#'   that returns one. Defaults to the bundled synthetic generator, useful
#'   for demos and testing but not real surveillance.
#' @param db_path Path to the EpiSODIC database: a SQLite file (created
#'   automatically if it does not exist yet) or a MariaDB/MySQL DSN (see
#'   [episodic_db_dsn_mariadb()]).
#' @param denominators Optional: your testing-volume data, in the same
#'   form as `cases` - normally a data set, a function if it has to be
#'   produced at run time (see [episodic_synthetic_denominators()] for the
#'   expected shape). Leave as `NULL` (the default) if you have none to
#'   supply - positivity panels simply stay blank.
#' @param institution_activity Optional: your hospital patient-days
#'   data (see [episodic_synthetic_institution_activity()] for the
#'   expected shape), normally as a data set, or as a function taking the
#'   current institutions table. Leave as `NULL` (the default) if you have
#'   none - detection falls back to raw case counts.
#' @param episodic_config_path Passed to [episodic_config_resolve()].
#' @param host,account Recorded with the run for audit purposes; default
#'   to the current machine and account.
#' @param run_date The date to treat as "today". Defaults to the system
#'   date; mainly useful to override in tests.
#' @param debug If `TRUE`, print a lot more than the phase-by-phase
#'   progress this function always writes: `sessionInfo()`, the versions
#'   of every package a fatal (non-catchable) crash is most likely to
#'   originate in, memory snapshots, per-stream detail inside the
#'   detection loop, and - for the calls implicated so far in a known
#'   MariaDB-only crash (`episodic_app_density()`, the population-vector
#'   lookup, the trend/detection writes, the assessment-event lookups,
#'   and `episodic_spatial_concentration()`'s own input) - the exact SQL
#'   and every bound parameter's value, class and encoding immediately
#'   before each such call runs, not only once it returns. Meant for
#'   chasing exactly the kind of failure that leaves no R-level error
#'   behind at all - a crashed session, a run that silently never
#'   returns - where the normal progress trace does not narrow things
#'   down enough on its own. Noisy; leave off for routine scheduled
#'   runs.
#' @return Invisibly, the `run_id` of the completed run. The run's row in
#'   `episodic_detection_run` holds its status, the per-feed load counts,
#'   and `error_text` if it failed. Case data that does not satisfy the
#'   contract throws instead of returning - the run row is still written,
#'   with `status = "failed"` and the same message in `error_text`.
#' @inheritSection episodic_case_data Check your data before you run anything
#' @seealso [episodic_check_cases()] to see what EpiSODIC makes of your
#'   extract before you schedule anything, and [episodic_case_data] for
#'   the contract it checks against.
#' @examples
#' \donttest{
#' db_path <- tempfile(fileext = ".sqlite")
#' cases <- episodic_synthetic_cases(
#'   start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
#' )
#' run_id <- episodic_run_cron(db_path = db_path, cases = cases)
#' file.remove(db_path)
#' }
#' @export
episodic_run_cron <- function(
    cases,
    denominators = NULL,
    institution_activity = NULL,
    episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
    db_path = Sys.getenv("EPISODIC_DB"),
    host = Sys.info()[["nodename"]],
    account = Sys.info()[["user"]],
    run_date = Sys.Date(),
    debug = FALSE) {
  start_time <- Sys.time()
  episodic_trace(
    "episodic_run_cron() starting (host=",
    host,
    ", account=",
    account,
    ")"
  )
  if (isTRUE(debug)) {
    episodic_trace_session_info(db_path)
  }

  episodic_trace("Resolving configuration")
  config <- episodic_config_resolve(episodic_config_path)
  hashed <- episodic_config_hash(config)
  episodic_trace(
    "Configuration resolved (hash ",
    substr(hashed$hash, 1, 12),
    ")"
  )

  episodic_trace("Connecting to database")
  con <- if (episodic_db_exists(db_path)) {
    episodic_db_connect(db_path)
  } else {
    episodic_trace("No existing database found - creating one")
    episodic_db_create(db_path)
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  episodic_trace(
    "Database connected (dialect: ",
    episodic_db_dialect(db_path),
    ")"
  )

  run_id <- episodic_db_run_start(
    con,
    host = host,
    account = account,
    run_date = run_date
  )
  episodic_trace("Run ", run_id, " started")

  # Data problems are the operator's to fix, so they must reach the
  # operator: resolve every feed and check it here, before the run has
  # written anything, and refuse out loud. A run that swallowed the
  # reason left an empty dashboard and a run row nobody was looking at -
  # which is exactly the situation somebody connecting their own extract
  # for the first time finds themselves in.
  episodic_trace("Resolving and checking case data")
  prepared <- tryCatch(
    {
      # A PC-to-province mapping an operator configured and this run
      # cannot use is a structural problem like any other, and belongs
      # here with them: falling back to the shipped demo ranges would
      # build the province level of the lattice on somebody else's
      # provinces without saying so.
      pc_province_problem <- episodic_pc_province_map_problem()
      if (!is.na(pc_province_problem)) {
        stop(pc_province_problem, call. = FALSE)
      }
      cases <- episodic_resolve_data(cases)
      episodic_trace_debug(
        debug,
        "debug: case data resolved (",
        nrow(cases),
        " rows)"
      )
      report <- episodic_check_cases(cases)
      problems <- report[report$severity == "problem", , drop = FALSE]
      if (nrow(problems) > 0) {
        stop(episodic_check_failure_message(problems), call. = FALSE)
      }
      episodic_trace(
        "Case data checked: ",
        nrow(cases),
        " rows, 0 problems, ",
        sum(report$severity == "advice"),
        " advisory finding(s)"
      )
      denominators <- episodic_resolve_data(denominators)
      if (!is.null(denominators)) {
        episodic_trace(
          "Validating denominator data (",
          nrow(denominators),
          " rows)"
        )
        episodic_validate_denominators(denominators)
      }
      # institution_activity is resolved against the institutions table,
      # which only exists once cases have loaded, so it cannot be
      # resolved and checked here the way cases and denominators are -
      # but its own contract (columns, filled, dates, patient_days) does
      # not depend on that, so it is still worth refusing on up front
      # rather than mid-transaction if it was handed over as a plain
      # data frame rather than a function.
      if (
        !is.null(institution_activity) && is.data.frame(institution_activity)
      ) {
        episodic_trace(
          "Validating institution activity data (",
          nrow(institution_activity),
          " rows)"
        )
        episodic_validate_institution_activity(institution_activity)
      }
      report
    },
    error = function(e) e
  )
  if (inherits(prepared, "condition")) {
    episodic_trace("Pre-run checks failed: ", conditionMessage(prepared))
    episodic_run_cron_finish(
      con,
      run_id,
      hashed,
      episodic_run_cron_failure(conditionMessage(prepared))
    )
    stop(conditionMessage(prepared), call. = FALSE)
  }

  # The data is usable, but usable is not the same as intended. Say once
  # that there is something to look at, rather than either burying it or
  # repeating the whole report into every scheduled run's log.
  advice <- prepared[prepared$severity == "advice", , drop = FALSE]
  if (nrow(advice) > 0) {
    message(
      "episodic_check_cases() has ",
      nrow(advice),
      if (nrow(advice) == 1) " advisory finding" else " advisory findings",
      " about this case data, starting with: ",
      advice$message[1],
      " Run episodic_check_cases() on it to see them all."
    )
  }

  # Not a failure - an empty extract is a legitimate thing to hand over -
  # but never something to discover from an empty dashboard either.
  if (nrow(cases) == 0) {
    warning(
      "The case data supplied to episodic_run_cron() has no rows, so this ",
      "run has nothing to detect on and writes no cases. Check the date ",
      "and positives-only filters in your own extract step.",
      call. = FALSE
    )
  }

  episodic_trace("Beginning transaction")
  result <- tryCatch(
    {
      DBI::dbBegin(con)
      stats <- episodic_run_cron_body(
        con,
        run_id,
        config,
        cases,
        denominators,
        institution_activity,
        run_date,
        debug = debug
      )
      episodic_trace("Committing transaction")
      DBI::dbCommit(con)
      stats
    },
    error = function(e) {
      episodic_trace(
        "Error during run body, rolling back: ",
        conditionMessage(e)
      )
      DBI::dbRollback(con)
      episodic_run_cron_failure(conditionMessage(e))
    }
  )

  # Overlaid here, not when `config` was first resolved: an is_admin
  # account's Settings-screen edits must reach the very next run without a
  # redeploy, and notifications is excluded from config_hash/
  # config_snapshot regardless of when this runs, so applying it after the
  # detection transaction has already committed changes nothing about
  # reproducibility.
  notify_config <- config
  notify_config$notifications <- episodic_config_resolve(
    episodic_config_path,
    con = con
  )$notifications
  tryCatch(
    episodic_notify(con, notify_config, result, run_id, run_date, host),
    error = function(e) {
      episodic_trace("Notification dispatch failed: ", conditionMessage(e))
    }
  )

  episodic_trace(
    "Finishing run ",
    run_id,
    " (status: ",
    result$status %||% "success",
    ")"
  )
  episodic_run_cron_finish(con, run_id, hashed, result)

  # The run row records this, but a scheduled run nobody reads the row of
  # must still say so where it ran.
  if (identical(result$status, "failed")) {
    warning(
      "EpiSODIC run ",
      run_id,
      " failed and wrote nothing: ",
      result$error_text,
      call. = FALSE
    )
  }

  elapsed <- round(
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    1
  )
  episodic_trace(
    "episodic_run_cron() finished in ",
    elapsed,
    "s (status: ",
    result$status %||% "success",
    ")"
  )
  if (isTRUE(debug)) {
    episodic_trace_debug(
      debug,
      "debug: memory at finish: ",
      episodic_trace_memory()
    )
  }

  invisible(run_id)
}

#' How many weeks of Farrington this run owes
#'
#' A nightly run owes one: the week that just became testable. A run
#' following a gap owes every week since the last completed one, because
#' nothing else will ever test those - a detector that only ever looks at
#' the current week turns a server outage into a hole in the surveillance
#' record. A first run, with no completed run behind it, owes the cap: an
#' instance starting against a backfilled history should open on the
#' current picture rather than on one week of it, and the cap is what
#' stops years of backfill arriving as years of dossiers.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param run_date The date this run treats as today.
#' @param config The resolved configuration; uses
#'   `config$farrington$max_weeks_tested`, defaulting to 8.
#' @return A positive integer.
#' @keywords internal
#' @noRd
episodic_farrington_weeks_owed <- function(con, run_date, config) {
  cap <- as.integer(config$farrington$max_weeks_tested %||% 8L)
  cap <- max(1L, cap)

  previous <- episodic_db_latest_run(
    con,
    status = episodic_run_statuses_complete
  )
  if (is.null(previous) || is.na(previous$finished_at)) {
    return(cap)
  }
  since <- suppressWarnings(as.Date(substr(previous$finished_at, 1, 10)))
  if (is.na(since)) {
    return(cap)
  }
  weeks <- as.numeric(difftime(as.Date(run_date), since, units = "days")) / 7
  min(cap, max(1L, ceiling(weeks)))
}

#' The counts a run that wrote nothing has to report
#'
#' NA rather than zero throughout, deliberately: a failed run did not
#' load zero cases, it never got as far as loading any.
#' @keywords internal
#' @noRd
episodic_run_cron_failure <- function(error_text) {
  list(
    status = "failed",
    error_text = error_text,
    n_streams = NA_integer_,
    n_detections = NA_integer_,
    n_signals_new = NA_integer_,
    n_signals_updated = NA_integer_,
    n_cases_supplied = NA_integer_,
    n_cases_deduplicated = NA_integer_,
    n_cases_inserted = NA_integer_,
    n_denominators_written = NA_integer_,
    n_activity_supplied = NA_integer_,
    n_activity_written = NA_integer_,
    n_activity_skipped = NA_integer_
  )
}

#' Close the run row off, however the run ended
#' @keywords internal
#' @noRd
episodic_run_cron_finish <- function(con, run_id, hashed, result) {
  pkg_versions <- jsonlite::toJSON(episodic_pkg_versions(), auto_unbox = TRUE)

  episodic_db_run_finish(
    con,
    run_id,
    status = if (is.null(result$status)) "success" else result$status,
    n_streams = result$n_streams,
    n_detections = result$n_detections,
    n_signals_new = result$n_signals_new,
    n_signals_updated = result$n_signals_updated,
    n_cases_supplied = result$n_cases_supplied,
    n_cases_deduplicated = result$n_cases_deduplicated,
    n_cases_inserted = result$n_cases_inserted,
    n_denominators_written = result$n_denominators_written,
    n_activity_supplied = result$n_activity_supplied,
    n_activity_written = result$n_activity_written,
    n_activity_skipped = result$n_activity_skipped,
    code_version = as.character(utils::packageVersion("EpiSODIC")),
    pkg_versions = as.character(pkg_versions),
    config_hash = hashed$hash,
    config_snapshot = hashed$snapshot,
    error_text = result$error_text
  )
  invisible(NULL)
}

#' Run statuses that mean the run completed and wrote its results
#'
#' `success` and `partial` differ only in whether rows of an optional
#' feed were skipped; both produced detections and both are safe to read
#' from. Anything asking for "the latest usable run" wants this, not
#' `"success"` alone - otherwise a `partial` run leaves the dashboard
#' quietly showing an older run's numbers.
#' @keywords internal
#' @noRd
episodic_run_statuses_complete <- c("success", "partial")

#' Resolve a data source argument to a data frame
#'
#' A small helper behind [episodic_run_cron()]'s `cases`,
#' `denominators`, and `institution_activity` arguments, each
#' of which accepts a data frame or `tibble` directly - the normal case -
#' or, if producing the data only makes sense at run time (a live database
#' query, for instance), a zero-argument function that returns one.
#'
#' Resolving is all this does: it does not look at what the data
#' contains. To find out whether your case data can actually be used -
#' which columns are missing, which values are outside their allowed set,
#' which dates do not read as dates, and which rows those are - run
#' [episodic_check_cases()] on it, or [episodic_validate_cases()] if you
#' want a script to stop. Both accept the same two forms this does, so you
#' can check a data set and the function that produces it alike.
#'
#' @param x A data frame or `tibble`, a function returning one, or `NULL`.
#' @param ... Passed to `x` if it is a function; ignored otherwise.
#' @return `NULL` if `x` is `NULL`; `x` itself if it is a data frame (a
#'   `tibble` included); the result of calling `x` otherwise.
#' @inheritSection episodic_case_data Check your data before you run anything
#' @seealso [episodic_check_cases()] to check the resolved data against
#'   the [episodic_case_data] contract.
#' @examples
#' df <- data.frame(x = 1:3)
#' identical(episodic_resolve_data(df), df)
#' identical(episodic_resolve_data(function() df), df)
#' is.null(episodic_resolve_data(NULL))
#'
#' # what a live query would look like, and how to check what it returns
#' my_extract <- function() {
#'   episodic_synthetic_cases(
#'     start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
#'   )
#' }
#' episodic_check_cases(episodic_resolve_data(my_extract))
#' @export
episodic_resolve_data <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.function(x)) {
    return(x(...))
  }
  stop(
    "data source must be a data frame (or tibble), or a function returning one, not ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
episodic_run_cron_body <- function(
    con,
    run_id,
    config,
    cases,
    denominators,
    institution_activity,
    run_date,
    debug = FALSE) {
  episodic_trace("Loading pathogen configuration")
  pathogen_config_path <- system.file(
    "config",
    "pathogen_config.csv",
    package = "EpiSODIC"
  )
  if (identical(pathogen_config_path, "")) {
    pathogen_config_path <- file.path("inst", "config", "pathogen_config.csv")
  }
  pathogen_config <- utils::read.csv(
    pathogen_config_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  episodic_db_pathogen_config_load(con, pathogen_config)
  episodic_trace(
    "Pathogen configuration loaded (",
    nrow(pathogen_config),
    " pathogen(s))"
  )

  episodic_trace("Loading case data into the database")
  cases <- episodic_resolve_data(cases)
  case_counts <- episodic_cases_load(con, cases, pathogen_config, run_id)
  episodic_trace(
    "Case data loaded: supplied=",
    case_counts$n_supplied,
    ", deduplicated=",
    case_counts$n_deduplicated,
    ", inserted=",
    case_counts$n_inserted
  )

  denominators <- episodic_resolve_data(denominators)
  if (!is.null(denominators)) {
    episodic_trace("Loading denominator (positivity) data")
    denominator_counts <- episodic_denominators_load(con, denominators)
    episodic_trace(
      "Denominator data loaded: supplied=",
      denominator_counts$n_supplied,
      ", written=",
      denominator_counts$n_written
    )
  } else {
    denominator_counts <- list(
      n_supplied = NA_integer_,
      n_written = NA_integer_
    )
  }

  episodic_trace("Fetching all known cases and institutions")
  cases_all <- episodic_db_cases(con)
  institutions <- episodic_db_institutions(con)
  episodic_trace_debug(
    debug,
    "debug: ",
    nrow(cases_all),
    " cases and ",
    nrow(institutions),
    " institutions on file, memory: ",
    episodic_trace_memory()
  )

  institution_activity <- episodic_resolve_data(
    institution_activity,
    institutions
  )
  if (!is.null(institution_activity)) {
    episodic_trace("Loading institution activity (patient-days) data")
    activity_counts <- episodic_institution_activity_load(
      con,
      institution_activity
    )
    episodic_trace(
      "Institution activity loaded: supplied=",
      activity_counts$n_supplied,
      ", written=",
      activity_counts$n_written,
      ", skipped=",
      activity_counts$n_skipped
    )
  } else {
    activity_counts <- list(
      n_supplied = NA_integer_,
      n_written = NA_integer_,
      n_skipped = NA_integer_
    )
  }

  episodic_trace("Enumerating lattice streams")
  episodic_lattice_enumerate(con, cases_all, institutions)

  n_detections_total <- 0L
  n_new_total <- 0L
  n_updated_total <- 0L
  new_cluster_ids_all <- integer(0)
  # Streams that were eligible for Farrington but do not have the baseline
  # its configured `b` needs. Collected so the run can say so once, rather
  # than each stream quietly producing nothing.
  farrington_short <- NULL
  n_muted_streams <- 0L
  muted_stream_ids <- episodic_db_muted_stream_ids(con, run_date)

  episodic_trace("Running same-place detector")
  same_place_detections <- episodic_detect_same_place(
    con,
    cases_all,
    institutions,
    config
  )
  episodic_trace(
    "Same-place detector found ",
    nrow(same_place_detections),
    " detection(s)"
  )
  episodic_trace("Running rare-trigger detector")
  rare_trigger_detections <- episodic_detect_rare_trigger(
    con,
    cases_all,
    config
  )
  episodic_trace(
    "Rare-trigger detector found ",
    nrow(rare_trigger_detections),
    " detection(s)"
  )

  # Asked once, not per stream: it is a property of the run, not of any
  # one stream.
  farrington_weeks <- episodic_farrington_weeks_owed(con, run_date, config)
  episodic_trace("Farrington owes ", farrington_weeks, " week(s) this run")

  streams <- episodic_db_streams(con) # refresh: same_place/rare_trigger may have created streams
  # Read once for the whole run rather than once per stream inside
  # episodic_reconcile_stream(). Only a stream's own candidates can open a
  # cluster on it, so a snapshot taken here cannot go stale for any stream
  # the loop then skips.
  streams_with_clusters <- unique(
    episodic_db_clusters(
      con,
      open_only = TRUE,
      include_suppressed = TRUE
    )$stream_id
  )
  episodic_trace(
    "Reconciling ",
    nrow(streams),
    " stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)"
  )

  for (i in seq_len(nrow(streams))) {
    stream <- streams[i, ]
    stream_cases <- episodic_cases_for_stream(cases_all, stream)
    episodic_trace_debug(
      debug,
      "debug: [",
      i,
      "/",
      nrow(streams),
      "] stream ",
      stream$stream_id,
      " (",
      stream$stream_key,
      "): ",
      nrow(stream_cases),
      " case(s)"
    )

    stream_detections <- rbind(
      same_place_detections[
        same_place_detections$stream_id == stream$stream_id,
      ],
      rare_trigger_detections[
        rare_trigger_detections$stream_id == stream$stream_id,
      ]
    )

    # A muted stream produces no new detections, which is exactly what the
    # app promises when it offers the action ("temporarily suppresses new
    # detections for this stream ... so the same cause is not flagged again
    # and again"). Until now the mute was written, shown in the activity
    # log, and consulted by nothing: an epidemiologist could mute a stream
    # for a known seasonal peak and the next run would open a dossier on it
    # regardless.
    #
    # Detections only. Reconciliation still runs below, so clusters that
    # were already open go on ageing and closing normally - a mute quiets
    # what is coming, it does not freeze what is already on the board.
    muted <- stream$stream_id %in% muted_stream_ids
    if (muted) {
      stream_detections <- stream_detections[0, , drop = FALSE]
      n_muted_streams <- n_muted_streams + 1L
      episodic_trace_debug(
        debug,
        "debug:   stream is muted; detections suppressed"
      )
    }

    # MEM runs on pathogen_region (L5) streams only, for pathogens
    # flagged mem_applicable - see episodic_detect_mem()'s own docs for
    # why L5 rather than every level.
    pc_mem <- pathogen_config[pathogen_config$pathogen == stream$pathogen, ]
    if (
      nrow(pc_mem) > 0 &&
        !muted &&
        isTRUE(as.logical(pc_mem$mem_applicable[1])) &&
        identical(stream$level, "pathogen_region")
    ) {
      stream_detections <- rbind(
        stream_detections,
        episodic_detect_mem(stream_cases, stream$stream_id, run_date, config)
      )
      episodic_trace_debug(debug, "debug:   MEM detect done")
    }

    eligible <- !muted &&
      nrow(stream_cases) > 0 &&
      episodic_eligibility_gate(stream_cases, run_date, config)
    episodic_trace_debug(debug, "debug:   eligibility gate: ", eligible)
    if (eligible) {
      # A period this stream's own history shows was a confirmed
      # epidemic must not silently raise next winter's baseline.
      # Excluded from the cases fed to Farrington only
      # (same_place/rare_trigger detect on raw counts and do not baseline
      # at all, so they are unaffected).
      excluded_windows <- episodic_baseline_excluded_windows(
        con,
        stream$stream_id
      )
      farrington_cases <- episodic_baseline_exclude_cases(
        stream_cases,
        excluded_windows
      )
      episodic_trace_debug(
        debug,
        "debug:   baseline exclusion done (",
        nrow(farrington_cases),
        " of ",
        nrow(stream_cases),
        " case(s) kept)"
      )

      # Patient-day normalisation at L2. Both
      # calls below build identical weekly bins from the same
      # (farrington_cases, run_date), so one population vector serves both.
      weekly_weeks <- episodic_weekly_bins(
        as.Date(farrington_cases$sample_date),
        run_date
      )$week_start
      episodic_trace_query(
        debug,
        "SELECT * FROM episodic_institution_activity WHERE institution_id = ? ORDER BY period_start",
        list(stream$institution_id)
      )
      population <- episodic_farrington_population_vector(
        con,
        stream$institution_id,
        stream$level,
        weekly_weeks
      )
      episodic_trace_debug(
        debug,
        "debug:   population vector done (",
        length(population),
        " week(s)); calling episodic_detect_farrington() with n_weeks=",
        farrington_weeks
      )

      farrington <- episodic_detect_farrington(
        farrington_cases,
        stream$stream_id,
        config,
        run_date,
        population = population,
        n_weeks = farrington_weeks
      )
      shortfall <- episodic_farrington_shortfall(farrington)
      if (!is.null(shortfall)) {
        farrington_short <- rbind(
          farrington_short,
          data.frame(
            stream_id = stream$stream_id,
            have = unname(shortfall[["have"]]),
            need = unname(shortfall[["need"]]),
            stringsAsFactors = FALSE
          )
        )
      }
      stream_detections <- rbind(stream_detections, farrington)
      episodic_trace_debug(debug, "debug:   episodic_detect_farrington() done")

      # trend cache for the multi-year trend panel; see
      # episodic_farrington_trend()'s own docs for the backfill-once,
      # top-up-thereafter strategy.
      n_existing_trend <- nrow(episodic_db_stream_trend(con, stream$stream_id))
      trend <- episodic_farrington_trend(
        farrington_cases,
        config,
        run_date,
        n_weeks_existing = n_existing_trend,
        population = population
      )
      episodic_trace_debug(
        debug,
        "debug:   episodic_farrington_trend() done (",
        nrow(trend),
        " week(s) to upsert)"
      )
      for (k in seq_len(nrow(trend))) {
        episodic_trace_query(
          debug,
          "INSERT/UPDATE episodic_stream_trend",
          list(
            stream_id = stream$stream_id,
            week_start = as.character(trend$week_start[k]),
            n_cases = trend$n_cases[k],
            expected = trend$expected[k],
            upperbound = trend$upperbound[k]
          )
        )
        episodic_db_stream_trend_upsert(
          con,
          stream_id = stream$stream_id,
          week_start = as.character(trend$week_start[k]),
          n_cases = trend$n_cases[k],
          expected = trend$expected[k],
          upperbound = trend$upperbound[k]
        )
      }
      episodic_trace_debug(debug, "debug:   trend upsert loop done")
    }

    # A stream with nothing detected this run still has to go through
    # episodic_reconcile_stream(): that is where a stream's open clusters
    # age (runs_since_detected) and where an unassessed cluster gone stale
    # (stale_open_days) or long undetected (close_after_runs) actually
    # gets auto-closed. Skipping the call entirely for a quiet stream - as
    # this used to - left every cluster on such a stream ineligible for
    # either kind of auto-close for as long as the stream stayed quiet,
    # which is exactly the case a truly dormant stream is in forever.
    if (nrow(stream_detections) > 0) {
      detection_ids <- integer(nrow(stream_detections))
      for (j in seq_len(nrow(stream_detections))) {
        d <- stream_detections[j, ]
        episodic_trace_query(
          debug,
          "INSERT INTO episodic_detection",
          list(
            run_id = run_id,
            stream_id = stream$stream_id,
            detector = d$detector,
            first_day = d$first_day,
            last_day = d$last_day,
            n_cases = d$n_cases,
            expected = d$expected,
            upperbound = d$upperbound,
            params_json = as.character(d$params)
          )
        )
        detection_ids[j] <- episodic_db_detection_insert(
          con,
          run_id = run_id,
          stream_id = stream$stream_id,
          detector = d$detector,
          first_day = d$first_day,
          last_day = d$last_day,
          n_cases = d$n_cases,
          expected = d$expected,
          upperbound = d$upperbound,
          params_json = as.character(d$params)
        )
      }
      stream_detections$detection_id <- detection_ids
      n_detections_total <- n_detections_total + nrow(stream_detections)
      episodic_trace_debug(
        debug,
        "debug:   ",
        nrow(stream_detections),
        " detection(s) inserted"
      )
    }

    pc <- pathogen_config[pathogen_config$pathogen == stream$pathogen, ]
    case_free_days <- if (nrow(pc) > 0) {
      pc$case_free_days[1]
    } else {
      config$reconciliation$case_free_days_default
    }
    cooldown_days <- if (nrow(pc) > 0) pc$cooldown_days[1] else NA

    weights <- config$priority_score$weights
    min_excess <- config$effect_size_floor$min_excess_over_upperbound %||% NA
    min_ratio <- config$effect_size_floor$min_ratio_observed_expected %||% NA
    # A stream with no candidates this run and no open cluster from a
    # previous one has nothing for reconciliation to do: matching, ageing
    # and staleness all operate on clusters that do not exist. Skipping is
    # worth a special case because it is the common case - most streams
    # never alarm - and the call is two `SELECT`s deep even when it does
    # nothing, which across several hundred streams was the single
    # largest remaining source of round trips in a run.
    if (
      nrow(stream_detections) == 0 &&
        !(stream$stream_id %in% streams_with_clusters)
    ) {
      episodic_trace_debug(
        debug,
        "debug:   no candidates and no open clusters, skipping reconciliation"
      )
      next
    }
    episodic_trace_debug(debug, "debug:   calling episodic_reconcile_stream()")
    reconcile_result <- episodic_reconcile_stream(
      con,
      stream_id = stream$stream_id,
      detections = stream_detections,
      case_free_days = case_free_days,
      run_id = run_id,
      close_after_runs = config$reconciliation$close_after_runs,
      cooldown_days = cooldown_days,
      cooldown_reopen_ratio = config$reconciliation$cooldown_reopen_ratio %||%
        NA,
      min_excess_over_upperbound = min_excess,
      min_ratio_observed_expected = min_ratio,
      stale_open_days = config$reconciliation$stale_open_days %||% NA,
      # Five of the seven priority components are properties of the
      # candidate episode and its cases, so they are computed here, where
      # both are in hand. They used to be left at their defaults - most
      # damagingly `ratio = n_cases / max(n_cases, 1)`, which is
      # identically 1 for every candidate - which collapsed the ranking
      # that orders the whole assessment queue down to severity weight
      # and detector agreement alone.
      priority_score_fn = function(candidate) {
        episodic_trace_debug(
          debug,
          "debug:     priority_score_fn() candidate ",
          candidate$first_day,
          "..",
          candidate$last_day
        )
        metrics <- episodic_reconcile_candidate_metrics(candidate)
        candidate_cases <- episodic_cases_in_window(
          stream_cases,
          candidate$first_day,
          candidate$last_day
        )
        episodic_trace_debug(
          debug,
          "debug:       ",
          nrow(candidate_cases),
          " candidate case(s); calling episodic_app_density()"
        )
        # Same descriptive rate the dossier's own density stat shows, so
        # the ranking and the displayed evidence cannot drift apart.
        density <- episodic_app_density(
          con,
          stream,
          candidate_cases,
          debug = debug
        )
        density_ratio <- if (
          is.null(density) || is.na(density$baseline) || density$baseline <= 0
        ) {
          NA_real_
        } else {
          density$value / density$baseline
        }
        episodic_trace_debug(
          debug,
          "debug:       density done; calling episodic_growth_slope()"
        )
        growth_slope <- episodic_growth_slope(
          stream_cases,
          candidate$last_day
        )
        episodic_trace_debug(
          debug,
          "debug:       growth_slope done; calling episodic_spatial_concentration()"
        )
        episodic_trace_pc_dump(debug, candidate_cases$pc)
        spatial_concentration <- episodic_spatial_concentration(
          candidate_cases
        )
        episodic_trace_debug(
          debug,
          "debug:       spatial_concentration done; scoring"
        )
        episodic_priority_score(
          excess = metrics$excess,
          ratio = metrics$ratio,
          severity_weight = if (nrow(pc) > 0) pc$severity_weight[1] else 1,
          growth_slope = growth_slope,
          detector_agreement = candidate$detector_agreement,
          n_detectors = 4, # farrington, same_place, rare_trigger, mem
          density_ratio = density_ratio,
          spatial_concentration = spatial_concentration,
          weights = weights
        )
      },
      has_assessment_fn = function(cluster_id) {
        episodic_trace_query(
          debug,
          "SELECT * FROM episodic_assessment_event WHERE cluster_id = ? ORDER BY created_at, event_id",
          list(cluster_id)
        )
        result <- nrow(episodic_db_assessment_events(con, cluster_id)) > 0
        result
      },
      verdict_fn = function(cluster_id) {
        episodic_trace_query(
          debug,
          "SELECT * FROM episodic_assessment_event WHERE cluster_id = ? ORDER BY created_at, event_id",
          list(cluster_id)
        )
        events <- episodic_db_assessment_events(con, cluster_id)
        classified <- events[!is.na(events$verdict), ]
        if (nrow(classified) == 0) {
          NA_character_
        } else {
          classified$verdict[nrow(classified)]
        }
      }
    )
    episodic_trace_debug(debug, "debug:   episodic_reconcile_stream() done")
    n_new_total <- n_new_total + reconcile_result$n_new
    n_updated_total <- n_updated_total + reconcile_result$n_updated
    new_cluster_ids_all <- c(
      new_cluster_ids_all,
      reconcile_result$new_cluster_ids
    )
  }
  if (n_muted_streams > 0) {
    episodic_trace(
      "Detection suppressed on ",
      n_muted_streams,
      " muted stream(s); their existing clusters still age and close normally"
    )
  }
  if (!is.null(farrington_short) && nrow(farrington_short) > 0) {
    episodic_trace(
      "Farrington had too little history on ",
      nrow(farrington_short),
      " of the eligible stream(s) and did not run there: it needs ",
      max(farrington_short$need),
      " weeks for the configured b, and the longest of those streams has ",
      max(farrington_short$have),
      ". Lower `farrington.b` or wait for the history to accrue."
    )
  }
  episodic_trace(
    "Stream reconciliation done: ",
    n_detections_total,
    " detection(s), ",
    n_new_total,
    " new signal(s), ",
    n_updated_total,
    " updated signal(s)"
  )

  # Suppression is a statement about the lattice as a whole - which level
  # of the same outbreak is the one worth a dossier - so it waits until
  # every stream in it has reconciled.
  episodic_trace("Suppressing lattice")
  episodic_suppress_lattice(con, config)
  episodic_trace_debug(
    debug,
    "debug: memory before finishing: ",
    episodic_trace_memory()
  )

  list(
    status = if (isTRUE(activity_counts$n_skipped > 0)) {
      "partial"
    } else {
      "success"
    },
    n_streams = nrow(streams),
    n_detections = n_detections_total,
    n_signals_new = n_new_total,
    n_signals_updated = n_updated_total,
    n_cases_supplied = case_counts$n_supplied,
    n_cases_deduplicated = case_counts$n_deduplicated,
    n_cases_inserted = case_counts$n_inserted,
    n_denominators_written = denominator_counts$n_written,
    n_activity_supplied = activity_counts$n_supplied,
    n_activity_written = activity_counts$n_written,
    n_activity_skipped = activity_counts$n_skipped,
    new_cluster_ids = new_cluster_ids_all,
    error_text = NA
  )
}

#' Filter a data frame of cases down to those belonging to one stream
#'
#' L1/L2 streams filter on `pathogen` and `institution_id` (and `ward` for
#' L1). L3-L5 streams filter on `pathogen` and sample date only, an
#' approximation of the L3/L4 region derivation itself
#' (`R/lattice_enumerate.R`); a real operator-supplied PC-to-region join
#' would tighten this.
#'
#' @param cases All currently known cases.
#' @param stream A single-row stream (from `episodic_db_streams()`).
#' @return The subset of `cases` belonging to `stream`.
#' @keywords internal
#' @noRd
episodic_cases_for_stream <- function(cases, stream) {
  matches <- cases$pathogen == stream$pathogen
  if (!is.na(stream$institution_id)) {
    matches <- matches &
      !is.na(cases$institution_id) &
      cases$institution_id == stream$institution_id
  }
  if (!is.na(stream$ward)) {
    matches <- matches & !is.na(cases$ward) & cases$ward == stream$ward
  }
  # A geographic stream is its own area, not the whole catchment. Without
  # this, an area stream was handed every case in the region and reported
  # the region's counts under the area's name - one signal, and a cluster
  # per area to go with it.
  if (!is.na(stream$region_code)) {
    region <- episodic_case_region_code(cases, stream$level)
    matches <- matches & !is.na(region) & region == stream$region_code
  }
  cases[matches, ]
}

#' @keywords internal
#' @noRd
episodic_pkg_versions <- function() {
  pkgs <- c("EpiSODIC", "surveillance", "EpiEstim")
  versions <- lapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) {
      as.character(utils::packageVersion(p))
    } else {
      NA
    }
  })
  stats::setNames(versions, pkgs)
}
