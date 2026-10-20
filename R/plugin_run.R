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

# Running a plugin: each step in a process of its own, never in the
# dashboard's or the cron's own R session. A step sees only an
# environment built from an allowlist, the secrets the plugin was
# granted, its own plain variables and the two files that are its
# interface: the payload it reads (EPISODIC_PAYLOAD) and the result it
# writes (EPISODIC_RESULT). Nothing is pasted into a step's code, so no
# value from the data can change what the code does.

#' Environment variables a step inherits from the process that runs it
#'
#' What a program needs to start and to reach the network through the
#' proxies and certificate stores hospital networks commonly require, and
#' nothing else: none of EpiSODIC's own variables (database, mail and
#' Microsoft 365 credentials), and none of the server's other secrets.
#' @keywords internal
#' @noRd
episodic_plugin_env_allowlist <- c(
  "PATH", "HOME", "USER", "LOGNAME", "LANG", "LANGUAGE", "TZ",
  "LC_ALL", "LC_CTYPE", "LC_COLLATE", "LC_MESSAGES", "LC_NUMERIC", "LC_TIME", "LC_MONETARY",
  "R_LIBS_USER", "R_LIBS_SITE",
  "http_proxy", "https_proxy", "no_proxy", "HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY",
  "SSL_CERT_FILE", "SSL_CERT_DIR", "CURL_CA_BUNDLE", "REQUESTS_CA_BUNDLE",
  "SYSTEMROOT", "SystemRoot", "WINDIR", "COMSPEC", "PATHEXT", "USERPROFILE",
  "APPDATA", "LOCALAPPDATA", "PROGRAMDATA", "ProgramFiles", "ProgramFiles(x86)"
)

#' The values of the named environment variables, named, `NA` when unset
#'
#' `Sys.getenv()` given no names returns every variable the process has,
#' so asking for "these secrets" when there are none would hand a plugin
#' the server's whole environment. No names here means no values.
#' @keywords internal
#' @noRd
episodic_plugin_getenv <- function(names) {
  if (length(names) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  Sys.getenv(names, unset = NA, names = TRUE)
}

#' The environment a plugin's step runs with
#'
#' @param plugin The plugin, from `episodic_plugin_read()`.
#' @param grant What the configuration grants it (`episodic_plugin_grant()`).
#' @param step The step.
#' @param work_dir The run's working directory.
#' @param lang The language a plugin should write its message in.
#' @return A named character vector.
#' @keywords internal
#' @noRd
episodic_plugin_environment <- function(plugin, grant, step, work_dir, lang) {
  inherited <- episodic_plugin_getenv(episodic_plugin_env_allowlist)
  env <- inherited[!is.na(inherited)]
  secrets <- intersect(plugin$secrets, grant$secrets)
  values <- episodic_plugin_getenv(secrets)
  missing <- secrets[is.na(values) | !nzchar(values)]
  if (length(missing) > 0) {
    stop(
      "The plugin's secret(s) ", paste(missing, collapse = ", "), " are not set in ",
      "the environment this process runs in.",
      call. = FALSE
    )
  }
  tmp <- file.path(work_dir, "tmp")
  dir.create(tmp, showWarnings = FALSE)
  c(
    env,
    step$env,
    values,
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
    TMPDIR = tmp, TMP = tmp, TEMP = tmp,
    EPISODIC_PAYLOAD = file.path(work_dir, "payload.json"),
    EPISODIC_RESULT = file.path(work_dir, "result.txt"),
    EPISODIC_PLUGIN_ID = plugin$id,
    EPISODIC_LANGUAGE = lang
  )
}

#' Read what a plugin reported
#'
#' Lines of `key=value`: `status` (`success` or `error`), `message` and
#' `link` (an http or https address). A later line overrides an earlier
#' one. Blank lines are ignored; any other line is a mistake in the
#' plugin and is reported as one rather than skipped.
#'
#' @param path The result file.
#' @return A list with `status`, `message` and `link`, or a condition.
#' @keywords internal
#' @noRd
episodic_plugin_result_read <- function(path) {
  result <- list(status = "success", message = NA_character_, link = NA_character_)
  if (!file.exists(path)) {
    return(result)
  }
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- sub("\r$", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  for (line in lines) {
    if (!grepl("^(status|message|link)=", line)) {
      stop("The plugin wrote a result line EpiSODIC does not understand: ", substr(line, 1, 80), call. = FALSE)
    }
    key <- sub("=.*$", "", line)
    result[[key]] <- trimws(sub("^[^=]*=", "", line))
  }
  if (!result$status %in% c("success", "error")) {
    stop("The plugin reported status '", substr(result$status, 1, 40), "'; it must be success or error.", call. = FALSE)
  }
  if (!is.na(result$link) && !grepl("^https?://[^[:space:]]+$", result$link)) {
    stop("The plugin reported a link that is not an http or https address.", call. = FALSE)
  }
  if (!is.na(result$message)) {
    result$message <- substr(result$message, 1, 1000)
  }
  result
}

#' Mask granted secret values in text a plugin produced
#' @keywords internal
#' @noRd
episodic_plugin_mask <- function(text, secrets) {
  for (value in secrets) {
    if (!is.na(value) && nchar(value) >= 4) {
      text <- gsub(value, "***", text, fixed = TRUE)
    }
  }
  text
}

#' The end of a step's error output, for the record
#' @keywords internal
#' @noRd
episodic_plugin_tail <- function(text, n = 2000L) {
  text <- trimws(text %||% "")
  if (nchar(text) > n) paste0("...", substr(text, nchar(text) - n + 1L, nchar(text))) else text
}

#' Run a plugin's steps against a payload
#'
#' Steps run in order, each as a process of its own with
#' `episodic_plugin_environment()`, under one deadline for the whole
#' plugin; whatever a step starts is stopped with it. A step that exits
#' with an error, or the deadline passing, ends the run there.
#'
#' @param plugin The plugin, from `episodic_plugin_read()`, with no problems.
#' @param grant What the configuration grants it.
#' @param payload The payload, a list written as JSON.
#' @param lang The language a plugin should write its message in.
#' @return A list with `status` (`"success"`, `"error"`, `"failed"` or
#'   `"timeout"`), `message`, `link`, `step` (the step that ended the
#'   run, or `NA`), `detail` (the end of its error output, secrets
#'   masked, or `NA`) and `duration` in seconds. Never raises.
#' @keywords internal
#' @noRd
episodic_plugin_execute <- function(plugin, grant, payload, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  started <- Sys.time()
  outcome <- function(status, message = NA_character_, link = NA_character_, step = NA_character_, detail = NA_character_) {
    list(
      status = status, message = message, link = link, step = step, detail = detail,
      duration = as.numeric(difftime(Sys.time(), started, units = "secs"))
    )
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    return(outcome("failed", detail = "The 'processx' package is needed to run plugins and is not installed."))
  }
  work_dir <- tempfile("episodic_plugin_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  written <- tryCatch(
    {
      jsonlite::write_json(payload, file.path(work_dir, "payload.json"), auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(written, "error")) {
    return(outcome("failed", detail = paste("The payload could not be written:", conditionMessage(written))))
  }
  secret_values <- episodic_plugin_getenv(intersect(plugin$secrets, grant$secrets))
  deadline <- started + plugin$timeout
  extension <- c(r = ".R", bash = ".sh", python = ".py")

  for (i in seq_along(plugin$steps)) {
    step <- plugin$steps[[i]]
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) {
      return(outcome("timeout", step = step$name))
    }
    env <- tryCatch(episodic_plugin_environment(plugin, grant, step, work_dir, episodic_lang(lang)), error = function(e) e)
    if (inherits(env, "error")) {
      return(outcome("failed", step = step$name, detail = conditionMessage(env)))
    }
    script <- file.path(work_dir, paste0("step", i, extension[[step$shell]]))
    writeLines(enc2utf8(step$run), script, useBytes = TRUE)
    program <- episodic_plugin_shell_program(step$shell)
    if (!nzchar(program)) {
      return(outcome("failed", step = step$name, detail = paste0("No program is available to run a ", step$shell, " step.")))
    }
    run <- tryCatch(
      processx::run(
        program,
        args = script,
        wd = work_dir,
        env = env,
        timeout = remaining,
        error_on_status = FALSE,
        cleanup_tree = TRUE,
        encoding = "UTF-8"
      ),
      error = function(e) e
    )
    if (inherits(run, "error")) {
      return(outcome("failed", step = step$name, detail = episodic_plugin_mask(conditionMessage(run), secret_values)))
    }
    if (isTRUE(run$timeout)) {
      return(outcome("timeout", step = step$name, detail = episodic_plugin_mask(episodic_plugin_tail(run$stderr), secret_values)))
    }
    if (!identical(run$status, 0L)) {
      return(outcome(
        "failed",
        step = step$name,
        detail = episodic_plugin_mask(
          episodic_plugin_tail(paste0("Exit status ", run$status, ". ", run$stderr)),
          secret_values
        )
      ))
    }
  }

  result <- tryCatch(episodic_plugin_result_read(file.path(work_dir, "result.txt")), error = function(e) e)
  if (inherits(result, "error")) {
    return(outcome("failed", detail = conditionMessage(result)))
  }
  outcome(
    result$status,
    message = episodic_plugin_mask(result$message, secret_values),
    link = result$link
  )
}
