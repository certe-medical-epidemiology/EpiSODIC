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

# Where an exchange file goes to and comes from.
#
# A file path always works. Beyond it, one protocol: HTTP(S), `PUT` to
# upload and `GET` to fetch, through `httr2` (in Suggests, so called as
# `httr2::fun()`), with no dependency on a system curl binary. That one
# protocol reaches WebDAV (Nextcloud and the like), a pre-signed S3 URL,
# an Azure Blob SAS URL and any web server that accepts a PUT.
#
# Credentials are never stored. An endpoint names the environment
# variable that holds each one - a bearer token, a user name and
# password, or the URL itself when the URL is the secret (a pre-signed
# URL is) - and the variable is read at the moment of use. A URL that is
# stored may therefore carry no credentials at all: no `user:password@`,
# and no query string, which is where a signature lives.

#' The endpoint types, per direction
#' @keywords internal
#' @noRd
episodic_exchange_endpoint_types <- list(
  export = c("file", "http_put"),
  import = c("file", "http_get")
)

#' A validated description of where a file goes or comes from
#'
#' @param type `"file"`, `"http_put"` (export) or `"http_get"` (import).
#' @param location A path, or a URL carrying no credentials; `NA` when
#'   `location_env` names the URL instead.
#' @param location_env The environment variable holding the URL, for a
#'   URL that is itself a secret; `NA` otherwise.
#' @param username_env,password_env Environment variables holding HTTP
#'   basic authentication credentials, both or neither.
#' @param token_env The environment variable holding a bearer token.
#' @param direction `"export"` or `"import"`.
#' @return A list with the same fields, `NA` where unused.
#' @keywords internal
#' @noRd
episodic_exchange_endpoint <- function(type,
                                       location = NA_character_,
                                       location_env = NA_character_,
                                       username_env = NA_character_,
                                       password_env = NA_character_,
                                       token_env = NA_character_,
                                       direction = c("export", "import")) {
  direction <- match.arg(direction)
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))
  clean <- function(x) if (blank(x)) NA_character_ else trimws(as.character(x[1]))
  location <- clean(location)
  location_env <- clean(location_env)
  username_env <- clean(username_env)
  password_env <- clean(password_env)
  token_env <- clean(token_env)
  if (blank(type) || !type %in% episodic_exchange_endpoint_types[[direction]]) {
    stop(
      "An ", direction, " goes ", if (direction == "export") "to" else "from",
      " one of: ", paste(episodic_exchange_endpoint_types[[direction]], collapse = ", "),
      "; not ", if (blank(type)) "nothing" else type, ".",
      call. = FALSE
    )
  }
  for (variable in c(location_env, username_env, password_env, token_env)) {
    if (!is.na(variable) && !episodic_exchange_env_name_valid(variable)) {
      stop("\"", variable, "\" is not an environment variable name.", call. = FALSE)
    }
  }
  if (identical(type, "file")) {
    if (is.na(location)) {
      stop("A file endpoint needs a path.", call. = FALSE)
    }
    if (!is.na(location_env) || !is.na(username_env) || !is.na(password_env) || !is.na(token_env)) {
      stop("A file endpoint takes a path only, no URL variable and no credentials.", call. = FALSE)
    }
  } else {
    if (is.na(location) == is.na(location_env)) {
      stop(
        "An HTTP endpoint needs either a URL, or the name of the ",
        "environment variable that holds it, not both and not neither.",
        call. = FALSE
      )
    }
    if (!is.na(location)) {
      episodic_exchange_url_check(location, stored = TRUE)
    }
    if (is.na(username_env) != is.na(password_env)) {
      stop("Basic authentication needs both a user name variable and a password variable.", call. = FALSE)
    }
    if (!is.na(token_env) && !is.na(username_env)) {
      stop("An HTTP endpoint authenticates with a bearer token or with a user name and password, not both.", call. = FALSE)
    }
  }
  list(
    type = type,
    location = location,
    location_env = location_env,
    username_env = username_env,
    password_env = password_env,
    token_env = token_env,
    direction = direction
  )
}

#' Refuse a URL EpiSODIC may not use or may not store
#'
#' Only `http://` and `https://`. A URL to be stored may carry neither
#' `user:password@` nor a query string, since either is a credential
#' written into the database; such a URL is named by its environment
#' variable instead.
#' @param url A URL.
#' @param stored Whether the URL is to be stored.
#' @return Invisibly, `url`.
#' @keywords internal
#' @noRd
episodic_exchange_url_check <- function(url, stored) {
  if (!grepl("^https?://[^/?#]+", url, ignore.case = TRUE)) {
    episodic_exchange_fail("transport_config", "Not an http:// or https:// URL.")
  }
  if (isTRUE(stored)) {
    authority <- sub("^https?://([^/?#]*).*$", "\\1", url, ignore.case = TRUE)
    if (grepl("@", authority, fixed = TRUE) || grepl("?", url, fixed = TRUE)) {
      episodic_exchange_fail(
        "transport_config",
        "This URL carries credentials (a user name and password, or a ",
        "query string such as a signature), and nothing secret is stored ",
        "in the database. Put the URL in an environment variable and name ",
        "the variable instead."
      )
    }
  }
  invisible(url)
}

#' What an endpoint is recorded and shown as
#'
#' The path or URL where those are stored, and `"env:<VARIABLE>"` where
#' the URL is itself a secret: never the secret.
#' @param endpoint An `episodic_exchange_endpoint()`.
#' @return A single string.
#' @keywords internal
#' @noRd
episodic_exchange_endpoint_label <- function(endpoint) {
  if (!is.na(endpoint$location)) endpoint$location else paste0("env:", endpoint$location_env)
}

#' The value of an environment variable an endpoint or key names
#' @param variable The variable's name.
#' @param what What it holds, for the error.
#' @return The value.
#' @keywords internal
#' @noRd
episodic_exchange_env_value <- function(variable, what) {
  value <- Sys.getenv(variable, unset = NA)
  if (is.na(value) || !nzchar(value)) {
    episodic_exchange_fail(
      "environment",
      what, " is read from the environment variable ", variable,
      ", which is not set in the environment this process runs in."
    )
  }
  value
}

#' The URL an HTTP endpoint points at, read now
#' @keywords internal
#' @noRd
episodic_exchange_endpoint_url <- function(endpoint) {
  if (!is.na(endpoint$location)) {
    return(endpoint$location)
  }
  url <- episodic_exchange_env_value(endpoint$location_env, "The URL")
  episodic_exchange_url_check(url, stored = FALSE)
  url
}

#' An HTTP request for an endpoint, authenticated as it names
#'
#' HTTP errors are not raised by `httr2` here: the status is read and
#' reported by the caller, so a 403 says "refused" rather than surfacing
#' as an error whose text may repeat a secret URL.
#' @keywords internal
#' @noRd
episodic_exchange_http_request <- function(endpoint, method) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    episodic_exchange_fail(
      "transport_config",
      "Exchanging files over HTTP(S) needs the 'httr2' package. Install it ",
      "with install.packages(\"httr2\"), or use a file path."
    )
  }
  req <- httr2::request(episodic_exchange_endpoint_url(endpoint))
  req <- httr2::req_method(req, method)
  req <- httr2::req_user_agent(
    req,
    paste0("EpiSODIC/", as.character(utils::packageVersion("EpiSODIC")))
  )
  req <- httr2::req_timeout(req, 120)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  if (!is.na(endpoint$token_env)) {
    req <- httr2::req_auth_bearer_token(
      req,
      episodic_exchange_env_value(endpoint$token_env, "The bearer token")
    )
  } else if (!is.na(endpoint$username_env)) {
    req <- httr2::req_auth_basic(
      req,
      episodic_exchange_env_value(endpoint$username_env, "The user name"),
      episodic_exchange_env_value(endpoint$password_env, "The password")
    )
  }
  req
}

#' Perform a request, turning a failure into an exchange error
#'
#' The message names the endpoint by its label, never by a URL read from
#' a variable: `httr2`'s own error for a connection failure repeats the
#' URL, and that URL may be the secret.
#' @keywords internal
#' @noRd
episodic_exchange_http_perform <- function(req, endpoint) {
  label <- episodic_exchange_endpoint_label(endpoint)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    detail <- conditionMessage(resp)
    if (!is.na(endpoint$location_env)) {
      detail <- "the connection failed (the error text is withheld, since it would repeat the URL)"
    }
    episodic_exchange_fail("transport", "Could not reach ", label, ": ", detail)
  }
  resp
}

#' Read an exchange file from an import endpoint
#' @param endpoint An import `episodic_exchange_endpoint()`.
#' @return The file's bytes.
#' @keywords internal
#' @noRd
episodic_exchange_endpoint_read <- function(endpoint) {
  if (identical(endpoint$type, "file")) {
    return(episodic_exchange_read_bytes(endpoint$location))
  }
  resp <- episodic_exchange_http_perform(
    episodic_exchange_http_request(endpoint, "GET"),
    endpoint
  )
  status <- httr2::resp_status(resp)
  if (status < 200 || status >= 300) {
    episodic_exchange_fail(
      "transport",
      "Fetching ", episodic_exchange_endpoint_label(endpoint), " was answered with HTTP ",
      status, if (status %in% c(401L, 403L)) " (check the credentials)" else "", "."
    )
  }
  if (!httr2::resp_has_body(resp)) {
    episodic_exchange_fail(
      "transport",
      "Fetching ", episodic_exchange_endpoint_label(endpoint), " returned no file."
    )
  }
  httr2::resp_body_raw(resp)
}

#' Write an exchange file to an export endpoint
#' @param endpoint An export `episodic_exchange_endpoint()`.
#' @param bytes The file's bytes.
#' @return Invisibly, the endpoint's label.
#' @keywords internal
#' @noRd
episodic_exchange_endpoint_write <- function(endpoint, bytes) {
  if (identical(endpoint$type, "file")) {
    episodic_exchange_write_file(bytes, endpoint$location)
    return(invisible(endpoint$location))
  }
  req <- episodic_exchange_http_request(endpoint, "PUT")
  req <- httr2::req_body_raw(req, bytes, type = "application/octet-stream")
  resp <- episodic_exchange_http_perform(req, endpoint)
  status <- httr2::resp_status(resp)
  if (status < 200 || status >= 300) {
    episodic_exchange_fail(
      "transport",
      "Uploading to ", episodic_exchange_endpoint_label(endpoint), " was answered with HTTP ",
      status, if (status %in% c(401L, 403L)) " (check the credentials)" else "", "."
    )
  }
  invisible(episodic_exchange_endpoint_label(endpoint))
}

#' One line of a connection test
#'
#' Kept as a translation key and its values rather than as English, so
#' the dashboard shows it in the reader's language; `detail` carries
#' text from outside EpiSODIC (an operating system or server message),
#' which stays as it came.
#' @param check A short code for what was checked.
#' @param ok `TRUE`, `FALSE`, or `NA` for a line that informs without
#'   passing or failing.
#' @param key An `episodic_tr()` key under `exchange.test.`.
#' @param ... Values for the key's placeholders.
#' @return A list with `check`, `ok`, `key` and `params` (a named list).
#' @keywords internal
#' @noRd
episodic_exchange_test_line <- function(check, ok, key, ...) {
  list(check = check, ok = ok, key = key, params = list(...))
}

#' Test an endpoint the way the next run will use it, writing nothing
#'
#' An import endpoint is read, exactly as the next run reads it. An
#' export endpoint is never written to by a test, since that would
#' replace the file partners fetch: a path's directory is checked for
#' existence and write permission, and an HTTP endpoint is asked for
#' `OPTIONS`, which shows whether the server answers and whether the
#' credentials are refused outright. Whether an upload is accepted is
#' known only from the first real one, and the test says so rather than
#' claiming more.
#'
#' @param endpoint An `episodic_exchange_endpoint()`.
#' @return A list with `lines` (a list of `episodic_exchange_test_line()`
#'   results) and `bytes` (the file read, for an import endpoint that could be
#'   read, else `NULL`).
#' @keywords internal
#' @noRd
episodic_exchange_endpoint_test <- function(endpoint) {
  label <- episodic_exchange_endpoint_label(endpoint)
  lines <- list()
  add <- function(...) lines[[length(lines) + 1L]] <<- episodic_exchange_test_line(...)
  bytes <- NULL

  for (variable in c(endpoint$location_env, endpoint$username_env, endpoint$password_env, endpoint$token_env)) {
    if (is.na(variable)) next
    value <- Sys.getenv(variable, unset = NA)
    if (is.na(value) || !nzchar(value)) {
      add("env", FALSE, "exchange.test.env_missing", variable = variable)
    } else {
      add("env", TRUE, "exchange.test.env_set", variable = variable)
    }
  }
  if (any(vapply(lines, function(l) identical(l$ok, FALSE), logical(1)))) {
    return(list(lines = lines, bytes = NULL))
  }

  if (identical(endpoint$direction, "import")) {
    read <- tryCatch(episodic_exchange_endpoint_read(endpoint), error = function(e) e)
    if (inherits(read, "error")) {
      add("reach", FALSE, "exchange.test.read_failed", location = label, detail = conditionMessage(read))
    } else {
      bytes <- read
      add("reach", TRUE, "exchange.test.read_ok", location = label, bytes = length(read))
    }
  } else if (identical(endpoint$type, "file")) {
    dir <- dirname(endpoint$location)
    if (!dir.exists(dir)) {
      add("reach", FALSE, "exchange.test.dir_missing", location = dir)
    } else if (file.access(dir, mode = 2) != 0) {
      add("reach", FALSE, "exchange.test.dir_unwritable", location = dir)
    } else {
      add("reach", TRUE, "exchange.test.dir_ok", location = dir)
    }
  } else {
    resp <- tryCatch(
      episodic_exchange_http_perform(episodic_exchange_http_request(endpoint, "OPTIONS"), endpoint),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      add("reach", FALSE, "exchange.test.unreachable", location = label, detail = conditionMessage(resp))
    } else {
      status <- httr2::resp_status(resp)
      if (status %in% c(401L, 407L)) {
        add("reach", FALSE, "exchange.test.refused", location = label, status = as.character(status))
      } else {
        add("reach", TRUE, "exchange.test.reachable", location = label, status = as.character(status))
        add("upload", NA, "exchange.test.upload_unknown")
      }
    }
  }
  list(lines = lines, bytes = bytes)
}
