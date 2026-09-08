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

# Per-channel notification send functions. Each takes a channel config
# (the YAML subtree under notifications.channels.<name>) and a message
# (the list built by episodic_notify_build_new_clusters() or
# episodic_notify_build_failure()). Errors propagate to the caller, which
# catches and logs them.

#' Send via ntfy
#' @keywords internal
#' @noRd
episodic_notify_ntfy <- function(channel, message) {
  rlang::check_installed("httr2")
  server <- channel$server
  topic <- channel$topic
  url <- paste0(sub("/$", "", server), "/", topic)

  req <- httr2::request(url)
  req <- httr2::req_headers(
    req,
    # ntfy reads Title as an HTTP header, which is ASCII; it accepts an
    # RFC 2047 encoded-word there, which is what a title in any of the
    # seven non-English languages EpiSODIC ships needs.
    Title = episodic_notify_header_encode(message$title),
    Priority = as.character(channel$priority %||% 4L)
  )
  req <- httr2::req_body_raw(
    req,
    charToRaw(enc2utf8(message$plain)),
    type = "text/plain; charset=utf-8"
  )

  auth <- channel$auth
  if (
    !is.null(auth$username) &&
      nzchar(auth$username %||% "") &&
      !is.null(auth$password) &&
      nzchar(auth$password %||% "")
  ) {
    req <- httr2::req_auth_basic(req, auth$username, auth$password)
  }

  httr2::req_perform(req)
  invisible(NULL)
}

#' Send via SMTP using curl
#' @keywords internal
#' @noRd
episodic_notify_smtp <- function(channel, message) {
  rlang::check_installed("curl")

  host <- channel$host
  port <- channel$port %||% 587L
  use_tls <- isTRUE(channel$tls %||% TRUE)
  from <- channel$from
  to <- channel$to
  username <- channel$username
  password <- channel$password

  mail_body <- episodic_notify_mime_message(
    from,
    to,
    message$title,
    message$html,
    attachment_path = message$attachment_path
  )

  smtp_url <- paste0("smtp://", host, ":", port)

  msg_file <- tempfile(fileext = ".eml")
  on.exit(unlink(msg_file), add = TRUE)
  # `cat()`, not `writeLines()`: the message already ends in the CRLF its
  # own format requires, and `writeLines()` would append a bare LF after
  # the terminating boundary. The message is 7-bit ASCII by construction
  # now (`episodic_notify_mime_message()` base64-encodes both the headers
  # that need it and the body), so no encoding conversion can apply.
  cat(mail_body, file = msg_file, sep = "")

  curl::send_mail(
    mail_from = from,
    mail_rcpt = to,
    message = msg_file,
    smtp_server = smtp_url,
    username = username %||% "",
    password = password %||% "",
    use_ssl = if (use_tls) "try" else "no",
    verbose = FALSE
  )
  invisible(NULL)
}

#' Encode a header value that may contain non-ASCII text (RFC 2047)
#'
#' Message headers are ASCII. EpiSODIC renders its dashboard and its
#' reports in eight languages, four of which are not written in the Latin
#' alphabet at all, so an Arabic, Hindi or Chinese subject line - or
#' merely a Dutch or Spanish one carrying a single accented character -
#' is the normal case rather than an edge one, and putting those bytes
#' into a header raw, as this used to,
#' gets them mangled into replacement characters by some relays and the
#' whole message rejected by others. Pure-ASCII values are left exactly
#' as they are, so nothing changes for an English instance.
#'
#' Encoded as one base64 "encoded-word" rather than split at the 75
#' characters RFC 2047 nominally allows: splitting a UTF-8 subject
#' safely means splitting on character boundaries that base64 hides, and
#' every mail client in practice accepts the long form. `\r` and `\n`
#' are stripped first regardless of alphabet - a newline in a header
#' value is header injection, and a subject line is built from a
#' pathogen name and a location that ultimately came from an operator's
#' own data feed.
#'
#' @param value A single header value.
#' @return The value, encoded if it needs to be.
#' @keywords internal
#' @noRd
episodic_notify_header_encode <- function(value) {
  value <- gsub("[\r\n]", " ", paste(value, collapse = " "))
  if (!any(grepl("[^\x01-\x7F]", value, useBytes = TRUE))) {
    return(value)
  }
  paste0(
    "=?UTF-8?B?",
    jsonlite::base64_enc(charToRaw(enc2utf8(value))),
    "?="
  )
}

#' Wrap base64 at the line length RFC 2045 requires
#'
#' `jsonlite::base64_enc()` returns one unbroken string; a MIME body part
#' whose lines exceed 998 octets is not a valid message and is rejected
#' or silently truncated by strict relays, which for a report attachment
#' of any size is every time.
#' @param x A base64 string.
#' @param width Line length; 76 is the RFC 2045 maximum.
#' @return The same base64, CRLF-wrapped.
#' @keywords internal
#' @noRd
episodic_notify_base64_wrap <- function(x, width = 76L) {
  x <- gsub("[\r\n]", "", paste(x, collapse = ""))
  if (!nzchar(x)) {
    return(x)
  }
  starts <- seq(1L, nchar(x), by = width)
  paste(substring(x, starts, starts + width - 1L), collapse = "\r\n")
}

#' Build a MIME message for SMTP/sendmail
#'
#' Two shapes, chosen by whether `attachment_path` is given: a plain
#' `text/html` message (every ordinary notification: new clusters, run
#' failures), or a `multipart/mixed` message carrying that same HTML
#' body alongside one file attachment (a scheduled report - see
#' `R/scheduled_reports.R`), base64-encoded, `Content-Disposition:
#' attachment` so it opens as a file rather than rendering inline.
#'
#' Both the headers and the HTML body are transfer-encoded rather than
#' sent as raw 8-bit octets. `8BITMIME` is an SMTP extension a relay is
#' free not to advertise, and a relay that does not will either strip the
#' high bit or refuse the message - so a Dutch cluster location, a
#' Spanish pathogen name or an entire Arabic or Chinese report email was
#' arriving corrupted or not at all, depending on the hop. Base64 is
#' 7-bit clean by construction and is understood everywhere.
#'
#' @param from,to,subject,html_body As sent to the recipient.
#' @param attachment_path Path to a file to attach, or `NULL` (default)
#'   for a plain message.
#' @param attachment_type The attachment's MIME type.
#' @keywords internal
#' @noRd
episodic_notify_mime_message <- function(from,
                                         to,
                                         subject,
                                         html_body,
                                         attachment_path = NULL,
                                         attachment_type = "text/html") {
  headers <- paste0(
    "From: ", episodic_notify_header_encode(from), "\r\n",
    "To: ", episodic_notify_header_encode(paste(to, collapse = ", ")), "\r\n",
    "Subject: ", episodic_notify_header_encode(subject), "\r\n",
    "MIME-Version: 1.0\r\n"
  )
  body_b64 <- episodic_notify_base64_wrap(
    jsonlite::base64_enc(charToRaw(enc2utf8(html_body)))
  )

  if (is.null(attachment_path)) {
    return(paste0(
      headers,
      "Content-Type: text/html; charset=UTF-8\r\n",
      "Content-Transfer-Encoding: base64\r\n",
      "\r\n",
      body_b64,
      "\r\n"
    ))
  }

  # A boundary that cannot plausibly collide with anything in the HTML
  # body or the base64-encoded attachment that follows it.
  boundary <- paste0("EpiSODIC-", digest::digest(list(subject, attachment_path, Sys.time())))
  attachment_raw <- readBin(attachment_path, what = "raw", n = file.size(attachment_path))
  attachment_b64 <- episodic_notify_base64_wrap(
    jsonlite::base64_enc(attachment_raw)
  )
  # A filename is not a header value and gets no encoded-word: a
  # `filename*` parameter (RFC 2231) is the correct spelling for a
  # non-ASCII one, and EpiSODIC's own render names files
  # `cluster-<id>-v<n>.html`, which is ASCII by construction.
  filename <- gsub('["\r\n]', "", basename(attachment_path))

  paste0(
    headers,
    "Content-Type: multipart/mixed; boundary=\"", boundary, "\"\r\n",
    "\r\n",
    "--", boundary, "\r\n",
    "Content-Type: text/html; charset=UTF-8\r\n",
    "Content-Transfer-Encoding: base64\r\n",
    "\r\n",
    body_b64,
    "\r\n\r\n",
    "--", boundary, "\r\n",
    "Content-Type: ", attachment_type, "; charset=UTF-8; name=\"", filename, "\"\r\n",
    "Content-Transfer-Encoding: base64\r\n",
    "Content-Disposition: attachment; filename=\"", filename, "\"\r\n",
    "\r\n",
    attachment_b64,
    "\r\n",
    "--", boundary, "--\r\n"
  )
}

#' Send via the system sendmail binary
#' @keywords internal
#' @noRd
episodic_notify_sendmail <- function(channel, message) {
  binary <- channel$binary %||% "/usr/sbin/sendmail"
  if (!file.exists(binary)) {
    stop("sendmail binary not found at ", binary, call. = FALSE)
  }

  from <- channel$from
  to <- channel$to

  mail_body <- episodic_notify_mime_message(
    from,
    to,
    message$title,
    message$html,
    attachment_path = message$attachment_path
  )

  args <- c("-f", from, to)
  result <- system2(
    binary,
    args = args,
    input = mail_body,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "sendmail exited with status ",
      status,
      ": ",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Find a cached Azure AD token for a tenant
#'
#' Looks up a Microsoft Graph login already cached on disk (by
#' [AzureGraph::create_graph_login()], `Microsoft365R::get_business_outlook()`,
#' or [episodic_setup_microsoft365()]) instead of starting a new interactive
#' or client-credentials login. This is how a laboratory that authenticates
#' its staff via a shared department login (rather than a per-app client
#' secret) can reuse that already-cached token for unattended sending: as
#' long as a valid token for the configured `tenant_id` exists in
#' `AzureAuth::AzureR_dir()` (overridable via the `R_AZURE_DATA_DIR`
#' environment variable), no `client_id` or `client_secret` is needed at
#' all. Matching is done against each cached login's tenant, allowing a
#' short tenant name (e.g. `"contoso"`) to match a full tenant domain
#' (`"contoso.onmicrosoft.com"`) or GUID.
#' @keywords internal
#' @noRd
episodic_notify_microsoft365_cached_token <- function(tenant_id) {
  rlang::check_installed("AzureAuth")
  logins <- unlist(AzureGraph::list_graph_logins(), recursive = FALSE)
  matches <- Filter(
    function(login) {
      login_tenant <- tryCatch(login$tenant, error = function(e) NULL)
      !is.null(login_tenant) &&
        nzchar(login_tenant) &&
        (grepl(tolower(tenant_id), tolower(login_tenant), fixed = TRUE) ||
          grepl(tolower(login_tenant), tolower(tenant_id), fixed = TRUE))
    },
    logins
  )
  if (length(matches) == 0) {
    stop(
      "microsoft365: no cached Azure AD token found for tenant '",
      tenant_id,
      "' in ",
      AzureAuth::AzureR_dir(),
      ". Sign in once with episodic_setup_microsoft365(tenant_id = \"",
      tenant_id,
      "\") (or Microsoft365R::get_business_outlook()) on this server, ",
      "or configure 'client_secret' for unattended app-only access.",
      call. = FALSE
    )
  }
  matches[[1]]$token
}

#' Send via Microsoft 365 (Graph API)
#' @keywords internal
#' @noRd
episodic_notify_microsoft365 <- function(channel, message) {
  rlang::check_installed("Microsoft365R")
  rlang::check_installed("AzureGraph")

  tenant_id <- channel$tenant_id
  client_id <- channel$client_id
  client_secret <- channel$client_secret
  from <- channel$from
  to <- channel$to

  if (!is.null(client_secret) && nzchar(client_secret %||% "")) {
    # app-only (client credentials): the app registration itself sends,
    # so the mailbox to send from must be named explicitly.
    args <- list(
      tenant = tenant_id,
      auth_type = "client_credentials",
      password = client_secret
    )
    if (!is.null(client_id) && nzchar(client_id %||% "")) {
      args$app <- client_id
    }
    gr <- do.call(AzureGraph::create_graph_login, args)
    user <- gr$get_user(from)
    outl <- user$get_outlook()
  } else if (!is.null(client_id) && nzchar(client_id %||% "")) {
    # delegated, own app registration: reuses a cached login for this
    # tenant/app/scopes combination, or starts an interactive login.
    outl <- Microsoft365R::get_business_outlook(tenant = tenant_id, app = client_id)
  } else {
    # delegated, no app registration configured: reuse whatever Azure AD
    # login is already cached on disk for this tenant (e.g. a token
    # obtained by staff signing in through Microsoft365R for other
    # purposes). Never starts a new interactive login. If 'from' names a
    # mailbox other than the signed-in user's own, it is opened as a
    # shared mailbox; whether the cached token actually carries
    # permission to send from it is between the operator and Azure AD,
    # not something this package can check up front.
    token <- episodic_notify_microsoft365_cached_token(tenant_id)
    if (!is.null(from) && nzchar(from %||% "")) {
      outl <- Microsoft365R::get_business_outlook(
        tenant = tenant_id,
        token = token,
        shared_mbox_email = from
      )
    } else {
      outl <- Microsoft365R::get_business_outlook(tenant = tenant_id, token = token)
    }
  }

  email <- outl$create_email(
    body = message$html,
    content_type = "html",
    subject = message$title,
    to = to
  )
  if (!is.null(message$attachment_path)) {
    email$add_attachment(message$attachment_path)
  }
  email$send()
  invisible(NULL)
}

#' Send via Teams (Power Automate Workflow webhook)
#' @keywords internal
#' @noRd
episodic_notify_teams <- function(channel, message) {
  rlang::check_installed("httr2")

  req <- httr2::request(channel$webhook_url)
  req <- httr2::req_body_raw(
    req,
    charToRaw(enc2utf8(as.character(message$teams_card))),
    type = "application/json; charset=utf-8"
  )
  httr2::req_perform(req)
  invisible(NULL)
}

#' Send via Slack (Incoming Webhook)
#' @keywords internal
#' @noRd
episodic_notify_slack <- function(channel, message) {
  rlang::check_installed("httr2")

  payload <- jsonlite::toJSON(
    list(text = message$slack_text),
    auto_unbox = TRUE
  )

  req <- httr2::request(channel$webhook_url)
  req <- httr2::req_body_raw(
    req,
    charToRaw(enc2utf8(as.character(payload))),
    type = "application/json; charset=utf-8"
  )
  httr2::req_perform(req)
  invisible(NULL)
}
