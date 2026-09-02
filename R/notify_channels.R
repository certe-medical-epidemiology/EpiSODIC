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
    Title = message$title,
    Priority = as.character(channel$priority %||% 4L)
  )
  req <- httr2::req_body_raw(req, message$plain, type = "text/plain")

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
    message$html
  )

  smtp_url <- paste0("smtp://", host, ":", port)

  msg_file <- tempfile(fileext = ".eml")
  on.exit(unlink(msg_file), add = TRUE)
  writeLines(mail_body, msg_file, useBytes = TRUE)

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

#' Build a MIME message for SMTP/sendmail
#' @keywords internal
#' @noRd
episodic_notify_mime_message <- function(from, to, subject, html_body) {
  to_header <- paste(to, collapse = ", ")
  paste0(
    "From: ",
    from,
    "\r\n",
    "To: ",
    to_header,
    "\r\n",
    "Subject: ",
    subject,
    "\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: text/html; charset=UTF-8\r\n",
    "Content-Transfer-Encoding: 8bit\r\n",
    "\r\n",
    html_body
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
    message$html
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
  email$send()
  invisible(NULL)
}

#' Send via Teams (Power Automate Workflow webhook)
#' @keywords internal
#' @noRd
episodic_notify_teams <- function(channel, message) {
  rlang::check_installed("httr2")

  req <- httr2::request(channel$webhook_url)
  req <- httr2::req_body_raw(req, message$teams_card, type = "application/json")
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
  req <- httr2::req_body_raw(req, payload, type = "application/json")
  httr2::req_perform(req)
  invisible(NULL)
}
