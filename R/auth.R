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

#' How sign-in works
#'
#' EpiSODIC's dashboard is used by a small board of epidemiologists (the
#' `"admin"` role - by definition they assess clusters and classify them)
#' and by `"viewer"` accounts for anyone who needs to see cluster detail,
#' including patient-level data, without classifying anything themselves.
#' Both roles sign in with a username and password; there is no
#' self-service registration - an administrator creates each account with
#' [episodic_provision_user()], and the new user sets their own password
#' on first sign-in.
#'
#' Passwords are hashed (never stored in plain text) and login history is
#' kept for audit purposes. This login exists to attribute who assessed
#' what, not to defend the system against attackers: EpiSODIC does not
#' implement TLS, account lockout, or rate limiting, so it should always be
#' deployed behind your own organisation's network controls (VPN, internal
#' network, or a reverse proxy that terminates TLS).
#'
#' @name episodic_auth
NULL

#' The password hash currently in effect for a user
#'
#' The most recent `password_change` event's hash, or the account's
#' original `password_hash` if the password has never been changed.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user A row from `episodic_db_user_by_username()`/`episodic_db_user_by_id()`.
#' @return A single character string.
#' @keywords internal
#' @noRd
episodic_auth_password_hash <- function(con, user) {
  events <- episodic_db_app_user_events(con, user$user_id)
  changes <- events[events$event_type == "password_change", ]
  if (nrow(changes) == 0) {
    return(user$password_hash)
  }
  changes$password_hash[nrow(changes)]
}

#' Whether a user is still required to change their password
#'
#' `TRUE` until the first `password_change` event is recorded for them,
#' mirroring `episodic_app_user.must_change`'s original intent without
#' needing to `UPDATE` that column once it is satisfied.
#'
#' @inheritParams episodic_auth_password_hash
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_auth_must_change <- function(con, user) {
  if (!as.logical(user$must_change)) {
    return(FALSE)
  }
  events <- episodic_db_app_user_events(con, user$user_id)
  !any(events$event_type == "password_change")
}

#' A user's most recent login time
#'
#' @inheritParams episodic_auth_password_hash
#' @return An ISO-8601 string, or `NA` if the user has never logged in.
#' @keywords internal
#' @noRd
episodic_auth_last_login <- function(con, user) {
  events <- episodic_db_app_user_events(con, user$user_id)
  logins <- events[events$event_type == "login", ]
  if (nrow(logins) == 0) {
    return(NA_character_)
  }
  logins$created_at[nrow(logins)]
}

#' Attempt to log in
#'
#' Verifies `username`/`password` against the account's current password
#' hash and, on success, records a
#' `login` event. Deliberately silent about *why* a login failed (unknown
#' username vs. wrong password vs. inactive account are folded into the
#' same generic outcome) - the login exists to attribute assessments made
#' by people already inside the building, not to resist an attacker who
#' would learn more from a distinguishing error message.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param username,password Login credentials.
#' @return A list: `ok` (logical), and if `ok` is `TRUE`, `user` (the
#'   account row) and `must_change` (logical, whether a forced password
#'   change is due).
#' @keywords internal
#' @noRd
episodic_auth_login <- function(con, username, password) {
  rlang::check_installed("sodium")
  user <- episodic_db_user_by_username(con, username)
  if (is.null(user) || !as.logical(user$is_active)) {
    return(list(ok = FALSE))
  }
  hash <- episodic_auth_password_hash(con, user)
  verified <- tryCatch(
    sodium::password_verify(hash, password),
    error = function(e) FALSE
  )
  if (!isTRUE(verified)) {
    return(list(ok = FALSE))
  }
  episodic_db_app_user_event_insert(con, user$user_id, "login")
  list(
    ok = TRUE,
    user = user,
    must_change = episodic_auth_must_change(con, user)
  )
}

#' Record a password change
#'
#' Appends a `password_change` event carrying the new hash; see
#' `episodic_auth_password_hash()` for how this becomes "current".
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user_id The account changing its password.
#' @param new_password The new plaintext password (hashed here, never
#'   stored or logged as plaintext).
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_auth_change_password <- function(con, user_id, new_password) {
  rlang::check_installed("sodium")
  hash <- sodium::password_store(new_password)
  episodic_db_app_user_event_insert(
    con,
    user_id,
    "password_change",
    password_hash = hash
  )
  invisible(NULL)
}

#' Create an account for a new admin or viewer
#'
#' There is no self-service registration and no in-app account management
#' screen: whoever administers the database creates accounts with this
#' function, typically once per new board member. The password you supply
#' is temporary - the new account is flagged to require a password change,
#' so the account holder chooses their own password the first time they
#' sign in.
#'
#' You only need to run this once per person. It opens the database, adds
#' the account, and closes the connection again, so it is meant to be run
#' interactively at the R console rather than from application code.
#'
#' @param db_path Path to the EpiSODIC database: an existing SQLite file, or
#'   a MariaDB/MySQL DSN (see [episodic_db_dsn_mariadb()]). Defaults to the
#'   `EPISODIC_DB` environment variable.
#' @param username,full_name,email The new account's sign-in name, display
#'   name, and email address.
#' @param password A temporary plaintext password, never stored or logged
#'   as-is: it is hashed before it reaches the database, and the account
#'   holder is required to replace it on first sign-in.
#' @param role Either `"admin"` (an epidemiologist: can classify and close
#'   clusters, in addition to everything a viewer can do) or `"viewer"`
#'   (read-only - can see everything a signed-in admin sees, including
#'   patient-level line lists, but cannot record an assessment). Both
#'   roles require sign-in; there is no anonymous access to patient
#'   detail.
#' @return Invisibly, the new account's `user_id`.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episodic_db_create(db_path)
#' DBI::dbDisconnect(con)
#'
#' user_id <- episodic_provision_user(
#'   db_path,
#'   username = "jdoe", full_name = "Jane Doe",
#'   email = "jane@example.org", password = "temporary-password"
#' )
#' user_id
#'
#' file.remove(db_path)
#' @export
episodic_provision_user <- function(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  username,
  full_name,
  email,
  password,
  role = "admin"
) {
  rlang::check_installed("sodium")
  role <- match.arg(role, c("admin", "viewer"))
  con <- episodic_db_open(db_path)
  on.exit(DBI::dbDisconnect(con))
  invisible(episodic_db_app_user_insert(
    con,
    username = username,
    full_name = full_name,
    email = email,
    password_hash = sodium::password_store(password),
    role = role
  ))
}

#' Whether a signed-in user may classify, close, or otherwise write
#'
#' The only role distinction the app enforces: `"admin"` accounts (the
#' epidemiologists who assess clusters and classify them) may write,
#' `"viewer"` accounts may not. Both roles see identical read access,
#' including patient-level detail - `NULL` (nobody signed in) is the only
#' case with no patient detail at all, handled separately by each panel's
#' own `is.null(current_user)` check.
#'
#' @param user The session's signed-in user row, or `NULL`.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_user_is_admin <- function(user) {
  !is.null(user) && identical(user$role, "admin")
}
