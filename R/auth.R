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

#' Authentication
#'
#' A handful of accounts, hashed with `sodium::password_store()` and checked
#' with `sodium::password_verify()`. No
#' lockout, no backoff, and no TLS is implemented *by this package*: the
#' login exists to attribute assessments, not to defend against attackers,
#' and network-level access control (VPN, internal network, a reverse
#' proxy terminating TLS) is the deploying operator's own responsibility,
#' not something this package provides or assumes. Since a single R
#' process serves every session,
#' `Sys.info()[["user"]]` returns the host account rather than the
#' assessor, so the login is the only available identity source.
#'
#' Password changes and login timestamps are the one bit of per-user
#' *mutable* state in the schema, yet the app never issues an `UPDATE`.
#' Resolved the same way `episode_cluster_
#' state` already resolves it for cluster state: `episode_app_user_event`
#' is an append-only log, and the "current" password hash / login time is
#' derived from it at read time (see `episode_auth_password_hash()`,
#' `episode_auth_last_login()`), falling back to `episode_app_user`'s own
#' initial values when no event has been recorded yet.
#' @name auth
NULL

#' The password hash currently in effect for a user
#'
#' The most recent `password_change` event's hash, or the account's
#' original `password_hash` if the password has never been changed.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user A row from `episode_db_user_by_username()`/`episode_db_user_by_id()`.
#' @return A single character string.
#' @keywords internal
#' @noRd
episode_auth_password_hash <- function(con, user) {
  events <- episode_db_app_user_events(con, user$user_id)
  changes <- events[events$event_type == "password_change", ]
  if (nrow(changes) == 0) return(user$password_hash)
  changes$password_hash[nrow(changes)]
}

#' Whether a user is still required to change their password
#'
#' `TRUE` until the first `password_change` event is recorded for them,
#' mirroring `episode_app_user.must_change`'s original intent without
#' needing to `UPDATE` that column once it is satisfied.
#'
#' @inheritParams episode_auth_password_hash
#' @return A single logical.
#' @keywords internal
#' @noRd
episode_auth_must_change <- function(con, user) {
  if (!as.logical(user$must_change)) return(FALSE)
  events <- episode_db_app_user_events(con, user$user_id)
  !any(events$event_type == "password_change")
}

#' A user's most recent login time
#'
#' @inheritParams episode_auth_password_hash
#' @return An ISO-8601 string, or `NA` if the user has never logged in.
#' @keywords internal
#' @noRd
episode_auth_last_login <- function(con, user) {
  events <- episode_db_app_user_events(con, user$user_id)
  logins <- events[events$event_type == "login", ]
  if (nrow(logins) == 0) return(NA_character_)
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
episode_auth_login <- function(con, username, password) {
  rlang::check_installed("sodium")
  user <- episode_db_user_by_username(con, username)
  if (is.null(user) || !as.logical(user$is_active)) {
    return(list(ok = FALSE))
  }
  hash <- episode_auth_password_hash(con, user)
  verified <- tryCatch(sodium::password_verify(hash, password), error = function(e) FALSE)
  if (!isTRUE(verified)) {
    return(list(ok = FALSE))
  }
  episode_db_app_user_event_insert(con, user$user_id, "login")
  list(ok = TRUE, user = user, must_change = episode_auth_must_change(con, user))
}

#' Record a password change
#'
#' Appends a `password_change` event carrying the new hash; see
#' `episode_auth_password_hash()` for how this becomes "current".
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user_id The account changing its password.
#' @param new_password The new plaintext password (hashed here, never
#'   stored or logged as plaintext).
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episode_auth_change_password <- function(con, user_id, new_password) {
  rlang::check_installed("sodium")
  hash <- sodium::password_store(new_password)
  episode_db_app_user_event_insert(con, user_id, "password_change", password_hash = hash)
  invisible(NULL)
}

#' Provision an assessor account
#'
#' There is deliberately no in-app account management screen - "four
#' accounts" are provisioned outside the app,
#' by whoever administers the database, not created by assessors
#' themselves. This is that provisioning step: hashes `password` with
#' `sodium::password_store()` and inserts the account, `must_change = 1`
#' by default so the first real sign-in forces a password of the
#' account holder's own choosing (see `episode_auth_change_password()`).
#'
#' Takes `db_path` rather than an open connection - opened and closed here
#' via [episode_db_open()] - so provisioning an account is one call at the
#' console, without first having to construct a `con` by hand.
#'
#' @param db_path Path to an existing SQLite database. Defaults to the
#'   `EPISODE_DB` environment variable.
#' @param username,full_name,email The new account's fields.
#' @param password An initial plaintext password (hashed here, never
#'   stored or logged as plaintext) - a temporary one the holder is
#'   expected to change at first sign-in.
#' @param role One of `"assessor"`, `"admin"`.
#' @return Invisibly, the new `user_id`.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episode_db_create(db_path)
#' DBI::dbDisconnect(con)
#' episode_provision_user(
#'   db_path, username = "jdoe", full_name = "Jane Doe",
#'   email = "jane@example.org", password = "temporary-password"
#' )
#' file.remove(db_path)
#' @export
episode_provision_user <- function(db_path = Sys.getenv("EPISODE_DB", unset = NA),
                                    username, full_name, email, password, role = "assessor") {
  rlang::check_installed("sodium")
  con <- episode_db_open(db_path)
  on.exit(DBI::dbDisconnect(con))
  invisible(episode_db_app_user_insert(
    con, username = username, full_name = full_name, email = email,
    password_hash = sodium::password_store(password), role = role
  ))
}
