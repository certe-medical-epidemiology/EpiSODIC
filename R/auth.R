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
#' `"epidemiologist"` role - by definition, they assess clusters and
#' classify them) and by `"viewer"` accounts for anyone who needs to see
#' cluster detail, including patient-level data, without classifying
#' anything themselves. Both roles sign in with a username and password;
#' there is no self-service registration - an administrator creates each
#' account with [episodic_add_user()], and the new user sets their
#' own password on first sign-in.
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

#' The most recent value of an insert-only account attribute
#'
#' Generalises the pattern `episodic_auth_password_hash()` established for
#' `password_hash`: an account attribute mutated only by appending an event
#' (never by `UPDATE`) is "whatever the most recent matching event set it
#' to, or the account row's own original value if no such event exists
#' yet". Used for `role`, `is_admin`, and `is_active`, each mutated by its
#' own event type and its own `new_*` column.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user A row from `episodic_db_user_by_username()`/`episodic_db_user_by_id()`.
#' @param event_type The event type carrying this attribute's changes.
#' @param new_column The `episodic_app_user_event` column holding the new value.
#' @param original_column The `episodic_app_user` column holding the original value.
#' @return A single value (whatever type `original_column` is).
#' @keywords internal
#' @noRd
episodic_auth_latest_value <- function(
    con,
    user,
    event_type,
    new_column,
    original_column) {
  events <- episodic_db_app_user_events(con, user$user_id)
  matching <- events[events$event_type == event_type, ]
  if (nrow(matching) == 0) {
    return(user[[original_column]])
  }
  matching[[new_column]][nrow(matching)]
}

#' The role currently in effect for a user
#'
#' @inheritParams episodic_auth_password_hash
#' @return `"epidemiologist"` or `"viewer"`.
#' @keywords internal
#' @noRd
episodic_auth_role <- function(con, user) {
  episodic_auth_latest_value(con, user, "role_change", "new_role", "role")
}

#' Whether a user currently has admin (Settings screen) access
#'
#' @inheritParams episodic_auth_password_hash
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_auth_is_admin <- function(con, user) {
  as.logical(episodic_auth_latest_value(
    con,
    user,
    "admin_change",
    "new_is_admin",
    "is_admin"
  ))
}

#' Whether a user's account is currently active
#'
#' @inheritParams episodic_auth_password_hash
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_auth_is_active <- function(con, user) {
  as.logical(episodic_auth_latest_value(
    con,
    user,
    "active_change",
    "new_is_active",
    "is_active"
  ))
}

#' Resolve every insert-only attribute of an account row at once
#'
#' `episodic_auth_login()` and the Settings screen's user list both need
#' `role`/`is_admin`/`is_active` as they currently stand, not as the
#' account was first added - this bundles the three
#' `episodic_auth_latest_value()` calls into the row itself so callers
#' downstream (e.g. `episodic_user_is_epidemiologist()`,
#' `episodic_user_is_admin()`) can keep reading `user$role`/`user$is_admin`
#' directly.
#'
#' @inheritParams episodic_auth_password_hash
#' @return `user`, with `role`, `is_admin`, and `is_active` replaced by
#'   their currently-resolved values.
#' @keywords internal
#' @noRd
episodic_auth_resolve_user <- function(con, user) {
  if (is.null(user)) {
    return(NULL)
  }
  user$role <- episodic_auth_role(con, user)
  user$is_admin <- as.integer(episodic_auth_is_admin(con, user))
  user$is_active <- as.integer(episodic_auth_is_active(con, user))
  user
}

#' Re-resolve a cached session user against the database, for a write check
#'
#' `current_user()` is a `shiny::reactiveVal` set once, at sign-in - it is
#' not itself invalidated by an `is_admin` account later revoking that
#' same account's access (`episodic_auth_set_active(..., FALSE)`) or
#' demoting it (`episodic_auth_set_role()`/`episodic_auth_set_admin()`)
#' from the Settings screen. Every privileged write path
#' (`episodic_app_server_assessment_actions()`, `episodic_app_server_report()`,
#' `episodic_app_server_settings()`) calls this immediately before its own
#' `episodic_user_is_epidemiologist()`/`episodic_user_is_admin()` check, so
#' authorization is decided from the database as of *this* write, not from
#' whatever was true when the browser tab first signed in - an
#' already-open session cannot keep writing once deactivated or demoted,
#' even though its own UI may still show as signed in until the next
#' unrelated re-render notices.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param cached_user The session's cached signed-in user row (from
#'   `current_user()`), or `NULL`.
#' @return The freshly resolved account row (role/is_admin/is_active as of
#'   now), or `NULL` if nobody was signed in, the account no longer
#'   exists, or the account is no longer active.
#' @keywords internal
#' @noRd
episodic_auth_refresh_user <- function(con, cached_user) {
  if (is.null(cached_user)) {
    return(NULL)
  }
  fresh <- episodic_db_user_by_id(con, cached_user$user_id)
  if (is.null(fresh) || !episodic_auth_is_active(con, fresh)) {
    return(NULL)
  }
  episodic_auth_resolve_user(con, fresh)
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
  if (identical(user$username, "demo") && isTRUE(sodium::password_verify(user$password_hash, "demo"))) {
    # No required password change for demo user
    return(FALSE)
  }
  if (!as.logical(user$must_change)) {
    return(FALSE)
  }
  events <- episodic_db_app_user_events(con, user$user_id)
  !any(events$event_type == "password_change")
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
  if (is.null(user) || !episodic_auth_is_active(con, user)) {
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
    user = episodic_auth_resolve_user(con, user),
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

#' Create an account for a new epidemiologist or viewer
#'
#' There is no self-service registration: an account is added either
#' from the Settings screen (by an `is_admin` account) or with this
#' function at the R console. The password you supply is temporary - the
#' new account is flagged to require a password change, so the account
#' holder chooses their own password the first time they sign in.
#'
#' At the console, you only need to run this once per person. It opens the
#' database, adds the account, and closes the connection again, so it is
#' meant to be run interactively rather than from application code.
#'
#' @param db_path Path to the EpiSODIC database: an existing SQLite file, or
#'   a MariaDB/MySQL DSN (see [episodic_db_dsn_mariadb()]). Defaults to the
#'   `EPISODIC_DB` environment variable.
#' @param username,full_name,email The new account's sign-in name, display
#'   name, and email address.
#' @param password A temporary plaintext password, never stored or logged
#'   as-is: it is hashed before it reaches the database, and the account
#'   holder is required to replace it on first sign-in.
#' @param role Either `"epidemiologist"` (can classify and close clusters,
#'   in addition to everything a viewer can do) or `"viewer"` (read-only -
#'   can see everything a signed-in epidemiologist sees, including
#'   patient-level line lists, but cannot record an assessment). Both
#'   roles require sign-in; there is no anonymous access to patient
#'   detail.
#' @param is_admin Whether the new account may also see the Settings
#'   screen (manage notification channels, other accounts, and export the
#'   configuration). Independent of `role` - an admin is still either an
#'   epidemiologist or a viewer for everything outside Settings.
#' @return Invisibly, the new account's `user_id`.
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' con <- episodic_db_create(db_path)
#' DBI::dbDisconnect(con)
#'
#' user_id <- episodic_add_user(
#'   db_path,
#'   username = "jdoe", full_name = "Jane Doe",
#'   email = "jane@example.org", password = "temporary-password"
#' )
#' user_id
#'
#' file.remove(db_path)
#' @export
episodic_add_user <- function(
    db_path = Sys.getenv("EPISODIC_DB", unset = NA),
    username,
    full_name,
    email,
    password,
    role = "epidemiologist",
    is_admin = FALSE) {
  rlang::check_installed("sodium")
  role <- match.arg(role, c("epidemiologist", "viewer"))
  con <- episodic_db_open(db_path)
  on.exit(DBI::dbDisconnect(con))
  invisible(episodic_db_app_user_insert(
    con,
    username = username,
    full_name = full_name,
    email = email,
    password_hash = sodium::password_store(password),
    role = role,
    is_admin = is_admin
  ))
}

#' Change a signed-in account's role, admin flag, or active state
#'
#' The Settings screen's user-management panel writes here rather than
#' via `UPDATE`, the same insert-only pattern
#' `episodic_auth_change_password()` uses for `password_hash`: each call
#' appends one event, and `episodic_auth_role()`/`episodic_auth_is_admin()`/
#' `episodic_auth_is_active()` resolve "current" as the most recent
#' matching event.
#'
#' @param con A [DBI::DBIConnection-class].
#' @param user_id The account being changed.
#' @param actor_user_id The signed-in `is_admin` account making the
#'   change, recorded for the audit trail.
#' @param role New role (`"epidemiologist"`/`"viewer"`), for
#'   `episodic_auth_set_role()`.
#' @param is_admin New admin flag, for `episodic_auth_set_admin()`.
#' @param is_active New active flag, for `episodic_auth_set_active()`
#'   (`FALSE` revokes sign-in without deleting the account or its
#'   history).
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
episodic_auth_set_role <- function(con, user_id, actor_user_id, role) {
  role <- match.arg(role, c("epidemiologist", "viewer"))
  episodic_db_app_user_event_insert(
    con,
    user_id,
    "role_change",
    actor_user_id = actor_user_id,
    new_role = role
  )
  invisible(NULL)
}

#' @rdname episodic_auth_set_role
#' @keywords internal
#' @noRd
episodic_auth_set_admin <- function(con, user_id, actor_user_id, is_admin) {
  episodic_db_app_user_event_insert(
    con,
    user_id,
    "admin_change",
    actor_user_id = actor_user_id,
    new_is_admin = as.integer(isTRUE(is_admin))
  )
  invisible(NULL)
}

#' @rdname episodic_auth_set_role
#' @keywords internal
#' @noRd
episodic_auth_set_active <- function(con, user_id, actor_user_id, is_active) {
  episodic_db_app_user_event_insert(
    con,
    user_id,
    "active_change",
    actor_user_id = actor_user_id,
    new_is_active = as.integer(isTRUE(is_active))
  )
  invisible(NULL)
}

#' Whether a signed-in user may classify, close, or otherwise write
#'
#' The only role distinction the app enforces: `"epidemiologist"` accounts
#' (who, by definition, assess clusters and classify them) may write,
#' `"viewer"` accounts may not. Both roles see identical read access,
#' including patient-level detail - `NULL` (nobody signed in) is the only
#' case with no patient detail at all, handled separately by each panel's
#' own `is.null(current_user)` check.
#'
#' @param user The session's signed-in user row, or `NULL`.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_user_is_epidemiologist <- function(user) {
  !is.null(user) && identical(user$role, "epidemiologist")
}

#' Whether a signed-in user may see the Settings screen
#'
#' Independent of `episodic_user_is_epidemiologist()` - an admin is still
#' separately either an epidemiologist or a viewer for everything outside
#' Settings.
#'
#' @inheritParams episodic_user_is_epidemiologist
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_user_is_admin <- function(user) {
  !is.null(user) && isTRUE(as.logical(user$is_admin))
}

#' Whether this instance closes the app to anonymous visitors
#'
#' `access.require_login` in the resolved configuration, read defensively:
#' anything that is not unambiguously true leaves the app in the
#' behaviour it has always had, so a malformed or absent value can never
#' silently lock an instance out of its own dashboard.
#'
#' Deliberately a YAML-only setting, with no Settings-screen override: a
#' login wall an admin account can switch off from inside the app is a
#' login wall that falls with that one account.
#'
#' @param config A resolved configuration from
#'   `episodic_config_resolve()`, or `NULL` to resolve one here.
#' @return A single logical, never `NA`.
#' @keywords internal
#' @noRd
episodic_app_require_login <- function(config = NULL) {
  config <- config %||% episodic_config_resolve()
  isTRUE(as.logical(config$access$require_login %||% FALSE))
}

#' Whether this session may be shown anything at all
#'
#' The whole of the anonymous-access policy, in one predicate, so that
#' every screen and every observer asks the same question in the same way
#' rather than each re-deriving it.
#'
#' It is asked on the *server*, and what it gates is whether a screen is
#' rendered at all - not whether it is hidden once rendered. A dashboard
#' that renders its data and then covers it with a modal has already sent
#' that data to the browser, where a developer console reaches it in
#' seconds; the modal is then decoration over a leak. Everything behind
#' this predicate is therefore never computed, never serialised, and
#' never sent.
#'
#' @param require_login Whether this instance requires a sign-in, from
#'   `episodic_app_require_login()`.
#' @param user The session's signed-in user row, or `NULL`.
#' @return A single logical.
#' @keywords internal
#' @noRd
episodic_app_access_granted <- function(require_login, user) {
  !isTRUE(require_login) || !is.null(user)
}
