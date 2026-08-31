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

auth_test_user <- function(con, password = "initial123", must_change = TRUE) {
  user_id <- episodic_db_app_user_insert(
    con,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    sodium::password_store(password)
  )
  if (!must_change) {
    DBI::dbExecute(
      con,
      "UPDATE episodic_app_user SET must_change = 0 WHERE user_id = ?",
      params = list(user_id)
    )
  }
  user_id
}

test_that("episodic_provision_user() takes a db_path (not an open connection) and creates an account that can immediately log in, flagged must_change", {
  db_path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(db_path))

  episodic_provision_user(
    db_path,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    "a-temporary-password"
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  result <- episodic_auth_login(con, "jdoe", "a-temporary-password")
  expect_true(result$ok)
  expect_equal(result$user$full_name, "Jane Doe")
  expect_true(result$must_change)
})

test_that("episodic_provision_user() falls back to the EPISODIC_DB environment variable when db_path is not given", {
  db_path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(db_path))
  old_env <- Sys.getenv("EPISODIC_DB", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_DB")
    } else {
      Sys.setenv(EPISODIC_DB = old_env)
    }
  )
  Sys.setenv(EPISODIC_DB = db_path)

  episodic_provision_user(
    username = "asmith",
    full_name = "Ann Smith",
    email = "a@x.nl",
    password = "pw123456"
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(episodic_auth_login(con, "asmith", "pw123456")$ok)
})

test_that("episodic_db_open() errors clearly when neither db_path nor EPISODIC_DB is given", {
  old_env <- Sys.getenv("EPISODIC_DB", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_DB")
    } else {
      Sys.setenv(EPISODIC_DB = old_env)
    }
  )
  Sys.unsetenv("EPISODIC_DB")

  expect_error(episodic_db_open(), "EPISODIC_DB")
})

test_that("episodic_auth_login() rejects an unknown username or wrong password without distinguishing", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  auth_test_user(con)

  expect_false(episodic_auth_login(con, "nobody", "whatever")$ok)
  expect_false(episodic_auth_login(con, "jdoe", "wrongpassword")$ok)
})

test_that("episodic_auth_login() rejects a deactivated account even with the right password", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)
  DBI::dbExecute(
    con,
    "UPDATE episodic_app_user SET is_active = 0 WHERE user_id = ?",
    params = list(user_id)
  )

  expect_false(episodic_auth_login(con, "jdoe", "initial123")$ok)
})

test_that("episodic_auth_login() succeeds with the right password and records a login event", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)

  result <- episodic_auth_login(con, "jdoe", "initial123")
  expect_true(result$ok)
  expect_equal(result$user$user_id, user_id)

  events <- episodic_db_app_user_events(con, user_id)
  expect_equal(nrow(events), 1)
  expect_equal(events$event_type[1], "login")
})

test_that("must_change starts TRUE and becomes FALSE only after a password_change event, never via UPDATE", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con) # must_change = 1 on the row

  expect_true(episodic_auth_login(con, "jdoe", "initial123")$must_change)

  episodic_auth_change_password(con, user_id, "newpassword")
  expect_false(episodic_auth_login(con, "jdoe", "newpassword")$must_change)

  # the row's own must_change column is untouched - it is the event log that changed
  row <- episodic_db_user_by_id(con, user_id)
  expect_equal(as.integer(row$must_change), 1L)
})

test_that("a must_change = 0 account never reports must_change, even with no password_change event", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  auth_test_user(con, must_change = FALSE)

  expect_false(episodic_auth_login(con, "jdoe", "initial123")$must_change)
})

test_that("episodic_auth_change_password() invalidates the old password and makes the new one work", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)

  episodic_auth_change_password(con, user_id, "brandnewpassword")
  expect_false(episodic_auth_login(con, "jdoe", "initial123")$ok)
  expect_true(episodic_auth_login(con, "jdoe", "brandnewpassword")$ok)
})

test_that("a second password change supersedes the first, not just the original", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)

  episodic_auth_change_password(con, user_id, "first_change")
  episodic_auth_change_password(con, user_id, "second_change")

  expect_false(episodic_auth_login(con, "jdoe", "initial123")$ok)
  expect_false(episodic_auth_login(con, "jdoe", "first_change")$ok)
  expect_true(episodic_auth_login(con, "jdoe", "second_change")$ok)
})

test_that("episodic_db_user_by_username() and episodic_db_user_by_id() return NULL for a nonexistent account", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_null(episodic_db_user_by_username(con, "nobody"))
  expect_null(episodic_db_user_by_id(con, 99999L))
})

test_that("episodic_provision_user() defaults to the epidemiologist role", {
  db_path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(db_path))

  episodic_provision_user(
    db_path,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    "a-temporary-password"
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  user <- episodic_db_user_by_username(con, "jdoe")
  expect_equal(user$role, "epidemiologist")
})

test_that("episodic_provision_user() also accepts the viewer role, and rejects anything else", {
  db_path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(db_path))

  episodic_provision_user(
    db_path,
    "vsmith",
    "Val Smith",
    "v@x.nl",
    "a-temporary-password",
    role = "viewer"
  )

  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  user <- episodic_db_user_by_username(con, "vsmith")
  expect_equal(user$role, "viewer")

  expect_error(
    episodic_provision_user(
      db_path,
      "other",
      "Other Person",
      "o@x.nl",
      "a-temporary-password",
      role = "moderator"
    )
  )
})

test_that("episodic_user_is_epidemiologist() is TRUE only for a signed-in epidemiologist, never for a viewer or NULL", {
  expect_true(episodic_user_is_epidemiologist(list(role = "epidemiologist")))
  expect_false(episodic_user_is_epidemiologist(list(role = "viewer")))
  expect_false(episodic_user_is_epidemiologist(NULL))
})

test_that("episodic_provision_user() defaults is_admin to FALSE, and accepts TRUE", {
  db_path <- tempfile(fileext = ".sqlite")
  DBI::dbDisconnect(episodic_db_create(db_path))
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  episodic_provision_user(
    db_path,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    "a-temporary-password"
  )
  episodic_provision_user(
    db_path,
    "asmith",
    "Ann Smith",
    "a@x.nl",
    "a-temporary-password",
    is_admin = TRUE
  )

  jdoe <- episodic_db_user_by_username(con, "jdoe")
  asmith <- episodic_db_user_by_username(con, "asmith")
  expect_equal(as.integer(jdoe$is_admin), 0L)
  expect_equal(as.integer(asmith$is_admin), 1L)
})

test_that("episodic_user_is_admin() is TRUE only for a resolved is_admin account", {
  expect_true(episodic_user_is_admin(list(is_admin = 1L)))
  expect_false(episodic_user_is_admin(list(is_admin = 0L)))
  expect_false(episodic_user_is_admin(NULL))
})

test_that("episodic_auth_set_role()/set_admin()/set_active() are insert-only and resolve as the most recent event", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  actor_id <- auth_test_user(con, must_change = FALSE)
  target_id <- episodic_db_app_user_insert(
    con,
    "vsmith",
    "Val Smith",
    "v@x.nl",
    sodium::password_store("initial123"),
    role = "viewer",
    is_admin = FALSE
  )
  target <- episodic_db_user_by_id(con, target_id)

  expect_equal(episodic_auth_role(con, target), "viewer")
  expect_false(episodic_auth_is_admin(con, target))
  expect_true(episodic_auth_is_active(con, target))

  episodic_auth_set_role(con, target_id, actor_id, "epidemiologist")
  episodic_auth_set_admin(con, target_id, actor_id, TRUE)
  episodic_auth_set_active(con, target_id, actor_id, FALSE)

  target <- episodic_db_user_by_id(con, target_id)
  expect_equal(episodic_auth_role(con, target), "epidemiologist")
  expect_true(episodic_auth_is_admin(con, target))
  expect_false(episodic_auth_is_active(con, target))

  # the row's own columns are untouched - only the event log changed
  raw <- DBI::dbGetQuery(
    con,
    "SELECT role, is_admin, is_active FROM episodic_app_user WHERE user_id = ?",
    params = list(target_id)
  )
  expect_equal(raw$role, "viewer")
  expect_equal(as.integer(raw$is_admin), 0L)
  expect_equal(as.integer(raw$is_active), 1L)

  # a second change to the same attribute supersedes the first, not just the original
  episodic_auth_set_role(con, target_id, actor_id, "viewer")
  target <- episodic_db_user_by_id(con, target_id)
  expect_equal(episodic_auth_role(con, target), "viewer")
})

test_that("episodic_auth_login() rejects a deactivated account even via an active_change event, not just the raw column", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  actor_id <- auth_test_user(con, password = "actorpw", must_change = FALSE)
  target_id <- episodic_db_app_user_insert(
    con,
    "tsmith",
    "Tom Smith",
    "t@x.nl",
    sodium::password_store("initial123")
  )
  expect_true(episodic_auth_login(con, "tsmith", "initial123")$ok)

  episodic_auth_set_active(con, target_id, actor_id, FALSE)
  expect_false(episodic_auth_login(con, "tsmith", "initial123")$ok)
})

test_that("episodic_auth_refresh_user() returns NULL for a cached user deactivated after sign-in, without touching the row", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  actor_id <- auth_test_user(con, password = "actorpw", must_change = FALSE)
  target_id <- episodic_db_app_user_insert(
    con,
    "tsmith",
    "Tom Smith",
    "t@x.nl",
    sodium::password_store("initial123")
  )
  cached <- episodic_auth_login(con, "tsmith", "initial123")$user
  expect_false(is.null(episodic_auth_refresh_user(con, cached)))

  episodic_auth_set_active(con, target_id, actor_id, FALSE)
  # the cached reactiveVal snapshot is unaffected by the deactivation -
  # only a fresh look at the database sees it
  expect_null(episodic_auth_refresh_user(con, cached))
})

test_that("episodic_auth_refresh_user() reflects a role/admin change made after sign-in, not the cached snapshot", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  actor_id <- auth_test_user(con, password = "actorpw", must_change = FALSE)
  target_id <- episodic_db_app_user_insert(
    con,
    "tsmith",
    "Tom Smith",
    "t@x.nl",
    sodium::password_store("initial123"),
    role = "viewer"
  )
  cached <- episodic_auth_login(con, "tsmith", "initial123")$user
  expect_false(episodic_user_is_epidemiologist(cached))

  episodic_auth_set_role(con, target_id, actor_id, "epidemiologist")
  refreshed <- episodic_auth_refresh_user(con, cached)
  expect_true(episodic_user_is_epidemiologist(refreshed))

  episodic_auth_set_admin(con, target_id, actor_id, TRUE)
  refreshed <- episodic_auth_refresh_user(con, cached)
  expect_true(episodic_user_is_admin(refreshed))
})

test_that("episodic_auth_refresh_user() returns NULL for NULL, and for an account that no longer exists", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_null(episodic_auth_refresh_user(con, NULL))
  expect_null(episodic_auth_refresh_user(con, list(user_id = 999999L)))
})

test_that("episodic_auth_login() returns a user row with role/is_admin/is_active resolved from events, not the raw row", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  actor_id <- auth_test_user(con, password = "actorpw", must_change = FALSE)
  target_id <- episodic_db_app_user_insert(
    con,
    "vsmith",
    "Val Smith",
    "v@x.nl",
    sodium::password_store("initial123"),
    role = "viewer",
    is_admin = FALSE
  )
  episodic_auth_set_role(con, target_id, actor_id, "epidemiologist")
  episodic_auth_set_admin(con, target_id, actor_id, TRUE)

  result <- episodic_auth_login(con, "vsmith", "initial123")
  expect_true(result$ok)
  expect_equal(result$user$role, "epidemiologist")
  expect_equal(as.integer(result$user$is_admin), 1L)
  expect_true(episodic_user_is_epidemiologist(result$user))
  expect_true(episodic_user_is_admin(result$user))
})
