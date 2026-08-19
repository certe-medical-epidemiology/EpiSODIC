auth_test_user <- function(con, password = "initial123", must_change = TRUE) {
  user_id <- episode_db_app_user_insert(con, "jdoe", "Jane Doe", "j@x.nl",
                                         sodium::password_store(password))
  if (!must_change) {
    DBI::dbExecute(con, "UPDATE episode_app_user SET must_change = 0 WHERE user_id = ?",
                    params = list(user_id))
  }
  user_id
}

test_that("episode_provision_user() creates an account that can immediately log in and is flagged must_change", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))

  episode_provision_user(con, "jdoe", "Jane Doe", "j@x.nl", "a-temporary-password")
  result <- episode_auth_login(con, "jdoe", "a-temporary-password")
  expect_true(result$ok)
  expect_equal(result$user$full_name, "Jane Doe")
  expect_true(result$must_change)
})

test_that("episode_auth_login() rejects an unknown username or wrong password without distinguishing", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  auth_test_user(con)

  expect_false(episode_auth_login(con, "nobody", "whatever")$ok)
  expect_false(episode_auth_login(con, "jdoe", "wrongpassword")$ok)
})

test_that("episode_auth_login() rejects a deactivated account even with the right password", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)
  DBI::dbExecute(con, "UPDATE episode_app_user SET is_active = 0 WHERE user_id = ?",
                 params = list(user_id))

  expect_false(episode_auth_login(con, "jdoe", "initial123")$ok)
})

test_that("episode_auth_login() succeeds with the right password and records a login event", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)

  result <- episode_auth_login(con, "jdoe", "initial123")
  expect_true(result$ok)
  expect_equal(result$user$user_id, user_id)

  events <- episode_db_app_user_events(con, user_id)
  expect_equal(nrow(events), 1)
  expect_equal(events$event_type[1], "login")
})

test_that("must_change starts TRUE and becomes FALSE only after a password_change event, never via UPDATE", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)  # must_change = 1 on the row

  expect_true(episode_auth_login(con, "jdoe", "initial123")$must_change)

  episode_auth_change_password(con, user_id, "newpassword")
  expect_false(episode_auth_login(con, "jdoe", "newpassword")$must_change)

  # the row's own must_change column is untouched - it is the event log that changed
  row <- episode_db_user_by_id(con, user_id)
  expect_equal(as.integer(row$must_change), 1L)
})

test_that("a must_change = 0 account never reports must_change, even with no password_change event", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  auth_test_user(con, must_change = FALSE)

  expect_false(episode_auth_login(con, "jdoe", "initial123")$must_change)
})

test_that("episode_auth_change_password() invalidates the old password and makes the new one work", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)

  episode_auth_change_password(con, user_id, "brandnewpassword")
  expect_false(episode_auth_login(con, "jdoe", "initial123")$ok)
  expect_true(episode_auth_login(con, "jdoe", "brandnewpassword")$ok)
})

test_that("a second password change supersedes the first, not just the original", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)

  episode_auth_change_password(con, user_id, "first_change")
  episode_auth_change_password(con, user_id, "second_change")

  expect_false(episode_auth_login(con, "jdoe", "initial123")$ok)
  expect_false(episode_auth_login(con, "jdoe", "first_change")$ok)
  expect_true(episode_auth_login(con, "jdoe", "second_change")$ok)
})

test_that("episode_auth_last_login() reflects the most recent login, NA before any login", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  user_id <- auth_test_user(con)
  user <- episode_db_user_by_id(con, user_id)

  expect_true(is.na(episode_auth_last_login(con, user)))
  episode_auth_login(con, "jdoe", "initial123")
  first_login <- episode_auth_last_login(con, episode_db_user_by_id(con, user_id))
  expect_false(is.na(first_login))
})

test_that("episode_db_user_by_username() and episode_db_user_by_id() return NULL for a nonexistent account", {
  con <- episode_test_db()
  on.exit(DBI::dbDisconnect(con))
  expect_null(episode_db_user_by_username(con, "nobody"))
  expect_null(episode_db_user_by_id(con, 99999L))
})
