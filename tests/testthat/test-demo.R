test_that("episode_demo(launch = FALSE) sets up a working demo database in one call", {
  skip_if_not_installed("sodium")
  db_path <- tempfile(fileext = ".sqlite")

  expect_message(
    result <- episode_demo(db_path = db_path, launch = FALSE),
    "demo account"
  )
  expect_equal(result, db_path)
  expect_true(file.exists(db_path))

  con <- episode_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_case")$n, 0)
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM episode_cluster")$n, 0)

  user <- episode_db_user_by_username(con, "demo")
  expect_false(is.null(user))
  expect_true(episode_auth_login(con, "demo", "episode-demo")$ok)
})

test_that("episode_demo() accepts custom credentials", {
  skip_if_not_installed("sodium")
  db_path <- tempfile(fileext = ".sqlite")

  episode_demo(db_path = db_path, username = "jdoe", full_name = "Jane Doe",
               email = "jdoe@example.org", password = "s3cret-enough", launch = FALSE)

  con <- episode_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  user <- episode_db_user_by_username(con, "jdoe")
  expect_equal(user$full_name, "Jane Doe")
  expect_true(episode_auth_login(con, "jdoe", "s3cret-enough")$ok)
})
