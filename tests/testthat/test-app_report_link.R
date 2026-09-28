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

#' A rendered report row whose file is written to a temporary directory,
#' with the hash `episodic_report_render()` would record for it.
#' @keywords internal
#' @noRd
report_link_fixture <- function(con, cluster_id, version_no, content) {
  path <- tempfile(fileext = ".html")
  writeLines(content, path)
  report_id <- episodic_db_report_render_insert(
    con,
    cluster_id = cluster_id,
    user_id = NA,
    file_path = path,
    file_sha256 = digest::digest(path, algo = "sha256", file = TRUE),
    params_json = "{}",
    case_ids_json = "[]",
    version_no = version_no
  )
  list(report_id = report_id, path = path)
}

report_link_user <- function(con) {
  user_id <- episodic_db_app_user_insert(
    con,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    sodium::password_store("initial123"),
    role = "viewer"
  )
  episodic_db_user_by_id(con, user_id)
}

test_that("a report version links to its report in a new tab only for a signed-in visitor and only while its file exists", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  kept <- report_link_fixture(env$con, env$cluster_id, 1L, "<html>v1</html>")
  gone <- report_link_fixture(env$con, env$cluster_id, 2L, "<html>v2</html>")
  on.exit(unlink(kept$path), add = TRUE)
  unlink(gone$path)
  user <- report_link_user(env$con)
  href <- function(report_id) paste0("dataobj/report?report_id=", report_id)

  signed_in <- as.character(episodic_ui_report_panel(
    env$con,
    env$cluster_id,
    user,
    lang = "en",
    report_href = href
  ))
  expect_true(grepl(
    sprintf('href="%s"', href(kept$report_id)),
    signed_in,
    fixed = TRUE
  ))
  expect_true(grepl('target="_blank"', signed_in, fixed = TRUE))
  expect_true(grepl('rel="noopener"', signed_in, fixed = TRUE))
  expect_false(grepl(href(gone$report_id), signed_in, fixed = TRUE))
  # The version whose file is gone is still listed, as text.
  expect_true(grepl("Version 2", signed_in, fixed = TRUE))

  anon <- as.character(episodic_ui_report_panel(
    env$con,
    env$cluster_id,
    NULL,
    lang = "en",
    report_href = href
  ))
  expect_false(grepl("<a ", anon, fixed = TRUE))
  expect_true(grepl("Version 1", anon, fixed = TRUE))

  no_route <- as.character(episodic_ui_report_panel(
    env$con,
    env$cluster_id,
    user,
    lang = "en"
  ))
  expect_false(grepl("<a ", no_route, fixed = TRUE))
})

test_that("episodic_app_report_response() serves an unchanged report to an active signed-in account and refuses everything else", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  report <- report_link_fixture(env$con, env$cluster_id, 1L, "<html>v1</html>")
  on.exit(unlink(report$path), add = TRUE)
  user <- report_link_user(env$con)
  id <- as.character(report$report_id)

  served <- episodic_app_report_response(env$con, user, id, lang = "en")
  expect_identical(served$status, 200L)
  expect_true(grepl("text/html", served$content_type, fixed = TRUE))
  expect_identical(
    served$content,
    readBin(report$path, what = "raw", n = file.size(report$path))
  )

  anon <- episodic_app_report_response(env$con, NULL, id, lang = "en")
  expect_identical(anon$status, 403L)
  expect_identical(
    anon$content,
    episodic_tr("panel.report.forbidden", lang = "en")
  )

  for (bad in list(NULL, NA_character_, "", "1; DROP", "-1")) {
    expect_identical(
      episodic_app_report_response(env$con, user, bad, lang = "en")$status,
      404L
    )
  }
  expect_identical(
    episodic_app_report_response(env$con, user, "999999", lang = "en")$status,
    404L
  )

  writeLines("<html>altered</html>", report$path)
  changed <- episodic_app_report_response(env$con, user, id, lang = "nl")
  expect_identical(changed$status, 409L)
  expect_identical(
    changed$content,
    episodic_tr("panel.report.file_changed", lang = "nl")
  )

  unlink(report$path)
  expect_identical(
    episodic_app_report_response(env$con, user, id, lang = "en")$status,
    410L
  )
})

test_that("episodic_app_report_response() refuses an account deactivated after sign-in", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  report <- report_link_fixture(env$con, env$cluster_id, 1L, "<html>v1</html>")
  on.exit(unlink(report$path), add = TRUE)
  user <- report_link_user(env$con)
  actor_id <- episodic_db_app_user_insert(
    env$con,
    "admin",
    "Admin",
    "a@x.nl",
    sodium::password_store("adminpw")
  )
  episodic_auth_set_active(env$con, user$user_id, actor_id, FALSE)

  expect_identical(
    episodic_app_report_response(
      env$con,
      user,
      as.character(report$report_id),
      lang = "en"
    )$status,
    403L
  )
})
