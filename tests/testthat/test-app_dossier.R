test_that("episode_ui_dossier() and episode_ui_assessment_rail() render the fixture cluster without error, in both languages", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  expect_s3_class(episode_ui_dossier(env$con, env$cluster_id, lang = "nl"), "shiny.tag")
  expect_s3_class(episode_ui_dossier(env$con, env$cluster_id, lang = "en"), "shiny.tag")
  expect_s3_class(episode_ui_assessment_rail(env$con, env$cluster_id, lang = "nl"), "shiny.tag")
})

test_that("episode_ui_geo_panel() shows the full per-PC4 breakdown, not just the dominant PC4", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  obj <- episode_cluster_object(env$con, env$cluster_id)

  panel <- episode_ui_geo_panel(obj, lang = "nl")
  rendered <- as.character(panel)
  expect_true(grepl("9711", rendered))
  expect_true(grepl("9712", rendered))
  expect_true(grepl("9713", rendered))
})

test_that("episode_ui_streams_screen() and episode_ui_status_strip() render the fixture run without error", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  screen <- episode_app_streams_screen(env$con)
  expect_s3_class(episode_ui_streams_screen(screen, lang = "nl"), "shiny.tag")

  status <- episode_app_status(env$con)
  expect_s3_class(episode_ui_status_strip(status, lang = "nl"), "shiny.tag")
})
