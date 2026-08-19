test_that("episode_ui_dossier() and episode_ui_assessment_rail() render the fixture cluster without error, in both languages", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))

  expect_s3_class(episode_ui_dossier(env$con, env$cluster_id, lang = "nl"), "shiny.tag")
  expect_s3_class(episode_ui_dossier(env$con, env$cluster_id, lang = "en"), "shiny.tag")
  expect_s3_class(episode_ui_assessment_rail(env$con, env$cluster_id, lang = "nl"), "shiny.tag")
})

test_that("episode_ui_assessment_rail() renders the classification and mute pickers as coloured buttons, not <select>, with a mute intro paragraph", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  fake_user <- data.frame(user_id = 1L, username = "jdoe", full_name = "Jane Doe", stringsAsFactors = FALSE)

  rendered <- as.character(episode_ui_assessment_rail(env$con, env$cluster_id, lang = "nl", current_user = fake_user))
  expect_false(grepl("<select", rendered, fixed = TRUE))
  expect_true(grepl("episode-picker-btn", rendered, fixed = TRUE))
  expect_true(grepl(episode_tr("verdict.possible_epidemic", lang = "nl"), rendered, fixed = TRUE))
  expect_true(grepl(episode_tr("assessment.mute_reason.seasonal", lang = "nl"), rendered, fixed = TRUE))
  expect_true(grepl(episode_tr("assessment.mute_intro", lang = "nl"), rendered, fixed = TRUE))

  # Wpg/GGD are Netherlands-specific and out of scope for a general
  # deployment; removed from the form entirely.
  expect_false(grepl("assess_wpg", rendered, fixed = TRUE))
  expect_false(grepl("assess_ggd", rendered, fixed = TRUE))

  # verdict buttons ordered mild/terminal to severe: artefact and
  # expected_variation (both terminal) before the escalating verdicts.
  pos <- function(needle) regexpr(needle, rendered, fixed = TRUE)
  expect_true(pos(episode_tr("verdict.artefact", lang = "nl")) < pos(episode_tr("verdict.cluster_not_yet", lang = "nl")))
  expect_true(pos(episode_tr("verdict.cluster_not_yet", lang = "nl")) < pos(episode_tr("verdict.possible_epidemic", lang = "nl")))
  expect_true(pos(episode_tr("verdict.possible_epidemic", lang = "nl")) < pos(episode_tr("verdict.confirmed_epidemic", lang = "nl")))

  # hints describe what the verdict means, not what happens next
  expect_false(grepl("gestart", rendered, fixed = TRUE))
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
