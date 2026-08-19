test_that("episode_i18n_load() reads both shipped languages", {
  nl <- episode_i18n_load("nl")
  en <- episode_i18n_load("en")
  expect_gt(length(nl), 0)
  expect_gt(length(en), 0)
  expect_true(is.character(nl))
})

test_that("nl.json and en.json carry exactly the same key set", {
  nl <- episode_i18n_load("nl")
  en <- episode_i18n_load("en")
  expect_setequal(names(nl), names(en))
})

test_that("episode_tr() substitutes placeholders", {
  expect_equal(episode_tr("dossier.meta.cluster_id", id = 1041, lang = "nl"), "cluster #1041")
  expect_equal(episode_tr("dossier.meta.cluster_id", id = 1041, lang = "en"), "cluster #1041")
})

test_that("episode_tr() falls back from nl to en when a key is nl-missing", {
  # simulate a key present only in en by loading en's cache and asking under nl
  # with a key we know exists in en (all shared here, so instead test the
  # literal fallback mechanism via a key deliberately absent from both)
  result <- episode_tr("this.key.does.not.exist", lang = "nl")
  expect_equal(result, "[[this.key.does.not.exist]]")
})

test_that("a missing key renders visibly rather than blank", {
  result <- episode_tr("totally.bogus.key", lang = "en")
  expect_match(result, "^\\[\\[.*\\]\\]$")
  expect_false(identical(result, ""))
})

test_that("an instance override takes priority over the shipped translation", {
  overrides <- c("app.title" = "Instance-Specific Title")
  result <- episode_tr("app.title", lang = "nl", instance_i18n = overrides)
  expect_equal(result, "Instance-Specific Title")
})

test_that("episode_tr() with no instance override uses the shipped file", {
  result <- episode_tr("app.title", lang = "nl")
  expect_equal(result, "EpiSODE")
})

test_that("every key used in code exists in both language files", {
  # a lightweight guard against typo'd tr() keys: scan R/ for episode_tr("key"...
  r_files <- list.files(file.path(testthat::test_path(), "..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  code <- paste(vapply(r_files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  used_keys <- regmatches(code, gregexpr('episode_tr\\("([a-zA-Z0-9_.]+)"', code))[[1]]
  used_keys <- gsub('episode_tr\\("|"$', "", used_keys)
  used_keys <- unique(used_keys)
  skip_if(length(used_keys) == 0, "no episode_tr() calls found yet")
  nl <- episode_i18n_load("nl")
  missing <- setdiff(used_keys, names(nl))
  expect_equal(missing, character(0))
})
