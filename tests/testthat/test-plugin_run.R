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

plugin_run_test <- function(steps, ..., grant = list(permissions = "cluster_summary", secrets = character(0)), payload = list(cluster = list(cluster_id = 7L))) {
  skip_if_not_installed("processx")
  instance <- plugin_test_instance(env = parent.frame())
  withr::local_envvar(instance$env, .local_envir = parent.frame())
  plugin <- episodic_plugin_read(plugin_test_write(instance$dir, "test-plugin", plugin_test_spec(steps = steps, ...)))
  expect_identical(plugin$problems, list())
  episodic_plugin_execute(plugin, grant, payload, lang = "nl")
}

test_that("a step reads the payload and reports a message and a link", {
  outcome <- plugin_run_test(list(plugin_test_step(paste(
    "p <- jsonlite::read_json(Sys.getenv('EPISODIC_PAYLOAD'))",
    "cat('message=Cluster ', p$cluster$cluster_id, ' in ', Sys.getenv('EPISODIC_LANGUAGE'), '\\n',",
    "    'link=https://regional.example/o/7\\n', sep = '', file = Sys.getenv('EPISODIC_RESULT'))",
    sep = "\n"
  ))))
  expect_identical(outcome$status, "success")
  expect_identical(outcome$message, "Cluster 7 in nl")
  expect_identical(outcome$link, "https://regional.example/o/7")
})

test_that("a step sees only allowlisted variables and its granted secrets, never EpiSODIC's own", {
  withr::local_envvar(EPISODIC_DB = "db-secret", UNRELATED_SECRET = "other", REGIONAL_TOKEN = "token-value", https_proxy = "http://proxy.example:3128")
  outcome <- plugin_run_test(
    list(plugin_test_step(paste(
      "v <- function(x) Sys.getenv(x, 'unset')",
      "cat('message=', v('EPISODIC_DB'), '|', v('UNRELATED_SECRET'), '|', v('REGIONAL_TOKEN') == 'token-value', '|', v('https_proxy'), '|', v('MODE'), '\\n',",
      "    sep = '', file = Sys.getenv('EPISODIC_RESULT'))",
      sep = "\n"
    ), env = list(MODE = "plain"))),
    secrets = list("REGIONAL_TOKEN"),
    grant = list(permissions = "cluster_summary", secrets = "REGIONAL_TOKEN")
  )
  expect_identical(outcome$message, "unset|unset|TRUE|http://proxy.example:3128|plain")
})

test_that("a secret that was declared but not granted is not passed", {
  withr::local_envvar(REGIONAL_TOKEN = "token-value")
  outcome <- plugin_run_test(
    list(plugin_test_step("cat('message=', Sys.getenv('REGIONAL_TOKEN', 'unset'), '\\n', sep = '', file = Sys.getenv('EPISODIC_RESULT'))")),
    secrets = list("REGIONAL_TOKEN")
  )
  expect_identical(outcome$message, "unset")
})

test_that("a granted secret that is not set stops the run before any step starts", {
  withr::local_envvar(REGIONAL_TOKEN = NA)
  outcome <- plugin_run_test(
    list(plugin_test_step("writeLines('started', 'started.txt')")),
    secrets = list("REGIONAL_TOKEN"),
    grant = list(permissions = "cluster_summary", secrets = "REGIONAL_TOKEN")
  )
  expect_identical(outcome$status, "failed")
  expect_match(outcome$detail, "REGIONAL_TOKEN are not set", fixed = TRUE)
})

test_that("a failing step ends the run with its error output, granted secrets masked", {
  withr::local_envvar(REGIONAL_TOKEN = "token-value")
  outcome <- plugin_run_test(
    list(
      plugin_test_step("stop('refused with token ', Sys.getenv('REGIONAL_TOKEN'))"),
      plugin_test_step("cat('message=never\\n', file = Sys.getenv('EPISODIC_RESULT'))")
    ),
    secrets = list("REGIONAL_TOKEN"),
    grant = list(permissions = "cluster_summary", secrets = "REGIONAL_TOKEN")
  )
  expect_identical(outcome$status, "failed")
  expect_identical(outcome$step, "Send")
  expect_match(outcome$detail, "refused with token ***", fixed = TRUE)
  expect_false(grepl("token-value", outcome$detail, fixed = TRUE))
  expect_true(is.na(outcome$message))
})

test_that("a plugin that runs past its timeout is stopped and reported as such", {
  started <- Sys.time()
  outcome <- plugin_run_test(list(plugin_test_step("Sys.sleep(30)")), timeout = 1L)
  expect_identical(outcome$status, "timeout")
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 15)
})

test_that("steps run in order in one working directory, in each shell", {
  skip_if(!nzchar(Sys.which("bash")), "bash is not available")
  outcome <- plugin_run_test(list(
    plugin_test_step("writeLines('from r', 'shared.txt')"),
    plugin_test_step("echo \"message=$(cat shared.txt) then bash\" >> \"$EPISODIC_RESULT\"", shell = "bash")
  ))
  expect_identical(outcome$message, "from r then bash")
})

test_that("a python step runs with the same interface", {
  skip_if(!nzchar(episodic_plugin_shell_program("python")), "Python is not available")
  outcome <- plugin_run_test(list(plugin_test_step(paste(
    "import json, os",
    "p = json.load(open(os.environ['EPISODIC_PAYLOAD']))",
    "open(os.environ['EPISODIC_RESULT'], 'a').write('message=python saw %d\\n' % p['cluster']['cluster_id'])",
    sep = "\n"
  ), shell = "python")))
  expect_identical(outcome$message, "python saw 7")
})

test_that("a plugin may report an error of its own, and a malformed result is a failure", {
  outcome <- plugin_run_test(list(plugin_test_step("cat('status=error\\nmessage=The platform said no\\n', file = Sys.getenv('EPISODIC_RESULT'))")))
  expect_identical(outcome$status, "error")
  expect_identical(outcome$message, "The platform said no")

  outcome <- plugin_run_test(list(plugin_test_step("cat('mesage=typo\\n', file = Sys.getenv('EPISODIC_RESULT'))")))
  expect_identical(outcome$status, "failed")
  expect_match(outcome$detail, "does not understand", fixed = TRUE)

  outcome <- plugin_run_test(list(plugin_test_step("cat('link=javascript:alert(1)\\n', file = Sys.getenv('EPISODIC_RESULT'))")))
  expect_identical(outcome$status, "failed")
  expect_match(outcome$detail, "not an http or https address", fixed = TRUE)
})

test_that("a plugin that reports nothing and exits cleanly succeeded without a message", {
  outcome <- plugin_run_test(list(plugin_test_step("invisible(1)")))
  expect_identical(outcome$status, "success")
  expect_true(is.na(outcome$message))
})

test_that("a plugin without secrets is given no secrets, not the whole environment", {
  expect_identical(episodic_plugin_getenv(character(0)), stats::setNames(character(0), character(0)))
  withr::local_envvar(UNRELATED_SECRET = "other")
  plugin <- list(id = "p", secrets = character(0))
  env <- episodic_plugin_environment(plugin, list(secrets = character(0)), list(env = character(0)), tempdir(), "en")
  expect_false("UNRELATED_SECRET" %in% names(env))
  expect_true(all(nzchar(names(env))))
})

test_that("a step's env placeholders are filled from the payload, as data and never as code", {
  outcome <- plugin_run_test(
    list(plugin_test_step(
      "cat('message=', Sys.getenv('LABEL'), '\\n', sep = '', file = Sys.getenv('EPISODIC_RESULT'))",
      env = list(LABEL = "Cluster ${{ cluster.cluster_id }}: ${{ cluster.note }}")
    )),
    payload = list(cluster = list(cluster_id = 7L, note = "'); stop('injected"))
  )
  expect_identical(outcome$status, "success")
  expect_identical(outcome$message, "Cluster 7: '); stop('injected")
})

test_that("a placeholder with no value in the payload stops the run, naming it", {
  outcome <- plugin_run_test(
    list(plugin_test_step(env = list(WARD = "${{ cluster.ward }}"))),
    payload = list(cluster = list(cluster_id = 7L, ward = NA))
  )
  expect_identical(outcome$status, "failed")
  expect_match(outcome$detail, "${{ cluster.ward }}", fixed = TRUE)
})
