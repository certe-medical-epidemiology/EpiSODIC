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

# The reference plugin shipped in inst/plugins is the template plugin
# authors start from, so it must be a valid plugin and must do what it
# says against a real HTTP endpoint: here, a server in a process of its
# own that records what it received.

reference_plugin_path <- function() {
  path <- system.file("plugins", "share-outbreak-http.yaml", package = "EpiSODIC")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "plugins", "share-outbreak-http.yaml")
  path
}

reference_plugin_server <- function(status, env = parent.frame()) {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("processx")
  received <- tempfile(fileext = ".json")
  port <- httpuv::randomPort()
  script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf("received <- %s", deparse(received)),
    "httpuv::runServer('127.0.0.1', PORT, list(call = function(req) {",
    "  body <- rawToChar(req$rook.input$read())",
    "  writeLines(c(req$HTTP_AUTHORIZATION %||% '', body), received)",
    sprintf("  list(status = %dL, headers = list('Content-Type' = 'text/plain', Location = 'https://regional.example/o/1'), body = 'ok')", status),
    "}))"
  ), script)
  text <- sub("PORT", port, paste(readLines(script), collapse = "\n"))
  writeLines(c("`%||%` <- function(x, y) if (is.null(x)) y else x", text), script)
  server <- processx::process$new(file.path(R.home("bin"), "Rscript"), script, stdout = "|", stderr = "|")
  withr::defer(server$kill(), envir = env)
  deadline <- Sys.time() + 15
  repeat {
    up <- tryCatch(
      {
        con <- socketConnection("127.0.0.1", port, open = "r+", timeout = 1)
        close(con)
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (up || Sys.time() > deadline) break
    Sys.sleep(0.2)
  }
  list(url = sprintf("http://127.0.0.1:%d/share", port), received = received)
}

reference_plugin_run <- function(env = parent.frame()) {
  instance <- plugin_test_instance(env = env)
  episodic_plugin_install(reference_plugin_path(), episodic_config_path = instance$config_path)
  episodic_plugin_enable("share-outbreak-http", instance$config_path)
  setup <- app_read_setup()
  withr::defer(DBI::dbDisconnect(setup$con), envir = env)
  plugin <- episodic_plugins(episodic_config_resolve(instance$config_path), instance$dir)[["share-outbreak-http"]]
  list(
    run = function() {
      episodic_plugin_invoke(
        setup$con, plugin,
        list(cluster_id = setup$cluster_id, user = list(username = "epi1", role = "epidemiologist")),
        "headless", episodic_config_resolve(instance$config_path),
        lang = "en"
      )
    },
    cluster_id = setup$cluster_id
  )
}

test_that("the reference plugin is a valid plugin", {
  plugin <- episodic_plugin_read(reference_plugin_path())
  codes <- vapply(plugin$problems, function(p) p$code, character(1))
  expect_length(setdiff(codes, episodic_plugin_requirement_codes), 0)
  expect_identical(plugin$id, "share-outbreak-http")
  expect_identical(plugin$secrets, c("SHARE_ENDPOINT_URL", "SHARE_ENDPOINT_TOKEN"))
})

test_that("the reference plugin sends the outbreak's summary, with the token, and reports the platform's link", {
  skip_if_not_installed("httr2")
  server <- reference_plugin_server(201)
  withr::local_envvar(SHARE_ENDPOINT_URL = server$url, SHARE_ENDPOINT_TOKEN = "secret-token")
  case <- reference_plugin_run()
  outcome <- case$run()
  expect_identical(outcome$status, "success")
  expect_identical(outcome$message, paste(episodic_object_ref(case$cluster_id, "outbreak", lang = "en"), "was shared (HTTP 201)."))
  expect_identical(outcome$link, "https://regional.example/o/1")
  received <- readLines(server$received)
  expect_identical(received[1], "Bearer secret-token")
  body <- jsonlite::fromJSON(paste(received[-1], collapse = "\n"))
  expect_identical(body$outbreak$cluster_id, case$cluster_id)
  expect_identical(body$shared_by, "epi1")
  expect_null(body$cases)
})

test_that("the reference plugin reports a refusal by the platform as the plugin's own error", {
  skip_if_not_installed("httr2")
  server <- reference_plugin_server(503)
  withr::local_envvar(SHARE_ENDPOINT_URL = server$url, SHARE_ENDPOINT_TOKEN = "secret-token")
  outcome <- reference_plugin_run()$run()
  expect_identical(outcome$status, "error")
  expect_identical(outcome$message, "The platform answered HTTP 503.")
})

test_that("the reference plugin does not start without its address", {
  withr::local_envvar(SHARE_ENDPOINT_URL = NA, SHARE_ENDPOINT_TOKEN = "secret-token")
  outcome <- reference_plugin_run()$run()
  expect_identical(outcome$status, "failed")
  expect_match(outcome$detail, "SHARE_ENDPOINT_URL", fixed = TRUE)
})
