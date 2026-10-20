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

# Where an exchange file goes and comes from: a path, or HTTP(S) PUT and
# GET with credentials read from named environment variables. HTTP is
# exercised against httr2's own mocked responses, never the network.

transport_reason <- function(expr) {
  tryCatch(expr, episodic_exchange_error = function(e) e$reason)
}

test_that("an endpoint is refused unless it is unambiguous", {
  expect_error(episodic_exchange_endpoint("file", direction = "export"), "needs a path")
  expect_error(
    episodic_exchange_endpoint("http_get", direction = "export"),
    "one of: file, http_put"
  )
  expect_error(
    episodic_exchange_endpoint("http_put", "https://x.example/f", "URL_VAR", direction = "export"),
    "not both and not neither"
  )
  expect_error(
    episodic_exchange_endpoint("http_put", "https://x.example/f", username_env = "U", direction = "export"),
    "both a user name variable and a password variable"
  )
  expect_error(
    episodic_exchange_endpoint("http_put", "https://x.example/f", token_env = "not a name", direction = "export"),
    "not an environment variable name"
  )
  expect_error(
    episodic_exchange_endpoint("file", "/tmp/x", token_env = "TOKEN", direction = "export"),
    "path only"
  )
})

test_that("a URL carrying credentials is never stored", {
  expect_identical(
    transport_reason(episodic_exchange_endpoint("http_put", "https://user:pw@x.example/f", direction = "export")),
    "transport_config"
  )
  expect_identical(
    transport_reason(episodic_exchange_endpoint("http_get", "https://x.example/f?X-Amz-Signature=abc", direction = "import")),
    "transport_config"
  )
  expect_identical(
    transport_reason(episodic_exchange_endpoint("http_get", "ftp://x.example/f", direction = "import")),
    "transport_config"
  )
  # Named by its variable instead, it is recorded and shown as the name.
  endpoint <- episodic_exchange_endpoint("http_get", location_env = "PARTNER_URL", direction = "import")
  expect_identical(episodic_exchange_endpoint_label(endpoint), "env:PARTNER_URL")
})

test_that("a file endpoint writes in one step and reads back the same bytes", {
  path <- tempfile(fileext = ".episodic")
  on.exit(unlink(path))
  out <- episodic_exchange_endpoint("file", path, direction = "export")
  inn <- episodic_exchange_endpoint("file", path, direction = "import")
  bytes <- as.raw(0:255)
  episodic_exchange_endpoint_write(out, bytes)
  expect_identical(episodic_exchange_endpoint_read(inn), bytes)
  expect_length(list.files(dirname(path), pattern = "^\\.episodic-exchange-"), 0L)
})

test_that("an upload is a PUT of the file's bytes with the named credentials", {
  skip_if_not_installed("httr2", "1.2.0")
  seen <- NULL
  withr::local_envvar(EXCHANGE_TEST_TOKEN = "s3cr3t")
  endpoint <- episodic_exchange_endpoint(
    "http_put", "https://dav.example/exchange/file.episodic",
    token_env = "EXCHANGE_TEST_TOKEN", direction = "export"
  )
  bytes <- as.raw(1:10)
  httr2::with_mocked_responses(
    function(req) {
      seen <<- req
      httr2::response(201)
    },
    episodic_exchange_endpoint_write(endpoint, bytes)
  )
  expect_identical(httr2::req_get_method(seen), "PUT")
  expect_identical(httr2::req_get_body(seen), bytes)
  expect_identical(
    httr2::req_get_headers(seen, redacted = "reveal")$Authorization,
    "Bearer s3cr3t"
  )
})

test_that("an upload the server refuses fails loudly, with the status", {
  skip_if_not_installed("httr2", "1.2.0")
  endpoint <- episodic_exchange_endpoint("http_put", "https://dav.example/f", direction = "export")
  expect_error(
    httr2::with_mocked_responses(
      function(req) httr2::response(403),
      episodic_exchange_endpoint_write(endpoint, raw(1))
    ),
    "HTTP 403 \\(check the credentials\\)"
  )
})

test_that("a fetch is a GET that returns the body, and a missing file fails loudly", {
  skip_if_not_installed("httr2", "1.2.0")
  endpoint <- episodic_exchange_endpoint("http_get", "https://dav.example/f", direction = "import")
  body <- as.raw(c(1, 2, 3))
  got <- httr2::with_mocked_responses(
    function(req) httr2::response(200, body = body),
    episodic_exchange_endpoint_read(endpoint)
  )
  expect_identical(got, body)
  expect_identical(
    transport_reason(httr2::with_mocked_responses(
      function(req) httr2::response(404),
      episodic_exchange_endpoint_read(endpoint)
    )),
    "transport"
  )
})

test_that("a URL held in a variable is read at use, and never repeated in an error", {
  skip_if_not_installed("httr2", "1.2.0")
  endpoint <- episodic_exchange_endpoint("http_get", location_env = "EXCHANGE_TEST_URL", direction = "import")
  withr::local_envvar(EXCHANGE_TEST_URL = NA)
  expect_identical(transport_reason(episodic_exchange_endpoint_read(endpoint)), "environment")
  withr::local_envvar(EXCHANGE_TEST_URL = "https://bucket.example/f?X-Amz-Signature=SECRETSIG")
  condition <- tryCatch(
    httr2::with_mocked_responses(
      function(req) stop("Could not resolve https://bucket.example/f?X-Amz-Signature=SECRETSIG"),
      episodic_exchange_endpoint_read(endpoint)
    ),
    episodic_exchange_error = function(e) e
  )
  expect_identical(condition$reason, "transport")
  expect_false(grepl("SECRETSIG", conditionMessage(condition), fixed = TRUE))
  expect_match(conditionMessage(condition), "env:EXCHANGE_TEST_URL", fixed = TRUE)
})

test_that("a connection test reports a missing variable before trying anything", {
  endpoint <- episodic_exchange_endpoint(
    "http_put", "https://dav.example/f",
    username_env = "EXCHANGE_TEST_USER", password_env = "EXCHANGE_TEST_PASS",
    direction = "export"
  )
  withr::local_envvar(EXCHANGE_TEST_USER = "me", EXCHANGE_TEST_PASS = NA)
  result <- episodic_exchange_endpoint_test(endpoint)
  keys <- vapply(result$lines, function(l) l$key, character(1))
  expect_identical(keys, c("exchange.test.env_set", "exchange.test.env_missing"))
  expect_identical(result$lines[[2]]$params$variable, "EXCHANGE_TEST_PASS")
})

test_that("a connection test of an export path checks the directory and writes nothing", {
  dir <- tempfile("exchange-out-")
  endpoint <- episodic_exchange_endpoint("file", file.path(dir, "f.episodic"), direction = "export")
  missing <- episodic_exchange_endpoint_test(endpoint)
  expect_identical(missing$lines[[1]]$key, "exchange.test.dir_missing")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  present <- episodic_exchange_endpoint_test(endpoint)
  expect_identical(present$lines[[1]]$key, "exchange.test.dir_ok")
  expect_true(isTRUE(present$lines[[1]]$ok))
  expect_length(list.files(dir), 0L)
})

test_that("a connection test of an upload says what it cannot know", {
  skip_if_not_installed("httr2", "1.2.0")
  endpoint <- episodic_exchange_endpoint("http_put", "https://dav.example/f", direction = "export")
  result <- httr2::with_mocked_responses(
    function(req) {
      expect_identical(httr2::req_get_method(req), "OPTIONS")
      httr2::response(405)
    },
    episodic_exchange_endpoint_test(endpoint)
  )
  keys <- vapply(result$lines, function(l) l$key, character(1))
  expect_identical(keys, c("exchange.test.reachable", "exchange.test.upload_unknown"))
  expect_true(is.na(result$lines[[2]]$ok))
  refused <- httr2::with_mocked_responses(
    function(req) httr2::response(401),
    episodic_exchange_endpoint_test(endpoint)
  )
  expect_identical(refused$lines[[1]]$key, "exchange.test.refused")
})

test_that("a connection test of an import reads the file the next run will read", {
  path <- tempfile(fileext = ".episodic")
  endpoint <- episodic_exchange_endpoint("file", path, direction = "import")
  failed <- episodic_exchange_endpoint_test(endpoint)
  expect_identical(failed$lines[[1]]$key, "exchange.test.read_failed")
  expect_null(failed$bytes)
  writeBin(as.raw(1:4), path)
  on.exit(unlink(path))
  ok <- episodic_exchange_endpoint_test(endpoint)
  expect_identical(ok$lines[[1]]$key, "exchange.test.read_ok")
  expect_identical(ok$bytes, as.raw(1:4))
})
