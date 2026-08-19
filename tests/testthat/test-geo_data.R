test_that("episode_geo_source_resolve() returns NULL when sf is not installed", {
  skip_if(requireNamespace("sf", quietly = TRUE))
  expect_null(episode_geo_source_resolve())
  expect_null(episode_geo_source_default())
  expect_null(episode_geo_join(data.frame(label = "1234", n = 3)))
})

test_that("episode_geo_source_default() ships a Netherlands PC4 sf object", {
  skip_if_not_installed("sf")
  geo <- episode_geo_source_default()
  expect_s3_class(geo, "sf")
  expect_true(all(c("pc4", "geometry") %in% names(geo)))
  expect_gt(nrow(geo), 0)
})

test_that("episode_geo_source_resolve() falls back to the default when EPISODE_GEO_DATA is unset or invalid", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODE_GEO_DATA", unset = NA)
  on.exit(if (is.na(old_env)) Sys.unsetenv("EPISODE_GEO_DATA") else Sys.setenv(EPISODE_GEO_DATA = old_env))

  Sys.unsetenv("EPISODE_GEO_DATA")
  expect_s3_class(episode_geo_source_resolve(), "sf")

  Sys.setenv(EPISODE_GEO_DATA = "/no/such/file.rds")
  expect_s3_class(episode_geo_source_resolve(), "sf")
})

test_that("episode_geo_source_resolve() honours an operator-supplied EPISODE_GEO_DATA file", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODE_GEO_DATA", unset = NA)
  on.exit(if (is.na(old_env)) Sys.unsetenv("EPISODE_GEO_DATA") else Sys.setenv(EPISODE_GEO_DATA = old_env))

  default <- episode_geo_source_default()
  custom <- default[seq_len(2), ]
  custom$pc4 <- c("9999", "9998")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(custom, tmp)
  Sys.setenv(EPISODE_GEO_DATA = tmp)

  resolved <- episode_geo_source_resolve()
  expect_equal(sort(as.character(resolved$pc4)), c("9998", "9999"))
})

test_that("episode_geo_join() right-joins case counts onto geographic reference data", {
  skip_if_not_installed("sf")
  geo <- episode_geo_source_default()
  pc4 <- as.character(geo$pc4[1])
  rows <- data.frame(label = pc4, n = 7)

  joined <- episode_geo_join(rows, geo_data = geo)
  expect_s3_class(joined, "sf")
  expect_equal(nrow(joined), nrow(geo))
  expect_equal(joined$n[joined$pc4 == pc4], 7)
})

test_that("episode_geo_join() returns NULL for empty rows or missing geo data", {
  skip_if_not_installed("sf")
  expect_null(episode_geo_join(data.frame(label = character(0), n = numeric(0)), geo_data = episode_geo_source_default()))
})
