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
  expect_true(all(c("pc", "geometry") %in% names(geo)))
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
  custom$pc <- c("9999", "9998")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(custom, tmp)
  Sys.setenv(EPISODE_GEO_DATA = tmp)

  resolved <- episode_geo_source_resolve()
  expect_equal(sort(as.character(resolved$pc)), c("9998", "9999"))
})

test_that("episode_geo_join() right-joins case counts onto geographic reference data", {
  skip_if_not_installed("sf")
  geo <- episode_geo_source_default()
  pc <- as.character(geo$pc[1])
  rows <- data.frame(label = pc, n = 7)

  joined <- episode_geo_join(rows, geo_data = geo)
  expect_s3_class(joined, "sf")
  expect_equal(nrow(joined), nrow(geo))
  expect_equal(joined$n[joined$pc == pc], 7)
})

test_that("episode_geo_join() returns NULL for empty rows or missing geo data", {
  skip_if_not_installed("sf")
  expect_null(episode_geo_join(data.frame(label = character(0), n = numeric(0)), geo_data = episode_geo_source_default()))
})

test_that("episode_geo_overlay_resolve() returns NULL when unset, invalid, or sf is not installed", {
  old_env <- Sys.getenv("EPISODE_GEO_DATA_OVERLAY", unset = NA)
  on.exit(if (is.na(old_env)) Sys.unsetenv("EPISODE_GEO_DATA_OVERLAY") else Sys.setenv(EPISODE_GEO_DATA_OVERLAY = old_env))

  Sys.unsetenv("EPISODE_GEO_DATA_OVERLAY")
  expect_null(episode_geo_overlay_resolve())

  Sys.setenv(EPISODE_GEO_DATA_OVERLAY = "/no/such/file.rds")
  expect_null(episode_geo_overlay_resolve())
})

test_that("episode_geo_overlay_resolve() loads an operator-supplied overlay needing only a geometry column", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODE_GEO_DATA_OVERLAY", unset = NA)
  on.exit(if (is.na(old_env)) Sys.unsetenv("EPISODE_GEO_DATA_OVERLAY") else Sys.setenv(EPISODE_GEO_DATA_OVERLAY = old_env))

  base <- episode_geo_source_default()
  overlay <- base[seq_len(2), "geometry"]  # no pc column at all - the point of this contract
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(overlay, tmp)
  Sys.setenv(EPISODE_GEO_DATA_OVERLAY = tmp)

  resolved <- episode_geo_overlay_resolve()
  expect_s3_class(resolved, "sf")
  expect_false("pc" %in% names(resolved))
  expect_equal(nrow(resolved), 2)
})

test_that("episode_geo_overlay_resolve() rejects a file with no geometry column", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODE_GEO_DATA_OVERLAY", unset = NA)
  on.exit(if (is.na(old_env)) Sys.unsetenv("EPISODE_GEO_DATA_OVERLAY") else Sys.setenv(EPISODE_GEO_DATA_OVERLAY = old_env))

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(data.frame(x = 1:2), tmp)
  Sys.setenv(EPISODE_GEO_DATA_OVERLAY = tmp)

  expect_null(episode_geo_overlay_resolve())
})

test_that("episode_ui_geo_map_chart() draws the overlay layer without disturbing the choropleth when present, and ignores it gracefully when absent", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODE_GEO_DATA_OVERLAY", unset = NA)
  on.exit(if (is.na(old_env)) Sys.unsetenv("EPISODE_GEO_DATA_OVERLAY") else Sys.setenv(EPISODE_GEO_DATA_OVERLAY = old_env))

  geo <- episode_geo_source_default()
  rows <- data.frame(label = as.character(geo$pc[1]), n = 4)

  Sys.unsetenv("EPISODE_GEO_DATA_OVERLAY")
  plot_without <- episode_ui_geo_map_chart(rows)
  expect_s3_class(plot_without, "ggplot")
  expect_equal(length(plot_without$layers), 1)

  overlay <- geo[seq_len(2), "geometry"]
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(overlay, tmp)
  Sys.setenv(EPISODE_GEO_DATA_OVERLAY = tmp)

  plot_with <- episode_ui_geo_map_chart(rows)
  expect_s3_class(plot_with, "ggplot")
  expect_equal(length(plot_with$layers), 2)
  overlay_layer <- plot_with$layers[[2]]
  expect_true(is.na(overlay_layer$aes_params$fill))
  expect_equal(overlay_layer$aes_params$linewidth, 0.6)
})
