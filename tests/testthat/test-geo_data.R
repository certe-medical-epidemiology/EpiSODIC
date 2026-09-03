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

test_that("episodic_geo_source_resolve() returns NULL when sf is not installed", {
  skip_if(
    requireNamespace("sf", quietly = TRUE),
    "sf is installed; this covers the fallback for when it is not"
  )
  expect_null(episodic_geo_source_resolve())
  expect_null(episodic_geo_source_default())
  expect_null(episodic_geo_join(data.frame(label = "1234", n = 3)))
})

test_that("episodic_geo_source_default() ships a Netherlands PC sf object", {
  skip_if_not_installed("sf")
  geo <- episodic_geo_source_default()
  expect_s3_class(geo, "sf")
  expect_true(all(c("pc", "geometry") %in% names(geo)))
  expect_gt(nrow(geo), 0)
})

test_that("episodic_geo_source_resolve() falls back to the default when EPISODIC_GEO_DATA is unset or invalid", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODIC_GEO_DATA", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA = old_env)
    }
  )

  Sys.unsetenv("EPISODIC_GEO_DATA")
  expect_s3_class(episodic_geo_source_resolve(), "sf")

  Sys.setenv(EPISODIC_GEO_DATA = "/no/such/file.rds")
  expect_s3_class(episodic_geo_source_resolve(), "sf")
})

test_that("episodic_geo_source_resolve() honours an operator-supplied EPISODIC_GEO_DATA file", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODIC_GEO_DATA", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA = old_env)
    }
  )

  default <- episodic_geo_source_default()
  custom <- default[seq_len(2), ]
  custom$pc <- c("9999", "9998")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(custom, tmp)
  Sys.setenv(EPISODIC_GEO_DATA = tmp)

  resolved <- episodic_geo_source_resolve()
  expect_equal(sort(as.character(resolved$pc)), c("9998", "9999"))
})

test_that("episodic_geo_join() right-joins case counts onto geographic reference data", {
  skip_if_not_installed("sf")
  geo <- episodic_geo_source_default()
  pc <- as.character(geo$pc[1])
  rows <- data.frame(label = pc, n = 7)

  joined <- episodic_geo_join(rows, geo_data = geo)
  expect_s3_class(joined, "sf")
  expect_equal(nrow(joined), nrow(geo))
  expect_equal(joined$n[joined$pc == pc], 7)
})

test_that("episodic_geo_join() returns NULL for empty rows or missing geo data", {
  skip_if_not_installed("sf")
  expect_null(episodic_geo_join(
    data.frame(label = character(0), n = numeric(0)),
    geo_data = episodic_geo_source_default()
  ))
})

test_that("episodic_geo_overlay_resolve() returns NULL when unset, invalid, or sf is not installed", {
  old_env <- Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = old_env)
    }
  )

  Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
  expect_null(episodic_geo_overlay_resolve())

  Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = "/no/such/file.rds")
  expect_null(episodic_geo_overlay_resolve())
})

test_that("episodic_geo_overlay_resolve() loads an operator-supplied overlay needing only a geometry column", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = old_env)
    }
  )

  base <- episodic_geo_source_default()
  overlay <- base[seq_len(2), "geometry"] # no pc column at all - the point of this contract
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(overlay, tmp)
  Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = tmp)

  resolved <- episodic_geo_overlay_resolve()
  expect_s3_class(resolved, "sf")
  expect_false("pc" %in% names(resolved))
  expect_equal(nrow(resolved), 2)
})

test_that("episodic_geo_overlay_resolve() rejects a file with no geometry column", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = old_env)
    }
  )

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(data.frame(x = 1:2), tmp)
  Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = tmp)

  expect_null(episodic_geo_overlay_resolve())
})

test_that("episodic_ui_geo_map_chart() draws the overlay layer without disturbing the choropleth when present, and ignores it gracefully when absent", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = old_env)
    }
  )

  geo <- episodic_geo_source_default()
  rows <- data.frame(label = as.character(geo$pc[1]), n = 4)

  Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
  plot_without <- episodic_ui_geo_map_chart(rows)
  expect_s3_class(plot_without, "ggplot")
  # choropleth + the PC/count labels drawn over the case-bearing areas
  expect_equal(length(plot_without$layers), 2)

  overlay <- geo[seq_len(2), "geometry"]
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(overlay, tmp)
  Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = tmp)

  plot_with <- episodic_ui_geo_map_chart(rows)
  expect_s3_class(plot_with, "ggplot")
  # choropleth, overlay, labels - the overlay stays layer 2, drawn over
  # the choropleth but under the labels
  expect_equal(length(plot_with$layers), 3)
  overlay_layer <- plot_with$layers[[2]]
  expect_true(is.na(overlay_layer$aes_params$fill))
  expect_equal(overlay_layer$aes_params$linewidth, 0.6)
})

test_that("episodic_ui_geo_map_chart() frames on the case-bearing areas, not the whole reference set", {
  skip_if_not_installed("sf")
  old_env <- Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
  on.exit(
    if (is.na(old_env)) {
      Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")
    } else {
      Sys.setenv(EPISODIC_GEO_DATA_OVERLAY = old_env)
    }
  )
  Sys.unsetenv("EPISODIC_GEO_DATA_OVERLAY")

  geo <- episodic_geo_source_default()
  skip_if(is.null(geo) || nrow(geo) < 50, "shipped geometry unavailable")

  rows <- data.frame(label = as.character(geo$pc[1]), n = 4)
  plot <- episodic_ui_geo_map_chart(rows)
  expect_s3_class(plot, "ggplot")

  # The frame has to be a small window on the reference set, otherwise
  # the postcode labels the panel exists to convey are unreadable.
  limits <- plot$coordinates$limits
  full <- sf::st_bbox(geo)
  frame_width <- diff(limits$x)
  expect_true(is.finite(frame_width))
  expect_lt(frame_width, (full[["xmax"]] - full[["xmin"]]) / 3)
})

test_that("episodic_geo_frame() pads by the reference extent when the cluster sits in one area", {
  skip_if_not_installed("sf")
  geo <- episodic_geo_source_default()
  skip_if(is.null(geo) || nrow(geo) < 50, "shipped geometry unavailable")

  matched <- geo[1, ]
  frame <- episodic_geo_frame(
    geo,
    matched,
    pad_share = 0.45,
    min_pad_share = 0.02
  )
  own <- sf::st_bbox(matched)
  # Strictly wider than the single area itself: a frame drawn tight
  # around one polygon shows no context at all.
  expect_lt(frame$xlim[1], own[["xmin"]])
  expect_gt(frame$xlim[2], own[["xmax"]])
  expect_equal(sf::st_crs(frame$bbox), sf::st_crs(geo))
})

test_that("episodic_geo_labels() labels the biggest areas first and caps how many it draws", {
  skip_if_not_installed("sf")
  geo <- episodic_geo_source_default()
  skip_if(is.null(geo) || nrow(geo) < 50, "shipped geometry unavailable")

  matched <- geo[seq_len(5), ]
  matched$n <- c(1L, 9L, 3L, 7L, 5L)
  labels <- episodic_geo_labels(matched, max_labels = 3L)
  expect_equal(nrow(labels), 3)
  # highest count first, and the label carries the count as well as the PC
  expect_match(labels$label[1], "9$")
  expect_true(all(c("x", "y", "label") %in% names(labels)))
})


test_that("an unset EPISODIC_PC_PROVINCE_MAP falls back to the shipped demo ranges", {
  expect_true(is.na(episodic_pc_province_map_problem(NA_character_)))
  expect_true(is.na(episodic_pc_province_map_problem("")))
  expect_null(episodic_pc_province_map_resolve(NA_character_))
  expect_equal(
    episodic_pc_to_province(c("9713", "8911", "7411", "1012"), path = NA),
    c("PROV_GRONINGEN", "PROV_FRYSLAN", "PROV_DRENTHE", NA)
  )
})

test_that("a configured EPISODIC_PC_PROVINCE_MAP that cannot be used is named, not fallen back from", {
  # Quietly substituting the demo ranges here hands an operator who
  # supplied their own mapping a lattice built on somebody else's
  # provinces, with nothing anywhere to say why - the one outcome this
  # package refuses to produce.
  problem <- function(path) episodic_pc_province_map_problem(path)

  expect_match(
    problem(file.path(tempdir(), "no-such-file.csv")),
    "no file exists there"
  )

  wrong_cols <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(postcode = "9713", province = "Groningen"),
    wrong_cols,
    row.names = FALSE
  )
  on.exit(unlink(wrong_cols), add = TRUE)
  expect_match(problem(wrong_cols), "province_code", fixed = TRUE)
  # and it names what the file actually has, so the mismatch is visible
  expect_match(problem(wrong_cols), "postcode", fixed = TRUE)

  empty <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(pc = character(0), province_code = character(0)),
    empty,
    row.names = FALSE
  )
  on.exit(unlink(empty), add = TRUE)
  expect_match(problem(empty), "no rows", fixed = TRUE)

  repeated <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      pc = c("9713", "9713"),
      province_code = c("Groningen", "Drenthe")
    ),
    repeated,
    row.names = FALSE
  )
  on.exit(unlink(repeated), add = TRUE)
  expect_match(problem(repeated), "more than one", fixed = TRUE)

  # the lookup underneath stays total: it is what both the cron and the
  # dashboard derive stream membership through, so it must not throw
  for (path in c(wrong_cols, empty, repeated)) {
    expect_null(episodic_pc_province_map_resolve(path))
    expect_silent(episodic_pc_to_province("9713", path = path))
  }
})

test_that("a detection run refuses to start on a PC-to-province mapping it cannot use", {
  wrong_cols <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(postcode = "9713", province = "Groningen"),
    wrong_cols,
    row.names = FALSE
  )
  old <- Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)
  on.exit(
    {
      unlink(wrong_cols)
      if (is.na(old)) {
        Sys.unsetenv("EPISODIC_PC_PROVINCE_MAP")
      } else {
        Sys.setenv(EPISODIC_PC_PROVINCE_MAP = old)
      }
    },
    add = TRUE
  )
  Sys.setenv(EPISODIC_PC_PROVINCE_MAP = wrong_cols)

  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  cases <- episodic_synthetic_cases(
    start_date = as.Date("2024-06-01"),
    end_date = as.Date("2024-06-30"),
    seed = 3
  )
  # It refuses with the pre-run checks, before the transaction opens, so
  # nothing is written and the run row carries the reason for the
  # Activity screen to show.
  expect_error(
    suppressMessages(episodic_run_cron(
      db_path = db,
      cases = cases,
      run_date = as.Date("2024-06-30")
    )),
    "province_code",
    fixed = TRUE
  )
  con <- episodic_db_connect(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  run <- DBI::dbGetQuery(
    con,
    "SELECT status, error_text FROM episodic_detection_run"
  )
  expect_equal(run$status, "failed")
  expect_match(run$error_text, "province_code", fixed = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_case")$n,
    0
  )
})

test_that("a usable EPISODIC_PC_PROVINCE_MAP is what the lattice maps postcodes with", {
  mapping <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      pc = c("9713", "8911"),
      province_code = c("Groningen", "Fryslan")
    ),
    mapping,
    row.names = FALSE
  )
  on.exit(unlink(mapping), add = TRUE)

  expect_true(is.na(episodic_pc_province_map_problem(mapping)))
  expect_equal(
    episodic_pc_to_province(c("9713", "8911", "1012"), path = mapping),
    c("Groningen", "Fryslan", NA)
  )
  # the demo ranges are not consulted at all once a mapping is supplied:
  # 7411 is Drenthe under the demo default and nothing under this file
  expect_equal(episodic_pc_to_province("7411", path = mapping), NA_character_)
})

test_that("the dossier shows the province next to each postcode, and says so when it cannot", {
  mapping <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(pc = "9713", province_code = "Groningen"),
    mapping,
    row.names = FALSE
  )
  old <- Sys.getenv("EPISODIC_PC_PROVINCE_MAP", unset = NA)
  on.exit(
    {
      unlink(mapping)
      if (is.na(old)) {
        Sys.unsetenv("EPISODIC_PC_PROVINCE_MAP")
      } else {
        Sys.setenv(EPISODIC_PC_PROVINCE_MAP = old)
      }
    },
    add = TRUE
  )

  Sys.setenv(EPISODIC_PC_PROVINCE_MAP = mapping)
  cases <- data.frame(pc = c("9713", "9713", "1012"), stringsAsFactors = FALSE)
  conc <- episodic_app_concentration(cases)
  expect_equal(conc$rows$province[conc$rows$label == "9713"], "Groningen")
  expect_true(is.na(conc$rows$province[conc$rows$label == "1012"]))
  expect_true(is.na(conc$province_error))

  # `label` stays the bare PC: it is what the map geometry joins on
  expect_setequal(conc$rows$label, c("9713", "1012"))
  bars <- episodic_ui_geo_bar_rows(conc$rows, lang = "en")
  expect_true(paste0("9713 \u00b7 Groningen") %in% bars$label)
  expect_true("1012" %in% bars$label)

  # a mapping that cannot be read does not take the dossier down; the
  # reason reaches the reader in the panel instead
  Sys.setenv(EPISODIC_PC_PROVINCE_MAP = file.path(tempdir(), "gone.csv"))
  broken <- episodic_app_concentration(cases)
  expect_true(all(is.na(broken$rows$province)))
  expect_false(is.na(broken$province_error))
  expect_match(broken$province_error, "no file exists there", fixed = TRUE)
  panel <- as.character(episodic_ui_geo_panel(
    list(concentration = broken, n_cases = 3L),
    lang = "en"
  ))
  expect_true(grepl("no file exists there", panel, fixed = TRUE))
  expect_false(grepl("[[", panel, fixed = TRUE))
})
