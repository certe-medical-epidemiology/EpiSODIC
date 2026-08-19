test_that("episode_palette() and episode_brand_bar() return usable hex colours, role-named", {
  pal <- episode_palette()
  expect_type(pal, "list")
  expect_true(all(c("ink", "muted", "primary", "secondary", "tertiary", "success", "warning", "danger") %in% names(pal)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", unlist(pal))))

  bar <- episode_brand_bar()
  expect_length(bar, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", bar)))
})

test_that("an instance palette override propagates, other roles keep the shipped default", {
  override_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(primary = "#123456"), override_path)

  pal <- episode_palette_config_resolve(override_path)
  expect_equal(pal$primary, "#123456")
  expect_false(identical(pal$primary_dark, "#123456"))  # untouched keys keep the shipped default
  expect_false(identical(pal$secondary, "#123456"))
})

test_that("episode_palette_from_certestyle() maps certestyle's own (Dutch) key names onto our role names, one hue per role, falling back per-role when absent", {
  # certestyle::certe.colours is a named character vector, not a list -
  # matched here so a regression to `cc$name` (invalid on an atomic
  # vector) fails this test rather than only failing for real users.
  fake_cc <- c(blauw = "#000001", geel = "#000002")  # only two of the hues certestyle would normally provide
  pal <- episode_palette_from_certestyle(fake_cc)

  expect_equal(pal$primary, "#000001")   # blauw
  expect_equal(pal$warning, "#000002")   # geel
  # a role certestyle didn't supply falls back to our own shipped default, not NULL
  expect_equal(pal$success, episode_palette_config_resolve()$success)
  # each of Certe's real hues keeps its own role - none merged into another
  expect_false(identical(pal$primary, pal$warning))
})

test_that("app widgets render to shiny tags without error, including empty-data edge cases", {
  expect_s3_class(episode_ui_chip("Norovirus", "#4A647D", filled = TRUE), "shiny.tag")
  expect_s3_class(episode_ui_panel("Title", shiny::tags$p("body")), "shiny.tag")
  expect_s3_class(episode_ui_panel_empty("Title", "Nothing here"), "shiny.tag")
  expect_s3_class(episode_ui_stat("Label", 5, sub = "sub"), "shiny.tag")

  rows <- data.frame(label = c("A", "B"), n = c(3, 1))
  expect_s3_class(episode_ui_bars(rows), "shiny.tag.list")
  expect_s3_class(episode_ui_bars(data.frame(label = character(0), n = integer(0))), "shiny.tag")

  demo <- data.frame(band = c("0-19", "20-39"), m = c(2, 1), v = c(1, 3))
  expect_s3_class(episode_ui_pyramid(demo), "shiny.tag.list")
  expect_s3_class(episode_ui_pyramid(data.frame(band = character(0), m = integer(0), v = integer(0))), "shiny.tag")

  expect_s3_class(episode_ui_state_dot("new"), "shiny.tag")
  expect_type(episode_ui_state_colour("new"), "character")
  expect_type(episode_ui_state_colour("unknown_state"), "character")
})

test_that("episode_ui_italicise_taxon() HTML-escapes unconditionally", {
  out <- episode_ui_italicise_taxon(c("A&B <weird>", "Influenza A"))
  expect_equal(out[1], "A&amp;B &lt;weird&gt;")
})

test_that("episode_ui_italicise_taxon() italicises AMR-recognised binomials, leaves other names alone", {
  skip_if_not_installed("AMR")
  out <- episode_ui_italicise_taxon(c("Escherichia coli", "Influenza A"))
  expect_equal(out[1], "<i>Escherichia coli</i>")
  expect_equal(out[2], "Influenza A")  # not a binomial AMR recognises -> unitalicised
})

test_that("episode_app_ui() assembles a full page without error, in both languages", {
  expect_s3_class(episode_app_ui("nl"), "shiny.tag.list")
  expect_s3_class(episode_app_ui("en"), "shiny.tag.list")
})

test_that("chart builders produce ggplot objects for typical and edge-case inputs", {
  curve <- data.frame(sample_date = as.Date("2025-01-01") + 0:3, n_cases = c(1, 2, 0, 3), incomplete = c(FALSE, FALSE, TRUE, TRUE))
  expect_s3_class(episode_ui_epi_curve_chart(curve, lang = "nl"), "ggplot")
  expect_s3_class(episode_ui_epi_curve_chart(curve, lang = "en"), "ggplot")

  trend <- data.frame(week_start = as.Date("2025-01-06") + (0:5) * 7, n_cases = c(1, 2, 3, 4, 2, 1),
                       expected = c(1, 1, 1, 1, 1, 1), upperbound = c(2, 2, 2, 2, 2, 2))
  expect_s3_class(episode_ui_trend_chart(trend), "ggplot")

  series <- data.frame(week_start = as.Date("2025-01-06") + (0:3) * 7, n_tests = c(10, 12, 8, 15),
                        n_cases = c(1, 3, 2, 4), positivity = c(0.1, 0.25, 0.25, 0.27))
  expect_s3_class(episode_ui_denominator_chart(series), "ggplot")
})
