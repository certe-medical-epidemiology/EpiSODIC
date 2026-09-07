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

# Directly seeds/reads episodic_graphics_probe_env rather than breaking a
# real graphics device: the point under test is the caching and gating
# logic around the probe result, not whether ragg itself is healthy on
# the machine running the test suite (which the CI runner and a given
# developer's machine may answer differently).
with_graphics_probe_result <- function(result, code) {
  old_done <- episodic_graphics_probe_env$done
  old_result <- episodic_graphics_probe_env$result
  episodic_graphics_probe_env$done <- TRUE
  episodic_graphics_probe_env$result <- result
  on.exit(
    {
      episodic_graphics_probe_env$done <- old_done
      episodic_graphics_probe_env$result <- old_result
    },
    add = TRUE
  )
  force(code)
}

package_issue <- list(
  packages = c("ragg", "systemfonts", "textshaping"),
  kind = "package"
)
build_issue <- list(packages = NULL, kind = "build")

test_that("episodic_graphics_probe() caches its result rather than re-probing every call", {
  with_graphics_probe_result(package_issue, {
    expect_equal(episodic_graphics_probe(), package_issue)
    # Seeding the cache directly (rather than calling the real device) is
    # itself proof of caching: a real probe would never return this exact
    # value unless it read the cache we just set.
    expect_equal(episodic_graphics_probe(), episodic_graphics_probe())
  })
})

test_that("episodic_graphics_probe() returns NULL when the cache says the device is healthy", {
  with_graphics_probe_result(NULL, {
    expect_null(episodic_graphics_probe())
  })
})

test_that("episodic_graphics_error_message() names every implicated package, per language", {
  msg_en <- episodic_graphics_error_message(package_issue, lang = "en")
  expect_match(msg_en, "ragg", fixed = TRUE)
  expect_match(msg_en, "systemfonts", fixed = TRUE)
  expect_match(msg_en, "textshaping", fixed = TRUE)

  msg_nl <- episodic_graphics_error_message(package_issue, lang = "nl")
  expect_match(msg_nl, "ragg", fixed = TRUE)
  expect_false(identical(msg_nl, msg_en))

  msg_single <- episodic_graphics_error_message(
    list(packages = "Cairo", kind = "package"),
    lang = "en"
  )
  expect_match(msg_single, "Cairo", fixed = TRUE)
})

test_that("episodic_graphics_error_message() names no package for a build-level failure, in every language", {
  # No CRAN package is implicated when even R's own built-in PNG device
  # fails to open - unlike the package-mismatch case, there is nothing
  # here to tell someone to reinstall, so the message must not name one.
  for (lang in c("en", "nl", "es", "de", "fr", "ar", "hi", "zh")) {
    msg <- episodic_graphics_error_message(build_issue, lang = lang)
    expect_false(grepl("[[", msg, fixed = TRUE), info = lang)
    expect_gt(nchar(msg), 0)
  }
})

test_that("dossier chart panels report the specific package(s) instead of attempting to render, when the graphics device is broken", {
  with_graphics_probe_result(package_issue, {
    expected <- episodic_graphics_error_message(package_issue, lang = "en")

    # con/cluster_id/obj are never touched on this path: the graphics
    # check runs before any of them would be read.
    epicurve_html <- as.character(episodic_ui_epicurve_panel(
      con = NULL,
      cluster_id = NULL,
      obj = NULL,
      lang = "en"
    ))
    expect_true(grepl(expected, epicurve_html, fixed = TRUE))

    trend_html <- as.character(episodic_ui_trend_panel(
      con = NULL,
      obj = NULL,
      lang = "en"
    ))
    expect_true(grepl(expected, trend_html, fixed = TRUE))

    rt_html <- as.character(episodic_ui_rt_panel(
      obj = list(rt_applicable = TRUE, rt = data.frame(t = 1)),
      lang = "en"
    ))
    expect_true(grepl(expected, rt_html, fixed = TRUE))

    denominator_html <- as.character(episodic_ui_denominator_panel(
      obj = list(denominator = list(series = data.frame(x = 1:2))),
      lang = "en"
    ))
    expect_true(grepl(expected, denominator_html, fixed = TRUE))
  })
})

test_that("dossier chart panels report a build-level message, naming no package, when even R's built-in device fails", {
  with_graphics_probe_result(build_issue, {
    expected <- episodic_graphics_error_message(build_issue, lang = "en")
    epicurve_html <- as.character(episodic_ui_epicurve_panel(
      con = NULL,
      cluster_id = NULL,
      obj = NULL,
      lang = "en"
    ))
    expect_true(grepl(expected, epicurve_html, fixed = TRUE))
    # The package-specific message tells someone to reinstall a named
    # package; there is no such package here, so that exact phrasing must
    # not appear (the build message legitimately uses "reinstalled" once,
    # in "not a package that can be reinstalled").
    expect_false(grepl("Reinstall them", epicurve_html, fixed = TRUE))
  })
})

test_that("dossier chart panels render their real plot, not the graphics message, when the device is healthy", {
  with_graphics_probe_result(NULL, {
    off_topic <- episodic_graphics_error_message(package_issue, lang = "en")
    env <- app_read_setup()
    on.exit(DBI::dbDisconnect(env$con), add = TRUE)
    html <- as.character(episodic_ui_dossier(env$con, env$cluster_id, lang = "en"))
    expect_false(grepl(off_topic, html, fixed = TRUE))
  })
})

test_that("pathogen chart panels report the specific package(s) instead of attempting to render, when the graphics device is broken", {
  cairo_issue <- list(packages = "Cairo", kind = "package")
  with_graphics_probe_result(cairo_issue, {
    expected <- episodic_graphics_error_message(cairo_issue, lang = "en")

    curve_html <- as.character(episodic_ui_pathogen_curve_panel(
      screen = list(
        weekly = data.frame(
          week_start = as.Date("2024-01-01") + 7 * (0:5),
          n_cases = c(1, 2, 3, 4, 5, 6)
        ),
        mem = list(thresholds = NULL),
        seasonal = FALSE
      ),
      lang = "en"
    ))
    expect_true(grepl(expected, curve_html, fixed = TRUE))

    overlay_html <- as.character(episodic_ui_pathogen_overlay_panel(
      screen = list(overlay = list(kind = "season")),
      lang = "en"
    ))
    expect_true(grepl(expected, overlay_html, fixed = TRUE))

    rt_html <- as.character(episodic_ui_pathogen_rt_panel(
      screen = list(rt = data.frame(t = 1), rt_unavailable_reason = NA),
      lang = "en"
    ))
    expect_true(grepl(expected, rt_html, fixed = TRUE))

    denominator_html <- as.character(episodic_ui_pathogen_denominator_panel(
      screen = list(denominator = data.frame(x = 1:2)),
      lang = "en"
    ))
    expect_true(grepl(expected, denominator_html, fixed = TRUE))
  })
})

test_that("a broken graphics device falls the geo panel back to the bar breakdown, with the reason stated", {
  ragg_issue <- list(packages = "ragg", kind = "package")
  with_graphics_probe_result(ragg_issue, {
    local_mocked_bindings(
      episodic_ui_geo_map_chart = function(...) ggplot2::ggplot()
    )
    expected <- episodic_graphics_error_message(ragg_issue, lang = "en")
    html <- as.character(episodic_ui_geo_panel(
      obj = list(
        concentration = list(
          rows = data.frame(label = "9711", n = 3),
          n_unknown_pc = 0,
          province_error = NA_character_
        ),
        n_cases = 3
      ),
      lang = "en"
    ))
    expect_true(grepl(expected, html, fixed = TRUE))
  })
})
