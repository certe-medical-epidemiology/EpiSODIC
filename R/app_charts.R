#' Chart panels
#'
#' Static, not
#' interactive - keeping the app's dependency footprint to a single,
#' ubiquitous CRAN plotting package rather than an htmlwidgets stack is a
#' deliberate simplification; see `QUESTIONS.md`.
#' @name app_charts
#' @importFrom rlang .data
NULL

#' @keywords internal
#' @noRd
episode_chart_theme <- function() {
  pal <- episode_palette()
  ggplot2::theme_minimal(base_family = "") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = pal$border, linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = pal$faint, size = 9),
      axis.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 9, colour = pal$muted)
    )
}

#' @rdname app_charts
#' @param curve A data frame from `episode_app_epi_curve()`.
#' @param lang Session language.
#' @examples
#' curve <- data.frame(
#'   sample_date = seq(as.Date("2025-01-01"), by = "day", length.out = 14),
#'   n_cases = c(1, 0, 2, 1, 3, 2, 4, 3, 5, 2, 1, 0, 1, 2),
#'   incomplete = c(rep(FALSE, 12), TRUE, TRUE)
#' )
#' p <- episode_ui_epi_curve_chart(curve)
#' @export
episode_ui_epi_curve_chart <- function(curve, lang = "nl") {
  pal <- episode_palette()
  curve$alpha <- ifelse(curve$incomplete, 0.45, 1)
  marks <- if (identical(lang, "en")) list(big = ",", decimal = ".") else list(big = ".", decimal = ",")
  ggplot2::ggplot(curve, ggplot2::aes(x = .data$sample_date, y = .data$n_cases)) +
    ggplot2::geom_col(ggplot2::aes(alpha = .data$alpha), fill = pal$primary, width = 0.7, show.legend = FALSE) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = marks$big, decimal.mark = marks$decimal, scientific = FALSE)) +
    ggplot2::labs(y = episode_tr("panel.epicurve.ylab", lang = lang)) +
    episode_chart_theme()
}

#' @rdname app_charts
#' @param trend A data frame from `episode_app_trend()`.
#' @examples
#' trend <- data.frame(
#'   week_start = seq(as.Date("2025-01-06"), by = "week", length.out = 8),
#'   n_cases = c(2, 3, 1, 4, 6, 5, 3, 2),
#'   expected = c(2, 2, 2, 2, 2, 2, 2, 2),
#'   upperbound = c(4, 4, 4, 4, 4, 4, 4, 4)
#' )
#' p <- episode_ui_trend_chart(trend)
#' @export
episode_ui_trend_chart <- function(trend, lang = "nl") {
  pal <- episode_palette()
  trend$week_start <- as.Date(trend$week_start)
  legend_labels <- c(
    obs = episode_tr("panel.trend.legend_observed", lang = lang),
    exp = episode_tr("panel.trend.legend_expected", lang = lang)
  )
  ggplot2::ggplot(trend, ggplot2::aes(x = .data$week_start)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = .data$upperbound), fill = pal$primary_tint, alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = .data$expected, colour = "exp"), linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$n_cases, colour = "obs"), linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = c(obs = pal$ink, exp = pal$primary_light), labels = legend_labels) +
    episode_chart_theme()
}

#' @rdname app_charts
#' @param rt A data frame from `episode_compute_rt()`.
#' @examples
#' rt <- data.frame(
#'   window_end = seq(as.Date("2025-01-08"), by = "day", length.out = 5),
#'   mean = c(1.4, 1.3, 1.1, 0.9, 0.8),
#'   lower = c(1.0, 0.9, 0.8, 0.6, 0.5),
#'   upper = c(1.8, 1.7, 1.4, 1.2, 1.1)
#' )
#' p <- episode_ui_rt_chart(rt)
#' @export
episode_ui_rt_chart <- function(rt, lang = "nl") {
  pal <- episode_palette()
  ggplot2::ggplot(rt, ggplot2::aes(x = .data$window_end)) +
    ggplot2::geom_hline(yintercept = 1, colour = pal$faint, linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), fill = pal$primary_tint, alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean), colour = pal$primary, linewidth = 0.9) +
    ggplot2::labs(y = "Rt") +
    episode_chart_theme()
}

#' A PC choropleth, guarded for absence of geographic data
#'
#' ARCHITECTURE.md section 9: "Geography. PC4 choropleth... A second
#' panel breaks the cluster down by institution... which is usually more
#' informative" - the bar breakdown (`episode_ui_bars()`) is that second
#' panel and stays regardless; this is the map itself, additive when both
#' `sf` and a geographic dataset (`R/geo_data.R`) are actually available.
#' Deliberately not built on `certegis` (see `R/geo_data.R`'s own docs and
#' QUESTIONS.md): geography here is operator-suppliable, not
#' Netherlands-only, matching how every other domain concept in this
#' codebase already works. An optional second layer
#' (`episode_geo_overlay_resolve()`, `EPISODE_GEO_DATA_OVERLAY`) draws
#' region outlines - no fill, a thicker line - on top for orientation;
#' its own absence or failure never affects the choropleth itself.
#'
#' @param rows A data frame from `episode_app_concentration()$rows`
#'   (`label` = a PC value, `n` = case count).
#' @return A `ggplot` object, or `NULL` if no geographic data is
#'   available at all, or the join/plot fails for any reason (e.g. a PC
#'   value not in the reference geometry - synthetic demo PC4s are drawn
#'   from a fixed pool that does not promise to match real postcode
#'   boundaries).
#' @keywords internal
#' @noRd
episode_ui_geo_map_chart <- function(rows) {
  if (nrow(rows) == 0) return(NULL)
  pal <- episode_palette()
  tryCatch({
    geo <- episode_geo_join(rows)
    if (is.null(geo)) return(NULL)
    p <- ggplot2::ggplot(geo) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$n), colour = pal$border, linewidth = 0.1) +
      ggplot2::scale_fill_gradient(low = pal$primary_tint, high = pal$primary, na.value = pal$bg_subtle)

    # EPISODE_GEO_DATA_OVERLAY: an optional second layer for orientation
    # (province/municipality outlines, ...) - colour, no fill, a
    # thicker line so it reads as a boundary on top of the choropleth
    # rather than competing with it. Its own failure (bad geometry, CRS
    # mismatch) must not take down the choropleth that already rendered
    # fine without it.
    overlay <- tryCatch(episode_geo_overlay_resolve(), error = function(e) NULL)
    if (!is.null(overlay)) {
      p <- tryCatch(
        p + ggplot2::geom_sf(data = overlay, fill = NA, colour = pal$ink, linewidth = 0.6),
        error = function(e) p
      )
    }

    p +
      ggplot2::coord_sf(datum = NA) +
      episode_chart_theme() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank())
  }, error = function(e) NULL)
}

#' @rdname app_charts
#' @param series A data frame from `episode_app_denominator_series()`.
#' @keywords internal
#' @noRd
episode_ui_denominator_chart <- function(series, lang = "nl") {
  pal <- episode_palette()
  max_tests <- max(series$n_tests, 1)
  scale_factor <- max_tests
  series$positivity_scaled <- series$positivity * scale_factor

  ggplot2::ggplot(series, ggplot2::aes(x = .data$week_start)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_tests), fill = pal$secondary, width = 4) +
    ggplot2::geom_line(ggplot2::aes(y = .data$positivity_scaled), colour = pal$danger, linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = .data$positivity_scaled), colour = pal$danger, size = 1.6) +
    ggplot2::scale_y_continuous(
      name = episode_tr("panel.denominator.legend_tests", lang = lang),
      sec.axis = ggplot2::sec_axis(~ . / scale_factor * 100, name = episode_tr("panel.denominator.legend_positivity", lang = lang))
    ) +
    episode_chart_theme() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 9, colour = pal$muted),
                   axis.title.y.right = ggplot2::element_text(size = 9, colour = pal$danger_dark))
}
