#' Chart panels
#'
#' `ggplot2`-based equivalents of `episode-mockup.jsx`'s Recharts
#' components (`DailyCurve`, `LongTrend`, `Denominator`). Static, not
#' interactive - keeping the app's dependency footprint to a single,
#' ubiquitous CRAN plotting package rather than an htmlwidgets stack is a
#' deliberate M2 simplification; documented in `QUESTIONS.md`.
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

#' A PC4 choropleth via `certegis`, guarded for absence
#'
#' ARCHITECTURE.md section 9: "Geography. PC4 choropleth via `certegis`...
#' A second panel breaks the cluster down by institution... which is
#' usually more informative" - the bar breakdown (`episode_ui_bars()`) is
#' that second panel and stays regardless; this is the map itself,
#' additive when `certegis` (Certe-internal-adjacent, CRAN-external,
#' `Suggests`-only) and the `sf` package it depends on are actually
#' installed. `certegis::add_map()` right-joins case counts onto its
#' bundled PC4 geometry (`data.frame(postcode = ..., n = ...) |>
#' add_map()`, its own documented usage) and returns an `sf` object;
#' plotted here with `ggplot2::geom_sf()` rather than a `certegis`-owned
#' plot function, since none exists - `certegis` only supplies the
#' geometry join, plotting is left to the caller.
#'
#' @param rows A data frame from `episode_app_concentration()$rows`
#'   (`label` = PC4, `n` = case count).
#' @return A `ggplot` object, or `NULL` if `certegis`/`sf` are not
#'   installed or the join/plot fails for any reason (e.g. a PC4 not in
#'   `certegis`'s bundled geometry - synthetic demo PC4s are drawn from a
#'   fixed pool that does not promise to match real postcode boundaries).
#' @keywords internal
#' @noRd
episode_ui_geo_map_chart <- function(rows) {
  if (!requireNamespace("certegis", quietly = TRUE) || nrow(rows) == 0) return(NULL)
  pal <- episode_palette()
  tryCatch({
    geo <- certegis::add_map(data.frame(postcode = as.numeric(rows$label), n = rows$n), crop_certe = FALSE)
    ggplot2::ggplot(geo) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$n), colour = pal$border, linewidth = 0.1) +
      ggplot2::scale_fill_gradient(low = pal$primary_tint, high = pal$primary, na.value = pal$bg_subtle) +
      episode_chart_theme() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank())
  }, error = function(e) NULL)
}

#' @rdname app_charts
#' @param series A data frame from `episode_app_denominator_series()`.
#' @export
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
