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

#' Draw the dashboard charts (epidemic curve, trend, and Rt)
#'
#' These functions build the three time-series charts used throughout the
#' EpiSODIC dashboard and outbreak reports: an epidemic curve, a trend chart
#' against the expected baseline, and an effective reproduction number
#' (\eqn{R_t}) chart. Each takes the small, already-summarised data frame the
#' app itself works with, so they are also useful for reproducing a dossier's
#' chart in your own report or presentation. All three return a static
#' [ggplot2::ggplot] object that can be printed, saved with
#' [ggplot2::ggsave()], or further customised with additional `ggplot2`
#' layers.
#'
#' @name episodic_charts
#' @importFrom rlang .data
NULL

#' Text sizes shared by every chart
#'
#' One place, so an axis label, an axis title and a legend key cannot end
#' up at three different sizes across the app.
#' @keywords internal
#' @noRd
episodic_chart_text_size <- c(axis = 11, title = 11, legend = 11)

#' The shared chart theme
#'
#' Axis labels are set in the muted grey the rest of the interface uses
#' for secondary text, not in `faint`. `faint` is a hairline colour meant
#' for rules and separators; at 9pt it left the axis barely legible
#' against white, which on a surveillance chart is not a cosmetic
#' problem - the axis is how you name the week a rise started, and an
#' axis you have to lean in to read is one you stop reading.
#' @keywords internal
#' @noRd
episodic_chart_theme <- function() {
  pal <- episodic_palette()
  sizes <- episodic_chart_text_size
  ggplot2::theme_minimal(base_family = "") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = pal$border,
        linewidth = 0.3
      ),
      axis.text = ggplot2::element_text(
        colour = pal$muted,
        size = sizes[["axis"]]
      ),
      axis.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        size = sizes[["legend"]],
        colour = pal$ink
      ),
      legend.key.width = ggplot2::unit(22, "pt")
    )
}

#' Translated month abbreviations, in month order
#'
#' The same `date.month.NN` keys `episodic_format_date_range()` reads, so
#' an axis and a date range in the same dossier never spell a month two
#' different ways.
#'
#' @param lang Session language.
#' @return A character vector of length 12.
#' @keywords internal
#' @noRd
episodic_chart_month_abbrevs <- function(
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  vapply(
    sprintf("%02d", 1:12),
    function(mm) episodic_tr(paste0("date.month.", mm), lang = lang),
    character(1),
    USE.NAMES = FALSE
  )
}

#' Breaks and labels for a weekly x axis
#'
#' `ggplot2`'s default date scale picks round calendar breaks, which on a
#' surveillance chart of a dozen-odd weeks means two labels - "Oct" and
#' "Jan" - for the whole period. That is not enough to locate a peak: an
#' epidemiologist reading a weekly curve wants to name the week the rise
#' started, and cannot do it from a month tick a quarter of a chart away.
#'
#' So the axis is built from the weeks themselves. Every label carries the
#' ISO week number, which is how surveillance weeks are referred to
#' everywhere else in the field, over the month it falls in; the year is
#' added on the first label and again whenever it changes, rather than
#' repeated on every tick. Beyond about eighteen months the week number
#' stops being the useful unit and the labels become month and year.
#'
#' @param week_starts The `Date` of each week on the chart.
#' @param lang Session language, for month names.
#' @param max_labels Roughly how many ticks to draw; the step between
#'   weeks is chosen to stay under this.
#' @return A list with `breaks` (`Date`) and `labels` (character), or
#'   `NULL` when there is nothing to label.
#' @keywords internal
#' @noRd
episodic_chart_week_axis <- function(
  week_starts,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  max_labels = 14L
) {
  week_starts <- sort(unique(as.Date(week_starts)))
  week_starts <- week_starts[!is.na(week_starts)]
  n <- length(week_starts)
  if (n == 0) {
    return(NULL)
  }

  step <- max(1L, as.integer(ceiling(n / max_labels)))
  breaks <- week_starts[seq(1L, n, by = step)]

  months <- episodic_chart_month_abbrevs(lang)
  # ISO week, not "week of the year": week 1 is the week containing the
  # first Thursday, which is the convention every surveillance season in
  # this codebase is already built on.
  iso_week <- as.integer(format(breaks, "%V"))
  # Month and year come from that same Thursday rather than from the
  # week's Monday. A week can straddle a year end - the week beginning
  # 30 December 2024 is ISO week 1 of 2025 - and taking the Monday's
  # calendar date would label it "w01 / Dec 2024", which contradicts
  # itself and never announces the new year at all. The Thursday is what
  # decides which year an ISO week belongs to, so it decides the label.
  thursday <- breaks + 3
  mon <- months[as.integer(format(thursday, "%m"))]
  years <- format(thursday, "%Y")

  span_days <- as.numeric(max(week_starts) - min(week_starts))
  if (span_days > 550) {
    return(list(breaks = breaks, labels = sprintf("%s\n%s", mon, years)))
  }

  new_year <- c(TRUE, years[-1] != years[-length(years)])
  bottom <- ifelse(new_year, paste(mon, years), mon)
  list(breaks = breaks, labels = sprintf("w%02d\n%s", iso_week, bottom))
}

#' Apply `episodic_chart_week_axis()` to a plot, if it has anything to say
#'
#' @param week_starts The `Date` of each week on the chart.
#' @param lang Session language.
#' @return A `scale_x_date` layer, or `NULL` (which `+` ignores).
#' @keywords internal
#' @noRd
episodic_chart_week_scale <- function(
  week_starts,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  axis <- episodic_chart_week_axis(week_starts, lang = lang)
  if (is.null(axis)) {
    return(NULL)
  }
  ggplot2::scale_x_date(breaks = axis$breaks, labels = axis$labels)
}

#' @rdname episodic_charts
#' @param curve A data frame with one row per day: `sample_date` (`Date`),
#'   `n_cases` (case count), and `incomplete` (logical, `TRUE` for the most
#'   recent day(s) where reporting is still catching up - these are drawn at
#'   reduced opacity as a visual reminder not to over-interpret a downturn
#'   that is really just a reporting lag).
#' @param lang Language for axis labels: `"nl"`, `"en"`, `"es"`, `"fr"`,
#'   `"de"`, `"zh"`, `"hi"`, or `"ar"`. Defaults to the
#'   `EPISODIC_LANGUAGE` environment variable, falling back to `"en"` if
#'   that is unset.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' curve <- data.frame(
#'   sample_date = seq(as.Date("2025-01-01"), by = "day", length.out = 14),
#'   n_cases = c(1, 0, 2, 1, 3, 2, 4, 3, 5, 2, 1, 0, 1, 2),
#'   incomplete = c(rep(FALSE, 12), TRUE, TRUE)
#' )
#' episodic_ui_epi_curve_chart(curve, lang = "en")
#' @export
episodic_ui_epi_curve_chart <- function(
  curve,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  curve$alpha <- ifelse(curve$incomplete, 0.45, 1)
  marks <- if (identical(episodic_lang(lang), "en")) {
    list(big = ",", decimal = ".")
  } else {
    list(big = ".", decimal = ",")
  }
  ggplot2::ggplot(
    curve,
    ggplot2::aes(x = .data$sample_date, y = .data$n_cases)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(alpha = .data$alpha),
      fill = pal$primary,
      width = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(labels = function(x) {
      format(
        x,
        big.mark = marks$big,
        decimal.mark = marks$decimal,
        scientific = FALSE
      )
    }) +
    ggplot2::labs(y = episodic_tr("panel.epicurve.ylab", lang = lang)) +
    episodic_chart_theme()
}

#' @rdname episodic_charts
#' @param trend A data frame with one row per week: `week_start` (`Date`),
#'   `n_cases` (observed count), `expected` (the Farrington baseline), and
#'   `upperbound` (the alarm threshold, shown as a shaded band).
#' @examples
#' trend <- data.frame(
#'   week_start = seq(as.Date("2025-01-06"), by = "week", length.out = 8),
#'   n_cases = c(2, 3, 1, 4, 6, 5, 3, 2),
#'   expected = c(2, 2, 2, 2, 2, 2, 2, 2),
#'   upperbound = c(4, 4, 4, 4, 4, 4, 4, 4)
#' )
#' episodic_ui_trend_chart(trend, lang = "en")
#' @export
episodic_ui_trend_chart <- function(
  trend,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  trend$week_start <- as.Date(trend$week_start)
  legend_labels <- c(
    obs = episodic_tr("panel.trend.legend_observed", lang = lang),
    exp = episodic_tr("panel.trend.legend_expected", lang = lang)
  )
  ggplot2::ggplot(trend, ggplot2::aes(x = .data$week_start)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = 0, ymax = .data$upperbound),
      fill = pal$primary_tint,
      alpha = 0.35
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$expected, colour = "exp"),
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$n_cases, colour = "obs"),
      linewidth = 0.9
    ) +
    ggplot2::scale_colour_manual(
      values = c(obs = pal$ink, exp = pal$primary_light),
      labels = legend_labels
    ) +
    episodic_chart_week_scale(trend$week_start, lang = lang) +
    episodic_chart_theme()
}

#' @rdname episodic_charts
#' @param rt A data frame with one row per estimation window: `window_end`
#'   (`Date`), `mean` (point estimate of \eqn{R_t}), and `lower`/`upper`
#'   (95% credible interval). A dashed reference line is drawn at
#'   \eqn{R_t = 1}, the threshold between a shrinking and a growing outbreak.
#' @examples
#' rt <- data.frame(
#'   window_end = seq(as.Date("2025-01-08"), by = "day", length.out = 5),
#'   mean = c(1.4, 1.3, 1.1, 0.9, 0.8),
#'   lower = c(1.0, 0.9, 0.8, 0.6, 0.5),
#'   upper = c(1.8, 1.7, 1.4, 1.2, 1.1)
#' )
#' episodic_ui_rt_chart(rt)
#' @export
episodic_ui_rt_chart <- function(rt, lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  pal <- episodic_palette()
  ggplot2::ggplot(rt, ggplot2::aes(x = .data$window_end)) +
    ggplot2::geom_hline(
      yintercept = 1,
      colour = pal$faint,
      linewidth = 0.4,
      linetype = "dashed"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      fill = pal$primary_tint,
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$mean),
      colour = pal$primary,
      linewidth = 0.9
    ) +
    ggplot2::labs(y = "Rt") +
    episodic_chart_theme()
}

#' A PC choropleth, cropped to where the cases actually are
#'
#' Geography is shown two ways: a bar breakdown by PC value
#' (`episodic_ui_bars()`), which is often the more informative view since
#' it breaks the cluster down by institution, and this map, additive on
#' top when both `sf` and a geographic dataset (`R/geo_data.R`) are
#' actually available. Deliberately not built on any single country's
#' own mapping package: geography here is operator-suppliable, not tied
#' to one jurisdiction, matching how every other domain concept in this
#' codebase already works. An optional second layer
#' (`episodic_geo_overlay_resolve()`, `EPISODIC_GEO_DATA_OVERLAY`) draws
#' region outlines - no fill, a thicker line - on top for orientation;
#' its own absence or failure never affects the choropleth itself.
#'
#' `episodic_geo_join()` keeps every reference polygon, not only the ones
#' with cases, which is right - a cluster has to be read against the
#' areas around it, not floating in white space - but drawing all of them
#' meant the map was always framed on the entire reference dataset. With
#' the shipped Netherlands default that is some four thousand PC4
#' polygons: a five-postcode cluster rendered as a handful of tinted
#' specks somewhere inside a whole country, at which scale the one
#' question the panel exists to answer - *which* postcodes, and are they
#' adjacent - cannot be answered at all. So the frame is cropped to the
#' case-bearing areas plus a margin of context around them, and each of
#' those areas is labelled with its own PC value and case count. What was
#' a shape you could only squint at becomes something you can read.
#'
#' The margin is proportional (a share of the cluster's own extent, with
#' a floor tied to the reference dataset's extent) rather than a fixed
#' distance, since the reference data may be in degrees or in metres and
#' EpiSODIC does not get to assume which.
#'
#' @param rows A data frame from `episodic_app_concentration()$rows`
#'   (`label` = a PC value, `n` = case count).
#' @param pad_share Margin around the case-bearing extent, as a share of
#'   that extent's longer side.
#' @param min_pad_share Floor for that margin, as a share of the whole
#'   reference dataset's longer side - so a cluster confined to a single
#'   postcode still gets a legible amount of surrounding context rather
#'   than a frame drawn tight around one polygon.
#' @param max_labels Cap on how many areas are labelled, highest case
#'   count first. A diffuse cluster spread over dozens of postcodes is
#'   past the point where labelling every one of them helps.
#' @param crop When `TRUE` (the default), the map is cropped tight
#'   around the case-bearing areas plus a margin - the detail view. When
#'   `FALSE`, the full reference extent is drawn and unmatched areas are
#'   left unfilled, for a companion "where in the region is this"
#'   context map; area labels are skipped since a full-region view has
#'   too many polygons to label legibly.
#' @return A `ggplot` object, or `NULL` if no geographic data is
#'   available at all, or the join/plot fails for any reason (e.g. a PC
#'   value not in the reference geometry - synthetic demo postcodes are
#'   drawn from a fixed pool that does not promise to match real
#'   postcode boundaries).
#' @keywords internal
#' @noRd
episodic_ui_geo_map_chart <- function(
  rows,
  pad_share = 0.45,
  min_pad_share = 0.02,
  max_labels = 30L,
  crop = TRUE
) {
  if (nrow(rows) == 0) {
    return(NULL)
  }
  pal <- episodic_palette()
  tryCatch(
    {
      geo <- episodic_geo_join(rows)
      if (is.null(geo)) {
        return(NULL)
      }
      matched <- geo[!is.na(geo$n), , drop = FALSE]
      # No PC in the cluster resolves to a polygon: a map of the whole
      # reference set with nothing on it says less than the bar breakdown
      # the caller falls back to.
      if (nrow(matched) == 0) {
        return(NULL)
      }

      if (crop) {
        frame <- episodic_geo_frame(
          geo,
          matched,
          pad_share = pad_share,
          min_pad_share = min_pad_share
        )
        # Crop the geometry as well as the frame: drawing four thousand
        # polygons and then hiding all but a dozen of them is work the plot
        # device does not need to do.
        plotted <- tryCatch(
          suppressWarnings(sf::st_crop(geo, frame$bbox)),
          error = function(e) geo
        )
        if (is.null(plotted) || nrow(plotted) == 0) {
          plotted <- geo
        }
      } else {
        full <- sf::st_bbox(geo)
        frame <- list(
          xlim = c(full[["xmin"]], full[["xmax"]]),
          ylim = c(full[["ymin"]], full[["ymax"]])
        )
        plotted <- geo
      }

      p <- ggplot2::ggplot(plotted) +
        ggplot2::geom_sf(
          ggplot2::aes(fill = .data$n),
          colour = pal$border,
          linewidth = 0.1
        ) +
        ggplot2::scale_fill_gradient(
          low = pal$primary_tint,
          high = pal$primary,
          na.value = pal$bg_subtle
        )

      # EPISODIC_GEO_DATA_OVERLAY: an optional second layer for orientation
      # (province/municipality outlines, ...) - colour, no fill, a
      # thicker line so it reads as a boundary on top of the choropleth
      # rather than competing with it. Its own failure (bad geometry, CRS
      # mismatch) must not take down the choropleth that already rendered
      # fine without it.
      overlay <- tryCatch(episodic_geo_overlay_resolve(), error = function(e) {
        NULL
      })
      if (!is.null(overlay)) {
        p <- tryCatch(
          p +
            ggplot2::geom_sf(
              data = overlay,
              fill = NA,
              colour = pal$ink,
              linewidth = 0.6
            ),
          error = function(e) p
        )
      }

      labels <- if (crop) episodic_geo_labels(matched, max_labels = max_labels)
      if (!is.null(labels)) {
        # Plain geom_text over pre-computed representative points rather
        # than geom_sf_text(): stat_sf_coordinates() emits a warning per
        # render on geographic coordinates, and a warning raised at print
        # time inside shiny::renderPlot() cannot be caught by the caller.
        p <- p +
          ggplot2::geom_text(
            data = labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            inherit.aes = FALSE,
            size = 2.9,
            lineheight = 0.95,
            colour = pal$ink
          )
      }

      p <- p +
        ggplot2::coord_sf(
          xlim = frame$xlim,
          ylim = frame$ylim,
          datum = NA,
          expand = FALSE
        ) +
        # No fill legend: every labelled area already carries its own
        # count (episodic_geo_labels()), and episodic_chart_theme()'s
        # legend.position = "bottom" would otherwise eat into the box
        # episodic_ui_map_render_height() sized for the map's geographic
        # aspect ratio alone - the map then renders shorter than that box
        # and floats in the middle of it, padded with blank space top and
        # bottom exactly where the legend's own row was budgeted for.
        ggplot2::guides(fill = "none") +
        episodic_chart_theme() +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "none",
          plot.margin = ggplot2::margin(0, 0, 0, 0)
        )
      # So the caller can size its render device to the map's own shape
      # instead of a fixed box: coord_sf() draws geographic space true to
      # scale (a degree of longitude is narrower than a degree of
      # latitude away from the equator), so the frame's raw xlim/ylim
      # span ratio is not the on-screen aspect ratio unless corrected for
      # that. Fed into a device box of a different ratio, the true-scale
      # constraint is still honoured - by shrinking the map to fit and
      # padding the rest with blank canvas, which is the whitespace this
      # exists to avoid.
      attr(p, "episodic_aspect_ratio") <- episodic_geo_aspect_ratio(
        geo,
        frame
      )
      p
    },
    error = function(e) NULL
  )
}

#' The on-screen width:height ratio a map's frame renders at
#'
#' @param geo The `sf` object the frame's CRS is read from.
#' @param frame A list with `xlim`, `ylim`, as from `episodic_geo_frame()`.
#' @return A finite positive number, or `1` if it cannot be determined.
#' @keywords internal
#' @noRd
episodic_geo_aspect_ratio <- function(geo, frame) {
  dx <- diff(frame$xlim)
  dy <- diff(frame$ylim)
  if (!is.finite(dx) || !is.finite(dy) || dx <= 0 || dy <= 0) {
    return(1)
  }
  longlat <- tryCatch(isTRUE(sf::st_is_longlat(geo)), error = function(e) FALSE)
  correction <- if (isTRUE(longlat)) {
    cos(mean(frame$ylim) * pi / 180)
  } else {
    1
  }
  ratio <- (dx * correction) / dy
  if (!is.finite(ratio) || ratio <= 0) 1 else ratio
}

#' The render height (in px) that keeps a map's own shape at a given width
#'
#' Paired with a CSS `max-width: <width>px` on the element the plot
#' renders into, so the box `shiny::renderPlot()` fills actually has this
#' aspect ratio rather than the panel's own (typically much wider) one -
#' see `episodic_geo_aspect_ratio()`.
#'
#' @param map_chart A `ggplot` from `episodic_ui_geo_map_chart()`, or
#'   `NULL`.
#' @param width The width (px) the map is capped to.
#' @param default Height (px) to fall back to when the aspect ratio is
#'   unavailable (e.g. `map_chart` is `NULL`).
#' @param min_height,max_height Bounds so an unusually shaped frame (a
#'   single sliver-thin cluster, say) does not collapse the panel to a
#'   few pixels or blow it out past the screen.
#' @return An integer height in px.
#' @keywords internal
#' @noRd
episodic_ui_map_render_height <- function(
  map_chart,
  width = 640,
  default = 430,
  min_height = 260,
  max_height = 600
) {
  ratio <- attr(map_chart, "episodic_aspect_ratio")
  height <- if (is.null(ratio) || !is.finite(ratio) || ratio <= 0) {
    default
  } else {
    width / ratio
  }
  as.integer(round(min(max(height, min_height), max_height)))
}

#' The cropped frame around a choropleth's case-bearing areas
#'
#' @param geo The full joined reference geometry.
#' @param matched The subset carrying case counts.
#' @param pad_share,min_pad_share See `episodic_ui_geo_map_chart()`.
#' @return A list with `xlim`, `ylim` and a `bbox` suitable for
#'   [sf::st_crop()].
#' @keywords internal
#' @noRd
episodic_geo_frame <- function(
  geo,
  matched,
  pad_share = 0.45,
  min_pad_share = 0.02
) {
  bb <- sf::st_bbox(matched)
  full <- sf::st_bbox(geo)

  span <- max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
  full_span <- max(
    full[["xmax"]] - full[["xmin"]],
    full[["ymax"]] - full[["ymin"]]
  )
  pad <- max(span * pad_share, full_span * min_pad_share)

  xlim <- c(bb[["xmin"]] - pad, bb[["xmax"]] + pad)
  ylim <- c(bb[["ymin"]] - pad, bb[["ymax"]] + pad)

  bbox <- sf::st_bbox(
    c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]),
    crs = sf::st_crs(geo)
  )
  list(xlim = xlim, ylim = ylim, bbox = bbox)
}

#' Readable "PC / count" labels for the case-bearing areas
#'
#' @param matched The joined geometry rows carrying case counts.
#' @param max_labels Cap, highest case count first.
#' @return A data frame with `x`, `y`, `label`, or `NULL` if
#'   representative points cannot be derived.
#' @keywords internal
#' @noRd
episodic_geo_labels <- function(matched, max_labels = 30L) {
  matched <- matched[order(-matched$n), , drop = FALSE]
  if (nrow(matched) > max_labels) {
    matched <- matched[seq_len(max_labels), , drop = FALSE]
  }

  # st_point_on_surface() rather than st_centroid(): the centroid of a
  # concave or multi-part area can land outside it, putting a postcode
  # label on top of a neighbour it does not belong to.
  points <- tryCatch(
    suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(
      matched
    )))),
    error = function(e) NULL
  )
  if (is.null(points) || nrow(points) < nrow(matched)) {
    return(NULL)
  }

  data.frame(
    x = points[seq_len(nrow(matched)), 1],
    y = points[seq_len(nrow(matched)), 2],
    label = sprintf("%s\n%d", as.character(matched$pc), as.integer(matched$n)),
    stringsAsFactors = FALSE
  )
}

#' Chart of tests performed and positivity rate, by week
#' @param series A data frame with `week_start`, `n_tests`, `positivity`.
#' @keywords internal
#' @noRd
episodic_ui_denominator_chart <- function(
  series,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  max_tests <- max(series$n_tests, 1)
  scale_factor <- max_tests
  # A positivity rate above 100% is not a real reading - it means the
  # supplied testing-volume feed does not cover every case counted (a
  # partial-coverage panel, say), not that more than every test came back
  # positive. Clamped here rather than upstream so the underlying numbers
  # stay available to anyone reading `series` directly; the chart is the
  # one place a rate has to fit on a percentage axis.
  series$positivity_scaled <- pmin(series$positivity, 1) * scale_factor

  ggplot2::ggplot(series, ggplot2::aes(x = .data$week_start)) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data$n_tests),
      fill = pal$secondary,
      width = 4
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$positivity_scaled),
      colour = pal$danger,
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$positivity_scaled),
      colour = pal$danger,
      size = 1.6
    ) +
    ggplot2::scale_y_continuous(
      name = episodic_tr("panel.denominator.legend_tests", lang = lang),
      limits = c(0, scale_factor),
      sec.axis = ggplot2::sec_axis(
        ~ . / scale_factor * 100,
        name = episodic_tr("panel.denominator.legend_positivity", lang = lang),
        breaks = seq(0, 100, by = 25)
      )
    ) +
    episodic_chart_week_scale(series$week_start, lang = lang) +
    episodic_chart_theme() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = episodic_chart_text_size[["title"]],
        colour = pal$muted
      ),
      axis.title.y.right = ggplot2::element_text(
        size = episodic_chart_text_size[["title"]],
        colour = pal$danger_dark
      )
    )
}

#' The pathogen-level weekly curve, with MEM thresholds drawn on it
#'
#' The thresholds are the point of the chart. MEM has been fitting a
#' pre-epidemic and a post-epidemic threshold for every seasonal pathogen
#' on every detection run all along, and using them only to decide
#' whether a detector fired; drawn against the season's own weekly curve
#' they answer the question an epidemiologist actually opens a seasonal
#' chart to ask - has the epidemic started, is it still running, and how
#' hard is this season compared with the ones it was fitted on.
#'
#' Weeks still filling up are drawn at reduced opacity, exactly as the
#' cluster epi curve draws its incomplete days, so the tail is never read
#' as a downturn.
#'
#' @param weekly A data frame with `week_start` (`Date`), `n_cases` and
#'   `incomplete`.
#' @param thresholds `episodic_mem_thresholds_for_season()`'s output, or
#'   `NULL` to draw the bars alone.
#' @param lang Language for labels.
#' @return A [ggplot2::ggplot] object.
#' @keywords internal
#' @noRd
episodic_ui_pathogen_curve_chart <- function(
  weekly,
  thresholds = NULL,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  weekly$week_start <- as.Date(weekly$week_start)
  weekly$alpha <- ifelse(weekly$incomplete %in% TRUE, 0.45, 1)

  p <- ggplot2::ggplot(
    weekly,
    ggplot2::aes(x = .data$week_start, y = .data$n_cases)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(alpha = .data$alpha),
      fill = pal$primary,
      width = 5.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_alpha_identity() +
    episodic_chart_week_scale(weekly$week_start, lang = lang) +
    ggplot2::labs(y = episodic_tr("panel.epicurve.ylab", lang = lang))

  lines <- episodic_mem_threshold_lines(thresholds, lang = lang)
  if (!is.null(lines)) {
    p <- p +
      ggplot2::geom_hline(
        data = lines,
        ggplot2::aes(yintercept = .data$value, colour = .data$key),
        linewidth = 0.7,
        linetype = "dashed"
      ) +
      ggplot2::scale_colour_manual(
        values = stats::setNames(lines$colour, lines$key),
        labels = stats::setNames(lines$label, lines$key),
        breaks = lines$key
      )
  }
  p + episodic_chart_theme()
}

#' The MEM threshold lines to draw, in ascending order
#'
#' @param thresholds `episodic_mem_thresholds_for_season()`'s output, or
#'   `NULL`.
#' @param lang Language for the line labels.
#' @return A data frame with `key`, `value`, `label`, `colour`, or `NULL`.
#' @keywords internal
#' @noRd
episodic_mem_threshold_lines <- function(
  thresholds,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  if (is.null(thresholds)) {
    return(NULL)
  }
  pal <- episodic_palette()

  keys <- c("pre_epidemic", "post_epidemic")
  values <- c(thresholds$pre_epidemic, thresholds$post_epidemic)
  colours <- c(pal$tertiary_dark, pal$secondary)

  if (!is.null(thresholds$intensity)) {
    keys <- c(keys, "medium", "high", "very_high")
    values <- c(values, as.numeric(thresholds$intensity))
    colours <- c(colours, pal$warning_dark, pal$danger, pal$danger_dark)
  }

  keep <- is.finite(values)
  if (!any(keep)) {
    return(NULL)
  }
  out <- data.frame(
    key = keys[keep],
    value = values[keep],
    colour = colours[keep],
    label = vapply(
      keys[keep],
      function(k) episodic_tr(paste0("pathogen.threshold.", k), lang = lang),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  out[order(out$value), ]
}

#' Season-over-season (or year-over-year) overlay
#'
#' Every period's weekly curve on one axis of week-within-period, the
#' selected one drawn heavier than the rest. This is the chart that
#' answers "early or late, bigger or smaller" directly, instead of
#' leaving it to be inferred by flipping between two separate charts.
#'
#' @param overlay `episodic_app_pathogen_overlay()`'s output.
#' @param lang Language for labels.
#' @return A [ggplot2::ggplot] object.
#' @keywords internal
#' @noRd
episodic_ui_pathogen_overlay_chart <- function(
  overlay,
  lang = Sys.getenv("EPISODIC_LANGUAGE")
) {
  pal <- episodic_palette()
  rows <- overlay$rows
  groups <- sort(unique(rows$group))
  rows$group <- factor(rows$group, levels = groups)

  earlier <- setdiff(groups, overlay$current)
  pool <- c(
    pal$primary_light,
    pal$tertiary,
    pal$warning,
    pal$faint,
    pal$secondary_dark
  )
  colours <- stats::setNames(rep(pool, length.out = length(groups)), groups)
  if (length(earlier) > 0) {
    colours[earlier] <- rep(pool, length.out = length(earlier))
  }
  colours[overlay$current] <- pal$danger_dark

  # Week labels run 40..52 then 1..20 for a season, so the x axis is an
  # index and the labels are looked up from it - a numeric week number
  # would put week 1 to the left of week 40.
  n_weeks <- max(rows$week_index)
  breaks <- seq(1L, n_weeks, by = 4L)
  labels <- rows$week_label[match(breaks, rows$week_index)]

  # Crop to the same weeks the weekly incidence panel above this one is
  # showing (see episodic_app_overlay_period_range()), rather than always
  # the full season/year, so the two top Pathogen-screen charts share an
  # x axis whatever period is selected.
  period_range <- overlay$period_range

  p <- ggplot2::ggplot(
    rows,
    ggplot2::aes(
      x = .data$week_index,
      y = .data$n_cases,
      group = .data$group,
      colour = .data$group
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE)

  current_rows <- rows[
    as.character(rows$group) == overlay$current, ,
    drop = FALSE
  ]
  if (nrow(current_rows) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = current_rows,
        linewidth = 1.4,
        show.legend = FALSE,
        na.rm = TRUE
      )
  }

  p <- p +
    ggplot2::scale_colour_manual(values = colours[as.character(groups)]) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
  if (!is.null(period_range)) {
    p <- p +
      ggplot2::coord_cartesian(xlim = period_range)
  }
  p +
    ggplot2::labs(
      y = episodic_tr("panel.epicurve.ylab", lang = lang),
      x = episodic_tr(
        paste0("pathogen.panel.overlay.xlab.", overlay$kind),
        lang = lang
      )
    ) +
    episodic_chart_theme() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        size = episodic_chart_text_size[["title"]],
        colour = pal$muted
      )
    )
}
