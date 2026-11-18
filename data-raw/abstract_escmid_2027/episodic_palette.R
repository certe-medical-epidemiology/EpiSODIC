# The figures take their colours from EpiSODIC's own shipped palette,
# read at render time rather than copied, so a figure cannot drift from
# the dashboard, the reports and the emails it illustrates.
#
# Series convention: EpiSODIC in the brand primary, every comparator in
# the palette's own neutrals, darkest for the strongest comparator. One
# saturated colour on a plot means one thing, and a reader who knows the
# dashboard recognises it.

episodic_style <- function(path = "../../inst/config/episodic_default_style.yaml") {
  path <- path.expand(path)
  if (!file.exists(path)) {
    stop("EpiSODIC's style file is not where it was expected: ", path, call. = FALSE)
  }
  style <- yaml::read_yaml(path)
  required <- c(
    "primary", "primary_dark", "primary_light", "primary_tint",
    "secondary", "ink", "muted", "faint", "border", "danger", "surface"
  )
  absent <- setdiff(required, names(style))
  if (length(absent)) {
    stop("EpiSODIC's style file names no ", paste(absent, collapse = ", "), call. = FALSE)
  }
  style
}

episodic_series_colours <- function(style, labels) {
  # EpiSODIC first, then the comparators from darkest to lightest.
  neutrals <- c(style$ink, style$muted, style$faint)
  if (length(labels) - 1 > length(neutrals)) {
    stop("more comparators than the palette has neutrals for", call. = FALSE)
  }
  stats::setNames(c(style$primary, neutrals[seq_len(length(labels) - 1)]), labels)
}

episodic_theme <- function(style, base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = style$muted),
      plot.title = ggplot2::element_text(size = base_size + 1, face = "bold", colour = style$ink),
      plot.subtitle = ggplot2::element_text(size = base_size - 1.5, colour = style$muted),
      plot.tag = ggplot2::element_text(size = base_size + 2, face = "bold", colour = style$ink),
      axis.title = ggplot2::element_text(size = base_size - 1.5, colour = style$muted),
      axis.text = ggplot2::element_text(size = base_size - 2, colour = style$muted),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = style$border, linewidth = 0.3),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = style$surface, colour = NA)
    )
}
