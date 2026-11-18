#!/usr/bin/env Rscript

# Figure 2 for the ESCMID Global 2027 abstract.
#
# Panel A: the share of all seeded outbreaks detected by day t, every
#          outbreak kept in the denominator so each curve plateaus at its
#          own sensitivity.
# Panel B: how the false-alarm burden grows with the number of pathogens
#          watched, against the rate the significance levels alone imply.
#
# Usage:
#   Rscript figure2_detection_burden.R [results_dir] [output_png]

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(script_dir, "episodic_palette.R"))
style <- episodic_style(file.path(script_dir, "..", "..", "inst", "config", "episodic_default_style.yaml"))

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) path.expand(args[[1]]) else file.path(script_dir, "..", "validation", "results")
out_png <- if (length(args) >= 2) path.expand(args[[2]]) else file.path(script_dir, "figure2_detection_burden_0.25.0.png")

read_required <- function(file) {
  path <- file.path(results_dir, file)
  if (!file.exists(path)) stop("missing results file: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

outbreaks_df <- read_required("outbreaks.csv")
burden_df <- read_required("alarm_burden.csv")

strategies <- data.frame(
  scenario = c("main", "comparator_shewhart", "comparator_same_place_14", "comparator_same_place_7"),
  label = c("EpiSODIC", "Shewhart", "Same-place, 3 in 14 days", "Same-place, 3 in 7 days"),
  stringsAsFactors = FALSE
)
strategies$label <- factor(strategies$label, levels = strategies$label)

missing <- setdiff(strategies$scenario, outbreaks_df$scenario)
if (length(missing)) stop("results hold no scenario called: ", paste(missing, collapse = ", "), call. = FALSE)

outbreaks <- outbreaks_df[outbreaks_df$scenario %in% strategies$scenario, ]
outbreaks$strategy <- strategies$label[match(outbreaks$scenario, strategies$scenario)]
outbreaks$found <- as.logical(outbreaks$detected)

x_max <- max(outbreaks$delay_from_first[outbreaks$found %in% TRUE], na.rm = TRUE)
days <- seq(0, x_max)

cumulative <- do.call(rbind, lapply(split(outbreaks, outbreaks$strategy), function(d) {
  if (nrow(d) == 0) stop("no outbreaks for a strategy", call. = FALSE)
  hit <- d$delay_from_first[d$found %in% TRUE]
  data.frame(
    label = d$strategy[[1]],
    time = days,
    detected = vapply(days, function(t) sum(hit <= t, na.rm = TRUE) / nrow(d), numeric(1)),
    stringsAsFactors = FALSE
  )
}))

blue <- style$primary
palette <- episodic_series_colours(style, levels(strategies$label))

base <- episodic_theme(style)

# EpiSODIC drawn last and heavier, so the brand colour sits on top of the
# neutrals wherever the curves cross.
cumulative$label <- factor(cumulative$label, levels = levels(strategies$label))
panel_a <- ggplot(cumulative, aes(x = time, y = detected, colour = label, linewidth = label)) +
  geom_step(direction = "hv") +
  scale_linewidth_manual(values = c(1.05, rep(0.7, nlevels(cumulative$label) - 1)), guide = "none") +
  scale_colour_manual(values = palette, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"), limits = c(0, 1)) +
  labs(
    title = "Share of all seeded outbreaks detected by day t",
    subtitle = "Every outbreak kept in the denominator, so each curve plateaus at its own sensitivity",
    x = "Days from the outbreak's first case",
    y = "Outbreaks detected"
  ) +
  base +
  theme(
    legend.position = c(0.99, 0.03),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 8.5),
    legend.key.height = unit(11, "pt"),
    legend.background = element_rect(fill = style$surface, colour = NA)
  )

# The burden scenarios watch histories with nothing seeded in them, so
# every alarm raised is a false one.
pathogens <- burden_df[grepl("^burden_pathogens_", burden_df$scenario), ]
if (!nrow(pathogens)) stop("alarm_burden.csv holds no pathogen-copy scenarios", call. = FALSE)
pathogens <- pathogens[order(pathogens$pathogens), ]

nominal <- data.frame(
  pathogens = pathogens$pathogens,
  value = pathogens$farrington_nominal_false_alarms_per_run,
  series = "Implied by the significance levels",
  stringsAsFactors = FALSE
)
observed <- data.frame(
  pathogens = pathogens$pathogens,
  value = pathogens$false_alarms_per_week,
  low = pathogens$false_alarms_per_week_ci_low,
  high = pathogens$false_alarms_per_week_ci_high,
  series = "Raised by EpiSODIC",
  stringsAsFactors = FALSE
)

delay <- burden_df[burden_df$scenario %in% c("burden_delay_weekly", "burden_delay_nightly"), ]
nightly_note <- if (nrow(delay) == 2) {
  weekly <- delay$false_alarms_per_week[delay$scenario == "burden_delay_weekly"]
  night <- delay$false_alarms_per_week[delay$scenario == "burden_delay_nightly"]
  sprintf(
    "Running nightly rather than weekly: %.3f against %.3f alarms per week",
    night, weekly
  )
} else {
  NULL
}

panel_b <- ggplot(mapping = aes(x = pathogens)) +
  geom_line(
    data = nominal, aes(y = value), colour = style$faint,
    linetype = "22", linewidth = 0.7
  ) +
  geom_point(data = nominal, aes(y = value), colour = style$faint, size = 2) +
  geom_linerange(data = observed, aes(ymin = low, ymax = high), colour = blue, linewidth = 0.6) +
  geom_line(data = observed, aes(y = value), colour = blue, linewidth = 0.85) +
  geom_point(data = observed, aes(y = value), colour = blue, size = 2.6) +
  annotate(
    "text",
    x = nominal$pathogens[[3]], y = nominal$value[[3]],
    label = "implied by the\nsignificance levels alone",
    hjust = 1.1, vjust = -0.25, size = 3.1, colour = style$muted, lineheight = 0.95
  ) +
  annotate(
    "text",
    x = observed$pathogens[[4]], y = observed$value[[4]],
    label = "raised by EpiSODIC",
    hjust = 1.02, vjust = -1.4, size = 3.1, colour = blue, fontface = "bold"
  ) +
  scale_x_continuous(breaks = pathogens$pathogens) +
  scale_y_continuous(limits = c(0, max(nominal$value) * 1.12)) +
  labs(
    title = "What watching more pathogens costs",
    subtitle = if (is.null(nightly_note)) {
      "False alarms per week on histories with nothing seeded in them"
    } else {
      paste0("False alarms per week on histories with nothing seeded in them.\n", nightly_note)
    },
    x = "Pathogens watched",
    y = "False alarms per week"
  ) +
  base

figure <- (panel_a / panel_b) +
  plot_layout(heights = c(1.1, 1)) +
  plot_annotation(tag_levels = "A")

ragg::agg_png(out_png, width = 1000, height = 1000, units = "px", res = 140, background = style$surface)
print(figure)
invisible(dev.off())

meta <- read_required("study_meta.csv")
cat(sprintf("wrote %s (1000 x 1000 px) from package version %s\n", out_png, meta$value[meta$key == "package_version"]))
print(pathogens[, c("pathogens", "streams_per_run", "false_alarms", "false_alarms_per_week",
                    "farrington_nominal_false_alarms_per_run")], row.names = FALSE)
