#!/usr/bin/env Rscript

# Figure 3 for the ESCMID Global 2027 abstract.
#
# Panel A: which kinds of outbreak each strategy finds, ordered from a
#          single case to a regional wave. A naive rule is deterministic
#          per type, so its cells are 20/20 or 0/20 and the blanks are
#          whole classes of outbreak it cannot see.
# Panel B: what one weekly run puts in front of an epidemiologist, split
#          into clusters that were real and alarms that were not.
#
# Usage:
#   Rscript figure3_coverage.R [results_dir] [output_png]

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(script_dir, "episodic_palette.R"))
style <- episodic_style(file.path(script_dir, "..", "..", "inst", "config", "episodic_default_style.yaml"))

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) path.expand(args[[1]]) else file.path(script_dir, "..", "validation", "results")
out_png <- if (length(args) >= 2) path.expand(args[[2]]) else file.path(script_dir, "figure3_coverage_0.25.0.png")

read_required <- function(file) {
  path <- file.path(results_dir, file)
  if (!file.exists(path)) stop("missing results file: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

outbreaks <- read_required("outbreaks.csv")
summary_df <- read_required("summary.csv")

strategies <- data.frame(
  scenario = c("main", "comparator_shewhart", "comparator_same_place_14", "comparator_same_place_7"),
  label = c("EpiSODIC", "Shewhart", "Same-place\n3 in 14 days", "Same-place\n3 in 7 days"),
  stringsAsFactors = FALSE
)
missing <- setdiff(strategies$scenario, outbreaks$scenario)
if (length(missing)) stop("results hold no scenario called: ", paste(missing, collapse = ", "), call. = FALSE)

# Ordered by how large the outbreak is, so the row reads from one case to
# a regional wave and the gaps fall at the two ends.
types <- data.frame(
  outbreak_id = c("RARE", "WARD", "LTC", "PS", "PROP", "WAVE"),
  heading = c(
    "Invasive\nmeningococcal\ndisease",
    "Ward\ncluster",
    "Nursing home\noutbreak",
    "Point\nsource",
    "Propagated\noutbreak",
    "Regional\nwave"
  ),
  stringsAsFactors = FALSE
)

d <- outbreaks[outbreaks$scenario %in% strategies$scenario, ]
d$detected <- as.logical(d$detected)

cases <- vapply(
  types$outbreak_id,
  function(id) stats::median(d$n_cases[d$scenario == "main" & d$outbreak_id == id]),
  numeric(1)
)
types$heading <- sprintf(
  "%s\n%s",
  types$heading,
  ifelse(cases == 1, "1 case", sprintf("%g cases", cases))
)

coverage <- do.call(rbind, lapply(seq_len(nrow(strategies)), function(i) {
  do.call(rbind, lapply(seq_len(nrow(types)), function(j) {
    rows <- d[d$scenario == strategies$scenario[[i]] & d$outbreak_id == types$outbreak_id[[j]], ]
    if (nrow(rows) == 0) stop("no outbreaks for a strategy and type", call. = FALSE)
    data.frame(
      strategy = strategies$label[[i]],
      type = types$heading[[j]],
      found = sum(rows$detected),
      total = nrow(rows),
      share = sum(rows$detected) / nrow(rows),
      stringsAsFactors = FALSE
    )
  }))
}))
# The bottom line in the same picture: an empty level leaves a gap, so
# the total reads as a summary of the row rather than a seventh type.
totals <- do.call(rbind, lapply(split(coverage, coverage$strategy), function(x) {
  data.frame(
    strategy = x$strategy[[1]],
    type = "All types\n120 outbreaks",
    found = sum(x$found),
    total = sum(x$total),
    share = sum(x$found) / sum(x$total),
    stringsAsFactors = FALSE
  )
}))
coverage <- rbind(coverage, totals)

coverage$strategy <- factor(coverage$strategy, levels = rev(strategies$label))
coverage$type <- factor(
  coverage$type,
  levels = c(types$heading, " ", "All types\n120 outbreaks")
)
coverage$shown <- sprintf("%d/%d", coverage$found, coverage$total)

# One weekly run's output, split by whether the cluster was real. The run
# count is the study's own, not assumed.
runs <- unique(summary_df$n_runs[summary_df$scenario == "main"]) *
  unique(summary_df$n_seeds_total[summary_df$scenario == "main"])
if (length(runs) != 1 || !is.finite(runs)) stop("cannot resolve the number of runs", call. = FALSE)

workload <- do.call(rbind, lapply(seq_len(nrow(strategies)), function(i) {
  p <- summary_df[
    summary_df$scenario == strategies$scenario[[i]] &
      summary_df$metric == "ppv" &
      summary_df$group == "all clusters raised",
  ]
  if (nrow(p) != 1) stop("expected one predictive-value row", call. = FALSE)
  data.frame(
    strategy = rep(strategies$label[[i]], 2),
    kind = c("Real clusters", "False alarms"),
    per_run = c(p$numerator, p$denominator - p$numerator) / runs,
    ppv = p$numerator / p$denominator,
    stringsAsFactors = FALSE
  )
}))
workload$strategy <- factor(workload$strategy, levels = rev(strategies$label))
workload$kind <- factor(workload$kind, levels = c("Real clusters", "False alarms"))

blue <- style$primary
rust <- style$danger

base <- episodic_theme(style) +
  theme(axis.text.y = element_text(size = 9.5, lineheight = 0.95, hjust = 1, colour = style$muted))

emphasise <- function(levels) {
  ifelse(levels == "EpiSODIC", "bold", "plain")
}

panel_a <- ggplot(coverage, aes(x = type, y = strategy, fill = share)) +
  geom_tile(colour = style$surface, linewidth = 2) +
  geom_text(
    aes(label = shown, colour = share > 0.55),
    size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_fill_gradient(low = style$primary_tint, high = blue, limits = c(0, 1)) +
  scale_colour_manual(values = c(`TRUE` = style$surface, `FALSE` = style$muted)) +
  scale_x_discrete(position = "top", expand = c(0, 0), drop = FALSE) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "Which kinds of outbreak each strategy finds",
    subtitle = "Outbreaks detected out of 20 seeds, from a single case to a regional wave",
    x = NULL, y = NULL
  ) +
  base +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x.top = element_text(size = 8.2, lineheight = 0.95, vjust = 0),
    axis.text.y = element_text(face = emphasise(levels(coverage$strategy)))
  )

labels_b <- workload[workload$kind == "Real clusters", ]
labels_b$total <- vapply(
  labels_b$strategy,
  function(s) sum(workload$per_run[workload$strategy == s]),
  numeric(1)
)

panel_b <- ggplot(workload, aes(x = per_run, y = strategy, fill = kind)) +
  geom_col(width = 0.62, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = labels_b,
    aes(x = total, y = strategy, label = sprintf("%.0f%% real", 100 * ppv)),
    inherit.aes = FALSE, hjust = -0.18, size = 3.2, colour = style$muted
  ) +
  scale_fill_manual(values = c(`Real clusters` = blue, `False alarms` = rust), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "What one weekly run puts in front of the epidemiologist",
    subtitle = "Clusters opened per run, across 520 runs",
    x = "Clusters per weekly run", y = NULL
  ) +
  base +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = style$border, linewidth = 0.3),
    axis.text.y = element_text(face = emphasise(levels(workload$strategy))),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(b = -4),
    legend.key.size = unit(10, "pt"),
    legend.text = element_text(size = 9)
  )

figure <- (panel_a / panel_b) +
  plot_layout(heights = c(1.15, 1)) +
  plot_annotation(tag_levels = "A")

ragg::agg_png(out_png, width = 1000, height = 1000, units = "px", res = 140, background = style$surface)
print(figure)
invisible(dev.off())

meta <- read_required("study_meta.csv")
cat(sprintf(
  "wrote %s (1000 x 1000 px) from package version %s, %d runs\n",
  out_png, meta$value[meta$key == "package_version"], runs
))
print(coverage[, c("strategy", "type", "shown")], row.names = FALSE)
