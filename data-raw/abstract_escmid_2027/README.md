# ESCMID Global 2027 abstract: figures

The three figures submitted with the abstract, and the scripts that make
them. Everything here is reproducible from the repository: nothing was
drawn by hand, and no number was typed in.

Run from this directory, with no arguments:

```bash
Rscript figure1_dossier.R
Rscript figure2_detection_burden.R
Rscript figure3_coverage.R
```

Each resolves its inputs relative to its own location, so the directory
moves as one piece. Each takes an input path and an output path as
optional arguments.

| Figure | Script | Source |
|---|---|---|
| `figure1_dossier_0.25.0.png` | `figure1_dossier.R` | `man/figures/main_screen.png` |
| `figure2_detection_burden_0.25.0.png` | `figure2_detection_burden.R` | `data-raw/validation/results/` |
| `figure3_coverage_0.25.0.png` | `figure3_coverage.R` | `data-raw/validation/results/` |

The version in each file name is the package version of the validation
study that produced the numbers, read from `results/study_meta.csv`, not
the version of the package when the figure was drawn. The study behind
these is 0.25.0, config hash `7fef366a`, 20 seeds by 26 weekly runs,
460,534 stream-weeks, 120 seeded outbreaks. Re-running a figure against
a later study means a new file name.

## Colours

`episodic_palette.R` reads `inst/config/episodic_default_style.yaml` at
render time rather than carrying a copy, so a figure cannot drift from
the dashboard, the reports and the emails it illustrates. EpiSODIC is
drawn in the brand primary and every comparator in the palette's own
neutrals, darkest for the strongest comparator: one saturated colour on
a plot means one thing, and a reader who knows the dashboard recognises
it.

## Intervals

Proportions carry exact binomial (Clopper-Pearson) intervals, computed
from the counts rather than read from `summary.csv`, which stores Wilson
intervals. See issue #90. They are reported at the whole percentage: at
120 outbreaks the estimate moves in steps of 0.83 percentage points, and
a decimal place would assert a resolution the denominator does not have.

## Not in the abstract

`figure2_detection_burden.R` also computes the sensitivity-against-alarm
-burden operating point, and the study holds a six-point threshold sweep
(`operating_points.csv`), a priority-score calibration curve and the
drop-one detector ablation. None of it fitted in three figures.
