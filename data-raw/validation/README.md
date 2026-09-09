# Detection validation study

**For the methods, the results and the limitations, read
[`HANDOVER.md`](HANDOVER.md).** This file is only how to run it.

`episodic_app_performance()` measures an instance against its own
epidemiologists' verdicts on the clusters it showed them. That is a good
operational metric and a circular one for a paper, and sensitivity is
structurally unmeasurable that way: an outbreak the detectors miss never
becomes a cluster for anyone to have an opinion about.

This directory measures the detectors against outbreaks that are known to
be there because `episodic_synthetic_cases()` put them there, and reports
what it found in a form a methods reviewer will recognise.

## Running it

From the package root:

```bash
Rscript data-raw/validation/run_study.R
```

Roughly three hours on a modern laptop: twenty seeds by twenty-six weekly
runs, each of which refits Farrington for every eligible stream, times
six detection scenarios, plus a six-point operating-curve sweep at five
seeds. The five comparator runs are arithmetic over the same generated
data and take seconds. Check the wall clock before raising the seed
count.

The naive same-place rule is run at both three-in-seven and
three-in-fourteen days. Reporting only the tighter setting would make the
comparator look weak for a reason that has nothing to do with it being
naive: `same_place` itself is configured for fourteen.

Each scenario's full result is cached under `results/raw/` and reused if
it is already there, so an interrupted run resumes rather than starting
over. **The cache knows nothing about the code that produced it**: change
anything under `R/` and the scenarios that exercise it must be re-run.
Delete `results/raw/`, or:

```bash
EPISODIC_VALIDATION_FRESH=true Rscript data-raw/validation/run_study.R
```

which ignores the cache entirely. The committed CSVs must always be
reproducible by a fresh run, and a fresh run is what settles any
disagreement.

For a five-minute smoke test that exercises every scenario at a fraction
of the size:

```bash
EPISODIC_VALIDATION_QUICK=true Rscript data-raw/validation/run_study.R
```

Nothing here ships. `data-raw/` is in `.Rbuildignore`, so none of it is in
the built package, none of it runs under `R CMD check`, and none of it is
reachable from an installed EpiSODIC. The exported entry point,
`episodic_validate_detection()`, returns in seconds at its own defaults,
and those defaults are what the examples and the test suite use.

Each replicate creates its own throwaway SQLite database with
`tempfile()`, writes its own instance configuration into a temporary
directory, and removes both afterwards. It never runs against a live
instance, and it deliberately does not go through `episodic_demo()`.

## What it writes

Into `results/`. The CSVs are committed; `results/raw/`, which holds the
full result object of every scenario, is not, and the script regenerates
it.

| File | One row per |
|---|---|
| `summary.csv` | (scenario, metric, group) - the headline numbers |
| `outbreaks.csv` | (scenario, seed, seeded outbreak) |
| `clusters.csv` | (scenario, seed, cluster raised) |
| `runs.csv` | (scenario, seed, weekly run) |
| `drop_one.csv` | detector removed - sensitivity lost, false alarms saved |
| `time_to_detection_km.csv` | (scenario, event time) on the Kaplan-Meier curve |
| `priority_score_calibration.csv` | score bin |
| `threshold_sensitivity.csv` | (min_recall, min_precision, metric, group) |
| `operating_points.csv` | detection-threshold setting |
| `study_meta.csv` | study parameter |

Every row carries the package version and the resolved `config_hash` that
produced it, so a number in the paper can be traced back to the
configuration it came from, and the seeds, runs and stream-weeks behind
it, so a rate is never read without its denominator in the same file.

## Reading the numbers

**Sensitivity** and **PPV** are proportions: `numerator`, `denominator`,
`estimate` and a Wilson interval. **False alarms** are a rate per
stream-week, with the stream-week denominator stated and an exact Poisson
interval; a count of false alarms without its denominator is
uninterpretable, and the denominator here (streams x runs) is large and
moves with the configuration. `median`, `q25` and `q75` describe the
spread across seeds, which are the replicates.

**Timeliness** is reported three ways, on purpose. The median delay over
detected outbreaks is in `summary.csv` and must never be quoted on its
own: it is computed over exactly the outbreaks the method found, so it is
biased downward, and worst where the method is weakest. Quote it with the
share never detected beside it, or quote the Kaplan-Meier median, which
keeps the misses in as censored observations.

**A metric with no denominator reports `NA`, never 0.** A negative
control has no outbreaks to find, so its sensitivity is `NA`; that is not
a sensitivity of zero, and averaging it as one would be a mistake the
tables are built to prevent.

## What is deliberately not here

No threshold was tuned to make a number look better. If the sensitivity
for a given shape is poor, the study reports it. The operating-point
sweep is a curve for the paper, not a proposal to move the shipped
defaults in `inst/config/episodic_default_config.yaml`; changing those is
a separate discussion with the maintainer, and doing it in the same
breath as the evaluation would be fitting the evaluation to the answer.

## Known restrictions

Two, stated rather than papered over.

**Warm-up.** Farrington needs `(farrington$b + 1) * 52` weeks of history
and MEM needs two seasons. The study generates four years before the
evaluation window opens, so the statistical detectors are fitting by the
first run. A shorter history measures the warm-up instead of the method.

**Where the outbreaks sit.** All six are anchored to the end of the
generated window by default, which would put every one of them in the
last eight weeks of a twenty-six week evaluation. The script disperses
them with `outbreak_offsets`, computed from each outbreak's own span so
that all six begin inside the window, and reports any that did not.
