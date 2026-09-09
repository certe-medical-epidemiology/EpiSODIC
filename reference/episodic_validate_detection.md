# Measure Detection Against Known Truth

The dashboard's Performance screen measures this instance against its
own epidemiologists' verdicts. That is the right operational metric and
a circular one for a paper: it reports what the board thought of what
the system showed them, and an outbreak the detectors miss never becomes
a cluster for anyone to have an opinion about, so sensitivity cannot be
measured that way at all.

## Usage

``` r
episodic_validate_detection(
  seeds = 1,
  end_date = Sys.Date(),
  history_years = 1,
  evaluation_weeks = 4,
  min_recall = 0.5,
  min_precision = 0.5,
  outbreaks = TRUE,
  outbreak_offsets = NULL,
  detectors = episodic_validation_detectors(),
  config = NULL,
  quiet = TRUE
)

# S3 method for class 'episodic_validation'
print(x, ...)
```

## Arguments

- seeds:

  RNG seeds to replicate over. Seeds are the replicates: one realisation
  of a stochastic generator is an anecdote, so the summary reports
  medians and interquartile ranges across them.

- end_date:

  The last day of generated history, and the last run the replay makes.
  Defaults to today, which means two runs on different days are not
  comparable; pin it for anything whose numbers are going to be quoted.

- history_years:

  Years of endemic history before the evaluation window, for the
  detectors to build a baseline from.

- evaluation_weeks:

  Weekly runs to replay.

- min_recall, min_precision:

  The matching thresholds described above.

- outbreaks, outbreak_offsets:

  Passed to
  [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md).
  `outbreaks = FALSE` generates history with nothing seeded in it, which
  is the negative control: every alarm raised on it is a false one.

- detectors:

  Which detectors to switch on, for a drop-one analysis. Any of
  `"farrington"`, `"mem"`, `"same_place"` and `"rare_trigger"`; the rest
  are switched off in the instance configuration this writes, so
  `config_hash` records which ran.

- config:

  Further instance configuration to write, as a nested list (e.g.
  `list(farrington = list(alpha = 0.01))`), merged over the shipped
  defaults and validated the same way an operator's file is. It may not
  set any detector's `enabled` key - that is what `detectors` is for,
  and two mechanisms for one setting is one too many.

- quiet:

  If `TRUE` (the default), the per-run progress
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  writes is suppressed; a replay is hundreds of lines of it. Warnings
  and errors are not suppressed.

- x:

  An `episodic_validation` object.

- ...:

  Ignored.

## Value

An `episodic_validation` object: a list of

- `outbreaks`:

  One row per (seed, seeded outbreak): whether it was detected, by which
  cluster and which detector, how much of it that cluster held, how late
  (three ways - see below), and how much of it was still to come at that
  moment.

- `clusters`:

  One row per (seed, cluster raised): its case-set precision, whether it
  counts as a true positive, its priority score, and which detectors
  fired on it.

- `runs`:

  One row per (seed, run): streams watched, clusters raised, and how
  many of those survived lattice suppression.

- `overlap`:

  Every (run, cluster, outbreak) that shared a case, which is the raw
  material both tables above are derived from.

- `truth`:

  What was injected: the outbreak table and the case-level membership,
  per seed. Kept with the result so the matching thresholds can be
  varied afterwards
  ([`episodic_validate_rethreshold()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_rethreshold.md))
  without replaying anything.

- `time_to_detection`:

  The Kaplan-Meier curve of days to detection over the outbreaks that
  began inside the evaluation window, with the ones nothing found kept
  in as right-censored observations rather than dropped.

- `summary`:

  The headline numbers, one row per metric and group.

- `meta`:

  The package version, the resolved `config_hash`, the thresholds, the
  seeds, and the denominators every figure rests on.

## Details

This measures the detectors against outbreaks that are known to be there
because they were put there. It generates synthetic history with
[`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md),
replays it week by week through
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
against a throwaway database, and compares the clusters that came out
against the ground truth that went in.

## How a cluster is matched to an outbreak

On the cases themselves, in both directions, never on overlapping dates.
An endemic winter cluster spanning the same fortnight as a seeded
outbreak overlaps it perfectly in time and shares not one case with it.

- An **outbreak is detected** when some cluster's final membership holds
  at least `min_recall` of the outbreak's cases.

- A **cluster is a true positive** when at least `min_precision` of its
  own cases belong to one seeded outbreak. Anything else is a false
  alarm.

Both thresholds are arguments because both are choices, and a result
that only holds at one particular pair of values is not a result. Sweep
them and see
([`episodic_validate_rethreshold()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_rethreshold.md)
does it without replaying anything; `data-raw/validation/` does it over
a grid).

## Three delays, not one

The day something first appeared on the board that turned out to be this
outbreak (`delay_from_open`), the day what was on the board was mostly
this outbreak (`delay_from_first`), and the day it held most of the
whole outbreak, cases still to come included, are three different
claims. They can be days apart. All three are reported, because picking
one and calling it "the delay" would be choosing a number rather than
measuring one.

## Why it replays week by week

A single run over the whole history says nothing about timeliness: it
sees the outbreak's last case at the same moment as its first. So the
replay steps `run_date` forward one week at a time and hands each run
only the cases sampled on or before that date - what a laboratory
extract taken that day would actually have contained.

Two consequences worth knowing before reading any number out of this.
Farrington needs `(farrington$b + 1) * 52` weeks of history and MEM
needs two seasons, so an evaluation window that starts before the
baseline requirement is met measures the warm-up rather than the method:
keep `history_years` at 4 or more if the statistical detectors are meant
to be running. And all six seeded outbreaks are anchored to the end of
the generated window unless `outbreak_offsets` disperses them, so a
prospective evaluation wants them dispersed.

## Cost

Every run refits Farrington for every eligible stream, so the cost is
roughly (seeds x weeks) full detection runs. The defaults are
deliberately small - one seed, a short window, a single year of
history - so this returns in seconds and can be run from an example.
They are a demonstration of the machinery, not a study: at one year of
history the statistical detectors have no baseline to fit against and
report nothing, and one seed is one realisation. The study lives in
`data-raw/validation/`, is measured in minutes to hours, and is not run
by the test suite, by `R CMD check`, or by anything a user installs.

## See also

[`episodic_synthetic_ground_truth()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_ground_truth.md)
for what is being measured against, and
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
for the detection run this replays.

## Examples

``` r
# \donttest{
result <- episodic_validate_detection()
result$summary[result$summary$metric == "sensitivity", ]
#>         metric     group_type         group n_seeds    median       q25
#> 1  sensitivity        overall all outbreaks       1 0.8333333 0.8333333
#> 2  sensitivity outbreak_shape           LTC       1        NA        NA
#> 3  sensitivity outbreak_shape          PROP       1        NA        NA
#> 4  sensitivity outbreak_shape            PS       1        NA        NA
#> 5  sensitivity outbreak_shape          RARE       1        NA        NA
#> 6  sensitivity outbreak_shape          WARD       1        NA        NA
#> 7  sensitivity outbreak_shape          WAVE       1        NA        NA
#> 8  sensitivity design_channel    farrington       1        NA        NA
#> 9  sensitivity design_channel  rare_trigger       1        NA        NA
#> 10 sensitivity design_channel    same_place       1 1.0000000 1.0000000
#>          q75 numerator denominator  estimate    ci_low   ci_high interval
#> 1  0.8333333         5           6 0.8333333 0.4364972 0.9699466   wilson
#> 2         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 3         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 4         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 5         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 6         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 7         NA         0           1 0.0000000 0.0000000 0.7934507   wilson
#> 8         NA         0           1 0.0000000 0.0000000 0.7934507   wilson
#> 9         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 10 1.0000000         4           4 1.0000000 0.5101092 1.0000000   wilson
#>          unit
#> 1  proportion
#> 2  proportion
#> 3  proportion
#> 4  proportion
#> 5  proportion
#> 6  proportion
#> 7  proportion
#> 8  proportion
#> 9  proportion
#> 10 proportion
# }
```
