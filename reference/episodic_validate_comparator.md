# Measure a Trivial Rule Against the Same Known Truth

A sensitivity of 0.8 and one false alarm per two thousand stream-weeks
mean very little on their own. They mean a great deal next to what a
rule anyone could write in an afternoon gets on the same data. This runs
such a rule over exactly the same generated history, week by week, and
reports it through the same matching and the same metrics as
[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md),
so the two sit in one table.

## Usage

``` r
episodic_validate_comparator(
  method = c("same_place", "shewhart"),
  seeds = 1,
  end_date = Sys.Date(),
  history_years = 1,
  evaluation_weeks = 4,
  min_recall = 0.5,
  min_precision = 0.5,
  outbreaks = TRUE,
  outbreak_offsets = NULL,
  n_cases = 3,
  k_days = 7,
  case_free_days = 14,
  baseline_weeks = 52,
  sd_multiplier = 2,
  quiet = TRUE
)
```

## Arguments

- method:

  Which rule to run.

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

- n_cases, k_days, case_free_days:

  The `"same_place"` rule's parameters: how many cases, within how many
  days, and how long a quiet gap ends one alarm and starts the next.

- baseline_weeks, sd_multiplier:

  The `"shewhart"` rule's parameters.

- quiet:

  If `TRUE` (the default), the per-run progress
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  writes is suppressed; a replay is hundreds of lines of it. Warnings
  and errors are not suppressed.

## Value

An `episodic_validation` object, in the same shape
[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)
returns, with `meta$method` naming the rule and `meta$config_hash`
absent - a rule this simple has no configuration to hash.

## Details

Two rules are available:

- `"same_place"`:

  Any `n_cases` cases of one pathogen at one place within `k_days`. A
  place is a ward inside a hospital and the institution itself
  everywhere else, which is what `episodic_detect_same_place()` watches.
  Cases at one place more than `case_free_days` apart belong to separate
  alarms.

- `"shewhart"`:

  A plain control limit on weekly counts: the whole catchment's count
  for one pathogen in the week just completed, against the mean plus
  `sd_multiplier` standard deviations of the `baseline_weeks` weeks
  before it. Consecutive alarming weeks are one alarm, not one each.

Neither reconciles, scores, suppresses or ages anything, which is the
point: they are the floor a four-detector design has to clear to be
worth its complexity. Their false-alarm denominator is their own -
places watched, or pathogens watched - and not EpiSODIC's stream count,
so read `denominator` before comparing two rates.

## See also

[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)

## Examples

``` r
# \donttest{
naive <- episodic_validate_comparator()
naive$summary[naive$summary$metric == "sensitivity", ]
#>         metric     group_type         group n_seeds median  q25  q75 numerator
#> 1  sensitivity        overall all outbreaks       1   0.50 0.50 0.50         3
#> 2  sensitivity outbreak_shape           LTC       1     NA   NA   NA         1
#> 3  sensitivity outbreak_shape          PROP       1     NA   NA   NA         1
#> 4  sensitivity outbreak_shape            PS       1     NA   NA   NA         1
#> 5  sensitivity outbreak_shape          RARE       1     NA   NA   NA         0
#> 6  sensitivity outbreak_shape          WARD       1     NA   NA   NA         0
#> 7  sensitivity outbreak_shape          WAVE       1     NA   NA   NA         0
#> 8  sensitivity design_channel    farrington       1     NA   NA   NA         0
#> 9  sensitivity design_channel  rare_trigger       1     NA   NA   NA         0
#> 10 sensitivity design_channel    same_place       1   0.75 0.75 0.75         3
#>    denominator estimate    ci_low   ci_high interval       unit
#> 1            6     0.50 0.1876163 0.8123837   wilson proportion
#> 2            1     1.00 0.2065493 1.0000000   wilson proportion
#> 3            1     1.00 0.2065493 1.0000000   wilson proportion
#> 4            1     1.00 0.2065493 1.0000000   wilson proportion
#> 5            1     0.00 0.0000000 0.7934507   wilson proportion
#> 6            1     0.00 0.0000000 0.7934507   wilson proportion
#> 7            1     0.00 0.0000000 0.7934507   wilson proportion
#> 8            1     0.00 0.0000000 0.7934507   wilson proportion
#> 9            1     0.00 0.0000000 0.7934507   wilson proportion
#> 10           4     0.75 0.3006418 0.9544127   wilson proportion
# }
```
