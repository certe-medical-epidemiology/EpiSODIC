# Generate Synthetic Data at Tunable Cluster Volume

[`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md)
injects six outbreaks in total - enough to show every detector working,
and few enough that a demo dashboard reads like a real morning's work.
That is the wrong shape for tuning a configuration against (e.g.
deciding how many dossiers your board can realistically review per
month). This function fills that gap: on top of the same baseline and
the same six outbreaks, it adds many independent case clusters for one
chosen pathogen, at a rate you control, so you can see how detection
volume responds as you adjust `n_bumps_per_month` or your own
configuration. Every case it adds carries a `PT-VOL-*` patient key so it
is always identifiable as synthetic tuning data, never mistaken for
anything else.

## Usage

``` r
episodic_synthetic_cases_calibration(
  start_date = end_date - 5 * 365,
  end_date = Sys.Date(),
  pathogen = "Clostridioides difficile",
  n_bumps_per_month = 3,
  seed = 1
)
```

## Arguments

- start_date, end_date:

  The window to generate over; defaults as in
  [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md).

- pathogen:

  Which pathogen to generate the extra clusters for. Defaults to
  *Clostridioides difficile*, a plausible example of an endemic pathogen
  that produces frequent clusters at a busy institution.

- n_bumps_per_month:

  Average number of independent case clusters generated per calendar
  month. Raise or lower this to see how detection volume responds.

- seed:

  RNG seed, for reproducible runs.

## Value

A data frame satisfying
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md),
including everything
[`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md)
produces (background baseline, the six demo outbreaks) plus the extra
volume.

## Examples

``` r
cases <- episodic_synthetic_cases_calibration(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-06-30"),
  n_bumps_per_month = 4
)
sum(startsWith(cases$patient_key, "PT-VOL-"))
#> [1] 141
```
