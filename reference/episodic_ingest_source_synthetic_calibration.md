# Generate synthetic data at tunable cluster volume

[`episodic_ingest_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_source_synthetic.md)
injects exactly two outbreaks in total - enough to demonstrate detection
working, but not enough to tune your own configuration against (e.g.
deciding how many dossiers your board can realistically review per
month). This function fills that gap: on top of the same baseline and
two standard outbreaks, it adds many independent case clusters for one
chosen pathogen, at a rate you control, so you can see how detection
volume responds as you adjust `n_bumps_per_month` or your own
configuration. Every case it adds carries a `PT-VOL-*` patient key so it
is always identifiable as synthetic tuning data, never mistaken for
anything else.

## Usage

``` r
episodic_ingest_source_synthetic_calibration(
  start_date = as.Date("2021-01-01"),
  end_date = as.Date("2025-12-31"),
  pathogen = "Clostridioides difficile",
  n_bumps_per_month = 3,
  seed = 1
)
```

## Arguments

- start_date, end_date:

  The window to generate over.

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
[`episodic_ingest_validate_source()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_validate_source.md),
including everything
[`episodic_ingest_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_source_synthetic.md)
produces (background baseline, the two standard demo outbreaks) plus the
extra volume.

## Examples

``` r
raw <- episodic_ingest_source_synthetic_calibration(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-06-30"),
  n_bumps_per_month = 4
)
sum(startsWith(raw$patient_key, "PT-VOL-"))
#> [1] 128
```
