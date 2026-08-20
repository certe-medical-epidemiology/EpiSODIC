# Synthetic data with realistic signal volume for one pathogen, for calibration testing

[`episode_ingest_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_ingest_source_synthetic.md)
injects exactly two outbreaks total across a multi-year window - enough
to prove the detectors and reconciliation work, nowhere near enough to
tune anything against. The eligibility gate's own calibration target is
concrete - roughly ten assessed clusters a month, system-wide
(`episode_eligibility_gate()`); there is no way to tune towards a target
using two data points. This is not a substitute for real signal volume -
it is still fabricated data, and no amount of realism turns it into
"real data" - but it is an honest stand-in: many independent
`same_place`-shaped case bumps for one named pathogen, spread across LTC
institutions and hospitals over the whole window, at a volume an
operator can actually run the Prestatie screen and the eligibility gate
against while deciding how to tune them for a real instance. Every
cluster this produces is clearly synthetic in origin (`PT-VOL-*` patient
keys) so it is never mistaken for anything else.

## Usage

``` r
episode_ingest_source_synthetic_calibration(
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

  Which organism to generate elevated volume for. Defaults to
  *Clostridioides difficile*, a worked example of an endemic organism
  that produces frequent `same_place` clusters at a busy institution.

- n_bumps_per_month:

  Average number of independent case bumps generated per calendar month
  (Poisson-distributed), each becoming its own candidate cluster once
  reconciled. Tune this up or down to see how detection volume
  responds - that response is the entire point of this function.

- seed:

  RNG seed, for reproducible runs.

## Value

A data frame satisfying
[`episode_ingest_validate_source()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_ingest_validate_source.md),
including everything
[`episode_ingest_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_ingest_source_synthetic.md)
produces (background baseline, the two standard demo outbreaks) plus the
extra volume.

## Examples

``` r
raw <- episode_ingest_source_synthetic_calibration(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-06-30"),
  n_bumps_per_month = 4
)
sum(startsWith(raw$patient_key, "PT-VOL-"))
#> [1] 128
```
