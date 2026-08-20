# Synthetic positivity metadata source

A worked example of the optional denominator contract
(`episode_denominator_ingest_run()`): weekly test-panel counts for a
multiplex GI PCR panel that reports Norovirus alongside several other
targets, the kind of aggregate a lab LIS can produce trivially even
though it would never hand over the underlying per-test rows. Not called
by
[`episode_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_run_cron.md)
unless a `denominator_source_fn` is supplied; demonstrates the shape
only.

## Usage

``` r
episode_denominator_source_synthetic(
  start_date = as.Date("2021-01-01"),
  end_date = as.Date("2025-12-31"),
  seed = 1
)
```

## Arguments

- start_date, end_date:

  The period to generate weekly rows for.

- seed:

  RNG seed.

## Value

A data frame with `pathogen`, `sample_date` (week start), `care_line`,
`area_code`, `n_tests`.

## Examples

``` r
denom <- episode_denominator_source_synthetic(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
head(denom)
#>    pathogen sample_date care_line area_code n_tests
#> 1 Norovirus  2025-01-01    second      <NA>      36
#> 2 Norovirus  2025-01-08    second      <NA>      48
#> 3 Norovirus  2025-01-15    second      <NA>      48
#> 4 Norovirus  2025-01-22    second      <NA>      42
#> 5 Norovirus  2025-01-29    second      <NA>      30
#> 6 Norovirus  2025-02-05    second      <NA>      43
```
