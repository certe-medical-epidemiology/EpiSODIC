# Add a Testing-Volume (Positivity) Feed

Case counts alone cannot distinguish a rise in infections from a rise in
testing. If you can supply how many tests were performed - even as a
weekly aggregate, not per-test detail - EpiSODIC can show a positivity
rate alongside the case count, which is often the more meaningful
signal. This feed is entirely optional: skip it and positivity panels
simply stay blank.

## Usage

``` r
episodic_synthetic_denominators(
  start_date = end_date - 5 * 365,
  end_date = Sys.Date(),
  seed = 1
)
```

## Arguments

- start_date, end_date:

  The period to generate weekly rows for. Defaults to the five years up
  to today, matching
  [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md).

- seed:

  RNG seed, for reproducible demo data.

## Value

A data frame with `pathogen`, `sample_date` (week start), `care_line`,
`area_code`, `n_tests`.

## Details

This function is a synthetic example showing the expected shape: weekly
counts of a multiplex GI PCR panel that also reports Norovirus. Use it
as a template for your own data, which you pass to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
as `denominators` - a data frame or `tibble` with the same five columns:
`pathogen`, `sample_date` (week start), `care_line`, `area_code` (may be
`NA`), and `n_tests`.

## Examples

``` r
denom <- episodic_synthetic_denominators(
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
