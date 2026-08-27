# Check your positivity data before you hand it to EpiSODIC

The same purpose as
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md),
for the optional `denominators` feed: every check
`episodic_validate_denominators()` would throw on, reported instead of
thrown - what is wrong, how many rows, which ones, and what to do about
it - plus a couple of things that are allowed but worth a look. Nothing
is written, no database is needed. Run this while building your extract
step, or when a run's positivity panels come out empty or look wrong.

## Usage

``` r
episodic_check_denominators(denominators)
```

## Arguments

- denominators:

  Your positivity data: a data frame or `tibble` with `pathogen`,
  `sample_date`, `care_line`, `area_code` and `n_tests` (see
  [`episodic_synthetic_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_denominators.md)
  for the shape), or a zero-argument function returning one.

## Value

A data frame of findings with class `episodic_case_check`, the same
shape
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
returns. Zero rows means the data set passed every check.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
for the case data feed,
[`episodic_check_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_institution_activity.md)
for the hospital activity feed.

## Examples

``` r
denom <- episodic_synthetic_denominators(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
episodic_check_denominators(denom)
#> -- EpiSODIC denominator data check -----------------------------------------
#>    13 rows, 5 columns
#> 
#> v Denominator data satisfies its contract, and is ready for episodic_run_cron().
```
