# Check that your case data has the right shape

Call this on the data set you intend to hand to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
while preparing it, to get a clear error while you can still fix the
extract - rather than halfway through a scheduled run, where the same
problem surfaces as a rolled-back transaction and a failed run.

## Usage

``` r
episodic_validate_cases(cases)
```

## Arguments

- cases:

  Your case data: a data frame or `tibble` in the shape
  [episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  describes, or a zero-argument function returning one (resolved with
  [`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md)
  first, so you can check either form of `cases`).

## Value

The validated data set, invisibly. Throws an informative error naming
the offending column, and the offending values, otherwise.

## Details

It checks everything
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
documents: that the columns are exactly
[episodic_case_columns](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md),
that `source_key` is unique, that the columns which may never be `NA`
are filled, that `care_line`, `institution_type` and `sex` hold only
allowed values, that the two date columns parse, and that `age` is
numeric. It reports what your data says, and does not change it: the
`NA` that
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
will read as `"unknown"` is left as `NA` in what comes back.

## Examples

``` r
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
)
episodic_validate_cases(cases)
```
