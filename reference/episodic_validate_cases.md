# Check that your case data has the right shape, or stop

Call this on the data set you intend to hand to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
while preparing it, to get a clear error while you can still fix the
extract - rather than halfway through a scheduled run, where the same
problem surfaces as a rolled-back transaction and a failed run.
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
calls it itself, before it writes anything, so a run never starts on
data it cannot use.

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
every offending column, its offending values and the rows they are in,
otherwise.

## Details

It runs exactly the checks
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
runs and reports every problem it finds at once, rather than stopping at
the first: that the columns are exactly
[episodic_case_columns](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md),
that `source_key` is unique, that the columns which may never be empty
are filled, that `care_line`, `institution_type` and `sex` hold only
allowed values, that the two date columns read as dates, and that `age`
is a number. It reports what your data says, and does not change it: the
`NA` that
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
will read as `"unknown"` is left as `NA` in what comes back.

Use
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
instead when you would rather look than stop: it returns the same
findings as a printed report, adds the advisory ones this function stays
silent about, and never throws.

## Check your data before you run anything

Do not find out from an empty dashboard. Run
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
on your extract - it needs no database, changes nothing, and reports
every problem it finds at once, with the rows and values involved and
what to do about each:

    cases <- my_extract_and_transform_function()
    episodic_check_cases(cases)

It also reports what is merely worth a look: one pathogen spelled two
ways (two streams instead of one), no `ward` on any hospital row (no
ward-level detection), a `patient_key` that never repeats (no
deduplication), postcodes the map cannot place, sample dates in the
future. `episodic_validate_cases()` runs the same checks but throws, for
use in a script;
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
runs it for you before every run, and refuses to start on data it cannot
use, naming what to fix.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
for the same checks as a report you can read and filter, and
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for what each column means.

## Examples

``` r
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
)
episodic_validate_cases(cases)
```
