# Check your case data before you hand it to EpiSODIC

Runs every check the
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
contract implies over your extract and reports *everything* it finds in
one go - what is wrong, how many rows are affected, which rows those
are, what the offending values look like, and what to do about each one.
Nothing is written, nothing is changed, and no database is needed: this
is the call to make while you are still building your extract step, and
the first call to make when a run did not produce what you expected.

## Usage

``` r
episodic_check_cases(cases)
```

## Arguments

- cases:

  Your case data: a data frame or `tibble` in the shape
  [episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  describes, or a zero-argument function returning one (resolved with
  [`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md)
  first, so you can check either form). Anything else is itself reported
  as a problem rather than throwing - the point of this function is that
  it always answers.

## Value

A data frame of findings with class `episodic_case_check` and one row
per finding, with columns `severity` (`"problem"` or `"advice"`),
`issue`, `column`, `n_rows`, `rows`, `values`, `message` and `fix`. Zero
rows means the data set passed every check. A summary of what was read
(rows, columns, date range, counts) is attached as the `"summary"`
attribute and shown when printing.

## Details

[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
is the same checks with a different answer: it throws an error listing
the problems that must be fixed, and is what
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
itself calls before a run. Use `episodic_check_cases()` when you want to
*look*, and
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
when you want a script to stop.

## What it reports

Two kinds of finding, deliberately kept apart:

- Problems:

  Things EpiSODIC cannot work around: a missing or unexpected column, an
  empty value in a column that must be filled, a value outside a fixed
  set, a date that does not read as a date, a duplicated `source_key`. A
  run refuses to start while any of these stand.

- Advice:

  Things that are *allowed* but rarely intended, and that quietly cost
  you signal: no `ward` on any hospital row (so ward-level detection has
  nothing to group on), one pathogen spelled two ways (so its cases are
  split over two streams), a `patient_key` that is unique per row (so
  deduplication and episode grouping cannot do anything), postcodes that
  the geography panel cannot place, sample dates in the future. A run
  proceeds regardless; these are for you to judge.

The value prints as a report; it is also a plain data frame, one row per
finding, so you can work with it programmatically (see the examples).

## See also

[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for what each column means and which values it accepts;
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
for the same checks as an error.

## Examples

``` r
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
)
episodic_check_cases(cases)
#> -- EpiSODIC case data check ------------------------------------------------
#>    206 rows, 15 columns
#>    sample_date from 2025-01-01 to 2025-01-31
#>    10 pathogens, 62 institutions, 200 patients
#> 
#> v This data set satisfies the case data contract, and is ready for
#>   episodic_run_cron(). See ?episodic_case_data (`?episodic_case_data()`) for what each
#>   column means.

# a typical extract mistake: dates written day-first, sex as words
broken <- cases
broken$sample_date <- format(as.Date(broken$sample_date), "%d-%m-%Y")
broken$sex <- ifelse(broken$sex == "M", "male", "female")
report <- episodic_check_cases(broken)
report
#> -- EpiSODIC case data check ------------------------------------------------
#>    206 rows, 15 columns
#>    10 pathogens, 62 institutions, 200 patients
#> 
#> x 2 problems - a detection run refuses to start until these are fixed:
#> 
#>   1. `sex` has 206 of 206 rows with a value outside the allowed set ("M",
#>      "F", "U", or NA).
#>      values: female, male
#>      rows:   1, 2, 3, 4, 5 (and 201 more)
#>      fix:    Map your own coding onto the allowed values in your extract
#>              step; they are in `episodic_sex_codes`, so you need not copy
#>              the strings by hand.
#> 
#>   2. `sample_date` has 206 of 206 rows that are not a Date and do not read
#>      as YYYY-MM-DD.
#>      values: 01-01-2025, 02-01-2025, 03-01-2025, 04-01-2025, 05-01-2025
#>              (and 26 more)
#>      rows:   1, 2, 3, 4, 5 (and 201 more)
#>      fix:    These look day-first (e.g. 31-12-2025): convert with
#>              as.Date(x, format = "%d-%m-%Y"). EpiSODIC accepts a Date
#>              column, or text in ISO 8601 YYYY-MM-DD form, and nothing else
#>              - a date silently read the wrong way round is worse than one
#>              that refuses to read.
#> 
#> Nothing was changed here. Fix the problems above in your own extract step,
#> then check again. See ?episodic_case_data (`?episodic_case_data()`) for what each column means
#>   and which values it accepts.

# it is a data frame too
report$column[report$severity == "problem"]
#> [1] "sex"         "sample_date"
```
