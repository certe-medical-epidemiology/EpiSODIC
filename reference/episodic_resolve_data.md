# Resolve a data source argument to a data frame

A small helper behind
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)'s
`cases`, `denominators`, and `institution_activity` arguments, each of
which accepts a data frame or `tibble` directly - the normal case - or,
if producing the data only makes sense at run time (a live database
query, for instance), a zero-argument function that returns one.

## Usage

``` r
episodic_resolve_data(x, ...)
```

## Arguments

- x:

  A data frame or `tibble`, a function returning one, or `NULL`.

- ...:

  Passed to `x` if it is a function; ignored otherwise.

## Value

`NULL` if `x` is `NULL`; `x` itself if it is a data frame (a `tibble`
included); the result of calling `x` otherwise.

## Details

Resolving is all this does: it does not look at what the data contains.
To find out whether your case data can actually be used - which columns
are missing, which values are outside their allowed set, which dates do
not read as dates, and which rows those are - run
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
on it, or
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
if you want a script to stop. Both accept the same two forms this does,
so you can check a data set and the function that produces it alike.

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
future.
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
runs the same checks but throws, for use in a script;
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
runs it for you before every run, and refuses to start on data it cannot
use, naming what to fix.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
to check the resolved data against the
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
contract.

## Examples

``` r
df <- data.frame(x = 1:3)
identical(episodic_resolve_data(df), df)
#> [1] TRUE
identical(episodic_resolve_data(function() df), df)
#> [1] TRUE
is.null(episodic_resolve_data(NULL))
#> [1] TRUE

# what a live query would look like, and how to check what it returns
my_extract <- function() {
  episodic_synthetic_cases(
    start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
  )
}
episodic_check_cases(episodic_resolve_data(my_extract))
#> -- EpiSODIC case data check ------------------------------------------------
#>    206 rows, 16 columns
#>    sample_date from 2025-01-01 to 2025-01-31
#>    10 pathogens, 62 institutions, 200 patients
#> 
#> v This data set satisfies the case data contract, and is ready for
#>   episodic_run_cron(). See ?episodic_case_data (`?episodic_case_data()`) for what each
#>   column means.
```
