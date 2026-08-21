# Resolve a data source argument to a data frame

A small helper behind
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)'s
`ingest_source`, `denominator_source`, and `institution_activity_source`
arguments, each of which accepts a data frame directly - the normal
case - or, if producing the data only makes sense at run time (a live
database query, for instance), a zero-argument function that returns
one.

## Usage

``` r
episodic_resolve_source(x, ...)
```

## Arguments

- x:

  A data frame, a function returning one, or `NULL`.

- ...:

  Passed to `x` if it is a function; ignored otherwise.

## Value

`NULL` if `x` is `NULL`; `x` itself if it is a data frame; the result of
calling `x` otherwise.

## Examples

``` r
df <- data.frame(x = 1:3)
identical(episodic_resolve_source(df), df)
#> [1] TRUE
identical(episodic_resolve_source(function() df), df)
#> [1] TRUE
is.null(episodic_resolve_source(NULL))
#> [1] TRUE
```
