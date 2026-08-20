# Resolve a `*_source_fn` argument to the data frame it names

Every
[`episode_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_run_cron.md)
data source (`ingest_source_fn`, `denominator_source_fn`,
`institution_activity_source_fn`) accepts either a function that
produces the data frame, or that data frame itself - a function is only
useful when producing the data has to happen at run time (a live query,
a freshly-generated synthetic set); an operator who already has the data
sitting in a variable has no reason to wrap it in `function() my_df`.

## Usage

``` r
episode_resolve_source(x, ...)
```

## Arguments

- x:

  A function, a data frame, or `NULL`.

- ...:

  Passed to `x` if it is a function; ignored otherwise (a data frame
  that is already the answer does not need `institutions` handed to it,
  for instance).

## Value

`NULL` if `x` is `NULL`; `x` itself if it is a data frame; the result of
calling `x` otherwise.

## Examples

``` r
df <- data.frame(x = 1:3)
identical(episode_resolve_source(df), df)
#> [1] TRUE
identical(episode_resolve_source(function() df), df)
#> [1] TRUE
is.null(episode_resolve_source(NULL))
#> [1] TRUE
```
