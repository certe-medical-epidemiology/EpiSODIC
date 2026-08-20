# Resolve the geographic reference dataset to use

Resolve the geographic reference dataset to use

## Usage

``` r
episode_geo_source_resolve(path = Sys.getenv("EPISODE_GEO_DATA", unset = NA))
```

## Arguments

- path:

  Path to an `.rds` file holding an `sf` object with `pc` and `geometry`
  columns. Defaults to the `EPISODE_GEO_DATA` environment variable; if
  unset (or the file does not exist), falls back to the shipped
  Netherlands postcode default.

## Value

An `sf` object, or `NULL` if `sf` is not installed.

## Examples

``` r
# falls back to the shipped Netherlands postcode default when sf is
# installed, or NULL when it is not
geo <- episode_geo_source_resolve(path = NA)
```
