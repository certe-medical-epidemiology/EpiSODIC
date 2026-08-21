# Resolve the optional region-outline overlay

A second, independent geographic layer, drawn as outlines (no fill, a
thicker line) on top of the PC choropleth - for boundaries an operator
wants visible for orientation (provinces, municipalities, catchment
areas) but that carry no case counts of their own, so
`episodic_geo_join()`'s `pc`-keyed contract does not apply here: the
overlay needs nothing but a `geometry` column. Unlike
[`episodic_geo_source_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo_source_resolve.md),
there is no shipped default - region boundaries are far more
jurisdiction-specific than postcode geometry, and guessing at a
"sensible default" (which country's provinces?) would be arbitrary in a
way the shipped postcode default is not (`EPISODIC_GEO_DATA` is
Netherlands-only *labelled as such*, not pretending to be universal). No
`EPISODIC_GEO_DATA_OVERLAY` set (or an invalid file) simply means no
overlay layer, same as no `sf` at all.

## Usage

``` r
episodic_geo_overlay_resolve(
  path = Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
)
```

## Arguments

- path:

  Path to an `.rds` file holding an `sf` object with a `geometry`
  column. Defaults to the `EPISODIC_GEO_DATA_OVERLAY` environment
  variable.

## Value

An `sf` object, or `NULL` if `sf` is not installed, the variable is
unset, or the file is missing/invalid.

## Examples

``` r
# NULL when unset (or when the sf package is not installed)
episodic_geo_overlay_resolve(path = NA)
#> NULL
```
