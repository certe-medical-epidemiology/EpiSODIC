# Show Clusters on a Map

The dashboard can plot cluster case counts on a choropleth map by
postcode (or any other geographic unit you use), provided the optional
`sf` package is installed and geographic reference data is available.
Without either, the dashboard still works fine - it just falls back to a
bar chart of case counts by area instead of a map.

## Usage

``` r
episodic_geo_overlay_resolve(
  path = Sys.getenv("EPISODIC_GEO_DATA_OVERLAY", unset = NA)
)

episodic_geo_source_resolve(path = Sys.getenv("EPISODIC_GEO_DATA", unset = NA))
```

## Arguments

- path:

  Path to an `.rds` file holding an `sf` object with `pc` and `geometry`
  columns. Defaults to the `EPISODIC_GEO_DATA` environment variable.
  With none set, there is no map and the dashboard shows its bar-chart
  fallback instead.

## Value

An `sf` object, or `NULL` if `sf` is not installed, no path is set, or
the file is missing or invalid.

## Details

EpiSODIC ships with Dutch four-digit postcode geometry as a working
default, but is not tied to the Netherlands or to postcodes: point the
`EPISODIC_GEO_DATA` environment variable at your own `.rds` file (an
`sf` object with a `pc` column matching your case data's area codes, and
a `geometry` column) to map your own region instead.

You can optionally add a second, purely visual layer of outlines - e.g.
province or municipality borders - drawn on top of the choropleth for
orientation, via `EPISODIC_GEO_DATA_OVERLAY` and
`episodic_geo_overlay_resolve()`. This layer carries no case counts, so
it only needs a `geometry` column.

## Why there is no default map

This used to fall back to the Netherlands PC4 geometry bundled with the
package whenever `EPISODIC_GEO_DATA` was unset, and even after warning
about an operator file it could not use. Postcode-like codes are four
digits in a great many countries, so a laboratory anywhere else got a
silent, confident map of the Netherlands with its own case counts joined
onto whichever Dutch postcodes happened to share a number - a
plausible-looking wrong answer, which is worse than no answer. There is
now no default: with nothing configured the map is simply absent, and
the dashboard falls back to a bar chart of case counts by area, which is
correct everywhere.

[`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
sets `EPISODIC_GEO_DATA` to the bundled Netherlands geometry itself, so
the demo still shows a map.

## Examples

``` r
# NULL when unset (or when the sf package is not installed)
episodic_geo_overlay_resolve(path = NA)
#> NULL
# NULL when unset (or when the sf package is not installed)
episodic_geo_source_resolve(path = NA)
#> NULL
```
