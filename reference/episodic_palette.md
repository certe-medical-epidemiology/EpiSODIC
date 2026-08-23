# The dashboard's colour palette

Returns the colours used throughout the EpiSODIC dashboard and charts,
as a named list of hex codes. Useful if you want to match your own plots
or reports to the house style, or check what colour a given status uses.

## Usage

``` r
episodic_palette()
```

## Value

A named list of hex colour strings. The greyscale neutrals are `ink`
(default text), `muted` (secondary text), `faint` (tertiary text),
`border`, `bg_subtle`, `bg`, and `surface`. The semantic roles are
`primary`, `secondary`, `tertiary`, `success`, `warning`, and `danger`,
each with `_dark`/`_light`/`_tint` variants where used.

## Details

The palette ships with an organisation-neutral default
(`inst/config/palette.yaml`). To use your own institute's colours
instead, point the `EPISODIC_PALETTE_CONFIG` environment variable at a
YAML file that overrides only the roles you want to change - anything
you do not set keeps its shipped default. This is independent of
[`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md)
on purpose: colours never affect the `config_hash` recorded with a
detection run, since they have no bearing on reproducibility.

## Examples

``` r
pal <- episodic_palette()
pal$primary
#> [1] "#008CBA"
pal$danger
#> [1] "#F36A5A"
```
