# The application colour palette

Read from `inst/config/palette.yaml`, the same defaults-then-instance-
override pattern
[`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md)
uses for detection configuration (deliberately a *separate* file and env
var, `EPISODIC_PALETTE_CONFIG`: colours must never affect `config_hash`,
which is about detection reproducibility, not display). Any organisation
runs EpiSODIC in its own house colours by pointing
`EPISODIC_PALETTE_CONFIG` at a YAML file overriding whichever roles it
wants to rebrand - a department that wants its own colours supplies its
own file, exactly as it supplies its own report template
(`EPISODIC_QUARTO_REPORT`) or geographic reference data
(`EPISODIC_GEO_DATA`); this package ships only the generic mechanism and
an organisation-neutral default, never a specific organisation's house
style.

## Usage

``` r
episodic_palette()
```

## Value

A named list of hex colour strings: the neutrals (`ink` default text,
`muted` secondary text, `faint` tertiary, `border`, `bg_subtle`, `bg`,
`surface` - a true grey scale, independent of whichever hue is
`primary`) and the semantic roles `primary` (+`_dark`/`_light`/`_tint`),
`secondary` (+`_dark`), `tertiary` (+`_dark`), `success` (+`_dark`),
`warning` (+`_dark`), `danger` (+`_dark`).

## Examples

``` r
pal <- episodic_palette()
pal$primary
#> [1] "#008CBA"
```
