# Render a list of detector names as inline code, joined by a separator

A detector name (`farrington`, `same_place`, `rare_trigger`, ...) is an
identifier from the codebase, not prose - rendered in `<code>` so it
reads as one, wherever it appears in a sentence or table (dossier meta
line, settings panel, timeline). Text is HTML-escaped before any tag is
added, so the result is always safe to pass to
[`shiny::HTML()`](https://rdrr.io/pkg/shiny/man/reexports.html).

## Usage

``` r
episodic_ui_code_join(detectors, sep = ", ")
```

## Arguments

- detectors:

  A character vector of detector names.

- sep:

  Separator between entries.

## Value

A single character string, safe to pass to
[`shiny::HTML()`](https://rdrr.io/pkg/shiny/man/reexports.html).

## Details

Exported for the same reason as
[`episodic_ui_italicise_taxon()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md):
the Quarto report template needs it available in its own fresh session.

## Examples

``` r
episodic_ui_code_join(c("farrington", "same_place"))
#> [1] "<code>farrington</code>, <code>same_place</code>"
```
