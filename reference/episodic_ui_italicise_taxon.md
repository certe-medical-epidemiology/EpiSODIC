# Format Text for Outbreak Reports and the Dashboard

Small HTML formatting helpers used when building dossier text, both in
the dashboard and in the Quarto outbreak report template. Both are
exported so that a custom report template of your own (set via
`EPISODIC_QUARTO_REPORT`) can use the same formatting as the shipped
one. Input text is always HTML-escaped first, so the result is safe to
pass on to
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

## Usage

``` r
episodic_ui_italicise_taxon(pathogen)

episodic_ui_code_join(detectors, sep = ", ")
```

## Arguments

- pathogen:

  A character vector of pathogen display names.

- detectors:

  A character vector of detector names.

- sep:

  Separator between entries.

## Value

A character vector (or, for `episodic_ui_code_join()`, a single string),
safe to pass to
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

## Details

`episodic_ui_italicise_taxon()` italicises pathogen names that `AMR`
recognises as a taxonomic binomial (e.g. *Escherichia coli*), following
standard microbiological convention. Names it does not recognise (e.g.
"Influenza A", a virus type rather than a species) are left as-is.

`episodic_ui_code_join()` renders a vector of detector names (e.g.
`"farrington"`, `"same_place"`) as inline `<code>`, joined into one
string - useful when listing which detectors flagged a cluster.

## Examples

``` r
episodic_ui_italicise_taxon(c("Escherichia coli", "Influenza A"))
#> [1] "<i>Escherichia coli</i>" "Influenza A"            
episodic_ui_code_join(c("farrington", "same_place"))
#> [1] "<code>farrington</code>, <code>same_place</code>"
```
