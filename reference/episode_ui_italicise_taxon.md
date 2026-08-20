# Italicise pathogen names `AMR` recognises as a taxonomic binomial

Wraps names found in `AMR::microorganisms$fullname` in `<i>...</i>`, for
display via
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)
(e.g. *Escherichia coli*); names `AMR` does not recognise as a species
(e.g. "Influenza A", a virus type rather than a binomial) pass through
unitalicised, exactly as intended - `pathogen` is deliberately
unconstrained free text and never resolved against
[`AMR::as.mo()`](https://amr-for-r.org/reference/as.mo.html) for
*detection* purposes, so viruses and other non-taxonomic values are
never excluded there; `AMR` is nonetheless a hard dependency of the
package as a whole, used here and by `episode_dedup()`. Text is
HTML-escaped before any tag is added, so this is always safe to pass to
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

## Usage

``` r
episode_ui_italicise_taxon(pathogen)
```

## Arguments

- pathogen:

  A character vector of pathogen display names.

## Value

A character vector, safe to pass to
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

## Details

Exported (not just internal): the Quarto report template
(`inst/report/cluster_report.qmd`) runs in its own fresh session where
only exported functions are attached by
[`library(EpiSODIC)`](https://github.com/certe-medical-epidemiology/EpiSODIC),
and an operator's own custom template (`EPISODE_QUARTO_REPORT`) should
have the same formatting helpers the shipped one uses.

## Examples

``` r
episode_ui_italicise_taxon(c("Escherichia coli", "Influenza A"))
#> [1] "<i>Escherichia coli</i>" "Influenza A"            
```
