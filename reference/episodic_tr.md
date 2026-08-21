# Translate a dashboard text key

Looks up a piece of dashboard text by its key and language, substituting
any `{placeholder}` tokens in the template. Mostly useful if you are
writing your own Quarto outbreak report template
(`EPISODIC_QUARTO_REPORT`) and want it to read the same wording, in the
same language, as the dashboard itself.

## Usage

``` r
episodic_tr(key, ..., lang = "nl", instance_i18n = NULL)
```

## Arguments

- key:

  A dotted key identifying the piece of text, e.g. `"nav.clusters"`. The
  full set of available keys and their Dutch and English wording lives
  in `inst/i18n/nl.json` and `inst/i18n/en.json`.

- ...:

  Named values substituted into `{name}` placeholders in the template.

- lang:

  Language: `"nl"` (default) or `"en"`.

- instance_i18n:

  An optional named character vector of your own wording overrides (key
  -\> template), checked before the shipped translations. `NULL` (the
  default) uses only the shipped text.

## Value

A single character string.

## Examples

``` r
episodic_tr("nav.clusters", lang = "nl")
#> [1] "Clusters"
episodic_tr("nav.clusters", lang = "en")
#> [1] "Clusters"
```
