# Translate a key, with placeholder substitution

Translate a key, with placeholder substitution

## Usage

``` r
episodic_tr(key, ..., lang = "nl", instance_i18n = NULL)
```

## Arguments

- key:

  A dotted i18n key, e.g. `"rail.title"`.

- ...:

  Named placeholder values substituted into `{name}` tokens in the
  template.

- lang:

  Session language, `"nl"` (default) or `"en"`.

- instance_i18n:

  An optional named character vector of operator overrides (dotted key
  -\> template), checked before the shipped translation files. `NULL`
  (the default) means no override.

## Value

A single character string, with placeholders substituted.

## Examples

``` r
episodic_tr("nav.clusters", lang = "nl")
#> [1] "Clusters"
episodic_tr("nav.clusters", lang = "en")
#> [1] "Clusters"
```
