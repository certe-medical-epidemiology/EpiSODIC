# How the dossier's narrative summary is written

Every cluster dossier includes a short, plain-language narrative
summarising what the numbers mean - e.g. how large the cluster is
relative to baseline, how concentrated it is at one institution, and
what to consider next. This text is not written by an LLM: it is
assembled deterministically from a fixed library of pre-written sentence
templates ("fragments"), each triggered by a specific condition on the
cluster's data. The same inputs always produce the same wording, and
every fragment is translated into every shipped language (English,
Arabic, Dutch, French, German, Hindi, Mandarin Chinese, and Spanish).

## Usage

``` r
episodic_interpretation_slots
```

## Details

The narrative is built up section by section, in this fixed order:
`episodic_interpretation_slots` lists them - magnitude, curve shape,
concentration, testing volume, demography, data completeness, and
finally a recommendation. Within a section, the first applicable
fragment is used; a section with nothing applicable (for example, the
testing-volume section when no positivity data was supplied) is simply
omitted rather than filled with a placeholder sentence.

## Examples

``` r
episodic_interpretation_slots
#> [1] "magnitude"      "curve_shape"    "concentration"  "denominator"   
#> [5] "demography"     "completeness"   "recommendation"
```
