# The interpretation fragment engine

No LLM at runtime - deterministic and testable by construction. An
interpretation (the Dutch UI has its own term for this, see
`inst/i18n/nl.json`'s `panel.interpretation.title`) is assembled from a
fixed library of fragments: each fragment has an id, a condition
function over the cluster object, a narrative slot, and a template (an
i18n key with placeholders). Deterministic, testable, translatable.

## Usage

``` r
episode_interpretation_slots
```

## Details

Slots fire in a fixed order: magnitude, concentration, denominator,
demography, completeness, recommendation. Within a slot, fragments are
tried in the order they are registered and the first whose condition
matches is used; a slot with no matching fragment (for example
`denominator` when no positivity metadata was supplied) is silently
skipped rather than padded with a placeholder sentence.

The **cluster object** every condition and template renders against is a
plain list; see `R/app_read.R`'s `episode_cluster_object()` for exactly
which fields it carries and where each one comes from.
