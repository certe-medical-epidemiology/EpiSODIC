# Ingestion interface

Defines the contract that any raw case data source must satisfy before
`episodic_ingest_run()` (see `R/ingest_pipeline.R`) can turn it into
rows of `episodic_case`. This is the entire boundary between EpiSODIC
and whatever laboratory or hospital system an operator runs: EpiSODIC
never calls any data source itself (see `README.md`'s data format
section) - the operator's own cron script extracts and transforms into
exactly this shape, then calls
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
with a function that returns it. The only implementation shipped in this
package is the synthetic generator (`R/ingest_synthetic.R`), used for
the bundled demo.

## Usage

``` r
episodic_ingest_columns
```

## Format

An object of class `character` of length 15.

## Details

**Mandatory, drives all detection: one row per confirmed-positive
isolate/result.** `pathogen` is a raw, lab-provided string, used
verbatim - never resolved against
[`AMR::as.mo()`](https://amr-for-r.org/reference/as.mo.html) or any
other taxonomy, since `AMR` only covers non-viral organisms and this
system must detect clusters of anything a lab reports. The same
underlying isolate can legitimately appear more than once under
different `pathogen` values when that is epidemiologically useful (an
ETEC isolate as both `"Escherichia coli"` and `"ETEC"`, so each is
watched on its own), and it is entirely the operator's transform step
that decides this, not EpiSODIC.

**No `positive`/test-outcome column, and no raw negatives.** Positivity
is handled separately as small, optional, pre-aggregated metadata (see
`R/ingest_denominator.R`); the mandatory feed here is positives only.

## Examples

``` r
episodic_ingest_columns
#>  [1] "source_key"               "patient_key"             
#>  [3] "sample_date"              "receipt_date"            
#>  [5] "pathogen"                 "care_line"               
#>  [7] "institution_key"          "institution_display_name"
#>  [9] "institution_type"         "municipality"            
#> [11] "ward"                     "specialism"              
#> [13] "pc"                       "sex"                     
#> [15] "age"                     
```
