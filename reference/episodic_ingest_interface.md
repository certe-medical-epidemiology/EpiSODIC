# Connect your own laboratory data

EpiSODIC does not connect to any laboratory or hospital system itself.
Instead, you write a small R function - an "ingestion source" - that
returns your own positive-result data as a plain data frame in the shape
described here, and pass that function to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md).
This keeps EpiSODIC decoupled from any specific laboratory information
system: whatever your source system is, your function is the only place
that needs to know about it.

## Usage

``` r
episodic_ingest_columns
```

## Details

`episodic_ingest_columns` lists the required columns, in order:

- `source_key`:

  A unique identifier for this result, stable across repeated runs (used
  to detect duplicates).

- `patient_key`:

  A pseudonymised patient identifier, consistent for the same patient
  across results.

- `sample_date`, `receipt_date`:

  Dates the sample was taken and received by the laboratory.

- `pathogen`:

  The pathogen name, exactly as your laboratory reports it (e.g.
  `"Escherichia coli"`, `"Influenza A"`). Used verbatim - never matched
  against a taxonomy - since detection must work for any pathogen a lab
  can report, viral or not. The same isolate may appear under more than
  one `pathogen` value if that is epidemiologically useful (e.g. an ETEC
  isolate reported as both `"Escherichia coli"` and `"ETEC"`, so each is
  monitored separately).

- `care_line`:

  Typically `"hospital"` or `"primary_care"`.

- `institution_key`, `institution_display_name`, `institution_type`:

  The reporting institution's identifier, display name, and type.

- `municipality`, `ward`, `specialism`, `pc`:

  Further location and clinical context, where available.

- `sex`, `age`:

  Patient demographics, where available.

Only confirmed-positive results belong in this feed - there is no
outcome column, so do not include negative results here. If you also
want a denominator (tests performed, for a positivity rate), supply that
separately as pre-aggregated counts; see `R/ingest_denominator.R`.

The only ingestion source shipped with the package is the synthetic
generator
([`episodic_ingest_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_source_synthetic.md))
used for the bundled demo - a useful template to base your own on.

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
