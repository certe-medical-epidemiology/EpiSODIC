# Connect your own laboratory data

EpiSODIC does not connect to any laboratory or hospital system itself.
Instead, you extract and transform your own positive-result data
beforehand - with whatever tooling you already use - and hand the result
to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
as a plain data frame (a `tibble` is equally fine) in the shape
described here. This keeps EpiSODIC decoupled from any specific
laboratory information system: whatever your source system is, your own
extract step is the only place that needs to know about it.

## Usage

``` r
episodic_case_columns

episodic_care_lines

episodic_institution_types

episodic_sex_codes
```

## Details

A data set is the normal case, and the one to reach for. If producing
the data only makes sense at run time - a live database query, for
instance - you can pass a zero-argument function returning such a data
set instead; EpiSODIC accepts either (see
[`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md)).

Only confirmed-positive results belong here - there is no outcome
column, so do not include negative results. If you also want a
denominator (tests performed, for a positivity rate), supply that
separately as pre-aggregated counts; see
[`episodic_synthetic_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_denominators.md).

## Required columns

`episodic_case_columns` lists all fifteen, in order. The set is an
explicit allow-list: a column outside it, or one missing from it, is
rejected by
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
rather than silently ignored. "Required" means the column must be
present; several may be `NA` throughout if your laboratory does not
record them.

Three columns accept only a fixed set of values, given as
`episodic_care_lines`, `episodic_institution_types` and
`episodic_sex_codes` so you can map onto them in your extract step
rather than copying the strings out of this page.

- `source_key`:

  Character, required, no `NA`, unique within the data set. A stable
  identifier for this result in your own source system, so re-running
  the same extract later cannot create duplicate cases. Any string will
  do, as long as the same result always carries the same one.

- `patient_key`:

  Character, required, no `NA`. A pseudonymised patient identifier, the
  same for the same patient across results. Deduplication and episode
  grouping key on it: without it every isolate becomes its own case.
  Never shown as-is in the dashboard, but do pseudonymise it before it
  reaches EpiSODIC - a BSN or hospital number must not be passed
  through.

- `sample_date`:

  Date, or character in `YYYY-MM-DD` (ISO 8601) form. Required, no `NA`.
  The date the sample was taken, and the anchor every detector, trend
  and report is built against. If your system falls back to a receipt
  date when the sample date is unfilled, apply that fallback in your own
  extract step, not here.

- `receipt_date`:

  Date, or character in `YYYY-MM-DD` form; `NA` allowed. The date the
  laboratory received the sample. Stored for provenance only -
  deliberately never used to measure reporting delay, since a lab's
  receipt-date field can itself be standing in for a missing sample
  date.

- `pathogen`:

  Character, required, no `NA`. The pathogen name exactly as your
  laboratory reports it (e.g. `"Escherichia coli"`, `"Influenza A"`,
  `"Norovirus"`). Free text, deliberately unconstrained: used verbatim
  and never matched against a taxonomy, since detection has to work for
  anything a lab can report, viral or not. Spelling must be stable
  across runs - `"Influenza A"` and `"influenza a"` are two different
  pathogens as far as detection is concerned. Names matching
  `inst/config/pathogen_config.csv` pick up that pathogen's episode
  length, incubation window and serial interval; anything else falls
  back to the schema defaults (30-day episode, 14 case-free days, no Rt,
  no MEM). The same isolate may appear under more than one `pathogen`
  value where that is epidemiologically useful - an ETEC isolate
  reported as both `"Escherichia coli"` and `"ETEC"`, so each is
  monitored separately.

- `care_line`:

  Character; `NA` allowed. One of `"first"` (primary care), `"second"`
  (secondary care), `"other"`, or `"unknown"` - the values in
  `episodic_care_lines`. Anything else is rejected. `NA` is read as
  `"unknown"` and stored that way, so you need not map missing values
  yourself: an empty `care_line`, an R `NA` and a database `NULL` all
  mean the same thing here, and the dashboard shows all three as
  "unknown".

- `institution_key`:

  Character, required, no `NA`. A stable identifier for the reporting
  institution. Hashed (SHA-1) on the way in, so a later rename does not
  fracture the institution's history, and so the raw key never reaches
  the database.

- `institution_display_name`:

  Character, required, no `NA`. The human-readable institution name
  shown in the dashboard.

- `institution_type`:

  Character, required. Exactly one of `"hospital"`, `"ltc_institution"`
  (long-term care), `"gp_municipality"`, `"ooh_service"` (out-of-hours
  service), or `"other"` - the values in `episodic_institution_types`.
  Anything else is rejected. This decides how the institution is
  handled: `"hospital"` institutions are monitored as first-class
  entities and are the only ones eligible for patient-day normalisation,
  while a `"gp_municipality"` is collapsed to its municipality.

- `municipality`:

  Character; `NA` allowed. The institution's municipality. Required in
  practice for `"gp_municipality"` rows, since that is what they are
  collapsed to.

- `ward`:

  Character; `NA` allowed. The ward within the institution. Only
  meaningful for `"hospital"` and `"ltc_institution"` rows; leave `NA`
  otherwise. This is the unit L1 (ward-level) detection watches, so an
  inconsistent spelling splits one ward into two streams.

- `specialism`:

  Character; `NA` allowed. The requesting clinical specialism. Context
  for the assessor, not a detection input.

- `pc`:

  Character; `NA` allowed. The *patient's* postcode area, not the
  institution's - it is what the geography panel and area-level (L3)
  detection use. Four digits as a string for the shipped Netherlands
  reference data (`"9713"`, leading zeros preserved, so store it as
  character and not as a number). With your own `EPISODIC_GEO_DATA`, it
  must match that file's `pc` column instead.

- `sex`:

  Character; `NA` allowed. Exactly one of `"M"`, `"F"`, or `"U"`
  (unknown) when present - the values in `episodic_sex_codes`. Anything
  else is rejected, so map your own coding (`1`/`2`,
  `"male"`/`"female"`) in your extract step.

- `age`:

  Integer; `NA` allowed. Age in whole years at sampling. Not age group -
  the dashboard bands it itself.

The only case data shipped with the package is what the synthetic
generator
([`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md))
returns for the bundled demo - a useful template for the shape your own
data should have.

## Examples

``` r
episodic_case_columns
#>  [1] "source_key"               "patient_key"             
#>  [3] "sample_date"              "receipt_date"            
#>  [5] "pathogen"                 "care_line"               
#>  [7] "institution_key"          "institution_display_name"
#>  [9] "institution_type"         "municipality"            
#> [11] "ward"                     "specialism"              
#> [13] "pc"                       "sex"                     
#> [15] "age"                     
episodic_care_lines
#> [1] "first"   "second"  "other"   "unknown"
episodic_institution_types
#> [1] "hospital"        "ltc_institution" "gp_municipality" "ooh_service"    
#> [5] "other"          
```
