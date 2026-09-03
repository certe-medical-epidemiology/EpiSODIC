# Getting your data in

EpiSODIC never queries a laboratory information system, data warehouse,
or any other data source itself. That is deliberately your own step, run
before EpiSODIC: extract from wherever your data lives, transform into
the shape below, then call
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
with the result as a plain data frame (a tibble works just as well).
This keeps the engine reusable by any laboratory.

``` r

cases <- my_extract_and_transform_function()

episodic_check_cases(cases)  # what is wrong with it, before anything runs

episodic_run_cron(
  cases = cases,
  db_path = "/path/to/episodic.sqlite",
  denominators = NULL  # optional, see "Positivity metadata" below
)
```

A data set is the normal case, and what these arguments are written for.
If producing the data only makes sense at run time (e.g. a live database
query), a zero-argument function returning one is accepted just as
well - EpiSODIC resolves either
([`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md)).

**Extract a recent window, not your full history.** A scheduled extract
should cover only the last few weeks, with some overlap (two weeks is a
reasonable default) as a safety margin against a missed or delayed run -
not a re-pull of every positive result you have ever had. This keeps
every scheduled run fast and light, however large your archive grows. It
is safe for the same reason re-running a whole extract by hand is safe:
deduplication checks each incoming result against what is already stored
for that patient and pathogen, so results that reappear inside your
overlap window - or that continue an episode whose first result is not
in this batch at all - are still recognised correctly, never
double-counted.

## Cases (mandatory)

One row per confirmed-positive laboratory result. This is the complete,
allow-listed column set (`episodic_case_columns`); a data set with any
column outside this list, or missing one from it, is rejected. Run
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
on your extract to check all of this - columns, allowed values, dates,
and `source_key` uniqueness - before you schedule anything (see “Check
your data first” below). The three fixed value sets are available as
`episodic_care_lines`, `episodic_institution_types` and
`episodic_sex_codes`, so your transform step can map onto them directly:

| Column | Type and allowed values | Meaning |
|----|----|----|
| `source_key` | character, required, unique | A unique identifier for the *row*, not the specimen - so re-running the same extract later cannot create duplicate cases. A bare lab accession number often is not unique per row by itself (see “One culture, more than one reported result” below); where that is true of your data, build `source_key` yourself from whatever combination of your own columns is unique per row. |
| `lab_number` | character, required | Your laboratory’s own specimen or culture number. Unlike `source_key` it is not required to be unique - two rows legitimately share one `lab_number` when a single culture produced more than one reported result. Shown alongside `patient_key` on the line list, since it is not itself a patient identifier. |
| `patient_key` | character, required | A stable, pseudonymised patient identifier. This is what deduplication and episode grouping key on: without it, EpiSODIC cannot tell that two positives belong to the same patient, and every positive would be treated as its own case. The pseudonym is shown on the line list, same as `lab_number`; do not pass through a real identifier (BSN, hospital number) as this column. |
| `sample_date` | `Date` or `"YYYY-MM-DD"`, required | The anchor date every detector, trend, and report is built against. If your system falls back to a receipt date when sample date is unfilled, that fallback should already have happened before this row reaches EpiSODIC. |
| `receipt_date` | `Date` or `"YYYY-MM-DD"`, `NA` allowed | When the result was received. Stored for provenance/audit, kept separate from `sample_date` - EpiSODIC deliberately does not use it to measure reporting delay, since a lab’s own receipt-date field can itself silently be a stand-in for a missing sample date; reporting completeness is instead measured empirically, from how a stream’s case counts change across successive detection runs. |
| `pathogen` | character, required, free text | The pathogen as your lab reports it, as free text. **Not** resolved against any taxonomy, since EpiSODIC has to detect clusters of anything a lab reports. The same underlying positive can appear more than once under different `pathogen` values when that is epidemiologically useful - an ETEC positive reported as both `"Escherichia coli"` and `"ETEC"`, so each is watched on its own. This is your transform step’s decision, not EpiSODIC’s. |
| `care_line` | `first`, `second`, `third`, `other`, `unknown` - `NA` allowed | Which part of the health system the case came from: `first` is primary care, `second` secondary care, `third` tertiary care. `NA` is read as `unknown` and stored that way - an empty value, an R `NA` and a database `NULL` all mean the same thing here. |
| `institution_key` | character, required | A stable identifier for the institution, hashed internally so a later rename does not fracture history. |
| `institution_display_name` | character, required | The human-readable name shown in the interface. |
| `institution_type` | `hospital`, `ltc_institution`, `gp_municipality`, `ooh_service`, `other` - required | How the institution is handled downstream; see the list below the table. |
| `municipality` | character, `NA` allowed | The institution’s municipality, used for `gp_municipality`-type rows (see below) and as a coarse geographic fallback. |
| `ward` | character, `NA` allowed | Only meaningful (and only used) for hospitals: this is what the `same_place` detector watches at ward level. |
| `specialism` | character, `NA` allowed | The treating specialism, shown on the line list for context; not itself a detection dimension. |
| `pc` | character, `NA` allowed | The patient’s postcode (or equivalent - see “Geographic reference data” below for how coarse or fine this can be). Drives the geography panel and the choropleth, and the concentration measure (“how localised is this cluster”) that feeds the priority score. |
| `sex` | `M`, `F`, `U` - `NA` allowed | The patient’s sex. Feeds the demography panel’s age/sex pyramid, one of the interpretation engine’s own evidence dimensions (a cluster’s demography shifting from an organisation’s usual baseline is itself a signal worth surfacing). |
| `age` | integer (whole years), `NA` allowed | The patient’s age at sample date. Same role as `sex`: demography panel and interpretation, not a detection input. |

`institution_type` values, beyond the self-explanatory `hospital`:

- `ltc_institution` — a long-term care institution (nursing home,
  residential care), kept as a first-class institution like a hospital.
- `gp_municipality` — a general practice. Stored as the municipality the
  practice is in, not the practice’s own identity: a single-handed GP
  practice is not a transmission unit, and its name adds identifiability
  without adding epidemiological information.
- `ooh_service` — an out-of-hours GP service (*huisartsenpost*), kept as
  a first-class institution like a hospital.
- `other` — anything that does not fit the above; institution identity
  is dropped (stored as `NULL`) for this category.

### `source_key`: one identifier per *result*, not per specimen

`source_key` exists purely so a re-run of the same extract cannot create
a duplicate case - it is never shown anywhere in the dashboard. A bare
lab accession number is often *not* enough to satisfy this by itself:
the same culture can legitimately produce more than one reported result
(a multiplex panel positive for two organisms, two isolates of the same
species with different antibiograms). When that is true of your source
data, build `source_key` in your own extract step from whatever
combination of columns is unique per row. Many laboratory systems need
all four of these to pin down one row:

``` r

cases$source_key <- paste(
  cases$lab_number,     # the culture/specimen this result came from
  cases$patient_key,    # who it is
  cases$test_code,      # which panel/test produced this result
  cases$isolate_number  # which isolate, if more than one was speciated
)
```

Only `lab_number` and `patient_key` need to reach EpiSODIC as their own
columns (see the table above); a test code or isolate number that exists
only in your source system is fine to fold into `source_key` and drop
otherwise - `source_key` is an internal deduplication key, not something
that needs to decompose back into its parts later.

### One culture, more than one reported result

Two isolates from *one* culture, reported separately (e.g. two *E. coli*
isolates with different antibiograms), collapse to one case if they
share `patient_key`, `pathogen` and a `sample_date` within one episode
of each other - **by design**. For counting cases and detecting
clusters, one patient’s one positive culture on one day is one
epidemiological event, however many isolates or antibiogram rows your
lab reported for it; counting it twice would inflate every downstream
signal - the epi curve, the Farrington baseline, the priority score, all
of it.

`lab_number` still ties both rows back to the same culture even though
only one survives deduplication as “the case” (and is now shown on the
line list, so an epidemiologist can look the rest up in the source
system if needed). Antibiogram-level detail - which isolate had which
resistance pattern - is not itself part of the case data contract, and
is a separate concern from whether EpiSODIC counts one case or two:
nothing in the detection or reconciliation algorithms needs it, since
they work from case counts, not resistance phenotypes. If per-isolate
resistance data ever becomes something the dossier should show
(e.g. “this cluster includes an ESBL-positive isolate”), that is a
future, separate addition to the schema, not something to force into how
cases are counted today.

### Deduplication is EpiSODIC’s job, not yours

EpiSODIC collapses positives for the same patient and pathogen into one
case per episode, using the episode length configured per pathogen in
`inst/config/pathogen_config.csv` - and it does this correctly across
runs, not just within one extract: each run checks incoming positives
against the most recent episode already on file for that
patient/pathogen, so a recurring extract only has to cover a recent
window (with a couple of weeks of overlap, so nothing slips through if a
run is ever missed) rather than a patient’s full history every time.
Re-sending a row EpiSODIC has already seen (same `source_key`) is always
safe regardless - it is simply a no-op.

**Do not send negative results here.** This feed drives every detector;
it is deliberately positives-only so an operator never has to ship a
multi-year, per-test linelist just to run detection.

## Check your data first

Before you schedule anything, run
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
on your extract. It needs no database, changes nothing, and reports
everything it finds in one go - which column is wrong, how many rows are
affected, which rows those are, what the offending values look like, and
what to do about each:

``` r

cases <- my_extract_and_transform_function()

episodic_check_cases(cases)
#> -- EpiSODIC case data check ------------------------------------------
#>    4,312 rows, 16 columns
#>    sample_date from 2024-01-02 to 2024-06-30
#>    12 pathogens, 7 institutions, 3,981 patients
#>
#> x 2 problems - a detection run refuses to start until these are fixed:
#>
#>   1. `sample_date` has 4312 of 4312 rows that are not a Date and do
#>      not read as YYYY-MM-DD.
#>      values: 02-01-2024, 03-01-2024, 04-01-2024 (and 178 more)
#>      rows:   1, 2, 3, 4, 5 (and 4307 more)
#>      fix:    These look day-first (e.g. 31-12-2025): convert with
#>              as.Date(x, format = "%d-%m-%Y"). EpiSODIC accepts a Date
#>              column, or text in ISO 8601 YYYY-MM-DD form, and nothing
#>              else [...]
#>
#>   2. `sex` has 2106 of 4312 rows with a value outside the allowed set
#>      (M, F, U, or NA).
#>      values: male, female
#>      [...]
#>
#> ! 2 things worth a look - a run proceeds regardless:
#>
#>   1. 1 pathogen name(s) appear in more than one spelling, differing
#>      only in capitalisation or spacing: "Influenza A" / "influenza a".
#>      [...]
```

It separates two kinds of finding on purpose. **Problems** are things
EpiSODIC cannot work around, and a run refuses to start while any of
them stand. **Advice** covers what is allowed but rarely intended and
quietly costs you signal: one pathogen spelled two ways (two streams
instead of one), no `ward` on any hospital row (nothing for ward-level
detection to group on), a `patient_key` that never repeats (nothing for
deduplication to do), postcodes the map cannot place, sample dates in
the future.

[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
runs the same checks and throws instead, for use in a script.
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
runs it for you before every run: data it cannot use stops the run with
the same message, before anything is written, and that message is
recorded against the run so it is also visible in the dashboard’s status
strip and activity screen. A failed run never passes in silence.

The optional feeds below have the same non-throwing check, over their
own contract:
[`episodic_check_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_denominators.md)
for positivity metadata,
[`episodic_check_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_institution_activity.md)
for institution activity. Both read the same way as
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
does - no database, nothing changed, problems versus advice - and
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
validates both the same way it validates `cases`, before writing
anything, whenever they are supplied as a plain data frame (a
zero-argument function is instead resolved and validated once the run is
already under way, since it may depend on data - e.g. the institutions
table - that only exists once the case feed has loaded).

## Positivity metadata (optional)

If, and only if, you can produce it: a small, pre-aggregated table of
test counts, used purely as an interpretive aid on the dossier (a rising
case count with flat test volume is a much weaker signal than the same
rise with stable positivity) and never as a detection input.

| Column | Meaning |
|----|----|
| `pathogen` | Matches the cases feed. |
| `sample_date` | A period start (e.g. week start); this is aggregate data, not per-test. |
| `care_line` | `first`, `second`, `third`, `other`, or `unknown`. |
| `area_code` | Optional geographic stratum. |
| `n_tests` | Total tests run for this pathogen/period/stratum, positive and negative. |

This is realistic to produce for a multiplex PCR panel testing a fixed
list of targets (your LIS can report “we ran 40 GI panels this week”
trivially). It is not meaningful for open-ended culture results, where
there is no closed list of things a negative result could have been - if
that is your situation, simply leave `denominators` at `NULL`, and
positivity panels stay blank for your streams. See
[`episodic_synthetic_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_denominators.md)
for a worked example.

## Institution activity (optional)

If, and only if, you can produce it: weekly patient-days per hospital,
used to normalise L2 (institution-level) Farrington detection by
occupancy rather than raw counts - a busy February and a quiet August at
identical transmission-per-patient-day should not read as different
signal strengths. Also optional; without it, L1/L2 detection uses raw
counts exactly as it always has.

| Column | Meaning |
|----|----|
| `institution_key` | Matches the cases feed. |
| `period_start`, `period_end` | The activity period this row covers (typically a week). |
| `patient_days` | Total patient-days across the institution for that period. |

Rows whose `institution_key` does not match a known institution are
skipped, not an error - an activity feed and a case feed need not be
perfectly synchronised. Skipped rows are never silent, though: the load
warns and names the unmatched keys, the count is recorded against the
run, and the run finishes `partial` rather than `success` (see “What a
run reports” below). See
[`episodic_synthetic_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_institution_activity.md)
for a worked example. There is no ward-level (L1) equivalent in the
schema, so L1 detection is never normalised, only L2.

## Geographic reference data (optional)

EpiSODIC is not built around any one country, coding system, or
administrative structure. Everywhere geography enters the picture - the
`pc` column on your case data (see the table above), the L3/L4 area and
province lattice levels, and the dossier’s map - it is a column your own
extract step fills in and a lookup you supply, never something hardcoded
to a specific jurisdiction. The Dutch postcode reference data described
below is a single bundled *example*, shipped so the demo and a first
trial run show a real map without any setup, not a sign that this
package assumes a Dutch deployment. Two independent things are
configurable, and either, both, or neither can be set - the rest of the
dashboard works the same regardless.

### The choropleth map

The dossier’s geography panel shows a choropleth when both the `sf`
package and a geographic reference dataset are available; otherwise it
falls back to a plain bar breakdown by PC value, exactly as if this
feature did not exist. EpiSODIC ships a Dutch postcode default
(`inst/extdata/geo_postcodes4_nl.rds`, geometry only, sourced from
`certegis` under the same GPL-2 licence - see
`data-raw/ geo_postcodes4_nl.R` for provenance) purely as a working
example: point `EPISODIC_GEO_DATA` at your own `.rds` file holding an
[`sf`](https://r-spatial.github.io/sf/) object with a `pc` column
(matching whatever your own `episodic_case.pc` values are - postcodes,
zip codes, municipality codes, census tracts, anything with a shape) and
a `geometry` column, and it is used instead of the shipped default. See
`R/geo_data.R` for the exact contract.

A second, independent layer can be drawn on top for orientation - region
outlines (provinces, counties, states, whatever is useful), colour but
no fill, a thicker line than the choropleth itself. Point
`EPISODIC_GEO_DATA_OVERLAY` at an `.rds` file holding an `sf` object
with just a `geometry` column (no `pc` join needed, since it carries no
case counts of its own). No default is shipped for this one - unlike the
postcode default above, region boundaries are too jurisdiction-specific
to guess a sensible default for.

### L4 (province-level) detection

Separate from the map, the lattice’s L4 level watches for clusters at
province/state/region scale, and it needs its own lookup from `pc` to
that coarser unit - a choropleth `sf` file already has geometry for
every postcode, but not which province each one belongs to. Point
`EPISODIC_PC_PROVINCE_MAP` at a CSV with `pc` (matching your case data’s
`pc` values exactly, not a prefix) and `province_code` columns. Left
unset, this defaults to the ranges the Dutch demo data happens to use,
which - being specific to that data - will not match postcodes from
anywhere else: outside that one region, L4 simply never has anything to
detect on until you supply your own mapping. L1-L3 and L5 do not depend
on this at all. See `episodic_pc_to_province()` for the exact contract.

Unset and unusable are two different things. Once the variable *is* set,
a file that does not exist, cannot be read as a CSV, holds no rows, has
no `pc` or no `province_code` column, or repeats a `pc` is a
configuration error: the run stops and `error_text` names the file and
which of those it is. It does not fall back to the demo ranges - doing
so would hand an operator who supplied their own mapping a lattice built
on somebody else’s provinces, with nothing anywhere to say why.

The run also says so in its trace when the mapping loads but places no
case at all, which is almost always a formatting mismatch: `"9713"` in
the CSV against `"9713 AB"` in the case data, or the reverse. The same
mapping is what puts a province name beside each postcode in the
dashboard’s geography panel, so a mapping that is not being used is
visible there too.

The dashboard’s **Info** screen answers the same question directly, for
this and every other `EPISODIC_*` file: its “Reference data” panel says
whether each one was read, was rejected (and why), or was read fine, and
what it put into the app - for the postcode mapping, how many postcodes
it holds, how many provinces they resolve to, and how many of the
distinct postcodes in your own case data it actually covers. That last
number is the one that separates a mapping that was never read from one
whose postcode format does not match yours. The resolved file paths on
that panel are shown only to a signed-in user.

## What a run reports

Every
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
call records what each feed actually delivered, so “did last night’s
extract arrive in full?” is answerable without re-running anything.
`episodic_detection_run` carries `n_cases_supplied`,
`n_cases_deduplicated`, `n_cases_inserted`, `n_denominators_written`,
`n_activity_supplied`, `n_activity_written` and `n_activity_skipped`
alongside the detection counts, and the dashboard’s Activity screen
shows them under each run.

The run’s `status` distinguishes three outcomes:

| Status | Meaning |
|----|----|
| `success` | Everything supplied was loaded. |
| `partial` | The run completed and its detections are valid, but rows of an optional feed were skipped - today that means institution activity whose `institution_key` matched no known institution. |
| `failed` | Nothing was written. The whole run is one transaction, so a failure leaves no partial state and is always safe to retry; `error_text` holds the reason. |

The distinction that matters: **structural problems fail the run, and
row-level facts are counted and reported, but never silently dropped.**
A malformed feed - a missing column, a value outside the allowed set, a
date that does not parse, a duplicate `source_key` - stops the run
before anything is written, and says which column and which values were
wrong. A run that quietly loaded 10% of your patient-days is the one
outcome the design refuses to report as green.

Both `success` and `partial` are usable runs, and the dashboard reads
from the most recent of either.

## See also

- [`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)
  for scheduling
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
  the database backend, accounts, and custom report templates.
- [`vignette("environment-variables")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.md)
  for `EPISODIC_GEO_DATA`, `EPISODIC_GEO_DATA_OVERLAY` and every other
  `EPISODIC_*` variable mentioned above.
- [`vignette("faq")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/faq.md)
  for combining more than one source system, resending data safely, and
  handling a rectified/corrected lab result.
