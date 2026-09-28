# Generate Synthetic Outbreak Data

Produces several years of laboratory surveillance data for a fictional
northern-Netherlands region - eight hospitals, twenty long-term care
institutions and the region's municipalities - and injects six outbreaks
for the detectors to find. This is what powers
[`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
and the package's test suite, and it doubles as a worked example of the
shape your own data should have (see
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)).

## Usage

``` r
episodic_synthetic_cases(
  start_date = end_date - 5 * 365,
  end_date = Sys.Date(),
  seed = 1,
  outbreaks = TRUE,
  outbreak_offsets = NULL
)
```

## Arguments

- start_date:

  First sample date to generate. Defaults to five years before
  `end_date` - Farrington needs four of them before it will compare
  anything against anything.

- end_date:

  Last sample date to generate. Defaults to today, so a demo built from
  this always shows current surveillance rather than whatever year the
  package was released in.

- seed:

  RNG seed, for reproducible demo data.

- outbreaks:

  Which outbreaks to inject: `TRUE` for all six (the default), `FALSE`
  for none, or a character vector of outbreak identifiers (`"RARE"`,
  `"WARD"`, `"LTC"`, `"PS"`, `"PROP"`, `"WAVE"`). A history with none
  injected is what a negative control needs: every alarm raised on it is
  a false one, which is the only clean way to measure an alarm rate.
  Each outbreak draws from the random stream whether or not it is kept,
  so a given seed always produces the same cases for a given outbreak
  regardless of which others were asked for.

- outbreak_offsets:

  Days to move outbreaks further back from `end_date`. All six are
  anchored to `end_date`, so by default they sit in the last few months
  of whatever window is generated - fine for a demo, wrong for a
  prospective evaluation, which needs them dispersed across the window
  instead. Either a single number applied to all of them, or a named
  vector giving days per outbreak identifier (any not named stays at 0).
  `NULL`, the default, leaves every outbreak anchored to `end_date`.

## Value

A data frame satisfying
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md),
carrying the ground truth of what was injected as an attribute - see
[`episodic_synthetic_ground_truth()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_ground_truth.md).

## What it puts in

A seasonal Poisson baseline for eight endemic pathogens, deliberately
thin per place: a demo whose baseline keeps tripping the rule-based
detectors by coincidence buries the outbreaks it is meant to
demonstrate. Influenza A and RSV come in short winter waves of varying
size and timing, as they do in life, so the Moving Epidemic Method has
seasons to fit its thresholds and intensity bands to; the others rise
and fall gently through the year. Patients recur - roughly one case in
six is a repeat positive from a patient already in the data - so
deduplication and episode grouping have something to do.

On top of that, six outbreaks, sized from a single case to a regional
wave, each shaped for a different detector:

|  |  |  |
|----|----|----|
| **Outbreak** | **Shape** | **Found by** |
| Invasive meningococcal disease | one case | `rare_trigger` |
| Ward cluster | 3 cases, one ward, 12 days | `same_place` |
| Nursing home outbreak | 8 cases, one institution, 9 days | `same_place` |
| Point source | 14 cases, one ward, days apart | `same_place` |
| Propagated | 15 cases, four generations, 90 days | `same_place`, Rt |
| Regional wave | 144 cases, diffuse, 6 rising weeks | Farrington, MEM |

The regional wave is deliberately spread one case to a place: no
institution sees enough of it for a rule-based detector to notice, and
only the statistical baseline comparison finds it. That contrast - a
diffuse signal no amount of local vigilance would catch - is half the
reason the statistical detectors exist.

Outbreaks are anchored to `end_date` and clipped to the window you ask
for, so a short window returns a partial one rather than cases outside
the range you asked for.

## What was injected, as data

The result carries its own ground truth: which outbreaks were injected,
where and when each of them ran, and exactly which cases belong to
which. Read it with
[`episodic_synthetic_ground_truth()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_ground_truth.md).
Injected cases also carry a `PT-OUTBREAK-*` patient key, but that is a
convenience for a human reading a line list and not an interface:
anything measuring detection against what was injected takes the ground
truth, never a pattern match on a key.

## See also

[`episodic_synthetic_ground_truth()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_ground_truth.md)
for what was injected,
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
to see what the requirements make of it, and
[`episodic_synthetic_cases_calibration()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases_calibration.md)
for many more clusters than a demo wants, to tune a configuration
against.

## Examples

``` r
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
nrow(cases)
#> [1] 421
head(cases)
#>                          patient_key sample_date receipt_date
#> 1                PT-Norovirus-000044  2025-01-01   2025-01-01
#> 2              PT-Influenza.A-000019  2025-01-01   2025-01-01
#> 3              PT-Influenza.A-000183  2025-01-01   2025-01-01
#> 4                      PT-RSV-000009  2025-01-01   2025-01-01
#> 5 PT-Clostridioides.difficile-000062  2025-01-01   2025-01-01
#> 6                PT-Norovirus-000098  2025-01-03   2025-01-03
#>                   pathogen care_line institution_key institution_display_name
#> 1                Norovirus    second         HOSP-07               Hospital G
#> 2              Influenza A    second         HOSP-01               Hospital A
#> 3              Influenza A     first           GP-15         De Fryske Marren
#> 4                      RSV     first          LTC-17           Zorgcentrum 17
#> 5 Clostridioides difficile     first           GP-14          Súdwest-Fryslân
#> 6                Norovirus    second         HOSP-05               Hospital E
#>   institution_type     municipality              ward        specialism   pc
#> 1         hospital             <NA> Internal Medicine Internal Medicine 7384
#> 2         hospital             <NA>         Neurology         Neurology 8870
#> 3  gp_municipality De Fryske Marren              <NA>              <NA> 8732
#> 4  ltc_institution             <NA>              <NA>              <NA> 9553
#> 5  gp_municipality  Súdwest-Fryslân              <NA>              <NA> 8741
#> 6         hospital             <NA>           Surgery           Surgery 8615
#>   sex age   source_key      lab_number
#> 1   F  33 SYN-00000001 LABSYN-00000001
#> 2   M  47 SYN-00000060 LABSYN-00000060
#> 3   F  38 SYN-00000061 LABSYN-00000061
#> 4   F  16 SYN-00000188 LABSYN-00000188
#> 5   M  56 SYN-00000208 LABSYN-00000208
#> 6   F  42 SYN-00000002 LABSYN-00000002
```
