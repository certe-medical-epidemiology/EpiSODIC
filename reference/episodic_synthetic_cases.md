# Generate synthetic outbreak data

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
  seed = 1
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

## Value

A data frame satisfying
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md).

## What it puts in

A seasonal Poisson baseline for eight endemic pathogens, deliberately
thin per place: a demo whose baseline keeps tripping the rule-based
detectors by coincidence buries the outbreaks it is meant to
demonstrate. Patients recur - roughly one case in six is a repeat
positive from a patient already in the data - so deduplication and
episode grouping have something to do.

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

Every injected case carries a `PT-OUTBREAK-*` patient key, so it is
always identifiable as an injected signal rather than baseline noise.
Outbreaks are anchored to `end_date` and clipped to the window you ask
for, so a short window returns a partial one rather than cases outside
the range you asked for.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
to see what the contract makes of it, and
[`episodic_synthetic_cases_calibration()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases_calibration.md)
for many more clusters than a demo wants, to tune a configuration
against.

## Examples

``` r
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
nrow(cases)
#> [1] 419
head(cases)
#>                          patient_key sample_date receipt_date
#> 1                PT-Norovirus-000044  2025-01-01   2025-01-01
#> 2              PT-Influenza.A-000105  2025-01-01   2025-01-01
#> 3                      PT-RSV-000072  2025-01-01   2025-01-01
#> 4 PT-Clostridioides.difficile-000055  2025-01-01   2025-01-02
#> 5              PT-Influenza.A-000162  2025-01-02   2025-01-02
#> 6              PT-Influenza.A-000047  2025-01-02   2025-01-02
#>                   pathogen care_line institution_key institution_display_name
#> 1                Norovirus    second         HOSP-07             Ziekenhuis G
#> 2              Influenza A    second          LTC-10           Zorgcentrum 10
#> 3                      RSV    second         HOSP-03             Ziekenhuis C
#> 4 Clostridioides difficile    second         HOSP-07             Ziekenhuis G
#> 5              Influenza A    second         HOSP-01             Ziekenhuis A
#> 6              Influenza A    second         HOSP-02             Ziekenhuis B
#>   institution_type municipality              ward          specialism   pc sex
#> 1         hospital         <NA> Internal Medicine   Internal Medicine 7384   F
#> 2  ltc_institution         <NA>              <NA>                <NA> 8524   M
#> 3         hospital         <NA>        Cardiology          Cardiology 9800   M
#> 4         hospital         <NA>       Pulmonology         Pulmonology 7600   M
#> 5         hospital         <NA>        Geriatrics Clinical Geriatrics 9930   F
#> 6         hospital         <NA>           Surgery             Surgery 9021   F
#>   age   source_key
#> 1  33 SYN-00000001
#> 2  37 SYN-00000060
#> 3  76 SYN-00000159
#> 4   8 SYN-00000203
#> 5  79 SYN-00000061
#> 6  14 SYN-00000062
```
