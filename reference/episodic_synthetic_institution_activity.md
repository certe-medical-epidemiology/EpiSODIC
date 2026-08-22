# Add a hospital activity feed (patient-days)

Raw case counts at a hospital can rise simply because the hospital is
busier, not because infection risk has increased. If you can supply
weekly patient-days (or admissions, or bed counts) per hospital,
EpiSODIC can normalise case counts against activity for a more reliable
signal. This feed is entirely optional: without it, detection falls back
to raw counts, which is a reasonable default, not a broken one.

## Usage

``` r
episodic_synthetic_institution_activity(
  institutions,
  start_date = as.Date("2021-01-01"),
  end_date = as.Date("2025-12-31"),
  seed = 1
)
```

## Arguments

- institutions:

  A data frame (or tibble) of institutions (as returned by your own
  institution registry), filtered internally to hospitals only.

- start_date, end_date:

  The period to generate weekly rows for.

- seed:

  RNG seed, for reproducible demo data.

## Value

A data frame with `institution_key`, `period_start`, `period_end`,
`patient_days`, `n_beds`, `source`.

## Details

This function is a synthetic example showing the expected shape: weekly
patient-days per hospital, modelled as bed count times occupancy, with a
realistic winter peak. Use it as a template for your own data, which you
pass to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
as `institution_activity` - normally a data frame or `tibble`.

## Examples

``` r
institutions <- data.frame(
  institution_key = "HOSP-1", institution_type = "hospital", n_beds = 320
)
activity <- episodic_synthetic_institution_activity(
  institutions,
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
head(activity)
#>   institution_key period_start period_end patient_days n_beds    source
#> 1          HOSP-1   2025-01-01 2025-01-07         1964    320 synthetic
#> 2          HOSP-1   2025-01-08 2025-01-14         1989    320 synthetic
#> 3          HOSP-1   2025-01-15 2025-01-21         2031    320 synthetic
#> 4          HOSP-1   2025-01-22 2025-01-28         2097    320 synthetic
#> 5          HOSP-1   2025-01-29 2025-02-04         1951    320 synthetic
#> 6          HOSP-1   2025-02-05 2025-02-11         2084    320 synthetic
```
