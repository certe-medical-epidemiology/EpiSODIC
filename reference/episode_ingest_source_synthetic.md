# Synthetic ingestion source

The default (and only) implementation of the ingestion interface shipped
with this package (`R/ingest_interface.R`). Generates several years of
seasonal baseline case data across a synthetic set of institutions,
pathogens, PC areas and care lines, then injects two outbreaks of known
shape so the detectors have something to visibly fire on: one point
source (`add_outbreak_point_source`, a ward-level cluster tightly
bunched in time) and one propagated (`add_outbreak_propagated`, a
community cluster with generation-interval-spaced case waves).

## Usage

``` r
episode_ingest_source_synthetic(
  start_date = as.Date("2021-01-01"),
  end_date = as.Date("2025-12-31"),
  seed = 1
)
```

## Arguments

- start_date:

  First sample date to generate, a `Date`.

- end_date:

  Last sample date to generate, a `Date`.

- seed:

  RNG seed, for reproducible demo data.

## Value

A data frame satisfying
[`episode_ingest_validate_source()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_ingest_validate_source.md).

## Details

No Diver column name is invented here; every field in the returned data
frame is entirely synthetic and matches `episode_ingest_columns`.

## Examples

``` r
raw <- episode_ingest_source_synthetic(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
nrow(raw)
#> [1] 622
head(raw)
#>                           patient_key sample_date receipt_date
#> 1                PT-Norovirus-4817862  2025-01-01   2025-01-02
#> 2                PT-Norovirus-3172321  2025-01-01   2025-01-01
#> 3            PT-Campylobacter-4699847  2025-01-01   2025-01-02
#> 4            PT-Campylobacter-8657051  2025-01-01   2025-01-02
#> 5               PT-Salmonella-1128334  2025-01-01   2025-01-01
#> 6 PT-Clostridioides.difficile-5206854  2025-01-01   2025-01-01
#>                   pathogen care_line institution_key institution_display_name
#> 1                Norovirus    second          LTC-12           Zorgcentrum 12
#> 2                Norovirus    second         HOSP-05             Ziekenhuis E
#> 3            Campylobacter     first           GP-03                    Assen
#> 4            Campylobacter    second          LTC-10           Zorgcentrum 10
#> 5               Salmonella    second         HOSP-08             Ziekenhuis H
#> 6 Clostridioides difficile     first           GP-08                 Drachten
#>   institution_type municipality      ward          specialism   pc sex age
#> 1  ltc_institution         <NA>      <NA>                <NA> 9403   F  10
#> 2         hospital         <NA> Geriatrie Klinische geriatrie 7656   F  66
#> 3  gp_municipality        Assen      <NA>                <NA> 9873   F  73
#> 4  ltc_institution         <NA>      <NA>                <NA> 9676   F  36
#> 5         hospital         <NA>        IC           Chirurgie 9877   F  57
#> 6  gp_municipality     Drachten      <NA>                <NA> 9939   F  44
#>     source_key
#> 1 SYN-00000001
#> 2 SYN-00000002
#> 3 SYN-00000336
#> 4 SYN-00000337
#> 5 SYN-00000390
#> 6 SYN-00000504
```
