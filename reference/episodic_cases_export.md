# Export a Date Range of Cases to an Encrypted Export File

Writes this instance's own cases with a sample date from `from` to `to`
into one encrypted, signed file that another EpiSODIC instance imports
with
[`episodic_cases_import()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_import.md).
The file is safe to keep in a public place (a public bucket, a shared
drive, a Git repository): without a recipient's key it reveals nothing
beyond its own size, and without your signing key nobody can forge or
alter it. It works headless, from `Rscript` or a scheduled job, with no
dashboard.

## Usage

``` r
episodic_cases_export(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  from,
  to,
  file,
  recipients = NULL,
  signing_key = Sys.getenv("EPISODIC_EXPORT_SIGNING_KEY", unset = NA),
  passphrase = NULL,
  fields = NULL,
  pc_characters = NULL,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- db_path:

  The EpiSODIC database: a SQLite file or a MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

- from, to:

  The sample-date range, inclusive: `Date`s or `"YYYY-MM-DD"`.

- file:

  The path to write. An existing file is replaced, in one step, so a
  reader never sees half of it.

- recipients:

  For public-key mode: one or more recipients' public encryption keys,
  each as its text or as the path of its `.pub` file.

- signing_key:

  Your secret signing key, as its text or as the path of the file
  [`episodic_import_export_keygen()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_keygen.md)
  wrote. Defaults to the `EPISODIC_EXPORT_SIGNING_KEY` environment
  variable.

- passphrase:

  For passphrase mode: the passphrase. Give either `recipients` or
  `passphrase`, not both.

- fields:

  The optional case columns to include (any of `receipt_date`,
  `care_line`, `municipality`, `ward`, `specialism`, `pc`, `sex`,
  `age`). `NULL` (the default) uses the configured `export.fields`. The
  required columns are always included.

- pc_characters:

  Keep only the first this many characters of each postcode. `NULL` (the
  default) uses the configured `export.pc_characters`, which ships
  unset: postcodes as stored.

- episodic_config_path:

  The configuration path. Defaults to the `EPISODIC_CONFIG` environment
  variable.

## Value

Invisibly, a list with `file`, `file_sha256`, `n_cases`, `fields`,
`omitted_fields` and `export_id` (the audit row).

## Details

Two ways to encrypt, and the first is the one to use:

- Public-key mode (`recipients`):

  The file is encrypted to each recipient's public encryption key, and
  only their secret keys open it. Several recipients can share one file.
  The recipients cannot forge a file in your name, and replacing one
  recipient's key involves nobody else.

- Passphrase mode (`passphrase`):

  The file is encrypted with a key derived from a passphrase (scrypt,
  with a fresh random salt per file). Everyone who holds the passphrase
  can read every file, so changing it means coordinating every party at
  once. At least 16 characters.

In both modes the file is signed with your signing key, and an importer
refuses it unless your public signing key is among the senders they have
pinned.

**What leaves the instance.** Cases are exported as this instance stores
them: one row per episode (see
[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)),
with the institution as its stored SHA-1 hash, never the raw key. Cases
this instance itself imported from a partner are never exported. Only
the optional columns in `fields` are included; the others are left out
of the file entirely, the file records which, and an importer receives
them as not recorded (`NA`), never as a default value.

Every export is recorded in the database (`episodic_export_event`):
when, by which host and account, the range, the columns, the recipients'
key fingerprints, the signing key's fingerprint, the number of cases and
the SHA-256 of the file. A failed export is recorded too.

## See also

[`episodic_cases_import()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_import.md),
[`episodic_import_export_keygen()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_keygen.md),
[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)

## Examples

``` r
# \donttest{
db_path <- tempfile(fileext = ".sqlite")
episodic_run_cron(
  cases = episodic_synthetic_cases(
    start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
  ),
  db_path = db_path,
  run_date = as.Date("2025-03-31")
)
#> 2026-10-20 21:58:10.491 | episodic_run_cron() starting (host=runnervmxehtw, account=runner)
#> 2026-10-20 21:58:10.491 | Resolving configuration
#> 2026-10-20 21:58:10.496 | Configuration resolved (hash 7fef366a6991)
#> 2026-10-20 21:58:10.496 | Connecting to database
#> 2026-10-20 21:58:10.497 | No existing database found - creating one
#> 2026-10-20 21:58:10.515 | Database connected (SQLite 3.53.3)
#> 2026-10-20 21:58:10.516 | Run 1 started
#> 2026-10-20 21:58:10.516 | Resolving and checking case data
#> 2026-10-20 21:58:10.556 | Case data checked: 421 rows, 0 problems, 0 advisory finding(s)
#> 2026-10-20 21:58:10.557 | Beginning transaction
#> 2026-10-20 21:58:10.558 | Loading pathogen configuration
#> 2026-10-20 21:58:10.560 | Pathogen configuration loaded (39 pathogen(s))
#> 2026-10-20 21:58:10.560 | Loading case data into the database
#> 2026-10-20 21:58:10.625 | Case data loaded: supplied=421, deduplicated=403, inserted=403
#> 2026-10-20 21:58:10.626 | Fetching all known cases and institutions
#> 2026-10-20 21:58:10.627 | First run on this database: reporting the whole case history rather than only what falls inside the detectors' lookback windows. Clusters whose last case is more than 60 day(s) before this run's date close in this same run and go straight to the Archive; the rest open for assessment. Every run after this one is bounded again, so nothing here is reported twice.
#> 2026-10-20 21:58:10.629 | Case history on file spans 2025-01-01 to 2025-03-31, ending 0 day(s) before this run's date (2025-03-31)
#> 2026-10-20 21:58:10.629 | Enumerating lattice streams
#> ! 2026-10-20 21:58:10.644 | no province could be resolved for any of the 105 postcode values in this run - province-level (L4) detection has nothing to run on. EPISODIC_PC_PROVINCE_MAP is unset, and there is no built-in rule to fall back on - deriving a province from a postcode is country-specific. Point it at your own pc/province_code CSV, or leave it unset and the province level stays empty.
#> 2026-10-20 21:58:10.648 | Running same-place detector
#> 2026-10-20 21:58:10.692 | Same-place detector found 5 detection(s)
#> 2026-10-20 21:58:10.692 | Running rare-trigger detector
#> 2026-10-20 21:58:10.695 | Rare-trigger detector found 1 detection(s)
#> 2026-10-20 21:58:10.695 | Farrington tests every week its streams can carry this run, rather than the 8-week catch-up cap: there is no earlier run to catch up to
#> 2026-10-20 21:58:10.697 | Reconciling 320 stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)
#> ! 2026-10-20 21:58:11.053 | MEM: declined stream Bordetella pertussis/pathogen_region/REGION, disabled by mem_mode
#> ! 2026-10-20 21:58:11.057 | MEM: declined stream Campylobacter/pathogen_region/REGION, insufficient history for climatology
#> ! 2026-10-20 21:58:11.060 | MEM: declined stream Clostridioides difficile/pathogen_region/REGION, disabled by mem_mode
#> ! 2026-10-20 21:58:11.062 | MEM: declined stream Giardia lamblia/pathogen_region/REGION, insufficient history for climatology
#> ! 2026-10-20 21:58:11.066 | MEM: declined stream Influenza A/pathogen_region/REGION, insufficient history for climatology
#> ! 2026-10-20 21:58:11.068 | MEM: declined stream MRSA/pathogen_region/REGION, disabled by mem_mode
#> ! 2026-10-20 21:58:11.071 | MEM: declined stream Neisseria meningitidis/pathogen_region/REGION, disabled by mem_mode
#> ! 2026-10-20 21:58:11.074 | MEM: declined stream Norovirus/pathogen_region/REGION, insufficient history for climatology
#> ! 2026-10-20 21:58:11.076 | MEM: declined stream RSV/pathogen_region/REGION, insufficient history for climatology
#> ! 2026-10-20 21:58:11.079 | MEM: declined stream Salmonella/pathogen_region/REGION, insufficient history for climatology
#> 2026-10-20 21:58:11.096 | Stream loop time by stage: cases 0.0s, mem 0.0s, eligibility 0.0s, farrington 0.0s, trend 0.0s, detections 0.0s, reconciliation 0.0s, total 0.1s (320 stream(s))
#> 2026-10-20 21:58:11.096 | Stream reconciliation done: 6 detection(s), 6 new signal(s), 0 updated signal(s)
#> 2026-10-20 21:58:11.096 | Backfill: 6 cluster(s) opened from the case history, 1 of them already closed by the system and in the Archive, 5 left open for assessment
#> 2026-10-20 21:58:11.098 | Suppressing lattice: weighing 6 cluster(s) across 5 pathogen(s)
#> 2026-10-20 21:58:11.099 | Suppressing lattice: 42 case link(s) read for 6 cluster(s), 0 assessed cluster(s) exempt
#> 2026-10-20 21:58:11.103 | Suppressing lattice done: 0 cluster(s) suppressed (0 parent(s) behind a dominant child, 0 child(ren) behind a diffuse parent)
#> 2026-10-20 21:58:11.112 | Nowcast: 0 of 5 stream(s) with an open cluster nowcast, 5 without enough reported yet to nowcast from, 0 failed
#> 2026-10-20 21:58:11.113 | Epidemic direction: no open epidemic
#> 2026-10-20 21:58:11.113 | Epidemic outlook: no open epidemic
#> 2026-10-20 21:58:11.122 | Outbreak-end forecast: 0 of 4 open outbreak(s) of a transmissible pathogen computed, 1 point source, 3 without the parameters or reporting history, 0 failed
#> 2026-10-20 21:58:11.122 | Committing transaction
#> 2026-10-20 21:58:11.124 | Finishing run 1 (status: success)
#> 2026-10-20 21:58:12.109 | episodic_run_cron() finished in 1.6s (status: success)
sender <- episodic_import_export_keygen(tempfile())
receiver <- episodic_import_export_keygen(tempfile())
file <- tempfile(fileext = ".episodic")
episodic_cases_export(
  db_path = db_path,
  from = "2025-03-01",
  to = "2025-03-31",
  file = file,
  recipients = receiver$encryption_public,
  signing_key = sender$signing_secret
)
# }
```
