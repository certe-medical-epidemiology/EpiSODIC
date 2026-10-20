# Import an Encrypted Export File as Case Data

Opens a file written by
[`episodic_cases_export()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_export.md)
at another instance, verifies it in full, and returns its cases as a
validated data frame in the case data format. It does **not** write to
the database: hand the result to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
as `imports`, and the cases are loaded, deduplicated, detected on and
recorded like every other case, with the import itself recorded against
that run.

## Usage

``` r
episodic_cases_import(
  file,
  key = Sys.getenv("EPISODIC_IMPORT_KEY", unset = NA),
  trusted_senders,
  passphrase = NULL,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- file:

  The export file: a path, or its bytes as a raw vector.

- key:

  Your secret encryption key, as its text or as the path of the file
  [`episodic_import_export_keygen()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_keygen.md)
  wrote. Defaults to the `EPISODIC_IMPORT_KEY` environment variable. Not
  used for a passphrase file.

- trusted_senders:

  The public signing keys of the partners whose files you accept, each
  as its text or as the path of its `.pub` file. A file signed by any
  other key is refused.

- passphrase:

  For a file written in passphrase mode: the passphrase. Leave `NULL`
  for a file encrypted to your key.

- episodic_config_path:

  The configuration path, for the geography the warnings are measured
  against. Defaults to the `EPISODIC_CONFIG` environment variable.

## Value

A data frame of class `episodic_imported_cases`, with exactly the case
data columns, and the `"import"` attribute.

## Details

    partner <- episodic_cases_import(
      "partner.episodic",
      trusted_senders = "keys/partner_signing.pub"
    )
    episodic_run_cron(cases = my_cases, imports = partner)

Nothing in the file is returned unless all of it holds: it must open
with your key (or the passphrase), its signature must verify against one
of `trusted_senders`, and every table must match the SHA-256 its
manifest records. A wrong key, an untrusted or forged sender, a tampered
or truncated file, a format version this EpiSODIC does not read, and a
hash mismatch each stop with an error that says which; a partially
readable file is never partially imported. The error carries class
`episodic_import_export_error` and a `reason` code.

What is returned:

- `source_key`, `patient_key` and `lab_number` are prefixed with the
  sender's instance identifier and a colon, so the partner's keys can
  never be taken for yours.

- `institution_key` is the stored hash the sender's database holds; the
  file declares it so, and the run uses it as it is.

- A column the sender left out is `NA` on every row, which the case data
  format reads as "not recorded", never a default value.

- Cases whose postcode this instance's reference data cannot place at
  some level are kept, with a warning per level.

The data frame carries class `episodic_imported_cases` and an `"import"`
attribute describing the file (sender, signer fingerprint, range,
omitted columns, the file's SHA-256, the warnings), which the run
records.

Imports are append-only: a case the sender later corrects or retracts
stays as it was first imported. See
[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)
for this and the other limits (patient linkage across laboratories,
pathogen naming, history imported after the first run).

## See also

[`episodic_cases_export()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_export.md),
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
partner <- episodic_cases_import(
  "partner.episodic",
  trusted_senders = "keys/partner_signing.pub"
)
attr(partner, "import")$omitted_fields
episodic_run_cron(cases = my_cases, imports = partner)
} # }
```
