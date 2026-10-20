# Generate the Keys for Importing and Exporting Data with Partners

Run this once per instance. It writes two key pairs into `path`:

## Usage

``` r
episodic_import_export_keygen(path, overwrite = FALSE)
```

## Arguments

- path:

  A directory to write the four files into; created if it does not
  exist.

- overwrite:

  Whether to replace key files already there. `FALSE` by default: a
  replaced key pair cannot open the files partners have already
  encrypted to the old one, and the partners have pinned the old signing
  key.

## Value

Invisibly, a list with the four file paths (`encryption_secret`,
`encryption_public`, `signing_secret`, `signing_public`) and the two
public keys' fingerprints (`encryption_fingerprint`,
`signing_fingerprint`).

## Details

- An encryption key pair:

  `episodic_import_export_encryption.key` (secret) and
  `episodic_import_export_encryption.pub` (public). A partner encrypts
  the files meant for you to the public key; only the secret key opens
  them. Point `EPISODIC_IMPORT_KEY` at the secret key file.

- A signing key pair:

  `episodic_import_export_signing.key` (secret) and
  `episodic_import_export_signing.pub` (public). Every file you export
  is signed with the secret key; a partner pins your public key and
  refuses any file it does not verify. Point
  `EPISODIC_EXPORT_SIGNING_KEY` at the secret key file.

Give your partners the two `.pub` files and your instance identifier
([`episodic_instance_id()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_instance_id.md)),
and never the `.key` files. The secret key files are written readable by
their owner only; on Windows, which keeps no such permission bits, keep
them in a directory only the account running EpiSODIC can read.

Keys never belong in the configuration file: they would enter a stored
configuration export, and whoever reads that could read or forge every
file.

## See also

[`episodic_cases_export()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_export.md),
[`episodic_cases_import()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_import.md),
[`episodic_import_export_key_fingerprint()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_key_fingerprint.md),
[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)

## Examples

``` r
dir <- tempfile()
keys <- episodic_import_export_keygen(dir)
basename(unlist(keys[1:4]))
#> [1] "episodic_import_export_encryption.key"
#> [2] "episodic_import_export_encryption.pub"
#> [3] "episodic_import_export_signing.key"   
#> [4] "episodic_import_export_signing.pub"   
unlink(dir, recursive = TRUE)
```
