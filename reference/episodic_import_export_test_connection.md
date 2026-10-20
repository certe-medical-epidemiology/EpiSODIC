# Test an Import Source or Export Schedule

Checks a source or schedule registered on the dashboard's Instance
screen the way the next scheduled run will use it, and writes nothing.
For an import source it reads the key or passphrase variable, fetches
the file and imports it in full (decryption, signature, hashes), so a
passing test means the next run takes it in. For an export schedule it
reads the signing key and passphrase variables and checks the
destination without writing to it: a path's directory must exist and be
writable; an HTTP(S) destination is asked for `OPTIONS`, which shows
whether it answers and refuses the credentials outright, while whether
an upload is accepted is known only from the first real export.

## Usage

``` r
episodic_import_export_test_connection(
  name,
  kind = c("source", "schedule"),
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- name:

  The source's or schedule's name.

- kind:

  `"source"` or `"schedule"`.

- db_path:

  The EpiSODIC database. Defaults to the `EPISODIC_DB` environment
  variable.

- episodic_config_path:

  The configuration path. Defaults to the `EPISODIC_CONFIG` environment
  variable.

## Value

An object of class `episodic_import_export_test`: a list with `name`,
`kind`, `ok` (`TRUE` when no check failed) and `lines`, one per check.
Printing it lists the checks.

## Details

The environment variables are read in the process this runs in. The
scheduled run reads its own, from wherever it is started; a test from a
console or the dashboard proves that environment, not the cron's.

## See also

[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_import_export_test_connection("Partner lab", kind = "source")
} # }
```
