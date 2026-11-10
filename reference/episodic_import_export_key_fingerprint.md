# Fingerprint of an Import/Export Public Key

The SHA-256 of a public key, as the audit trail records it. Read it to
your partner over a channel other than the one you sent the key file
through (a telephone call does): when both of you see the same
fingerprint, the file was not swapped on the way.

## Usage

``` r
episodic_import_export_key_fingerprint(key)
```

## Arguments

- key:

  A public key: its text, or the path of the `.pub` file
  [`episodic_import_export_keygen()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_keygen.md)
  wrote. An encryption key and a signing key are both accepted; a secret
  key is refused.

## Value

A 64-character hexadecimal string.

## See also

[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)

## Examples

``` r
dir <- tempfile()
keys <- episodic_import_export_keygen(dir)
episodic_import_export_key_fingerprint(keys$signing_public)
#> [1] "b3aaf7ca38675b08fe1996bb684d3eb52dc25639e61f2545aa21e1f103cbbb08"
unlink(dir, recursive = TRUE)
```
