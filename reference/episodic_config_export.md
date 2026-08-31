# Export the resolved configuration as a zip file

Bundles the fully-resolved configuration (shipped defaults, your
`EPISODIC_CONFIG` YAML overlay, and - if `db_path` points at a
database - any Settings-screen `notifications` override) together with
the pathogen configuration and, if set, the colour palette override,
into a single zip file. This is the Settings screen's "export
configuration" button; it is also just a plain function you can call
from the console to snapshot an instance's configuration for backup or
migration.

## Usage

``` r
episodic_config_export(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
  output_dir = NULL,
  include_secrets = FALSE
)
```

## Arguments

- db_path:

  Path to the EpiSODIC database: an existing SQLite file, or a
  MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable. Only used to read
  a Settings-screen `notifications` override, if one exists - `NA`/unset
  skips this and exports the YAML-resolved configuration only.

- episodic_config_path:

  Passed to
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md).

- output_dir:

  Directory to write the zip into. Defaults to a `config_exports/`
  directory next to `db_path` (mirroring where
  [`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
  writes outbreak reports), or a temporary directory if `db_path` is not
  set.

- include_secrets:

  If `FALSE` (the default), notification secrets (SMTP/webhook/client
  passwords) are replaced with `"***"` in the exported configuration -
  the same masking the Settings screen applies on-screen. Pass `TRUE`
  only when you specifically intend the export to be usable to restore
  working notification channels elsewhere, and can handle the file with
  the same care as the secrets themselves.

## Value

Invisibly, the path to the written zip file.

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_config_export(db_path = Sys.getenv("EPISODIC_DB"))
} # }
```
