# Changelog

## EpiSODIC 0.2.0

- Rewrote all documentation for an epidemiologist audience: every man
  page is now written for someone running or configuring EpiSODIC, not
  for a contributor reading the source. Several related functions were
  combined onto a single help page (dashboard chart builders, HTML
  formatting helpers, geographic reference data), and examples now show
  their output instead of assigning it silently.
- `EPISODIC_DB` also accepts a `mysql://` DSN
  ([`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)
  /
  [`episodic_db_dsn_mysql()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md))
  to run against MariaDB/MySQL instead of SQLite, from the same schema
  file.
- Renamed the internal `episode_`/`episode-` prefix to `episodic_`/
  `episodic-` throughout (functions, database tables, CSS classes) to
  match the package name.
- Package version shown in the Shiny app header.
- README screenshots now ship in `man/figures/` so they render on CRAN
  too; removed placeholder pkgdown reference pages and trimmed the
  internal dev notes (`data-raw/`).

## EpiSODIC 0.1.0

First development version.

- Detection: automatic lattice enumeration, five detectors
  (`farrington`, `ears`, `same_place`, `rare_trigger`, `mem`), cluster
  reconciliation across runs, cross-lattice suppression, baseline
  feedback.
- Interface: bilingual (NL/EN) Shiny app, read-only for anonymous
  visitors; dossier view (epi curve, trend, geography, Rt, line list);
  audit-trailed classification workflow; Performance screen.
- Reporting: versioned Quarto outbreak reports with small-count
  suppression.
- SQLite backend; optional `EpiEstim`/`mem`/`quarto`/`sf` integrations,
  each with a documented fallback when absent.
- [`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
  runs the whole system against bundled synthetic data - no credentials
  or configuration required.
