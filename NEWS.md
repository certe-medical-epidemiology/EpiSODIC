# EpiSODIC 0.3.0

- Expanded dashboard translations from Dutch and English to also cover
  Spanish, French, German, Mandarin Chinese, Hindi, and (Modern Standard)
  Arabic (`inst/i18n/{es,fr,de,zh,hi,ar}.json`), covering the top spoken
  languages in the world alongside Dutch and English. Pass any of these
  as `lang` to [episodic_run_app()], [episodic_demo()], or
  [episodic_report_render()]. `episodic_format_date_range()` now also
  formats month names for these languages, sourced from the same
  `inst/i18n/*.json` files as every other piece of dashboard text.
- Added the `EPISODIC_LANGUAGE` environment variable, read by
  [episodic_run_app()], [episodic_demo()], [episodic_report_render()],
  and [episodic_tr()] as the default `lang` when it is not passed
  explicitly. Falls back to `"en"` if unset.

# EpiSODIC 0.2.0

- Rewrote all documentation for an epidemiologist audience: every man page
  is now written for someone running or configuring EpiSODIC, not for a
  contributor reading the source. Several related functions were combined
  onto a single help page (dashboard chart builders, HTML formatting
  helpers, geographic reference data), and examples now show their output
  instead of assigning it silently.
- `EPISODIC_DB` also accepts a `mysql://` DSN (`episodic_db_dsn_mariadb()`
  / `episodic_db_dsn_mysql()`) to run against MariaDB/MySQL instead of
  SQLite, from the same schema file.
- Renamed the internal `episode_`/`episode-` prefix to `episodic_`/
  `episodic-` throughout (functions, database tables, CSS classes) to
  match the package name.
- Package version shown in the Shiny app header.
- README screenshots now ship in `man/figures/` so they render on CRAN
  too; removed placeholder pkgdown reference pages and trimmed the
  internal dev notes (`data-raw/`).

# EpiSODIC 0.1.0

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
- `episodic_demo()` runs the whole system against bundled synthetic
  data - no credentials or configuration required.
