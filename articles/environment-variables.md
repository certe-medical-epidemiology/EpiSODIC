# Environment variables

Every `EPISODIC_*` variable is optional; each entry point works fine
with the equivalent argument passed explicitly instead. Environment
variables exist so an operator can configure a running instance (a
`systemd` unit, a Docker container) without editing R code - set them
once where the instance runs, and every entry point that falls back to
them picks the setting up automatically.

| Variable | Used by | Meaning |
|----|----|----|
| `EPISODIC_DB` | [`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md), [`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md) (`db_path` argument) | Path to the instance’s SQLite database, or a `mysql://` DSN pointing at a MariaDB/MySQL database instead - see “Database backend” above. |
| `EPISODIC_LANGUAGE` | every `lang` argument - [`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md), [`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md), [`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md), [`episodic_tr()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_tr.md) and every renderer beneath them | Dashboard/report language: `nl`, `en`, `es`, `fr`, `de`, `zh`, `hi`, or `ar`. Defaults to `en` if unset. Fixed for the whole running app - there is no in-app language switcher. |
| `EPISODIC_CONFIG` | [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md) (`episodic_config_path` argument) | Path to an instance override of detection configuration (pathogen thresholds, `same_place`/`rare_trigger`/Farrington settings), overlaid key-by-key on `inst/config/default.yaml`’s shipped defaults. |
| `EPISODIC_PALETTE_CONFIG` | [`episodic_palette()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_palette.md) (`palette_config_path` argument) | Path to an instance override of the UI colour palette, overlaid key-by-key on `inst/config/palette.yaml`’s shipped defaults. Deliberately separate from `EPISODIC_CONFIG`: colour is a display concern, never part of [`episodic_config_hash()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_hash.md)’s detection-reproducibility guarantee. |
| `EPISODIC_GEO_DATA` | [`episodic_geo_source_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo.md) (`path` argument) | Path to an `.rds` file holding an operator’s own geographic reference data (an `sf` object with `pc`/`geometry` columns), overriding the shipped Netherlands postcode default. See “Geographic reference data” above. |
| `EPISODIC_GEO_DATA_OVERLAY` | [`episodic_geo_overlay_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo.md) (`path` argument) | Path to an `.rds` file holding an optional region-outline overlay (an `sf` object with just a `geometry` column), drawn on top of the choropleth. No default. See “Geographic reference data” above. |
| `EPISODIC_QUARTO_REPORT` | [`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md) (`qmd_path` argument) | Path to an operator’s own Quarto report template, overriding the shipped `inst/report/cluster_report.qmd`. See “Custom report templates” above. |

None of these need to be set to run the demo -
[`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
uses a temporary SQLite file and every shipped default. See
`vignettes/deployment.Rmd` for `EPISODIC_DB` and
`EPISODIC_CONFIG`/`EPISODIC_PALETTE_CONFIG` in context, and the README’s
“Database backend”, “Geographic reference data” and “Custom report
templates” sections for the detail behind each of the rows above.
