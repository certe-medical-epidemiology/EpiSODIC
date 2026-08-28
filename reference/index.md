# Package index

## Try it out

The fastest way to see what EpiSODIC does, with no data or configuration
of your own required.

- [`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
  : Try EpiSODIC with synthetic outbreak data

## Run EpiSODIC

The two calls that run a real instance: detect on a schedule, and open
the dashboard for your board to review the results.

- [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  : Run one surveillance detection cycle
- [`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md)
  : Open the EpiSODIC dashboard
- [`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md)
  : Resolve a data source argument to a data frame

## Connect your own data

How to plug in your laboratory results and, optionally, testing volume
and hospital activity data - and how to check your extract against the
contract before you run anything.

- [`episodic_case_columns`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  [`episodic_care_lines`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  [`episodic_institution_types`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  [`episodic_sex_codes`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  : Connect your own laboratory data
- [`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
  : Check your case data before you hand it to EpiSODIC
- [`episodic_check_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_denominators.md)
  : Check your positivity data before you hand it to EpiSODIC
- [`episodic_check_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_institution_activity.md)
  : Check your hospital activity data before you hand it to EpiSODIC
- [`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
  : Check that your case data has the right shape, or stop
- [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md)
  : Generate synthetic outbreak data
- [`episodic_synthetic_cases_calibration()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases_calibration.md)
  : Generate synthetic data at tunable cluster volume
- [`episodic_synthetic_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_denominators.md)
  : Add a testing-volume (positivity) feed
- [`episodic_synthetic_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_institution_activity.md)
  : Add a hospital activity feed (patient-days)

## Set up a database

Create and connect to the database EpiSODIC stores its data in.

- [`episodic_db_create()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_create.md)
  : Set up a new EpiSODIC database

- [`episodic_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_connect.md)
  : Connect to an existing EpiSODIC database

- [`episodic_db_open()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_open.md)
  :

  Connect using the `EPISODIC_DB` environment variable

- [`episodic_db_truncate()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_truncate.md)
  : Empty every EpiSODIC table, keeping the schema itself

- [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)
  [`episodic_db_dsn_mysql()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)
  : Connect EpiSODIC to a MariaDB or MySQL server

## Accounts and sign-in

- [`episodic_auth`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_auth.md)
  : How sign-in works
- [`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md)
  : Create an account for a new epidemiologist or viewer

## Configure detection

Detection thresholds, priority scoring, and reproducibility.

- [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md)
  : Read the surveillance configuration
- [`episodic_config_hash()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_hash.md)
  : Fingerprint a configuration for reproducibility

## Interpret and report results

How the dashboard’s narrative summary is generated, and how to send an
outbreak report to clinical colleagues.

- [`episodic_interpretation_slots`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_interpretation.md)
  : How the dossier's narrative summary is written
- [`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
  : Render an outbreak report for clinical colleagues

## Customise the look and feel

House colours, maps, translations, and dashboard chart building blocks -
useful if you are adapting EpiSODIC or writing your own report template.

- [`episodic_palette()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_palette.md)
  : The dashboard's colour palette
- [`episodic_geo_overlay_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo.md)
  [`episodic_geo_source_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo.md)
  : Show clusters on a map
- [`i18n`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/i18n.md)
  : Translation lookup
- [`episodic_tr()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_tr.md)
  : Translate a dashboard text key
- [`episodic_ui_epi_curve_chart()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_charts.md)
  [`episodic_ui_trend_chart()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_charts.md)
  [`episodic_ui_rt_chart()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_charts.md)
  : Draw the dashboard charts (epidemic curve, trend, and Rt)
- [`episodic_ui_italicise_taxon()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md)
  [`episodic_ui_code_join()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md)
  : Format text for outbreak reports and the dashboard
