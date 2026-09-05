# Package index

## Try it out

The fastest way to see what EpiSODIC does, with no data or configuration
of your own required.

- [`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
  : Try EpiSODIC With Synthetic Outbreak Data

## Run EpiSODIC

The two calls that run a real instance: detect on a schedule, and open
the dashboard for your board to review the results.

- [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  : Run One Surveillance Detection Cycle
- [`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md)
  : Open the EpiSODIC Dashboard

## Connect your own data

How to plug in your laboratory results and, optionally, testing volume
and hospital activity data - and how to check your extract against the
requirements before you run anything.

- [`episodic_case_data`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  : Connect your own laboratory data
- [`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
  : Check Your Case Data Before You Hand It to EpiSODIC
- [`episodic_check_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_denominators.md)
  : Check Your Positivity Data Before You Hand It to EpiSODIC
- [`episodic_check_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_institution_activity.md)
  : Check Your Hospital Activity Data Before You Hand It to EpiSODIC
- [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md)
  : Generate Synthetic Outbreak Data
- [`episodic_synthetic_cases_calibration()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases_calibration.md)
  : Generate Synthetic Data at Tunable Cluster Volume
- [`episodic_synthetic_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_denominators.md)
  : Add a Testing-Volume (Positivity) Feed
- [`episodic_synthetic_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_institution_activity.md)
  : Add a Hospital Activity Feed (Patient-Days)

## Add clusters from another system

Add clusters detected by another algorithm or system - never connected
to your own case data.

- [`episodic_add_manual_cluster()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_manual_cluster.md)
  : Add One or More Manual (External) Clusters

## Set up a database

Create and connect to the database EpiSODIC stores its data in.

- [`episodic_db_create()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_create.md)
  : Set Up a New EpiSODIC Database

- [`episodic_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_connect.md)
  : Connect to an Existing EpiSODIC Database

- [`episodic_db_open()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_open.md)
  :

  Connect Using the `EPISODIC_DB` Environment Variable

- [`episodic_db_truncate()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_truncate.md)
  : Empty Every EpiSODIC Table, Keeping the Schema Itself

- [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)
  [`episodic_db_dsn_mysql()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)
  : Connect EpiSODIC to a MariaDB or MySQL Server

## Accounts and sign-in

- [`episodic_auth`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_auth.md)
  : How sign-in works
- [`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md)
  : Create an Account for a New Epidemiologist or Viewer

## Report results

How to send an outbreak report to clinical colleagues.

- [`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
  : Render an Outbreak Report for Clinical Colleagues

## Notifications

Send alerts to your team when new clusters are detected or a detection
run fails. Supports ntfy, SMTP, sendmail, Microsoft 365, Teams, and
Slack.

- [`episodic_notifications`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_notifications.md)
  : How notifications work
- [`episodic_notify_test()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_notify_test.md)
  : Send a Test Notification Through All Configured Channels
- [`episodic_setup_microsoft365()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_setup_microsoft365.md)
  : Set Up Microsoft 365 Authentication for Notifications

## Customise the look and feel

House colours, maps, translations, and dashboard chart building blocks -
useful if you are adapting EpiSODIC or writing your own report template.

- [`episodic_palette()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_palette.md)
  : The Dashboard's Colour Palette and Typography
- [`episodic_geo_overlay_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo.md)
  [`episodic_geo_source_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo.md)
  : Show Clusters on a Map
- [`episodic_ui_epi_curve_chart()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_charts.md)
  [`episodic_ui_trend_chart()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_charts.md)
  [`episodic_ui_rt_chart()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_charts.md)
  : Draw the Dashboard Charts (Epidemic Curve, Trend, and Rt)
- [`episodic_ui_italicise_taxon()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md)
  [`episodic_ui_code_join()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md)
  : Format Text for Outbreak Reports and the Dashboard

## Configure export

Make sure to export the settings.

- [`episodic_config_export()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_export.md)
  : Export the Resolved Configuration as a Zip File
