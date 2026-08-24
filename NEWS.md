# EpiSODIC 0.5.*

## New

- `episodic_check_cases()` reports all case data problems at once, with row-level detail and fixes
- Lattice suppression collapses one outbreak seen at multiple levels into a single dossier
- Linked clusters show e.g. "Linked to #123" and a Related clusters panel
- `episodic_case_data` documents every column's type, nullability and accepted values
- Deduplication now checks against the most recently stored episode, not just the current batch
- "Isolate" renamed to "positive" throughout the app, reports and validation advice
- New "Frequently asked questions" vignette
- Rail cards show the cluster's care line as a colour-coded chip (1st/2nd/3rd line) beside the pathogen name, and the cluster id in small type next to it
- The geography panel shows a second, uncropped choropleth alongside the existing cropped one, for region-wide context
- `episodic_care_lines` gains `"third"` (tertiary care)

## Changed

- Accounts now have exactly two roles: `"epidemiologist"` (read/write) and `"viewer"` (read-only)
- `episodic_validate_cases()` reports all problems in one error, not just the first
- `episodic_run_cron()` checks feeds before writing and stops cleanly on failure
- Status strip and Activity screen show why a run failed
- Farrington now retests every week since the last completed run, capped at 8 weeks
- `config$effect_size_floor` now gates new-cluster candidates against borderline signals
- `episodic_synthetic_cases()` demo data rebalanced to avoid spurious chance clusters
- `lang` now defaults correctly to `EPISODIC_LANGUAGE` everywhere; unset means English
- Empty strings in required columns are now treated as invalid, like `NA`
- Cluster status chips render filled (colour background, white text) throughout, not just outlined
- Rail card dates spell the month out when the range falls within a single month (`"2-12 January 2025"`), abbreviated otherwise (`"12 Jan. - 4 Feb. 2025"`)
- Rail card's last line shows case count and priority score; ratio dropped
- Bar chart labels widen from ~30 to ~50 characters before truncating, with a mouse-over tooltip for the full value

## Fixed

- Region-level streams no longer report the whole catchment's counts under one region's name
- Cluster line lists no longer include unrelated cases from the same ward or catchment
- Reconciliation no longer opens duplicate dossiers for an ageing cluster
- Translation placeholders no longer break on backslashes
- Epidemic curve thousands separator now follows the language by default
- `episodic_run_cron()` no longer corrupts `sample_date`/`receipt_date`/`period_start`/`period_end` when the operator supplies them as `Date` columns (as `episodic_case_data` explicitly allows) - RSQLite was binding them by their raw numeric epoch value instead of `YYYY-MM-DD` text, so a later read failed with "character string is not in a standard unambiguous format"
- `episodic_check_cases()`'s allowed-value messages (`care_line`, `institution_type`, `sex`) now quote each value, and its `?episodic_case_data` references are clickable in terminals/RStudio that support it
- `episodic_db_create()` against MySQL no longer fails with "BLOB/TEXT column can't have a default value" [1101] - `episodic_stream.denominator` and `episodic_case.care_line` are now widened to `VARCHAR` under the `mariadb` dialect, since MySQL (unlike MariaDB and SQLite) rejects a `DEFAULT` on `TEXT` columns outright
- `episodic_db_create()` against MySQL no longer fails with "BLOB/TEXT column ... used in key specification without a key length" [1170] - every remaining `TEXT` column used in a `PRIMARY KEY`, composite `PRIMARY KEY`, or `CREATE INDEX` (`pathogen`, `patient_key`, `sample_date`, `run_date`, `week_start`, `period_start`, `care_line`, `area_code`) is now widened to a bounded `VARCHAR` under the `mariadb` dialect, since MySQL requires an explicit key length on `TEXT`/`BLOB` columns used as a key
- `episodic_denominators_load()` against MySQL no longer fails to write a denominator row with no area stratum (`area_code = NA`, which `episodic_validate_denominators()` has always allowed) - `episodic_denominator`'s composite `PRIMARY KEY` included the nullable `area_code`, and MySQL implicitly forces every `PRIMARY KEY` column `NOT NULL`; it now has a surrogate `denominator_id` primary key with the natural key as a `UNIQUE` constraint instead, which permits multiple `NULL`s in both MySQL and SQLite
- `episodic_db_create()` against MySQL no longer fails with "you have an error in your SQL syntax ... near 'trigger'" [1064] - `trigger` is a reserved word in MySQL; `episodic_cluster_state.trigger` and the raw SQL that reads/writes it are now backtick-quoted
- `episodic_detect_same_place()` no longer hardcodes `care_line = NA` on every stream it creates or upserts - it is the detector responsible for the institution-level streams (LTC, out-of-hours, general practice) that lattice enumeration never creates on its own, and for hospital ward-level streams, so this had silently hidden the care line on almost every stream capable of showing one
- Logo no longer shows an opaque white background outside the hexagon

# EpiSODIC 0.4.0

## Changed

- Renamed data interface from "ingestion" to case-based naming (`R/cases*.R`, `episodic_cases_load()`, etc.), no deprecation shims
- Documented data frames/tibbles as normal input; zero-argument functions still accepted
- `care_line` now accepts `NA`, normalised to `"unknown"`
- Corrected `care_line`'s documented values to `first`, `second`, `other`, `unknown`
- A run that skips rows now finishes `partial`, not `success`
- Skipped institution activity rows now warn with counts and unmatched keys
- Denominators and institution activity validated like cases, with clear errors

## New

- Every case column documents type, nullability and permitted values
- Exported `episodic_care_lines`, `episodic_institution_types`, `episodic_sex_codes`
- `episodic_validate_cases()` enforces the full data contract
- `episodic_detection_run` records per-feed supply/dedup/insert/skip counts
- Activity screen shows per-run arrival and skip counts

## Fixed

- Institution activity rows with unmatched keys no longer silently dropped
- Missing translations for `running`/`partial` run statuses added in all languages

# EpiSODIC 0.3.1

## New

- New Pathogen screen: weekly incidence, seasonal comparison, Rt, positivity, age/sex, geography, signals
- MEM thresholds drawn against the season curve with intensity bands
- Rt now estimated per pathogen, not only per cluster
- `mem` detector documented on the Info screen

## Fixed

- `expected`, `excess` and `ratio` now correctly persisted and carried through reconciliation
- Priority score now computes all seven components instead of defaulting most to zero
- Missing components no longer drag the score down
- Doubling time now uses a Poisson fit to daily counts instead of cumulative log-regression
- Positivity now compares region-wide cases and tests over the same week
- Reporting lag now measured from cluster's last case day, not run date
- Rt no longer overestimated for clusters younger than one serial interval
- `episodic_mem_status()` no longer returns ambiguous `NULL` off-season
- MEM now evaluates the last complete week and fits only on full seasons
- Concentration now a share of cases with known postcode, not all cases
- Demographic baseline now excludes the cluster being compared
- Top navigation now correctly highlights the active screen

## Changed

- Choropleth cropped to relevant areas instead of the full reference dataset
- Geography panel now spans the full dossier width
- Standardised terminology to "pathogen" across all languages
- Axis labels and legend text standardised to 11pt secondary grey
- Season overlay stops at the current week instead of running flat to zero
- Dutch terminology standardised to "casus"
- `case_free_days` now translated instead of shown as a raw column name
- Weekly charts now show ISO week number with month/year, thinning past ~18 months
- Cluster ids now shown beside pathogen name in dossier and Pathogen screen
- Pathogen screen's cluster table renamed "Clusters in this period" and links through properly
- Archive now links through and includes a cluster id column
- Top navigation now follows links out of the Pathogen screen
- Pathogen picker's count now labelled as an all-time case count

# EpiSODIC 0.3.0

## New

- Dashboard translated into Spanish, French, German, Mandarin, Hindi and Arabic, alongside Dutch and English
- `EPISODIC_LANGUAGE` sets the default language, falling back to English

# EpiSODIC 0.2.0

## Changed

- Documentation rewritten for an epidemiologist audience
- Internal `episode_` prefix renamed to `episodic_` throughout

## New

- `EPISODIC_DB` accepts a `mysql://` DSN for MariaDB/MySQL
- Package version shown in the app header
- README screenshots now ship in `man/figures/`

# EpiSODIC 0.1.0

First development version.

- Detection: lattice enumeration, five detectors, cluster reconciliation, cross-lattice suppression, baseline feedback
- Interface: bilingual (NL/EN) Shiny app, dossier view, audit-trailed classification, Performance screen
- Reporting: versioned Quarto outbreak reports with small-count suppression
- SQLite backend, with optional `EpiEstim`/`mem`/`quarto`/`sf`
- `episodic_demo()` runs the whole system against bundled synthetic data
