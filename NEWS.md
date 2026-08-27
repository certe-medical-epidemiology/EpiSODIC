# EpiSODIC 0.8.0

## New

- `episodic_check_denominators()` and `episodic_check_institution_activity()`: the same non-throwing pre-flight report `episodic_check_cases()` gives the case feed, now available for the optional positivity and hospital-activity feeds too
- `episodic_run_cron()` now refuses on a malformed `institution_activity` feed before writing anything (when handed as a plain data frame), instead of only mid-transaction
- `episodic_run_cron()` writes a timestamped `message()` at every phase of a run (config resolution, DB connect, each feed load, lattice enumeration, each detector, and each stream's reconciliation), always on - useful in any interactive load-through and in a real cron job's own log. A new `debug = TRUE` argument adds `sessionInfo()`, package/driver versions, memory snapshots, and per-stream detail on top, for chasing a failure (including a crashed session) that leaves no R-level error behind

## Changed

- L4 (province) stream derivation supports an operator-supplied PC-to-province lookup via `EPISODIC_PC_PROVINCE_MAP`, falling back to the bundled Northern Netherlands demo ranges as before - a deployment outside that demo region previously got no L4 streams at all, silently
- Archive screen sorts by Period (descending) instead of by closing date
- Dossier top card reorders to confirmed, unique patients, duration (new), doubling time, case-free days, priority, with ratio/incidence density (where shown) moved after priority
- Pathogen screen's detection parameter table shows a long `source_ref` reference in the "What it does" column instead of "Value"
- Pathogen screen's positivity chart no longer renders a percentage axis past 100%
- Pathogen screen's weekly incidence and season/year comparison charts share the same x-axis span for any period that falls within a single season/year
- Geography map charts drop their fill legend (redundant with the on-map count labels) so the render height computed for the map's own aspect ratio is not shortened by legend space, removing the whitespace above/below the map
- `.shiny-plot-output` centres its contents vertically and horizontally; the geo-panel map group centres horizontally as well

# EpiSODIC 0.7.0

This release includes a database schema migration (`episodic_case` gains
a required `lab_number` column) - existing databases need to be
recreated or migrated before running against this version.

## New

- Case data contract gains a required `lab_number` column: your laboratory's own specimen/culture number, distinct from `source_key` and, unlike it, not required to be unique - two rows may share one when a single culture yields more than one reported result. Shown on the line list alongside `patient_key`, replacing the previously-shown (and not useful) `source_key`
- Similar-clusters panel on the dossier gains a cluster ID column and opens the same way every other cluster table does

## Changed

- `episodic_db_case_insert_new()` batches its existence check and insert into chunked round trips instead of one query pair per row, a large speedup for larger imports

# EpiSODIC 0.6.*

## New

- Unassessed clusters auto-close once their last case day is older than `config$reconciliation$stale_open_days` (default 60 days)
- Run-detail modal shows how many clusters a run auto-closed
- Info screen shows the app's version, description, license and website, read live from `DESCRIPTION`
- Line list gains `patient_key` as its first column

## Changed

- Rail sorts by last case day (newest first) instead of priority score
- Assessment rationale is now optional, not required
- Rail's bulk-select checkbox sits inline with the pathogen name
- Version number moved from the navbar to the Info screen

## Fixed

- Modal close buttons (dead under Bootstrap 5)
- Several Shiny performance issues in the rail and colour palette

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
