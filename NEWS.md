# EpiSODIC 0.11.0

## New

- New `access.require_login` configuration key (default `false`, keeping the current behaviour): when `true`, an anonymous visitor gets a sign-in prompt and nothing else, because the server renders no navigation, no status strip and no screen for them rather than hiding it client-side
- The Settings screen has a read-only "Dashboard access" panel reporting which way `access.require_login` is set; it is deliberately not editable in-app, so a login wall cannot be switched off from inside the app by a single account
- A cluster's dossier can now be opened by URL: `?cluster=<id>` on the dashboard address selects that cluster on the Clusters screen
- New-cluster notifications now link each cluster id straight to its dossier when `dashboard_url` is configured
- The geography panel and the outbreak report now name the province each postcode falls in, alongside the postcode itself
- The Info screen has a "Reference data" panel reporting, per `EPISODIC_*` variable, whether the instance's own file was read, was rejected, or was read and matched nothing, and what it delivered
- The reference-data panel reports how many of the case data's own postcodes resolve to a province, which is what distinguishes a `EPISODIC_PC_PROVINCE_MAP` that was never read from one whose postcode format does not match the case data
- `episodic_default_style.yaml` (and `EPISODIC_STYLE`) now also carries typography: `font` (a CSS font-family stack) and `font_size_base`, which scales every other font size in the dashboard, since those are now expressed in `rem` relative to it
- Every cluster table now has a "Case days" column: the number of distinct dates with at least one case, as its own figure next to the case count and the calendar duration - a cluster can run 90 days with cases on only 10 of them (a sharp peak) or on 39 of them (a flat, sustained rise), the same duration and case count, two different epidemic shapes
- `episodic_add_user()` is now exported, so accounts can be provisioned from a script without reaching into the package's internals

## Changed

- Trimmed the exported API surface to what an operator actually needs: `episodic_care_lines()`, `episodic_case_columns()`, `episodic_config_hash()`, `episodic_config_resolve()`, `episodic_institution_types()`, `episodic_interpretation_slots()`, `episodic_provision_user()`, `episodic_resolve_data()`, `episodic_sex_codes()`, `episodic_tr()` and `episodic_validate_cases()` are no longer exported (still used internally)
- The shipped default config, style, pathogen and report files are renamed to self-explanatory names: `default.yaml` -> `episodic_default_config.yaml`, `palette.yaml` -> `episodic_default_style.yaml`, `pathogen_config.csv` -> `episodic_default_pathogen_config.csv`, `cluster_report.qmd` -> `episodic_default_report.qmd`
- Every table of clusters (Archive, Pathogen screen, the dossier's related and similar-clusters panels, the outbreak report and the new-cluster notification) now shares one implementation with the same columns in the same order: cluster id, then context, then the case period (first case - last case, as one range), cases, case days, then what was decided (classification/state/closure date/shared cases), then duration and priority
- Every table of clusters is now sorted on the last case day, most recent first, with priority and cluster id breaking ties
- A cluster row that does not open a dossier, because lattice suppression folded it into another, now shows its id in the warning colour with a tooltip explaining why, instead of a link that does nothing
- The per-screen cluster table column translation keys are replaced by one `column.*` family shared by every cluster table
- `episodic_config_hash()` now also excludes the `access` section, for the same reason it excludes `notifications`: it governs how the instance is operated, never what a run computes
- `EPISODIC_PC_PROVINCE_MAP` pointing at a missing, unreadable, empty, wrongly-columned or duplicate-postcode CSV is now a run-stopping error naming the file and the problem, instead of a silent fall back to the shipped Dutch demo ranges
- A detection run in which no postcode resolves to a province now says so in its trace, instead of leaving province-level detection silently empty
- The dashboard's area/province/region place label now shows `region_code` verbatim instead of cosmetically reformatting it, so an operator can see at a glance, character for character, whether `EPISODIC_PC_PROVINCE_MAP` resolved the code they expected, without risking a mangled real place name (e.g. the hyphen in "Noord-Holland")

## Fixed

- Opening a cluster from a table row or a "linked to #N" chip now moves the rail's own highlight to match; it used to leave the rail showing whichever cluster was selected before, because the rail deliberately does not re-render on every selection
- Opening the dashboard via a `?cluster=<id>` link no longer crashes with "Can't access reactive value 'url_search' outside of reactive consumer"
- `episodic_demo()` no longer leaves `EPISODIC_CONFIG`/`EPISODIC_DB`/`EPISODIC_GEO_DATA` permanently overwritten for the rest of the R session once it returns


# EpiSODIC 0.10.3

## Fixed

- Email/notification messages for new clusters and run failures are now rendered in `EPISODIC_LANGUAGE`, in all 8 shipped languages, instead of always in English
- The new-clusters email's numeric columns are now horizontally centred, and its dates are formatted the same way the dashboard formats them
- `microsoft365` notifications under the cached-login flow (Option C) now honour `from`: when set, mail is sent from that mailbox via `Microsoft365R::get_business_outlook(shared_mbox_email = from)` instead of always sending as the signed-in account
- Fixed a `grepl(..., ignore.case = TRUE, fixed = TRUE)` warning when matching a Microsoft 365 tenant against cached Azure AD logins; the match is now done case-insensitively without triggering the warning


# EpiSODIC 0.10.2

## New

- `microsoft365` notifications can now reuse an already-cached Azure AD login (e.g. one obtained via `Microsoft365R::get_business_outlook()` or `episodic_setup_microsoft365()`) by tenant, with no `client_id`, `client_secret`, or `from` required


# EpiSODIC 0.10.1

## Fixed

- `episodic_demo()`'s startup message once again mentions "demo account", after an unrelated styling commit reworded it away from that phrase

## Changed

- Redesigned the Settings screen: notification channels now sit in a compact wrapping card grid with a toggle switch per channel instead of full-width stacked blocks, and the read-only detection configuration is grouped by section with readable field labels instead of one dense grid of dotted, all-caps keys
- Documentation updates


# EpiSODIC 0.10.0

## New

- Implemented various ways to send notifications of new clusters: ntfy, SMTP, sendmail, Microsoft 365, Teams (Power Automate Workflow), Slack
- Added `vignette("notifications")` with setup steps per channel
- Added an `is_admin` account property and an in-app Settings screen for managing notification channels, dashboard accounts, and viewing (read-only) detection configuration, without needing server access
- Added `episodic_config_export()` to export the resolved configuration as a zip file, with notification secrets masked by default

## Fixed

- Fixed pkgdown build failure from three undocumented notification topics

## Changed

- Documented every `episodic_default_config.yaml` config key with a short inline explanation
- Every write action now re-resolves the signed-in account against the database immediately before writing (`episodic_auth_refresh_user()`), so deactivating or demoting an account from the Settings screen takes effect on its already-open sessions too, not only on its next sign-in
- An `is_admin` account can no longer revoke its own admin access or deactivate its own account from the Settings screen (self-lockout guard)


# EpiSODIC 0.8.17

## Changed

- Minor UI changes


# EpiSODIC 0.8.16

## Fixed

- Fixed duplicate-key crash when an institution had two care lines: batch insert now keeps one row per key, matching prior per-row behaviour
- Fixed two lattice streams with colliding concatenated grouping keys silently merging, causing one stream to disappear without error

## Changed

- Denominator feed now loads in one batched read/insert/update instead of a query and write per row (1,246 to 2 round trips for 623 rows)
- Removed six dead functions superseded by batched writers or duplicated logic; tests now use `episodic_test_institution()` fixture helper


# EpiSODIC 0.8.15

## Changed

- MEM's `min_seasons` moved from a hard-coded function default into config (`mem.min_seasons`), keeping cron and app in agreement
- Chart axes now use the reading language's number format via `episodic_chart_number_labels()`, fixing Rt misread as e.g. "1.4" vs 1,400

## Fixed

- Removed three unused schema columns (`revoked_at`, `left_at`, `last_login_at`) describing capabilities that never existed
- Removed two unused internal readers and two unused function arguments (`institutions`, `level`) that made call sites pass dead values


# EpiSODIC 0.8.14

## Fixed

- Fixed `n_cases` counting wrong cases for ward/area-level clusters; now uses one shared function, `episodic_db_cases_for_stream_id()`
- Fixed stream muting doing nothing; a run now actually suppresses new detections on muted streams and reports how many were suppressed
- Reconciliation functions no longer swallow database errors via `tryCatch()`; only a genuinely missing stream still falls back safely


# EpiSODIC 0.8.13

## Fixed

- Fixed Farrington detector being silently inactive on <4 years of history; now reports shortfall and `farrington.b` default lowered to 2

## Changed

- Removed unbounded `episodic_reporting_triangle` table; delay curve now derived on the fly from existing case/run data
- `episodic_detection_run` gains a `run_date` column representing the run's business date (distinct from `started_at`)
- Batched remaining per-row database writes in a run; round trips dropped from 1,868 to 203 on a synthetic run
- Streams screen baseline-window queries reduced from 52 per page to 3 via `episodic_baseline_excluded_windows_many()`


# EpiSODIC 0.8.12

## Changed

- `episodic_run_cron()` now batches writes instead of row-by-row, cutting MariaDB round trips from 12,549 to 1,868 on a synthetic run
- Note: chunked `dbBind()` is not a true batch; single multi-row statements via new `episodic_db_write_many()` are what collapse round trips


# EpiSODIC 0.8.11

## Changed

- Pathogen, Archive and dossier similar-clusters screens now derive cluster state in bulk instead of per-cluster, fixing slow MariaDB loads
- Query counts now flat rather than linear in cluster count (e.g. Pathogen screen: 259 to 7 queries on 63 clusters); output unchanged


# EpiSODIC 0.8.10

## Fixed

- Fixed R session crash on MariaDB caused by re-entrant connection use when a scoring closure ran inside `dbExecute()`; now computed eagerly
- Fixed institution ids read back as `integer64` on MariaDB being corrupted to 0 on insert, collapsing institution-level streams together

## Changed

- MariaDB connections now explicitly set `utf8mb4` character encoding instead of relying on server defaults


# EpiSODIC 0.8.9

## Changed

- Reverted 0.8.2's explicit NA-typing of cron-write defaults; column types already enforce this and the scaffolding proved unnecessary


# EpiSODIC 0.8.8

## Changed

- `episodic_run_cron(debug = TRUE)` now logs SQL and bound parameters before each call, aiding the ongoing MariaDB crash investigation


# EpiSODIC 0.8.7

## Changed

- Author list trimmed to its actual contributors and copyright holders: Matthijs Berends, Certe Medical Diagnostics & Advice Foundation, and University Medical Center Groningen (ROR-identified, same format as Certe)
- Every language listing (README, vignettes, roxygen docs) now orders English first, then the rest alphabetically, instead of leading with Dutch
- Geography is documented as operator-supplied throughout, not Netherlands-specific: the "Geographic reference data" section in `vignette("data-format")` is substantially expanded, now also covering `EPISODIC_PC_PROVINCE_MAP` (L4 stream detection) alongside the map data, and the FAQ's geography entry does the same
- `vignette("overview")`'s screen-by-screen section no longer stops at three screens ("a third screen closes the loop") - it now covers all seven (Clusters, Pathogen, Performance, Archive, Streams, Activity, Info)
- `episodic_check_denominators()` and `episodic_check_institution_activity()` are now mentioned alongside `episodic_check_cases()` everywhere the docs discuss checking input data before a run (`vignette("data-format")`, `vignette("deployment")`, `vignette("faq")`, README)

## Fixed

- `care_line`'s `"third"` (tertiary care) value was missing from every column-contract table and roxygen doc listing its allowed values - `episodic_care_lines` already included it, the docs just did not


# EpiSODIC 0.8.6

## Changed

- Reverted 0.8.5's `gc(full = TRUE)` forcing in `episodic_run_cron(debug = TRUE)`: a second lab reproduction, with that forcing in place, showed a `gc()` completing cleanly immediately after `episodic_growth_slope()` and then the crash landing on the very next call, `episodic_spatial_concentration()` - ruling out delayed-onset corruption from an earlier call and pointing at that one call on that specific data instead, which the extra `gc()` calls were never able to help with and were only slowing every run down (~600-1000ms added per stream, a ~5 minute run turning into "10-20 seconds on SQLite" by the lab's own comparison)
- `episodic_spatial_concentration()` (the isolated crash site) no longer uses `table()`/`factor()`, which sort their levels and so invoke locale-aware string collation - not guaranteed safe against a string whose declared encoding does not match its actual bytes, which is exactly what a client/server character-set mismatch can hand back from a MariaDB connection. Counts via `tabulate(match(pc, unique(pc)))` instead, which only ever hashes and byte-compares the strings themselves and sorts nothing but the resulting integer codes
- `episodic_run_cron(debug = TRUE)` now dumps each reconciliation candidate's `pc` values (encoding, validity, raw bytes) immediately before `episodic_spatial_concentration()` runs, in place of the removed `gc()` forcing - cheap (a handful of values at most) and aimed squarely at confirming or ruling out the character-encoding hypothesis above


# EpiSODIC 0.8.5

## Changed

- `episodic_run_cron(debug = TRUE)` now forces a full garbage collection (`gc(full = TRUE)`) immediately after every database round trip inside the per-stream reconciliation loop. A fatal crash from corrupted memory typically surfaces later than its actual cause - whenever R's own, otherwise lazily scheduled, garbage collector happens to stumble on the damage - so forcing one right after each call is what lets the trace log land on the actual culprit instead of a downstream symptom several calls away. Confirmed necessary: a lab reproduction of the ongoing MariaDB crash showed the trace stopping at a different line on each run (once after `episodic_growth_slope()`, once mid-`episodic_app_density()`) despite hitting the exact same stream and data, the signature of exactly this kind of delayed-onset corruption. Noisy and slow; `debug = TRUE` was already meant only for chasing a failure like this, not for routine runs


# EpiSODIC 0.8.4

## Fixed

- `episodic_db_truncate()` against MariaDB/MySQL crashed with `Error: bad_weak_ptr` right after successfully truncating every table. `on.exit(..., add = TRUE)` appends to the end of the exit-handler list, so the handler that restores `FOREIGN_KEY_CHECKS` (registered after the connection's own `on.exit(dbDisconnect(con))`) ran *after* the connection had already been closed - RMariaDB's response to a query against a freed connection is a C++ abort, not a catchable R error. Now restores foreign-key checks explicitly right after truncating, with the `on.exit` handler only as a safety net for the error path, registered with `after = FALSE` so it runs before the disconnect even then


# EpiSODIC 0.8.3

## New

- `episodic_db_truncate()`: empties every EpiSODIC table (including dashboard accounts) while keeping the schema itself, for a hard reset back to "freshly created, no data" without a schema migration. Refuses to run outside an interactive session, and requires typing the database's own name back at a prompt before it deletes anything


# EpiSODIC 0.8.2

## Fixed

- Every cron-side database write function's optional parameters defaulted to a bare, untyped `NA` (R's logical `NA`) rather than the column's own type (`NA_integer_`, `NA_real_`, `NA_character_`). RSQLite coerces this silently; RMariaDB does not treat a logical `NA` bound to a non-logical column the same way as a properly typed one, which is a documented source of crashes rather than a catchable error. Most notably, `episodic_db_detection_insert()`'s `cluster_id` - untyped, and hit on every single detection insert in every run, since a detection is always written before a cluster is attached to it - now defaults to `NA_integer_`


# EpiSODIC 0.8.1

## Changed

- `episodic_run_cron(debug = TRUE)` traces every sub-step inside a stream's own reconciliation (triangle update, MEM/Farrington detection, baseline exclusion, population vector, trend caching, detection insert, and each reconciliation candidate's priority-score components) instead of only the stream as a whole - narrows a crash or a stall down to the exact call inside a single stream's processing, not just which stream


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
