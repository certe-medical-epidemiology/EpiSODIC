# EpiSODIC 0.8.16

## Fixed

- A run against MariaDB rolled back on `Duplicate entry '...' for key 'institution_key'` while loading case data. Institutions were gathered as distinct *combinations* of key, name, type, care line and municipality, but an institution is keyed on `institution_key` alone - so a hospital reporting both a second- and a third-line care line, which nothing in the data contract forbids, appeared twice in the same batch. While institutions were written one at a time this was invisible, each row being its own statement and the second updating what the first inserted; batched into one `INSERT` it is a duplicate on a `UNIQUE` index, and the run died before a single case was written. The batch now carries one row per key - the last, which is what the loop it replaced ended on and what `episodic_check_cases()` already tells an operator to expect
- Two lattice streams whose grouping fields happened to concatenate to the same string were silently merged into one. The grouping key pasted the columns together with no separator, so pathogen `Flu A` in area `12` and `Flu A1` in area `2` were one group - and whichever of them was not the group's first row simply stopped existing, with no error and nothing on screen to say a stream had gone. A surveillance system may report nothing; it may not lose a stream quietly

## Changed

- The denominator feed is loaded in a batch like everything else: one read bounded by the feed's own date span, one insert, and an update only where a count actually moved - instead of a `SELECT` plus a write per row, which for a 623-row feed was 1,246 round trips. Re-sending a feed unchanged now writes nothing at all, and `n_written` says so rather than always echoing the row count back. The match is made in R rather than by an upsert on the table's unique index, and has to be: `area_code` is nullable, `NULL` is not equal to `NULL` in SQL, and every row without an area stratum would therefore insert afresh on every run, for ever
- Removed six more functions that nothing but their own tests called, all of them left behind by work that replaced them: `episodic_db_denominator_upsert()` and `episodic_db_institution_upsert()` (superseded by the batched writers), `episodic_db_cluster_case_link()` (a one-line alias for `episodic_db_cluster_case_link_many()`), `episodic_db_institution_get()`, `episodic_auth_last_login()` (the R-side twin of the `last_login_at` column dropped in 0.8.15, displayed nowhere), and `episodic_interpretation_paragraphs()`/`episodic_interpretation_recommendation()`, which duplicated a split the dossier panel already makes inline - and each re-ran generation to do it, so using both would have generated the same interpretation twice. Every test that exercised them now goes through the path a run actually takes; institutions in test fixtures are built by `episodic_test_institution()`, which hashes the key the same way a run does instead of each test hashing its own

# EpiSODIC 0.8.15

## Changed

- MEM's season requirement moves into the configuration file. `episodic_detect_mem()`, `episodic_mem_status()` and `episodic_mem_thresholds_for_season()` each hard-coded `min_seasons = 2L` as a function default that no caller ever passed, so the one dial deciding whether the seasonal detector can speak at all lived in the source rather than beside `farrington.b` - the same class of dial, with the same consequence when set beyond what an instance carries. It is now `mem.min_seasons`, read in all three places, so the cron and the app cannot end up disagreeing about how much history MEM needs: raising it can no longer silence the detector while the activity screen goes on drawing thresholds fitted on fewer seasons than the detector will trust
- Chart axes are formatted in the reading language's own number conventions, not only the epidemic curve's. `episodic_ui_rt_chart()` took a `lang` argument and used it for nothing, so an Rt of 1.4 rendered as "1.4" for a Dutch reader, who reads that as fourteen hundred - on the one chart whose entire point is which side of 1 a value falls. Both charts now share `episodic_chart_number_labels()`

## Fixed

- Removed three columns nothing has ever written: `episodic_stream_mute.revoked_at`, `episodic_cluster_state.left_at` and `episodic_app_user.last_login_at`. Each described a capability that does not exist. `revoked_at` implied a mute could be ended early - there is no revoke path anywhere, and a mute is bounded when it is written, so the `revoked_at IS NULL` predicate the new mute query carried was always true. `left_at` duplicated what an append-only trajectory already says: a state ends where the next row begins. `last_login_at` was superseded by `episodic_app_user_event`, as that table's own comment already explained. The schema now says so where each column stood
- Removed two internal readers nothing called (`episodic_db_detections_for_run()`, `episodic_db_stream_mutes()`) and two arguments nothing read: `episodic_detect_rare_trigger()`'s `institutions`, documented as kept "for signature parity", and `episodic_app_concentration()`'s `level`, documented as kept so callers "stay explicit" - both made call sites pass values that did nothing. `episodic_app_server_assessment_actions()` also no longer takes a `lang` it never used, that module having no user-facing text at all. The `(input, output, session)` triple stays on all three server fragments, used or not, because that is the Shiny signature and a future observer in any of them needs it

# EpiSODIC 0.8.14

## Fixed

- A cluster's `n_cases` counted the wrong cases for any stream narrower than an institution. `episodic_reconcile_case_count()` recounted on pathogen and institution alone, so a ward cluster was recorded as holding every case in the hospital and an area cluster every case of that pathogen anywhere - while `episodic_reconcile_link_cases()`, which builds the same cluster's line list, correctly applied the ward and region filters. The two disagreed, and `n_cases` is not cosmetic: it feeds `ratio = n_cases / expected`, and so the priority score and the order of the assessment queue. Both now go through one function, `episodic_db_cases_for_stream_id()`, which is the single place stream membership is defined; a run asserts that every cluster's `n_cases` equals its own linked line list
- Muting a stream did nothing. The app offers the action and tells the user it "temporarily suppresses new detections for this stream ... so the same cause is not flagged again and again"; the mute was written to `episodic_stream_mute`, shown in the activity log, and read by nothing that decides anything, so the next run opened a dossier on the muted stream exactly as before. A stream under an unrevoked mute covering the run date now produces no new detections from any detector. Deliberately detections only: clusters already open go on ageing and closing normally, because a mute quiets what is coming rather than freezing what is already on the board. A run reports how many streams it suppressed
- `episodic_reconcile_case_count()` and `episodic_reconcile_link_cases()` no longer swallow database errors. Both wrapped their whole body in a `tryCatch()` that turned any failure into a plausible-looking fallback - a count taken from the detector's own reckoning, or a cluster with no line list at all - so a connection that failed mid-run committed wrong numbers under a run that reported success. Only the documented case, a stream that does not exist, still falls back; anything else propagates and rolls the run back

# EpiSODIC 0.8.13

## Fixed

- Farrington was silently switched off. It needs `(b + 1) * 52` weeks of history before `surveillance::farringtonFlexible()` will fit at all, and at the shipped `b: 3` that is four years - so an instance carrying two years of data got nothing from it on every stream, indistinguishable on screen from a detector that had looked and found nothing. For a surveillance system that is the wrong silence: three of the four detectors appeared to be watching streams that only one was. The guard now reports the shortfall instead of returning empty. A run says once how many eligible streams could not be fitted and how much history they would need, and the Streams screen carries a per-stream `Farrington` column reading `weeks of history / weeks required` (a dash when the detector is running). `farrington.b` drops to `2`; raise it as history accrues and the detector starts fitting again by itself. Note that Farrington never required `denominators` - it falls back to raw counts when no population vector is available, and always did

## Changed

- `episodic_reporting_triangle` is gone, and the reporting-delay curve is derived instead. The table was redundant: `episodic_case.first_seen_run` records the run that first saw each case and `episodic_detection_run` now records each run's own `run_date`, which together are exactly "how many cases with sample date D were visible at run R". It was also the most expensive thing in a run and grew without bound - the cron rewrote every stream's whole history on every run, so five consecutive runs over unchanged data took the table from 3,472 rows to 17,360 while only ~11% of those rows fell within the 21-day lag its one reader ever looked at. Nightly for a year that is over a million rows nothing reads. `episodic_triangle_completeness()` keeps its signature and its output; verified identical across 622 streams on a five-run fixture with real reporting lag
- `episodic_detection_run` gains a `run_date` column: the run's own business date, which is nearly always the date part of `started_at` but not by construction - a backfill or a replay names the date it stands in for
- The remaining per-row database traffic in a run is batched: cluster-case links, the pathogen configuration load, and a reconciliation call that is now skipped entirely for streams with neither candidates nor open clusters. Together with the triangle, a synthetic run goes from 1,868 round trips to **203**, and five consecutive runs over unchanged data now grow only the run and detection audit trail
- The Streams screen no longer issues one query per stream on the page for its excluded-baseline windows: **52 queries per page to 3**. `episodic_baseline_excluded_windows_many()` answers for a whole page in two

# EpiSODIC 0.8.12

## Changed

- `episodic_run_cron()` now writes in batches rather than a row at a time, which is what made a run against a networked MariaDB take minutes where the same run against a local SQLite file takes seconds. The reporting-triangle update (one `SELECT` plus an `INSERT` or `UPDATE` per distinct sample date per stream - by far the hottest write in a run, twelve seconds for a single large stream), the case insert, institution resolution and lattice-stream enumeration all now send one statement per batch instead of one per row. `episodic_reconcile_stream()` also no longer reads a stream's open clusters at a point where the value is always overwritten before anything reads it, and `episodic_suppress_lattice()` clears last run's suppression in one statement rather than one per cluster. On a 950-case, 605-stream synthetic run this takes the number of database round trips from 12,549 to 1,868; every one of the nineteen tables is byte-identical afterwards, ids included
- Note for anyone extending the write layer: chunked `DBI::dbBind()` looks like a batch and is not one. RMariaDB binds a multi-row parameter list with `while (bind_next_row()) { execute(); }`, one `mysql_stmt_execute()` per row, so it saves the statement parse and pays every round trip regardless. What actually collapses them is a single statement carrying every row's placeholders, which is what the new internal `episodic_db_write_many()` builds (with `ON CONFLICT`/`ON DUPLICATE KEY UPDATE` per dialect where an upsert is wanted)

# EpiSODIC 0.8.11

## Changed

- The Pathogen and Archive screens and the dossier's similar-clusters panel now derive cluster state in bulk instead of one cluster at a time, which is what made them slow to open against a MariaDB database (and invisible against a local SQLite file). `episodic_app_derive_state_for_cluster()` costs five or six queries per cluster - it re-reads the cluster, its stream, the pathogen configuration, its assessment events and its state history one row at a time, and for a `mem_applicable` pathogen re-reads every case for that pathogen as well - and all three screens called it in a loop. The Pathogen screen additionally fetched each cluster's assessment events itself and then called the function that fetches them again. `episodic_app_derive_states_batch()` and the `_batch()` readers it uses already existed and were already in production on the Clusters rail; these three call sites just never adopted them. Query counts are now flat in the number of clusters rather than linear: on a 63-cluster pathogen, the Pathogen screen goes from 259 queries to 7, the Archive from 330 to 7, and a dossier's similar-clusters panel from 195 to 10. Output is unchanged - state labels, verdict labels, ordering and closed-on dates all verified identical against the previous implementation

# EpiSODIC 0.8.10

## Fixed

- `episodic_run_cron()` no longer crashes the R session partway through a run against MariaDB. The cause was re-entrancy, not data: `episodic_reconcile_stream()` passed `priority_score_fn(candidate)` straight into `episodic_db_cluster_insert()`/`episodic_db_cluster_update()` as an argument, and an R argument is a promise - so the scoring closure did not run at the call site but inside `dbExecute()`, after the driver had already prepared the INSERT/UPDATE on the connection. The closure then queried that same connection (via `episodic_app_density()`), RMariaDB cancelled and freed the pending statement, and `dbBind()` - which, unlike `dbFetch()`, never checks that its result is still active - bound its parameters into freed memory. That is a native crash, so no condition was ever raised and `tryCatch()` never saw anything; the session simply died. SQLite was never affected because RSQLite permits concurrent results on one connection. The score is now computed into a local before any statement is opened, and every write helper in `R/db_cron_write.R` and `R/db_app_write.R` builds its `params` list into a local first rather than inlining it as an argument, so no caller-supplied value can be evaluated mid-statement again. A six-line reproduction, independent of EpiSODIC, is recorded in the reconciliation tests' comments
- Institution ids read back after an `INSERT` are now plain integers on both backends. MariaDB's `LAST_INSERT_ID()` is a `BIGINT`, which RMariaDB returns as a `bit64::integer64` - physically a double carrying the integer's *bit pattern*, not its value - and `episodic_institutions_resolve()` assigned that into an ordinary `integer()` vector, which drops the class and keeps the payload. Every institution id so obtained silently became a subnormal double (institution 368 became 1.8e-321) and was then written into `episodic_case.institution_id`, an `INTEGER` column, as **0**. Every case loaded into a MariaDB database therefore pointed at a non-existent institution 0, collapsing institution-level streams together; SQLite, whose `last_insert_rowid()` is a plain numeric, was correct throughout. `episodic_db_last_insert_id()` now coerces and range-checks, and MariaDB connections are opened with `bigint = "integer"` so no `integer64` enters the package at all. **Existing MariaDB databases carry the bad ids already written and need their `episodic_case.institution_id` values re-linked; a fresh load after this release is correct**

## Changed

- MariaDB connections now declare their character set explicitly (`SET NAMES utf8mb4`) instead of inheriting whatever the server's `my.cnf` happens to configure, so what comes back is the same on every machine that connects

# EpiSODIC 0.8.9

## Changed

- Reverted 0.8.2's explicit `NA_integer_`/`NA_real_`/`NA_character_` typing of every cron-write function's optional defaults. The column types themselves already constrain this on both SQLite and MariaDB; the typing was defensive scaffolding added while chasing an unrelated MariaDB-only crash and turned out not to be needed by either backend

# EpiSODIC 0.8.8

## Changed

- `episodic_run_cron(debug = TRUE)` now prints the exact SQL and every bound parameter's value, class and encoding immediately before each database call the ongoing MariaDB crash investigation has implicated so far - `episodic_app_density()`'s two queries, the population-vector lookup, the trend/detection writes, and the assessment-event lookups - rather than only reporting once the call has returned. The crash has now landed on three different calls across three otherwise-identical reproductions on the exact same stream and candidate, so seeing precisely what was sent (not just where the trace stopped) is the next step in pinning it down as an EpiSODIC-side issue rather than a driver one

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
