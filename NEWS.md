# EpiSODIC 0.5.0

## New

- `episodic_check_cases()` reports everything wrong with your case data at once - the column, how many
  rows, which rows, the offending values, and the fix for each - without a database and without changing
  anything. It separates problems, which stop a run, from advice, which does not: one pathogen spelled two
  ways, no `ward` on any hospital row, a `patient_key` that never repeats, postcodes the map cannot place.
  A wrong date format is named for what it is (day-first, date-time, Excel serial, `YYYYMMDD`) with the
  conversion to apply, and an unexpected column is matched against what it probably should have been.
- Lattice suppression, described by `config$suppression` and the reconciliation vignette since 0.3.0 and
  implemented by nothing: one outbreak seen at five levels became five dossiers. A child cluster now
  suppresses its parent when it holds most of the parent's cases; a parent suppresses its children when
  the rise is spread across several with none dominant. Nothing is discarded - a suppressed cluster keeps
  its cases, history and assessment, and appears on the surviving cluster's dossier. An assessed cluster is
  never suppressed, and it is recomputed every run.
- Clusters that share cases but stand separately - suppression works within a containment chain, not across
  the two, so a regional rise never hides the ward outbreak feeding it - now say so. The dossier header
  carries a "Linked to #123" chip that opens the other cluster, and a Related clusters panel lists both what
  this cluster suppressed and what it merely overlaps.
- `episodic_case_data` documents every column's type, whether it may be empty and what it accepts in one
  table, with a worked example of a minimal valid extract.

## Changed

- `episodic_validate_cases()` reports every problem in one error instead of stopping at the first.
- `episodic_run_cron()` checks the case and denominator feeds before writing anything and stops if they
  cannot be used, rather than returning quietly with a `failed` run row nobody was looking at. A run that
  fails for any other reason warns, and an empty extract warns before the run starts.
- The status strip says why the last run failed; the Activity screen shows each run's first line with a
  Details button opening the whole message, its per-feed counts and its provenance.
- Farrington tests every week back to the last completed run, capped by `farrington$max_weeks_tested` (8),
  instead of only the week the run fell in - an outage no longer leaves weeks nothing ever tested.
- `config$effect_size_floor`, previously read by nothing, now gates a candidate opening a new cluster: a
  statistical signal too close to its own upperbound is a true alarm and not a dossier. Candidates matching
  an open cluster still extend it; `same_place` and `rare_trigger` are unaffected.
- `episodic_synthetic_cases()` injects six outbreaks sized from one case to a regional wave, over a
  baseline thin enough per place that coincidence no longer trips the rule-based detectors - the demo
  opened on some three hundred chance clusters before. Patients recur, so deduplication has something to
  do; the feeds default to the five years up to today; `episodic_demo()` gained `run_date`.
- Every `lang` argument defaults to `EPISODIC_LANGUAGE`, as the entry points always did. Eighty-four
  internal renderers defaulted to `"nl"`, so anything reached through a default came out Dutch whatever the
  instance had asked for. Unset means English. A test fails if a hardcoded default creeps back.
- An empty string in a column that must always be filled is a problem, like `NA`.

## Fixed

- Only lattice enumeration knew how a case maps to a region code, so every `pathogen_area` and
  `pathogen_province` stream was handed the whole catchment and reported the region's counts under its own
  name. The rule now lives in one place and both halves of the lattice use it.
- A cluster's cases were selected by pathogen, window and institution alone, so a ward cluster was linked
  to every case in the building and an area cluster to every case in the catchment - which is what the
  dossier's line list showed.
- Reconciliation matched each candidate against the clusters the run *started* with, so a cluster kept
  advertising its original last day and, once that was `case_free_days` behind, the next week of the same
  outbreak opened a second dossier - and a third, every 21 days.
- Translation placeholders are substituted literally, so a value carrying a backslash can no longer rewrite
  the sentence around it.
- The epidemic curve's thousands separator followed the language only when one was passed explicitly.

# EpiSODIC 0.4.0

## Changed

- Renamed the data interface after epidemiological objects. "Ingestion" is a transmission route, and
  `_source` was wrong once these arguments took data rather than a producer of it. No deprecation shims.
- Internals follow: `R/ingest_*.R` are now `R/cases*.R`, `R/denominators.R`, `R/institution_activity.R`;
  `episodic_dedup()` is `episodic_cases_deduplicate()`; `episodic_ingest_run()` is `episodic_cases_load()`.
  `episodic_synthetic_outbreak_point_source()` keeps its name - a point source is an epidemiological term.
- Documented data frames and tibbles as the normal input. A zero-argument function is still accepted, for
  data that only exists at run time.
- `care_line` accepts `NA`, normalised to `"unknown"` on load. Same in the denominator feed.
- Corrected `care_line`'s documented values, which were `"hospital"`/`"primary_care"`. They are `first`,
  `second`, `other`, `unknown` - an extract built from the old help page failed the database `CHECK`.
- A run that skipped rows now finishes `partial`, not `success`. Both are usable; the dashboard reads the
  most recent of either. `episodic_db_latest_run()` takes several statuses.
- Skipped institution activity rows warn, naming the count and unmatched keys, instead of passing silently.
- Denominators and institution activity are validated like cases; their failures used to surface as raw
  SQLite constraint errors from inside a rolled-back transaction.

## New

- Every case column documents its type, whether `NA` is allowed, and its permitted values.
- Exported `episodic_care_lines`, `episodic_institution_types` and `episodic_sex_codes`, to map onto in an
  extract step.
- `episodic_validate_cases()` enforces the whole contract: allow-listed columns, `source_key` uniqueness,
  never-`NA` columns, the three value sets, dates parsing as `Date` or `YYYY-MM-DD`, numeric `age`. It
  names the offending column and values, and does not alter the data.
- `episodic_detection_run` records what each feed delivered: `n_cases_supplied`, `n_cases_deduplicated`,
  `n_cases_inserted`, `n_denominators_written`, `n_activity_supplied`, `n_activity_written`,
  `n_activity_skipped`.
- The Activity screen shows, per run, how many cases arrived and how many were new, naming skipped rows.
  The status strip distinguishes a partial run.

## Fixed

- Institution activity rows with an unmatched `institution_key` were skipped and the count discarded. An
  operator whose feeds keyed institutions differently lost every patient-day and saw a green run.
- Run statuses `running` and `partial` had no translation and rendered as `[[activity.action_run_running]]`.
  Both are now translated in all eight languages, with the load summary and partial status strip.

# EpiSODIC 0.3.1

## New

- Pathogen screen, alongside Clusters: pick a pathogen and a period, and read weekly incidence, seasons on
  a shared week-of-season axis, region-wide Rt, positivity, age and sex, geography, and signals raised.
- MEM thresholds are drawn against the season's curve with intensity bands, so "has the epidemic started"
  and "how hard is this season" can be read directly. A season is fitted only on the seasons before it.
- Rt is estimated per pathogen, not only per cluster. A cluster's incidence series is systematically
  incomplete for a process that is region-wide. The per-cluster panel remains.
- The `mem` detector is documented on the Info screen.

## Fixed

- `expected`, `excess` and `ratio` were never written. Every cluster persisted them as `NA`, so the O/E
  ratio never appeared and the magnitude fragments could not fire. Reconciliation now carries them through.
- Five of the priority score's seven components sat at their defaults, `ratio` identically 1. The queue
  ranking was severity weight times detector agreement. All components are now computed.
- A component that cannot be computed no longer drags the score down. `same_place` and `rare_trigger`
  clusters have no baseline, scored zero on excess and ratio, and kept the weight - so ward-level clusters
  were outranked structurally. Ratio and density are also anchored so "as expected" contributes nothing.
- Doubling time regressed log(*cumulative*) cases, which climbs for any series at all, so every three-case
  cluster reported a finite doubling time. Now a Poisson fit to daily counts, `NA` unless growth is real.
- Positivity was the cluster's cases over the region's tests. Both are now region-wide over the same week,
  windowed to the cluster rather than every week a denominator was ever supplied.
- Reporting lag is measured from the run date, not the cluster's last case day, which permanently faded the
  epi-curve tail of any cluster that had stopped. The under-ascertained day count was also one short.
- Rt no longer estimates windows falling inside one serial interval of the series start, where the renewal
  denominator is too small and Rt reads high on the youngest clusters.
- `episodic_mem_status()` returned `NULL` off-season, indistinguishable from "MEM unavailable", so
  `mem_applicable` clusters had no closure route between May and September. The calendar now answers.
- MEM evaluates the last complete week, and fits only on seasons the data spans end to end.
- Concentration is a share of cases with a known postcode. Dividing by all cases diluted localised clusters.
- The demographic baseline excludes the cluster compared to it, and others in its stream. A rare pathogen's
  one cluster dominated its own baseline and could never be found to have shifted.
- The top navigation shows which screen you are on; the stylesheet rule was never applied.

## Changed

- The choropleth is cropped to areas with cases plus context, each labelled with postcode and count. It was
  framed on the whole reference dataset - four thousand PC4 polygons for the Netherlands default.
- Geography takes the full dossier width, with the per-area bars under the map rather than instead of it.
- English says **pathogen**, never "organism"; translations followed (*patógeno*, *pathogène*, *रोगजनक*,
  *العامل الممرض*). Dutch is **verwekker**, German **Erreger**. `AMR::microorganisms` keeps its name.
- `tests/testthat/test-i18n.R` fails if a language calls it an organism again, if Dutch renders "pathogeen",
  or if a navigation entry and screen title stop agreeing.
- Axis labels, axis titles and legend text are 11pt in secondary greys, from one definition. They were 9pt
  in `faint`, a hairline colour meant for rules.
- The season overlay stops the period in progress at the current week, instead of running flat along zero
  through weeks that have not happened.
- Dutch says **casus**, never *geval* - 27 strings, including the two where the gender changes the grammar.
- `case_free_days` was showing as a raw column name in every language.
- Weekly charts carry the ISO week number over its month, the year stated once and again when it turns,
  thinning past ~18 months. Both read off the week's Thursday, so 30 December 2024 reads "w01 / Jan 2025".
- Cluster ids sit beside the pathogen name in the dossier title and lead every Pathogen screen row. Each
  language marks the reference its own way (`n.º`, `n°`, `Nr.`, `رقم`).
- The Pathogen screen's cluster table links through, by click or keyboard, and is named **Clusters in this
  period** rather than "signals". The rail no longer resets the selection for a cluster that is not open,
  which would have landed every such link on the top of the rail.
- The Archive links through and gained a cluster id column. Both cluster tables build rows from one helper.
- The top navigation renders from the view the server is showing, so it follows a link out of the Pathogen
  screen rather than staying behind.
- The pathogen picker's count is labelled: it is an all-time case count, which decides whether a pathogen
  has enough history for the screen.

# EpiSODIC 0.3.0

## New

- Dashboard translations for Spanish, French, German, Mandarin Chinese, Hindi and Modern Standard Arabic
  alongside Dutch and English. Pass as `lang` to `episodic_run_app()`, `episodic_demo()` or
  `episodic_report_render()`. `episodic_format_date_range()` formats month names from the same files.
- `EPISODIC_LANGUAGE` sets the default `lang` for those functions and `episodic_tr()`. Falls back to `"en"`.

# EpiSODIC 0.2.0

## Changed

- Rewrote all documentation for an epidemiologist audience rather than a contributor reading the source.
  Related functions share a help page, and examples show their output.
- Renamed the internal `episode_`/`episode-` prefix to `episodic_`/`episodic-` throughout - functions,
  tables, CSS classes - to match the package name.

## New

- `EPISODIC_DB` accepts a `mysql://` DSN (`episodic_db_dsn_mariadb()`, `episodic_db_dsn_mysql()`) to run
  against MariaDB/MySQL from the same schema file.
- Package version shown in the app header.
- README screenshots ship in `man/figures/` so they render on CRAN.

# EpiSODIC 0.1.0

First development version.

- Detection: lattice enumeration, five detectors (`farrington`, `ears`, `same_place`, `rare_trigger`,
  `mem`), cluster reconciliation across runs, cross-lattice suppression, baseline feedback.
- Interface: bilingual (NL/EN) Shiny app, read-only for anonymous visitors; dossier view (epi curve, trend,
  geography, Rt, line list); audit-trailed classification workflow; Performance screen.
- Reporting: versioned Quarto outbreak reports with small-count suppression.
- SQLite backend; optional `EpiEstim`/`mem`/`quarto`/`sf`, each with a documented fallback.
- `episodic_demo()` runs the whole system against bundled synthetic data.
