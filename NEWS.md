# EpiSODIC 0.4.0

A release about the seam between your data and EpiSODIC: what it is
called, what it will accept, and what it tells you afterwards. Nothing is
deployed yet, so the renames are hard ones - no deprecation shims.

## Renamed: the data interface speaks in epidemiological objects

The vocabulary for supplying data was data-engineering jargon. You passed
an "ingestion source". Two problems with that. In an infectious-disease
package "ingestion" is a transmission route - for norovirus, ETEC and
*C. difficile*, exactly the organisms the synthetic generator ships,
"ingestion source" reads as the vehicle. And `_source` became wrong
outright once these arguments took data rather than something that
produces data.

Everything user-facing is now named after the epidemiological object,
which is what the database has always called these things: the schema has
`episodic_case`, `episodic_denominator` and `episodic_institution_activity`,
and never said "ingest" anywhere. The R API was the only layer out of step.

| Was | Now |
|---|---|
| `ingest_source =` | `cases =` |
| `denominator_source =` | `denominators =` |
| `institution_activity_source =` | `institution_activity =` |
| `episodic_ingest_columns` | `episodic_case_columns` |
| `episodic_ingest_validate_source()` | `episodic_validate_cases()` |
| `episodic_ingest_source_synthetic()` | `episodic_synthetic_cases()` |
| `episodic_ingest_source_synthetic_calibration()` | `episodic_synthetic_cases_calibration()` |
| `episodic_denominator_source_synthetic()` | `episodic_synthetic_denominators()` |
| `episodic_synthetic_institution_activity_source()` | `episodic_synthetic_institution_activity()` |
| `episodic_resolve_source()` | `episodic_resolve_data()` |

Internal helpers and source files follow the same scheme: `R/ingest_*.R`
became `R/cases.R`, `R/cases_dedup.R`, `R/cases_load.R`,
`R/cases_synthetic.R`, `R/denominators.R` and `R/institution_activity.R`.
`episodic_dedup()` is `episodic_cases_deduplicate()`, `episodic_ingest_run()`
is `episodic_cases_load()`, and so on. The database schema needed no
change at all.

Left deliberately alone: `episodic_synthetic_outbreak_point_source()`.
A point source is a real epidemiological thing, not the `_source` suffix
this release was removing.

## The documentation now says what the code has long done

What you hand to `episodic_run_cron()` is normally a data frame or
tibble. The older framing, in which you wrote a function and passed the
function itself, described a form that is still supported but is no
longer the one to reach for. A function is now documented for what it is
actually good at: producing the data at run time, e.g. a live database
query. `episodic_validate_cases()` accepts either form, resolving a
function before it checks anything.

## Case data: every column's allowed values are specified, and enforced

The help page listed the fifteen columns and left the rest to be
discovered at insert time. Every column now documents its type, whether
`NA` is allowed, and its permitted values.

That gap had been hiding a real bug: `care_line` was documented as
`"hospital"`/`"primary_care"`, when the values are `first`, `second`,
`other` and `unknown`. An extract built by following the help page failed
the database's own `CHECK` constraint partway through a run.

- The three fixed value sets are exported as `episodic_care_lines`,
  `episodic_institution_types` and `episodic_sex_codes`, so a transform
  step can map onto them instead of copying strings out of a help page.
- `episodic_validate_cases()` now enforces the whole contract: the
  allow-listed columns, `source_key` uniqueness, the columns that may
  never be `NA`, the three value sets, both date columns parsing as
  `Date` or `YYYY-MM-DD`, and numeric `age`. It names the offending
  column and the offending values. It reports what your data says without
  changing it, and returns the validated data set invisibly.

## A missing `care_line` means unknown

An absent care line, an R `NA` and a database `NULL` all say the same
thing - we do not know which part of the health system this case came
from - and the schema already has a word for it. Requiring the operator's
extract step to spell out `"unknown"` was asking for a mapping the
package can do itself.

`care_line` now accepts `NA` and is normalised to `"unknown"` as cases
are loaded, before deduplication, so both the case row and the
institution upsert see the value the `NOT NULL` constraint expects. The
denominator feed reads `NA` the same way.

## A run now reports what it actually loaded

Runs recorded whether detection succeeded, and nothing about whether the
data arrived. The worst case was silent: institution activity rows whose
`institution_key` matched no known institution were skipped by design,
and the count was computed and then discarded. An operator whose activity
feed and case feed keyed institutions differently lost every patient-day,
saw a green run, and had no way to find out - L2 detection quietly fell
back to raw counts.

The rule now: **structural problems fail the run, and row-level facts are
counted and reported, but never silently dropped.**

- `episodic_detection_run` gained `n_cases_supplied`,
  `n_cases_deduplicated`, `n_cases_inserted`, `n_denominators_written`,
  `n_activity_supplied`, `n_activity_written` and `n_activity_skipped`.
  Every load step already computed these; they now land somewhere.
- `partial` became a real run status. It had been in the schema's `CHECK`
  constraint from the beginning and nothing ever wrote it. A run that
  completed but skipped rows finishes `partial` rather than `success`.
  Both are complete, usable runs and the dashboard reads from the most
  recent of either; `partial` means go and look at why rows were skipped.
- Skipped activity rows raise a warning naming the count and the
  unmatched keys, so an interactive run says so at the console.
- The denominator and institution activity feeds are validated the way
  cases are: allowed values, dates that parse, numeric counts, and the
  columns that may never be `NA`. Their failures used to surface as raw
  SQLite constraint errors from inside a rolled-back transaction.
- The Activity screen shows, under each run, how many cases arrived and
  how many were new, naming skipped rows when there were any. The status
  strip distinguishes a partial run from a clean one.

Two consequences worth naming. Read-side callers that meant "the latest
usable run" asked for status `success` specifically, so a `partial` run
would have left the dashboard showing an older run's numbers; they now
ask for `episodic_run_statuses_complete`, and `episodic_db_latest_run()`
takes several statuses. And the Activity screen builds its label from the
run status, so `running` and `partial` would have rendered as
`[[activity.action_run_running]]`; both are now translated in all eight
languages, along with the load summary and the partial status strip.

# EpiSODIC 0.3.1

## New: the Pathogen screen

- Added a Pathogen screen to the dashboard, alongside Clusters. Pick an
  pathogen and a period - a surveillance season, the last twelve months,
  the last five years, or an exact date range - and read that pathogen at
  the epidemiological level rather than the operational one: weekly
  incidence for the whole catchment, this period laid over earlier ones
  on a shared week-of-season axis, region-wide Rt, testing volume and
  positivity, age and sex, geography, and the signals that were raised
  while the period ran.
- The Moving Epidemic Method's thresholds are now visible. They have been
  fitted on every run since `mem` was added, and used only to decide
  whether a detector fired; the Pathogen screen draws them against the
  season's own curve, together with the medium/high/very high intensity
  bands, so "has the epidemic started" and "how hard is this season" can
  be read directly. Thresholds for a season are fitted only on the
  seasons before it, never on the season being judged.
- Rt is now also estimated for a pathogen as a whole, not only per
  cluster. Rt is a property of a transmission process, and for something
  like influenza A that process is the region's influenza A rather than
  the cases that happened to reconcile into one ward cluster; estimating
  it on a cluster feeds `EpiEstim` an incidence series that is
  systematically incomplete. The per-cluster panel remains - "is this
  ward outbreak still growing" is a real question - but it is no longer
  the only one on offer.
- The `mem` detector is now documented on the Info screen, alongside the
  three that were already there.

## Corrections to the epidemiological logic

- **Observed versus expected is now recorded.** `episodic_cluster` has
  always carried `expected`, `excess` and `ratio` columns, and both the
  dossier's stat grid and the interpretation engine's magnitude fragments
  read them - but nothing ever wrote them. Every cluster was persisted
  with all three left at `NA`, so the O/E ratio never appeared on a
  dossier or in the rail, and the "well above expectation" fragments
  could not fire. Reconciliation now carries the detections' own
  `expected`/`upperbound` through to the cluster.
- **The priority score now uses its evidence.** Five of its seven
  components were being left at their defaults, most damagingly
  `ratio = n_cases / max(n_cases, 1)`, which is identically 1 for every
  candidate; the ranking that orders the entire assessment queue was
  effectively severity weight times detector agreement. Excess, ratio,
  growth, incidence density and spatial concentration are now all
  computed and passed.
- **A component that cannot be computed no longer drags the score down.**
  Weight renormalisation applied only to the density component; a
  `same_place` or `rare_trigger` cluster, which by construction has no
  baseline and therefore no excess or ratio, was scored zero on both
  while keeping their weight. Ward-level same-place clusters - the ones
  an infection prevention nurse has to act on the same day - were
  systematically outranked by Farrington signals for a purely structural
  reason. The ratio and density components are also now anchored so that
  "exactly as expected" contributes nothing rather than half a score.
- **Doubling time no longer reports growth that is not there.** It was a
  regression of log(*cumulative*) cases on day, which has a positive
  slope for any series at all - a constant incidence still makes the
  cumulative count climb - so every cluster with three cases reported a
  finite doubling time. It is now a Poisson fit to the daily counts, and
  `NA` unless growth is real and fast enough to be measurable within the
  fitted window.
- **Positivity is a rate again.** The dossier's positivity line was the
  *cluster's* case count over the *region's* test count: numerator and
  denominator from different populations, so it tracked cluster size
  rather than test yield and could not answer the one question the panel
  exists for. Both are now counted region-wide over the same week, and
  the series is windowed to the cluster instead of spanning every week a
  denominator was ever supplied - which had left the interpretation
  engine comparing a week years before a cluster began against one long
  after it ended.
- **Reporting lag is measured from the run date, not from the cluster.**
  The incompleteness window was anchored on a cluster's own last case
  day, so a cluster that stopped generating cases weeks ago still had its
  epi-curve tail faded and its final Rt estimates withheld, permanently.
  The number of under-ascertained days was also one short, and a single
  noisy dip in a completion curve could stretch the window to a
  fortnight.
- **Rt no longer starts estimating with no infection history.**
  `EpiEstim` conditions each estimate on the infections before it, and at
  the start of a series there are none recorded - not because none
  occurred, but because the series starts there. The renewal denominator
  is too small and Rt reads high, on the youngest and least-evidenced
  clusters. Windows falling inside one mean serial interval of the series
  start are now dropped.
- **Seasonal clusters can close in summer.** `episodic_mem_status()`
  returned `NULL` off-season, and the closure criterion could not tell
  that apart from "MEM unavailable", so a `mem_applicable` cluster had no
  closure route at all between May and September: every confirmed
  influenza or RSV epidemic stayed open on the rail right through the
  summer. Out of season the calendar answers the question, subject to the
  ordinary case-free interval.
- **MEM evaluates the last complete week**, not the partial one in
  progress, and fits its thresholds only on seasons the case data spans
  end to end - a site whose data begins in January no longer contributes
  a season that is three-quarters structural zeros, which used to drag
  the pre-epidemic threshold down and fire the next winter's epidemic
  start early.
- **Concentration is a share of the cases with a known postcode.**
  Dividing by every case in the cluster diluted the measure by however
  many had no postcode recorded, pushing genuinely localised clusters
  down the queue; a missing postcode is absence of evidence about
  localisation, not evidence of dispersal.
- **The demographic baseline excludes the cluster being compared to it**,
  and every other cluster in its stream. For a rare pathogen whose
  recorded history is largely one cluster, that cluster dominated its own
  baseline and could never be found to have shifted away from it -
  exactly where a demographic shift is most worth surfacing.

## Geography

- The choropleth is cropped to the areas that actually have cases, plus a
  margin of context around them, and each of those areas is labelled with
  its postcode and case count. It used to be framed on the entire
  reference dataset - some four thousand PC4 polygons for the shipped
  Netherlands default - so a five-postcode cluster rendered as a handful
  of tinted specks inside a whole country, at which scale the question
  the panel exists to answer cannot be answered at all.
- The geography panel now takes the full width of the dossier pane rather
  than sharing a row with the demography pyramid, and shows the per-area
  bar breakdown underneath the map rather than instead of it.

## Terminology

- One word per language for the thing being watched, everywhere. English
  now says **pathogen** throughout, never "organism": influenza is not an
  organism, and neither is any of the other viruses this system spends
  most of its winter watching. Six shipped strings said "organism", and
  their translations followed suit - Spanish *organismo*, French
  *organisme*, Hindi *जीव*, Arabic *كائن* - so all of those moved with it,
  to *patógeno*, *pathogène*, *रोगजनक* and *العامل الممرض*. Dutch is
  **verwekker** and German **Erreger**, without exception. `AMR::
  microorganisms` keeps its name: it is the AMR package's own data
  object, and a hard dependency of this one.
- Asserted rather than left to review: `tests/testthat/test-i18n.R` now
  fails if any shipped language calls the concept an organism again, if
  Dutch renders it "pathogeen" in any casing, or if a language's
  navigation entry and screen title stop agreeing with each other.

## Interface

- Chart text is legible. Axis labels were set in `faint`, a hairline
  colour meant for rules and separators, at 9pt - which on a surveillance
  chart is not a cosmetic problem, since the axis is how you name the
  week a rise started, and an axis you have to lean in to read is one you
  stop reading. Axis labels, axis titles and legend text are now 11pt in
  the same greys the rest of the interface uses for secondary text, from
  one shared definition so the three cannot drift apart.
- The season-over-season overlay stops the period in progress at the
  current week instead of running it flat along zero to the end of the
  year. Every period is zero-filled across its whole span, which is right
  for a finished one - a quiet week there really did have no cases - but
  for the period still running it drew a long horizontal line through
  weeks that have not happened, saying "no cases" where the truth is "not
  observed". On a chart whose purpose is comparing this period's shape
  against earlier ones, that line was the most prominent mark on it.
- Dutch says **casus**, never *geval*, throughout - 27 strings, including
  the two where the switch changes the grammar around it (*het geval* is
  neuter, *de casus* is not, so "elk gerapporteerd geval" becomes "elke
  gerapporteerde casus").
- `case_free_days` was showing as a raw column name in the detection
  settings table, in every language. It is now translated.

- The Archive links through too, and gained a cluster id column of its
  own. Last winter's assessment is only a useful precedent if you can
  open it and read the reasoning; until now the archive named clusters it
  gave no way in to. Both cluster tables build their rows from one
  helper, so the id column, the click target and the keyboard handling
  cannot drift apart.
- The top navigation is rendered from the view the server is actually
  showing, rather than each link updating itself when clicked. The
  highlight has to follow every way the view can change, and clicking a
  row in the Pathogen screen's cluster table is one of them: it moved the
  content to the Clusters screen and left the highlight sitting on
  Pathogen.
- The Pathogen screen's cluster table links through. Selecting a row -
  by click or by keyboard, since a table row carries no focus of its own
  - opens that cluster's dossier on the Clusters screen. The table is
  also named for what it holds: **Clusters in this period**, not
  "signals". A signal is a detection; these are the reconciled clusters
  those detections became, each carrying a verdict and a state.

  The rail's auto-selection had to be corrected for this to be honest.
  It reset the selection whenever the selected cluster was not currently
  *open*, and most of the clusters this table lists are closed - so every
  such link would have quietly landed on the top of the rail instead. It
  now only re-selects when nothing is selected or when the selected
  cluster has been merged into another, which is the case where the
  dossier really is stale. A cluster that closes while you are reading it
  no longer disappears out from under you either.
- Cluster ids are readable now. The id sits beside the pathogen name in
  the dossier title - *Salmonella* #300, muted and upright against the
  italicised taxon - rather than a line further down among the metadata,
  and it leads every row of the Pathogen screen's signals table, which
  previously listed clusters without ever naming one. It is what an
  assessor quotes in an email and searches the archive by, so it belongs
  where the eye lands. Each language marks the reference its own way
  (`n.º`, `n°`, `Nr.`, `رقم`), which is why it stays a translation key.
- Weekly charts label weeks. `ggplot2`'s default date scale put two ticks
  - "Oct" and "Jan" - across a whole quarter of weekly bars, which is not
  enough to name the week a rise started in. The epidemic curve, the
  multi-year trend and the positivity chart now carry the ISO week number
  over the month it falls in, with the year stated once and again only
  when it turns, thinning to month-and-year once the span passes about
  eighteen months. Week and month are both read off the week's Thursday,
  the day that decides which ISO year a week belongs to, so the week
  beginning 30 December 2024 reads "w01 / Jan 2025" rather than
  contradicting itself.
- The number beside each pathogen in the picker now says what it counts.
  "Norovirus (2197)" reads as an identifier; it is an all-time case
  count, and it is the number that decides whether a pathogen has enough
  history for the screen to say anything at all.
- The top navigation now shows which screen you are on. The stylesheet
  has always had a rule for it; nothing ever applied the class.

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
