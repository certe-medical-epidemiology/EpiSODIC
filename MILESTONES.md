# EpiSODE, milestones

Read `instruction.md` first; it carries the conventions, hard rules and environment constraints that apply to every milestone below. Section references point into `ARCHITECTURE.md`.

Milestones are sequential. Each has a definition of done that must be met before the next begins.

---

## M1, core engine

**Goal.** A cron run that ingests, detects, reconciles and persists, with no interface at all.

Reconciliation is why this comes first. Neither `certestats` function emits a stable cluster identity, so mapping fresh detections onto persistent clusters is the load-bearing component of the whole system. Section 6 specifies it. Treat it as the hard part, because it is.

**Order of work.** Commit at each step.

1. Package skeleton: DESCRIPTION, LICENSE (GPL-2), .gitignore, README stub.
2. Schema. Generate `inst/sql/schema.sql` in SQLite dialect from section 5, using the type mapping given there. Add a migration runner that creates a fresh database from it. One dialect only; there is no MySQL in this system.
3. Repository layer. Thin DBI functions per entity. Split them explicitly by owner: cron-side writers may upsert, app-side writers may only insert. No SQL anywhere else in the package.
4. `inst/config/pathogen_config.csv`, roughly twenty organisms. Where you lack a defensible literature value for a serial interval or incubation period, leave it blank and set `rt_applicable` to 0. An absent estimate is correct; an invented one is not.
5. Synthetic source. Seasonal baselines over several years, plausible PC4, care line, institution, ward and specialism, with injected outbreaks of known shape (one point source, one propagated) so detectors visibly fire.
6. Ingestion. Define the interface that `certedb::get_diver_data()` will eventually satisfy, with the synthetic generator as the default implementation. Explicit column allow-list, deduplication per patient per episode, writes to `episode_case` and `episode_reporting_triangle`.
7. Lattice. Stream enumeration across L1 to L5 with deterministic `stream_key` hashing, plus the eligibility gate from section 8.
8. Detectors. Wrappers producing a common detection record whatever the source. Start with `same_place`, which needs no baseline and no external package. Wrap `certestats` calls behind `requireNamespace()`.
9. Reconciliation, plus the derived-state function as a pure function.
10. Cron entry point. One transaction per run, writing `episode_detection_run` with host, account, timings, package versions, and the resolved `config_hash` and `config_snapshot`. Configuration is read from `EPISODE_CONFIG`; the package must work with that variable pointing anywhere at all, and must never assume configuration lives inside the package or the repository.

**Reconciliation must satisfy these properties.**

- A cluster extended by a later case keeps its identity
- A cluster split by a case-free gap yields the correct number of clusters
- Two clusters growing together merge, the older identity survives, and neither assessment history is lost
- A backfilled case with an older sample date attaches to the right cluster and resets the case-free clock
- Running the same detection twice creates no new clusters and no duplicate rows
- Runs applied out of order converge to the same state
- A partially failed run followed by a retry leaves no partial state

**Done when.** The package installs, `R CMD check` is clean, one function call builds a SQLite database, generates data, detects, reconciles and writes clusters, and every property above is tested and passing.

**The handoff contract.** The cron's only output is rows in the SQLite file. There are no intermediate files, no exports and no message passing. Whatever the app needs to display must exist as a row when the cron finishes, because the app computes nothing expensive at read time. Design the cron's writes with that in mind: if a panel needs a number, the cron stores the number.

---

## M2, read-only interface

**Goal.** The dossier, rendering real data from the database. No writes, no login.

`episode-mockup.jsx` is the layout and vocabulary reference. Reproduce its structure and its Dutch wording; do not reproduce its React idioms.

**Scope.**

- `run_app()`, Shiny with `bslib`, packaged in the R package
- Certe palette from `certestyle::certe.colours`, with the darker variants for text where the saturated colour fails contrast
- Left rail of open clusters, the dossier, the figures strip, the status trajectory band
- Panels: epidemic curve with the incompleteness zone shaded, multi-year trend with threshold band, denominator and positivity, age and sex, geography, institution or ward, phenotypic resistance profile, line list, detection settings
- The **Duiding fragment engine**: each fragment is an id, a condition function over the cluster object, a narrative slot, and a template with placeholders. Slots fire in a fixed order (magnitude, concentration, denominator, demography, completeness, recommendation). Fragments live in the i18n files. Every fragment records which condition fired
- i18n scaffolding: flat JSON, dotted keys, Dutch default, English fallback, missing keys rendered visibly rather than blank
- Read-only Streams screen displaying the configuration from the latest run's `config_snapshot`, not from the file

**Watch for.** Dutch number agreement in fragments (`1 geval` against `2 gevallen`) needs helpers and golden tests, or you will ship ungrammatical sentences to microbiologists. The app performs cheap reads only; any expensive work belongs in the cron.

**Done when.** The app runs against the M1 synthetic database, every panel renders for every demo cluster, the Duiding reads as fluent Dutch across all fragment paths, and no string is hardcoded.

---

## M3, assessment

**Goal.** The four assessors can classify, and everything they do is recorded.

**Scope.**

- `episode_app_user`, hashing with `sodium::password_store()` and `password_verify()`
- **Passwordless viewer**: the app opens read-only for anyone who reaches it. Login only to classify
- The line list panel is hidden for anonymous viewers, along with its export. Everything else stays visible. Render it as a locked panel explaining that signing in reveals it, not as an absent tab
- Classification write path, append-only `episode_assessment_event`, mandatory rationale
- Derived state wired to the interface, with `episode_cluster_state` written on every transition
- Closure as an explicit act, available on any non-terminal classification, prompted but never gated by the closure criterion
- Review date, snooze, mute with reason and expiry
- Autosave of in-progress rationale text
- Archive screen with search across current and archived clusters, including every cluster ever raised for a given organism
- Activity screen: name, timestamp, action, target, with system runs visually distinct
- Wpg notification fields on confirmed epidemics

**Watch for.** A single R process serves every session, so `Sys.info()` identifies the host account, not the assessor. The login is the only identity source.

**Done when.** Four accounts can classify concurrently without losing work, every action appears in the activity log, an anonymous session can read everything except the line list, and no assessment is ever overwritten. Verify by inspection that the app issues no UPDATE or DELETE statements at all.

---

## M4, reporting

**Goal.** A defensible artefact for medical staff.

**Scope.**

- Parameterised Quarto template rendering self-contained HTML
- `episode_report_render` with the query, the included case IDs, a file hash and a version number. Regeneration creates a version, never an overwrite
- Cron pre-renders for every cluster classified as a possible epidemic or above
- Line-list inclusion decided at render time, independent of the viewer's session state
- Small-count suppression configurable for reports leaving the department

**Done when.** A report renders from a cluster ID alone, is byte-reproducible from its stored parameters, and opens correctly with no network access.

---

## M5, analytical depth

**Goal.** The epidemiology the assessment actually rests on.

**Scope.**

- Rt via `EpiEstim`, serial interval from `episode_pathogen_config`, suppressed entirely where `rt_applicable` is false. Show credible intervals; do not withhold estimates for being wide
- MEM for organisms flagged `mem_applicable`, both as a detector and as the closure criterion for seasonal streams
- Curve-shape classification: point source against propagated, from the case date distribution and `incub_max_days`
- Historical analogue matching on organism, level, size, season and duration
- Baseline feedback: periods classified as confirmed epidemics excluded from subsequent baselines, derived from the assessment events so it updates when a verdict is revised
- Patient-day normalisation at L1 and L2 via `populationOffset`
- Geography through `certegis`, guarded for absence
- Cool-down escape hatch: a closed cluster whose excess materially exceeds what was assessed reopens as Herbeoordeling nodig

**Watch for.** Whether `certestats::detect_farrington()` exposes `populationOffset` and whether `linelist2sts()` fills the `sts` population slot are unverified. Check the source. If neither does, write a local wrapper and record it as an upstream request.

**Done when.** Every analytical panel in the mockup is backed by a real computation, and each has a stated behaviour when its inputs are unavailable.

---

## M6, calibration and performance

**Goal.** Evidence that the system works, and the means to tune it.

**Scope.**

- Prestatie screen: positive predictive value per detector per organism from the stored verdicts, classification distribution, time from first case to detection, detection to first assessment, detection to classification
- Eligibility gate tuned against real signal volume towards roughly ten assessed clusters a month
- Priority score weight calibration, with the density component renormalising where no patient-day denominator exists
- Suppression threshold review across the lattice
- Annual overview suitable for the medical board and the accreditation file

**Watch for.** This milestone needs real data and elapsed time. It cannot be completed in one session, and its outputs are the basis for a publication as well as for tuning.

---

## M7, publication

**Goal.** A stranger clones the repository and runs the whole system in under a minute.

**Scope.**

- Demo mode: synthetic data bundled, no Diver, no credentials, one function call
- Every Certe package dependency verified optional, with a documented fallback
- English i18n complete
- Vignettes: architecture overview, detection and reconciliation, deployment
- README with screenshots, GPL-2 licence, NEWS.md
- Final audit that no instance configuration, credential or patient record has ever entered the git history, including deleted files

**Done when.** A clean checkout on a machine with no Certe access runs the demo, and the git history is verifiably free of anything that should not be public.
