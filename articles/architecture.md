# Architecture overview

## What EpiSODIC is

EpiSODIC (**Epi**demiological **S**ignal **O**bservation, **D**etection,
**I**dentification, and **C**lassification) is an R package: an outbreak
cluster detection and assessment system for infectious disease
epidemiologists. It reads laboratory-confirmed infections, runs
statistical and rule-based detectors over them, reconciles raised
signals into persistent clusters across repeated runs, and gives a small
board of epidemiologists a dossier to formally assess each one - with a
full audit trail and outbreak reports for clinical colleagues (e.g.,
clinical microbiologists) and infection prevention nurses at the end.

This is a narrative summary of the system’s design, covering the parts
that matter most for understanding how it works day to day.

## Engine and instance are separate

This package is open-source software with no data and no site-specific
configuration in it. A running instance - a real organisation’s
database, its `EPISODIC_CONFIG` overrides, its provisioned accounts -
lives entirely outside the package, addressed by environment variables
(`EPISODIC_DB`, `EPISODIC_CONFIG`, `EPISODIC_GEO_DATA`, …; see the
README’s “Environment variables” table for the full list). Nothing about
a specific laboratory, hospital, or patient can enter this repository,
because the repository never holds any of it.

## Topology

Two entry points, one database:

``` r

# The cron entry point - typically scheduled, e.g. nightly
cases <- my_extract_and_transform_function()   # a data frame or tibble

episodic_run_cron(
  db_path = "/path/to/episodic.sqlite",
  cases = cases
)

# The interactive entry point - a Shiny app, aggregate data only for
# anonymous visitors; signing in unlocks patient-level detail for both
# roles, and only the "epidemiologist" role can also classify
episodic_run_app(db_path = "/path/to/episodic.sqlite")
```

[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
does the expensive work: loading case data, stream enumeration,
detection, reconciliation, all inside one transaction, so a partially
failed run leaves no partial state and a retry is always safe.
[`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md)
only ever performs cheap reads against already-computed results, plus a
small set of insert-only write actions (classify, close, mute) gated
behind authentication. This split keeps the interactive app fast
regardless of how expensive detection itself is, and keeps every write
path auditable.

## The data model, briefly

Cases are the only mandatory input: one row per confirmed-positive
laboratory result, deduplicated internally per patient/pathogen/episode.
Everything downstream - streams (the unit a detector watches),
detections (what a detector raised), clusters (what reconciliation
turned detections into across runs), assessment events, and rendered
reports - is derived and stored so that any result is explainable from
the database alone: which run produced it, against which configuration
(hashed and snapshotted), from which raw detections.

Positivity metadata, institution activity (patient-days), and geographic
reference data are all optional, additive inputs - the system runs
completely without any of them, using raw counts and a plain PC
breakdown instead of a normalised trend or a choropleth.

## Where to go next

- [`vignette("detection-reconciliation")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/detection-reconciliation.md)
  for how detectors raise signals and how those signals become
  persistent, trackable clusters across repeated runs.
- [`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)
  for actually standing up an instance: the data contract, environment
  variables, and account provisioning.
