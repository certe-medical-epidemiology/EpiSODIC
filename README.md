# EpiSODE

**Epidemiological Signal Observation and Detection Engine**

EpiSODE is an outbreak cluster detection and assessment system for a
department of medical epidemiology. It ingests laboratory-confirmed
infections, detects statistical and rule-based aberrations, reconciles them
into persistent clusters, and gives a small board of epidemiologists a
dossier to assess each one, with a full audit trail and outbreak reports for
clinical colleagues.

The engine and the instance it runs against are kept separate: this
repository is open-source software with no data and no site-specific
configuration. See `ARCHITECTURE.md` for the full design and `MILESTONES.md`
for the build plan.

## Status

Under active development. Milestone 1 (core detection engine, no interface)
is in progress.

## Installation

```r
# install.packages("remotes")
remotes::install_github("certe-medical-epidemiology/episode")
```

Certe-internal packages (`certestats`, `certedb`, `certegis`, `certeplot2`,
`certestyle`) are optional. Without them, EpiSODE runs entirely on its
bundled synthetic demo data using open-source detectors and backends.

## Licence

GPL-2, see `LICENSE`.
