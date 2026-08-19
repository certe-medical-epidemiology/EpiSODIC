# EpiSODE 0.1.0 (development)

Milestone 1 (core engine, no interface): a cron run that ingests,
detects, reconciles and persists.

- SQLite schema (`inst/sql/schema.sql`) and migration runner.
- DBI repository layer split by writer: cron may upsert, app only inserts.
- Synthetic ingestion source with seasonal baselines and two injected
  outbreaks (point source, propagated), the default and only source since
  `certedb::get_diver_data()` is unavailable outside Certe.
- Episode deduplication (one isolate per patient per episode) via
  `AMR::get_episode()`, with a documented fallback when `AMR` is absent.
- Lattice enumeration across L1-L5 with deterministic `stream_key` hashing
  and the eligibility gate.
- `same_place` rule-based detector (no baseline required); `certestats`
  wrappers guarded by `requireNamespace()`.
- Cluster reconciliation: extension, split, merge, backfill, idempotent
  reruns, out-of-order convergence, and transactional all-or-nothing runs.
- Derived cluster state as a pure function.
- Cron entry point (`episode_run_cron()`) writing `episode_detection_run`
  with host, account, timings, package versions, `config_hash` and
  `config_snapshot`.
- `inst/config/pathogen_config.csv` for 22 organisms and
  `inst/config/default.yaml` for shipped detection defaults.

See `QUESTIONS.md` for assumptions adopted along the way, including two
gaps found in `ARCHITECTURE.md` during implementation.
