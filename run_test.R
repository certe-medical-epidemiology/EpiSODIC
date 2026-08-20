library(EpiSODE)

# A persistent path, not a tempfile: the point of manual testing is to
# sign in, classify clusters, close them, mute a stream, generate a
# report - and see that work still be there next time this script runs.
# *.sqlite is already .gitignore'd, so this never gets committed.
db_path <- "episode-demo.sqlite"

Sys.setenv(
  EPISODE_CONFIG = system.file("config", "default.yaml", package = "EpiSODE"),
  EPISODE_DB = db_path,
  EPISODE_GEO_DATA = system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODE")
)

if (!file.exists(db_path)) {
  # First run only: a representative synthetic dataset - the full
  # default 2021-2025 window, several years of seasonal baseline across
  # every institution/pathogen/PC4 combination, plus two injected
  # outbreaks (one point-source, one propagated) - and one assessor
  # account to sign in with.
  episode_run_cron(
    db_path,
    ingest_source_fn = episode_ingest_source_synthetic,
    denominator_source_fn = episode_denominator_source_synthetic
  )

  episode_provision_user(
    username = "5580",
    full_name = "Matthijs Berends",
    email = "m.berends@domain.com",
    password = "123"
  )
} else {
  message("Reusing existing demo database at '", db_path, "' - delete it to regenerate from scratch.")
}

# Run this again (with the database already present) to simulate a
# second detection cycle over the same data - useful for exercising
# reconciliation across runs, the Activiteit log with more than one run
# recorded, and the Prestatie screen's timeliness figures once a few
# clusters have been assessed:
# episode_run_cron(db_path, ingest_source_fn = episode_ingest_source_synthetic,
#                   denominator_source_fn = episode_denominator_source_synthetic)

# review the simulated data
episode_run_app()
