library(EpiSODIC)
# or:
devtools::load_all()

# A persistent path, not a tempfile: the point of manual testing is to
# sign in, classify clusters, close them, mute a stream, generate a
# report - and see that work still be there next time this script runs.
# *.sqlite is already .gitignore'd, so this never gets committed.
db_path <- tempfile(fileext = ".sqlite")

Sys.setenv(
  EPISODIC_DB = db_path,
  EPISODIC_LANGUAGE = "nl",
  EPISODIC_CONFIG = system.file("config", "episodic_default_config.yaml", package = "EpiSODIC"),
  EPISODIC_GEO_DATA = system.file(
    "extdata",
    "geo_postcodes4_nl.rds",
    package = "EpiSODIC"
  )
)

if (!file.exists(db_path)) {
  # First run only: a representative synthetic dataset - the full
  # default 2021-2025 window, several years of seasonal baseline across
  # every institution/pathogen/PC combination, plus two injected
  # outbreaks (one point-source, one propagated) - and one epidemiologist
  # account to sign in with.
  episodic_run_cron(
    db_path,
    cases = episodic_synthetic_cases,
    denominators = episodic_synthetic_denominators
  )

  episodic_add_user(
    username = "5580",
    full_name = "Matthijs Berends",
    email = "m.berends@domain.com",
    password = "123"
  )
} else {
  message(
    "Reusing existing demo database at '",
    db_path,
    "' - delete it to regenerate from scratch."
  )
}

# Run this again (with the database already present) to simulate a
# second detection cycle over the same data - useful for exercising
# reconciliation across runs, the Activiteit log with more than one run
# recorded, and the Prestatie screen's timeliness figures once a few
# clusters have been assessed:
# episodic_run_cron(db_path, cases = episodic_synthetic_cases,
#                   denominators = episodic_synthetic_denominators)

# review the simulated data
episodic_run_app()
