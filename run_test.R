# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

library(EpiSODIC)

# A persistent path, not a tempfile: the point of manual testing is to
# sign in, classify clusters, close them, mute a stream, generate a
# report - and see that work still be there next time this script runs.
# *.sqlite is already .gitignore'd, so this never gets committed.
db_path <- "episodic-demo.sqlite"

Sys.setenv(
  EPISODIC_CONFIG = system.file("config", "default.yaml", package = "EpiSODIC"),
  EPISODIC_DB = db_path,
  EPISODIC_GEO_DATA = system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODIC")
)

if (!file.exists(db_path)) {
  # First run only: a representative synthetic dataset - the full
  # default 2021-2025 window, several years of seasonal baseline across
  # every institution/pathogen/PC combination, plus two injected
  # outbreaks (one point-source, one propagated) - and one assessor
  # account to sign in with.
  episodic_run_cron(
    db_path,
    ingest_source_fn = episodic_ingest_source_synthetic,
    denominator_source_fn = episodic_denominator_source_synthetic
  )

  episodic_provision_user(
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
# episodic_run_cron(db_path, ingest_source_fn = episodic_ingest_source_synthetic,
#                   denominator_source_fn = episodic_denominator_source_synthetic)

# review the simulated data
episodic_run_app()
