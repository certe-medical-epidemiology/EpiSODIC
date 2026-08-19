library(EpiSODE)

db_path <- tempfile(fileext = ".sqlite")
Sys.setenv(
  EPISODE_CONFIG = system.file("config", "default.yaml", package = "EpiSODE"),
  EPISODE_DB = db_path,
  EPISODE_GEO_DATA = system.file("extdata", "geo_postcodes4_nl.rds", package = "EpiSODE")
)

fake_cases <- episode_ingest_source_synthetic
fake_aggregated_data <- episode_denominator_source_synthetic

# run a simulated run with no real data
episode_run_cron(db_path,
                 ingest_source_fn      = fake_cases,
                 denominator_source_fn = fake_aggregated_data)

# add an authorised user that classifies clusters (i.e., an epidemiologist)
episode_provision_user(
  username = "5580",
  full_name = "Matthijs Berends",
  email = "m.berends@domain.com",
  password = "123"
)

# review the simulated data
episode_run_app()
