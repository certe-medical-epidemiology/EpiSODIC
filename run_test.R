pkgload::load_all()

db_path <- tempfile(fileext = ".sqlite")
Sys.setenv(EPISODE_CONFIG = system.file("config", "default.yaml", package = "EpiSODE"))

episode_run_cron(db_path, denominator_source_fn = episode_denominator_source_synthetic)

episode_run_app(db_path)
