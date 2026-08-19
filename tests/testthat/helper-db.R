# Test helpers: an in-memory-ish (temp file) EpiSODE database, and small
# fixtures used across several test files.

# Caller is responsible for DBI::dbDisconnect(con); each call gets its own
# fresh temp-file database, so leaving one open across a test does not leak
# into other tests.
episode_test_db <- function() {
  path <- tempfile(fileext = ".sqlite")
  episode_db_create(path)
}

episode_test_pathogen_config <- function() {
  path <- system.file("config", "pathogen_config.csv", package = "EpiSODE")
  if (identical(path, "")) path <- file.path("inst", "config", "pathogen_config.csv")
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

episode_test_config <- function() {
  episode_config_resolve(NA)
}
