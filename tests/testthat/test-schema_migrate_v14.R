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


# Schema version 14 adds an epidemic's outlook by week
# (episodic_cluster_forecast_week).

v14_fixture <- function() {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  DBI::dbExecute(con, "DROP TABLE episodic_cluster_forecast_week")
  DBI::dbExecute(
    con,
    "UPDATE episodic_schema_version SET version = 13 WHERE version = ?",
    params = list(episodic_schema_version)
  )
  DBI::dbDisconnect(con)
  path
}

test_that("migration 14 adds the outlook week table, and a second pass changes nothing", {
  path <- v14_fixture()
  on.exit(unlink(c(path, Sys.glob(paste0(path, ".schema-*.bak")))))
  suppressMessages(episodic_db_migrate(path))
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(episodic_db_schema_version(con), episodic_schema_version)
  expect_setequal(
    DBI::dbListFields(con, "episodic_cluster_forecast_week"),
    c(
      "cluster_forecast_id", "target_date", "horizon", "mean",
      names(episodic_forecast_probs), "p_low", "p_medium", "p_high", "p_very_high"
    )
  )
  # A probability outside 0 to 1 is refused.
  expect_error(DBI::dbExecute(
    con,
    "INSERT INTO episodic_cluster_forecast_week
       (cluster_forecast_id, target_date, horizon, mean, q025, q05, q10, q25, q50, q75, q90, q95, q975, p_high)
     VALUES (1, '2025-01-06', 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.5)"
  ))
  expect_no_error(episodic_db_migrations()[["14"]](con, "sqlite"))
})
