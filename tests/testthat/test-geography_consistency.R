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

# One run, one geography.
#
# `episodic_run_cron(episodic_config_path = ...)` is a documented way to
# run under a configuration other than EPISODIC_CONFIG's, and the
# geography is part of what it configures. Resolved separately at each
# site, half the lattice reads that setting from the run's own
# configuration and half from the environment: L5 names its catchment
# from one, L3 its areas from the other, and case membership is tested
# against a third. The result is silent - geographic streams that match
# no case however many arrive, so the statistical detectors never run on
# them, and clusters open with nothing linked to them.

instance_config <- function() {
  directory <- tempfile("episodic-geography-")
  dir.create(directory)
  path <- file.path(directory, "instance.yaml")
  writeLines(
    c(
      "geography:",
      "  region_code: OTHER_REGION",
      "  area_code_prefix: \"ZONE-\"",
      "  area_pc_characters: 2"
    ),
    path
  )
  path
}

small_cases <- function() {
  data.frame(
    source_key = sprintf("GEO-%02d", 1:6),
    lab_number = sprintf("LABGEO-%02d", 1:6),
    patient_key = sprintf("PGEO-%02d", 1:6),
    sample_date = as.character(as.Date("2025-05-01") + c(0, 1, 2, 3, 4, 5)),
    receipt_date = as.character(as.Date("2025-05-01") + c(0, 1, 2, 3, 4, 5)),
    pathogen = "Norovirus",
    care_line = "second",
    institution_key = "GEO-HOSP-01",
    institution_display_name = "Geography Hospital",
    institution_type = "hospital",
    municipality = "Groningen",
    ward = "B4",
    specialism = "Interne",
    pc = "9711",
    sex = "F",
    age = 70L,
    stringsAsFactors = FALSE
  )
}

test_that("the whole lattice is named from the run's own configuration", {
  config_path <- instance_config()
  db_path <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(dirname(config_path), recursive = TRUE)
    unlink(db_path)
  })
  # The suite's own EPISODIC_CONFIG says something else entirely, which
  # is the whole point of the test.
  expect_false(identical(episodic_test_region_code(), "OTHER_REGION"))

  suppressMessages(episodic_run_cron(
    cases = small_cases(),
    db_path = db_path,
    episodic_config_path = config_path,
    run_date = as.Date("2025-05-08")
  ))
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  streams <- episodic_db_streams(con)
  region <- streams[streams$level == "pathogen_region", ]
  area <- streams[streams$level == "pathogen_area", ]
  expect_equal(unique(region$region_code), "OTHER_REGION")
  expect_true(all(startsWith(area$region_code, "ZONE-")))
})

test_that("a geographic stream sees its own cases under that configuration", {
  config_path <- instance_config()
  db_path <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(dirname(config_path), recursive = TRUE)
    unlink(db_path)
  })
  suppressMessages(episodic_run_cron(
    cases = small_cases(),
    db_path = db_path,
    episodic_config_path = config_path,
    run_date = as.Date("2025-05-08")
  ))
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  geography <- episodic_geography_config(episodic_config_resolve(config_path))
  streams <- episodic_db_streams(con)
  region <- streams[streams$level == "pathogen_region", ][1, ]
  cases_all <- episodic_db_cases(con)

  expect_equal(
    nrow(episodic_cases_for_stream(cases_all, region, geography)),
    6
  )
  expect_equal(
    nrow(episodic_db_cases_for_stream_id(
      con,
      region$stream_id,
      geography = geography
    )),
    6
  )
  # And the failure the fix is about: asked under a different geography,
  # the same stream matches nothing at all.
  expect_equal(
    nrow(episodic_cases_for_stream(
      cases_all,
      region,
      list(
        region_code = "SOMEWHERE_ELSE",
        area_code_prefix = "AREA-",
        area_pc_characters = 2
      )
    )),
    0
  )
})

test_that("a cluster on a geographic stream gets its cases linked", {
  config_path <- instance_config()
  db_path <- tempfile(fileext = ".sqlite")
  on.exit({
    unlink(dirname(config_path), recursive = TRUE)
    unlink(db_path)
  })
  suppressMessages(episodic_run_cron(
    cases = small_cases(),
    db_path = db_path,
    episodic_config_path = config_path,
    run_date = as.Date("2025-05-08")
  ))
  con <- episodic_db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  geography <- episodic_geography_config(episodic_config_resolve(config_path))
  streams <- episodic_db_streams(con)
  region_id <- streams$stream_id[streams$level == "pathogen_region"][1]
  run_id <- episodic_db_run_start(con, "host", "account")
  cluster_id <- episodic_db_cluster_insert(
    con,
    stream_id = region_id,
    first_day = "2025-05-01",
    last_day = "2025-05-06",
    n_cases = 6,
    priority_score = 50,
    detector_agreement = 1,
    run_id = run_id
  )
  episodic_reconcile_link_cases(
    con,
    region_id,
    cluster_id,
    "2025-05-01",
    "2025-05-06",
    geography = geography
  )

  linked <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM episodic_cluster_case WHERE cluster_id = ?",
    params = list(cluster_id)
  )
  # A cluster the run itself would have opened here, with no cases
  # linked to it, is exactly what an operator saw: an empty line list, an
  # empty epidemic curve and no way to tell why.
  expect_equal(linked$n, 6L)
  expect_equal(
    episodic_reconcile_case_count(
      con,
      region_id,
      "2025-05-01",
      "2025-05-06",
      existing = data.frame(n_cases = 0L),
      candidate = data.frame(n_cases = 0L),
      geography = geography
    ),
    6L
  )
})
