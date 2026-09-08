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

# One outbreak, seen at two levels. suppress_setup() builds the pair and
# returns both cluster ids, so each test only has to say how the cases
# are distributed between them.
suppress_setup <- function(child_share = 1, n_children = 1, n_cases = 10) {
  con <- episodic_test_db()
  key <- digest::digest("h", algo = "sha1", serialize = FALSE)
  DBI::dbExecute(
    con,
    "INSERT INTO episodic_institution
      (institution_key, display_name, institution_type, care_line, is_monitored, is_active)
     VALUES (?, 'H', 'hospital', 'second', 1, 1)",
    params = list(key)
  )
  institution_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[
    1
  ]
  run_id <- episodic_db_run_start(con, "h", "a")

  stream <- function(level, ward = NA) {
    episodic_db_stream_upsert(
      con,
      stream_key = episodic_stream_key(
        level,
        "Test pathogen",
        institution_id = institution_id,
        ward = ward
      ),
      level = level,
      pathogen = "Test pathogen",
      institution_id = institution_id,
      ward = ward,
      observed_date = "2026-07-06"
    )
  }
  cluster <- function(stream_id, n) {
    episodic_db_cluster_insert(
      con,
      stream_id = stream_id,
      first_day = "2026-07-06",
      last_day = "2026-07-19",
      n_cases = n,
      priority_score = 50,
      detector_agreement = 1,
      run_id = run_id
    )
  }

  parent_id <- cluster(stream("pathogen_institution"), n_cases)
  child_ids <- vapply(
    seq_len(n_children),
    function(k) {
      cluster(stream("pathogen_ward", ward = paste0("W", k)), n_cases)
    },
    integer(1)
  )

  # every case belongs to the parent; the children take their share
  case_ids <- vapply(
    seq_len(n_cases),
    function(i) {
      DBI::dbExecute(
        con,
        "INSERT INTO episodic_case
        (source_key, lab_number, patient_key, sample_date, pathogen, care_line, institution_id, first_seen_run)
       VALUES (?, ?, ?, '2026-07-10', 'Test pathogen', 'second', ?, ?)",
        params = list(
          sprintf("C%d", i),
          sprintf("C%d", i),
          sprintf("P%d", i),
          institution_id,
          run_id
        )
      )
      DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    },
    numeric(1)
  )
  episodic_db_cluster_case_link_many(con, parent_id, case_ids)
  per_child <- floor(n_cases * child_share / n_children)
  for (k in seq_len(n_children)) {
    taken <- case_ids[seq_len(per_child) + (k - 1) * per_child]
    episodic_db_cluster_case_link_many(con, child_ids[k], taken[!is.na(taken)])
  }

  list(con = con, parent = parent_id, children = child_ids)
}

suppressed_by <- function(con, cluster_id) {
  episodic_db_clusters(
    con,
    include_suppressed = TRUE
  )$suppressed_by[
    episodic_db_clusters(con, include_suppressed = TRUE)$cluster_id ==
      cluster_id
  ]
}

test_that("a child holding most of the parent's cases suppresses the parent", {
  env <- suppress_setup(child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))

  episodic_suppress_lattice(env$con, episodic_config_resolve())

  expect_equal(suppressed_by(env$con, env$parent), env$children[1])
  expect_true(is.na(suppressed_by(env$con, env$children[1])))
  # and the queue shows one cluster, not two
  expect_equal(nrow(episodic_db_clusters(env$con, open_only = TRUE)), 1)
})

test_that("a rise spread over several children with no dominant one suppresses the children", {
  env <- suppress_setup(child_share = 0.8, n_children = 4)
  on.exit(DBI::dbDisconnect(env$con))

  episodic_suppress_lattice(env$con, episodic_config_resolve())

  expect_true(is.na(suppressed_by(env$con, env$parent)))
  for (child in env$children) {
    expect_equal(suppressed_by(env$con, child), env$parent)
  }
  expect_equal(nrow(episodic_db_clusters(env$con, open_only = TRUE)), 1)
})

test_that("a child that is neither dominant nor one of several leaves everything standing", {
  # one child holding 60%: over the diffuse threshold, under the
  # dominance one, and the only child there is. Nothing is a restatement
  # of anything, so both stay.
  env <- suppress_setup(child_share = 0.6)
  on.exit(DBI::dbDisconnect(env$con))

  episodic_suppress_lattice(env$con, episodic_config_resolve())

  expect_true(is.na(suppressed_by(env$con, env$parent)))
  expect_true(is.na(suppressed_by(env$con, env$children[1])))
  expect_equal(nrow(episodic_db_clusters(env$con, open_only = TRUE)), 2)
})

test_that("a cluster somebody has assessed is never suppressed out of the queue", {
  # The board's own record of a decision is not something a later run
  # gets to hide.
  env <- suppress_setup(child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))
  user_id <- episodic_db_app_user_insert(
    env$con,
    "jdoe",
    "Jane Doe",
    "j@x.nl",
    "hash"
  )
  episodic_app_submit_assessment(
    env$con,
    env$parent,
    user_id,
    verdict = "possible_epidemic",
    rationale = "looked at this one"
  )

  episodic_suppress_lattice(env$con, episodic_config_resolve())

  expect_true(is.na(suppressed_by(env$con, env$parent)))
})

test_that("suppression lifts when the picture that justified it changes", {
  env <- suppress_setup(child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))
  config <- episodic_config_resolve()

  episodic_suppress_lattice(env$con, config)
  expect_equal(suppressed_by(env$con, env$parent), env$children[1])

  # the ward cluster loses its cases to the rest of the hospital: the
  # rise is no longer local, and the parent has to come back
  DBI::dbExecute(
    env$con,
    "DELETE FROM episodic_cluster_case WHERE cluster_id = ?",
    params = list(env$children[1])
  )
  episodic_suppress_lattice(env$con, config)
  expect_true(is.na(suppressed_by(env$con, env$parent)))
})

test_that("the dossier shows what a cluster suppressed", {
  env <- suppress_setup(child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))
  episodic_suppress_lattice(env$con, episodic_config_resolve())

  attached <- episodic_db_clusters_suppressed_by(env$con, env$children[1])
  expect_equal(attached$cluster_id, env$parent)
  expect_equal(attached$level, "pathogen_institution")

  rendered <- as.character(episodic_ui_related_panel(
    env$con,
    env$children[1],
    lang = "en"
  ))
  expect_true(grepl("institution", rendered, ignore.case = TRUE))
  expect_true(grepl("suppressed", rendered, ignore.case = TRUE))
  expect_false(grepl("[[", rendered, fixed = TRUE))

  # It is not a dossier of its own, so the row does not open one - and
  # rather than a link that goes nowhere, the id is marked and says why.
  expect_false(grepl("open_cluster", rendered, fixed = TRUE))
  expect_true(grepl("episodic-id-unlinked", rendered, fixed = TRUE))
  expect_true(grepl(
    episodic_tr(
      "cluster.unlinked.suppressed",
      ref = episodic_tr("dossier.cluster_ref", id = env$children[1], lang = "en"),
      lang = "en"
    ),
    rendered,
    fixed = TRUE
  ))

  # and it carries the same spine every other cluster table does
  for (key in c(
    "column.cluster",
    "column.period",
    "column.cases",
    "column.duration",
    "column.priority"
  )) {
    expect_true(grepl(episodic_tr(key, lang = "en"), rendered, fixed = TRUE))
  }
})

test_that("a cluster sharing cases with one that stands separately says so, and links to it", {
  # Suppression only collapses within a containment chain. A ward
  # outbreak and a regional rise built partly out of it are two dossiers
  # by design, and an epidemiologist reading either without the other is
  # reading it wrong.
  env <- suppress_setup(child_share = 0.6)
  on.exit(DBI::dbDisconnect(env$con))
  episodic_suppress_lattice(env$con, episodic_config_resolve())

  linked <- episodic_db_clusters_linked_to(env$con, env$parent)
  expect_equal(linked$cluster_id, env$children[1])
  expect_equal(linked$shared_cases, 6)

  # the header names it and can be operated from the keyboard
  chips <- as.character(episodic_ui_linked_chips(linked, lang = "en"))
  expect_true(grepl("Linked to", chips, fixed = TRUE))
  # episodicOpenCluster() (see R/app_ui.R) both sets the `open_cluster`
  # Shiny input and moves the rail's own highlight, so a chip calls it
  # instead of Shiny.setInputValue('open_cluster', ...) directly.
  opens <- paste0("episodicOpenCluster(", env$children[1], ")")
  expect_true(grepl(opens, chips, fixed = TRUE))
  expect_true(grepl("onkeydown", chips, fixed = TRUE))

  # and the panel carries it, marked as standing separately
  panel <- as.character(episodic_ui_related_panel(
    env$con,
    env$parent,
    lang = "en"
  ))
  expect_true(grepl("linked", panel, ignore.case = TRUE))
  expect_false(grepl("[[", panel, fixed = TRUE))
})

test_that("a suppressed cluster is not also advertised as a link", {
  # It is not a separate dossier: it is this one, seen at another level.
  env <- suppress_setup(child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))
  episodic_suppress_lattice(env$con, episodic_config_resolve())

  linked <- episodic_db_clusters_linked_to(env$con, env$children[1])
  expect_equal(nrow(linked), 0)
  expect_null(episodic_ui_linked_chips(linked, lang = "en"))
})

test_that("the header names at most three links and counts the rest", {
  linked <- data.frame(
    cluster_id = 101:105,
    n_cases = 5,
    shared_cases = 5,
    stringsAsFactors = FALSE
  )
  chips <- as.character(episodic_ui_linked_chips(linked, lang = "en"))
  expect_true(grepl("#101", chips, fixed = TRUE))
  expect_true(grepl("#103", chips, fixed = TRUE))
  expect_false(grepl("#104", chips, fixed = TRUE))
  expect_true(grepl("+2 more", chips, fixed = TRUE))
  expect_null(episodic_ui_linked_chips(linked[0, ], lang = "en"))
})

# ---------------------------------------------------------------------
# What suppression must never do
# ---------------------------------------------------------------------

test_that("a manual cluster neither suppresses nor is suppressed", {
  # A manual cluster's case detail lives in episodic_cluster_manual_case,
  # never in episodic_case, so it has no episodic_cluster_case rows and
  # every share computed for it is zero. Read as "spread thinly across
  # its children" rather than as "unmeasurable", that would let a
  # hand-added cluster with no case data hide every real cluster that
  # happened to overlap it in time.
  env <- suppress_setup(n_cases = 10, n_children = 3, child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))

  clusters <- episodic_db_clusters_for_suppression(env$con)
  expect_true(all(clusters$origin == "detected"))
  expect_equal(nrow(clusters), 1 + length(env$children))

  # Turn the parent into a manual cluster and it leaves the picture
  # entirely - the children stand on their own.
  DBI::dbExecute(
    env$con,
    "UPDATE episodic_cluster SET origin = 'manual' WHERE cluster_id = ?",
    params = list(env$parent)
  )
  remaining <- episodic_db_clusters_for_suppression(env$con)
  expect_false(env$parent %in% remaining$cluster_id)

  episodic_suppress_lattice(env$con, episodic_config_resolve(NA))
  for (child_id in env$children) {
    expect_true(is.na(suppressed_by(env$con, child_id)), info = child_id)
  }
  expect_true(is.na(suppressed_by(env$con, env$parent)))
})

test_that("a parent with no linked cases suppresses nothing", {
  # Zero shares are the absence of a measurement, not evidence of a
  # diffuse rise.
  env <- suppress_setup(n_cases = 10, n_children = 3, child_share = 0.9)
  on.exit(DBI::dbDisconnect(env$con))
  DBI::dbExecute(
    env$con,
    "DELETE FROM episodic_cluster_case WHERE cluster_id = ?",
    params = list(env$parent)
  )

  episodic_suppress_lattice(env$con, episodic_config_resolve(NA))
  for (child_id in env$children) {
    expect_true(is.na(suppressed_by(env$con, child_id)), info = child_id)
  }
})

test_that("suppression never chains: a suppressed cluster suppresses nothing further", {
  # `suppressed_by` is followed exactly one step by everything that reads
  # it, so a two-link chain puts a cluster on the dossier of a cluster
  # that is itself out of the queue - and the one at the far end is shown
  # nowhere at all. The geographic chain (area -> province -> region) is
  # where this arises.
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  run_id <- episodic_db_run_start(con, "h", "a")

  geo_stream <- function(level, region_code) {
    episodic_db_stream_upsert(
      con,
      stream_key = episodic_stream_key(
        level,
        "Test pathogen",
        region_code = region_code
      ),
      level = level,
      pathogen = "Test pathogen",
      region_code = region_code,
      observed_date = "2026-07-10"
    )
  }
  geo_cluster <- function(stream_id, n) {
    episodic_db_cluster_insert(
      con,
      stream_id = stream_id,
      first_day = "2026-07-01",
      last_day = "2026-07-20",
      n_cases = n,
      priority_score = 50,
      detector_agreement = 1,
      run_id = run_id
    )
  }

  area <- geo_cluster(geo_stream("pathogen_area", "AREA-97"), 10)
  province <- geo_cluster(geo_stream("pathogen_province", "PROV"), 10)
  region <- geo_cluster(geo_stream("pathogen_region", "REGION"), 10)

  case_ids <- vapply(seq_len(10), function(i) {
    DBI::dbExecute(
      con,
      "INSERT INTO episodic_case
        (source_key, lab_number, patient_key, sample_date, pathogen, care_line, pc, first_seen_run)
       VALUES (?, ?, ?, '2026-07-10', 'Test pathogen', 'second', '9711', ?)",
      params = list(sprintf("G%d", i), sprintf("G%d", i), sprintf("PG%d", i), run_id)
    )
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  }, numeric(1))

  # Every level holds the same cases, so the area dominates the province
  # and the province would dominate the region.
  for (id in c(area, province, region)) {
    episodic_db_cluster_case_link_many(con, id, case_ids)
  }

  episodic_suppress_lattice(con, episodic_config_resolve(NA))

  # The area took the province, as it should.
  expect_equal(suppressed_by(con, province), area)
  # And the province, now suppressed itself, did not go on to take the
  # region: the region stays in the queue rather than pointing at a
  # cluster nobody can see.
  expect_true(is.na(suppressed_by(con, region)))
  expect_true(is.na(suppressed_by(con, area)))
})
