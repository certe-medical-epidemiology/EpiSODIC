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
  # htmltools escapes the quotes in an attribute value, so compare
  # against what the browser will actually run rather than the source.
  handler <- gsub("&#39;", "'", chips, fixed = TRUE)
  opens <- paste0("open_cluster', ", env$children[1])
  expect_true(grepl(opens, handler, fixed = TRUE))
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
