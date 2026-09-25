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

# The batch readers the Archive and the rail are built from: every
# cluster the instance holds, read in as few queries as the database
# allows, and grouped once rather than filtered per cluster.

test_that("a query over many ids is chunked and comes back complete, in id order", {
  con <- episodic_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TEMP TABLE t (id INTEGER)")
  n <- episodic_db_chunk_size * 2L + 37L
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO t (id) VALUES %s",
      paste0("(", seq_len(n), ")", collapse = ", ")
    )
  )

  # Shuffled, with duplicates: more ids than one chunk holds, in no order.
  ids <- c(rev(seq_len(n)), 1L, 2L)
  got <- episodic_db_get_query_in(
    con,
    "SELECT id FROM t WHERE id IN (%s) ORDER BY id",
    ids
  )
  expect_equal(got$id, seq_len(n))

  none <- episodic_db_get_query_in(
    con,
    "SELECT id FROM t WHERE id IN (%s) ORDER BY id",
    integer(0)
  )
  expect_equal(nrow(none), 0)
  expect_named(none, "id")
})

test_that("states derived in bulk agree with states derived one cluster at a time", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  con <- env$con

  new_cluster <- function(day) {
    episodic_db_cluster_insert(
      con,
      stream_id = env$stream_id,
      first_day = day,
      last_day = day,
      n_cases = 3,
      priority_score = 10,
      detector_agreement = 1,
      run_id = env$run_id
    )
  }
  user_id <- episodic_db_app_user_insert(
    con,
    "tester",
    "Test User",
    "t@example.com",
    "hash"
  )

  # Never assessed.
  untouched <- new_cluster("2025-02-01")
  # Assessed, still open.
  assessed <- new_cluster("2025-02-02")
  episodic_db_assessment_event_insert(
    con,
    assessed,
    user_id = user_id,
    verdict = "cluster_not_yet"
  )
  # Closed by a person.
  closed <- new_cluster("2025-02-03")
  episodic_app_submit_assessment(
    con,
    closed,
    user_id,
    verdict = "artefact",
    rationale = "",
    close = TRUE
  )
  # Closed by the system, never assessed.
  system_closed <- new_cluster("2025-02-04")
  episodic_db_cluster_state_insert(
    con,
    cluster_id = system_closed,
    state = "closed",
    trigger = "system"
  )

  clusters <- episodic_db_clusters(con, open_only = TRUE)
  batch <- episodic_app_derive_states_batch(con, clusters)
  one_by_one <- vapply(
    clusters$cluster_id,
    function(id) episodic_app_derive_state_for_cluster(con, id),
    character(1)
  )
  expect_equal(batch, unname(one_by_one))
  expect_equal(
    batch[match(c(untouched, assessed, closed, system_closed), clusters$cluster_id)],
    c("new", "monitoring", "closed", "closed")
  )

  # The same answer whatever numeric type the ids arrive as.
  as_double <- clusters
  as_double$cluster_id <- as.numeric(as_double$cluster_id)
  expect_equal(episodic_app_derive_states_batch(con, as_double), batch)
})

test_that("each closing user is looked up once, however many clusters they closed", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  states <- data.frame(
    cluster_id = c(1L, 2L, 3L, 4L),
    state = "closed",
    user_id = c(7L, 7L, NA, 7L),
    entered_at = "2025-03-01 10:00:00",
    stringsAsFactors = FALSE
  )
  looked_up <- integer(0)
  local_mocked_bindings(
    episodic_app_actor_label = function(con, user_id, lang) {
      looked_up <<- c(looked_up, user_id)
      if (is.na(user_id)) "System" else paste("User", user_id)
    }
  )
  got <- episodic_app_closed_by_from(env$con, states, c(4L, 3L, 9L, 1L))
  expect_equal(got, c("User 7", "System", NA, "User 7"))
  expect_equal(sort(looked_up, na.last = TRUE), c(7L, NA))
})
