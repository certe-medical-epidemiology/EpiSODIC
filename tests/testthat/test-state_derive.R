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

events_none <- function() {
  data.frame(
    event_id = integer(0), verdict = character(0), snooze_until = character(0),
    created_at = character(0), stringsAsFactors = FALSE
  )
}

events_one <- function(verdict = NA, snooze_until = NA) {
  data.frame(
    event_id = 1L, verdict = verdict, snooze_until = snooze_until,
    created_at = "2025-01-01T00:00:00Z", stringsAsFactors = FALSE
  )
}

test_that("no assessment event is always Nieuw (new)", {
  expect_equal(episode_derive_state(events_none()), "new")
  # unaffected by other flags: absence of events dominates
  expect_equal(episode_derive_state(events_none(), changed_since_assessment = TRUE), "new")
  expect_equal(episode_derive_state(events_none(), closure_criterion_met = TRUE), "new")
})

test_that("an event with no verdict yet is In beoordeling (assessing)", {
  expect_equal(episode_derive_state(events_one(verdict = NA_character_)), "assessing")
})

test_that("explicit closure wins over any classification history", {
  expect_equal(episode_derive_state(events_one(verdict = "confirmed_epidemic"), explicitly_closed = TRUE), "closed")
  expect_equal(episode_derive_state(events_one(verdict = NA_character_), explicitly_closed = TRUE), "closed")
})

test_that("a cluster with no assessment history is closed if explicitly_closed, new otherwise", {
  # This does arise from real use: the cron auto-closes a cluster with no
  # assessment at all after close_after_runs (ARCHITECTURE.md section 6,
  # step 5), which never creates an assessment event - only an
  # episode_cluster_state row (trigger = "system"). Without checking
  # explicitly_closed here too, such a cluster would read as "new" forever
  # and never leave the open rail.
  expect_equal(episode_derive_state(events_none(), explicitly_closed = TRUE), "closed")
  expect_equal(episode_derive_state(events_none(), explicitly_closed = FALSE), "new")
})

test_that("snoozed cluster with a non-terminal verdict is In beoordeling (assessing)", {
  future <- as.character(Sys.Date() + 5)
  expect_equal(
    episode_derive_state(events_one(verdict = "cluster_not_yet", snooze_until = future), today = Sys.Date()),
    "assessing"
  )
})

test_that("an expired snooze does not suppress the verdict-derived state", {
  past <- as.character(Sys.Date() - 5)
  expect_equal(
    episode_derive_state(events_one(verdict = "cluster_not_yet", snooze_until = past), today = Sys.Date()),
    "monitoring"
  )
})

test_that("terminal verdicts (artefact, expected_variation) are Afgesloten (closed), unless the cool-down escape hatch flagged them changed", {
  expect_equal(episode_derive_state(events_one(verdict = "artefact")), "closed")
  expect_equal(episode_derive_state(events_one(verdict = "expected_variation")), "closed")
  # closure_criterion_met alone (not the escape hatch) leaves a terminal verdict closed
  expect_equal(
    episode_derive_state(events_one(verdict = "expected_variation"), closure_criterion_met = TRUE), "closed"
  )
  # changed_since_assessment on a terminal verdict IS the cool-down escape
  # hatch (ARCHITECTURE.md section 6.5) - it must surface as Herbeoordeling
  # nodig (reassess), not stay silently closed
  expect_equal(
    episode_derive_state(events_one(verdict = "artefact"), changed_since_assessment = TRUE), "reassess"
  )
  expect_equal(
    episode_derive_state(events_one(verdict = "expected_variation"), changed_since_assessment = TRUE), "reassess"
  )
})

test_that("a non-terminal verdict with changed data is Herbeoordeling nodig (reassess)", {
  for (v in c("cluster_not_yet", "possible_epidemic", "confirmed_epidemic")) {
    expect_equal(
      episode_derive_state(events_one(verdict = v), changed_since_assessment = TRUE), "reassess",
      info = v
    )
  }
})

test_that("changed_since_assessment takes priority over the closure criterion", {
  expect_equal(
    episode_derive_state(
      events_one(verdict = "possible_epidemic"),
      changed_since_assessment = TRUE, closure_criterion_met = TRUE
    ),
    "reassess"
  )
})

test_that("a non-terminal verdict with unmet closure criterion is Monitoring", {
  for (v in c("cluster_not_yet", "possible_epidemic", "confirmed_epidemic")) {
    expect_equal(
      episode_derive_state(events_one(verdict = v), closure_criterion_met = FALSE), "monitoring",
      info = v
    )
  }
})

test_that("a non-terminal verdict with the closure criterion met is Af te sluiten (closable)", {
  for (v in c("cluster_not_yet", "possible_epidemic", "confirmed_epidemic")) {
    expect_equal(
      episode_derive_state(events_one(verdict = v), closure_criterion_met = TRUE), "closable",
      info = v
    )
  }
})

test_that("the latest event in a multi-row history is what determines state", {
  events <- rbind(
    events_one(verdict = "cluster_not_yet"),
    data.frame(event_id = 2L, verdict = "artefact", snooze_until = NA, created_at = "2025-02-01T00:00:00Z")
  )
  expect_equal(episode_derive_state(events), "closed")
})

test_that("exhaustive: every (verdict-class x changed x closure x snooze x explicit) combination is covered", {
  verdict_classes <- list(
    none = NA_character_,
    terminal = "artefact",
    nonterminal = "possible_epidemic"
  )
  for (vc_name in names(verdict_classes)) {
    for (changed in c(FALSE, TRUE)) {
      for (closure in c(FALSE, TRUE)) {
        for (explicit in c(FALSE, TRUE)) {
          verdict <- verdict_classes[[vc_name]]
          events <- if (vc_name == "none") events_none() else events_one(verdict = verdict)
          state <- episode_derive_state(
            events, changed_since_assessment = changed, closure_criterion_met = closure,
            explicitly_closed = explicit
          )
          expected <- if (vc_name == "none") {
            if (explicit) "closed" else "new"
          } else if (explicit) {
            "closed"
          } else if (vc_name == "terminal") {
            if (changed) "reassess" else "closed"  # cool-down escape hatch, ARCHITECTURE.md section 6.5
          } else if (changed) {
            "reassess"
          } else if (closure) {
            "closable"
          } else {
            "monitoring"
          }
          expect_equal(
            state, expected,
            info = sprintf("verdict_class=%s changed=%s closure=%s explicit=%s", vc_name, changed, closure, explicit)
          )
        }
      }
    }
  }
})
