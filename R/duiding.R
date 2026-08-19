#' The Duiding fragment engine
#'
#' No LLM at runtime (standing brief hard rule 1). A Duiding is assembled
#' from a fixed library of fragments: each fragment has an id, a condition
#' function over the cluster object, a narrative slot, and a template (an
#' i18n key with placeholders). Deterministic, testable, translatable.
#'
#' Slots fire in a fixed order (MILESTONES.md M2): magnitude, concentration,
#' denominator, demography, completeness, recommendation. Within a slot,
#' fragments are tried in the order they are registered and the first whose
#' condition matches is used; a slot with no matching fragment (for example
#' `denominator` when no positivity metadata was supplied) is silently
#' skipped rather than padded with a placeholder sentence.
#'
#' The **cluster object** every condition and template renders against is a
#' plain list; see `R/app_read.R`'s `episode_cluster_object()` for exactly
#' which fields it carries and where each one comes from.
#' @name duiding
NULL

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

#' Build the placeholder context for a cluster
#'
#' One shared set of pre-formatted placeholder strings, so every fragment
#' template can reference any of them; unused names in a given template are
#' harmless (`episode_tr()` only substitutes placeholders the template
#' actually contains).
#'
#' @param cluster A cluster object, see `episode_cluster_object()`.
#' @param lang Session language, for number-agreement phrases.
#' @return A named list of character scalars.
#' @keywords internal
#' @noRd
episode_duiding_context <- function(cluster, lang = "nl") {
  case_word <- if (lang == "nl") c("geval", "gevallen") else c("case", "cases")
  day_word <- if (lang == "nl") c("dag", "dagen") else c("day", "days")

  list(
    obs = cluster$n_cases,
    obs_phrase = episode_count_phrase(cluster$n_cases, case_word[1], case_word[2]),
    expected = cluster$expected %||% NA,
    ratio = cluster$ratio %||% NA,
    dominant_label = cluster$concentration$dominant_label %||% "",
    dominant_share_pct = round((cluster$concentration$dominant_share %||% 0) * 100),
    dominant_n = cluster$concentration$dominant_n %||% NA,
    total_n = cluster$concentration$total %||% cluster$n_cases,
    test_first = cluster$denominator$n_tests_first %||% NA,
    test_last = cluster$denominator$n_tests_last %||% NA,
    positivity_first_pct = round((cluster$denominator$positivity_first %||% 0) * 1000) / 10,
    positivity_last_pct = round((cluster$denominator$positivity_last %||% 0) * 1000) / 10,
    dominant_band = cluster$demography$dominant_band %||% "",
    baseline_band = cluster$demography$baseline_band %||% "",
    incomplete_days = cluster$completeness$incomplete_days %||% 0,
    incomplete_days_phrase = episode_count_phrase(cluster$completeness$incomplete_days %||% 0, day_word[1], day_word[2]),
    density_value = cluster$density$value %||% NA,
    density_baseline = cluster$density$baseline %||% NA,
    place = cluster$place %||% "",
    priority_score = cluster$priority_score %||% NA
  )
}

#' The fragment registry
#'
#' A plain list of fragment definitions. Not a function of the cluster;
#' built once and reused, which is also what keeps the fragment paths
#' cheap to test exhaustively.
#'
#' @return A list of fragments, each `list(id, slot, condition, key)`.
#' @keywords internal
#' @noRd
episode_duiding_fragments <- function() {
  list(
    # -- magnitude --------------------------------------------------------
    list(id = "magnitude.rare_trigger", slot = "magnitude",
         condition = function(cl) "rare_trigger" %in% (cl$detectors %||% character(0)),
         key = "duiding.fragment.magnitude.rare_trigger"),
    list(id = "magnitude.high_ratio", slot = "magnitude",
         condition = function(cl) isTRUE((cl$ratio %||% 0) >= 3),
         key = "duiding.fragment.magnitude.high_ratio"),
    list(id = "magnitude.moderate_ratio", slot = "magnitude",
         condition = function(cl) isTRUE((cl$ratio %||% 0) >= 1.5),
         key = "duiding.fragment.magnitude.moderate_ratio"),
    list(id = "magnitude.default", slot = "magnitude",
         condition = function(cl) !is.null(cl$n_cases),
         key = "duiding.fragment.magnitude.default"),

    # -- concentration ------------------------------------------------------
    list(id = "concentration.high", slot = "concentration",
         condition = function(cl) isTRUE((cl$concentration$dominant_share %||% 0) >= 0.7),
         key = "duiding.fragment.concentration.high"),
    list(id = "concentration.moderate", slot = "concentration",
         condition = function(cl) isTRUE((cl$concentration$dominant_share %||% 0) >= 0.5),
         key = "duiding.fragment.concentration.moderate"),
    list(id = "concentration.diffuse", slot = "concentration",
         condition = function(cl) !is.null(cl$concentration) && (cl$concentration$dominant_share %||% 0) < 0.5,
         key = "duiding.fragment.concentration.diffuse"),

    # -- denominator --------------------------------------------------------
    list(id = "denominator.rising_volume_flat_positivity", slot = "denominator",
         condition = function(cl) {
           d <- cl$denominator
           !is.null(d) && isTRUE(d$n_tests_last > d$n_tests_first * 1.3) &&
             isTRUE(abs((d$positivity_last %||% 0) - (d$positivity_first %||% 0)) < 0.01)
         },
         key = "duiding.fragment.denominator.rising_volume_flat_positivity"),
    list(id = "denominator.rising_positivity", slot = "denominator",
         condition = function(cl) {
           d <- cl$denominator
           !is.null(d) && isTRUE((d$positivity_last %||% 0) > (d$positivity_first %||% 0) * 1.5)
         },
         key = "duiding.fragment.denominator.rising_positivity"),
    list(id = "denominator.stable", slot = "denominator",
         condition = function(cl) !is.null(cl$denominator),
         key = "duiding.fragment.denominator.stable"),

    # -- demography -----------------------------------------------------
    list(id = "demography.shifted", slot = "demography",
         condition = function(cl) isTRUE(cl$demography$shifted),
         key = "duiding.fragment.demography.shifted"),

    # -- completeness ---------------------------------------------------
    list(id = "completeness.warning", slot = "completeness",
         condition = function(cl) isTRUE((cl$completeness$incomplete_days %||% 0) > 0),
         key = "duiding.fragment.completeness.warning"),

    # -- recommendation ---------------------------------------------------
    list(id = "recommendation.rare_trigger", slot = "recommendation",
         condition = function(cl) "rare_trigger" %in% (cl$detectors %||% character(0)),
         key = "duiding.fragment.recommendation.rare_trigger"),
    list(id = "recommendation.high_priority", slot = "recommendation",
         condition = function(cl) isTRUE((cl$priority_score %||% 0) >= 80),
         key = "duiding.fragment.recommendation.high_priority"),
    list(id = "recommendation.moderate_priority", slot = "recommendation",
         condition = function(cl) isTRUE((cl$priority_score %||% 0) >= 50),
         key = "duiding.fragment.recommendation.moderate_priority"),
    list(id = "recommendation.default", slot = "recommendation",
         condition = function(cl) TRUE,
         key = "duiding.fragment.recommendation.default")
  )
}

#' @rdname duiding
#' @export
episode_duiding_slots <- c("magnitude", "concentration", "denominator", "demography", "completeness", "recommendation")

#' Generate the Duiding for a cluster
#'
#' @param cluster A cluster object, see `episode_cluster_object()`.
#' @param lang Session language, `"nl"` or `"en"`.
#' @param instance_i18n Optional operator overrides, passed to [episode_tr()].
#' @return A list with `text` (a character vector, one string per slot that
#'   fired, in slot order) and `fired` (a character vector of the fragment
#'   ids that fired, same order) - "every fragment records which condition
#'   fired" (MILESTONES.md M2).
#' @export
episode_duiding_generate <- function(cluster, lang = "nl", instance_i18n = NULL) {
  fragments <- episode_duiding_fragments()
  ctx <- episode_duiding_context(cluster, lang = lang)

  text <- character(0)
  fired <- character(0)

  for (slot in episode_duiding_slots) {
    slot_fragments <- Filter(function(f) f$slot == slot, fragments)
    for (fragment in slot_fragments) {
      if (isTRUE(tryCatch(fragment$condition(cluster), error = function(e) FALSE))) {
        rendered <- do.call(episode_tr, c(list(key = fragment$key, lang = lang, instance_i18n = instance_i18n), ctx))
        text <- c(text, rendered)
        fired <- c(fired, fragment$id)
        break  # one fragment per slot
      }
    }
  }

  list(text = text, fired = fired)
}

#' Render the recommendation slot separately
#'
#' The recommendation slot is displayed with distinct visual treatment (an
#' advisory callout, not a plain paragraph) in the dossier, matching
#' `episode-mockup.jsx`'s `advies` box. This re-runs generation and returns
#' just that slot's text so the UI does not have to know fragment ids.
#'
#' @inheritParams episode_duiding_generate
#' @return A single character string (the recommendation text), or `""` if
#'   somehow nothing fired (should not happen: `recommendation.default`
#'   always matches).
#' @export
episode_duiding_recommendation <- function(cluster, lang = "nl", instance_i18n = NULL) {
  generated <- episode_duiding_generate(cluster, lang = lang, instance_i18n = instance_i18n)
  idx <- which(startsWith(generated$fired, "recommendation."))
  if (length(idx) == 0) return("")
  generated$text[idx[1]]
}

#' The narrative paragraphs only (excludes the recommendation slot)
#'
#' @inheritParams episode_duiding_generate
#' @return A character vector, one string per non-recommendation slot fired.
#' @export
episode_duiding_paragraphs <- function(cluster, lang = "nl", instance_i18n = NULL) {
  generated <- episode_duiding_generate(cluster, lang = lang, instance_i18n = instance_i18n)
  keep <- !startsWith(generated$fired, "recommendation.")
  generated$text[keep]
}
