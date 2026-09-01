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

#' How the dossier's narrative summary is written
#'
#' Every cluster dossier includes a short, plain-language narrative
#' summarising what the numbers mean - e.g. how large the cluster is
#' relative to baseline, how concentrated it is at one institution, and
#' what to consider next. This text is not written by an LLM: it is
#' assembled deterministically from a fixed library of pre-written
#' sentence templates ("fragments"), each triggered by a specific condition
#' on the cluster's data. The same inputs always produce the same wording,
#' and every fragment is translated into every shipped language (English,
#' Arabic, Dutch, French, German, Hindi, Mandarin Chinese, and Spanish).
#'
#' The narrative is built up section by section, in this fixed order:
#' `episodic_interpretation_slots` lists them - magnitude, curve shape,
#' concentration, testing volume, demography, data completeness, and
#' finally a recommendation. Within a section, the first applicable
#' fragment is used; a section with nothing applicable (for example, the
#' testing-volume section when no positivity data was supplied) is simply
#' omitted rather than filled with a placeholder sentence.
#' @name episodic_interpretation
NULL

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

#' Build the placeholder context for a cluster
#'
#' One shared set of pre-formatted placeholder strings, so every fragment
#' template can reference any of them; unused names in a given template are
#' harmless (`episodic_tr()` only substitutes placeholders the template
#' actually contains).
#'
#' @param cluster A cluster object, see `episodic_cluster_object()`.
#' @param lang Session language, for number-agreement phrases.
#' @return A named list of character scalars.
#' @keywords internal
#' @noRd
episodic_interpretation_context <- function(
    cluster,
    lang = Sys.getenv("EPISODIC_LANGUAGE")) {
  case_word <- c(
    episodic_tr("unit.case", lang = lang),
    episodic_tr("unit.cases", lang = lang)
  )
  day_word <- c(
    episodic_tr("unit.day", lang = lang),
    episodic_tr("unit.days", lang = lang)
  )

  list(
    obs = cluster$n_cases,
    obs_phrase = episodic_count_phrase(
      cluster$n_cases,
      case_word[1],
      case_word[2]
    ),
    expected = round(cluster$expected %||% NA, 1),
    ratio = round(cluster$ratio %||% NA, 1),
    dominant_label = cluster$concentration$dominant_label %||% "",
    dominant_share_pct = round(
      (cluster$concentration$dominant_share %||% 0) * 100
    ),
    dominant_n = cluster$concentration$dominant_n %||% NA,
    total_n = cluster$concentration$total %||% cluster$n_cases,
    test_first = cluster$denominator$n_tests_first %||% NA,
    test_last = cluster$denominator$n_tests_last %||% NA,
    positivity_first_pct = round(
      (cluster$denominator$positivity_first %||% 0) * 1000
    ) /
      10,
    positivity_last_pct = round(
      (cluster$denominator$positivity_last %||% 0) * 1000
    ) /
      10,
    dominant_band = cluster$demography$dominant_band %||% "",
    baseline_band = cluster$demography$baseline_band %||% "",
    incomplete_days = cluster$completeness$incomplete_days %||% 0,
    incomplete_days_phrase = episodic_count_phrase(
      cluster$completeness$incomplete_days %||% 0,
      day_word[1],
      day_word[2]
    ),
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
episodic_interpretation_fragments <- function() {
  list(
    # -- magnitude --------------------------------------------------------
    list(
      id = "magnitude.rare_trigger",
      slot = "magnitude",
      condition = function(cl) {
        "rare_trigger" %in% (cl$detectors %||% character(0))
      },
      key = "interpretation.fragment.magnitude.rare_trigger"
    ),
    list(
      id = "magnitude.high_ratio",
      slot = "magnitude",
      condition = function(cl) isTRUE((cl$ratio %||% 0) >= 3),
      key = "interpretation.fragment.magnitude.high_ratio"
    ),
    list(
      id = "magnitude.moderate_ratio",
      slot = "magnitude",
      condition = function(cl) isTRUE((cl$ratio %||% 0) >= 1.5),
      key = "interpretation.fragment.magnitude.moderate_ratio"
    ),
    list(
      id = "magnitude.default",
      slot = "magnitude",
      condition = function(cl) !is.null(cl$n_cases),
      key = "interpretation.fragment.magnitude.default"
    ),

    # -- curve_shape ------------------------------------------------------
    # The first question in any outbreak investigation - does it redirect
    # the enquiry from person-to-person spread towards a common exposure -
    # is stated in the Duiding, not a separate dossier panel; see
    # episodic_classify_curve_shape().
    list(
      id = "curve_shape.point_source",
      slot = "curve_shape",
      condition = function(cl) {
        identical(cl$curve_shape %||% NA, "point_source")
      },
      key = "interpretation.fragment.curve_shape.point_source"
    ),
    list(
      id = "curve_shape.propagated",
      slot = "curve_shape",
      condition = function(cl) identical(cl$curve_shape %||% NA, "propagated"),
      key = "interpretation.fragment.curve_shape.propagated"
    ),
    list(
      id = "curve_shape.ambiguous",
      slot = "curve_shape",
      condition = function(cl) identical(cl$curve_shape %||% NA, "ambiguous"),
      key = "interpretation.fragment.curve_shape.ambiguous"
    ),

    # -- concentration ------------------------------------------------------
    list(
      id = "concentration.high",
      slot = "concentration",
      condition = function(cl) {
        isTRUE((cl$concentration$dominant_share %||% 0) >= 0.7)
      },
      key = "interpretation.fragment.concentration.high"
    ),
    list(
      id = "concentration.moderate",
      slot = "concentration",
      condition = function(cl) {
        isTRUE((cl$concentration$dominant_share %||% 0) >= 0.5)
      },
      key = "interpretation.fragment.concentration.moderate"
    ),
    list(
      id = "concentration.diffuse",
      slot = "concentration",
      condition = function(cl) {
        !is.null(cl$concentration) &&
          (cl$concentration$dominant_share %||% 0) < 0.5
      },
      key = "interpretation.fragment.concentration.diffuse"
    ),

    # -- denominator --------------------------------------------------------
    list(
      id = "denominator.rising_volume_flat_positivity",
      slot = "denominator",
      condition = function(cl) {
        d <- cl$denominator
        !is.null(d) &&
          isTRUE(d$n_tests_last > d$n_tests_first * 1.3) &&
          isTRUE(
            abs((d$positivity_last %||% 0) - (d$positivity_first %||% 0)) < 0.01
          )
      },
      key = "interpretation.fragment.denominator.rising_volume_flat_positivity"
    ),
    list(
      id = "denominator.rising_positivity",
      slot = "denominator",
      condition = function(cl) {
        d <- cl$denominator
        !is.null(d) &&
          isTRUE((d$positivity_last %||% 0) > (d$positivity_first %||% 0) * 1.5)
      },
      key = "interpretation.fragment.denominator.rising_positivity"
    ),
    list(
      id = "denominator.stable",
      slot = "denominator",
      condition = function(cl) !is.null(cl$denominator),
      key = "interpretation.fragment.denominator.stable"
    ),

    # -- demography -----------------------------------------------------
    list(
      id = "demography.shifted",
      slot = "demography",
      condition = function(cl) isTRUE(cl$demography$shifted),
      key = "interpretation.fragment.demography.shifted"
    ),

    # -- completeness ---------------------------------------------------
    list(
      id = "completeness.warning",
      slot = "completeness",
      condition = function(cl) {
        isTRUE((cl$completeness$incomplete_days %||% 0) > 0)
      },
      key = "interpretation.fragment.completeness.warning"
    ),

    # -- recommendation ---------------------------------------------------
    list(
      id = "recommendation.rare_trigger",
      slot = "recommendation",
      condition = function(cl) {
        "rare_trigger" %in% (cl$detectors %||% character(0))
      },
      key = "interpretation.fragment.recommendation.rare_trigger"
    ),
    list(
      id = "recommendation.high_priority",
      slot = "recommendation",
      condition = function(cl) isTRUE((cl$priority_score %||% 0) >= 80),
      key = "interpretation.fragment.recommendation.high_priority"
    ),
    list(
      id = "recommendation.moderate_priority",
      slot = "recommendation",
      condition = function(cl) isTRUE((cl$priority_score %||% 0) >= 50),
      key = "interpretation.fragment.recommendation.moderate_priority"
    ),
    list(
      id = "recommendation.default",
      slot = "recommendation",
      condition = function(cl) TRUE,
      key = "interpretation.fragment.recommendation.default"
    )
  )
}

#' @rdname episodic_interpretation
#' @examples
#' episodic_interpretation_slots
#' @export
episodic_interpretation_slots <- c(
  "magnitude",
  "curve_shape",
  "concentration",
  "denominator",
  "demography",
  "completeness",
  "recommendation"
)

#' Generate the interpretation for a cluster
#'
#' @param cluster A cluster object, see `episodic_cluster_object()`.
#' @param lang Session language: `"en"`, `"ar"`, `"nl"`, `"fr"`, `"de"`,
#'   `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
#'   environment variable, falling back to `"en"` if that is unset.
#' @param instance_i18n Optional operator overrides, passed to [episodic_tr()].
#' @return A list with `text` (a character vector, one string per slot that
#'   fired, in slot order) and `fired` (a character vector of the fragment
#'   ids that fired, same order) - every fragment records which
#'   condition fired, so the interpretation is always traceable back to
#'   the evidence that produced it.
#' @keywords internal
#' @noRd
episodic_interpretation_generate <- function(
    cluster,
    lang = Sys.getenv("EPISODIC_LANGUAGE"),
    instance_i18n = NULL) {
  fragments <- episodic_interpretation_fragments()
  ctx <- episodic_interpretation_context(cluster, lang = lang)

  text <- character(0)
  fired <- character(0)

  for (slot in episodic_interpretation_slots) {
    slot_fragments <- Filter(function(f) f$slot == slot, fragments)
    for (fragment in slot_fragments) {
      if (
        isTRUE(tryCatch(fragment$condition(cluster), error = function(e) FALSE))
      ) {
        rendered <- do.call(
          episodic_tr,
          c(
            list(
              key = fragment$key,
              lang = lang,
              instance_i18n = instance_i18n
            ),
            ctx
          )
        )
        text <- c(text, rendered)
        fired <- c(fired, fragment$id)
        break # one fragment per slot
      }
    }
  }

  list(text = text, fired = fired)
}
