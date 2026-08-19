#' Resolve a pathogen code to an `AMR::as.mo()` identity
#'
#' `episode_pathogen_config.csv` ships placeholder `mo_code` values (see
#' `QUESTIONS.md` item 18), since real `AMR::as.mo()` codes cannot be
#' generated without the `AMR` package. When `AMR` is installed, this
#' resolves the human-readable `mo_name` through `AMR::as.mo()` and returns
#' the real code and taxonomic rank, so that `episode_stream` is keyed on the
#' genuine taxonomy wherever possible. Without `AMR`, the placeholder code
#' and the rank recorded in the config file are returned unchanged, which is
#' enough to run the bundled demo but is not a substitute for the real
#' lookup.
#'
#' @param mo_code The `mo_code` as it appears in `pathogen_config.csv`.
#' @param mo_name The human-readable organism name from the same file.
#' @param mo_rank The taxonomic rank from the same file (used only as a
#'   fallback when `AMR` is unavailable).
#' @return A list with elements `mo_code`, `mo_name` and `mo_rank`.
#' @export
episode_mo_resolve <- function(mo_code, mo_name, mo_rank) {
  if (requireNamespace("AMR", quietly = TRUE)) {
    resolved <- AMR::as.mo(mo_name)
    list(
      mo_code = as.character(resolved),
      mo_name = AMR::mo_name(resolved, language = NULL),
      mo_rank = AMR::mo_rank(resolved, language = NULL)
    )
  } else {
    list(mo_code = mo_code, mo_name = mo_name, mo_rank = mo_rank)
  }
}
