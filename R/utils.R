# ===================================================================== #
#  This open-source project is publicly available at:                   #
#  https://github.com/msberends/EpiSODIC                                #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at the University Medical Center Groningen, and non-profit #
#  organisation Certe Medical Diagnostics & Advice, in the Netherlands. #
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

doc_vect <- function(x) {
  paste0("one of: ", paste0('`"', x, '"`', collapse = ", "))
}

doc_palette <- function() {
  lines <- readLines(system.file("config", "episodic_default_style.yaml", package = "EpiSODIC"))
  # From the file's own description on: the licence banner above it is
  # not part of what an operator copies.
  start <- grep("^# EpiSODIC's shipped default palette", lines)[1]
  paste0("```yaml\n", paste(lines[start:length(lines)], collapse = "\n"), "\n```")
}

#' How many foreign keys the schema declares
#'
#' For `vignette("deployment")`, which tells an operator what to expect
#' from `information_schema.REFERENTIAL_CONSTRAINTS` on their own server.
#' Counted from the schema rather than written down, so a reference added
#' to `inst/sql/schema.sql` does not leave the vignette quoting a number
#' that stopped being true.
#' @return A single integer.
#' @keywords internal
#' @noRd
doc_foreign_key_count <- function() {
  sum(vapply(
    strsplit(episodic_db_schema_statements("mariadb"), "\n", fixed = TRUE),
    function(lines) sum(grepl("^\\s*FOREIGN KEY \\(", lines, perl = TRUE)),
    integer(1)
  ))
}

doc_system_file <- function(path) {
  urls <- trimws(strsplit(utils::packageDescription("EpiSODIC")$URL, ",", fixed = TRUE)[[1]])
  url <- urls[grepl("github.com", urls)][1]
  url_remote <- paste0(url, "/blob/main/", path)
  return(paste0("[`", path, "`](", url_remote, ")"))
}
