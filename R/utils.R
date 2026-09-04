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

doc_vect <- function(x) {
  paste0("one of: ", paste0('`"', x, '"`', collapse = ", "))
}

doc_palette <- function() {
  vals <- readLines(system.file("config", "episodic_default_style.yaml", package = "EpiSODIC"))
  vals <- vals[grepl(": ", vals) & !grepl("^#", vals)]
  vals_split <- strsplit(vals, ":")
  keys <- format(paste0(vapply(FUN.VALUE = character(1), vals_split, function(x) x[1]), ":"))
  values <- vapply(FUN.VALUE = character(1), vals_split, function(x) x[2])
  paste0("```yaml\n", paste0(paste(keys, values), collapse = "\n"), "\n```")
}

doc_system_file <- function(path) {
  urls <- trimws(strsplit(packageDescription("EpiSODIC")$URL, ",", fixed = TRUE)[[1]])
  url <- urls[grepl("github.com", urls)][1]
  url_remote <- paste0(url, "/blob/main/", path)
  return(paste0("[`", path, "`](", url_remote, ")"))
}
