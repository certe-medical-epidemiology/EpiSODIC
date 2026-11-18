#!/usr/bin/env Rscript

# Figure 1 for the ESCMID Global 2027 abstract: the cluster dossier.
#
# The screenshot ships with the package, in man/figures/, regenerated
# against episodic_demo(). This crops the browser chrome away, which
# carries a local file path and a loopback URL that say nothing about
# EpiSODIC, and resizes to ESCMID's 1000 px ceiling.
#
# Needs ImageMagick's `convert` on the path.
#
# Usage:
#   Rscript figure1_dossier.R [source_png] [output_png]

args <- commandArgs(trailingOnly = TRUE)
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))

source_png <- if (length(args) >= 1) {
  path.expand(args[[1]])
} else {
  file.path(script_dir, "..", "..", "man", "figures", "main_screen.png")
}
out_png <- if (length(args) >= 2) {
  path.expand(args[[2]])
} else {
  file.path(script_dir, "figure1_dossier_0.25.0.png")
}

if (!file.exists(source_png)) {
  stop("no screenshot at ", source_png, call. = FALSE)
}
if (!nzchar(Sys.which("convert"))) {
  stop("ImageMagick's `convert` is not on the path", call. = FALSE)
}

# The crop is in the source image's own pixels: past the window title bar
# and the address bar, and inside the window's rounded border.
status <- system2(
  "convert",
  c(
    shQuote(source_png),
    "-crop", "1544x1112+16+95",
    "+repage",
    "-filter", "Lanczos",
    "-resize", "1000x1000",
    "-unsharp", "0x0.75+0.75+0.008",
    "-strip",
    paste0("PNG24:", shQuote(out_png))
  )
)
if (status != 0) {
  stop("convert failed with status ", status, call. = FALSE)
}

size <- file.info(out_png)$size
cat(sprintf("wrote %s (%.0f KB)\n", out_png, size / 1024))
