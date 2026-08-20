# Small reusable UI building blocks

Thin
[`shiny::tags`](https://rstudio.github.io/htmltools/reference/builder.html)
wrappers for the interface's small recurring primitives (a chip, a
panel, a stat tile, a bar, a pyramid), server-side rendered rather than
a client-side component library. Styling lives in
`inst/app/www/episode.css`; these functions only assign class names and
content.
