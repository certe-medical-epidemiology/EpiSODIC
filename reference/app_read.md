# App read models

Every function here is a cheap read against an already-populated
database. Nothing here recomputes a statistical model; anything
expensive (Farrington, reconciliation, priority scoring) has already
happened in the cron and is simply looked up. This is the layer that
turns raw tables into the shapes the Shiny UI and the interpretation
engine consume.
