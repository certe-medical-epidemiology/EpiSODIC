# App write orchestration

`R/db_app_write.R` exposes one insert per table. The functions here
combine those into the actual actions a signed-in assessor takes, adding
the one piece of bookkeeping a raw insert cannot do on its own:
recording every state transition. Still insert-only throughout - nothing
here issues `UPDATE` or `DELETE`.
