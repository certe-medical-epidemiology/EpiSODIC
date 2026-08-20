# Shared read helpers

Plain `DBI`-based readers used by both the cron and the app. Read-only:
none of these functions write to the database. All SQL for the package
lives behind functions in `R/db_*.R`; nothing outside this layer issues
SQL directly.
