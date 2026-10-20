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

# Fixtures for the exchange tests: two instances' worth of keys, a small
# hand-made case set whose keys collide with another laboratory's on
# purpose, and a sender database holding it.

# A key directory per party, removed by the caller.
exchange_test_party <- function() {
  dir <- tempfile("exchange-party-")
  keys <- episodic_exchange_keygen(dir)
  keys$dir <- dir
  keys
}

# Four cases at four different patients, so none is folded into
# another's episode. Keys that another laboratory might just as well use,
# and NA where the case data format allows it.
exchange_test_cases <- function(prefix = "") {
  data.frame(
    source_key = paste0(prefix, c("12345", "12346", "12347", "12348")),
    lab_number = paste0(prefix, c("L-1", "L-2", "L-3", "L-4")),
    patient_key = paste0(prefix, c("P001", "P002", "P003", "P004")),
    sample_date = c("2025-03-03", "2025-03-04", "2025-03-10", "2025-04-02"),
    receipt_date = c("2025-03-04", NA, "2025-03-11", "2025-04-03"),
    pathogen = "Norovirus",
    care_line = c("second", "second", NA, "first"),
    institution_key = c("HOSP-01", "HOSP-01", "HOSP-01", "GP-01"),
    institution_display_name = c("Test Hospital", "Test Hospital", "Test Hospital", "Test GP"),
    institution_type = c("hospital", "hospital", "hospital", "gp_municipality"),
    municipality = c("Groningen", "Groningen", "Groningen", "Assen"),
    ward = c("A2", "A2", "B1", NA),
    specialism = c("Internal medicine", NA, "Surgery", NA),
    pc = c("9713", "9714", NA, "9401"),
    sex = c("F", "M", "U", NA),
    age = c(71L, 8L, NA, 45L),
    stringsAsFactors = FALSE
  )
}

# A sender's database holding `cases` as its own, loaded the way a run
# loads them. The caller unlinks the returned path.
exchange_test_sender_db <- function(cases = exchange_test_cases()) {
  path <- episodic_test_db_path()
  con <- episodic_db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  run_id <- episodic_db_run_start(con, "test", "test", as.Date("2025-04-30"))
  suppressMessages(episodic_cases_load(
    con,
    cases,
    episodic_pathogen_config_resolve(NA),
    run_id
  ))
  episodic_db_run_finish(con, run_id, status = "success")
  path
}

# An admin account to record schedules and sources under.
exchange_test_admin <- function(con) {
  episodic_db_app_user_insert(
    con,
    username = paste0("admin-", episodic_instance_id_generate()),
    full_name = "Exchange Admin",
    email = "admin@example.org",
    password_hash = "x",
    role = "epidemiologist"
  )
}

# The text of a public key file's key line.
exchange_test_key_line <- function(path) {
  lines <- readLines(path)
  lines[!startsWith(lines, "#")]
}
