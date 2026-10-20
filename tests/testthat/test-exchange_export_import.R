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

# Exporting a range from one instance and importing it at another: what
# leaves, what arrives, what is refused, and what the receiving run makes
# of it. Helpers in helper-exchange.R.

exchange_test_export <- function(db, receiver, sender, file = tempfile(fileext = ".episodic"), ...) {
  episodic_exchange_export(
    db_path = db,
    from = "2025-03-01",
    to = "2025-03-31",
    file = file,
    recipients = receiver$encryption_public,
    signing_key = sender$signing_secret,
    ...
  )
}

exchange_test_import <- function(file, receiver, sender, ...) {
  suppressWarnings(episodic_exchange_import(
    file,
    key = receiver$encryption_secret,
    trusted_senders = sender$signing_public,
    ...
  ))
}

test_that("a range round-trips: the sender's own cases in it, keys prefixed, institution as stored", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, receiver$dir, db), recursive = TRUE))

  out <- exchange_test_export(db, receiver, sender)
  expect_equal(out$n_cases, 3L) # the April case is outside the range
  imported <- exchange_test_import(out$file, receiver, sender)

  id <- episodic_instance_id(db)
  expect_s3_class(imported, "episodic_exchange_cases")
  expect_identical(names(imported), episodic_case_columns)
  expect_setequal(imported$source_key, paste0(id, ":", c("12345", "12346", "12347")))
  expect_true(all(startsWith(imported$patient_key, paste0(id, ":"))))
  expect_true(all(startsWith(imported$lab_number, paste0(id, ":"))))
  expect_true(all(imported$institution_key == episodic_institution_key_hash("HOSP-01")))
  meta <- attr(imported, "exchange")
  expect_identical(meta$sender_instance_id, id)
  expect_identical(meta$signer_fingerprint, sender$signing_fingerprint)
  expect_identical(meta$from, "2025-03-01")
  expect_identical(meta$to, "2025-03-31")
  expect_identical(meta$file_sha256, out$file_sha256)
  # NA stays NA.
  row <- imported[imported$source_key == paste0(id, ":12346"), ]
  expect_true(is.na(row$receipt_date))
  expect_true(is.na(row$specialism))
})

test_that("one file reaches several recipients", {
  sender <- exchange_test_party()
  b <- exchange_test_party()
  d <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, b$dir, d$dir, db), recursive = TRUE))
  file <- tempfile(fileext = ".episodic")
  episodic_exchange_export(
    db_path = db, from = "2025-03-01", to = "2025-03-31", file = file,
    recipients = c(b$encryption_public, d$encryption_public),
    signing_key = sender$signing_secret
  )
  expect_equal(nrow(exchange_test_import(file, b, sender)), 3L)
  expect_equal(nrow(exchange_test_import(file, d, sender)), 3L)
})

test_that("a passphrase export round-trips, and a short passphrase is refused", {
  sender <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, db), recursive = TRUE))
  file <- tempfile(fileext = ".episodic")
  passphrase <- "a long enough passphrase"
  episodic_exchange_export(
    db_path = db, from = "2025-03-01", to = "2025-03-31", file = file,
    passphrase = passphrase, signing_key = sender$signing_secret
  )
  imported <- suppressWarnings(episodic_exchange_import(
    file,
    trusted_senders = sender$signing_public,
    passphrase = passphrase
  ))
  expect_equal(nrow(imported), 3L)
  expect_identical(attr(imported, "exchange")$mode, "passphrase")
  expect_error(
    episodic_exchange_export(
      db_path = db, from = "2025-03-01", to = "2025-03-31",
      file = tempfile(), passphrase = "short", signing_key = sender$signing_secret
    ),
    "at least 16"
  )
})

test_that("an export names either recipients or a passphrase", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, receiver$dir, db), recursive = TRUE))
  expect_error(
    episodic_exchange_export(
      db_path = db, from = "2025-03-01", to = "2025-03-31", file = tempfile(),
      signing_key = sender$signing_secret
    ),
    "not both and not neither"
  )
  expect_error(
    episodic_exchange_export(
      db_path = db, from = "2025-03-01", to = "2025-03-31", file = tempfile(),
      recipients = receiver$encryption_public, passphrase = "a long enough passphrase",
      signing_key = sender$signing_secret
    ),
    "not both and not neither"
  )
})

test_that("a field left out is absent from the file and NA on import, never a default", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, receiver$dir, db), recursive = TRUE))
  out <- exchange_test_export(db, receiver, sender, fields = c("pc", "ward"), pc_characters = 2)
  expect_setequal(
    out$omitted_fields,
    c("receipt_date", "care_line", "municipality", "specialism", "sex", "age")
  )
  imported <- exchange_test_import(out$file, receiver, sender)
  expect_true(all(is.na(imported$sex))) # not "U"
  expect_true(all(is.na(imported$age))) # not 0
  expect_true(all(is.na(imported$care_line))) # not "unknown"
  expect_true(all(is.na(imported$receipt_date)))
  expect_true(all(nchar(imported$pc[!is.na(imported$pc)]) == 2L))
  expect_identical(sort(attr(imported, "exchange")$omitted_fields), sort(out$omitted_fields))
  # The left-out values are not in the payload at all.
  payload <- episodic_exchange_open(
    readBin(out$file, "raw", file.info(out$file)$size),
    episodic_exchange_key_read(receiver$encryption_secret, "encryption", "secret", "key"),
    NULL,
    list(episodic_exchange_key_read(sender$signing_public, "signing", "public", "key"))
  )$body
  json <- rawToChar(memDecompress(payload, type = "gzip"))
  expect_false(grepl("Internal medicine", json, fixed = TRUE))
  expect_false(grepl("Groningen", json, fixed = TRUE))
})

test_that("an export without a field list uses the configured one", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  config <- tempfile(fileext = ".yaml")
  writeLines(c("exchange:", "  export_fields: [pc]"), config)
  on.exit(unlink(c(sender$dir, receiver$dir, db, config), recursive = TRUE))
  out <- exchange_test_export(db, receiver, sender, episodic_config_path = config)
  expect_true("age" %in% out$omitted_fields)
  expect_false("pc" %in% out$omitted_fields)
})

test_that("a field that is not an optional case column is refused", {
  expect_error(
    episodic_exchange_fields(c("pc", "postcode"), episodic_config_resolve()),
    "postcode"
  )
  # A required column named in the list is simply exported, as always.
  selection <- episodic_exchange_fields(c("source_key", "pc"), episodic_config_resolve())
  expect_true("source_key" %in% selection$fields)
})

test_that("every export is recorded, with the file's hash and the recipients' fingerprints", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, receiver$dir, db), recursive = TRUE))
  out <- exchange_test_export(db, receiver, sender)
  con <- episodic_db_connect(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  row <- DBI::dbGetQuery(con, "SELECT * FROM episodic_exchange_export_event")
  expect_equal(nrow(row), 1L)
  expect_identical(row$status, "written")
  expect_identical(row$initiated_by, "headless")
  expect_identical(row$file_sha256, out$file_sha256)
  expect_identical(
    row$file_sha256,
    sodium::bin2hex(sodium::sha256(readBin(out$file, "raw", file.info(out$file)$size)))
  )
  expect_identical(jsonlite::fromJSON(row$recipient_fingerprints), receiver$encryption_fingerprint)
  expect_identical(row$signer_fingerprint, sender$signing_fingerprint)
  expect_equal(row$n_cases, 3L)
  expect_identical(row$date_from, "2025-03-01")
})

test_that("a failed export is recorded and raised, and writes no file", {
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(receiver$dir, db), recursive = TRUE))
  file <- tempfile(fileext = ".episodic")
  expect_error(
    episodic_exchange_export(
      db_path = db, from = "2025-03-01", to = "2025-03-31", file = file,
      recipients = receiver$encryption_public, signing_key = NA_character_
    ),
    "EPISODIC_EXCHANGE_SIGNING_KEY"
  )
  expect_false(file.exists(file))
  con <- episodic_db_connect(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  row <- DBI::dbGetQuery(con, "SELECT status, error_text FROM episodic_exchange_export_event")
  expect_identical(row$status, "failed")
  expect_match(row$error_text, "signing key")
})

test_that("an import is refused for the wrong key, an untrusted sender and a damaged file", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  stranger <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, receiver$dir, stranger$dir, db), recursive = TRUE))
  out <- exchange_test_export(db, receiver, sender)
  reason <- function(expr) tryCatch(expr, episodic_exchange_error = function(e) e$reason)
  expect_identical(reason(exchange_test_import(out$file, stranger, sender)), "wrong_key")
  expect_identical(reason(exchange_test_import(out$file, receiver, stranger)), "untrusted_sender")
  bytes <- readBin(out$file, "raw", file.info(out$file)$size)
  bytes[length(bytes) - 5L] <- as.raw(bitwXor(as.integer(bytes[length(bytes) - 5L]), 1L))
  expect_identical(reason(exchange_test_import(bytes, receiver, sender)), "tampered")
  truncated <- readBin(out$file, "raw", file.info(out$file)$size - 100L)
  expect_true(reason(exchange_test_import(truncated, receiver, sender)) %in% c("truncated", "tampered"))
  expect_error(
    episodic_exchange_import(out$file, key = receiver$encryption_secret, trusted_senders = character(0)),
    class = "episodic_exchange_error"
  )
})

test_that("cases imported from a partner are never exported onward", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, receiver$dir, db), recursive = TRUE))
  con <- episodic_db_connect(db)
  run_id <- episodic_db_run_start(con, "test", "test", as.Date("2025-04-30"))
  partner <- episodic_exchange_prefix_keys(exchange_test_cases(), "0123456789")
  partner$institution_key <- episodic_institution_key_hash(partner$institution_key)
  suppressMessages(episodic_cases_load(
    con, partner, episodic_pathogen_config_resolve(NA), run_id,
    origin_instance_id = "0123456789"
  ))
  DBI::dbDisconnect(con)
  out <- exchange_test_export(db, receiver, sender)
  expect_equal(out$n_cases, 3L)
  imported <- exchange_test_import(out$file, receiver, sender)
  expect_false(any(grepl("0123456789:", imported$source_key, fixed = TRUE)))
})

test_that("a receiving run keeps both laboratories' colliding keys, and does not re-hash the institution", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  receiver_db <- episodic_test_db_path()
  on.exit(unlink(c(sender$dir, receiver$dir, db, receiver_db), recursive = TRUE))
  out <- exchange_test_export(db, receiver, sender)
  imported <- exchange_test_import(out$file, receiver, sender)

  # The receiver's own cases use the very same source_key, patient_key and
  # lab_number values as the sender's.
  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = exchange_test_cases(),
    imports = imported,
    db_path = receiver_db,
    run_date = as.Date("2025-04-30")
  )))
  con <- episodic_db_connect(receiver_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  stored <- DBI::dbGetQuery(con, "SELECT source_key, patient_key, origin_instance_id FROM episodic_case")
  sender_id <- episodic_instance_id(db)
  expect_equal(nrow(stored), 7L) # 4 own + 3 imported, none skipped
  expect_true("12345" %in% stored$source_key)
  expect_true(paste0(sender_id, ":12345") %in% stored$source_key)
  expect_identical(
    sort(unique(stored$origin_instance_id[startsWith(stored$source_key, sender_id)])),
    sender_id
  )
  expect_true(all(is.na(stored$origin_instance_id[!startsWith(stored$source_key, sender_id)])))
  # One hospital, not two: the sender's stored hash is the receiver's.
  institutions <- DBI::dbGetQuery(con, "SELECT institution_key FROM episodic_institution")
  expect_true(episodic_institution_key_hash("HOSP-01") %in% institutions$institution_key)
  expect_equal(nrow(institutions), 2L)

  events <- DBI::dbGetQuery(con, "SELECT * FROM episodic_exchange_import_event")
  expect_equal(nrow(events), 1L)
  expect_identical(events$status, "imported")
  expect_identical(events$sender_instance_id, sender_id)
  expect_identical(events$file_sha256, out$file_sha256)
  run <- DBI::dbGetQuery(con, "SELECT run_id, status FROM episodic_detection_run")
  expect_identical(events$run_id, run$run_id)
})

test_that("an imported institution never renames one the receiver already knows", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  renamed <- exchange_test_cases()
  renamed$institution_display_name[renamed$institution_key == "HOSP-01"] <- "Partner's spelling"
  db <- exchange_test_sender_db(renamed)
  receiver_db <- episodic_test_db_path()
  on.exit(unlink(c(sender$dir, receiver$dir, db, receiver_db), recursive = TRUE))
  imported <- exchange_test_import(exchange_test_export(db, receiver, sender)$file, receiver, sender)
  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = exchange_test_cases(),
    db_path = receiver_db,
    run_date = as.Date("2025-04-29")
  )))
  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = exchange_test_cases()[0, ],
    imports = imported,
    db_path = receiver_db,
    run_date = as.Date("2025-04-30")
  )))
  con <- episodic_db_connect(receiver_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  name <- DBI::dbGetQuery(
    con,
    "SELECT display_name FROM episodic_institution WHERE institution_key = ?",
    params = list(episodic_institution_key_hash("HOSP-01"))
  )$display_name
  expect_identical(name, "Test Hospital")
})

test_that("the same file imported twice, in one run or in two, is taken in once", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  receiver_db <- episodic_test_db_path()
  on.exit(unlink(c(sender$dir, receiver$dir, db, receiver_db), recursive = TRUE))
  imported <- exchange_test_import(exchange_test_export(db, receiver, sender)$file, receiver, sender)
  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = exchange_test_cases()[0, ],
    imports = list(imported, imported),
    db_path = receiver_db,
    run_date = as.Date("2025-04-29")
  )))
  suppressMessages(suppressWarnings(episodic_run_cron(
    cases = exchange_test_cases()[0, ],
    imports = imported,
    db_path = receiver_db,
    run_date = as.Date("2025-04-30")
  )))
  con <- episodic_db_connect(receiver_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_case")$n, 3L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM episodic_exchange_import_event")$n, 3L)
})

test_that("a run refuses an import of its own export, and one whose keys lost their prefix", {
  sender <- exchange_test_party()
  db <- exchange_test_sender_db()
  on.exit(unlink(c(sender$dir, db), recursive = TRUE))
  # Exported to the sender's own key, so it can open its own file.
  out <- exchange_test_export(db, sender, sender)
  own <- exchange_test_import(out$file, sender, sender)
  expect_error(
    suppressMessages(episodic_run_cron(
      cases = exchange_test_cases()[0, ], imports = own, db_path = db,
      run_date = as.Date("2025-04-30")
    )),
    "this instance itself"
  )
  receiver_db <- episodic_test_db_path()
  on.exit(unlink(receiver_db), add = TRUE)
  altered <- own
  altered$source_key <- sub("^[0-9a-f]{10}:", "", altered$source_key)
  expect_error(
    suppressMessages(episodic_run_cron(
      cases = exchange_test_cases()[0, ], imports = altered, db_path = receiver_db,
      run_date = as.Date("2025-04-30")
    )),
    "without its prefix"
  )
})

test_that("`imports` takes only what episodic_exchange_import() returns", {
  expect_error(episodic_exchange_imports_normalise(exchange_test_cases()), "goes in `cases`")
  expect_identical(episodic_exchange_imports_normalise(NULL), list())
})

test_that("cases an import cannot place are kept, with a warning per level", {
  sender <- exchange_test_party()
  receiver <- exchange_test_party()
  db <- exchange_test_sender_db()
  map <- tempfile(fileext = ".csv")
  writeLines(c("pc,province_code", "9713,GR"), map)
  on.exit(unlink(c(sender$dir, receiver$dir, db, map), recursive = TRUE))
  out <- exchange_test_export(db, receiver, sender)
  withr::local_envvar(EPISODIC_PC_PROVINCE_MAP = map)
  warnings <- testthat::capture_warnings(imported <- episodic_exchange_import(
    out$file,
    key = receiver$encryption_secret,
    trusted_senders = sender$signing_public
  ))
  expect_equal(nrow(imported), 3L) # nothing dropped
  expect_true(any(grepl("carry no postcode", warnings)))
  expect_true(any(grepl("EPISODIC_PC_PROVINCE_MAP does not list", warnings)))
  expect_identical(attr(imported, "exchange")$warnings, warnings)
})

test_that("a postcode cut shorter than the receiver's areas is said so", {
  warnings <- episodic_exchange_geography_warnings(
    data.frame(pc = c("9", "8"), stringsAsFactors = FALSE),
    episodic_config_resolve(),
    pc_characters = 1L,
    province_map_path = NA
  )
  expect_true(any(grepl("streams of their own", warnings)))
  expect_true(any(grepl("No EPISODIC_PC_PROVINCE_MAP is configured", warnings)))
})
