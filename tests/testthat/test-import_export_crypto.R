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

# Keys and the envelope of an export file: everything that opens,
# opens only for the right key and the right sender, and everything that
# does not fails with a reason code rather than a partial result.

import_export_test_keys <- function() {
  dir <- tempfile("keys-")
  keys <- episodic_import_export_keygen(dir)
  keys$dir <- dir
  keys$encryption_secret_raw <- episodic_import_export_key_read(
    keys$encryption_secret, "encryption", "secret", "key"
  )
  keys$encryption_public_raw <- episodic_import_export_key_read(
    keys$encryption_public, "encryption", "public", "key"
  )
  keys$signing_secret_raw <- episodic_import_export_key_read(
    keys$signing_secret, "signing", "secret", "key"
  )
  keys$signing_public_raw <- episodic_import_export_key_read(
    keys$signing_public, "signing", "public", "key"
  )
  keys
}

import_export_reason <- function(expr) {
  condition <- tryCatch(expr, episodic_import_export_error = function(e) e)
  if (inherits(condition, "episodic_import_export_error")) condition$reason else NA_character_
}

test_that("keygen writes four files, and the secret ones only their owner can read", {
  keys <- import_export_test_keys()
  on.exit(unlink(keys$dir, recursive = TRUE))
  for (f in unlist(keys[c("encryption_secret", "encryption_public", "signing_secret", "signing_public")])) {
    expect_true(file.exists(f), info = f)
  }
  skip_on_os("windows")
  for (f in c(keys$encryption_secret, keys$signing_secret)) {
    expect_equal(as.integer(file.info(f)$mode) %% 64L, 0L, info = f)
  }
})

test_that("keygen refuses to replace keys unless told to", {
  keys <- import_export_test_keys()
  on.exit(unlink(keys$dir, recursive = TRUE))
  expect_error(episodic_import_export_keygen(keys$dir), "already exist")
  replaced <- episodic_import_export_keygen(keys$dir, overwrite = TRUE)
  expect_false(identical(replaced$signing_fingerprint, keys$signing_fingerprint))
})

test_that("a key file's comments are ignored and its key read back exactly", {
  keys <- import_export_test_keys()
  on.exit(unlink(keys$dir, recursive = TRUE))
  expect_length(keys$encryption_secret_raw, 32L)
  expect_length(keys$signing_secret_raw, 64L)
  expect_identical(sodium::pubkey(keys$encryption_secret_raw), keys$encryption_public_raw)
  expect_identical(sodium::sig_pubkey(keys$signing_secret_raw), keys$signing_public_raw)
  # The key text itself is accepted wherever a path is.
  text <- episodic_import_export_key_text("signing", "public", keys$signing_public_raw)
  expect_identical(
    episodic_import_export_key_read(text, "signing", "public", "key"),
    keys$signing_public_raw
  )
})

test_that("a key of the wrong kind is refused by name", {
  keys <- import_export_test_keys()
  on.exit(unlink(keys$dir, recursive = TRUE))
  expect_error(
    episodic_import_export_key_read(keys$signing_public, "encryption", "public", "The recipient key"),
    "is a signing public key, and an encryption public key is needed"
  )
  expect_error(
    episodic_import_export_key_read(keys$encryption_secret, "encryption", "public", "The recipient key"),
    "Never hand a secret key"
  )
  expect_identical(
    import_export_reason(episodic_import_export_key_read("not a key", "signing", "public", "key")),
    "no_key"
  )
  expect_identical(
    import_export_reason(episodic_import_export_key_read(NA_character_, "signing", "public", "key")),
    "no_key"
  )
  short <- "episodic-import-export-signing-public-v1:abcd"
  expect_identical(
    import_export_reason(episodic_import_export_key_read(short, "signing", "public", "key")),
    "bad_key"
  )
})

test_that("the fingerprint is the SHA-256 of the public key and refuses a secret key", {
  keys <- import_export_test_keys()
  on.exit(unlink(keys$dir, recursive = TRUE))
  expect_identical(episodic_import_export_key_fingerprint(keys$signing_public), keys$signing_fingerprint)
  expect_identical(episodic_import_export_key_fingerprint(keys$encryption_public), keys$encryption_fingerprint)
  expect_match(keys$signing_fingerprint, "^[0-9a-f]{64}$")
  expect_error(episodic_import_export_key_fingerprint(keys$signing_secret), "secret")
  expect_match(episodic_import_export_fingerprint_short(keys$signing_fingerprint), "^[0-9a-f]{4}( [0-9a-f]{4}){3}$")
})

test_that("a sealed file opens for each of several recipients, and for nobody else", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  b <- import_export_test_keys()
  stranger <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir, b$dir, stranger$dir), recursive = TRUE))
  body <- charToRaw("payload bytes")
  bytes <- episodic_import_export_seal(
    body,
    recipients = list(a$encryption_public_raw, b$encryption_public_raw),
    passphrase = NULL,
    signing_secret = sender$signing_secret_raw
  )
  for (recipient in list(a, b)) {
    opened <- episodic_import_export_open(
      bytes,
      secret_key = recipient$encryption_secret_raw,
      passphrase = NULL,
      trusted_senders = list(sender$signing_public_raw)
    )
    expect_identical(opened$body, body)
    expect_identical(opened$mode, "public_key")
    expect_identical(opened$signer_fingerprint, sender$signing_fingerprint)
  }
  expect_identical(
    import_export_reason(episodic_import_export_open(
      bytes, stranger$encryption_secret_raw, NULL, list(sender$signing_public_raw)
    )),
    "wrong_key"
  )
})

test_that("the plaintext header names neither the sender nor the payload", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir), recursive = TRUE))
  body <- charToRaw("SECRET-MARKER-SECRET-MARKER")
  bytes <- episodic_import_export_seal(body, list(a$encryption_public_raw), NULL, sender$signing_secret_raw)
  hex <- sodium::bin2hex(bytes)
  expect_false(grepl(sodium::bin2hex(charToRaw("SECRET-MARKER")), hex, fixed = TRUE))
  expect_false(grepl(sodium::bin2hex(sender$signing_public_raw), hex, fixed = TRUE))
  expect_false(grepl(sodium::bin2hex(a$encryption_public_raw), hex, fixed = TRUE))
})

test_that("a passphrase file opens with the passphrase and with nothing else", {
  sender <- import_export_test_keys()
  on.exit(unlink(sender$dir, recursive = TRUE))
  body <- charToRaw("payload bytes")
  passphrase <- "correct horse battery staple"
  bytes <- episodic_import_export_seal(body, NULL, passphrase, sender$signing_secret_raw)
  opened <- episodic_import_export_open(bytes, NULL, passphrase, list(sender$signing_public_raw))
  expect_identical(opened$body, body)
  expect_identical(opened$mode, "passphrase")
  expect_identical(
    import_export_reason(episodic_import_export_open(bytes, NULL, "wrong horse battery staple", list(sender$signing_public_raw))),
    "wrong_passphrase_or_tampered"
  )
  # Two files with one passphrase carry different salts, so different keys.
  again <- episodic_import_export_seal(body, NULL, passphrase, sender$signing_secret_raw)
  expect_false(identical(bytes[12:43], again[12:43]))
})

test_that("a file is opened the way it was sealed, not the other way", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir), recursive = TRUE))
  keyed <- episodic_import_export_seal(raw(1), list(a$encryption_public_raw), NULL, sender$signing_secret_raw)
  phrased <- episodic_import_export_seal(raw(1), NULL, "a passphrase of some length", sender$signing_secret_raw)
  expect_identical(import_export_reason(episodic_import_export_open(keyed, NULL, "x", list(sender$signing_public_raw))), "wrong_mode")
  expect_identical(import_export_reason(episodic_import_export_open(phrased, a$encryption_secret_raw, NULL, list(sender$signing_public_raw))), "wrong_mode")
})

test_that("a file from a sender nobody pinned is refused", {
  sender <- import_export_test_keys()
  other <- import_export_test_keys()
  a <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, other$dir, a$dir), recursive = TRUE))
  bytes <- episodic_import_export_seal(raw(4), list(a$encryption_public_raw), NULL, sender$signing_secret_raw)
  expect_identical(
    import_export_reason(episodic_import_export_open(bytes, a$encryption_secret_raw, NULL, list(other$signing_public_raw))),
    "untrusted_sender"
  )
  expect_identical(
    import_export_reason(episodic_import_export_open(bytes, a$encryption_secret_raw, NULL, list())),
    "untrusted_sender"
  )
})

test_that("content altered under a valid encryption fails the signature", {
  # In passphrase mode the test knows the content key, so it can decrypt,
  # alter one byte of the signed body and encrypt again: the authenticator
  # then holds and only the signature can catch it.
  sender <- import_export_test_keys()
  on.exit(unlink(sender$dir, recursive = TRUE))
  passphrase <- "a passphrase of some length"
  bytes <- episodic_import_export_seal(charToRaw("original body"), NULL, passphrase, sender$signing_secret_raw)
  header_n <- 8L + 2L + 1L + 32L + 24L
  salt <- bytes[12:43]
  nonce <- bytes[44:67]
  key <- sodium::scrypt(charToRaw(passphrase), salt = salt, size = 32)
  inner <- sodium::data_decrypt(bytes[(header_n + 1):length(bytes)], key, nonce = nonce)
  inner[length(inner)] <- as.raw(bitwXor(as.integer(inner[length(inner)]), 1L))
  forged <- c(bytes[seq_len(header_n)], as.raw(sodium::data_encrypt(inner, key, nonce = nonce)))
  expect_identical(
    import_export_reason(episodic_import_export_open(forged, NULL, passphrase, list(sender$signing_public_raw))),
    "bad_signature"
  )
})

test_that("a flipped byte anywhere in the encrypted content is refused", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir), recursive = TRUE))
  bytes <- episodic_import_export_seal(charToRaw("payload bytes"), list(a$encryption_public_raw), NULL, sender$signing_secret_raw)
  for (i in c(length(bytes), length(bytes) - 20L, 8L + 2L + 1L + 2L + 80L + 24L + 1L)) {
    flipped <- bytes
    flipped[i] <- as.raw(bitwXor(as.integer(flipped[i]), 255L))
    expect_identical(
      import_export_reason(episodic_import_export_open(flipped, a$encryption_secret_raw, NULL, list(sender$signing_public_raw))),
      "tampered",
      info = i
    )
  }
})

test_that("a truncated file is refused, wherever it was cut", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir), recursive = TRUE))
  bytes <- episodic_import_export_seal(charToRaw("payload bytes"), list(a$encryption_public_raw), NULL, sender$signing_secret_raw)
  for (n in c(9L, 12L, 50L, 8L + 2L + 1L + 2L + 80L + 24L + 10L, length(bytes) - 1L)) {
    reason <- import_export_reason(episodic_import_export_open(
      bytes[seq_len(n)], a$encryption_secret_raw, NULL, list(sender$signing_public_raw)
    ))
    expect_true(reason %in% c("truncated", "tampered"), info = n)
  }
})

test_that("a file in a newer format version is refused with an instruction", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir), recursive = TRUE))
  bytes <- episodic_import_export_seal(raw(4), list(a$encryption_public_raw), NULL, sender$signing_secret_raw)
  newer <- bytes
  newer[9:10] <- episodic_import_export_u16(episodic_import_export_format_version + 1L)
  expect_error(
    episodic_import_export_open(newer, a$encryption_secret_raw, NULL, list(sender$signing_public_raw)),
    "Upgrade EpiSODIC"
  )
  expect_identical(
    import_export_reason(episodic_import_export_open(newer, a$encryption_secret_raw, NULL, list(sender$signing_public_raw))),
    "unsupported_version"
  )
})

test_that("a file that is not an export file is refused as such", {
  expect_identical(
    import_export_reason(episodic_import_export_open(charToRaw("PK\003\004 a zip file"), raw(32), NULL, list())),
    "not_export_file"
  )
  expect_identical(import_export_reason(episodic_import_export_open(raw(0), raw(32), NULL, list())), "not_export_file")
})

test_that("sealing leaves the session's random number stream where it was", {
  sender <- import_export_test_keys()
  a <- import_export_test_keys()
  b <- import_export_test_keys()
  on.exit(unlink(c(sender$dir, a$dir, b$dir), recursive = TRUE))
  set.seed(42)
  expected <- stats::runif(3)
  set.seed(42)
  episodic_import_export_seal(raw(4), list(a$encryption_public_raw, b$encryption_public_raw), NULL, sender$signing_secret_raw)
  expect_identical(stats::runif(3), expected)
})

# ---- Payload ---------------------------------------------------------------

import_export_test_table <- function() {
  data.frame(
    source_key = c("a", "b", NA),
    age = c(34L, NA, 7L),
    stringsAsFactors = FALSE
  )
}

test_that("a payload round-trips its tables, NA as NA and never as a value", {
  types <- list(t = c(source_key = "character", age = "integer"))
  body <- episodic_import_export_payload_build(
    list(sender_instance_id = "abcdef0123", fields = I("source_key")),
    list(t = import_export_test_table()),
    types
  )
  read <- episodic_import_export_payload_read(body, types)
  expect_equal(read$tables$t, import_export_test_table())
  expect_identical(read$manifest$sender_instance_id, "abcdef0123")
  # A list of one stays a list.
  expect_identical(read$manifest$fields, list("source_key"))
})

test_that("an empty table round-trips with its columns", {
  types <- list(t = c(source_key = "character", age = "integer"))
  empty <- import_export_test_table()[0, ]
  rownames(empty) <- NULL
  read <- episodic_import_export_payload_read(
    episodic_import_export_payload_build(list(), list(t = empty), types),
    types
  )
  expect_identical(nrow(read$tables$t), 0L)
  expect_identical(names(read$tables$t), c("source_key", "age"))
  expect_type(read$tables$t$age, "integer")
})

test_that("a table that does not match its recorded hash is refused whole", {
  types <- list(t = c(source_key = "character", age = "integer"))
  body <- episodic_import_export_payload_build(list(), list(t = import_export_test_table()), types)
  json <- rawToChar(memDecompress(body, type = "gzip"))
  altered <- sub("34", "35", json, fixed = TRUE)
  expect_false(identical(altered, json))
  expect_identical(
    import_export_reason(episodic_import_export_payload_read(memCompress(charToRaw(altered), type = "gzip"), types)),
    "hash_mismatch"
  )
})

test_that("a payload from a newer EpiSODIC is refused, and so is one with unknown tables", {
  types <- list(t = c(source_key = "character", age = "integer"))
  body <- episodic_import_export_payload_build(list(), list(t = import_export_test_table()), types)
  json <- rawToChar(memDecompress(body, type = "gzip"))
  newer <- sub('"payload_version":1', '"payload_version":2', json, fixed = TRUE)
  expect_identical(
    import_export_reason(episodic_import_export_payload_read(memCompress(charToRaw(newer), type = "gzip"), types)),
    "unsupported_version"
  )
  expect_identical(
    import_export_reason(episodic_import_export_payload_read(body, list(other = types$t))),
    "bad_payload"
  )
})

test_that("a value of the wrong type is refused, not coerced", {
  expect_error(episodic_import_export_column_values(list("x"), "integer", "age", "cases"), "whole number")
  expect_error(episodic_import_export_column_values(list(1.5), "integer", "age", "cases"), "whole number")
  expect_error(episodic_import_export_column_values(list(1), "character", "pc", "cases"), "text")
  expect_identical(episodic_import_export_column_values(list(NULL, 3), "integer", "age", "cases"), c(NA_integer_, 3L))
})
