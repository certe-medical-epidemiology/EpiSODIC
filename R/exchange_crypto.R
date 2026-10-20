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

# Keys and the encrypted envelope of an exchange file.
#
# An exchange file is built so that it is safe to keep in a public place:
# without a recipient's key it reveals nothing beyond its own size, and
# without the sender's signing key it cannot be forged or altered. Every
# primitive is libsodium's, through the `sodium` package already in
# Imports:
#
# - the payload is encrypted once with a random 32-byte content key
#   (XSalsa20-Poly1305, `sodium::data_encrypt()`), so tampering with any
#   byte of it, or cutting it short, fails authentication;
# - in public-key mode that content key is wrapped once per recipient in a
#   sealed box (X25519, `sodium::simple_encrypt()`), which names no
#   recipient: a file shows how many recipients it has, never who;
# - in passphrase mode the content key is derived from the passphrase with
#   scrypt and a random 32-byte salt (`sodium::scrypt()`), never taken
#   from the passphrase directly;
# - in both modes the decrypted content is signed with the sender's
#   Ed25519 key (`sodium::sig_sign()`) over the plaintext header as well,
#   and the importer refuses a signature from a key it has not pinned.
#
# The layout is documented byte for byte in `vignette("exchange")`; the
# constants below are the only place it is written down in code.

#' Envelope constants of exchange format version 1
#' @keywords internal
#' @noRd
episodic_exchange_magic <- charToRaw("EPSDXCHG")

#' @keywords internal
#' @noRd
episodic_exchange_format_version <- 1L

#' @keywords internal
#' @noRd
episodic_exchange_modes <- c(public_key = 1L, passphrase = 2L)

#' Byte sizes the format relies on, as libsodium defines them
#'
#' A sealed box adds 48 bytes to what it seals (`crypto_box_SEALBYTES`),
#' so a wrapped 32-byte content key is 80 bytes; a secret box adds a
#' 16-byte authenticator (`crypto_secretbox_MACBYTES`) and takes a 24-byte
#' nonce; scrypt as `sodium::scrypt()` calls it takes exactly 32 bytes of
#' salt; an Ed25519 signature is 64 bytes and its public key 32.
#' @keywords internal
#' @noRd
episodic_exchange_sizes <- list(
  content_key = 32L,
  wrapped_key = 80L,
  nonce = 24L,
  mac = 16L,
  salt = 32L,
  signature = 64L,
  signing_public = 32L,
  max_recipients = 1000L
)

#' The minimum passphrase length an export accepts
#'
#' Passphrase mode is the weaker of the two by construction: anyone who
#' holds the passphrase can read every file, and a short one can be
#' guessed offline against a file kept in a public place, scrypt or not.
#' Sixteen characters keeps that guessing out of reach for a passphrase
#' chosen at random; it is a floor, not a recommendation. Imports accept
#' whatever passphrase the file was written with.
#' @keywords internal
#' @noRd
episodic_exchange_passphrase_min_chars <- 16L

#' Raise an exchange failure
#'
#' Every way an exchange can fail raises this one condition class, with a
#' `reason` code beside the message, so a caller (a scheduled run, a
#' test) can tell a wrong key from a forged file without parsing English.
#' @param reason A short code, e.g. `"wrong_key"`.
#' @param ... Pasted into the message.
#' @keywords internal
#' @noRd
episodic_exchange_fail <- function(reason, ...) {
  stop(structure(
    class = c("episodic_exchange_error", "error", "condition"),
    list(message = paste0(...), call = NULL, reason = reason)
  ))
}

#' The key kinds an exchange uses, and their raw lengths
#'
#' An encryption key pair (X25519) receives files; a signing key pair
#' (Ed25519) sends them. libsodium's Ed25519 secret key is 64 bytes: the
#' 32-byte seed followed by the public key.
#' @keywords internal
#' @noRd
episodic_exchange_key_lengths <- list(
  encryption = c(secret = 32L, public = 32L),
  signing = c(secret = 64L, public = 32L)
)

#' Write a key as the one-line text a key file holds
#'
#' The type is spelled out in the text itself, so a signing key handed
#' over where an encryption key is expected, or a secret key where a
#' public one is, is refused by name rather than producing a file nobody
#' can open.
#' @param kind `"encryption"` or `"signing"`.
#' @param part `"secret"` or `"public"`.
#' @param raw The key bytes.
#' @return A single string, e.g. `"episodic-exchange-signing-public-v1:<hex>"`.
#' @keywords internal
#' @noRd
episodic_exchange_key_text <- function(kind, part, raw) {
  paste0("episodic-exchange-", kind, "-", part, "-v1:", sodium::bin2hex(raw))
}

#' The pattern a key text matches
#' @keywords internal
#' @noRd
episodic_exchange_key_pattern <- "^episodic-exchange-(encryption|signing)-(secret|public)-v1:([0-9a-f]+)$"

#' "a" or "an" before a key kind, for the messages that name one
#' @keywords internal
#' @noRd
episodic_exchange_article <- function(kind) {
  if (grepl("^[aeiou]", kind)) "an" else "a"
}

#' Read a key from its text, refusing any other kind of key
#'
#' @param text A key text, as `episodic_exchange_key_text()` writes it.
#' @param kind,part What the caller needs.
#' @param what How the error names the key, e.g. `"the recipient key"`.
#' @return The raw key.
#' @keywords internal
#' @noRd
episodic_exchange_key_parse <- function(text, kind, part, what) {
  text <- trimws(text)
  match <- regmatches(text, regexec(episodic_exchange_key_pattern, text))[[1]]
  if (length(match) == 0) {
    episodic_exchange_fail(
      "bad_key",
      what, " is not an EpiSODIC exchange key. A key file holds one line ",
      "starting with \"episodic-exchange-\", as episodic_exchange_keygen() ",
      "writes it."
    )
  }
  if (!identical(match[2], kind) || !identical(match[3], part)) {
    episodic_exchange_fail(
      "bad_key",
      what, " is ", episodic_exchange_article(match[2]), " ", match[2], " ",
      match[3], " key, and ", episodic_exchange_article(kind), " ", kind, " ",
      part, " key is needed here. ",
      if (identical(match[3], "secret") && identical(part, "public")) {
        "Never hand a secret key to anyone; give the .pub file instead."
      } else {
        "Check which of the files episodic_exchange_keygen() wrote you passed."
      }
    )
  }
  if (nchar(match[4]) %% 2L != 0L) {
    episodic_exchange_fail("bad_key", what, " is cut short: its hex has an odd length.")
  }
  raw <- sodium::hex2bin(match[4])
  expected <- episodic_exchange_key_lengths[[kind]][[part]]
  if (length(raw) != expected) {
    episodic_exchange_fail(
      "bad_key",
      what, " holds ", length(raw), " bytes, and ",
      episodic_exchange_article(kind), " ", kind, " ", part, " key is ",
      expected, " bytes. The key text has been altered or cut short."
    )
  }
  raw
}

#' The one key line a key argument holds
#'
#' A key reaches EpiSODIC as its text or as the path of a file holding
#' it, through the same arguments and environment variables, because a
#' secret store hands an operator a value and a deployment hands them a
#' file, and either is a legitimate way to keep a key out of the
#' configuration. The two cannot be confused: a key text matches
#' `episodic_exchange_key_pattern`, which no plausible path does. A
#' file's lines starting with `#` are comments.
#'
#' @param x A key text, a path, or `NA`/`""` for none given.
#' @param what How the error names the key.
#' @param env The environment variable the caller defaults `x` to, named
#'   in the error when no key was given, or `NULL`.
#' @return A list with `text` (the key line) and `what` (the name to use
#'   in later errors, with the file's path where there is one).
#' @keywords internal
#' @noRd
episodic_exchange_key_line <- function(x, what, env = NULL) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    episodic_exchange_fail(
      "no_key",
      what, " was not given",
      if (!is.null(env)) paste0(", and ", env, " is not set") else "",
      ". Pass the key text or the path of its key file."
    )
  }
  if (grepl(episodic_exchange_key_pattern, trimws(x))) {
    return(list(text = trimws(x), what = what))
  }
  if (!file.exists(x)) {
    episodic_exchange_fail(
      "no_key",
      what, " is neither a key text nor an existing file (", x, ")."
    )
  }
  lines <- trimws(readLines(x, warn = FALSE, encoding = "UTF-8"))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) != 1L) {
    episodic_exchange_fail(
      "bad_key",
      what, " (", x, ") holds ", length(lines), " key lines, and a key ",
      "file holds exactly one."
    )
  }
  list(text = lines, what = paste0(what, " (", x, ")"))
}

#' Read a key given as its text or as the path of a file holding it
#'
#' @param x A key text, a path, or `NA`/`""` for none given.
#' @param kind,part,what As for `episodic_exchange_key_parse()`.
#' @param env As for `episodic_exchange_key_line()`.
#' @return The raw key.
#' @keywords internal
#' @noRd
episodic_exchange_key_read <- function(x, kind, part, what, env = NULL) {
  line <- episodic_exchange_key_line(x, what, env = env)
  episodic_exchange_key_parse(line$text, kind, part, line$what)
}

#' The fingerprint of a public key
#'
#' SHA-256 of the raw key, in hexadecimal: what the audit trail records
#' for every recipient and sender, and what two partners read to each
#' other over the telephone to be sure the key file they were sent is the
#' one the other party generated.
#' @param raw A raw public key.
#' @return A 64-character hexadecimal string.
#' @keywords internal
#' @noRd
episodic_exchange_fingerprint <- function(raw) {
  sodium::bin2hex(sodium::sha256(raw))
}

#' A fingerprint shortened for display
#'
#' The first sixteen hexadecimal characters in groups of four: enough to
#' tell keys apart on a screen and to read aloud, with the full
#' fingerprint kept in the database.
#' @param fingerprint A fingerprint, or `NA`.
#' @return A single string, or `NA`.
#' @keywords internal
#' @noRd
episodic_exchange_fingerprint_short <- function(fingerprint) {
  if (length(fingerprint) != 1L || is.na(fingerprint) || nchar(fingerprint) < 16L) {
    return(NA_character_)
  }
  head <- substr(fingerprint, 1L, 16L)
  paste(substring(head, seq(1L, 13L, 4L), seq(4L, 16L, 4L)), collapse = " ")
}

#' Fingerprint of an Exchange Public Key
#'
#' The SHA-256 of a public key, as the audit trail records it. Read it to
#' your partner over a channel other than the one you sent the key file
#' through (a telephone call does): when both of you see the same
#' fingerprint, the file was not swapped on the way.
#'
#' @param key A public key: its text, or the path of the `.pub` file
#'   [episodic_exchange_keygen()] wrote. An encryption key and a signing
#'   key are both accepted; a secret key is refused.
#' @return A 64-character hexadecimal string.
#' @seealso `vignette("exchange")`
#' @examples
#' dir <- tempfile()
#' keys <- episodic_exchange_keygen(dir)
#' episodic_exchange_key_fingerprint(keys$signing_public)
#' unlink(dir, recursive = TRUE)
#' @export
episodic_exchange_key_fingerprint <- function(key) {
  line <- episodic_exchange_key_line(key, "The key")
  kind <- regmatches(line$text, regexec(episodic_exchange_key_pattern, line$text))[[1]][2]
  episodic_exchange_fingerprint(
    episodic_exchange_key_parse(line$text, kind, "public", line$what)
  )
}

#' Write a secret key file only its owner can read
#'
#' The file is created under a `0077` umask and then set to `0600`
#' explicitly, so there is no moment at which it exists readable by
#' anyone else; on a system that keeps POSIX permissions the result is
#' checked, and a key file left readable by others is removed and the
#' call fails. Windows keeps no such bits, which `vignette("exchange")`
#' says: there, the directory's own access control is what protects it.
#' @param path The file to write.
#' @param lines Its lines.
#' @return Invisibly, `path`.
#' @keywords internal
#' @noRd
episodic_exchange_write_secret <- function(path, lines) {
  old <- Sys.umask("0077")
  on.exit(Sys.umask(old), add = TRUE)
  writeLines(lines, path, useBytes = TRUE)
  Sys.chmod(path, mode = "0600", use_umask = FALSE)
  if (.Platform$OS.type == "unix") {
    mode <- file.info(path)$mode
    if (is.na(mode) || (as.integer(mode) %% 64L) != 0L) {
      unlink(path)
      episodic_exchange_fail(
        "key_permissions",
        "Could not make ", path, " readable by its owner only (it came out ",
        "as ", format(mode), "), so it was removed again. Generate the ",
        "keys in a directory on a file system that keeps permissions."
      )
    }
  }
  invisible(path)
}

#' Generate the Keys for Exchanging Data with Partners
#'
#' Run this once per instance. It writes two key pairs into `path`:
#'
#' \describe{
#'   \item{An encryption key pair}{`episodic_exchange_encryption.key`
#'     (secret) and `episodic_exchange_encryption.pub` (public). A partner
#'     encrypts the files meant for you to the public key; only the
#'     secret key opens them. Point `EPISODIC_EXCHANGE_KEY` at the secret
#'     key file.}
#'   \item{A signing key pair}{`episodic_exchange_signing.key` (secret)
#'     and `episodic_exchange_signing.pub` (public). Every file you
#'     export is signed with the secret key; a partner pins your public
#'     key and refuses any file it does not verify. Point
#'     `EPISODIC_EXCHANGE_SIGNING_KEY` at the secret key file.}
#' }
#'
#' Give your partners the two `.pub` files and your instance identifier
#' ([episodic_instance_id()]), and never the `.key` files. The secret key
#' files are written readable by their owner only; on Windows, which
#' keeps no such permission bits, keep them in a directory only the
#' account running EpiSODIC can read.
#'
#' Keys never belong in the configuration file: they would enter a stored
#' configuration export, and whoever reads that could read or forge
#' every file.
#'
#' @param path A directory to write the four files into; created if it
#'   does not exist.
#' @param overwrite Whether to replace key files already there. `FALSE`
#'   by default: a replaced key pair cannot open the files partners have
#'   already encrypted to the old one, and the partners have pinned the
#'   old signing key.
#' @return Invisibly, a list with the four file paths
#'   (`encryption_secret`, `encryption_public`, `signing_secret`,
#'   `signing_public`) and the two public keys' fingerprints
#'   (`encryption_fingerprint`, `signing_fingerprint`).
#' @seealso [episodic_exchange_export()], [episodic_exchange_import()],
#'   [episodic_exchange_key_fingerprint()], `vignette("exchange")`
#' @examples
#' dir <- tempfile()
#' keys <- episodic_exchange_keygen(dir)
#' basename(unlist(keys[1:4]))
#' unlink(dir, recursive = TRUE)
#' @export
episodic_exchange_keygen <- function(path, overwrite = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single directory path.", call. = FALSE)
  }
  files <- list(
    encryption_secret = file.path(path, "episodic_exchange_encryption.key"),
    encryption_public = file.path(path, "episodic_exchange_encryption.pub"),
    signing_secret = file.path(path, "episodic_exchange_signing.key"),
    signing_public = file.path(path, "episodic_exchange_signing.pub")
  )
  present <- unlist(files)[file.exists(unlist(files))]
  if (length(present) > 0 && !isTRUE(overwrite)) {
    stop(
      "Key files already exist: ", paste(present, collapse = ", "), ". A new ",
      "key pair cannot open files encrypted to the old one, and partners ",
      "have pinned the old signing key; pass overwrite = TRUE only if you ",
      "mean to replace them and will send partners the new public keys.",
      call. = FALSE
    )
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE, mode = "0700")
  if (!dir.exists(path)) {
    stop("Could not create the directory ", path, ".", call. = FALSE)
  }

  encryption_secret <- sodium::keygen()
  encryption_public <- sodium::pubkey(encryption_secret)
  signing_secret <- sodium::sig_keygen()
  signing_public <- sodium::sig_pubkey(signing_secret)

  episodic_exchange_write_secret(files$encryption_secret, c(
    "# EpiSODIC exchange SECRET encryption key. Never share this file.",
    "# Point EPISODIC_EXCHANGE_KEY at it to open files sent to you.",
    episodic_exchange_key_text("encryption", "secret", encryption_secret)
  ))
  episodic_exchange_write_secret(files$signing_secret, c(
    "# EpiSODIC exchange SECRET signing key. Never share this file.",
    "# Point EPISODIC_EXCHANGE_SIGNING_KEY at it to sign the files you export.",
    episodic_exchange_key_text("signing", "secret", signing_secret)
  ))
  writeLines(c(
    "# EpiSODIC exchange public encryption key. Give this file to partners",
    "# who send you data; they encrypt their files to it.",
    episodic_exchange_key_text("encryption", "public", encryption_public)
  ), files$encryption_public, useBytes = TRUE)
  writeLines(c(
    "# EpiSODIC exchange public signing key. Give this file to partners",
    "# who receive your data; they pin it to verify your files.",
    episodic_exchange_key_text("signing", "public", signing_public)
  ), files$signing_public, useBytes = TRUE)

  invisible(c(
    files,
    list(
      encryption_fingerprint = episodic_exchange_fingerprint(encryption_public),
      signing_fingerprint = episodic_exchange_fingerprint(signing_public)
    )
  ))
}

#' A two-byte unsigned big-endian integer
#' @keywords internal
#' @noRd
episodic_exchange_u16 <- function(n) {
  as.raw(c(n %/% 256L, n %% 256L))
}

#' @keywords internal
#' @noRd
episodic_exchange_read_u16 <- function(bytes) {
  as.integer(bytes[1]) * 256L + as.integer(bytes[2])
}

#' Seal a payload into an exchange file
#'
#' @param body The payload bytes, already compressed.
#' @param recipients A list of raw recipient encryption public keys, for
#'   public-key mode, or `NULL`.
#' @param passphrase A passphrase, for passphrase mode, or `NULL`.
#'   Exactly one of `recipients` and `passphrase` is given.
#' @param signing_secret The sender's raw Ed25519 secret key.
#' @return The file's bytes.
#' @keywords internal
#' @noRd
episodic_exchange_seal <- function(body, recipients, passphrase, signing_secret) {
  sizes <- episodic_exchange_sizes
  if (is.null(recipients) == is.null(passphrase)) {
    stop(
      "An exchange file is sealed either to recipients' public keys or ",
      "with a passphrase, not both and not neither.",
      call. = FALSE
    )
  }
  nonce <- sodium::random(sizes$nonce)
  if (!is.null(recipients)) {
    if (length(recipients) < 1L || length(recipients) > sizes$max_recipients) {
      stop(
        "An exchange file has between 1 and ", sizes$max_recipients,
        " recipients, not ", length(recipients), ".",
        call. = FALSE
      )
    }
    content_key <- sodium::random(sizes$content_key)
    wrapped <- lapply(recipients, function(pub) sodium::simple_encrypt(content_key, pub))
    # The recipients' order would say which key a sender listed first;
    # shuffled, the plaintext says how many recipients there are and
    # nothing more. Shuffled with libsodium's random bytes rather than
    # sample(), which would move the session's own random number stream
    # and so change what a seeded analysis run after an export draws.
    draws <- matrix(as.numeric(sodium::random(4L * length(wrapped))), nrow = 4L)
    wrapped <- wrapped[order(colSums(draws * c(16777216, 65536, 256, 1)))]
    header <- c(
      episodic_exchange_magic,
      episodic_exchange_u16(episodic_exchange_format_version),
      as.raw(episodic_exchange_modes[["public_key"]]),
      episodic_exchange_u16(length(wrapped)),
      unlist(wrapped),
      nonce
    )
  } else {
    salt <- sodium::random(sizes$salt)
    content_key <- sodium::scrypt(
      charToRaw(enc2utf8(passphrase)),
      salt = salt,
      size = sizes$content_key
    )
    header <- c(
      episodic_exchange_magic,
      episodic_exchange_u16(episodic_exchange_format_version),
      as.raw(episodic_exchange_modes[["passphrase"]]),
      salt,
      nonce
    )
  }
  signing_public <- sodium::sig_pubkey(signing_secret)
  signature <- sodium::sig_sign(c(header, signing_public, body), signing_secret)
  inner <- c(signing_public, signature, body)
  ciphertext <- sodium::data_encrypt(inner, content_key, nonce = nonce)
  c(header, as.raw(ciphertext))
}

#' Open an exchange file, verifying everything before returning anything
#'
#' Returns the payload only once the file has been decrypted, its
#' authenticator has held over every byte, its sender is one of the
#' pinned keys and its signature verifies over the header and the
#' payload together. Any failure raises an `episodic_exchange_error`
#' naming what failed; there is no partial result.
#'
#' @param bytes The file's bytes.
#' @param secret_key The recipient's raw encryption secret key, for
#'   public-key mode, or `NULL`.
#' @param passphrase The passphrase, for passphrase mode, or `NULL`.
#' @param trusted_senders A list of raw signing public keys.
#' @return A list with `body` (the compressed payload), `mode`,
#'   `format_version`, `signer_public` (raw) and `signer_fingerprint`.
#' @keywords internal
#' @noRd
episodic_exchange_open <- function(bytes, secret_key, passphrase, trusted_senders) {
  sizes <- episodic_exchange_sizes
  magic_n <- length(episodic_exchange_magic)
  if (length(bytes) < magic_n || !identical(bytes[seq_len(magic_n)], episodic_exchange_magic)) {
    episodic_exchange_fail(
      "not_exchange_file",
      "This is not an EpiSODIC exchange file: it does not start with the ",
      "format's magic bytes."
    )
  }
  if (length(bytes) < magic_n + 3L) {
    episodic_exchange_fail("truncated", "This exchange file is truncated: its header is incomplete.")
  }
  version <- episodic_exchange_read_u16(bytes[magic_n + 1:2])
  if (version > episodic_exchange_format_version) {
    episodic_exchange_fail(
      "unsupported_version",
      "This exchange file is in format version ", version, ", written by a ",
      "newer EpiSODIC; this one reads format version ",
      episodic_exchange_format_version, " and older. Upgrade EpiSODIC to ",
      "import it."
    )
  }
  if (version < 1L) {
    episodic_exchange_fail(
      "unsupported_version",
      "This exchange file claims format version ", version, ", which no ",
      "EpiSODIC has written."
    )
  }
  mode_code <- as.integer(bytes[magic_n + 3L])
  mode <- names(episodic_exchange_modes)[match(mode_code, episodic_exchange_modes)]
  if (is.na(mode)) {
    episodic_exchange_fail(
      "bad_header",
      "This exchange file names an encryption mode (", mode_code, ") ",
      "format version ", version, " does not have."
    )
  }
  at <- magic_n + 4L
  need <- function(n) {
    if (length(bytes) < at + n - 1L) {
      episodic_exchange_fail("truncated", "This exchange file is truncated: its header is incomplete.")
    }
  }

  if (identical(mode, "public_key")) {
    if (is.null(secret_key)) {
      episodic_exchange_fail(
        "wrong_mode",
        "This exchange file is encrypted to recipients' public keys, so it ",
        "is opened with this instance's secret encryption key, not a ",
        "passphrase."
      )
    }
    need(2L)
    n <- episodic_exchange_read_u16(bytes[at + 0:1])
    at <- at + 2L
    if (n < 1L) {
      episodic_exchange_fail("bad_header", "This exchange file lists no recipients.")
    }
    need(n * sizes$wrapped_key + sizes$nonce)
    content_key <- NULL
    for (i in seq_len(n)) {
      wrapped <- bytes[at + (i - 1L) * sizes$wrapped_key + seq_len(sizes$wrapped_key) - 1L]
      opened <- tryCatch(sodium::simple_decrypt(wrapped, secret_key), error = function(e) NULL)
      if (!is.null(opened) && length(opened) == sizes$content_key) {
        content_key <- opened
        break
      }
    }
    at <- at + n * sizes$wrapped_key
    if (is.null(content_key)) {
      episodic_exchange_fail(
        "wrong_key",
        "This exchange file is not addressed to this instance's key: none ",
        "of its ", n, " recipient key(s) opens with it. Ask the sender ",
        "to encrypt to the public key whose fingerprint is ",
        episodic_exchange_fingerprint_short(
          episodic_exchange_fingerprint(sodium::pubkey(secret_key))
        ),
        "."
      )
    }
  } else {
    if (is.null(passphrase)) {
      episodic_exchange_fail(
        "wrong_mode",
        "This exchange file is encrypted with a passphrase, so it is opened ",
        "with that passphrase, not with a secret key."
      )
    }
    need(sizes$salt + sizes$nonce)
    salt <- bytes[at + seq_len(sizes$salt) - 1L]
    at <- at + sizes$salt
    content_key <- sodium::scrypt(
      charToRaw(enc2utf8(passphrase)),
      salt = salt,
      size = sizes$content_key
    )
  }

  nonce <- bytes[at + seq_len(sizes$nonce) - 1L]
  at <- at + sizes$nonce
  header <- bytes[seq_len(at - 1L)]
  ciphertext <- if (length(bytes) >= at) bytes[at:length(bytes)] else raw(0)
  if (length(ciphertext) < sizes$mac + sizes$signing_public + sizes$signature) {
    episodic_exchange_fail("truncated", "This exchange file is truncated: its encrypted content is incomplete.")
  }
  inner <- tryCatch(
    sodium::data_decrypt(ciphertext, content_key, nonce = nonce),
    error = function(e) NULL
  )
  if (is.null(inner)) {
    if (identical(mode, "public_key")) {
      episodic_exchange_fail(
        "tampered",
        "This exchange file has been altered or truncated: its encrypted ",
        "content fails authentication. Nothing in it was read."
      )
    }
    episodic_exchange_fail(
      "wrong_passphrase_or_tampered",
      "This exchange file does not open with this passphrase. Either the ",
      "passphrase is wrong, or the file has been altered or truncated; the ",
      "format cannot tell the two apart. Nothing in it was read."
    )
  }
  if (length(inner) < sizes$signing_public + sizes$signature) {
    episodic_exchange_fail("tampered", "This exchange file's signed content is incomplete.")
  }
  signer_public <- inner[seq_len(sizes$signing_public)]
  signature <- inner[sizes$signing_public + seq_len(sizes$signature)]
  body <- if (length(inner) > sizes$signing_public + sizes$signature) {
    inner[(sizes$signing_public + sizes$signature + 1L):length(inner)]
  } else {
    raw(0)
  }
  signer_fingerprint <- episodic_exchange_fingerprint(signer_public)
  trusted <- any(vapply(trusted_senders, identical, logical(1), signer_public))
  if (!trusted) {
    episodic_exchange_fail(
      "untrusted_sender",
      "This exchange file is signed by a key that is not among the trusted ",
      "senders (fingerprint ",
      episodic_exchange_fingerprint_short(signer_fingerprint),
      "). Import only files from partners whose public signing key you ",
      "have pinned; if this is a partner's new key, verify its fingerprint ",
      "with them before adding it."
    )
  }
  verified <- tryCatch(
    sodium::sig_verify(c(header, signer_public, body), signature, signer_public),
    error = function(e) FALSE
  )
  if (!isTRUE(verified)) {
    episodic_exchange_fail(
      "bad_signature",
      "This exchange file's signature does not verify: it was not signed ",
      "by the trusted sender it names, or it was altered after signing. ",
      "Nothing in it was read."
    )
  }
  list(
    body = body,
    mode = mode,
    format_version = version,
    signer_public = signer_public,
    signer_fingerprint = signer_fingerprint
  )
}
