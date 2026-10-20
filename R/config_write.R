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

# The operator's configuration file (`EPISODIC_CONFIG`) is the only
# record of an instance's settings. When the dashboard changes one, it
# writes that file, replacing the one key it changes and leaving every
# other line - comments, ordering, blank lines, quoting - exactly as the
# operator wrote it. A round trip through `yaml::write_yaml()` would
# rewrite the whole file and drop every comment in it.
#
# The edit is made on the text and then proven on the parse: the edited
# text must read back as the original configuration with exactly that one
# key changed, and must pass `episodic_config_validate()`, or nothing is
# written. A file whose layout the text edit cannot follow (flow
# mappings, anchors, several documents) therefore fails loudly instead
# of being rewritten into something the operator did not write.

#' Set one key in the operator's configuration file
#'
#' @param key_path A character vector naming the key, from the top level
#'   down, such as `c("plugins", "enabled")`.
#' @param value The new value: a scalar, a list, or `NULL` for YAML's `~`.
#' @param episodic_config_path The configuration file. Unset, there is no
#'   file to write and this stops.
#' @return Invisibly, the path written.
#' @keywords internal
#' @noRd
episodic_config_set <- function(key_path,
                                value,
                                episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)) {
  if (!is.character(key_path) || length(key_path) == 0 || anyNA(key_path) || !all(nzchar(key_path))) {
    stop("`key_path` must name a configuration key, such as c(\"plugins\", \"enabled\").", call. = FALSE)
  }
  if (is.na(episodic_config_path) || !nzchar(episodic_config_path)) {
    stop(
      "There is no configuration file to write this setting to: EPISODIC_CONFIG ",
      "is not set. Point it at the instance's configuration file (an empty ",
      "file will do) and try again.",
      call. = FALSE
    )
  }
  if (!file.exists(episodic_config_path)) {
    stop(
      "EPISODIC_CONFIG points at '", episodic_config_path, "', but no file ",
      "exists there, so this setting cannot be written to it.",
      call. = FALSE
    )
  }
  if (file.access(episodic_config_path, mode = 2) != 0) {
    stop(
      "The configuration file '", episodic_config_path, "' is not writable by ",
      "the account this process runs as, so this setting cannot be saved.",
      call. = FALSE
    )
  }
  episodic_config_check_value(value, key_path)

  lock <- episodic_config_lock(episodic_config_path)
  on.exit(episodic_config_unlock(lock), add = TRUE)

  raw <- readBin(episodic_config_path, "raw", file.info(episodic_config_path)$size)
  text <- rawToChar(raw)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    stop("The configuration file '", episodic_config_path, "' is not valid UTF-8.", call. = FALSE)
  }
  crlf <- grepl("\r\n", text, fixed = TRUE)
  final_newline <- !nzchar(text) || grepl("\n$", text)
  lines <- strsplit(gsub("\r\n", "\n", text, fixed = TRUE), "\n", fixed = TRUE)[[1]]

  original <- episodic_config_parse_text(lines, episodic_config_path)
  edited <- episodic_config_set_lines(lines, key_path, value, original)
  parsed <- tryCatch(
    episodic_config_parse_text(edited, episodic_config_path),
    warning = function(w) NULL
  )

  expected <- episodic_config_set_value(original, key_path, value)
  if (is.null(parsed) || !episodic_config_same(parsed, expected)) {
    stop(
      "The configuration file '", episodic_config_path, "' is laid out in a ",
      "way this edit cannot follow safely (for example a flow mapping in ",
      "braces, an anchor, or several documents), so `",
      paste(key_path, collapse = "."), "` was not written. Set it in the ",
      "file by hand.",
      call. = FALSE
    )
  }
  episodic_config_validate(parsed, episodic_config_defaults(), episodic_config_path)

  out <- paste(edited, collapse = "\n")
  if (final_newline) out <- paste0(out, "\n")
  if (crlf) out <- gsub("\n", "\r\n", out, fixed = TRUE)
  episodic_config_write_atomic(episodic_config_path, enc2utf8(out))
  invisible(episodic_config_path)
}

#' The shipped default configuration, as read from its file
#' @keywords internal
#' @noRd
episodic_config_defaults <- function() {
  path <- system.file("config", "episodic_default_config.yaml", package = "EpiSODIC")
  if (identical(path, "")) {
    path <- file.path("inst", "config", "episodic_default_config.yaml")
  }
  yaml::read_yaml(path)
}

#' Refuse a value YAML cannot carry back unchanged
#' @keywords internal
#' @noRd
episodic_config_check_value <- function(value, key_path) {
  walk <- function(x) {
    if (is.null(x)) {
      return(invisible(TRUE))
    }
    if (is.list(x)) {
      lapply(x, walk)
      return(invisible(TRUE))
    }
    if (!is.atomic(x) || !(is.character(x) || is.numeric(x) || is.logical(x))) {
      stop("`", paste(key_path, collapse = "."), "` can only be set to text, numbers, true/false, lists or ~.", call. = FALSE)
    }
    if (anyNA(x)) {
      stop("`", paste(key_path, collapse = "."), "` cannot be set to a missing value; use NULL for ~.", call. = FALSE)
    }
    if (is.numeric(x) && any(!is.finite(x))) {
      stop("`", paste(key_path, collapse = "."), "` cannot be set to an infinite number.", call. = FALSE)
    }
    invisible(TRUE)
  }
  walk(value)
}

#' Parse configuration text as `episodic_config_resolve_files()` reads it
#' @keywords internal
#' @noRd
episodic_config_parse_text <- function(lines, path) {
  parsed <- tryCatch(
    yaml::yaml.load(paste(lines, collapse = "\n")),
    error = function(e) e
  )
  if (inherits(parsed, "condition")) {
    stop("The configuration file '", path, "' could not be read as YAML: ", conditionMessage(parsed), call. = FALSE)
  }
  if (is.null(parsed)) {
    parsed <- list()
  }
  if (!is.list(parsed) || (length(parsed) > 0 && is.null(names(parsed)))) {
    stop("The configuration file '", path, "' does not hold a mapping of settings at its top level.", call. = FALSE)
  }
  parsed
}

#' A configuration list with one key set, the way YAML would read it back
#' @keywords internal
#' @noRd
episodic_config_set_value <- function(config, key_path, value) {
  key <- key_path[1]
  if (length(key_path) == 1) {
    config[key] <- list(value)
    return(config)
  }
  sub <- config[[key]]
  if (!is.list(sub) || (length(sub) > 0 && is.null(names(sub)))) {
    sub <- list()
  }
  config[key] <- list(episodic_config_set_value(sub, key_path[-1], value))
  config
}

#' Whether two parsed configurations say the same thing
#'
#' Numbers compare by value, so `5` and `5.0` agree; names and order
#' compare exactly.
#' @keywords internal
#' @noRd
episodic_config_same <- function(a, b) {
  if (is.null(a) || is.null(b)) {
    return(is.null(a) && is.null(b))
  }
  # YAML reads a sequence of scalars back as a vector, so an unnamed list
  # and a vector of the same items say the same thing.
  if (is.list(a) && is.null(names(a)) && is.atomic(b)) b <- as.list(b)
  if (is.list(b) && is.null(names(b)) && is.atomic(a)) a <- as.list(a)
  if (is.list(a) || is.list(b)) {
    if (!is.list(a) || !is.list(b) || length(a) != length(b) || !identical(names(a), names(b))) {
      return(FALSE)
    }
    return(all(vapply(seq_along(a), function(i) episodic_config_same(a[[i]], b[[i]]), logical(1))))
  }
  if (is.numeric(a) && is.numeric(b)) {
    return(length(a) == length(b) && all(a == b))
  }
  identical(a, b)
}

#' Emit a key and its value as YAML lines, indented
#' @keywords internal
#' @noRd
episodic_config_emit <- function(key, value, indent) {
  handlers <- list(
    logical = function(x) {
      out <- ifelse(x, "true", "false")
      class(out) <- "verbatim"
      out
    },
    numeric = function(x) {
      out <- ifelse(x == round(x) & abs(x) < 1e15, format(x, scientific = FALSE, trim = TRUE), as.character(x))
      class(out) <- "verbatim"
      out
    }
  )
  item <- list(value)
  names(item) <- key
  text <- yaml::as.yaml(item, handlers = handlers, indent.mapping.sequence = TRUE, unicode = TRUE)
  out <- strsplit(sub("\n$", "", text), "\n", fixed = TRUE)[[1]]
  paste0(strrep(" ", indent), out)
}

#' The indentation of a line, and whether it carries content
#' @keywords internal
#' @noRd
episodic_config_line_indent <- function(line) {
  nchar(line) - nchar(sub("^ +", "", line))
}

episodic_config_line_is_content <- function(line) {
  grepl("\\S", line) && !grepl("^\\s*#", line)
}

#' The line that holds `key` at `indent` within lines `from` to `to`
#' @keywords internal
#' @noRd
episodic_config_find_key <- function(lines, key, indent, from, to) {
  if (from > to) {
    return(NULL)
  }
  pattern <- paste0(
    "^ {", indent, "}(", episodic_config_regex_escape(key), "|\"",
    episodic_config_regex_escape(key), "\"|'", episodic_config_regex_escape(key), "')\\s*:(\\s|$)"
  )
  for (i in from:to) {
    if (episodic_config_line_is_content(lines[i]) &&
      episodic_config_line_indent(lines[i]) == indent &&
      grepl(pattern, lines[i], perl = TRUE)) {
      return(i)
    }
  }
  NULL
}

episodic_config_regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.-])", "\\\\\\1", x)
}

#' The last line of the value that starts on line `i`
#'
#' A value runs on while lines are indented deeper than its key, or are
#' sequence items at the key's own indentation. Comment and blank lines
#' after its last content line are left outside it: they introduce the
#' next key.
#' @keywords internal
#' @noRd
episodic_config_block_end <- function(lines, i, indent, to) {
  last <- i
  j <- i + 1
  while (j <= to) {
    line <- lines[j]
    if (episodic_config_line_is_content(line)) {
      ind <- episodic_config_line_indent(line)
      if (ind > indent || (ind == indent && grepl("^ *-( |$)", line))) {
        last <- j
      } else {
        break
      }
    }
    j <- j + 1
  }
  last
}

#' The text of `lines` with one key set
#'
#' Descends through block mappings line by line. Where the path leaves
#' block layout - a parent written inline, or empty - the highest such
#' key is replaced whole, with its value taken from the parse, so nothing
#' the operator wrote is lost except what the edit replaces.
#' @keywords internal
#' @noRd
episodic_config_set_lines <- function(lines, key_path, value, parsed, from = 1L, to = length(lines), indent = 0L) {
  key <- key_path[1]
  i <- episodic_config_find_key(lines, key, indent, from, to)

  if (is.null(i)) {
    nested <- if (length(key_path) == 1) value else episodic_config_set_value(list(), key_path[-1], value)
    new <- episodic_config_emit(key, nested, indent)
    # After the last content line of the enclosing block, so trailing
    # comments that introduce whatever follows keep their place.
    content <- which(vapply(lines[seq_len(to)], episodic_config_line_is_content, logical(1)))
    content <- content[content >= from]
    at <- if (length(content) == 0) from - 1L else max(content)
    if (indent == 0 && at > 0) {
      new <- c("", new)
    }
    return(append(lines, new, after = at))
  }

  end <- episodic_config_block_end(lines, i, indent, to)
  inline <- sub("^[^:]*:\\s*", "", lines[i])
  is_block <- !nzchar(sub("\\s*#.*$", "", inline)) && end > i

  if (length(key_path) == 1 || !is_block) {
    current <- if (is.list(parsed)) parsed[[key]] else NULL
    nested <- if (length(key_path) == 1) value else episodic_config_set_value(
      if (is.list(current) && (length(current) == 0 || !is.null(names(current)))) current else list(),
      key_path[-1], value
    )
    new <- episodic_config_emit(key, nested, indent)
    # Keep an end-of-line comment on a scalar written without quotes.
    comment <- regmatches(lines[i], regexec("^[^'\"#]*?(\\s+#.*)$", lines[i]))[[1]]
    if (length(new) == 1 && length(comment) == 2 && end == i) {
      new <- paste0(new, comment[2])
    }
    return(c(lines[seq_len(i - 1)], new, lines[seq_along(lines) > end]))
  }

  children <- which(vapply(lines[(i + 1):end], episodic_config_line_is_content, logical(1))) + i
  child_indent <- episodic_config_line_indent(lines[children[1]])
  if (child_indent <= indent || grepl("^ *-( |$)", lines[children[1]])) {
    stop(
      "`", key, "` in the configuration file holds a list, not a mapping, so `",
      paste(key_path, collapse = "."), "` cannot be set inside it.",
      call. = FALSE
    )
  }
  episodic_config_set_lines(
    lines, key_path[-1], value, parsed[[key]],
    from = i + 1L, to = end, indent = child_indent
  )
}

#' Take the configuration file's write lock
#'
#' A directory beside the file, since `dir.create()` either creates it or
#' fails, atomically, on every platform. Two dashboard processes saving
#' at once would otherwise each read the file before the other wrote it,
#' and the second write would undo the first.
#' @keywords internal
#' @noRd
episodic_config_lock <- function(path, wait_seconds = 10, stale_seconds = 120) {
  lock <- paste0(path, ".lock")
  deadline <- Sys.time() + wait_seconds
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) {
      return(lock)
    }
    age <- as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "secs"))
    if (!is.na(age) && age > stale_seconds) {
      unlink(lock, recursive = TRUE)
      next
    }
    if (Sys.time() > deadline) {
      stop(
        "The configuration file '", path, "' is being written by another ",
        "process (its lock '", lock, "' is held), so this setting was not ",
        "saved. Try again in a moment.",
        call. = FALSE
      )
    }
    Sys.sleep(0.1)
  }
}

episodic_config_unlock <- function(lock) {
  unlink(lock, recursive = TRUE)
}

#' Replace a file's content in one step
#'
#' Written beside it and renamed over it, with the original's permission
#' bits, so a reader never sees half a file and a configuration holding
#' secrets does not become readable by more accounts than it was.
#' @keywords internal
#' @noRd
episodic_config_write_atomic <- function(path, text) {
  tmp <- tempfile(paste0(".", basename(path), "."), tmpdir = dirname(path))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  con <- file(tmp, open = "wb")
  writeBin(charToRaw(text), con)
  close(con)
  mode <- file.info(path)$mode
  if (!is.na(mode)) {
    Sys.chmod(tmp, mode, use_umask = FALSE)
  }
  if (!file.rename(tmp, path)) {
    stop("The configuration file '", path, "' could not be replaced with its edited version.", call. = FALSE)
  }
  invisible(path)
}
