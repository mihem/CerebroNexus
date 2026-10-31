if (!exists(".builderRandomBytes", mode = "function", inherits = TRUE)) {
  .builderRandomBytes <- function(n) {
    getFromNamespace(".builderRandomBytes", "CerebroNexus")(n)
  }
}

##----------------------------------------------------------------------------##
## Parent-owned, recoverable publication of one Builder release.
##
## A coordinator holds one owner-identified directory lock from stage
## registration through publication. Every destructive transition is preceded
## or followed by an atomic journal record so a later parent can recover after
## process death without guessing ownership from elapsed time.
##----------------------------------------------------------------------------##

.builder_release_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_release_exists <- function(path) {
  file.exists(path) || dir.exists(path) || .builder_release_link(path)
}

.builder_release_link <- function(path) {
  linked <- tryCatch(fs::is_link(path), error = function(error) NA)
  length(linked) == 1L && (is.na(linked) || isTRUE(unname(linked)))
}

.builder_release_io_path <- function(
  path,
  os_type = .Platform$OS.type
) {
  if (!identical(os_type, "windows")) {
    return(path)
  }
  slash_path <- gsub("\\", "/", path, fixed = TRUE)
  if (grepl("^//[?.]/", slash_path)) {
    return(gsub("/", "\\", slash_path, fixed = TRUE))
  }
  if (grepl("^[A-Za-z]:/", slash_path)) {
    return(paste0("\\\\?\\", gsub("/", "\\", slash_path, fixed = TRUE)))
  }
  if (grepl("^//[^/]+/[^/]+($|/)", slash_path)) {
    return(paste0(
      "\\\\?\\UNC\\",
      gsub("/", "\\", substring(slash_path, 3L), fixed = TRUE)
    ))
  }
  path
}

.builder_release_remove_stage <- function(
  stage,
  .unlink = unlink,
  os_type = .Platform$OS.type
) {
  exists <- function(path) file.exists(path) || dir.exists(path)
  if (!exists(stage)) {
    return(invisible(TRUE))
  }
  suppressWarnings(.unlink(stage, recursive = TRUE, force = TRUE))
  if (!exists(stage)) {
    return(invisible(TRUE))
  }
  if (identical(os_type, "windows")) {
    remove_entry <- function(path) {
      io_path <- .builder_release_io_path(path, os_type = os_type)
      linked <- tryCatch(
        fs::is_link(path),
        error = function(error) NA
      )
      if (isTRUE(unname(linked)) || !dir.exists(path)) {
        suppressWarnings(.unlink(io_path, recursive = FALSE, force = TRUE))
        return(invisible(NULL))
      }
      entries <- list.files(
        path,
        all.files = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )
      for (entry in entries) {
        remove_entry(entry)
      }
      suppressWarnings(.unlink(io_path, recursive = FALSE, force = TRUE))
      if (dir.exists(path) && !identical(path, stage)) {
        # R 4.3 cannot always remove an empty near-MAX_PATH directory even
        # through the device prefix. Rename it to a short sibling while its
        # parent is still addressable, then remove that bounded path.
        isolated <- tempfile(pattern = ".delete-", tmpdir = dirname(path))
        renamed <- suppressWarnings(file.rename(path, isolated))
        if (isTRUE(renamed)) {
          suppressWarnings(.unlink(isolated, recursive = TRUE, force = TRUE))
        }
      }
      invisible(NULL)
    }
    remove_entry(stage)
    # Once extended-path leaves have gone, the bounded stage root is again
    # removable by R's ordinary recursive implementation.
    suppressWarnings(.unlink(stage, recursive = TRUE, force = TRUE))
  }
  removed <- !exists(stage)
  if (!removed) {
    stop(
      "The assigned build stage could not be removed safely; recovery is required.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_release_token <- function(prefix = "owner") {
  if (!.builder_release_text(prefix)) {
    stop("A release token prefix is required.", call. = FALSE)
  }
  random <- paste(format(.builderRandomBytes(8L)), collapse = "")
  paste0(prefix, "-", random)
}

.builder_release_token_max_length <- function(prefix = "owner") {
  if (!.builder_release_text(prefix)) {
    stop("A release token prefix is required.", call. = FALSE)
  }
  nchar(prefix, type = "chars") + 17L
}

.builder_release_host <- function() {
  host <- unname(Sys.info()[["nodename"]])
  if (!.builder_release_text(host)) "unknown-host" else host
}

.builder_release_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

.builder_release_mode_owner_only <- function(path) {
  if (identical(.Platform$OS.type, "windows")) {
    return(TRUE)
  }
  mode <- file.info(path)$mode
  length(mode) == 1L && !is.na(mode) && bitwAnd(as.integer(mode), 63L) == 0L
}

.builder_release_path <- function(target) {
  if (!.builder_release_text(target)) {
    stop("A release target is required.", call. = FALSE)
  }
  if (.builder_release_link(target)) {
    stop("The release target cannot be a symbolic link.", call. = FALSE)
  }
  canonical <- .canonicalTargetPath(target)
  leaf <- basename(canonical)
  if (
    !nzchar(leaf) ||
      leaf %in% c(".", "..", "/") ||
      .windowsPathSegmentInvalid(leaf)
  ) {
    stop("The release target has an unsafe name.", call. = FALSE)
  }
  parent <- dirname(canonical)
  if (!dir.exists(parent) || .builder_release_link(parent)) {
    stop(
      "The release parent must be an existing real directory.",
      call. = FALSE
    )
  }
  canonical
}

.builder_release_legacy_control_path <- function(target) {
  target <- .builder_release_path(target)
  .canonicalTargetPath(file.path(
    dirname(target),
    paste0(".", basename(target), ".cerebro-control")
  ))
}

builder_release_control_path <- function(target) {
  target <- .builder_release_path(target)
  identity <- target
  if (identical(.Platform$OS.type, "windows")) {
    identity <- tolower(identity)
  }
  digest <- paste(
    format(digest::digest(
      charToRaw(enc2utf8(identity)),
      algo = "md5",
      serialize = FALSE
    )),
    collapse = ""
  )
  .canonicalTargetPath(file.path(
    dirname(target),
    paste0(".cbr-", substr(digest, 1L, 16L))
  ))
}

.builder_release_control_candidates <- function(target) {
  unique(c(
    builder_release_control_path(target),
    .builder_release_legacy_control_path(target)
  ))
}

.builder_release_relative <- function(path, root) {
  root <- .canonicalTargetPath(root)
  vapply(
    path,
    function(candidate) {
      candidate <- .canonicalTargetPath(candidate)
      if (!.pathWithin(candidate, root, canonicalize = base::identity)) {
        stop("A release entry escaped its owned root.", call. = FALSE)
      }
      if (identical(candidate, root)) {
        ""
      } else {
        substring(candidate, nchar(root) + 2L)
      }
    },
    ""
  )
}

.builder_release_file_fingerprint <- function(info) {
  if (is.null(info) || nrow(info) != 1L) {
    stop("A release entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint <- list(
    type = as.character(info$type),
    size = as.double(info$size),
    permissions = as.character(info$permissions),
    device_id = as.double(info$device_id),
    inode = as.double(info$inode),
    hard_links = as.double(info$hard_links),
    modification_time = as.double(info$modification_time),
    change_time = as.double(info$change_time)
  )
  scalar <- vapply(fingerprint, length, integer(1)) == 1L
  missing <- vapply(fingerprint, function(value) anyNA(value), logical(1))
  numeric_fields <- c(
    "size",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  finite <- vapply(
    fingerprint[numeric_fields],
    function(value) is.finite(value),
    logical(1)
  )
  if (
    !all(scalar) ||
      any(missing) ||
      !all(finite) ||
      !nzchar(fingerprint$type) ||
      !nzchar(fingerprint$permissions) ||
      fingerprint$size < 0 ||
      fingerprint$size != floor(fingerprint$size) ||
      fingerprint$device_id < 0 ||
      fingerprint$inode < 0 ||
      fingerprint$hard_links < 1
  ) {
    stop("A release entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint
}

.builder_release_normalized_paths <- function(paths) {
  normalized <- tryCatch(
    as.character(fs::path_norm(fs::path_abs(paths))),
    error = function(error) NULL
  )
  if (
    is.null(normalized) ||
      length(normalized) != length(paths) ||
      anyNA(normalized) ||
      any(!nzchar(normalized))
  ) {
    stop("Release entries returned unsafe paths.", call. = FALSE)
  }
  normalized
}

.builder_release_file_info <- function(paths, message) {
  info <- tryCatch(
    fs::file_info(paths, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (is.null(info) || nrow(info) != length(paths)) {
    stop(message, call. = FALSE)
  }
  paths <- .builder_release_normalized_paths(paths)
  info_paths <- .builder_release_normalized_paths(info$path)
  if (
    anyDuplicated(paths) ||
      anyDuplicated(info_paths) ||
      !setequal(paths, info_paths)
  ) {
    stop(message, call. = FALSE)
  }
  info[match(paths, info_paths), , drop = FALSE]
}

.builder_release_list_directory <- function(path) {
  as.character(fs::dir_ls(
    path,
    all = TRUE,
    recurse = FALSE,
    type = "any",
    fail = TRUE
  ))
}

.builder_release_enumerate <- function(
  target,
  .list_directory = .builder_release_list_directory
) {
  root_path <- unname(.builder_release_normalized_paths(target))
  queue <- target
  entries <- character()
  entry_fingerprints <- list()
  snapshots <- list()
  while (length(queue)) {
    directory <- queue[[1L]]
    queue <- queue[-1L]
    before <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (is.null(before) || !identical(as.character(before$type), "directory")) {
      stop("A release directory changed before enumeration.", call. = FALSE)
    }
    listed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (is.null(listed)) {
      stop("A release directory could not be enumerated safely.", call. = FALSE)
    }
    after <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      is.null(after) ||
        !identical(
          .builder_release_file_fingerprint(before),
          .builder_release_file_fingerprint(after)
        )
    ) {
      stop("A release directory changed during enumeration.", call. = FALSE)
    }
    if (length(listed)) {
      parents <- unique(vapply(listed, dirname, ""))
      canonical_directory <- .canonicalTargetPath(directory)
      canonical_parents <- vapply(parents, .canonicalTargetPath, "")
      if (
        anyDuplicated(listed) ||
          !all(canonical_parents == canonical_directory)
      ) {
        stop("A release directory returned unsafe entries.", call. = FALSE)
      }
      listed_info <- .builder_release_file_info(
        listed,
        "Release entries changed during enumeration."
      )
      listed_fingerprints <- lapply(
        seq_len(nrow(listed_info)),
        function(index) {
          .builder_release_file_fingerprint(
            listed_info[index, , drop = FALSE]
          )
        }
      )
      directories <- as.character(listed_info$type) %in% "directory"
      queue <- c(queue, listed[directories])
      entries <- c(entries, listed)
      entry_fingerprints <- c(entry_fingerprints, listed_fingerprints)
    }
    snapshots[[directory]] <- listed
  }
  for (directory in names(snapshots)) {
    confirmed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (is.null(confirmed) || !identical(confirmed, snapshots[[directory]])) {
      stop("A release directory changed after enumeration.", call. = FALSE)
    }
  }
  relative <- character()
  if (length(entries)) {
    confirmed_info <- .builder_release_file_info(
      entries,
      "Release entries changed after enumeration."
    )
    entry_paths <- .builder_release_normalized_paths(entries)
    confirmed_fingerprints <- lapply(
      seq_len(nrow(confirmed_info)),
      function(index) {
        .builder_release_file_fingerprint(
          confirmed_info[index, , drop = FALSE]
        )
      }
    )
    stable <- mapply(
      identical,
      entry_fingerprints,
      confirmed_fingerprints,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    if (!all(stable)) {
      stop("Release entries changed after enumeration.", call. = FALSE)
    }
    contained <- vapply(
      entry_paths,
      .pathWithin,
      logical(1),
      parent = root_path,
      canonicalize = base::identity
    )
    if (!all(contained)) {
      stop("A release entry escaped its owned root.", call. = FALSE)
    }
    relative <- substring(entry_paths, nchar(root_path) + 2L)
  }
  list(
    paths = entries,
    fingerprints = entry_fingerprints,
    relative = relative
  )
}

.builder_release_digest_cache_key <- function(snapshot) {
  numeric <- c(
    "size",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  paste(
    snapshot$type,
    snapshot$permissions,
    vapply(snapshot[numeric], function(value) sprintf("%.17g", value), ""),
    collapse = "\034"
  )
}

.builder_release_digest_cache_collision_key <- function(key) {
  paste0("\035", key)
}

.builder_release_payload_md5_valid <- function(md5) {
  is.character(md5) &&
    length(md5) == 1L &&
    !is.na(md5) &&
    grepl("^[[:xdigit:]]{32}$", md5)
}

.builder_release_payload_md5 <- function(path) {
  md5 <- tryCatch(
    suppressWarnings(unname(tools::md5sum(path))),
    error = function(error) NA_character_
  )
  if (!.builder_release_payload_md5_valid(md5)) {
    stop("A release payload could not be read safely.", call. = FALSE)
  }
  md5
}

.builder_release_ignorable_metadata <- function(paths) {
  paths <- gsub("\\", "/", as.character(paths), fixed = TRUE)
  basename(paths) == ".DS_Store"
}

builder_release_identity <- function(
  target,
  .list_directory = .builder_release_list_directory,
  .hash_file = .builder_release_payload_md5,
  .digest_cache = NULL,
  .on_file = NULL
) {
  target <- .builder_release_path(target)
  if (!.builder_release_exists(target)) {
    return(list(schema_version = 1L, exists = FALSE, entries = list()))
  }
  if (!dir.exists(target) || .builder_release_link(target)) {
    stop("A Builder release must be a real directory.", call. = FALSE)
  }
  enumerated <- .builder_release_enumerate(target, .list_directory)
  entries <- enumerated$paths
  fingerprints <- enumerated$fingerprints
  if (!length(entries)) {
    return(list(schema_version = 1L, exists = TRUE, entries = list()))
  }
  relative <- enumerated$relative
  linked <- vapply(entries, .builder_release_link, logical(1))
  entry_types <- vapply(
    enumerated$fingerprints,
    `[[`,
    character(1),
    "type"
  )
  ignorable <- .builder_release_ignorable_metadata(relative) &
    !linked &
    !is.na(entry_types) &
    entry_types == "file"
  if (any(ignorable)) {
    entries <- entries[!ignorable]
    relative <- relative[!ignorable]
    linked <- linked[!ignorable]
    entry_types <- entry_types[!ignorable]
    fingerprints <- fingerprints[!ignorable]
  }
  if (!length(entries)) {
    return(list(schema_version = 1L, exists = TRUE, entries = list()))
  }
  if (any(linked)) {
    stop(
      "A release identity cannot include a symbolic link: ",
      basename(entries[[which(linked)[[1L]]]]),
      call. = FALSE
    )
  }
  unsupported <- is.na(entry_types) |
    !entry_types %in% c("file", "directory")
  if (any(unsupported)) {
    stop(
      "A release identity cannot include an unsupported filesystem entry: ",
      basename(entries[[which(unsupported)[[1L]]]]),
      call. = FALSE
    )
  }
  order_index <- order(relative, method = "radix")
  entries <- entries[order_index]
  relative <- relative[order_index]
  entry_types <- entry_types[order_index]
  fingerprints <- fingerprints[order_index]
  file_indexes <- which(entry_types == "file")
  file_paths <- entries[file_indexes]
  before <- fingerprints[file_indexes]
  cache_keys <- lapply(before, function(snapshot) {
    if (is.environment(.digest_cache)) {
      .builder_release_digest_cache_key(snapshot)
    } else {
      NULL
    }
  })
  # Progress only: `.on_file(index, total)` sees each file before its digest.
  report_file <- if (is.function(.on_file)) .on_file else function(...) NULL
  report_file(0L, length(file_paths))
  md5 <- vapply(
    seq_along(file_paths),
    function(index) {
      report_file(index, length(file_paths))
      cache_key <- cache_keys[[index]]
      collision_key <- if (is.null(cache_key)) {
        NULL
      } else {
        .builder_release_digest_cache_collision_key(cache_key)
      }
      value <- if (
        !is.null(cache_key) &&
          !exists(collision_key, envir = .digest_cache, inherits = FALSE) &&
          exists(cache_key, envir = .digest_cache, inherits = FALSE)
      ) {
        get(cache_key, envir = .digest_cache, inherits = FALSE)
      } else {
        tryCatch(.hash_file(file_paths[[index]]), error = function(error) {
          NA_character_
        })
      }
      if (!.builder_release_payload_md5_valid(value)) {
        stop("A release payload could not be read safely.", call. = FALSE)
      }
      value
    },
    ""
  )
  after <- if (length(file_paths)) {
    after_info <- .builder_release_file_info(
      file_paths,
      "Release payloads changed while their identity was read."
    )
    lapply(seq_along(file_paths), function(index) {
      .builder_release_file_fingerprint(
        after_info[index, , drop = FALSE]
      )
    })
  } else {
    list()
  }
  if (!all(mapply(identical, before, after, USE.NAMES = FALSE))) {
    stop("The release changed while its identity was read.", call. = FALSE)
  }
  if (is.environment(.digest_cache)) {
    for (index in seq_along(md5)) {
      collision_key <- .builder_release_digest_cache_collision_key(
        cache_keys[[index]]
      )
      if (!exists(collision_key, envir = .digest_cache, inherits = FALSE)) {
        assign(cache_keys[[index]], md5[[index]], envir = .digest_cache)
      }
    }
  }
  records <- lapply(seq_along(entries), function(index) {
    path <- entries[[index]]
    directory <- identical(entry_types[[index]], "directory")
    if (directory) {
      return(list(
        path = gsub("\\", "/", relative[[index]], fixed = TRUE),
        type = "directory",
        size = 0,
        md5 = NA_character_
      ))
    }
    file_index <- match(index, file_indexes)
    list(
      path = gsub("\\", "/", relative[[index]], fixed = TRUE),
      type = "file",
      size = before[[file_index]]$size,
      md5 = md5[[file_index]]
    )
  })
  list(schema_version = 1L, exists = TRUE, entries = records)
}

.builder_release_record_name <- ".cerebro-builder-release-v1"
.builder_release_record_header <- "CEREBRO_BUILDER_RELEASE_V1"

.builder_release_identity_members <- function(
  identity,
  include_record = FALSE
) {
  entries <- identity$entries
  if (!include_record && length(entries)) {
    paths <- vapply(entries, `[[`, character(1), "path")
    entries <- entries[!paths %in% .builder_release_record_name]
  }
  lapply(entries, function(entry) {
    list(
      type = if (identical(entry$type, "directory")) "D" else "F",
      path = entry$path
    )
  })
}

.builder_release_record_error <- function(message) {
  stop("The release ownership record ", message, call. = FALSE)
}

.builder_release_utf8 <- function(value) {
  converted <- iconv(value, from = "", to = "UTF-8", sub = NA_character_)
  if (anyNA(converted) || !all(validUTF8(converted))) {
    .builder_release_record_error("contains text that is not valid UTF-8.")
  }
  non_ascii <- grepl("[^ -~]", converted)
  Encoding(converted[non_ascii]) <- "UTF-8"
  converted
}

.builder_release_raw_md5 <- function(bytes) {
  path <- tempfile("cerebro-builder-record-")
  connection <- .builder_release_open_owner_only(path)
  on.exit(
    {
      try(close(connection), silent = TRUE)
      if (file.exists(path)) {
        unlink(path, force = TRUE)
      }
    },
    add = TRUE
  )
  writeBin(bytes, connection)
  close(connection)
  unname(tools::md5sum(path))
}

.builder_release_open_owner_only <- function(path) {
  previous_umask <- Sys.umask("0177")
  on.exit(Sys.umask(previous_umask), add = TRUE)
  suppressWarnings(file(path, open = "w+xb"))
}

.builder_release_split_preserve <- function(text, delimiter) {
  positions <- gregexpr(delimiter, text, fixed = TRUE)[[1L]]
  if (identical(positions, -1L)) {
    return(text)
  }
  starts <- c(1L, positions + nchar(delimiter))
  ends <- c(positions - 1L, nchar(text))
  mapply(
    function(start, end) substr(text, start, end),
    starts,
    ends,
    USE.NAMES = FALSE
  )
}

.builder_release_read_record <- function(
  target,
  identity = builder_release_identity(target),
  exact = TRUE,
  allow_abandoned = FALSE
) {
  target <- .builder_release_path(target)
  record_path <- file.path(target, .builder_release_record_name)
  if (!file.exists(record_path)) {
    return(NULL)
  }
  if (dir.exists(record_path) || .builder_release_link(record_path)) {
    .builder_release_record_error("is not a regular file.")
  }
  if (!identical(.Platform$OS.type, "windows")) {
    mode <- file.info(record_path)$mode
    if (
      length(mode) != 1L || is.na(mode) || bitwAnd(as.integer(mode), 73L) != 0L
    ) {
      .builder_release_record_error("must not be executable.")
    }
  }
  size <- file.info(record_path)$size
  if (length(size) != 1L || is.na(size) || size < 1) {
    .builder_release_record_error("is empty or unreadable.")
  }
  bytes <- readBin(record_path, what = "raw", n = as.integer(size))
  identity_paths <- if (length(identity$entries)) {
    vapply(identity$entries, `[[`, character(1), "path")
  } else {
    character()
  }
  record_index <- which(identity_paths %in% .builder_release_record_name)
  if (
    length(record_index) != 1L ||
      !identical(identity$entries[[record_index]]$type, "file") ||
      !identical(
        as.double(length(bytes)),
        identity$entries[[record_index]]$size
      ) ||
      !identical(
        .builder_release_raw_md5(bytes),
        identity$entries[[record_index]]$md5
      )
  ) {
    .builder_release_record_error("changed while its snapshot was read.")
  }
  text <- tryCatch(rawToChar(bytes), error = function(error) NA_character_)
  if (
    length(text) != 1L ||
      is.na(text) ||
      !isTRUE(validUTF8(text)) ||
      grepl("[\001-\010\013\014\016-\037\177]", text)
  ) {
    .builder_release_record_error("is not valid UTF-8 text.")
  }
  Encoding(text) <- "UTF-8"
  lines <- .builder_release_split_preserve(text, "\n")
  if (length(lines) && !nzchar(lines[[length(lines)]])) {
    lines <- lines[-length(lines)]
  }
  if (
    !length(lines) || !identical(lines[[1L]], .builder_release_record_header)
  ) {
    .builder_release_record_error("has an invalid header.")
  }
  member_lines <- lines[-1L]
  if (any(!nzchar(member_lines))) {
    .builder_release_record_error("contains a blank member.")
  }
  if (!identical(member_lines, sort(member_lines, method = "radix"))) {
    .builder_release_record_error("members are not sorted.")
  }
  members <- lapply(member_lines, function(line) {
    fields <- .builder_release_split_preserve(line, "\t")
    if (length(fields) != 2L || !fields[[1L]] %in% c("F", "D")) {
      .builder_release_record_error("contains a malformed member.")
    }
    path <- tryCatch(
      .portableBundlePath(fields[[2L]], "An ownership record member"),
      error = function(error) NULL
    )
    if (
      is.null(path) ||
        identical(path, .builder_release_record_name) ||
        grepl("[[:cntrl:]]", path)
    ) {
      .builder_release_record_error("contains an unsafe member path.")
    }
    list(type = fields[[1L]], path = path)
  })
  member_paths <- if (length(members)) {
    vapply(members, `[[`, character(1), "path")
  } else {
    character()
  }
  if (anyDuplicated(member_paths)) {
    .builder_release_record_error("contains a duplicate member.")
  }
  actual <- .builder_release_identity_members(identity)
  actual_paths <- if (length(actual)) {
    vapply(actual, `[[`, character(1), "path")
  } else {
    character()
  }
  actual_by_path <- stats::setNames(actual, actual_paths)
  missing <- setdiff(member_paths, actual_paths)
  wrong_type <- vapply(
    members,
    function(member) {
      !member$path %in% actual_paths ||
        !identical(actual_by_path[[member$path]]$type, member$type)
    },
    logical(1)
  )
  recorded_present <- intersect(member_paths, actual_paths)
  shell_foreign <- actual_paths[
    !.builder_release_ignorable_metadata(actual_paths)
  ]
  abandoned <- isTRUE(allow_abandoned) &&
    !isTRUE(exact) &&
    length(member_paths) > 0L &&
    !length(recorded_present) &&
    !length(shell_foreign)
  if ((length(missing) || any(wrong_type)) && !abandoned) {
    .builder_release_record_error("does not match its recorded members.")
  }
  foreign <- if (abandoned) {
    character()
  } else {
    setdiff(actual_paths, member_paths)
  }
  foreign <- foreign[!.builder_release_ignorable_metadata(foreign)]
  if (isTRUE(exact) && length(foreign)) {
    .builder_release_record_error("does not match the complete release.")
  }
  list(
    bytes = bytes,
    members = members,
    foreign = foreign,
    abandoned = abandoned,
    identity = identity
  )
}

.builder_release_record_postcondition <- function(path, expected_size) {
  if (
    .builder_release_link(path) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    return(NULL)
  }
  info <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(info) ||
      !identical(as.character(info$type), "file") ||
      !identical(as.double(info$size), as.double(expected_size)) ||
      !identical(as.double(info$hard_links), 1)
  ) {
    return(NULL)
  }
  if (!identical(.Platform$OS.type, "windows")) {
    mode <- suppressWarnings(as.integer(info$permissions))
    if (
      length(mode) != 1L ||
        is.na(mode) ||
        bitwAnd(mode, strtoi("177", base = 8L)) != 0L
    ) {
      return(NULL)
    }
  }
  info
}

.builder_release_record_bytes <- function(path, expected_size) {
  if (
    length(expected_size) != 1L ||
      is.na(expected_size) ||
      expected_size < 0 ||
      expected_size >= .Machine$integer.max
  ) {
    return(NULL)
  }
  connection <- tryCatch(
    file(path, open = "rb"),
    error = function(error) NULL
  )
  if (is.null(connection)) {
    return(NULL)
  }
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  tryCatch(
    readBin(connection, what = "raw", n = as.integer(expected_size) + 1L),
    error = function(error) NULL
  )
}

.builder_release_write_record <- function(
  target,
  identity,
  token,
  .move = file.rename,
  .after_create = function(temporary) invisible(NULL),
  .digest_cache = NULL,
  .on_file = NULL
) {
  target <- .builder_release_path(target)
  if (
    !.builder_release_identity_valid(identity) || !.builder_release_text(token)
  ) {
    .builder_release_record_error("cannot be written from invalid state.")
  }
  members <- .builder_release_identity_members(identity)
  if (length(members)) {
    utf8_paths <- .builder_release_utf8(vapply(
      members,
      `[[`,
      character(1),
      "path"
    ))
    members <- Map(
      function(member, path) {
        member$path <- path
        member
      },
      members,
      utf8_paths
    )
  }
  lines <- if (length(members)) {
    sort(
      vapply(
        members,
        function(member) {
          paste(member$type, member$path, sep = "\t")
        },
        ""
      ),
      method = "radix"
    )
  } else {
    character()
  }
  content <- paste0(
    paste(c(.builder_release_record_header, lines), collapse = "\n"),
    "\n"
  )
  record_path <- file.path(target, .builder_release_record_name)
  temporary <- file.path(
    target,
    paste0(".o-", token, ".tmp")
  )
  connection <- tryCatch(
    .builder_release_open_owner_only(temporary),
    error = function(error) NULL
  )
  if (is.null(connection)) {
    .builder_release_record_error(
      "temporary path could not be created exclusively."
    )
  }
  created <- TRUE
  connection_open <- TRUE
  moved <- FALSE
  on.exit(
    {
      if (connection_open) {
        try(close(connection), silent = TRUE)
      }
      if (!moved && created) {
        unlink(temporary, force = TRUE)
      }
    },
    add = TRUE
  )
  content_bytes <- charToRaw(enc2utf8(content))
  writeBin(content_bytes, connection)
  flush(connection)
  created_info <- tryCatch(
    fs::file_info(temporary, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(created_info) ||
      !identical(as.character(created_info$type), "file") ||
      !identical(as.double(created_info$size), as.double(length(content_bytes)))
  ) {
    .builder_release_record_error("temporary file is unsafe after creation.")
  }
  .after_create(temporary)
  final_info <- tryCatch(
    fs::file_info(temporary, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  stable_fields <- c(
    "type",
    "size",
    "permissions",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  seek(connection, where = 0L, origin = "start", rw = "read")
  confirmed_bytes <- readBin(
    connection,
    what = "raw",
    n = length(content_bytes) + 1L
  )
  verified_info <- tryCatch(
    fs::file_info(temporary, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(final_info) ||
      is.null(verified_info) ||
      !identical(
        as.list(created_info[stable_fields]),
        as.list(final_info[stable_fields])
      ) ||
      !identical(
        as.list(final_info[stable_fields]),
        as.list(verified_info[stable_fields])
      ) ||
      !identical(confirmed_bytes, content_bytes)
  ) {
    .builder_release_record_error(
      "temporary file is unsafe before publication."
    )
  }
  close(connection)
  connection_open <- FALSE
  record_move <- .builder_release_move_attempt(
    temporary,
    record_path,
    action = "publish release ownership record",
    .move = .move
  )
  if (!isTRUE(record_move$ok)) {
    .builder_release_record_error(paste(
      "could not be replaced atomically.",
      .nativeFileOperationMessage(record_move)
    ))
  }
  moved <- TRUE
  tryCatch(
    {
      published_info <- .builder_release_record_postcondition(
        record_path,
        length(content_bytes)
      )
      published_bytes <- .builder_release_record_bytes(
        record_path,
        length(content_bytes)
      )
      if (
        is.null(published_info) ||
          !identical(published_bytes, content_bytes)
      ) {
        .builder_release_record_error("changed during publication.")
      }
      final_identity <- builder_release_identity(
        target,
        .digest_cache = .digest_cache,
        .on_file = .on_file
      )
      published <- .builder_release_read_record(
        target,
        final_identity,
        exact = TRUE
      )
      confirmed_info <- .builder_release_record_postcondition(
        record_path,
        length(content_bytes)
      )
      confirmed_bytes <- .builder_release_record_bytes(
        record_path,
        length(content_bytes)
      )
      if (
        is.null(confirmed_info) ||
          !identical(published$bytes, content_bytes) ||
          !identical(confirmed_bytes, content_bytes)
      ) {
        .builder_release_record_error("changed during publication.")
      }
      published
    },
    error = function(error) {
      unlink(record_path, force = TRUE)
      stop(error)
    }
  )
}

builder_release_state <- function(
  target,
  exact_record = TRUE,
  allow_abandoned = FALSE,
  .digest_cache = NULL
) {
  if (is.null(.digest_cache)) {
    .digest_cache <- new.env(parent = emptyenv())
  }
  identity <- builder_release_identity(target, .digest_cache = .digest_cache)
  record <- if (isTRUE(identity$exists)) {
    .builder_release_read_record(
      target,
      identity,
      exact = exact_record,
      allow_abandoned = allow_abandoned
    )
  } else {
    NULL
  }
  confirmed_identity <- builder_release_identity(
    target,
    .digest_cache = .digest_cache
  )
  if (!identical(confirmed_identity, identity)) {
    stop("The release changed while its snapshot was read.", call. = FALSE)
  }
  list(
    schema_version = 1L,
    identity = identity,
    record = if (is.null(record)) {
      NULL
    } else {
      list(
        bytes = record$bytes,
        members = record$members,
        foreign = record$foreign,
        abandoned = isTRUE(record$abandoned)
      )
    }
  )
}

builder_release_output_preflight <- function(target, expected_roots) {
  expected_roots <- unique(sub("/.*$", "", as.character(expected_roots)))
  prior_state <- tryCatch(
    builder_release_state(
      target,
      exact_record = FALSE,
      allow_abandoned = TRUE
    ),
    error = function(error) error
  )
  if (inherits(prior_state, "condition")) {
    return(list(
      ok = FALSE,
      error = conditionMessage(prior_state),
      foreign = character()
    ))
  }
  prior_members <- prior_state$record$members %||% list()
  prior_roots <- if (length(prior_members)) {
    unique(sub(
      "/.*$",
      "",
      vapply(prior_members, `[[`, character(1), "path")
    ))
  } else {
    character()
  }
  actual_roots <- list.files(target, all.files = TRUE, no.. = TRUE)
  metadata <- .builder_release_ignorable_metadata(actual_roots)
  if (any(metadata)) {
    metadata_paths <- file.path(target, actual_roots[metadata])
    metadata[metadata] <- file.exists(metadata_paths) &
      !dir.exists(metadata_paths) &
      !vapply(metadata_paths, .builder_release_link, logical(1))
    actual_roots <- actual_roots[!metadata]
  }
  foreign <- setdiff(
    actual_roots,
    c(expected_roots, prior_roots, .builder_release_record_name)
  )
  list(
    ok = !length(foreign),
    error = NULL,
    foreign = foreign,
    prior_state = prior_state
  )
}

.builder_release_identity_valid <- function(identity) {
  is.list(identity) &&
    identical(identity$schema_version, 1L) &&
    is.logical(identity$exists) &&
    length(identity$exists) == 1L &&
    !is.na(identity$exists) &&
    is.list(identity$entries)
}

.builder_release_atomic_directory <- function(control) {
  directory <- file.path(control, "diagnostics")
  if (!dir.exists(directory)) {
    if (
      .builder_release_exists(directory) ||
        !dir.create(directory, mode = "0700")
    ) {
      stop("The release journal workspace could not be created.", call. = FALSE)
    }
  }
  if (
    .builder_release_link(directory) ||
      !.builder_release_mode_owner_only(directory)
  ) {
    stop("The release journal workspace is unsafe.", call. = FALSE)
  }
  directory
}

.builder_release_atomic_rds <- function(value, path, token, control) {
  if (!.pathWithin(path, control)) {
    stop("The release journal path escaped its control root.", call. = FALSE)
  }
  temporary_root <- .builder_release_atomic_directory(control)
  temporary <- file.path(
    temporary_root,
    paste0(".r-", .builder_release_token("a"), ".tmp")
  )
  if (.builder_release_exists(temporary)) {
    stop("A journal temporary path is already occupied.", call. = FALSE)
  }
  saved <- FALSE
  on.exit(
    {
      if (!saved && file.exists(temporary)) unlink(temporary, force = TRUE)
    },
    add = TRUE
  )
  saveRDS(value, temporary, version = 3)
  Sys.chmod(temporary, mode = "0600")
  moved <- tryCatch(
    {
      fs::file_move(temporary, path)
      !file.exists(temporary) &&
        file.exists(path) &&
        !dir.exists(path) &&
        !.builder_release_link(path)
    },
    error = function(error) FALSE
  )
  if (!isTRUE(moved)) {
    stop(
      "The publication journal could not be replaced atomically.",
      call. = FALSE
    )
  }
  saved <- TRUE
  invisible(value)
}

.builder_release_read_rds <- function(path, subject) {
  if (!file.exists(path) || dir.exists(path) || .builder_release_link(path)) {
    stop(subject, " is missing or unsafe.", call. = FALSE)
  }
  tryCatch(readRDS(path), error = function(error) {
    stop(subject, " cannot be read safely.", call. = FALSE)
  })
}

.builder_release_allowed_control <- c(
  "journal.rds",
  "lock",
  "stages",
  "backup",
  "diagnostics"
)

.builder_release_assert_control <- function(control) {
  if (!dir.exists(control) || .builder_release_link(control)) {
    stop("The release control path is not a real directory.", call. = FALSE)
  }
  if (!.builder_release_mode_owner_only(control)) {
    stop("The release control directory must be owner-only.", call. = FALSE)
  }
  entries <- list.files(control, all.files = TRUE, no.. = TRUE)
  unknown <- setdiff(entries, .builder_release_allowed_control)
  if (length(unknown)) {
    stop(
      "The release has unknown control occupants: ",
      paste(unknown, collapse = ", "),
      ". Nothing was removed.",
      call. = FALSE
    )
  }
  known_paths <- file.path(control, entries)
  linked <- vapply(known_paths, .builder_release_link, logical(1))
  if (any(linked)) {
    stop(
      "The release control directory contains a symbolic link: ",
      entries[[which(linked)[[1L]]]],
      ". Nothing was removed.",
      call. = FALSE
    )
  }
  diagnostics <- file.path(control, "diagnostics")
  if (
    .builder_release_exists(diagnostics) &&
      (!dir.exists(diagnostics) ||
        !.builder_release_mode_owner_only(diagnostics))
  ) {
    stop("The release journal workspace is unsafe.", call. = FALSE)
  }
  invisible(control)
}

.builder_release_ensure_control <- function(target) {
  control <- builder_release_control_path(target)
  if (!dir.exists(control)) {
    if (
      .builder_release_exists(control) || !dir.create(control, mode = "0700")
    ) {
      stop("The release control directory could not be created.", call. = FALSE)
    }
  }
  .builder_release_assert_control(control)
  control
}

.builder_release_owner <- function(token) {
  list(
    schema_version = 1L,
    token = token,
    host = .builder_release_host(),
    pid = as.integer(Sys.getpid()),
    acquired_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

.builder_release_acquire_lock <- function(control, token) {
  lock <- file.path(control, "lock")
  if (!dir.create(lock, mode = "0700", showWarnings = FALSE)) {
    stop(
      "Another coordinator owns this release, or recovery is required.",
      call. = FALSE
    )
  }
  owner_path <- file.path(lock, "owner.rds")
  owner <- .builder_release_owner(token)
  tryCatch(
    .builder_release_atomic_rds(owner, owner_path, token, control),
    error = function(error) {
      entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
      if (!length(entries)) {
        unlink(lock, recursive = TRUE, force = TRUE)
      }
      stop(error)
    }
  )
  lock
}

.builder_release_lock_owner <- function(lock) {
  owner <- .builder_release_read_rds(
    file.path(lock, "owner.rds"),
    "The release lock owner"
  )
  valid <- is.list(owner) &&
    identical(owner$schema_version, 1L) &&
    .builder_release_text(owner$token) &&
    .builder_release_text(owner$host) &&
    is.integer(owner$pid) &&
    length(owner$pid) == 1L &&
    !is.na(owner$pid)
  if (!valid) {
    stop("The release lock owner is invalid.", call. = FALSE)
  }
  owner
}

.builder_release_assert_lock <- function(lock, token) {
  if (!dir.exists(lock) || .builder_release_link(lock)) {
    stop("The coordinator no longer owns the release lock.", call. = FALSE)
  }
  owner <- .builder_release_lock_owner(lock)
  if (!identical(owner$token, token)) {
    stop("The coordinator release lock identity changed.", call. = FALSE)
  }
  invisible(owner)
}

.builder_release_transfer_lock <- function(
  lock,
  token,
  from_pid,
  to_pid = as.integer(Sys.getpid())
) {
  valid_pid <- function(pid) {
    is.integer(pid) && length(pid) == 1L && !is.na(pid) && pid > 0L
  }
  from_pid <- as.integer(from_pid)
  to_pid <- as.integer(to_pid)
  if (!valid_pid(from_pid) || !valid_pid(to_pid)) {
    stop("The release lock process identity is invalid.", call. = FALSE)
  }
  owner <- .builder_release_assert_lock(lock, token)
  if (identical(owner$pid, to_pid)) {
    return(invisible(owner))
  }
  if (
    !identical(owner$pid, from_pid) ||
      !identical(owner$host, .builder_release_host())
  ) {
    stop("The release lock ownership changed.", call. = FALSE)
  }
  owner$pid <- to_pid
  owner$transferred_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  .builder_release_atomic_rds(
    owner,
    file.path(lock, "owner.rds"),
    token,
    dirname(lock)
  )
  confirmed <- .builder_release_assert_lock(lock, token)
  if (!identical(confirmed$pid, to_pid)) {
    stop("The release lock ownership transfer failed.", call. = FALSE)
  }
  invisible(confirmed)
}

.builder_release_lock_known <- function(lock) {
  entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
  identical(entries, "owner.rds")
}

.builder_release_release_lock <- function(control, lock, token) {
  .builder_release_assert_lock(lock, token)
  if (!.builder_release_lock_known(lock)) {
    stop(
      "The release lock contains unknown files and was preserved.",
      call. = FALSE
    )
  }
  isolated <- file.path(
    .builder_release_atomic_directory(control),
    paste0(".l-", .builder_release_token("a"))
  )
  if (.builder_release_exists(isolated) || !file.rename(lock, isolated)) {
    stop("The release lock could not be isolated safely.", call. = FALSE)
  }
  unlink(isolated, recursive = TRUE, force = TRUE)
  !dir.exists(lock)
}

.builder_release_journal_path <- function(control) {
  file.path(control, "journal.rds")
}

.builder_release_write_phase <- function(handle, phase, detail = NULL) {
  journal <- handle$record
  journal$phase <- phase
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$detail <- detail
  .builder_release_atomic_rds(
    journal,
    handle$journal,
    handle$token,
    handle$control
  )
  handle$record <- journal
  handle
}

.builder_release_journal <- function(
  target,
  required = FALSE,
  control = builder_release_control_path(target)
) {
  path <- .builder_release_journal_path(control)
  if (!file.exists(path)) {
    if (required) {
      stop("The release journal is missing.", call. = FALSE)
    }
    return(NULL)
  }
  journal <- .builder_release_read_rds(path, "The release journal")
  valid <- is.list(journal) &&
    identical(journal$schema_version, 1L) &&
    .builder_release_text(journal$target) &&
    .builder_release_text(journal$control) &&
    .builder_release_text(journal$build_id) &&
    .builder_release_text(journal$token) &&
    .builder_release_text(journal$phase) &&
    identical(
      .canonicalTargetPath(journal$target),
      .builder_release_path(target)
    ) &&
    identical(.canonicalTargetPath(journal$control), control)
  if (!valid) {
    stop("The release journal is invalid.", call. = FALSE)
  }
  journal
}

.builder_release_retire_terminal_control <- function(
  target,
  control,
  lock,
  token,
  phase,
  .move = file.rename,
  .unlink = unlink
) {
  retired <- NULL
  cleanup <- tryCatch(
    {
      .builder_release_assert_control(control)
      .builder_release_assert_lock(lock, token)
      if (!.builder_release_lock_known(lock)) {
        stop("The release lock contains unknown files.", call. = FALSE)
      }
      journal <- .builder_release_journal(
        target,
        required = TRUE,
        control = control
      )
      if (
        !identical(journal$phase, phase) ||
          !identical(journal$token, token) ||
          !identical(
            .canonicalTargetPath(journal$lock),
            .canonicalTargetPath(lock)
          )
      ) {
        stop("The terminal release journal changed.", call. = FALSE)
      }
      backup <- .builder_release_or(
        journal$backup,
        file.path(control, "backup")
      )
      if (.builder_release_exists(backup)) {
        stop(
          "The terminal release still has a preserved backup.",
          call. = FALSE
        )
      }
      stages <- file.path(control, "stages")
      diagnostics <- file.path(control, "diagnostics")
      entries <- list.files(control, all.files = TRUE, no.. = TRUE)
      if (
        !setequal(entries, c("diagnostics", "journal.rds", "lock", "stages"))
      ) {
        stop(
          "The terminal release control contains unexpected entries.",
          call. = FALSE
        )
      }
      for (directory in c(stages, diagnostics)) {
        if (
          !dir.exists(directory) ||
            .builder_release_link(directory) ||
            !.builder_release_mode_owner_only(directory) ||
            length(list.files(directory, all.files = TRUE, no.. = TRUE))
        ) {
          stop(
            "The terminal release control is not empty and safe to remove.",
            call. = FALSE
          )
        }
      }
      retired <- tempfile(
        pattern = ".cbr-retired-",
        tmpdir = dirname(control)
      )
      # Isolate the entire registry while its lock is still inside it. A new
      # coordinator can then create the canonical control path without racing
      # recursive cleanup of the retired transaction.
      if (
        .builder_release_exists(retired) ||
          !isTRUE(.move(control, retired)) ||
          .builder_release_exists(control) ||
          !dir.exists(retired)
      ) {
        stop(
          "The terminal release control could not be isolated.",
          call. = FALSE
        )
      }
      removal <- tryCatch(
        {
          .builder_release_remove_stage(retired, .unlink = .unlink)
          NULL
        },
        error = function(error) error
      )
      list(
        cleaned = TRUE,
        warning = if (inherits(removal, "condition")) {
          paste0(
            "The terminal release control was isolated but could not be removed: ",
            retired
          )
        } else {
          NULL
        },
        residue = if (dir.exists(retired)) retired else NULL
      )
    },
    error = function(error) error
  )
  if (!inherits(cleanup, "condition")) {
    return(cleanup)
  }
  if (!.builder_release_exists(control) && !is.null(retired)) {
    return(list(
      cleaned = TRUE,
      warning = paste0(
        "The terminal release control was isolated but could not be removed: ",
        retired
      ),
      residue = if (dir.exists(retired)) retired else NULL
    ))
  }
  if (dir.exists(lock)) {
    .builder_release_release_lock(control, lock, token)
  }
  list(
    cleaned = FALSE,
    warning = paste0(
      "The terminal release control was retained: ",
      conditionMessage(cleanup)
    ),
    residue = control
  )
}

.builder_release_recovery_at <- function(target, control) {
  .builder_release_assert_control(control)
  journal <- .builder_release_journal(target, control = control)
  lock <- file.path(control, "lock")
  if (
    !is.null(journal) &&
      journal$phase %in% c("complete", "aborted", "recovered") &&
      dir.exists(lock)
  ) {
    return(list(
      state = "stale_lock",
      target = target,
      control = control,
      backup = NULL,
      journal = journal
    ))
  }
  if (
    is.null(journal) || journal$phase %in% c("complete", "aborted", "recovered")
  ) {
    return(list(
      state = "ready",
      target = target,
      control = control,
      backup = NULL,
      journal = journal
    ))
  }
  backup <- .builder_release_or(journal$backup, file.path(control, "backup"))
  backup_exists <- dir.exists(backup)
  phase <- journal$phase
  abort_phase <- if (
    identical(phase, "recovering_abort") &&
      .builder_release_text(journal$recovery_from)
  ) {
    journal$recovery_from
  } else {
    phase
  }
  list(
    state = "recovery_required",
    target = target,
    control = control,
    stage = journal$stage,
    backup = backup,
    phase = phase,
    detail = journal$detail,
    backup_exists = backup_exists,
    safe_to_abort = abort_phase %in% c("prepared", "locked") && !backup_exists,
    journal = journal,
    message = if (backup_exists) {
      paste0(
        "An earlier release was interrupted during ",
        phase,
        ". The previous release backup was preserved: ",
        backup
      )
    } else {
      paste0(
        "An earlier release was interrupted during ",
        phase,
        ". No previous-release backup is present."
      )
    }
  )
}

builder_discover_recovery <- function(target) {
  target <- .builder_release_path(target)
  controls <- .builder_release_control_candidates(target)
  controls <- controls[dir.exists(controls)]
  if (!length(controls)) {
    return(list(state = "ready", target = target, backup = NULL))
  }
  states <- lapply(
    controls,
    function(control) .builder_release_recovery_at(target, control)
  )
  actionable <- which(vapply(
    states,
    function(state) !identical(state$state, "ready"),
    logical(1)
  ))
  if (length(actionable) > 1L) {
    stop(
      "Multiple release-control directories require recovery; manual inspection is required.",
      call. = FALSE
    )
  }
  if (length(actionable)) {
    return(states[[actionable[[1L]]]])
  }
  preferred <- match(builder_release_control_path(target), controls)
  with_journal <- which(vapply(
    states,
    function(state) is.list(state$journal),
    logical(1)
  ))
  if (!is.na(preferred) && preferred %in% with_journal) {
    return(states[[preferred]])
  }
  if (length(with_journal)) {
    return(states[[with_journal[[1L]]]])
  }
  if (!is.na(preferred)) states[[preferred]] else states[[1L]]
}

builder_release_cleanup_control <- function(target) {
  target <- .builder_release_path(target)
  if (.builder_release_exists(target)) {
    stop(
      "Release control data cannot be removed while its target exists.",
      call. = FALSE
    )
  }
  controls <- .builder_release_control_candidates(target)
  controls <- controls[dir.exists(controls)]
  if (!length(controls)) {
    return(TRUE)
  }
  plans <- lapply(controls, function(control) {
    .builder_release_assert_control(control)
    recovery <- .builder_release_recovery_at(target, control)
    journal <- recovery$journal
    entries <- list.files(control, all.files = TRUE, no.. = TRUE)
    if (!length(entries) && is.null(journal)) {
      return(list(control = control, empty = TRUE))
    }
    if (
      !identical(recovery$state, "ready") ||
        !is.list(journal) ||
        !identical(journal$phase, "complete")
    ) {
      stop(
        "Incomplete release control data was preserved for recovery.",
        call. = FALSE
      )
    }
    diagnostics <- file.path(control, "diagnostics")
    if (
      dir.exists(diagnostics) &&
        length(list.files(diagnostics, all.files = TRUE, no.. = TRUE))
    ) {
      stop(
        "Release diagnostics were preserved because they are not empty.",
        call. = FALSE
      )
    }
    permitted <- c(
      "journal.rds",
      "stages",
      if (dir.exists(diagnostics)) {
        "diagnostics"
      }
    )
    if (!setequal(entries, permitted)) {
      stop(
        "Release control data contains entries that must be preserved.",
        call. = FALSE
      )
    }
    stages <- file.path(control, "stages")
    if (
      !dir.exists(stages) ||
        .builder_release_link(stages) ||
        length(list.files(stages, all.files = TRUE, no.. = TRUE))
    ) {
      stop(
        "Release stage data was preserved because it is not empty and safe.",
        call. = FALSE
      )
    }
    list(
      control = control,
      empty = FALSE,
      diagnostics = diagnostics,
      stages = stages,
      journal = file.path(control, "journal.rds")
    )
  })
  for (plan in plans) {
    control <- plan$control
    if (!isTRUE(plan$empty)) {
      if (dir.exists(plan$diagnostics)) {
        unlink(plan$diagnostics, recursive = TRUE, force = TRUE)
      }
      unlink(plan$stages, recursive = TRUE, force = TRUE)
      unlink(plan$journal, force = TRUE)
    }
    if (length(list.files(control, all.files = TRUE, no.. = TRUE))) {
      stop("Release control data changed during cleanup.", call. = FALSE)
    }
    unlink(control, recursive = TRUE, force = TRUE)
    if (dir.exists(control)) {
      stop("Release control data could not be removed.", call. = FALSE)
    }
  }
  TRUE
}

builder_prepare_release <- function(
  target,
  build_id,
  expected_prior = NULL,
  expected_prior_state = NULL,
  .stage_guard = function(stage) invisible(TRUE)
) {
  target <- .builder_release_path(target)
  if (!.builder_release_text(build_id)) {
    stop("A release build id is required.", call. = FALSE)
  }
  .portableBundlePath(
    gsub("[^A-Za-z0-9._-]", "-", build_id),
    "The release build id"
  )
  if (!is.function(.stage_guard)) {
    stop("The release stage guard is invalid.", call. = FALSE)
  }
  control <- builder_release_control_path(target)
  stages <- file.path(control, "stages")
  stage <- gsub(
    "\\",
    "/",
    tempfile(pattern = "s-", tmpdir = stages),
    fixed = TRUE
  )
  .stage_guard(stage)
  control <- .builder_release_ensure_control(target)
  recovery <- builder_discover_recovery(target)
  if (identical(recovery$state, "stale_lock")) {
    .builder_release_isolate_stale_lock(recovery$control, recovery$journal)
    recovery <- builder_discover_recovery(target)
  }
  completed_journal <- recovery$journal
  completed_control <- recovery$control %||% control
  if (
    identical(recovery$state, "ready") &&
      is.list(completed_journal) &&
      identical(completed_journal$phase, "aborted") &&
      .builder_release_text(completed_journal$stage) &&
      dir.exists(completed_journal$stage)
  ) {
    abandoned_stage <- .canonicalTargetPath(completed_journal$stage)
    owned_stages <- .canonicalTargetPath(file.path(completed_control, "stages"))
    if (
      !identical(dirname(abandoned_stage), owned_stages) ||
        .builder_release_link(abandoned_stage)
    ) {
      stop(
        "The aborted release stage is outside its owned registry; manual recovery is required.",
        call. = FALSE
      )
    }
    .builder_release_remove_stage(abandoned_stage)
  }
  completed_backup <- if (is.null(completed_journal)) {
    NULL
  } else {
    completed_journal$backup
  }
  if (
    identical(recovery$state, "ready") &&
      .builder_release_text(completed_backup) &&
      dir.exists(completed_backup)
  ) {
    backup_identity <- builder_release_identity(completed_backup)
    if (!identical(backup_identity, completed_journal$expected_prior)) {
      stop(
        "A completed release has an unrecognized preserved backup.",
        call. = FALSE
      )
    }
    unlink(completed_backup, recursive = TRUE, force = TRUE)
    if (dir.exists(completed_backup)) {
      stop("The completed release backup could not be removed.", call. = FALSE)
    }
  }
  if (
    identical(recovery$state, "recovery_required") &&
      isTRUE(recovery$safe_to_abort)
  ) {
    auto_recovered <- tryCatch(
      builder_recover_release(target, action = "abort"),
      error = function(error) error
    )
    if (
      !inherits(auto_recovered, "condition") &&
        isTRUE(auto_recovered$recovered)
    ) {
      recovery <- builder_discover_recovery(target)
      control <- .builder_release_ensure_control(target)
    }
  }
  if (identical(recovery$state, "recovery_required")) {
    stop(recovery$message, call. = FALSE)
  }
  token <- .builder_release_token("release")
  lock <- .builder_release_acquire_lock(control, token)
  prepared <- FALSE
  on.exit(
    {
      if (!prepared && dir.exists(lock)) {
        try(.builder_release_release_lock(control, lock, token), silent = TRUE)
      }
    },
    add = TRUE
  )
  if (!dir.exists(stages) && !dir.create(stages, mode = "0700")) {
    stop("The release stage registry could not be created.", call. = FALSE)
  }
  if (
    .builder_release_link(stages) || !.builder_release_mode_owner_only(stages)
  ) {
    stop("The release stage registry is unsafe.", call. = FALSE)
  }
  # The build id and owner token belong in the journal, not in every payload
  # path. A short, bounded stage name preserves substantially more of the
  # legacy Windows MAX_PATH budget for real dataset and Viewer files.
  if (.builder_release_exists(stage) || !dir.create(stage, mode = "0700")) {
    stop("The assigned release stage could not be created.", call. = FALSE)
  }
  if (is.null(expected_prior)) {
    expected_prior <- builder_release_identity(target)
  }
  if (!.builder_release_identity_valid(expected_prior)) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  if (is.null(expected_prior_state)) {
    expected_prior_state <- builder_release_state(target, exact_record = FALSE)
  }
  if (
    !is.list(expected_prior_state) ||
      !identical(expected_prior_state$schema_version, 1L) ||
      !identical(expected_prior_state$identity, expected_prior)
  ) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("The expected prior release state is invalid.", call. = FALSE)
  }
  journal <- .builder_release_journal_path(control)
  backup <- file.path(control, "backup")
  if (.builder_release_exists(backup)) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("A preserved release backup requires recovery.", call. = FALSE)
  }
  record <- list(
    schema_version = 1L,
    target = target,
    control = control,
    stage = stage,
    backup = backup,
    journal = journal,
    lock = lock,
    build_id = build_id,
    token = token,
    host = .builder_release_host(),
    pid = as.integer(Sys.getpid()),
    expected_prior = expected_prior,
    expected_prior_state = expected_prior_state,
    phase = "prepared",
    updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    detail = NULL
  )
  .builder_release_atomic_rds(record, journal, token, control)
  prepared <- TRUE
  structure(
    c(record, list(record = record)),
    class = c("builder_release_handle", "list")
  )
}

.builder_release_handle <- function(handle) {
  if (!inherits(handle, "builder_release_handle")) {
    stop("A release handle is required.", call. = FALSE)
  }
  .builder_release_assert_lock(handle$lock, handle$token)
  journal <- .builder_release_journal(
    handle$target,
    required = TRUE,
    control = handle$control
  )
  if (!identical(journal$token, handle$token)) {
    stop("The release journal identity changed.", call. = FALSE)
  }
  handle$record <- journal
  handle
}

.builder_release_root_names <- function(root) {
  if (!dir.exists(root) || .builder_release_link(root)) {
    return(character())
  }
  roots <- list.files(root, all.files = TRUE, no.. = TRUE)
  roots <- unique(as.character(roots))
  if (
    any(
      is.na(roots) |
        !nzchar(roots) |
        roots %in% c(".", "..") |
        grepl("[/\\\\]", roots)
    )
  ) {
    stop("A release root entry has an unsafe name.", call. = FALSE)
  }
  roots
}

.builder_release_move_attempt <- function(
  from,
  to,
  action,
  .move = file.rename,
  .sleep = Sys.sleep,
  .retry_delays = if (identical(.move, file.rename)) {
    c(0, 0.05, 0.15, 0.3, 0.5)
  } else {
    0
  }
) {
  delays <- suppressWarnings(as.numeric(.retry_delays))
  if (
    !length(delays) ||
      anyNA(delays) ||
      any(!is.finite(delays)) ||
      any(delays < 0)
  ) {
    stop("Release move retry delays are invalid.", call. = FALSE)
  }
  attempt <- NULL
  for (index in seq_along(delays)) {
    if (index > 1L && delays[[index]] > 0) {
      .sleep(delays[[index]])
    }
    attempt <- .nativeFileOperationAttempt(
      action = action,
      source = from,
      destination = to,
      operation = function() .move(from, to)
    )
    attempt$attempt_count <- index
    if (isTRUE(attempt$ok)) {
      return(attempt)
    }
    safe_to_retry <-
      .builder_release_exists(from) &&
      !.builder_release_link(from) &&
      !.builder_release_exists(to) &&
      !.builder_release_link(to)
    if (!isTRUE(safe_to_retry)) break
  }
  attempt
}

.builder_release_move_error <- function(summary, attempt) {
  paste(summary, .nativeFileOperationMessage(attempt))
}

.builder_release_move_roots <- function(
  from,
  to,
  roots,
  .move = file.rename,
  action = "move release entry"
) {
  roots <- unique(as.character(if (is.null(roots)) character() else roots))
  moved <- character()
  for (root in roots) {
    source <- file.path(from, root)
    destination <- file.path(to, root)
    if (!.builder_release_exists(source)) {
      next
    }
    if (.builder_release_exists(destination)) {
      attempt <- .nativeFileOperationAttempt(
        action = action,
        source = source,
        destination = destination,
        operation = function() FALSE
      )
      attempt$error <- list(
        message = "the destination already exists",
        class = "builder_release_destination_exists",
        fields = character()
      )
      return(list(
        ok = FALSE,
        moved = moved,
        failed = root,
        attempt = attempt
      ))
    }
    attempt <- .builder_release_move_attempt(
      source,
      destination,
      action = action,
      .move = .move
    )
    if (!isTRUE(attempt$ok)) {
      return(list(
        ok = FALSE,
        moved = moved,
        failed = root,
        attempt = attempt
      ))
    }
    moved <- c(moved, root)
  }
  list(ok = TRUE, moved = moved, failed = NULL, attempt = NULL)
}

.builder_release_restore_in_place <- function(
  handle,
  detail,
  .move = file.rename
) {
  phase <- if (is.null(handle$record$phase)) "" else handle$record$phase
  origin <- handle$record$in_place_recovery_from
  if (is.null(origin)) {
    origin <- handle$record$recovery_from
  }
  if (is.null(origin)) {
    origin <- phase
  }
  move_new_back <- origin %in%
    c(
      "new_moving_in_place",
      "new_published",
      "complete",
      "recovery_required"
    )
  if (!dir.exists(handle$target) && !dir.create(handle$target)) {
    handle$record$in_place_recovery_from <- origin
    handle <- .builder_release_write_phase(handle, "recovery_required", detail)
    return(list(handle = handle, restored = FALSE))
  }
  if (!dir.exists(handle$stage) && !dir.create(handle$stage, mode = "0700")) {
    handle$record$in_place_recovery_from <- origin
    handle <- .builder_release_write_phase(handle, "recovery_required", detail)
    return(list(handle = handle, restored = FALSE))
  }
  if (move_new_back) {
    moved_new <- .builder_release_move_roots(
      handle$target,
      handle$stage,
      handle$record$new_roots,
      .move = .move,
      action = "restore new release entry to stage"
    )
    if (!isTRUE(moved_new$ok)) {
      detail <- .builder_release_move_error(detail, moved_new$attempt)
      handle$record$in_place_recovery_from <- origin
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(
        handle = handle,
        restored = FALSE,
        attempt = moved_new$attempt
      ))
    }
  }
  restored_prior <- .builder_release_move_roots(
    handle$backup,
    handle$target,
    handle$record$prior_roots,
    .move = .move,
    action = "restore prior release entry from backup"
  )
  if (
    !isTRUE(restored_prior$ok) ||
      !identical(
        builder_release_identity(handle$target),
        handle$expected_prior
      )
  ) {
    if (!isTRUE(restored_prior$ok)) {
      detail <- .builder_release_move_error(
        detail,
        restored_prior$attempt
      )
    }
    handle$record$in_place_recovery_from <- origin
    handle <- .builder_release_write_phase(handle, "recovery_required", detail)
    return(list(
      handle = handle,
      restored = FALSE,
      attempt = restored_prior$attempt
    ))
  }
  if (dir.exists(handle$backup)) {
    unlink(handle$backup, recursive = TRUE, force = TRUE)
  }
  handle$record$in_place_recovery_from <- NULL
  handle <- .builder_release_write_phase(handle, "prepared", detail)
  list(handle = handle, restored = TRUE, attempt = NULL)
}

.builder_release_restore <- function(handle, detail, .move = file.rename) {
  if (identical(handle$record$publication_mode, "in_place")) {
    return(.builder_release_restore_in_place(handle, detail, .move = .move))
  }
  target_exists <- .builder_release_exists(handle$target)
  backup_exists <- dir.exists(handle$backup)
  if (target_exists) {
    if (.builder_release_exists(handle$stage)) {
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(handle = handle, restored = FALSE))
    }
    moved_target <- .builder_release_move_attempt(
      handle$target,
      handle$stage,
      action = "restore published release to stage",
      .move = .move
    )
    if (!isTRUE(moved_target$ok)) {
      detail <- .builder_release_move_error(detail, moved_target)
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(
        handle = handle,
        restored = FALSE,
        attempt = moved_target
      ))
    }
  }
  moved_backup <- if (backup_exists) {
    .builder_release_move_attempt(
      handle$backup,
      handle$target,
      action = "restore prior release from backup",
      .move = .move
    )
  } else {
    NULL
  }
  if (is.list(moved_backup) && !isTRUE(moved_backup$ok)) {
    detail <- .builder_release_move_error(detail, moved_backup)
    handle <- .builder_release_write_phase(handle, "recovery_required", detail)
    return(list(
      handle = handle,
      restored = FALSE,
      attempt = moved_backup
    ))
  }
  handle <- .builder_release_write_phase(handle, "prepared", detail)
  list(handle = handle, restored = TRUE, attempt = NULL)
}

builder_publish_release <- function(
  handle,
  .move = file.rename,
  .after_phase = function(phase) invisible(NULL),
  .after_move = function(move) invisible(NULL),
  .verify_payload = function(root, phase) TRUE,
  .digest_cache = NULL,
  .allow_in_place = identical(.Platform$OS.type, "windows") &&
    identical(.move, file.rename)
) {
  if (is.null(.digest_cache)) {
    .digest_cache <- new.env(parent = emptyenv())
  }
  handle <- .builder_release_handle(handle)
  verify_or_restore <- function(root, phase) {
    verified <- tryCatch(
      identical(.verify_payload(root, phase), TRUE),
      error = function(error) FALSE
    )
    if (isTRUE(verified)) {
      return(invisible(TRUE))
    }
    restored <- .builder_release_restore(
      handle,
      "Publication verification failed; the prior release was restored.",
      .move = .move
    )
    if (!isTRUE(restored$restored)) {
      stop("Publication verification and restoration failed.", call. = FALSE)
    }
    stop(
      "Publication verification failed; the prior release was restored.",
      call. = FALSE
    )
  }
  if (!identical(handle$record$phase, "prepared")) {
    stop("The release is not in its prepared phase.", call. = FALSE)
  }
  if (!dir.exists(handle$stage) || .builder_release_link(handle$stage)) {
    stop("The assigned release stage is missing or unsafe.", call. = FALSE)
  }
  handle$record$prepared_identity <- builder_release_identity(
    handle$stage,
    .digest_cache = .digest_cache
  )
  if (
    !is.null(handle$expected_stage_identity) &&
      !identical(
        handle$record$prepared_identity,
        handle$expected_stage_identity
      )
  ) {
    stop("The verified stage changed before publication.", call. = FALSE)
  }
  handle <- .builder_release_write_phase(handle, "locked")
  current_state <- builder_release_state(
    handle$target,
    exact_record = FALSE,
    allow_abandoned = isTRUE(
      handle$expected_prior_state$record$abandoned
    ),
    .digest_cache = .digest_cache
  )
  current <- current_state$identity
  if (
    !identical(current, handle$expected_prior) ||
      !identical(current_state, handle$expected_prior_state)
  ) {
    stop(
      "The release changed after Review; nothing was published.",
      call. = FALSE
    )
  }
  in_place <- FALSE
  old_move <- NULL
  if (isTRUE(current$exists)) {
    handle <- .builder_release_write_phase(handle, "old_moving")
    .after_phase("old_moving")
    old_move <- .builder_release_move_attempt(
      handle$target,
      handle$backup,
      action = "protect prior release",
      .move = .move
    )
    moved <- isTRUE(old_move$ok)
    if (!isTRUE(moved)) {
      if (
        isTRUE(.allow_in_place) &&
          dir.exists(handle$target) &&
          !.builder_release_link(handle$target)
      ) {
        handle$record$publication_mode <- "in_place"
        handle$record$prior_roots <- .builder_release_root_names(handle$target)
        handle$record$prior_roots <- handle$record$prior_roots[
          !.builder_release_ignorable_metadata(handle$record$prior_roots)
        ]
        handle$record$new_roots <- .builder_release_root_names(handle$stage)
        handle <- .builder_release_write_phase(handle, "old_moving_in_place")
        if (
          !dir.exists(handle$backup) &&
            !dir.create(handle$backup, mode = "0700")
        ) {
          stop("The prior release backup could not be created.", call. = FALSE)
        }
        protected <- .builder_release_move_roots(
          handle$target,
          handle$backup,
          handle$record$prior_roots,
          .move = .move,
          action = "protect prior release entry"
        )
        if (!isTRUE(protected$ok)) {
          protected_error <- .builder_release_move_error(
            "The prior release entries could not be protected.",
            protected$attempt
          )
          restored <- .builder_release_restore_in_place(
            handle,
            protected_error,
            .move = .move
          )
          if (!isTRUE(restored$restored)) {
            restore_error <- if (
              inherits(
                restored$attempt,
                "native_file_operation_attempt"
              )
            ) {
              paste(
                protected_error,
                .nativeFileOperationMessage(restored$attempt)
              )
            } else {
              protected_error
            }
            stop(
              "The prior release entries could not be protected or restored. ",
              restore_error,
              call. = FALSE
            )
          }
          stop(protected_error, call. = FALSE)
        }
        in_place <- TRUE
      }
    }
    if (!isTRUE(moved) && !isTRUE(in_place)) {
      prior_unchanged <-
        !.builder_release_exists(handle$backup) &&
        identical(
          tryCatch(
            builder_release_state(
              handle$target,
              exact_record = FALSE,
              .digest_cache = .digest_cache
            ),
            error = function(error) NULL
          ),
          handle$expected_prior_state
        )
      if (isTRUE(prior_unchanged)) {
        handle <- .builder_release_write_phase(
          handle,
          "prepared",
          "The prior release could not be protected."
        )
      }
      stop(
        .builder_release_move_error(
          "The prior release could not be protected.",
          old_move
        ),
        call. = FALSE
      )
    }
    .after_move("old_to_backup")
  }
  handle <- .builder_release_write_phase(handle, "old_moved")
  .after_phase("old_moved")
  verify_or_restore(handle$stage, "before_rename")
  install_attempt <- NULL
  if (isTRUE(in_place)) {
    handle <- .builder_release_write_phase(handle, "new_moving_in_place")
    installed <- .builder_release_move_roots(
      handle$stage,
      handle$target,
      handle$record$new_roots,
      .move = .move,
      action = "install prepared release entry"
    )
    moved <- isTRUE(installed$ok)
    install_attempt <- installed$attempt
    if (isTRUE(moved) && !length(.builder_release_root_names(handle$stage))) {
      unlink(handle$stage, recursive = TRUE, force = TRUE)
    }
  } else {
    install_attempt <- .builder_release_move_attempt(
      handle$stage,
      handle$target,
      action = "install prepared release",
      .move = .move
    )
    moved <- isTRUE(install_attempt$ok)
  }
  if (!isTRUE(moved)) {
    publication_error <- .builder_release_move_error(
      "Publication failed.",
      install_attempt
    )
    restored <- .builder_release_restore(
      handle,
      paste0(publication_error, " Preserved backup: ", handle$backup),
      .move = .move
    )
    if (!isTRUE(restored$restored)) {
      restore_diagnostic <- if (
        inherits(
          restored$attempt,
          "native_file_operation_attempt"
        )
      ) {
        paste(publication_error, .nativeFileOperationMessage(restored$attempt))
      } else {
        publication_error
      }
      stop(
        "Publication and restoration failed. Preserved backup: ",
        handle$backup,
        ". ",
        restore_diagnostic,
        call. = FALSE
      )
    }
    stop(
      "Publication failed; the prior release was restored. ",
      .nativeFileOperationMessage(install_attempt),
      call. = FALSE
    )
  }
  .after_move("new_to_target")
  verify_or_restore(
    handle$target,
    if (isTRUE(in_place)) "after_in_place" else "after_rename"
  )
  published_identity <- builder_release_identity(
    handle$target,
    .digest_cache = .digest_cache
  )
  handle <- .builder_release_write_phase(handle, "new_published")
  .after_phase("new_published")
  handle <- .builder_release_write_phase(handle, "complete")
  .after_phase("complete")
  warning <- NULL
  retired_backup <- NULL
  if (dir.exists(handle$backup)) {
    candidate <- file.path(
      .builder_release_atomic_directory(handle$control),
      paste0(".b-", .builder_release_token("a"))
    )
    retire_attempt <- if (!.builder_release_exists(candidate)) {
      .builder_release_move_attempt(
        handle$backup,
        candidate,
        action = "retire prior release backup"
      )
    } else {
      NULL
    }
    if (is.list(retire_attempt) && isTRUE(retire_attempt$ok)) {
      retired_backup <- candidate
    } else {
      warning <- paste0(
        "The release was published, but its prior backup remains: ",
        handle$backup,
        if (inherits(retire_attempt, "native_file_operation_attempt")) {
          paste0(". ", .nativeFileOperationMessage(retire_attempt))
        } else {
          ". The retirement destination already exists."
        }
      )
    }
  }
  if (!is.null(retired_backup)) {
    unlink(retired_backup, recursive = TRUE, force = TRUE)
    if (dir.exists(retired_backup)) {
      warning <- paste0(
        "The release was published, but its prior backup remains: ",
        retired_backup
      )
    }
  }
  terminal_cleanup <- .builder_release_retire_terminal_control(
    handle$target,
    handle$control,
    handle$lock,
    handle$token,
    "complete"
  )
  if (.builder_release_text(terminal_cleanup$warning)) {
    warnings <- c(warning, terminal_cleanup$warning)
    warning <- paste(
      warnings[vapply(warnings, .builder_release_text, logical(1))],
      collapse = " "
    )
  }
  list(
    error = NULL,
    published = TRUE,
    target = handle$target,
    identity = published_identity,
    journal = if (isTRUE(terminal_cleanup$cleaned)) NULL else handle$journal,
    publication_mode = handle$record$publication_mode %||% "rename",
    control_cleaned = isTRUE(terminal_cleanup$cleaned),
    warning = warning
  )
}

builder_abort_release <- function(handle) {
  handle <- .builder_release_handle(handle)
  if (!handle$record$phase %in% c("prepared", "locked")) {
    stop("This release requires recovery and cannot be aborted.", call. = FALSE)
  }
  if (dir.exists(handle$stage)) {
    .builder_release_remove_stage(handle$stage)
  }
  handle <- .builder_release_write_phase(handle, "aborted")
  terminal_cleanup <- .builder_release_retire_terminal_control(
    handle$target,
    handle$control,
    handle$lock,
    handle$token,
    "aborted"
  )
  list(
    aborted = TRUE,
    target = handle$target,
    control_cleaned = isTRUE(terminal_cleanup$cleaned),
    warning = terminal_cleanup$warning
  )
}

.builder_release_pid_alive <- function(pid) {
  if (identical(.Platform$OS.type, "windows")) {
    if (!requireNamespace("ps", quietly = TRUE)) {
      return(TRUE)
    }
    return(isTRUE(tryCatch(
      ps::ps_is_running(ps::ps_handle(as.integer(pid))),
      error = function(error) {
        if (inherits(error, "no_such_process")) FALSE else TRUE
      }
    )))
  }
  isTRUE(tryCatch(tools::pskill(pid, signal = 0L), error = function(error) {
    FALSE
  }))
}

.builder_release_isolate_stale_lock <- function(control, journal) {
  lock <- file.path(control, "lock")
  owner <- .builder_release_lock_owner(lock)
  recoverable <- identical(owner$token, journal$token) &&
    identical(owner$host, .builder_release_host()) &&
    !.builder_release_pid_alive(owner$pid) &&
    .builder_release_lock_known(lock)
  if (!recoverable) {
    stop(
      "The release lock is not safely recoverable; manual inspection is required.",
      call. = FALSE
    )
  }
  isolated <- file.path(
    .builder_release_atomic_directory(control),
    paste0(".s-", .builder_release_token("a"))
  )
  if (.builder_release_exists(isolated) || !file.rename(lock, isolated)) {
    stop("The stale release lock could not be isolated.", call. = FALSE)
  }
  unlink(isolated, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

builder_recover_release <- function(target, action = c("restore", "abort")) {
  action <- match.arg(action)
  recovery <- builder_discover_recovery(target)
  if (!identical(recovery$state, "recovery_required")) {
    return(list(recovered = FALSE, state = recovery$state))
  }
  journal <- recovery$journal
  control <- recovery$control
  lock <- file.path(control, "lock")
  if (dir.exists(lock)) {
    .builder_release_isolate_stale_lock(control, journal)
  }
  token <- .builder_release_token("recovery")
  recovery_lock <- .builder_release_acquire_lock(control, token)
  on.exit(
    {
      if (dir.exists(recovery_lock)) {
        try(
          .builder_release_release_lock(control, recovery_lock, token),
          silent = TRUE
        )
      }
    },
    add = TRUE
  )
  journal$recovery_from <- if (
    startsWith(journal$phase, "recovering_") &&
      .builder_release_text(journal$recovery_from)
  ) {
    journal$recovery_from
  } else {
    journal$phase
  }
  journal$phase <- paste0("recovering_", action)
  journal$detail <- paste0("Recovery action started: ", action)
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$token <- token
  journal$host <- .builder_release_host()
  journal$pid <- as.integer(Sys.getpid())
  journal$lock <- recovery_lock
  .builder_release_atomic_rds(
    journal,
    .builder_release_journal_path(control),
    token,
    control
  )
  target <- recovery$target
  backup <- recovery$backup
  in_place <- identical(journal$publication_mode, "in_place")
  if (identical(action, "restore") && in_place) {
    already_restored <-
      .builder_release_exists(target) &&
      identical(builder_release_identity(target), journal$expected_prior)
    if (!already_restored) {
      recovery_handle <- structure(
        c(journal, list(record = journal)),
        class = c("builder_release_handle", "list")
      )
      restored <- .builder_release_restore_in_place(
        recovery_handle,
        "The interrupted in-place release could not be restored."
      )
      if (!isTRUE(restored$restored)) {
        stop(
          "The interrupted in-place release could not be restored.",
          call. = FALSE
        )
      }
      journal <- restored$handle$record
    } else if (
      dir.exists(backup) &&
        !length(.builder_release_root_names(backup))
    ) {
      unlink(backup, recursive = TRUE, force = TRUE)
    }
  } else if (identical(action, "restore")) {
    if (!dir.exists(backup)) {
      already_restored <-
        .builder_release_exists(target) &&
        identical(
          builder_release_identity(target),
          journal$expected_prior
        )
      if (!already_restored) {
        stop("The preserved release backup is missing.", call. = FALSE)
      }
    }
    if (dir.exists(backup) && .builder_release_exists(target)) {
      published_identity <- journal$prepared_identity
      owned_published <-
        journal$recovery_from %in%
        c("old_moved", "new_published") &&
        .builder_release_identity_valid(published_identity) &&
        identical(builder_release_identity(target), published_identity) &&
        !.builder_release_exists(journal$stage)
      if (!owned_published || !file.rename(target, journal$stage)) {
        stop(
          "The release target is occupied; the backup was preserved.",
          call. = FALSE
        )
      }
    }
    if (dir.exists(backup) && !file.rename(backup, target)) {
      stop("The preserved release backup could not be restored.", call. = FALSE)
    }
  } else {
    if (dir.exists(backup)) {
      stop(
        "A preserved prior release cannot be discarded by abort.",
        call. = FALSE
      )
    }
    current_identity <- builder_release_identity(target)
    empty_identity <- function(identity) {
      .builder_release_identity_valid(identity) && !length(identity$entries)
    }
    if (
      !identical(current_identity, journal$expected_prior) &&
        !(empty_identity(current_identity) &&
          empty_identity(journal$expected_prior))
    ) {
      stop(
        "The release target changed; the interrupted release cannot be cleared automatically.",
        call. = FALSE
      )
    }
  }
  stage <- journal$stage
  if (dir.exists(stage) && .pathWithin(stage, file.path(control, "stages"))) {
    .builder_release_remove_stage(stage)
  }
  journal$phase <- "recovered"
  journal$detail <- paste0("Recovery action: ", action)
  journal$recovery_from <- NULL
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$token <- token
  journal$lock <- recovery_lock
  .builder_release_atomic_rds(
    journal,
    .builder_release_journal_path(control),
    token,
    control
  )
  terminal_cleanup <- .builder_release_retire_terminal_control(
    target,
    control,
    recovery_lock,
    token,
    "recovered"
  )
  list(
    recovered = TRUE,
    action = action,
    target = target,
    control_cleaned = isTRUE(terminal_cleanup$cleaned),
    warning = terminal_cleanup$warning
  )
}
