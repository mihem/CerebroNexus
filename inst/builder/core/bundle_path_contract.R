# Portable bundle paths ----------------------------------------------------

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(left, right) if (is.null(left)) right else left
}

.windowsPathSegmentInvalid <- function(parts) {
  grepl("[[:cntrl:]<>:\"|?*]", parts) |
    grepl("[. ]$", parts) |
    grepl(
      "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])($|\\.)",
      parts,
      ignore.case = TRUE
    )
}

.portableBundlePath <- function(path, subject) {
  valid <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    !grepl("/$", path) &&
    !grepl("\\\\", path) &&
    !grepl("^(/|~|[A-Za-z]:)", path)
  parts <- if (valid) {
    strsplit(path, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  windows_invalid <- length(parts) > 0L &&
    any(.windowsPathSegmentInvalid(parts))
  if (
    !valid ||
      length(parts) == 0L ||
      any(!nzchar(parts)) ||
      any(parts %in% c(".", "..")) ||
      windows_invalid
  ) {
    stop(
      subject,
      " must be one portable relative path using forward slashes, without ",
      "empty, '.', '..', or Windows-incompatible segments.",
      call. = FALSE
    )
  }
  paste(parts, collapse = "/")
}

# Native path resolution ---------------------------------------------------

.nativeFileConditionFields <- function(condition) {
  if (!is.list(condition)) {
    return(character())
  }
  fields <- condition[setdiff(names(condition), c("message", "call"))]
  values <- vapply(
    names(fields),
    function(name) {
      value <- fields[[name]]
      if (
        length(value) != 1L ||
          (!is.atomic(value) && !is.factor(value)) ||
          is.na(value)
      ) {
        return(NA_character_)
      }
      paste0(name, "=", as.character(value))
    },
    character(1)
  )
  values[!is.na(values) & nzchar(values)]
}

.nativeFileOperationAttempt <- function(
  action,
  source,
  destination = NULL,
  operation
) {
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(trimws(value))
  }
  if (
    !scalar_text(action) ||
      !scalar_text(source) ||
      (!is.null(destination) && !scalar_text(destination)) ||
      !is.function(operation)
  ) {
    stop("A valid native file operation is required.", call. = FALSE)
  }
  label <- function(path) {
    if (is.null(path)) {
      return(NULL)
    }
    normalized <- tryCatch(
      suppressWarnings(normalizePath(path, winslash = "/", mustWork = FALSE)),
      error = function(error) NULL
    )
    if (scalar_text(normalized)) normalized else as.character(path)
  }
  warnings <- list()
  failure <- NULL
  value <- tryCatch(
    withCallingHandlers(
      operation(),
      warning = function(warning) {
        warnings[[length(warnings) + 1L]] <<- list(
          message = conditionMessage(warning),
          class = class(warning),
          fields = .nativeFileConditionFields(warning)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) {
      failure <<- list(
        message = conditionMessage(error),
        class = class(error),
        fields = .nativeFileConditionFields(error)
      )
      FALSE
    }
  )
  structure(
    list(
      ok = isTRUE(value),
      action = action,
      source = label(source),
      destination = label(destination),
      platform = if (identical(.Platform$OS.type, "windows")) {
        "Windows"
      } else {
        as.character(.Platform$OS.type)
      },
      warnings = warnings,
      error = failure,
      result = if (length(value) == 1L && is.atomic(value)) value else NULL
    ),
    class = c("native_file_operation_attempt", "list")
  )
}

.nativeFileOperationMessage <- function(attempt) {
  if (!inherits(attempt, "native_file_operation_attempt")) {
    stop("A native file operation attempt is required.", call. = FALSE)
  }
  condition_text <- function(condition) {
    fields <- if (is.null(condition$fields)) character() else condition$fields
    paste(c(condition$message, fields), collapse = " | ")
  }
  warning_text <- if (length(attempt$warnings)) {
    paste(
      vapply(attempt$warnings, condition_text, character(1)),
      collapse = " | "
    )
  } else {
    "none reported"
  }
  error_text <- if (is.list(attempt$error)) {
    condition_text(attempt$error)
  } else if (!isTRUE(attempt$ok)) {
    result <- if (is.null(attempt$result)) FALSE else attempt$result
    paste0("operation returned ", as.character(result))
  } else {
    "none reported"
  }
  destination <- if (is.null(attempt$destination)) {
    ""
  } else {
    paste0(
      "; destination=",
      encodeString(attempt$destination, quote = '"')
    )
  }
  attempts <- suppressWarnings(as.integer(attempt$attempt_count %||% 1L)[1L])
  attempt_text <- if (is.finite(attempts) && attempts > 1L) {
    paste0("; attempts=", attempts)
  } else {
    ""
  }
  paste0(
    "Native file operation: action=",
    encodeString(attempt$action, quote = '"'),
    "; source=",
    encodeString(attempt$source, quote = '"'),
    destination,
    "; platform=",
    attempt$platform,
    attempt_text,
    "; warnings=",
    warning_text,
    "; error=",
    error_text,
    "."
  )
}

.absolutePathStyle <- function(path, os_type = .Platform$OS.type) {
  slash_path <- gsub("\\", "/", path, fixed = TRUE)
  if (grepl("^//[?.]/", slash_path)) {
    return("windows-device")
  }
  if (grepl("^[A-Za-z]:/", slash_path)) {
    return("drive")
  }
  if (startsWith(slash_path, "//")) {
    if (grepl("^//[^/]+/[^/]+($|/)", slash_path)) {
      return("unc")
    }
    if (identical(os_type, "windows")) {
      return("windows-malformed")
    }
    return(NA_character_)
  }
  if (identical(os_type, "windows") && startsWith(slash_path, "/")) {
    return("windows-root")
  }
  if (startsWith(path, "/")) {
    return("posix")
  }
  NA_character_
}

.nativeAbsolutePath <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  if (is.na(style)) {
    return(FALSE)
  }
  if (identical(os_type, "windows")) {
    return(TRUE)
  }
  identical(style, "posix")
}

.nativeFilesystemStyle <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  if (
    !identical(os_type, "windows") &&
      startsWith(path, "/")
  ) {
    return("posix")
  }
  style
}

## Windows device, extended-length and malformed paths fail closed.
.rejectWindowsNamespacePath <- function(style) {
  if (style %in% c("windows-device", "windows-malformed")) {
    stop(
      "Windows ",
      if (identical(style, "windows-device")) {
        "device or extended-length"
      } else {
        "malformed Windows"
      },
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
}

.lexicalAbsolutePath <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  parsed <- .absolutePathComponents(path, style, os_type = os_type)
  normalized <- character()
  for (part in parsed$parts) {
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(normalized) > 0L) {
        normalized <- normalized[-length(normalized)]
      }
      next
    }
    normalized <- c(normalized, part)
  }
  .composeAbsolutePath(parsed$root, normalized)
}

.absolutePathComponents <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  slash_path <- if (
    identical(style, "posix") &&
      !identical(os_type, "windows")
  ) {
    path
  } else {
    gsub("\\", "/", path, fixed = TRUE)
  }
  if (identical(style, "drive")) {
    root <- paste0(substr(slash_path, 1L, 2L), "/")
    remainder <- sub("^[A-Za-z]:/+", "", slash_path)
  } else if (identical(style, "unc")) {
    unc_parts <- strsplit(
      sub("^/+", "", slash_path),
      "/",
      fixed = TRUE
    )[[1L]]
    root <- paste0("//", unc_parts[[1L]], "/", unc_parts[[2L]])
    remainder <- paste(unc_parts[-seq_len(2L)], collapse = "/")
  } else {
    root <- "/"
    remainder <- sub("^/+", "", slash_path)
  }
  parts <- if (nzchar(remainder)) {
    strsplit(remainder, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  list(root = root, parts = parts)
}

.composeAbsolutePath <- function(root, parts) {
  if (length(parts) == 0L) {
    return(root)
  }
  paste0(
    root,
    if (endsWith(root, "/")) "" else "/",
    paste(parts, collapse = "/")
  )
}

.relativeLinkComponents <- function(path, os_type = .Platform$OS.type) {
  if (identical(os_type, "windows")) {
    path <- gsub("\\", "/", path, fixed = TRUE)
  }
  strsplit(path, "/", fixed = TRUE)[[1L]]
}

.nativePathExists <- function(path) {
  file.exists(path) || dir.exists(path)
}

.readNativeLink <- function(
  path,
  os_type = .Platform$OS.type,
  is_link = function(candidate) fs::is_link(candidate),
  link_path = function(candidate) fs::link_path(candidate)
) {
  if (identical(os_type, "windows")) {
    linked <- is_link(path)
    if (length(linked) != 1L || is.na(linked) || !isTRUE(unname(linked))) {
      return("")
    }
    return(as.character(link_path(path))[[1L]])
  }
  Sys.readlink(path)
}

.normalizeExistingPath <- function(
  path,
  os_type = .Platform$OS.type,
  path_real = function(candidate) fs::path_real(candidate)
) {
  if (identical(os_type, "windows")) {
    return(as.character(path_real(path))[[1L]])
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.pathDirectoryEntryExists <- function(
  path,
  os_type = .Platform$OS.type,
  dir_exists = dir.exists,
  access_parent = function(parent) file.access(parent, mode = 5L),
  list_entries = function(parent) {
    list.files(parent, all.files = TRUE, no.. = TRUE)
  }
) {
  parent <- dirname(path)
  if (!isTRUE(dir_exists(parent))) {
    return(FALSE)
  }

  inspection_error <- NULL
  access_status <- tryCatch(
    access_parent(parent),
    error = function(error_condition) {
      inspection_error <<- conditionMessage(error_condition)
      NULL
    }
  )
  if (
    !is.null(inspection_error) ||
      length(access_status) != 1L ||
      is.na(access_status) ||
      access_status != 0L
  ) {
    stop(
      "The path cannot be resolved safely because the parent directory ",
      "could not be inspected.",
      call. = FALSE
    )
  }

  entries <- tryCatch(
    withCallingHandlers(
      list_entries(parent),
      warning = function(warning_condition) {
        inspection_error <<- conditionMessage(warning_condition)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) {
      inspection_error <<- conditionMessage(error_condition)
      NULL
    }
  )
  if (!is.null(inspection_error) || !is.character(entries)) {
    stop(
      "The path cannot be resolved safely because the parent directory ",
      "could not be inspected.",
      call. = FALSE
    )
  }

  needle <- basename(path)
  if (identical(os_type, "windows")) {
    needle <- tolower(needle)
    entries <- tolower(entries)
  }
  any(entries == needle)
}

## Resolve each component so intermediate symbolic links are reflected in the
## canonical path and callers can detect escape from an allowed root.
## Uninspectable filesystem entries fail closed.
.resolveNativeSymbolicLinks <- function(
  path,
  style = .absolutePathStyle(path),
  max_links = 64L,
  os_type = .Platform$OS.type,
  read_link = function(candidate) {
    .readNativeLink(candidate, os_type = os_type)
  },
  path_exists = .nativePathExists,
  normalize_existing = function(candidate) {
    .normalizeExistingPath(candidate, os_type = os_type)
  },
  entry_exists = function(candidate) {
    .pathDirectoryEntryExists(candidate, os_type = os_type)
  }
) {
  .rejectWindowsNamespacePath(style)
  parsed <- .absolutePathComponents(path, style, os_type = os_type)
  if (identical(os_type, "windows") && identical(style, "unc")) {
    unc_root_parts <- strsplit(
      sub("^/+", "", parsed$root),
      "/",
      fixed = TRUE
    )[[1L]]
    if (any(.windowsPathSegmentInvalid(unc_root_parts))) {
      stop(
        "Native Windows paths cannot contain Windows-incompatible segments.",
        call. = FALSE
      )
    }
  }
  root <- parsed$root
  resolved <- character()
  pending <- parsed$parts
  links_followed <- 0L

  if (isTRUE(path_exists(root))) {
    physical_root <- normalize_existing(root)
    physical_style <- .absolutePathStyle(physical_root, os_type = os_type)
    physical_style <- .nativeFilesystemStyle(
      physical_root,
      physical_style,
      os_type = os_type
    )
    physical_root <- .absolutePathComponents(
      physical_root,
      physical_style,
      os_type = os_type
    )
    root <- physical_root$root
    resolved <- physical_root$parts
  }

  while (length(pending) > 0L) {
    part <- pending[[1L]]
    pending <- pending[-1L]
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(resolved) > 0L) {
        resolved <- resolved[-length(resolved)]
      }
      next
    }
    if (
      identical(os_type, "windows") &&
        .windowsPathSegmentInvalid(part)
    ) {
      stop(
        "Native Windows paths cannot contain Windows-incompatible segments.",
        call. = FALSE
      )
    }

    candidate <- .composeAbsolutePath(root, c(resolved, part))
    link <- read_link(candidate)
    if (!is.na(link) && nzchar(link)) {
      links_followed <- links_followed + 1L
      if (links_followed > max_links) {
        stop(
          "Could not resolve the path because it contains too many ",
          "symbolic links.",
          call. = FALSE
        )
      }
      link_style <- .absolutePathStyle(link, os_type = os_type)
      native_absolute <- if (identical(os_type, "windows")) {
        !is.na(link_style)
      } else {
        startsWith(link, "/")
      }
      if (native_absolute) {
        if (!identical(os_type, "windows")) {
          link_style <- "posix"
        }
        link_parts <- .absolutePathComponents(
          link,
          link_style,
          os_type = os_type
        )
        root <- link_parts$root
        resolved <- character()
        pending <- c(link_parts$parts, pending)
      } else {
        pending <- c(
          .relativeLinkComponents(link, os_type = os_type),
          pending
        )
      }
      next
    }

    if (isTRUE(path_exists(candidate))) {
      physical <- normalize_existing(candidate)
      physical_style <- .absolutePathStyle(physical, os_type = os_type)
      physical_style <- .nativeFilesystemStyle(
        physical,
        physical_style,
        os_type = os_type
      )
      if (is.na(physical_style)) {
        stop(
          "The path cannot be resolved safely to an absolute native path.",
          call. = FALSE
        )
      }
      physical <- .absolutePathComponents(
        physical,
        physical_style,
        os_type = os_type
      )
      root <- physical$root
      resolved <- physical$parts
      next
    }
    if (isTRUE(entry_exists(candidate))) {
      stop(
        "The path cannot be resolved safely because it contains an ",
        "unresolved filesystem entry.",
        call. = FALSE
      )
    }
    resolved <- c(resolved, part)
  }

  .composeAbsolutePath(root, resolved)
}

.canonicalTargetPath <- function(path, os_type = .Platform$OS.type) {
  style <- .absolutePathStyle(path, os_type = os_type)
  .rejectWindowsNamespacePath(style)
  style <- .nativeFilesystemStyle(path, style, os_type = os_type)
  if (is.na(style)) {
    path <- file.path(getwd(), path)
    style <- .absolutePathStyle(path, os_type = os_type)
    style <- .nativeFilesystemStyle(path, style, os_type = os_type)
  }
  if (!.nativeAbsolutePath(path, style, os_type = os_type)) {
    return(.lexicalAbsolutePath(path, style, os_type = os_type))
  }
  path <- .resolveNativeSymbolicLinks(
    path,
    style,
    os_type = os_type
  )
  if (.nativePathExists(path)) {
    return(.normalizeExistingPath(path, os_type = os_type))
  }
  .lexicalAbsolutePath(
    path,
    .absolutePathStyle(path, os_type = os_type),
    os_type = os_type
  )
}

.normalizeOverridePath <- function(path, key) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop(
      "cerebro_options[['",
      key,
      "']] must be one non-empty absolute path.",
      call. = FALSE
    )
  }
  style <- .absolutePathStyle(path)
  if (style %in% c("windows-device", "windows-malformed")) {
    detail <- if (identical(style, "windows-device")) {
      "device or extended-length"
    } else {
      "malformed Windows"
    }
    stop(
      "Windows ",
      detail,
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
  if (is.na(style)) {
    stop(
      "cerebro_options[['",
      key,
      "']] must be one absolute path.",
      call. = FALSE
    )
  }
  if (
    !identical(.Platform$OS.type, "windows") &&
      identical(style, "unc") &&
      startsWith(path, "/")
  ) {
    stop(
      "Forward-slash UNC paths are ambiguous outside Windows; use a ",
      "backslash UNC or drive path for a Windows host-managed override.",
      call. = FALSE
    )
  }
  if (!.nativeAbsolutePath(path, style)) {
    return(path)
  }
  .canonicalTargetPath(path)
}

.pathWithin <- function(
  candidate,
  parent,
  os_type = .Platform$OS.type,
  canonicalize = function(path) {
    .canonicalTargetPath(path, os_type = os_type)
  }
) {
  candidate_style <- .absolutePathStyle(candidate, os_type = os_type)
  candidate_style <- .nativeFilesystemStyle(
    candidate,
    candidate_style,
    os_type = os_type
  )
  if (!.nativeAbsolutePath(candidate, candidate_style, os_type = os_type)) {
    return(FALSE)
  }
  candidate <- canonicalize(candidate)
  parent <- canonicalize(parent)
  if (identical(os_type, "windows")) {
    candidate <- tolower(candidate)
    parent <- tolower(parent)
  }
  parent_prefix <- if (endsWith(parent, "/")) {
    parent
  } else {
    paste0(parent, "/")
  }
  identical(candidate, parent) || startsWith(candidate, parent_prefix)
}

# OS-backed entropy for generated application credentials. Never fall back to
# R's seeded statistical PRNG: these bytes can become authentication secrets.
.builderRandomBytes <- function(n) {
  if (
    length(n) != 1L ||
      !is.numeric(n) ||
      is.na(n) ||
      n < 1 ||
      n > 65536 ||
      n != as.integer(n)
  ) {
    stop("The requested random byte count is invalid.", call. = FALSE)
  }
  n <- as.integer(n)
  if (.Platform$OS.type == "windows") {
    command <- sprintf(
      paste0(
        "$ErrorActionPreference='Stop'; $bytes=New-Object byte[] %d; ",
        "$rng=[System.Security.Cryptography.RandomNumberGenerator]::Create(); ",
        "try {$rng.GetBytes($bytes); [BitConverter]::ToString($bytes).Replace('-', '')} ",
        "finally {$rng.Dispose()}"
      ),
      n
    )
    output <- system2(
      "powershell.exe",
      c(
        "-NoLogo",
        "-NoProfile",
        "-NonInteractive",
        "-Command",
        shQuote(command)
      ),
      stdout = TRUE,
      stderr = FALSE
    )
    if (!is.null(attr(output, "status")) || length(output) != 1L) {
      stop("Operating-system secure randomness is unavailable.", call. = FALSE)
    }
    if (
      nchar(output, type = "bytes") != 2L * n ||
        !grepl("^[0-9A-Fa-f]+$", output)
    ) {
      stop("Operating-system secure randomness is invalid.", call. = FALSE)
    }
    starts <- seq.int(1L, 2L * n, by = 2L)
    bytes <- as.raw(strtoi(substring(output, starts, starts + 1L), base = 16L))
  } else {
    connection <- file("/dev/urandom", open = "rb", raw = TRUE)
    on.exit(close(connection), add = TRUE)
    bytes <- readBin(connection, what = "raw", n = n)
  }
  if (!is.raw(bytes) || length(bytes) != n) {
    stop("Operating-system secure randomness is incomplete.", call. = FALSE)
  }
  bytes
}
