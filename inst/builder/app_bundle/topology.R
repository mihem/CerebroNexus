.builder_app_path_exists <- function(path) {
  file.exists(path) || dir.exists(path) || .builder_app_is_link(path)
}

.builder_app_is_link <- function(path) {
  linked <- tryCatch(fs::is_link(path), error = function(error) NA)
  length(linked) != 1L || is.na(linked) || isTRUE(unname(linked))
}

.builder_app_path_within <- function(path, root, must_exist = TRUE) {
  resolved <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = must_exist),
    error = function(error) NULL
  )
  if (is.null(resolved)) {
    return(FALSE)
  }
  identical(resolved, root) || startsWith(resolved, paste0(root, "/"))
}

.builder_app_file_fingerprint <- function(info) {
  if (is.null(info) || nrow(info) != 1L) {
    stop("A staged App entry could not be fingerprinted safely.", call. = FALSE)
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
      fingerprint$device_id < 0 ||
      fingerprint$inode < 0 ||
      fingerprint$hard_links < 1
  ) {
    stop("A staged App entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint
}

## The fingerprints of every row of `info`, checked as .builder_app_file_fingerprint()
## checks one row, from the columns instead of slicing the table once per entry
## (about half the time of an enumeration of a few hundred files).
.builder_app_file_fingerprints <- function(info) {
  if (is.null(info) || !nrow(info)) {
    return(list())
  }
  columns <- list(
    type = as.character(info$type),
    size = as.double(info$size),
    permissions = as.character(info$permissions),
    device_id = as.double(info$device_id),
    inode = as.double(info$inode),
    hard_links = as.double(info$hard_links),
    modification_time = as.double(info$modification_time),
    change_time = as.double(info$change_time)
  )
  numeric_fields <- c(
    "size",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  valid <- all(lengths(columns) == nrow(info)) &&
    !any(vapply(columns, anyNA, TRUE)) &&
    all(vapply(columns[numeric_fields], function(x) all(is.finite(x)), TRUE)) &&
    all(nzchar(columns$type)) &&
    all(nzchar(columns$permissions)) &&
    all(columns$size >= 0) &&
    all(columns$device_id >= 0) &&
    all(columns$inode >= 0) &&
    all(columns$hard_links >= 1)
  if (!valid) {
    stop("A staged App entry could not be fingerprinted safely.", call. = FALSE)
  }
  lapply(seq_len(nrow(info)), function(index) {
    lapply(columns, `[[`, index)
  })
}

.builder_app_list_directory <- function(path) {
  as.character(fs::dir_ls(
    path,
    all = TRUE,
    recurse = FALSE,
    type = "any",
    fail = TRUE
  ))
}

.builder_app_assert_readable_directory <- function(path, info = NULL) {
  if (identical(.Platform$OS.type, "windows")) {
    return(invisible(TRUE))
  }
  mode <- if (is.null(info)) {
    file.info(path)$mode
  } else {
    info$permissions
  }
  mode <- suppressWarnings(as.integer(mode))
  readable_and_searchable <- length(mode) == 1L &&
    !is.na(mode) &&
    any(vapply(
      c(320L, 40L, 5L),
      function(mask) {
        bitwAnd(mode, mask) == mask
      },
      logical(1)
    ))
  if (!readable_and_searchable) {
    stop(
      "A staged App directory could not be enumerated safely.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_enumerate_tree <- function(
  root,
  .list_directory = .builder_app_list_directory
) {
  queue <- list(list(path = root, depth = 0L))
  entries <- character()
  fingerprints <- list()
  snapshots <- list()
  while (length(queue)) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    directory <- current$path
    if (current$depth > 64L) {
      stop(
        "The staged App directory tree is too deep to verify.",
        call. = FALSE
      )
    }
    before <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      is.null(before) ||
        !identical(as.character(before$type), "directory") ||
        .builder_app_is_link(directory)
    ) {
      stop("A staged App directory changed before enumeration.", call. = FALSE)
    }
    .builder_app_assert_readable_directory(directory, before)
    if (
      !identical(.Platform$OS.type, "windows") &&
        file.access(directory, mode = 5L) != 0L
    ) {
      stop(
        "A staged App directory could not be enumerated safely.",
        call. = FALSE
      )
    }
    listed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (is.null(listed)) {
      stop(
        "A staged App directory could not be enumerated safely.",
        call. = FALSE
      )
    }
    after <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      is.null(after) ||
        !identical(
          .builder_app_file_fingerprint(before),
          .builder_app_file_fingerprint(after)
        )
    ) {
      stop("A staged App directory changed during enumeration.", call. = FALSE)
    }
    if (length(listed)) {
      if (
        anyDuplicated(listed) ||
          !all(dirname(listed) == directory) ||
          length(entries) + length(listed) > 100000L
      ) {
        stop("The staged App directory returned unsafe entries.", call. = FALSE)
      }
      info <- tryCatch(
        fs::file_info(listed, fail = TRUE, follow = FALSE),
        error = function(error) NULL
      )
      linked <- tryCatch(fs::is_link(listed), error = function(error) NULL)
      if (
        is.null(info) ||
          nrow(info) != length(listed) ||
          is.null(linked) ||
          length(linked) != length(listed) ||
          anyNA(linked) ||
          any(linked)
      ) {
        stop(
          "The staged App contains a symbolic or unreadable entry.",
          call. = FALSE
        )
      }
      types <- as.character(info$type)
      if (anyNA(types) || any(!types %in% c("file", "directory"))) {
        stop(
          "The staged App contains an unsupported filesystem entry.",
          call. = FALSE
        )
      }
      listed_fingerprints <- .builder_app_file_fingerprints(info)
      directories <- listed[types == "directory"]
      queue <- c(
        queue,
        lapply(directories, function(path) {
          list(path = path, depth = current$depth + 1L)
        })
      )
      entries <- c(entries, listed)
      fingerprints <- c(fingerprints, listed_fingerprints)
    }
    snapshots[[directory]] <- list(
      entries = listed,
      fingerprint = .builder_app_file_fingerprint(after)
    )
  }
  for (directory in names(snapshots)) {
    confirmed_directory <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    confirmed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (
      is.null(confirmed_directory) ||
        .builder_app_is_link(directory) ||
        !identical(
          .builder_app_file_fingerprint(confirmed_directory),
          snapshots[[directory]]$fingerprint
        ) ||
        is.null(confirmed) ||
        !identical(confirmed, snapshots[[directory]]$entries)
    ) {
      stop("A staged App directory changed after enumeration.", call. = FALSE)
    }
  }
  if (length(entries)) {
    confirmed <- tryCatch(
      fs::file_info(entries, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (is.null(confirmed) || nrow(confirmed) != length(entries)) {
      stop("Staged App entries changed after enumeration.", call. = FALSE)
    }
    confirmed_fingerprints <- .builder_app_file_fingerprints(confirmed)
    if (
      !all(mapply(
        identical,
        fingerprints,
        confirmed_fingerprints,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      ))
    ) {
      stop("Staged App entries changed after enumeration.", call. = FALSE)
    }
  }
  list(paths = entries, fingerprints = fingerprints)
}

.builder_app_tree_identity <- function(
  root,
  .digest_file = tools::md5sum,
  .previous = NULL
) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  enumerated <- .builder_app_enumerate_tree(root)
  relative <- if (length(enumerated$paths)) {
    substring(enumerated$paths, nchar(root) + 2L)
  } else {
    character()
  }
  order <- order(relative, method = "radix")
  paths <- enumerated$paths[order]
  relative <- relative[order]
  fingerprints <- enumerated$fingerprints[order]
  entries <- lapply(seq_along(paths), function(index) {
    fingerprint <- fingerprints[[index]]
    if (identical(fingerprint$type, "file")) {
      cached <- if (is.list(.previous) && is.list(.previous$entries)) {
        .previous$entries[[relative[[index]]]]
      } else {
        NULL
      }
      if (
        is.list(cached) &&
          identical(cached$path, relative[[index]]) &&
          identical(cached[names(fingerprint)], fingerprint) &&
          is.character(cached$md5) &&
          length(cached$md5) == 1L &&
          !is.na(cached$md5) &&
          grepl("^[[:xdigit:]]{32}$", cached$md5)
      ) {
        return(cached)
      }
      identity <- .builder_app_capture_file_identity(
        paths[[index]],
        .digest_file = .digest_file
      )
      return(list(
        path = relative[[index]],
        type = "file",
        size = identity$size,
        modification_time = identity$modification_time,
        change_time = identity$change_time,
        device_id = identity$device_id,
        inode = identity$inode,
        hard_links = identity$hard_links,
        permissions = identity$permissions,
        md5 = identity$md5
      ))
    }
    list(
      path = relative[[index]],
      type = "directory",
      fingerprint = fingerprint
    )
  })
  names(entries) <- relative
  root_info <- tryCatch(
    fs::file_info(root, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  list(
    schema_version = 1L,
    root_fingerprint = .builder_app_file_fingerprint(root_info),
    entries = entries
  )
}

.builder_app_portable_tree_entries <- function(identity, prefix = NULL) {
  entries <- identity$entries
  if (!is.null(prefix)) {
    child_prefix <- paste0(prefix, "/")
    selected <- startsWith(names(entries), child_prefix)
    entries <- entries[selected]
    names(entries) <- substring(names(entries), nchar(child_prefix) + 1L)
  }
  portable <- lapply(entries, function(entry) {
    if (identical(entry$type, "directory")) {
      return(list(path = entry$path, type = "directory"))
    }
    .builder_app_portable_file(
      entry$path,
      entry$size,
      entry$md5
    )
  })
  if (!is.null(prefix)) {
    portable <- lapply(seq_along(portable), function(index) {
      entry <- portable[[index]]
      entry$path <- names(portable)[[index]]
      entry
    })
    names(portable) <- names(entries)
  }
  portable
}

.builder_app_without_system_metadata <- function(identity) {
  paths <- names(identity$entries)
  names_only <- basename(paths)
  keep <- names_only != ".DS_Store" & !startsWith(names_only, "._")
  identity$entries <- identity$entries[keep]
  identity
}

.builder_app_package_path <- function(...) {
  source_root <- Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = "")
  source_runtime <- nzchar(source_root) &&
    file.exists(file.path(source_root, "DESCRIPTION")) &&
    dir.exists(file.path(source_root, "R")) &&
    length(list.files(
      file.path(source_root, "R"),
      pattern = "[.][Rr]$"
    )) >
      0L
  path <- if (source_runtime) {
    file.path(source_root, "inst", ...)
  } else {
    system.file(..., package = "CerebroNexus")
  }
  if (!nzchar(path) || !.builder_app_path_exists(path)) {
    stop("A package-owned trusted template is missing.", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.builder_app_assert_root_topology <- function(
  identity,
  spatial_images,
  auth_enabled = FALSE
) {
  paths <- names(identity$entries)
  root_paths <- paths[!grepl("/", paths, fixed = TRUE)]
  expected <- c(
    "app.R" = "file",
    "cerebro_config.rds" = "file",
    "extdata" = "directory",
    "private-data" = "directory"
  )
  if (length(spatial_images)) {
    expected <- c(expected, "spatial-assets" = "directory")
  }
  expected <- c(expected, "viewer" = "directory")
  if (isTRUE(auth_enabled)) {
    expected <- c(expected, "viewer-auth.env" = "file")
  }
  actual <- vapply(
    identity$entries[root_paths],
    `[[`,
    character(1),
    "type"
  )
  if (!identical(actual, expected)) {
    stop(
      "The staged App root differs from its trusted topology.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_assert_spatial_topology <- function(
  identity,
  request,
  spatial_images
) {
  entries <- identity$entries
  spatial_entries <- entries[
    names(entries) == "spatial-assets" |
      startsWith(names(entries), "spatial-assets/")
  ]
  if (!length(spatial_images)) {
    if (length(spatial_entries)) {
      stop(
        "The staged App contains undeclared spatial assets.",
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }
  files <- character()
  expected_content <- list()
  for (dataset in names(spatial_images)) {
    for (section in names(spatial_images[[dataset]])) {
      for (label in names(spatial_images[[dataset]][[section]])) {
        descriptor <- spatial_images[[dataset]][[section]][[label]]
        path <- if (is.character(descriptor)) descriptor else descriptor$path
        files <- c(files, path)
      }
    }
  }
  for (dataset in names(request$spatial_images)) {
    for (section in names(request$spatial_images[[dataset]])) {
      for (label in names(request$spatial_images[[dataset]][[section]])) {
        descriptor <- request$spatial_images[[dataset]][[section]][[label]]
        target <- .builder_app_spatial_target(
          dataset,
          section,
          label,
          descriptor$path
        )
        source <- request$spatial_image_identities[[dataset]][[section]][[
          label
        ]]
        expected_content[[target]] <- .builder_app_portable_file(
          target,
          source$size,
          source$md5
        )
      }
    }
  }
  directories <- unique(unlist(
    lapply(files, function(path) {
      components <- strsplit(dirname(path), "/", fixed = TRUE)[[1L]]
      vapply(
        seq_along(components),
        function(index) {
          paste(components[seq_len(index)], collapse = "/")
        },
        character(1)
      )
    }),
    use.names = FALSE
  ))
  expected_paths <- c(directories, files)
  if (
    anyDuplicated(files) ||
      !setequal(names(spatial_entries), expected_paths)
  ) {
    stop(
      "The staged App spatial-assets topology differs from its manifest.",
      call. = FALSE
    )
  }
  directory_types <- vapply(
    entries[directories],
    `[[`,
    character(1),
    "type"
  )
  actual_content <- lapply(names(expected_content), function(path) {
    entry <- entries[[path]]
    if (!is.list(entry) || !identical(entry$type, "file")) {
      return(NULL)
    }
    .builder_app_portable_file(path, entry$size, entry$md5)
  })
  names(actual_content) <- names(expected_content)
  if (
    any(directory_types != "directory") ||
      !identical(actual_content, expected_content)
  ) {
    stop(
      "The staged App spatial assets differ from frozen input.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_assert_trusted_templates <- function(identity) {
  app_template <- .builder_app_package_path(
    "viewer",
    "_bundle_app.R"
  )
  expected_app <- .builder_app_capture_file_identity(app_template)
  actual_app <- identity$entries[["app.R"]]
  if (
    is.null(actual_app) ||
      !identical(actual_app$type, "file") ||
      !identical(
        .builder_app_portable_file(
          "app.R",
          actual_app$size,
          actual_app$md5
        ),
        .builder_app_portable_file(
          "app.R",
          expected_app$size,
          expected_app$md5
        )
      )
  ) {
    stop("The staged App differs from its trusted template.", call. = FALSE)
  }

  trusted_roots <- list(
    "viewer" = .builder_app_package_path("viewer"),
    extdata = .builder_app_package_path("extdata")
  )
  for (relative_root in names(trusted_roots)) {
    root_entry <- identity$entries[[relative_root]]
    if (is.null(root_entry) || !identical(root_entry$type, "directory")) {
      stop("The staged App differs from its trusted template.", call. = FALSE)
    }
    expected <- .builder_app_portable_tree_entries(
      .builder_app_without_system_metadata(
        .builder_app_tree_identity(trusted_roots[[relative_root]])
      )
    )
    if (identical(relative_root, "extdata")) {
      expected <- expected[
        !grepl("/", names(expected), fixed = TRUE) &
          vapply(expected, `[[`, character(1), "type") == "file"
      ]
    }
    actual <- .builder_app_portable_tree_entries(
      .builder_app_without_system_metadata(identity),
      relative_root
    )
    if (identical(relative_root, "viewer")) {
      ## createShinyApp() writes the HLA core into the bundle from the
      ## package R/ directory; it is not part of the Viewer template.
      core <- actual[["hla_tcr_motifs/core/hla_core.R"]]
      if (!is.null(core) && !identical(core$type, "file")) {
        stop("The staged App differs from its trusted template.", call. = FALSE)
      }
      actual <- actual[
        !names(actual) %in%
          c("hla_tcr_motifs/core", "hla_tcr_motifs/core/hla_core.R")
      ]
    }
    if (!identical(actual, expected)) {
      stop("The staged App differs from its trusted template.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.builder_app_output_content_identities <- function(identity, request) {
  relative_crbs <- names(request$backend_plan$entries)
  content <- lapply(relative_crbs, function(relative_crb) {
    crb_entry <- identity$entries[[relative_crb]]
    crb <- if (is.list(crb_entry) && identical(crb_entry$type, "file")) {
      .builder_app_portable_file(
        relative_crb,
        crb_entry$size,
        crb_entry$md5
      )
    } else {
      NULL
    }
    plan_entry <- request$backend_plan$entries[[relative_crb]]
    backend <- if (identical(plan_entry$type, "embedded")) {
      list(type = "embedded", root = NULL, entries = list())
    } else {
      root <- paste0("private-data/", plan_entry$location)
      if (identical(plan_entry$type, "h5")) {
        entry <- identity$entries[[root]]
        entries <- if (is.list(entry) && identical(entry$type, "file")) {
          stats::setNames(
            list(.builder_app_portable_file(
              root,
              entry$size,
              entry$md5
            )),
            root
          )
        } else {
          list()
        }
      } else {
        prefix <- paste0(root, "/")
        paths <- names(identity$entries)
        paths <- paths[startsWith(paths, prefix)]
        entries <- lapply(paths, function(path) {
          entry <- identity$entries[[path]]
          if (identical(entry$type, "directory")) {
            return(list(path = path, type = "directory"))
          }
          .builder_app_portable_file(path, entry$size, entry$md5)
        })
        names(entries) <- paths
      }
      list(type = plan_entry$type, root = root, entries = entries)
    }
    spatial_plan <- request$spatial_molecule_identities[[relative_crb]]
    spatial_molecules <- if (is.null(spatial_plan)) {
      list(type = "none", root = NULL, entries = list())
    } else {
      root <- paste0("private-data/", basename(spatial_plan$root))
      prefix <- paste0(root, "/")
      paths <- names(identity$entries)
      paths <- paths[startsWith(paths, prefix)]
      entries <- lapply(paths, function(path) {
        entry <- identity$entries[[path]]
        if (identical(entry$type, "directory")) {
          return(list(path = path, type = "directory"))
        }
        .builder_app_portable_file(path, entry$size, entry$md5)
      })
      names(entries) <- paths
      list(type = "directory", root = root, entries = entries)
    }
    list(
      crb = crb,
      backend = backend,
      spatial_molecules = spatial_molecules
    )
  })
  names(content) <- relative_crbs
  content
}

.builder_app_assert_private_topology <- function(identity, request) {
  expected <- character()
  for (content in request$content_identities) {
    expected[[content$crb$path]] <- "file"
    backend <- content$backend
    if (identical(backend$type, "directory")) {
      expected[[backend$root]] <- "directory"
    }
    for (entry in backend$entries) {
      expected[[entry$path]] <- entry$type
    }
    spatial_molecules <- content$spatial_molecules
    if (identical(spatial_molecules$type, "directory")) {
      expected[[spatial_molecules$root]] <- "directory"
    }
    for (entry in spatial_molecules$entries) {
      expected[[entry$path]] <- entry$type
    }
  }
  if (isTRUE(request$auth$enabled)) {
    expected[["private-data/auth"]] <- "directory"
    expected[["private-data/auth/credentials.sqlite"]] <- "file"
  }
  expected <- expected[order(names(expected), method = "radix")]
  paths <- names(identity$entries)
  private_paths <- paths[startsWith(paths, "private-data/")]
  actual <- vapply(
    identity$entries[private_paths],
    `[[`,
    character(1),
    "type"
  )
  if (!identical(actual, expected)) {
    stop(
      "The staged App copied content has an unexpected private topology.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_tree_summary <- function(identity) {
  portable <- .builder_app_portable_tree_entries(identity)
  digest_file <- tempfile("builder-app-tree-", fileext = ".rds")
  on.exit(unlink(digest_file), add = TRUE)
  saveRDS(portable, digest_file, version = 3)
  types <- vapply(portable, `[[`, character(1), "type")
  list(
    schema_version = 1L,
    entry_count = length(portable),
    file_count = sum(types == "file"),
    directory_count = sum(types == "directory"),
    aggregate_md5 = unname(tolower(as.character(tools::md5sum(digest_file))))
  )
}

.builder_app_real_path <- function(path) {
  resolved <- tryCatch(
    as.character(fs::path_real(path)),
    error = function(error) NULL
  )
  if (
    is.null(resolved) ||
      length(resolved) != 1L ||
      is.na(resolved) ||
      !nzchar(resolved)
  ) {
    stop("A staged App entry has no canonical path.", call. = FALSE)
  }
  resolved
}

.builder_app_validate_private_locations <- function(
  identity,
  root,
  .canonical_path = .builder_app_real_path
) {
  paths <- names(identity$entries)
  if (!length(paths)) {
    return(invisible(TRUE))
  }
  lower <- tolower(paths)
  private_root <- .canonical_path(file.path(root, "private-data"))
  canonical_entries <- vapply(
    paths,
    function(path) .canonical_path(file.path(root, path)),
    character(1)
  )
  private <- canonical_entries == private_root |
    startsWith(canonical_entries, paste0(private_root, "/"))
  data_entry <-
    grepl("[.](crb|h5|hdf5|rds)$", lower)
  data_entry[paths == "cerebro_config.rds"] <- FALSE
  expected_demo_hash <- unname(.builder_app_demo_data[paths])
  allowed_demo <-
    !is.na(expected_demo_hash) &
    vapply(
      identity$entries,
      function(entry) identical(entry$type, "file"),
      logical(1)
    ) &
    vapply(
      seq_along(identity$entries),
      function(index) {
        identical(
          identity$entries[[index]]$md5,
          expected_demo_hash[[index]]
        )
      },
      logical(1)
    )
  if (any(data_entry & !private & !allowed_demo)) {
    stop(
      "The staged App contains private data outside private-data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_safe_relative <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      grepl("\\", path, fixed = TRUE) ||
      startsWith(path, "/") ||
      grepl("^[A-Za-z]:", path)
  ) {
    return(FALSE)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  all(nzchar(parts)) && !any(parts %in% c(".", ".."))
}
