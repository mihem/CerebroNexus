## A Builder project is one small file, `builder-project.rds`, in a folder the
## user chooses. It records what the user chose for each dataset (settings,
## acknowledgements, spatial alignment drafts) and where its source file is.
## It does not copy data, build caches or keep backups: opening a project loads
## the sources again from their paths, so a source that was uploaded through the
## browser, or that has moved, has to be added again.

.builder_project_manifest_name <- "builder-project.rds"
.builder_project_manifest_format <- "cerebronexus-builder-project"
.builder_project_schema_version <- 1L
.builder_project_configuration_contract_version <- 3L
.builder_project_manifest_max_bytes <- 64L * 1024L^2

.builder_project_phases <- c(
  "none",
  "clean",
  "dirty",
  "choosing",
  "saving",
  "autosaving",
  "opening",
  "restoring",
  "registering",
  "save_failed",
  "conflict"
)

builder_project_legacy_qs_datasets <- function(entries) {
  entries <- Filter(is.list, entries %||% list())
  legacy <- Filter(
    function(entry) {
      format <- as.character(entry$format %||% "")
      length(format) == 1L &&
        !is.na(format) &&
        identical(tolower(trimws(format)), "qs")
    },
    entries
  )
  lapply(legacy, function(entry) {
    id <- as.character(entry$id %||% "")
    if (length(id) != 1L || is.na(id)) {
      id <- ""
    }
    name <- as.character(
      entry$settings$name %||% entry$name %||% entry$filename %||% id
    )
    if (length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
      name <- id
    }
    list(id = id, name = name)
  })
}

builder_activity_state <- function(
  connection = "connected",
  client_imports = 0L,
  server_imports = FALSE,
  project_phase = "none",
  spatial_dirty = FALSE,
  source_syncing = FALSE,
  crb_generating = FALSE,
  build_locked = FALSE,
  has_project = FALSE,
  has_datasets = FALSE
) {
  if (
    !identical(connection, "connected") &&
      !identical(connection, "disconnected")
  ) {
    stop("A valid Builder connection state is required.", call. = FALSE)
  }
  client_imports <- suppressWarnings(as.integer(client_imports))
  if (
    length(client_imports) != 1L || is.na(client_imports) || client_imports < 0L
  ) {
    stop("A valid client import count is required.", call. = FALSE)
  }
  if (
    !is.logical(server_imports) ||
      length(server_imports) != 1L ||
      is.na(server_imports) ||
      !is.character(project_phase) ||
      length(project_phase) != 1L ||
      is.na(project_phase) ||
      !project_phase %in% .builder_project_phases ||
      !is.logical(spatial_dirty) ||
      length(spatial_dirty) != 1L ||
      is.na(spatial_dirty) ||
      !is.logical(source_syncing) ||
      length(source_syncing) != 1L ||
      is.na(source_syncing) ||
      !is.logical(crb_generating) ||
      length(crb_generating) != 1L ||
      is.na(crb_generating) ||
      !is.logical(build_locked) ||
      length(build_locked) != 1L ||
      is.na(build_locked) ||
      !is.logical(has_project) ||
      length(has_project) != 1L ||
      is.na(has_project) ||
      !is.logical(has_datasets) ||
      length(has_datasets) != 1L ||
      is.na(has_datasets)
  ) {
    stop("A valid Builder activity state is required.", call. = FALSE)
  }
  structure(
    list(
      connection = connection,
      client_imports = client_imports,
      server_imports = server_imports,
      project_phase = project_phase,
      spatial_dirty = spatial_dirty,
      source_syncing = source_syncing,
      crb_generating = crb_generating,
      build_locked = build_locked,
      has_project = has_project,
      has_datasets = has_datasets
    ),
    class = c("builder_activity_state", "list")
  )
}

builder_activity_capabilities <- function(activity) {
  if (!inherits(activity, "builder_activity_state")) {
    stop("A Builder activity state is required.", call. = FALSE)
  }
  connected <- identical(activity$connection, "connected")
  importing <- activity$client_imports > 0L || isTRUE(activity$server_imports)
  project_busy <- activity$project_phase %in%
    c("choosing", "saving", "opening", "restoring", "conflict")
  # Autosave and reusable-CRB registration write the manifest in the
  # background and merge concurrent edits, so they only hold back other
  # manifest writers (Save, Open, Build) rather than the whole workspace.
  manifest_busy <- activity$project_phase %in% c("autosaving", "registering")
  mutable <- connected && !isTRUE(activity$build_locked) && !project_busy
  stable <- mutable && !importing
  # Optional CRB builds do not hold Open back: opening a Project cancels
  # them when it replaces the workspace.
  open_safe <- connected &&
    !isTRUE(activity$build_locked) &&
    !importing &&
    !isTRUE(activity$source_syncing) &&
    !activity$project_phase %in%
      c(
        "choosing",
        "saving",
        "autosaving",
        "opening",
        "restoring",
        "registering"
      )
  list(
    select_dataset = connected &&
      !identical(activity$project_phase, "choosing"),
    add_dataset = mutable,
    edit_dataset = mutable,
    mutate_datasets = mutable,
    ## Completed datasets remain configurable/checkable while another import
    ## is queued. Review/build still use the stable capability below.
    check_dataset = mutable,
    create_project = mutable &&
      !isTRUE(activity$has_project) &&
      isTRUE(activity$has_datasets),
    save_project = stable &&
      isTRUE(activity$has_project) &&
      activity$project_phase %in%
        c("clean", "dirty", "save_failed"),
    open_project = open_safe,
    navigate_workflow = mutable ||
      (connected &&
        !isTRUE(activity$build_locked) &&
        activity$project_phase %in% c("saving", "registering")),
    build = stable && !manifest_busy,
    page_inert = connected &&
      activity$project_phase %in%
        c("choosing", "opening"),
    warn_before_unload = importing ||
      isTRUE(activity$build_locked) ||
      isTRUE(activity$source_syncing) ||
      isTRUE(activity$crb_generating) ||
      isTRUE(activity$spatial_dirty) ||
      (isTRUE(activity$has_datasets) && !isTRUE(activity$has_project)) ||
      activity$project_phase %in%
        c(
          "dirty",
          "choosing",
          "saving",
          "autosaving",
          "opening",
          "restoring",
          "registering",
          "save_failed",
          "conflict"
        )
  )
}

builder_activity_requires_open_confirmation <- function(activity) {
  if (!inherits(activity, "builder_activity_state")) {
    stop("A Builder activity state is required.", call. = FALSE)
  }
  isTRUE(activity$spatial_dirty) ||
    activity$project_phase %in% c("dirty", "save_failed", "conflict") ||
    (isTRUE(activity$has_datasets) && !isTRUE(activity$has_project))
}

builder_activity_reason <- function(activity, operation) {
  capabilities <- builder_activity_capabilities(activity)
  if (isTRUE(capabilities[[operation]])) {
    return(NULL)
  }
  if (identical(activity$connection, "disconnected")) {
    return("Reconnect to Builder before continuing.")
  }
  if (isTRUE(activity$build_locked)) {
    return("Wait for the active build to finish.")
  }
  if (identical(activity$project_phase, "conflict")) {
    return("Reopen the project before saving more changes.")
  }
  if (identical(activity$project_phase, "choosing")) {
    return("Choose a project folder before continuing.")
  }
  if (identical(activity$project_phase, "registering")) {
    return("Wait for the dataset cache to finish saving.")
  }
  if (activity$project_phase %in% c("saving", "autosaving")) {
    return("Wait for the project operation to finish.")
  }
  if (activity$project_phase %in% c("opening", "restoring")) {
    return("Wait for the selected project datasets to finish loading.")
  }
  if (activity$client_imports > 0L || isTRUE(activity$server_imports)) {
    return("Wait for all dataset imports to finish.")
  }
  if (isTRUE(activity$source_syncing)) {
    return("Wait for the current Project source files to finish saving.")
  }
  switch(
    operation,
    create_project = "Add a dataset before creating a Builder project.",
    save_project = "Add a dataset before saving the project.",
    open_project = "Wait for the current operation to finish.",
    "This action is not available right now."
  )
}

builder_project_retain_check_marks <- function(
  marks,
  live_ids,
  last_removed = NULL,
  can_undo_remove = FALSE
) {
  retained_ids <- as.character(live_ids %||% character())
  if (
    isTRUE(can_undo_remove) &&
      is.list(last_removed) &&
      .builder_project_identifier(last_removed$id %||% NULL)
  ) {
    retained_ids <- c(retained_ids, as.character(last_removed$id))
  }
  marks[names(marks) %in% unique(retained_ids)]
}

.builder_project_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_project_integer <- function(value, default = 0L) {
  parsed <- suppressWarnings(as.integer(value %||% default))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 0L) {
    return(as.integer(default))
  }
  parsed
}

.builder_project_identifier <- function(value) {
  .builder_project_text(value) &&
    nchar(value, type = "bytes") <= 128L &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", value)
}

builder_project_allocate_dataset_id <- function(
  sequence,
  existing_ids = character(),
  restored_id = NULL
) {
  sequence <- .builder_project_integer(sequence)
  existing_ids <- as.character(existing_ids)
  if (!is.null(restored_id)) {
    if (!.builder_project_identifier(restored_id)) {
      stop("A restored dataset requires a safe stable id.", call. = FALSE)
    }
    if (restored_id %in% existing_ids) {
      stop("The restored dataset id is already in use.", call. = FALSE)
    }
    matched <- regmatches(
      restored_id,
      regexec("^ds([0-9]+)$", restored_id)
    )[[1L]]
    if (length(matched) == 2L) {
      restored_sequence <- suppressWarnings(as.integer(matched[[2L]]))
      if (!is.na(restored_sequence)) {
        sequence <- max(sequence, restored_sequence)
      }
    }
    return(list(id = restored_id, sequence = as.integer(sequence)))
  }
  repeat {
    if (sequence >= .Machine$integer.max) {
      stop("No dataset id remains available in this session.", call. = FALSE)
    }
    sequence <- sequence + 1L
    id <- paste0("ds", sequence)
    if (!id %in% existing_ids) {
      return(list(id = id, sequence = as.integer(sequence)))
    }
  }
}

builder_project_allocation_ids <- function(
  entries,
  import_entries = list(),
  replacing_id = NULL,
  reserved_ids = character()
) {
  entry_ids <- vapply(
    entries %||% list(),
    function(entry) {
      as.character(entry$id %||% "")
    },
    character(1)
  )
  import_ids <- vapply(
    import_entries %||% list(),
    function(entry) {
      as.character(entry$id %||% "")
    },
    character(1)
  )
  ids <- c(
    entry_ids[nzchar(entry_ids)],
    import_ids[nzchar(import_ids)],
    as.character(reserved_ids %||% character())
  )
  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  if (.builder_project_identifier(replacing_id)) {
    ids <- ids[ids != replacing_id]
  }
  ids
}

builder_project_dataset_reservations <- function(
  manifest,
  excluding_id = NULL
) {
  records <- manifest$datasets %||% list()
  if (!is.list(records)) {
    records <- list()
  }
  records <- Filter(
    function(record) {
      is.list(record) &&
        .builder_project_identifier(record$id %||% NULL) &&
        !identical(as.character(record$id), as.character(excluding_id %||% ""))
    },
    records
  )
  list(
    ids = unique(vapply(
      records,
      function(record) {
        as.character(record$id)
      },
      character(1)
    ))
  )
}

builder_project_example_source <- function(example_id, catalog) {
  if (!.builder_project_text(example_id) || !is.list(catalog)) {
    stop("A valid Builder example is required.", call. = FALSE)
  }
  record <- catalog[[example_id]] %||% NULL
  source <- if (is.list(record)) record$serialized_path %||% NULL else NULL
  if (!.builder_project_text(source) || !file.exists(source)) {
    stop("The Builder example source file was not found.", call. = FALSE)
  }
  list(
    origin = "example",
    example = example_id,
    path = normalizePath(source, winslash = "/", mustWork = TRUE),
    filename = basename(source)
  )
}

builder_project_source_storage_name <- function(filename, fallback = "rds") {
  filename <- as.character(filename %||% "")
  filename <- if (length(filename)) filename[[1L]] else ""
  extension <- tolower(tools::file_ext(filename))
  if (!extension %in% c("rds", "qs", "qs2", "h5ad", "h5mu", "zip")) {
    extension <- fallback
  }
  paste0("source.", extension)
}

builder_project_copy_file <- function(source, target) {
  cloned <- FALSE
  if (
    identical(unname(Sys.info()[["sysname"]]), "Darwin") &&
      file.exists("/bin/cp")
  ) {
    status <- suppressWarnings(system2(
      "/bin/cp",
      c("-c", "--", shQuote(source), shQuote(target)),
      stdout = FALSE,
      stderr = FALSE
    ))
    cloned <- isTRUE(status == 0L) && file.exists(target)
    if (!cloned) {
      unlink(target, force = TRUE)
    }
  }
  cloned ||
    file.copy(
      source,
      target,
      overwrite = TRUE,
      copy.mode = TRUE
    )
}

.builder_project_lexical_path <- function(path) {
  value <- gsub("\\\\", "/", path.expand(as.character(path)[[1L]]))
  if (!grepl("^/|^[A-Za-z]:/", value)) {
    value <- file.path(getwd(), value)
  }
  gsub("/+", "/", value)
}

.builder_project_rebase_lexical_path <- function(path, root) {
  lexical_root <- .builder_project_lexical_path(root)
  normalized_root <- normalizePath(
    lexical_root,
    winslash = "/",
    mustWork = TRUE
  )
  lexical_path <- .builder_project_lexical_path(path)
  if (
    identical(lexical_path, lexical_root) ||
      startsWith(lexical_path, paste0(lexical_root, "/"))
  ) {
    suffix <- substring(lexical_path, nchar(lexical_root) + 1L)
    lexical_path <- paste0(normalized_root, suffix)
  }
  list(path = lexical_path, root = normalized_root)
}

.builder_project_path_has_link_within <- function(
  path,
  root,
  allow_missing_leaf = FALSE
) {
  if (!.builder_project_text(path) || !.builder_project_text(root)) {
    return(TRUE)
  }
  rebased <- .builder_project_rebase_lexical_path(path, root)
  root <- rebased$root
  path <- rebased$path
  if (
    !identical(path, root) &&
      (!startsWith(path, paste0(root, "/")) ||
        any(strsplit(path, "/", fixed = TRUE)[[1L]] %in% c(".", "..")))
  ) {
    return(TRUE)
  }
  relative <- if (identical(path, root)) {
    ""
  } else {
    substring(path, nchar(root) + 2L)
  }
  components <- if (nzchar(relative)) {
    strsplit(relative, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  current <- root
  for (index in seq_along(components)) {
    component <- components[[index]]
    current <- file.path(current, component)
    link <- tryCatch(
      Sys.readlink(current),
      error = function(error) NA_character_
    )
    if (is.na(link)) {
      missing_leaf <- isTRUE(allow_missing_leaf) &&
        identical(index, length(components)) &&
        !file.exists(current) &&
        !dir.exists(current)
      if (!missing_leaf) {
        return(TRUE)
      }
      next
    }
    if (nzchar(link)) {
      return(TRUE)
    }
  }
  FALSE
}

builder_project_cleanup_session_sources <- function(session_root) {
  if (!.builder_project_text(session_root) || !dir.exists(session_root)) {
    return(FALSE)
  }
  root <- normalizePath(session_root, winslash = "/", mustWork = TRUE)
  source_root <- file.path(root, "session-sources")
  if (!dir.exists(source_root)) {
    return(TRUE)
  }
  if (
    nzchar(Sys.readlink(source_root)) ||
      .builder_project_path_has_link_within(source_root, root)
  ) {
    return(FALSE)
  }
  dataset_dirs <- list.files(
    source_root,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  for (dataset_dir in dataset_dirs) {
    if (nzchar(Sys.readlink(dataset_dir)) || !dir.exists(dataset_dir)) {
      return(FALSE)
    }
    members <- list.files(
      dataset_dir,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    if (
      length(members) &&
        any(vapply(
          members,
          function(member) {
            nzchar(Sys.readlink(member)) || dir.exists(member)
          },
          logical(1)
        ))
    ) {
      return(FALSE)
    }
  }
  unlink(source_root, recursive = TRUE, force = TRUE)
  !dir.exists(source_root)
}

builder_project_retain_session_source <- function(
  source,
  filename,
  root,
  dataset_id
) {
  target <- builder_project_session_source_path(
    source,
    filename,
    root,
    dataset_id
  )
  target_dir <- dirname(target)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    stop("The session source folder could not be created.", call. = FALSE)
  }
  same <- identical(
    normalizePath(source, winslash = "/", mustWork = TRUE),
    normalizePath(target, winslash = "/", mustWork = FALSE)
  )
  if (
    !same &&
      !builder_project_copy_file(source, target)
  ) {
    stop("The uploaded dataset source could not be retained.", call. = FALSE)
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

builder_project_session_source_path <- function(
  source,
  filename,
  root,
  dataset_id
) {
  if (
    !.builder_project_text(source) ||
      !file.exists(source) ||
      !.builder_project_text(root) ||
      !dir.exists(root) ||
      !.builder_project_identifier(dataset_id)
  ) {
    stop("A valid uploaded dataset source is required.", call. = FALSE)
  }
  display_name <- if (.builder_project_text(filename)) {
    basename(filename)
  } else {
    basename(source)
  }
  if (!nzchar(display_name) || display_name %in% c(".", "..")) {
    display_name <- paste0(dataset_id, ".rds")
  }
  retained_name <- builder_project_source_storage_name(display_name)
  file.path(
    normalizePath(root, winslash = "/", mustWork = TRUE),
    "session-sources",
    dataset_id,
    retained_name
  )
}

builder_project_transport_retained_write <- function(path, id, retained_path) {
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (
    !scalar_text(path) ||
      !scalar_text(id) ||
      nchar(id, type = "bytes") > 128L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", id) ||
      !scalar_text(retained_path) ||
      !dir.exists(dirname(path))
  ) {
    return(FALSE)
  }
  temporary <- tempfile(
    pattern = ".transport-retained-",
    tmpdir = dirname(path),
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saved <- try(
    saveRDS(
      list(
        id = as.character(id),
        retained_path = normalizePath(
          retained_path,
          winslash = "/",
          mustWork = TRUE
        )
      ),
      temporary,
      version = 2L
    ),
    silent = TRUE
  )
  if (inherits(saved, "try-error")) {
    return(FALSE)
  }
  moved <- isTRUE(file.rename(temporary, path))
  if (!moved && file.exists(path)) {
    unlink(path, force = TRUE)
    moved <- isTRUE(file.rename(temporary, path))
  }
  moved
}

builder_project_transport_retained_read <- function(path, id) {
  if (
    !.builder_project_text(path) ||
      !file.exists(path) ||
      !.builder_project_identifier(id)
  ) {
    return(NULL)
  }
  record <- try(readRDS(path), silent = TRUE)
  if (
    inherits(record, "try-error") ||
      !is.list(record) ||
      !identical(names(record), c("id", "retained_path")) ||
      !identical(record$id, as.character(id)) ||
      !.builder_project_text(record$retained_path) ||
      !file.exists(record$retained_path)
  ) {
    return(NULL)
  }
  record
}

builder_project_transport_retained_remove <- function(path) {
  if (!.builder_project_text(path)) {
    return(FALSE)
  }
  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  !file.exists(path)
}

builder_project_load_retained_source <- function(
  id,
  source,
  progress,
  .adapter = builder_seurat_file_adapter,
  .register = function(adapter, id, progress) {
    .builder_register_adapter(adapter, id, progress)
  },
  .copy = builder_project_copy_file,
  .transport_writer = builder_project_transport_retained_write
) {
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (!is.list(source) || !scalar_text(source$retained_path)) {
    stop("A retained dataset path is required.", call. = FALSE)
  }
  target <- path.expand(source$retained_path)
  if (!file.exists(target)) {
    if (!scalar_text(source$source) || !file.exists(source$source)) {
      stop("The uploaded dataset source is no longer available.", call. = FALSE)
    }
    target_dir <- dirname(target)
    if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
      stop("The session source folder could not be created.", call. = FALSE)
    }
    part <- tempfile(
      pattern = paste0(".", basename(target), "-"),
      tmpdir = target_dir,
      fileext = ".part"
    )
    on.exit(unlink(part, force = TRUE), add = TRUE)
    # A browser upload's temporary file belongs to Builder: on the same
    # volume it is moved instead of copied. A user's own file is never moved.
    moved <- isTRUE(source$transport_owned) &&
      isTRUE(suppressWarnings(file.rename(source$source, target)))
    if (!moved) {
      copied <- .copy(source$source, part)
      if (!isTRUE(copied) || !file.rename(part, target)) {
        stop(
          "The uploaded dataset source could not be retained.",
          call. = FALSE
        )
      }
    }
  }
  target <- normalizePath(target, winslash = "/", mustWork = TRUE)
  if (
    scalar_text(source$transport_retained_path) &&
      !.transport_writer(
        source$transport_retained_path,
        id,
        target
      )
  ) {
    stop(
      "The retained upload could not be acknowledged safely.",
      call. = FALSE
    )
  }
  value <- .register(.adapter(target), id, progress)
  value$retained_path <- target
  value
}

builder_project_configuration_entry <- function(entry) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id %||% NULL)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  list(
    id = as.character(entry$id),
    revision = as.integer(entry$revision %||% 0L),
    settings = entry$settings %||% list(),
    acknowledgements = entry$acknowledgements %||% character(),
    spatial_drafts = entry$spatial_drafts %||% list()
  )
}

.builder_project_map_spatial_images <- function(entry, transform) {
  images <- entry$settings$images %||% list()
  if (!is.list(images) || !length(images)) {
    return(entry)
  }
  for (section in names(images)) {
    collection <- images[[section]]
    legacy_record <- is.list(collection) &&
      any(
        c("source_path", "source_uri", "uri", "project_asset") %in%
          names(collection)
      )
    if (legacy_record) {
      label <- collection$source$name %||% "image"
      images[[section]] <- transform(collection, section, as.character(label))
      next
    }
    if (!is.list(collection) || !length(collection)) {
      next
    }
    for (label in names(collection)) {
      images[[section]][[label]] <- transform(
        collection[[label]],
        section,
        label
      )
    }
  }
  entry$settings$images <- images
  entry
}

builder_project_configuration_digest_value <- function(entry) {
  digest_entry <- list(
    settings = entry$settings %||% list(),
    acknowledgements = entry$acknowledgements %||% character(),
    spatial_drafts = entry$spatial_drafts %||% list()
  )
  tables <- digest_entry$settings$tables %||% list()
  if (is.list(tables) && length(tables)) {
    digest_entry$settings$tables <- lapply(tables, function(record) {
      if (is.list(record)) {
        record$source_path <- NULL
        record$table <- NULL
        record$project_asset <- NULL
      }
      record
    })
  }
  digest_entry <- .builder_project_map_spatial_images(
    digest_entry,
    function(record, section, label) {
      if (is.list(record)) {
        source_md5 <- record$source_content_md5 %||%
          record$project_asset$fingerprint$md5 %||%
          NULL
        if (
          !.builder_project_text(source_md5) ||
            !grepl("^[[:xdigit:]]{32}$", source_md5)
        ) {
          source_path <- record[["source_path", exact = TRUE]]
          source_md5 <- if (
            .builder_project_text(source_path) &&
              file.exists(source_path) &&
              !dir.exists(source_path)
          ) {
            unname(as.character(tools::md5sum(source_path)))
          } else {
            NULL
          }
        }
        # Digest-only content identity; never serialized as project UI state.
        record$source_content_md5 <- source_md5
        record$source_path <- NULL
        record$source_uri <- NULL
        record$uri <- NULL
        record$project_asset <- NULL
      }
      record
    }
  )
  # `levels` is source-derived and immutable after import. Source fingerprinting,
  # rather than the editable-configuration digest, invalidates it on reload.
  list(
    contract_version = .builder_project_configuration_contract_version,
    settings = digest_entry$settings %||% list(),
    acknowledgements = digest_entry$acknowledgements %||% character(),
    spatial_drafts = digest_entry$spatial_drafts %||% list()
  )
}

## The digest hashes the canonical UTF-8 JSON bytes themselves, not an R
## serialization, so a Project saved by one R runtime opens identically in
## another.
builder_project_configuration_digest <- function(
  entry,
  memo = .builder_project_configuration_digest_memo
) {
  value <- builder_project_configuration_digest_value(entry)
  # serializeJSON() dominates a Save (tens of ms per dataset, several times
  # per dataset and transaction). The digest is a pure function of `value`,
  # so key it by the much cheaper in-process R serialization of that value.
  key <- if (is.environment(memo)) {
    digest::digest(value, algo = "md5")
  } else {
    NULL
  }
  cached <- if (is.null(key)) {
    NULL
  } else {
    get0(key, envir = memo, inherits = FALSE)
  }
  if (.builder_project_text(cached %||% NULL)) {
    return(cached)
  }
  canonical <- jsonlite::serializeJSON(
    value,
    digits = NA,
    pretty = FALSE
  )
  result <- unclass(as.character(digest::digest(
    charToRaw(enc2utf8(canonical)),
    algo = "md5",
    serialize = FALSE
  )))
  if (!is.null(key)) {
    if (length(ls(memo, all.names = TRUE)) >= 512L) {
      rm(list = ls(memo, all.names = TRUE), envir = memo)
    }
    assign(key, result, envir = memo)
  }
  result
}

builder_project_cached_configuration_digest <- function(
  entry,
  cache,
  digest = builder_project_configuration_digest,
  variant = "live"
) {
  if (
    !is.environment(cache) || !.builder_project_identifier(entry$id %||% NULL)
  ) {
    return(digest(entry))
  }
  key <- paste(
    entry$id,
    as.integer(entry$revision %||% 0L),
    as.character(variant %||% "live"),
    sep = "::"
  )
  cached <- get0(key, envir = cache, inherits = FALSE)
  if (.builder_project_text(cached %||% NULL)) {
    return(cached)
  }
  value <- digest(entry)
  assign(key, value, envir = cache)
  id_prefix <- paste0(entry$id, "::")
  revision_prefix <- paste0(
    entry$id,
    "::",
    as.integer(entry$revision %||% 0L),
    "::"
  )
  keys <- ls(cache)
  stale <- keys[
    startsWith(keys, id_prefix) & !startsWith(keys, revision_prefix)
  ]
  if (length(stale)) {
    rm(list = stale, envir = cache)
  }
  revision_keys <- keys[startsWith(keys, revision_prefix)]
  overflow <- setdiff(revision_keys, key)
  if (length(revision_keys) > 8L && length(overflow)) {
    rm(
      list = utils::head(
        sort(overflow, method = "radix"),
        length(revision_keys) - 8L
      ),
      envir = cache
    )
  }
  value
}

builder_project_configuration_cache_clear <- function(cache) {
  if (!is.environment(cache)) {
    return(invisible(FALSE))
  }
  keys <- ls(cache)
  if (length(keys)) {
    rm(list = keys, envir = cache)
  }
  invisible(TRUE)
}

builder_project_configuration_cache_retain_datasets <- function(cache, ids) {
  if (!is.environment(cache)) {
    return(invisible(FALSE))
  }
  keys <- ls(cache)
  cached_ids <- sub("::.*$", "", keys)
  drop <- keys[!cached_ids %in% as.character(ids %||% character())]
  if (length(drop)) {
    rm(list = drop, envir = cache)
  }
  invisible(TRUE)
}

builder_project_configuration <- function(
  review_options = NULL,
  build_mode = FALSE,
  auth_enabled = FALSE,
  initial_dataset = NULL
) {
  scalar_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (!scalar_flag(build_mode) || !scalar_flag(auth_enabled)) {
    stop("Builder project preferences are invalid.", call. = FALSE)
  }
  if (!is.null(initial_dataset)) {
    initial_dataset <- as.character(initial_dataset)
    if (
      length(initial_dataset) != 1L ||
        is.na(initial_dataset) ||
        !.builder_project_identifier(initial_dataset)
    ) {
      stop("The Builder starting dataset preference is invalid.", call. = FALSE)
    }
  }
  allowed <- c(
    "welcome_message",
    "initial_page",
    "point_size",
    "variable_to_compare",
    "host",
    "port",
    "max_request_size",
    "display_mode",
    "launch_browser",
    "show_upload_ui"
  )
  saved_options <- if (is.null(review_options)) {
    NULL
  } else {
    if (!is.list(review_options)) {
      stop("Builder Review preferences are invalid.", call. = FALSE)
    }
    candidate <- unclass(review_options)[
      intersect(allowed, names(review_options))
    ]
    required <- setdiff(allowed, names(candidate))
    if (length(required)) {
      stop("Builder Review preferences are incomplete.", call. = FALSE)
    }
    valid_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    }
    valid_number <- function(value, lower, upper = Inf, whole = FALSE) {
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value >= lower &&
        value <= upper &&
        (!whole || value == floor(value))
    }
    valid_flag <- function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }
    if (
      !valid_text(candidate$welcome_message) ||
        !valid_text(candidate$initial_page) ||
        !valid_number(candidate$point_size, 0, 20) ||
        !valid_flag(candidate$variable_to_compare) ||
        !valid_text(candidate$host) ||
        !valid_number(candidate$port, 1, 65535, whole = TRUE) ||
        !valid_number(candidate$max_request_size, .Machine$double.eps) ||
        !valid_text(candidate$display_mode) ||
        !candidate$display_mode %in% c("auto", "normal", "showcase") ||
        !valid_flag(candidate$launch_browser) ||
        !valid_flag(candidate$show_upload_ui)
    ) {
      stop("Builder Review preferences are invalid.", call. = FALSE)
    }
    candidate$point_size <- as.double(candidate$point_size)
    candidate$port <- as.integer(candidate$port)
    candidate$max_request_size <- as.double(candidate$max_request_size)
    candidate
  }
  list(
    review_options = saved_options,
    build_mode = isTRUE(build_mode),
    auth_enabled = isTRUE(auth_enabled),
    initial_dataset = initial_dataset
  )
}

builder_project_last_ui_spatial <- function(
  spatial,
  selected_dataset = NULL
) {
  if (is.null(spatial)) {
    return(NULL)
  }
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (
    !is.list(spatial) ||
      !scalar_text(spatial$dataset) ||
      !scalar_text(spatial$section) ||
      (!is.null(spatial$image) && !scalar_text(spatial$image)) ||
      (!is.null(selected_dataset) &&
        !identical(spatial$dataset, selected_dataset))
  ) {
    return(NULL)
  }
  list(
    dataset = spatial$dataset,
    section = spatial$section,
    image = spatial$image %||% NULL
  )
}

builder_project_last_ui_target <- function(
  last_ui,
  available_ids
) {
  available_ids <- unique(as.character(available_ids %||% character()))
  saved_dataset <- if (is.list(last_ui)) last_ui$selected_dataset else NULL
  selected_dataset <- if (
    length(available_ids) &&
      is.character(saved_dataset) &&
      length(saved_dataset) == 1L &&
      !is.na(saved_dataset) &&
      saved_dataset %in% available_ids
  ) {
    saved_dataset
  } else if (length(available_ids)) {
    available_ids[[1L]]
  } else {
    NULL
  }
  list(
    selected_dataset = selected_dataset,
    stage = if (length(available_ids)) "configure" else "upload",
    spatial = builder_project_last_ui_spatial(
      if (is.list(last_ui)) last_ui$spatial %||% NULL else NULL,
      selected_dataset = selected_dataset
    )
  )
}

builder_project_restore_progress_detail <- function(progress) {
  if (!is.list(progress)) {
    return(NULL)
  }
  queue_total <- suppressWarnings(as.integer(progress$total %||% 0L))
  remaining <- suppressWarnings(as.integer(progress$remaining %||% 0L))
  selected_total <- suppressWarnings(as.integer(
    progress$selected_total %||% queue_total
  ))
  ready <- suppressWarnings(as.integer(
    progress$ready %||% (queue_total - remaining)
  ))
  valid_count <- function(value) {
    length(value) == 1L && !is.na(value) && is.finite(value)
  }
  if (!valid_count(selected_total) || selected_total <= 0L) {
    return(NULL)
  }
  if (!valid_count(ready)) {
    ready <- 0L
  }
  ready <- max(0L, min(ready, selected_total))
  paste0(ready, " of ", selected_total, " selected datasets ready")
}

builder_project_check_identity <- function(
  entry,
  identity_cache = NULL,
  variant = "live"
) {
  if (
    is.list(entry) &&
      identical(entry$load_state %||% "loaded", "artifact_ready") &&
      is.list(entry$project_artifact)
  ) {
    value <- entry$project_artifact$fingerprint$md5 %||%
      entry$project_artifact$path %||%
      entry$id
    return(paste0("artifact:", value))
  }
  revision_value <- suppressWarnings(as.integer(
    entry$revision %||% NA_integer_
  ))
  revision <- if (
    length(revision_value) == 1L &&
      !is.na(revision_value) &&
      revision_value >= 0L
  ) {
    revision_value
  } else {
    NA_integer_
  }
  if (
    identical(variant, "live") &&
      !is.na(revision) &&
      .builder_project_identifier(entry$id %||% NULL)
  ) {
    return(paste0(
      "configuration:",
      entry$id,
      ":revision:",
      revision
    ))
  }
  paste0(
    "configuration:",
    builder_project_cached_configuration_digest(
      entry,
      identity_cache,
      variant = variant
    ),
    ":revision:",
    if (is.na(revision)) 0L else revision
  )
}

builder_project_effective_check_identity <- function(
  entry,
  coordinate_drafts = list(),
  identity_cache = NULL
) {
  records <- if (
    is.list(entry) &&
      .builder_project_text(entry$id %||% "") &&
      is.list(coordinate_drafts)
  ) {
    coordinate_drafts[[entry$id]] %||% list()
  } else {
    list()
  }
  if (length(records)) {
    variant <- unclass(as.character(digest::digest(
      serialize(records, NULL, version = 3L),
      algo = "md5",
      serialize = FALSE
    )))
    snapshot_identity <- tryCatch(
      .builder_worker_identity(entry$snapshot),
      error = function(error) NULL
    )
    applied <- if (is.null(snapshot_identity)) {
      NULL
    } else {
      tryCatch(
        builder_coordinate_drafts_apply_entry(
          entry,
          records,
          snapshot_identity = snapshot_identity
        ),
        error = function(error) NULL
      )
    }
    if (!is.list(applied) || !is.list(applied$entry)) {
      return(NA_character_)
    }
    entry <- applied$entry
  } else {
    variant <- "live"
  }
  builder_project_check_identity(entry, identity_cache, variant)
}

builder_project_checked_ids <- function(
  entries,
  marks,
  coordinate_drafts = list(),
  identity_cache = NULL
) {
  ids <- vapply(entries, `[[`, character(1), "id")
  identities <- vapply(
    entries,
    builder_project_effective_check_identity,
    character(1),
    coordinate_drafts = coordinate_drafts,
    identity_cache = identity_cache
  )
  matched <- !is.na(identities) & ids %in% names(marks)
  matched[matched] <- unname(marks[ids[matched]]) == identities[matched]
  ids[matched]
}

builder_project_pending_draft_ids <- function(pending_drafts) {
  if (!is.list(pending_drafts) || !length(pending_drafts)) {
    return(character())
  }
  ids <- unlist(
    lapply(pending_drafts, function(drafts) {
      if (is.list(drafts)) names(drafts) %||% character() else character()
    }),
    use.names = FALSE
  )
  unique(ids[!is.na(ids) & nzchar(ids)])
}

builder_project_loaded_settings_compatible <- function(entry) {
  profiles <- entry$profile$assay_profiles %||% NULL
  if (!is.list(profiles) || is.null(names(profiles))) {
    return(TRUE)
  }
  assay <- as.character(entry$settings$assay %||% "")
  layer <- as.character(entry$settings$layer %||% "")
  if (
    length(assay) != 1L ||
      is.na(assay) ||
      !nzchar(assay) ||
      length(layer) != 1L ||
      is.na(layer) ||
      !nzchar(layer)
  ) {
    return(FALSE)
  }
  assay_profile <- profiles[[assay]] %||% NULL
  is.list(assay_profile) &&
    layer %in% as.character(assay_profile$layers %||% character())
}


.builder_project_configuration_digest_memo <- new.env(parent = emptyenv())

## The project file ------------------------------------------------------------

builder_project_manifest_path <- function(root) {
  file.path(root, .builder_project_manifest_name)
}

## What a folder can be for a project: a folder that already holds a project,
## or any other existing folder. `NULL` root means it cannot be used.
builder_project_folder_state <- function(path) {
  if (!.builder_project_text(path) || !dir.exists(path)) {
    stop("Choose an existing folder for the project.", call. = FALSE)
  }
  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (nzchar(Sys.readlink(root))) {
    stop("A project folder cannot be a symbolic link.", call. = FALSE)
  }
  if (file.access(root, 2L) != 0L) {
    stop("The project folder is not writable.", call. = FALSE)
  }
  list(
    root = root,
    kind = if (file.exists(builder_project_manifest_path(root))) {
      "project"
    } else {
      "new"
    }
  )
}

## Plain data only: a project file is read back with readRDS(), so anything
## that could carry code (functions, environments, calls, S4 objects) is refused.
.builder_project_plain_data <- function(value, depth = 0L) {
  if (depth > 64L) {
    return(FALSE)
  }
  if (is.null(value) || is.atomic(value)) {
    return(!isS4(value))
  }
  if (is.list(value) && !is.object(value) || inherits(value, "data.frame")) {
    return(all(vapply(
      unclass(value),
      .builder_project_plain_data,
      logical(1),
      depth = depth + 1L
    )))
  }
  FALSE
}

.builder_project_source_record <- function(entry) {
  origin <- as.character(entry$source_origin %||% "upload")[[1L]]
  filename <- as.character(entry$filename %||% basename(entry$path %||% ""))[[
    1L
  ]]
  if (identical(origin, "example")) {
    return(list(
      kind = "example",
      origin = "example",
      example = as.character(entry$example %||% ""),
      filename = filename
    ))
  }
  path <- entry$path %||% NULL
  info <- if (.builder_project_text(path) && file.exists(path)) {
    file.info(path)
  } else {
    NULL
  }
  list(
    kind = "file",
    origin = origin,
    path = if (identical(origin, "local") && .builder_project_text(path)) {
      normalizePath(path, winslash = "/", mustWork = FALSE)
    } else {
      NULL
    },
    filename = filename,
    bytes = if (is.null(info)) NA_real_ else as.double(info$size),
    modified_at = if (is.null(info)) {
      NA_character_
    } else {
      format(info$mtime, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    }
  )
}

builder_project_dataset_record <- function(entry, order = 1L, checked = FALSE) {
  list(
    id = as.character(entry$id),
    name = as.character(entry$settings$name %||% entry$id),
    order = as.integer(order),
    format = entry$format %||% NULL,
    source = .builder_project_source_record(entry),
    configuration = builder_project_configuration_entry(entry),
    checked = isTRUE(checked)
  )
}

builder_project_snapshot <- function(
  name,
  entries,
  checked_ids = character(),
  configuration = NULL,
  last_ui = NULL,
  saved_at = Sys.time()
) {
  list(
    format = .builder_project_manifest_format,
    schema_version = .builder_project_schema_version,
    project = list(
      name = as.character(name),
      saved_at = format(saved_at, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    ),
    configuration = configuration %||% builder_project_configuration(),
    last_ui = last_ui %||% list(),
    datasets = lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]
      builder_project_dataset_record(
        entry,
        order = index,
        checked = as.character(entry$id) %in% checked_ids
      )
    })
  )
}

## Writes the project next to nothing else: a temporary file in the same
## folder, then a rename, so a failed write never leaves half a project.
builder_project_write <- function(root, manifest) {
  folder <- builder_project_folder_state(root)
  if (!.builder_project_plain_data(manifest)) {
    stop("The project holds a value that cannot be saved.", call. = FALSE)
  }
  path <- builder_project_manifest_path(folder$root)
  temporary <- tempfile(
    pattern = ".builder-project-",
    tmpdir = folder$root,
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saved <- try(saveRDS(manifest, temporary, version = 3L), silent = TRUE)
  if (inherits(saved, "try-error") || !file.exists(temporary)) {
    stop("The project file could not be written.", call. = FALSE)
  }
  if (!isTRUE(file.rename(temporary, path))) {
    unlink(path, force = TRUE)
    if (!isTRUE(file.rename(temporary, path))) {
      stop("The project file could not be replaced.", call. = FALSE)
    }
  }
  invisible(path)
}

builder_project_read <- function(path) {
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    stop("The project file was not found.", call. = FALSE)
  }
  size <- file.size(path)
  if (is.na(size) || size > .builder_project_manifest_max_bytes) {
    stop("The project file is too large to open.", call. = FALSE)
  }
  manifest <- try(readRDS(path), silent = TRUE)
  if (
    inherits(manifest, "try-error") ||
      !is.list(manifest) ||
      !.builder_project_plain_data(manifest) ||
      !identical(manifest$format %||% NULL, .builder_project_manifest_format)
  ) {
    stop("This is not a Builder project file.", call. = FALSE)
  }
  if (
    !identical(
      manifest$schema_version %||% NULL,
      .builder_project_schema_version
    )
  ) {
    stop(
      "This project was saved by a different Builder version.",
      call. = FALSE
    )
  }
  records <- manifest$datasets %||% list()
  if (!is.list(records)) {
    stop("The project file lists no datasets.", call. = FALSE)
  }
  valid <- vapply(
    records,
    function(record) {
      is.list(record) &&
        .builder_project_identifier(record$id %||% NULL) &&
        is.list(record$source) &&
        record$source$kind %in% c("file", "example") &&
        is.list(record$configuration) &&
        identical(record$configuration$id, record$id)
    },
    logical(1)
  )
  ids <- vapply(records, function(record) as.character(record$id %||% ""), "")
  if (!all(valid) || anyDuplicated(ids)) {
    stop("The project file has a damaged dataset record.", call. = FALSE)
  }
  settings <- manifest$configuration %||% list()
  manifest$configuration <- builder_project_configuration(
    review_options = settings$review_options %||% NULL,
    build_mode = settings$build_mode %||% FALSE,
    auth_enabled = settings$auth_enabled %||% FALSE,
    initial_dataset = settings$initial_dataset %||% NULL
  )
  manifest$project$name <- as.character(
    manifest$project$name %||% basename(dirname(path))
  )[[1L]]
  manifest
}

builder_project_record_map <- function(manifest) {
  records <- manifest$datasets %||% list()
  ids <- vapply(records, function(record) as.character(record$id), character(1))
  stats::setNames(records, ids)
}

## Saved datasets that are unchanged in the live workspace: the same datasets
## in the same order, each with the configuration that was saved.
## A picture the Builder extracted from a source lives in that session's
## temporary folder, so its path differs from one session to the next. A record
## that carries the picture's content hash is the same picture whatever its
## path; comparing two settings ignores the path of such a record.
.builder_project_without_session_paths <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  if (
    "source_path" %in%
      names(value) &&
      .builder_project_text(value[["source_content_md5", exact = TRUE]])
  ) {
    value[["source_path"]] <- NULL
    return(value)
  }
  lapply(value, .builder_project_without_session_paths)
}

builder_project_live_dirty <- function(entries, manifest) {
  records <- manifest$datasets %||% list()
  if (length(entries) != length(records)) {
    return(TRUE)
  }
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    record <- records[[index]]
    saved <- record$configuration %||% list()
    live <- builder_project_configuration_entry(entry)
    if (
      !identical(as.character(entry$id), as.character(record$id)) ||
        !identical(
          .builder_project_without_session_paths(live$settings),
          .builder_project_without_session_paths(saved$settings)
        ) ||
        !identical(live$acknowledgements, saved$acknowledgements) ||
        !identical(live$spatial_drafts, saved$spatial_drafts)
    ) {
      return(TRUE)
    }
  }
  FALSE
}

## Opening --------------------------------------------------------------------

## Which saved datasets can be loaded again, and why the others cannot.
## `path_allowed` is a function(path) -> TRUE/FALSE for a server that only lets
## Builder read from some folders; a project file must not widen that.
builder_project_restore_plan <- function(
  manifest,
  examples = NULL,
  path_allowed = NULL
) {
  records <- manifest$datasets %||% list()
  plan <- lapply(records, function(record) {
    source <- record$source
    status <- "ready"
    reason <- NULL
    if (identical(source$kind, "example")) {
      known <- is.null(examples) || source$example %in% examples
      if (!known) {
        status <- "missing"
        reason <- "The example is not available in this Builder."
      }
    } else if (!identical(source$origin, "local")) {
      status <- "upload"
      reason <- "It was uploaded in the browser. Add it again."
    } else if (
      !.builder_project_text(source$path %||% NULL) ||
        !file.exists(source$path) ||
        dir.exists(source$path)
    ) {
      status <- "missing"
      reason <- paste0("The file is no longer at ", source$path %||% "?", ".")
    } else if (
      is.function(path_allowed) && !isTRUE(path_allowed(source$path))
    ) {
      status <- "missing"
      reason <- "The file is outside the folders this server allows."
    }
    list(id = record$id, name = record$name, status = status, reason = reason)
  })
  names(plan) <- vapply(plan, `[[`, "", "id")
  plan
}

## A saved picture or table that lives in a file that is gone, and carries no
## copy of its own, is dropped so the picture the source file brought along
## (re-read on load) is not hidden by a dead path.
.builder_project_usable_record <- function(record) {
  if (!is.list(record)) {
    return(TRUE)
  }
  path <- record[["source_path", exact = TRUE]]
  uri <- record[["source_uri", exact = TRUE]] %||% record[["uri", exact = TRUE]]
  has_copy <- .builder_project_text(uri %||% NULL)
  !(.builder_project_text(path %||% NULL) && !file.exists(path) && !has_copy)
}

## A saved picture whose file was in a session's temporary folder is the same
## picture the source brought again when the content hash matches: keep the
## saved adjustments (position, scale, opacity, ...) and point it at the new file.
.builder_project_rebind_pictures <- function(saved, fresh) {
  if (!is.list(saved) || !is.list(fresh)) {
    return(saved)
  }
  if ("source_path" %in% names(saved)) {
    path <- saved[["source_path", exact = TRUE]]
    hash <- saved[["source_content_md5", exact = TRUE]]
    if (
      .builder_project_text(path) &&
        !file.exists(path) &&
        .builder_project_text(hash) &&
        .builder_project_text(fresh[["source_path", exact = TRUE]]) &&
        identical(hash, fresh[["source_content_md5", exact = TRUE]])
    ) {
      saved[["source_path"]] <- fresh[["source_path", exact = TRUE]]
      if (!is.null(fresh[["source", exact = TRUE]])) {
        saved[["source"]] <- fresh[["source", exact = TRUE]]
      }
    }
    return(saved)
  }
  for (name in intersect(names(saved), names(fresh))) {
    saved[[name]] <- .builder_project_rebind_pictures(
      saved[[name]],
      fresh[[name]]
    )
  }
  saved
}

.builder_project_usable_settings <- function(settings) {
  for (field in c("images", "tables")) {
    collection <- settings[[field]]
    if (!is.list(collection) || !length(collection)) {
      next
    }
    keep_nested <- function(value) {
      if (!is.list(value)) {
        return(value)
      }
      if (any(c("source_path", "source_uri", "uri") %in% names(value))) {
        return(if (.builder_project_usable_record(value)) value else NULL)
      }
      kept <- lapply(value, keep_nested)
      kept[!vapply(kept, is.null, logical(1))]
    }
    collection <- keep_nested(collection)
    if (is.list(collection) && !is.null(names(collection))) {
      collection <- collection[!vapply(collection, is.null, logical(1))]
    }
    settings[[field]] <- collection
  }
  settings
}

## The loaded entry gets the settings the project saved for it, when they still
## fit what the source holds; otherwise it keeps its defaults.
builder_project_finalize_loaded_entry <- function(loaded, record) {
  if (
    !is.list(loaded) ||
      !.builder_project_identifier(loaded$id) ||
      !is.list(record) ||
      !identical(as.character(record$id %||% ""), loaded$id)
  ) {
    stop(
      "The saved dataset configuration does not match the loaded entry.",
      call. = FALSE
    )
  }
  saved <- record$configuration %||% NULL
  if (!is.list(saved) || !is.list(saved$settings)) {
    return(loaded)
  }
  fresh <- loaded
  saved_settings <- saved$settings
  for (field in c("images", "tables")) {
    if (is.list(saved_settings[[field]])) {
      saved_settings[[field]] <- .builder_project_rebind_pictures(
        saved_settings[[field]],
        loaded$settings[[field]] %||% list()
      )
    }
  }
  saved_settings <- .builder_project_usable_settings(saved_settings)
  fresh$settings <- utils::modifyList(
    loaded$settings %||% list(),
    saved_settings[setdiff(names(saved_settings), c("images", "tables"))],
    keep.null = TRUE
  )
  for (field in c("images", "tables")) {
    if (!is.null(saved_settings[[field]])) {
      fresh$settings[[field]] <- utils::modifyList(
        loaded$settings[[field]] %||% list(),
        saved_settings[[field]]
      )
    }
  }
  if (!builder_project_loaded_settings_compatible(fresh)) {
    return(loaded)
  }
  fresh$acknowledgements <- saved$acknowledgements %||% NULL
  fresh$spatial_drafts <- saved$spatial_drafts %||% NULL
  fresh$revision <- max(
    as.integer(loaded$revision %||% 0L),
    as.integer(saved$revision %||% 0L)
  )
  fresh
}

builder_project_restore_summary <- function(plan, failed = character()) {
  names_of <- function(status) {
    vapply(
      Filter(function(item) identical(item$status, status), plan),
      function(item) as.character(item$name %||% item$id),
      ""
    )
  }
  list(
    upload = unname(names_of("upload")),
    missing = unname(names_of("missing")),
    failed = as.character(failed)
  )
}
