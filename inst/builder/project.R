## Builder projects keep durable user choices separate from session-only
## workers. Schema v3 is an index: exact configuration lives in small typed
## sidecars while source-derived profiles are regenerated from source data.
## Earlier schemas (v1 and v2, with inline configuration payloads and R
## serialization digests) are not read.

.builder_project_schema_version <- 3L
.builder_project_config_schema_version <- 1L
.builder_project_configuration_contract_version <- 3L
.builder_project_manifest_max_bytes <- 10L * 1024L^2

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

builder_project_abandoned_entries <- function(
  pending,
  live_ids = character(),
  import_ids = character(),
  operation = "idle"
) {
  if (!identical(operation, "restoring") || !length(pending)) {
    return(character())
  }
  setdiff(names(pending), c(live_ids, import_ids))
}

## A background manifest write returns entries derived from `base`, the live
## entries when the write started. Entries the user left alone take the saved
## version; entries edited, added or removed meanwhile keep their live state
## and leave the project dirty for the next save.
builder_project_merge_saved_entries <- function(base, saved, live) {
  if (identical(live, base)) {
    return(saved)
  }
  entry_ids <- function(entries) {
    vapply(
      entries,
      function(entry) as.character(entry$id %||% ""),
      character(1)
    )
  }
  base_ids <- entry_ids(base)
  saved_ids <- entry_ids(saved)
  merged <- lapply(live, function(entry) {
    id <- as.character(entry$id %||% "")
    base_index <- match(id, base_ids)
    saved_index <- match(id, saved_ids)
    if (
      nzchar(id) &&
        !is.na(base_index) &&
        !is.na(saved_index) &&
        identical(entry, base[[base_index]])
    ) {
      saved[[saved_index]]
    } else {
      entry
    }
  })
  names(merged) <- names(live)
  merged
}

builder_project_live_dirty <- function(
  entries,
  checked_ids,
  manifest,
  identity_cache = NULL,
  ignored_ids = character()
) {
  if (!is.list(manifest) || !is.list(manifest$datasets)) {
    return(FALSE)
  }
  record_ids <- vapply(
    manifest$datasets,
    function(record) as.character(record$id),
    character(1)
  )
  records <- stats::setNames(manifest$datasets, record_ids)
  ids <- vapply(entries, `[[`, character(1), "id")
  if (anyDuplicated(ids) || any(!ids %in% names(records))) {
    return(TRUE)
  }
  saved_order <- names(sort(vapply(
    records,
    function(record) as.integer(record$order %||% 0L),
    integer(1)
  )))
  active_saved_order <- saved_order[vapply(
    records[saved_order],
    function(record) isTRUE(record$release$included %||% TRUE),
    logical(1)
  )]
  ignored_ids <- unique(as.character(ignored_ids %||% character()))
  ignored_ids <- ignored_ids[ignored_ids %in% active_saved_order]
  expected_live_order <- active_saved_order[
    !active_saved_order %in% ignored_ids
  ]
  if (!identical(ids, expected_live_order)) {
    return(TRUE)
  }
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    record <- records[[ids[[index]]]]
    if (
      !identical(
        ids[[index]] %in% checked_ids,
        builder_project_record_configuration_confirmed(record)
      )
    ) {
      return(TRUE)
    }
    live_revision <- .builder_project_integer(entry$revision %||% 0L)
    saved_revision <- .builder_project_integer(
      record$configuration$revision %||% 0L
    )
    if (!identical(live_revision, saved_revision)) {
      return(TRUE)
    }
    if (
      !identical(
        builder_project_cached_configuration_digest(entry, identity_cache),
        as.character(record$configuration$digest %||% "")
      )
    ) {
      return(TRUE)
    }
  }
  FALSE
}

## Whether an explicit Save has nothing to write: every live dataset's source is saved,
## the Project settings and last view match, and no attachment needs staging.
## The server adds the session checks (clean phase, nothing queued).
builder_project_save_unchanged <- function(
  manifest,
  entries,
  context,
  attachment_jobs = list()
) {
  if (
    !is.list(manifest) ||
      !is.list(manifest$datasets) ||
      length(attachment_jobs) ||
      !identical(
        suppressWarnings(as.integer(manifest$schema_version %||% NA_integer_)),
        .builder_project_schema_version
      ) ||
      (is.list(manifest$pending_build) &&
        !identical(manifest$pending_build$status %||% NULL, "running"))
  ) {
    return(FALSE)
  }
  records <- stats::setNames(
    manifest$datasets,
    vapply(
      manifest$datasets,
      function(record) as.character(record$id %||% ""),
      character(1)
    )
  )
  saved <- vapply(
    entries %||% list(),
    function(entry) {
      record <- records[[as.character(entry$id %||% "")]] %||% NULL
      configuration <- record$configuration %||% list()
      is.list(record) &&
        (identical(entry$load_state %||% "loaded", "artifact_ready") ||
          identical(record$source$status %||% NULL, "ready"))
    },
    logical(1)
  )
  same <- function(left, right) {
    canonical <- function(value) {
      jsonlite::toJSON(
        value %||% list(),
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = NA
      )
    }
    identical(as.character(canonical(left)), as.character(canonical(right)))
  }
  all(saved) &&
    same(context$configuration, manifest$configuration) &&
    same(context$last_ui, manifest$last_ui)
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
.builder_project_manifest_name <- "builder-project.json"
.builder_project_managed_root_names <- c(
  "artifacts",
  "checkpoints",
  "datasets",
  "session-sources",
  "spatial-assets",
  "sources"
)

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

.builder_project_stored_integer <- function(value, default = NULL) {
  value <- value %||% default
  valid <- length(value) == 1L &&
    !is.na(value) &&
    ((is.numeric(value) && is.finite(value) && value == floor(value)) ||
      (is.character(value) && grepl("^(0|[1-9][0-9]*)$", value)))
  parsed <- if (isTRUE(valid)) suppressWarnings(as.double(value)) else NA_real_
  if (!is.finite(parsed) || parsed < 0 || parsed > .Machine$integer.max) {
    stop("A persisted integer is invalid.", call. = FALSE)
  }
  as.integer(parsed)
}

.builder_project_contract_version <- function(value, default = NULL) {
  parsed <- tryCatch(
    .builder_project_stored_integer(value, default),
    error = function(error) NA_integer_
  )
  if (!identical(parsed, .builder_project_configuration_contract_version)) {
    stop("The dataset configuration contract is not supported.", call. = FALSE)
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

builder_project_reused_plan_matches <- function(saved, expected) {
  if (!is.list(saved) || !is.list(expected)) {
    return(FALSE)
  }
  fields <- c(
    "id",
    "filename",
    "sidecars",
    "viewer_bundle_assets",
    "private_assets",
    "viewer_bundle_asset_claims",
    "private_asset_claims"
  )
  all(vapply(
    fields,
    function(field) identical(saved[[field]], expected[[field]]),
    logical(1)
  ))
}

builder_project_id <- function() {
  paste0(
    "project-",
    format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
    "-",
    sprintf("%08x", sample.int(.Machine$integer.max, 1L))
  )
}

builder_project_manifest_path <- function(root) {
  file.path(root, .builder_project_manifest_name)
}

builder_project_normalize_root <- function(root, must_work = TRUE) {
  if (!.builder_project_text(root)) {
    stop("Choose a project folder.", call. = FALSE)
  }
  root <- normalizePath(
    path.expand(root),
    winslash = "/",
    mustWork = must_work
  )
  if (!dir.exists(root)) {
    stop("The project folder is not available.", call. = FALSE)
  }
  root
}

builder_project_manifest_lock_path <- function(root) {
  file.path(
    builder_project_normalize_root(root),
    ".builder-project-write.lock"
  )
}

.builder_project_manifest_lock_owned <- function(path, token) {
  if (
    !.builder_project_text(path) ||
      !.builder_project_text(token) ||
      !dir.exists(path) ||
      nzchar(tryCatch(Sys.readlink(path), error = function(error) ""))
  ) {
    return(FALSE)
  }
  expected <- c("owner-token", "owner.txt")
  entries <- tryCatch(
    list.files(path, all.files = TRUE, no.. = TRUE),
    error = function(error) NULL
  )
  if (!is.character(entries) || !setequal(entries, expected)) {
    return(FALSE)
  }
  metadata <- file.path(path, expected)
  if (
    !all(utils::file_test("-f", metadata)) ||
      any(vapply(
        metadata,
        function(candidate) {
          nzchar(tryCatch(Sys.readlink(candidate), error = function(error) ""))
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  sizes <- suppressWarnings(as.double(file.info(metadata)$size))
  if (
    anyNA(sizes) ||
      any(sizes < 1) ||
      sizes[[1L]] > 4096 ||
      sizes[[2L]] > 65536
  ) {
    return(FALSE)
  }
  stored <- tryCatch(
    readLines(metadata[[1L]], warn = FALSE),
    error = function(error) NULL
  )
  is.character(stored) &&
    length(stored) == 1L &&
    identical(stored, token)
}

builder_project_acquire_manifest_lock <- function(root) {
  root <- builder_project_normalize_root(root)
  path <- builder_project_manifest_lock_path(root)
  if (!dir.create(path, mode = "0700", showWarnings = FALSE)) {
    stop(
      paste0(
        "Another Builder window is saving this project, or its write lock ",
        "needs recovery. Wait and retry; if no save is running, inspect the ",
        "stale lock in the project folder."
      ),
      call. = FALSE
    )
  }
  token <- basename(tempfile(pattern = paste0("owner-", Sys.getpid(), "-")))
  token_path <- file.path(path, "owner-token")
  owner_path <- file.path(path, "owner.txt")
  host <- unname(Sys.info()[["nodename"]])
  if (!.builder_project_text(host)) {
    host <- "unknown"
  }
  initialized <- tryCatch(
    {
      writeLines(token, token_path, useBytes = TRUE)
      writeLines(
        c(
          "schema=1",
          paste0("pid=", Sys.getpid()),
          paste0("host=", encodeString(host, quote = '"')),
          paste0("acquired_at=", format(Sys.time(), tz = "UTC", usetz = TRUE))
        ),
        owner_path,
        useBytes = TRUE
      )
      Sys.chmod(c(token_path, owner_path), mode = "0600")
      TRUE
    },
    error = function(error) error
  )
  if (inherits(initialized, "condition")) {
    stop(
      paste0(
        "The Project write lock could not be initialized. An incomplete ",
        "lock may remain and should be inspected before retrying."
      ),
      call. = FALSE
    )
  }
  structure(
    list(path = path, token = token, root = root),
    class = c("builder_project_manifest_lock", "list")
  )
}

builder_project_release_manifest_lock <- function(lock) {
  if (
    !inherits(lock, "builder_project_manifest_lock") ||
      !.builder_project_text(lock$path %||% NULL) ||
      !.builder_project_text(lock$token %||% NULL)
  ) {
    warning(
      "The Project write lock could not be verified and was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (!dir.exists(lock$path)) {
    return(invisible(FALSE))
  }
  if (!.builder_project_manifest_lock_owned(lock$path, lock$token)) {
    warning(
      "The Project write lock ownership changed and was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  isolated <- tempfile(
    pattern = ".builder-project-write.lock-release-",
    tmpdir = dirname(lock$path)
  )
  if (!file.rename(lock$path, isolated)) {
    warning(
      "The Project write lock could not be isolated for release.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (!.builder_project_manifest_lock_owned(isolated, lock$token)) {
    if (!dir.exists(lock$path)) {
      file.rename(isolated, lock$path)
    }
    warning(
      "The Project write lock changed during release and was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  metadata <- file.path(isolated, c("owner-token", "owner.txt"))
  file.remove(metadata)
  if (
    any(file.exists(metadata)) ||
      unlink(isolated, recursive = TRUE, force = TRUE) != 0L
  ) {
    warning(
      "The isolated Project write lock could not be removed.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

builder_project_folder_state <- function(root) {
  root <- builder_project_normalize_root(root)
  if (file.exists(builder_project_manifest_path(root))) {
    return(list(
      kind = "project",
      root = root,
      managed_conflicts = character()
    ))
  }
  entries <- list.files(root, all.files = TRUE, no.. = TRUE)
  managed_conflicts <- entries[
    tolower(entries) %in% tolower(.builder_project_managed_root_names)
  ]
  list(
    kind = if (length(entries)) "nonempty" else "empty",
    root = root,
    managed_conflicts = managed_conflicts
  )
}

builder_project_relative_path <- function(path, root) {
  root <- builder_project_normalize_root(root)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop("A managed project file escaped the project folder.", call. = FALSE)
  }
  substring(path, nchar(prefix) + 1L)
}

builder_project_resolve_path <- function(
  path,
  root,
  kind = "managed",
  .root_normalized = FALSE
) {
  if (!.builder_project_text(path)) {
    return(NULL)
  }
  if (identical(kind, "external")) {
    return(normalizePath(path.expand(path), winslash = "/", mustWork = FALSE))
  }
  if (!isTRUE(.root_normalized)) {
    root <- builder_project_normalize_root(root)
  }
  normalized <- gsub("\\\\", "/", as.character(path), fixed = FALSE)
  if (
    grepl("^/", normalized) ||
      grepl("^[A-Za-z]:/", normalized) ||
      grepl("^//", normalized)
  ) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  parts <- strsplit(normalized, "/", fixed = TRUE)[[1L]]
  if (!length(parts) || any(parts %in% c("", ".", ".."))) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  candidate <- root
  for (part in parts) {
    candidate <- file.path(candidate, part)
    link <- tryCatch(Sys.readlink(candidate), error = function(error) "")
    if (.builder_project_text(link)) {
      stop(
        "A managed project path contains a symbolic link.",
        call. = FALSE
      )
    }
  }
  prefix <- paste0(root, "/")
  resolved <- if (file.exists(candidate) || dir.exists(candidate)) {
    normalizePath(candidate, winslash = "/", mustWork = TRUE)
  } else {
    candidate
  }
  if (!startsWith(resolved, prefix)) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  candidate
}

.builder_project_files_metadata <- function(paths) {
  paths <- as.character(paths)
  result <- rep(list(NULL), length(paths))
  valid <- !is.na(paths) & nzchar(paths)
  if (!any(valid)) {
    return(result)
  }
  stat <- tryCatch(
    fs::file_info(paths[valid], fail = FALSE),
    error = function(error) NULL
  )
  if (!is.data.frame(stat) || nrow(stat) != sum(valid)) {
    return(result)
  }
  result[which(valid)] <- lapply(seq_len(nrow(stat)), function(index) {
    if (!identical(as.character(stat$type[[index]]), "file")) {
      return(NULL)
    }
    stat_value <- function(name) {
      value <- suppressWarnings(as.double(stat[[name]][[index]]))
      if (length(value) == 1L && is.finite(value)) {
        sprintf("%.17g", value)
      } else {
        NULL
      }
    }
    changed_at <- stat_value("change_time")
    device_id <- stat_value("device_id")
    inode <- stat_value("inode")
    trusted <- .builder_project_text(changed_at) &&
      .builder_project_text(device_id) &&
      .builder_project_text(inode)
    list(
      bytes = as.double(stat$size[[index]]),
      modified_at = format(
        stat$modification_time[[index]],
        "%Y-%m-%dT%H:%M:%OS3Z",
        tz = "UTC"
      ),
      changed_at = changed_at,
      stat_version = if (trusted) 1L else NULL,
      file_id = if (trusted) paste(device_id, inode, sep = ":") else NULL,
      md5 = NULL
    )
  })
  result
}

builder_project_file_metadata <- function(path) {
  if (!.builder_project_text(path)) {
    return(NULL)
  }
  resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
  .builder_project_files_metadata(resolved)[[1L]]
}

builder_project_file_fingerprint <- function(
  path,
  content = FALSE,
  .md5 = tools::md5sum
) {
  before <- builder_project_file_metadata(path)
  if (is.null(before) || !isTRUE(content)) {
    return(before)
  }
  md5 <- unname(as.character(.md5(path)))
  after <- builder_project_file_metadata(path)
  stable <- is.list(after) &&
    builder_project_fingerprint_metadata_matches(before, after) &&
    builder_project_fingerprint_stat_matches(before, after)
  if (!stable || !.builder_project_text(md5)) {
    return(NULL)
  }
  after$md5 <- md5
  after
}

builder_project_file_fingerprint_with_md5 <- function(path, md5) {
  fingerprint <- builder_project_file_fingerprint(path, content = FALSE)
  if (is.null(fingerprint) || !.builder_project_text(md5)) {
    return(NULL)
  }
  fingerprint$md5 <- as.character(md5)
  fingerprint
}

builder_project_fingerprint_metadata_matches <- function(recorded, current) {
  if (is.null(recorded) || is.null(current)) {
    return(FALSE)
  }
  identical(
    suppressWarnings(as.double(recorded$bytes)),
    suppressWarnings(as.double(current$bytes))
  ) &&
    identical(
      as.character(recorded$modified_at %||% ""),
      as.character(current$modified_at %||% "")
    )
}

builder_project_fingerprint_stat_matches <- function(recorded, current) {
  if (!is.list(recorded) || !is.list(current)) {
    return(FALSE)
  }
  recorded_version <- suppressWarnings(as.integer(
    recorded$stat_version %||% NA_integer_
  ))
  current_version <- suppressWarnings(as.integer(
    current$stat_version %||% NA_integer_
  ))
  recorded_changed <- as.character(recorded$changed_at %||% "")
  current_changed <- as.character(current$changed_at %||% "")
  recorded_changed_value <- suppressWarnings(as.double(recorded_changed))
  current_changed_value <- suppressWarnings(as.double(current_changed))
  recorded_id <- as.character(recorded$file_id %||% "")
  current_id <- as.character(current$file_id %||% "")
  version_matches <- (identical(recorded_version, 1L) &&
    identical(current_version, 1L) &&
    .builder_project_text(recorded_id) &&
    identical(recorded_id, current_id)) ||
    (!identical(.Platform$OS.type, "windows") &&
      is.na(recorded_version) &&
      is.na(current_version))
  version_matches &&
    .builder_project_text(recorded_changed) &&
    is.finite(recorded_changed_value) &&
    is.finite(current_changed_value) &&
    identical(recorded_changed, current_changed)
}

builder_project_content_fingerprint_matches <- function(recorded, current) {
  if (!is.list(recorded) || !is.list(current)) {
    return(FALSE)
  }
  recorded_md5 <- recorded$md5 %||% NULL
  current_md5 <- current$md5 %||% NULL
  if (
    .builder_project_text(recorded_md5) && .builder_project_text(current_md5)
  ) {
    return(identical(as.character(recorded_md5), as.character(current_md5)))
  }
  is.null(recorded_md5) &&
    is.null(current_md5) &&
    builder_project_fingerprint_metadata_matches(recorded, current)
}

builder_project_recorded_fingerprint_md5 <- function(fingerprint) {
  if (!is.list(fingerprint)) {
    return(NULL)
  }
  md5 <- fingerprint$md5 %||% NULL
  if (
    !is.character(md5) ||
      length(md5) != 1L ||
      is.na(md5) ||
      !grepl("^[[:xdigit:]]{32}$", md5)
  ) {
    return(NULL)
  }
  tolower(md5)
}

builder_project_content_addressed_source <- function(path, id) {
  parts <- strsplit(as.character(path %||% ""), "/", fixed = TRUE)[[1L]]
  length(parts) >= 5L &&
    identical(parts[[1L]], "sources") &&
    identical(parts[[2L]], as.character(id %||% "")) &&
    identical(parts[[3L]], "blobs") &&
    grepl("^[[:xdigit:]]{32}$", parts[[4L]])
}

builder_project_verification_cache_key <- function(
  path,
  metadata,
  verification_cache
) {
  if (
    !is.environment(verification_cache) ||
      !builder_project_fingerprint_stat_matches(metadata, metadata)
  ) {
    return(NULL)
  }
  unclass(as.character(digest::digest(
    serialize(
      list(
        path = normalizePath(path, winslash = "/", mustWork = FALSE),
        bytes = metadata$bytes,
        modified_at = metadata$modified_at,
        changed_at = metadata$changed_at,
        file_id = metadata$file_id
      ),
      NULL,
      version = 3L
    ),
    algo = "md5",
    serialize = FALSE
  )))
}

## Stat-keyed verification results (managed file MD5s, configuration sidecar
## checks) kept between Project transactions. Sidecars are immutable and
## content-addressed, and a key covers path, size, mtime, ctime and file id,
## so any rewrite misses it. A warm Project writer therefore checks an
## unchanged Project with a few stat calls instead of re-reading and
## re-hashing every sidecar on every Save.
.builder_project_write_verification_cache <- new.env(parent = emptyenv())

builder_project_write_verification_cache <- function(
  cache = .builder_project_write_verification_cache,
  limit = 4096L
) {
  keys <- ls(cache, all.names = TRUE)
  if (length(keys) > limit) {
    rm(list = keys, envir = cache)
  }
  cache
}

builder_project_cached_verified_fingerprint <- function(
  path,
  verification_cache
) {
  metadata <- builder_project_file_metadata(path)
  cache_key <- builder_project_verification_cache_key(
    path,
    metadata,
    verification_cache
  )
  current <- if (.builder_project_text(cache_key)) {
    get0(cache_key, envir = verification_cache, inherits = FALSE)
  } else {
    NULL
  }
  if (
    is.list(current) &&
      builder_project_fingerprint_metadata_matches(current, metadata) &&
      builder_project_fingerprint_stat_matches(current, metadata)
  ) {
    current
  } else {
    NULL
  }
}

builder_project_managed_file_matches <- function(
  recorded,
  path,
  verify_content = TRUE,
  metadata = NULL,
  verification_cache = NULL
) {
  recorded_md5 <- builder_project_recorded_fingerprint_md5(recorded)
  if (!.builder_project_text(recorded_md5)) {
    return(FALSE)
  }
  metadata <- metadata %||%
    builder_project_file_fingerprint(path, content = FALSE)
  metadata_matches <- builder_project_fingerprint_metadata_matches(
    recorded,
    metadata
  )
  if (
    metadata_matches &&
      builder_project_fingerprint_stat_matches(recorded, metadata)
  ) {
    return(TRUE)
  }
  if (!isTRUE(verify_content)) {
    return(FALSE)
  }
  cache_key <- builder_project_verification_cache_key(
    path,
    metadata,
    verification_cache
  )
  current <- if (.builder_project_text(cache_key)) {
    get0(cache_key, envir = verification_cache, inherits = FALSE)
  } else {
    NULL
  }
  if (!is.list(current)) {
    current <- builder_project_file_fingerprint(path, content = TRUE)
    if (.builder_project_text(cache_key) && is.list(current)) {
      assign(cache_key, current, envir = verification_cache)
    }
  }
  current_md5 <- builder_project_recorded_fingerprint_md5(current)
  .builder_project_text(current_md5) && identical(recorded_md5, current_md5)
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

builder_project_source_fingerprint_md5 <- function(fingerprint) {
  if (
    !.builder_project_text(fingerprint) ||
      !startsWith(as.character(fingerprint), "builder-snapshot-v2:")
  ) {
    return(NULL)
  }
  md5 <- sub("^.*:", "", as.character(fingerprint))
  if (grepl("^[[:xdigit:]]{32}$", md5)) tolower(md5) else NULL
}

builder_project_snapshot_source_md5 <- function(entry) {
  builder_project_source_fingerprint_md5(
    entry$snapshot$source_fingerprint %||% NULL
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

builder_project_source_job <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  source <- entry$path %||% NULL
  filename <- entry$filename %||%
    if (.builder_project_text(source)) {
      basename(source)
    } else {
      NULL
    }
  if (!.builder_project_text(filename)) {
    filename <- paste0(entry$id, ".rds")
  }
  filename <- basename(filename)
  storage_name <- builder_project_source_storage_name(filename)
  source_md5 <- builder_project_snapshot_source_md5(entry)
  relative <- if (.builder_project_text(source_md5)) {
    paste("sources", entry$id, "blobs", source_md5, storage_name, sep = "/")
  } else {
    paste("sources", entry$id, storage_name, sep = "/")
  }
  target <- builder_project_resolve_path(relative, root, "managed")
  list(
    id = entry$id,
    source = source,
    root = root,
    relative = relative,
    target = target,
    part = paste0(target, ".part"),
    filename = filename,
    storage_name = storage_name,
    source_md5 = source_md5,
    origin = entry$source_origin %||%
      if (!is.null(entry$example)) {
        "example"
      } else {
        "upload"
      },
    example = entry$example %||% NULL
  )
}

builder_project_prepare_source <- function(entry, root, prior = NULL) {
  job <- builder_project_source_job(entry, root)
  relative <- job$relative
  prior_source <- if (is.list(prior)) prior$source %||% list() else list()
  prior_path <- if (
    identical(prior_source$kind %||% NULL, "managed") &&
      .builder_project_text(prior_source$path %||% NULL)
  ) {
    tryCatch(
      builder_project_resolve_path(prior_source$path, root, "managed"),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  entry_path <- if (.builder_project_text(entry$path %||% NULL)) {
    tryCatch(
      normalizePath(entry$path, winslash = "/", mustWork = TRUE),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  prior_parts <- strsplit(
    gsub("\\\\", "/", as.character(prior_source$path %||% "")),
    "/",
    fixed = TRUE
  )[[1L]]
  prior_blob_md5 <- if (
    length(prior_parts) == 5L && identical(prior_parts[[3L]], "blobs")
  ) {
    tolower(prior_parts[[4L]])
  } else {
    ""
  }
  prior_content_matches <-
    identical(prior_source$status %||% NULL, "ready") &&
    builder_project_content_addressed_source(
      prior_source$path %||% NULL,
      entry$id
    ) &&
    .builder_project_text(job$source_md5) &&
    identical(prior_blob_md5, tolower(as.character(job$source_md5))) &&
    identical(
      tolower(as.character(job$source_md5)),
      tolower(as.character(prior_source$fingerprint$md5 %||% ""))
    )
  prior_ready <- .builder_project_text(prior_path) &&
    file.exists(prior_path) &&
    !dir.exists(prior_path) &&
    ((identical(
      entry_path,
      normalizePath(prior_path, winslash = "/", mustWork = TRUE)
    ) &&
      (!.builder_project_text(job$source_md5) || prior_content_matches)) ||
      prior_content_matches)
  ready_target <- if (prior_ready) prior_path else job$target
  if (prior_ready) {
    relative <- prior_source$path
  }
  target_ready <- file.exists(ready_target) && !dir.exists(ready_target)
  if (target_ready) {
    current_metadata <- builder_project_file_fingerprint(
      ready_target,
      content = FALSE
    )
    prior_fingerprint <- prior_source$fingerprint %||% NULL
    verified_fingerprint <- entry$project_source_fingerprint %||% NULL
    fingerprint <- if (
      !is.null(entry_path) &&
        identical(
          entry_path,
          normalizePath(ready_target, winslash = "/", mustWork = TRUE)
        ) &&
        .builder_project_text(verified_fingerprint$md5 %||% NULL) &&
        builder_project_managed_file_matches(
          verified_fingerprint,
          ready_target,
          verify_content = FALSE,
          metadata = current_metadata
        )
    ) {
      verified_fingerprint
    } else if (
      isTRUE(prior_ready) &&
        !is.null(entry_path) &&
        identical(
          entry_path,
          normalizePath(ready_target, winslash = "/", mustWork = TRUE)
        ) &&
        identical(prior_source$status %||% NULL, "ready") &&
        builder_project_content_addressed_source(
          prior_source$path %||% NULL,
          entry$id
        ) &&
        is.list(prior_fingerprint)
    ) {
      ## Opening a Project may deliberately defer hashing a legacy multi-GB
      ## source until it is loaded.  Saving an unchanged reference is not a new
      ## integrity assertion: retain the exact descriptor and let restore/build
      ## perform the content check.  A loaded source takes the verified branch
      ## above and upgrades this descriptor to the cheap stat identity.
      prior_fingerprint
    } else if (
      identical(as.character(prior_source$path %||% ""), relative) &&
        .builder_project_text(prior_fingerprint$md5 %||% NULL) &&
        builder_project_managed_file_matches(
          prior_fingerprint,
          ready_target,
          verify_content = FALSE,
          metadata = current_metadata
        )
    ) {
      prior_fingerprint
    } else {
      builder_project_file_fingerprint(ready_target, content = TRUE)
    }
    if (
      .builder_project_text(job$source_md5) &&
        (!.builder_project_text(fingerprint$md5 %||% NULL) ||
          !identical(
            tolower(as.character(fingerprint$md5)),
            tolower(as.character(job$source_md5))
          ))
    ) {
      stop(
        "An immutable Project source failed its integrity check.",
        call. = FALSE
      )
    }
    return(list(
      entry = entry,
      source = list(
        kind = "managed",
        origin = job$origin,
        example = job$example,
        filename = job$filename,
        path = relative,
        status = "ready",
        fingerprint = fingerprint
      ),
      job = NULL
    ))
  }
  if (!.builder_project_text(job$source) || !file.exists(job$source)) {
    source <- prior_source
    source$status <- "failed"
    source$origin <- source$origin %||% job$origin
    source$example <- source$example %||% job$example
    source$filename <- source$filename %||% job$filename
    source$path <- source$path %||% relative
    source$error <- "The dataset source is no longer available."
    return(list(entry = entry, source = source, job = NULL))
  }
  list(
    entry = entry,
    source = list(
      kind = "managed",
      origin = job$origin,
      example = job$example,
      filename = job$filename,
      path = relative,
      status = "pending",
      fingerprint = NULL
    ),
    job = job
  )
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

builder_project_publish_verified_file <- function(part, target, md5, message) {
  staged <- builder_project_file_metadata(part)
  if (
    is.null(staged) ||
      !.builder_project_text(md5) ||
      !grepl("^[[:xdigit:]]{32}$", md5)
  ) {
    stop(message, call. = FALSE)
  }
  fingerprint <- NULL
  if (!file.exists(target)) {
    moved <- file.rename(part, target)
    if (isTRUE(moved)) {
      published <- builder_project_file_metadata(target)
      same_file <-
        builder_project_fingerprint_metadata_matches(staged, published) &&
        identical(staged$stat_version %||% NULL, 1L) &&
        identical(published$stat_version %||% NULL, 1L) &&
        .builder_project_text(staged$file_id %||% NULL) &&
        identical(staged$file_id, published$file_id)
      if (isTRUE(same_file)) {
        published$md5 <- md5
        fingerprint <- published
      }
    } else if (!file.exists(target)) {
      stop(message, call. = FALSE)
    }
  }
  if (is.null(fingerprint)) {
    fingerprint <- builder_project_file_fingerprint(target, content = TRUE)
  }
  if (
    !is.list(fingerprint) ||
      !identical(tolower(as.character(fingerprint$md5 %||% "")), tolower(md5))
  ) {
    stop(message, call. = FALSE)
  }
  unlink(part, force = TRUE)
  fingerprint
}

## Incremental MD5 so a streamed copy is hashed while its bytes pass through.
## Uses openssl's digest state when that package is installed; NULL means
## the caller hashes the finished file once with tools::md5sum().
.builder_project_md5_stream <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    return(NULL)
  }
  namespace <- getNamespace("openssl")
  functions <- lapply(
    c(init = "md_init", feed = "md_feed", final = "md_final"),
    function(name) {
      get0(name, envir = namespace, mode = "function", inherits = FALSE)
    }
  )
  if (!all(vapply(functions, is.function, logical(1)))) {
    return(NULL)
  }
  state <- tryCatch(functions$init("md5"), error = function(error) NULL)
  if (is.null(state)) {
    return(NULL)
  }
  list(
    feed = function(bytes) functions$feed(state, bytes),
    final = function() {
      tolower(paste(as.character(functions$final(state)), collapse = ""))
    }
  )
}

## MD5 of a file, read in chunks so `progress(bytes)` can report as it goes.
builder_project_stream_md5 <- function(
  path,
  progress = NULL,
  buffer_bytes = 16L * 1024L^2
) {
  stream <- .builder_project_md5_stream()
  if (is.null(stream)) {
    md5 <- unname(tolower(as.character(tools::md5sum(path))))
    if (is.function(progress)) {
      progress(suppressWarnings(as.double(file.info(path)$size[[1L]])))
    }
    return(md5)
  }
  input <- file(path, open = "rb")
  on.exit(close(input), add = TRUE)
  done <- 0
  repeat {
    bytes <- readBin(input, what = "raw", n = as.integer(buffer_bytes))
    if (!length(bytes)) {
      break
    }
    stream$feed(bytes)
    done <- done + length(bytes)
    if (is.function(progress)) {
      progress(done)
    }
  }
  stream$final()
}

## Copies a file to a new destination, hashing while streaming and reporting
## `progress(bytes)`. Same contract as builder_copy_file_verified() in io.R.
builder_project_stream_copy <- function(
  source,
  destination,
  expected_md5 = NULL,
  progress = NULL,
  buffer_bytes = 16L * 1024L^2
) {
  if (
    !.builder_project_text(source) ||
      !file.exists(source) ||
      dir.exists(source) ||
      !.builder_project_text(destination) ||
      file.exists(destination)
  ) {
    stop(
      "A verified copy requires one file and a new destination.",
      call. = FALSE
    )
  }
  expected_md5 <- if (is.null(expected_md5)) NULL else tolower(expected_md5)
  if (
    !is.null(expected_md5) &&
      (length(expected_md5) != 1L ||
        !grepl("^[[:xdigit:]]{32}$", expected_md5))
  ) {
    stop("The expected file MD5 is invalid.", call. = FALSE)
  }
  expected_bytes <- suppressWarnings(as.double(file.info(source)$size[[1L]]))
  input <- output <- NULL
  complete <- FALSE
  on.exit(
    {
      if (!is.null(input)) {
        try(close(input), silent = TRUE)
      }
      if (!is.null(output)) {
        try(close(output), silent = TRUE)
      }
      if (!complete) unlink(destination, force = TRUE)
    },
    add = TRUE
  )
  if (
    identical(unname(Sys.info()[["sysname"]]), "Darwin") &&
      file.exists("/bin/cp")
  ) {
    # APFS clones share blocks; only the verification read remains.
    status <- suppressWarnings(system2(
      "/bin/cp",
      c("-c", "--", shQuote(source), shQuote(destination)),
      stdout = FALSE,
      stderr = FALSE
    ))
    if (isTRUE(status == 0L) && file.exists(destination)) {
      cloned <- suppressWarnings(as.double(file.info(destination)$size[[1L]]))
      actual_md5 <- builder_project_stream_md5(destination, progress = progress)
      if (
        !is.finite(expected_bytes) ||
          !identical(cloned, expected_bytes) ||
          !grepl("^[[:xdigit:]]{32}$", actual_md5) ||
          (!is.null(expected_md5) && !identical(actual_md5, expected_md5))
      ) {
        stop("The cloned file could not be verified.", call. = FALSE)
      }
      complete <- TRUE
      return(list(path = destination, bytes = cloned, md5 = actual_md5))
    }
    unlink(destination, force = TRUE)
  }
  stream <- .builder_project_md5_stream()
  input <- file(source, open = "rb")
  output <- file(destination, open = "wb")
  copied <- 0
  repeat {
    bytes <- readBin(input, what = "raw", n = as.integer(buffer_bytes))
    if (!length(bytes)) {
      break
    }
    writeBin(bytes, output)
    if (!is.null(stream)) {
      stream$feed(bytes)
    }
    copied <- copied + length(bytes)
    if (is.function(progress)) {
      progress(copied)
    }
  }
  close(input)
  close(output)
  input <- output <- NULL
  if (
    !is.finite(expected_bytes) || !identical(as.double(copied), expected_bytes)
  ) {
    stop(
      "The source file ended before the verified copy completed.",
      call. = FALSE
    )
  }
  actual_md5 <- if (is.null(stream)) {
    unname(tolower(as.character(tools::md5sum(destination))))
  } else {
    stream$final()
  }
  if (!is.null(expected_md5) && !identical(actual_md5, expected_md5)) {
    stop("The copied file does not match its saved MD5.", call. = FALSE)
  }
  complete <- TRUE
  list(path = destination, bytes = as.double(copied), md5 = actual_md5)
}

## Whether a source path is the Builder's own retained session copy (under
## `<session root>/session-sources/`, without links), which a Save may move
## into the Project instead of copying.
builder_project_session_owned_source <- function(path, session_root) {
  if (
    !.builder_project_text(path) ||
      !.builder_project_text(session_root) ||
      !dir.exists(session_root) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    return(FALSE)
  }
  isTRUE(tryCatch(
    {
      rebased <- .builder_project_rebase_lexical_path(path, session_root)
      source_root <- file.path(rebased$root, "session-sources")
      dir.exists(source_root) &&
        !nzchar(Sys.readlink(source_root)) &&
        startsWith(rebased$path, paste0(source_root, "/")) &&
        !.builder_project_path_has_link_within(rebased$path, rebased$root)
    },
    error = function(error) FALSE
  ))
}

## Live entries that still point at a session copy moved into the Project
## now point at its Project blob.
builder_project_relocate_moved_sources <- function(entries, results) {
  moved <- Filter(
    function(result) {
      identical(result$status, "ready") &&
        .builder_project_text(result$moved_from %||% NULL) &&
        .builder_project_text(result$path %||% NULL)
    },
    results %||% list()
  )
  if (!length(moved)) {
    return(entries)
  }
  from <- vapply(
    moved,
    function(result) {
      normalizePath(result$moved_from, winslash = "/", mustWork = FALSE)
    },
    character(1)
  )
  lapply(entries, function(entry) {
    path <- entry$path %||% NULL
    if (!.builder_project_text(path)) {
      return(entry)
    }
    index <- match(normalizePath(path, winslash = "/", mustWork = FALSE), from)
    if (!is.na(index) && !file.exists(path)) {
      entry$path <- normalizePath(
        moved[[index]]$path,
        winslash = "/",
        mustWork = TRUE
      )
    }
    entry
  })
}

## Fingerprints of sources this process published or verified, keyed by path
## and stat identity. A warm copier keeps it between calls, so a job queued
## again for an already-published blob is skipped without re-reading it.
.builder_project_source_verification_cache <- new.env(parent = emptyenv())

## The content-addressed target of a job whose source MD5 is known, when it
## already exists with that content.
builder_project_verified_source_target <- function(
  job,
  verification_cache = .builder_project_source_verification_cache
) {
  md5 <- builder_project_recorded_fingerprint_md5(list(
    md5 = job$source_md5 %||% NULL
  ))
  if (
    !.builder_project_text(md5) ||
      !builder_project_content_addressed_source(job$relative, job$id) ||
      !.builder_project_text(job$target) ||
      !file.exists(job$target) ||
      dir.exists(job$target)
  ) {
    return(NULL)
  }
  fingerprint <- builder_project_cached_verified_fingerprint(
    job$target,
    verification_cache
  )
  if (is.null(fingerprint)) {
    fingerprint <- builder_project_file_fingerprint(job$target, content = TRUE)
    cache_key <- builder_project_verification_cache_key(
      job$target,
      fingerprint,
      verification_cache
    )
    if (.builder_project_text(cache_key) && is.list(fingerprint)) {
      assign(cache_key, fingerprint, envir = verification_cache)
    }
  }
  if (
    !is.list(fingerprint) ||
      !identical(tolower(as.character(fingerprint$md5 %||% "")), md5)
  ) {
    return(NULL)
  }
  fingerprint
}

## Identifies the content a source job would publish, so a job already
## running (or queued) for the same dataset and bytes is not copied twice.
builder_project_source_job_key <- function(job) {
  paste(
    as.character(job$id %||% ""),
    tolower(as.character(job$source_md5 %||% "")),
    as.character(job$source %||% ""),
    as.character(job$relative %||% ""),
    sep = "\r"
  )
}

## Adds jobs to the queue, keyed by dataset. A job identical to one already
## running is dropped: the running copy's result is merged into the latest
## manifest when it finishes.
builder_project_source_queue_add <- function(queued, jobs, running = list()) {
  running_keys <- vapply(
    running %||% list(),
    builder_project_source_job_key,
    character(1)
  )
  for (job in jobs %||% list()) {
    if (builder_project_source_job_key(job) %in% running_keys) {
      queued[[job$id]] <- NULL
      next
    }
    queued[[job$id]] <- job
  }
  queued
}

builder_project_copy_source_job <- function(job, progress = NULL) {
  moved <- NULL
  fail <- function(message) {
    if (is.list(moved) && file.exists(job$part) && !file.exists(job$source)) {
      # Never discard the session copy: put it back where the entry expects.
      suppressWarnings(file.rename(job$part, job$source))
    }
    if (is.character(job$part) && length(job$part) == 1L) {
      unlink(job$part, force = TRUE)
    }
    list(
      id = job$id,
      status = "failed",
      error = message,
      planned = planned
    )
  }
  planned <- if (is.list(job)) job$relative %||% NULL else NULL
  verified <- if (is.list(job)) {
    tryCatch(
      builder_project_verified_source_target(job),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  if (is.list(verified)) {
    return(list(
      id = job$id,
      status = "ready",
      path = job$target,
      fingerprint = verified,
      planned = planned
    ))
  }
  if (
    !is.list(job) ||
      !.builder_project_identifier(job$id) ||
      !.builder_project_text(job$source) ||
      !file.exists(job$source)
  ) {
    return(fail("The dataset source is no longer available."))
  }
  target_dir <- dirname(job$target)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    return(fail("The Project source directory could not be created."))
  }
  unlink(job$part, force = TRUE)
  moved <- builder_project_move_session_source(job, progress)
  if (is.list(moved) && identical(moved$status, "failed")) {
    return(fail(moved$error))
  }
  if (is.list(moved)) {
    copied <- moved
  } else {
    before <- builder_project_file_metadata(job$source)
    copied <- tryCatch(
      builder_project_stream_copy(
        job$source,
        job$part,
        expected_md5 = job$source_md5 %||% NULL,
        progress = progress
      ),
      error = identity
    )
    after <- builder_project_file_metadata(job$source)
    if (
      inherits(copied, "condition") ||
        !builder_project_fingerprint_metadata_matches(before, after) ||
        !builder_project_fingerprint_stat_matches(before, after)
    ) {
      return(fail(
        if (inherits(copied, "condition")) {
          conditionMessage(copied)
        } else {
          "The dataset source changed while it was copied."
        }
      ))
    }
  }
  source_md5 <- copied$md5
  job$relative <- paste(
    "sources",
    job$id,
    "blobs",
    source_md5,
    job$storage_name %||% builder_project_source_storage_name(job$filename),
    sep = "/"
  )
  job$target <- tryCatch(
    builder_project_resolve_path(job$relative, job$root, "managed"),
    error = identity
  )
  if (inherits(job$target, "condition")) {
    return(fail(conditionMessage(job$target)))
  }
  target_dir <- dirname(job$target)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    return(fail("The Project source directory could not be created."))
  }
  fingerprint <- tryCatch(
    builder_project_publish_verified_file(
      job$part,
      job$target,
      source_md5,
      "An immutable Project source failed its integrity check."
    ),
    error = identity
  )
  if (inherits(fingerprint, "condition")) {
    return(fail(conditionMessage(fingerprint)))
  }
  cache_key <- builder_project_verification_cache_key(
    job$target,
    fingerprint,
    .builder_project_source_verification_cache
  )
  if (.builder_project_text(cache_key)) {
    assign(
      cache_key,
      fingerprint,
      envir = .builder_project_source_verification_cache
    )
  }
  list(
    id = job$id,
    status = "ready",
    path = job$target,
    fingerprint = fingerprint,
    planned = planned,
    moved_from = if (is.list(moved)) job$source else NULL
  )
}

## Moves the Builder's own session copy into the Project (same volume) instead
## of copying it and deleting the session copy later. Only content-addressed
## jobs qualify, so a later Save finds the blob even if the move outlives the
## entry that pointed at the session copy. Returns NULL to fall back to a
## copy, a "failed" result, or the copy-shaped result with the verified MD5.
builder_project_move_session_source <- function(job, progress = NULL) {
  md5 <- builder_project_recorded_fingerprint_md5(list(
    md5 = job$source_md5 %||% NULL
  ))
  if (
    !isTRUE(job$session_owned) ||
      !.builder_project_text(md5) ||
      !builder_project_content_addressed_source(job$relative, job$id)
  ) {
    return(NULL)
  }
  before <- builder_project_file_metadata(job$source)
  if (
    is.null(before) ||
      !isTRUE(suppressWarnings(file.rename(job$source, job$part)))
  ) {
    return(NULL)
  }
  restore <- function(message) {
    restored <- isTRUE(suppressWarnings(file.rename(job$part, job$source)))
    list(
      status = "failed",
      error = if (restored) {
        message
      } else {
        paste(message, "The session copy is at:", job$part)
      }
    )
  }
  staged <- builder_project_file_metadata(job$part)
  if (
    !builder_project_fingerprint_metadata_matches(before, staged) ||
      !identical(before$file_id %||% NULL, staged$file_id %||% NULL)
  ) {
    return(restore("The dataset source changed while it was moved."))
  }
  # The bytes were never rewritten, but a new immutable blob is still
  # checked against its recorded MD5 (one read, reported as progress).
  actual <- tryCatch(
    builder_project_stream_md5(job$part, progress = progress),
    error = identity
  )
  after <- builder_project_file_metadata(job$part)
  if (
    inherits(actual, "condition") ||
      !builder_project_fingerprint_metadata_matches(staged, after)
  ) {
    return(restore("The dataset source changed while it was verified."))
  }
  if (!identical(actual, md5)) {
    return(restore("The moved file does not match its saved MD5."))
  }
  list(
    status = "moved",
    path = job$part,
    bytes = as.double(after$bytes),
    md5 = actual
  )
}

builder_project_copy_source_jobs <- function(
  jobs,
  progress_path = NULL,
  progress_interval = 0.25
) {
  results <- vector("list", length(jobs))
  failed <- 0L
  sizes <- vapply(
    jobs,
    function(job) {
      size <- suppressWarnings(as.double(file.info(job$source %||% "")$size[[
        1L
      ]]))
      if (is.finite(size)) size else 0
    },
    numeric(1)
  )
  bytes_total <- sum(sizes)
  bytes_finished <- 0
  last_written <- -Inf
  write_progress <- function(
    completed,
    bytes_done,
    current = NULL,
    last = NULL
  ) {
    if (!.builder_project_text(progress_path)) {
      return(invisible(FALSE))
    }
    temporary <- paste0(progress_path, ".tmp")
    saveRDS(
      list(
        completed = completed,
        total = length(jobs),
        failed = failed,
        bytes_done = min(bytes_total, bytes_done),
        bytes_total = bytes_total,
        current = current,
        last = last
      ),
      temporary,
      version = 3L
    )
    moved <- try(fs::file_move(temporary, progress_path), silent = TRUE)
    if (inherits(moved, "try-error")) {
      unlink(temporary, force = TRUE)
    }
    last_written <<- as.numeric(Sys.time())
    invisible(TRUE)
  }
  for (index in seq_along(jobs)) {
    job <- jobs[[index]]
    current <- as.character(job$filename %||% basename(job$source %||% ""))
    progress <- function(bytes) {
      now <- as.numeric(Sys.time())
      if (now - last_written >= progress_interval) {
        write_progress(index - 1L, bytes_finished + bytes, current)
      }
    }
    results[[index]] <- builder_project_copy_source_job(
      job,
      progress = progress
    )
    failed <- failed + as.integer(identical(results[[index]]$status, "failed"))
    bytes_finished <- bytes_finished + sizes[[index]]
    write_progress(
      index,
      bytes_finished,
      last = results[[index]][intersect(
        c("id", "status", "error"),
        names(results[[index]])
      )]
    )
  }
  results
}

## `only_pending = TRUE` merges a finished copy into a manifest written after
## the copy started: a result is applied only to a record still waiting for
## that same planned source, never over a newer source choice.
builder_project_apply_source_results <- function(
  manifest,
  results,
  root,
  only_pending = FALSE
) {
  if (!is.list(manifest) || !is.list(manifest$datasets)) {
    stop("A Builder project manifest is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  result_map <- stats::setNames(
    results,
    vapply(results, function(result) as.character(result$id), character(1))
  )
  manifest$datasets <- lapply(manifest$datasets, function(record) {
    result <- result_map[[record$id]] %||% NULL
    if (is.null(result)) {
      return(record)
    }
    if (
      isTRUE(only_pending) &&
        (!identical(record$source$status %||% NULL, "pending") ||
          (!is.null(result$planned) &&
            !identical(
              as.character(record$source$path %||% ""),
              as.character(result$planned)
            )))
    ) {
      return(record)
    }
    source <- record$source %||% list(kind = "managed")
    source$status <- result$status %||% "failed"
    source$error <- result$error %||% NULL
    if (identical(source$status, "ready")) {
      path <- normalizePath(result$path, winslash = "/", mustWork = TRUE)
      prefix <- paste0(root, "/")
      if (!startsWith(path, prefix)) {
        stop("A synchronized source escaped the Project folder.", call. = FALSE)
      }
      source$path <- substring(path, nchar(prefix) + 1L)
      source$fingerprint <- result$fingerprint
    }
    record$source <- source
    record
  })
  manifest
}

builder_project_source_context <- function(
  project,
  generation,
  active_ids = character()
) {
  if (
    !is.list(project) ||
      !is.list(project$manifest) ||
      !is.list(project$manifest$project)
  ) {
    stop(
      "A source synchronization requires a saved project identity.",
      call. = FALSE
    )
  }
  project_id <- project$manifest$project$id %||% NULL
  revision <- project$manifest$project$revision %||% NULL
  if (
    !.builder_project_identifier(project_id) ||
      !is.numeric(generation) ||
      length(generation) != 1L ||
      is.na(generation) ||
      !is.finite(generation) ||
      generation < 0 ||
      !is.numeric(revision) ||
      length(revision) != 1L ||
      is.na(revision) ||
      !is.finite(revision) ||
      revision < 0
  ) {
    stop(
      "A source synchronization requires a saved project identity.",
      call. = FALSE
    )
  }
  root <- builder_project_normalize_root(project$root)
  path <- normalizePath(
    project$path %||% builder_project_manifest_path(root),
    winslash = "/",
    mustWork = FALSE
  )
  active_ids <- unique(as.character(active_ids %||% character()))
  if (
    length(active_ids) &&
      any(
        !vapply(
          active_ids,
          .builder_project_identifier,
          logical(1)
        )
      )
  ) {
    stop("Source synchronization dataset ids are invalid.", call. = FALSE)
  }
  list(
    generation = as.double(generation),
    project_id = as.character(project_id),
    root = root,
    path = path,
    revision = as.integer(revision),
    active_ids = active_ids
  )
}

builder_project_source_context_matches <- function(
  context,
  project,
  generation
) {
  if (!is.list(context) || !is.list(project)) {
    return(FALSE)
  }
  current <- tryCatch(
    builder_project_source_context(project, generation),
    error = function(error) NULL
  )
  # The revision is deliberately not compared: autosave, Save and CRB
  # registration commit new revisions while a copy runs, and its results are
  # merged into whichever manifest is current when it finishes.
  is.list(current) &&
    all(vapply(
      c("generation", "project_id", "root", "path"),
      function(field) identical(context[[field]], current[[field]]),
      logical(1)
    ))
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

builder_project_release_session_source <- function(path, session_root) {
  if (
    !.builder_project_text(path) ||
      !.builder_project_text(session_root) ||
      !dir.exists(session_root) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    return(FALSE)
  }
  if (
    nzchar(tryCatch(
      Sys.readlink(path.expand(session_root)),
      error = function(error) ""
    ))
  ) {
    return(FALSE)
  }
  rebased <- .builder_project_rebase_lexical_path(path, session_root)
  root <- rebased$root
  source_root <- file.path(root, "session-sources")
  if (!dir.exists(source_root) || nzchar(Sys.readlink(source_root))) {
    return(FALSE)
  }
  candidate <- rebased$path
  if (
    !startsWith(candidate, paste0(source_root, "/")) ||
      .builder_project_path_has_link_within(candidate, root)
  ) {
    return(FALSE)
  }
  unlink(candidate, force = TRUE)
  if (file.exists(candidate)) {
    return(FALSE)
  }
  dataset_dir <- dirname(candidate)
  if (
    dir.exists(dataset_dir) &&
      identical(dirname(dataset_dir), source_root) &&
      !length(list.files(dataset_dir, all.files = TRUE, no.. = TRUE))
  ) {
    unlink(dataset_dir, recursive = TRUE, force = TRUE)
  }
  TRUE
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

builder_project_commit_source_entries <- function(
  entries,
  manifest,
  results,
  root,
  session_root = NULL
) {
  records <- stats::setNames(
    manifest$datasets %||% list(),
    vapply(
      manifest$datasets %||% list(),
      function(record) {
        as.character(record$id)
      },
      character(1)
    )
  )
  ready <- Filter(function(result) identical(result$status, "ready"), results)
  ready <- stats::setNames(
    ready,
    vapply(
      ready,
      function(result) as.character(result$id),
      character(1)
    )
  )
  committed <- lapply(entries, function(entry) {
    result <- ready[[entry$id]] %||% NULL
    record <- records[[entry$id]] %||% NULL
    if (is.null(result) || !is.list(record$source)) {
      return(list(entry = entry, released = NULL))
    }
    managed <- builder_project_resolve_path(record$source$path, root, "managed")
    if (
      !file.exists(managed) ||
        dir.exists(managed) ||
        !identical(
          normalizePath(result$path, winslash = "/", mustWork = TRUE),
          normalizePath(managed, winslash = "/", mustWork = TRUE)
        )
    ) {
      return(list(entry = entry, released = NULL))
    }
    prior <- entry$path %||% NULL
    entry$path <- normalizePath(managed, winslash = "/", mustWork = TRUE)
    entry$source <- record$source
    released <- if (
      .builder_project_text(session_root %||% NULL) &&
        .builder_project_text(prior %||% NULL) &&
        is.list(entry$snapshot %||% NULL) &&
        builder_project_release_session_source(prior, session_root)
    ) {
      entry$id
    } else {
      NULL
    }
    list(entry = entry, released = released)
  })
  list(
    entries = lapply(committed, `[[`, "entry"),
    released = unique(as.character(unlist(lapply(
      committed,
      `[[`,
      "released"
    ))))
  )
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

## Collect only file descriptors on the Shiny process. Decoding, hashing and
## copying new tissue images belongs to the same worker as table attachments.
builder_project_attachment_jobs <- function(entries, root) {
  jobs <- builder_project_table_asset_jobs(entries, root)
  for (entry in entries) {
    .builder_project_map_spatial_images(
      entry,
      function(record, section, label) {
        source <- record$source_path %||% ""
        asset <- record$project_asset
        managed <- if (is.list(asset)) {
          builder_project_resolve_path(asset$path, root, "managed")
        } else {
          NULL
        }
        if (
          .builder_project_text(managed) &&
            file.exists(managed) &&
            builder_project_fingerprint_metadata_matches(
              asset$fingerprint,
              builder_project_file_fingerprint(managed, content = FALSE)
            ) &&
            (!.builder_project_text(source) ||
              identical(
                normalizePath(source, winslash = "/", mustWork = FALSE),
                managed
              ))
        ) {
          return(record)
        }
        if (
          !.builder_project_text(source) ||
            !file.exists(source) ||
            dir.exists(source)
        ) {
          stop(
            "A Spatial image source is no longer available. Add it again before saving.",
            call. = FALSE
          )
        }
        source <- normalizePath(source, winslash = "/", mustWork = TRUE)
        jobs[[length(jobs) + 1L]] <<- list(
          kind = "spatial",
          entry_id = entry$id,
          section = section,
          label = label,
          source = source,
          field_order = names(record),
          source_fingerprint = builder_project_file_fingerprint(
            source,
            content = FALSE
          )
        )
        record
      }
    )
  }
  jobs
}

builder_project_stage_attachment_jobs <- function(jobs, root) {
  lapply(jobs, function(job) {
    if (!identical(job$kind, "spatial")) {
      return(builder_project_stage_table_asset_jobs(list(job), root)[[1L]])
    }
    unchanged <- function() {
      builder_project_fingerprint_metadata_matches(
        job$source_fingerprint,
        builder_project_file_fingerprint(job$source, content = FALSE)
      )
    }
    if (!unchanged()) {
      stop("A Spatial image changed before saving.", call. = FALSE)
    }
    images <- stats::setNames(
      list(stats::setNames(
        list(list(source_path = job$source)),
        job$label
      )),
      job$section
    )
    staged <- builder_project_stage_spatial_assets(
      list(id = job$entry_id, settings = list(images = images)),
      root
    )
    if (!unchanged()) {
      stop("A Spatial image changed while saving.", call. = FALSE)
    }
    staged$settings$images[[job$section]][[
      job$label
    ]]$project_asset$field_order <- job$field_order
    list(
      kind = "spatial",
      entry_id = job$entry_id,
      source = job$source,
      section = job$section,
      label = job$label,
      asset = staged$settings$images[[job$section]][[job$label]]$project_asset
    )
  })
}

builder_project_attachment_results_match <- function(jobs, results) {
  key <- function(x) {
    paste(
      x$kind %||% "table",
      x$entry_id,
      x$source,
      x$section %||% "",
      x$label %||% "",
      sep = "\r"
    )
  }
  expected <- vapply(jobs, key, character(1))
  actual <- vapply(results, key, character(1))
  length(expected) == length(actual) &&
    !anyDuplicated(actual) &&
    setequal(expected, actual) &&
    all(vapply(results, function(x) is.list(x$asset), logical(1)))
}

builder_project_apply_attachment_results <- function(entries, results, root) {
  spatial <- Filter(function(x) identical(x$kind, "spatial"), results)
  entries <- builder_project_apply_table_asset_results(
    entries,
    Filter(function(x) !identical(x$kind, "spatial"), results),
    root
  )
  lapply(entries, function(entry) {
    .builder_project_map_spatial_images(
      entry,
      function(record, section, label) {
        matches <- Filter(
          function(x) {
            identical(x$entry_id, entry$id) &&
              identical(x$section, section) &&
              identical(x$label, label)
          },
          spatial
        )
        if (length(matches) == 1L) {
          record$project_asset <- matches[[1L]]$asset
          record$source_path <- builder_project_resolve_path(
            record$project_asset$path,
            root,
            "managed"
          )
          record$source_content_md5 <- record$project_asset$fingerprint$md5
          record$source_uri <- NULL
          record$uri <- NULL
        }
        record
      }
    )
  })
}

builder_project_table_asset_jobs <- function(entries, root) {
  root <- builder_project_normalize_root(root)
  jobs <- list()
  seen <- character()
  for (entry in entries %||% list()) {
    if (!is.list(entry) || !.builder_project_identifier(entry$id %||% NULL)) {
      stop("A dataset entry is required.", call. = FALSE)
    }
    for (record in entry$settings$tables %||% list()) {
      source <- record$source_path %||% ""
      if (!.builder_project_text(source) && is.data.frame(record$table)) {
        next
      }
      if (
        !.builder_project_text(source) ||
          !file.exists(source) ||
          dir.exists(source)
      ) {
        stop("An attachment source is no longer available.", call. = FALSE)
      }
      source <- normalizePath(source, winslash = "/", mustWork = TRUE)
      asset <- record$project_asset %||% list()
      source_fingerprint <- builder_project_file_fingerprint(
        source,
        content = FALSE
      )
      managed <- tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
      if (
        .builder_project_text(managed) &&
          identical(source, managed) &&
          builder_project_fingerprint_metadata_matches(
            asset$fingerprint,
            source_fingerprint
          )
      ) {
        next
      }
      key <- paste(entry$id, source, sep = "\r")
      if (key %in% seen) {
        next
      }
      seen <- c(seen, key)
      jobs[[length(jobs) + 1L]] <- list(
        entry_id = entry$id,
        source = source,
        filename = basename(record$file_name %||% source),
        source_fingerprint = source_fingerprint
      )
    }
  }
  jobs
}

builder_project_stage_table_asset_jobs <- function(jobs, root) {
  lapply(jobs %||% list(), function(job) {
    if (
      !is.list(job) ||
        !.builder_project_identifier(job$entry_id %||% NULL) ||
        !.builder_project_text(job$source %||% NULL) ||
        !file.exists(job$source) ||
        dir.exists(job$source)
    ) {
      stop("A table attachment job is invalid.", call. = FALSE)
    }
    if (
      !builder_project_fingerprint_metadata_matches(
        job$source_fingerprint,
        builder_project_file_fingerprint(job$source, content = FALSE)
      )
    ) {
      stop(
        "A table attachment changed while it was being saved.",
        call. = FALSE
      )
    }
    staged <- builder_project_stage_table_assets(
      list(
        id = job$entry_id,
        settings = list(
          tables = list(
            attachment = list(
              file_name = basename(job$filename %||% job$source),
              source_path = job$source
            )
          )
        )
      ),
      root
    )
    asset <- staged$settings$tables[[1L]]$project_asset
    if (
      !builder_project_fingerprint_metadata_matches(
        job$source_fingerprint,
        builder_project_file_fingerprint(job$source, content = FALSE)
      )
    ) {
      stop(
        "A table attachment changed while it was being saved.",
        call. = FALSE
      )
    }
    list(
      entry_id = job$entry_id,
      source = normalizePath(job$source, winslash = "/", mustWork = TRUE),
      asset = asset
    )
  })
}

builder_project_apply_table_asset_results <- function(entries, results, root) {
  result_keys <- vapply(
    results %||% list(),
    function(result) {
      paste(result$entry_id %||% "", result$source %||% "", sep = "\r")
    },
    character(1)
  )
  lapply(entries %||% list(), function(entry) {
    tables <- entry$settings$tables %||% list()
    for (key in names(tables)) {
      source <- tables[[key]]$source_path %||% ""
      if (!.builder_project_text(source) || !file.exists(source)) {
        next
      }
      source <- normalizePath(source, winslash = "/", mustWork = TRUE)
      index <- match(paste(entry$id, source, sep = "\r"), result_keys)
      if (is.na(index)) {
        next
      }
      asset <- results[[index]]$asset %||% NULL
      if (!is.list(asset)) {
        stop("A staged table attachment is invalid.", call. = FALSE)
      }
      tables[[key]]$project_asset <- asset
      tables[[key]]$source_path <- builder_project_resolve_path(
        asset$path,
        root,
        "managed"
      )
      tables[[key]]$table <- NULL
    }
    entry$settings$tables <- tables
    entry
  })
}

builder_project_table_asset_results_match <- function(jobs, results) {
  key <- function(value) {
    paste(value$entry_id %||% "", value$source %||% "", sep = "\r")
  }
  expected <- vapply(jobs %||% list(), key, character(1))
  actual <- vapply(results %||% list(), key, character(1))
  length(expected) == length(actual) &&
    !anyDuplicated(actual) &&
    setequal(expected, actual) &&
    all(vapply(
      results %||% list(),
      function(result) is.list(result$asset),
      logical(1)
    ))
}

builder_project_table_save_signature <- function(project, entries) {
  if (!is.list(project) || !is.list(project$manifest$project)) {
    stop("A Builder project is required.", call. = FALSE)
  }
  records <- lapply(entries %||% list(), function(entry) {
    list(
      id = entry$id,
      revision = as.integer(entry$revision %||% 0L),
      configuration = builder_project_configuration_digest(entry)
    )
  })
  ids <- vapply(records, `[[`, character(1), "id")
  list(
    project_id = project$manifest$project$id,
    root = builder_project_normalize_root(project$root),
    manifest_revision = as.integer(project$manifest$project$revision %||% 0L),
    entries = records[order(ids, method = "radix")]
  )
}

builder_project_stage_table_assets <- function(entry, root) {
  tables <- entry$settings$tables %||% list()
  if (!length(tables)) {
    return(entry)
  }
  root <- builder_project_normalize_root(root)
  assets <- list()
  for (key in names(tables)) {
    record <- tables[[key]]
    source <- record$source_path %||% ""
    if (!.builder_project_text(source) && is.data.frame(record$table)) {
      next
    }
    if (
      !.builder_project_text(source) ||
        !file.exists(source) ||
        dir.exists(source)
    ) {
      stop(
        paste0(
          "Attachment ",
          record$display_name %||% key,
          " is no longer available. Add it again before saving."
        ),
        call. = FALSE
      )
    }
    source <- normalizePath(source, winslash = "/", mustWork = TRUE)
    project_asset <- record$project_asset %||% list()
    project_path <- tryCatch(
      builder_project_resolve_path(
        project_asset$path %||% "",
        root,
        "managed"
      ),
      error = function(error) NULL
    )
    if (
      .builder_project_text(project_path) &&
        identical(source, project_path) &&
        builder_project_fingerprint_metadata_matches(
          project_asset$fingerprint,
          builder_project_file_fingerprint(source, content = FALSE)
        )
    ) {
      record$source_path <- NULL
      record$table <- NULL
      tables[[key]] <- record
      next
    }
    asset <- assets[[source]]
    if (is.null(asset)) {
      filename <- basename(record$file_name %||% source)
      before <- builder_project_file_metadata(source)
      temporary <- tempfile("attachment-", tmpdir = root)
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      copied <- builder_copy_file_verified(source, temporary)
      after <- builder_project_file_metadata(source)
      if (
        !builder_project_fingerprint_metadata_matches(before, after) ||
          !builder_project_fingerprint_stat_matches(before, after)
      ) {
        stop(
          "A table attachment changed while it was being saved.",
          call. = FALSE
        )
      }
      target <- builder_project_resolve_path(
        file.path(
          "attachments",
          entry$id,
          paste0(
            .builder_project_asset_segment(filename),
            "-",
            substr(copied$md5, 1L, 10L)
          )
        ),
        root,
        "managed"
      )
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      target_fingerprint <- builder_project_publish_verified_file(
        temporary,
        target,
        copied$md5,
        paste0("Attachment ", filename, " could not be saved safely.")
      )
      asset <- list(
        path = builder_project_relative_path(target, root),
        fingerprint = target_fingerprint
      )
      assets[[source]] <- asset
    }
    record$project_asset <- asset
    record$source_path <- NULL
    record$table <- NULL
    tables[[key]] <- record
  }
  entry$settings$tables <- tables
  entry
}

builder_project_adopt_table_assets <- function(entry, staged, root = NULL) {
  if (!is.list(entry) || !is.list(staged)) {
    return(entry)
  }
  if (is.null(entry$settings$tables)) {
    return(entry)
  }
  tables <- entry$settings$tables %||% list()
  staged_tables <- staged$settings$tables %||% list()
  for (key in intersect(names(tables), names(staged_tables))) {
    asset <- staged_tables[[key]]$project_asset %||% NULL
    tables[[key]]$project_asset <- asset
    if (!is.null(root) && is.list(asset)) {
      tables[[key]]$source_path <- tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
    }
  }
  entry$settings$tables <- tables
  entry
}

builder_project_restore_table_assets <- function(entry, root, observed = NULL) {
  if (is.null(entry$settings$tables)) {
    return(entry)
  }
  tables <- entry$settings$tables %||% list()
  fingerprints <- list()
  for (key in names(tables)) {
    record <- tables[[key]]
    asset <- record$project_asset %||% list()
    if (is.null(asset$path) && is.data.frame(record$table)) {
      next
    }
    reference <- asset$path %||% ""
    fingerprint <- fingerprints[[reference]] %||% observed[[reference]]
    path <- fingerprint$path %||%
      tryCatch(
        builder_project_resolve_path(reference, root, "managed"),
        error = function(error) NULL
      )
    fingerprint <- fingerprint %||%
      if (.builder_project_text(path)) {
        builder_project_file_fingerprint(path, content = FALSE)
      } else {
        NULL
      }
    fingerprints[[reference]] <- fingerprint
    matches <- !isFALSE(fingerprint$exists %||% TRUE) &&
      builder_project_managed_file_matches(
        asset$fingerprint,
        path,
        metadata = fingerprint
      )
    if (is.null(path) || !isTRUE(matches)) {
      stop(
        paste0(
          "Attachment ",
          record$display_name %||% key,
          " is missing or has changed."
        ),
        call. = FALSE
      )
    }
    record$source_path <- path
    tables[[key]] <- record
  }
  entry$settings$tables <- tables
  entry
}

builder_project_dataset_config_path <- function(
  dataset_id,
  root,
  content_id
) {
  if (
    !.builder_project_identifier(dataset_id) ||
      !.builder_project_text(content_id)
  ) {
    stop("A safe dataset id and content id are required.", call. = FALSE)
  }
  relative <- paste(
    "datasets",
    dataset_id,
    "configs",
    paste0(content_id, ".json"),
    sep = "/"
  )
  builder_project_resolve_path(
    relative,
    root,
    "managed"
  )
}

builder_project_read_configuration_file <- function(path, fingerprint = FALSE) {
  before <- builder_project_file_metadata(path)
  if (!.builder_project_text(path) || is.null(before)) {
    stop("A saved dataset configuration is missing.", call. = FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(
    connection,
    what = "raw",
    n = as.integer(before$bytes + 1L)
  )
  after <- builder_project_file_metadata(path)
  if (
    is.null(after) ||
      !builder_project_fingerprint_metadata_matches(before, after) ||
      !builder_project_fingerprint_stat_matches(before, after)
  ) {
    stop(
      "A saved dataset configuration changed while it was read.",
      call. = FALSE
    )
  }
  config <- tryCatch(
    jsonlite::unserializeJSON(rawToChar(bytes)),
    error = identity
  )
  parse_error <- inherits(config, "condition")
  if (!isTRUE(fingerprint)) {
    if (parse_error) {
      stop("A saved dataset configuration is not valid JSON.", call. = FALSE)
    }
    return(config)
  }
  after$md5 <- unclass(as.character(digest::digest(
    bytes,
    algo = "md5",
    serialize = FALSE
  )))
  list(
    config = if (parse_error) NULL else config,
    fingerprint = after,
    parse_error = parse_error
  )
}

## Key of a verified configuration sidecar: its stat identity plus what it was
## verified to contain. NULL when the file has no trusted stat identity.
.builder_project_configuration_verify_key <- function(
  path,
  metadata,
  dataset_id,
  revision,
  digest,
  contract_version,
  verification_cache
) {
  stat_key <- builder_project_verification_cache_key(
    path,
    metadata,
    verification_cache
  )
  if (!.builder_project_text(stat_key)) {
    return(NULL)
  }
  paste(
    "configuration-verify",
    stat_key,
    as.character(dataset_id),
    .builder_project_stored_integer(revision %||% 0L, 0L),
    as.character(digest %||% ""),
    as.character(contract_version),
    sep = "::"
  )
}

builder_project_verify_configuration <- function(
  path,
  dataset_id,
  revision,
  digest,
  fingerprint = NULL,
  contract_version = .builder_project_configuration_contract_version,
  verification_cache = NULL
) {
  cache_key <- NULL
  if (is.environment(verification_cache)) {
    cache_key <- .builder_project_configuration_verify_key(
      path,
      builder_project_file_metadata(path),
      dataset_id,
      revision,
      digest,
      contract_version,
      verification_cache
    )
    if (.builder_project_text(cache_key)) {
      cached <- get0(cache_key, envir = verification_cache, inherits = FALSE)
      if (
        is.list(cached) &&
          (!is.list(fingerprint) ||
            builder_project_content_fingerprint_matches(
              fingerprint,
              cached$fingerprint
            ))
      ) {
        return(cached)
      }
    }
  }
  loaded <- builder_project_read_configuration_file(path, fingerprint = TRUE)
  current <- loaded$fingerprint
  if (
    is.list(fingerprint) &&
      !builder_project_content_fingerprint_matches(fingerprint, current)
  ) {
    stop(
      "A saved dataset configuration failed its MD5 integrity check.",
      call. = FALSE
    )
  }
  if (isTRUE(loaded$parse_error)) {
    stop("A saved dataset configuration is not valid JSON.", call. = FALSE)
  }
  config <- loaded$config
  if (
    !is.list(config) ||
      !identical(as.character(config$id %||% ""), as.character(dataset_id))
  ) {
    stop(
      "A saved dataset configuration has the wrong dataset id.",
      call. = FALSE
    )
  }
  if (
    !identical(
      .builder_project_stored_integer(config$revision %||% 0L, 0L),
      .builder_project_stored_integer(revision %||% 0L, 0L)
    )
  ) {
    stop("A saved dataset configuration has the wrong revision.", call. = FALSE)
  }
  contract_version <- .builder_project_contract_version(contract_version)
  actual_digest <- builder_project_configuration_digest(config)
  if (
    !.builder_project_text(digest %||% NULL) ||
      !identical(as.character(actual_digest), as.character(digest))
  ) {
    stop(
      "A saved dataset configuration has the wrong configuration identity.",
      call. = FALSE
    )
  }
  verified <- list(
    config = config,
    fingerprint = current,
    digest = actual_digest,
    contract_version = contract_version
  )
  if (.builder_project_text(cache_key)) {
    assign(cache_key, verified, envir = verification_cache)
  }
  verified
}

## A configuration sidecar is verified as its staged part file, then renamed
## into place. When the target is still that file (same file id, size and
## modification time) its bytes were verified a moment ago; record the result
## under the target's identity so the commit check and the integrity passes of
## the same Save do not read and parse it again. Anything else is checked from
## disk as before.
builder_project_carry_configuration_verification <- function(
  target,
  verified,
  staged_fingerprint,
  config,
  digest,
  verification_cache
) {
  if (!is.environment(verification_cache) || !is.list(verified)) {
    return(invisible(FALSE))
  }
  metadata <- builder_project_file_metadata(target)
  same_file <- is.list(metadata) &&
    .builder_project_text(metadata$file_id %||% NULL) &&
    identical(metadata$file_id, staged_fingerprint$file_id %||% NULL) &&
    builder_project_fingerprint_metadata_matches(staged_fingerprint, metadata)
  if (!isTRUE(same_file)) {
    return(invisible(FALSE))
  }
  key <- .builder_project_configuration_verify_key(
    target,
    metadata,
    config$id,
    config$revision,
    digest,
    verified$contract_version %||%
      .builder_project_configuration_contract_version,
    verification_cache
  )
  if (!.builder_project_text(key)) {
    return(invisible(FALSE))
  }
  metadata$md5 <- staged_fingerprint$md5
  verified$fingerprint <- metadata
  assign(key, verified, envir = verification_cache)
  invisible(TRUE)
}

builder_project_write_dataset_config <- function(
  entry,
  root,
  expected_digest = NULL,
  verification_cache = NULL,
  .write = function(payload, path) {
    connection <- file(path, open = "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(charToRaw(enc2utf8(payload)), connection)
    flush(connection)
    invisible(TRUE)
  },
  .move = file.rename
) {
  root <- builder_project_normalize_root(root)
  config <- builder_project_configuration_entry(entry)
  target_dir <- builder_project_resolve_path(
    paste("datasets", config$id, "configs", sep = "/"),
    root,
    "managed"
  )
  if (
    !dir.exists(target_dir) &&
      !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop(
      "The dataset configuration directory could not be created.",
      call. = FALSE
    )
  }
  payload <- jsonlite::serializeJSON(config, digits = NA, pretty = TRUE)
  payload_md5 <- unclass(as.character(digest::digest(
    charToRaw(enc2utf8(payload)),
    algo = "md5",
    serialize = FALSE
  )))
  expected_digest <- as.character(
    expected_digest %||% builder_project_configuration_digest(config)
  )
  target <- builder_project_dataset_config_path(
    config$id,
    root,
    payload_md5
  )
  if (file.exists(target)) {
    expected_fingerprint <- builder_project_file_fingerprint_with_md5(
      target,
      payload_md5
    )
    if (is.null(expected_fingerprint)) {
      stop(
        "An immutable Project configuration failed its integrity check.",
        call. = FALSE
      )
    }
    committed <- builder_project_verify_configuration(
      target,
      config$id,
      config$revision,
      expected_digest,
      expected_fingerprint,
      verification_cache = verification_cache
    )
    return(list(
      schema_version = .builder_project_config_schema_version,
      contract_version = .builder_project_configuration_contract_version,
      path = builder_project_relative_path(target, root),
      revision = config$revision,
      fingerprint = committed$fingerprint,
      digest = committed$digest
    ))
  }
  temporary <- tempfile(
    ".config-",
    tmpdir = target_dir,
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  written <- tryCatch(.write(payload, temporary), error = identity)
  if (inherits(written, "condition") || !file.exists(temporary)) {
    stop("A Project configuration could not be written.", call. = FALSE)
  }
  staged_fingerprint <- builder_project_file_fingerprint(
    temporary,
    content = TRUE
  )
  if (!identical(staged_fingerprint$md5, payload_md5)) {
    stop("A Project configuration could not be written safely.", call. = FALSE)
  }
  staged_verification <- builder_project_verify_configuration(
    temporary,
    config$id,
    config$revision,
    expected_digest,
    staged_fingerprint
  )
  if (file.exists(target)) {
    current <- builder_project_file_fingerprint_with_md5(target, payload_md5)
    if (is.null(current)) {
      stop(
        "An immutable Project configuration failed its integrity check.",
        call. = FALSE
      )
    }
  } else if (!isTRUE(.move(temporary, target))) {
    stop("A Project configuration could not be committed.", call. = FALSE)
  } else {
    builder_project_carry_configuration_verification(
      target,
      staged_verification,
      staged_fingerprint,
      config,
      expected_digest,
      verification_cache
    )
  }
  committed <- builder_project_verify_configuration(
    target,
    config$id,
    config$revision,
    expected_digest,
    staged_fingerprint,
    verification_cache = verification_cache
  )
  list(
    schema_version = .builder_project_config_schema_version,
    contract_version = .builder_project_configuration_contract_version,
    path = builder_project_relative_path(target, root),
    revision = config$revision,
    fingerprint = committed$fingerprint,
    digest = committed$digest
  )
}

builder_project_read_dataset_config <- function(record, root, details = FALSE) {
  configuration <- record$configuration %||% list()
  path <- configuration$path %||% NULL
  if (.builder_project_text(path)) {
    resolved <- builder_project_resolve_path(path, root, "managed")
    if (!is.list(configuration$fingerprint)) {
      stop(
        "A saved dataset configuration fingerprint is missing.",
        call. = FALSE
      )
    }
    verified <- builder_project_verify_configuration(
      resolved,
      record$id,
      configuration$revision %||% 0L,
      configuration$digest %||% NULL,
      configuration$fingerprint,
      contract_version = configuration$contract_version
    )
    verified$config$runtime_project_configuration <- list(
      verified = TRUE,
      contract_version = verified$contract_version,
      digest = verified$digest,
      artifact_digest = tryCatch(
        builder_project_artifact_configuration_digest(verified$config),
        error = function(error) NULL
      )
    )
    return(
      if (isTRUE(details)) {
        verified$path <- resolved
        verified
      } else {
        verified$config
      }
    )
  }
  stop("A saved dataset configuration is missing.", call. = FALSE)
}

builder_project_configuration_integrity <- function(
  record,
  root,
  scan_previous = FALSE,
  verification_cache = NULL
) {
  configuration <- record$configuration %||% list()
  expected_path <- as.character(configuration$path %||% "")
  resolved <- tryCatch(
    builder_project_resolve_path(expected_path, root, "managed"),
    error = function(error) NULL
  )
  exists <- .builder_project_text(resolved) &&
    file.exists(resolved) &&
    !dir.exists(resolved)
  cache_key <- NULL
  if (
    isTRUE(exists) &&
      !isTRUE(scan_previous) &&
      is.environment(verification_cache)
  ) {
    metadata <- builder_project_file_metadata(resolved)
    stat_key <- builder_project_verification_cache_key(
      resolved,
      metadata,
      verification_cache
    )
    if (.builder_project_text(stat_key)) {
      cache_key <- paste0(
        "configuration::",
        unclass(as.character(digest::digest(
          serialize(
            list(
              stat = stat_key,
              id = as.character(record$id %||% ""),
              expected = configuration
            ),
            NULL,
            version = 3L
          ),
          algo = "md5",
          serialize = FALSE
        )))
      )
      cached <- get0(cache_key, envir = verification_cache, inherits = FALSE)
      if (is.list(cached)) {
        return(cached)
      }
    }
  }
  # A sidecar this writer verified (written or checked earlier, same stat
  # identity, same expected id, revision and identity) is not read again.
  verified <- if (isTRUE(exists) && .builder_project_text(cache_key)) {
    verify_key <- .builder_project_configuration_verify_key(
      resolved,
      metadata,
      record$id %||% "",
      configuration$revision %||% 0L,
      configuration$digest %||% "",
      tryCatch(
        .builder_project_contract_version(configuration$contract_version),
        error = function(error) NA_integer_
      ),
      verification_cache
    )
    hit <- if (.builder_project_text(verify_key)) {
      get0(verify_key, envir = verification_cache, inherits = FALSE)
    } else {
      NULL
    }
    if (is.list(hit)) hit else NULL
  } else {
    NULL
  }
  # Windows ACL/readability checks via file.access() are not reliable for files
  # created in the current process. The binary read below is the authority.
  readable <- exists
  loaded <- if (is.list(verified)) {
    list(config = verified$config, fingerprint = verified$fingerprint)
  } else if (exists) {
    tryCatch(
      builder_project_read_configuration_file(resolved, fingerprint = TRUE),
      error = identity
    )
  } else {
    NULL
  }
  readable <- is.list(loaded) && !inherits(loaded, "condition")
  current <- if (readable) loaded$fingerprint else NULL
  expected_md5 <- as.character(configuration$fingerprint$md5 %||% "")
  actual_md5 <- as.character(current$md5 %||% "")
  md5_matches <- .builder_project_text(expected_md5) &&
    identical(tolower(expected_md5), tolower(actual_md5))
  parsed <- if (readable) loaded$config else loaded
  json_parseable <- is.list(parsed) && !inherits(parsed, "condition")
  contract_version <- tryCatch(
    .builder_project_contract_version(configuration$contract_version),
    error = function(error) NA_integer_
  )
  actual_digest <- if (is.list(verified)) {
    as.character(verified$digest)
  } else if (json_parseable && !is.na(contract_version)) {
    tryCatch(
      builder_project_configuration_digest(parsed),
      error = function(error) ""
    )
  } else {
    ""
  }
  expected_digest <- as.character(configuration$digest %||% "")
  identity_matches <- .builder_project_text(expected_digest) &&
    identical(actual_digest, expected_digest)
  expected_revision <- tryCatch(
    .builder_project_stored_integer(configuration$revision %||% 0L, 0L),
    error = function(error) NA_integer_
  )
  actual_revision <- if (json_parseable) {
    tryCatch(
      .builder_project_stored_integer(parsed$revision %||% 0L, 0L),
      error = function(error) NA_integer_
    )
  } else {
    NA_integer_
  }
  revision_matches <- json_parseable &&
    identical(actual_revision, expected_revision)
  dataset_matches <- json_parseable &&
    identical(
      as.character(parsed$id %||% ""),
      as.character(record$id %||% "")
    )
  path_matches_md5 <- .builder_project_text(actual_md5) &&
    identical(
      tolower(tools::file_path_sans_ext(basename(expected_path))),
      tolower(actual_md5)
    )
  valid <- isTRUE(
    exists &&
      readable &&
      json_parseable &&
      md5_matches &&
      identity_matches &&
      revision_matches &&
      dataset_matches &&
      path_matches_md5
  )
  errors <- character()
  if (!exists) {
    errors <- c(errors, "configuration file is missing")
  }
  if (exists && !readable) {
    errors <- c(errors, "configuration file is unreadable")
  }
  if (readable && !json_parseable) {
    errors <- c(errors, "configuration JSON is invalid")
  }
  if (exists && !md5_matches) {
    errors <- c(errors, "configuration MD5 does not match")
  }
  if (json_parseable && !dataset_matches) {
    errors <- c(errors, "configuration dataset id does not match")
  }
  if (json_parseable && !revision_matches) {
    errors <- c(errors, "configuration revision does not match")
  }
  if (json_parseable && !identity_matches) {
    errors <- c(errors, "configuration identity does not match")
  }
  if (exists && !path_matches_md5) {
    errors <- c(
      errors,
      "configuration path is not content-addressed by its MD5"
    )
  }
  previous <- NULL
  if (isTRUE(scan_previous) && .builder_project_text(resolved)) {
    candidates <- list.files(
      dirname(resolved),
      pattern = "^[[:xdigit:]]{32}\\.json$",
      full.names = TRUE
    )
    candidates <- setdiff(candidates, resolved)
    inspected <- lapply(candidates, function(candidate) {
      fingerprint <- builder_project_file_fingerprint(candidate, content = TRUE)
      entry <- tryCatch(
        builder_project_read_configuration_file(candidate),
        error = function(error) NULL
      )
      if (!is.list(entry) || !is.list(fingerprint)) {
        return(NULL)
      }
      content_addressed <- identical(
        tolower(tools::file_path_sans_ext(basename(candidate))),
        tolower(as.character(fingerprint$md5 %||% ""))
      )
      if (
        !content_addressed ||
          !identical(
            as.character(entry$id %||% ""),
            as.character(record$id %||% "")
          )
      ) {
        return(NULL)
      }
      list(
        path = builder_project_relative_path(candidate, root),
        revision = .builder_project_integer(entry$revision %||% 0L),
        digest = tryCatch(
          builder_project_configuration_digest(entry),
          error = function(error) NULL
        ),
        md5 = fingerprint$md5
      )
    })
    inspected <- Filter(Negate(is.null), inspected)
    prior <- Filter(
      function(candidate) candidate$revision < expected_revision,
      inspected
    )
    if (length(prior)) {
      revisions <- vapply(prior, `[[`, integer(1), "revision")
      previous <- prior[[which.max(revisions)]]
    }
  }
  result <- list(
    valid = valid,
    checked = isTRUE(configuration$checked),
    expected_path = expected_path,
    exists = exists,
    readable = readable,
    json_parseable = json_parseable,
    expected_md5 = expected_md5,
    actual_md5 = actual_md5,
    md5_matches = md5_matches,
    expected_identity = expected_digest,
    actual_identity = actual_digest,
    identity_matches = identity_matches,
    identity_contract_version = contract_version,
    expected_revision = expected_revision,
    actual_revision = actual_revision,
    revision_matches = revision_matches,
    dataset_matches = dataset_matches,
    path_matches_md5 = path_matches_md5,
    latest_valid_previous_revision = previous,
    errors = errors,
    entry = if (json_parseable) parsed else NULL
  )
  if (.builder_project_text(cache_key)) {
    assign(cache_key, result, envir = verification_cache)
  }
  result
}

builder_project_dataset_integrity <- function(
  record,
  root,
  scan_previous = FALSE,
  verification_cache = NULL
) {
  configuration <- builder_project_configuration_integrity(
    record,
    root,
    scan_previous = scan_previous,
    verification_cache = verification_cache
  )
  source <- record$source %||% list()
  source_status <- as.character(source$status %||% "")
  source_kind <- as.character(source$kind %||% "managed")
  source_required <- !source_kind %in% c("example", "missing") &&
    !source_status %in% c("pending", "failed")
  source_md5 <- builder_project_recorded_fingerprint_md5(
    source$fingerprint %||% NULL
  )
  reference_matches <- function(fingerprint, path) {
    if (!is.list(fingerprint) || !.builder_project_text(path)) {
      return(FALSE)
    }
    builder_project_managed_file_matches(
      fingerprint,
      path,
      verification_cache = verification_cache
    )
  }
  source_valid <- TRUE
  if (source_required) {
    source_path <- tryCatch(
      builder_project_resolve_path(
        source$path %||% "",
        root,
        source_kind
      ),
      error = function(error) NULL
    )
    source_valid <- .builder_project_text(source_md5) &&
      .builder_project_text(source_path) &&
      file.exists(source_path) &&
      !dir.exists(source_path) &&
      reference_matches(source$fingerprint, source_path)
  }
  assets_valid <- FALSE
  if (isTRUE(configuration$valid)) {
    assets_valid <- isTRUE(tryCatch(
      {
        entry <- builder_project_restore_spatial_assets(
          configuration$entry,
          root
        )
        builder_project_restore_table_assets(entry, root)
        TRUE
      },
      error = function(error) FALSE
    ))
  }
  artifact <- record$artifact %||% NULL
  artifact_required <- is.list(artifact) && identical(artifact$status, "ready")
  artifact_valid <- TRUE
  if (artifact_required) {
    artifact_references <- c(
      list(list(path = artifact$path, fingerprint = artifact$fingerprint)),
      lapply(artifact$members %||% list(), function(member) {
        list(path = member$path, fingerprint = member$fingerprint)
      })
    )
    artifact_valid <- all(vapply(
      artifact_references,
      function(reference) {
        path <- tryCatch(
          builder_project_resolve_path(reference$path %||% "", root, "managed"),
          error = function(error) NULL
        )
        .builder_project_text(path) &&
          file.exists(path) &&
          !dir.exists(path) &&
          reference_matches(reference$fingerprint, path)
      },
      logical(1)
    ))
  }
  errors <- configuration$errors
  if (source_required && !source_valid) {
    errors <- c(errors, "required dataset source is missing or changed")
  }
  if (isTRUE(configuration$valid) && !assets_valid) {
    errors <- c(errors, "required configuration assets are missing or changed")
  }
  if (artifact_required && !artifact_valid) {
    errors <- c(errors, "required artifact sidecar is missing or changed")
  }
  if (isTRUE(configuration$checked) && !isTRUE(configuration$valid)) {
    errors <- c(errors, "checked dataset has no valid configuration sidecar")
  }
  list(
    id = as.character(record$id %||% ""),
    valid = isTRUE(configuration$valid) &&
      isTRUE(source_valid) &&
      isTRUE(assets_valid) &&
      isTRUE(artifact_valid),
    configuration = configuration,
    source = list(
      required = source_required,
      status = source_status,
      valid = source_valid
    ),
    artifact = list(required = artifact_required, valid = artifact_valid),
    errors = unique(errors)
  )
}

builder_project_manifest_integrity <- function(
  manifest,
  root,
  scan_previous = FALSE,
  verification_cache = NULL
) {
  datasets <- lapply(manifest$datasets %||% list(), function(record) {
    builder_project_dataset_integrity(
      record,
      root,
      scan_previous = scan_previous,
      verification_cache = verification_cache
    )
  })
  names(datasets) <- vapply(
    datasets,
    function(status) status$id,
    character(1)
  )
  list(
    valid = all(vapply(
      datasets,
      function(status) isTRUE(status$valid),
      logical(1)
    )),
    datasets = datasets
  )
}

builder_project_assert_manifest_integrity <- function(
  manifest,
  root,
  verification_cache = NULL
) {
  status <- builder_project_manifest_integrity(
    manifest,
    root,
    verification_cache = verification_cache
  )
  if (isTRUE(status$valid)) {
    return(invisible(status))
  }
  broken <- Filter(function(dataset) !isTRUE(dataset$valid), status$datasets)
  detail <- vapply(
    broken,
    function(dataset) {
      paste0(dataset$id, ": ", paste(dataset$errors, collapse = "; "))
    },
    character(1)
  )
  stop(
    "The Builder project failed reference-integrity validation: ",
    paste(detail, collapse = " | "),
    call. = FALSE
  )
}

builder_project_upgrade_verified_source_fingerprints <- function(
  manifest,
  disk,
  root,
  verification_cache
) {
  if (
    !is.list(manifest) ||
      !is.list(disk) ||
      !is.environment(verification_cache)
  ) {
    return(manifest)
  }
  disk_records <- disk$datasets %||% list()
  names(disk_records) <- vapply(
    disk_records,
    function(record) as.character(record$id %||% ""),
    character(1)
  )
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    saved <- disk_records[[as.character(record$id %||% "")]] %||% NULL
    source <- record$source %||% NULL
    saved_source <- saved$source %||% NULL
    source_md5 <- as.character(source$fingerprint$md5 %||% "")
    saved_md5 <- as.character(saved_source$fingerprint$md5 %||% "")
    unchanged_reference <- is.list(source) &&
      is.list(saved_source) &&
      identical(source$kind %||% "managed", "managed") &&
      identical(saved_source$kind %||% "managed", "managed") &&
      identical(source$status %||% "", "ready") &&
      identical(saved_source$status %||% "", "ready") &&
      identical(
        as.character(source$path %||% ""),
        as.character(saved_source$path %||% "")
      ) &&
      builder_project_content_addressed_source(source$path, record$id) &&
      .builder_project_text(source_md5) &&
      identical(tolower(source_md5), tolower(saved_md5))
    if (!isTRUE(unchanged_reference)) {
      return(record)
    }
    path <- tryCatch(
      builder_project_resolve_path(source$path, root, "managed"),
      error = function(error) NULL
    )
    verified <- builder_project_cached_verified_fingerprint(
      path,
      verification_cache
    )
    if (
      !is.list(verified) ||
        !builder_project_content_fingerprint_matches(
          saved_source$fingerprint,
          verified
        ) ||
        !builder_project_content_fingerprint_matches(
          source$fingerprint,
          verified
        )
    ) {
      return(record)
    }
    record$source$fingerprint <- verified
    record
  })
  manifest
}

builder_project_assert_configuration_records <- function(
  manifest,
  root,
  verification_cache = NULL
) {
  invalid <- Filter(
    function(result) !isTRUE(result$status$valid),
    lapply(manifest$datasets %||% list(), function(record) {
      list(
        id = as.character(record$id %||% ""),
        status = builder_project_configuration_integrity(
          record,
          root,
          verification_cache = verification_cache
        )
      )
    })
  )
  if (!length(invalid)) {
    return(invisible(TRUE))
  }
  detail <- vapply(
    invalid,
    function(result) {
      paste0(
        result$id,
        ": ",
        paste(result$status$errors, collapse = "; ")
      )
    },
    character(1)
  )
  stop(
    "The Builder project has an invalid dataset configuration: ",
    paste(detail, collapse = " | "),
    call. = FALSE
  )
}

.builder_project_asset_segment <- function(value) {
  value <- as.character(value %||% "")[[1L]]
  digest <- unclass(as.character(digest::digest(
    charToRaw(enc2utf8(value)),
    algo = "md5",
    serialize = FALSE
  )))
  paste0("a-", substr(digest, 1L, 12L))
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

builder_project_stage_spatial_assets <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  staged <- entry
  .builder_project_map_spatial_images(staged, function(record, section, label) {
    if (!is.list(record)) {
      return(record)
    }
    field_order <- names(record)
    source_path <- record[["source_path", exact = TRUE]]
    asset <- record[["project_asset", exact = TRUE]]
    fail <- function(reason) {
      stop(
        paste0(
          "Spatial image asset for dataset “",
          entry$id,
          "”, FOV “",
          section,
          "”, image “",
          label,
          "” ",
          reason,
          "."
        ),
        call. = FALSE
      )
    }
    asset_path <- if (is.list(asset)) {
      tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
    } else {
      NULL
    }
    asset_metadata <- if (
      .builder_project_text(asset_path) &&
        file.exists(asset_path) &&
        !dir.exists(asset_path)
    ) {
      builder_project_file_fingerprint(asset_path, content = FALSE)
    } else {
      NULL
    }
    # Project open/restore verifies the persisted content digest. During a
    # subsequent save, unchanged metadata is enough to retain that already
    # verified, content-addressed asset without rescanning a potentially huge
    # tissue image on Windows.
    valid_asset <- !is.null(asset_metadata) &&
      builder_project_fingerprint_metadata_matches(
        asset$fingerprint,
        asset_metadata
      )
    canonical_source <- if (
      .builder_project_text(source_path) &&
        file.exists(source_path) &&
        !dir.exists(source_path)
    ) {
      normalizePath(source_path, winslash = "/", mustWork = TRUE)
    } else {
      NULL
    }
    canonical_asset <- if (isTRUE(valid_asset)) {
      normalizePath(asset_path, winslash = "/", mustWork = TRUE)
    } else {
      NULL
    }
    if (
      isTRUE(valid_asset) &&
        (is.null(canonical_source) ||
          identical(canonical_source, canonical_asset))
    ) {
      record$source_content_md5 <- asset$fingerprint$md5 %||%
        record$source_content_md5 %||%
        NULL
      record$source_path <- NULL
      record$source_uri <- NULL
      record$uri <- NULL
      return(record)
    }
    if (!is.null(canonical_source)) {
      source <- record[["source", exact = TRUE]]
      inspected <- builder_read_image(
        canonical_source,
        filename = if (is.list(source)) {
          source$name %||% canonical_source
        } else {
          canonical_source
        }
      )
      if (!is.null(inspected$error)) {
        fail(paste0("is invalid: ", inspected$error))
      }
      mime <- inspected$mime
      asset_id <- inspected$source_content_md5
      extension <- if (identical(mime, "image/png")) "png" else "jpg"
    } else {
      fail(
        "has no external source file; re-upload images saved by an older Builder"
      )
    }
    target_dir <- builder_project_resolve_path(
      paste(
        "spatial-assets",
        entry$id,
        .builder_project_asset_segment(section),
        sep = "/"
      ),
      root,
      "managed"
    )
    if (
      !dir.exists(target_dir) &&
        !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    ) {
      stop("The Spatial asset directory could not be created.", call. = FALSE)
    }
    target <- file.path(target_dir, paste0(asset_id, ".", extension))
    if (
      !file.exists(target) ||
        !identical(
          unname(as.character(tools::md5sum(target))),
          asset_id
        )
    ) {
      part <- tempfile(
        pattern = paste0(asset_id, "-"),
        tmpdir = target_dir,
        fileext = ".part"
      )
      on.exit(unlink(part, force = TRUE), add = TRUE)
      copied <- builder_project_copy_file(canonical_source, part)
      if (
        !isTRUE(copied) ||
          !identical(unname(as.character(tools::md5sum(part))), asset_id) ||
          !file.rename(part, target)
      ) {
        stop(
          "A Spatial image asset could not be committed safely.",
          call. = FALSE
        )
      }
    }
    record$project_asset <- list(
      schema_version = 1L,
      path = builder_project_relative_path(target, root),
      mime = mime,
      fingerprint = builder_project_file_fingerprint_with_md5(target, asset_id),
      field_order = field_order
    )
    record$source_content_md5 <- asset_id
    record$source_path <- NULL
    record$source_uri <- NULL
    record$uri <- NULL
    record
  })
}

builder_project_adopt_spatial_assets <- function(entry, staged, root = NULL) {
  if (!is.list(entry) || !is.list(staged)) {
    return(entry)
  }
  staged_record <- function(section, label) {
    collection <- staged$settings$images[[section]] %||% NULL
    legacy <- is.list(collection) &&
      any(
        c("source_path", "source_uri", "uri", "project_asset") %in%
          names(collection)
      )
    if (legacy) collection else collection[[label]] %||% NULL
  }
  .builder_project_map_spatial_images(entry, function(record, section, label) {
    saved <- staged_record(section, label)
    if (
      is.list(record) &&
        is.list(saved) &&
        is.list(saved$project_asset %||% NULL)
    ) {
      record$project_asset <- saved$project_asset
      if (!is.null(root)) {
        record$source_path <- tryCatch(
          builder_project_resolve_path(
            saved$project_asset$path %||% "",
            root,
            "managed"
          ),
          error = function(error) NULL
        )
      }
      record$source_uri <- NULL
      record$uri <- NULL
    }
    record
  })
}

builder_project_restore_spatial_assets <- function(
  entry,
  root,
  observed = NULL
) {
  dataset_id <- as.character(entry$id %||% "dataset")
  .builder_project_map_spatial_images(entry, function(record, section, label) {
    asset <- if (is.list(record)) record$project_asset %||% NULL else NULL
    if (is.null(asset)) {
      return(record)
    }
    fail <- function(reason) {
      stop(
        paste0(
          "Spatial image asset for dataset “",
          dataset_id,
          "”, FOV “",
          section,
          "”, image “",
          label,
          "” ",
          reason,
          "."
        ),
        call. = FALSE
      )
    }
    reference <- asset$path %||% ""
    fingerprint <- observed[[reference]]
    path <- fingerprint$path %||%
      tryCatch(
        builder_project_resolve_path(reference, root, "managed"),
        error = function(error) NULL
      )
    fingerprint <- fingerprint %||%
      if (.builder_project_text(path)) {
        builder_project_file_fingerprint(path, content = FALSE)
      } else {
        NULL
      }
    if (is.null(fingerprint) || isFALSE(fingerprint$exists %||% TRUE)) {
      fail("is missing")
    }
    if (
      !builder_project_managed_file_matches(
        asset$fingerprint,
        path,
        metadata = fingerprint
      )
    ) {
      fail("failed its integrity check")
    }
    record$source_content_md5 <- asset$fingerprint$md5 %||%
      record$source_content_md5 %||%
      NULL
    record$source_path <- path
    record$source_uri <- NULL
    record$uri <- NULL
    field_order <- as.character(asset$field_order %||% character())
    if (length(field_order)) {
      record <- record[c(
        intersect(field_order, names(record)),
        setdiff(names(record), field_order)
      )]
    }
    record
  })
}

builder_project_checkpoint_entries <- function(entries) {
  lapply(entries, function(entry) {
    if (length(entry$settings$images %||% list())) {
      entry$settings$spatial_image_storage <- "external"
    }
    entry
  })
}

builder_project_plan_artifact_reusable <- function(item) {
  !length(setdiff(
    item$private_assets %||% character(),
    c(item$filename, item$sidecars %||% character())
  ))
}

builder_project_checkpoint_budget <- function(entries, root) {
  closure_bytes <- sum(vapply(
    entries,
    function(entry) {
      value <- suppressWarnings(as.double(entry$snapshot$closure_bytes %||% 0))
      if (length(value) == 1L && is.finite(value) && value >= 0) value else 0
    },
    numeric(1)
  ))
  required <- 2 * closure_bytes + 1024^3
  available <- tryCatch(
    .builder_snapshot_available_bytes(root),
    error = function(error) error
  )
  if (inherits(available, "condition")) {
    return(list(
      ok = FALSE,
      required = required,
      available = NA_real_,
      error = conditionMessage(available)
    ))
  }
  list(
    ok = is.finite(available) && available >= required,
    required = required,
    available = available,
    error = if (is.finite(available) && available >= required) {
      NULL
    } else {
      paste0(
        "The project volume does not have enough free space for the checkpoint ",
        "and verified artifact promotion. Free disk space and retry."
      )
    }
  )
}

builder_project_cleanup_checkpoint <- function(path, root) {
  if (!.builder_project_text(path) || !dir.exists(path)) {
    return(FALSE)
  }
  root <- builder_project_normalize_root(root)
  candidate <- .builder_project_lexical_path(path)
  lexical_root <- dirname(dirname(candidate))
  normalized_lexical_root <- tryCatch(
    normalizePath(lexical_root, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  normalized_candidate <- tryCatch(
    normalizePath(candidate, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    !identical(normalized_lexical_root, root) ||
      !identical(
        dirname(normalized_candidate %||% ""),
        file.path(root, "checkpoints")
      ) ||
      .builder_project_path_has_link_within(candidate, lexical_root)
  ) {
    return(FALSE)
  }
  unlink(candidate, recursive = TRUE, force = TRUE)
  if (dir.exists(candidate)) {
    return(FALSE)
  }
  if (exists("builder_release_cleanup_control", mode = "function")) {
    return(isTRUE(tryCatch(
      builder_release_cleanup_control(candidate),
      error = function(error) FALSE
    )))
  }
  TRUE
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

.builder_project_configuration_digest_memo <- new.env(parent = emptyenv())

builder_project_artifact_configuration_digest <- function(
  entry,
  digest = builder_project_configuration_digest
) {
  artifact_entry <- entry
  viewer_only <- c(
    "initial_projections",
    "viewer_display",
    "overview_point_size",
    "overview_point_opacity",
    "overview_percentage_cells_to_show",
    "spatial_percentage_cells_to_show",
    "spatial_point_appearance",
    "spatial_roi_settings"
  )
  settings <- artifact_entry$settings %||% list()
  settings[intersect(names(settings), viewer_only)] <- NULL
  artifact_entry$settings <- settings
  artifact_entry <- .builder_project_map_spatial_images(
    artifact_entry,
    function(record, section, label) {
      if (is.list(record)) {
        record$point_opacity <- NULL
        record$point_size <- NULL
      }
      record
    }
  )
  digest(artifact_entry)
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

builder_project_dataset_record <- function(
  entry,
  source,
  checked = FALSE,
  artifact = NULL,
  order = 1L,
  payload_entry = entry,
  root,
  configuration_digest = NULL,
  verification_cache = NULL
) {
  profile <- entry$profile %||% list()
  spatial <- entry$dataset_profile$content$spatial %||%
    entry$profile$viewer_content$spatial %||%
    list()
  sections <- spatial$sections %||% spatial$fovs %||% character()
  descriptor <- builder_project_write_dataset_config(
    payload_entry,
    root,
    expected_digest = configuration_digest,
    verification_cache = verification_cache
  )
  digest <- as.character(descriptor$digest %||% "")
  if (!.builder_project_text(digest)) {
    stop(
      "The committed dataset configuration identity is missing.",
      call. = FALSE
    )
  }
  if (
    !is.null(configuration_digest) &&
      !identical(as.character(configuration_digest), digest)
  ) {
    stop(
      "The dataset configuration changed while the Project was being saved.",
      call. = FALSE
    )
  }
  checked <- isTRUE(checked)
  if (!checked) {
    artifact <- NULL
  }
  configuration <- utils::modifyList(
    descriptor,
    list(
      digest = digest,
      reviewed = checked,
      reviewed_digest = if (checked) digest else NULL,
      checked = isTRUE(checked),
      checked_digest = if (isTRUE(checked)) digest else NULL,
      contract_version = .builder_project_configuration_contract_version
    )
  )
  list(
    id = entry$id,
    name = entry$settings$name %||% entry$id,
    order = as.integer(order),
    format = entry$format %||% NULL,
    source = source,
    inspection = list(
      cells = as.integer(profile$n_cells %||% 0L),
      genes = as.integer(profile$n_genes %||% 0L),
      assays = as.character(names(profile$assay_profiles %||% list())),
      projections = as.character(entry$settings$reductions %||% character()),
      fovs = as.character(names(sections) %||% sections %||% character())
    ),
    configuration = configuration,
    artifact = artifact,
    release = list(included = TRUE)
  )
}

builder_project_restore_entry <- function(
  record,
  root,
  entry,
  observed = NULL
) {
  if (!is.list(entry)) {
    stop("The saved dataset configuration is unavailable.", call. = FALSE)
  }
  entry$id <- as.character(record$id)
  entry$source_id <- entry$id
  entry$output_id <- entry$id
  entry$revision <- as.integer(
    entry$revision %||% record$configuration$revision %||% 0L
  )
  entry$settings <- entry$settings %||% list(name = record$name %||% entry$id)
  inspection <- record$inspection %||% list()
  entry$profile <- entry$profile %||%
    list(
      n_cells = as.integer(inspection$cells %||% 0L),
      n_genes = as.integer(inspection$genes %||% 0L),
      assays = as.character(inspection$assays %||% character()),
      reductions = as.character(inspection$projections %||% character())
    )
  entry$dataset_profile <- entry$dataset_profile %||% list()
  entry$levels <- entry$levels %||% list()
  entry$format <- entry$format %||% record$format %||% NULL
  entry$load_state <- "reload_required"
  source <- record$source %||% list()
  entry$source_origin <- source$origin %||%
    if (identical(source$kind, "example")) "example" else "upload"
  if (identical(source$kind, "example")) {
    entry$example <- source$example
    entry$path <- NULL
  } else {
    entry$example <- if (identical(entry$source_origin, "example")) {
      source$example %||% NULL
    } else {
      NULL
    }
    entry$path <- observed[[source$path %||% ""]]$path %||%
      builder_project_resolve_path(
        source$path %||% "",
        root,
        source$kind %||% "managed"
      )
  }
  entry
}

builder_project_attach_loaded_source_fingerprint <- function(
  loaded,
  record,
  root,
  require_match = TRUE
) {
  source <- record$source %||% list()
  if (identical(source$kind, "example")) {
    return(loaded)
  }
  state <- loaded$snapshot$source_state %||% NULL
  actual <- builder_project_source_fingerprint_md5(
    loaded$snapshot$source_fingerprint %||% NULL
  )
  expected <- builder_project_recorded_fingerprint_md5(
    source$fingerprint %||% NULL
  )
  if (!is.list(state)) {
    stop(
      "The source worker did not return a verified file identity.",
      call. = FALSE
    )
  }
  state_bytes <- suppressWarnings(as.double(state$bytes %||% NA_real_))
  if (
    !.builder_project_text(state$location %||% NULL) ||
      length(state_bytes) != 1L ||
      !is.finite(state_bytes) ||
      !.builder_project_text(state$modified_at %||% NULL) ||
      !.builder_project_text(actual)
  ) {
    stop(
      "The source worker did not return a verified file identity.",
      call. = FALSE
    )
  }
  source_path <- builder_project_resolve_path(
    source$path %||% "",
    root,
    source$kind %||% "managed"
  )
  if (
    !identical(
      normalizePath(state$location %||% "", winslash = "/", mustWork = FALSE),
      normalizePath(source_path, winslash = "/", mustWork = FALSE)
    )
  ) {
    stop("The source worker returned a different file.", call. = FALSE)
  }
  current <- builder_project_file_metadata(source_path)
  if (
    !builder_project_fingerprint_metadata_matches(state, current) ||
      !builder_project_fingerprint_stat_matches(state, current)
  ) {
    stop(
      "The saved dataset source changed during Project restore.",
      call. = FALSE
    )
  }
  if (
    isTRUE(require_match) &&
      (!.builder_project_text(expected) ||
        !identical(tolower(expected), tolower(actual)))
  ) {
    stop(
      "The saved dataset source changed since the Project was saved.",
      call. = FALSE
    )
  }
  loaded$project_source_fingerprint <- list(
    bytes = state_bytes,
    modified_at = state$modified_at,
    changed_at = state$changed_at,
    stat_version = state$stat_version,
    file_id = state$file_id,
    md5 = actual
  )
  loaded
}

builder_project_finalize_loaded_entry <- function(loaded, record, root) {
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
  action <- as.character(record$runtime_restore_action %||% "")
  if (!action %in% c("resume", "reload", "edit")) {
    stop("The Project restore action is missing.", call. = FALSE)
  }
  status <- record$runtime_restore_status %||% NULL
  authorized_action <- if (identical(action, "edit")) "resume" else action
  if (
    !is.list(status) ||
      !authorized_action %in% (status$actions %||% character())
  ) {
    stop("The Project restore action is no longer valid.", call. = FALSE)
  }
  loaded <- builder_project_attach_loaded_source_fingerprint(
    loaded,
    record,
    root,
    require_match = !identical(action, "reload")
  )
  if (identical(action, "reload")) {
    return(loaded)
  }
  saved <- status$configuration_entry %||% NULL
  if (
    !builder_project_status_snapshot_fresh(
      status,
      record,
      root,
      needs = c("configuration", "spatial", "table")
    )
  ) {
    stop(
      "The saved dataset inputs changed during Project restore.",
      call. = FALSE
    )
  }
  if (!is.list(saved)) {
    stop("The saved dataset configuration is unavailable.", call. = FALSE)
  }
  loaded$settings <- saved$settings
  loaded$acknowledgements <- saved$acknowledgements %||% NULL
  loaded$spatial_drafts <- saved$spatial_drafts %||% NULL
  loaded$revision <- as.integer(
    saved$revision %||% record$configuration$revision %||% 0L
  )
  loaded
}

builder_project_verified_spatial_sidecar <- function(built, verification) {
  path <- verification$spatial_molecule_path %||% NULL
  if (is.null(path)) {
    return(NULL)
  }
  if (
    !.builder_project_text(path) ||
      !dir.exists(path) ||
      nzchar(Sys.readlink(path))
  ) {
    stop("The verified spatial molecule sidecar is unavailable.", call. = FALSE)
  }
  built <- normalizePath(built, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  expected <- paste0(
    tools::file_path_sans_ext(basename(built)),
    ".spatial"
  )
  if (
    !identical(dirname(path), dirname(built)) ||
      !identical(basename(path), expected)
  ) {
    stop(
      "The verified spatial molecule sidecar does not match its dataset cache.",
      call. = FALSE
    )
  }
  basename(path)
}

builder_project_store_artifact_bundle <- function(
  built,
  sidecars = character(),
  dataset_id,
  root,
  promote = FALSE,
  verified_release = NULL
) {
  if (
    !.builder_project_identifier(dataset_id) ||
      !.builder_project_text(built) ||
      !file.exists(built) ||
      dir.exists(built)
  ) {
    stop("A completed dataset artifact is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  built <- normalizePath(built, winslash = "/", mustWork = TRUE)
  build_root <- normalizePath(dirname(built), winslash = "/", mustWork = TRUE)
  sidecars <- unique(as.character(sidecars %||% character()))
  member_source <- function(target, allow_directory = FALSE) {
    normalized_target <- gsub("\\\\", "/", target)
    if (
      !.builder_project_text(target) ||
        grepl("^/", target) ||
        grepl("^[A-Za-z]:[/\\\\]", target) ||
        any(
          strsplit(normalized_target, "/", fixed = TRUE)[[1L]] %in%
            c("", ".", "..")
        ) ||
        identical(normalized_target, basename(built))
    ) {
      stop("An artifact member path is unsafe.", call. = FALSE)
    }
    candidate <- file.path(build_root, target)
    source <- normalizePath(
      candidate,
      winslash = "/",
      mustWork = TRUE
    )
    if (
      !startsWith(source, paste0(build_root, "/")) ||
        .builder_project_path_has_link_within(candidate, build_root) ||
        (!isTRUE(allow_directory) && dir.exists(source))
    ) {
      stop("An artifact member escaped the build folder.", call. = FALSE)
    }
    source
  }
  expand_member <- function(target) {
    source <- member_source(target, allow_directory = TRUE)
    if (!dir.exists(source)) {
      return(gsub("\\\\", "/", target))
    }
    descendants <- list.files(
      source,
      all.files = TRUE,
      no.. = TRUE,
      recursive = TRUE,
      full.names = FALSE,
      include.dirs = FALSE
    )
    if (!length(descendants)) {
      stop("An artifact member directory is empty.", call. = FALSE)
    }
    gsub("\\\\", "/", file.path(target, descendants))
  }
  sidecars <- unique(unlist(lapply(sidecars, expand_member), use.names = FALSE))
  safe_member <- function(target) {
    normalized_target <- gsub("\\\\", "/", target)
    source <- member_source(target)
    list(target = normalized_target, source = source)
  }
  members <- lapply(sidecars, safe_member)
  normalized_members <- vapply(members, `[[`, character(1), "target")
  if (anyDuplicated(normalized_members)) {
    stop("Artifact member paths must be unique.", call. = FALSE)
  }
  spatial_prefix <- paste0(
    tools::file_path_sans_ext(basename(built)),
    ".spatial/"
  )
  has_spatial_molecules <- any(startsWith(
    gsub("\\\\", "/", normalized_members),
    spatial_prefix
  ))
  spatial_closure_available <- function(path, stored_members, fingerprint) {
    !has_spatial_molecules ||
      builder_project_artifact_spatial_closure_available(
        path,
        stored_members,
        fingerprint
      )
  }
  release_md5 <- function(path) {
    target <- verified_release$target %||% NULL
    entries <- verified_release$identity$entries %||% list()
    if (!.builder_project_text(target) || !length(entries)) {
      return(NULL)
    }
    target <- tryCatch(
      normalizePath(target, winslash = "/", mustWork = TRUE),
      error = function(error) NULL
    )
    path <- tryCatch(
      normalizePath(path, winslash = "/", mustWork = TRUE),
      error = function(error) NULL
    )
    if (
      is.null(target) ||
        is.null(path) ||
        !startsWith(path, paste0(target, "/"))
    ) {
      return(NULL)
    }
    relative <- substring(path, nchar(target) + 2L)
    matched <- Filter(
      function(entry) {
        is.list(entry) &&
          identical(entry$type %||% "", "file") &&
          identical(gsub("\\\\", "/", entry$path %||% ""), relative)
      },
      entries
    )
    if (length(matched) != 1L) {
      return(NULL)
    }
    md5 <- as.character(matched[[1L]]$md5 %||% "")
    if (
      length(md5) != 1L ||
        is.na(md5) ||
        !grepl("^[[:xdigit:]]{32}$", md5)
    ) {
      return(NULL)
    }
    md5
  }
  fingerprint_source <- function(path) {
    md5 <- release_md5(path)
    if (.builder_project_text(md5)) {
      return(list(
        fingerprint = builder_project_file_fingerprint_with_md5(path, md5),
        preverified = TRUE
      ))
    }
    list(
      fingerprint = builder_project_file_fingerprint(path, content = TRUE),
      preverified = FALSE
    )
  }
  primary_source <- fingerprint_source(built)
  member_sources <- lapply(members, function(member) {
    fingerprint_source(member$source)
  })
  primary_fingerprint <- primary_source$fingerprint
  member_fingerprints <- lapply(member_sources, `[[`, "fingerprint")
  if (
    is.null(primary_fingerprint) ||
      any(vapply(member_fingerprints, is.null, logical(1)))
  ) {
    stop("An artifact source could not be read safely.", call. = FALSE)
  }
  identity <- list(
    primary = list(name = basename(built), md5 = primary_fingerprint$md5),
    members = lapply(seq_along(members), function(index) {
      list(
        target = members[[index]]$target,
        md5 = member_fingerprints[[index]]$md5
      )
    })
  )
  bundle_id <- unclass(as.character(digest::digest(
    serialize(identity, NULL, version = 3L),
    algo = "md5",
    serialize = FALSE
  )))
  relative_dir <- paste(
    "artifacts",
    dataset_id,
    "generations",
    bundle_id,
    sep = "/"
  )
  generation_dir <- builder_project_resolve_path(relative_dir, root, "managed")
  if (
    dir.exists(generation_dir) &&
      (isTRUE(primary_source$preverified) ||
        any(vapply(member_sources, `[[`, logical(1), "preverified")))
  ) {
    verified_primary <- builder_project_file_fingerprint(built, content = TRUE)
    verified_members <- lapply(members, function(member) {
      builder_project_file_fingerprint(member$source, content = TRUE)
    })
    if (
      is.null(verified_primary) ||
        !identical(verified_primary$md5, primary_fingerprint$md5) ||
        any(vapply(
          seq_along(verified_members),
          function(index) {
            is.null(verified_members[[index]]) ||
              !identical(
                verified_members[[index]]$md5,
                member_fingerprints[[index]]$md5
              )
          },
          logical(1)
        ))
    ) {
      stop("A published artifact changed before registration.", call. = FALSE)
    }
    primary_fingerprint <- verified_primary
    member_fingerprints <- verified_members
  }
  generation_parent <- dirname(generation_dir)
  if (
    !dir.exists(generation_parent) &&
      !dir.create(generation_parent, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop("The artifact generation folder could not be created.", call. = FALSE)
  }
  staging <- tempfile(
    pattern = ".artifact-",
    tmpdir = generation_parent
  )
  promoted <- list()
  generation_committed <- FALSE
  on.exit(
    {
      if (!generation_committed && length(promoted)) {
        for (move in rev(promoted)) {
          if (file.exists(move$staged) && !file.exists(move$source)) {
            dir.create(
              dirname(move$source),
              recursive = TRUE,
              showWarnings = FALSE
            )
            file.rename(move$staged, move$source)
          }
        }
      }
      unlink(staging, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )
  read_existing_generation <- function() {
    primary_path <- file.path(generation_dir, basename(built))
    stored_members <- lapply(seq_along(members), function(index) {
      member <- members[[index]]
      path <- file.path(generation_dir, member$target)
      if (!file.exists(path) || dir.exists(path)) {
        stop("A stored artifact member is missing.", call. = FALSE)
      }
      list(
        target = member$target,
        path = builder_project_relative_path(path, root),
        fingerprint = builder_project_file_fingerprint(path, content = TRUE)
      )
    })
    if (!file.exists(primary_path) || dir.exists(primary_path)) {
      stop("The stored dataset artifact is missing.", call. = FALSE)
    }
    stored_primary <- builder_project_file_fingerprint(
      primary_path,
      content = TRUE
    )
    if (
      !builder_project_content_fingerprint_matches(
        primary_fingerprint,
        stored_primary
      ) ||
        any(
          !vapply(
            seq_along(stored_members),
            function(index) {
              builder_project_content_fingerprint_matches(
                member_fingerprints[[index]],
                stored_members[[index]]$fingerprint
              )
            },
            logical(1)
          )
        )
    ) {
      stop(
        "The stored artifact generation failed its integrity check.",
        call. = FALSE
      )
    }
    if (
      !spatial_closure_available(
        primary_path,
        stored_members,
        stored_primary
      )
    ) {
      stop(
        "The stored artifact spatial molecule closure is not portable.",
        call. = FALSE
      )
    }
    list(
      path = builder_project_relative_path(primary_path, root),
      fingerprint = stored_primary,
      members = stored_members
    )
  }
  if (!dir.exists(generation_dir)) {
    if (!dir.create(staging, showWarnings = FALSE)) {
      stop("The artifact staging folder could not be created.", call. = FALSE)
    }
    primary_target <- file.path(staging, basename(built))
    # A promoted file keeps its identity when renamed on the same volume. If
    # its size, modification time and file id are unchanged, the bytes hashed
    # above are the staged bytes and need no second read.
    renamed_md5 <- character()
    stage_file <- function(source, target, fingerprint) {
      moved <- isTRUE(promote) && isTRUE(file.rename(source, target))
      if (moved) {
        promoted[[length(promoted) + 1L]] <<- list(
          source = source,
          staged = target
        )
        staged <- builder_project_file_metadata(target)
        if (
          is.list(fingerprint) &&
            builder_project_fingerprint_metadata_matches(fingerprint, staged) &&
            identical(fingerprint$stat_version %||% NULL, 1L) &&
            identical(staged$stat_version %||% NULL, 1L) &&
            .builder_project_text(fingerprint$file_id %||% NULL) &&
            identical(fingerprint$file_id, staged$file_id) &&
            .builder_project_text(fingerprint$md5 %||% NULL)
        ) {
          renamed_md5[[target]] <<- as.character(fingerprint$md5)
        }
        return(TRUE)
      }
      builder_project_copy_file(source, target)
    }
    # Only MD5s this process computed are reused; release MD5s are still
    # checked against the staged bytes.
    hashed_here <- function(source, fingerprint) {
      if (isTRUE(source$preverified)) NULL else fingerprint
    }
    if (
      !stage_file(
        built,
        primary_target,
        hashed_here(primary_source, primary_fingerprint)
      )
    ) {
      stop("The dataset artifact could not be staged.", call. = FALSE)
    }
    for (index in seq_along(members)) {
      member_target <- file.path(staging, members[[index]]$target)
      if (
        !dir.exists(dirname(member_target)) &&
          !dir.create(
            dirname(member_target),
            recursive = TRUE,
            showWarnings = FALSE
          )
      ) {
        stop("An artifact member folder could not be staged.", call. = FALSE)
      }
      if (
        !stage_file(
          members[[index]]$source,
          member_target,
          hashed_here(member_sources[[index]], member_fingerprints[[index]])
        )
      ) {
        stop("An artifact member could not be staged.", call. = FALSE)
      }
    }
    staged_paths <- c(
      primary_target,
      vapply(
        members,
        function(member) {
          file.path(staging, member$target)
        },
        character(1)
      )
    )
    expected <- c(
      primary_fingerprint$md5,
      vapply(member_fingerprints, `[[`, character(1), "md5")
    )
    staged_md5 <- unname(renamed_md5[staged_paths])
    unverified <- is.na(staged_md5)
    if (any(unverified)) {
      staged_md5[unverified] <- unname(as.character(
        tools::md5sum(staged_paths[unverified])
      ))
    }
    if (!identical(staged_md5, expected)) {
      stop(
        "The artifact generation could not be committed safely.",
        call. = FALSE
      )
    }
    staged_fingerprints <- lapply(seq_along(staged_paths), function(index) {
      builder_project_file_fingerprint_with_md5(
        staged_paths[[index]],
        staged_md5[[index]]
      )
    })
    if (any(vapply(staged_fingerprints, is.null, logical(1)))) {
      stop("The staged artifact generation is incomplete.", call. = FALSE)
    }
    stored_primary <- staged_fingerprints[[1L]]
    stored_members <- lapply(seq_along(members), function(index) {
      list(
        target = members[[index]]$target,
        path = paste(relative_dir, members[[index]]$target, sep = "/"),
        fingerprint = staged_fingerprints[[index + 1L]]
      )
    })
    if (
      !spatial_closure_available(
        primary_target,
        stored_members,
        stored_primary
      )
    ) {
      stop(
        "The staged artifact spatial molecule closure is not portable.",
        call. = FALSE
      )
    }
    if (!file.rename(staging, generation_dir)) {
      if (dir.exists(generation_dir)) {
        return(read_existing_generation())
      }
      stop(
        "The artifact generation could not be committed safely.",
        call. = FALSE
      )
    }
    generation_committed <- TRUE
    return(list(
      path = paste(relative_dir, basename(built), sep = "/"),
      fingerprint = stored_primary,
      members = stored_members
    ))
  }
  read_existing_generation()
}

.builder_project_artifact_closure_cache <- new.env(parent = emptyenv())

builder_project_artifact_spatial_closure_available <- function(
  path,
  members,
  fingerprint = NULL
) {
  capture <- get0(
    ".builder_app_capture_spatial_molecule_identity",
    mode = "function",
    inherits = TRUE
  )
  if (!is.function(capture)) {
    return(TRUE)
  }
  cache_key <- paste(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    fingerprint$bytes %||% "",
    fingerprint$modified_at %||% "",
    fingerprint$changed_at %||% "",
    fingerprint$md5 %||% "",
    sep = "\r"
  )
  cached <- get0(
    cache_key,
    envir = .builder_project_artifact_closure_cache,
    inherits = FALSE
  )
  required <- if (is.list(cached) && identical(names(cached), "required")) {
    cached$required
  } else {
    identity <- tryCatch(capture(path), error = function(error) error)
    if (inherits(identity, "condition")) {
      return(FALSE)
    }
    entries <- identity$entries %||% list()
    value <- vapply(
      Filter(function(entry) identical(entry$type, "file"), entries),
      function(entry) gsub("\\\\", "/", entry$path),
      character(1)
    )
    assign(
      cache_key,
      list(required = value),
      envir = .builder_project_artifact_closure_cache
    )
    value
  }
  if (!length(required)) {
    return(TRUE)
  }
  recorded <- vapply(
    members,
    function(member) gsub("\\\\", "/", member$target %||% ""),
    character(1)
  )
  all(required %in% recorded)
}

builder_project_artifact_available <- function(
  artifact,
  root,
  observed = NULL,
  verification_cache = NULL
) {
  if (
    !is.list(artifact) ||
      !identical(artifact$status, "ready") ||
      isFALSE(artifact$reusable %||% TRUE)
  ) {
    return(FALSE)
  }
  reference <- artifact$path %||% ""
  metadata <- observed[[reference]]
  path <- metadata$path %||%
    tryCatch(
      builder_project_resolve_path(reference, root, "managed"),
      error = function(error) NULL
    )
  metadata <- metadata %||%
    if (.builder_project_text(path)) {
      builder_project_file_fingerprint(path, content = FALSE)
    } else {
      NULL
    }
  if (is.null(metadata) || isFALSE(metadata$exists %||% TRUE)) {
    return(FALSE)
  }
  if (
    !builder_project_managed_file_matches(
      artifact$fingerprint,
      path,
      metadata = metadata,
      verification_cache = verification_cache
    )
  ) {
    return(FALSE)
  }
  members <- artifact$members %||% list()
  if (!is.list(members)) {
    return(FALSE)
  }
  members_available <- all(vapply(
    members,
    function(member) {
      reference <- member$path %||% ""
      metadata <- observed[[reference]]
      member_path <- metadata$path %||%
        tryCatch(
          builder_project_resolve_path(reference, root, "managed"),
          error = function(error) NULL
        )
      metadata <- metadata %||%
        if (.builder_project_text(member_path)) {
          builder_project_file_fingerprint(member_path, content = FALSE)
        } else {
          NULL
        }
      if (is.null(metadata) || isFALSE(metadata$exists %||% TRUE)) {
        return(FALSE)
      }
      builder_project_managed_file_matches(
        member$fingerprint,
        member_path,
        metadata = metadata,
        verification_cache = verification_cache
      )
    },
    logical(1)
  ))
  no_spatial_expected <- is.list(artifact$plan_item) &&
    !length(
      artifact$plan_item$artifact_identity$spatial_sections %||% character()
    )
  current_closure <- isTRUE(no_spatial_expected) ||
    (.builder_project_text(artifact$built_from_configuration %||% NULL) &&
      is.list(artifact$plan_item) &&
      identical(artifact$bundle_contract_version %||% NULL, 1L))
  isTRUE(members_available) &&
    (isTRUE(current_closure) ||
      builder_project_artifact_spatial_closure_available(
        path,
        members,
        artifact$fingerprint
      ))
}

builder_project_artifact_matches_entry <- function(
  artifact,
  entry,
  source = NULL,
  identity_cache = NULL,
  .verified_artifact_digest = NULL
) {
  is.null(builder_project_artifact_entry_mismatch(
    artifact,
    entry,
    source = source,
    identity_cache = identity_cache,
    .verified_artifact_digest = .verified_artifact_digest
  ))
}

## NULL when `artifact` was built from `entry` (and its source), otherwise
## the reason it was not.
builder_project_artifact_entry_mismatch <- function(
  artifact,
  entry,
  source = NULL,
  identity_cache = NULL,
  .verified_artifact_digest = NULL
) {
  if (!is.list(artifact) || !is.list(entry)) {
    return("The dataset cache or its configuration is missing.")
  }
  runtime_configuration <- entry$runtime_project_configuration %||% list()
  contract_version <- tryCatch(
    .builder_project_contract_version(
      runtime_configuration$contract_version %||%
        .builder_project_configuration_contract_version
    ),
    error = function(error) NA_integer_
  )
  if (is.na(contract_version)) {
    return("The saved configuration uses an unsupported contract version.")
  }
  verified_artifact_digest <- as.character(
    runtime_configuration$artifact_digest %||% ""
  )
  supplied_artifact_digest <- as.character(
    .verified_artifact_digest %||% ""
  )
  current_artifact_digest <- if (
    isTRUE(runtime_configuration$verified) &&
      grepl("^[[:xdigit:]]{32}$", verified_artifact_digest) &&
      identical(supplied_artifact_digest, verified_artifact_digest)
  ) {
    verified_artifact_digest
  } else {
    builder_project_cached_configuration_digest(
      entry,
      identity_cache,
      digest = builder_project_artifact_configuration_digest,
      variant = paste0("project-artifact-v", contract_version)
    )
  }
  stored_artifact_digest <- as.character(
    artifact$built_from_configuration %||% ""
  )
  if (!identical(stored_artifact_digest, current_artifact_digest)) {
    return("The dataset cache was built from a different configuration.")
  }
  artifact_source <- artifact$built_from_source_fingerprint %||% NULL
  current_source <- entry$snapshot$source_fingerprint %||% NULL
  current_md5 <- builder_project_source_fingerprint_md5(current_source)
  if (
    is.null(current_md5) &&
      is.list(source$fingerprint %||% NULL) &&
      .builder_project_text(source$fingerprint$md5 %||% NULL)
  ) {
    current_md5 <- tolower(as.character(source$fingerprint$md5))
  }
  source_matches <- if (!is.null(current_md5)) {
    identical(
      builder_project_source_fingerprint_md5(artifact_source),
      current_md5
    )
  } else {
    !.builder_project_text(current_source) ||
      identical(
        as.character(artifact_source %||% ""),
        as.character(current_source)
      )
  }
  if (!isTRUE(source_matches)) {
    return("The dataset cache was built from a different source file.")
  }
  NULL
}

builder_project_entries_requiring_crb <- function(
  entries,
  artifacts,
  root,
  identity_cache = NULL,
  verification_cache = NULL
) {
  Filter(
    function(entry) {
      artifact <- artifacts[[entry$id]] %||% NULL
      !is.list(artifact) ||
        !builder_project_artifact_available(
          artifact,
          root,
          verification_cache = verification_cache
        ) ||
        !builder_project_artifact_matches_entry(
          artifact,
          entry,
          identity_cache = identity_cache
        )
    },
    entries
  )
}

builder_project_checkpoint_state <- function(
  entry,
  checked = FALSE,
  artifact = NULL,
  root = NULL,
  job = NULL,
  identity_cache = NULL,
  verification_cache = NULL
) {
  if (!is.list(entry) || !.builder_project_text(entry$id %||% NULL)) {
    stop(
      "A dataset entry is required to derive checkpoint state.",
      call. = FALSE
    )
  }
  checked <- isTRUE(checked)
  if (!checked) {
    return(list(
      status = "unchecked",
      checked = FALSE,
      crb_ready = FALSE
    ))
  }
  artifact_ready <-
    .builder_project_text(root %||% NULL) &&
    is.list(artifact) &&
    builder_project_artifact_available(
      artifact,
      root,
      verification_cache = verification_cache
    ) &&
    builder_project_artifact_matches_entry(
      artifact,
      entry,
      identity_cache = identity_cache
    )
  if (artifact_ready) {
    return(list(
      status = "ready",
      checked = TRUE,
      crb_ready = TRUE
    ))
  }
  job_status <- as.character(job$status %||% "")
  if (job_status %in% c("queued", "generating", "registering")) {
    return(list(
      status = "generating",
      checked = TRUE,
      crb_ready = FALSE
    ))
  }
  list(
    status = "no_crb",
    checked = TRUE,
    crb_ready = FALSE
  )
}

builder_project_checkpoint_states <- function(
  entries,
  checked_ids = character(),
  artifacts = list(),
  root = NULL,
  jobs = list(),
  identity_cache = NULL,
  verification_cache = NULL
) {
  states <- lapply(entries %||% list(), function(entry) {
    builder_project_checkpoint_state(
      entry,
      checked = entry$id %in% checked_ids,
      artifact = artifacts[[entry$id]] %||% entry$project_artifact %||% NULL,
      root = root,
      job = jobs[[entry$id]] %||% NULL,
      identity_cache = identity_cache,
      verification_cache = verification_cache
    )
  })
  names(states) <- vapply(entries %||% list(), `[[`, character(1), "id")
  states
}

builder_project_artifact_source_fingerprint <- function(
  entry,
  plan_item,
  previous_artifact = NULL
) {
  current <- entry$snapshot$source_fingerprint %||% NULL
  if (.builder_project_text(current)) {
    return(as.character(current))
  }
  previous <- previous_artifact$built_from_source_fingerprint %||% NULL
  if (is.list(plan_item$reused_artifact) && .builder_project_text(previous)) {
    return(as.character(previous))
  }
  NULL
}

## What a Project dataset-cache job runs in its background R process. An
## installed Builder has no package source among `runtime_files`, so
## `load_package` attaches CerebroNexus for the exporter functions.
builder_project_crb_job_expression <- function() {
  quote({
    if (isTRUE(load_package)) {
      suppressPackageStartupMessages(library(CerebroNexus))
    }
    runtime <- new.env(parent = globalenv())
    runtime$`%||%` <- function(left, right) {
      if (is.null(left)) right else left
    }
    for (runtime_file in runtime_files) {
      source_utf8(runtime_file, runtime)
    }
    snapshots <- lapply(
      plan$source_snapshot_identities,
      function(identity) {
        if (is.null(identity$snapshot)) NULL else identity$snapshot
      }
    )
    value <- runtime$builder_execute_plan(plan, output, snapshots)
    runtime$builder_project_prepare_build_artifacts(
      plan = list(items = plan$items),
      value = value,
      root = root,
      previous_artifacts = previous_artifacts,
      entries = list(entry),
      promote = TRUE
    )
  })
}

## A cache build that stopped on an optional analysis has no `error`; its
## reason is the failed line of the analysis log (marked with a cross:
## "<dataset>: <cross> <analysis>: <reason>"). Keep only that line so the rail
## row stays short.
builder_project_build_failure_text <- function(value) {
  log <- as.character(unlist(value$analysis_log %||% character()))
  failed <- log[grepl("\u2717", log, fixed = TRUE)]
  if (length(failed)) {
    first <- strsplit(failed[[1L]], "\n", fixed = TRUE)[[1L]][[1L]]
    return(paste0(
      "The dataset cache stopped on an optional analysis: ",
      trimws(sub("\\s*\u2717\\s*", " ", first))
    ))
  }
  failures <- as.character(unlist(value$failures %||% character()))
  if (length(failures) && nzchar(failures[[1L]])) {
    return(failures[[1L]])
  }
  "Dataset caches could not be prepared."
}

builder_project_prepare_build_artifacts <- function(
  plan,
  value,
  root,
  previous_artifacts,
  entries,
  promote = FALSE
) {
  build_succeeded <- is.list(value) &&
    identical(value$state, "success") &&
    length(value$built %||% character()) > 0L
  artifacts <- previous_artifacts %||% list()
  entry_ids <- vapply(entries %||% list(), `[[`, character(1), "id")
  registration_error <- if (build_succeeded) {
    NULL
  } else {
    value$error %||% builder_project_build_failure_text(value)
  }
  for (item in if (build_succeeded) plan$items else list()) {
    if (!builder_project_plan_artifact_reusable(item)) {
      registration_error <- paste0(
        "The prepared cache for ",
        item$name,
        " still depends on files outside its artifact bundle."
      )
      break
    }
    stored_item <- item
    stored_item[c(
      "reused_artifact",
      "recommendations",
      "readiness",
      "nomenclature",
      "histology_coverage",
      "group_color_overrides",
      "color_custom_count"
    )] <- NULL
    entry_index <- match(item$id, entry_ids)
    previous_artifact <- previous_artifacts[[item$id]] %||% NULL
    if (is.list(item$reused_artifact)) {
      previous_path <- tryCatch(
        builder_project_resolve_path(
          previous_artifact$path %||% "",
          root,
          "managed"
        ),
        error = function(error) NULL
      )
      member_signature <- function(members) {
        lapply(members %||% list(), function(member) {
          list(
            target = member$target %||% NULL,
            fingerprint = member$fingerprint %||% list()
          )
        })
      }
      reusable_unchanged <-
        is.list(previous_artifact) &&
        builder_project_artifact_available(previous_artifact, root) &&
        .builder_project_text(previous_path %||% "") &&
        identical(
          normalizePath(
            item$reused_artifact$path,
            winslash = "/",
            mustWork = TRUE
          ),
          normalizePath(previous_path, winslash = "/", mustWork = TRUE)
        ) &&
        identical(
          item$reused_artifact$fingerprint %||% list(),
          previous_artifact$fingerprint %||% list()
        ) &&
        identical(
          member_signature(item$reused_artifact$members),
          member_signature(previous_artifact$members)
        )
      if (!isTRUE(reusable_unchanged)) {
        registration_error <- paste0(
          "The reusable Project artifact for ",
          item$name,
          " changed during Build."
        )
        break
      }
      artifacts[[item$id]] <- previous_artifact
      artifacts[[item$id]]$plan_payload <- jsonlite::serializeJSON(
        stored_item,
        digits = NA,
        pretty = FALSE
      )
      next
    }
    built <- value$built[[item$id]] %||% NULL
    if (!.builder_project_text(built) || !file.exists(built)) {
      registration_error <- paste0(
        "The dataset cache for ",
        item$name,
        " is missing."
      )
      break
    }
    spatial_sidecar <- tryCatch(
      builder_project_verified_spatial_sidecar(
        built,
        value$verifications[[item$id]] %||% list()
      ),
      error = identity
    )
    if (inherits(spatial_sidecar, "condition")) {
      registration_error <- conditionMessage(spatial_sidecar)
      break
    }
    bundle <- tryCatch(
      builder_project_store_artifact_bundle(
        built,
        sidecars = c(
          item$sidecars %||% character(),
          spatial_sidecar %||% character()
        ),
        dataset_id = item$id,
        root = root,
        promote = isTRUE(promote),
        verified_release = value$release %||% NULL
      ),
      error = identity
    )
    if (inherits(bundle, "condition")) {
      registration_error <- conditionMessage(bundle)
      break
    }
    revision <- if (!is.na(entry_index)) {
      entries[[entry_index]]$revision %||% 0L
    } else {
      0L
    }
    artifacts[[item$id]] <- list(
      status = "ready",
      reusable = TRUE,
      bundle_contract_version = 1L,
      path = bundle$path,
      fingerprint = bundle$fingerprint,
      built_from_revision = as.integer(revision),
      built_from_source_fingerprint = if (!is.na(entry_index)) {
        builder_project_artifact_source_fingerprint(
          entries[[entry_index]],
          item,
          previous_artifacts[[item$id]] %||% NULL
        )
      } else {
        NULL
      },
      built_from_configuration = if (!is.na(entry_index)) {
        builder_project_artifact_configuration_digest(entries[[entry_index]])
      } else {
        NULL
      },
      built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      plan_payload = jsonlite::serializeJSON(
        stored_item,
        digits = NA,
        pretty = FALSE
      ),
      members = bundle$members
    )
  }
  list(
    artifacts = artifacts,
    error = registration_error,
    build_succeeded = build_succeeded,
    completed = length(value$built %||% character())
  )
}

builder_project_entries_for_build <- function(
  entries,
  artifacts,
  root,
  identity_cache = NULL,
  verification_cache = NULL
) {
  required <- builder_project_entries_requiring_crb(
    entries,
    artifacts,
    root,
    identity_cache,
    verification_cache = verification_cache
  )
  required_ids <- vapply(required, `[[`, character(1), "id")
  lapply(entries, function(entry) {
    if (
      identical(entry$load_state %||% "loaded", "artifact_ready") ||
        entry$id %in% required_ids
    ) {
      return(entry)
    }
    builder_project_artifact_entry(entry, artifacts[[entry$id]], root)
  })
}

builder_project_saved_configuration_status <- function(record, root) {
  source <- record$source %||% list()
  configuration_path <- record$configuration$path %||% NULL
  if (
    identical(source$kind, "external") &&
      identical(source$path, configuration_path)
  ) {
    stop("A project file reference is ambiguous.", call. = FALSE)
  }
  verified <- tryCatch(
    builder_project_read_dataset_config(record, root, details = TRUE),
    error = identity
  )
  entry <- if (inherits(verified, "condition")) NULL else verified$config
  signature <- builder_project_status_stat_signature(
    record,
    root,
    entry,
    needs = if (is.list(entry)) {
      c("source", "artifact", "spatial", "table")
    } else {
      "source"
    }
  )
  if (is.list(verified$fingerprint %||% NULL)) {
    path <- verified$path
    signature[[record$configuration$path]] <- list(
      path = path,
      exists = TRUE,
      bytes = verified$fingerprint$bytes,
      modified_at = verified$fingerprint$modified_at,
      changed_at = verified$fingerprint$changed_at,
      stat_version = verified$fingerprint$stat_version,
      file_id = verified$fingerprint$file_id
    )
  }
  if (!is.list(entry)) {
    return(list(
      entry = NULL,
      error = conditionMessage(verified),
      signature = signature
    ))
  }
  entry$id <- as.character(record$id %||% entry$id %||% "dataset")
  entry <- tryCatch(
    {
      entry <- builder_project_restore_spatial_assets(
        entry,
        root,
        observed = signature
      )
      builder_project_restore_table_assets(entry, root, observed = signature)
    },
    error = identity
  )
  list(
    entry = if (inherits(entry, "condition")) NULL else entry,
    error = if (inherits(entry, "condition")) conditionMessage(entry) else NULL,
    signature = signature
  )
}

builder_project_status_stat_signature <- function(
  record,
  root,
  entry = NULL,
  needs = c("configuration", "source", "artifact", "spatial", "table")
) {
  needs <- intersect(
    unique(as.character(needs %||% character())),
    c("configuration", "source", "artifact", "spatial", "table")
  )
  references <- list()
  add <- function(path, kind = "managed", category) {
    if (category %in% needs && .builder_project_text(path %||% NULL)) {
      kind <- if (identical(kind, "external")) "external" else "managed"
      key <- as.character(path)
      if (
        is.list(references[[key]]) &&
          !identical(references[[key]]$kind, kind)
      ) {
        stop("A project file reference is ambiguous.", call. = FALSE)
      }
      references[[key]] <<- list(path = path, kind = kind)
    }
  }
  configuration <- record$configuration %||% list()
  add(configuration$path %||% NULL, category = "configuration")
  source <- record$source %||% list()
  if (!identical(source$kind %||% "managed", "example")) {
    add(
      source$path %||% NULL,
      source$kind %||% "managed",
      category = "source"
    )
  }
  artifact <- record$artifact %||% list()
  add(artifact$path %||% NULL, category = "artifact")
  for (member in artifact$members %||% list()) {
    add(member$path %||% NULL, category = "artifact")
  }
  if (is.list(entry)) {
    .builder_project_map_spatial_images(entry, function(image, section, label) {
      if (is.list(image)) {
        add(image$project_asset$path %||% NULL, category = "spatial")
      }
      image
    })
    for (table in entry$settings$tables %||% list()) {
      if (is.list(table$project_asset %||% NULL)) {
        add(table$project_asset$path %||% NULL, category = "table")
      }
    }
  }
  unresolved_prefix <- "\001unresolved\001"
  managed <- vapply(
    references,
    function(reference) {
      !identical(reference$kind, "external")
    },
    logical(1)
  )
  normalized_root <- if (any(managed)) {
    builder_project_normalize_root(root)
  } else {
    root
  }
  resolved <- vapply(
    references,
    function(reference) {
      resolved_path <- tryCatch(
        builder_project_resolve_path(
          reference$path,
          normalized_root,
          reference$kind,
          .root_normalized = !identical(reference$kind, "external")
        ),
        error = function(error) {
          paste0(
            unresolved_prefix,
            reference$kind,
            "\001",
            reference$path
          )
        }
      )
      as.character(resolved_path)
    },
    character(1)
  )
  unsafe <- startsWith(resolved, unresolved_prefix)
  paths <- sort(unique(resolved[!unsafe]), method = "radix")
  fingerprints <- stats::setNames(.builder_project_files_metadata(paths), paths)
  stats::setNames(
    lapply(seq_along(references), function(index) {
      if (unsafe[[index]]) {
        return(list(exists = FALSE, unsafe = TRUE))
      }
      path <- resolved[[index]]
      fingerprint <- fingerprints[[path]]
      if (is.null(fingerprint)) {
        return(list(path = path, exists = FALSE))
      }
      list(
        path = path,
        exists = TRUE,
        bytes = fingerprint$bytes,
        modified_at = fingerprint$modified_at,
        changed_at = fingerprint$changed_at,
        stat_version = fingerprint$stat_version,
        file_id = fingerprint$file_id
      )
    }),
    names(references)
  )
}

builder_project_status_snapshot_fresh <- function(
  status,
  record,
  root,
  needs = c("configuration", "source", "artifact", "spatial", "table")
) {
  if (!is.list(status) || !is.list(status$stat_signature %||% NULL)) {
    return(FALSE)
  }
  current_signature <- builder_project_status_stat_signature(
    record,
    root,
    status$configuration_entry %||% NULL,
    needs = needs
  )
  identical(
    status$stat_signature[names(current_signature)],
    current_signature
  )
}

builder_project_record_configuration_confirmed <- function(record) {
  if (!is.list(record) || !is.list(record$configuration)) {
    return(FALSE)
  }
  configuration <- record$configuration
  checked <- isTRUE(configuration$checked) || isTRUE(configuration$reviewed)
  checked_digest <- if (isTRUE(configuration$checked)) {
    configuration$checked_digest %||% configuration$digest %||% ""
  } else {
    configuration$reviewed_digest %||% configuration$digest %||% ""
  }
  checked &&
    identical(
      as.character(checked_digest),
      as.character(configuration$digest %||% "")
    )
}

builder_project_dataset_status <- function(
  record,
  root,
  format_capabilities = builder_format_capabilities()
) {
  configuration <- builder_project_saved_configuration_status(record, root)
  entry <- configuration$entry %||% NULL
  signature <- configuration$signature %||% list()
  source <- record$source %||% list()
  source_kind <- as.character(source$kind %||% "managed")
  source_status <- as.character(source$status %||% "")
  source_is_example <- identical(source_kind, "example")
  source_reference <- source$path %||% ""
  source_observation <- if (source_is_example) {
    NULL
  } else {
    signature[[source_reference]]
  }
  source_path <- if (source_is_example) {
    NULL
  } else {
    source_observation$path %||%
      tryCatch(
        builder_project_resolve_path(
          source_reference,
          root,
          source_kind
        ),
        error = function(error) NULL
      )
  }
  source_available <- source_is_example ||
    (!identical(source_kind, "missing") &&
      !source_status %in% c("pending", "failed") &&
      is.list(source_observation) &&
      !isFALSE(source_observation$exists %||% TRUE))
  source_format <- if (source_is_example) {
    NULL
  } else {
    builder_path_format_capability(
      source$filename %||% source_path %||% "",
      capabilities = format_capabilities
    )
  }
  source_format_problem <- if (
    source_available &&
      is.list(source_format) &&
      identical(source_format$available, FALSE)
  ) {
    source_format$unavailable_reason
  } else {
    NULL
  }
  source_loadable <- source_available && is.null(source_format_problem)
  recorded_md5 <- builder_project_recorded_fingerprint_md5(
    source$fingerprint %||% NULL
  )
  source_state <- if (source_is_example) {
    "matched"
  } else if (!source_available) {
    "missing"
  } else if (
    .builder_project_text(recorded_md5) &&
      builder_project_managed_file_matches(
        source$fingerprint,
        source_path,
        verify_content = FALSE,
        metadata = source_observation
      )
  ) {
    "matched"
  } else if (.builder_project_text(recorded_md5)) {
    "verify"
  } else {
    "reload"
  }
  configuration_ready <- is.list(entry)
  artifact_ready <- configuration_ready &&
    builder_project_artifact_available(
      record$artifact,
      root,
      observed = signature
    ) &&
    builder_project_artifact_matches_entry(
      record$artifact,
      entry,
      source,
      .verified_artifact_digest = entry$runtime_project_configuration$artifact_digest %||%
        NULL
    )
  actions <- character()
  if (artifact_ready) {
    actions <- c(actions, "reuse")
  }
  if (
    source_loadable &&
      configuration_ready &&
      source_state %in% c("matched", "verify")
  ) {
    actions <- c(actions, "resume")
  } else if (source_loadable) {
    actions <- c(actions, "reload")
  }
  actions <- c(actions, "skip")
  checked <- builder_project_record_configuration_confirmed(record)
  default_action <- if (!isTRUE(record$release$included %||% TRUE)) {
    "skip"
  } else if (artifact_ready && checked) {
    "reuse"
  } else if ("resume" %in% actions) {
    "resume"
  } else if ("reload" %in% actions) {
    "reload"
  } else if ("reuse" %in% actions) {
    "reuse"
  } else {
    "skip"
  }
  message <- if (!is.null(source_format_problem) && !artifact_ready) {
    "Source reader unavailable"
  } else if (!configuration_ready) {
    "Settings unavailable"
  } else if (artifact_ready && checked) {
    "Checked · Cached"
  } else if (checked) {
    "Checked · Not cached"
  } else if (source_available || artifact_ready) {
    "Needs checking"
  } else {
    "Source unavailable"
  }
  problem <- source_format_problem %||% configuration$error %||% NULL
  if (
    is.null(problem) &&
      is.list(record$source) &&
      length(record$source) > 0L &&
      !any(actions %in% c("reuse", "resume", "reload"))
  ) {
    problem <- "No dataset cache or available source was found."
  }
  detail <- if (!is.null(source_format_problem) && artifact_ready) {
    paste(
      "The cached copy can still be used without loading the original source.",
      source_format_problem
    )
  } else if (artifact_ready && checked && identical(source_state, "verify")) {
    paste(
      "The cached copy can be used immediately.",
      "The source will be verified only if you load it."
    )
  } else if (artifact_ready && checked) {
    "The cached copy can be used immediately; the source is not loaded."
  } else if (checked && identical(source_state, "verify")) {
    "The original source will be verified while loading with saved settings."
  } else if (checked && source_available) {
    "The original source will load with saved settings."
  } else if (checked) {
    problem
  } else {
    NULL
  }
  status_checked <- configuration_ready && checked
  list(
    actions = actions,
    default_action = default_action,
    checked = status_checked,
    crb_ready = artifact_ready && status_checked,
    configuration_entry = entry,
    stat_signature = signature,
    message = message,
    detail = detail,
    problem = problem
  )
}

builder_project_validate_external_sources <- function(manifest, roots) {
  if (!is.list(manifest)) {
    stop("A Builder Project manifest is required.", call. = FALSE)
  }
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    source <- record$source %||% list()
    if (!identical(source$kind, "external")) {
      return(record)
    }
    label <- as.character(
      record$name %||% source$filename %||% record$id %||% "dataset"
    )
    resolved <- tryCatch(
      builder_server_path_resolve(source$path, type = "file", roots = roots),
      error = identity
    )
    if (inherits(resolved, "condition")) {
      stop(
        "External source for ",
        label,
        " is not allowed: ",
        conditionMessage(resolved),
        call. = FALSE
      )
    }
    record$source$path <- resolved
    record
  })
  manifest
}

builder_project_open_snapshot <- function(path, external_roots = NULL) {
  root <- dirname(path)
  snapshot <- builder_project_read_stable_manifest(path)
  manifest <- snapshot$manifest
  if (!is.null(external_roots)) {
    manifest <- builder_project_validate_external_sources(
      manifest,
      roots = external_roots
    )
  }
  format_capabilities <- builder_format_capabilities()
  manifest$datasets <- lapply(
    manifest$datasets %||% list(),
    function(record) {
      record$runtime_restore_status <- builder_project_dataset_status(
        record,
        root,
        format_capabilities = format_capabilities
      )
      record
    }
  )
  list(
    manifest = manifest,
    root = root,
    path = path,
    manifest_fingerprint = snapshot$fingerprint,
    legacy_qs = builder_project_legacy_qs_datasets(manifest$datasets)
  )
}

builder_project_new_manifest <- function(root, name = basename(root)) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  list(
    schema_version = .builder_project_schema_version,
    project = list(
      id = builder_project_id(),
      name = as.character(name),
      revision = 0L,
      created_at = now,
      updated_at = now,
      builder_version = as.character(
        tryCatch(
          utils::packageVersion("CerebroNexus"),
          error = function(error) "development"
        )
      )
    ),
    datasets = list(),
    configuration = builder_project_configuration(),
    pending_build = NULL,
    releases = list(),
    last_ui = list(stage = "upload", selected_dataset = NULL)
  )
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

builder_project_normalize_manifest <- function(manifest) {
  configuration <- manifest$configuration %||% list()
  manifest$configuration <- builder_project_configuration(
    review_options = configuration$review_options %||% NULL,
    build_mode = configuration$build_mode %||% FALSE,
    auth_enabled = configuration$auth_enabled %||% FALSE,
    initial_dataset = configuration$initial_dataset %||% NULL
  )
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    record$cache <- NULL
    record$runtime_restore_status <- NULL
    record$runtime_restore_action <- NULL
    record
  })
  last_ui <- manifest$last_ui %||% list()
  stage <- as.character(last_ui$stage %||% "configure")
  if (
    length(stage) != 1L ||
      is.na(stage) ||
      !stage %in% c("upload", "configure", "review", "build")
  ) {
    stage <- "configure"
  }
  selected <- last_ui$selected_dataset %||% NULL
  if (!is.null(selected)) {
    selected <- as.character(selected)
    if (length(selected) != 1L || is.na(selected) || !nzchar(selected)) {
      selected <- NULL
    }
  }
  manifest$last_ui <- list(
    stage = stage,
    selected_dataset = selected,
    spatial = builder_project_last_ui_spatial(
      last_ui$spatial %||% NULL,
      selected_dataset = selected
    )
  )
  manifest
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

builder_project_read_manifest_payload <- function(path, payload = NULL) {
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    stop("Choose a Builder project JSON file.", call. = FALSE)
  }
  input_bytes <- if (is.raw(payload)) {
    length(payload)
  } else {
    suppressWarnings(as.double(file.info(path)$size[[1L]]))
  }
  if (
    !is.finite(input_bytes) ||
      input_bytes < 1 ||
      input_bytes > .builder_project_manifest_max_bytes
  ) {
    stop("The Builder project exceeds its input size limit.", call. = FALSE)
  }
  if (!is.raw(payload)) {
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    payload <- readBin(
      connection,
      what = "raw",
      n = as.integer(.builder_project_manifest_max_bytes + 1L)
    )
  }
  if (length(payload) > .builder_project_manifest_max_bytes) {
    stop("The Builder project exceeds its input size limit.", call. = FALSE)
  }
  manifest <- jsonlite::fromJSON(
    rawToChar(payload),
    simplifyVector = FALSE
  )
  schema_version <- tryCatch(
    .builder_project_stored_integer(manifest$schema_version),
    error = function(error) NA_integer_
  )
  project_revision <- tryCatch(
    if (is.list(manifest$project)) {
      .builder_project_stored_integer(manifest$project$revision, 0L)
    } else {
      NA_integer_
    },
    error = function(error) NA_integer_
  )
  if (
    !is.list(manifest) ||
      !identical(schema_version, .builder_project_schema_version) ||
      !is.list(manifest$project) ||
      !.builder_project_identifier(manifest$project$id) ||
      is.na(project_revision) ||
      !is.list(manifest$datasets)
  ) {
    stop("This is not a supported Builder project.", call. = FALSE)
  }
  dataset_ids <- vapply(
    manifest$datasets,
    function(record) {
      if (!is.list(record) || !.builder_project_identifier(record$id)) {
        return(NA_character_)
      }
      as.character(record$id)
    },
    character(1)
  )
  if (anyNA(dataset_ids) || anyDuplicated(dataset_ids)) {
    stop(
      "The Builder project contains unsafe or duplicate dataset ids.",
      call. = FALSE
    )
  }
  manifest$schema_version <- schema_version
  manifest$project$revision <- project_revision
  manifest
}

builder_project_read <- function(path) {
  manifest <- builder_project_read_manifest_payload(path)
  builder_project_normalize_manifest(manifest)
}

builder_project_read_stable_manifest <- function(
  path,
  .read = builder_project_read_manifest_payload,
  .metadata = builder_project_file_metadata
) {
  before <- .metadata(path)
  if (
    !is.list(before) ||
      !is.finite(suppressWarnings(as.double(before$bytes %||% NA_real_))) ||
      before$bytes < 1 ||
      before$bytes > .builder_project_manifest_max_bytes
  ) {
    stop(
      "The Builder project manifest could not be fingerprinted.",
      call. = FALSE
    )
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  payload <- readBin(
    connection,
    what = "raw",
    n = as.integer(.builder_project_manifest_max_bytes + 1L)
  )
  after <- .metadata(path)
  if (
    length(payload) > .builder_project_manifest_max_bytes ||
      as.double(length(payload)) != as.double(before$bytes) ||
      !is.list(after) ||
      !builder_project_fingerprint_metadata_matches(before, after) ||
      !builder_project_fingerprint_stat_matches(before, after)
  ) {
    stop(
      "The Builder project manifest changed while it was being read.",
      call. = FALSE
    )
  }
  manifest <- builder_project_normalize_manifest(
    .read(path, payload = payload)
  )
  after$md5 <- unclass(as.character(digest::digest(
    payload,
    algo = "md5",
    serialize = FALSE
  )))
  list(manifest = manifest, fingerprint = after)
}

builder_project_diagnose <- function(path) {
  diagnose_one <- function(candidate) {
    if (!file.exists(candidate) || dir.exists(candidate)) {
      return(list(
        path = candidate,
        exists = FALSE,
        valid = FALSE,
        error = NULL
      ))
    }
    manifest <- tryCatch(
      builder_project_read_manifest_payload(candidate),
      error = identity
    )
    if (inherits(manifest, "condition")) {
      return(list(
        path = candidate,
        exists = TRUE,
        valid = FALSE,
        error = conditionMessage(manifest)
      ))
    }
    integrity <- builder_project_manifest_integrity(
      manifest,
      dirname(candidate),
      scan_previous = TRUE
    )
    list(
      path = candidate,
      exists = TRUE,
      valid = isTRUE(integrity$valid),
      project_revision = manifest$project$revision,
      datasets = integrity$datasets,
      error = NULL
    )
  }
  list(
    manifest = diagnose_one(path),
    backup = diagnose_one(paste0(path, ".bak"))
  )
}

builder_project_backup_recovery_offer <- function(path) {
  current <- tryCatch(
    builder_project_read_stable_manifest(path),
    error = identity
  )
  if (!inherits(current, "condition")) {
    stop(
      "The current Project manifest is readable; backup recovery is not available.",
      call. = FALSE
    )
  }
  primary_fingerprint <- builder_project_file_fingerprint(path, content = TRUE)
  backup_path <- paste0(path, ".bak")
  backup <- builder_project_read_stable_manifest(backup_path)
  builder_project_assert_manifest_integrity(
    backup$manifest,
    dirname(path)
  )
  list(
    path = path,
    backup_path = backup_path,
    primary_error = conditionMessage(current),
    primary_fingerprint = primary_fingerprint,
    backup_fingerprint = backup$fingerprint,
    project_name = backup$manifest$project$name %||% basename(dirname(path)),
    project_revision = backup$manifest$project$revision %||% 0L,
    updated_at = backup$manifest$project$updated_at %||% NULL,
    dataset_count = length(backup$manifest$datasets %||% list())
  )
}

builder_project_recover_backup <- function(
  path,
  expected_primary_fingerprint,
  expected_backup_fingerprint,
  .copy = file.copy,
  .move = file.rename
) {
  root <- dirname(path)
  write_lock <- builder_project_acquire_manifest_lock(root)
  on.exit(builder_project_release_manifest_lock(write_lock), add = TRUE)
  offer <- builder_project_backup_recovery_offer(path)
  if (
    !builder_project_content_fingerprint_matches(
      expected_primary_fingerprint,
      offer$primary_fingerprint
    ) ||
      !builder_project_content_fingerprint_matches(
        expected_backup_fingerprint,
        offer$backup_fingerprint
      )
  ) {
    stop(
      paste(
        "The Project manifest or its backup changed after recovery was offered.",
        "Reopen the Project and review the current files."
      ),
      call. = FALSE
    )
  }
  staged <- tempfile(
    ".builder-project-recovery-",
    tmpdir = root,
    fileext = ".json"
  )
  displaced <- tempfile(
    ".builder-project-corrupt-",
    tmpdir = root,
    fileext = ".json"
  )
  on.exit(unlink(staged, force = TRUE), add = TRUE)
  attempt <- function(action, source, destination, operation) {
    result <- .nativeFileOperationAttempt(
      action = action,
      source = source,
      destination = destination,
      operation = operation
    )
    if (!isTRUE(result$ok)) {
      stop(.nativeFileOperationMessage(result), call. = FALSE)
    }
    invisible(TRUE)
  }
  attempt(
    "stage verified Project backup",
    offer$backup_path,
    staged,
    function() .copy(offer$backup_path, staged, overwrite = FALSE)
  )
  staged_snapshot <- builder_project_read_stable_manifest(staged)
  if (
    !builder_project_content_fingerprint_matches(
      offer$backup_fingerprint,
      staged_snapshot$fingerprint
    )
  ) {
    stop(
      "The staged Project backup changed while it was copied.",
      call. = FALSE
    )
  }
  builder_project_assert_manifest_integrity(
    staged_snapshot$manifest,
    root
  )
  attempt(
    "preserve damaged Project manifest",
    path,
    displaced,
    function() .move(path, displaced)
  )
  committed <- tryCatch(
    {
      attempt(
        "restore verified Project backup",
        staged,
        path,
        function() .move(staged, path)
      )
      restored <- builder_project_read_stable_manifest(path)
      builder_project_assert_manifest_integrity(
        restored$manifest,
        root
      )
      if (
        !builder_project_content_fingerprint_matches(
          offer$backup_fingerprint,
          restored$fingerprint
        )
      ) {
        stop(
          "The restored Project manifest failed fingerprint validation.",
          call. = FALSE
        )
      }
      restored
    },
    error = identity
  )
  if (inherits(committed, "condition")) {
    if (file.exists(path)) {
      unlink(path, force = TRUE)
    }
    rollback <- tryCatch(
      {
        attempt(
          "roll back damaged Project manifest",
          displaced,
          path,
          function() .move(displaced, path)
        )
        TRUE
      },
      error = identity
    )
    if (inherits(rollback, "condition")) {
      stop(
        conditionMessage(committed),
        " The damaged manifest could not be restored from ",
        normalizePath(displaced, winslash = "/", mustWork = FALSE),
        ": ",
        conditionMessage(rollback),
        call. = FALSE
      )
    }
    stop(conditionMessage(committed), call. = FALSE)
  }
  list(
    manifest = committed$manifest,
    path = path,
    backup_path = offer$backup_path,
    displaced_path = normalizePath(displaced, winslash = "/", mustWork = TRUE)
  )
}

builder_project_build_manifest <- function(
  entries,
  project,
  context,
  cache = NULL,
  verification_cache = NULL
) {
  manifest <- project$manifest
  if (
    is.list(manifest$pending_build) &&
      !identical(manifest$pending_build$status %||% NULL, "running")
  ) {
    manifest$pending_build <- NULL
  }
  previous <- manifest$datasets %||% list()
  names(previous) <- vapply(previous, `[[`, character(1), "id")
  checked <- context$checked
  artifacts <- context$artifacts
  staged_entries <- vector("list", length(entries))
  records <- vector("list", length(entries))
  jobs <- vector("list", length(entries))
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    prior <- previous[[entry$id]] %||% NULL
    if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
      staged <- list(
        entry = entry,
        source = prior$source %||% list(kind = "missing", path = NULL),
        job = NULL
      )
    } else {
      staged <- builder_project_prepare_source(entry, project$root, prior)
    }
    configuration_entry <- builder_project_configuration_entry(staged$entry)
    configuration_entry <- builder_project_stage_spatial_assets(
      configuration_entry,
      project$root
    )
    configuration_entry <- builder_project_stage_table_assets(
      configuration_entry,
      project$root
    )
    current_digest <- builder_project_cached_configuration_digest(
      configuration_entry,
      cache,
      variant = "project-manifest"
    )
    artifact_digest <- builder_project_cached_configuration_digest(
      configuration_entry,
      cache,
      digest = builder_project_artifact_configuration_digest,
      variant = "project-artifact"
    )
    staged$entry <- builder_project_adopt_spatial_assets(
      staged$entry,
      configuration_entry,
      project$root
    )
    staged$entry <- builder_project_adopt_table_assets(
      staged$entry,
      configuration_entry,
      project$root
    )
    artifact <- artifacts[[entry$id]] %||%
      entry$project_artifact %||%
      prior$artifact %||%
      NULL
    if (is.list(artifact)) {
      if (
        !identical(
          artifact$built_from_configuration %||% "",
          artifact_digest
        )
      ) {
        artifact <- NULL
      } else {
        if (
          is.list(artifact$plan_item) &&
            !.builder_project_text(artifact$plan_payload %||% "")
        ) {
          artifact$plan_payload <- jsonlite::serializeJSON(
            artifact$plan_item,
            digits = NA,
            pretty = FALSE
          )
        }
        artifact$plan_item <- NULL
        artifact$resolved_path <- NULL
        if (length(artifact$members %||% list())) {
          artifact$members <- lapply(artifact$members, function(member) {
            member$resolved_path <- NULL
            member
          })
        }
      }
    }
    staged_entries[[index]] <- staged$entry
    jobs[[index]] <- staged$job
    records[[index]] <- builder_project_dataset_record(
      staged$entry,
      staged$source,
      checked = entry$id %in% checked,
      artifact = artifact,
      order = index,
      payload_entry = configuration_entry,
      root = project$root,
      configuration_digest = current_digest,
      verification_cache = verification_cache
    )
  }
  retained_ids <- vapply(records, `[[`, character(1), "id")
  inactive <- previous[setdiff(names(previous), retained_ids)]
  if (length(inactive)) {
    skipped_ids <- context$skipped_ids
    inactive <- lapply(inactive, function(record) {
      if (!record$id %in% skipped_ids) {
        record$release <- list(included = FALSE)
      }
      record
    })
    records <- c(records, unname(inactive))
  }
  manifest$datasets <- records
  manifest$configuration <- context$configuration
  manifest$last_ui <- context$last_ui
  list(
    manifest = manifest,
    entries = staged_entries,
    jobs = Filter(Negate(is.null), jobs)
  )
}

builder_project_manifest_with_artifact <- function(
  manifest,
  root,
  id,
  artifact
) {
  project <- list(root = root, manifest = manifest)
  records <- project$manifest$datasets %||% list()
  ids <- vapply(
    records,
    function(record) {
      as.character(record$id %||% "")
    },
    character(1)
  )
  # FALSE carries the failed check as its "reason" attribute.
  rejected <- function(reason) structure(FALSE, reason = reason)
  index <- match(id, ids)
  if (is.na(index)) {
    return(rejected("The dataset is no longer in this project."))
  }
  record <- records[[index]]
  if (!builder_project_record_configuration_confirmed(record)) {
    return(rejected("The saved configuration is no longer checked."))
  }
  saved_entry <- tryCatch(
    builder_project_read_dataset_config(record, project$root),
    error = identity
  )
  if (inherits(saved_entry, "condition")) {
    return(rejected(conditionMessage(saved_entry)))
  }
  if (!builder_project_artifact_available(artifact, project$root)) {
    return(rejected("The dataset cache file is missing or changed."))
  }
  mismatch <- builder_project_artifact_entry_mismatch(
    artifact,
    saved_entry,
    source = record$source %||% NULL
  )
  if (!is.null(mismatch)) {
    return(rejected(mismatch))
  }
  stored <- artifact
  if (
    is.list(stored$plan_item) &&
      !.builder_project_text(stored$plan_payload %||% "")
  ) {
    stored$plan_payload <- jsonlite::serializeJSON(
      stored$plan_item,
      digits = NA,
      pretty = FALSE
    )
  }
  stored$plan_item <- NULL
  stored$resolved_path <- NULL
  if (length(stored$members %||% list())) {
    stored$members <- lapply(stored$members, function(member) {
      member$resolved_path <- NULL
      member
    })
  }
  manifest <- project$manifest
  manifest$datasets[[index]]$artifact <- stored
  manifest
}

## The body of one manifest transaction. `runtime_for(full)` returns the
## Builder runtime; `full = TRUE` adds the app bundle and package files that
## artifact closure checks need.
builder_project_write_task <- function(runtime_for, args) {
  manifest <- args$manifest
  root <- args$root
  entries <- args$entries
  context <- args$context
  artifact_record <- args$artifact_record
  needs_artifacts <- !is.null(artifact_record) ||
    any(vapply(
      manifest$datasets,
      function(record) is.list(record$artifact),
      logical(1)
    )) ||
    any(vapply(
      entries,
      function(entry) is.list(entry$project_artifact),
      logical(1)
    )) ||
    any(vapply(context$artifacts, is.list, logical(1)))
  runtime <- runtime_for(needs_artifacts)
  if (
    needs_artifacts &&
      !is.function(runtime$.builder_app_capture_spatial_molecule_identity)
  ) {
    stop("The project artifact verification runtime is unavailable.")
  }
  cache <- list2env(args$configuration_cache, parent = emptyenv())
  verification_cache <- runtime$builder_project_write_verification_cache()
  built <- if (!is.null(entries)) {
    runtime$builder_project_build_manifest(
      entries,
      list(root = root, manifest = manifest),
      context,
      cache,
      verification_cache = verification_cache
    )
  } else {
    NULL
  }
  if (!is.null(artifact_record)) {
    manifest <- runtime$builder_project_manifest_with_artifact(
      manifest,
      root,
      artifact_record$id,
      artifact_record$artifact
    )
    if (!is.list(manifest)) {
      reason <- attr(manifest, "reason", exact = TRUE)
      stop(
        "The dataset cache no longer matches this project.",
        if (is.character(reason) && length(reason) == 1L) paste0(" ", reason)
      )
    }
  }
  written <- runtime$builder_project_write(
    if (is.null(built)) manifest else built$manifest,
    root,
    args$expected_revision,
    verification_cache = verification_cache
  )
  written$built <- built
  written$configuration_cache <- as.list(cache)
  # The transaction already inspected these CRBs in this worker. Carry the
  # content-keyed closure results back so status rendering need not read them
  # again. Artifact availability still checks every file before using them.
  written$artifact_closure_cache <- as.list(
    runtime$.builder_project_artifact_closure_cache
  )
  written
}

## Readies a warm Project writer before the first Save (`root = NULL`, the
## Project does not exist yet) or for the Project just opened: loads the full
## runtime, compiles it and, with a Project, runs the integrity pass every Save
## repeats. The first Save then finds the code compiled and the unchanged
## sidecars already in the stat-keyed verification cache, instead of paying
## for both while the workspace is locked. Read-only; any failure is left for
## the Save to report.
builder_project_writer_warmup_task <- function(runtime_for, args) {
  runtime <- runtime_for(TRUE)
  # A freshly sourced function is byte-compiled the first time it runs, which
  # was about half a second of the first Save. Compile the Project functions
  # now (about two seconds, all of the write path) instead.
  # Once per loaded runtime: a Project opened after the pre-Save warm-up only
  # needs the integrity pass.
  if (
    !isTRUE(get0(
      ".builder_project_compiled",
      envir = runtime,
      inherits = FALSE
    ))
  ) {
    names <- ls(runtime, pattern = "^[.]?builder_project", all.names = TRUE)
    for (name in names) {
      value <- get(name, envir = runtime, inherits = FALSE)
      if (is.function(value) && !is.primitive(value)) {
        compiled <- tryCatch(compiler::cmpfun(value), error = function(error) {
          NULL
        })
        if (is.function(compiled)) {
          assign(name, compiled, envir = runtime)
        }
      }
    }
    assign(".builder_project_compiled", TRUE, envir = runtime)
  }
  if (
    !is.character(args$root) ||
      length(args$root) != 1L ||
      is.na(args$root) ||
      !nzchar(args$root)
  ) {
    return(FALSE)
  }
  manifest <- tryCatch(
    runtime$builder_project_read(
      runtime$builder_project_manifest_path(args$root)
    ),
    error = function(error) NULL
  )
  if (!is.list(manifest)) {
    return(FALSE)
  }
  status <- tryCatch(
    runtime$builder_project_manifest_integrity(
      manifest,
      args$root,
      verification_cache = runtime$builder_project_write_verification_cache()
    ),
    error = function(error) NULL
  )
  isTRUE(status$valid)
}

## Functions sent to a worker must not carry the Builder environment along.
builder_project_portable_function <- function(func) {
  func <- utils::removeSource(func)
  environment(func) <- globalenv()
  func
}

## Runs `task(runtime_for, args)` inside a Project worker process. The loaded
## runtime is kept in the worker's global environment, so a warm worker only
## sources the Builder files on its first call; the full runtime (package, app
## bundle) is sourced only when a task asks for it, in the original order.
builder_project_worker_run <- function(
  task,
  args,
  runtime_files,
  full_runtime_files,
  source_utf8
) {
  load_runtime <- function(files) {
    runtime <- new.env(parent = globalenv())
    runtime$`%||%` <- function(left, right) if (is.null(left)) right else left
    for (path in files) {
      source_utf8(path, runtime)
    }
    runtime
  }
  state <- get0(
    ".builder_project_worker_state",
    envir = globalenv(),
    inherits = FALSE
  )
  if (
    !is.list(state) ||
      !identical(state$runtime_files, runtime_files) ||
      !identical(state$full_runtime_files, full_runtime_files)
  ) {
    state <- list(
      runtime_files = runtime_files,
      full_runtime_files = full_runtime_files,
      runtime = NULL,
      full = FALSE
    )
  }
  runtime_for <- function(full = FALSE) {
    if (isTRUE(full) && !isTRUE(state$full)) {
      state$runtime <<- load_runtime(full_runtime_files)
      state$full <<- TRUE
    } else if (is.null(state$runtime)) {
      state$runtime <<- load_runtime(runtime_files)
    }
    assign(".builder_project_worker_state", state, envir = globalenv())
    state$runtime
  }
  task(runtime_for, args)
}

builder_project_write_process <- function(
  manifest,
  root,
  expected_revision,
  runtime_files,
  source_utf8,
  .spawn = callr::r_bg,
  entries = NULL,
  context = NULL,
  configuration_cache = list(),
  artifact_record = NULL,
  full_runtime_files = runtime_files,
  worker = NULL
) {
  call_args <- list(
    task = builder_project_portable_function(builder_project_write_task),
    args = list(
      manifest = manifest,
      root = root,
      expected_revision = expected_revision,
      entries = entries,
      context = context,
      configuration_cache = configuration_cache,
      artifact_record = artifact_record
    ),
    runtime_files = runtime_files,
    full_runtime_files = full_runtime_files,
    source_utf8 = source_utf8
  )
  if (!is.null(worker)) {
    return(builder_project_worker_call(
      worker,
      builder_project_worker_run,
      call_args
    ))
  }
  .spawn(
    builder_project_worker_run,
    args = call_args,
    # Once a transaction starts, closing the browser must not kill it between
    # backup and commit. The existing manifest lock/revision checks still apply.
    supervise = FALSE,
    cleanup = FALSE,
    stdout = "|",
    stderr = "|"
  )
}

## Warm Project worker --------------------------------------------------------
## One long-lived R session runs Project transactions in submission order. A
## call returns a handle with the callr process methods the Builder polls
## (is_alive, get_result, kill, kill_tree, wait), so callers keep their own
## stale-result and cancellation logic. A crashed or killed session is replaced
## on the next call. The session is started without process cleanup: when the
## Builder exits, the worker finishes its current transaction and then ends at
## the end of its input.
builder_project_worker_new <- function(
  name = "project",
  .start = function() {
    callr::r_session$new(
      options = callr::r_session_options(extra = list(cleanup = FALSE)),
      wait = FALSE
    )
  }
) {
  worker <- new.env(parent = emptyenv())
  worker$name <- name
  worker$start <- .start
  worker$session <- NULL
  worker$ready <- FALSE
  worker$current <- NULL
  worker$queue <- list()
  worker$closing <- FALSE
  worker$starts <- 0L
  worker
}

.builder_project_worker_session_alive <- function(session) {
  !is.null(session) &&
    isTRUE(tryCatch(session$is_alive(), error = function(error) FALSE))
}

.builder_project_worker_finish <- function(
  handle,
  result = NULL,
  error = NULL
) {
  handle$state <- if (is.null(error)) "completed" else "failed"
  handle$result <- result
  handle$error <- error
  if (identical(handle$worker$current, handle)) {
    handle$worker$current <- NULL
  }
  invisible(handle)
}

.builder_project_worker_drop_session <- function(worker, kill = TRUE) {
  session <- worker$session
  worker$session <- NULL
  worker$ready <- FALSE
  if (!is.null(session) && isTRUE(kill)) {
    try(session$kill_tree(), silent = TRUE)
    try(session$kill(), silent = TRUE)
  }
  invisible(TRUE)
}

.builder_project_worker_error <- function(error) {
  parent <- error
  while (inherits(parent, "callr_error") && !is.null(parent$parent)) {
    parent <- parent$parent
  }
  simpleError(conditionMessage(parent))
}

## Advances the worker without blocking: consumes the session start message,
## collects a finished call and starts the next queued call.
builder_project_worker_pump <- function(worker) {
  repeat {
    session <- worker$session
    current <- worker$current
    if (!is.null(current) && identical(current$state, "running")) {
      if (!.builder_project_worker_session_alive(session)) {
        .builder_project_worker_drop_session(worker, kill = FALSE)
        .builder_project_worker_finish(
          current,
          error = simpleError("The Project worker stopped unexpectedly.")
        )
        next
      }
      ready <- tryCatch(
        identical(session$poll_process(0L), "ready"),
        error = function(error) FALSE
      )
      if (!ready) {
        return(invisible(FALSE))
      }
      message <- tryCatch(session$read(), error = identity)
      if (inherits(message, "condition")) {
        .builder_project_worker_drop_session(worker)
        .builder_project_worker_finish(
          current,
          error = .builder_project_worker_error(message)
        )
        next
      }
      code <- suppressWarnings(as.integer(message$code %||% NA_integer_))
      if (is.na(code)) {
        return(invisible(FALSE))
      }
      if (code >= 500L) {
        .builder_project_worker_drop_session(worker)
        .builder_project_worker_finish(
          current,
          error = simpleError("The Project worker stopped unexpectedly.")
        )
        next
      }
      if (!identical(code, 200L)) {
        # Progress and output messages; keep waiting for the result.
        next
      }
      if (!is.null(message$error)) {
        .builder_project_worker_finish(
          current,
          error = .builder_project_worker_error(message$error)
        )
      } else {
        .builder_project_worker_finish(current, result = message$result)
      }
      next
    }
    if (!is.null(session) && !isTRUE(worker$ready)) {
      if (!.builder_project_worker_session_alive(session)) {
        .builder_project_worker_drop_session(worker, kill = FALSE)
        if (length(worker$queue)) {
          failed <- worker$queue[[1L]]
          worker$queue <- worker$queue[-1L]
          .builder_project_worker_finish(
            failed,
            error = simpleError("The Project worker could not be started.")
          )
        }
        next
      }
      ready <- tryCatch(
        identical(session$poll_process(0L), "ready"),
        error = function(error) FALSE
      )
      if (!ready) {
        return(invisible(FALSE))
      }
      message <- tryCatch(session$read(), error = identity)
      if (
        !inherits(message, "condition") &&
          identical(as.integer(message$code %||% NA_integer_), 201L)
      ) {
        worker$ready <- TRUE
      }
      next
    }
    if (!length(worker$queue)) {
      if (isTRUE(worker$closing) && !is.null(session)) {
        try(session$close(), silent = TRUE)
        worker$session <- NULL
        worker$ready <- FALSE
      }
      return(invisible(TRUE))
    }
    if (is.null(session)) {
      started <- tryCatch(worker$start(), error = identity)
      if (inherits(started, "condition")) {
        failed <- worker$queue[[1L]]
        worker$queue <- worker$queue[-1L]
        .builder_project_worker_finish(
          failed,
          error = .builder_project_worker_error(started)
        )
        next
      }
      worker$session <- started
      worker$ready <- FALSE
      worker$starts <- worker$starts + 1L
      next
    }
    handle <- worker$queue[[1L]]
    worker$queue <- worker$queue[-1L]
    called <- tryCatch(
      {
        session$call(handle$func, handle$args)
        TRUE
      },
      error = identity
    )
    if (inherits(called, "condition")) {
      .builder_project_worker_drop_session(worker)
      .builder_project_worker_finish(
        handle,
        error = .builder_project_worker_error(called)
      )
      next
    }
    handle$state <- "running"
    worker$current <- handle
  }
}

builder_project_worker_call <- function(worker, func, args = list()) {
  if (!is.environment(worker) || !is.function(func)) {
    stop(
      "A Project worker call requires a worker and a function.",
      call. = FALSE
    )
  }
  # Calls are self-contained: never serialize the caller's environment.
  func <- builder_project_portable_function(func)
  handle <- new.env(parent = emptyenv())
  handle$worker <- worker
  handle$func <- func
  handle$args <- args
  handle$state <- "queued"
  handle$result <- NULL
  handle$error <- NULL
  handle$is_alive <- function() {
    builder_project_worker_pump(worker)
    handle$state %in% c("queued", "running")
  }
  handle$get_result <- function() {
    builder_project_worker_pump(worker)
    if (identical(handle$state, "failed")) {
      stop(handle$error)
    }
    if (!identical(handle$state, "completed")) {
      stop("The Project worker call has not finished.", call. = FALSE)
    }
    handle$result
  }
  handle$kill <- function() {
    if (identical(handle$state, "queued")) {
      worker$queue <- Filter(
        function(candidate) !identical(candidate, handle),
        worker$queue
      )
    } else if (
      identical(handle$state, "running") &&
        identical(worker$current, handle)
    ) {
      .builder_project_worker_drop_session(worker)
    }
    if (handle$state %in% c("queued", "running")) {
      .builder_project_worker_finish(
        handle,
        error = simpleError("The Project worker call was cancelled.")
      )
    }
    builder_project_worker_pump(worker)
    invisible(TRUE)
  }
  handle$kill_tree <- handle$kill
  handle$wait <- function(timeout = -1) {
    deadline <- if (timeout < 0) Inf else Sys.time() + timeout / 1000
    while (isTRUE(handle$is_alive()) && Sys.time() < deadline) {
      session <- worker$session
      if (!is.null(session)) {
        try(session$poll_process(50L), silent = TRUE)
      } else {
        Sys.sleep(0.01)
      }
    }
    invisible(handle)
  }
  class(handle) <- "builder_project_worker_call"
  worker$queue[[length(worker$queue) + 1L]] <- handle
  builder_project_worker_pump(worker)
  handle
}

builder_project_worker_busy <- function(worker) {
  is.environment(worker) &&
    (!is.null(worker$current) || length(worker$queue) > 0L)
}

## Ends the worker once its queued transactions finish; `kill = TRUE` stops it
## immediately (only for work that is safe to interrupt).
builder_project_worker_close <- function(worker, kill = FALSE) {
  if (!is.environment(worker)) {
    return(invisible(FALSE))
  }
  if (isTRUE(kill)) {
    for (handle in c(worker$queue, list(worker$current))) {
      if (is.environment(handle)) {
        .builder_project_worker_finish(
          handle,
          error = simpleError("The Project worker was stopped.")
        )
      }
    }
    worker$queue <- list()
    worker$current <- NULL
    .builder_project_worker_drop_session(worker)
    return(invisible(TRUE))
  }
  worker$closing <- TRUE
  builder_project_worker_pump(worker)
  invisible(TRUE)
}

builder_project_write <- function(
  manifest,
  root,
  expected_revision = NULL,
  .move = file.rename,
  .write_json = jsonlite::write_json,
  .after_commit = function(path) invisible(path),
  verification_cache = new.env(parent = emptyenv())
) {
  root <- builder_project_normalize_root(root)
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    record$runtime_restore_status <- NULL
    record$runtime_restore_action <- NULL
    record
  })
  target <- builder_project_manifest_path(root)
  write_lock <- builder_project_acquire_manifest_lock(root)
  on.exit(builder_project_release_manifest_lock(write_lock), add = TRUE)
  disk <- if (file.exists(target)) builder_project_read(target) else NULL
  disk_revision <- if (is.null(disk)) 0L else disk$project$revision
  if (
    !is.null(expected_revision) &&
      !identical(as.integer(expected_revision), as.integer(disk_revision))
  ) {
    stop(
      "This project was updated by another Builder window. Reopen it before saving.",
      call. = FALSE
    )
  }
  if (!is.null(disk)) {
    tryCatch(
      builder_project_assert_manifest_integrity(
        disk,
        root,
        verification_cache = verification_cache
      ),
      error = function(error) {
        stop(
          "The current Builder project is incomplete and cannot be overwritten: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
  }
  manifest <- builder_project_upgrade_verified_source_fingerprints(
    manifest,
    disk,
    root,
    verification_cache
  )
  if (!is.list(manifest$project)) {
    stop("The Builder project header is missing.", call. = FALSE)
  }
  builder_project_assert_configuration_records(
    manifest,
    root,
    verification_cache = verification_cache
  )
  runtime_configuration <- vapply(
    manifest$datasets %||% list(),
    function(record) {
      configuration <- record$configuration %||% list()
      any(startsWith(names(configuration) %||% character(), "runtime_"))
    },
    logical(1)
  )
  if (any(runtime_configuration)) {
    stop(
      "Runtime-only Project configuration state cannot be persisted.",
      call. = FALSE
    )
  }
  manifest$schema_version <- .builder_project_schema_version
  manifest$configuration <- builder_project_configuration(
    review_options = manifest$configuration$review_options %||% NULL,
    build_mode = manifest$configuration$build_mode %||% FALSE,
    auth_enabled = manifest$configuration$auth_enabled %||% FALSE,
    initial_dataset = manifest$configuration$initial_dataset %||% NULL
  )
  manifest$project$revision <- as.integer(disk_revision) + 1L
  manifest$project$updated_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%OS3Z",
    tz = "UTC"
  )
  temporary <- tempfile("builder-project-", tmpdir = root, fileext = ".json")
  backup <- paste0(target, ".bak")
  backup_hold <- tempfile(
    ".builder-project-backup-",
    tmpdir = root,
    fileext = ".hold"
  )
  on.exit(unlink(c(temporary, backup_hold), force = TRUE), add = TRUE)
  move_failures <- list()
  move_path <- function(action, from, to) {
    attempt <- .nativeFileOperationAttempt(
      action = action,
      source = from,
      destination = to,
      operation = function() .move(from, to)
    )
    if (!isTRUE(attempt$ok)) {
      move_failures[[length(move_failures) + 1L]] <<- attempt
    }
    isTRUE(attempt$ok)
  }
  move_error <- function(summary) {
    if (!length(move_failures)) {
      return(summary)
    }
    paste(
      c(
        summary,
        vapply(
          move_failures,
          .nativeFileOperationMessage,
          character(1)
        )
      ),
      collapse = " "
    )
  }
  builder_project_assert_manifest_integrity(
    manifest,
    root,
    verification_cache = verification_cache
  )
  upgraded_manifest <- builder_project_upgrade_verified_source_fingerprints(
    manifest,
    disk,
    root,
    verification_cache
  )
  if (!identical(upgraded_manifest, manifest)) {
    manifest <- upgraded_manifest
    builder_project_assert_manifest_integrity(
      manifest,
      root,
      verification_cache = verification_cache
    )
  }
  write_result <- tryCatch(
    .write_json(
      manifest,
      temporary,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      pretty = TRUE,
      digits = NA
    ),
    error = identity
  )
  if (inherits(write_result, "condition") || !file.exists(temporary)) {
    stop("The project manifest could not be written.", call. = FALSE)
  }
  manifest_bytes <- suppressWarnings(as.double(file.info(temporary)$size[[1L]]))
  if (
    !is.finite(manifest_bytes) ||
      manifest_bytes > .builder_project_manifest_max_bytes
  ) {
    stop(
      "The Project manifest exceeded the schema v3 size limit.",
      call. = FALSE
    )
  }
  staged_manifest <- tryCatch(
    builder_project_read_manifest_payload(temporary),
    error = identity
  )
  if (inherits(staged_manifest, "condition")) {
    stop("The staged project manifest is not readable.", call. = FALSE)
  }
  builder_project_assert_manifest_integrity(
    staged_manifest,
    root,
    verification_cache = verification_cache
  )
  backup_existed <- file.exists(backup)
  restore_backup_hold <- function() {
    if (!backup_existed) {
      return(TRUE)
    }
    if (file.exists(backup)) {
      unlink(backup, force = TRUE)
    }
    move_path("restore previous Project backup", backup_hold, backup)
  }
  restore_previous <- function() {
    if (file.exists(target)) {
      unlink(target, force = TRUE)
    }
    restored_target <- !file.exists(backup) ||
      move_path(
        "restore previous Project manifest",
        backup,
        target
      )
    restored_backup <- restore_backup_hold()
    isTRUE(restored_target) &&
      isTRUE(restored_backup) &&
      (!is.null(disk) == file.exists(target))
  }
  if (file.exists(target)) {
    if (
      backup_existed &&
        !move_path(
          "protect previous Project backup",
          backup,
          backup_hold
        )
    ) {
      stop(
        move_error(
          "The previous project backup could not be protected."
        ),
        call. = FALSE
      )
    }
    if (!move_path("back up previous Project manifest", target, backup)) {
      if (!restore_backup_hold()) {
        stop(
          move_error(
            "The previous project manifest could not be backed up and its backup could not be restored."
          ),
          call. = FALSE
        )
      }
      stop(
        move_error(
          "The previous project manifest could not be backed up."
        ),
        call. = FALSE
      )
    }
    backup_manifest <- tryCatch(
      builder_project_read_manifest_payload(backup),
      error = identity
    )
    backup_valid <- !inherits(backup_manifest, "condition") &&
      isTRUE(tryCatch(
        {
          builder_project_assert_manifest_integrity(
            backup_manifest,
            root,
            verification_cache = verification_cache
          )
          TRUE
        },
        error = function(error) FALSE
      ))
    if (!backup_valid) {
      if (!restore_previous()) {
        stop(
          move_error(
            "The verified project backup could not be restored safely."
          ),
          call. = FALSE
        )
      }
      stop(
        "The previous project manifest is not a valid backup.",
        call. = FALSE
      )
    }
  }
  if (!move_path("commit Project manifest", temporary, target)) {
    if (!restore_previous()) {
      stop(
        move_error(paste0(
          "The project manifest could not be committed or restored. Recover it from: ",
          normalizePath(backup, winslash = "/", mustWork = FALSE)
        )),
        call. = FALSE
      )
    }
    stop(
      move_error(
        "The project manifest could not be committed."
      ),
      call. = FALSE
    )
  }
  post_commit <- tryCatch(
    {
      .after_commit(target)
      committed <- builder_project_read_manifest_payload(target)
      builder_project_assert_manifest_integrity(
        committed,
        root,
        verification_cache = verification_cache
      )
      TRUE
    },
    error = identity
  )
  if (inherits(post_commit, "condition")) {
    if (!restore_previous()) {
      stop(
        move_error(paste0(
          "The project manifest failed post-commit validation and the previous manifest could not be restored. Recover it from: ",
          normalizePath(backup, winslash = "/", mustWork = FALSE)
        )),
        call. = FALSE
      )
    }
    stop(
      "The project manifest failed post-commit validation: ",
      conditionMessage(post_commit),
      call. = FALSE
    )
  }
  unlink(backup_hold, force = TRUE)
  list(manifest = manifest, path = target)
}

builder_project_artifact_entry <- function(
  entry,
  artifact,
  root,
  observed = NULL
) {
  entry$load_state <- "artifact_ready"
  entry$snapshot <- NULL
  if (.builder_project_text(artifact$plan_payload %||% "")) {
    artifact$plan_item <- jsonlite::unserializeJSON(artifact$plan_payload)
    artifact$plan_payload <- NULL
  }
  artifact$resolved_path <- observed[[artifact$path]]$path %||%
    builder_project_resolve_path(artifact$path, root, "managed")
  if (length(artifact$members %||% list())) {
    artifact$members <- lapply(artifact$members, function(member) {
      member$resolved_path <- observed[[member$path]]$path %||%
        builder_project_resolve_path(member$path, root, "managed")
      member
    })
  }
  entry$project_artifact <- artifact
  entry
}

## The opened manifest with the user's dataset choices as the defaults, so
## the choices dialog can be shown again without losing them.
builder_project_manifest_with_restore_actions <- function(manifest, actions) {
  if (!is.list(actions) || !length(actions)) {
    return(manifest)
  }
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    status <- record$runtime_restore_status %||% NULL
    action <- actions[[as.character(record$id %||% "")]] %||% NULL
    if (
      is.list(status) &&
        is.character(action) &&
        length(action) == 1L &&
        action %in% c(status$actions %||% character(), "skip")
    ) {
      status$default_action <- action
      record$runtime_restore_status <- status
    }
    record
  })
  manifest
}

builder_project_prepare_open_selection <- function(manifest, root, actions) {
  records <- manifest$datasets %||% list()
  names(records) <- vapply(
    records,
    function(record) {
      as.character(record$id)
    },
    character(1)
  )
  reusable_entries <- list()
  pending_entries <- list()
  artifacts <- list()
  marks <- character()
  skipped_ids <- character()
  for (record in records) {
    action <- as.character(actions[[record$id]] %||% "skip")
    status <- record$runtime_restore_status %||% NULL
    if (!is.list(status) || !action %in% status$actions) {
      stop(
        "The selected action is no longer valid for dataset ",
        record$id,
        ". Reopen the Project and try again.",
        call. = FALSE
      )
    }
    if (identical(action, "skip")) {
      skipped_ids <- c(skipped_ids, record$id)
      next
    }
    if (identical(action, "reuse")) {
      if (
        !builder_project_status_snapshot_fresh(
          status,
          record,
          root,
          needs = c("configuration", "artifact", "spatial", "table")
        )
      ) {
        stop(
          "The dataset cache inputs changed after the Project was opened.",
          call. = FALSE
        )
      }
      entry <- builder_project_restore_entry(
        record,
        root,
        status$configuration_entry %||% NULL,
        status$stat_signature
      )
      entry <- builder_project_artifact_entry(
        entry,
        record$artifact,
        root,
        status$stat_signature
      )
      reusable_entries[[length(reusable_entries) + 1L]] <- entry
      artifacts[[record$id]] <- record$artifact
      mark <- builder_project_restored_check_identity(record, entry)
      if (!is.null(mark)) {
        marks[[record$id]] <- mark
      }
      next
    }
    record$runtime_restore_action <- action
    pending_entries[[record$id]] <- record
  }
  list(
    reusable_entries = reusable_entries,
    pending_entries = pending_entries,
    artifacts = artifacts,
    marks = marks,
    skipped_ids = skipped_ids
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

builder_project_restored_check_identity <- function(record, entry) {
  restorable <- identical(
    entry$load_state %||% "loaded",
    "artifact_ready"
  ) ||
    (record$runtime_restore_action %||% "") %in% c("resume", "edit")
  if (
    !is.list(record) ||
      !is.list(entry) ||
      !isTRUE(restorable) ||
      !builder_project_record_configuration_confirmed(record) ||
      (!identical(entry$load_state %||% "loaded", "artifact_ready") &&
        !builder_project_loaded_settings_compatible(entry))
  ) {
    return(NULL)
  }
  builder_project_check_identity(entry)
}
