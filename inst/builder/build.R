##----------------------------------------------------------------------------##
## Frozen BuildPlan execution and staged-artifact verification.
##
## The coordinator creates the stage. This module never chooses a publication
## target and never moves an artifact into the final release directory.
##----------------------------------------------------------------------------##

.builder_build_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

builder_build_queue_note <- function(plan) {
  items <- plan$items %||% list()
  reused <- sum(vapply(
    items,
    function(item) is.list(item$reused_artifact %||% NULL),
    logical(1)
  ))
  rebuilding <- length(items) - reused
  parts <- character()
  if (reused > 0L) {
    parts <- c(parts, paste("Reusing", reused, "CRBs"))
  }
  if (rebuilding > 0L) {
    parts <- c(
      parts,
      paste(
        "Building",
        rebuilding,
        if (rebuilding == 1L) "dataset" else "datasets"
      )
    )
  }
  paste0(paste(parts, collapse = " · "), "…")
}

## Session reuse records ----------------------------------------------------
## After a published build, the coordinator's release identity already holds
## the MD5 of every CRB and sidecar, so recording them for the next build
## needs no hashing on the Shiny process.

builder_build_retention_root <- function(root = tempdir()) {
  path <- file.path(root, "builder-reuse")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, mode = "0700", showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    return(NULL)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.builder_build_published_reuse_record <- function(item, value, prefix) {
  root <- value$release$target %||% ""
  entries <- value$release$identity$entries %||% list()
  verification <- value$verifications[[item$id]] %||% NULL
  built <- value$built[[item$id]] %||% ""
  if (
    !.builder_build_text(root) ||
      !length(entries) ||
      !is.list(verification) ||
      !.builder_build_text(built) ||
      !identical(basename(built), item$filename)
  ) {
    return(NULL)
  }
  files <- entries[vapply(
    entries,
    function(entry) identical(entry$type, "file"),
    logical(1)
  )]
  file_paths <- vapply(files, function(entry) entry$path %||% "", "")
  primary_index <- match(paste0(prefix, item$filename), file_paths)
  primary <- if (is.na(primary_index)) NULL else files[[primary_index]]
  if (!is.list(primary) || !.builder_build_text(primary$md5 %||% "")) {
    return(NULL)
  }
  companions <- c(
    item$sidecars %||% character(),
    verification$spatial_molecule_backend$location %||% character()
  )
  relative <- substring(file_paths, nchar(prefix) + 1L)
  selected <- startsWith(file_paths, prefix) &
    vapply(
      relative,
      function(path) {
        any(path == companions | startsWith(path, paste0(companions, "/")))
      },
      logical(1)
    )
  if (length(companions) && !any(selected)) {
    return(NULL)
  }
  fingerprint <- function(entry) {
    list(md5 = tolower(entry$md5), size = as.double(entry$size))
  }
  list(
    path = file.path(root, primary$path),
    fingerprint = fingerprint(primary),
    members = lapply(unname(which(selected)), function(index) {
      list(
        resolved_path = file.path(root, files[[index]]$path),
        target = relative[[index]],
        fingerprint = fingerprint(files[[index]])
      )
    }),
    verification = verification
  )
}

builder_build_reuse_records <- function(plan, value) {
  if (!inherits(plan, "builder_build_plan") || !is.list(value)) {
    return(list())
  }
  items <- plan$items %||% list()
  names(items) <- vapply(items, `[[`, character(1), "id")
  records <- if (identical(value$state, "needs_decision")) {
    value$retained_artifacts %||% list()
  } else if (
    identical(value$state, "success") &&
      isTRUE(value$published)
  ) {
    prefix <- if (isTRUE(plan$make_app)) "cerebro_app/private-data/" else ""
    Filter(
      Negate(is.null),
      lapply(items, .builder_build_published_reuse_record, value, prefix)
    )
  } else {
    list()
  }
  records <- records[intersect(names(records), names(items))]
  lapply(names(records), function(id) {
    record <- records[[id]]
    verification <- record$verification
    record$verification <- NULL
    list(
      id = id,
      key = builder_plan_reuse_key(items[[id]]),
      record = record,
      md5 = record$fingerprint$md5 %||% NULL,
      verification = verification
    )
  })
}

## The deep read-back opens and hydrates the whole CRB. A reused CRB whose
## copy matched an MD5 already read back against the same expectation in this
## session takes that verification, rebased onto the staged copy, after a
## cheap check that its sidecars are where the record says.
.builder_build_reused_verification <- function(verification, target, item) {
  if (
    !is.list(verification) ||
      !isTRUE(verification$valid) ||
      !.builder_build_text(verification$path %||% "")
  ) {
    return(NULL)
  }
  tryCatch(
    {
      if (identical(item$expression_backend, "h5")) {
        .builder_build_sidecar_path(target, item)
      } else if (length(item$sidecars)) {
        stop("The staged CRB sidecar does not match BuildPlan.", call. = FALSE)
      }
      directory <- dirname(target)
      rebase <- function(path) {
        if (.builder_build_text(path %||% "")) {
          file.path(directory, basename(path))
        } else {
          path
        }
      }
      spatial <- verification$spatial_molecule_backend %||% NULL
      if (!is.null(spatial)) {
        spatial_path <- file.path(directory, spatial$location)
        if (
          !dir.exists(spatial_path) ||
            nzchar(Sys.readlink(spatial_path)) ||
            !.builder_build_path_within(spatial_path, directory)
        ) {
          stop("The reused CRB spatial sidecar is missing.", call. = FALSE)
        }
        verification$spatial_molecule_path <- normalizePath(
          spatial_path,
          winslash = "/",
          mustWork = TRUE
        )
      }
      verification$path <- target
      verification$file_fingerprint <- .builder_build_runtime_function(
        ".bundlePreflightFingerprint"
      )(target)
      preflight <- verification$bundle_preflight
      if (is.list(preflight)) {
        if (is.list(preflight$spatial_backend)) {
          preflight$spatial_backend$root <- rebase(
            preflight$spatial_backend$root
          )
        }
        if (is.list(preflight$dataset_info)) {
          preflight$dataset_info$path <- target
        }
        verification$bundle_preflight <- preflight
      }
      verification
    },
    error = function(error) NULL
  )
}

.builder_build_progress_phases <- c(
  "datasets",
  "viewer",
  "verify",
  "publish"
)

.builder_build_progress_phase <- function(phase) {
  if (
    !is.character(phase) ||
      length(phase) != 1L ||
      !phase %in% .builder_build_progress_phases
  ) {
    stop("The worker reported an unsupported build phase.", call. = FALSE)
  }
  phase
}

builder_build_progress_note <- function(plan, phase) {
  phase <- .builder_build_progress_phase(phase)
  switch(
    phase,
    datasets = builder_build_queue_note(plan),
    viewer = "Packaging Viewer…",
    verify = "Verifying staged output…",
    publish = "Publishing release atomically…"
  )
}

## Progress records are either a bare phase (settlement) or a detail record
## naming the dataset and step, so the overlay can say what is running.
.builder_build_progress_detail_fields <- c(
  "dataset_index",
  "dataset_count",
  "dataset_name",
  "step_label",
  "step_index",
  "step_count",
  "started_at"
)

.builder_build_progress_record <- function(phase, detail = NULL) {
  if (!is.list(detail) || !length(detail)) {
    return(phase)
  }
  text <- function(value) {
    if (.builder_build_text(value)) substr(value, 1L, 200L) else NULL
  }
  count <- function(value) {
    value <- suppressWarnings(as.integer(value))
    if (length(value) == 1L && !is.na(value) && value >= 0L) value else NULL
  }
  started <- suppressWarnings(as.numeric(detail$started_at))
  record <- list(
    schema_version = 1L,
    phase = phase,
    dataset_index = count(detail$dataset_index),
    dataset_count = count(detail$dataset_count),
    dataset_name = text(detail$dataset_name),
    step_label = text(detail$step_label),
    step_index = count(detail$step_index),
    step_count = count(detail$step_count),
    started_at = if (length(started) == 1L && is.finite(started)) {
      started
    } else {
      NULL
    }
  )
  Filter(Negate(is.null), record)
}

builder_build_progress_write <- function(path, phase, detail = NULL) {
  phase <- .builder_build_progress_phase(phase)
  if (!.builder_build_text(path) || !dir.exists(dirname(path))) {
    return(FALSE)
  }
  temporary <- tempfile(".progress-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saved <- try(
    saveRDS(
      .builder_build_progress_record(phase, detail),
      temporary,
      version = 2
    ),
    silent = TRUE
  )
  if (inherits(saved, "try-error")) {
    return(FALSE)
  }
  try(Sys.chmod(temporary, mode = "0600"), silent = TRUE)
  moved <- isTRUE(file.rename(temporary, path))
  if (!moved && file.exists(path)) {
    unlink(path, force = TRUE)
    moved <- isTRUE(file.rename(temporary, path))
  }
  if (moved) {
    try(Sys.chmod(path, mode = "0600"), silent = TRUE)
  }
  moved
}

builder_build_progress_detail <- function(path) {
  if (!.builder_build_text(path) || !file.exists(path)) {
    return(NULL)
  }
  record <- try(readRDS(path), silent = TRUE)
  detailed <- is.list(record) && identical(record$schema_version, 1L)
  phase <- try(
    .builder_build_progress_phase(if (detailed) record$phase else record),
    silent = TRUE
  )
  if (inherits(phase, "try-error")) {
    return(NULL)
  }
  if (!detailed) {
    return(list(phase = phase))
  }
  value <- .builder_build_progress_record(
    phase,
    record[intersect(names(record), .builder_build_progress_detail_fields)]
  )
  if (is.list(value)) value else list(phase = phase)
}

builder_build_progress_read <- function(path) {
  builder_build_progress_detail(path)$phase
}

builder_build_progress_remove <- function(path) {
  if (!.builder_build_text(path)) {
    return(FALSE)
  }
  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  unlink(paste0(path, ".cancel"), force = TRUE)
  !file.exists(path)
}

builder_build_cancel_requested <- function(path) {
  .builder_build_text(path) && file.exists(paste0(path, ".cancel"))
}

builder_build_request_cancel <- function(path) {
  if (!.builder_build_text(path) || !dir.exists(dirname(path))) {
    return(FALSE)
  }
  marker <- paste0(path, ".cancel")
  created <- isTRUE(file.create(marker))
  if (created) {
    Sys.chmod(marker, mode = "0600")
  }
  created
}

builder_build_check_cancel <- function(path) {
  if (builder_build_cancel_requested(path)) {
    stop(structure(
      list(
        message = "Build stopped by the user. The staged output was not published.",
        call = NULL
      ),
      class = c("builder_build_cancelled", "error", "condition")
    ))
  }
  invisible(NULL)
}

builder_build_progress_finish <- function(plan, path, current_note = NULL) {
  phase <- builder_build_progress_read(path)
  builder_build_progress_remove(path)
  note <- if (is.null(phase)) {
    NULL
  } else {
    builder_build_progress_note(plan, phase)
  }
  list(
    phase = phase,
    note = note,
    defer_settlement = identical(phase, "viewer") &&
      !identical(current_note, note)
  )
}

.builder_build_cancel_callback <- function(path) {
  if (is.null(path)) {
    return(NULL)
  }
  function() builder_build_check_cancel(path)
}

# Hooks wrap their own errors as dataset failures; a Stop must still unwind.
.builder_build_raise_cancel <- function(value) {
  if (inherits(value, "builder_build_cancelled")) {
    stop(value)
  }
  value
}

.builder_build_hook_accepts <- function(hook, argument) {
  arguments <- names(formals(hook))
  argument %in% arguments || "..." %in% arguments
}

.builder_build_progress_callback <- function(path) {
  started_at <- as.numeric(Sys.time())
  function(phase, detail = NULL) {
    if (is.null(path)) {
      return(invisible(FALSE))
    }
    builder_build_check_cancel(path)
    if (is.list(detail)) {
      detail$started_at <- started_at
    }
    invisible(builder_build_progress_write(path, phase, detail))
  }
}

.builder_build_copy_file <- function(
  source,
  target,
  .sysname = unname(Sys.info()[["sysname"]]),
  .command = system2,
  .fallback = file.copy
) {
  if (
    !.builder_build_text(source) ||
      !.builder_build_text(target) ||
      !file.exists(source) ||
      dir.exists(source) ||
      file.exists(target) ||
      dir.exists(target)
  ) {
    return(FALSE)
  }
  cloned <- FALSE
  command_args <- if (identical(.sysname, "Darwin")) {
    c("-c", "--", shQuote(source), shQuote(target))
  } else if (identical(.sysname, "Linux")) {
    c("--reflink=auto", "--", shQuote(source), shQuote(target))
  } else {
    NULL
  }
  copy_command <- unname(Sys.which("cp"))
  if (length(command_args) && nzchar(copy_command)) {
    status <- suppressWarnings(tryCatch(
      .command(
        copy_command,
        command_args,
        stdout = FALSE,
        stderr = FALSE
      ),
      error = function(error) 1L
    ))
    cloned <- isTRUE(status == 0L) && file.exists(target)
    if (!cloned) {
      unlink(target, force = TRUE)
    }
  }
  cloned ||
    .fallback(
      source,
      target,
      overwrite = FALSE,
      copy.mode = TRUE
    )
}

.builder_build_number_equal <- function(left, right, tolerance = 1e-12) {
  is.numeric(left) &&
    is.numeric(right) &&
    !is.object(left) &&
    !is.object(right) &&
    length(left) == 1L &&
    length(right) == 1L &&
    is.finite(left) &&
    is.finite(right) &&
    abs(as.numeric(left) - as.numeric(right)) <= tolerance
}

.builder_build_value_equal <- function(left, right, tolerance = 1e-12) {
  isTRUE(all.equal(
    left,
    right,
    tolerance = tolerance,
    check.attributes = TRUE
  ))
}

.builder_build_path_within <- function(path, stage, must_exist = TRUE) {
  if (!.builder_build_text(path)) {
    return(FALSE)
  }
  canonical <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = must_exist),
    error = function(error) NULL
  )
  if (is.null(canonical)) {
    return(FALSE)
  }
  identical(canonical, stage) || startsWith(canonical, paste0(stage, "/"))
}

.builder_build_safe_relative <- function(path) {
  if (!.builder_build_text(path) || grepl("\\", path, fixed = TRUE)) {
    return(FALSE)
  }
  if (
    startsWith(path, "/") ||
      startsWith(path, "//") ||
      grepl("^[A-Za-z]:", path)
  ) {
    return(FALSE)
  }
  components <- strsplit(path, "/", fixed = TRUE)[[1L]]
  length(components) > 0L &&
    all(nzchar(components)) &&
    !any(components %in% c(".", ".."))
}

.builder_build_copy_verified <- function(
  source,
  target,
  fingerprint,
  .copy = .builder_build_copy_file,
  .digest_file = tools::md5sum
) {
  copied <- is.function(.copy) && isTRUE(.copy(source, target))
  valid <- copied &&
    .builder_build_text(target) &&
    file.exists(target) &&
    !dir.exists(target) &&
    is.list(fingerprint) &&
    .builder_build_text(fingerprint$md5 %||% NULL) &&
    isTRUE(tryCatch(
      identical(
        unname(as.character(.digest_file(target))),
        as.character(fingerprint$md5)
      ),
      error = function(error) FALSE
    ))
  if (!valid && file.exists(target)) {
    unlink(target, force = TRUE)
  }
  valid
}

.builder_build_failure <- function(message, failures = character()) {
  list(
    state = "failure",
    publishable = FALSE,
    error = message,
    failures = failures,
    built = character(),
    labels = character(),
    verifications = list(),
    analysis_log = character(),
    failed_analyses = character(),
    retry_closure = character(),
    spatial_images = list(),
    spatial_image_settings = list(),
    app_dir = NULL,
    app_verification = NULL,
    app_tree_identity = NULL
  )
}

.builder_build_condition_diagnostics <- function(
  condition,
  calls = sys.calls()
) {
  render_call <- function(call) {
    if (is.null(call)) {
      return("")
    }
    paste(deparse(call, width.cutoff = 500L), collapse = " ")
  }
  rendered_calls <- vapply(
    tail(as.list(calls), 64L),
    render_call,
    character(1),
    USE.NAMES = FALSE
  )
  list(
    class = class(condition),
    message = conditionMessage(condition),
    call = if (is.null(conditionCall(condition))) {
      NULL
    } else {
      render_call(conditionCall(condition))
    },
    calls = rendered_calls,
    calls_truncated = length(calls) > length(rendered_calls),
    captured_at = Sys.time()
  )
}

.builder_build_cleanup_spatial_assets <- function(stage) {
  exists <- function(value) {
    file.exists(value) || dir.exists(value) || .builder_app_is_link(value)
  }
  for (name in c(".spatial", ".builder-spatial-assets")) {
    path <- file.path(stage, name)
    if (!exists(path)) {
      next
    }
    if (
      .builder_app_is_link(path) ||
        !.builder_build_path_within(path, stage, must_exist = TRUE)
    ) {
      stop("Builder spatial asset staging is unsafe to clean.", call. = FALSE)
    }
    unlink(path, recursive = TRUE, force = TRUE)
    if (exists(path)) {
      stop("Builder spatial asset staging could not be cleaned.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.builder_build_field <- function(object, name) {
  if (is.environment(object)) {
    return(get0(name, envir = object, inherits = FALSE))
  }
  if (is.list(object)) {
    return(object[[name]])
  }
  NULL
}

.builder_build_runtime_function <- function(name) {
  value <- get0(name, mode = "function", inherits = TRUE)
  if (is.null(value)) {
    value <- get(name, envir = asNamespace("CerebroNexus"), inherits = FALSE)
  }
  value
}

.builder_build_identity <- function(object, axis) {
  expression <- .builder_build_field(object, "expression")
  ids <- if (identical(axis, "cells")) {
    colnames(expression)
  } else {
    rownames(expression)
  }
  if (length(ids)) {
    return(as.character(ids))
  }
  fallback <- if (identical(axis, "cells")) {
    metadata <- .builder_build_field(object, "meta_data")
    if (
      is.data.frame(metadata) &&
        "cell_barcode" %in% colnames(metadata)
    ) {
      metadata[["cell_barcode"]]
    } else {
      rownames(metadata)
    }
  } else {
    rownames(.builder_build_field(object, "gene_data"))
  }
  as.character(fallback %||% character())
}

.builder_build_sidecar_path <- function(path, item) {
  if (
    length(item$sidecars) != 1L ||
      !.builder_build_safe_relative(item$sidecars[[1L]])
  ) {
    stop("The staged CRB sidecar does not match BuildPlan.", call. = FALSE)
  }
  root <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  sidecar <- file.path(dirname(path), item$sidecars[[1L]])
  info <- tryCatch(
    fs::file_info(sidecar, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    nzchar(Sys.readlink(sidecar)) ||
      is.null(info) ||
      !identical(as.character(info$type), "file") ||
      !.builder_build_path_within(sidecar, root, must_exist = TRUE)
  ) {
    stop("The staged CRB sidecar does not match BuildPlan.", call. = FALSE)
  }
  normalizePath(sidecar, winslash = "/", mustWork = TRUE)
}

.builder_build_h5_identities <- function(path, item) {
  if (!identical(item$expression_backend, "h5")) {
    return(NULL)
  }
  if (!requireNamespace("HDF5Array", quietly = TRUE)) {
    stop(
      "HDF5Array is required to verify the staged H5 sidecar.",
      call. = FALSE
    )
  }
  sidecar <- .builder_build_sidecar_path(path, item)
  matrix <- tryCatch(
    DelayedArray::t(HDF5Array::TENxMatrix(sidecar, group = "expression")),
    error = function(error) NULL
  )
  if (is.null(matrix)) {
    stop("The staged H5 sidecar cannot be reopened.", call. = FALSE)
  }
  list(
    cells = as.character(colnames(matrix) %||% character()),
    features = as.character(rownames(matrix) %||% character())
  )
}

.builder_crb_visible_pages <- function(object) {
  present <- function(field) {
    value <- .builder_build_field(object, field)
    !is.null(value) && length(value) > 0L
  }
  extra <- .builder_build_field(object, "extra_material")
  immune <- .builder_build_field(object, "immune_repertoire")
  tcr_chains <- tryCatch(
    hla_detect_chains(immune),
    error = function(error) character()
  )
  pages <- c(
    if (present("marker_genes")) "marker_genes",
    if (present("most_expressed_genes")) "most_expressed_genes",
    if (present("enriched_pathways")) "enriched_pathways",
    if (!is.null(extra) && length(extra$tables) > 0L) "extra_material",
    if (present("immune_repertoire")) "immune_repertoire",
    if (present("trajectories")) "trajectory",
    if (present("spatial")) "spatial",
    if (present("trekker")) "trekker",
    if (any(tcr_chains %in% c("TRA", "TRB"))) {
      "hla_tcr_motifs"
    }
  )
  as.character(pages)
}

#' Reopen and compare one staged CRB with its frozen expectation.
.builder_build_normalize_spatial_descriptor <- function(
  path,
  serialized
) {
  stored <- .builder_build_field(serialized, "spatial_molecule_backend")
  if (!is.list(stored) || !"root" %in% names(stored)) {
    return(serialized)
  }
  portable <- stored[c("type", "location")]
  if (is.environment(serialized)) {
    assign("spatial_molecule_backend", portable, envir = serialized)
  } else {
    serialized[["spatial_molecule_backend"]] <- portable
  }
  temporary <- tempfile(".crb-portable-", tmpdir = dirname(path))
  backup <- tempfile(".crb-legacy-", tmpdir = dirname(path))
  on.exit(unlink(c(temporary, backup), force = TRUE), add = TRUE)
  codec <- .builder_build_runtime_function(".cerebroPayloadCodec")(path)
  written <- try(
    .builder_build_runtime_function(".writeCerebroPayload")(
      serialized,
      temporary,
      codec
    ),
    silent = TRUE
  )
  if (inherits(written, "try-error") || !file.exists(temporary)) {
    stop(
      "The staged CRB legacy descriptor could not be normalized.",
      call. = FALSE
    )
  }
  if (!file.rename(path, backup)) {
    stop(
      "The staged CRB could not be protected before normalization.",
      call. = FALSE
    )
  }
  if (!file.rename(temporary, path)) {
    restored <- file.rename(backup, path)
    if (!isTRUE(restored)) {
      stop(
        "The staged CRB normalization failed and its staged backup could not be restored.",
        call. = FALSE
      )
    }
    stop(
      "The staged CRB legacy descriptor could not be replaced.",
      call. = FALSE
    )
  }
  unlink(backup, force = TRUE)
  serialized
}

builder_verify_crb <- function(
  path,
  item,
  .read_payload = .builder_build_runtime_function(".readCerebroPayload"),
  .backend_descriptor = .builder_build_runtime_function(".cerebroBackend"),
  .read_crb = NULL,
  .hydrate_crb = .builder_build_runtime_function(".hydrateCerebroPayload")
) {
  if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path))) {
    stop("The staged CRB is missing or is not a regular file.", call. = FALSE)
  }
  if (identical(item$expression_backend, "h5")) {
    .builder_build_sidecar_path(path, item)
  } else if (length(item$sidecars)) {
    stop("The staged CRB sidecar does not match BuildPlan.", call. = FALSE)
  }
  serialized <- tryCatch(.read_payload(path), error = function(error) error)
  backend_descriptor <- if (inherits(serialized, "condition")) {
    serialized
  } else {
    tryCatch(
      .backend_descriptor(serialized),
      error = function(error) error
    )
  }
  if (inherits(backend_descriptor, "condition")) {
    stop(
      "The staged CRB cannot be reopened: ",
      conditionMessage(backend_descriptor),
      call. = FALSE
    )
  }
  if (!identical(backend_descriptor$type, item$expression_backend)) {
    stop(
      "The staged CRB expression backend differs from BuildPlan.",
      call. = FALSE
    )
  }
  expected_location <- if (length(item$sidecars) == 1L) {
    item$sidecars[[1L]]
  } else {
    NULL
  }
  if (!identical(backend_descriptor$location, expected_location)) {
    stop(
      "The staged CRB sidecar location differs from BuildPlan.",
      call. = FALSE
    )
  }
  spatial_backend_descriptor <- tryCatch(
    .builder_build_runtime_function(".spatialMoleculeBackend")(
      serialized,
      path
    ),
    error = function(error) error
  )
  if (inherits(spatial_backend_descriptor, "condition")) {
    stop(
      "The staged CRB spatial molecule backend is invalid: ",
      conditionMessage(spatial_backend_descriptor),
      call. = FALSE
    )
  }
  if (!is.null(spatial_backend_descriptor)) {
    serialized <- .builder_build_normalize_spatial_descriptor(
      path,
      serialized
    )
    spatial_backend_descriptor <- .builder_build_runtime_function(
      ".spatialMoleculeBackend"
    )(serialized, path)
  }
  fingerprint_file <- .builder_build_runtime_function(
    ".bundlePreflightFingerprint"
  )
  file_fingerprint <- fingerprint_file(path)
  spatial_backend_path <- NULL
  if (!is.null(spatial_backend_descriptor)) {
    expected_spatial_location <- paste0(
      tools::file_path_sans_ext(basename(path)),
      ".spatial"
    )
    spatial_backend_path <- spatial_backend_descriptor$root
    spatial_info <- tryCatch(
      fs::file_info(spatial_backend_path, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      !identical(spatial_backend_descriptor$type, "directory") ||
        !identical(
          spatial_backend_descriptor$location,
          expected_spatial_location
        ) ||
        is.null(spatial_info) ||
        !identical(as.character(spatial_info$type), "directory") ||
        nzchar(Sys.readlink(spatial_backend_path)) ||
        !.builder_build_path_within(
          spatial_backend_path,
          dirname(path),
          must_exist = TRUE
        )
    ) {
      stop(
        "The staged CRB spatial molecule sidecar does not match its descriptor.",
        call. = FALSE
      )
    }
    spatial_backend_path <- normalizePath(
      spatial_backend_path,
      winslash = "/",
      mustWork = TRUE
    )
  }
  object <- tryCatch(
    if (is.function(.read_crb)) {
      .read_crb(path)
    } else {
      .hydrate_crb(serialized, path)
    },
    error = function(error) error
  )
  if (inherits(object, "condition")) {
    stop(
      "The staged CRB cannot be reopened: ",
      conditionMessage(object),
      call. = FALSE
    )
  }
  expectation <- item$artifact_identity
  expected_axis <- function(value) {
    compact <- builder_axis_identity_normalize(value)
    if (!is.null(compact)) {
      return(compact)
    }
    if (is.character(value) && !anyNA(value)) {
      return(builder_axis_identity(value))
    }
    NULL
  }
  expected_cells <- expected_axis(expectation$cells)
  expected_features <- expected_axis(expectation$features)
  if (is.null(expected_cells) || is.null(expected_features)) {
    stop("BuildPlan contains an invalid dataset identity.", call. = FALSE)
  }
  cells <- .builder_build_identity(object, "cells")
  features <- .builder_build_identity(object, "features")
  h5_identity <- .builder_build_h5_identities(path, item)
  if (!is.null(h5_identity)) {
    if (
      !identical(
        builder_axis_identity(h5_identity$cells),
        expected_cells
      )
    ) {
      stop(
        "The staged H5 sidecar cell identity differs from BuildPlan.",
        call. = FALSE
      )
    }
    if (
      !identical(
        builder_axis_identity(h5_identity$features),
        expected_features
      )
    ) {
      stop(
        "The staged H5 sidecar feature identity differs from BuildPlan.",
        call. = FALSE
      )
    }
  }
  if (!length(cells)) {
    cells <- h5_identity$cells %||% character()
  }
  if (!length(features)) {
    features <- h5_identity$features %||% character()
  }
  cell_identity <- builder_axis_identity(cells)
  feature_identity <- builder_axis_identity(features)
  if (!identical(cell_identity, expected_cells)) {
    stop("The staged CRB cell identity differs from BuildPlan.", call. = FALSE)
  }
  if (!identical(feature_identity, expected_features)) {
    stop(
      "The staged CRB feature identity differs from BuildPlan.",
      call. = FALSE
    )
  }
  groups_value <- .builder_build_field(object, "groups")
  groups <- names(groups_value)
  expected_groups <- names(expectation$group_levels)
  if (!identical(groups, expected_groups)) {
    stop(
      "The staged CRB grouping variables differ from BuildPlan.",
      call. = FALSE
    )
  }
  for (group in expected_groups) {
    if (
      !identical(
        as.character(groups_value[[group]]),
        expectation$group_levels[[group]]
      )
    ) {
      stop(
        "The staged CRB group levels differ from BuildPlan: ",
        group,
        call. = FALSE
      )
    }
  }
  projections <- names(.builder_build_field(object, "projections"))
  if (!identical(projections, expectation$projections)) {
    stop("The staged CRB projections differ from BuildPlan.", call. = FALSE)
  }
  if (!is.null(expectation$trajectories)) {
    trajectories <- .builder_build_field(object, "trajectories")
    trajectory_identity <- lapply(trajectories %||% list(), names)
    if (!identical(trajectory_identity, expectation$trajectories)) {
      stop(
        "The staged CRB trajectories differ from BuildPlan.",
        call. = FALSE
      )
    }
  }
  metadata <- colnames(.builder_build_field(object, "meta_data"))
  if (!identical(metadata, expectation$metadata)) {
    stop("The staged CRB metadata differs from BuildPlan.", call. = FALSE)
  }
  spatial_sections <- names(.builder_build_field(object, "spatial"))
  spatial_sections <- spatial_sections %||% character()
  if (!identical(spatial_sections, expectation$spatial_sections)) {
    stop(
      "The staged CRB spatial sections differ from BuildPlan.",
      call. = FALSE
    )
  }
  spatial <- .builder_build_field(object, "spatial")
  embedded_sections <- names(spatial)[vapply(
    spatial,
    function(section) {
      is.list(section) &&
        (length(section$histology_images %||% list()) > 0L ||
          !is.null(section[["histology_image", exact = TRUE]]) ||
          !is.null(section[["histology_image_bounds", exact = TRUE]]))
    },
    logical(1)
  )]
  if (length(embedded_sections)) {
    stop(
      "The staged CRB contains embedded Spatial images; Builder supports ",
      "external Spatial images only: ",
      paste(embedded_sections, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  expected_coordinate_transforms <- item$spatial_coordinate_transforms %||%
    list()
  legacy_artifact_matches <- NULL
  for (section in names(expected_coordinate_transforms)) {
    observed_spatial <- spatial[[section]]
    coordinates <- if (is.list(observed_spatial)) {
      observed_spatial$coordinates
    } else {
      NULL
    }
    valid_coordinates <- is.data.frame(coordinates) &&
      all(c("x", "y") %in% names(coordinates)) &&
      is.numeric(coordinates$x) &&
      is.numeric(coordinates$y) &&
      !is.object(coordinates$x) &&
      !is.object(coordinates$y) &&
      all(is.finite(coordinates$x)) &&
      all(is.finite(coordinates$y))
    if (!isTRUE(valid_coordinates)) {
      stop(
        "The staged CRB spatial coordinates are invalid for transformed FOV: ",
        section,
        call. = FALSE
      )
    }
    observed_transform <- observed_spatial$coordinate_transform
    expected_transform <- expected_coordinate_transforms[[section]]
    valid_pivot <- is.numeric(observed_transform$pivot) &&
      !is.object(observed_transform$pivot) &&
      identical(names(observed_transform$pivot), c("x", "y")) &&
      length(observed_transform$pivot) == 2L &&
      all(is.finite(observed_transform$pivot))
    observed_fingerprint <- tryCatch(
      .spx_coordinate_transform_fingerprint(coordinates),
      error = function(error) NULL
    )
    fingerprint_version <- observed_transform$coordinate_fingerprint_version
    fingerprint_matches <-
      !is.null(observed_fingerprint) &&
      (is.null(fingerprint_version) || identical(fingerprint_version, 2L)) &&
      identical(
        observed_transform$transformed_coordinate_fingerprint,
        observed_fingerprint
      )
    if (
      !fingerprint_matches &&
        !is.null(observed_fingerprint) &&
        is.null(fingerprint_version)
    ) {
      legacy_fingerprint <- tryCatch(
        .spx_coordinate_transform_fingerprint(coordinates, .legacy = TRUE),
        error = function(error) NULL
      )
      fingerprint_matches <- !is.null(legacy_fingerprint) &&
        identical(
          observed_transform$transformed_coordinate_fingerprint,
          legacy_fingerprint
        )
      # Legacy coordinate hashes included the exporting R version. Across R
      # versions, accept only the exact immutable CRB frozen in the reuse plan,
      # just as legacy Project configuration identities require a content hash.
      # A newly built, untrusted, or modified CRB cannot take this path.
      if (!fingerprint_matches) {
        if (is.null(legacy_artifact_matches)) {
          expected_md5 <- item$reused_artifact$fingerprint$md5 %||% NULL
          legacy_artifact_matches <- .builder_build_text(expected_md5) &&
            identical(unname(as.character(tools::md5sum(path))), expected_md5)
        }
        old_hash <- observed_transform$transformed_coordinate_fingerprint
        fingerprint_matches <- isTRUE(legacy_artifact_matches) &&
          .builder_build_text(old_hash) &&
          grepl("^[0-9a-f]{32}$", old_hash)
      }
    }
    # A floating-point rotation is not losslessly invertible, so hashing the
    # inverse can reject an otherwise exact staged export. The source hash is
    # provenance; staged integrity is bound to the actual transformed values
    # below, while the frozen plan still binds the rotation and scale.
    valid_source_fingerprint <-
      is.character(observed_transform$source_coordinate_fingerprint) &&
      !is.object(observed_transform$source_coordinate_fingerprint) &&
      length(observed_transform$source_coordinate_fingerprint) == 1L &&
      !is.na(observed_transform$source_coordinate_fingerprint) &&
      grepl(
        "^[0-9a-f]{32}$",
        observed_transform$source_coordinate_fingerprint
      )
    if (
      !is.list(observed_transform) ||
        !identical(observed_transform$schema_version, 1L) ||
        !.builder_build_number_equal(
          observed_transform$rotation_degrees %||% NA_real_,
          expected_transform$rotation_degrees
        ) ||
        !.builder_build_number_equal(
          observed_transform$scale %||% NA_real_,
          expected_transform$scale
        ) ||
        !isTRUE(valid_pivot) ||
        !identical(observed_transform$pivot_method, "bounds_center") ||
        !identical(
          observed_transform$convention,
          "counterclockwise_degrees"
        ) ||
        !isTRUE(fingerprint_matches) ||
        !isTRUE(valid_source_fingerprint)
    ) {
      stop(
        "The staged CRB spatial coordinate transform differs from BuildPlan: ",
        section,
        call. = FALSE
      )
    }
  }
  expected_trekker_alignment <- item$trekker_alignment %||% NULL
  if (!is.null(expected_trekker_alignment)) {
    observed_trekker <- .builder_build_field(object, "trekker")
    if (
      !is.list(observed_trekker) ||
        !is.null(observed_trekker$histology_image) ||
        !is.null(observed_trekker$histology_image_bounds) ||
        !.builder_build_value_equal(
          observed_trekker$histology_alignment,
          builder_alignment_payload(expected_trekker_alignment)
        )
    ) {
      stop(
        "The staged CRB Trekker alignment differs from BuildPlan.",
        call. = FALSE
      )
    }
  }
  backend <- .builder_build_field(object, "expression_backend")
  backend_type <- if (is.null(backend)) "embedded" else backend$type
  if (!identical(backend_type, item$expression_backend)) {
    stop(
      "The staged CRB expression backend differs from BuildPlan.",
      call. = FALSE
    )
  }
  if (length(item$sidecars)) {
    expected_location <- item$sidecars[[1L]]
    if (!identical(backend$location, expected_location)) {
      stop(
        "The staged CRB sidecar location differs from BuildPlan.",
        call. = FALSE
      )
    }
  }
  visible <- .builder_crb_visible_pages(object)
  expected_visible <- item$viewer_page_expectations$visible_conditional %||%
    character()
  if (!setequal(visible, expected_visible)) {
    stop(
      "The staged CRB Viewer page contract differs from BuildPlan.",
      call. = FALSE
    )
  }
  bundle_preflight <- if (is.null(file_fingerprint)) {
    NULL
  } else {
    tryCatch(
      list(
        backend = .builder_build_runtime_function(".readBundleBackend")(
          path,
          object
        ),
        spatial_catalog = .builder_build_runtime_function(
          ".readBundleSpatialCatalog"
        )(
          object,
          item$name %||% basename(path)
        ),
        spatial_backend = spatial_backend_descriptor,
        dataset_info = .builder_build_runtime_function(
          ".readBundleDatasetInfo"
        )(
          object,
          item$name %||% basename(path),
          path
        )
      ),
      error = function(error) NULL
    )
  }
  if (
    !is.null(file_fingerprint) &&
      !identical(file_fingerprint, fingerprint_file(path))
  ) {
    stop("The staged CRB changed while it was verified.", call. = FALSE)
  }
  list(
    valid = TRUE,
    path = path,
    cell_count = as.integer(cell_identity$count),
    feature_count = as.integer(feature_identity$count),
    cell_identity = cell_identity,
    feature_identity = feature_identity,
    groups = groups,
    projections = projections,
    metadata = metadata,
    spatial_sections = spatial_sections,
    image_sections = character(),
    backend = backend,
    spatial_molecule_backend = if (is.null(spatial_backend_descriptor)) {
      NULL
    } else {
      spatial_backend_descriptor[c("type", "location")]
    },
    spatial_molecule_path = spatial_backend_path,
    file_fingerprint = file_fingerprint,
    bundle_preflight = bundle_preflight,
    page_contract = list(visible_conditional = visible)
  )
}

.builder_build_prepare_immune <- function(object, item) {
  if (!methods::is(object, "Seurat")) {
    return(object)
  }
  immune_record <- item$manifest[["immune_repertoire"]]
  motif_record <- item$manifest[["hla_tcr_motifs"]]
  if (is.null(immune_record) && is.null(motif_record)) {
    return(object)
  }
  clear_sources <- function(value) {
    value@misc$immune_repertoire <- NULL
    value@misc$bcr_data <- NULL
    value@misc$tcr_data <- NULL
    value
  }
  included_selection <- function(record) {
    if (
      !is.list(record) ||
        !(record$disposition %||% "") %in%
          c("preserved", "converted", "attached")
    ) {
      return(character())
    }
    record$evidence$selected_sources %||% character()
  }
  hidden <- function(record) {
    is.list(record) &&
      (record$disposition %||% "") %in% c("filtered", "stored_only")
  }
  selected_candidates <- function(record, selected) {
    candidates <- record$evidence$selected_candidates %||% list()
    if (!is.list(candidates)) {
      return(list())
    }
    candidates[intersect(selected, names(candidates))]
  }
  candidate_flag <- function(record, selected, flag) {
    candidates <- selected_candidates(record, selected)
    if (!length(selected) || length(candidates) != length(selected)) {
      return(NA)
    }
    flags <- vapply(
      candidates,
      function(candidate) isTRUE(candidate[[flag]]),
      logical(1)
    )
    any(flags)
  }
  full_selected <- included_selection(immune_record)
  motif_selected <- included_selection(motif_record)
  if (
    length(full_selected) &&
      length(motif_selected) &&
      !all(motif_selected %in% full_selected)
  ) {
    stop(
      "The frozen immune sources cannot be realized by one frozen immune payload.",
      call. = FALSE
    )
  }
  motif_exportable <- candidate_flag(
    motif_record,
    motif_selected,
    "full_ir_ready"
  )
  if (length(motif_selected) && !isTRUE(motif_exportable)) {
    stop(
      "The frozen motif source cannot be exported as one immune payload.",
      call. = FALSE
    )
  }
  if (length(motif_selected) && !length(full_selected)) {
    stop(
      "The frozen immune payload cannot hide only one Viewer page.",
      call. = FALSE
    )
  }
  if (
    length(motif_selected) &&
      hidden(immune_record)
  ) {
    stop(
      "The frozen immune payload cannot hide only one Viewer page.",
      call. = FALSE
    )
  }
  if (length(full_selected) && hidden(motif_record)) {
    full_has_motif <- candidate_flag(
      immune_record,
      full_selected,
      "hla_tcr_ready"
    )
    if (is.na(full_has_motif) || isTRUE(full_has_motif)) {
      stop(
        "The frozen immune payload cannot hide only one Viewer page.",
        call. = FALSE
      )
    }
  }
  selected <- if (length(full_selected)) full_selected else motif_selected
  selected_record <- if (length(full_selected)) immune_record else motif_record
  filtered <- vapply(
    Filter(Negate(is.null), list(immune_record, motif_record)),
    function(record) {
      (record$disposition %||% "") %in% c("filtered", "stored_only")
    },
    logical(1)
  )
  if (!length(selected) && length(filtered) && any(filtered)) {
    return(clear_sources(object))
  }
  if (!length(selected)) {
    return(object)
  }
  if (identical(selected, "unified_misc")) {
    object@misc$bcr_data <- NULL
    object@misc$tcr_data <- NULL
    return(object)
  }
  if (identical(selected, "metadata")) {
    candidate <- selected_record$evidence$selected_candidates[["metadata"]]
    sample_column <- candidate$normalized$sample_column %||% NULL
    object <- clear_sources(object)
    return(addImmuneRepertoire(
      object,
      sample_col = sample_column,
      groups = item$included_groups,
      from_metadata = TRUE,
      verbose = FALSE
    ))
  }
  supported_legacy <- c("legacy_bcr", "legacy_tcr")
  if (length(setdiff(selected, supported_legacy))) {
    stop(
      "The frozen immune source is not supported at build time.",
      call. = FALSE
    )
  }
  bcr <- if ("legacy_bcr" %in% selected) object@misc$bcr_data else NULL
  tcr <- if ("legacy_tcr" %in% selected) object@misc$tcr_data else NULL
  object <- clear_sources(object)
  addImmuneRepertoire(
    object,
    tcr = tcr,
    bcr = bcr,
    from_metadata = FALSE,
    verbose = FALSE
  )
}

.builder_build_select_trajectories <- function(
  trajectories,
  included,
  default = NULL
) {
  # A missing field identifies a legacy BuildPlan. Preserve its historical
  # payload; an explicit empty list means the user chose no trajectories.
  if (is.null(included)) {
    return(trajectories)
  }
  if (!is.list(trajectories) || !is.list(included)) {
    stop("The frozen trajectory selection is invalid.", call. = FALSE)
  }
  missing_methods <- setdiff(names(included), names(trajectories))
  missing_names <- unlist(
    lapply(names(included), function(method) {
      setdiff(included[[method]], names(trajectories[[method]]))
    }),
    use.names = FALSE
  )
  if (length(missing_methods) || length(missing_names)) {
    stop(
      "A frozen included trajectory is missing from the built object.",
      call. = FALSE
    )
  }
  if (
    is.list(default) &&
      .builder_build_text(default$method) &&
      .builder_build_text(default$name) &&
      default$method %in% names(included) &&
      default$name %in% included[[default$method]]
  ) {
    method <- default$method
    included[[method]] <- c(
      default$name,
      included[[method]][included[[method]] != default$name]
    )
    included <- c(included[method], included[names(included) != method])
  }
  selected <- lapply(names(included), function(method) {
    trajectories[[method]][included[[method]]]
  })
  names(selected) <- names(included)
  selected
}

.builder_build_prepare_qc <- function(object, item) {
  fields <- unique(c(item$nUMI, item$nGene))
  fields <- fields[vapply(fields, .builder_build_text, logical(1))]
  missing <- setdiff(fields, colnames(object@meta.data))
  if (length(missing)) {
    stop(
      "The source object is missing selected QC metadata: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (field in fields) {
    values <- builder_qc_numeric_values(object@meta.data[[field]])
    if (is.null(values)) {
      stop(
        "Selected QC metadata `",
        field,
        "` must contain only numeric values.",
        call. = FALSE
      )
    }
    object@meta.data[[field]] <- values
  }
  object
}

.builder_build_prepare_trekker <- function(object, item) {
  if (!methods::is(object, "Seurat")) {
    return(object)
  }
  trekker <- object@misc$trekker
  if (is.null(trekker) || !length(trekker)) {
    return(object)
  }
  if (!is.list(trekker)) {
    stop("Trekker data must be a list.", call. = FALSE)
  }

  group <- item$default_group %||% NULL
  trekker$builder_group <- group
  trekker$builder_colors <- if (is.null(group)) {
    NULL
  } else {
    (item$colors %||% list())[[group]] %||% NULL
  }
  barcodes <- as.character(trekker$barcodes %||% character())
  if (
    !is.null(group) &&
      length(barcodes) &&
      group %in% colnames(object@meta.data) &&
      all(barcodes %in% rownames(object@meta.data))
  ) {
    trekker$builder_group_values <- as.character(
      object@meta.data[barcodes, group, drop = TRUE]
    )
  }

  alignment <- builder_alignment_normalize(
    item$trekker_alignment %||% NULL,
    section_id = "trekker",
    section_kind = "trekker"
  )
  trekker$histology_image <- NULL
  trekker$histology_image_bounds <- NULL
  if (!is.null(alignment)) {
    trekker$histology_alignment <- builder_alignment_payload(alignment)
  }
  object@misc$trekker <- trekker
  object
}

.builder_build_prepare <- function(object, item) {
  if (methods::is(object, "Seurat")) {
    object@reductions <- object@reductions[item$included_projections]
    object@misc$trajectories <- .builder_build_select_trajectories(
      object@misc$trajectories %||% list(),
      item$included_trajectories,
      item$default_trajectory
    )
    object <- builder_prepare_export_layer(object, item$assay, item$layer)
    object <- .builder_build_prepare_qc(object, item)
    object <- .builder_build_prepare_immune(object, item)
    for (group in names(item$artifact_identity$group_levels)) {
      values <- object@meta.data[[group]]
      object@meta.data[[group]] <- factor(
        as.character(values),
        levels = item$artifact_identity$group_levels[[group]]
      )
    }
    object <- .builder_build_prepare_trekker(object, item)
    if (length(item$tables %||% list())) {
      object <- builder_attach_tables(
        object,
        builder_materialize_tables(item$tables)
      )
    }
    if (length(item$marker_imports %||% list())) {
      object <- builder_attach_marker_imports(object, item$marker_imports)
    }
  }
  object
}

.builder_build_apply_metadata_policy <- function(object, item) {
  if (!methods::is(object, "Seurat")) {
    return(object)
  }
  retained <- unique(c(
    item$included_groups,
    item$cell_cycle %||% character(),
    item$nUMI,
    item$nGene,
    item$metadata_policy$retained %||% item$metadata_policy$included,
    item$artifact_identity$source_metadata
  ))
  retained <- setdiff(retained, "cell_barcode")
  missing <- setdiff(retained, colnames(object@meta.data))
  if (length(missing)) {
    stop(
      "The source object is missing retained metadata: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  object@meta.data <- object@meta.data[, retained, drop = FALSE]
  object
}

.builder_build_export <- function(object, item, path) {
  object <- .builder_build_apply_metadata_policy(object, item)
  coordinate_transforms <- item$spatial_coordinate_transforms %||% NULL
  if (
    is.list(coordinate_transforms) &&
      !is.object(coordinate_transforms) &&
      !length(coordinate_transforms)
  ) {
    coordinate_transforms <- NULL
  }
  exportFromSeurat(
    object = object,
    assay = item$assay,
    slot = item$layer,
    file = path,
    experiment_name = item$name,
    organism = item$organism,
    groups = item$included_groups,
    main_group = item$default_group,
    cell_cycle = item$cell_cycle %||% NULL,
    nUMI = item$nUMI,
    nGene = item$nGene,
    add_all_meta_data = TRUE,
    projections = item$included_projections,
    expression_matrix_mode = item$expression_backend,
    codec = "qs2",
    spatial_coordinate_transforms = coordinate_transforms,
    verbose = FALSE
  )
  path
}

.builder_build_attach_extras <- function(path, object, item) {
  trekker <- if (methods::is(object, "Seurat")) {
    tryCatch(object@misc$trekker, error = function(error) NULL)
  } else {
    NULL
  }
  if (!is.null(trekker) && length(trekker) && !is.list(trekker)) {
    stop("Trekker data must be a list.", call. = FALSE)
  }
  trekker_applied <- is.list(trekker) && length(trekker) > 0L
  result <- builder_attach_crb_extras(
    crb_path = path,
    images = list(),
    external_images = item$images %||% list()
  )
  if (!is.null(result$error)) {
    stop(result$error, call. = FALSE)
  }
  result$trekker <- trekker_applied
  external <- .builder_build_materialize_spatial_images(item, dirname(path))
  result$external_images <- external$images
  result$external_settings <- external$settings
  result
}

.builder_build_materialize_spatial_images <- function(item, stage) {
  opaque_component <- function(value, prefix) {
    original <- enc2utf8(as.character(value)[[1L]])
    digest <- substr(
      as.character(digest::digest(
        charToRaw(original),
        algo = "md5",
        serialize = FALSE
      )),
      1L,
      12L
    )
    paste0(prefix, "-", digest)
  }
  images <- list()
  settings <- list()
  materialized_paths <- character()
  collection_input <- item$images %||% list()
  if (!is.null(item$trekker_alignment)) {
    collection_input[["trekker"]] <- item$trekker_alignment
  }
  collection <- builder_image_collection_normalize(collection_input)
  for (section_id in names(collection)) {
    section_dir <- file.path(
      stage,
      ".spatial",
      opaque_component(item$id, "d"),
      opaque_component(section_id, "s")
    )
    dir.create(
      section_dir,
      recursive = TRUE,
      mode = "0700",
      showWarnings = FALSE
    )
    for (label in names(collection[[section_id]])) {
      record <- collection[[section_id]][[label]]
      source_path <- tryCatch(
        normalizePath(record$source_path, winslash = "/", mustWork = TRUE),
        error = function(error) NULL
      )
      if (is.null(source_path) || !isTRUE(file_test("-f", source_path))) {
        stop("A Builder Spatial image asset is missing.", call. = FALSE)
      }
      source <- record[["source", exact = TRUE]]
      inspected <- builder_read_image(
        source_path,
        filename = if (is.list(source)) {
          source$name %||% source_path
        } else {
          source_path
        }
      )
      if (!is.null(inspected$error)) {
        stop(inspected$error, call. = FALSE)
      }
      if (
        !is.null(record$source_content_md5) &&
          !identical(record$source_content_md5, inspected$source_content_md5)
      ) {
        stop(
          "A Builder Spatial image failed its integrity check.",
          call. = FALSE
        )
      }
      extension <- switch(
        inspected$mime,
        `image/png` = "png",
        `image/jpeg` = "jpg",
        stop("Builder image has an unsupported MIME type.", call. = FALSE)
      )
      filename <- paste0(
        opaque_component(
          paste(
            label,
            if (is.list(source)) source$name %||% "" else "",
            inspected$source_content_md5,
            sep = "::"
          ),
          "i"
        ),
        ".",
        extension
      )
      materialized <- file.path(section_dir, filename)
      if (materialized %in% materialized_paths || file.exists(materialized)) {
        stop("Builder Spatial image asset paths must be unique.", call. = FALSE)
      }
      if (
        !file.copy(
          source_path,
          materialized,
          overwrite = FALSE,
          copy.mode = TRUE
        )
      ) {
        stop("A Builder Spatial image could not be copied.", call. = FALSE)
      }
      if (
        !identical(unname(file.size(materialized)), inspected$bytes) ||
          !identical(
            unname(as.character(tools::md5sum(materialized))),
            inspected$source_content_md5
          )
      ) {
        unlink(materialized, force = TRUE)
        stop(
          "A Builder Spatial image copy failed its integrity check.",
          call. = FALSE
        )
      }
      materialized_paths <- c(materialized_paths, materialized)
      materialized <- normalizePath(
        materialized,
        winslash = "/",
        mustWork = TRUE
      )
      descriptor <- list(
        path = materialized,
        bounds = unlist(record$base_bounds[c("xmin", "xmax", "ymin", "ymax")])
      )
      if (.builder_alignment_valid_bounds(record$viewport_bounds)) {
        descriptor$viewport_bounds <- unlist(record$viewport_bounds[c(
          "xmin",
          "xmax",
          "ymin",
          "ymax"
        )])
      }
      descriptor$label <- record$image_label %||% label
      scope <- intersect(c("roi_field", "roi_value"), names(record))
      descriptor[scope] <- record[scope]
      images[[item$id]][[section_id]][[label]] <- descriptor
      settings[[item$id]][[section_id]][[label]] <-
        builder_alignment_viewer_setting(record)
    }
  }
  list(images = images, settings = settings)
}

## The worker's App tree identity is a diagnostic the coordinator compares with
## its own full read of the same tree, and the coordinator separately checks
## every private-data copy against the inputs it hashes itself. Seeding the
## copied CRB and sidecar entries from the request's input MD5s therefore
## skips a whole re-read of the data here without weakening either check: a
## bad copy still fails publication.
.builder_build_app_tree_seed <- function(app_dir, request) {
  entries <- list()
  for (content in request$content_identities %||% list()) {
    expected_files <- Filter(
      function(entry) is.list(entry) && identical(entry$type, "file"),
      c(
        list(content$crb),
        unname(content$backend$entries %||% list()),
        unname(content$spatial_molecules$entries %||% list())
      )
    )
    for (expected in expected_files) {
      fingerprint <- tryCatch(
        .builder_app_file_fingerprint(fs::file_info(
          file.path(app_dir, expected$path),
          fail = TRUE,
          follow = FALSE
        )),
        error = function(error) NULL
      )
      if (
        !is.list(fingerprint) ||
          !identical(fingerprint$type, "file") ||
          !identical(fingerprint$size, as.double(expected$size))
      ) {
        next
      }
      entries[[expected$path]] <- c(
        list(path = expected$path),
        fingerprint,
        list(md5 = tolower(expected$md5))
      )
    }
  }
  list(entries = entries)
}

builder_build_hooks <- function() {
  list(
    open_snapshot = builder_open_snapshot,
    prepare = .builder_build_prepare,
    run_analyses = function(object, item, on_progress = function(...) NULL) {
      settings <- item
      settings$groups <- item$included_groups
      builder_run_analyses(
        object,
        item$analyses,
        settings,
        on_progress = on_progress
      )
    },
    export = .builder_build_export,
    attach_extras = .builder_build_attach_extras,
    verify = builder_verify_crb,
    build_app = builder_build_app
  )
}

#' Execute one frozen BuildPlan without publishing its outputs.
builder_execute_plan <- function(
  plan,
  stage,
  snapshots,
  hooks = builder_build_hooks(),
  auth_material = NULL,
  objects = list(),
  on_progress = NULL,
  check_cancel = NULL
) {
  on.exit(auth_material <- NULL, add = TRUE)
  if (!inherits(plan, "builder_build_plan") || !is.list(plan$items)) {
    stop("Build execution requires a frozen BuildPlan.", call. = FALSE)
  }
  if (!.builder_build_text(stage) || !dir.exists(stage)) {
    stop("Build execution requires an existing assigned stage.", call. = FALSE)
  }
  if (nzchar(Sys.readlink(stage))) {
    stop("The assigned stage cannot be a symbolic link.", call. = FALSE)
  }
  stage <- normalizePath(stage, winslash = "/", mustWork = TRUE)
  auth_enabled <- isTRUE(plan$app_auth$enabled)
  cleanup_complete <- !auth_enabled
  on.exit(
    {
      if (!cleanup_complete && auth_enabled) {
        cleaned <- try(
          .builder_auth_remove_partial_material(stage),
          silent = TRUE
        )
        if (inherits(cleaned, "try-error") || !isTRUE(cleaned)) {
          stop(
            "The authentication files could not be cleaned up.",
            call. = FALSE
          )
        }
      }
    },
    add = TRUE
  )
  if (auth_enabled) {
    auth_material <- tryCatch(
      builder_auth_validate_material(auth_material, stage),
      error = function(error) error
    )
    if (inherits(auth_material, "condition")) {
      return(.builder_build_failure(conditionMessage(auth_material)))
    }
  } else if (!is.null(auth_material)) {
    return(.builder_build_failure(
      "A public build cannot use authentication material."
    ))
  }
  if (isTRUE(plan$make_app) && !identical(plan$app_contract_version, 1L)) {
    return(.builder_build_failure(
      "Generated-app execution requires frozen contract version 1."
    ))
  }
  if (!is.list(snapshots)) {
    stop("Build execution requires a snapshot registry.", call. = FALSE)
  }
  required_hooks <- c(
    "open_snapshot",
    "prepare",
    "run_analyses",
    "export",
    "attach_extras",
    "verify"
  )
  if (!all(vapply(hooks[required_hooks], is.function, logical(1)))) {
    stop("Build execution hooks are incomplete.", call. = FALSE)
  }
  report_progress <- function(phase, detail = NULL) {
    if (is.function(on_progress)) {
      progress_result <- try(
        if (is.null(detail)) {
          on_progress(phase)
        } else {
          on_progress(phase, detail)
        },
        silent = TRUE
      )
      if (
        inherits(attr(progress_result, "condition"), "builder_build_cancelled")
      ) {
        stop(attr(progress_result, "condition"))
      }
    }
    invisible(NULL)
  }
  # Dataset/step detail for the overlay; only for callbacks that take it, so
  # a phase-only callback still sees each phase once.
  detail_progress <- is.function(on_progress) &&
    .builder_build_hook_accepts(on_progress, "detail")
  item_count <- length(plan$items)
  item_index <- 0L
  item_name <- NULL
  report_step <- function(label) {
    if (isTRUE(detail_progress)) {
      report_progress(
        "datasets",
        list(
          dataset_index = item_index,
          dataset_count = item_count,
          dataset_name = item_name,
          step_label = label
        )
      )
    }
    invisible(NULL)
  }
  # Checked between datasets and between hooks so Stop does not wait for
  # every remaining dataset to finish loading, analysing and exporting.
  cancel_point <- function() {
    if (is.function(check_cancel)) {
      check_cancel()
    }
    invisible(NULL)
  }
  report_progress("datasets")

  result <- list(
    state = "success",
    publishable = FALSE,
    error = NULL,
    failures = character(),
    built = character(),
    labels = character(),
    verifications = list(),
    analysis_log = character(),
    failed_analyses = character(),
    retry_closure = character(),
    app_dir = NULL,
    app_verification = NULL,
    app_tree_identity = NULL,
    stage = stage
  )
  copied_file_identities <- stats::setNames(
    lapply(plan$items, function(item) list()),
    vapply(plan$items, `[[`, character(1L), "id")
  )
  for (item in plan$items) {
    cancel_point()
    item_index <- item_index + 1L
    item_name <- item$name %||% item$id
    reused <- item$reused_artifact %||% NULL
    if (is.list(reused)) {
      report_step("Reusing CRB")
      if (
        !.builder_build_safe_relative(item$filename) ||
          !is.character(reused$path) ||
          length(reused$path) != 1L ||
          is.na(reused$path) ||
          !file.exists(reused$path) ||
          dir.exists(reused$path)
      ) {
        return(.builder_build_failure(paste0(
          item$name,
          ": the reusable CRB is unavailable or has changed."
        )))
      }
      target <- file.path(stage, item$filename)
      target_within_stage <- .builder_build_path_within(
        target,
        stage,
        must_exist = FALSE
      )
      copied_identity <- NULL
      copied <- target_within_stage &&
        .builder_build_copy_verified(
          reused$path,
          target,
          reused$fingerprint %||% list(),
          .digest_file = if (isTRUE(plan$make_app)) {
            function(path) {
              copied_identity <<- .builder_app_capture_file_identity(
                path,
                item$name
              )
              copied_identity$md5
            }
          } else {
            tools::md5sum
          }
        )
      if (!copied) {
        return(.builder_build_failure(paste0(
          item$name,
          ": the reusable CRB could not be staged."
        )))
      }
      if (!is.null(copied_identity)) {
        copied_file_identities[[item$id]][[item$filename]] <- copied_identity
      }
      members <- reused$members %||% list()
      for (member in members) {
        member_source <- member$resolved_path %||% NULL
        member_target <- member$target %||% NULL
        if (
          !is.character(member_source) ||
            length(member_source) != 1L ||
            is.na(member_source) ||
            !file.exists(member_source) ||
            !.builder_build_safe_relative(member_target %||% "")
        ) {
          return(.builder_build_failure(paste0(
            item$name,
            ": a reusable CRB companion file is unavailable."
          )))
        }
        destination <- file.path(stage, member_target)
        dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
        member_identity <- NULL
        destination_within_stage <- .builder_build_path_within(
          destination,
          stage,
          must_exist = FALSE
        )
        member_copied <- destination_within_stage &&
          .builder_build_copy_verified(
            member_source,
            destination,
            member$fingerprint %||% list(),
            .digest_file = if (isTRUE(plan$make_app)) {
              function(path) {
                member_identity <<- .builder_app_capture_file_identity(
                  path,
                  member_target
                )
                member_identity$md5
              }
            } else {
              tools::md5sum
            }
          )
        if (!member_copied) {
          return(.builder_build_failure(paste0(
            item$name,
            ": a reusable CRB companion file could not be staged."
          )))
        }
        if (!is.null(member_identity)) {
          copied_file_identities[[item$id]][[member_target]] <-
            member_identity
        }
      }
      verified <- .builder_build_reused_verification(
        reused$verification %||% NULL,
        target,
        item
      )
      if (is.null(verified)) {
        verified <- .builder_build_raise_cancel(tryCatch(
          hooks$verify(target, item),
          error = function(error) error
        ))
      }
      if (inherits(verified, "condition") || !isTRUE(verified$valid)) {
        message <- if (inherits(verified, "condition")) {
          conditionMessage(verified)
        } else {
          "Artifact verification did not return a valid result."
        }
        return(.builder_build_failure(paste0(item$name, ": ", message)))
      }
      external <- tryCatch(
        .builder_build_materialize_spatial_images(item, stage),
        error = function(error) error
      )
      if (inherits(external, "condition")) {
        return(.builder_build_failure(paste0(
          item$name,
          ": ",
          conditionMessage(external)
        )))
      }
      if (length(external$images[[item$id]] %||% list())) {
        result$spatial_images[[item$id]] <- external$images[[item$id]]
        result$spatial_image_settings[[item$id]] <-
          external$settings[[item$id]]
      }
      result$built <- c(result$built, stats::setNames(target, item$id))
      result$labels <- c(result$labels, item$name)
      result$verifications[[item$id]] <- verified
      next
    }
    snapshot <- snapshots[[item$id]]
    object <- objects[[item$id]] %||% NULL
    if (is.null(snapshot) && is.null(object)) {
      return(.builder_build_failure(paste0(
        "The frozen snapshot is missing for ",
        item$name,
        "."
      )))
    }
    if (is.null(object)) {
      report_step("Opening dataset")
    }
    object <- if (!is.null(object)) {
      object
    } else {
      .builder_build_raise_cancel(tryCatch(
        hooks$open_snapshot(snapshot),
        error = function(error) error
      ))
    }
    cancel_point()
    if (inherits(object, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(object)
      )))
    }
    report_step("Preparing dataset")
    object <- .builder_build_raise_cancel(tryCatch(
      hooks$prepare(object, item),
      error = function(error) error
    ))
    cancel_point()
    if (inherits(object, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(object)
      )))
    }
    analyses <- .builder_build_raise_cancel(tryCatch(
      if (.builder_build_hook_accepts(hooks$run_analyses, "on_progress")) {
        hooks$run_analyses(
          object,
          item,
          on_progress = function(label = NULL, ...) {
            cancel_point()
            if (.builder_build_text(label)) {
              report_step(label)
            }
          }
        )
      } else {
        hooks$run_analyses(object, item)
      },
      error = function(error) error
    ))
    cancel_point()
    if (inherits(analyses, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(analyses)
      )))
    }
    if (length(analyses$log)) {
      result$analysis_log <- c(
        result$analysis_log,
        paste0(item$name, ": ", analyses$log)
      )
    }
    if (length(analyses$failed)) {
      failed <- analyses$failed[[1L]]
      graph <- item$analysis_dependency_graph
      if (is.null(graph) || !failed %in% names(graph)) {
        graph <- builder_analysis_graph(item$analyses)
      }
      result$state <- "needs_decision"
      result$failed_analyses <- failed
      result$failed_dataset_id <- item$id
      result$retry_closure <- builder_retry_closure(graph, failed)
      result$failures <- paste0(item$name, ": analysis `", failed, "` failed.")
      return(result)
    }
    object <- analyses$object
    if (!.builder_build_safe_relative(item$filename)) {
      return(.builder_build_failure(
        "A build artifact target is outside the assigned stage."
      ))
    }
    target <- file.path(stage, item$filename)
    if (!.builder_build_path_within(target, stage, must_exist = FALSE)) {
      return(.builder_build_failure(
        "A build artifact target is outside the assigned stage."
      ))
    }
    report_step("Writing CRB")
    export_diagnostics <- NULL
    exported <- tryCatch(
      withCallingHandlers(
        hooks$export(object, item, target),
        error = function(error) {
          captured <- try(
            .builder_build_condition_diagnostics(error, sys.calls()),
            silent = TRUE
          )
          if (!inherits(captured, "try-error")) {
            export_diagnostics <<- captured
          }
          invisible(NULL)
        }
      ),
      error = function(error) error
    )
    .builder_build_raise_cancel(exported)
    cancel_point()
    if (inherits(exported, "condition")) {
      failure <- .builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(exported)
      ))
      if (is.null(export_diagnostics)) {
        export_diagnostics <- tryCatch(
          .builder_build_condition_diagnostics(exported, list()),
          error = function(error) NULL
        )
      }
      failure$diagnostics <- list(export = export_diagnostics)
      return(failure)
    }
    if (!.builder_build_path_within(exported, stage, must_exist = TRUE)) {
      return(.builder_build_failure(
        "A build artifact was written outside the assigned stage."
      ))
    }
    extras <- .builder_build_raise_cancel(tryCatch(
      hooks$attach_extras(exported, object, item),
      error = function(error) error
    ))
    if (inherits(extras, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(extras)
      )))
    }
    if (length(extras$external_images %||% list())) {
      result$spatial_images[[item$id]] <- extras$external_images[[item$id]]
      result$spatial_image_settings[[item$id]] <-
        extras$external_settings[[item$id]]
    }
    objects[[item$id]] <- NULL
    object <- NULL
    analyses <- NULL
    extras <- NULL
    invisible(gc(FALSE))
    report_step("Verifying CRB")
    verified <- .builder_build_raise_cancel(tryCatch(
      hooks$verify(exported, item),
      error = function(error) error
    ))
    if (inherits(verified, "condition") || !isTRUE(verified$valid)) {
      message <- if (inherits(verified, "condition")) {
        conditionMessage(verified)
      } else {
        "Artifact verification did not return a valid result."
      }
      return(.builder_build_failure(paste0(item$name, ": ", message)))
    }
    result$built <- c(result$built, stats::setNames(exported, item$id))
    result$labels <- c(result$labels, item$name)
    result$verifications[[item$id]] <- verified
  }
  if (isTRUE(plan$make_app)) {
    report_progress("viewer")
    if (!is.function(hooks$build_app)) {
      return(.builder_build_failure(
        "Generated-App build hooks are incomplete."
      ))
    }
    app_plan <- plan
    for (index in seq_along(app_plan$items)) {
      dataset <- app_plan$items[[index]]$id
      app_plan$items[[index]]$external_images <-
        result$spatial_images[[dataset]] %||% list()
      app_plan$items[[index]]$external_image_settings <-
        result$spatial_image_settings[[dataset]] %||% list()
    }
    verified_preflight <- lapply(app_plan$items, function(item) {
      result$verifications[[item$id]] %||% list()
    })
    verified_spatial_backends <- if (
      all(vapply(
        verified_preflight,
        function(verification) is.list(verification$bundle_preflight),
        logical(1)
      ))
    ) {
      stats::setNames(
        lapply(
          verified_preflight,
          function(verification) {
            verification$bundle_preflight$spatial_backend %||% NULL
          }
        ),
        plan$dataset_order
      )
    } else {
      NULL
    }
    request <- tryCatch(
      builder_app_bundle_request(
        app_plan,
        result$built,
        result$labels,
        verified_spatial_backends = verified_spatial_backends,
        previous_file_identities = copied_file_identities
      ),
      error = function(error) error
    )
    if (inherits(request, "condition")) {
      return(.builder_build_failure(conditionMessage(request)))
    }
    cacheable <- all(vapply(
      verified_preflight,
      function(verification) {
        is.list(verification$bundle_preflight) &&
          is.list(verification$file_fingerprint)
      },
      logical(1)
    ))
    clear_preflight <- .builder_build_runtime_function(
      ".clearBundlePreflightCache"
    )
    cache_preflight <- .builder_build_runtime_function(
      ".cacheBundlePreflightData"
    )
    clear_preflight()
    on.exit(clear_preflight(), add = TRUE)
    if (cacheable) {
      preflight_data <- list(
        backends = stats::setNames(
          lapply(verified_preflight, function(x) x$bundle_preflight$backend),
          plan$dataset_order
        ),
        spatial_backends = stats::setNames(
          lapply(
            verified_preflight,
            function(x) x$bundle_preflight$spatial_backend
          ),
          plan$dataset_order
        ),
        spatial_catalogs = stats::setNames(
          lapply(
            verified_preflight,
            function(x) x$bundle_preflight$spatial_catalog
          ),
          plan$dataset_order
        ),
        dataset_catalog = stats::setNames(
          lapply(verified_preflight, function(x) {
            x$bundle_preflight$dataset_info
          }),
          plan$dataset_order
        )
      )
      fingerprints <- stats::setNames(
        lapply(verified_preflight, `[[`, "file_fingerprint"),
        plan$dataset_order
      )
      cache_preflight(
        request$cerebro_data,
        preflight_data,
        fingerprints
      )
    }
    app_dir <- tryCatch(
      hooks$build_app(request, stage, auth_material = auth_material),
      error = function(error) error
    )
    if (inherits(app_dir, "condition")) {
      return(.builder_build_failure(conditionMessage(app_dir)))
    }
    auth_env_file <- if (auth_enabled) {
      file.path(app_dir, "viewer-auth.env")
    } else {
      NULL
    }
    app_tree_identity <- tryCatch(
      .builder_app_tree_summary(.builder_app_tree_identity(
        app_dir,
        .previous = .builder_build_app_tree_seed(app_dir, request)
      )),
      error = identity
    )
    if (
      inherits(app_tree_identity, "condition") ||
        !is.list(app_tree_identity) ||
        .builder_app_has_reference(app_tree_identity)
    ) {
      return(.builder_build_failure(
        if (inherits(app_tree_identity, "condition")) {
          conditionMessage(app_tree_identity)
        } else {
          "The staged App identity is invalid."
        }
      ))
    }
    result$app_dir <- app_dir
    result$app_tree_identity <- app_tree_identity
    cleaned_assets <- tryCatch(
      .builder_build_cleanup_spatial_assets(stage),
      error = function(error) error
    )
    if (inherits(cleaned_assets, "condition")) {
      return(.builder_build_failure(conditionMessage(cleaned_assets)))
    }
  }
  if (auth_enabled) {
    cleaned <- try(
      builder_auth_cleanup_material(
        auth_material,
        stage,
        keep_env = FALSE
      ),
      silent = TRUE
    )
    if (inherits(cleaned, "try-error")) {
      return(.builder_build_failure(
        "The authentication files could not be cleaned up."
      ))
    }
    cleanup_complete <- TRUE
    result$auth_enabled <- TRUE
    result$auth_env_file <- auth_env_file
  } else {
    result$auth_enabled <- FALSE
    result$auth_env_file <- NULL
  }
  result$publishable <- length(result$built) == length(plan$items)
  result
}
