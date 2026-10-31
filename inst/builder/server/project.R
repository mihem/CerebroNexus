## Builder server: durable projects.

builder_project <- reactiveVal(NULL)
builder_project_restore <- reactiveVal(NULL)
builder_project_pending_entries <- reactiveVal(list())
builder_project_artifacts <- reactiveVal(list())
builder_project_skipped_ids <- reactiveVal(character())
builder_project_build_plan <- reactiveVal(NULL)
builder_project_auto_save_signature <- reactiveVal(NULL)
builder_project_operation_phase <- reactiveVal("idle")
builder_project_last_save_error <- reactiveVal(NULL)
builder_project_pending_folder <- reactiveVal(NULL)
# Which project picker the "choosing" phase belongs to: "create" or "open".
builder_project_picker_kind <- reactiveVal(NULL)
builder_project_source_sync <- reactiveVal(list(
  status = "idle",
  completed = 0L,
  total = 0L,
  failed = 0L
))
builder_project_source_process <- reactiveVal(NULL)
builder_project_source_generation <- reactiveVal(0)
builder_project_source_run <- reactiveVal(NULL)
builder_project_source_queue <- reactiveVal(list())
builder_project_source_progress <- reactiveVal(NULL)
builder_project_source_poll_scheduled <- reactiveVal(FALSE)
builder_project_open_task <- reactiveVal(NULL)
builder_project_registration_task <- reactiveVal(NULL)
builder_project_crb_jobs <- reactiveVal(list())
builder_project_crb_task <- reactiveVal(NULL)
builder_project_crb_batch <- reactiveVal(0L)
builder_project_source_save_waiter <- reactiveVal(NULL)
builder_project_open_generation <- reactiveVal(0)
builder_project_open_previous_phase <- reactiveVal("idle")
builder_project_open_actions <- reactiveVal(NULL)
builder_project_open_request <- reactiveVal(NULL)
builder_project_open_discard_confirmed <- reactiveVal(FALSE)
builder_project_open_wait <- reactiveVal(NULL)
builder_project_table_asset_process <- reactiveVal(NULL)
builder_project_table_asset_generation <- reactiveVal(0)
builder_client_import_state <- reactiveVal(list(nonce = 0, pending = 0L))
builder_connection_state <- reactiveVal("connected")
builder_project_restore_progress <- reactiveVal(list(
  mode = "idle",
  total = 0L,
  remaining = 0L
))
builder_project_phase <- reactiveVal("none")
builder_build_stop_requested <- reactiveVal(FALSE)
builder_legacy_qs_notice_queue <- reactiveVal(list())
builder_legacy_qs_notice_seen <- reactiveVal(character())

builder_queue_legacy_qs_notice <- function(entries) {
  found <- builder_project_legacy_qs_datasets(entries)
  if (!length(found)) {
    return(invisible(FALSE))
  }
  pending <- isolate(builder_legacy_qs_notice_queue())
  seen <- isolate(builder_legacy_qs_notice_seen())
  for (dataset in found) {
    key <- as.character(dataset$id %||% dataset$name %||% "")
    if (!nzchar(key) || key %in% seen || !is.null(pending[[key]])) {
      next
    }
    pending[[key]] <- dataset
    seen <- c(seen, key)
  }
  builder_legacy_qs_notice_seen(unique(seen))
  builder_legacy_qs_notice_queue(pending)
  invisible(length(pending) > 0L)
}

builder_project_build_entries <- function(entries) {
  project <- isolate(builder_project())
  if (is.null(project)) {
    return(entries)
  }
  builder_project_entries_for_build(
    entries,
    isolate(builder_project_artifacts()),
    project$root,
    builder_configuration_identity_cache,
    verification_cache = builder_checkpoint_verification_cache
  )
}

## Applies entries returned by a Project write or source copy. Unlike sets(),
## which clears every per-dataset cache, only the entries that changed are
## evicted, so the derived state of untouched datasets is reused.
builder_project_update_entries <- function(entries) {
  current_state <- isolate(store())
  updated <- tryCatch(
    builder_reduce_state(
      current_state,
      list(type = "replace_all", datasets = entries),
      trusted = TRUE
    ),
    error = identity
  )
  if (inherits(updated, "condition")) {
    sets(entries)
    return(invisible(TRUE))
  }
  entry_ids <- function(values) {
    vapply(values, function(entry) as.character(entry$id), character(1))
  }
  previous <- current_state$datasets %||% list()
  previous_ids <- entry_ids(previous)
  next_ids <- entry_ids(updated$datasets)
  changed <- next_ids[
    !vapply(
      seq_along(updated$datasets),
      function(index) {
        position <- match(next_ids[[index]], previous_ids)
        !is.na(position) &&
          identical(updated$datasets[[index]], previous[[position]])
      },
      logical(1)
    )
  ]
  if (!length(changed) && identical(next_ids, previous_ids)) {
    return(invisible(FALSE))
  }
  kept <- setdiff(intersect(next_ids, previous_ids), changed)
  builder_dataset_state_cache_retain(builder_dataset_state_cache, kept)
  builder_project_configuration_cache_retain_datasets(
    builder_configuration_identity_cache,
    kept
  )
  store(updated)
  invisible(TRUE)
}

builder_project_record_map <- function(manifest) {
  records <- manifest$datasets %||% list()
  ids <- vapply(records, function(record) as.character(record$id), character(1))
  stats::setNames(records, ids)
}

register_loaded_entry_finalizer(function(entry) {
  pending <- isolate(builder_project_pending_entries())
  record <- pending[[entry$id]] %||% NULL
  project <- isolate(builder_project())
  if (is.null(record) || is.null(project)) {
    return(entry)
  }
  builder_project_finalize_loaded_entry(entry, record, project$root)
})

send_builder_project_source_load_state <- function(
  id,
  status,
  message = NULL
) {
  if (!builder_has_text(id) || !status %in% c("loading", "ready", "error")) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage(
    "builder_project_source_load_state",
    list(
      dataset = as.character(id),
      status = as.character(status),
      message = if (builder_has_text(message)) as.character(message) else NULL
    )
  )
  invisible(TRUE)
}

builder_project_commit_restored_entry <- function(entry, error = NULL) {
  id <- as.character(entry$id %||% "")
  if (!builder_has_text(id)) {
    return(invisible(FALSE))
  }
  pending <- isolate(builder_project_pending_entries())
  record <- pending[[id]] %||% NULL
  if (is.null(record)) {
    return(invisible(FALSE))
  }
  restore_action <- record$runtime_restore_action %||% ""
  editing_source <- identical(restore_action, "edit")
  # Batch Open must retain the saved selected dataset. Only an explicit
  # "edit source" action should move focus as its background load completes.
  focus_source <- editing_source
  pending[[id]] <- NULL
  builder_project_pending_entries(pending)
  marks <- isolate(dataset_check_marks())
  if (is.null(error)) {
    builder_project_skipped_ids(setdiff(
      isolate(builder_project_skipped_ids()),
      id
    ))
    mark <- builder_project_restored_check_identity(record, entry)
    if (is.null(mark)) {
      marks <- marks[setdiff(names(marks), id)]
    } else {
      marks[[id]] <- mark
    }
    alignment_server$discard_project_drafts(id)
    project <- isolate(builder_project())
    if (!is.null(project) && isTRUE(focus_source)) {
      project$manifest$last_ui$stage <- "configure"
      project$manifest$last_ui$selected_dataset <- id
      builder_project(project)
    }
  } else {
    marks <- marks[setdiff(names(marks), id)]
    builder_project_mark_restore_skipped(id)
  }
  dataset_check_marks(marks)
  progress <- isolate(builder_project_restore_progress())
  builder_project_restore_progress(list(
    mode = if (length(pending)) "restoring" else "idle",
    total = as.integer(progress$total %||% length(pending)),
    remaining = length(pending),
    selected_total = as.integer(
      progress$selected_total %||% (length(isolate(sets())) + length(pending))
    ),
    ready = length(isolate(sets()))
  ))
  if (!length(pending)) {
    finish_builder_project_restore()
  }
  if (isTRUE(editing_source)) {
    send_builder_project_source_load_state(
      id,
      if (is.null(error)) "ready" else "error",
      error
    )
  }
  invisible(TRUE)
}

builder_project_dirty <- reactive({
  project <- builder_project()
  if (is.null(project)) {
    return(FALSE)
  }
  builder_project_live_dirty(
    sets(),
    checked_dataset_ids(),
    project$manifest,
    identity_cache = builder_configuration_identity_cache,
    ignored_ids = builder_project_skipped_ids()
  )
})

builder_current_project_phase <- function() {
  operation <- builder_project_operation_phase()
  if (!identical(operation, "idle")) {
    return(operation)
  }
  if (is.null(builder_project())) {
    return("none")
  }
  if (isTRUE(builder_project_dirty())) "dirty" else "clean"
}

observe({
  next_phase <- builder_current_project_phase()
  if (!identical(next_phase, isolate(builder_project_phase()))) {
    builder_project_phase(next_phase)
  }
})

builder_spatial_drafts_dirty <- reactive({
  drafts <- alignment_server$pending_drafts()
  length(drafts %||% list()) > 0L
})

builder_activity <- reactive({
  project <- builder_project()
  client_state <- builder_client_import_state()
  builder_activity_state(
    connection = builder_connection_state(),
    client_imports = client_state$pending %||% 0L,
    server_imports = length(imports()$entries %||% list()) > 0L,
    project_phase = builder_project_phase(),
    spatial_dirty = builder_spatial_drafts_dirty(),
    source_syncing = builder_project_source_sync()$status %in%
      c("syncing", "cancelling"),
    crb_generating = !is.null(builder_project_crb_task()),
    build_locked = builder_mutations_locked(build_flow(), protocol()),
    has_project = !is.null(project),
    has_datasets = isTRUE(workflow_has_datasets())
  )
})

builder_build_overlay <- reactive({
  settlement <- release_settlement_context()
  pending <- protocol()$pending
  builder_build_operation_overlay_model(
    flow = build_flow(),
    protocol = protocol(),
    note = busy_note(),
    result = result(),
    phase = build_operation_phase(),
    can_stop = !builder_build_stop_requested() &&
      ((is.list(settlement) && identical(settlement$status, "settling")) ||
        (is.list(pending) &&
          identical(pending$kind, "build") &&
          builder_has_text(pending$payload$progress_path %||% "")))
  )
})

builder_capabilities <- reactive({
  builder_activity_capabilities(builder_activity())
})

builder_operation_allowed <- function(operation, notify = TRUE) {
  activity <- isolate(builder_activity())
  activity$project_phase <- isolate(builder_current_project_phase())
  allowed <- isTRUE(builder_activity_capabilities(activity)[[operation]])
  if (!allowed && isTRUE(notify)) {
    builder_notify(
      "info",
      builder_activity_reason(activity, operation),
      duration = 6
    )
  }
  allowed
}

observeEvent(
  input$builder_client_import_state,
  {
    state <- input$builder_client_import_state
    if (
      !is.list(state) ||
        is.object(state) ||
        !all(c("nonce", "pending") %in% names(state)) ||
        !is.numeric(state$nonce) ||
        length(state$nonce) != 1L ||
        is.na(state$nonce) ||
        !is.finite(state$nonce) ||
        !is.numeric(state$pending) ||
        length(state$pending) != 1L ||
        is.na(state$pending) ||
        !is.finite(state$pending) ||
        state$pending < 0
    ) {
      return()
    }
    previous <- isolate(builder_client_import_state())
    if (state$nonce < (previous$nonce %||% 0)) {
      return()
    }
    builder_client_import_state(list(
      nonce = as.double(state$nonce),
      pending = as.integer(state$pending)
    ))
  },
  ignoreInit = TRUE
)

observeEvent(
  input$builder_client_connection,
  {
    state <- input$builder_client_connection
    if (is.list(state) && identical(state$status %||% NULL, "connected")) {
      builder_connection_state("connected")
      activity_message <- builder_activity_message(
        isolate(builder_activity()),
        build_overlay = isolate(builder_build_overlay()),
        picker_kind = isolate(builder_project_picker_kind())
      )
      session$onFlushed(
        function() {
          session$sendCustomMessage(
            "builder_activity_state",
            activity_message
          )
        },
        once = TRUE
      )
    }
  },
  ignoreInit = TRUE
)

builder_activity_message <- function(
  activity,
  restore_progress = list(),
  build_overlay = list(active = FALSE),
  picker_kind = NULL
) {
  creating <- identical(picker_kind, "create")
  capabilities <- builder_activity_capabilities(activity)
  phase <- activity$project_phase
  build_active <- is.list(build_overlay) && isTRUE(build_overlay$active)
  restore_entry <- if (identical(phase, "restoring")) {
    restore_id <- import_focus_id()
    if (builder_has_text(restore_id)) {
      builder_import_find(imports(), restore_id)
    } else {
      NULL
    }
  } else {
    NULL
  }
  restore_file <- restore_entry$filename %||% restore_entry$label %||% NULL
  restore_bytes <- suppressWarnings(as.numeric(
    restore_entry$size %||% NA_real_
  )[1L])
  restore_size <- if (isTRUE(is.finite(restore_bytes)) && restore_bytes >= 0) {
    builder_file_human_size(restore_bytes)
  } else {
    NULL
  }
  started_at_ms <- suppressWarnings(as.numeric(
    restore_entry$started_at_ms %||% NA_real_
  )[1L])
  restore_elapsed <- if (
    isTRUE(restore_entry$load_state %in% .builder_import_active_states) &&
      isTRUE(is.finite(started_at_ms))
  ) {
    elapsed <- max(
      0L,
      as.integer((as.numeric(Sys.time()) * 1000 - started_at_ms) / 1000)
    )
    paste0(elapsed %/% 60L, "m ", sprintf("%02d", elapsed %% 60L), "s elapsed")
  } else {
    NULL
  }
  title <- if (build_active) {
    build_overlay$title
  } else {
    switch(
      phase,
      choosing = if (creating) "Choose project folder" else "Open project",
      opening = "Opening project",
      restoring = "Restoring datasets",
      saving = "Saving project",
      registering = "Saving dataset cache",
      "Working on your Builder project"
    )
  }
  message <- if (build_active) {
    build_overlay$message
  } else {
    switch(
      phase,
      choosing = if (creating) {
        paste(
          "The folder picker is open.",
          builder_project_save_location_note()
        )
      } else {
        "The project picker is open."
      },
      saving = "Keep this page open until saving finishes.",
      opening = "Checking saved datasets…",
      restoring = if (builder_has_text(restore_file)) {
        paste0("Loading ", restore_file, "…")
      } else {
        "Loading the selected source datasets…"
      },
      registering = "Saving verified datasets. Keep this page open.",
      conflict = "This project changed in another Builder window.",
      NULL
    )
  }
  total <- as.integer(restore_progress$total %||% 0L)
  restore_detail <- if (identical(phase, "restoring") && total > 0L) {
    c(
      builder_project_restore_progress_detail(restore_progress),
      restore_size,
      restore_elapsed
    )
  } else {
    character()
  }
  detail <- if (build_active) {
    build_overlay$detail
  } else if (identical(phase, "opening")) {
    "Keep this page open."
  } else if (length(restore_detail)) {
    paste(restore_detail, collapse = " · ")
  } else {
    NULL
  }
  list(
    phase = phase,
    capabilities = capabilities,
    busy_title = title,
    busy_message = message,
    busy_detail = detail,
    busy_detail_items = if (length(restore_detail)) restore_detail else NULL,
    busy_phase = if (build_active) build_overlay$phase %||% NULL else NULL,
    has_project = isTRUE(activity$has_project),
    picker_waiting = identical(phase, "choosing") ||
      isTRUE(build_overlay$picker_waiting),
    # Opening owns a Shiny dialog (checking, compatibility or recovery).
    # Keep the workspace inert without stacking a second operation dialog.
    busy_modal = identical(phase, "opening") &&
      identical(restore_progress$mode %||% NULL, "opening"),
    build_cancelable = build_active && isTRUE(build_overlay$cancelable),
    open_cancelable = identical(phase, "opening") &&
      identical(restore_progress$mode %||% NULL, "opening"),
    warn_before_unload = isTRUE(capabilities$warn_before_unload) ||
      build_active,
    # build_locked already disables edits, Save and Open; the Build progress
    # panel no longer needs the whole page inert to stay usable.
    page_inert = isTRUE(capabilities$page_inert),
    # Shown beside the Build actions while a Project write holds Build back,
    # so "Build again" is visibly waiting instead of ignoring the click.
    build_reason = builder_activity_reason(activity, "build")
  )
}

## The activity depends on the import queue, which changes about ten times a
## second while data load; the browser sweeps its controls for every message.
builder_activity_sent <- new.env(parent = emptyenv())
observe({
  activity <- builder_activity()
  if (identical(activity$project_phase, "restoring")) {
    shiny::invalidateLater(1000, session)
  }
  message <- builder_activity_message(
    activity,
    builder_project_restore_progress(),
    builder_build_overlay(),
    builder_project_picker_kind()
  )
  if (identical(message, builder_activity_sent$message)) {
    return()
  }
  builder_activity_sent$message <- message
  session$sendCustomMessage("builder_activity_state", message)
})

observe({
  pending <- builder_legacy_qs_notice_queue()
  if (!length(pending)) {
    return()
  }
  client_pending <- as.integer(
    builder_client_import_state()$pending %||% 0L
  )
  server_pending <- length(imports()$entries %||% list())
  phase <- builder_project_operation_phase()
  if (
    client_pending > 0L ||
      server_pending > 0L ||
      phase %in% c("choosing", "opening", "restoring") ||
      length(builder_project_pending_entries()) > 0L ||
      !is.null(builder_project_restore())
  ) {
    return()
  }
  builder_legacy_qs_notice_queue(list())
  session$onFlushed(
    function() {
      shiny::showModal(
        builder_legacy_qs_notice_dialog(unname(pending), origin = "add"),
        session = session
      )
    },
    once = TRUE
  )
})

observe({
  progress <- builder_project_source_sync()
  session$sendCustomMessage(
    "builder_project_source_progress",
    list(
      status = progress$status %||% "idle",
      completed = as.integer(progress$completed %||% 0L),
      total = as.integer(progress$total %||% 0L),
      failed = as.integer(progress$failed %||% 0L),
      bytes_done = as.double(progress$bytes_done %||% 0),
      bytes_total = as.double(progress$bytes_total %||% 0),
      error = progress$error %||% NULL
    )
  )
})

builder_project_capture_build_plan <- function(plan) {
  if (is.null(isolate(builder_project()))) {
    builder_project_build_plan(NULL)
    return(invisible(FALSE))
  }
  builder_project_build_plan(plan)
  invisible(plan)
}

builder_project_source_runtime_file <- function() {
  candidates <- c(
    file.path(getwd(), "project.R"),
    file.path(getwd(), "inst", "builder", "project.R")
  )
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("The Builder Project runtime could not be found.", call. = FALSE)
  }
  normalizePath(found[[1L]], winslash = "/", mustWork = TRUE)
}

builder_project_open_runtime_files <- function(include_server_paths = FALSE) {
  project <- builder_project_source_runtime_file()
  builder_dir <- dirname(project)
  files <- if (isTRUE(include_server_paths)) {
    c(
      file.path(builder_dir, "core", "bundle_path_contract.R"),
      file.path(builder_dir, "io.R"),
      project
    )
  } else {
    c(file.path(builder_dir, "io.R"), project)
  }
  if (any(!file.exists(files))) {
    stop("The Builder Project open runtime could not be found.", call. = FALSE)
  }
  normalizePath(files, winslash = "/", mustWork = TRUE)
}

builder_project_package_runtime_files <- function(builder_dir) {
  source_root <- .builder_worker_package_source(builder_dir)
  if (is.null(source_root)) {
    return(character())
  }
  description <- read.dcf(file.path(source_root, "DESCRIPTION"))
  collate <- if ("Collate" %in% colnames(description)) {
    description[1L, "Collate"]
  } else {
    NA_character_
  }
  files <- if (is.na(collate) || !nzchar(collate)) {
    sort(list.files(
      file.path(source_root, "R"),
      pattern = "[.][Rr]$",
      full.names = TRUE
    ))
  } else {
    file.path(
      source_root,
      "R",
      scan(text = collate, what = character(), quiet = TRUE)
    )
  }
  if (any(!file.exists(files))) {
    stop("The Builder package runtime is incomplete.", call. = FALSE)
  }
  normalizePath(files, winslash = "/", mustWork = TRUE)
}

builder_project_registration_runtime_files <- function() {
  project <- builder_project_source_runtime_file()
  builder_dir <- dirname(project)
  viewer_dir <- file.path(dirname(builder_dir), "viewer")
  files <- c(
    builder_project_package_runtime_files(builder_dir),
    file.path(builder_dir, "prerequisite.R"),
    file.path(builder_dir, "core", "bundle_path_contract.R"),
    file.path(builder_dir, "app_bundle.R"),
    file.path(viewer_dir, "core", "viewer_content_contract.R"),
    file.path(viewer_dir, "core", "spatial_coordinate_contract.R"),
    file.path(viewer_dir, "core", "spatial_coordinate_transform.R"),
    file.path(builder_dir, "io.R"),
    file.path(builder_dir, "spatial.R"),
    file.path(builder_dir, "manifest.R"),
    file.path(builder_dir, "content_tables.R"),
    file.path(builder_dir, "content_immune.R"),
    file.path(builder_dir, "content_spatial.R"),
    file.path(builder_dir, "content.R"),
    file.path(builder_dir, "profile.R"),
    file.path(builder_dir, "inspect.R"),
    file.path(builder_dir, "adapters.R"),
    file.path(builder_dir, "preview.R"),
    file.path(builder_dir, "extras.R"),
    file.path(builder_dir, "analysis.R"),
    file.path(builder_dir, "marker_import.R"),
    file.path(builder_dir, "worker.R"),
    project,
    file.path(builder_dir, "build.R")
  )
  if (any(!file.exists(files))) {
    stop(
      "The Builder Project registration runtime could not be found.",
      call. = FALSE
    )
  }
  normalizePath(files, winslash = "/", mustWork = TRUE)
}

## An installed Builder has no package source to source into the worker, so
## the worker must attach CerebroNexus itself before running the build code.
builder_project_runtime_needs_package <- function() {
  builder_dir <- dirname(builder_project_source_runtime_file())
  !length(builder_project_package_runtime_files(builder_dir))
}

## Warm Project workers. Manifest writes and attachment staging share one
## session so their transactions stay ordered; source copies run in their own
## session so a long copy never delays a manifest write.
builder_project_writer <- builder_project_worker_new("writer")
builder_project_copier <- builder_project_worker_new("copier")

builder_project_worker_runtime_files <- function(full = FALSE) {
  project <- builder_project_source_runtime_file()
  builder_dir <- dirname(project)
  files <- if (isTRUE(full)) {
    c(
      builder_project_package_runtime_files(builder_dir),
      file.path(builder_dir, "core", "bundle_path_contract.R"),
      file.path(builder_dir, "io.R"),
      project,
      file.path(
        builder_dir,
        "app_bundle",
        c("contract.R", "backend.R", "topology.R")
      ),
      file.path(builder_dir, "extras.R")
    )
  } else {
    c(
      file.path(builder_dir, "core", "bundle_path_contract.R"),
      file.path(builder_dir, "io.R"),
      project,
      file.path(builder_dir, "extras.R")
    )
  }
  if (any(!file.exists(files))) {
    stop(
      "The Builder Project worker runtime could not be found.",
      call. = FALSE
    )
  }
  normalizePath(files, winslash = "/", mustWork = TRUE)
}

builder_project_worker_task_call <- function(worker, task, args) {
  builder_project_worker_call(
    worker,
    builder_project_worker_run,
    list(
      task = builder_project_portable_function(task),
      args = args,
      runtime_files = builder_project_worker_runtime_files(),
      full_runtime_files = builder_project_worker_runtime_files(full = TRUE),
      source_utf8 = builder_source_utf8
    )
  )
}

## Load the worker runtimes as soon as there is something to save, so the
## first Save (which also creates the Project) does not wait for an R session
## to start, source and compile the Builder. The writer also checks each newly
## active Project once (see builder_project_writer_warmup_task), keyed by
## Project and worker start so a restarted worker is readied again.
builder_project_prewarm_workers <- function(
  project = isolate(builder_project()),
  has_datasets = isTRUE(isolate(workflow_has_datasets()))
) {
  if (
    builder_session_closed() ||
      !isTRUE(getOption("CerebroNexus.builder.project_worker_prewarm", TRUE))
  ) {
    return(invisible(FALSE))
  }
  writer <- builder_project_writer
  warm_root <- if (is.list(project) && builder_has_text(project$root %||% "")) {
    project$root
  } else {
    NULL
  }
  warm_key <- if (!is.null(warm_root)) {
    paste(warm_root, project$manifest$project$id %||% "", sep = "::")
  } else if (isTRUE(has_datasets)) {
    "no-project"
  } else {
    NULL
  }
  if (
    !is.null(warm_key) &&
      !builder_project_worker_busy(writer) &&
      !identical(writer$warmed_for, list(warm_key, writer$starts))
  ) {
    warmed <- try(
      builder_project_worker_task_call(
        writer,
        builder_project_writer_warmup_task,
        list(root = warm_root)
      ),
      silent = TRUE
    )
    if (!inherits(warmed, "try-error")) {
      writer$warmed_for <- list(warm_key, writer$starts)
      # The worker only advances when polled; keep it moving through the
      # session start and the warm-up so it is done before the first Save.
      poll <- function() {
        if (isTRUE(tryCatch(warmed$is_alive(), error = function(e) FALSE))) {
          later::later(poll, delay = 0.1)
        }
      }
      later::later(poll, delay = 0.1)
    }
  }
  if (is.null(warm_key)) {
    return(invisible(FALSE))
  }
  for (worker in list(writer, builder_project_copier)) {
    if (is.null(worker$session) && !builder_project_worker_busy(worker)) {
      try(
        builder_project_worker_task_call(
          worker,
          function(runtime_for, args) {
            runtime_for(FALSE)
            TRUE
          },
          list()
        ),
        silent = TRUE
      )
    }
  }
  invisible(TRUE)
}

observe({
  if (is.null(builder_project()) && !isTRUE(workflow_has_datasets())) {
    return()
  }
  later::later(
    function() {
      if (!builder_session_closed()) {
        builder_project_prewarm_workers()
      }
    },
    delay = 0.5
  )
})

reset_builder_project_crb_pipeline <- function() {
  task <- isolate(builder_project_crb_task())
  if (
    inherits(task, "builder_async_task") &&
      task$state %in% c("queued", "running", "cancelling")
  ) {
    stop("Wait for the dataset cache to finish.", call. = FALSE)
  }
  builder_project_crb_task(NULL)
  builder_project_crb_jobs(list())
  builder_project_source_save_waiter(NULL)
  invisible(TRUE)
}

builder_project_update_crb_job <- function(id, value = NULL) {
  jobs <- isolate(builder_project_crb_jobs())
  if (is.null(value)) {
    jobs[[id]] <- NULL
  } else {
    jobs[[id]] <- value
  }
  builder_project_crb_jobs(jobs)
  invisible(value)
}

builder_project_crb_job_token <- function(id, identity) {
  paste(
    id,
    substr(as.character(identity %||% ""), 1L, 12L),
    format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC"),
    sample.int(.Machine$integer.max, 1L),
    sep = "-"
  )
}

builder_project_crb_job_current <- function(id, token) {
  job <- isolate(builder_project_crb_jobs())[[id]] %||% NULL
  is.list(job) && identical(job$token %||% NULL, token)
}

builder_project_record_crb <- function(
  id,
  artifact,
  on_complete = NULL,
  on_closed = NULL
) {
  project <- isolate(builder_project())
  if (is.null(project) || !is.list(artifact)) {
    return(FALSE)
  }
  apply_written <- function(written) {
    if (inherits(written, "condition")) {
      return(FALSE)
    }
    project$manifest <- written$manifest
    project$path <- written$path
    project$name <- written$manifest$project$name
    builder_project(project)
    artifacts <- isolate(builder_project_artifacts())
    artifacts[[id]] <- artifact
    builder_project_artifacts(artifacts)
    TRUE
  }
  if (is.function(on_complete)) {
    previous_phase <- isolate(builder_project_operation_phase())
    builder_project_operation_phase("registering")
    start_builder_project_manifest_write(
      project$manifest,
      project,
      function(written) {
        ok <- if (inherits(written, "condition")) {
          written
        } else {
          tryCatch(apply_written(written), error = identity)
        }
        builder_project_operation_phase(previous_phase)
        on_complete(ok)
      },
      on_closed = on_closed,
      artifact_record = list(id = id, artifact = artifact)
    )
    return(invisible("pending"))
  }
  manifest <- builder_project_manifest_with_artifact(
    project$manifest,
    project$root,
    id,
    artifact
  )
  if (!is.list(manifest)) {
    return(FALSE)
  }
  written <- tryCatch(
    builder_project_write(
      manifest,
      project$root,
      expected_revision = project$manifest$project$revision
    ),
    error = identity
  )
  apply_written(written)
}

builder_project_cache_notice_id <- function(id) {
  paste0("builder-dataset-cache-", id)
}

builder_project_finish_crb_job <- function(id, token, prepared, output) {
  if (builder_session_closed()) {
    try(
      builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
      silent = TRUE
    )
    builder_project_update_crb_job(id, NULL)
    builder_project_crb_task(NULL)
    return(invisible(FALSE))
  }
  if (!builder_project_crb_job_current(id, token)) {
    try(
      builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
      silent = TRUE
    )
    builder_project_crb_task(NULL)
    start_next_builder_project_crb_job()
    return(invisible(FALSE))
  }
  source_status <- isolate(builder_project_source_sync())$status %||% "idle"
  if (
    isolate(builder_project_operation_phase()) %in%
      c("saving", "autosaving") ||
      source_status %in% c("syncing", "cancelling") ||
      !is.null(isolate(builder_project_table_asset_process()))
  ) {
    job <- isolate(builder_project_crb_jobs())[[id]]
    job$status <- "registering"
    builder_project_update_crb_job(id, job)
    later::later(
      function() {
        shiny::withReactiveDomain(session, {
          builder_project_finish_crb_job(id, token, prepared, output)
        })
      },
      delay = 0.2
    )
    return(invisible(TRUE))
  }
  handed_off <- FALSE
  cleanup <- function() {
    try(
      builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
      silent = TRUE
    )
    builder_project_crb_task(NULL)
    start_next_builder_project_crb_job()
  }
  on.exit(if (!handed_off) cleanup(), add = TRUE)
  job <- isolate(builder_project_crb_jobs())[[id]]
  project <- isolate(builder_project())
  current_entry <- isolate(entry_of(id))
  current_mark <- isolate(dataset_check_marks())[[id]] %||% NULL
  current_identity <- if (is.list(current_entry)) {
    tryCatch(
      builder_project_check_identity(
        current_entry,
        builder_configuration_identity_cache
      ),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  if (
    is.null(project) ||
      !identical(project$root, job$root %||% NULL) ||
      !.builder_project_text(current_identity %||% NULL) ||
      !identical(current_mark, job$identity) ||
      !identical(current_identity, job$identity)
  ) {
    builder_project_update_crb_job(id, NULL)
    return(invisible(FALSE))
  }
  if (
    !is.list(prepared) ||
      !isTRUE(prepared$build_succeeded) ||
      .builder_project_text(prepared$error %||% NULL) ||
      !is.list(prepared$artifacts[[id]] %||% NULL)
  ) {
    message <- prepared$error %||%
      "The dataset cache could not be generated and verified."
    builder_project_update_crb_job(id, NULL)
    builder_notify(
      "warning",
      paste(message, "The dataset remains checked without a cache."),
      target = list(dataset = id),
      id = builder_project_cache_notice_id(id)
    )
    return(invisible(FALSE))
  }
  candidate <- prepared$artifacts[[id]]
  if (
    !builder_project_artifact_matches_entry(
      candidate,
      current_entry,
      identity_cache = builder_configuration_identity_cache
    )
  ) {
    message <- "The generated dataset cache did not match the current dataset."
    builder_project_update_crb_job(id, NULL)
    builder_notify(
      "warning",
      paste(message, "The dataset remains checked without a cache."),
      target = list(dataset = id),
      id = builder_project_cache_notice_id(id)
    )
    return(invisible(FALSE))
  }
  finish_record <- function(recorded) {
    if (!builder_project_crb_job_current(id, token)) {
      # Cancelled, or the Project was replaced, while the record was written.
      return(invisible(isTRUE(recorded)))
    }
    if (!isTRUE(recorded)) {
      builder_project_update_crb_job(id, NULL)
      builder_notify(
        "warning",
        tagList(
          "Dataset cache not saved. Select Save to retry.",
          if (inherits(recorded, "condition")) {
            tags$details(
              tags$summary("Details"),
              tags$p(conditionMessage(recorded))
            )
          }
        ),
        target = list(dataset = id),
        id = builder_project_cache_notice_id(id),
        action = list(label = "Save", click = "save_builder_project")
      )
      return(invisible(FALSE))
    }
    builder_project_update_crb_job(id, NULL)
    builder_notify_clear(builder_project_cache_notice_id(id))
    builder_notify(
      "success",
      paste0(current_entry$settings$name %||% id, ": dataset cache ready."),
      target = list(dataset = id),
      duration = 3
    )
    invisible(TRUE)
  }
  job$recording <- TRUE
  builder_project_update_crb_job(id, job)
  recorded <- tryCatch(
    builder_project_record_crb(
      id,
      candidate,
      on_complete = function(ok) {
        on.exit(cleanup(), add = TRUE)
        finish_record(ok)
      },
      on_closed = function() {
        try(
          builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
          silent = TRUE
        )
      }
    ),
    error = function(error) FALSE
  )
  if (identical(recorded, "pending")) {
    handed_off <- TRUE
    return(invisible(TRUE))
  }
  finish_record(recorded)
}

builder_project_fail_crb_job <- function(id, token, error, output = NULL) {
  if (builder_project_crb_job_current(id, token)) {
    builder_project_update_crb_job(id, NULL)
    builder_notify(
      "warning",
      paste(
        conditionMessage(error),
        "The dataset remains checked without a cache."
      ),
      target = list(dataset = id),
      id = builder_project_cache_notice_id(id)
    )
  }
  if (.builder_project_text(output %||% NULL)) {
    try(
      builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
      silent = TRUE
    )
  }
  builder_project_crb_task(NULL)
  start_next_builder_project_crb_job()
  invisible(FALSE)
}

start_next_builder_project_crb_job <- function() {
  if (!is.null(isolate(builder_project_crb_task()))) {
    return(invisible(FALSE))
  }
  project <- isolate(builder_project())
  if (is.null(project)) {
    return(invisible(FALSE))
  }
  jobs <- isolate(builder_project_crb_jobs())
  queued <- names(Filter(function(job) identical(job$status, "queued"), jobs))
  if (!length(queued)) {
    return(invisible(FALSE))
  }
  id <- queued[[1L]]
  job <- jobs[[id]]
  entry <- isolate(entry_of(id))
  mark <- isolate(dataset_check_marks())[[id]] %||% NULL
  entry_identity <- if (is.list(entry)) {
    tryCatch(
      builder_project_check_identity(
        entry,
        builder_configuration_identity_cache
      ),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  if (
    !.builder_project_text(entry_identity %||% NULL) ||
      !identical(mark, job$identity) ||
      !identical(entry_identity, job$identity)
  ) {
    builder_project_update_crb_job(id, NULL)
    return(start_next_builder_project_crb_job())
  }
  budget <- builder_project_checkpoint_budget(list(entry), project$root)
  if (!isTRUE(budget$ok)) {
    return(builder_project_fail_crb_job(
      id,
      job$token,
      simpleError(
        budget$error %||% "The project volume does not have enough free space."
      )
    ))
  }
  checkpoint_parent <- file.path(project$root, "checkpoints")
  if (
    !dir.exists(checkpoint_parent) &&
      !dir.create(checkpoint_parent, recursive = TRUE, showWarnings = FALSE)
  ) {
    return(builder_project_fail_crb_job(
      id,
      job$token,
      simpleError("The checkpoint folder could not be created.")
    ))
  }
  output <- tempfile(
    pattern = paste0("crb-", gsub("[^A-Za-z0-9_.-]", "-", id), "-"),
    tmpdir = checkpoint_parent
  )
  if (!dir.create(output, recursive = TRUE, showWarnings = FALSE)) {
    return(builder_project_fail_crb_job(
      id,
      job$token,
      simpleError("The checkpoint staging folder could not be created.")
    ))
  }
  plan <- tryCatch(
    isolate(freeze_plan_for_output(
      output,
      overwrite = TRUE,
      output_options = builder_build_options(make_app = FALSE),
      entries_override = builder_project_checkpoint_entries(list(entry))
    )),
    error = identity
  )
  if (
    inherits(plan, "condition") ||
      !inherits(plan, "builder_build_plan") ||
      !identical(plan$readiness, "ready")
  ) {
    message <- if (inherits(plan, "condition")) {
      conditionMessage(plan)
    } else {
      plan$error %||% "The dataset cache plan is not ready."
    }
    return(builder_project_fail_crb_job(
      id,
      job$token,
      simpleError(message),
      output
    ))
  }
  runtime_files <- tryCatch(
    builder_project_registration_runtime_files(),
    error = identity
  )
  if (inherits(runtime_files, "condition")) {
    return(builder_project_fail_crb_job(
      id,
      job$token,
      runtime_files,
      output
    ))
  }
  captured_entry <- entry[intersect(
    names(entry),
    c(
      "id",
      "revision",
      "snapshot",
      "settings",
      "acknowledgements",
      "spatial_drafts"
    )
  )]
  previous_artifacts <- isolate(builder_project_artifacts())
  token <- job$token
  job$status <- "generating"
  job$output <- output
  job$root <- project$root
  builder_project_update_crb_job(id, job)
  task <- tryCatch(
    builder_async_submit(
      builder_project_crb_job_expression(),
      list(
        plan = plan,
        output = output,
        root = project$root,
        previous_artifacts = previous_artifacts,
        entry = captured_entry,
        runtime_files = runtime_files,
        load_package = builder_project_runtime_needs_package(),
        source_utf8 = builder_source_utf8
      ),
      .pool = "crb"
    ),
    error = identity
  )
  if (inherits(task, "condition")) {
    return(builder_project_fail_crb_job(id, token, task, output))
  }
  builder_project_crb_task(task)
  builder_async_then(
    task,
    session,
    on_fulfilled = function(prepared) {
      builder_project_finish_crb_job(id, token, prepared, output)
    },
    on_rejected = function(error) {
      builder_project_fail_crb_job(id, token, error, output)
    }
  )
  invisible(TRUE)
}

queue_builder_project_crbs <- function(ids = checked_dataset_ids()) {
  ids <- intersect(as.character(ids), isolate(checked_dataset_ids()))
  added <- 0L
  entries <- isolate(sets())
  entry_map <- stats::setNames(
    entries,
    vapply(entries, `[[`, character(1), "id")
  )
  project <- isolate(builder_project())
  root <- if (is.null(project)) NULL else project$root
  artifacts <- isolate(builder_project_artifacts())
  jobs <- isolate(builder_project_crb_jobs())
  marks <- isolate(dataset_check_marks())
  for (id in ids) {
    entry <- entry_map[[id]] %||% NULL
    if (!is.list(entry)) {
      next
    }
    state <- builder_project_checkpoint_state(
      entry,
      checked = TRUE,
      artifact = artifacts[[id]] %||% NULL,
      root = root,
      identity_cache = builder_configuration_identity_cache
    )
    if (isTRUE(state$crb_ready)) {
      jobs[[id]] <- NULL
      next
    }
    identity <- marks[[id]] %||% NULL
    current <- jobs[[id]] %||% NULL
    if (
      is.list(current) &&
        identical(current$identity %||% NULL, identity) &&
        current$status %in% c("queued", "generating", "registering")
    ) {
      next
    }
    if (!is.list(current)) {
      added <- added + 1L
    }
    jobs[[id]] <- list(
      status = "queued",
      identity = identity,
      token = builder_project_crb_job_token(id, identity)
    )
  }
  builder_project_crb_batch(
    if (length(jobs)) {
      as.integer(isolate(builder_project_crb_batch())) + added
    } else {
      0L
    }
  )
  builder_project_crb_jobs(jobs)
  start_next_builder_project_crb_job()
  invisible(TRUE)
}

## Progress of the CRB queue for the status bar: the dataset being built and
## its position in the batch queued since the queue was last empty.
builder_project_crb_queue_status <- reactive({
  jobs <- builder_project_crb_jobs()
  if (!length(jobs)) {
    return(NULL)
  }
  total <- max(as.integer(builder_project_crb_batch()), length(jobs))
  active <- Filter(
    function(job) job$status %in% c("generating", "registering"),
    jobs
  )
  id <- if (length(active)) names(active)[[1L]] else names(jobs)[[1L]]
  entry <- isolate(entry_of(id))
  list(
    total = as.integer(total),
    position = as.integer(total - length(jobs) + 1L),
    name = as.character(entry$settings$name %||% id)
  )
})

observe({
  if (
    !length(builder_project_crb_jobs()) &&
      !identical(isolate(builder_project_crb_batch()), 0L)
  ) {
    builder_project_crb_batch(0L)
  }
})

## Stops queued and running CRB builds. A CRB already being recorded in the
## manifest finishes its transaction; Save queues the missing CRBs again.
cancel_builder_project_crb_queue <- function() {
  jobs <- isolate(builder_project_crb_jobs())
  task <- isolate(builder_project_crb_task())
  recording <- Filter(function(job) isTRUE(job$recording), jobs)
  if (
    inherits(task, "builder_async_task") &&
      task$state %in% c("queued", "running", "cancelling")
  ) {
    try(builder_async_cancel(task), silent = TRUE)
  }
  for (job in jobs[setdiff(names(jobs), names(recording))]) {
    output <- job$output %||% NULL
    if (.builder_project_text(output)) {
      try(
        builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
        silent = TRUE
      )
    }
  }
  builder_project_crb_jobs(recording)
  if (!length(recording)) {
    builder_project_crb_task(NULL)
  }
  invisible(length(jobs) > 0L)
}

observeEvent(input$cancel_builder_project_crbs, {
  if (!length(isolate(builder_project_crb_jobs()))) {
    return()
  }
  cancel_builder_project_crb_queue()
  builder_notify(
    "info",
    "Dataset cache builds stopped. Save again to build them.",
    target = "status",
    duration = 5
  )
})

builder_project_open_is_current <- function(generation) {
  identical(
    as.double(generation),
    as.double(isolate(builder_project_open_generation()))
  )
}

builder_project_open_needs_confirmation <- function() {
  activity <- isolate(builder_activity())
  activity$project_phase <- isolate(builder_current_project_phase())
  builder_activity_requires_open_confirmation(activity)
}

builder_project_open_discard_dialog <- function() {
  shiny::modalDialog(
    title = builder_dialog_title(
      "Discard unsaved workspace changes?",
      "Open project"
    ),
    shiny::tags$p(
      paste(
        "Opening a project will replace the datasets and settings in the",
        "current workspace. Unsaved changes cannot be recovered."
      )
    ),
    footer = shiny::tagList(
      shiny::actionButton(
        "cancel_discard_builder_project_open",
        "Keep current workspace",
        class = "btn-default"
      ),
      shiny::actionButton(
        "confirm_discard_builder_project_open",
        "Discard and open",
        class = "btn-danger"
      )
    ),
    easyClose = FALSE
  )
}

builder_project_reset_open_state <- function(
  generation = NULL,
  restore_previous = FALSE
) {
  if (
    !is.null(generation) &&
      !builder_project_open_is_current(generation)
  ) {
    return(invisible(FALSE))
  }
  builder_project_open_task(NULL)
  builder_project_open_actions(NULL)
  builder_project_open_request(NULL)
  builder_project_restore_progress(list(
    mode = "idle",
    total = 0L,
    remaining = 0L
  ))
  if (identical(isolate(builder_project_operation_phase()), "opening")) {
    previous_phase <- isolate(builder_project_open_previous_phase())
    next_phase <- if (
      isTRUE(restore_previous) &&
        previous_phase %in% c("idle", "save_failed", "conflict")
    ) {
      previous_phase
    } else {
      "idle"
    }
    builder_project_operation_phase(next_phase)
    builder_project_open_previous_phase(next_phase)
  }
  invisible(TRUE)
}

builder_project_fail_open <- function(generation, error) {
  if (!builder_project_open_is_current(generation)) {
    return(invisible(FALSE))
  }
  builder_project_open_discard_confirmed(FALSE)
  builder_project_restore(NULL)
  task <- isolate(builder_project_open_task())
  if (!is.null(task)) {
    builder_async_cancel(task)
  }
  builder_project_reset_open_state(
    generation,
    restore_previous = TRUE
  )
  shiny::removeModal()
  message <- if (inherits(error, "condition")) {
    conditionMessage(error)
  } else {
    as.character(error %||% "The project file could not be opened.")
  }
  builder_notify("error", message, session = session)
  invisible(FALSE)
}

invalidate_builder_project_open <- function(kill = TRUE) {
  task <- isolate(builder_project_open_task())
  generation <- as.double(isolate(builder_project_open_generation())) + 1
  builder_project_open_generation(generation)
  send_dataset_action_owner(project_epoch = generation)
  if (!is.null(task) && isTRUE(kill)) {
    builder_async_cancel(task)
  }
  builder_project_reset_open_state(generation, restore_previous = TRUE)
}

builder_project_receive_open <- function(
  generation,
  opened,
  selected_path,
  external_roots = NULL
) {
  if (!builder_project_open_is_current(generation)) {
    return(invisible(FALSE))
  }
  builder_project_open_task(NULL)
  if (is.list(opened) && is.list(opened$recovery_offer)) {
    offer <- opened$recovery_offer
    if (
      !is.list(offer$primary_fingerprint) ||
        !is.list(offer$backup_fingerprint) ||
        !builder_has_text(offer$backup_fingerprint$md5 %||% NULL)
    ) {
      return(builder_project_fail_open(
        generation,
        "The Project backup recovery check returned an invalid result."
      ))
    }
    builder_project_open_request(list(
      kind = "backup_recovery",
      generation = generation,
      path = selected_path,
      external_roots = external_roots,
      recovery_offer = offer
    ))
    shiny::showModal(
      builder_project_backup_recovery_dialog(offer),
      session = session
    )
    return(invisible(TRUE))
  }
  if (
    !is.list(opened) ||
      !is.list(opened$manifest) ||
      !is.list(opened$legacy_qs) ||
      !is.list(opened$manifest_fingerprint) ||
      !builder_has_text(opened$manifest_fingerprint$md5 %||% NULL)
  ) {
    return(builder_project_fail_open(
      generation,
      "The background Project check returned an invalid result."
    ))
  }
  if (length(opened$legacy_qs)) {
    builder_project_open_request(list(
      kind = "legacy_qs",
      generation = generation,
      opened = opened
    ))
    shiny::showModal(
      builder_legacy_qs_notice_dialog(opened$legacy_qs, origin = "project"),
      session = session
    )
    return(invisible(TRUE))
  }
  builder_project_open_request(NULL)
  builder_project_finish_open(generation, opened)
}

builder_project_start_open <- function(
  selected_path,
  external_roots = NULL,
  recover_backup = FALSE,
  expected_primary_fingerprint = NULL,
  expected_backup_fingerprint = NULL
) {
  if (builder_session_closed()) {
    return(invisible(FALSE))
  }
  if (
    !is.null(isolate(builder_project_open_task())) ||
      !is.null(isolate(builder_project_open_request()))
  ) {
    invalidate_builder_project_open(kill = TRUE)
  }
  builder_project_restore_progress(list(
    mode = "opening",
    total = 0L,
    remaining = 0L
  ))
  builder_project_operation_phase("opening")
  generation <- as.double(isolate(builder_project_open_generation())) + 1
  builder_project_open_generation(generation)
  send_dataset_action_owner(project_epoch = generation)
  builder_project_restore(NULL)
  builder_project_open_request(NULL)
  shiny::showModal(builder_project_opening_dialog(), session = session)
  task <- tryCatch(
    builder_async_submit(
      quote({
        runtime <- new.env(parent = globalenv())
        runtime$`%||%` <- function(left, right) {
          if (is.null(left)) right else left
        }
        for (runtime_file in runtime_files) {
          source_utf8(runtime_file, runtime)
        }
        if (isTRUE(recover_backup)) {
          runtime$builder_project_recover_backup(
            selected_path,
            expected_primary_fingerprint = expected_primary_fingerprint,
            expected_backup_fingerprint = expected_backup_fingerprint
          )
        }
        opened <- tryCatch(
          runtime$builder_project_open_snapshot(
            selected_path,
            external_roots = external_roots
          ),
          error = identity
        )
        if (inherits(opened, "condition")) {
          offer <- tryCatch(
            runtime$builder_project_backup_recovery_offer(selected_path),
            error = identity
          )
          if (inherits(offer, "condition")) {
            stop(
              conditionMessage(opened),
              " No verified Project backup is available: ",
              conditionMessage(offer),
              call. = FALSE
            )
          }
          return(list(recovery_offer = offer))
        }
        opened
      }),
      list(
        selected_path = selected_path,
        recover_backup = recover_backup,
        expected_primary_fingerprint = expected_primary_fingerprint,
        expected_backup_fingerprint = expected_backup_fingerprint,
        runtime_files = builder_project_open_runtime_files(
          include_server_paths = !is.null(external_roots)
        ),
        external_roots = external_roots,
        source_utf8 = builder_source_utf8
      )
    ),
    error = identity
  )
  if (inherits(task, "condition")) {
    return(builder_project_fail_open(generation, task))
  }
  if (!builder_project_open_is_current(generation)) {
    builder_async_cancel(task)
    return(invisible(FALSE))
  }
  builder_project_open_task(task)
  builder_async_then(
    task,
    session,
    on_fulfilled = function(opened) {
      builder_project_receive_open(
        generation,
        opened,
        selected_path,
        external_roots = external_roots
      )
    },
    on_rejected = function(error) {
      builder_project_fail_open(generation, error)
    }
  )
  invisible(TRUE)
}

builder_project_finish_open <- function(generation, opened) {
  if (builder_session_closed()) {
    invalidate_builder_project_open(kill = TRUE)
    return(invisible(FALSE))
  }
  if (!builder_project_open_is_current(generation)) {
    return(invisible(FALSE))
  }
  if (
    !is.list(opened) ||
      !is.list(opened$manifest) ||
      !.builder_project_text(opened$root) ||
      !.builder_project_text(opened$path)
  ) {
    return(builder_project_fail_open(
      generation,
      "The background Project check returned an invalid result."
    ))
  }
  manifest <- opened$manifest
  if (identical(manifest$pending_build$status %||% NULL, "running")) {
    manifest$pending_build$status <- "interrupted"
  }
  builder_project_restore(list(
    manifest = manifest,
    root = opened$root,
    path = opened$path,
    manifest_fingerprint = opened$manifest_fingerprint
  ))
  builder_project_reset_open_state(
    generation,
    restore_previous = TRUE
  )
  shiny::showModal(
    builder_project_restore_dialog(manifest),
    session = session
  )
  invisible(TRUE)
}

builder_project_schedule_source_sync_poll <- function(delay = 0.2) {
  if (isTRUE(isolate(builder_project_source_poll_scheduled()))) {
    return(invisible(FALSE))
  }
  builder_project_source_poll_scheduled(TRUE)
  scheduled <- tryCatch(
    {
      later::later(
        function() {
          builder_project_source_poll_scheduled(FALSE)
          if (builder_session_closed()) {
            builder_project_poll_source_sync()
          } else {
            shiny::withReactiveDomain(session, {
              builder_project_poll_source_sync()
            })
          }
        },
        delay = max(0, as.numeric(delay))
      )
      TRUE
    },
    error = identity
  )
  if (inherits(scheduled, "condition")) {
    builder_project_source_poll_scheduled(FALSE)
    stop(scheduled)
  }
  invisible(TRUE)
}

builder_project_stop_source_sync_process <- function(kill = TRUE) {
  process <- isolate(builder_project_source_process())
  progress_path <- isolate(builder_project_source_progress())
  alive <- if (is.null(process)) {
    FALSE
  } else {
    tryCatch(process$is_alive(), error = identity)
  }
  if (!is.null(process) && isTRUE(kill) && isTRUE(alive)) {
    try(process$kill_tree(), silent = TRUE)
    alive <- tryCatch(process$is_alive(), error = identity)
    if (isTRUE(alive)) {
      try(process$kill(), silent = TRUE)
      alive <- tryCatch(process$is_alive(), error = identity)
    }
  }
  if (!identical(alive, FALSE)) {
    return(invisible(FALSE))
  }
  if (.builder_project_text(progress_path)) {
    unlink(progress_path, force = TRUE)
    unlink(paste0(progress_path, ".tmp"), force = TRUE)
  }
  builder_project_source_process(NULL)
  builder_project_source_run(NULL)
  builder_project_source_progress(NULL)
  invisible(TRUE)
}

invalidate_builder_project_source_sync <- function(kill = TRUE) {
  builder_project_source_generation(
    as.double(isolate(builder_project_source_generation())) + 1
  )
  builder_project_source_queue(list())
  stopped <- builder_project_stop_source_sync_process(kill = kill)
  if (isTRUE(stopped)) {
    builder_project_source_sync(list(
      status = "idle",
      completed = 0L,
      total = 0L,
      failed = 0L
    ))
    return(invisible(TRUE))
  }
  source_run <- isolate(builder_project_source_run())
  if (!is.list(source_run)) {
    source_run <- list()
  }
  source_run$cancelling <- TRUE
  source_run$cancel_started <- source_run$cancel_started %||% Sys.time()
  source_run$cancel_deadline <- source_run$cancel_deadline %||%
    (Sys.time() + 10)
  builder_project_source_run(source_run)
  progress <- isolate(builder_project_source_sync())
  builder_project_source_sync(list(
    status = "cancelling",
    completed = as.integer(progress$completed %||% 0L),
    total = as.integer(progress$total %||% 0L),
    failed = as.integer(progress$failed %||% 0L),
    error = "The previous source copy is still stopping. Keep this page open."
  ))
  builder_project_schedule_source_sync_poll(delay = 0.2)
  invisible(FALSE)
}

builder_project_start_source_sync <- function() {
  process <- isolate(builder_project_source_process())
  if (!is.null(process)) {
    return(invisible(FALSE))
  }
  jobs <- isolate(builder_project_source_queue())
  if (!length(jobs)) {
    return(invisible(FALSE))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    message <- paste(
      "Source files could not be saved in the background because",
      "callr is unavailable."
    )
    builder_project_source_sync(list(
      status = "failed",
      completed = 0L,
      total = length(jobs),
      failed = length(jobs),
      error = message
    ))
    builder_notify("error", message)
    return(invisible(FALSE))
  }
  project <- isolate(builder_project())
  if (is.null(project)) {
    return(invisible(FALSE))
  }
  active_ids <- vapply(jobs, `[[`, character(1), "id")
  source_run <- tryCatch(
    builder_project_source_context(
      project,
      isolate(builder_project_source_generation()),
      active_ids = active_ids
    ),
    error = identity
  )
  if (inherits(source_run, "condition")) {
    builder_project_source_sync(list(
      status = "failed",
      completed = 0L,
      total = length(jobs),
      failed = length(jobs),
      error = conditionMessage(source_run)
    ))
    builder_notify("error", conditionMessage(source_run))
    return(invisible(FALSE))
  }
  progress_path <- tempfile(
    pattern = ".builder-source-sync-",
    tmpdir = project$root,
    fileext = ".rds"
  )
  builder_project_source_progress(progress_path)
  process <- tryCatch(
    builder_project_worker_task_call(
      builder_project_copier,
      function(runtime_for, args) {
        runtime_for(FALSE)$builder_project_copy_source_jobs(
          args$jobs,
          args$progress_path
        )
      },
      list(jobs = unname(jobs), progress_path = progress_path)
    ),
    error = identity
  )
  if (inherits(process, "condition")) {
    unlink(progress_path, force = TRUE)
    builder_project_source_progress(NULL)
    builder_project_source_run(NULL)
    builder_project_source_sync(list(
      status = "failed",
      completed = 0L,
      total = length(jobs),
      failed = length(jobs),
      error = conditionMessage(process)
    ))
    builder_notify("error", conditionMessage(process))
    return(invisible(FALSE))
  }
  queued <- isolate(builder_project_source_queue())
  for (id in names(jobs)) {
    if (identical(queued[[id]], jobs[[id]])) {
      queued[[id]] <- NULL
    }
  }
  builder_project_source_queue(queued)
  source_run$jobs <- unname(jobs)
  builder_project_source_process(process)
  builder_project_source_run(source_run)
  builder_project_source_sync(list(
    status = "syncing",
    completed = 0L,
    total = length(jobs),
    failed = 0L
  ))
  builder_project_schedule_source_sync_poll(delay = 0.2)
  invisible(TRUE)
}

builder_project_poll_source_sync <- function() {
  process <- isolate(builder_project_source_process())
  if (is.null(process)) {
    return(invisible(FALSE))
  }
  source_run <- isolate(builder_project_source_run())
  project <- isolate(builder_project())
  run_is_current <- builder_project_source_context_matches(
    source_run,
    project,
    isolate(builder_project_source_generation())
  )
  progress_path <- isolate(builder_project_source_progress())
  if (
    isTRUE(run_is_current) &&
      .builder_project_text(progress_path) &&
      file.exists(progress_path)
  ) {
    progress <- tryCatch(readRDS(progress_path), error = function(error) NULL)
    if (is.list(progress)) {
      builder_project_source_sync(list(
        status = "syncing",
        completed = as.integer(progress$completed %||% 0L),
        total = as.integer(progress$total %||% 0L),
        failed = as.integer(progress$failed %||% 0L),
        bytes_done = as.double(progress$bytes_done %||% 0),
        bytes_total = as.double(progress$bytes_total %||% 0),
        current = progress$current %||% NULL
      ))
    }
  }
  alive <- tryCatch(process$is_alive(), error = identity)
  cancelling <- is.list(source_run) && isTRUE(source_run$cancelling)
  if (inherits(alive, "condition")) {
    progress <- isolate(builder_project_source_sync())
    builder_project_source_sync(list(
      status = if (cancelling) "cancelling" else "syncing",
      completed = as.integer(progress$completed %||% 0L),
      total = as.integer(progress$total %||% 0L),
      failed = as.integer(progress$failed %||% 0L),
      error = paste0(
        "The source copy process state could not be confirmed: ",
        conditionMessage(alive)
      )
    ))
    builder_project_schedule_source_sync_poll(
      delay = if (cancelling) 0.5 else 0.2
    )
    return(invisible(TRUE))
  }
  if (!is.logical(alive) || length(alive) != 1L || is.na(alive)) {
    progress <- isolate(builder_project_source_sync())
    builder_project_source_sync(list(
      status = if (cancelling) "cancelling" else "syncing",
      completed = as.integer(progress$completed %||% 0L),
      total = as.integer(progress$total %||% 0L),
      failed = as.integer(progress$failed %||% 0L),
      error = "The source copy process returned an invalid liveness state."
    ))
    builder_project_schedule_source_sync_poll(
      delay = if (cancelling) 0.5 else 0.2
    )
    return(invisible(TRUE))
  }
  if (isTRUE(alive)) {
    if (cancelling) {
      try(process$kill_tree(), silent = TRUE)
      try(process$kill(), silent = TRUE)
      cancel_deadline <- source_run$cancel_deadline %||% Sys.time()
      if (Sys.time() >= cancel_deadline) {
        progress <- isolate(builder_project_source_sync())
        builder_project_source_sync(list(
          status = "cancelling",
          completed = as.integer(progress$completed %||% 0L),
          total = as.integer(progress$total %||% 0L),
          failed = as.integer(progress$failed %||% 0L),
          error = paste(
            "The previous source copy has not stopped yet.",
            "Its process remains owned and will continue to be reaped."
          )
        ))
      }
    }
    builder_project_schedule_source_sync_poll(
      delay = if (cancelling) 0.5 else 0.2
    )
    return(invisible(TRUE))
  }
  results <- tryCatch(process$get_result(), error = identity)
  active_ids <- if (is.list(source_run)) {
    source_run$active_ids %||% character()
  } else {
    character()
  }
  if (inherits(results, "condition")) {
    results <- lapply(active_ids, function(id) {
      list(
        id = id,
        status = "failed",
        error = conditionMessage(results)
      )
    })
  }
  # Session copies moved into the Project no longer exist at their old path;
  # repoint live entries now, whatever happens to the results write.
  live_entries <- isolate(sets())
  relocated <- builder_project_relocate_moved_sources(live_entries, results)
  if (!identical(relocated, live_entries)) {
    builder_project_update_entries(relocated)
  }
  finish <- function(written) {
    run_is_current <- builder_project_source_context_matches(
      source_run,
      isolate(builder_project()),
      isolate(builder_project_source_generation())
    )
    unlink(progress_path %||% "", force = TRUE)
    unlink(paste0(progress_path %||% "", ".tmp"), force = TRUE)
    builder_project_source_process(NULL)
    builder_project_source_run(NULL)
    builder_project_source_progress(NULL)
    if (!isTRUE(run_is_current)) {
      builder_project_source_sync(list(
        status = "idle",
        completed = 0L,
        total = 0L,
        failed = 0L
      ))
      if (length(isolate(builder_project_source_queue()))) {
        builder_project_start_source_sync()
      }
      return(invisible(FALSE))
    }
    failed <- sum(vapply(
      results,
      function(result) identical(result$status, "failed"),
      logical(1)
    ))
    if (inherits(written, "error") || is.null(written)) {
      failed <- max(1L, failed)
      builder_notify(
        "error",
        if (inherits(written, "error")) {
          conditionMessage(written)
        } else {
          "The Project source results could not be saved."
        }
      )
    } else {
      project <- isolate(builder_project())
      project$manifest <- written$manifest
      project$path <- written$path
      builder_project(project)
      current_worker <- isolate(worker())
      live_entries <- isolate(sets())
      committed <- tryCatch(
        builder_project_commit_source_entries(
          live_entries,
          written$manifest,
          results,
          project$root,
          session_root = current_worker$snapshot_root %||% NULL
        ),
        error = identity
      )
      if (
        !inherits(committed, "condition") &&
          !identical(committed$entries, live_entries)
      ) {
        builder_project_update_entries(committed$entries)
      }
    }
    builder_project_source_sync(list(
      status = if (failed) "failed" else "ready",
      completed = length(results),
      total = length(results),
      failed = as.integer(failed)
    ))
    save_waiter <- isolate(builder_project_source_save_waiter())
    if (is.list(save_waiter) && isTRUE(save_waiter$waiting)) {
      builder_project_source_save_waiter(NULL)
      if (failed) {
        builder_project_last_save_error(
          "One or more dataset sources could not be saved to the Project."
        )
      }
      finish_builder_project_save_request(
        failed == 0L,
        save_waiter$after,
        only_when_idle = TRUE
      )
    }
    if (length(isolate(builder_project_source_queue()))) {
      builder_project_start_source_sync()
    }
    invisible(failed == 0L)
  }
  if (!isTRUE(run_is_current)) {
    return(finish(NULL))
  }
  # Merge into the manifest current when the write runs: Saves, autosaves and
  # CRB registrations committed while the copy ran keep their changes, and
  # the copy is not discarded.
  start_builder_project_manifest_write(
    function(current) {
      builder_project_apply_source_results(
        current$manifest,
        results,
        current$root,
        only_pending = TRUE
      )
    },
    project,
    finish
  )
  invisible(TRUE)
}

request_builder_project_source_sync <- function(jobs) {
  jobs <- Filter(Negate(is.null), jobs %||% list())
  if (!length(jobs)) {
    return(invisible(FALSE))
  }
  source_run <- isolate(builder_project_source_run())
  running <- if (is.list(source_run) && !isTRUE(source_run$cancelling)) {
    source_run$jobs %||% list()
  } else {
    list()
  }
  # A job identical to the running copy waits for that copy instead.
  # The Builder's own session copies may be moved into the Project instead
  # of copied, when both are on the same volume.
  session_root <- isolate(worker())$snapshot_root %||% NULL
  jobs <- lapply(jobs, function(job) {
    job$session_owned <- builder_project_session_owned_source(
      job$source,
      session_root
    )
    job
  })
  queued <- builder_project_source_queue_add(
    isolate(builder_project_source_queue()),
    jobs,
    running
  )
  builder_project_source_queue(queued)
  if (!length(queued)) {
    return(invisible(TRUE))
  }
  if (!is.null(isolate(builder_project_source_process()))) {
    return(invisible(TRUE))
  }
  invisible(isTRUE(builder_project_start_source_sync()))
}

builder_project_manifest_context <- function(last_ui = NULL) {
  list(
    checked = isolate(checked_dataset_ids()),
    artifacts = isolate(builder_project_artifacts()),
    skipped_ids = isolate(builder_project_skipped_ids()),
    configuration = builder_project_configuration(
      review_options = isolate(review_options()),
      build_mode = isTRUE(isolate(build_mode())),
      auth_enabled = isTRUE(isolate(auth_enabled())),
      initial_dataset = isolate(build_initial_dataset())
    ),
    last_ui = last_ui %||%
      list(
        stage = isolate(selected_workflow_stage()),
        selected_dataset = isolate(current()),
        spatial = builder_project_last_ui_spatial(
          isolate(alignment_server$project_selection()),
          selected_dataset = isolate(current())
        )
      )
  )
}

restore_builder_project_preferences <- function(manifest) {
  configuration <- manifest$configuration %||% builder_project_configuration()
  saved_options <- configuration$review_options %||% NULL
  if (is.list(saved_options)) {
    restored <- try(
      do.call(builder_review_options, saved_options),
      silent = TRUE
    )
    if (!inherits(restored, "try-error")) {
      review_options(restored)
      review_validation(list(ok = TRUE, error = NULL))
    }
  }
  make_app <- isTRUE(configuration$build_mode)
  build_mode(make_app)
  build_initial_dataset(configuration$initial_dataset %||% NULL)
  auth_accounts(builder_auth_empty_accounts())
  login_requested <- make_app && isTRUE(configuration$auth_enabled)
  auth_enabled(login_requested)
  auth_validation(
    if (login_requested) {
      list(
        ok = FALSE,
        error = "Add login accounts again before building this App."
      )
    } else {
      list(ok = TRUE, error = NULL)
    }
  )
  invisible(TRUE)
}

restore_builder_project_last_ui <- function(manifest) {
  ids <- vapply(isolate(sets()), `[[`, character(1), "id")
  target <- builder_project_last_ui_target(
    manifest$last_ui %||% list(),
    available_ids = ids
  )
  if (!is.null(target$selected_dataset)) {
    current(target$selected_dataset)
  }
  alignment_server$restore_project_selection(target$spatial)
  saved_initial <- isolate(build_initial_dataset())
  if (!is.null(saved_initial) && !saved_initial %in% ids) {
    build_initial_dataset(NULL)
  }
  navigate_workflow_stage(target$stage)
  invisible(target)
}

restore_builder_project_last_ui_after_flush <- function(last_ui) {
  session$onFlushed(
    function() {
      shiny::withReactiveDomain(builder_lifecycle_session, {
        shiny::isolate(
          restore_builder_project_last_ui(list(last_ui = last_ui))
        )
      })
    },
    once = TRUE
  )
}

## Manifest writes run one at a time. Each starts from the Project as it is
## when its turn comes, so a write queued behind another (autosave, Save, CRB
## registration, source results) commits on top of it instead of failing the
## revision check. `manifest` is the Project manifest the caller started from
## (replaced by the current one), or a function deriving the manifest to write
## from the current Project.
builder_project_manifest_writes <- new.env(parent = emptyenv())
builder_project_manifest_writes$active <- FALSE
builder_project_manifest_writes$queue <- list()

builder_project_manifest_write_pending <- function() {
  isTRUE(builder_project_manifest_writes$active) ||
    length(builder_project_manifest_writes$queue) > 0L
}

start_builder_project_manifest_write <- function(
  manifest,
  project,
  complete,
  on_closed = NULL,
  entries = NULL,
  context = NULL,
  artifact_record = NULL
) {
  queue <- builder_project_manifest_writes$queue
  queue[[length(queue) + 1L]] <- list(
    manifest = manifest,
    project = project,
    complete = complete,
    on_closed = on_closed,
    entries = entries,
    context = context,
    artifact_record = artifact_record
  )
  builder_project_manifest_writes$queue <- queue
  dispatch_builder_project_manifest_write()
  invisible(TRUE)
}

dispatch_builder_project_manifest_write <- function() {
  state <- builder_project_manifest_writes
  if (isTRUE(state$active) || !length(state$queue)) {
    return(invisible(FALSE))
  }
  request <- state$queue[[1L]]
  state$queue <- state$queue[-1L]
  state$active <- TRUE
  deliver <- function(written) {
    on.exit(
      {
        state$active <- FALSE
        dispatch_builder_project_manifest_write()
      },
      add = TRUE
    )
    if (!builder_session_closed()) {
      if (
        !inherits(written, "condition") &&
          length(written$artifact_closure_cache)
      ) {
        list2env(
          written$artifact_closure_cache,
          envir = .builder_project_artifact_closure_cache
        )
      }
      shiny::withReactiveDomain(
        session,
        shiny::isolate(request$complete(written))
      )
    } else if (is.function(request$on_closed)) {
      request$on_closed()
    }
    invisible(TRUE)
  }
  deliver_later <- function(value) {
    later::later(function() deliver(value), delay = 0)
    invisible(TRUE)
  }
  current <- isolate(builder_project())
  if (
    !is.list(current) ||
      !identical(current$root, request$project$root) ||
      !identical(
        current$manifest$project$id %||% NULL,
        request$project$manifest$project$id %||% NULL
      )
  ) {
    return(deliver_later(simpleError(
      "The Project changed before this save could run. Save again."
    )))
  }
  manifest <- if (is.function(request$manifest)) {
    tryCatch(request$manifest(current), error = identity)
  } else if (identical(request$manifest, request$project$manifest)) {
    current$manifest
  } else {
    request$manifest
  }
  if (inherits(manifest, "condition")) {
    return(deliver_later(manifest))
  }
  process <- tryCatch(
    builder_project_write_process(
      manifest,
      current$root,
      current$manifest$project$revision,
      builder_project_worker_runtime_files(),
      builder_source_utf8,
      full_runtime_files = builder_project_worker_runtime_files(full = TRUE),
      worker = builder_project_writer,
      entries = request$entries,
      context = request$context,
      artifact_record = request$artifact_record,
      configuration_cache = if (is.null(request$entries)) {
        list()
      } else {
        as.list(builder_configuration_identity_cache)
      }
    ),
    error = identity
  )
  if (inherits(process, "condition")) {
    return(deliver_later(process))
  }
  poll <- function() {
    if (isTRUE(tryCatch(process$is_alive(), error = function(e) FALSE))) {
      later::later(poll, delay = 0.05)
      return(invisible(TRUE))
    }
    deliver(tryCatch(process$get_result(), error = identity))
  }
  later::later(poll, delay = 0.05)
  invisible(TRUE)
}

save_builder_project_state <- function(
  show_actions = FALSE,
  materialize = TRUE,
  notify = TRUE,
  manage_lifecycle = TRUE,
  last_ui = NULL,
  table_asset_results = NULL,
  on_complete = NULL
) {
  project <- isolate(builder_project())
  if (is.null(project)) {
    if (isTRUE(notify)) {
      builder_notify(
        "info",
        "Choose a project folder first: select Save.",
        target = "status",
        duration = 6
      )
    }
    return(invisible(FALSE))
  }
  if (
    isTRUE(manage_lifecycle) &&
      isolate(builder_project_operation_phase()) %in% c("saving", "autosaving")
  ) {
    return(invisible(FALSE))
  }
  if (isTRUE(manage_lifecycle)) {
    builder_project_operation_phase("saving")
  }
  save_failed <- function(message) {
    builder_project_last_save_error(message)
    if (isTRUE(manage_lifecycle)) {
      builder_project_operation_phase(
        if (grepl("updated by another Builder window", message, fixed = TRUE)) {
          "conflict"
        } else {
          "save_failed"
        }
      )
    }
    invisible(FALSE)
  }
  entries <- isolate(sets())
  if (isTRUE(materialize) && length(entries)) {
    materialized <- alignment_server$materialize_coordinate_drafts(
      notify = FALSE,
      require_settled = TRUE
    )
    if (!isTRUE(materialized$ok)) {
      if (isTRUE(notify)) {
        builder_notify(
          "error",
          materialized$error %||%
            "Current spatial settings could not be saved."
        )
      }
      return(save_failed(
        materialized$error %||% "Current spatial settings could not be saved."
      ))
    }
    entries <- materialized$all_entries
  }
  if (!is.null(table_asset_results)) {
    entries <- builder_project_apply_attachment_results(
      entries,
      table_asset_results,
      project$root
    )
  }
  live_base <- isolate(sets())
  context <- builder_project_manifest_context(last_ui)
  built <- NULL
  apply_written <- function(written) {
    if (inherits(written, "condition")) {
      if (isTRUE(notify)) {
        builder_notify("error", conditionMessage(written))
      }
      return(save_failed(conditionMessage(written)))
    }
    if (!is.null(written$built)) {
      built <- written$built
    }
    # Compare with what the workspace holds, not with `entries`: those already
    # carry the staged attachment results, so comparing with them never
    # adopted the Project copies and every later Save staged them again.
    entries_changed <- !identical(live_base, built$entries)
    if (length(written$configuration_cache)) {
      list2env(
        written$configuration_cache,
        envir = builder_configuration_identity_cache
      )
    }
    live_entries <- isolate(sets())
    if (!identical(live_entries, live_base)) {
      # Background saves leave the workspace editable; keep edits made while
      # the manifest was being written instead of restoring the old entries.
      merged <- builder_project_merge_saved_entries(
        live_base,
        built$entries,
        live_entries
      )
      if (!identical(merged, live_entries)) {
        builder_project_update_entries(merged)
      }
    } else if (entries_changed) {
      builder_project_update_entries(built$entries)
    }
    project$manifest <- written$manifest
    project$path <- written$path
    project$name <- written$manifest$project$name
    builder_project(project)
    source_sync_started <- if (length(built$jobs)) {
      request_builder_project_source_sync(built$jobs)
    } else {
      TRUE
    }
    if (length(built$jobs) && !isTRUE(source_sync_started)) {
      source_sync <- isolate(builder_project_source_sync())
      return(save_failed(
        source_sync$error %||%
          paste(
            "Project settings were saved, but source files could not be saved.",
            "Use Save project to retry."
          )
      ))
    }
    builder_project_last_save_error(NULL)
    if (isTRUE(manage_lifecycle)) {
      builder_project_operation_phase("idle")
    }
    if (isTRUE(show_actions)) {
      records <- builder_project_record_map(written$manifest)
      checked <- sum(vapply(
        records,
        function(record) isTRUE(record$configuration$checked),
        logical(1)
      ))
      checked_entries <- Filter(
        function(entry) entry$id %in% isolate(checked_dataset_ids()),
        built$entries
      )
      required_entries <- builder_project_entries_requiring_crb(
        checked_entries,
        isolate(builder_project_artifacts()),
        project$root,
        builder_configuration_identity_cache,
        verification_cache = builder_checkpoint_verification_cache
      )
      reusable <- length(checked_entries) - length(required_entries)
      source_sync <- isolate(builder_project_source_sync())
      session$sendCustomMessage(
        "builder_project_save_result",
        list(
          checked = as.integer(checked),
          reusable = as.integer(reusable),
          source_syncing = length(built$jobs) > 0L ||
            identical(
              source_sync$status,
              "syncing"
            ),
          completed = as.integer(source_sync$completed %||% 0L),
          total = as.integer(max(
            source_sync$total %||% 0L,
            length(built$jobs)
          ))
        )
      )
    } else if (isTRUE(notify)) {
      builder_notify(
        "success",
        if (length(built$jobs)) {
          "Settings saved. Saving source files…"
        } else {
          "Project saved."
        },
        target = "status",
        duration = 4
      )
    }
    invisible(TRUE)
  }
  if (is.function(on_complete)) {
    start_builder_project_manifest_write(
      project$manifest,
      project,
      function(written) {
        ok <- tryCatch(apply_written(written), error = function(error) {
          builder_project_last_save_error(conditionMessage(error))
          FALSE
        })
        on_complete(isTRUE(ok))
      },
      entries = entries,
      context = context
    )
    return(invisible("pending"))
  }
  built <- tryCatch(
    builder_project_build_manifest(
      entries,
      project,
      context,
      builder_configuration_identity_cache
    ),
    error = function(error) error
  )
  if (inherits(built, "condition")) {
    if (isTRUE(notify)) {
      builder_notify("error", conditionMessage(built))
    }
    return(save_failed(conditionMessage(built)))
  }
  if (is.list(last_ui)) {
    built$manifest$last_ui <- last_ui
  }
  written <- tryCatch(
    builder_project_write(
      built$manifest,
      project$root,
      expected_revision = project$manifest$project$revision
    ),
    error = function(error) error
  )
  apply_written(written)
}

finish_builder_project_save_request <- function(
  ok,
  after = NULL,
  only_when_idle = FALSE
) {
  if (
    isTRUE(only_when_idle) &&
      !identical(isolate(builder_project_operation_phase()), "idle")
  ) {
    # The save already released the workspace and another project operation
    # now owns the phase; only report the outcome to the caller.
    NULL
  } else if (isTRUE(ok)) {
    builder_project_operation_phase("idle")
  } else {
    message <- isolate(builder_project_last_save_error()) %||% ""
    builder_project_operation_phase(
      if (grepl("updated by another Builder window", message, fixed = TRUE)) {
        "conflict"
      } else {
        "save_failed"
      }
    )
  }
  if (is.function(after)) {
    after(isTRUE(ok))
  }
  invisible(isTRUE(ok))
}

complete_builder_project_save_request <- function(
  show_actions = FALSE,
  materialize = TRUE,
  notify = TRUE,
  after = NULL,
  table_asset_results = NULL
) {
  if (isTRUE(show_actions)) {
    previous_waiter <- isolate(builder_project_source_save_waiter())
    previous_after <- if (is.list(previous_waiter)) {
      previous_waiter$after
    } else {
      NULL
    }
    if (is.function(previous_after)) {
      # An earlier Save is still waiting for its source copy; keep its
      # follow-up (CRB queueing) instead of dropping it.
      current_after <- after
      after <- function(ok) {
        previous_after(ok)
        if (is.function(current_after)) {
          current_after(ok)
        }
        invisible(isTRUE(ok))
      }
    }
    builder_project_source_save_waiter(list(
      waiting = FALSE,
      after = after
    ))
  }
  settle <- function(ok) {
    source_waiter <- isolate(builder_project_source_save_waiter())
    source_progress <- isolate(builder_project_source_sync())
    if (
      isTRUE(ok) &&
        isTRUE(show_actions) &&
        identical(source_progress$status %||% NULL, "syncing")
    ) {
      # The manifest is committed. Release the workspace now and let the
      # source copy report through source_syncing; only the follow-up waits.
      source_waiter$waiting <- TRUE
      builder_project_source_save_waiter(source_waiter)
      builder_project_operation_phase("idle")
      return(invisible(TRUE))
    }
    if (isTRUE(show_actions)) {
      builder_project_source_save_waiter(NULL)
    }
    finish_builder_project_save_request(ok, after)
  }
  ok <- tryCatch(
    save_builder_project_state(
      show_actions = show_actions,
      materialize = materialize,
      notify = notify,
      manage_lifecycle = FALSE,
      table_asset_results = table_asset_results,
      on_complete = settle
    ),
    error = function(error) {
      builder_project_last_save_error(conditionMessage(error))
      if (isTRUE(notify)) {
        builder_notify("error", conditionMessage(error))
      }
      FALSE
    }
  )
  if (identical(ok, "pending")) {
    return(invisible(TRUE))
  }
  settle(ok)
}

fail_builder_project_table_asset_save <- function(
  message,
  notify = TRUE,
  after = NULL,
  detail = NULL
) {
  builder_project_last_save_error(message)
  if (isTRUE(notify)) {
    builder_notify(
      "error",
      tagList(
        message,
        if (builder_has_text(detail)) {
          tags$details(tags$summary("Details"), tags$p(detail))
        }
      ),
      id = "builder-project-save-error"
    )
  }
  finish_builder_project_save_request(FALSE, after)
}

invalidate_builder_project_table_asset_save <- function(kill = TRUE) {
  process <- isolate(builder_project_table_asset_process())
  builder_project_table_asset_generation(
    as.double(isolate(builder_project_table_asset_generation())) + 1
  )
  builder_project_table_asset_process(NULL)
  if (isTRUE(kill) && !is.null(process)) {
    try(process$kill(), silent = TRUE)
  }
  invisible(TRUE)
}

start_builder_project_table_asset_save <- function(
  jobs,
  signature,
  show_actions,
  materialize,
  notify,
  after,
  background = FALSE
) {
  generation <- as.double(isolate(builder_project_table_asset_generation())) + 1
  builder_project_table_asset_generation(generation)
  process <- tryCatch(
    builder_project_worker_task_call(
      builder_project_writer,
      function(runtime_for, args) {
        runtime_for(FALSE)$builder_project_stage_attachment_jobs(
          args$jobs,
          args$root
        )
      },
      list(jobs = jobs, root = signature$root)
    ),
    error = identity
  )
  if (inherits(process, "condition")) {
    return(fail_builder_project_table_asset_save(
      "Project attachments could not be saved.",
      notify,
      after,
      detail = conditionMessage(process)
    ))
  }
  builder_project_table_asset_process(process)
  poll <- function() {
    if (builder_session_closed()) {
      invalidate_builder_project_table_asset_save(kill = TRUE)
      return(invisible(FALSE))
    }
    if (
      !identical(
        generation,
        as.double(isolate(builder_project_table_asset_generation()))
      )
    ) {
      return(invisible(FALSE))
    }
    alive <- tryCatch(process$is_alive(), error = identity)
    if (inherits(alive, "condition")) {
      invalidate_builder_project_table_asset_save(kill = FALSE)
      return(fail_builder_project_table_asset_save(
        "Project attachments could not be saved.",
        notify,
        after,
        detail = conditionMessage(alive)
      ))
    }
    if (isTRUE(alive)) {
      later::later(
        function() shiny::withReactiveDomain(session, shiny::isolate(poll())),
        delay = 0.1
      )
      return(invisible(TRUE))
    }
    results <- tryCatch(process$get_result(), error = identity)
    builder_project_table_asset_process(NULL)
    if (
      inherits(results, "condition") ||
        !builder_project_attachment_results_match(jobs, results)
    ) {
      return(fail_builder_project_table_asset_save(
        "Project attachments could not be saved.",
        notify,
        after,
        detail = if (inherits(results, "condition")) {
          conditionMessage(results)
        } else {
          "The returned files did not match the save request. Retry Save."
        }
      ))
    }
    current_project <- isolate(builder_project())
    current_entries <- isolate(sets())
    current_signature <- tryCatch(
      builder_project_table_save_signature(current_project, current_entries),
      error = identity
    )
    current_jobs <- tryCatch(
      builder_project_attachment_jobs(current_entries, current_project$root),
      error = identity
    )
    if (
      inherits(current_signature, "condition") ||
        inherits(current_jobs, "condition") ||
        !identical(signature, current_signature) ||
        !identical(jobs, current_jobs)
    ) {
      return(fail_builder_project_table_asset_save(
        "Project settings changed while attachments were saving. Save again.",
        notify,
        after
      ))
    }
    if (
      isTRUE(background) &&
        identical(isolate(builder_project_operation_phase()), "saving")
    ) {
      # The attachments are staged and checked against the live settings;
      # the manifest write merges later edits, so unlock the workspace again.
      builder_project_operation_phase("autosaving")
    }
    complete_builder_project_save_request(
      show_actions = show_actions,
      materialize = materialize,
      notify = notify,
      after = after,
      table_asset_results = results
    )
  }
  later::later(
    function() shiny::withReactiveDomain(session, shiny::isolate(poll())),
    delay = 0
  )
  invisible(TRUE)
}

start_builder_project_save_request <- function(
  show_actions = FALSE,
  materialize = TRUE,
  notify = TRUE,
  after = NULL,
  background = FALSE
) {
  builder_notify_clear("builder-project-save-error")
  session$onFlushed(
    function() {
      shiny::isolate({
        project <- builder_project()
        entries <- sets()
        prepared <- tryCatch(
          {
            jobs <- builder_project_attachment_jobs(entries, project$root)
            list(
              jobs = jobs,
              signature = if (length(jobs)) {
                builder_project_table_save_signature(project, entries)
              } else {
                NULL
              }
            )
          },
          error = identity
        )
        if (inherits(prepared, "condition")) {
          fail_builder_project_table_asset_save(
            conditionMessage(prepared),
            notify,
            after
          )
        } else if (length(prepared$jobs)) {
          if (identical(builder_project_operation_phase(), "autosaving")) {
            # Attachment staging rejects concurrent edits, so the workspace is
            # locked while new attachments are staged (usually well under a
            # second); the manifest write afterwards runs in the background.
            builder_project_operation_phase("saving")
          }
          start_builder_project_table_asset_save(
            prepared$jobs,
            prepared$signature,
            show_actions,
            materialize,
            notify,
            after,
            background = isTRUE(background)
          )
        } else {
          complete_builder_project_save_request(
            show_actions,
            materialize,
            notify,
            after
          )
        }
      })
    },
    once = TRUE
  )
  invisible(TRUE)
}

## Saves write the manifest in the background ("autosaving"): the workspace
## stays editable, and edits made meanwhile are merged back and leave the
## Project dirty (builder_project_merge_saved_entries). Only other manifest
## writers (Save, Open, Build) wait. `background = FALSE` locks the workspace
## for the whole Save instead.
request_builder_project_save <- function(
  show_actions = FALSE,
  materialize = TRUE,
  notify = TRUE,
  after = NULL,
  background = TRUE
) {
  if (
    isolate(builder_project_operation_phase()) %in% c("saving", "autosaving")
  ) {
    return(invisible(FALSE))
  }
  # Decide the lock now, not when the request runs after this flush: a Save
  # that stages new attachments starts locked, so an edit made in between is
  # not accepted by the browser and then refused by the locked server.
  stages_attachments <- isTRUE(background) &&
    isTRUE(tryCatch(
      {
        project <- isolate(builder_project())
        is.list(project) &&
          length(builder_project_attachment_jobs(
            isolate(sets()),
            project$root
          )) >
            0L
      },
      error = function(error) TRUE
    ))
  builder_project_operation_phase(
    if (isTRUE(background) && !stages_attachments) "autosaving" else "saving"
  )
  completion <- after
  if (isTRUE(show_actions)) {
    after <- function(ok) {
      if (isTRUE(ok)) {
        # Queuing CRBs freezes a plan and checks free disk space on the main
        # thread (over a second on Windows). Let the flush that ends "Saving
        # project…" reach the browser first.
        later::later(
          function() {
            if (!builder_session_closed()) {
              shiny::withReactiveDomain(builder_lifecycle_session, {
                queue_builder_project_crbs(isolate(checked_dataset_ids()))
              })
            }
          },
          delay = 0.05
        )
      }
      if (is.function(completion)) {
        completion(isTRUE(ok))
      }
      invisible(isTRUE(ok))
    }
  }
  start_builder_project_save_request(
    show_actions = show_actions,
    materialize = materialize,
    notify = notify,
    after = after,
    background = background
  )
}

output$project_status <- renderUI({
  project <- builder_project()
  builder_project_status_ui(
    if (is.null(project)) NULL else list(name = project$name),
    phase = builder_project_phase(),
    source_sync = builder_project_source_sync(),
    crb = builder_project_crb_queue_status()
  )
})

observe({
  project <- builder_project()
  entries <- sets()
  if (is.null(project) || !length(entries)) {
    return()
  }
  if (!isTRUE(builder_capabilities()$save_project)) {
    return()
  }
  current_ids <- vapply(entries, `[[`, character(1), "id")
  saved_ids <- names(builder_project_record_map(project$manifest))
  missing_ids <- sort(setdiff(current_ids, saved_ids))
  if (!length(missing_ids)) {
    builder_project_auto_save_signature(NULL)
    return()
  }
  signature <- paste(missing_ids, collapse = "\r")
  if (identical(signature, isolate(builder_project_auto_save_signature()))) {
    return()
  }
  builder_project_auto_save_signature(signature)
  request_builder_project_save(
    show_actions = FALSE,
    materialize = FALSE,
    notify = FALSE,
    background = TRUE,
    after = function(ok) {
      if (!isTRUE(ok)) {
        builder_notify("error", "Dataset not saved. Select Save to retry.")
      }
    }
  )
})

show_builder_project_folder_error <- function(message, type = "error") {
  builder_notify(
    type,
    message,
    target = list(
      field = c(
        "builder_project_server_folder",
        "builder_project_server_manifest"
      )
    )
  )
  invisible(FALSE)
}

show_builder_project_server_path <- function(
  kind = c("create", "open"),
  detail = NULL
) {
  kind <- match.arg(kind)
  shiny::showModal(builder_server_path_dialog(
    title = if (identical(kind, "create")) {
      "Choose project folder"
    } else {
      "Open project"
    },
    input_id = if (identical(kind, "create")) {
      "builder_project_server_folder"
    } else {
      "builder_project_server_manifest"
    },
    action_id = if (identical(kind, "create")) {
      "use_builder_project_server_folder"
    } else {
      "use_builder_project_server_manifest"
    },
    label = if (identical(kind, "create")) {
      "Project folder"
    } else {
      "Project file (builder-project.json)"
    },
    action_label = if (identical(kind, "create")) {
      "Continue"
    } else {
      "Open project"
    },
    detail = detail,
    note = if (identical(kind, "create")) builder_project_save_location_note()
  ))
  invisible(TRUE)
}

builder_project_server_path <- function(
  value,
  type,
  roots = builder_server_path_roots()
) {
  path <- tryCatch(
    builder_server_path_resolve(value, type = type, roots = roots),
    error = identity
  )
  if (inherits(path, "condition")) {
    show_builder_project_folder_error(conditionMessage(path))
    return(NULL)
  }
  path
}

builder_project_folder_available <- function(folder) {
  if (inherits(folder, "condition")) {
    return(show_builder_project_folder_error(conditionMessage(folder)))
  }
  conflicts <- folder$managed_conflicts %||% character()
  if (length(conflicts)) {
    return(show_builder_project_folder_error(paste0(
      "This folder already contains Builder-managed names: ",
      paste(conflicts, collapse = ", "),
      ". Choose another folder."
    )))
  }
  TRUE
}

create_builder_project_in_folder <- function(folder, existing = NULL) {
  manifest_path <- builder_project_manifest_path(folder$root)
  manifest <- builder_project_new_manifest(folder$root)
  if (is.list(existing) && is.list(existing$project)) {
    manifest$project$id <- existing$project$id
    manifest$project$name <- existing$project$name
    manifest$project$revision <- existing$project$revision
    manifest$project$created_at <- existing$project$created_at
  }
  builder_project_pending_folder(NULL)
  reset_builder_project_crb_pipeline()
  builder_project(list(
    root = folder$root,
    path = manifest_path,
    name = manifest$project$name,
    manifest = manifest
  ))
  builder_project_skipped_ids(character())
  shiny::removeModal()
  request_builder_project_save(show_actions = TRUE, materialize = TRUE)
  invisible(TRUE)
}

select_builder_project_folder <- function(path) {
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  if (!isTRUE(builder_project_folder_available(folder))) {
    return(invisible(FALSE))
  }
  if (identical(folder$kind, "project")) {
    builder_project_pending_folder(path)
    shiny::showModal(builder_project_existing_folder_dialog(path))
    return(invisible(TRUE))
  }
  if (identical(folder$kind, "nonempty")) {
    builder_project_pending_folder(path)
    shiny::showModal(builder_project_nonempty_folder_dialog(path))
    return(invisible(TRUE))
  }
  create_builder_project_in_folder(folder)
}

request_builder_project_folder <- function() {
  builder_project_pending_folder(NULL)
  if (!builder_operation_allowed("create_project")) {
    return(invisible(FALSE))
  }
  # The picker overlay (Enter path · Cancel) sits below Shiny modals; a dialog
  # left open here would hide the manual-path fallback.
  shiny::removeModal()
  builder_project_picker_kind("create")
  builder_project_operation_phase("choosing")
  builder_schedule_native_picker(
    "project_directory",
    key = "project-create",
    on_result = function(choice) {
      shiny::isolate({
        builder_project_operation_phase("idle")
        if (!builder_operation_allowed("create_project")) {
          return(invisible(FALSE))
        }
        if (identical(choice$status, "cancelled")) {
          return(invisible(FALSE))
        }
        if (!identical(choice$status, "selected")) {
          show_builder_project_server_path("create", choice$error)
          return(invisible(FALSE))
        }
        select_builder_project_folder(choice$path)
      })
    },
    on_error = function(error) {
      builder_project_operation_phase("idle")
      show_builder_project_server_path("create", conditionMessage(error))
    }
  )
  invisible(TRUE)
}

observeEvent(input$use_builder_project_server_folder, {
  if (!builder_operation_allowed("create_project")) {
    return()
  }
  path <- builder_project_server_path(
    input$builder_project_server_folder,
    type = "directory"
  )
  if (!is.null(path)) {
    select_builder_project_folder(path)
  }
})

observeEvent(input$confirm_builder_project_folder, {
  if (!builder_operation_allowed("create_project")) {
    return()
  }
  path <- isolate(builder_project_pending_folder())
  if (is.null(path)) {
    return()
  }
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  builder_project_pending_folder(NULL)
  if (!isTRUE(builder_project_folder_available(folder))) {
    shiny::removeModal()
    return()
  }
  create_builder_project_in_folder(folder)
})

observeEvent(input$confirm_existing_builder_project_folder, {
  if (!builder_operation_allowed("create_project")) {
    return()
  }
  path <- isolate(builder_project_pending_folder())
  if (is.null(path)) {
    return()
  }
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  if (
    inherits(folder, "condition") ||
      !identical(folder$kind %||% NULL, "project")
  ) {
    builder_project_pending_folder(NULL)
    shiny::removeModal()
    show_builder_project_folder_error(
      if (inherits(folder, "condition")) {
        conditionMessage(folder)
      } else {
        "The selected folder is no longer a Builder project."
      }
    )
    return()
  }
  existing <- tryCatch(
    builder_project_read(builder_project_manifest_path(folder$root)),
    error = identity
  )
  if (inherits(existing, "condition")) {
    builder_project_pending_folder(NULL)
    shiny::removeModal()
    show_builder_project_folder_error(conditionMessage(existing))
    return()
  }
  create_builder_project_in_folder(folder, existing = existing)
})

observeEvent(input$choose_another_builder_project_folder, {
  builder_project_pending_folder(NULL)
  shiny::removeModal()
  request_builder_project_folder()
})

observeEvent(input$cancel_builder_project_folder, {
  builder_project_pending_folder(NULL)
  shiny::removeModal()
})

session$onSessionEnded(function() {
  builder_project_pending_folder(NULL)
  invalidate_builder_project_source_sync(kill = TRUE)
  invalidate_builder_project_table_asset_save(kill = TRUE)
  invalidate_builder_project_open(kill = TRUE)
  crb_task <- isolate(builder_project_crb_task())
  stopped <- is.null(crb_task) ||
    crb_task$state %in% c("completed", "failed", "cancelled", "delivered") ||
    isTRUE(tryCatch(builder_async_cancel(crb_task), error = function(error) {
      FALSE
    }))
  if (isTRUE(stopped)) {
    jobs <- isolate(builder_project_crb_jobs())
    for (job in jobs) {
      output <- job$output %||% NULL
      if (.builder_project_text(output)) {
        try(
          builder_project_cleanup_checkpoint(output, dirname(dirname(output))),
          silent = TRUE
        )
      }
    }
  }
  builder_project_crb_jobs(list())
  builder_project_crb_task(NULL)
  # A running manifest transaction finishes before the writer ends; copies
  # were cancelled above and need no grace.
  builder_project_worker_close(builder_project_copier, kill = TRUE)
  builder_project_worker_close(builder_project_writer)
})

## An explicit Save with nothing to write or start: no unsaved change, no
## queued copy, write, attachment or CRB, and every checked dataset already
## has its CRB. Such a Save reports "Saved" without a manifest transaction.
builder_project_save_is_noop <- function() {
  project <- isolate(builder_project())
  if (
    is.null(project) ||
      !identical(isolate(builder_current_project_phase()), "clean") ||
      isTRUE(isolate(builder_spatial_drafts_dirty())) ||
      builder_project_manifest_write_pending() ||
      length(isolate(builder_project_source_queue())) > 0L ||
      !is.null(isolate(builder_project_source_process())) ||
      !is.null(isolate(builder_project_table_asset_process())) ||
      !is.null(isolate(builder_project_crb_task())) ||
      length(isolate(builder_project_crb_jobs())) > 0L ||
      isolate(builder_project_source_sync())$status %in%
        c("syncing", "cancelling", "failed")
  ) {
    return(FALSE)
  }
  checked <- isolate(checked_dataset_ids())
  states <- isolate(dataset_checkpoint_states())
  if (
    any(vapply(
      states[intersect(names(states), checked)],
      function(state) !isTRUE(state$crb_ready),
      logical(1)
    ))
  ) {
    return(FALSE)
  }
  entries <- isolate(sets())
  attachment_jobs <- tryCatch(
    builder_project_attachment_jobs(entries, project$root),
    error = function(error) NULL
  )
  if (is.null(attachment_jobs)) {
    return(FALSE)
  }
  isTRUE(tryCatch(
    builder_project_save_unchanged(
      project$manifest,
      entries,
      builder_project_manifest_context(),
      attachment_jobs
    ),
    error = function(error) FALSE
  ))
}

report_builder_project_unchanged_save <- function() {
  checked <- intersect(
    isolate(checked_dataset_ids()),
    vapply(isolate(sets()), `[[`, character(1), "id")
  )
  session$sendCustomMessage(
    "builder_project_save_result",
    list(
      checked = length(checked),
      reusable = length(checked),
      source_syncing = FALSE,
      completed = 0L,
      total = 0L
    )
  )
  invisible(TRUE)
}

observeEvent(input$save_builder_project, {
  if (is.null(isolate(builder_project()))) {
    if (!builder_operation_allowed("create_project")) {
      return()
    }
    # The first Save goes straight to the folder picker; the overlay and the
    # manual-path dialog say what the folder will hold.
    request_builder_project_folder()
  } else {
    if (!builder_operation_allowed("save_project")) {
      return()
    }
    if (builder_project_save_is_noop()) {
      builder_notify_clear("builder-project-save-error")
      return(report_builder_project_unchanged_save())
    }
    request_builder_project_save(show_actions = TRUE, materialize = TRUE)
  }
})

request_builder_project_open <- function(confirmed = FALSE) {
  if (!builder_operation_allowed("open_project")) {
    return(invisible(FALSE))
  }
  if (!isTRUE(confirmed) && builder_project_open_needs_confirmation()) {
    # Ask before the picker, so choosing a project and its datasets is not
    # followed by a question that throws those choices away.
    builder_project_open_actions(NULL)
    builder_project_open_discard_confirmed(FALSE)
    shiny::showModal(builder_project_open_discard_dialog(), session = session)
    return(invisible(FALSE))
  }
  builder_project_open_discard_confirmed(isTRUE(confirmed))
  previous_phase <- isolate(builder_project_operation_phase())
  # Keep the picker overlay and its manual-path fallback visible.
  shiny::removeModal()
  builder_project_picker_kind("open")
  builder_project_operation_phase("choosing")
  builder_schedule_native_picker(
    "project_manifest",
    key = "project-open",
    on_result = function(choice) {
      shiny::isolate({
        builder_project_operation_phase(previous_phase)
        if (!builder_operation_allowed("open_project")) {
          return()
        }
        if (identical(choice$status, "cancelled")) {
          builder_project_open_discard_confirmed(FALSE)
          return()
        }
        if (!identical(choice$status, "selected")) {
          show_builder_project_server_path("open", choice$error)
          return()
        }
        builder_project_open_previous_phase(previous_phase)
        builder_project_start_open(choice$path)
      })
    },
    on_error = function(error) {
      builder_project_operation_phase(previous_phase)
      show_builder_project_server_path("open", conditionMessage(error))
    }
  )
  invisible(TRUE)
}

observeEvent(input$open_builder_project, {
  request_builder_project_open()
})

observeEvent(input$use_builder_project_server_manifest, {
  if (!builder_operation_allowed("open_project")) {
    return()
  }
  roots <- tryCatch(builder_server_path_roots(), error = identity)
  if (inherits(roots, "condition")) {
    show_builder_project_folder_error(conditionMessage(roots))
    return()
  }
  path <- builder_project_server_path(
    input$builder_project_server_manifest,
    type = "file",
    roots = roots
  )
  if (is.null(path)) {
    return()
  }
  previous_phase <- isolate(builder_project_operation_phase())
  shiny::removeModal()
  builder_project_open_previous_phase(previous_phase)
  builder_project_start_open(path, external_roots = roots)
})

observeEvent(
  input$cancel_builder_project_open,
  {
    progress <- isolate(builder_project_restore_progress())
    if (
      !identical(isolate(builder_project_operation_phase()), "opening") ||
        !identical(progress$mode %||% NULL, "opening")
    ) {
      return()
    }
    shiny::removeModal()
    builder_project_restore(NULL)
    builder_project_open_discard_confirmed(FALSE)
    invalidate_builder_project_open(kill = TRUE)
  },
  ignoreInit = TRUE
)

observeEvent(
  input$cancel_builder_project_restore_loads,
  {
    if (!identical(isolate(builder_project_operation_phase()), "restoring")) {
      return()
    }
    pending <- isolate(builder_project_pending_entries())
    cancelled <- vapply(names(pending), remove_pending_import, logical(1))
    for (id in names(pending)[cancelled]) {
      builder_project_commit_restored_entry(
        list(id = id),
        error = "Source restoration cancelled."
      )
    }
    builder_notify(
      "info",
      paste(
        "Restoration cancelled. Completed datasets are available.",
        "Reopen the project to retry."
      ),
      duration = 10
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input$cancel_builder_project_backup_recovery,
  {
    request <- isolate(builder_project_open_request())
    if (!identical(request$kind %||% NULL, "backup_recovery")) {
      return()
    }
    shiny::removeModal()
    builder_project_restore(NULL)
    builder_project_reset_open_state(
      request$generation,
      restore_previous = TRUE
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input$confirm_builder_project_backup_recovery,
  {
    request <- isolate(builder_project_open_request())
    if (
      !identical(request$kind %||% NULL, "backup_recovery") ||
        !builder_project_open_is_current(request$generation)
    ) {
      shiny::removeModal()
      return()
    }
    offer <- request$recovery_offer
    builder_project_open_request(NULL)
    shiny::removeModal()
    builder_project_start_open(
      request$path,
      external_roots = request$external_roots,
      recover_backup = TRUE,
      expected_primary_fingerprint = offer$primary_fingerprint,
      expected_backup_fingerprint = offer$backup_fingerprint
    )
  },
  ignoreInit = TRUE
)

observeEvent(input$choose_saved_project_datasets, {
  if (!builder_operation_allowed("edit_dataset")) {
    return()
  }
  project <- isolate(builder_project())
  if (
    is.null(project) ||
      length(isolate(sets())) ||
      length(isolate(imports()$entries %||% list()))
  ) {
    return()
  }
  previous_phase <- isolate(builder_project_operation_phase())
  builder_project_open_previous_phase(previous_phase)
  builder_project_start_open(project$path)
})

observeEvent(
  input$cancel_builder_project_restore,
  {
    builder_project_restore(NULL)
    builder_project_open_actions(NULL)
    builder_project_open_wait(NULL)
    builder_project_open_discard_confirmed(FALSE)
    shiny::removeModal()
  },
  ignoreInit = TRUE
)

observeEvent(
  input$continue_builder_project_legacy_qs,
  {
    request <- isolate(builder_project_open_request())
    if (
      !identical(request$kind %||% NULL, "legacy_qs") ||
        !builder_project_open_is_current(request$generation) ||
        !is.list(request$opened)
    ) {
      shiny::removeModal()
      return()
    }
    builder_project_open_request(NULL)
    builder_project_finish_open(request$generation, request$opened)
  },
  ignoreInit = TRUE
)

start_builder_project_record_load <- function(record, root) {
  if (!isTRUE(isolate(worker_available()))) {
    stop("The background worker is still starting.", call. = FALSE)
  }
  source <- record$source %||% list()
  started <- if (identical(source$kind, "example")) {
    start_load(
      "example",
      source$example,
      record$name,
      dataset_id = record$id
    )
  } else {
    kind <- source$kind %||% "managed"
    source_state <- record$runtime_restore_status$stat_signature[[
      source$path
    ]] %||%
      NULL
    path <- source_state$path %||%
      builder_project_resolve_path(source$path, root, kind)
    start_load(
      "file",
      path,
      record$name,
      file_meta = list(
        name = source$filename %||% basename(path),
        type = "application/octet-stream",
        size = as.double(source_state$bytes %||% NA_real_)
      ),
      dataset_id = record$id,
      source_origin = source$origin %||% "upload",
      example_id = source$example %||% NULL,
      retained_path = if (identical(kind, "managed")) path,
      expected_md5 = if (identical(record$runtime_restore_action, "reload")) {
        NULL
      } else {
        builder_project_recorded_fingerprint_md5(source$fingerprint)
      }
    )
  }
  if (!isTRUE(started)) {
    reason <- isolate(add_error())
    stop(
      if (builder_has_text(reason %||% "")) {
        reason
      } else {
        "The source could not be queued for the background worker."
      },
      call. = FALSE
    )
  }
  invisible(TRUE)
}

builder_project_mark_restore_skipped <- function(ids) {
  ids <- unique(as.character(ids %||% character()))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) {
    return(invisible(FALSE))
  }
  builder_project_skipped_ids(unique(c(
    isolate(builder_project_skipped_ids()),
    ids
  )))
  invisible(TRUE)
}

finish_builder_project_restore <- function() {
  if (length(isolate(builder_project_pending_entries()))) {
    return(invisible(FALSE))
  }
  project <- isolate(builder_project())
  last_ui <- project$manifest$last_ui %||% list()
  ## Opening a Project is read-only. An automatic save here synchronously
  ## rechecks every source and sidecar (including skipped datasets), blocking
  ## the UI after the selected imports are already finished. Keep any migrated
  ## settings dirty until an explicit Save; that path retains full validation.
  builder_project_restore_progress(list(
    mode = "idle",
    total = 0L,
    remaining = 0L
  ))
  builder_project_operation_phase("idle")
  if (!is.null(project)) {
    restore_builder_project_last_ui_after_flush(last_ui)
  }
  invisible(TRUE)
}

builder_project_open_worker_ready <- function(need_worker = TRUE) {
  current_protocol <- isolate(protocol())
  protocol_ready <- is.null(current_protocol) ||
    isTRUE(tryCatch(
      builder_protocol_is_quiescent(current_protocol),
      error = function(error) FALSE
    ))
  worker_ready <- !isTRUE(need_worker) ||
    (isTRUE(isolate(worker_available())) &&
      !is.null(isolate(worker())) &&
      !is.null(current_protocol))
  list(protocol = protocol_ready, worker = worker_ready)
}

## The dataset choices dialog again, with the choices the user made.
show_builder_project_restore_choices <- function(actions = NULL) {
  pending <- isolate(builder_project_restore())
  if (is.null(pending)) {
    shiny::removeModal()
    return(invisible(FALSE))
  }
  shiny::showModal(
    builder_project_restore_dialog(
      builder_project_manifest_with_restore_actions(pending$manifest, actions)
    ),
    session = session
  )
  invisible(TRUE)
}

## Waits for the background worker with a spinner instead of a toast asking
## the user to retry, then completes the open with the same choices.
wait_for_builder_project_open_worker <- function(actions, ready) {
  token <- as.double(isolate(builder_project_open_generation())) +
    stats::runif(1L)
  builder_project_open_wait(list(token = token, actions = actions))
  shiny::showModal(
    builder_project_open_wait_dialog(
      if (!isTRUE(ready$protocol)) {
        "The worker is finishing its current task. The project opens next."
      } else {
        "The worker is starting. The project opens as soon as it is ready."
      }
    ),
    session = session
  )
  check <- function() {
    if (builder_session_closed()) {
      return(invisible(FALSE))
    }
    shiny::withReactiveDomain(
      session,
      shiny::isolate({
        wait <- builder_project_open_wait()
        if (!is.list(wait) || !identical(wait$token, token)) {
          return(invisible(FALSE))
        }
        if (is.null(builder_project_restore())) {
          builder_project_open_wait(NULL)
          return(invisible(FALSE))
        }
        state <- builder_project_open_worker_ready()
        if (!isTRUE(state$protocol) || !isTRUE(state$worker)) {
          later::later(check, delay = 0.25)
          return(invisible(TRUE))
        }
        builder_project_open_wait(NULL)
        completed <- complete_builder_project_open(wait$actions)
        if (
          identical(completed, FALSE) &&
            !is.null(builder_project_restore()) &&
            is.null(builder_project_open_wait())
        ) {
          show_builder_project_restore_choices(wait$actions)
        }
        invisible(TRUE)
      })
    )
  }
  later::later(check, delay = 0.25)
  invisible("waiting")
}

complete_builder_project_open <- function(actions = NULL) {
  pending <- isolate(builder_project_restore())
  operation <- isolate(builder_project_operation_phase())
  if (
    is.null(pending) ||
      !operation %in% c("idle", "save_failed", "conflict")
  ) {
    return(invisible(FALSE))
  }
  manifest <- pending$manifest
  root <- pending$root
  builder_project_operation_phase("opening")
  current_manifest <- builder_project_file_metadata(pending$path)
  if (
    !is.list(pending$manifest_fingerprint) ||
      !builder_project_fingerprint_metadata_matches(
        pending$manifest_fingerprint,
        current_manifest
      ) ||
      !builder_project_fingerprint_stat_matches(
        pending$manifest_fingerprint,
        current_manifest
      )
  ) {
    builder_project_operation_phase(operation)
    builder_project_restore(NULL)
    builder_project_open_actions(NULL)
    shiny::removeModal()
    builder_notify(
      "error",
      paste(
        "The Project changed while dataset choices were open.",
        "Reopen it and review the current choices."
      )
    )
    return(invisible(FALSE))
  }
  records <- builder_project_record_map(manifest)
  if (is.null(actions)) {
    actions <- lapply(records, function(record) {
      input[[paste0("project_restore_", record$id)]] %||% "skip"
    })
  }
  prepared <- tryCatch(
    builder_project_prepare_open_selection(manifest, root, actions),
    error = identity
  )
  if (inherits(prepared, "condition")) {
    builder_project_operation_phase(operation)
    builder_notify(
      "error",
      paste0(
        "The selected Project datasets could not be opened: ",
        conditionMessage(prepared)
      )
    )
    return(invisible(FALSE))
  }
  reusable_entries <- prepared$reusable_entries
  pending_entries <- prepared$pending_entries
  artifacts <- prepared$artifacts
  marks <- prepared$marks
  skipped_ids <- prepared$skipped_ids
  next_store <- tryCatch(
    builder_state(
      datasets = reusable_entries,
      current_dataset = if (length(reusable_entries)) {
        reusable_entries[[1L]]$id
      } else {
        NULL
      }
    ),
    error = identity
  )
  if (inherits(next_store, "condition")) {
    builder_project_operation_phase(operation)
    builder_notify(
      "error",
      paste0(
        "The dataset caches could not be attached to this session: ",
        conditionMessage(next_store)
      )
    )
    return(invisible(FALSE))
  }
  current_protocol <- isolate(protocol())
  ready <- builder_project_open_worker_ready(length(pending_entries) > 0L)
  if (!isTRUE(ready$protocol) || !isTRUE(ready$worker)) {
    builder_project_operation_phase(operation)
    return(wait_for_builder_project_open_worker(actions, ready))
  }
  next_protocol <- if (is.null(current_protocol)) {
    NULL
  } else {
    builder_request_protocol(current_protocol$epoch)
  }
  invalidate_builder_project_source_sync()
  # Opening replaces the workspace, so its optional CRB builds stop here
  # instead of holding Open back.
  cancel_builder_project_crb_queue()
  reset_builder_project_crb_pipeline()
  protocol(next_protocol)
  store(next_store)
  projection_previews(list())
  trajectory_previews(list())
  spatial_previews(list())
  builder_project_configuration_cache_clear(
    builder_configuration_identity_cache
  )
  dataset_check_marks(marks)
  builder_project_pending_entries(pending_entries)
  builder_project_artifacts(artifacts)
  builder_project_skipped_ids(skipped_ids)
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    record$runtime_restore_status <- NULL
    record$runtime_restore_action <- NULL
    record
  })
  builder_project(list(
    root = root,
    path = pending$path,
    name = manifest$project$name,
    manifest = manifest
  ))
  restore_builder_project_preferences(manifest)
  builder_project_restore(NULL)
  builder_project_open_actions(NULL)
  builder_project_open_wait(NULL)
  builder_project_open_discard_confirmed(FALSE)
  ## Rendering the opened workspace takes seconds for large datasets. Close the
  ## dataset dialog (which shows "Opening selected datasets…") only once that
  ## render has reached the page, so the window is never an empty Builder.
  session$onFlushed(
    function() shiny::removeModal(session = session),
    once = TRUE
  )
  restore_total <- length(pending_entries)
  if (!restore_total) {
    builder_project_restore_progress(list(
      mode = "idle",
      total = 0L,
      remaining = 0L
    ))
    builder_project_operation_phase("idle")
    restore_builder_project_last_ui_after_flush(manifest$last_ui)
    return(invisible(TRUE))
  }
  builder_project_restore_progress(list(
    mode = "restoring",
    total = restore_total,
    remaining = restore_total,
    selected_total = length(reusable_entries) + restore_total,
    ready = length(reusable_entries)
  ))
  ## Keep the project locked while the client receives the restore UI update,
  ## but do not advertise the sources as restoring until start_load() has
  ## registered their import records. Otherwise the abandoned-entry observer
  ## can run in this flush, see neither a live dataset nor an import, and drop
  ## the saved settings before the onFlushed callback gets to enqueue them.
  builder_project_operation_phase("opening")
  entries_to_restore <- pending_entries
  session$onFlushed(
    function() {
      shiny::isolate({
        failed <- character()
        for (record in entries_to_restore) {
          started <- try(
            start_builder_project_record_load(record, root),
            silent = TRUE
          )
          if (!isTRUE(started)) {
            reason <- if (inherits(started, "try-error")) {
              builder_import_public_error(
                conditionMessage(attr(started, "condition"))
              )
            } else {
              "The source could not be queued for the background worker."
            }
            failed <- c(
              failed,
              paste0(record$name %||% record$id, ": ", reason)
            )
            builder_project_commit_restored_entry(
              list(id = record$id),
              error = reason
            )
          }
        }
        if (length(failed)) {
          builder_notify(
            "error",
            paste0(
              "Could not start loading the saved dataset. ",
              paste(failed, collapse = " ")
            )
          )
          add_error(paste(failed, collapse = "\n"))
        }
        if (length(isolate(builder_project_pending_entries()))) {
          builder_project_operation_phase("restoring")
        }
      })
    },
    once = TRUE
  )
}

observeEvent(input$confirm_builder_project_open, {
  pending <- isolate(builder_project_restore())
  if (is.null(pending)) {
    return()
  }
  records <- builder_project_record_map(pending$manifest)
  actions <- lapply(records, function(record) {
    input[[paste0("project_restore_", record$id)]] %||% "skip"
  })
  if (
    builder_project_open_needs_confirmation() &&
      !isTRUE(isolate(builder_project_open_discard_confirmed()))
  ) {
    builder_project_open_actions(actions)
    shiny::showModal(builder_project_open_discard_dialog(), session = session)
    return()
  }
  complete_builder_project_open(actions)
})

observeEvent(input$confirm_discard_builder_project_open, {
  pending <- isolate(builder_project_restore())
  actions <- isolate(builder_project_open_actions())
  if (is.null(pending) || is.null(actions)) {
    # Confirmed before the picker.
    shiny::removeModal()
    request_builder_project_open(confirmed = TRUE)
    return()
  }
  builder_project_open_discard_confirmed(TRUE)
  complete_builder_project_open(actions)
})

observeEvent(input$cancel_discard_builder_project_open, {
  actions <- isolate(builder_project_open_actions())
  builder_project_open_actions(NULL)
  if (is.null(isolate(builder_project_restore())) || is.null(actions)) {
    shiny::removeModal()
    return()
  }
  # Keep the project and the dataset choices: back to the choices dialog.
  show_builder_project_restore_choices(actions)
})

observeEvent(input$cancel_builder_project_open_wait, {
  wait <- isolate(builder_project_open_wait())
  builder_project_open_wait(NULL)
  show_builder_project_restore_choices(if (is.list(wait)) wait$actions)
})

observe({
  if (!identical(builder_project_operation_phase(), "restoring")) {
    return()
  }
  pending <- builder_project_pending_entries()
  if (!length(pending)) {
    finish_builder_project_restore()
    return()
  }
  live_ids <- vapply(sets(), `[[`, character(1), "id")
  import_ids <- vapply(
    imports()$entries %||% list(),
    `[[`,
    character(1),
    "id"
  )
  abandoned <- builder_project_abandoned_entries(
    pending,
    live_ids,
    import_ids,
    builder_project_operation_phase()
  )
  if (!length(abandoned)) {
    return()
  }
  for (id in abandoned) {
    builder_project_commit_restored_entry(
      list(id = id),
      error = "The restored source was abandoned before completion."
    )
  }
})

observeEvent(input$project_resume_current_source, {
  if (!builder_operation_allowed("edit_dataset")) {
    return()
  }
  id <- isolate(current())
  project <- isolate(builder_project())
  if (is.null(project) || is.null(id)) {
    return()
  }
  records <- builder_project_record_map(project$manifest)
  record <- records[[id]] %||% NULL
  status <- if (is.null(record)) {
    NULL
  } else {
    builder_project_dataset_status(
      record,
      project$root
    )
  }
  if (is.null(record) || !"resume" %in% (status$actions %||% character())) {
    builder_notify(
      "warning",
      "The source file must be re-linked before editing.",
      target = list(dataset = id)
    )
    return()
  }
  pending <- isolate(builder_project_pending_entries())
  previous_pending <- pending[[id]] %||% NULL
  record$runtime_restore_status <- status
  record$runtime_restore_action <- "edit"
  pending[[id]] <- record
  builder_project_pending_entries(pending)
  started <- tryCatch(
    start_builder_project_record_load(record, project$root),
    error = identity
  )
  if (inherits(started, "condition")) {
    pending <- isolate(builder_project_pending_entries())
    pending[[id]] <- previous_pending
    builder_project_pending_entries(pending)
    builder_notify(
      "error",
      paste0(
        "The source dataset could not be loaded: ",
        conditionMessage(started)
      ),
      target = list(dataset = id)
    )
    send_builder_project_source_load_state(
      id,
      "error",
      conditionMessage(started)
    )
    return()
  }
  if (!isTRUE(started)) {
    pending <- isolate(builder_project_pending_entries())
    pending[[id]] <- previous_pending
    builder_project_pending_entries(pending)
    message <- paste(
      "The source could not be queued for the background worker.",
      "Try loading it again."
    )
    builder_notify("error", message, target = list(dataset = id))
    send_builder_project_source_load_state(id, "error", message)
    return()
  }
  send_builder_project_source_load_state(id, "loading")
})

observe({
  value <- result()
  plan <- isolate(builder_project_build_plan())
  project <- isolate(builder_project())
  if (is.null(plan) || !is.list(value)) {
    return()
  }
  terminal <- any(vapply(
    c("success", "failure", "needs_decision"),
    identical,
    logical(1),
    y = value$state
  )) ||
    isTRUE(value$published)
  if (!isTRUE(terminal)) {
    return()
  }
  ordinary_registration_required <-
    identical(value$state, "success") &&
    length(value$built %||% character()) > 0L &&
    any(vapply(
      plan$items,
      function(item) !is.list(item$reused_artifact),
      logical(1)
    ))
  if (!isTRUE(ordinary_registration_required)) {
    builder_project_build_plan(NULL)
    return()
  }
  if (is.null(project)) {
    builder_project_build_plan(NULL)
    return()
  }

  builder_project_operation_phase("registering")
  builder_project_build_plan(NULL)

  project_before_registration <- project
  previous_artifacts <- isolate(builder_project_artifacts())
  registration_entries <- lapply(isolate(sets()), function(entry) {
    entry[intersect(
      names(entry),
      c(
        "id",
        "revision",
        "snapshot",
        "settings",
        "acknowledgements",
        "spatial_drafts"
      )
    )]
  })
  registration_value <- value[intersect(
    names(value),
    c("state", "built", "verifications", "release", "error")
  )]
  registration_plan <- list(items = plan$items)

  fail_registration <- function(error) {
    message <- if (inherits(error, "condition")) {
      conditionMessage(error)
    } else {
      as.character(error %||% "Dataset caches could not be registered.")
    }
    builder_project_registration_task(NULL)
    builder_project_operation_phase("save_failed")
    builder_notify("error", message)
    invisible(FALSE)
  }

  task <- tryCatch(
    builder_async_submit(
      quote({
        runtime <- new.env(parent = globalenv())
        runtime$`%||%` <- function(left, right) {
          if (is.null(left)) right else left
        }
        for (runtime_file in runtime_files) {
          source_utf8(runtime_file, runtime)
        }
        runtime$builder_project_prepare_build_artifacts(
          plan = plan,
          value = value,
          root = root,
          previous_artifacts = previous_artifacts,
          entries = entries,
          promote = promote
        )
      }),
      list(
        plan = registration_plan,
        value = registration_value,
        root = project$root,
        previous_artifacts = previous_artifacts,
        entries = registration_entries,
        promote = FALSE,
        runtime_files = builder_project_registration_runtime_files(),
        source_utf8 = builder_source_utf8
      )
    ),
    error = identity
  )
  if (inherits(task, "condition")) {
    fail_registration(task)
    return()
  }
  builder_project_registration_task(task)
  builder_async_then(
    task,
    session,
    on_fulfilled = function(prepared) {
      if (!identical(isolate(builder_project_registration_task()), task)) {
        return(invisible(FALSE))
      }
      builder_project_registration_task(NULL)
      current_project <- isolate(builder_project())
      if (
        is.null(current_project) ||
          !identical(current_project$root, project$root) ||
          !identical(current_project$path, project$path)
      ) {
        return(fail_registration(
          "Project changed. Save again to update the dataset cache."
        ))
      }
      if (
        !is.list(prepared) ||
          !is.list(prepared$artifacts) ||
          !is.logical(prepared$build_succeeded) ||
          length(prepared$build_succeeded) != 1L
      ) {
        return(fail_registration(
          "The Project artifact registration returned an invalid result."
        ))
      }
      registration_error <- prepared$error %||% NULL
      artifacts_changed <- !builder_has_text(registration_error) &&
        !identical(prepared$artifacts, previous_artifacts)
      save_required <- artifacts_changed
      if (artifacts_changed) {
        builder_project_artifacts(prepared$artifacts)
        ## A dataset that now has a cache no longer needs its earlier
        ## "without a cache" warning.
        for (artifact_id in names(prepared$artifacts)) {
          builder_notify_clear(builder_project_cache_notice_id(artifact_id))
        }
      }
      finish_registration <- function(saved) {
        builder_project_operation_phase(
          if (isTRUE(saved)) "idle" else "save_failed"
        )
        if (!isTRUE(saved)) {
          if (artifacts_changed) {
            builder_project_artifacts(previous_artifacts)
          }
          builder_project(project_before_registration)
        }
        if (builder_has_text(registration_error)) {
          if (isTRUE(prepared$build_succeeded)) {
            builder_notify("error", registration_error)
          }
        } else if (isTRUE(saved)) {
          if (save_required) {
            builder_notify(
              "success",
              "Dataset caches were added to the project.",
              target = "status",
              duration = 5
            )
          }
        }
        invisible(isTRUE(saved) && !builder_has_text(registration_error))
      }
      if (!save_required) {
        return(finish_registration(TRUE))
      }
      # A Save may have released the workspace while its source copy still
      # runs; writing the manifest now would orphan that copy's results.
      save_registration <- function() {
        if (builder_session_closed()) {
          return(invisible(FALSE))
        }
        source_status <- isolate(builder_project_source_sync())$status %||%
          "idle"
        if (
          source_status %in%
            c("syncing", "cancelling") ||
            !is.null(isolate(builder_project_source_process()))
        ) {
          later::later(
            function() {
              shiny::withReactiveDomain(
                session,
                shiny::isolate(save_registration())
              )
            },
            delay = 0.2
          )
          return(invisible(TRUE))
        }
        saved <- save_builder_project_state(
          show_actions = FALSE,
          materialize = FALSE,
          notify = FALSE,
          manage_lifecycle = FALSE,
          on_complete = finish_registration
        )
        if (identical(saved, "pending")) {
          return(invisible(TRUE))
        }
        finish_registration(saved)
      }
      save_registration()
    },
    on_rejected = fail_registration
  )
})
