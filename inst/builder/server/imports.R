## Builder server: imports.

dataset_mutations_locked <- function(notify = TRUE) {
  if (exists("builder_operation_allowed", mode = "function", inherits = TRUE)) {
    return(
      !isTRUE(builder_operation_allowed(
        "mutate_datasets",
        notify = notify
      ))
    )
  }
  locked <- builder_mutations_locked(
    isolate(build_flow()),
    isolate(protocol())
  )
  if (isTRUE(locked) && isTRUE(notify)) {
    builder_notify(
      "info",
      "Wait for the active build to finish before changing datasets.",
      duration = 6
    )
  }
  isTRUE(locked)
}

start_load <- function(
  kind,
  arg,
  label,
  file_meta = NULL,
  client_id = NULL,
  dataset_id = NULL,
  source_origin = NULL,
  example_id = NULL,
  retained_path = NULL,
  expected_md5 = NULL
) {
  if (isTRUE(replay_existing_client_import(client_id))) {
    return(invisible(FALSE))
  }
  source_name <- if (identical(kind, "file")) {
    file_meta$name %||% retained_path %||% arg
  } else {
    NULL
  }
  source_format <- if (builder_has_text(source_name %||% "")) {
    builder_path_format_capability(source_name)
  } else {
    NULL
  }
  if (
    is.list(source_format) &&
      identical(source_format$available, FALSE)
  ) {
    reason <- source_format$unavailable_reason
    add_error(reason)
    release_client_import(
      client_id,
      outcome = "rejected",
      message = reason
    )
    return(invisible(FALSE))
  }
  restoring_source <- !is.null(dataset_id) &&
    exists("builder_project_pending_entries", inherits = TRUE) &&
    dataset_id %in% names(isolate(builder_project_pending_entries()))
  import_queue <- isolate(imports())
  existing_import <- if (isTRUE(restoring_source)) {
    builder_import_find(import_queue, dataset_id)
  } else {
    NULL
  }
  if (
    isTRUE(restoring_source) &&
      is.list(existing_import) &&
      existing_import$load_state %in%
        c(
          "queued",
          "reading",
          "inspecting",
          "validating",
          "preparing"
        )
  ) {
    return(invisible(TRUE))
  }
  if (!isTRUE(restoring_source) && dataset_mutations_locked()) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "Dataset changes are locked while the active build finishes."
    )
    return(invisible(FALSE))
  }
  rs <- worker()
  if (is.null(rs)) {
    add_error("The background worker is not ready yet.")
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "The background worker is not ready yet."
    )
    return(invisible(FALSE))
  }
  reservation_entries <- sets()
  if (isTRUE(restoring_source)) {
    reservation_entries <- Filter(
      function(entry) !identical(entry$id, dataset_id),
      reservation_entries
    )
  }
  source_key <- builder_source_key(kind, arg)
  pending <- isolate(pending_sources())
  if (isTRUE(restoring_source) && source_key %in% pending) {
    reservation_owned <- any(vapply(
      import_queue$entries %||% list(),
      function(entry) {
        identical(entry$source$reservation_key %||% NULL, source_key)
      },
      logical(1)
    ))
    if (!isTRUE(reservation_owned)) {
      pending <- builder_source_release(pending, source_key)
      pending_sources(pending)
    }
  }
  reservation <- builder_source_reserve(
    reservation_entries,
    pending,
    kind,
    arg
  )
  if (!isTRUE(reservation$ok)) {
    add_error(
      if (identical(reservation$code, "source_pending")) {
        "This source is already loading in the background."
      } else {
        "This dataset has already been added or is already waiting."
      }
    )
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "This dataset has already been added or is already waiting."
    )
    return(invisible(FALSE))
  }
  pending_sources(reservation$pending)
  add_error(NULL)
  existing_ids <- builder_project_allocation_ids(
    entries = sets(),
    import_entries = imports()$entries %||% list(),
    replacing_id = if (isTRUE(restoring_source)) dataset_id else NULL,
    reserved_ids = builder_project_dataset_reservations(
      isolate(builder_project())$manifest %||% list(),
      excluding_id = if (isTRUE(restoring_source)) dataset_id else NULL
    )$ids
  )
  allocation <- builder_project_allocate_dataset_id(
    seq_id(),
    existing_ids,
    restored_id = dataset_id
  )
  seq_id(allocation$sequence)
  id <- allocation$id
  filename <- NULL
  file_type <- NULL
  file_size <- NA_real_
  retained_source <- if (
    builder_has_text(retained_path) && file.exists(retained_path)
  ) {
    normalizePath(retained_path, winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }
  if (identical(kind, "file") && is.list(file_meta)) {
    uploads <- pending_uploads()
    filename <- builder_safe_file_name(file_meta$name, paste0(label, ".rds"))
    file_type <- builder_file_type_label(filename, file_meta$type)
    file_size <- suppressWarnings(as.numeric(file_meta$size %||% NA_real_))
    uploads[[id]] <- list(
      id = id,
      filename = filename,
      type = file_type,
      size = file_size,
      visible = TRUE
    )
    pending_uploads(uploads)
  } else if (identical(kind, "example")) {
    example_source <- try(
      builder_project_example_source(arg, builder_example_catalog()),
      silent = TRUE
    )
    if (!inherits(example_source, "try-error")) {
      filename <- example_source$filename
      file_size <- suppressWarnings(as.numeric(
        file.info(example_source$path)$size[[1L]]
      ))
      retained_source <- example_source$path
    }
  }
  generation <- 1L
  progress_path <- NULL
  transport_retained_path <- NULL
  snapshot_root <- rs$snapshot_root %||% NULL
  if (
    is.character(snapshot_root) &&
      length(snapshot_root) == 1L &&
      !is.na(snapshot_root) &&
      dir.exists(snapshot_root)
  ) {
    candidate <- try(
      builder_import_progress_path(snapshot_root, id, generation),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      progress_path <- candidate
    }
  }
  retain_example <- identical(kind, "example") &&
    builder_has_text(retained_source) &&
    builder_has_text(snapshot_root) &&
    dir.exists(snapshot_root)
  retain_upload <- identical(kind, "file") &&
    !identical(source_origin, "local") &&
    !builder_has_text(retained_source)
  retain_in_worker <- FALSE
  if (retain_upload) {
    retained <- try(
      builder_project_session_source_path(
        arg,
        filename,
        snapshot_root,
        id
      ),
      silent = TRUE
    )
    retain_in_worker <- !inherits(retained, "try-error")
  } else if (retain_example) {
    retained <- try(
      builder_project_retain_session_source(
        retained_source,
        filename,
        snapshot_root,
        id
      ),
      silent = TRUE
    )
  } else {
    retained <- retained_source
  }
  if (retain_upload || retain_example) {
    if (inherits(retained, "try-error")) {
      pending_sources(builder_source_release(
        pending_sources(),
        reservation$key
      ))
      uploads <- pending_uploads()
      uploads[[id]] <- NULL
      pending_uploads(uploads)
      release_client_import(
        client_id,
        outcome = "error",
        message = "The uploaded source could not be retained for this session."
      )
      add_error("The uploaded source could not be retained for this session.")
      return(invisible(FALSE))
    }
    retained_source <- retained
  }
  transport_owned <- isTRUE(retain_in_worker) &&
    isTRUE(file_meta$transport_owned)
  if (isTRUE(retain_in_worker) && builder_has_text(progress_path)) {
    transport_retained_path <- paste0(
      progress_path,
      ".transport-retained.rds"
    )
    builder_project_transport_retained_remove(transport_retained_path)
  }
  source_descriptor <- list(
    kind = kind,
    origin = source_origin %||%
      if (identical(kind, "example")) {
        "example"
      } else {
        "upload"
      },
    staged_path = retained_source %||%
      if (identical(kind, "file")) arg else NULL,
    transport_path = if (isTRUE(retain_in_worker)) arg else NULL,
    transport_owned = transport_owned,
    example = example_id %||% if (identical(kind, "example")) arg else NULL,
    reservation_key = reservation$key,
    expected_md5 = expected_md5,
    fingerprint = if (identical(kind, "example")) {
      paste0("example:", arg, ":builder-profile-v1")
    } else {
      info <- suppressWarnings(file.info(arg))
      paste(
        suppressWarnings(as.numeric(info$size[[1L]] %||% file_size)),
        suppressWarnings(as.numeric(info$mtime[[1L]] %||% NA_real_)),
        sep = ":"
      )
    }
  )
  pending_entry <- builder_import_entry(
    id = id,
    label = label,
    source = source_descriptor,
    filename = filename,
    file_type = file_type,
    size = file_size,
    generation = generation
  )
  imports(builder_import_add(isolate(imports()), pending_entry))
  next_focus <- builder_import_auto_focus(
    isolate(current()),
    isolate(active_import_id()),
    id
  )
  if (!is.null(next_focus)) {
    active_import_id(next_focus)
  }
  if (builder_has_text(client_id)) {
    bind_client_import(client_id, id, filename %||% label, kind)
  }
  queued <- enqueue(list(
    kind = "load",
    source = kind,
    id = id,
    path = if (identical(kind, "file")) arg else NA_character_,
    retained_path = retained_source,
    retain_source = retain_in_worker,
    transport_owned = transport_owned,
    transport_retained_path = transport_retained_path,
    example = example_id %||% if (identical(kind, "example")) arg else NULL,
    source_origin = source_descriptor$origin,
    label = label,
    filename = filename,
    reservation_key = reservation$key,
    import_generation = generation,
    expected_md5 = expected_md5,
    progress_path = progress_path,
    note = paste0("Loading ", label, "…")
  ))
  if (!isTRUE(queued)) {
    pending_sources(builder_source_release(
      pending_sources(),
      reservation$key
    ))
    uploads <- pending_uploads()
    uploads[[id]] <- NULL
    pending_uploads(uploads)
    forget_import(id)
    builder_import_progress_remove(progress_path %||% "")
    builder_project_transport_retained_remove(transport_retained_path %||% "")
    release_client_import(
      client_id,
      server_id = NULL,
      outcome = "error",
      message = "The background worker is not ready yet."
    )
  }
  invisible(isTRUE(queued))
}

process_client_import_upload <- function(uploads, dispatch) {
  pending_client_id <- if (is.list(dispatch)) dispatch$client_id else NULL
  if (
    !is.data.frame(uploads) ||
      nrow(uploads) != 1L ||
      !all(c("name", "datapath") %in% names(uploads))
  ) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "Expected exactly one uploaded file."
    )
    return()
  }
  path <- as.character(uploads$datapath[[1L]])
  label <- as.character(uploads$name[[1L]])
  size <- if ("size" %in% names(uploads)) {
    suppressWarnings(as.numeric(uploads$size[[1L]]))
  } else {
    NA_real_
  }
  type <- if ("type" %in% names(uploads)) {
    as.character(uploads$type[[1L]])
  } else {
    ""
  }
  metadata_matches <- is.list(dispatch) &&
    builder_has_text(pending_client_id) &&
    identical(dispatch$name, label) &&
    (is.na(dispatch$size) ||
      is.na(size) ||
      identical(as.numeric(dispatch$size), as.numeric(size)))
  if (
    !metadata_matches || !builder_has_text(path) || !builder_has_text(label)
  ) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "The uploaded file did not match its dispatch metadata."
    )
    return()
  }
  supported_extensions <- unique(tolower(unlist(lapply(
    builder_formats,
    `[[`,
    "extensions"
  ))))
  if (!tolower(tools::file_ext(label)) %in% supported_extensions) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "This file format is not supported."
    )
    return()
  }
  start_load(
    "file",
    path,
    tools::file_path_sans_ext(basename(label)),
    file_meta = list(
      name = label,
      type = type,
      size = size,
      transport_owned = builder_upload_transport_owned(path)
    ),
    client_id = pending_client_id
  )
  invisible(TRUE)
}

consume_client_import_upload <- function() {
  dispatch <- isolate(pending_client_import_dispatch())
  upload <- isolate(pending_client_upload())
  if (is.null(dispatch) || is.null(upload)) {
    return(invisible(FALSE))
  }
  pending_client_import_dispatch(NULL)
  pending_client_upload(NULL)
  process_client_import_upload(upload$files, dispatch)
}

expire_pending_client_upload <- function(token) {
  if (builder_session_closed()) {
    return(invisible(FALSE))
  }
  upload <- isolate(pending_client_upload())
  dispatch <- isolate(pending_client_import_dispatch())
  if (
    is.null(upload) ||
      !identical(upload$token, token) ||
      !is.null(dispatch)
  ) {
    return(invisible(FALSE))
  }
  pending_client_upload(NULL)
  invisible(TRUE)
}

process_native_dataset_files <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  supported_extensions <- builder_dataset_extensions()
  accepted <- 0L
  for (path in paths) {
    filename <- basename(path)
    format <- builder_path_format_capability(filename)
    if (is.list(format) && identical(format$available, FALSE)) {
      builder_notify("error", format$unavailable_reason)
      next
    }
    if (
      !file.exists(path) ||
        dir.exists(path) ||
        !tolower(tools::file_ext(filename)) %in% supported_extensions
    ) {
      builder_notify(
        "error",
        paste0(
          "The selected dataset is unavailable or unsupported: ",
          filename
        )
      )
      next
    }
    info <- suppressWarnings(file.info(path))
    started <- start_load(
      "file",
      path,
      tools::file_path_sans_ext(filename),
      file_meta = list(
        name = filename,
        type = "application/octet-stream",
        size = suppressWarnings(as.numeric(info$size[[1L]]))
      ),
      source_origin = "local"
    )
    if (isTRUE(started)) {
      accepted <- accepted + 1L
    }
  }
  invisible(accepted)
}

observeEvent(input$builder_choose_dataset_files, {
  picker_idle <- function() {
    session$sendCustomMessage(
      "builder_dataset_picker_state",
      list(active = FALSE)
    )
  }
  if (dataset_mutations_locked()) {
    picker_idle()
    return()
  }
  builder_schedule_native_picker(
    "dataset_files",
    key = "dataset-files",
    on_finally = picker_idle,
    on_result = function(choice) {
      if (identical(choice$status, "cancelled")) {
        return(invisible(FALSE))
      }
      if (!identical(choice$status, "selected")) {
        if (isTRUE(choice$fallback)) {
          session$sendCustomMessage("builder_dataset_picker_fallback", list())
        } else {
          builder_notify(
            "error",
            choice$error %||% "The dataset picker could not be opened."
          )
        }
        return(invisible(FALSE))
      }
      process_native_dataset_files(choice$paths %||% choice$path)
    },
    on_error = function(error) {
      session$sendCustomMessage("builder_dataset_picker_fallback", list())
    }
  )
})

observeEvent(input$builder_client_import_dispatch, {
  event <- input$builder_client_import_dispatch
  if (!is.list(event) || is.object(event)) {
    pending_client_import_dispatch(NULL)
    return()
  }
  pending_client_import_dispatch(list(
    client_id = event$client_id %||% NULL,
    name = event$name %||% NULL,
    size = suppressWarnings(as.numeric(event$size %||% NA_real_)[1L]),
    token = event$nonce %||% NULL
  ))
  consumed <- consume_client_import_upload()
  if (!isTRUE(consumed)) {
    current <- isolate(pending_client_import_dispatch())
    if (is.list(current) && identical(current$token, event$nonce %||% NULL)) {
      session$sendCustomMessage(
        "builder_client_import_dispatch_ready",
        list(client_id = current$client_id)
      )
    }
  }
  token <- event$nonce %||% NULL
  later::later(
    function() {
      if (builder_session_closed()) {
        return(invisible(FALSE))
      }
      dispatch <- isolate(pending_client_import_dispatch())
      if (is.null(dispatch) || !identical(dispatch$token, token)) {
        return()
      }
      pending_client_import_dispatch(NULL)
      release_client_import(
        dispatch$client_id,
        outcome = "rejected",
        message = "The file upload was not received in time."
      )
    },
    delay = 30
  )
})

observeEvent(input$dataset_files, {
  token <- isolate(pending_client_upload_sequence()) + 1L
  pending_client_upload_sequence(token)
  pending_client_upload(list(
    files = input$dataset_files,
    received = Sys.time(),
    token = token
  ))
  consume_client_import_upload()
  later::later(
    function() expire_pending_client_upload(token),
    delay = 30
  )
})

observeEvent(input$builder_import_example, {
  event <- input$builder_import_example
  client_id <- if (is.list(event) && !is.object(event)) {
    event$client_id
  } else {
    NULL
  }
  example_id <- if (is.list(event) && !is.object(event)) event$example else NULL
  ex <- Filter(
    function(e) identical(e$id, example_id),
    builder_examples()
  )
  if (!builder_has_text(client_id) || !length(ex)) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "The selected example is unavailable."
    )
    return()
  }
  start_load(
    "example",
    ex[[1L]]$id,
    ex[[1L]]$label,
    client_id = client_id
  )
})

observeEvent(input$builder_import_sync_request, {
  ids <- isolate(client_import_server_ids())
  transport_released <- isolate(transport_released_imports())
  queue <- isolate(imports())
  active <- lapply(names(ids), function(server_id) {
    entry <- builder_import_find(queue, server_id)
    if (is.null(entry)) {
      return(NULL)
    }
    list(
      client_id = unname(ids[[server_id]]),
      server_id = server_id,
      state = entry$load_state,
      transport_retained = server_id %in% transport_released,
      message = entry$error
    )
  })
  active <- Filter(Negate(is.null), active)
  terminal <- unname(isolate(released_client_import_records()))
  dispatch <- isolate(pending_client_import_dispatch())
  if (is.list(dispatch) && builder_has_text(dispatch$client_id)) {
    active <- c(
      active,
      list(list(
        client_id = dispatch$client_id,
        server_id = NULL,
        state = "awaiting_upload",
        transport_retained = FALSE,
        message = NULL
      ))
    )
  }
  session$sendCustomMessage(
    "builder_import_sync",
    list(
      imports = unname(c(active, terminal)),
      server_busy = builder_has_text(isolate(external_import_active()))
    )
  )
})

remove_pending_import <- function(id) {
  entry <- isolate(import_of(id))
  if (
    !is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id) ||
      is.null(entry)
  ) {
    return(invisible(FALSE))
  }
  current_protocol <- isolate(protocol())
  if (is.null(current_protocol)) {
    return(invisible(FALSE))
  }
  belongs <- function(request) {
    !is.null(request) &&
      identical(request$dataset, id) &&
      identical(request$kind, "load")
  }
  in_flight <- belongs(current_protocol$pending)
  running <- in_flight ||
    any(vapply(current_protocol$awaiting_ack, belongs, logical(1)))
  if (running) {
    if (isTRUE(entry$cancelling)) {
      return(invisible(TRUE))
    }
    uploads <- isolate(pending_uploads())
    if (!is.null(uploads[[id]])) {
      uploads[[id]]$visible <- FALSE
      pending_uploads(uploads)
    }
    cancelled_loads(unique(c(cancelled_loads(), id)))
    if (in_flight) {
      # Show "Cancelling…" first; the worker restart then runs after this
      # flush so the click answers before the worker is stopped.
      imports(builder_import_mark_cancelling(isolate(imports()), id))
      request_id <- current_protocol$pending$request_id
      session$onFlushed(
        function() {
          later::later(
            function() {
              if (!builder_session_closed()) {
                shiny::withReactiveDomain(
                  session,
                  shiny::isolate(cancel_running_import(id, request_id))
                )
              }
            },
            delay = 0
          )
        },
        once = TRUE
      )
      return(invisible(TRUE))
    }
    forget_import(id)
    return(invisible(TRUE))
  }
  if (identical(entry$load_state, "error")) {
    release_pending_source(list(
      kind = "load",
      source = entry$source$kind,
      id = entry$id,
      path = entry$source$transport_path %||% entry$source$staged_path,
      example = entry$source$example,
      reservation_key = entry$source$reservation_key
    ))
    return(invisible(TRUE))
  }
  forgotten <- try(
    builder_protocol_forget_dataset(
      current_protocol,
      id,
      reason = "upload_cancelled"
    ),
    silent = TRUE
  )
  if (inherits(forgotten, "try-error")) {
    add_error("This file could not be removed while it was loading.")
    return(invisible(FALSE))
  }
  removed <- c(forgotten$failed, forgotten$discarded)
  if (!length(removed)) {
    return(invisible(FALSE))
  }
  protocol(forgotten$protocol)
  invisible(lapply(removed, function(request) {
    release_client_import(
      client_import_id_for(request$payload$id),
      server_id = request$payload$id,
      outcome = "cancelled"
    )
    release_pending_source(request$payload)
  }))
  invisible(TRUE)
}

## Stop the worker that is still reading or inspecting a cancelled import.
## Without this the hidden load keeps the worker busy, and every later
## import, preview and Build queues behind it until it finishes.
cancel_running_import <- function(id, request_id) {
  current_protocol <- protocol()
  pending <- current_protocol$pending
  current_worker <- worker()
  if (
    is.null(current_worker) ||
      !is.list(pending) ||
      !identical(pending$request_id, request_id)
  ) {
    # The load finished meanwhile; its result path drops cancelled loads.
    forget_import(id)
    return(invisible(FALSE))
  }
  restart_worker_protocol(
    current_worker,
    current_protocol,
    "The import was cancelled.",
    grace_ms = 0L,
    async = TRUE,
    notify = FALSE
  )
  recovered <- protocol()
  forgotten <- if (is.null(recovered)) {
    NULL
  } else {
    try(
      builder_protocol_forget_dataset(
        recovered,
        id,
        reason = "upload_cancelled"
      ),
      silent = TRUE
    )
  }
  if (is.list(forgotten) && !inherits(forgotten, "try-error")) {
    # Recovery requeued the cancelled load; drop it before it dispatches.
    protocol(forgotten$protocol)
  }
  release_pending_source(pending$payload)
  forget_import(id)
  invisible(TRUE)
}

observeEvent(input$cancel_pending_upload, {
  event <- input$cancel_pending_upload
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  remove_pending_import(id)
})

observeEvent(input$remove_import, {
  event <- input$remove_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  remove_pending_import(id)
})

observeEvent(input$pick_import, {
  event <- input$pick_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    event
  }
  if (
    is.character(id) &&
      length(id) == 1L &&
      !is.na(id) &&
      !is.null(isolate(import_of(id)))
  ) {
    active_import_id(id)
  }
})

observeEvent(input$retry_import, {
  event <- input$retry_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  entry <- if (is.character(id) && length(id) == 1L) {
    isolate(import_of(id))
  } else {
    NULL
  }
  if (is.null(entry) || !identical(entry$load_state, "error")) {
    return()
  }
  next_queue <- builder_import_retry(isolate(imports()), id)
  entry <- builder_import_find(next_queue, id)
  current_worker <- isolate(worker())
  progress_path <- NULL
  if (
    is.list(current_worker) &&
      is.character(current_worker$snapshot_root) &&
      length(current_worker$snapshot_root) == 1L &&
      dir.exists(current_worker$snapshot_root)
  ) {
    candidate <- try(
      builder_import_progress_path(
        current_worker$snapshot_root,
        id,
        entry$generation
      ),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      progress_path <- candidate
    }
  }
  imports(next_queue)
  active_import_id(id)
  external_import_active(id)
  session$sendCustomMessage(
    "builder_import_scheduler_state",
    list(active = TRUE, server_id = id)
  )
  cancelled_loads(setdiff(cancelled_loads(), id))
  retry_path <- entry$source$transport_path %||% entry$source$staged_path
  queued <- enqueue(list(
    kind = "load",
    source = entry$source$kind,
    id = entry$id,
    path = retry_path,
    retained_path = entry$source$staged_path,
    retain_source = builder_has_text(entry$source$transport_path),
    transport_owned = isTRUE(entry$source$transport_owned),
    example = entry$source$example,
    source_origin = entry$source$origin,
    label = entry$label,
    filename = entry$filename,
    reservation_key = entry$source$reservation_key,
    import_generation = entry$generation,
    expected_md5 = entry$source$expected_md5 %||% NULL,
    progress_path = progress_path,
    note = paste0("Loading ", entry$label, "…")
  ))
  if (!isTRUE(queued)) {
    external_import_active(NULL)
    session$sendCustomMessage(
      "builder_import_scheduler_state",
      list(active = FALSE, server_id = id)
    )
    set_import_state(
      id,
      "error",
      entry$generation,
      "The background worker is not ready yet."
    )
  }
})

schedule_failed_import_cleanup <- function(current_worker) {
  if (
    inherits(current_worker, "builder_worker") &&
      !length(current_worker$snapshot_registry)
  ) {
    failed_import_cleanup_pending(TRUE)
  }
  invisible(TRUE)
}

observe({
  if (!isTRUE(failed_import_cleanup_pending())) {
    return()
  }
  current_protocol <- protocol()
  current_worker <- worker()
  if (is.null(current_protocol) || is.null(current_worker)) {
    return()
  }
  if (length(current_worker$snapshot_registry)) {
    failed_import_cleanup_pending(FALSE)
    return()
  }
  if (!builder_protocol_is_quiescent(current_protocol)) {
    return()
  }
  failed_import_cleanup_pending(FALSE)
  recycled <- restart_worker_protocol(
    current_worker,
    current_protocol,
    "Reclaiming memory after a failed dataset import."
  )
  if (isTRUE(recycled)) {
    add_error(NULL)
  }
})

finish_builder_build_dispatch_failure <- function(
  dispatched_protocol,
  request,
  value
) {
  message <- if (
    is.list(value) &&
      is.character(value$error) &&
      length(value$error) == 1L &&
      !is.na(value$error) &&
      nzchar(value$error)
  ) {
    value$error
  } else {
    "The Build could not be started."
  }
  completed <- builder_protocol_complete(
    dispatched_protocol,
    builder_worker_response(request, list(error = message))
  )
  protocol(builder_protocol_acknowledge(
    completed$protocol,
    request$request_id
  ))
  result(value)
  busy_note(NULL)
  build_operation_phase(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  invisible(FALSE)
}

## -- dispatcher: send the next request when the worker is free ----------
release_preparation_request <- reactiveVal(NULL)
observe({
  current_protocol <- protocol()
  if (is.null(current_protocol)) {
    return()
  }
  if (identical(isolate(worker())$state %||% NULL, "starting")) {
    # A cancel restarted the worker in the background; dispatch requeued
    # work once startup marks it available.
    worker_available()
    return()
  }
  dispatched <- builder_protocol_dispatch(current_protocol)
  if (is.null(dispatched$request)) {
    return()
  }
  protocol(builder_protocol_redact_auth(dispatched$protocol))
  if (!identical(dispatched$request$payload$kind, "build")) {
    dispatch_builder_worker_request(dispatched)
    return()
  }
  build_operation_phase("stage")
  busy_note("Preparing build…")
  release_preparation_request(dispatched$request$request_id)
  preparation <- tryCatch(
    builder_start_release_preparation_process(
      dispatched$request$payload$plan,
      dispatched$request$build_id
    ),
    error = identity
  )
  poll <- function() {
    if (!inherits(preparation, "condition") && preparation$is_alive()) {
      later::later(poll, delay = 0.05)
      return(invisible(TRUE))
    }
    prepared <- if (inherits(preparation, "condition")) {
      preparation
    } else {
      tryCatch(preparation$get_result(), error = identity)
    }
    current <- isolate(protocol())
    if (
      identical(
        isolate(release_preparation_request()),
        dispatched$request$request_id
      )
    ) {
      release_preparation_request(NULL)
    }
    if (
      builder_session_closed() ||
        !identical(current$pending$request_id, dispatched$request$request_id)
    ) {
      if (is.list(prepared$coordinator)) {
        try(builder_coordinator_abort(prepared$coordinator), silent = TRUE)
      }
      dispatched$request$payload$auth_accounts <- NULL
      return(invisible(FALSE))
    }
    shiny::withReactiveDomain(
      session,
      shiny::isolate(
        dispatch_builder_worker_request(dispatched, prepared)
      )
    )
    dispatched$request$payload$auth_accounts <- NULL
    invisible(TRUE)
  }
  later::later(poll, delay = 0.05)
})

dispatch_builder_worker_request <- function(dispatched, preparation = NULL) {
  dispatched$protocol <- isolate(protocol())
  request <- dispatched$request
  nxt <- request$payload
  coordinator <- NULL
  auth_material <- NULL
  build_auth_accounts <- NULL
  execution_runtime <- NULL
  handed_off <- FALSE
  if (identical(nxt$kind, "build")) {
    build_auth_accounts <- nxt$auth_accounts
    nxt$auth_accounts <- NULL
    request$payload$auth_accounts <- NULL
    dispatched$request <- builder_request_redact_auth(dispatched$request)
    dispatched$protocol <- builder_protocol_redact_auth(dispatched$protocol)
    current_protocol <- builder_protocol_redact_auth(isolate(protocol()))
    on.exit(
      {
        build_auth_accounts <- NULL
        auth_material <- NULL
        current_protocol <- NULL
        dispatched$request <- NULL
        nxt$auth_accounts <- NULL
        request$payload$auth_accounts <- NULL
        latest_protocol <- isolate(protocol())
        if (!is.null(latest_protocol)) {
          protocol(builder_protocol_redact_auth(latest_protocol))
        }
        if (!is.null(coordinator) && !handed_off) {
          try(builder_coordinator_abort(coordinator), silent = TRUE)
        }
      },
      add = TRUE
    )
    protocol(dispatched$protocol)
  } else {
    protocol(dispatched$protocol)
  }
  current_worker <- worker()
  req(current_worker)
  if (identical(nxt$kind, "load")) {
    set_import_state(
      nxt$id,
      "reading",
      nxt$import_generation %||% 1L
    )
  }
  if (identical(nxt$kind, "build")) {
    plan <- nxt$plan
    plan_error <- plan$error
    if (!is.null(plan_error)) {
      finish_builder_build_dispatch_failure(
        dispatched$protocol,
        request,
        builder_result_failure(plan_error)
      )
      return()
    }
    if (inherits(preparation, "condition")) {
      finish_builder_build_dispatch_failure(
        isolate(protocol()),
        request,
        builder_result_failure(conditionMessage(preparation))
      )
      return()
    }
    execution_runtime <- preparation$runtime
    coordinator <- preparation$coordinator
    if (builder_build_cancel_requested(nxt$progress_path %||% "")) {
      finish_builder_build_dispatch_failure(
        isolate(protocol()),
        request,
        builder_result_failure("Build cancelled. No output was published.")
      )
      return()
    }
    if (isTRUE(plan$app_auth$enabled)) {
      auth_material <- try(
        builder_auth_create_material(build_auth_accounts, coordinator$stage),
        silent = TRUE
      )
      build_auth_accounts <- NULL
      if (inherits(auth_material, "try-error")) {
        plan_error <- conditionMessage(attr(auth_material, "condition"))
        release_error <- builder_release_error_result(
          plan_error,
          plan$output_release$directory
        )
        release_error$failure_phase <- "stage"
        release_error$failure_stage <- coordinator$stage %||% NULL
        release_error$failure_build_id <- request$build_id
        finish_builder_build_dispatch_failure(
          dispatched$protocol,
          request,
          release_error
        )
        return()
      }
    }
    build_auth_accounts <- NULL
    nxt$plan <- plan
    build_operation_phase("dispatch")
    busy_note("Starting build…")
  }
  started_call <- try(
    switch(
      nxt$kind,
      load = if (identical(nxt$source, "file")) {
        builder_session_load(
          current_worker,
          nxt$id,
          if (isTRUE(nxt$retain_source)) {
            list(
              source = nxt$path,
              retained_path = nxt$retained_path,
              transport_retained_path = nxt$transport_retained_path,
              transport_owned = isTRUE(nxt$transport_owned)
            )
          } else {
            nxt$path
          },
          request,
          progress_path = nxt$progress_path,
          import_generation = nxt$import_generation %||% 1L,
          expected_md5 = nxt$expected_md5 %||% NULL,
          .importer = if (isTRUE(nxt$retain_source)) {
            builder_session_worker_callback(
              builder_project_load_retained_source
            )
          } else {
            NULL
          },
          .copy = if (isTRUE(nxt$retain_source)) {
            builder_session_worker_callback(builder_project_copy_file)
          } else {
            NULL
          },
          .transport_writer = if (isTRUE(nxt$retain_source)) {
            builder_session_worker_callback(
              builder_project_transport_retained_write
            )
          } else {
            NULL
          }
        )
      } else {
        builder_session_example(
          current_worker,
          nxt$id,
          nxt$example,
          request,
          progress_path = nxt$progress_path,
          import_generation = nxt$import_generation %||% 1L
        )
      },
      preview = builder_session_preview(
        current_worker,
        nxt$id,
        nxt$reduction,
        nxt$group,
        BUILDER_PREVIEW_MAX,
        request
      ),
      projection_previews = builder_session_projection_previews(
        current_worker,
        nxt$id,
        nxt$projections,
        nxt$group,
        nxt$max_cells %||% BUILDER_PREVIEW_MAX,
        request
      ),
      trajectory_previews = builder_session_trajectory_previews(
        current_worker,
        nxt$id,
        nxt$trajectories,
        nxt$max_cells %||% BUILDER_PREVIEW_MAX,
        request
      ),
      coords = builder_session_coords(
        current_worker,
        nxt$id,
        nxt$image,
        BUILDER_PREVIEW_MAX,
        request
      ),
      spatial_preview = builder_session_spatial_preview(
        current_worker,
        nxt$id,
        nxt$default_projection,
        nxt$group,
        nxt$roi,
        nxt$section,
        nxt$assay,
        nxt$layer,
        base_coordinate_transform = nxt$base_coordinate_transform,
        max_cells = nxt$max_cells %||% BUILDER_SPATIAL_PREVIEW_MAX,
        request = request
      ),
      align_all = builder_session_section_bounds(
        current_worker,
        nxt$id,
        nxt$sections,
        nxt$mode,
        nxt$extent_width,
        nxt$extent_height,
        nxt$um_per_px,
        nxt$dx,
        nxt$dy,
        nxt$scale,
        request
      ),
      build = builder_session_build(
        current_worker,
        nxt$plan,
        request,
        coordinator = coordinator,
        auth_material = auth_material,
        progress_path = nxt$progress_path %||% NULL,
        runtime_files = execution_runtime$files,
        runtime_identity = execution_runtime$identity
      ),
      drop = builder_session_drop(current_worker, nxt$id, request)
    ),
    silent = TRUE
  )
  if (inherits(started_call, "try-error")) {
    if (identical(nxt$kind, "build")) {
      builder_build_progress_remove(nxt$progress_path %||% "")
      release <- isolate(active_release())
      if (!is.null(release)) {
        try(builder_coordinator_abort(release$handle), silent = TRUE)
        active_release(NULL)
      }
    }
    dispatch_error <- conditionMessage(attr(started_call, "condition"))
    if (identical(nxt$kind, "load")) {
      dispatch_error <- builder_import_public_error(
        dispatch_error,
        nxt$path %||% character()
      )
    }
    restart_worker_protocol(
      current_worker,
      dispatched$protocol,
      dispatch_error
    )
    build_operation_phase(NULL)
    return()
  }
  if (identical(nxt$kind, "build")) {
    handed_off <- TRUE
    auth_material <- NULL
    active_release(list(
      id = request$build_id,
      handle = coordinator,
      plan = plan,
      runtime_files = execution_runtime$files,
      runtime_identity = execution_runtime$identity
    ))
  }
  busy_note(
    if (identical(nxt$kind, "build")) {
      "Starting build…"
    } else {
      nxt$note
    }
  )
}

fail_spatial_preview <- function(payload) {
  if (!identical(payload$kind, "spatial_preview")) {
    return(invisible(FALSE))
  }
  failure <- builder_spatial_preview_failure(
    isolate(spatial_previews()),
    payload
  )
  spatial_previews(failure$cache)
  if (isTRUE(failure$matched)) {
    if (
      exists("alignment_server", inherits = TRUE) &&
        is.function(alignment_server$fail_preview_switch)
    ) {
      alignment_server$fail_preview_switch(
        payload$id,
        payload$section,
        payload$switch_token
      )
    } else {
      session$sendCustomMessage(
        "builder_dataset_switch_state",
        failure$message
      )
    }
  }
  invisible(TRUE)
}

builder_release_package_runtime <- function(builder_root) {
  source_root <- normalizePath(
    file.path(builder_root, "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  source_checkout <- file.exists(file.path(source_root, "DESCRIPTION")) &&
    dir.exists(file.path(source_root, "R"))
  if (!source_checkout) {
    source_root <- .builder_worker_package_source(builder_root)
  }
  if (is.null(source_root)) {
    return(list(source_root = NULL, files = character()))
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
  if (!length(files) || any(!file.exists(files))) {
    stop("The source-tree release runtime is incomplete.", call. = FALSE)
  }
  list(source_root = source_root, files = files)
}

builder_release_execution_runtime <- function(hash = TRUE) {
  release <- builder_release_runtime_files()
  package <- builder_release_package_runtime(release$root)
  installed_package_files <- character()
  if (is.null(package$source_root)) {
    installed_root <- system.file(package = "CerebroNexus")
    installed_candidates <- c(
      file.path(installed_root, "DESCRIPTION"),
      file.path(installed_root, "NAMESPACE"),
      list.files(
        file.path(installed_root, "R"),
        recursive = TRUE,
        full.names = TRUE
      ),
      list.files(
        file.path(installed_root, "libs"),
        recursive = TRUE,
        full.names = TRUE
      )
    )
    installed_package_files <- installed_candidates[
      file.exists(installed_candidates) &
        !dir.exists(installed_candidates) &
        utils::file_test("-f", installed_candidates)
    ]
    if (!length(installed_package_files)) {
      stop("The installed release runtime is incomplete.", call. = FALSE)
    }
  }
  builder_files <- list.files(
    release$root,
    pattern = "[.][Rr]$",
    full.names = TRUE
  )
  app_bundle_files <- list.files(
    file.path(release$root, "app_bundle"),
    pattern = "[.][Rr]$",
    full.names = TRUE
  )
  viewer_files <- list.files(
    file.path(dirname(release$root), "viewer"),
    pattern = "[.][Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  files <- unique(c(
    unlist(release[setdiff(names(release), "root")], use.names = FALSE),
    builder_files,
    app_bundle_files,
    viewer_files,
    package$files,
    installed_package_files
  ))
  list(
    files = normalizePath(files, winslash = "/", mustWork = TRUE),
    identity = if (hash) builder_runtime_file_identity(files) else NULL
  )
}

## -- coordinator processes ---------------------------------------------------
## Hashes, release preflight and settlement run in trusted coordinator
## processes, separate from both Shiny and the data worker. Each one used to
## start cold (library(CerebroNexus) alone is ~5 s from an installed package)
## and hash all ~250 runtime files again. A process is now started ahead of
## need: it loads and hashes the runtime while the user picks a folder or the
## worker builds, then waits for its one job in a private inbox file. Runtime
## MD5s are reused while file size and modification time are unchanged. Each
## process still runs exactly one preparation or settlement, and first checks
## that the code it loaded is still the runtime on disk.

builder_coordinator_process_state <- new.env(parent = emptyenv())
builder_coordinator_process_state$warm <- list()

builder_coordinator_idle_seconds <- function() {
  value <- suppressWarnings(as.numeric(getOption(
    "CerebroNexus.builder.coordinator_idle_seconds",
    900
  ))[1L])
  if (!is.finite(value) || value <= 0) 900 else value
}

builder_coordinator_private_dir <- function() {
  path <- file.path(tempdir(), "builder-coordinator")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, mode = "0700", showWarnings = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

builder_coordinator_runtime_cache_path <- function() {
  file.path(builder_coordinator_private_dir(), "runtime-identity.rds")
}

builder_coordinator_runtime_previous <- function() {
  path <- builder_coordinator_runtime_cache_path()
  value <- if (file.exists(path)) {
    tryCatch(readRDS(path), error = function(error) NULL)
  } else {
    NULL
  }
  if (is.list(value) && is.list(value$identity) && is.list(value$stats)) {
    value
  } else {
    NULL
  }
}

# A fresh coordinator byte-compiles each function the first time it runs:
# seconds of a Build's Verify and Publish steps. While it waits for its job it
# compiles the release, App and path-contract functions those steps run, one
# per call so an arriving job never waits behind it. Other functions are left
# to the JIT so an idle process does not compete with the worker for CPU.
# `pending = NULL` starts the queue; returns the names still to compile.
builder_coordinator_compile_ahead <- function(runtime, pending = NULL) {
  if (is.null(pending)) {
    names <- ls(runtime, all.names = TRUE)
    release <- grepl(
      paste0(
        "^[.]?builder_(coordinator|release|app|publish|verify|auth)|",
        "^[.](canonical|resolveNative|normalizeExisting|readNative|pathWithin|",
        "absolutePath|composeAbsolute|nativ|lexical|builderRandom)"
      ),
      names
    )
    return(names[release])
  }
  if (!length(pending)) {
    return(character())
  }
  value <- get0(pending[[1L]], envir = runtime, inherits = FALSE)
  if (is.function(value) && !is.primitive(value)) {
    compiled <- tryCatch(compiler::cmpfun(value), error = function(error) NULL)
    if (is.function(compiled)) {
      assign(pending[[1L]], compiled, envir = runtime)
    }
  }
  pending[-1L]
}

# Runs in the coordinator process: load and hash the runtime, then wait for
# the job. Self-contained; callr evaluates it in a fresh global environment.
builder_coordinator_process_main <- function(
  inbox,
  runtime_files,
  package_files,
  package_source,
  execution_files,
  previous,
  cache_path,
  parent_pid,
  idle_seconds,
  runtime_identity,
  source_utf8,
  compile_ahead = NULL
) {
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(left, right) if (is.null(left)) right else left
  stats <- new.env(parent = emptyenv())
  loaded_identity <- runtime_identity(
    execution_files,
    .previous = previous,
    .stats = stats
  )
  if (is.null(package_source) || !length(package_files)) {
    suppressPackageStartupMessages(library(CerebroNexus))
  } else {
    Sys.setenv(CEREBRO_PACKAGE_SOURCE = package_source)
    for (path in package_files) {
      source_utf8(path, envir = runtime)
    }
  }
  for (name in setdiff(names(runtime_files), "root")) {
    source_utf8(runtime_files[[name]], envir = runtime)
  }
  if (!is.null(package_source) && length(package_files)) {
    validator <- get0(
      ".viewerAuthValidateDatabase",
      envir = runtime,
      inherits = FALSE
    )
    verify_pair <- get0(
      "builder_auth_verify_database_pair",
      envir = runtime,
      inherits = FALSE
    )
    if (!is.function(validator) || !is.function(verify_pair)) {
      stop("The source-tree authentication runtime is incomplete.")
    }
    verify_formals <- formals(verify_pair)
    verify_formals$.validate <- quote(.viewerAuthValidateDatabase)
    formals(verify_pair) <- verify_formals
    assign("builder_auth_verify_database_pair", verify_pair, envir = runtime)
  }
  loaded <- list(identity = loaded_identity, stats = stats$stats)
  try(
    {
      temporary <- tempfile(".runtime-", tmpdir = dirname(cache_path))
      saveRDS(loaded, temporary, version = 2)
      if (!file.rename(temporary, cache_path)) {
        unlink(temporary, force = TRUE)
      }
    },
    silent = TRUE
  )
  deadline <- Sys.time() + idle_seconds
  checked_parent <- Sys.time()
  pending_compile <- if (is.function(compile_ahead)) {
    compile_ahead(runtime)
  } else {
    character()
  }
  while (!file.exists(inbox)) {
    if (Sys.time() > deadline) {
      return(list(idle = TRUE))
    }
    if (as.numeric(Sys.time() - checked_parent, units = "secs") > 2) {
      checked_parent <- Sys.time()
      alive <- tryCatch(
        ps::pid_exists(parent_pid),
        error = function(error) TRUE
      )
      if (!isTRUE(alive)) {
        return(list(idle = TRUE))
      }
    }
    if (length(pending_compile)) {
      pending_compile <- compile_ahead(runtime, pending_compile)
      next
    }
    Sys.sleep(0.05)
  }
  job <- readRDS(inbox)
  unlink(inbox, force = TRUE)
  # The loaded code must still be the runtime on disk.
  current <- runtime_identity(execution_files, .previous = loaded)
  if (!identical(current, loaded$identity)) {
    stop(
      "The Builder runtime changed while its coordinator was starting. ",
      "Nothing was published; try again.",
      call. = FALSE
    )
  }
  job$run(runtime, loaded$identity, job$args)
}

builder_coordinator_spawn <- function(kind) {
  runtime_files <- builder_release_runtime_files()
  package_runtime <- builder_release_package_runtime(runtime_files$root)
  execution <- builder_release_execution_runtime(hash = FALSE)
  # Sent to the process; it must not carry this session's environment along.
  compile_ahead <- builder_coordinator_compile_ahead
  environment(compile_ahead) <- globalenv()
  inbox <- tempfile(
    paste0(kind, "-inbox-"),
    tmpdir = builder_coordinator_private_dir(),
    fileext = ".rds"
  )
  process <- callr::r_bg(
    builder_coordinator_process_main,
    args = list(
      inbox = inbox,
      runtime_files = runtime_files,
      package_files = package_runtime$files,
      package_source = package_runtime$source_root,
      execution_files = execution$files,
      previous = builder_coordinator_runtime_previous(),
      cache_path = builder_coordinator_runtime_cache_path(),
      parent_pid = Sys.getpid(),
      idle_seconds = builder_coordinator_idle_seconds(),
      runtime_identity = builder_runtime_file_identity,
      source_utf8 = builder_source_utf8,
      compile_ahead = compile_ahead
    ),
    # Preparation transfers release ownership to this process before it exits
    # and must not be killed with it; settlement is supervised.
    supervise = identical(kind, "settlement"),
    cleanup = !identical(kind, "preparation"),
    stdout = "|",
    stderr = "|"
  )
  list(kind = kind, process = process, inbox = inbox, files = execution$files)
}

builder_coordinator_warm <- function(kind) {
  warm <- builder_coordinator_process_state$warm[[kind]]
  alive <- is.list(warm) &&
    isTRUE(tryCatch(warm$process$is_alive(), error = function(error) FALSE))
  if (!alive) {
    warm <- tryCatch(
      builder_coordinator_spawn(kind),
      error = function(error) NULL
    )
    builder_coordinator_process_state$warm[[kind]] <- warm
  }
  invisible(warm)
}

builder_prewarm_release_processes <- function(
  kinds = c("preparation", "settlement")
) {
  for (kind in kinds) {
    builder_coordinator_warm(kind)
  }
  invisible(TRUE)
}

builder_coordinator_stop_warm <- function() {
  for (warm in builder_coordinator_process_state$warm) {
    if (is.list(warm)) {
      try(warm$process$kill(), silent = TRUE)
    }
  }
  builder_coordinator_process_state$warm <- list()
  invisible(TRUE)
}

session$onSessionEnded(builder_coordinator_stop_warm)

builder_coordinator_submit <- function(kind, run, args) {
  warm <- builder_coordinator_warm(kind)
  if (!is.list(warm)) {
    stop("The release coordinator could not be started.", call. = FALSE)
  }
  builder_coordinator_process_state$warm[[kind]] <- NULL
  environment(run) <- globalenv()
  temporary <- tempfile(".job-", tmpdir = dirname(warm$inbox))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(list(run = run, args = args), temporary, version = 2)
  Sys.chmod(temporary, mode = "0600")
  if (!file.rename(temporary, warm$inbox)) {
    try(warm$process$kill(), silent = TRUE)
    stop("The release coordinator job could not be delivered.", call. = FALSE)
  }
  warm$process
}

builder_start_release_preparation_process <- function(plan, build_id) {
  process <- builder_coordinator_submit(
    "preparation",
    function(runtime, identity, args) {
      handle <- runtime$builder_coordinator_prepare(args$plan, args$build_id)
      transferred <- FALSE
      on.exit(
        if (!transferred) {
          try(runtime$builder_coordinator_abort(handle), silent = TRUE)
        }
      )
      handle <- runtime$builder_coordinator_claim(handle, args$parent_pid)
      transferred <- TRUE
      list(
        runtime = list(files = args$files, identity = identity),
        coordinator = handle
      )
    },
    list(
      plan = plan,
      build_id = build_id,
      files = builder_release_execution_runtime(hash = FALSE)$files,
      parent_pid = Sys.getpid()
    )
  )
  # Settlement loads while the worker builds.
  builder_coordinator_warm("settlement")
  process
}

builder_start_release_settlement_process <- function(
  release,
  value,
  progress_path = NULL,
  retention_root = builder_build_retention_root()
) {
  builder_coordinator_submit(
    "settlement",
    function(runtime, loaded_identity, args) {
      value <- args$value
      successful_build <- is.list(value) &&
        identical(value$state, "success") &&
        isTRUE(value$publishable)
      if (successful_build) {
        runtime$builder_runtime_assert_identity(
          args$expected_runtime_identity,
          args$worker_runtime_identity,
          context = "worker completion"
        )
        runtime$builder_runtime_assert_identity(
          args$expected_runtime_identity,
          loaded_identity,
          context = "parent verification"
        )
      }
      progress_path <- args$progress_path
      handle <- runtime$builder_coordinator_claim(args$handle)
      report_progress <- function(phase, detail = NULL) {
        if (
          !is.character(progress_path) ||
            length(progress_path) != 1L ||
            is.na(progress_path) ||
            !nzchar(progress_path) ||
            !phase %in% c("verify", "publish")
        ) {
          return(invisible(FALSE))
        }
        temporary <- tempfile(".progress-", tmpdir = dirname(progress_path))
        on.exit(unlink(temporary, force = TRUE), add = TRUE)
        # A detail names the verification sub-step for the Build card.
        record <- if (is.list(detail) && length(detail)) {
          c(list(schema_version = 1L, phase = phase), detail)
        } else {
          phase
        }
        saveRDS(record, temporary, version = 2)
        try(Sys.chmod(temporary, mode = "0600"), silent = TRUE)
        moved <- isTRUE(file.rename(temporary, progress_path))
        if (!moved && file.exists(progress_path)) {
          unlink(progress_path, force = TRUE)
          moved <- isTRUE(file.rename(temporary, progress_path))
        }
        invisible(moved)
      }
      preserve_on_abort <- !is.list(value) ||
        !identical(value$state, "needs_decision")
      abort_with_diagnostics <- function(handle) {
        if (isTRUE(preserve_on_abort)) {
          try(
            {
              diagnostics <- runtime$.builder_release_atomic_directory(
                handle$control
              )
              marker_id <- gsub(
                "[^A-Za-z0-9._-]",
                "-",
                substr(handle$build_id, 1L, 80L)
              )
              marker <- file.path(
                diagnostics,
                paste0("settlement-pending-", marker_id)
              )
              if (!dir.exists(marker) && !file.exists(marker)) {
                dir.create(marker, mode = "0700", showWarnings = FALSE)
              }
            },
            silent = TRUE
          )
        }
        runtime$builder_coordinator_abort(handle)
      }
      publish_with_progress <- function(handle, build_result) {
        runtime$builder_coordinator_publish(
          handle,
          build_result,
          .on_progress = report_progress
        )
      }
      if (successful_build) {
        report_progress("verify")
      }
      if (is.list(value) && identical(value$state, "needs_decision")) {
        retained <- tryCatch(
          runtime$builder_coordinator_retain_partial(
            handle,
            value,
            args$retention_root
          ),
          error = function(error) list()
        )
        if (length(retained)) {
          value$retained_artifacts <- retained
        }
      }
      runtime$builder_coordinator_settle(
        handle,
        value,
        .publish = publish_with_progress,
        .abort = abort_with_diagnostics
      )
    },
    list(
      handle = release$handle,
      value = value,
      expected_runtime_identity = release$runtime_identity,
      worker_runtime_identity = value$runtime_identity %||% NULL,
      progress_path = progress_path,
      retention_root = retention_root
    )
  )
}

builder_release_settlement_result <- function(
  settled,
  release,
  worker_result = NULL
) {
  target <- release$handle$target %||% ""
  has_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  worker_error <- NULL
  if (is.list(worker_result)) {
    for (field in c("error", "message")) {
      candidate <- worker_result[[field]]
      if (has_text(candidate)) {
        worker_error <- candidate
        break
      }
    }
  }
  preserve_settlement_error <- function(result, settlement_error) {
    if (
      is.list(result) &&
        has_text(worker_error) &&
        has_text(settlement_error) &&
        !identical(worker_error, settlement_error)
    ) {
      result$settlement_error <- settlement_error
    }
    result
  }
  if (inherits(settled, "condition")) {
    try(builder_coordinator_abort(release$handle), silent = TRUE)
    settlement_error <- paste0(
      "The completed Build could not be published: ",
      conditionMessage(settled)
    )
    return(preserve_settlement_error(
      builder_release_error_result(worker_error %||% settlement_error, target),
      settlement_error
    ))
  }
  if (
    !is.list(settled) ||
      !is.logical(settled$ok) ||
      length(settled$ok) != 1L ||
      is.na(settled$ok)
  ) {
    try(builder_coordinator_abort(release$handle), silent = TRUE)
    settlement_error <- "The release process returned an invalid result."
    return(preserve_settlement_error(
      builder_release_error_result(worker_error %||% settlement_error, target),
      settlement_error
    ))
  }
  if (!isTRUE(settled$ok)) {
    recovery <- settled$recovery
    settlement_error <- settled$settlement_error %||%
      settled$error %||%
      "The completed Build could not be published."
    primary_error <- worker_error %||% settlement_error
    if (
      is.list(recovery) &&
        identical(recovery$state, "recovery_required")
    ) {
      return(preserve_settlement_error(
        builder_result_recovery_required(
          primary_error,
          recovery = recovery,
          recovery_message = recovery$message %||% NULL,
          cleanup_error = settled$cleanup_error %||% NULL,
          recovery_error = settled$recovery_error %||% NULL
        ),
        settlement_error
      ))
    }
    return(preserve_settlement_error(
      builder_result_failure(
        primary_error,
        cleanup_error = settled$cleanup_error %||% NULL,
        recovery_error = settled$recovery_error %||% NULL
      ),
      settlement_error
    ))
  }
  typed <- try(builder_as_result(settled$value), silent = TRUE)
  if (inherits(typed, "try-error")) {
    settlement_error <-
      "The release process returned an unsupported Build result."
    return(preserve_settlement_error(
      builder_result_failure(worker_error %||% settlement_error),
      settlement_error
    ))
  }
  typed
}

builder_preserve_build_failure <- function(value, settlement_error) {
  if (
    is.list(value) &&
      (identical(value$state, "failure") ||
        identical(value$state, "recovery_required")) &&
      .builder_diagnostics_text(value$error %||% "")
  ) {
    if (!.builder_diagnostics_text(value$settlement_error %||% "")) {
      value$settlement_error <- settlement_error
    }
    return(value)
  }
  failure <- builder_result_failure(settlement_error)
  if (is.list(value)) {
    for (field in c("diagnostics", "diagnostics_path")) {
      if (!is.null(value[[field]])) {
        failure[[field]] <- value[[field]]
      }
    }
  }
  failure
}

acknowledge_builder_build <- function(request_id) {
  acknowledged <- try(
    builder_protocol_acknowledge(isolate(protocol()), request_id),
    silent = TRUE
  )
  if (inherits(acknowledged, "try-error")) {
    protocol(NULL)
    worker_available(FALSE)
    add_error(paste0(
      "The completed Build could not be acknowledged. ",
      "Restart this Builder session."
    ))
    return(invisible(FALSE))
  }
  protocol(acknowledged)
  invisible(TRUE)
}

builder_persist_release_process_diagnostics <- function(
  context,
  settlement_result,
  process = NULL,
  kind = "settlement"
) {
  fallback <- list(
    path = NULL,
    capture = if (is.list(context)) context$diagnostics else NULL
  )
  tryCatch(
    {
      if (!is.list(context) || !is.list(context$release)) {
        return(fallback)
      }
      capture <- builder_diagnostics_drain(process, context$diagnostics)
      process_facts <- builder_diagnostics_process_facts(process)
      payload <- list(
        schema_version = 1L,
        worker_result = context$worker_result,
        worker_transport = context$worker_transport,
        settlement_result = settlement_result,
        process = process_facts,
        recorded_at = Sys.time()
      )
      list(
        path = builder_diagnostics_persist_release(
          context$release,
          result = payload,
          capture = capture,
          process = process,
          kind = kind
        ),
        capture = capture
      )
    },
    error = function(error) fallback
  )
}

builder_persist_worker_transport_failure <- function(
  release,
  worker_result,
  worker_transport,
  process = NULL
) {
  fallback <- list(path = NULL, capture = NULL)
  tryCatch(
    {
      if (!is.list(release) || !is.list(release$handle)) {
        return(fallback)
      }
      context <- list(
        release = release,
        worker_result = worker_result,
        worker_transport = worker_transport,
        diagnostics = builder_diagnostics_merge_transport(
          builder_diagnostics_capture(),
          worker_transport
        )
      )
      condition <- if (
        is.list(worker_transport) &&
          inherits(worker_transport$error_condition, "condition")
      ) {
        worker_transport$error_condition
      } else {
        simpleError(
          worker_result$error %||% "The background worker failed."
        )
      }
      builder_persist_release_process_diagnostics(
        context,
        condition,
        process = process,
        kind = "worker-transport-failed"
      )
    },
    error = function(error) fallback
  )
}

finish_builder_release_settlement <- function(settled) {
  context <- isolate(release_settlement_context())
  process <- isolate(release_settlement_process())
  diagnostics_path <- NULL
  if (
    is.list(context) &&
      isTRUE(builder_diagnostics_settlement_failed(settled))
  ) {
    recorded <- builder_persist_release_process_diagnostics(
      context,
      settled,
      process = process
    )
    context$diagnostics <- recorded$capture
    diagnostics_path <- recorded$path
    pending <- file.path(
      context$release$handle$control,
      "diagnostics",
      paste0(
        "settlement-pending-",
        gsub(
          "[^A-Za-z0-9._-]",
          "-",
          substr(context$release$handle$build_id, 1L, 80L)
        )
      )
    )
    if (
      dir.exists(pending) &&
        !.builder_diagnostics_link(pending) &&
        !length(list.files(pending, all.files = TRUE, no.. = TRUE))
    ) {
      unlink(pending, recursive = FALSE, force = TRUE)
    }
    if (.builder_diagnostics_text(diagnostics_path %||% NULL)) {
      try(
        message("Builder release diagnostics saved to ", diagnostics_path),
        silent = TRUE
      )
    }
  }
  if (
    is.list(settled) &&
      isTRUE(settled$abort_required) &&
      is.list(context) &&
      is.list(context$release) &&
      is.list(context$release$handle)
  ) {
    aborted <- tryCatch(
      builder_coordinator_abort(context$release$handle),
      error = identity
    )
    if (inherits(aborted, "condition")) {
      settled$cleanup_error <- conditionMessage(aborted)
    } else if (!isTRUE(aborted$aborted)) {
      settled$cleanup_error <-
        "The assigned build stage could not be cleaned."
    }
    recovered <- tryCatch(
      builder_coordinator_recovery(context$release$handle$target),
      error = identity
    )
    if (inherits(recovered, "condition")) {
      settled$recovery_error <- conditionMessage(recovered)
    } else {
      settled$recovery <- recovered
    }
    settled$abort_required <- NULL
  }
  release_settlement_poll_generation(
    as.double(isolate(release_settlement_poll_generation())) + 1
  )
  release_settlement_poll_scheduled(FALSE)
  release_settlement_process(NULL)
  release_settlement_context(NULL)
  if (is.list(context)) {
    builder_build_progress_remove(context$progress_path %||% "")
  }
  if (!is.list(context) || !is.list(context$release)) {
    busy_note(NULL)
    add_error("The completed Build lost its release context.")
    return(invisible(FALSE))
  }
  value <- builder_release_settlement_result(
    settled,
    context$release,
    context$worker_result
  )
  if (
    is.list(value) &&
      (identical(value$state, "failure") ||
        identical(value$state, "recovery_required"))
  ) {
    value$failure_phase <- context$phase %||% "verify"
    value$failure_stage <- context$release$handle$stage %||% NULL
    value$failure_build_id <- context$build_id %||% NULL
  }
  if (
    is.list(value) &&
      .builder_diagnostics_text(diagnostics_path %||% NULL)
  ) {
    value$diagnostics_path <- diagnostics_path
  }
  active <- isolate(active_release())
  if (
    is.null(active) ||
      !identical(active$id, context$build_id)
  ) {
    value <- builder_preserve_build_failure(
      value,
      "The completed Build no longer matches its release coordinator."
    )
  }
  result(value)
  if (identical(value$state, "success") && isTRUE(value$published)) {
    selected_output(NULL)
  }
  active_release(NULL)
  acknowledge_builder_build(context$request_id)
  busy_note(NULL)
  build_operation_phase(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  invisible(TRUE)
}

builder_release_settlement_monitor_limit <- 3L

builder_release_settlement_timeout <- function(
  value = getOption("cerebro.builder.release_settlement_timeout", 7200)
) {
  value <- suppressWarnings(as.numeric(value)[1L])
  if (!is.finite(value) || value <= 0) {
    return(7200)
  }
  value
}

builder_release_settlement_timed_out <- function(context, now = Sys.time()) {
  is.list(context) &&
    identical(context$status, "settling") &&
    inherits(context$deadline, "POSIXt") &&
    isTRUE(now >= context$deadline)
}

fail_builder_release_settlement_monitor <- function(message) {
  context <- isolate(release_settlement_context())
  process <- isolate(release_settlement_process())
  if (!is.list(context) || !is.list(context$release)) {
    busy_note(NULL)
    build_operation_phase(NULL)
    build_flow(list(stage = "idle", plan = NULL))
    add_error(message)
    return(invisible(FALSE))
  }
  if (identical(context$status, "monitor_failed")) {
    return(invisible(FALSE))
  }
  context$status <- "monitor_failed"
  context$error <- message
  release_settlement_context(context)
  release_settlement_poll_generation(
    as.double(isolate(release_settlement_poll_generation())) + 1
  )
  release_settlement_poll_scheduled(FALSE)

  if (!is.null(process)) {
    try(process$kill_tree(), silent = TRUE)
    try(process$kill(), silent = TRUE)
    try(process$wait(timeout = 2000L), silent = TRUE)
    alive <- tryCatch(process$is_alive(), error = identity)
    if (identical(alive, FALSE)) {
      return(finish_builder_release_cancellation())
    }
  }

  recorded <- builder_persist_release_process_diagnostics(
    context,
    simpleError(message),
    process = process,
    kind = "settlement-monitor-failed"
  )
  recovery_message <- paste(
    "Release finalization could not be monitored safely.",
    "The coordinator remains locked; restart Builder and use release recovery."
  )
  failure <- builder_result_recovery_required(
    message,
    recovery = list(
      state = "recovery_required",
      target = context$release$handle$target %||% NULL,
      message = recovery_message
    ),
    recovery_message = recovery_message,
    diagnostics_path = recorded$path,
    failure_phase = context$phase %||% "verify",
    failure_stage = context$release$handle$stage %||% NULL,
    failure_build_id = context$build_id %||% NULL
  )
  result(failure)
  acknowledge_builder_build(context$request_id)
  build_operation_phase(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  busy_note("Release recovery is required.")
  invisible(FALSE)
}

schedule_builder_release_settlement_poll <- function(
  delay = 0.1,
  .schedule = later::later
) {
  if (isTRUE(isolate(release_settlement_poll_scheduled()))) {
    return(invisible(FALSE))
  }
  generation <- as.double(isolate(release_settlement_poll_generation()))
  release_settlement_poll_scheduled(TRUE)
  scheduled <- tryCatch(
    {
      .schedule(
        function() {
          if (builder_session_closed()) {
            return(invisible(FALSE))
          }
          if (
            !identical(
              generation,
              as.double(isolate(release_settlement_poll_generation()))
            )
          ) {
            return(invisible(FALSE))
          }
          release_settlement_poll_scheduled(FALSE)
          shiny::withReactiveDomain(
            builder_lifecycle_session,
            poll_builder_release_settlement()
          )
        },
        delay = max(0, as.numeric(delay))
      )
      TRUE
    },
    error = identity
  )
  if (inherits(scheduled, "condition")) {
    release_settlement_poll_scheduled(FALSE)
    fail_builder_release_settlement_monitor(paste0(
      "Release cleanup could not be scheduled: ",
      conditionMessage(scheduled)
    ))
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

builder_abort_active_release <- function(expected_id = NULL) {
  active <- isolate(active_release())
  if (is.null(active)) {
    return(invisible(TRUE))
  }
  if (!is.null(expected_id) && !identical(active$id, expected_id)) {
    return(invisible(FALSE))
  }
  aborted <- try(builder_coordinator_abort(active$handle), silent = TRUE)
  if (inherits(aborted, "try-error") || !isTRUE(aborted$aborted)) {
    return(invisible(FALSE))
  }
  active_release(NULL)
  invisible(TRUE)
}

finish_builder_release_cancellation <- function() {
  context <- isolate(release_settlement_context())
  process <- isolate(release_settlement_process())
  release_settlement_poll_generation(
    as.double(isolate(release_settlement_poll_generation())) + 1
  )
  release_settlement_poll_scheduled(FALSE)
  if (!is.list(context) || !is.list(context$release)) {
    release_settlement_process(NULL)
    context <- list(
      status = "cancel_failed",
      error = "The stopped release process lost its coordinator context."
    )
    release_settlement_context(context)
    build_operation_phase(NULL)
    busy_note("Release recovery is required.")
    return(invisible(FALSE))
  }
  aborted <- builder_abort_active_release(context$build_id)
  if (!isTRUE(aborted)) {
    context$status <- "cancel_failed"
    context$error <- paste(
      "The release process stopped, but its coordinator could not be aborted.",
      "The release remains locked for recovery."
    )
    recorded <- builder_persist_release_process_diagnostics(
      context,
      list(cancelled = TRUE, error = context$error),
      process = process,
      kind = "settlement-cancel-failed"
    )
    context$diagnostics <- recorded$capture
    context$diagnostics_path <- recorded$path
    release_settlement_process(NULL)
    release_settlement_context(context)
    busy_note("Release recovery is required.")
    if (!builder_session_closed()) {
      add_error(context$error)
    }
    return(invisible(FALSE))
  }
  diagnostics_path <- NULL
  if (.builder_diagnostics_text(context$error %||% NULL)) {
    recorded <- builder_persist_release_process_diagnostics(
      context,
      list(cancelled = TRUE, error = context$error),
      process = process,
      kind = "settlement-cancel-warning"
    )
    context$diagnostics <- recorded$capture
    diagnostics_path <- recorded$path
  }
  release_settlement_process(NULL)
  release_settlement_context(NULL)
  builder_build_progress_remove(context$progress_path %||% "")
  acknowledge_builder_build(context$request_id)
  busy_note(NULL)
  build_operation_phase(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  if (
    .builder_diagnostics_text(context$error %||% NULL) &&
      !builder_session_closed()
  ) {
    failure <- builder_result_failure(context$error)
    failure$failure_phase <- context$phase %||% "verify"
    failure$failure_stage <- context$release$handle$stage %||% NULL
    failure$failure_build_id <- context$build_id %||% NULL
    if (.builder_diagnostics_text(diagnostics_path %||% NULL)) {
      failure$diagnostics_path <- diagnostics_path
    }
    result(failure)
  }
  invisible(TRUE)
}

poll_builder_release_settlement <- function() {
  process <- isolate(release_settlement_process())
  if (is.null(process)) {
    return(invisible(FALSE))
  }
  context <- isolate(release_settlement_context())
  if (builder_release_settlement_timed_out(context)) {
    return(stop_builder_release_settlement(paste(
      "Release finalization exceeded its time limit.",
      "The staged output was not published."
    )))
  }
  if (is.list(context)) {
    phase <- builder_build_progress_read(context$progress_path %||% "")
    if (!is.null(phase) && phase %in% c("verify", "publish")) {
      context$phase <- phase
      build_operation_phase(phase)
      busy_note(builder_build_progress_note(context$release$plan, phase))
    }
  }
  if (is.list(context)) {
    drained <- tryCatch(
      builder_diagnostics_drain(process, context$diagnostics),
      error = function(error) context$diagnostics
    )
    context$diagnostics <- drained
    release_settlement_context(context)
  }
  cancelling <- is.list(context) && identical(context$status, "cancelling")
  alive <- tryCatch(process$is_alive(), error = identity)
  if (
    inherits(alive, "condition") ||
      !is.logical(alive) ||
      length(alive) != 1L ||
      is.na(alive)
  ) {
    if (is.list(context)) {
      context$liveness_failures <- as.integer(
        context$liveness_failures %||% 0L
      ) +
        1L
      context$error <- if (inherits(alive, "condition")) {
        paste0(
          "The release process state could not be confirmed: ",
          conditionMessage(alive)
        )
      } else {
        "The release process returned an invalid liveness state."
      }
      release_settlement_context(context)
      if (
        context$liveness_failures >= builder_release_settlement_monitor_limit
      ) {
        return(fail_builder_release_settlement_monitor(context$error))
      }
    }
    schedule_builder_release_settlement_poll(
      delay = if (cancelling) 0.5 else 0.1
    )
    return(invisible(TRUE))
  }
  if (is.list(context) && !is.null(context$liveness_failures)) {
    context$liveness_failures <- 0L
    release_settlement_context(context)
  }
  if (isTRUE(alive)) {
    if (cancelling) {
      try(process$kill_tree(), silent = TRUE)
      try(process$kill(), silent = TRUE)
      if (Sys.time() >= (context$cancel_deadline %||% Sys.time())) {
        context$stop_warning <- paste(
          "The release process has not stopped yet.",
          "Its coordinator remains locked while cleanup continues."
        )
        release_settlement_context(context)
        busy_note("Still stopping release finalization…")
      }
    }
    schedule_builder_release_settlement_poll(
      delay = if (cancelling) 0.5 else 0.1
    )
    return(invisible(TRUE))
  }
  if (cancelling) {
    return(finish_builder_release_cancellation())
  }
  settled <- tryCatch(process$get_result(), error = identity)
  finish_builder_release_settlement(settled)
}

start_builder_release_settlement <- function(
  release,
  value,
  request,
  worker_transport = NULL
) {
  existing_process <- isolate(release_settlement_process())
  if (!is.null(existing_process)) {
    existing_context <- isolate(release_settlement_context())
    if (
      is.list(existing_context) &&
        identical(existing_context$request_id, request$request_id)
    ) {
      return(invisible(TRUE))
    }
    add_error("Another release is already being finalized.")
    return(invisible(FALSE))
  }
  if (
    !is.list(release) ||
      !is.list(release$handle) ||
      !identical(release$id, request$build_id)
  ) {
    actual_release <- isolate(active_release())
    mismatch <- builder_preserve_build_failure(
      value,
      "The completed Build no longer matches its release coordinator."
    )
    settled <- if (is.list(actual_release) && is.list(actual_release$handle)) {
      recorded <- builder_persist_worker_transport_failure(
        actual_release,
        mismatch,
        worker_transport
      )
      if (.builder_diagnostics_text(recorded$path %||% NULL)) {
        mismatch$diagnostics_path <- recorded$path
      }
      builder_release_settlement_result(
        builder_coordinator_settle(actual_release$handle, mismatch),
        actual_release,
        mismatch
      )
    } else {
      mismatch
    }
    result(settled)
    acknowledge_builder_build(request$request_id)
    active_release(NULL)
    busy_note(NULL)
    build_operation_phase(NULL)
    build_flow(list(stage = "idle", plan = NULL))
    return(invisible(FALSE))
  }
  progress_path <- request$payload$progress_path %||% NULL
  settlement_phase <- if (
    is.list(value) &&
      (identical(value$state, "failure") ||
        identical(value$state, "recovery_required")) &&
      is.character(value$failure_phase) &&
      length(value$failure_phase) == 1L
  ) {
    value$failure_phase
  } else {
    "verify"
  }
  builder_build_progress_remove(progress_path %||% "")
  if (identical(settlement_phase, "verify")) {
    builder_build_progress_write(progress_path, "verify")
  }
  build_operation_phase(settlement_phase)
  busy_note(
    if (identical(settlement_phase, "verify")) {
      "Verifying staged output…"
    } else {
      "Cleaning the failed build stage…"
    }
  )
  started_at <- Sys.time()
  release_settlement_context(list(
    release = release,
    build_id = request$build_id,
    request_id = request$request_id,
    status = "settling",
    started_at = started_at,
    deadline = started_at + builder_release_settlement_timeout(),
    phase = settlement_phase,
    progress_path = progress_path,
    worker_result = value,
    worker_transport = worker_transport,
    diagnostics = builder_diagnostics_merge_transport(
      builder_diagnostics_capture(),
      worker_transport
    )
  ))
  process <- tryCatch(
    builder_start_release_settlement_process(
      release,
      value,
      progress_path = progress_path
    ),
    error = identity
  )
  if (inherits(process, "condition")) {
    settled <- list(
      ok = FALSE,
      value = NULL,
      error = paste0(
        "The release process could not start: ",
        conditionMessage(process)
      ),
      target = release$handle$target,
      recovery = NULL,
      abort_required = TRUE
    )
    finish_builder_release_settlement(settled)
    return(invisible(FALSE))
  }
  release_settlement_process(process)
  schedule_builder_release_settlement_poll(delay = 0)
  invisible(TRUE)
}

stop_builder_release_settlement <- function(reason = NULL) {
  process <- isolate(release_settlement_process())
  if (is.null(process)) {
    return(is.null(isolate(release_settlement_context())))
  }
  context <- isolate(release_settlement_context())
  if (!is.list(context)) {
    context <- list()
  }
  context$status <- "cancelling"
  context$cancel_started <- context$cancel_started %||% Sys.time()
  context$cancel_deadline <- context$cancel_deadline %||% (Sys.time() + 10)
  context$error <- if (.builder_diagnostics_text(reason %||% NULL)) {
    reason
  } else {
    context$error %||% NULL
  }
  release_settlement_context(context)
  busy_note("Stopping release finalization…")
  alive <- tryCatch(process$is_alive(), error = identity)
  if (isTRUE(alive)) {
    try(process$kill_tree(), silent = TRUE)
    try(process$kill(), silent = TRUE)
    alive <- tryCatch(process$is_alive(), error = identity)
  }
  if (identical(alive, FALSE)) {
    return(isTRUE(finish_builder_release_cancellation()))
  }
  if (inherits(alive, "condition")) {
    context$error <- paste0(
      "The release process state could not be confirmed: ",
      conditionMessage(alive)
    )
    release_settlement_context(context)
  }
  schedule_builder_release_settlement_poll(delay = 0.5)
  FALSE
}

observeEvent(
  input$stop_builder_release,
  {
    context <- isolate(release_settlement_context())
    if (is.list(context) && identical(context$status, "settling")) {
      stop_builder_release_settlement(paste(
        "Build stopped by the user during release finalization.",
        "The staged output was not published."
      ))
    } else {
      pending <- isolate(protocol())$pending
      if (
        is.list(pending) &&
          identical(pending$kind, "build") &&
          builder_build_request_cancel(pending$payload$progress_path %||% "")
      ) {
        builder_build_stop_requested(TRUE)
        busy_note(
          "Stopping after the current step. No output will be published…"
        )
        schedule_builder_build_stop_fallback(pending$request_id)
      }
    }
  },
  ignoreInit = TRUE
)

builder_build_stop_grace_ms <- function() {
  value <- suppressWarnings(as.numeric(getOption(
    "CerebroNexus.builder.build_stop_grace_ms",
    10000
  ))[1L])
  if (!is.finite(value) || value < 0) 10000 else value
}

## The worker only sees the cancel marker between steps; one long marker or
## export call can run for minutes. If the Build is still pending after the
## grace period, kill the worker and settle the Build as stopped.
schedule_builder_build_stop_fallback <- function(request_id) {
  later::later(
    function() {
      if (builder_session_closed()) {
        return(invisible(FALSE))
      }
      shiny::withReactiveDomain(
        session,
        shiny::isolate({
          current_protocol <- protocol()
          pending <- current_protocol$pending
          current_worker <- worker()
          if (
            !is.list(pending) ||
              !identical(pending$kind, "build") ||
              !identical(pending$request_id, request_id) ||
              is.null(current_worker)
          ) {
            return(invisible(FALSE))
          }
          busy_note("Stopping the background worker…")
          restart_worker_protocol(
            current_worker,
            current_protocol,
            "Build stopped by the user. The staged output was not published.",
            grace_ms = 0L,
            async = TRUE,
            notify = FALSE
          )
          invisible(TRUE)
        })
      )
    },
    delay = builder_build_stop_grace_ms() / 1000
  )
  invisible(TRUE)
}

consume_transport_retained <- function(payload) {
  if (
    !is.list(payload) ||
      !identical(payload$kind, "load") ||
      !isTRUE(payload$retain_source) ||
      !builder_has_text(payload$transport_retained_path)
  ) {
    return(invisible(FALSE))
  }
  retained <- builder_project_transport_retained_read(
    payload$transport_retained_path,
    payload$id
  )
  if (is.null(retained)) {
    return(invisible(FALSE))
  }
  if (
    !builder_project_transport_retained_remove(
      payload$transport_retained_path
    )
  ) {
    return(invisible(FALSE))
  }
  release_client_import_transport(
    client_import_id_for(payload$id),
    payload$id
  )
}

## -- one poller drains whatever the worker was asked to do ---------------
observe({
  current_protocol <- protocol()
  if (is.null(current_protocol) || is.null(current_protocol$pending)) {
    return()
  }
  if (
    identical(
      release_preparation_request(),
      current_protocol$pending$request_id
    )
  ) {
    return()
  }
  current_worker <- worker()
  req(current_worker)
  invalidateLater(100, session)
  got <- try(builder_session_poll(current_worker), silent = TRUE)
  if (inherits(got, "try-error")) {
    poll_error <- conditionMessage(attr(got, "condition"))
    pending_payload <- current_protocol$pending$payload
    if (identical(pending_payload$kind, "load")) {
      poll_error <- builder_import_public_error(
        poll_error,
        pending_payload$path %||% character()
      )
    }
    restart_worker_protocol(
      current_worker,
      current_protocol,
      poll_error
    )
    return()
  }
  worker(got$worker)
  if (identical(got$event, "restarted")) {
    recovered <- apply_protocol_recovery(
      current_protocol,
      got$worker,
      "The background worker stopped before returning its result.",
      retry_persistent = TRUE
    )
    if (
      isTRUE(recovered) &&
        identical(got$worker$state %||% NULL, "starting")
    ) {
      await_restarted_worker("Reopening datasets from their snapshots…")
    }
    return()
  }
  if (identical(got$event, "restart_failed")) {
    apply_protocol_recovery(
      current_protocol,
      got$worker,
      "The background worker stopped and could not be restored.",
      retry_persistent = FALSE,
      error = got$error %||% "The background worker could not restart."
    )
    return()
  }
  request <- current_protocol$pending
  p <- request$payload
  consume_transport_retained(p)
  if (is.null(got$result)) {
    if (identical(p$kind, "load") && !is.null(p$progress_path)) {
      progress <- builder_import_progress_read(
        p$progress_path,
        p$import_generation %||% 1L
      )
      if (!is.null(progress)) {
        set_import_state(
          p$id,
          progress$stage,
          progress$generation
        )
        imports(builder_import_set_progress_bytes(
          isolate(imports()),
          p$id,
          progress$generation,
          progress$bytes_done %||% NULL,
          progress$bytes_total %||% NULL
        ))
      }
    } else if (identical(p$kind, "build") && !is.null(p$progress_path)) {
      phase <- builder_build_progress_read(p$progress_path)
      if (!is.null(phase)) {
        build_operation_phase(phase)
        if (!builder_build_cancel_requested(p$progress_path)) {
          busy_note(builder_build_progress_note(p$plan, phase))
        }
      }
    }
    return()
  }
  terminal_progress <- NULL
  if (identical(p$kind, "build")) {
    # A stop can arrive after the worker has completed but before settlement.
    # Handle it through the same verified abort path as any failed build.
    if (builder_build_cancel_requested(p$progress_path %||% "")) {
      got$result$error <- "Build stopped by the user. The staged output was not published."
    }
    terminal_progress <- builder_build_progress_finish(
      p$plan,
      p$progress_path %||% "",
      current_note = isolate(busy_note())
    )
    if (isTRUE(terminal_progress$defer_settlement)) {
      build_operation_phase(terminal_progress$phase)
      busy_note(terminal_progress$note)
    }
  }
  settle_completed_build <- function(release, value) {
    if (
      is.list(value) &&
        (identical(value$state, "failure") ||
          identical(value$state, "recovery_required"))
    ) {
      value$failure_phase <- isolate(build_operation_phase()) %||% "datasets"
      value$failure_stage <- if (is.list(release)) {
        release$handle$stage %||% NULL
      } else {
        NULL
      }
      value$failure_build_id <- request$build_id %||% NULL
    }
    settle <- function() {
      start_builder_release_settlement(
        release,
        value,
        request,
        worker_transport = got$result$transport %||% NULL
      )
    }
    if (isTRUE(terminal_progress$defer_settlement)) {
      session$onFlushed(settle, once = TRUE)
      return(invisible(TRUE))
    }
    settle()
  }
  if (!is.null(got$result$error)) {
    worker_error <- if (identical(p$kind, "load")) {
      builder_import_public_error(
        got$result$error,
        p$path %||% character()
      )
    } else {
      got$result$error
    }
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      worker_transport <- got$result$transport %||% NULL
      response_diagnostics <- if (
        is.list(worker_transport) && is.list(worker_transport$response)
      ) {
        worker_transport$response$diagnostics
      } else {
        NULL
      }
      release_result <- builder_result_failure(
        worker_error,
        diagnostics = response_diagnostics
      )
      diagnostics_path <- NULL
      if (!is.null(release)) {
        recorded <- builder_persist_worker_transport_failure(
          release,
          release_result,
          worker_transport,
          process = got$worker$process %||% NULL
        )
        diagnostics_path <- recorded$path
        release_result <- abort_release_result(release, worker_error)
        if (.builder_diagnostics_text(diagnostics_path %||% NULL)) {
          release_result$diagnostics_path <- diagnostics_path
        }
        active_release(NULL)
      }
      release_result$failure_phase <-
        isolate(build_operation_phase()) %||% "datasets"
      release_result$failure_stage <- if (is.list(release)) {
        release$handle$stage %||% NULL
      } else {
        NULL
      }
      release_result$failure_build_id <- request$build_id %||% NULL
      result(release_result)
    } else {
      add_error(worker_error)
    }
    fail_spatial_preview(p)
    restart_worker_protocol(
      got$worker,
      current_protocol,
      worker_error
    )
    if (identical(request$kind, "build")) {
      # Protocol recovery terminalizes the same pending Build and writes its
      # own generic result. Restore the richer first-failure result, including
      # its persisted diagnostic bundle, after that bookkeeping completes.
      result(release_result)
      build_operation_phase(NULL)
    }
    return()
  }
  completed <- builder_protocol_complete(
    current_protocol,
    got$result$value
  )
  if (!is.null(completed$protocol$pending)) {
    restart_worker_protocol(
      got$worker,
      current_protocol,
      "A worker response did not match the pending request."
    )
    return()
  }
  protocol(completed$protocol)
  if (!isTRUE(terminal_progress$defer_settlement)) {
    busy_note(NULL)
  }
  if (!isTRUE(completed$accepted)) {
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      if (
        is.null(release) ||
          !identical(release$id, request$build_id)
      ) {
        release <- NULL
      }
      settle_completed_build(
        release,
        builder_result_failure(
          "A stale Build result was rejected. Retry the action."
        )
      )
      return()
    }
    if (identical(p$kind, "load")) {
      release_client_import(
        client_import_id_for(p$id),
        server_id = p$id,
        outcome = "error",
        message = "A stale dataset import result was rejected."
      )
    }
    release_pending_source(p)
    if (isTRUE(request$persistent)) {
      protocol(builder_protocol_acknowledge(
        protocol(),
        request$request_id
      ))
      if (identical(request$kind, "drop")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          "A stale dataset release result was rejected."
        )
        return()
      }
      add_error(
        "A stale persistent worker result was discarded. Retry the action."
      )
    }
    return()
  }
  value <- completed$value
  if (!is.null(completed$error)) {
    fail_spatial_preview(p)
    cancelled <- identical(p$kind, "load") && p$id %in% cancelled_loads()
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      if (
        is.null(release) ||
          !identical(release$id, request$build_id)
      ) {
        release <- NULL
      }
      response_diagnostics <- if (
        is.list(got$result$transport) &&
          is.list(got$result$transport$response)
      ) {
        got$result$transport$response$diagnostics
      } else {
        NULL
      }
      settle_completed_build(
        release,
        builder_result_failure(
          completed$error,
          diagnostics = response_diagnostics
        )
      )
      return()
    } else if (identical(p$kind, "load") && !cancelled) {
      set_import_state(
        p$id,
        "error",
        p$import_generation %||% 1L,
        completed$error
      )
      builder_import_progress_remove(p$progress_path %||% "")
      add_error(NULL)
    } else if (!cancelled) {
      add_error(completed$error)
    }
    if (cancelled) {
      release_pending_source(p)
    }
    if (isTRUE(request$persistent)) {
      protocol(builder_protocol_acknowledge(
        protocol(),
        request$request_id
      ))
    }
    if (identical(p$kind, "load")) {
      if (!cancelled) {
        release_pending_source(p, drop_import = FALSE)
        if (
          exists("builder_project_commit_restored_entry", mode = "function")
        ) {
          builder_project_commit_restored_entry(
            list(id = p$id),
            error = completed$error
          )
        }
      }
      schedule_failed_import_cleanup(got$worker)
    }
    return()
  }
  if (identical(p$kind, "load")) {
    cancelled <- p$id %in% cancelled_loads()
    if (!is.null(value$error)) {
      if (!cancelled) {
        set_import_state(
          p$id,
          "error",
          p$import_generation %||% 1L,
          value$error
        )
        builder_import_progress_remove(p$progress_path %||% "")
        add_error(NULL)
      } else {
        release_pending_source(p)
      }
      protocol(builder_protocol_acknowledge(protocol(), request$request_id))
      if (!cancelled) {
        release_pending_source(p, drop_import = FALSE)
        if (
          exists("builder_project_commit_restored_entry", mode = "function")
        ) {
          builder_project_commit_restored_entry(
            list(id = p$id),
            error = value$error
          )
        }
      }
      schedule_failed_import_cleanup(got$worker)
      return()
    }
    pending_entry <- isolate(import_of(p$id))
    if (
      is.null(pending_entry) ||
        !identical(
          pending_entry$generation,
          as.integer(p$import_generation %||% 1L)
        )
    ) {
      cancelled <- TRUE
    }
    if (cancelled) {
      updated_worker <- try(
        builder_worker_register_snapshot(got$worker, p$id, value$snapshot),
        silent = TRUE
      )
      if (inherits(updated_worker, "try-error")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          conditionMessage(attr(updated_worker, "condition"))
        )
        return()
      }
      worker(updated_worker)
      identity <- .builder_worker_identity(value$snapshot)
      accepted <- builder_protocol_dataset(protocol(), p$id, 1L, identity)
      acknowledged <- try(
        builder_protocol_acknowledge(accepted, request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
        add_error(paste0(
          "The cancelled upload could not be released safely. ",
          "Restart this Builder session."
        ))
        return()
      }
      protocol(acknowledged)
      pending_drops <- pending_snapshot_drops()
      pending_drops[[p$id]] <- identity
      pending_snapshot_drops(pending_drops)
      release_pending_source(p)
      queued <- enqueue(list(
        kind = "drop",
        id = p$id,
        dataset_revision = 1L,
        snapshot_identity = identity,
        note = "Releasing cancelled upload…"
      ))
      if (!isTRUE(queued)) {
        pending_drops[[p$id]] <- NULL
        pending_snapshot_drops(pending_drops)
        add_error(
          "The cancelled upload will be released when this session closes."
        )
      }
      return()
    }
    profile <- value$profile
    set_import_state(
      p$id,
      "preparing",
      p$import_generation %||% 1L
    )
    ## Package loading can be slow on first use. The import worker tests the
    ## same library paths and returns only capability flags to the UI process.
    backend_capabilities <- value$backend_capabilities %||%
      list(
        build = list(h5 = FALSE),
        viewer = list(h5 = FALSE)
      )
    recommendations <- list(
      metadata = builder_recommend_metadata(value$dataset_profile),
      backend = builder_recommend_backend(
        value$matrix_summary,
        backend_capabilities
      )
    )
    settings <- builder_default_settings(
      profile,
      builder_unique_dataset_name(
        p$label,
        vapply(
          isolate(sets()),
          function(entry) as.character(entry$settings$name %||% ""),
          character(1)
        )
      ),
      dataset_profile = value$dataset_profile
    )
    settings$recommendations <- recommendations
    settings$metadata_policy <- recommendations$metadata
    settings$expression_backend <- recommendations$backend$value %||% "embedded"
    ## Pictures the source file brought along. Failing to place one must not
    ## fail the import: the Spatial page still takes an uploaded picture.
    if (length(value$source_images %||% list())) {
      seeded <- tryCatch(
        builder_source_image_settings(value$source_images, source_image_dir()),
        error = function(error) list()
      )
      if (length(seeded)) {
        settings$images <- utils::modifyList(
          settings$images %||% list(),
          seeded
        )
      }
    }
    loaded_path <- value$retained_path %||% p$retained_path %||% p$path
    entry <- list(
      id = p$id,
      source_id = p$id,
      output_id = p$id,
      path = loaded_path,
      filename = p$filename %||% basename(loaded_path),
      source_origin = p$source_origin %||%
        if (identical(p$source, "example")) "example" else "upload",
      ## Which built-in example produced this, so removing it puts the
      ## example back on offer. NULL for anything read from a file.
      example = p$example,
      format = value$format,
      profile = profile,
      dataset_profile = value$dataset_profile,
      snapshot = value$snapshot,
      revision = 1L,
      ## Level names per grouping variable, in the order the exporter will
      ## produce them -- the keys a configured palette has to match.
      levels = value$levels %||% list(),
      settings = settings
    )
    prepared <- try(
      {
        if (identical(p$source, "example")) {
          entry <- builder_apply_example_import_preset(entry, p$example)
        }
        builder_prepare_loaded_entry_attachment(entry)
      },
      silent = TRUE
    )
    if (inherits(prepared, "try-error")) {
      message <- conditionMessage(attr(prepared, "condition"))
      released <- try(.builder_snapshot_release(value$snapshot), silent = TRUE)
      acknowledged <- try(
        builder_protocol_acknowledge(protocol(), request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
      } else {
        protocol(acknowledged)
      }
      set_import_state(
        p$id,
        "error",
        p$import_generation %||% 1L,
        error = message
      )
      add_error(
        if (isTRUE(released)) {
          message
        } else {
          paste0(
            message,
            " The temporary snapshot will be cleaned up when Builder stops."
          )
        }
      )
      builder_import_progress_remove(p$progress_path %||% "")
      release_pending_source(p)
      if (exists("builder_project_commit_restored_entry", mode = "function")) {
        builder_project_commit_restored_entry(list(id = p$id), error = message)
      }
      return()
    }
    entry <- prepared$entry
    next_state <- prepared$state
    updated_worker <- try(
      builder_worker_register_snapshot(
        got$worker,
        p$id,
        value$snapshot
      ),
      silent = TRUE
    )
    if (inherits(updated_worker, "try-error")) {
      restart_worker_protocol(
        got$worker,
        protocol(),
        conditionMessage(attr(updated_worker, "condition"))
      )
      return()
    }
    worker(updated_worker)
    started_at_ms <- suppressWarnings(as.numeric(
      pending_entry$started_at_ms %||% NA_real_
    ))
    entry$import_elapsed_ms <- if (
      length(started_at_ms) == 1L &&
        !is.na(started_at_ms) &&
        is.finite(started_at_ms)
    ) {
      max(0, as.numeric(Sys.time()) * 1000 - started_at_ms)
    } else {
      NULL
    }
    next_state <- builder_reduce_state(
      next_state,
      list(type = "replace", id = entry$id, entry = entry),
      trusted = TRUE
    )
    store(next_state)
    if (exists("builder_queue_legacy_qs_notice", mode = "function")) {
      restoring_project <- exists(
        "builder_project_pending_entries",
        mode = "function"
      ) &&
        p$id %in% names(isolate(builder_project_pending_entries()))
      if (!isTRUE(restoring_project)) {
        builder_queue_legacy_qs_notice(list(entry))
      }
    }
    protocol(builder_protocol_dataset(
      isolate(protocol()),
      p$id,
      entry$revision,
      .builder_worker_identity(entry$snapshot)
    ))
    watched <- identical(isolate(active_import_id()), p$id)
    current_id <- isolate(current())
    next_current <- builder_import_ready_target(
      watched = watched,
      current_id = current_id,
      loaded_id = p$id
    )
    focus_configure <- !identical(next_current, current_id)
    if (focus_configure) {
      current(next_current)
    }
    session$sendCustomMessage(
      "builder_import_status",
      list(text = paste0(entry$settings$name, " is ready."))
    )
    set_import_state(
      p$id,
      "ready",
      p$import_generation %||% 1L
    )
    workflow(builder_reduce_workflow(
      isolate(workflow()),
      list(type = "datasets_ready")
    ))
    if (exists("builder_project_commit_restored_entry", mode = "function")) {
      builder_project_commit_restored_entry(entry)
    }
    if (focus_configure) {
      session$onFlushed(
        function() {
          session$sendCustomMessage(
            "builder_focus_stage",
            list(id = "configure")
          )
        },
        once = TRUE
      )
    }
    release_pending_source(p)
    result(NULL)
  } else if (identical(p$kind, "preview")) {
    if (identical(current(), p$id)) {
      preview_frame(value)
    }
  } else if (identical(p$kind, "projection_previews")) {
    projection_previews(builder_preview_cache_store(
      isolate(projection_previews()),
      p$id,
      value
    ))
  } else if (identical(p$kind, "trajectory_previews")) {
    trajectory_previews(builder_preview_cache_store(
      isolate(trajectory_previews()),
      p$id,
      value
    ))
  } else if (identical(p$kind, "coords")) {
    if (
      identical(current(), p$id) &&
        identical(active_slice(), p$image)
    ) {
      spatial_coords(value)
    }
  } else if (identical(p$kind, "spatial_preview")) {
    cache_key <- p$preview_cache_key %||%
      builder_spatial_preview_cache_key(p$id, p$section)
    cache <- isolate(spatial_previews())
    contract_matches <- is.list(p$preview_contract) &&
      builder_spatial_preview_cache_hit(
        cache,
        cache_key,
        p$preview_contract
      )
    if (contract_matches) {
      cache <- builder_spatial_preview_cache_store_if_match(
        cache,
        cache_key,
        p$preview_contract,
        value
      )
      spatial_previews(cache)
    }
    if (
      contract_matches &&
        identical(current(), p$id) &&
        identical(active_slice(), p$section)
    ) {
      alignment_preview(value)
      if (isTRUE(value$available)) {
        spatial_coords(list(
          x = value$spatial$x,
          y = value$spatial$y,
          sx = value$spatial$x,
          sy = value$spatial$y
        ))
      }
    }
  } else if (identical(p$kind, "align_all")) {
    apply_section_bounds(p$id, value, p$picture)
  } else if (identical(p$kind, "build")) {
    release <- isolate(active_release())
    if (
      is.null(release) ||
        !identical(release$id, request$build_id)
    ) {
      release <- NULL
    }
    settle_completed_build(release, value)
  } else if (identical(p$kind, "drop")) {
    active_state <- store()
    retained <- active_state$datasets
    if (is.list(active_state$last_removed)) {
      retained <- c(retained, list(active_state$last_removed$entry))
    }
    pending_drops <- pending_snapshot_drops()
    # The transition excludes other_drop_ids with a shared pending identity.
    released <- try(
      builder_snapshot_release_transition(
        worker = got$worker,
        id = p$id,
        identity = request$snapshot_identity,
        retained = retained,
        pending = pending_drops,
        release = function(worker, id, identity) {
          builder_worker_release_snapshot(
            worker,
            id,
            expected_identity = identity
          )
        },
        unregister = function(worker, id) {
          worker$snapshot_registry[[id]] <- NULL
          worker
        },
        identity_of = .builder_worker_identity
      ),
      silent = TRUE
    )
    if (inherits(released, "try-error")) {
      restart_worker_protocol(
        got$worker,
        protocol(),
        conditionMessage(attr(released, "condition"))
      )
      return()
    }
    worker(released$worker)
    pending_snapshot_drops(released$pending)
    all <- Filter(function(e) !identical(e$id, p$id), sets())
    sets(all)
    projection_previews(builder_preview_cache_drop_dataset(
      isolate(projection_previews()),
      p$id
    ))
    trajectory_previews(builder_preview_cache_drop_dataset(
      isolate(trajectory_previews()),
      p$id
    ))
    spatial_previews(builder_spatial_preview_cache_drop_dataset(
      isolate(spatial_previews()),
      p$id
    ))
    if (identical(current(), p$id)) {
      current(if (length(all)) all[[1]]$id else NULL)
      result(NULL)
    }
    acknowledged <- try(
      builder_protocol_acknowledge(protocol(), request$request_id),
      silent = TRUE
    )
    if (inherits(acknowledged, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The dataset was removed, but its protocol acknowledgement failed. ",
        "Restart this Builder session."
      ))
      return()
    }
    forgotten <- try(
      builder_protocol_forget_dataset(acknowledged, p$id),
      silent = TRUE
    )
    if (inherits(forgotten, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The dataset was removed, but its queued actions could not be ",
        "cleared. Restart this Builder session."
      ))
      return()
    }
    protocol(forgotten$protocol)
    cancelled <- length(forgotten$failed) + length(forgotten$discarded)
    if (cancelled) {
      builder_notify(
        "info",
        paste0(
          "Dataset removed; ",
          cancelled,
          " obsolete queued action",
          if (cancelled == 1L) " was" else "s were",
          " cancelled."
        ),
        duration = 5
      )
    }
    return()
  }
  if (isTRUE(request$persistent) && !identical(request$kind, "build")) {
    protocol(builder_protocol_acknowledge(protocol(), request$request_id))
  }
})

## Pictures that arrive with a source file (AnnData, SingleCellExperiment) are
## written here and described like uploaded ones; the folder goes with the
## session.
source_image_state <- new.env(parent = emptyenv())
source_image_dir <- function() {
  if (is.null(source_image_state$path)) {
    path <- tempfile("cerebro-builder-source-images-")
    dir.create(path, recursive = TRUE, mode = "0700", showWarnings = FALSE)
    source_image_state$path <- normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    )
    session$onSessionEnded(function() {
      unlink(source_image_state$path, recursive = TRUE, force = TRUE)
    })
  }
  source_image_state$path
}

update_enhance_histology_choices <- function(entry) {
  collection <- builder_image_collection_normalize(
    entry$settings$images %||% list()
  )
  choices <- lapply(collection, names)
  invisible(choices)
}

commit_enhance_images <- function(entry, images, internal = FALSE) {
  entry$settings$images <- builder_image_collection_normalize(images)
  changed <- replace_entry(entry, internal = internal)
  if (!isTRUE(changed)) {
    return(invisible(FALSE))
  }
  update_enhance_histology_choices(entry)
  invisible(entry)
}

## Take the per-section extents the worker computed and pair each with the
## one shared picture.
apply_section_bounds <- function(id, per_section, a) {
  if (is.null(a) || !length(per_section)) {
    return()
  }
  e <- isolate(entry_of(id))
  if (is.null(e)) {
    return()
  }
  paired <- builder_pair_sections(a, per_section)
  imgs <- utils::modifyList(e$settings$images %||% list(), paired)
  short <- names(Filter(function(x) x$outside > 0, paired))
  commit_enhance_images(e, imgs)

  ## Saying "done" when four of five slides have every cell off the image is
  ## how the earlier version of this hid its own bug.
  if (length(short)) {
    builder_notify(
      "warning",
      paste0(
        "Applied to ",
        length(per_section),
        " sections, but cells still fall outside the image in: ",
        paste(short, collapse = ", "),
        ". Bounding-box mode usually works best for multi-section objects."
      ),
      target = list(
        selector = ".builder-viewer-spatial-alignment .builder-viewer-card-body"
      )
    )
  } else {
    builder_notify(
      "info",
      paste0(
        "The image was fitted to the coordinates of all ",
        length(per_section),
        " sections."
      ),
      target = list(
        selector = ".builder-viewer-spatial-alignment .builder-viewer-card-body"
      ),
      duration = 5
    )
  }
}

output$add_error <- renderUI({
  msg <- add_error()
  if (is.null(msg)) {
    return(NULL)
  }
  div(class = "notice bad", msg)
})
