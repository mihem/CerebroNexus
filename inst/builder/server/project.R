## Builder server: projects.
##
## A project is one file in a folder (see project.R). Save writes it; Open reads
## it and loads each dataset again from its path. Nothing is copied, cached or
## written in the background.

builder_project <- reactiveVal(NULL)
builder_project_pending_entries <- reactiveVal(list())
builder_project_skipped_ids <- reactiveVal(character())
builder_project_operation_phase <- reactiveVal("idle")
builder_project_pending_folder <- reactiveVal(NULL)
# Which project picker the "choosing" phase belongs to: "create" or "open".
builder_project_picker_kind <- reactiveVal(NULL)
builder_project_open_generation <- reactiveVal(0)
builder_client_import_state <- reactiveVal(list(nonce = 0, pending = 0L))
builder_connection_state <- reactiveVal("connected")
builder_project_restore_progress <- reactiveVal(list(
  mode = "idle",
  total = 0L,
  remaining = 0L
))
builder_project_restore_outcome <- reactiveVal(
  list(upload = character(), missing = character(), failed = character())
)
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
    source_syncing = FALSE,
    crb_generating = FALSE,
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
      length(builder_project_pending_entries()) > 0L
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

## The live workspace against the saved project ---------------------------------

builder_project_dirty <- reactive({
  project <- builder_project()
  if (is.null(project)) {
    return(FALSE)
  }
  builder_project_live_dirty(sets(), project$manifest)
})

register_loaded_entry_finalizer(function(entry) {
  pending <- isolate(builder_project_pending_entries())
  record <- pending[[entry$id]] %||% NULL
  if (is.null(record)) {
    return(entry)
  }
  builder_project_finalize_loaded_entry(entry, record)
})

builder_project_manifest_context <- function() {
  list(
    configuration = builder_project_configuration(
      review_options = isolate(review_options()),
      build_mode = isTRUE(isolate(build_mode())),
      auth_enabled = isTRUE(isolate(auth_enabled())),
      initial_dataset = isolate(build_initial_dataset())
    ),
    last_ui = list(
      stage = isolate(selected_workflow_stage()),
      selected_dataset = isolate(current()),
      spatial = builder_project_last_ui_spatial(
        isolate(alignment_server$project_selection()),
        selected_dataset = isolate(current())
      )
    )
  )
}

## Saving ------------------------------------------------------------------------

builder_project_save_failed <- function(error) {
  builder_project_operation_phase("save_failed")
  builder_notify(
    "error",
    paste0("The project was not saved. ", conditionMessage(error)),
    id = "builder-project-save-error"
  )
  invisible(FALSE)
}

save_builder_project_state <- function(project) {
  entries <- isolate(sets())
  context <- builder_project_manifest_context()
  manifest <- builder_project_snapshot(
    project$name,
    entries,
    checked_ids = isolate(checked_dataset_ids()),
    configuration = context$configuration,
    last_ui = context$last_ui
  )
  # Datasets that were saved but are not in the workspace (still loading, or
  # not restored when the project was opened) stay in the file.
  held <- union(
    names(isolate(builder_project_pending_entries())),
    isolate(builder_project_skipped_ids())
  )
  previous <- builder_project_record_map(project$manifest)
  kept <- previous[setdiff(
    intersect(held, names(previous)),
    vapply(
      entries,
      function(entry) as.character(entry$id),
      character(1)
    )
  )]
  if (length(kept)) {
    manifest$datasets <- c(manifest$datasets, unname(kept))
    for (index in seq_along(manifest$datasets)) {
      manifest$datasets[[index]]$order <- as.integer(index)
    }
  }
  builder_project_operation_phase("saving")
  path <- tryCatch(
    builder_project_write(project$root, manifest),
    error = identity
  )
  if (inherits(path, "condition")) {
    return(builder_project_save_failed(path))
  }
  builder_project_operation_phase("idle")
  builder_notify_clear("builder-project-save-error")
  builder_project(list(
    root = project$root,
    path = path,
    name = project$name,
    manifest = manifest
  ))
  builder_notify("success", "Project saved.", target = "status", duration = 3)
  invisible(TRUE)
}

request_builder_project_save <- function() {
  project <- isolate(builder_project())
  if (is.null(project)) {
    return(request_builder_project_folder())
  }
  save_builder_project_state(project)
}

create_builder_project_in_folder <- function(folder, existing = NULL) {
  builder_project_pending_folder(NULL)
  shiny::removeModal()
  name <- if (is.list(existing)) {
    existing$project$name
  } else {
    basename(folder$root)
  }
  manifest <- builder_project_snapshot(name, list())
  builder_project(list(
    root = folder$root,
    path = builder_project_manifest_path(folder$root),
    name = name,
    manifest = manifest
  ))
  builder_project_skipped_ids(character())
  save_builder_project_state(isolate(builder_project()))
}

## Choosing the folder ------------------------------------------------------------------

show_builder_project_folder_error <- function(message, type = "error") {
  builder_notify(
    type,
    message,
    target = list(field = "builder_project_server_folder")
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
    input_id = "builder_project_server_folder",
    action_id = if (identical(kind, "create")) {
      "use_builder_project_server_folder"
    } else {
      "use_builder_project_server_open"
    },
    label = "Project folder",
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

builder_project_server_path <- function(value) {
  path <- tryCatch(
    builder_server_path_resolve(
      value,
      type = "directory",
      roots = builder_server_path_roots()
    ),
    error = identity
  )
  if (inherits(path, "condition")) {
    show_builder_project_folder_error(conditionMessage(path))
    return(NULL)
  }
  path
}

select_builder_project_folder <- function(path) {
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  if (inherits(folder, "condition")) {
    return(show_builder_project_folder_error(conditionMessage(folder)))
  }
  if (identical(folder$kind, "project")) {
    builder_project_pending_folder(path)
    shiny::showModal(builder_project_existing_folder_dialog(path))
    return(invisible(TRUE))
  }
  create_builder_project_in_folder(folder)
}

request_builder_project_folder <- function() {
  builder_project_pending_folder(NULL)
  if (!builder_operation_allowed("create_project")) {
    return(invisible(FALSE))
  }
  shiny::removeModal()
  builder_project_picker_kind("create")
  builder_project_operation_phase("choosing")
  builder_schedule_native_picker(
    "project_directory",
    key = "project-create",
    on_result = function(choice) {
      shiny::isolate({
        builder_project_operation_phase("idle")
        if (
          !builder_operation_allowed("create_project") ||
            identical(choice$status, "cancelled")
        ) {
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
  path <- builder_project_server_path(input$builder_project_server_folder)
  if (!is.null(path)) {
    select_builder_project_folder(path)
  }
})

observeEvent(input$confirm_existing_builder_project_folder, {
  if (!builder_operation_allowed("create_project")) {
    return()
  }
  path <- isolate(builder_project_pending_folder())
  builder_project_pending_folder(NULL)
  folder <- if (is.null(path)) {
    NULL
  } else {
    tryCatch(builder_project_folder_state(path), error = identity)
  }
  if (!is.list(folder)) {
    shiny::removeModal()
    return()
  }
  existing <- tryCatch(
    builder_project_read(builder_project_manifest_path(folder$root)),
    error = function(error) NULL
  )
  create_builder_project_in_folder(folder, existing = existing)
})

observeEvent(input$choose_another_builder_project_folder, {
  builder_project_pending_folder(NULL)
  shiny::removeModal()
  request_builder_project_folder()
})

observeEvent(input$save_builder_project, {
  if (is.null(isolate(builder_project()))) {
    if (!builder_operation_allowed("create_project")) {
      return()
    }
    request_builder_project_folder()
  } else {
    if (!builder_operation_allowed("save_project")) {
      return()
    }
    request_builder_project_save()
  }
})

## Opening ---------------------------------------------------------------------------

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

request_builder_project_open <- function(confirmed = FALSE) {
  if (!builder_operation_allowed("open_project")) {
    return(invisible(FALSE))
  }
  if (!isTRUE(confirmed) && builder_project_open_needs_confirmation()) {
    shiny::showModal(builder_project_open_discard_dialog(), session = session)
    return(invisible(FALSE))
  }
  previous_phase <- isolate(builder_project_operation_phase())
  shiny::removeModal()
  builder_project_picker_kind("open")
  builder_project_operation_phase("choosing")
  builder_schedule_native_picker(
    "project_directory",
    key = "project-open",
    on_result = function(choice) {
      shiny::isolate({
        builder_project_operation_phase(previous_phase)
        if (
          !builder_operation_allowed("open_project") ||
            identical(choice$status, "cancelled")
        ) {
          return()
        }
        if (!identical(choice$status, "selected")) {
          show_builder_project_server_path("open", choice$error)
          return()
        }
        start_builder_project_open(choice$path)
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

observeEvent(input$use_builder_project_server_open, {
  if (!builder_operation_allowed("open_project")) {
    return()
  }
  path <- builder_project_server_path(input$builder_project_server_folder)
  if (!is.null(path)) {
    shiny::removeModal()
    start_builder_project_open(path, roots = builder_server_path_roots())
  }
})

observeEvent(input$confirm_discard_builder_project_open, {
  shiny::removeModal()
  request_builder_project_open(confirmed = TRUE)
})

observeEvent(input$cancel_discard_builder_project_open, {
  shiny::removeModal()
})

start_builder_project_open <- function(path, roots = NULL) {
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  manifest <- if (inherits(folder, "condition")) {
    folder
  } else if (!identical(folder$kind, "project")) {
    simpleError("There is no Builder project in this folder.")
  } else {
    tryCatch(
      builder_project_read(builder_project_manifest_path(folder$root)),
      error = identity
    )
  }
  if (inherits(manifest, "condition")) {
    return(builder_notify(
      "error",
      paste0("The project could not be opened. ", conditionMessage(manifest))
    ))
  }
  if (!isTRUE(isolate(worker_available()))) {
    return(builder_notify(
      "error",
      "The background worker is still starting. Try again in a moment."
    ))
  }
  examples <- tryCatch(
    vapply(builder_example_catalog(), function(e) as.character(e$id), ""),
    error = function(error) NULL
  )
  plan <- builder_project_restore_plan(
    manifest,
    examples,
    path_allowed = if (!is.null(roots)) {
      function(source_path) {
        !inherits(
          tryCatch(
            builder_server_path_resolve(source_path, "file", roots = roots),
            error = identity
          ),
          "condition"
        )
      }
    }
  )
  startable <- Filter(function(item) identical(item$status, "ready"), plan)
  records <- builder_project_record_map(manifest)[names(startable)]
  skipped <- setdiff(names(plan), names(startable))
  # Opening replaces the workspace: a new protocol epoch drops whatever the
  # background worker still holds for the datasets being closed.
  current_protocol <- isolate(protocol())
  protocol(
    if (is.null(current_protocol)) {
      NULL
    } else {
      builder_request_protocol(current_protocol$epoch)
    }
  )
  store(builder_state())
  projection_previews(list())
  trajectory_previews(list())
  spatial_previews(list())
  builder_project_configuration_cache_clear(
    builder_configuration_identity_cache
  )
  dataset_check_marks(list())
  generation <- as.double(isolate(builder_project_open_generation())) + 1
  builder_project_open_generation(generation)
  send_dataset_action_owner(project_epoch = generation)
  builder_project_pending_entries(records)
  builder_project_skipped_ids(skipped)
  builder_project(list(
    root = folder$root,
    path = builder_project_manifest_path(folder$root),
    name = manifest$project$name,
    manifest = manifest
  ))
  restore_builder_project_preferences(manifest)
  builder_project_restore_outcome(builder_project_restore_summary(plan))
  if (!length(records)) {
    finish_builder_project_restore()
    return(invisible(TRUE))
  }
  builder_project_restore_progress(list(
    mode = "restoring",
    total = length(records),
    remaining = length(records),
    selected_total = length(records),
    ready = 0L
  ))
  builder_project_operation_phase("restoring")
  session$onFlushed(
    function() {
      shiny::isolate({
        for (record in records) {
          started <- try(
            start_builder_project_record_load(record),
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
            outcome <- isolate(builder_project_restore_outcome())
            outcome$failed <- c(
              outcome$failed,
              paste0(record$name %||% record$id, ": ", reason)
            )
            builder_project_restore_outcome(outcome)
            builder_project_commit_restored_entry(
              list(id = record$id),
              error = reason
            )
          }
        }
      })
    },
    once = TRUE
  )
  invisible(TRUE)
}

start_builder_project_record_load <- function(record) {
  source <- record$source %||% list()
  started <- if (identical(source$kind, "example")) {
    start_load(
      "example",
      source$example,
      record$name,
      dataset_id = record$id
    )
  } else {
    start_load(
      "file",
      source$path,
      record$name,
      file_meta = list(
        name = source$filename %||% basename(source$path),
        type = "application/octet-stream",
        size = as.double(source$bytes %||% NA_real_)
      ),
      dataset_id = record$id,
      source_origin = "local"
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
  pending[[id]] <- NULL
  builder_project_pending_entries(pending)
  marks <- isolate(dataset_check_marks())
  if (is.null(error)) {
    builder_project_skipped_ids(setdiff(
      isolate(builder_project_skipped_ids()),
      id
    ))
    if (isTRUE(record$checked)) {
      marks[[id]] <- builder_project_check_identity(entry)
    } else {
      marks <- marks[setdiff(names(marks), id)]
    }
    alignment_server$discard_project_drafts(id)
  } else {
    marks <- marks[setdiff(names(marks), id)]
    builder_project_skipped_ids(union(
      isolate(builder_project_skipped_ids()),
      id
    ))
  }
  dataset_check_marks(marks)
  progress <- isolate(builder_project_restore_progress())
  builder_project_restore_progress(list(
    mode = if (length(pending)) "restoring" else "idle",
    total = as.integer(progress$total %||% length(pending)),
    remaining = length(pending),
    selected_total = as.integer(progress$selected_total %||% length(pending)),
    ready = length(isolate(sets()))
  ))
  if (!length(pending)) {
    finish_builder_project_restore()
  }
  invisible(TRUE)
}

finish_builder_project_restore <- function() {
  builder_project_restore_progress(list(
    mode = "idle",
    total = 0L,
    remaining = 0L
  ))
  builder_project_operation_phase("idle")
  project <- isolate(builder_project())
  if (!is.null(project)) {
    restore_builder_project_last_ui_after_flush(project$manifest$last_ui)
  }
  outcome <- isolate(builder_project_restore_outcome())
  message <- builder_project_restore_notice(outcome)
  if (!is.null(message)) {
    builder_notify("warning", message)
  }
  invisible(TRUE)
}

session$onSessionEnded(function() {
  builder_project_pending_folder(NULL)
})

output$project_status <- renderUI({
  project <- builder_project()
  builder_project_status_ui(
    if (is.null(project)) NULL else list(name = project$name),
    phase = builder_project_phase()
  )
})
