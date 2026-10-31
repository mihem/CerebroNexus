## Builder server: foundation.

## Each entry: id, path, format, object, profile, settings.
## `settings` is what the user chose; it is written back whenever an input
## changes so switching between data sets does not lose it.
store <- reactiveVal(builder_state())
imports <- reactiveVal(builder_import_queue(max_active = 1L))
workflow <- reactiveVal(builder_workflow_state())
selected_output <- reactiveVal(NULL)
active_import_id <- reactiveVal(NULL)
import_focus_id <- reactive(builder_import_focus_id(imports()))
client_import_server_ids <- reactiveVal(character())
released_client_imports <- reactiveVal(character())
released_client_import_records <- reactiveVal(list())
transport_released_imports <- reactiveVal(character())
pending_client_import_dispatch <- reactiveVal(NULL)
pending_client_upload <- reactiveVal(NULL)
pending_client_upload_sequence <- reactiveVal(0L)
external_import_active <- reactiveVal(NULL)
client_import_history_limit <- 200L

loaded_entry_lifecycle <- new.env(parent = emptyenv())
loaded_entry_lifecycle$finalize <- function(entry) entry
register_loaded_entry_finalizer <- function(finalizer) {
  stopifnot(is.function(finalizer))
  loaded_entry_lifecycle$finalize <- finalizer
  invisible(finalizer)
}
finalize_loaded_entry <- function(entry) {
  loaded_entry_lifecycle$finalize(entry)
}
builder_prepare_loaded_entry_attachment <- function(entry) {
  finalized <- finalize_loaded_entry(entry)
  current_state <- isolate(store())
  existing <- Filter(
    function(candidate) identical(candidate$id, finalized$id),
    current_state$datasets
  )
  replacing_artifact <- length(existing) == 1L &&
    identical(existing[[1L]]$load_state %||% "loaded", "artifact_ready")
  next_state <- builder_reduce_state(
    current_state,
    if (replacing_artifact) {
      list(type = "replace", id = finalized$id, entry = finalized)
    } else {
      list(type = "add", entry = finalized)
    },
    trusted = TRUE
  )
  list(entry = finalized, state = next_state)
}

client_import_id_for <- function(server_id) {
  ids <- isolate(client_import_server_ids())
  value <- if (builder_has_text(server_id) && server_id %in% names(ids)) {
    unname(ids[[server_id]])
  } else {
    character()
  }
  if (length(value) == 1L && !is.na(value) && nzchar(value)) value else NULL
}

replay_existing_client_import <- function(client_id) {
  if (!builder_has_text(client_id)) {
    return(invisible(FALSE))
  }
  record <- isolate(released_client_import_records())[[client_id]]
  if (is.list(record)) {
    session$sendCustomMessage(
      "builder_client_import_release",
      list(
        client_id = client_id,
        server_id = record$server_id,
        outcome = record$state,
        message = record$message
      )
    )
    return(invisible(TRUE))
  }
  ids <- isolate(client_import_server_ids())
  server_ids <- names(ids)[unname(ids) == client_id]
  if (!length(server_ids)) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage(
    "builder_client_import_accepted",
    list(
      client_id = client_id,
      server_id = server_ids[[1L]],
      name = NULL,
      kind = NULL
    )
  )
  if (server_ids[[1L]] %in% isolate(transport_released_imports())) {
    session$sendCustomMessage(
      "builder_client_import_transport_release",
      list(client_id = client_id, server_id = server_ids[[1L]])
    )
  }
  invisible(TRUE)
}

bind_client_import <- function(client_id, server_id, name, kind) {
  if (
    !builder_has_text(client_id) ||
      !builder_has_text(server_id) ||
      !builder_has_text(name) ||
      !kind %in% c("file", "example")
  ) {
    return(invisible(FALSE))
  }
  ids <- isolate(client_import_server_ids())
  existing_server_ids <- names(ids)[unname(ids) == client_id]
  if (length(existing_server_ids) && !server_id %in% existing_server_ids) {
    session$sendCustomMessage(
      "builder_client_import_accepted",
      list(
        client_id = client_id,
        server_id = existing_server_ids[[1L]],
        name = name,
        kind = kind
      )
    )
    return(invisible(FALSE))
  }
  ids[[server_id]] <- client_id
  client_import_server_ids(ids)
  session$sendCustomMessage(
    "builder_client_import_accepted",
    list(
      client_id = client_id,
      server_id = server_id,
      name = name,
      kind = kind
    )
  )
  invisible(TRUE)
}

release_client_import_transport <- function(client_id, server_id) {
  if (!builder_has_text(client_id) || !builder_has_text(server_id)) {
    return(invisible(FALSE))
  }
  ids <- isolate(client_import_server_ids())
  if (
    !server_id %in% names(ids) ||
      !identical(unname(ids[[server_id]]), client_id)
  ) {
    return(invisible(FALSE))
  }
  released <- isolate(transport_released_imports())
  if (server_id %in% released) {
    return(invisible(FALSE))
  }
  transport_released_imports(c(released, server_id))
  session$sendCustomMessage(
    "builder_client_import_transport_release",
    list(client_id = client_id, server_id = server_id)
  )
  invisible(TRUE)
}

release_client_import <- function(
  client_id,
  server_id = NULL,
  outcome,
  message = NULL
) {
  if (
    !builder_has_text(client_id) ||
      !builder_has_text(outcome) ||
      !outcome %in% c("ready", "error", "cancelled", "rejected")
  ) {
    return(invisible(FALSE))
  }
  released <- isolate(released_client_imports())
  if (client_id %in% released) {
    return(invisible(FALSE))
  }
  released <- c(released, client_id)
  records <- isolate(released_client_import_records())
  records[[client_id]] <- list(
    client_id = client_id,
    server_id = server_id,
    state = outcome,
    message = message
  )
  if (length(records) > client_import_history_limit) {
    keep <- tail(names(records), client_import_history_limit)
    records <- records[keep]
    released <- released[released %in% keep]
  }
  released_client_imports(released)
  released_client_import_records(records)
  ids <- isolate(client_import_server_ids())
  bound_server_ids <- names(ids)[unname(ids) == client_id]
  client_import_server_ids(ids[unname(ids) != client_id])
  transport_released_imports(setdiff(
    isolate(transport_released_imports()),
    bound_server_ids
  ))
  session$sendCustomMessage(
    "builder_client_import_release",
    list(
      client_id = client_id,
      server_id = server_id,
      outcome = outcome,
      message = message
    )
  )
  invisible(TRUE)
}
current_id <- reactiveVal(NULL)
configure_workbench_surface <- reactiveVal(NULL)
workflow_has_datasets <- reactiveVal(FALSE)
rail_dataset_count <- reactiveVal(
  length(isolate(store())$datasets) + length(isolate(imports())$entries)
)
rail_undo_available <- reactiveVal(isTRUE(isolate(store())$can_undo_remove))
update_current_id <- function(value) {
  if (!identical(value, isolate(current_id()))) {
    current_id(value)
  }
  invisible(value)
}
observe({
  update_current_id(store()$current_dataset)
})
observe({
  next_value <- length(store()$datasets %||% list()) > 0L
  if (!identical(next_value, isolate(workflow_has_datasets()))) {
    workflow_has_datasets(next_value)
  }
})
observe({
  state <- store()
  queue <- imports()
  next_count <- length(state$datasets) + length(queue$entries)
  next_undo <- isTRUE(state$can_undo_remove)
  if (!identical(next_count, isolate(rail_dataset_count()))) {
    rail_dataset_count(next_count)
  }
  if (!identical(next_undo, isolate(rail_undo_available()))) {
    rail_undo_available(next_undo)
  }
})
observe({
  state <- store()
  id <- state$current_dataset
  entries <- state$datasets %||% list()
  index <- which(vapply(
    entries,
    function(entry) identical(entry$id, id),
    logical(1)
  ))
  entry <- if (length(index) == 1L) entries[[index]] else NULL
  entry_owner <- if (is.list(entry$snapshot)) {
    entry$snapshot$owner_token %||% NULL
  } else if (is.list(entry$project_artifact)) {
    entry$project_artifact$resolved_path %||%
      entry$project_artifact$path %||%
      NULL
  } else {
    NULL
  }
  surface <- list(
    id = id,
    load_state = if (is.null(entry)) NULL else entry$load_state %||% "loaded",
    entry_owner = entry_owner
  )
  if (!identical(surface, isolate(configure_workbench_surface()))) {
    configure_workbench_surface(surface)
  }
})
observe({
  loaded <- store()$datasets %||% list()
  pending <- imports()$entries %||% list()
  state <- isolate(workflow())
  if (!length(loaded) && !length(pending)) {
    if (!is.null(isolate(selected_output()))) {
      selected_output(NULL)
    }
    if (!identical(state$stage, "upload") || !is.null(state$review_snapshot)) {
      workflow(builder_reduce_workflow(state, list(type = "empty")))
    }
  }
})
sets <- function(value) {
  if (missing(value)) {
    return(store()$datasets)
  }
  current_state <- isolate(store())
  updated <- try(
    builder_reduce_state(
      current_state,
      list(type = "replace_all", datasets = value),
      trusted = TRUE
    ),
    silent = TRUE
  )
  if (inherits(updated, "try-error")) {
    if (!isTRUE(current_state$.state_only_fixture)) {
      stop(attr(updated, "condition"))
    }
    ids <- vapply(value, `[[`, character(1), "id")
    stopifnot(!anyDuplicated(ids))
    current_state$datasets <- value
    if (
      is.null(current_state$current_dataset) ||
        !current_state$current_dataset %in% ids
    ) {
      current_state["current_dataset"] <- list(
        if (length(ids)) ids[[1L]] else NULL
      )
    }
    current_state$revision <- as.integer(current_state$revision) + 1L
    updated <- current_state
  }
  if (exists("builder_dataset_state_cache", inherits = TRUE)) {
    builder_dataset_state_cache_clear(builder_dataset_state_cache)
  }
  if (exists("builder_configuration_identity_cache", inherits = TRUE)) {
    builder_project_configuration_cache_clear(
      builder_configuration_identity_cache
    )
  }
  store(updated)
  invisible(value)
}
current <- function(value) {
  if (missing(value)) {
    return(current_id())
  }
  if (is.null(value)) {
    return(invisible(NULL))
  }
  updated <- builder_reduce_state(
    isolate(store()),
    list(type = "select", id = value),
    trusted = TRUE
  )
  store(updated)
  active_import_id(NULL)
  update_current_id(updated$current_dataset)
  invisible(value)
}
dataset_check_marks <- reactiveVal(character())
dataset_order <- reactiveVal(character())
dataset_revisions <- reactive({
  entries <- sets()
  stats::setNames(
    vapply(
      entries,
      function(entry) as.integer(entry$revision %||% 0L),
      integer(1)
    ),
    vapply(entries, `[[`, character(1), "id")
  )
})
builder_configuration_identity_cache <- new.env(parent = emptyenv())
builder_dataset_state_cache <- new.env(parent = emptyenv())
builder_spatial_image_verification_cache <- new.env(parent = emptyenv())
checked_dataset_ids <- reactive({
  entries <- sets()
  marks <- dataset_check_marks()
  pending_drafts <- if (
    exists("alignment_server", inherits = TRUE) &&
      is.list(alignment_server) &&
      is.function(alignment_server$pending_drafts)
  ) {
    alignment_server$pending_drafts()
  } else {
    list()
  }
  checked <- builder_project_checked_ids(
    entries,
    marks,
    identity_cache = builder_configuration_identity_cache
  )
  setdiff(checked, builder_project_pending_draft_ids(pending_drafts))
})
# Status rendering may recur on selection changes. Reuse content verification
# only while path, size, modification/change time and file identity still match.
builder_checkpoint_verification_cache <- new.env(parent = emptyenv())
builder_checkpoint_verification_root <- NULL
dataset_checkpoint_states <- reactive({
  project <- builder_project()
  root <- if (is.null(project)) NULL else project$root
  if (
    !identical(root, builder_checkpoint_verification_root) ||
      length(builder_checkpoint_verification_cache) > 2048L
  ) {
    rm(
      list = ls(builder_checkpoint_verification_cache),
      envir = builder_checkpoint_verification_cache
    )
    builder_checkpoint_verification_root <<- root
  }
  builder_project_checkpoint_states(
    sets(),
    checked_ids = checked_dataset_ids(),
    artifacts = builder_project_artifacts(),
    root = root,
    jobs = builder_project_crb_jobs(),
    identity_cache = builder_configuration_identity_cache,
    verification_cache = builder_checkpoint_verification_cache
  )
})
all_datasets_checked <- reactive({
  ids <- vapply(sets(), `[[`, character(1), "id")
  length(ids) > 0L && all(ids %in% checked_dataset_ids())
})
observe({
  state <- store()
  ids <- vapply(state$datasets, `[[`, character(1), "id")
  if (!identical(ids, isolate(dataset_order()))) {
    dataset_order(ids)
  }
  marks <- isolate(dataset_check_marks())
  kept <- builder_project_retain_check_marks(
    marks,
    live_ids = ids,
    last_removed = state$last_removed,
    can_undo_remove = state$can_undo_remove
  )
  if (!identical(kept, marks)) {
    dataset_check_marks(kept)
  }
  builder_project_configuration_cache_retain_datasets(
    builder_configuration_identity_cache,
    ids
  )
  builder_dataset_state_cache_retain(builder_dataset_state_cache, ids)
})
result <- reactiveVal(NULL)
build_flow <- reactiveVal(list(stage = "idle", plan = NULL))
builder_build_controls_locked <- function(flow) {
  !is.list(flow) || !identical(flow$stage, "idle")
}
review_options <- reactiveVal(builder_review_options())
review_validation <- reactiveVal(list(ok = TRUE, error = NULL))
build_mode <- reactiveVal(isTRUE(app_capability$available))
build_initial_dataset <- reactiveVal(NULL)
auth_enabled <- reactiveVal(FALSE)
auth_accounts <- reactiveVal(builder_auth_empty_accounts())
auth_validation <- reactiveVal(list(ok = TRUE, error = NULL))
enhance_contract <- reactiveVal(list(
  id = NULL,
  organism = NULL,
  analysis_dependencies = character(),
  marker_import_ids = character()
))
seq_id <- reactiveVal(0L)
add_error <- reactiveVal(NULL)
preview_frame <- reactiveVal(NULL)
projection_previews <- reactiveVal(list())
trajectory_previews <- reactiveVal(list())
spatial_previews <- reactiveVal(list())
spatial_coords <- reactiveVal(NULL)
alignment_preview <- reactiveVal(NULL)
marker_import_drafts <- reactiveVal(list())

marker_import_draft_of <- function(id) {
  marker_import_drafts()[[id]]
}

replace_marker_import_draft <- function(id, draft) {
  drafts <- isolate(marker_import_drafts())
  if (is.null(draft)) {
    drafts[[id]] <- NULL
  } else {
    drafts[[id]] <- draft
  }
  marker_import_drafts(drafts)
  invisible(draft)
}

entry_of <- function(id) {
  all <- sets()
  hit <- Filter(function(e) identical(e$id, id), all)
  if (length(hit)) hit[[1]] else NULL
}

observe({
  state <- store()
  id <- state$current_dataset
  entries <- state$datasets %||% list()
  index <- which(vapply(
    entries,
    function(entry) identical(entry$id, id),
    logical(1)
  ))
  entry <- if (length(index) == 1L) entries[[index]] else NULL
  next_contract <- list(
    id = id,
    organism = entry$settings$organism %||% NULL,
    analysis_dependencies = intersect(
      unname(entry$settings$analyses %||% character()),
      "marker_genes"
    ),
    marker_import_ids = names(entry$settings$marker_imports %||% list()) %||%
      character()
  )
  if (!identical(next_contract, isolate(enhance_contract()))) {
    enhance_contract(next_contract)
  }
})

send_dataset_action_owner <- function(
  entry = NULL,
  project_epoch = NULL
) {
  if (is.null(entry)) {
    id <- isolate(current())
    entry <- if (builder_has_text(id)) isolate(entry_of(id)) else NULL
  }
  if (
    is.null(project_epoch) &&
      exists(
        "builder_project_open_generation",
        mode = "function",
        inherits = TRUE
      )
  ) {
    project_epoch <- isolate(builder_project_open_generation())
  }
  owner <- builder_content_action_owner(entry, project_epoch)
  if (is.null(owner)) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage(
    "builder_dataset_action_owner",
    c(list(dataset = entry$id), owner)
  )
  if (
    exists("refresh_marker_dialog_owner", mode = "function", inherits = TRUE)
  ) {
    refresh_marker_dialog_owner(owner$project_epoch)
  }
  invisible(TRUE)
}

replace_entry <- function(updated, internal = FALSE) {
  if (
    !isTRUE(internal) &&
      exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("edit_dataset", notify = FALSE))
  ) {
    return(invisible(FALSE))
  }
  all <- isolate(sets())
  index <- which(vapply(
    all,
    function(entry) identical(entry$id, updated$id),
    logical(1)
  ))
  if (length(index) != 1L) {
    return(invisible(FALSE))
  }
  existing <- all[[index]]
  if (identical(existing$settings, updated$settings)) {
    return(invisible(FALSE))
  }
  existing$settings <- updated$settings
  existing$revision <- as.integer(existing$revision %||% 0L) + 1L
  current_state <- isolate(store())
  updated_state <- try(
    builder_reduce_state(
      current_state,
      list(type = "replace", id = existing$id, entry = existing),
      trusted = TRUE
    ),
    silent = TRUE
  )
  if (inherits(updated_state, "try-error")) {
    if (!isTRUE(current_state$.state_only_fixture)) {
      stop(attr(updated_state, "condition"))
    }
    all[[index]] <- existing
    current_state$datasets <- all
    current_state$revision <- as.integer(current_state$revision) + 1L
    updated_state <- current_state
  }
  store(updated_state)
  send_dataset_action_owner(existing)
  current_protocol <- protocol()
  if (!is.null(current_protocol)) {
    protocol(builder_protocol_dataset(
      current_protocol,
      existing$id,
      existing$revision,
      .builder_worker_identity(existing$snapshot)
    ))
  }
  invisible(TRUE)
}

output$ds_count <- renderText({
  n <- rail_dataset_count()
  if (n == 0) "" else paste0(n)
})
output$rail_undo <- renderUI({
  if (!rail_undo_available()) {
    return(NULL)
  }
  actionLink("undo_remove", "Undo remove")
})

## -- the worker process ---------------------------------------------------
## Objects live there, never here. The protocol owns queue order, request
## identity and acknowledgement barriers; the poller only applies a result
## after that identity has been validated.
worker <- reactiveVal(NULL)
worker_available <- reactiveVal(FALSE)
protocol <- reactiveVal(NULL)
active_release <- reactiveVal(NULL)
release_settlement_process <- reactiveVal(NULL)
release_settlement_context <- reactiveVal(NULL)
release_settlement_poll_scheduled <- reactiveVal(FALSE)
release_settlement_poll_generation <- reactiveVal(0)
request_sequence <- reactiveVal(0L)
pending_snapshot_drops <- reactiveVal(list())
pending_sources <- reactiveVal(character())
pending_uploads <- reactiveVal(list())
cancelled_loads <- reactiveVal(character())
failed_import_cleanup_pending <- reactiveVal(FALSE)
import_of <- function(id) {
  builder_import_find(imports(), id)
}
set_import_state <- function(id, state, generation, error = NULL) {
  updated <- try(
    builder_import_transition(
      isolate(imports()),
      id,
      state,
      generation,
      error = error
    ),
    silent = TRUE
  )
  if (inherits(updated, "try-error")) {
    return(invisible(FALSE))
  }
  imports(updated)
  if (state %in% c("ready", "error", "cancelled")) {
    entry <- builder_import_find(updated, id)
    if (identical(state, "error")) {
      raw_error <- if (inherits(error, "condition")) {
        conditionMessage(error)
      } else {
        paste(as.character(error), collapse = " ")
      }
      message(
        "Dataset import failed [",
        id,
        ": ",
        entry$label,
        "]: ",
        raw_error
      )
    }
    client_id <- client_import_id_for(id)
    release_message <- if (identical(state, "error")) entry$error else NULL
    session$onFlushed(
      function() {
        release_client_import(
          client_id,
          server_id = id,
          outcome = state,
          message = release_message
        )
      },
      once = TRUE
    )
    if (identical(isolate(external_import_active()), id)) {
      external_import_active(NULL)
      session$sendCustomMessage(
        "builder_import_scheduler_state",
        list(active = FALSE, server_id = id)
      )
    }
  }
  invisible(TRUE)
}
forget_import <- function(id) {
  current_imports <- isolate(imports())
  if (is.null(builder_import_find(current_imports, id))) {
    return(invisible(FALSE))
  }
  imports(builder_import_remove(current_imports, id))
  if (identical(isolate(active_import_id()), id)) {
    active_import_id(NULL)
  }
  invisible(TRUE)
}
release_pending_source <- function(payload, drop_import = TRUE) {
  if (!is.list(payload) || !identical(payload$kind, "load")) {
    return(invisible(FALSE))
  }
  value <- if (identical(payload$source, "file")) {
    payload$path
  } else {
    payload$example
  }
  key <- payload$reservation_key %||% builder_source_key(payload$source, value)
  pending_sources(builder_source_release(pending_sources(), key))
  id <- as.character(payload$id %||% character())
  if (length(id) == 1L && !is.na(id) && nzchar(id)) {
    if (id %in% isolate(cancelled_loads())) {
      release_client_import(
        client_import_id_for(id),
        server_id = id,
        outcome = "cancelled"
      )
    }
    if (isTRUE(drop_import)) {
      forget_import(id)
    }
    builder_import_progress_remove(payload$progress_path %||% "")
    builder_project_transport_retained_remove(
      payload$transport_retained_path %||% ""
    )
    uploads <- pending_uploads()
    uploads[[id]] <- NULL
    pending_uploads(uploads)
    cancelled_loads(setdiff(cancelled_loads(), id))
  }
  invisible(TRUE)
}
enqueue <- function(req) {
  current_protocol <- protocol()
  if (is.null(current_protocol) || !isTRUE(worker_available())) {
    add_error("The background worker is not ready yet.")
    return(invisible(FALSE))
  }
  dataset <- req$id %||% "session"
  entry <- entry_of(dataset)
  revision <- if (!is.null(req$dataset_revision)) {
    as.integer(req$dataset_revision)
  } else if (is.null(entry)) {
    0L
  } else {
    as.integer(entry$revision %||% 0L)
  }
  snapshot_identity <- req$snapshot_identity %||%
    if (is.null(entry)) {
      NULL
    } else {
      .builder_worker_identity(entry$snapshot)
    }
  if (!is.null(entry)) {
    current_protocol <- builder_protocol_dataset(
      current_protocol,
      dataset,
      revision,
      snapshot_identity
    )
  }
  replaceable <- req$kind %in%
    c(
      "preview",
      "projection_previews",
      "trajectory_previews",
      "coords",
      "spatial_preview"
    )
  if (replaceable) {
    request_sequence(request_sequence() + 1L)
    if (builder_preview_revision_independent(req$kind)) {
      req$revision_independent <- TRUE
    }
    request <- builder_query(
      kind = req$kind,
      dataset = dataset,
      generation = request_sequence(),
      slot = req$replaces %||% req$kind,
      payload = req,
      revision = revision,
      snapshot_identity = snapshot_identity
    )
  } else {
    if (identical(req$kind, "build") && is.null(req$id)) {
      request_sequence(request_sequence() + 1L)
      req$id <- paste0("build-", request_sequence())
    }
    request <- builder_command(
      kind = req$kind,
      dataset = dataset,
      payload = req,
      revision = revision,
      snapshot_identity = snapshot_identity
    )
  }
  queued <- try(builder_enqueue(current_protocol, request), silent = TRUE)
  if (inherits(queued, "try-error")) {
    add_error(conditionMessage(attr(queued, "condition")))
    return(invisible(FALSE))
  }
  protocol(queued)
  invisible(TRUE)
}
busy_note <- reactiveVal(NULL)
build_operation_phase <- reactiveVal(NULL)

abort_release_result <- function(release, reason) {
  if (is.null(release)) {
    return(builder_result_failure(reason))
  }
  try(builder_coordinator_abort(release$handle), silent = TRUE)
  builder_release_error_result(reason, release$handle$target)
}

settle_failed_builds <- function(recovery, reason) {
  failed <- Filter(
    function(request) identical(request$kind, "build"),
    recovery$failed %||% list()
  )
  for (request in failed) {
    builder_build_progress_remove(
      request$payload$progress_path %||% ""
    )
    release <- isolate(active_release())
    release_result <- builder_result_failure(reason)
    if (!is.null(release) && identical(release$id, request$build_id)) {
      release_result <- abort_release_result(release, reason)
      active_release(NULL)
    }
    result(release_result)
  }
  if (length(failed)) {
    build_flow(list(stage = "idle", plan = NULL))
  }
}

apply_protocol_recovery <- function(
  current_protocol,
  recovered_worker,
  reason,
  retry_persistent,
  error = NULL,
  notify = TRUE
) {
  recovered <- try(
    builder_protocol_recover(
      current_protocol,
      epoch = if (isTRUE(retry_persistent)) {
        recovered_worker$epoch
      } else {
        .builder_worker_epoch()
      },
      reason = reason,
      retry_persistent = retry_persistent
    ),
    silent = TRUE
  )
  busy_note(NULL)
  build_operation_phase(NULL)
  if (inherits(recovered, "try-error")) {
    protocol(NULL)
    worker_available(FALSE)
    add_error(paste0(
      "The worker protocol could not be recovered. Restart this Builder session. ",
      conditionMessage(attr(recovered, "condition"))
    ))
    return(invisible(FALSE))
  }
  worker(recovered_worker)
  worker_available(isTRUE(retry_persistent))
  protocol(recovered$protocol)
  settle_failed_builds(recovered, reason)
  failed_requests <- recovered$failed %||% list()
  terminal_preview_requests <- c(
    failed_requests,
    recovered$discarded %||% list()
  )
  invisible(lapply(terminal_preview_requests, function(request) {
    payload <- request$payload
    if (
      identical(payload$kind, "spatial_preview") &&
        exists("fail_spatial_preview", mode = "function", inherits = TRUE)
    ) {
      fail_spatial_preview(payload)
    }
  }))
  invisible(lapply(failed_requests, function(request) {
    payload <- request$payload
    if (identical(payload$kind, "load")) {
      entry <- isolate(import_of(payload$id))
      if (!is.null(entry)) {
        set_import_state(
          payload$id,
          "error",
          payload$import_generation %||% entry$generation,
          reason
        )
        release_pending_source(payload, drop_import = FALSE)
        if (
          exists("builder_project_commit_restored_entry", mode = "function")
        ) {
          builder_project_commit_restored_entry(
            list(id = payload$id),
            error = reason
          )
        }
        return(invisible(TRUE))
      }
    }
    release_pending_source(payload)
  }))
  retried_builds <- Filter(
    function(request) identical(request$kind, "build"),
    recovered$retried %||% list()
  )
  if (length(retried_builds)) {
    release <- isolate(active_release())
    if (!is.null(release)) {
      try(builder_coordinator_abort(release$handle), silent = TRUE)
      active_release(NULL)
    }
  }

  if (!is.null(error)) {
    has_failed_load <- any(vapply(
      failed_requests,
      function(request) identical(request$payload$kind, "load"),
      logical(1)
    ))
    public_error <- if (has_failed_load) {
      builder_import_public_error(error)
    } else {
      error
    }
    add_error(paste0(
      public_error,
      " No queued action was left pending; restart this Builder session."
    ))
    return(invisible(FALSE))
  }
  retried <- length(recovered$retried %||% list())
  failed <- length(recovered$failed %||% list())
  discarded <- length(recovered$discarded %||% list())
  detail <- c(
    if (retried) paste(retried, "persistent action(s) will resume"),
    if (failed) paste(failed, "action(s), including any Build, were stopped"),
    if (discarded) {
      paste(discarded, "obsolete preview request(s) were discarded")
    }
  )
  if (isTRUE(notify)) {
    add_error(paste0(
      "The background worker restarted from immutable snapshots.",
      if (length(detail)) {
        paste0(" ", paste(detail, collapse = "; "), ".")
      } else {
        ""
      }
    ))
  }
  invisible(TRUE)
}

## The replacement always boots in the background so the page stays
## responsive while the worker re-sources its runtime. `async = TRUE` is for
## user-requested stops: the old worker is killed without waiting for an
## interrupt (which callr cannot deliver on Windows).
restart_worker_protocol <- function(
  current_worker,
  current_protocol,
  reason,
  grace_ms = 5000L,
  async = FALSE,
  notify = TRUE
) {
  restarted <- try(
    builder_worker_restart(
      current_worker,
      grace_ms = grace_ms,
      .async = TRUE,
      verify_tree = !isTRUE(async)
    ),
    silent = TRUE
  )
  typed_failure <- inherits(restarted, "builder_worker_restart_failed") ||
    (is.list(restarted) && identical(restarted$event, "restart_failed"))
  if (inherits(restarted, "try-error") || typed_failure) {
    failed_worker <- if (
      is.list(restarted) &&
        inherits(restarted$worker, "builder_worker")
    ) {
      restarted$worker
    } else {
      current_worker
    }
    restart_error <- if (inherits(restarted, "try-error")) {
      conditionMessage(attr(restarted, "condition"))
    } else {
      restarted$error %||% "The background worker could not restart."
    }
    return(apply_protocol_recovery(
      current_protocol,
      failed_worker,
      reason,
      retry_persistent = FALSE,
      error = restart_error
    ))
  }
  recovered <- apply_protocol_recovery(
    current_protocol,
    restarted,
    reason,
    retry_persistent = TRUE,
    notify = notify
  )
  if (isTRUE(recovered) && identical(restarted$state, "starting")) {
    await_restarted_worker(
      if (isTRUE(async)) {
        "Stopping the cancelled task…"
      } else {
        "Reopening datasets from their snapshots…"
      }
    )
  }
  recovered
}

## The dispatcher holds requeued work until poll_builder_worker_startup
## reports a background-started replacement ready.
await_restarted_worker <- function(detail) {
  worker_available(FALSE)
  session$sendCustomMessage(
    "builder_worker_status",
    list(
      state = "starting",
      title = "Restarting background workspace…",
      detail = detail
    )
  )
  later::later(
    function() {
      shiny::withReactiveDomain(
        builder_lifecycle_session,
        poll_builder_worker_startup()
      )
    },
    delay = 0.1
  )
  invisible(TRUE)
}

start_builder_worker <- function() {
  if (builder_session_closed()) {
    return(invisible(FALSE))
  }
  if (!is.null(shiny::isolate(worker()))) {
    return(invisible(TRUE))
  }
  prewarmed <- builder_app_take_prewarmed_worker()
  started <- if (is.null(prewarmed)) {
    builder_session_start(getwd(), .async = TRUE)
  } else {
    list(worker = prewarmed, session = prewarmed$process)
  }
  if (!is.null(started$error)) {
    add_error(started$error)
    session$sendCustomMessage(
      "builder_worker_status",
      list(
        state = "error",
        title = "Background workspace unavailable",
        detail = "Restart this Builder session to try again."
      )
    )
    return(invisible(FALSE))
  }
  worker(started$worker)
  protocol(builder_request_protocol(started$worker$epoch))
  later::later(
    function() {
      shiny::withReactiveDomain(
        builder_lifecycle_session,
        poll_builder_worker_startup()
      )
    },
    delay = 0.1
  )
  invisible(TRUE)
}

builder_lifecycle_session <- session

builder_session_closed <- function() {
  is.function(builder_lifecycle_session$isClosed) &&
    isTRUE(builder_lifecycle_session$isClosed())
}

builder_native_pickers <- new.env(parent = emptyenv())

builder_cancel_native_picker <- function(key) {
  key <- as.character(key)[1L]
  picker <- builder_native_pickers[[key]] %||% NULL
  if (!is.null(picker)) {
    builder_kill_native_picker(picker)
    rm(list = key, envir = builder_native_pickers)
  }
  invisible(TRUE)
}

builder_schedule_native_picker <- function(
  kind,
  on_result,
  key = kind,
  on_error = NULL,
  on_finally = NULL
) {
  stopifnot(is.function(on_result), builder_has_text(key))
  key <- as.character(key)[1L]
  if (exists(key, envir = builder_native_pickers, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  picker <- tryCatch(builder_start_native_picker(kind), error = identity)
  if (inherits(picker, "condition")) {
    if (is.function(on_error)) {
      shiny::isolate(on_error(picker))
    }
    if (is.function(on_finally)) {
      shiny::isolate(on_finally())
    }
    return(invisible(FALSE))
  }
  picker$on_result <- on_result
  picker$on_finally <- on_finally
  builder_native_pickers[[key]] <- picker
  poll <- function() {
    if (builder_session_closed()) {
      builder_cancel_native_picker(key)
      return(invisible(FALSE))
    }
    current <- builder_native_pickers[[key]] %||% NULL
    if (is.null(current)) {
      return(invisible(FALSE))
    }
    if (builder_native_picker_is_alive(current)) {
      later::later(
        function() shiny::withReactiveDomain(builder_lifecycle_session, poll()),
        delay = 0.1
      )
      return(invisible(TRUE))
    }
    result <- tryCatch(builder_collect_native_picker(current), error = identity)
    rm(list = key, envir = builder_native_pickers)
    if (is.function(on_finally)) {
      on.exit(shiny::isolate(on_finally()), add = TRUE)
    }
    if (inherits(result, "condition")) {
      if (is.function(on_error)) {
        shiny::isolate(on_error(result))
      }
      return(invisible(FALSE))
    }
    shiny::isolate(on_result(result))
    invisible(TRUE)
  }
  later::later(
    function() shiny::withReactiveDomain(builder_lifecycle_session, poll()),
    delay = 0
  )
  invisible(TRUE)
}

observeEvent(
  input$builder_picker_alternative,
  {
    keys <- ls(builder_native_pickers, all.names = TRUE)
    if (length(keys) != 1L) {
      return()
    }
    key <- keys[[1L]]
    picker <- builder_native_pickers[[key]]
    builder_cancel_native_picker(key)
    if (is.function(picker$on_finally)) {
      on.exit(picker$on_finally(), add = TRUE)
    }
    picker$on_result(list(
      status = if (
        identical(input$builder_picker_alternative$action, "cancel")
      ) {
        "cancelled"
      } else {
        "unavailable"
      },
      manual = TRUE,
      path = NULL,
      paths = character()
    ))
  },
  ignoreInit = TRUE
)

poll_builder_worker_startup <- function() {
  if (builder_session_closed()) {
    return(invisible(FALSE))
  }
  current_worker <- shiny::isolate(worker())
  if (is.null(current_worker)) {
    return(invisible(FALSE))
  }
  startup <- builder_session_poll_startup(current_worker)
  worker(startup$worker)
  if (identical(startup$state, "starting")) {
    later::later(
      function() {
        shiny::withReactiveDomain(
          builder_lifecycle_session,
          poll_builder_worker_startup()
        )
      },
      delay = 0.1
    )
    return(invisible(TRUE))
  }
  if (identical(startup$state, "failed")) {
    current_protocol <- shiny::isolate(protocol())
    if (
      !is.null(current_protocol) &&
        !builder_protocol_is_quiescent(current_protocol)
    ) {
      ## A background restart failed after requeueing work: terminate it the
      ## same way a synchronous restart failure would.
      apply_protocol_recovery(
        current_protocol,
        startup$worker,
        "The background worker could not restart.",
        retry_persistent = FALSE,
        error = startup$error
      )
    } else {
      add_error(startup$error)
    }
    session$sendCustomMessage(
      "builder_worker_status",
      list(
        state = "error",
        title = "Background workspace unavailable",
        detail = startup$error
      )
    )
    return(invisible(FALSE))
  }
  worker_available(TRUE)
  ## Keep one empty, isolated worker warm for the next browser session. A page
  ## refresh creates a new Shiny session; without replenishment Windows must
  ## cold-start another R process while the refreshed page waits.
  try(builder_app_schedule_prewarm_worker(), silent = TRUE)
  session$sendCustomMessage(
    "builder_worker_status",
    list(
      state = "ready",
      title = "Builder is ready",
      detail = "Datasets can now be added."
    )
  )
  invisible(TRUE)
}

observeEvent(
  input$builder_client_connection,
  {
    connection <- input$builder_client_connection
    if (
      !is.list(connection) ||
        !identical(connection$status %||% NULL, "connected")
    ) {
      return()
    }
    current_worker <- isolate(worker())
    state <- if (isTRUE(isolate(worker_available()))) {
      "ready"
    } else {
      current_worker$state %||% "starting"
    }
    status <- switch(
      state,
      ready = list(
        state = "ready",
        title = "Builder is ready",
        detail = "Datasets can now be added."
      ),
      failed = list(
        state = "error",
        title = "Background workspace unavailable",
        detail = current_worker$error %||%
          "Restart this Builder session to try again."
      ),
      list(
        state = "starting",
        title = "Starting Builder…",
        detail = "Preparing your workspace…"
      )
    )
    session$sendCustomMessage("builder_worker_status", status)
  },
  ignoreInit = FALSE
)

session$onFlushed(
  function() {
    session$sendCustomMessage(
      "builder_worker_status",
      list(
        state = "starting",
        title = "Starting Builder…",
        detail = "Preparing your workspace…"
      )
    )
    start_builder_worker()
  },
  once = TRUE
)

session$onSessionEnded(function() {
  for (key in ls(builder_native_pickers, all.names = TRUE)) {
    try(builder_cancel_native_picker(key), silent = TRUE)
  }
  settlement_stopped <- try(
    stop_builder_release_settlement(),
    silent = TRUE
  )
  settlement_stopped <- !inherits(settlement_stopped, "try-error") &&
    isTRUE(settlement_stopped)
  current_worker <- isolate(worker())
  if (!is.null(current_worker)) {
    snapshot_root <- current_worker$snapshot_root %||% NULL
    empty_worker <-
      !length(current_worker$snapshot_registry %||% list()) &&
      builder_has_text(snapshot_root) &&
      dir.exists(snapshot_root) &&
      !length(list.files(
        snapshot_root,
        all.files = TRUE,
        no.. = TRUE
      ))
    stopped <- if (empty_worker) {
      try(current_worker$process$close(), silent = TRUE)
      alive <- try(current_worker$process$is_alive(), silent = TRUE)
      current_worker$ready <- FALSE
      current_worker$cleanup_safe <-
        !inherits(alive, "try-error") && !isTRUE(alive)
      list(
        worker = current_worker,
        stopped = current_worker$cleanup_safe
      )
    } else {
      try(
        builder_worker_stop(current_worker, grace_ms = 5000L),
        silent = TRUE
      )
    }
    if (inherits(stopped, "try-error") || !isTRUE(stopped$stopped)) {
      return()
    }
    if (!isTRUE(stopped$worker$cleanup_safe)) {
      return()
    }
    current_worker <- stopped$worker
    released_all <- TRUE
    released_identities <- character()
    for (snapshot in current_worker$snapshot_registry) {
      identity <- .builder_worker_identity(snapshot)
      if (identity %in% released_identities) {
        next
      }
      released <- try(.builder_snapshot_release(snapshot), silent = TRUE)
      released_all <- released_all && isTRUE(released)
      if (isTRUE(released)) {
        released_identities <- c(released_identities, identity)
      }
    }
    builder_import_progress_cleanup(current_worker$snapshot_root)
    if (isTRUE(current_worker$owns_root)) {
      builder_project_cleanup_session_sources(current_worker$snapshot_root)
    }
    remaining <- list.files(
      current_worker$snapshot_root,
      all.files = TRUE,
      no.. = TRUE
    )
    if (
      isTRUE(current_worker$owns_root) &&
        released_all &&
        !length(remaining)
    ) {
      unlink(current_worker$snapshot_root, recursive = TRUE, force = TRUE)
    }
  }
  if (isTRUE(settlement_stopped)) {
    try(builder_abort_active_release(), silent = TRUE)
  }
})
