## ----------------------------------------------------------------------------##
## Persistent dataset rail UI and controller boundaries.
##
## Ordering and selection semantics live in state.R. This file renders the rail,
## validates Shiny events and next-plan removal, reserves input sources, and
## coordinates snapshot alias/release transitions through injected callbacks.
## ----------------------------------------------------------------------------##

.builder_rail_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

builder_source_key <- function(kind, value) {
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      is.na(kind) ||
      !kind %in% c("file", "example") ||
      !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop("A source key requires one file path or example id.", call. = FALSE)
  }
  normalized <- if (identical(kind, "file")) {
    path <- path.expand(value)
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
      path <- file.path(getwd(), path)
    }
    if (file.exists(path)) {
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    }
    path <- gsub("\\\\", "/", path)
    prefix <- if (grepl("^[A-Za-z]:/", path)) {
      substr(path, 1L, 3L)
    } else {
      "/"
    }
    parts <- strsplit(sub("^([A-Za-z]:)?/+", "", path), "/", fixed = FALSE)[[
      1L
    ]]
    collapsed <- character()
    for (part in parts) {
      if (!nzchar(part) || identical(part, ".")) {
        next
      }
      if (identical(part, "..")) {
        if (length(collapsed)) collapsed <- head(collapsed, -1L)
      } else {
        collapsed <- c(collapsed, part)
      }
    }
    canonical <- paste0(prefix, paste(collapsed, collapse = "/"))
    if (identical(.Platform$OS.type, "windows")) {
      tolower(canonical)
    } else {
      canonical
    }
  } else {
    value
  }
  paste(kind, normalized, sep = ":")
}

builder_source_release <- function(pending, key) {
  setdiff(as.character(pending), key)
}

builder_source_reserve <- function(entries, pending, kind, value) {
  key <- builder_source_key(kind, value)
  stored <- unlist(
    lapply(entries, function(entry) {
      if (!is.null(entry$example)) {
        return(builder_source_key("example", entry$example))
      }
      if (
        is.character(entry$path) &&
          length(entry$path) == 1L &&
          !is.na(entry$path) &&
          nzchar(entry$path)
      ) {
        return(builder_source_key("file", entry$path))
      }
      NULL
    }),
    use.names = FALSE
  )
  pending <- unique(as.character(pending))
  if (key %in% pending) {
    return(list(
      ok = FALSE,
      code = "source_pending",
      key = key,
      pending = pending
    ))
  }
  list(
    ok = TRUE,
    code = if (key %in% stored) "source_in_store" else NULL,
    key = key,
    pending = c(pending, key)
  )
}

builder_snapshot_release_transition <- function(
  worker,
  id,
  identity,
  retained,
  pending,
  release,
  unregister,
  identity_of = function(snapshot) snapshot$identity
) {
  stopifnot(
    is.function(release),
    is.function(unregister),
    is.function(identity_of)
  )
  retained_shared <- any(vapply(
    retained,
    function(entry) {
      identical(identity_of(entry$snapshot), identity)
    },
    logical(1)
  ))
  other_ids <- setdiff(names(pending), id)
  pending_shared <- any(vapply(
    pending[other_ids],
    identical,
    logical(1),
    y = identity
  ))
  updated_worker <- if (retained_shared || pending_shared) {
    unregister(worker, id)
  } else {
    release(worker, id, identity)
  }
  updated_pending <- pending
  updated_pending[[id]] <- NULL
  list(worker = updated_worker, pending = updated_pending)
}

.builder_rail_readiness <- function(dataset_state) {
  records <- list(
    ready = list(label = "Ready", icon = "\u2713"),
    needs_attention = list(label = "Needs attention", icon = "!"),
    blocked = list(label = "Blocked", icon = "\u00d7"),
    loading = list(label = "Loading", icon = "\u2026"),
    reload_required = list(label = "Reload required", icon = "\u21bb"),
    artifact_ready = list(label = "CRB ready", icon = "\u2713")
  )
  .builder_rail_or(
    records[[dataset_state$readiness]],
    records$blocked
  )
}

builder_dataset_remove_requires_confirmation <- function(entry) {
  settings <- entry$settings
  spatial <- .builder_rail_or(entry$spatial_drafts, list())
  length(.builder_rail_or(settings, list())) > 0L || length(spatial) > 0L
}

.builder_rail_validation <- function(
  ok,
  code = NULL,
  message = NULL,
  dataset_ids = character(),
  expected_members = character(),
  plan = NULL
) {
  structure(
    list(
      ok = isTRUE(ok),
      code = code,
      message = message,
      dataset_ids = dataset_ids,
      expected_members = expected_members,
      plan = plan
    ),
    class = c("builder_rail_validation", "list")
  )
}

.builder_rail_remove_event <- function(value) {
  if (is.character(value) && length(value) == 1L) {
    return(list(id = value, confirmed = FALSE))
  }
  if (!is.list(value) || is.object(value)) {
    return(NULL)
  }
  list(
    id = .subset2(value, "id"),
    confirmed = isTRUE(.subset2(value, "confirmed"))
  )
}

.builder_rail_dataset_id <- function(value, ids) {
  if (
    !is.character(value) ||
      is.object(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.null(attributes(value)) ||
      !nzchar(trimws(value)) ||
      !value %in% ids
  ) {
    return(NULL)
  }
  value
}

.builder_rail_pick_event <- function(value, ids) {
  if (is.character(value)) {
    id <- .builder_rail_dataset_id(value, ids)
    return(if (is.null(id)) NULL else list(id = id, switch_token = NULL))
  }
  if (
    !is.list(value) ||
      is.object(value) ||
      !identical(names(value), c("id", "switch_token"))
  ) {
    return(NULL)
  }
  id <- .builder_rail_dataset_id(.subset2(value, "id"), ids)
  token <- .subset2(value, "switch_token")
  if (
    is.null(id) ||
      !is.numeric(token) ||
      is.object(token) ||
      length(token) != 1L ||
      is.na(token) ||
      !is.finite(token) ||
      token < 1
  ) {
    return(NULL)
  }
  list(id = id, switch_token = token)
}

builder_dataset_rail_server <- function(
  input,
  session,
  store,
  validate_remove,
  select_dataset = function(id, commit, switch_token = NULL) commit(),
  on_select = function(...) invisible(NULL),
  on_reorder = function(...) invisible(NULL),
  on_remove = function(...) invisible(NULL),
  on_undo = function(...) invisible(NULL),
  on_validation = function(...) invisible(NULL),
  mutations_locked = function() FALSE,
  on_locked = function(...) invisible(NULL)
) {
  stopifnot(
    is.function(store),
    is.function(validate_remove),
    is.function(select_dataset),
    is.function(on_select),
    is.function(on_reorder),
    is.function(on_remove),
    is.function(on_undo),
    is.function(on_validation),
    is.function(mutations_locked),
    is.function(on_locked)
  )
  reject_locked <- function() {
    locked <- tryCatch(
      isTRUE(mutations_locked()),
      error = function(error) TRUE
    )
    if (locked) {
      on_locked()
    }
    locked
  }
  validation <- shiny::reactiveVal(.builder_rail_validation(
    FALSE,
    code = "not_run",
    message = "No removal has been requested."
  ))

  shiny::observeEvent(input$pick, {
    state <- shiny::isolate(store())
    ids <- vapply(state$datasets, `[[`, character(1), "id")
    event <- .builder_rail_pick_event(input$pick, ids)
    if (is.null(event)) {
      return()
    }
    id <- event$id
    commit <- local({
      requested_id <- id
      committed <- FALSE
      function() {
        if (committed) {
          return(invisible(FALSE))
        }
        latest <- shiny::isolate(store())
        latest_ids <- vapply(latest$datasets, `[[`, character(1), "id")
        selected_id <- .builder_rail_dataset_id(requested_id, latest_ids)
        if (is.null(selected_id)) {
          return(invisible(FALSE))
        }
        committed <<- TRUE
        store(builder_reduce_state(
          latest,
          list(type = "select", id = selected_id),
          trusted = TRUE
        ))
        on_select(selected_id)
        invisible(TRUE)
      }
    })
    if (is.null(event$switch_token)) {
      select_dataset(id, commit)
    } else {
      select_dataset(id, commit, event$switch_token)
    }
  })
  shiny::observeEvent(input$reorder_ds, {
    if (reject_locked()) {
      return()
    }
    event <- input$reorder_ds
    state <- shiny::isolate(store())
    ids <- vapply(state$datasets, `[[`, character(1), "id")
    if (
      !is.list(event) ||
        is.object(event) ||
        !identical(names(event), c("id", "direction")) ||
        !is.character(event$id) ||
        length(event$id) != 1L ||
        is.na(event$id) ||
        !is.null(attributes(event$id)) ||
        !event$id %in% ids ||
        !is.character(event$direction) ||
        length(event$direction) != 1L ||
        is.na(event$direction) ||
        !is.null(attributes(event$direction)) ||
        !event$direction %in% c("up", "down")
    ) {
      return()
    }
    updated <- builder_reduce_state(
      state,
      list(type = "move", id = event$id, direction = event$direction),
      trusted = TRUE
    )
    store(updated)
    on_reorder(state, updated)
  })
  shiny::observeEvent(input$drop_ds, {
    if (reject_locked()) {
      return()
    }
    event <- .builder_rail_remove_event(input$drop_ds)
    if (is.null(event)) {
      return()
    }
    previous <- shiny::isolate(store())
    ids <- vapply(previous$datasets, `[[`, character(1), "id")
    id <- .builder_rail_dataset_id(event$id, ids)
    if (is.null(id)) {
      return()
    }
    entry <- previous$datasets[[match(id, ids)]]
    if (
      builder_dataset_remove_requires_confirmation(entry) &&
        !isTRUE(event$confirmed)
    ) {
      result <- .builder_rail_validation(
        FALSE,
        code = "confirmation_required",
        message = "Confirm removal before changing the frozen release."
      )
      validation(result)
      on_validation(result)
      return()
    }
    next_state <- builder_reduce_state(
      previous,
      list(type = "remove", id = id),
      trusted = TRUE
    )
    result <- validate_remove(next_state, id)
    if (!inherits(result, "builder_rail_validation")) {
      result <- .builder_rail_validation(
        FALSE,
        code = "invalid_plan_validation",
        message = "Removal validation returned an invalid result."
      )
    }
    validation(result)
    on_validation(result)
    if (!isTRUE(result$ok)) {
      return()
    }
    store(next_state)
    on_remove(previous, next_state, id, result)
  })
  shiny::observeEvent(input$undo_remove, {
    if (reject_locked()) {
      return()
    }
    state <- shiny::isolate(store())
    if (!isTRUE(state$can_undo_remove)) {
      return()
    }
    store(builder_reduce_state(
      state,
      list(type = "undo_remove"),
      trusted = TRUE
    ))
    on_undo()
  })

  list(
    state = shiny::reactive(store()),
    validation = shiny::reactive(validation())
  )
}

builder_empty_workbench_ui <- function(
  project_active = FALSE,
  dataset_count = 0L,
  formats = get0("builder_formats", inherits = TRUE),
  examples = builder_example_directory(),
  loading_count = 0L
) {
  format_records <- if (
    is.list(formats) &&
      length(formats) &&
      all(vapply(formats, is.list, logical(1)))
  ) {
    formats
  } else {
    lapply(as.character(formats), function(format) {
      list(id = format, label = format, available = TRUE)
    })
  }
  available_formats <- Filter(
    function(format) !identical(format$available, FALSE),
    format_records
  )
  unavailable_formats <- Filter(
    function(format) identical(format$available, FALSE),
    format_records
  )
  format_labels <- builder_advertised_format_labels(available_formats)
  shiny::tags$section(
    class = "builder-stage builder-empty-state",
    `aria-labelledby` = "builder-dropzone-title",
    shiny::tags$button(
      type = "button",
      class = "builder-dataset-dropzone builder-add-datasets",
      `aria-labelledby` = "builder-dropzone-title",
      `aria-describedby` = "builder-empty-description builder-empty-formats",
      shiny::tags$div(
        class = "builder-dataset-dropzone-icon",
        `aria-hidden` = "true",
        shiny::icon("cloud-upload")
      ),
      shiny::tags$div(
        class = "builder-dataset-dropzone-copy",
        shiny::h2(id = "builder-dropzone-title", "Add your data"),
        shiny::p(
          id = "builder-empty-description",
          "Drop files here or click to browse."
        ),
        shiny::p(
          id = "builder-empty-formats",
          class = "builder-dataset-dropzone-formats",
          paste(format_labels, collapse = ", ")
        ),
        builder_deprecated_format_note_ui(available_formats)
      )
    ),
    if (isTRUE(loading_count > 0L)) {
      builder_import_loading_note_ui()
    },
    shiny::tags$div(
      class = "builder-data-sources",
      shiny::tags$p(
        class = "builder-data-sources-label",
        "Or start with an example"
      ),
      builder_example_buttons_ui(examples),
      shiny::uiOutput("add_error")
    ),
    if (isTRUE(project_active)) {
      shiny::tags$div(
        class = "builder-empty-project-action",
        shiny::actionButton(
          "choose_saved_project_datasets",
          "Choose saved datasets…",
          class = "btn"
        )
      )
    },
    if (dataset_count > 0L) {
      builder_stage_footer_ui(
        paste(
          dataset_count,
          if (identical(as.integer(dataset_count), 1L)) {
            "dataset"
          } else {
            "datasets"
          }
        ),
        shiny::actionButton(
          "configure_datasets",
          "Configure Datasets",
          class = "btn btn-primary"
        )
      )
    }
  )
}

builder_format_unavailable_ui <- function(formats) {
  unavailable <- Filter(
    function(format) is.list(format) && identical(format$available, FALSE),
    formats %||% list()
  )
  if (!length(unavailable)) {
    return(NULL)
  }
  shiny::tags$div(
    class = "builder-format-unavailable builder-format-unavailable--rail",
    role = "status",
    lapply(unavailable, function(format) {
      shiny::tagList(
        shiny::tags$span(format$unavailable_reason),
        if (!is.null(format$unavailable_detail)) {
          shiny::tags$details(
            shiny::tags$summary("Details"),
            shiny::tags$p(format$unavailable_detail)
          )
        }
      )
    })
  )
}

## A small status line on the Data page while datasets load. Loading is
## tracked on each rail row; the page itself stays usable.
builder_import_loading_note_ui <- function() {
  shiny::tags$p(
    class = "builder-import-loading-note",
    role = "status",
    `aria-live` = "polite",
    shiny::tags$span(
      class = "builder-import-loading-dot",
      `aria-hidden` = "true"
    ),
    ## Browser uploads still waiting are not counted on the server, so the
    ## note does not give a number that could disagree with the rail.
    "Loading in the background. Track each dataset in the dataset list."
  )
}

builder_failed_import_workbench_ui <- function(
  failed_count,
  dataset_count = 0L
) {
  failed_count <- as.integer(failed_count)
  dataset_count <- as.integer(dataset_count)
  stopifnot(
    length(failed_count) == 1L,
    !is.na(failed_count),
    failed_count > 0L
  )
  shiny::tags$section(
    class = "builder-stage builder-import-failed-state builder-stage-upload",
    `data-workflow-stage` = "upload",
    role = "alert",
    `aria-labelledby` = "builder-import-failed-title",
    shiny::tags$div(
      class = "builder-loading-copy",
      shiny::tags$h2(
        id = "builder-import-failed-title",
        if (failed_count == 1L) {
          "Dataset could not be loaded"
        } else {
          "Datasets could not be loaded"
        }
      ),
      shiny::tags$p(
        paste(
          "Read the error in the dataset list.",
          "Remove the failed item, or retry after resolving the requirement."
        )
      )
    ),
    if (dataset_count > 0L) {
      builder_stage_footer_ui(
        paste(
          dataset_count,
          if (dataset_count == 1L) "dataset is ready" else "datasets are ready"
        ),
        shiny::actionButton(
          "configure_datasets",
          "Configure Datasets",
          class = "btn btn-primary"
        )
      )
    }
  )
}

builder_import_rail_row_model <- function(entry, index, current = NULL) {
  stopifnot(inherits(entry, "builder_import_entry"))
  detail <- Filter(
    function(value) {
      is.character(value) && length(value) == 1L && nzchar(value)
    },
    list(entry$filename, entry$file_type)
  )
  list(
    index = as.integer(index),
    id = entry$id,
    label = entry$label,
    detail = if (length(detail)) {
      paste(unlist(detail), collapse = " · ")
    } else {
      NULL
    },
    load_state = entry$load_state,
    progress_label = entry$progress_label,
    progress_percent = if (isTRUE(entry$cancelling)) {
      NULL
    } else {
      entry$progress_percent
    },
    cancelling = isTRUE(entry$cancelling),
    error = entry$error,
    started_at_ms = suppressWarnings(as.numeric(entry$started_at_ms)),
    import_elapsed_ms = suppressWarnings(as.numeric(entry$import_elapsed_ms)),
    selected = identical(entry$id, current)
  )
}

builder_import_rail_row_fingerprint <- function(model) {
  as.character(jsonlite::toJSON(
    unname(model[c(
      "index",
      "id",
      "label",
      "detail",
      "load_state",
      "progress_label",
      "progress_percent",
      "cancelling",
      "error",
      "started_at_ms",
      "import_elapsed_ms",
      "selected"
    )]),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
}

## What decides the row's elements and buttons. Fields left out (position,
## selection, progress text, timer values) are patched in place by
## builder.js, so a progress tick never replaces the row under the pointer.
builder_import_rail_row_structure <- function(model) {
  finite <- function(value) {
    length(value) == 1L && is.finite(value)
  }
  phase <- if (identical(model$load_state, "error")) {
    "failed"
  } else if (identical(model$load_state, "queued")) {
    "queued"
  } else {
    "running"
  }
  as.character(jsonlite::toJSON(
    list(
      model$id,
      model$label,
      model$detail,
      phase,
      isTRUE(model$cancelling),
      model$error,
      finite(model$import_elapsed_ms) || finite(model$started_at_ms)
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
}

.builder_rail_load_time <- function(elapsed_ms, started_at_ms = NA_real_) {
  if (length(elapsed_ms) == 1L && is.finite(elapsed_ms)) {
    return(shiny::span(
      class = "builder-load-time",
      `data-elapsed-ms` = round(elapsed_ms),
      sprintf("%.1fs", elapsed_ms / 1000)
    ))
  }
  if (length(started_at_ms) == 1L && is.finite(started_at_ms)) {
    return(shiny::span(
      class = "builder-load-time",
      `data-started-at-ms` = round(started_at_ms),
      "0.0s"
    ))
  }
  NULL
}

## The status line, with the byte-level percent of a running copy.
.builder_rail_import_status <- function(model) {
  percent <- model$progress_percent
  has_percent <- is.numeric(percent) &&
    length(percent) == 1L &&
    !is.na(percent)
  shiny::span(
    class = paste(
      "builder-import-status",
      paste0("is-", model$load_state)
    ),
    role = if (has_percent) "progressbar" else NULL,
    `aria-valuemin` = if (has_percent) 0 else NULL,
    `aria-valuemax` = if (has_percent) 100 else NULL,
    `aria-valuenow` = if (has_percent) percent else NULL,
    if (has_percent) {
      paste0(model$progress_label, " ", percent, "%")
    } else {
      model$progress_label
    }
  )
}

builder_import_rail_row_ui <- function(model) {
  failed <- identical(model$load_state, "error")
  queued <- identical(model$load_state, "queued")
  running <- !failed && !queued
  importing <- !failed
  shiny::div(
    class = paste(
      c(
        "ds ds--import",
        if (model$selected) "is-active",
        if (importing) "is-importing",
        if (failed) "is-error"
      ),
      collapse = " "
    ),
    `data-import-id` = model$id,
    `data-load-state` = model$load_state,
    `data-import-fingerprint` = builder_import_rail_row_fingerprint(model),
    `data-import-structure` = builder_import_rail_row_structure(model),
    shiny::tags$button(
      type = "button",
      class = "ds-pick builder-pick-import",
      `data-import-id` = model$id,
      `aria-label` = if (failed) {
        paste("Open failed import", model$label)
      } else {
        paste("Open loading dataset", model$label)
      },
      `aria-current` = if (model$selected) "true" else NULL,
      shiny::span(class = "ds-idx", model$index),
      shiny::span(class = "ds-state-dot", `aria-hidden` = "true"),
      shiny::span(
        class = "ds-body",
        shiny::span(class = "nm", title = model$label, model$label),
        if (!is.null(model$detail)) shiny::span(class = "meta", model$detail),
        .builder_rail_import_status(model),
        if (
          failed &&
            is.character(model$error) &&
            length(model$error) == 1L &&
            !is.na(model$error) &&
            nzchar(model$error)
        ) {
          shiny::span(class = "builder-import-error", model$error)
        },
        .builder_rail_load_time(
          model$import_elapsed_ms,
          model$started_at_ms
        )
      )
    ),
    if (failed || queued || running) {
      shiny::div(
        class = "ds-actions",
        if (failed) {
          shiny::tags$button(
            type = "button",
            class = "ds-move builder-retry-import",
            `data-import-id` = model$id,
            "Retry"
          )
        },
        shiny::tags$button(
          type = "button",
          class = paste(
            "ds-del",
            if (running) {
              "btn btn-remove-soft builder-cancel-import"
            } else {
              "btn-remove-soft"
            },
            "builder-remove-import"
          ),
          `data-import-id` = model$id,
          disabled = if (isTRUE(model$cancelling)) NA else NULL,
          `aria-busy` = if (isTRUE(model$cancelling)) "true" else NULL,
          `aria-label` = if (failed) {
            paste("Remove failed import", model$label)
          } else if (isTRUE(model$cancelling)) {
            paste("Cancelling import", model$label)
          } else if (running) {
            paste("Cancel active import", model$label)
          } else {
            paste("Remove queued import", model$label)
          },
          if (failed) {
            "Remove"
          } else if (isTRUE(model$cancelling)) {
            "Cancelling…"
          } else if (running) {
            "Cancel"
          } else {
            "Remove from queue"
          }
        )
      )
    }
  )
}

builder_import_rail_model <- function(entries, current = NULL, offset = 0L) {
  Map(
    builder_import_rail_row_model,
    entries,
    seq_along(entries) + as.integer(offset),
    MoreArgs = list(current = current)
  )
}

builder_import_rail_patch <- function(entries, current = NULL, offset = 0L) {
  models <- builder_import_rail_model(entries, current, offset)
  list(
    rows = unname(lapply(models, function(model) {
      list(
        id = model$id,
        fingerprint = builder_import_rail_row_fingerprint(model),
        html = htmltools::renderTags(builder_import_rail_row_ui(model))$html
      )
    }))
  )
}

builder_dataset_rail_row_model <- function(
  entry,
  index,
  total,
  current,
  checked = character(),
  checkpoint_state = NULL,
  state_cache = NULL
) {
  readiness <- .builder_rail_readiness(
    builder_cached_dataset_state(entry, state_cache)
  )
  list(
    index = as.integer(index),
    id = entry$id,
    label = .builder_rail_or(entry$settings$name, entry$id),
    cells = as.integer(.builder_rail_or(entry$profile$n_cells, 0L)),
    format = entry$format,
    import_elapsed_ms = suppressWarnings(as.numeric(
      .builder_rail_or(entry$import_elapsed_ms, NA_real_)
    )),
    readiness_label = readiness$label,
    checkpoint_status = checkpoint_state$status %||% NULL,
    checked = if (is.list(checkpoint_state)) {
      isTRUE(checkpoint_state$checked)
    } else {
      entry$id %in% checked
    },
    selected = identical(entry$id, current),
    confirm = builder_dataset_remove_requires_confirmation(entry),
    can_up = index > 1L,
    can_down = index < total
  )
}

builder_dataset_rail_row_fingerprint <- function(model) {
  as.character(jsonlite::toJSON(
    unname(model[c(
      "index",
      "id",
      "label",
      "cells",
      "format",
      "import_elapsed_ms",
      "readiness_label",
      "checkpoint_status",
      "checked",
      "selected",
      "confirm",
      "can_up",
      "can_down"
    )]),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
}

builder_dataset_rail_status <- function(model) {
  if (identical(model$checkpoint_status, "generating")) {
    return(list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Generating cache…",
      crb_status = "generating"
    ))
  }
  if (identical(model$checkpoint_status, "ready")) {
    return(list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Cached · opens fast",
      crb_status = "ready"
    ))
  }
  if (identical(model$checkpoint_status, "no_crb")) {
    return(list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Not cached",
      crb_status = "no-crb"
    ))
  }
  if (identical(model$checkpoint_status, "unchecked")) {
    return(list(label = "Needs checking", ready = FALSE))
  }
  if (identical(model$readiness_label, "CRB ready")) {
    return(list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Cached · opens fast",
      crb_status = "ready"
    ))
  }
  if (identical(model$readiness_label, "Loading")) {
    return(list(label = "Loading source…", ready = FALSE))
  }
  if (isTRUE(model$checked)) {
    return(list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Not cached",
      crb_status = "no-crb"
    ))
  }
  if (identical(model$readiness_label, "Reload required")) {
    return(list(label = "Source reload required", ready = FALSE))
  }
  list(label = "Needs checking", ready = FALSE)
}

builder_dataset_rail_row_ui <- function(model) {
  status <- builder_dataset_rail_status(model)
  # The rail is narrow: "Built-in example" was cut to "Built-in exam...".
  format_label <- if (identical(model$format, "Built-in example")) {
    "Example"
  } else {
    model$format
  }
  cells <- format(model$cells, big.mark = ",")
  meta_label <- sprintf("%s cells · %s", cells, format_label)
  # The load time is only interesting while loading; once ready it moves to
  # the tooltip with the full, untruncated line.
  elapsed <- model$import_elapsed_ms
  meta_title <- paste0(
    sprintf("%s cells · %s", cells, model$format),
    if (length(elapsed) == 1L && is.finite(elapsed)) {
      sprintf(" · loaded in %.1fs", elapsed / 1000)
    }
  )
  shiny::div(
    class = paste(
      c("ds", "ds--ready", if (model$selected) "is-active"),
      collapse = " "
    ),
    `data-ds` = model$id,
    `data-load-state` = "ready",
    `data-rail-fingerprint` = builder_dataset_rail_row_fingerprint(model),
    shiny::tags$button(
      class = "ds-pick builder-pick",
      id = paste0("pick_", model$id),
      `data-ds` = model$id,
      `aria-label` = paste0("Open ", model$label),
      `aria-current` = if (model$selected) "true" else NULL,
      shiny::span(class = "ds-idx", model$index),
      shiny::span(
        class = "ds-body",
        shiny::span(class = "nm", title = model$label, model$label),
        shiny::span(
          class = if (identical(model$readiness_label, "Needs attention")) {
            "meta bad"
          } else {
            "meta"
          },
          title = meta_title,
          meta_label
        ),
        shiny::span(
          class = paste(
            "ds-ready-summary",
            if (status$ready) "is-checked" else "needs-review"
          ),
          role = "status",
          `aria-live` = "polite",
          `aria-atomic` = "true",
          shiny::span(
            class = paste(
              "ds-ready-stamp",
              if (status$ready) "is-checked" else "needs-review"
            ),
            status$label
          ),
          if (!is.null(status$crb_label)) {
            shiny::span(
              class = paste("ds-crb-status", paste0("is-", status$crb_status)),
              status$crb_label
            )
          }
        )
      )
    ),
    shiny::div(
      class = "ds-actions",
      shiny::tags$button(
        class = "ds-move builder-reorder",
        title = "Move up",
        `aria-label` = paste0("Move ", model$label, " up"),
        `data-ds` = model$id,
        `data-direction` = "up",
        `data-icon` = "up",
        `data-at-edge` = if (!model$can_up) "true" else NULL,
        disabled = if (!model$can_up) "disabled" else NULL
      ),
      shiny::tags$button(
        class = "ds-move builder-reorder",
        title = "Move down",
        `aria-label` = paste0("Move ", model$label, " down"),
        `data-ds` = model$id,
        `data-direction` = "down",
        `data-icon` = "down",
        `data-at-edge` = if (!model$can_down) "true" else NULL,
        disabled = if (!model$can_down) "disabled" else NULL
      ),
      shiny::tags$button(
        class = "ds-del btn-remove-soft builder-drop",
        title = "Remove this dataset",
        `aria-label` = paste0("Remove ", model$label),
        `data-ds` = model$id,
        `data-confirm` = if (model$confirm) "true" else "false",
        shiny::span(class = "ds-action-label", "Remove")
      )
    )
  )
}

builder_dataset_rail_model <- function(
  state,
  current = state$current_dataset,
  checked = character(),
  checkpoint_states = list(),
  state_cache = NULL
) {
  ids <- .builder_store_assert(state)
  lapply(seq_along(state$datasets), function(index) {
    builder_dataset_rail_row_model(
      state$datasets[[index]],
      index,
      length(ids),
      current,
      checked,
      checkpoint_states[[state$datasets[[index]]$id]] %||% NULL,
      state_cache
    )
  })
}

builder_dataset_rail_ui <- function(
  state,
  current = state$current_dataset,
  checked = character(),
  checkpoint_states = list(),
  state_cache = NULL
) {
  rows <- builder_dataset_rail_model(
    state,
    current,
    checked,
    checkpoint_states,
    state_cache
  )
  if (!length(rows)) {
    return(shiny::div(
      class = "rail-empty",
      "No datasets yet"
    ))
  }
  shiny::tagList(lapply(rows, builder_dataset_rail_row_ui))
}

builder_dataset_rail_patch <- function(
  state,
  current = state$current_dataset,
  checked = character(),
  checkpoint_states = list(),
  row_cache = NULL,
  force_html = FALSE,
  state_cache = NULL
) {
  if (is.null(row_cache)) {
    rows <- builder_dataset_rail_model(
      state,
      current,
      checked,
      checkpoint_states,
      state_cache
    )
    records <- lapply(rows, function(model) {
      list(
        id = model$id,
        fingerprint = builder_dataset_rail_row_fingerprint(model),
        html = htmltools::renderTags(builder_dataset_rail_row_ui(model))$html
      )
    })
  } else {
    if (!is.environment(row_cache)) {
      stop("`row_cache` must be an environment.", call. = FALSE)
    }
    entries <- state$datasets %||% list()
    ids <- vapply(entries, `[[`, character(1), "id")
    stale <- setdiff(ls(row_cache, all.names = TRUE), ids)
    if (length(stale)) {
      rm(list = stale, envir = row_cache)
    }
    records <- lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]
      id <- ids[[index]]
      key <- list(
        revision = as.integer(entry$revision %||% 0L),
        source_id = entry$source_id %||% NULL,
        output_id = entry$output_id %||% NULL,
        load_state = entry$load_state %||% "loaded",
        snapshot = entry$snapshot[c("object_md5", "source_md5")],
        artifact = entry$project_artifact$fingerprint$md5 %||% NULL,
        label = entry$settings$name %||% id,
        cells = entry$profile$n_cells %||% 0L,
        format = entry$format %||% NULL,
        import_elapsed_ms = entry$import_elapsed_ms %||% NULL,
        index = as.integer(index),
        can_down = index < length(entries),
        checked = id %in% checked,
        checkpoint = checkpoint_states[[id]] %||% NULL,
        selected = identical(id, current)
      )
      cached <- get0(id, envir = row_cache, inherits = FALSE)
      if (is.list(cached) && identical(cached$key, key)) {
        return(list(
          id = id,
          fingerprint = cached$fingerprint,
          html = if (isTRUE(force_html)) cached$html else NULL
        ))
      }
      model <- builder_dataset_rail_row_model(
        entry,
        index,
        length(entries),
        current,
        checked,
        checkpoint_states[[id]] %||% NULL,
        state_cache
      )
      fingerprint <- builder_dataset_rail_row_fingerprint(model)
      html <- htmltools::renderTags(builder_dataset_rail_row_ui(model))$html
      unchanged <- is.list(cached) &&
        identical(cached$fingerprint, fingerprint)
      assign(
        id,
        list(key = key, fingerprint = fingerprint, html = html),
        envir = row_cache
      )
      list(
        id = id,
        fingerprint = fingerprint,
        html = if (isTRUE(force_html) || !unchanged) html else NULL
      )
    })
  }
  list(
    rows = records,
    empty_html = htmltools::renderTags(shiny::div(
      class = "rail-empty",
      "No datasets yet"
    ))$html
  )
}
