## Builder project controls and recovery surfaces.

builder_project_toolbar_ui <- function() {
  tags$div(
    class = "builder-project-toolbar",
    uiOutput("project_status"),
    actionButton(
      "open_builder_project",
      tagList(
        span(`aria-hidden` = "true", shiny::icon("folder-open")),
        span("Open")
      ),
      class = "btn builder-project-action"
    ),
    actionButton(
      "save_builder_project",
      tagList(
        span(`aria-hidden` = "true", shiny::icon("floppy-disk")),
        span("Save")
      ),
      class = "btn builder-project-action"
    )
  )
}

builder_project_status_ui <- function(
  project = NULL,
  phase = "none",
  source_sync = NULL,
  crb = NULL
) {
  syncing <- is.list(source_sync) && identical(source_sync$status, "syncing")
  crb_building <- is.list(crb) &&
    isTRUE(as.integer(crb$total %||% 0L) > 0L) &&
    phase %in% c("clean", "dirty")
  cancelling <- is.list(source_sync) &&
    identical(source_sync$status, "cancelling")
  sync_failed <- is.list(source_sync) && identical(source_sync$status, "failed")
  sync_ready <- is.list(source_sync) && identical(source_sync$status, "ready")
  label <- if (cancelling) {
    "Stopping file copy…"
  } else if (syncing) {
    bytes_total <- suppressWarnings(as.double(source_sync$bytes_total %||% 0))
    if (
      isTRUE(bytes_total > 0) &&
        exists("builder_file_human_size", mode = "function")
    ) {
      paste0(
        "Saving source files · ",
        builder_file_human_size(source_sync$bytes_done %||% 0),
        " of ",
        builder_file_human_size(bytes_total)
      )
    } else {
      paste0(
        "Saving source files · ",
        source_sync$completed %||% 0L,
        " of ",
        source_sync$total %||% 0L
      )
    }
  } else if (sync_failed) {
    "Some source files could not be saved"
  } else if (crb_building) {
    paste0(
      "Building dataset cache · ",
      as.integer(crb$position %||% 1L),
      " of ",
      as.integer(crb$total),
      " · ",
      crb$name %||% "Dataset"
    )
  } else if (sync_ready && identical(phase, "clean")) {
    "Saved · Safe to close"
  } else if (is.null(project)) {
    "No project saved"
  } else {
    switch(
      phase,
      dirty = "Unsaved changes",
      choosing = "Choosing project location…",
      saving = "Saving project…",
      autosaving = "Saving project…",
      opening = "Opening project…",
      restoring = "Restoring project…",
      registering = "Saving dataset cache…",
      save_failed = "Project save failed",
      conflict = "Project changed · Reopen to continue",
      "Saved"
    )
  }
  status_class <- if (syncing || cancelling || (crb_building && !sync_failed)) {
    "is-busy"
  } else if (sync_failed) {
    "is-error"
  } else {
    switch(
      phase,
      dirty = "is-dirty",
      choosing = "is-busy",
      saving = "is-busy",
      autosaving = "is-busy",
      opening = "is-busy",
      restoring = "is-busy",
      registering = "is-busy",
      save_failed = "is-error",
      conflict = "is-error",
      if (is.null(project)) "is-unsaved" else "is-saved"
    )
  }
  tags$div(
    class = paste("builder-project-status", status_class),
    role = "status",
    `aria-live` = "polite",
    if (!is.null(project)) tags$strong(project$name),
    tags$span(
      class = "builder-project-status-detail",
      tags$span(class = "builder-project-status-dot", `aria-hidden` = "true"),
      if (crb_building && !syncing && !cancelling && !sync_failed) {
        tagList(
          # The label is not the last child here, so it carries the
          # truncation the stylesheet gives the last status span.
          tags$span(class = "builder-project-status-label", label),
          tags$button(
            id = "cancel_builder_project_crbs",
            type = "button",
            class = "action-button builder-project-status-cancel",
            `aria-label` = "Cancel dataset cache builds",
            "Cancel"
          )
        )
      } else {
        tags$span(label)
      }
    )
  )
}

# Every Builder dialog title is a small kicker over the question itself.
builder_dialog_title <- function(title, kicker = "Builder project") {
  tagList(
    tags$span(class = "builder-project-modal-kicker", kicker),
    tags$span(title)
  )
}

builder_project_dialog_content <- function(primary, secondary, icon) {
  tags$div(
    class = "builder-project-dialog",
    tags$p(class = "builder-project-dialog-lead", primary),
    tags$div(
      class = "builder-project-dialog-summary",
      tags$span(
        class = "builder-project-dialog-icon",
        `aria-hidden` = "true",
        shiny::icon(icon)
      ),
      tags$p(secondary)
    )
  )
}

builder_legacy_qs_notice_content <- function(datasets, origin = "add") {
  datasets <- Filter(is.list, datasets %||% list())
  count <- length(datasets)
  labels <- vapply(
    datasets,
    function(dataset) {
      label <- as.character(dataset$name %||% dataset$id %||% "Dataset")
      if (length(label) != 1L || is.na(label) || !nzchar(trimws(label))) {
        "Dataset"
      } else {
        label
      }
    },
    character(1)
  )
  duplicate_labels <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  if (any(duplicate_labels)) {
    ids <- vapply(
      datasets,
      function(dataset) as.character(dataset$id %||% "Dataset"),
      character(1)
    )
    labels[duplicate_labels] <- paste0(
      labels[duplicate_labels],
      " (",
      ids[duplicate_labels],
      ")"
    )
  }
  context <- if (identical(origin, "project")) {
    "project"
  } else {
    "files you selected"
  }
  tags$div(
    class = "builder-project-dialog-note builder-legacy-qs-notice",
    tags$span(
      class = "builder-project-dialog-icon",
      `aria-hidden` = "true",
      shiny::icon("exclamation-triangle")
    ),
    tags$div(
      class = "builder-legacy-qs-copy",
      tags$p(
        class = "builder-legacy-qs-summary",
        paste0(
          count,
          if (count == 1L) " dataset in the " else " datasets in the ",
          context,
          if (count == 1L) " uses " else " use ",
          "the deprecated .qs format."
        )
      ),
      tags$p(
        paste(
          "These files can still open. Use QS2 for future projects."
        )
      ),
      if (length(labels)) {
        tags$details(
          tags$summary(paste0("Datasets (", length(labels), ")")),
          do.call(tags$ul, unname(lapply(labels, tags$li)))
        )
      }
    )
  )
}

builder_legacy_qs_notice_dialog <- function(datasets, origin = "add") {
  project_notice <- identical(origin, "project")
  modalDialog(
    title = builder_dialog_title("Legacy QS files", "Compatibility warning"),
    tags$div(
      class = "builder-project-dialog builder-legacy-qs-dialog",
      builder_legacy_qs_notice_content(datasets, origin = origin)
    ),
    footer = if (project_notice) {
      tagList(
        actionButton(
          "cancel_builder_project_open",
          "Cancel",
          class = "btn btn-secondary"
        ),
        actionButton(
          "continue_builder_project_legacy_qs",
          "Review Datasets",
          class = "btn btn-primary"
        )
      )
    } else {
      tags$button(
        type = "button",
        class = "btn btn-primary",
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal",
        "Continue"
      )
    },
    easyClose = !project_notice,
    size = "m"
  )
}

builder_project_opening_dialog <- function() {
  modalDialog(
    title = builder_dialog_title("Checking project"),
    tags$div(
      class = "builder-project-dialog builder-project-opening-dialog",
      tags$div(
        class = "builder-project-dialog-summary",
        tags$span(
          class = "builder-project-dialog-icon",
          `aria-hidden` = "true",
          shiny::icon("folder-open")
        ),
        tags$div(
          tags$strong("Preparing saved datasets"),
          tags$p(
            paste(
              "Checking the project and saved dataset cache…"
            )
          )
        )
      ),
      tags$div(
        class = "builder-project-opening-progress",
        `aria-hidden` = "true",
        tags$span(),
        tags$span(),
        tags$span()
      )
    ),
    footer = actionButton(
      "cancel_builder_project_open",
      "Cancel",
      class = "btn-default"
    ),
    easyClose = FALSE,
    size = "s"
  )
}

builder_project_open_wait_dialog <- function(message) {
  modalDialog(
    title = builder_dialog_title("Opening project"),
    tags$div(
      class = "builder-project-dialog builder-project-opening-dialog",
      role = "status",
      `aria-live` = "polite",
      tags$div(
        class = "builder-project-dialog-summary",
        tags$span(
          class = "builder-project-dialog-icon",
          `aria-hidden` = "true",
          shiny::icon("hourglass-half")
        ),
        tags$div(
          tags$strong("Waiting for the background worker"),
          tags$p(message)
        )
      ),
      tags$div(
        class = "builder-project-opening-progress",
        `aria-hidden` = "true",
        tags$span(),
        tags$span(),
        tags$span()
      )
    ),
    footer = actionButton(
      "cancel_builder_project_open_wait",
      "Cancel",
      class = "btn-default"
    ),
    easyClose = FALSE,
    size = "s"
  )
}

builder_project_backup_recovery_dialog <- function(offer) {
  modalDialog(
    title = builder_dialog_title("Recover Builder project?", "Open project"),
    tags$p(
      "The project file is damaged. A verified backup is available."
    ),
    tags$dl(
      tags$dt("Project"),
      tags$dd(as.character(offer$project_name %||% "Builder project")),
      tags$dt("Backup revision"),
      tags$dd(as.character(offer$project_revision %||% 0L)),
      tags$dt("Datasets"),
      tags$dd(as.character(offer$dataset_count %||% 0L))
    ),
    tags$p(
      "The damaged file will be kept. The backup is checked before restoring."
    ),
    footer = tagList(
      actionButton(
        "cancel_builder_project_backup_recovery",
        "Cancel",
        class = "btn-default"
      ),
      actionButton(
        "confirm_builder_project_backup_recovery",
        "Restore verified backup",
        class = "btn-primary"
      )
    ),
    easyClose = FALSE
  )
}

builder_project_save_location_note <- function() {
  paste(
    "Includes source files and settings.",
    "Keep this page open to prepare the dataset cache after saving."
  )
}

builder_project_existing_folder_dialog <- function(path) {
  modalDialog(
    title = builder_dialog_title("Update existing project?", "Save project"),
    builder_project_dialog_content(
      "This folder already contains a Builder project.",
      paste(
        "Its Builder-managed project data will be replaced by the current",
        "workspace. Other files in the folder will not be changed."
      ),
      "exclamation-triangle"
    ),
    tags$p(
      class = "builder-project-dialog-path",
      tags$code(path)
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        "choose_another_builder_project_folder",
        "Choose another folder",
        class = "btn-default"
      ),
      actionButton(
        "confirm_existing_builder_project_folder",
        "Update project",
        class = "btn btn-primary"
      )
    ),
    easyClose = FALSE,
    size = "m"
  )
}

builder_project_nonempty_folder_dialog <- function(path) {
  modalDialog(
    title = builder_dialog_title(
      "Folder already contains files",
      "Save project"
    ),
    builder_project_dialog_content(
      "Create the Builder project in this folder?",
      paste(
        "Existing files will be kept. Builder will add its project manifest",
        "and managed data."
      ),
      "exclamation-triangle"
    ),
    tags$p(
      class = "builder-project-dialog-path",
      tags$code(path)
    ),
    footer = tagList(
      actionButton(
        "cancel_builder_project_folder",
        "Cancel",
        class = "btn-default"
      ),
      actionButton(
        "choose_another_builder_project_folder",
        "Choose another folder",
        class = "btn-default"
      ),
      actionButton(
        "confirm_builder_project_folder",
        "Create project here",
        class = "btn btn-primary"
      )
    ),
    easyClose = FALSE,
    size = "m"
  )
}

builder_project_restore_size <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes %||% NA_real_))
  if (length(bytes) != 1L || !is.finite(bytes) || bytes < 0) {
    return(NULL)
  }
  units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- if (bytes == 0) {
    1L
  } else {
    min(length(units), floor(log(bytes, base = 1024)) + 1L)
  }
  value <- bytes / (1024^(unit_index - 1L))
  digits <- if (unit_index <= 2L || value >= 100) {
    0L
  } else if (value >= 10) {
    1L
  } else {
    2L
  }
  paste0(
    formatC(value, format = "f", digits = digits),
    " ",
    units[[unit_index]]
  )
}

builder_project_restore_source_size <- function(record) {
  builder_project_restore_size(record$source$fingerprint$bytes %||% NULL)
}

builder_project_restore_artifact_size <- function(record) {
  artifact <- record$artifact %||% NULL
  if (!is.list(artifact)) {
    return(NULL)
  }
  bytes <- c(
    artifact$fingerprint$bytes %||% NA_real_,
    vapply(
      artifact$members %||% list(),
      function(member) {
        suppressWarnings(as.numeric(member$fingerprint$bytes %||% NA_real_))
      },
      numeric(1)
    )
  )
  bytes <- suppressWarnings(as.numeric(bytes))
  bytes <- bytes[is.finite(bytes) & bytes >= 0]
  if (!length(bytes)) {
    return(NULL)
  }
  builder_project_restore_size(sum(bytes))
}

builder_project_restore_notice <- function(
  total,
  skipped,
  reloaded,
  reused = 0L,
  sourced = total - skipped - reused
) {
  if (skipped > 0L) {
    paste0(
      skipped,
      if (skipped == 1L) " dataset is" else " datasets are",
      " set to Skip and will stay unloaded in this session."
    )
  } else if (reloaded > 0L) {
    paste0(
      reloaded,
      if (reloaded == 1L) " dataset will" else " datasets will",
      " reload from source and reset saved settings."
    )
  } else if (total > 0L) {
    paste0(
      reused,
      if (reused == 1L) {
        " dataset will open from cache; "
      } else {
        " datasets will open from cache; "
      },
      sourced,
      if (sourced == 1L) {
        " dataset will load its original source."
      } else {
        " datasets will load their original sources."
      }
    )
  } else {
    "This project does not contain saved datasets."
  }
}

builder_project_restore_row_ui <- function(record) {
  status <- record$runtime_restore_status %||% NULL
  if (!is.list(status)) {
    stop(
      "The background Project check did not return a dataset status.",
      call. = FALSE
    )
  }
  labels <- c(
    reuse = "Open from cache (fast)",
    resume = "Reload from source file",
    reload = "Re-import source · Reset settings",
    skip = "Skip · Not loaded"
  )
  choices <- stats::setNames(status$actions, labels[status$actions])
  selected <- status$default_action
  status_tone <- if (!is.null(status$problem)) {
    "warning"
  } else if (isTRUE(status$checked)) {
    "ready"
  } else {
    "neutral"
  }
  status_icon <- switch(
    status_tone,
    ready = "check",
    warning = "exclamation",
    "info"
  )
  source_size <- builder_project_restore_source_size(record)
  artifact_size <- builder_project_restore_artifact_size(record)
  status_detail <- status$detail %||% status$problem %||% NULL
  crb_tag <- if (isTRUE(status$checked)) {
    if (isTRUE(status$crb_ready)) {
      list(label = "Cached", tone = "ready")
    } else {
      list(label = "Not cached", tone = "none")
    }
  } else {
    NULL
  }
  status_label <- if (is.null(crb_tag)) status$message else "Checked"
  tags$section(
    class = "builder-project-restore-row",
    `data-current-action` = selected,
    `data-status-tone` = status_tone,
    tags$div(
      class = "builder-project-restore-summary",
      tags$h5(record$name %||% record$id),
      tags$div(
        class = "builder-project-restore-meta",
        tags$span(
          class = "builder-project-restore-status",
          title = status_detail,
          tags$span(
            class = "builder-project-restore-status-icon",
            `aria-hidden` = "true",
            shiny::icon(status_icon)
          ),
          tags$span(status_label),
          if (!is.null(crb_tag)) {
            tags$span(
              class = "builder-project-restore-crb-tag",
              `data-tone` = crb_tag$tone,
              crb_tag$label
            )
          }
        ),
        if (!is.null(status_detail)) {
          tags$span(
            class = "builder-project-restore-status-detail",
            title = status_detail,
            status_detail
          )
        },
        tags$span(
          class = "builder-project-restore-sizes",
          tags$span(
            class = "builder-project-restore-size",
            paste("Source", source_size %||% "unknown")
          ),
          tags$span(
            class = "builder-project-restore-size",
            title = "Stored dataset cache size",
            if (is.null(artifact_size)) {
              "Not cached"
            } else {
              paste("Cache", artifact_size)
            }
          )
        )
      )
    ),
    tags$div(
      class = "builder-project-restore-action",
      tags$span(
        class = "builder-project-restore-action-label",
        `aria-hidden` = "true",
        "Open mode"
      ),
      selectInput(
        paste0("project_restore_", record$id),
        label = tags$span(
          class = "visually-hidden",
          paste("How to open", record$name %||% record$id)
        ),
        choices = choices,
        selected = selected,
        selectize = FALSE,
        width = "100%"
      )
    )
  )
}

builder_project_restore_dialog <- function(manifest) {
  datasets <- manifest$datasets %||% list()
  actions <- vapply(
    datasets,
    function(record) {
      status <- record$runtime_restore_status %||% NULL
      if (!is.list(status)) {
        stop(
          "The background Project check did not return a dataset status.",
          call. = FALSE
        )
      }
      as.character(status$default_action)
    },
    character(1)
  )
  total <- length(datasets)
  skipped <- sum(actions == "skip")
  reloaded <- sum(actions == "reload")
  reused <- sum(actions == "reuse")
  sourced <- sum(actions %in% c("resume", "reload"))
  selected <- total - skipped
  notice_tone <- if (skipped > 0L) {
    "warning"
  } else if (reloaded > 0L) {
    "caution"
  } else if (reused > 0L) {
    "ready"
  } else {
    "neutral"
  }
  modalDialog(
    title = builder_dialog_title("Choose Datasets to Open"),
    tags$div(
      class = "builder-project-restore-dialog",
      tags$section(
        class = "builder-project-restore-overview",
        `aria-label` = "Project summary",
        tags$div(
          class = "builder-project-restore-overview-copy",
          tags$p(
            class = "builder-project-restore-project-name",
            manifest$project$name %||% "Builder project"
          ),
          tags$p(
            class = "builder-project-restore-overview-hint",
            paste0(
              total,
              if (total == 1L) " saved dataset. " else " saved datasets. ",
              "Choose what appears in this Builder session."
            )
          )
        ),
        tags$div(
          class = "builder-project-restore-counts",
          tagList(
            tags$span(
              tags$strong(
                `data-project-restore-count` = "reuse",
                reused
              ),
              " cached"
            ),
            tags$span(
              tags$strong(
                `data-project-restore-count` = "source",
                sourced
              ),
              " from source"
            ),
            tags$span(
              tags$strong(
                `data-project-restore-count` = "skipped",
                skipped
              ),
              " skipped"
            )
          )
        )
      ),
      tags$div(
        class = "builder-project-restore-notice",
        # Success only when something actually opens from the cache.
        `data-tone` = notice_tone,
        role = "status",
        `aria-live` = "polite",
        tags$span(
          class = "builder-project-restore-notice-icon",
          `aria-hidden` = "true",
          shiny::icon(
            switch(
              notice_tone,
              ready = "check",
              neutral = "info",
              "exclamation"
            )
          )
        ),
        tags$p(
          `data-project-restore-notice` = "true",
          builder_project_restore_notice(
            total,
            skipped,
            reloaded,
            reused,
            sourced
          )
        )
      ),
      tags$div(
        class = "builder-project-restore-list",
        tags$div(
          class = "builder-project-restore-columns",
          `aria-hidden` = "true",
          tags$span("Dataset & Status"),
          tags$span("Open mode")
        ),
        lapply(datasets, builder_project_restore_row_ui)
      )
    ),
    footer = tagList(
      tags$span(
        class = "builder-project-restore-footer-summary",
        tagList(
          tags$strong(
            `data-project-restore-footer-selected` = "true",
            selected
          ),
          paste0(" of ", total, " datasets selected")
        )
      ),
      tags$div(
        class = "builder-project-restore-footer-actions",
        actionButton(
          "cancel_builder_project_restore",
          "Cancel",
          class = "btn-default"
        ),
        actionButton(
          "confirm_builder_project_open",
          "Open Project",
          class = "btn btn-primary"
        )
      )
    ),
    easyClose = FALSE,
    size = "l"
  )
}

builder_project_artifact_workbench_ui <- function(entry, root = NULL) {
  artifact <- entry$project_artifact %||% list()
  path <- artifact$path %||% "Dataset cache"
  tags$div(
    class = "builder-stage builder-stage-shell builder-project-artifact-stage",
    `data-workflow-stage` = "configure",
    builder_stage_header_ui(
      "Configure",
      entry$settings$name %||% entry$id,
      "Cached copy ready to build."
    ),
    builder_stage_section_ui(
      "Current data source",
      tags$div(
        class = "builder-project-artifact-card",
        tags$span(class = "builder-project-artifact-mark", "Cache"),
        tags$div(
          tags$strong("Ready to build"),
          tags$details(tags$summary("File location"), tags$p(path))
        )
      ),
      tags$div(
        id = "project-source-load-status",
        class = "builder-project-source-load-status",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        hidden = "hidden",
        tags$span(
          class = "spinner builder-project-source-load-spinner",
          `aria-hidden` = "true"
        ),
        tags$div(
          tags$strong(
            class = "builder-project-source-load-title",
            "Queueing original source…"
          ),
          tags$p(
            class = "builder-project-source-load-detail",
            "Loading source. The cached copy remains available."
          )
        )
      ),
      description = "Load the source to edit. Saved settings are kept."
    ),
    uiOutput("configure_actions")
  )
}

builder_project_artifact_actions_ui <- function(
  message,
  can_continue,
  dataset_checked = FALSE,
  remaining = 0L,
  check_ready = TRUE
) {
  builder_stage_footer_ui(
    message,
    actionButton(
      "project_resume_current_source",
      shiny::tagList(
        tags$span(
          class = "spinner builder-project-source-button-spinner",
          `aria-hidden` = "true"
        ),
        tags$span(
          class = "builder-project-source-button-label",
          "Load original source to edit"
        )
      ),
      class = "btn builder-project-source-load-button",
      `aria-describedby` = "project-source-load-status"
    ),
    builder_configure_primary_action_ui(
      can_continue = can_continue,
      dataset_checked = dataset_checked,
      remaining = remaining,
      check_ready = check_ready
    )
  )
}
