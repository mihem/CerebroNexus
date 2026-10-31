## Builder project controls.

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

builder_project_save_location_note <- function() {
  "The project is one small file. It lists the datasets and your choices, not the data."
}

builder_project_existing_folder_dialog <- function(path) {
  modalDialog(
    title = builder_dialog_title("Update existing project?", "Save project"),
    builder_project_dialog_content(
      "This folder already contains a Builder project.",
      paste(
        "Its project file will be replaced by the current workspace.",
        "Other files in the folder will not be changed."
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

builder_project_status_ui <- function(project = NULL, phase = "none") {
  label <- if (is.null(project)) {
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
      save_failed = "Project save failed",
      "Saved"
    )
  }
  status_class <- switch(
    phase,
    dirty = "is-dirty",
    choosing = "is-busy",
    saving = "is-busy",
    autosaving = "is-busy",
    opening = "is-busy",
    restoring = "is-busy",
    save_failed = "is-error",
    if (is.null(project)) "is-unsaved" else "is-saved"
  )
  tags$div(
    class = paste("builder-project-status", status_class),
    role = "status",
    `aria-live` = "polite",
    if (!is.null(project)) tags$strong(project$name),
    tags$span(
      class = "builder-project-status-detail",
      tags$span(class = "builder-project-status-dot", `aria-hidden` = "true"),
      tags$span(label)
    )
  )
}

## What could not be brought back when a project was opened, or NULL.
builder_project_restore_notice <- function(outcome) {
  join <- function(names) paste(names, collapse = ", ")
  parts <- c(
    if (length(outcome$upload)) {
      paste0(
        "Uploaded in the browser, add again: ",
        join(outcome$upload),
        "."
      )
    },
    if (length(outcome$missing)) {
      paste0("Source not found: ", join(outcome$missing), ".")
    },
    if (length(outcome$failed)) {
      paste0("Could not be loaded: ", join(outcome$failed), ".")
    }
  )
  if (!length(parts)) {
    return(NULL)
  }
  paste(c("Some datasets were not restored.", parts), collapse = " ")
}
