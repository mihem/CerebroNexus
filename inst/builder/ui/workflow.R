builder_workflow_progress_ui <- function(
  stage,
  available,
  confirmed = FALSE,
  locked = FALSE
) {
  stages <- c("upload", "configure", "review", "build")
  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !stage %in% stages
  ) {
    stop("A valid Builder workflow stage is required.", call. = FALSE)
  }
  if (!is.logical(confirmed) || length(confirmed) != 1L || is.na(confirmed)) {
    stop("A confirmation state is required.", call. = FALSE)
  }
  if (
    !is.logical(available) ||
      !identical(names(available), stages) ||
      anyNA(available) ||
      !is.logical(locked) ||
      length(locked) != 1L ||
      is.na(locked)
  ) {
    stop("Valid Builder workflow availability is required.", call. = FALSE)
  }
  labels <- c("Data", "Configure", "Review", "Build")
  descriptions <- c(
    "Add datasets",
    "Set each dataset",
    "Resolve issues",
    "Create output"
  )
  current_index <- match(stage, stages)
  tags$nav(
    class = "builder-workflow-progress",
    `aria-label` = "Builder progress",
    `data-workflow-confirmed` = if (confirmed) "true" else "false",
    tags$ol(lapply(seq_along(stages), function(index) {
      stage_id <- stages[[index]]
      current <- identical(stage, stage_id)
      complete <- index < current_index
      enabled <- isTRUE(available[[stage_id]]) && !isTRUE(locked)
      step_label <- tagList(
        tags$span(
          class = "builder-workflow-step-number",
          if (complete) "✓" else index
        ),
        tags$span(
          class = "builder-workflow-step-copy",
          tags$strong(labels[[index]]),
          tags$small(descriptions[[index]])
        )
      )
      label <- if (current) {
        tags$span(`aria-label` = labels[[index]], step_label)
      } else if (enabled) {
        actionButton(
          paste0("workflow_stage_", stage_id),
          step_label,
          class = "builder-workflow-stage-link",
          `aria-label` = if (complete) {
            paste(labels[[index]], "completed")
          } else {
            labels[[index]]
          }
        )
      } else {
        tags$span(
          `aria-disabled` = "true",
          `aria-label` = if (complete) {
            paste(labels[[index]], "completed")
          } else {
            labels[[index]]
          },
          step_label
        )
      }
      tags$li(
        class = paste(
          if (current) "is-current" else NULL,
          if (isTRUE(available[[stage_id]])) {
            "is-available"
          } else {
            "is-unavailable"
          },
          if (isTRUE(locked) && !current) "is-locked" else NULL,
          if (complete) "is-complete" else NULL
        ),
        `aria-current` = if (current) "step" else NULL,
        label
      )
    }))
  )
}

builder_stage_header_ui <- function(stage, title, intro) {
  tags$header(
    class = "builder-stage-header",
    tags$h2(title),
    tags$p(class = "stage-intro", intro)
  )
}

builder_stage_summary_ui <- function(..., class = NULL) {
  tags$div(class = paste("builder-stage-summary", class), ...)
}

builder_stage_section_ui <- function(
  title,
  ...,
  description = NULL,
  class = NULL
) {
  tags$section(
    class = paste("builder-stage-section", class),
    tags$div(
      class = "builder-stage-section-head",
      tags$h3(title),
      if (!is.null(description)) tags$p(description)
    ),
    ...
  )
}

builder_stage_footer_ui <- function(status, ..., status_class = NULL) {
  tags$footer(
    class = "builder-stage-footer",
    tags$p(
      class = paste("builder-stage-footer-status", status_class),
      status
    ),
    tags$div(class = "builder-stage-footer-actions", ...)
  )
}

builder_configure_action_button <- function(
  id,
  label,
  class,
  disabled = FALSE
) {
  tags$button(
    id = id,
    type = "button",
    class = paste("btn btn-default builder-configure-action", class),
    `data-builder-configure-action` = id,
    disabled = if (isTRUE(disabled)) NA else NULL,
    tags$span(class = "action-label", label)
  )
}

builder_configure_primary_action_ui <- function(
  can_continue,
  dataset_checked = FALSE,
  remaining = 0L,
  check_ready = TRUE
) {
  if (remaining < 1L) {
    return(builder_configure_action_button(
      "continue_to_review",
      "Continue to Review",
      class = "btn-action builder-review-ready",
      disabled = !isTRUE(can_continue)
    ))
  }
  builder_configure_action_button(
    "complete_dataset_check",
    if (isTRUE(dataset_checked)) "Next" else "Finish Checking",
    class = "btn-dataset-check",
    disabled = !isTRUE(check_ready)
  )
}

## Datasets still loading are rows in the rail too; count them so the footer
## never says "0 of 3" next to four rows.
builder_configure_check_message <- function(checked, loaded, loading = 0L) {
  checked <- as.integer(checked)
  loaded <- as.integer(loaded)
  loading <- as.integer(loading)
  if (loading > 0L) {
    return(paste0(
      checked,
      " of ",
      loaded + loading,
      " datasets checked · ",
      loading,
      " still loading"
    ))
  }
  paste0(
    checked,
    " of ",
    loaded,
    " datasets checked · ",
    loaded - checked,
    " remaining"
  )
}

builder_configure_actions_ui <- function(
  message,
  can_continue,
  dataset_checked = FALSE,
  remaining = 0L,
  check_ready = TRUE
) {
  stopifnot(
    is.character(message),
    length(message) == 1L,
    !is.na(message),
    is.logical(can_continue),
    length(can_continue) == 1L,
    !is.na(can_continue),
    is.logical(check_ready),
    length(check_ready) == 1L,
    !is.na(check_ready)
  )
  review_ready <- remaining < 1L && isTRUE(can_continue)
  builder_stage_footer_ui(
    if (review_ready) {
      tagList(
        tags$span(
          class = "builder-stage-footer-status-icon",
          `aria-hidden` = "true",
          "✓"
        ),
        tags$span(
          class = "builder-stage-footer-status-copy",
          tags$strong("All datasets checked"),
          tags$small(message)
        )
      )
    } else {
      message
    },
    builder_configure_primary_action_ui(
      can_continue = can_continue,
      dataset_checked = dataset_checked,
      remaining = remaining,
      check_ready = check_ready
    ),
    status_class = if (review_ready) "is-review-ready" else NULL
  )
}

builder_server_path_dialog <- function(
  title,
  input_id,
  action_id,
  label,
  action_label,
  detail = NULL,
  roots = tryCatch(builder_server_path_roots(), error = function(e) NULL),
  note = NULL,
  kicker = "Builder project"
) {
  roots_id <- paste0(input_id, "_roots")
  modalDialog(
    title = builder_dialog_title(title, kicker),
    tags$p(
      "Enter an absolute path on this server."
    ),
    if (is.character(note) && length(note) == 1L && nzchar(note)) {
      tags$p(class = "builder-project-dialog-note", note)
    },
    if (is.character(detail) && length(detail) == 1L && nzchar(detail)) {
      tags$details(tags$summary("Details"), tags$p(detail))
    },
    tags$label(label, `for` = input_id),
    tags$input(
      id = input_id,
      type = "text",
      class = "shiny-input-text form-control",
      autocomplete = "off",
      spellcheck = "false",
      `aria-describedby` = if (length(roots)) roots_id
    ),
    if (length(roots)) {
      tags$p(
        id = roots_id,
        class = "builder-project-dialog-note builder-server-path-roots",
        paste0("Allowed folders: ", builder_server_path_roots_label(roots))
      )
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton(action_id, action_label, class = "btn btn-primary")
    ),
    # Typed paths must not be lost to a stray backdrop click or Escape.
    easyClose = FALSE,
    size = "m"
  )
}

builder_build_stage_footer_ui <- function(model, controls_disabled = FALSE) {
  builder_stage_footer_ui(
    builder_build_stage_status_label(model),
    actionButton(
      "back_to_review",
      "Back to Review",
      class = "btn",
      disabled = controls_disabled
    ),
    builder_build_stage_primary_action_ui(
      model,
      controls_disabled = controls_disabled
    )
  )
}

builder_build_workbench_ui <- function(model) {
  stopifnot(is.list(model))
  output_label <- if (isTRUE(model$output$make_app)) {
    "CRB files + Viewer app"
  } else {
    "CRB files"
  }
  div(
    class = "builder-stage builder-stage-shell builder-stage-build",
    `data-workflow-stage` = "build",
    builder_stage_header_ui(
      "Build",
      "Build your output",
      "Packaging starts only after Build is clicked."
    ),
    builder_stage_summary_ui(
      class = "builder-build-summary",
      span(
        paste0(
          model$output$crb_count,
          " dataset",
          if (identical(model$output$crb_count, 1L)) "" else "s"
        )
      ),
      span(output_label)
    ),
    uiOutput("build_output_options"),
    div(
      id = "build-stage-status",
      class = "builder-build-stage-status",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      uiOutput("build_stage_status_content")
    ),
    uiOutput("build_stage_footer")
  )
}
