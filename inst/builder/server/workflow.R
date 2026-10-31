selected_workflow_stage <- reactiveVal(isolate(workflow())$stage)
observe({
  next_stage <- workflow()$stage
  if (!identical(next_stage, isolate(selected_workflow_stage()))) {
    selected_workflow_stage(next_stage)
  }
})

output$workflow_progress <- renderUI({
  builder_workflow_progress_ui(
    selected_workflow_stage(),
    available = builder_workflow_stage_availability(
      workflow(),
      datasets_ready = workflow_has_datasets()
    ),
    confirmed = is.list(workflow()$confirmation),
    locked = builder_build_controls_locked(build_flow())
  )
})

navigate_workflow_stage <- function(stage) {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("navigate_workflow"))
  ) {
    return(invisible(FALSE))
  }
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return(invisible(FALSE))
  }
  state <- isolate(workflow())
  datasets_ready <- length(isolate(store()$datasets %||% list())) > 0L
  if (stage %in% c("review", "build") && !isTRUE(all_datasets_checked())) {
    return(invisible(FALSE))
  }
  if (identical(stage, "review")) {
    if (!datasets_ready) {
      return(invisible(FALSE))
    }
    snapshot <- isolate(current_review_snapshot())
    if (!builder_review_snapshot_valid(snapshot)) {
      return(invisible(FALSE))
    }
    state <- builder_reduce_workflow(
      state,
      list(type = "open_review", snapshot = snapshot)
    )
  } else {
    available <- builder_workflow_stage_availability(
      state,
      datasets_ready = datasets_ready
    )
    if (!isTRUE(available[[stage]])) {
      return(invisible(FALSE))
    }
  }
  if (identical(stage, "build")) {
    snapshot <- state$review_snapshot
    if (!builder_workflow_confirmation_matches(state, snapshot)) {
      workflow(builder_reduce_workflow(
        state,
        list(type = "open_review", snapshot = snapshot)
      ))
      builder_notify(
        "warning",
        "Settings changed. Review the updated plan before building.",
        target = "review"
      )
      return(invisible(FALSE))
    }
  }
  workflow(builder_reduce_workflow(
    state,
    list(type = "navigate", stage = stage, datasets_ready = TRUE)
  ))
  if (!identical(stage, "build")) {
    build_flow(list(stage = "idle", plan = NULL))
    session$sendCustomMessage("builder_build_dialog", list(action = "close"))
  }
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = stage))
    },
    once = TRUE
  )
  invisible(TRUE)
}

observeEvent(input$workflow_stage_upload, {
  navigate_workflow_stage("upload")
})
observeEvent(input$workflow_stage_configure, {
  navigate_workflow_stage("configure")
})
observeEvent(input$workflow_stage_review, {
  navigate_workflow_stage("review")
})
observeEvent(input$workflow_stage_build, {
  navigate_workflow_stage("build")
})
observeEvent(input$configure_datasets, {
  navigate_workflow_stage("configure")
})

## Import progress changes many times a second; the workbench only needs
## the counts, so it is not rendered again on every tick.
workbench_import_counts <- reactiveVal(c(active = 0L, failed = 0L))
observe({
  states <- vapply(
    imports()$entries %||% list(),
    function(entry) entry$load_state,
    character(1)
  )
  counts <- c(
    active = sum(!states %in% c("error", "cancelled")),
    failed = sum(states == "error")
  )
  if (!identical(counts, isolate(workbench_import_counts()))) {
    workbench_import_counts(counts)
  }
})

output$workbench <- renderUI({
  stage <- selected_workflow_stage()
  import_counts <- workbench_import_counts()
  switch(
    stage,
    upload = if (
      import_counts[["failed"]] > 0L && import_counts[["active"]] == 0L
    ) {
      builder_failed_import_workbench_ui(
        import_counts[["failed"]],
        dataset_count = length(store()$datasets %||% list())
      )
    } else {
      tagAppendAttributes(
        builder_empty_workbench_ui(
          project_active = !is.null(builder_project()),
          dataset_count = length(store()$datasets %||% list()),
          formats = builder_format_capabilities(),
          examples = builder_example_directory(),
          loading_count = import_counts[["active"]]
        ),
        class = "builder-stage-upload",
        `data-workflow-stage` = "upload"
      )
    },
    configure = render_configure_workbench(),
    review = render_review_workbench(),
    build = render_build_workbench(),
    stop("Unsupported Builder workflow stage.", call. = FALSE)
  )
})

observeEvent(input$continue_to_review, {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("navigate_workflow"))
  ) {
    return()
  }
  req(all_datasets_checked())
  snapshot <- current_review_snapshot()
  req(builder_review_snapshot_valid(snapshot))
  workflow(builder_reduce_workflow(
    isolate(workflow()),
    list(type = "open_review", snapshot = snapshot)
  ))
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "review"))
    },
    once = TRUE
  )
})

render_build_workbench <- function() {
  state <- workflow()
  snapshot <- state$review_snapshot
  if (
    !identical(state$stage, "build") ||
      !builder_review_snapshot_valid(snapshot) ||
      !builder_workflow_confirmation_matches(state, snapshot)
  ) {
    return(NULL)
  }
  builder_build_workbench_ui(builder_review_model(snapshot))
}

observeEvent(input$back_to_review, {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("navigate_workflow"))
  ) {
    return()
  }
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return()
  }
  state <- isolate(workflow())
  if (!identical(state$stage, "build")) {
    return()
  }
  workflow(builder_reduce_workflow(
    state,
    list(type = "back_to_review")
  ))
  build_flow(list(stage = "idle", plan = NULL))
  session$sendCustomMessage(
    "builder_build_dialog",
    list(action = "close")
  )
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "review"))
    },
    once = TRUE
  )
})

observe({
  state <- workflow()
  if (
    (is.null(state$review_snapshot) || is.null(state$confirmation)) &&
      !is.null(isolate(selected_output()))
  ) {
    selected_output(NULL)
  }
})
