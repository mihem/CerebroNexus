if (!exists(".builderRandomBytes", mode = "function", inherits = TRUE)) {
  .builderRandomBytes <- function(n) {
    getFromNamespace(".builderRandomBytes", "CerebroNexus")(n)
  }
}

## Builder server: build.

## -- build ---------------------------------------------------------------
## The whole export runs in the worker: analyses, matrix write, bundle. This
## process only sends a plan and waits for the report, so the page keeps
## answering while a marker-gene run takes its minutes.
auth_accounts_state <- auth_accounts

observeEvent(
  input$build_output_mode,
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    app_required <- isTRUE(isolate(workflow())$review_snapshot$make_app)
    requested <- identical(input$build_output_mode, "app")
    enabled <- (requested || app_required) &&
      isTRUE(app_capability$available)
    build_mode(enabled)
    if (!enabled) {
      auth_enabled(FALSE)
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(ok = TRUE, error = NULL))
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input$build_initial_dataset,
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    value <- input$build_initial_dataset
    if (builder_has_text(value %||% "")) {
      build_initial_dataset(value)
    }
  },
  ignoreInit = TRUE
)

build_options_dataset_choices <- function(snapshot) {
  items <- snapshot$items %||% list()
  stats::setNames(
    vapply(items, `[[`, character(1), "id"),
    vapply(items, `[[`, character(1), "name")
  )
}

build_options_selected_dataset <- function(dataset_choices) {
  selected <- isolate(build_initial_dataset())
  if (
    (is.null(selected) || !selected %in% dataset_choices) &&
      length(dataset_choices)
  ) {
    selected <- unname(dataset_choices[[1L]])
  }
  selected
}

build_options_page_choices <- function(snapshot, dataset) {
  items <- snapshot$items %||% list()
  index <- match(dataset, vapply(items, `[[`, character(1), "id"))
  expectations <- if (length(index) == 1L && !is.na(index)) {
    items[[index]]$viewer_page_expectations %||% list()
  } else {
    list()
  }
  builder_review_initial_page_choices(expectations)
}

build_options_current <- function(make_app, initial_dataset) {
  options <- isolate(review_options())
  builder_build_options(
    make_app = isTRUE(make_app),
    welcome_message = options$welcome_message,
    initial_page = options$initial_page,
    point_size = options$point_size,
    variable_to_compare = options$variable_to_compare,
    host = options$host,
    port = options$port,
    max_request_size = options$max_request_size,
    display_mode = options$display_mode,
    launch_browser = FALSE,
    show_upload_ui = options$show_upload_ui,
    initial_dataset = initial_dataset
  )
}

build_options_auth_summary <- function() {
  list(
    enabled = isTRUE(isolate(auth_enabled())),
    account_count = as.integer(length(isolate(auth_accounts()))),
    error = isolate(auth_validation())$error %||% NULL,
    available = isTRUE(auth_capability()$available)
  )
}

output$build_output_options <- renderUI({
  req(identical(workflow()$stage, "build"))
  snapshot <- workflow()$review_snapshot
  auth <- auth_capability()
  # Activity messages update the existing controls in place. Re-rendering this
  # form for each background checkpoint update loses in-flight edits and focus.
  # Output mode and starting dataset do not re-render it either: the Viewer
  # App section has its own output and the page list updates in place.
  controls_disabled <- isolate(builder_mutations_locked(
    build_flow(),
    protocol()
  ))
  dataset_choices <- build_options_dataset_choices(snapshot)
  builder_build_options_ui(
    build_options_current(
      isolate(build_mode()),
      build_options_selected_dataset(dataset_choices)
    ),
    app_available = isTRUE(app_capability$available),
    app_reason = app_capability$reason,
    app_required = isTRUE(snapshot$make_app),
    dataset_choices = dataset_choices,
    auth = list(
      enabled = isTRUE(isolate(auth_enabled())),
      account_count = as.integer(length(isolate(auth_accounts()))),
      error = isolate(auth_validation())$error %||% NULL,
      available = isTRUE(auth$available)
    ),
    auth_controls = uiOutput("build_auth_controls"),
    controls_disabled = controls_disabled,
    app_settings = uiOutput("build_app_settings")
  )
})

output$build_app_settings <- renderUI({
  req(identical(workflow()$stage, "build"), isTRUE(build_mode()))
  snapshot <- isolate(workflow())$review_snapshot
  dataset_choices <- build_options_dataset_choices(snapshot)
  selected <- build_options_selected_dataset(dataset_choices)
  builder_build_app_settings_ui(
    build_options_current(TRUE, selected),
    initial_page_choices = build_options_page_choices(snapshot, selected),
    dataset_choices = dataset_choices,
    auth = build_options_auth_summary(),
    auth_controls = uiOutput("build_auth_controls")
  )
})

observeEvent(
  build_initial_dataset(),
  {
    snapshot <- isolate(workflow())$review_snapshot
    choices <- build_options_page_choices(snapshot, build_initial_dataset())
    selected <- isolate(input$build_initial_page) %||%
      isolate(review_options()$initial_page)
    if (!isTRUE(selected %in% choices)) {
      selected <- unname(choices[[1L]])
    }
    updateSelectInput(
      session,
      "build_initial_page",
      choices = choices,
      selected = selected
    )
  },
  ignoreInit = TRUE
)

output$build_auth_controls <- renderUI({
  req(identical(workflow()$stage, "build"), isTRUE(build_mode()))
  auth <- auth_capability()
  builder_build_auth_controls_ui(list(
    enabled = isTRUE(auth_enabled()),
    account_count = as.integer(length(auth_accounts())),
    error = auth_validation()$error %||% NULL,
    available = isTRUE(auth$available)
  ))
})

current_build_options <- function() {
  options <- isolate(review_options())
  builder_build_options(
    make_app = isTRUE(isolate(build_mode())),
    welcome_message = options$welcome_message,
    initial_page = options$initial_page,
    point_size = options$point_size,
    variable_to_compare = options$variable_to_compare,
    host = options$host,
    port = options$port,
    max_request_size = options$max_request_size,
    display_mode = options$display_mode,
    launch_browser = FALSE,
    show_upload_ui = options$show_upload_ui,
    initial_dataset = isolate(build_initial_dataset())
  )
}

build_stage_status_projection <- reactive({
  req(identical(workflow()$stage, "build"))
  flow <- build_flow()
  ## "Saving reusable CRBs" only follows a build this session started. A
  ## plain Save also registers dataset caches; that is not a build, and Build
  ## stays available (its own lock explains the wait). The phase is read
  ## reactively so the card updates when registration ends instead of
  ## staying on "Build in progress".
  project_phase <- if (
    exists(
      "builder_project_operation_phase",
      mode = "function",
      inherits = TRUE
    )
  ) {
    builder_project_operation_phase()
  } else {
    "idle"
  }
  finalizing <- identical(flow$stage %||% NULL, "building") &&
    project_phase %in% c("registering", "saving")
  builder_build_stage_status_model(
    flow = flow,
    protocol = protocol(),
    note = if (finalizing) {
      "Export complete. Saving reusable CRBs to the Builder Project…"
    } else {
      busy_note()
    },
    result = result(),
    finalizing = finalizing,
    last_output_parent = builder_build_last_parent()
  )
})

builder_build_reuse_state <- new.env(parent = emptyenv())
builder_build_reuse_state$plan <- NULL

builder_build_remember_plan <- function(plan) {
  builder_build_reuse_state$plan <- plan
  builder_build_reuse_state$record_artifacts <- is.environment(
    builder_session_reuse_cache_for_build()
  )
  invisible(plan)
}

## Record verified CRBs for the next build in this session. A failed build
## forgets entries it tried to reuse, so a stale cache cannot fail twice.
builder_build_record_reuse <- function(value) {
  plan <- builder_build_reuse_state$plan
  if (is.null(plan) || !is.list(value) || is.null(value$state)) {
    return(invisible(FALSE))
  }
  builder_build_reuse_state$plan <- NULL
  cache <- builder_session_reuse_cache
  if (value$state %in% c("failure", "recovery_required")) {
    reused <- vapply(
      plan$items,
      function(item) is.list(item$reused_artifact),
      logical(1)
    )
    ids <- vapply(plan$items, `[[`, character(1), "id")
    builder_plan_reuse_forget(cache, ids[reused])
    return(invisible(FALSE))
  }
  records <- tryCatch(
    builder_build_reuse_records(plan, value),
    error = function(error) list()
  )
  items <- plan$items
  names(items) <- vapply(items, `[[`, character(1), "id")
  for (entry in records) {
    if (isTRUE(builder_build_reuse_state$record_artifacts)) {
      builder_plan_reuse_store(cache, entry$key, entry$record)
    }
    builder_plan_verification_store(
      builder_session_verification_cache,
      entry$md5,
      items[[entry$id]],
      entry$verification
    )
  }
  invisible(length(records) > 0L)
}

observeEvent(
  result(),
  {
    value <- result()
    builder_build_record_reuse(value)
    if (
      is.list(value) &&
        (identical(value$state, "failure") ||
          identical(value$state, "recovery_required"))
    ) {
      message(
        "Builder build failed: ",
        value$error %||% value$message %||% "Unknown error."
      )
    }
  },
  ignoreInit = TRUE
)

output$build_stage_status_content <- renderUI({
  body <- builder_build_stage_status_body_ui(build_stage_status_projection())
  if (is.null(body)) {
    return(NULL)
  }
  tags$section(
    class = "builder-stage-section builder-build-status-section",
    tags$h3("Build status"),
    body
  )
})

output$build_stage_footer <- renderUI({
  builder_build_stage_footer_ui(
    build_stage_status_projection(),
    controls_disabled = builder_build_controls_locked(build_flow())
  )
})

## Dataset/step progress written by the worker (and phases written by the
## settlement process) for the Build progress panel.
builder_build_progress_path <- reactive({
  pending <- protocol()$pending
  path <- if (is.list(pending) && identical(pending$kind, "build")) {
    pending$payload$progress_path %||% NULL
  } else {
    NULL
  }
  if (is.null(path)) {
    context <- release_settlement_context()
    if (is.list(context)) {
      path <- context$progress_path %||% NULL
    }
  }
  if (builder_has_text(path %||% "")) path else NULL
})

builder_build_progress_sent <- new.env(parent = emptyenv())
builder_build_progress_sent$message <- NULL

builder_send_build_progress <- function(message) {
  if (identical(message, builder_build_progress_sent$message)) {
    return(invisible(FALSE))
  }
  builder_build_progress_sent$message <- message
  session$sendCustomMessage("builder_build_progress", message)
  invisible(TRUE)
}

observe({
  path <- builder_build_progress_path()
  if (is.null(path)) {
    if (!is.null(builder_build_progress_sent$message)) {
      builder_send_build_progress(list(active = FALSE))
    }
    return()
  }
  invalidateLater(750)
  detail <- tryCatch(
    builder_build_progress_detail(path),
    error = function(error) NULL
  )
  if (is.null(detail)) {
    return()
  }
  message <- builder_build_progress_message(detail)
  # The browser runs the clock; resend only when the step changes.
  sent <- builder_build_progress_sent$message
  if (
    is.list(sent) &&
      identical(
        sent[c("active", "phase", "text")],
        message[c(
          "active",
          "phase",
          "text"
        )]
      )
  ) {
    return()
  }
  builder_send_build_progress(message)
})

observe({
  session$sendCustomMessage(
    "builder_dataset_mutation_lock",
    list(locked = builder_mutations_locked(build_flow(), protocol()))
  )
})

builder_build_confirmation_status <- function(state, plan) {
  if (!identical(state$stage, "build")) {
    return(list(ok = FALSE, reason = "stage_mismatch"))
  }
  snapshot <- state$review_snapshot
  if (!builder_review_snapshot_valid(snapshot)) {
    return(list(ok = FALSE, reason = "review_snapshot_unavailable"))
  }
  if (
    !inherits(plan, "builder_build_plan") ||
      !is.list(plan) ||
      !identical(plan$readiness, "ready") ||
      !is.null(plan$error)
  ) {
    return(list(ok = FALSE, reason = "candidate_plan_unavailable"))
  }
  if (!builder_workflow_confirmation_matches(state, snapshot)) {
    return(list(ok = FALSE, reason = "confirmation_mismatch"))
  }
  if (
    !identical(
      builder_review_snapshot_identity(snapshot),
      builder_review_plan_identity(plan)
    )
  ) {
    return(list(ok = FALSE, reason = "identity_mismatch"))
  }
  list(ok = TRUE, reason = NULL)
}

builder_build_recovery_needs_fresh_review <- function(message) {
  state <- isolate(workflow())
  if (state$stage %in% c("review", "build")) {
    workflow(builder_reduce_workflow(state, list(type = "invalidate")))
  }
  selected_output(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  result(NULL)
  session$sendCustomMessage(
    "builder_build_dialog",
    list(action = "close")
  )
  builder_notify("warning", message, target = "build")
  invisible(FALSE)
}

## Login passwords are never kept between builds. Check them before the
## folder picker or a retry starts, and ask for them in place: the plan does
## not depend on the passwords, so there is nothing to review again.
builder_build_accounts_missing <- function() {
  if (!isTRUE(isolate(build_mode())) || !isTRUE(isolate(auth_enabled()))) {
    return(FALSE)
  }
  accounts <- isolate(auth_accounts())
  parsed <- builder_auth_validate_payload(TRUE, accounts)
  !isTRUE(parsed$ok) ||
    !length(parsed$accounts) ||
    !identical(length(parsed$accounts), length(accounts))
}

builder_build_request_accounts <- function() {
  session$sendCustomMessage("builder_open_auth_dialog", list(open = TRUE))
  builder_notify(
    "info",
    "Enter the login accounts for this build, then build again.",
    target = list(selector = "#builder-auth-dialog"),
    duration = 6
  )
  invisible(FALSE)
}

builder_build_recovery_ready <- function() {
  state <- isolate(workflow())
  if (builder_build_accounts_missing()) {
    return(builder_build_request_accounts())
  }
  reviewed <- state$review_snapshot
  live <- isolate(current_review_snapshot())
  matches <- builder_review_snapshot_valid(reviewed) &&
    builder_review_snapshot_valid(live) &&
    builder_workflow_confirmation_matches(state, reviewed) &&
    identical(
      builder_review_snapshot_identity(reviewed),
      builder_review_snapshot_identity(live)
    )
  if (!isTRUE(matches)) {
    return(builder_build_recovery_needs_fresh_review(
      "Settings changed. Review the updated plan before retrying."
    ))
  }
  invisible(TRUE)
}

builder_require_confirmed_build_plan <- function(plan, output_path = NULL) {
  state <- isolate(workflow())
  status <- builder_build_confirmation_status(state, plan)
  if (
    isTRUE(status$ok) &&
      !is.null(output_path) &&
      (!builder_has_text(output_path) ||
        !identical(isolate(selected_output()), output_path))
  ) {
    status <- list(ok = FALSE, reason = "output_mismatch")
  }
  if (isTRUE(status$ok)) {
    return(invisible(TRUE))
  }
  builder_build_recovery_needs_fresh_review(
    "Settings changed. Review the updated plan before building."
  )
}

builder_build_attempt_failed <- function(message) {
  build_flow(list(stage = "idle", plan = NULL))
  build_operation_phase(NULL)
  result(builder_result_failure(message))
  session$sendCustomMessage(
    "builder_build_dialog",
    list(action = "close")
  )
  builder_notify("warning", message, target = "build")
  invisible(FALSE)
}

enqueue_build_plan <- function(
  plan,
  auth_accounts
) {
  if (!isTRUE(builder_require_confirmed_build_plan(plan, plan$out_dir))) {
    return(invisible(FALSE))
  }
  rs <- isolate(worker())
  if (is.null(rs)) {
    return(builder_build_attempt_failed(
      "The background worker is not ready. Try Build again in a moment."
    ))
  }
  current_protocol <- isolate(protocol())
  protocol_ready <- !is.null(current_protocol) &&
    isTRUE(tryCatch(
      builder_protocol_is_quiescent(current_protocol),
      error = function(error) FALSE
    ))
  if (!protocol_ready) {
    return(builder_build_attempt_failed(
      "The background worker is busy. Try Build again when it is ready."
    ))
  }
  parsed_auth <- builder_auth_validate_payload(
    isTRUE(plan$app_auth$enabled),
    auth_accounts
  )
  if (
    !isTRUE(parsed_auth$ok) ||
      length(parsed_auth$accounts) != plan$app_auth$account_count
  ) {
    return(invisible(FALSE))
  }
  progress_path <- NULL
  snapshot_root <- if (is.list(rs)) rs$snapshot_root %||% NULL else NULL
  if (builder_has_text(snapshot_root) && dir.exists(snapshot_root)) {
    candidate <- try(
      builder_import_progress_path(
        snapshot_root,
        paste0("build-progress-", request_sequence() + 1L),
        1L
      ),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      builder_build_progress_remove(candidate)
      progress_path <- candidate
    }
  }
  result(NULL)
  build_operation_phase("preflight")
  queued <- enqueue(list(
    kind = "build",
    plan = plan,
    auth_accounts = parsed_auth$accounts,
    progress_path = progress_path,
    note = builder_build_queue_note(plan)
  ))
  if (!isTRUE(queued)) {
    return(builder_build_attempt_failed(
      "The build could not be queued. Try Build again."
    ))
  }
  if (exists("builder_project_capture_build_plan", mode = "function")) {
    builder_project_capture_build_plan(plan)
  }
  builder_build_remember_plan(plan)
  build_flow(list(stage = "building", plan = NULL))
  auth_accounts_state(builder_auth_empty_accounts())
  auth_validation(list(
    ok = FALSE,
    error = "Set up login accounts again before the next build."
  ))
  session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
  invisible(TRUE)
}

prepare_selected_output <- function(path) {
  isolate({
    plan <- freeze_materialized_plan_for_output(
      path,
      output_options = current_build_options()
    )
    if (
      !inherits(plan, "builder_build_plan") ||
        !identical(plan$readiness, "ready")
    ) {
      return(builder_build_attempt_failed(
        plan$error %||% "The selected folder cannot be used."
      ))
    }
    target_exists <- tryCatch(
      .builder_release_exists(plan$out_dir) ||
        .builder_release_link(plan$out_dir),
      error = identity
    )
    if (inherits(target_exists, "condition")) {
      build_flow(list(stage = "idle", plan = NULL))
      builder_notify("error", conditionMessage(target_exists), target = "build")
      return(invisible(FALSE))
    }
    if (isTRUE(target_exists)) {
      build_flow(list(stage = "idle", plan = NULL))
      builder_notify(
        "warning",
        "The new release directory is no longer available. Choose the parent folder again.",
        target = "build"
      )
      return(invisible(FALSE))
    }
    enqueue_build_plan(
      plan,
      auth_accounts = isolate(auth_accounts())
    )
  })
}

builder_new_release_target <- function(
  parent,
  make_app,
  .random_bytes = .builderRandomBytes,
  .attempts = 128L
) {
  parent <- tryCatch(
    normalizePath(parent, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    !builder_has_text(parent %||% "") ||
      !dir.exists(parent) ||
      .builder_release_link(parent) ||
      !is.logical(make_app) ||
      length(make_app) != 1L ||
      is.na(make_app) ||
      !is.function(.random_bytes) ||
      !is.numeric(.attempts) ||
      length(.attempts) != 1L ||
      is.na(.attempts) ||
      !is.finite(.attempts) ||
      .attempts < 1L ||
      .attempts > .Machine$integer.max ||
      .attempts != floor(.attempts)
  ) {
    stop("The selected release parent is unsafe.", call. = FALSE)
  }
  prefix <- if (isTRUE(make_app)) {
    "CerebroNexus_viewer_"
  } else {
    "CerebroNexus_crb_"
  }
  for (attempt in seq_len(as.integer(.attempts))) {
    bytes <- tryCatch(.random_bytes(3L), error = function(error) NULL)
    if (!is.raw(bytes) || length(bytes) != 3L) {
      stop("A release identifier could not be generated.", call. = FALSE)
    }
    suffix <- paste(sprintf("%02x", as.integer(bytes)), collapse = "")
    candidate <- .builder_release_path(file.path(
      parent,
      paste0(prefix, suffix)
    ))
    if (
      !.builder_release_exists(candidate) &&
        !.builder_release_link(candidate)
    ) {
      return(candidate)
    }
  }
  stop("A unique release directory could not be allocated.", call. = FALSE)
}

builder_build_last_parent <- reactiveVal(NULL)

start_builder_new_release <- function(parent) {
  request <- builder_build_output_preflight(parent)
  if (!isTRUE(request$ok)) {
    stop(
      request$error %||% "The selected release parent cannot be used.",
      call. = FALSE
    )
  }
  builder_build_last_parent(request$path)
  target <- builder_new_release_target(
    request$path,
    make_app = isTRUE(request$make_app)
  )
  selected_output(target)
  schedule_selected_output_build(target)
}

builder_build_output_preflight <- function(path) {
  state <- isolate(workflow())
  snapshot <- state$review_snapshot
  if (!builder_workflow_confirmation_matches(state, snapshot)) {
    return(list(
      ok = FALSE,
      error = "Settings changed. Review the updated plan before building."
    ))
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  options <- current_build_options()
  list(
    ok = TRUE,
    error = NULL,
    path = path,
    make_app = isTRUE(options$make_app)
  )
}

show_builder_build_server_path <- function(detail = NULL) {
  shiny::showModal(builder_server_path_dialog(
    title = "Choose output folder",
    input_id = "builder_build_server_folder",
    action_id = "use_builder_build_server_folder",
    label = "Parent folder",
    action_label = "Build here",
    detail = detail,
    kicker = "Build output"
  ))
  invisible(TRUE)
}

choose_build_folder <- function() {
  build_flow(list(stage = "choosing_folder", plan = NULL))
  builder_schedule_native_picker(
    "output_directory",
    key = "build-output",
    on_result = function(choice) {
      on.exit(
        session$sendCustomMessage(
          "builder_build_dialog",
          list(action = "close")
        ),
        add = TRUE
      )
      tryCatch(
        {
          if (identical(choice$status, "cancelled")) {
            build_flow(list(stage = "idle", plan = NULL))
            return()
          }
          if (!identical(choice$status, "selected")) {
            build_flow(list(stage = "idle", plan = NULL))
            show_builder_build_server_path(choice$error)
            return()
          }
          start_builder_new_release(choice$path)
        },
        error = function(error) {
          build_flow(list(stage = "idle", plan = NULL))
          builder_notify(
            "error",
            paste0(
              "The selected folder could not be checked: ",
              conditionMessage(error)
            ),
            target = "build"
          )
        }
      )
    },
    on_error = function(error) {
      session$sendCustomMessage("builder_build_dialog", list(action = "close"))
      build_flow(list(stage = "idle", plan = NULL))
      show_builder_build_server_path(conditionMessage(error))
    }
  )
}

observeEvent(input$use_builder_build_server_folder, {
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return()
  }
  path <- tryCatch(
    builder_server_path_resolve(
      input$builder_build_server_folder %||% "",
      "directory"
    ),
    error = identity
  )
  if (inherits(path, "condition")) {
    builder_notify(
      "error",
      conditionMessage(path),
      target = list(field = "builder_build_server_folder")
    )
    return()
  }
  accepted <- tryCatch(
    start_builder_new_release(path),
    error = function(error) {
      build_flow(list(stage = "idle", plan = NULL))
      builder_notify(
        "error",
        paste0(
          "The selected folder could not be checked: ",
          conditionMessage(error)
        ),
        target = list(field = "builder_build_server_folder")
      )
      FALSE
    }
  )
  if (isTRUE(accepted)) {
    shiny::removeModal()
  }
})

schedule_selected_output_build <- function(path) {
  builder_build_stop_requested(FALSE)
  result(NULL)
  build_flow(list(stage = "preparing", plan = NULL))
  session$sendCustomMessage("builder_focus_build_status", list())
  later::later(
    function() {
      shiny::withReactiveDomain(builder_lifecycle_session, {
        prepare_selected_output(path)
      })
    },
    delay = 0
  )
  invisible(TRUE)
}

start_confirmed_build <- function(select_output = FALSE) {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("build"))
  ) {
    return(invisible(FALSE))
  }
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return(invisible(FALSE))
  }
  if (builder_build_accounts_missing()) {
    return(builder_build_request_accounts())
  }
  # The release coordinator loads its runtime while the user picks a folder.
  try(builder_prewarm_release_processes("preparation"), silent = TRUE)
  if (isTRUE(select_output)) {
    choose_build_folder()
    return(invisible(TRUE))
  }
  output_path <- isolate(selected_output())
  if (!builder_has_text(output_path)) {
    choose_build_folder()
    return(invisible(TRUE))
  }
  schedule_selected_output_build(output_path)
}

retry_confirmed_build <- function() {
  if (!isTRUE(builder_build_recovery_ready())) {
    return(invisible(FALSE))
  }
  start_confirmed_build()
}

## A new release directory in the parent folder chosen last time, without
## the folder picker.
observeEvent(input$build_same_folder, {
  parent <- isolate(builder_build_last_parent())
  if (
    !builder_has_text(parent %||% "") ||
      builder_build_controls_locked(isolate(build_flow())) ||
      (exists(
        "builder_operation_allowed",
        mode = "function",
        inherits = TRUE
      ) &&
        !isTRUE(builder_operation_allowed("build")))
  ) {
    return()
  }
  if (builder_build_accounts_missing()) {
    builder_build_request_accounts()
    return()
  }
  try(builder_prewarm_release_processes("preparation"), silent = TRUE)
  tryCatch(
    start_builder_new_release(parent),
    error = function(error) {
      build_flow(list(stage = "idle", plan = NULL))
      builder_notify(
        "error",
        paste0(
          "The previous output folder could not be used: ",
          conditionMessage(error),
          " Choose a folder instead."
        ),
        target = "build"
      )
    }
  )
})

observeEvent(input$build, {
  current <- isolate(result())
  if (is.list(current) && identical(current$state, "failure")) {
    retry_confirmed_build()
  } else {
    start_confirmed_build(select_output = TRUE)
  }
})

observeEvent(input$recover_builder_release, {
  current <- isolate(result())
  if (!is.list(current)) {
    return()
  }
  recovery <- current$recovery %||% NULL
  if (
    !identical(current$state, "recovery_required") ||
      !is.list(recovery) ||
      !builder_has_text(recovery$target %||% "")
  ) {
    return()
  }
  target <- recovery$target
  action <- builder_release_recovery_action(recovery)
  if (is.null(action)) {
    message <- paste(
      "The interrupted release cannot be recovered automatically;",
      "manual inspection is required."
    )
    result(builder_result_recovery_followup(current, message, recovery))
    builder_notify("error", message, target = "build")
    return()
  }
  current_worker <- isolate(worker())
  current_protocol <- isolate(protocol())
  if (!is.null(current_worker) && !is.null(current_protocol)) {
    restarted <- try(
      restart_worker_protocol(
        current_worker,
        current_protocol,
        "The background worker was restarted to release build files."
      ),
      silent = TRUE
    )
    if (inherits(restarted, "try-error") || !isTRUE(restarted)) {
      message <- paste(
        "The background worker could not release the interrupted build files.",
        "Restart this Builder session and try recovery again."
      )
      result(builder_result_recovery_followup(current, message, recovery))
      builder_notify("error", message, target = "build")
      return()
    }
  }
  refreshed <- tryCatch(
    builder_coordinator_recovery(target),
    error = function(error) error
  )
  if (inherits(refreshed, "condition")) {
    recovered <- refreshed
  } else if (identical(refreshed$state, "ready")) {
    recovered <- list(recovered = TRUE, action = action, target = target)
  } else {
    recovery <- refreshed
    action <- builder_release_recovery_action(recovery)
    recovered <- if (is.null(action)) {
      simpleError(
        "The interrupted release is no longer safe to recover automatically."
      )
    } else {
      tryCatch(
        builder_coordinator_recover(target, action = action),
        error = function(error) error
      )
    }
  }
  if (inherits(recovered, "condition") || !isTRUE(recovered$recovered)) {
    refreshed <- tryCatch(
      builder_coordinator_recovery(target),
      error = function(error) recovery
    )
    message <- if (inherits(recovered, "condition")) {
      conditionMessage(recovered)
    } else {
      "The interrupted release could not be recovered."
    }
    result(builder_result_recovery_followup(current, message, refreshed))
    builder_notify("error", message, target = "build")
    return()
  }
  result(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  busy_note(NULL)
  builder_notify(
    "success",
    if (identical(action, "restore")) {
      "The previous release was restored. You can build again."
    } else {
      "The interrupted release was cleared. You can build again."
    },
    target = "build",
    duration = 6
  )
})

validate_rail_removal <- function(next_state, id) {
  .builder_rail_validation(
    TRUE,
    code = "dataset_removed",
    dataset_ids = vapply(
      next_state$datasets,
      `[[`,
      character(1),
      "id"
    )
  )
}

remove_dataset <- function(
  previous_state,
  updated,
  id,
  validation
) {
  ids <- vapply(previous_state$datasets, `[[`, character(1), "id")
  entry <- previous_state$datasets[[match(id, ids)]]
  if (!is.null(isolate(import_of(id)))) {
    remove_pending_import(id)
  }
  previous_removed <- previous_state$last_removed
  if (is.list(previous_removed)) {
    identity <- .builder_worker_identity(previous_removed$entry$snapshot)
    pending_drops <- isolate(pending_snapshot_drops())
    pending_drops[[previous_removed$id]] <- identity
    pending_snapshot_drops(pending_drops)
    queued <- enqueue(list(
      kind = "drop",
      id = previous_removed$id,
      dataset_revision = previous_removed$entry$revision %||% 0L,
      snapshot_identity = identity,
      note = "Releasing memory…"
    ))
    if (!isTRUE(queued)) {
      pending_drops[[previous_removed$id]] <- NULL
      pending_snapshot_drops(pending_drops)
    }
  }
  result(NULL)
  builder_notify(
    "info",
    tagList(
      paste0("Removed ", entry$settings$name, ". "),
      actionLink("undo_remove_notice", "Undo")
    ),
    duration = 10
  )
}

# observeEvent(input$drop_ds, ...) is owned by builder_dataset_rail_server().
builder_dataset_rail_server(
  input = input,
  session = session,
  store = store,
  validate_remove = validate_rail_removal,
  select_dataset = function(id, commit, switch_token = NULL) {
    if (
      exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
        !isTRUE(builder_operation_allowed("select_dataset"))
    ) {
      if (!is.null(switch_token)) {
        session$sendCustomMessage(
          "builder_dataset_switch_state",
          list(dataset = id, state = "error", switch_token = switch_token)
        )
      }
      return(invisible(FALSE))
    }
    alignment_server$request_dataset_switch(id, commit, switch_token)
  },
  on_select = function(id) {
    active_import_id(NULL)
    result(NULL)
  },
  on_reorder = function(previous, updated) {
    if (identical(previous$datasets, updated$datasets)) {
      return(invisible(FALSE))
    }
    state <- isolate(workflow())
    if (!is.null(state$review_snapshot) || !is.null(state$confirmation)) {
      workflow(builder_reduce_workflow(state, list(type = "invalidate")))
      build_flow(list(stage = "idle", plan = NULL))
      result(NULL)
      session$sendCustomMessage(
        "builder_build_dialog",
        list(action = "close")
      )
    }
    invisible(TRUE)
  },
  on_remove = remove_dataset,
  on_undo = function() result(NULL),
  on_validation = function(validation) {
    if (isTRUE(validation$ok)) {
      add_error(NULL)
    } else if (!identical(validation$code, "confirmation_required")) {
      add_error(validation$message)
    }
  },
  mutations_locked = dataset_mutations_locked,
  on_locked = function() dataset_mutations_locked(notify = TRUE)
)

output$busy <- renderUI({
  note <- busy_note()
  if (is.null(note) || identical(workflow()$stage, "build")) {
    return(NULL)
  }
  div(
    class = "busy",
    span(class = "spinner"),
    span(note)
  )
})
