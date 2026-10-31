builder_stage_contract_source_runtime(environment())
builder_profile_source_runtime(environment())
builder_plan_contract_source_runtime(environment())

test_that("build progress distinguishes reused and rebuilt datasets", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  plan <- list(
    make_app = TRUE,
    items = list(
      list(reused_artifact = list(path = "a.crb")),
      list(reused_artifact = list(path = "b.crb")),
      list(reused_artifact = NULL)
    )
  )

  expect_identical(
    app_env$builder_build_queue_note(plan),
    "Reusing 2 CRBs · Building 1 dataset…"
  )
  plan$items[[3L]]$reused_artifact <- list(path = "c.crb")
  expect_identical(
    app_env$builder_build_queue_note(plan),
    "Reusing 3 CRBs…"
  )
  expect_identical(
    app_env$builder_build_progress_note(plan, "viewer"),
    "Packaging Viewer…"
  )
})

test_that("new gallery imports apply the complete example preset", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  record <- app_env$builder_example_catalog()$complete_viewer_data
  object <- record$make()$object
  entry <- list(
    id = "example-import",
    levels = list(cell_type = levels(object$cell_type)),
    settings = list()
  )

  configured <- app_env$builder_apply_example_import_preset(
    entry,
    "complete_viewer_data",
    catalog = list(complete_viewer_data = record)
  )

  expect_named(configured$settings$marker_imports, "fixture_sidecars")
  expect_length(configured$settings$tables, 3L)
  expect_identical(
    sum(lengths(lapply(configured$settings$images, names))),
    0L
  )
  expect_identical(
    app_env$builder_apply_example_import_preset(entry, NULL),
    entry
  )
})

test_that("the project header yields to a compact sticky workflow", {
  project_ui <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  builder_js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("Untitled Builder project", project_ui, fixed = TRUE))
  expect_match(builder_js, "is-workflow-compact", fixed = TRUE)
  expect_match(
    builder_js,
    'workflow.classList.contains("is-workflow-compact")',
    fixed = TRUE
  )
  expect_match(
    builder_js,
    "compact ? threshold - 48 : threshold",
    fixed = TRUE
  )
})

test_that("clickable workflow steps keep the same horizontal layout", {
  components <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    components,
    ".builder-workflow-progress .builder-workflow-stage-link > .action-label",
    fixed = TRUE
  )
})

test_that("the Configure footer sticks to the bottom of the viewport", {
  components <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "
"
  )
  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "
"
  )

  expect_match(
    components,
    paste0(
      ".builder-stage-configure > #configure_actions {
",
      "  position: sticky;
",
      "  bottom: 0;"
    ),
    fixed = TRUE
  )
  ## The sticky wrapper must be a direct child of the Configure stage, after
  ## the form, or it has no room to stick in.
  expect_match(
    review_server,
    paste0(
      "builder_spatial_stage_ui(\"enhance\", enhance_model)
",
      "    ),
",
      "    uiOutput(\"configure_actions\")
",
      "  )"
    ),
    fixed = TRUE
  )
})

test_that("Build footer actions have matching desktop widths", {
  components <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    components,
    paste0(
      ".builder-stage-build .builder-stage-footer-actions .btn {\n",
      "  min-inline-size: 8.75rem;\n",
      "}"
    ),
    fixed = TRUE
  )
  expect_match(
    components,
    ".builder-stage-footer-actions .btn { width: 100%; }",
    fixed = TRUE
  )
})

builder_stage_finished_output_preflight <- function() {
  list(
    is_alive = function() FALSE,
    get_result = function() {
      list(
        ok = TRUE,
        error = NULL,
        foreign = character(),
        prior_state = list(
          schema_version = 1L,
          identity = list(schema_version = 1L, exists = FALSE, entries = list())
        )
      )
    },
    kill = function() invisible(TRUE)
  )
}

builder_stage_checked_marks <- function(entries, identity_fun, identity_cache) {
  ids <- vapply(entries, `[[`, character(1), "id")
  marks <- vapply(
    entries,
    identity_fun,
    character(1),
    identity_cache = identity_cache
  )
  stats::setNames(marks, ids)
}

test_that("project picker permissions update immediately after selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this picker state test.")
  }

  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(list(
      id = "dataset-a",
      settings = list(name = "Dataset A")
    )))
    session$flushReact()

    expect_true(isolate(builder_operation_allowed(
      "create_project",
      notify = FALSE
    )))
    builder_project_operation_phase("choosing")
    session$flushReact()
    expect_false(isolate(builder_operation_allowed(
      "create_project",
      notify = FALSE
    )))

    builder_project_operation_phase("idle")
    expect_true(isolate(builder_operation_allowed(
      "create_project",
      notify = FALSE
    )))
  })
})

test_that("Build reuses current project CRBs after preserving review identity", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this reuse test.")
  }
  captured <- NULL
  app_env$builder_freeze_plan <- function(entries, review_identity, ...) {
    captured <<- list(entries = entries, review_identity = review_identity)
    captured
  }

  shiny::testServer(app_env$server, {
    root <- withr::local_tempdir()
    artifact_path <- file.path(root, "dataset-a.crb")
    writeLines("ready", artifact_path)
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      load_state = "loaded",
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L),
        source_fingerprint = "source-a"
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(name = "Dataset A"),
      acknowledgements = character(),
      spatial_drafts = list()
    )
    use_state_only_fixture(list(entry))
    original_identity <- app_env$builder_review_configuration_identity(
      list(entry),
      builder_configuration_identity_cache
    )
    artifact <- list(
      status = "ready",
      reusable = TRUE,
      path = basename(artifact_path),
      fingerprint = app_env$builder_project_file_fingerprint(
        artifact_path,
        content = TRUE
      ),
      members = list(),
      built_from_revision = 0L,
      built_from_source_fingerprint = "source-a",
      built_from_configuration = app_env$builder_project_configuration_digest(
        entry
      ),
      plan_item = list(
        artifact_identity = list(spatial_sections = character())
      ),
      bundle_contract_version = 1L
    )
    builder_project(list(root = root, manifest = list(datasets = list())))
    builder_project_artifacts(list(`dataset-a` = artifact))

    freeze_plan_for_output(tempfile("reuse-project-crb-"))

    expect_identical(captured$review_identity, original_identity)
    expect_identical(captured$entries[[1L]]$load_state, "artifact_ready")
    expect_identical(isolate(sets())[[1L]]$load_state, "loaded")
  })
})

test_that("Projection cards distinguish inclusion from initial display", {
  ui <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "core_stage", "projections.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  js <- paste(
    readLines(builder_profile_inst_path("builder", "www", "builder.js")),
    collapse = "\n"
  )

  expect_match(ui, 'span("Show initially")', fixed = TRUE)
  expect_match(ui, 'class = "viewer-projection-initial"', fixed = TRUE)
  expect_match(js, "initial: initialProjections", fixed = TRUE)
})

test_that("automatic dataset review advance requests top-of-workbench focus", {
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(review, '"builder_focus_dataset_start"', fixed = TRUE)
  expect_match(review, "dataset = target", fixed = TRUE)
  expect_match(
    review,
    "generation = as.integer(isolate(entry_of(target))$revision",
    fixed = TRUE
  )
})

test_that("dataset-scoped browser actions retain their current entry owner", {
  foundation <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  enhancements <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "enhancements.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  project <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(foundation, "entry_owner = entry_owner", fixed = TRUE)
  expect_match(foundation, '"builder_dataset_action_owner"', fixed = TRUE)
  expect_match(
    enhancements,
    "builder_content_action_owner_is_current(",
    fixed = TRUE
  )
  expect_match(
    enhancements,
    "marker_dialog_owner <- reactiveVal(NULL)",
    fixed = TRUE
  )
  marker_guards <- gregexpr(
    "if (!marker_dialog_is_current())",
    enhancements,
    fixed = TRUE
  )[[1L]]
  expect_identical(sum(marker_guards > 0L), 6L)
  expect_match(
    foundation,
    "refresh_marker_dialog_owner(owner$project_epoch)",
    fixed = TRUE
  )
  expect_match(
    review,
    "`data-generation` = table_action_owner$generation",
    fixed = TRUE
  )
  expect_match(
    review,
    "`data-project-epoch` = table_action_owner$project_epoch",
    fixed = TRUE
  )
  expect_match(
    review,
    "`data-owner-token` = table_action_owner$owner_token",
    fixed = TRUE
  )
  epoch_updates <- gregexpr(
    "send_dataset_action_owner(project_epoch = generation)",
    project,
    fixed = TRUE
  )[[1L]]
  expect_identical(sum(epoch_updates > 0L), 2L)
})

test_that("Configure renders Extras before the final Views content", {
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  extras_stage <- regexpr('builder_enhance_stage_ui(', review, fixed = TRUE)
  views_stage <- regexpr('builder_views_stage_ui(', review, fixed = TRUE)

  expect_true(extras_stage[[1L]] > 0L)
  expect_true(views_stage[[1L]] > extras_stage[[1L]])
  expect_false(grepl("builder-configure-tabs", review, fixed = TRUE))
})

test_that("checked datasets react to every pending spatial draft", {
  foundation <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    foundation,
    "alignment_server$pending_drafts()",
    fixed = TRUE
  )
  expect_match(
    foundation,
    "builder_project_checked_ids",
    fixed = TRUE
  )
  expect_match(
    foundation,
    "builder_project_pending_draft_ids(pending_drafts)",
    fixed = TRUE
  )
})

test_that("an accepted Build requests absolute page-top scrolling", {
  build <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  build_lines <- readLines(
    builder_profile_inst_path("builder", "server", "build.R"),
    warn = FALSE
  )
  start_line <- grep(
    "schedule_selected_output_build <- function(path) {",
    build_lines,
    fixed = TRUE
  )
  end_line <- grep(
    "start_confirmed_build <- function(select_output = FALSE) {",
    build_lines,
    fixed = TRUE
  )
  start <- paste(build_lines[start_line:(end_line - 1L)], collapse = "\n")

  expect_match(start, 'build_flow(list(stage = "preparing"', fixed = TRUE)
  expect_match(start, '"builder_focus_build_status"', fixed = TRUE)
  expect_lt(
    regexpr('build_flow(list(stage = "preparing"', start, fixed = TRUE)[1L],
    regexpr('"builder_focus_build_status"', start, fixed = TRUE)[1L]
  )
})

test_that("Builder shell and workflow UI expose detailed Build stages", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  shell <- builder_app_source_text()
  expect_false(grepl('uiOutput("actionbar")', shell, fixed = TRUE))
  expect_false(grepl('uiOutput("result_card")', shell, fixed = TRUE))
  expect_identical(
    lengths(regmatches(
      shell,
      gregexpr('uiOutput("build_stage_status")', shell, fixed = TRUE)
    )),
    0L
  )
  expect_match(shell, 'class = "topbar builder-project-header"', fixed = TRUE)
  expect_match(shell, 'class = "builder-skip-link"', fixed = TRUE)
  expect_match(shell, 'tags$header(', fixed = TRUE)
  expect_match(shell, 'tags$main(', fixed = TRUE)
  expect_match(shell, 'tags$h1(', fixed = TRUE)
  expect_match(shell, 'class = "builder-project-brand"', fixed = TRUE)
  toolbar <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(toolbar, 'uiOutput("project_status")', fixed = TRUE)
  expect_match(toolbar, 'span("Save")', fixed = TRUE)
  expect_match(toolbar, 'shiny::icon("floppy-disk")', fixed = TRUE)
  expect_match(shell, 'class = "shell builder-shell"', fixed = TRUE)
  expect_match(shell, 'id = "builder-workspace"', fixed = TRUE)
  expect_match(shell, 'id = "builder-build-progress"', fixed = TRUE)
  expect_match(shell, 'id = "builder-build-progress-summary"', fixed = TRUE)
  expect_match(shell, '`aria-modal` = "true"', fixed = TRUE)
  expect_match(shell, '`aria-live` = "polite"', fixed = TRUE)
  expect_false(grepl("text-overflow:ellipsis", shell, fixed = TRUE))
  expect_match(shell, '"Verify output"', fixed = TRUE)
  expect_match(shell, '"Publish release"', fixed = TRUE)
  expect_false(grepl("builder-build-progress-activity", shell, fixed = TRUE))
  expect_false(grepl('tags$progress(', shell, fixed = TRUE))
  expect_false(grepl(
    "Build your first Viewer in four steps",
    shell,
    fixed = TRUE
  ))

  progress <- app_env$builder_workflow_progress_ui(
    "configure",
    available = c(
      upload = TRUE,
      configure = TRUE,
      review = FALSE,
      build = FALSE
    ),
    confirmed = FALSE,
    locked = FALSE
  )
  progress_html <- htmltools::renderTags(progress)$html
  expect_match(progress_html, "builder-workflow-progress", fixed = TRUE)
  expect_match(progress_html, 'aria-label="Builder progress"', fixed = TRUE)
  expect_match(progress_html, 'aria-current="step"', fixed = TRUE)
  expect_match(progress_html, 'data-workflow-confirmed="false"', fixed = TRUE)
  expect_match(progress_html, "Configure", fixed = TRUE)
  expect_match(progress_html, 'id="workflow_stage_upload"', fixed = TRUE)
  expect_match(progress_html, '<button', fixed = TRUE)
  expect_false(grepl('<a ', progress_html, fixed = TRUE))
  expect_false(grepl('href="#"', progress_html, fixed = TRUE))
  expect_match(progress_html, 'aria-disabled="true"', fixed = TRUE)
  expect_match(progress_html, "is-unavailable", fixed = TRUE)
  expect_match(progress_html, "builder-workflow-step-number", fixed = TRUE)
  expect_match(progress_html, "Set each dataset", fixed = TRUE)
  expect_match(progress_html, 'aria-label="Configure"', fixed = TRUE)
  confirmed_progress <- htmltools::renderTags(
    app_env$builder_workflow_progress_ui(
      "build",
      available = stats::setNames(
        rep(TRUE, 4L),
        c(
          "upload",
          "configure",
          "review",
          "build"
        )
      ),
      confirmed = TRUE,
      locked = FALSE
    )
  )$html
  expect_match(
    confirmed_progress,
    'data-workflow-confirmed="true"',
    fixed = TRUE
  )
  expect_identical(
    lengths(regmatches(
      progress_html,
      gregexpr("<li", progress_html, fixed = TRUE)
    )),
    4L
  )
  expect_error(
    app_env$builder_workflow_progress_ui(
      "future",
      available = stats::setNames(
        rep(FALSE, 4L),
        c(
          "upload",
          "configure",
          "review",
          "build"
        )
      ),
      confirmed = FALSE,
      locked = FALSE
    ),
    "valid Builder workflow stage"
  )

  actions <- app_env$builder_configure_actions_ui(
    "Wait for all datasets to finish loading.",
    can_continue = FALSE
  )
  actions_html <- htmltools::renderTags(actions)$html
  expect_match(actions_html, "builder-stage-footer", fixed = TRUE)
  expect_match(actions_html, "builder-stage-footer-status", fixed = TRUE)
  expect_match(actions_html, "builder-stage-footer-actions", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      actions_html,
      gregexpr('id="continue_to_review"', actions_html, fixed = TRUE)
    )),
    1L
  )
  expect_match(actions_html, "Continue to Review", fixed = TRUE)
  expect_false(grepl("✓|→", actions_html))
  expect_match(actions_html, " disabled", fixed = TRUE)
  expect_false(grepl("make_app", actions_html, fixed = TRUE))
  expect_false(grepl("Create a Viewer app", actions_html, fixed = TRUE))

  ready_html <- htmltools::renderTags(app_env$builder_configure_actions_ui(
    "1 dataset ready",
    can_continue = TRUE
  ))$html
  expect_match(ready_html, "All datasets checked", fixed = TRUE)
  expect_match(ready_html, "1 dataset ready", fixed = TRUE)
  expect_match(
    ready_html,
    "builder-stage-footer-status is-review-ready",
    fixed = TRUE
  )
  expect_match(ready_html, "builder-stage-footer-status-icon", fixed = TRUE)
  expect_match(ready_html, 'aria-hidden="true"', fixed = TRUE)
  expect_false(grepl(" disabled", ready_html, fixed = TRUE))

  unchecked_html <- htmltools::renderTags(app_env$builder_configure_actions_ui(
    "Select an organism before marking this dataset checked.",
    can_continue = FALSE,
    remaining = 1L
  ))$html
  expect_match(unchecked_html, 'id="complete_dataset_check"', fixed = TRUE)
  expect_match(
    unchecked_html,
    'data-builder-configure-action="complete_dataset_check"',
    fixed = TRUE
  )
  expect_match(unchecked_html, "Finish Checking", fixed = TRUE)
  expect_false(grepl("action-button", unchecked_html, fixed = TRUE))
  expect_false(grepl(" disabled", unchecked_html, fixed = TRUE))
  expect_false(grepl("is-review-ready", unchecked_html, fixed = TRUE))
  checked_html <- htmltools::renderTags(app_env$builder_configure_actions_ui(
    "1 of 2 datasets checked · 1 remaining",
    can_continue = FALSE,
    dataset_checked = TRUE,
    remaining = 1L
  ))$html
  expect_match(checked_html, 'id="complete_dataset_check"', fixed = TRUE)
  expect_match(checked_html, ">Next</span>", fixed = TRUE)
  expect_false(grepl("Finish Checking", checked_html, fixed = TRUE))
  settling_html <- htmltools::renderTags(app_env$builder_configure_actions_ui(
    "Waiting for the spatial preview to finish.",
    can_continue = FALSE,
    remaining = 1L,
    check_ready = FALSE
  ))$html
  expect_match(settling_html, 'id="complete_dataset_check"', fixed = TRUE)
  expect_match(settling_html, " disabled", fixed = TRUE)

  primary_states <- list(
    unchecked = list(
      FALSE,
      FALSE,
      2L,
      TRUE,
      "complete_dataset_check",
      "Finish Checking",
      FALSE
    ),
    checked = list(
      FALSE,
      TRUE,
      1L,
      TRUE,
      "complete_dataset_check",
      "Next",
      FALSE
    ),
    unchecked_saving = list(
      FALSE,
      FALSE,
      1L,
      FALSE,
      "complete_dataset_check",
      "Finish Checking",
      TRUE
    ),
    checked_saving = list(
      FALSE,
      TRUE,
      1L,
      FALSE,
      "complete_dataset_check",
      "Next",
      TRUE
    ),
    review_ready = list(
      TRUE,
      TRUE,
      0L,
      TRUE,
      "continue_to_review",
      "Continue to Review",
      FALSE
    ),
    review_blocked = list(
      FALSE,
      TRUE,
      0L,
      TRUE,
      "continue_to_review",
      "Continue to Review",
      TRUE
    )
  )
  for (name in names(primary_states)) {
    state <- primary_states[[name]]
    html <- htmltools::renderTags(app_env$builder_configure_primary_action_ui(
      can_continue = state[[1L]],
      dataset_checked = state[[2L]],
      remaining = state[[3L]],
      check_ready = state[[4L]]
    ))$html
    expect_match(
      html,
      paste0('id="', state[[5L]], '"'),
      fixed = TRUE,
      info = name
    )
    expect_match(html, state[[6L]], fixed = TRUE, info = name)
    expect_identical(
      grepl(" disabled", html, fixed = TRUE),
      state[[7L]],
      info = name
    )
    expect_match(
      html,
      paste0('data-builder-configure-action="', state[[5L]], '"'),
      fixed = TRUE,
      info = name
    )
    expect_false(grepl("action-button", html, fixed = TRUE), info = name)
  }
  expect_true(
    "check_ready" %in% names(formals(app_env$builder_configure_actions_ui))
  )
  expect_false(
    "can_check" %in% names(formals(app_env$builder_configure_actions_ui))
  )

  components <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    components,
    ".builder-stage-footer-status.is-review-ready",
    fixed = TRUE
  )
  expect_match(
    components,
    "background: var(--c-success-50)",
    fixed = TRUE
  )

  confirmation_html <- htmltools::renderTags(
    app_env$builder_review_confirmation_ui()
  )$html
  expect_identical(
    lengths(regmatches(
      confirmation_html,
      gregexpr('id="confirm_review"', confirmation_html, fixed = TRUE)
    )),
    1L
  )
  expect_identical(
    lengths(regmatches(
      confirmation_html,
      gregexpr('id="back_to_settings"', confirmation_html, fixed = TRUE)
    )),
    1L
  )
  expect_match(confirmation_html, "builder-stage-footer", fixed = TRUE)
  expect_match(confirmation_html, "Configuration checked", fixed = TRUE)
  expect_match(
    confirmation_html,
    "Continue to Build",
    fixed = TRUE
  )
  expect_match(confirmation_html, "Back to Configure", fixed = TRUE)
  expect_false(grepl("Ready to continue?", confirmation_html, fixed = TRUE))
  expect_false(grepl("<input|<select|<textarea", confirmation_html))

  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  model <- app_env$builder_review_model(
    builder_stage_review_snapshot(builder_stage_frozen_plan(FALSE))
  )
  review_html <- htmltools::renderTags(
    app_env$builder_review_stage_ui("review", model)
  )$html
  build_html <- htmltools::renderTags(
    app_env$builder_build_workbench_ui(model)
  )$html
  expect_false(grepl("Checked configuration", review_html, fixed = TRUE))
  expect_false(grepl("Confirmed plan revision", build_html, fixed = TRUE))
})

test_that("Build output UI locks CRB-only when external images require an App", {
  skip_if_not_installed("shiny")
  options <- builder_build_options(make_app = TRUE)
  html <- htmltools::renderTags(builder_build_options_ui(
    options,
    app_required = TRUE
  ))$html

  expect_match(
    html,
    "Spatial images or ROI settings require CRB files + Viewer App output.",
    fixed = TRUE
  )
  expect_match(
    html,
    '<input type="radio" name="build_output_mode" value="crb" disabled="disabled" aria-disabled="true"/>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<input type="radio" name="build_output_mode" value="app" checked="checked"/>',
    fixed = TRUE
  )
  expect_error(
    builder_build_options_ui(options, app_required = c(TRUE, FALSE))
  )
  expect_error(builder_build_options_ui(options, app_required = NA))
})

test_that("shared stage layout primitives expose one visual grammar", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  header <- app_env$builder_stage_header_ui(
    "Configure",
    "Choose data to include",
    "Define the content saved to each CRB file."
  )
  section <- app_env$builder_stage_section_ui(
    "Core content",
    htmltools::tags$p("Required content")
  )
  footer <- app_env$builder_stage_footer_ui(
    "1 dataset ready",
    shiny::actionButton("continue_to_review", "Continue")
  )
  html <- htmltools::renderTags(htmltools::tagList(
    header,
    section,
    footer
  ))$html

  expect_match(html, "builder-stage-header", fixed = TRUE)
  expect_false(grepl("builder-stage-eyebrow", html, fixed = TRUE))
  expect_false(grepl("Step 2 of 4", html, fixed = TRUE))
  expect_match(html, "builder-stage-section", fixed = TRUE)
  expect_match(html, "builder-stage-footer", fixed = TRUE)
  expect_match(html, "builder-stage-footer-status", fixed = TRUE)
  expect_match(html, "builder-stage-footer-actions", fixed = TRUE)
  expect_match(html, 'id="continue_to_review"', fixed = TRUE)
})

test_that("Builder content does not use an invalid aria-current token", {
  javascript <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl(
    'setAttribute("aria-current", "stage")',
    javascript,
    fixed = TRUE
  ))
  expect_match(javascript, 'dataset.currentStage = "true"', fixed = TRUE)
  expect_false(grepl(
    'output.setAttribute("role", "status")',
    javascript,
    fixed = TRUE
  ))
  expect_match(
    javascript,
    'status.setAttribute("aria-hidden", "true")',
    fixed = TRUE
  )
})

test_that("Project toolbar icons do not pollute button names", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  html <- htmltools::renderTags(app_env$builder_project_toolbar_ui())$html

  expect_identical(
    lengths(regmatches(
      html,
      gregexpr('aria-hidden="true"', html, fixed = TRUE)
    )),
    2L
  )
})

test_that("Inspect guidance reacts when an organism is selected", {
  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(review_server, 'input[["core-organism"]]', fixed = TRUE)
  expect_match(
    review_server,
    'setdiff(attention, "settings_organism")',
    fixed = TRUE
  )
  expect_match(review_server, "state$attention_ids <- attention", fixed = TRUE)
})

test_that("an assay without a full-cell layer gives a recovery path", {
  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    review_server,
    "has no expression layer covering every cell",
    fixed = TRUE
  )
  expect_match(
    review_server,
    "Reopen this dataset with its saved CRB",
    fixed = TRUE
  )
})

test_that("Build stage exclusively owns its live status projection", {
  app <- builder_app_source_text()
  workflow_ui <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  build_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  enhancements <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "enhancements.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  status_renderer <- substr(
    build_server,
    regexpr(
      "build_stage_status_projection <- reactive({",
      build_server,
      fixed = TRUE
    ),
    regexpr(
      "builder_build_confirmation_status <- function",
      build_server,
      fixed = TRUE
    ) -
      1L
  )

  expect_identical(
    lengths(regmatches(
      workflow_ui,
      gregexpr(
        'uiOutput("build_stage_status_content")',
        workflow_ui,
        fixed = TRUE
      )
    )),
    1L
  )
  expect_identical(
    lengths(regmatches(
      workflow_ui,
      gregexpr('uiOutput("build_stage_footer")', workflow_ui, fixed = TRUE)
    )),
    1L
  )
  expect_identical(
    lengths(regmatches(
      build_server,
      gregexpr(
        "output$build_stage_status_content <- renderUI({",
        build_server,
        fixed = TRUE
      )
    )),
    1L
  )
  expect_match(status_renderer, "flow <- build_flow()", fixed = TRUE)
  expect_match(status_renderer, "flow = flow", fixed = TRUE)
  ## A plain Save also registers dataset caches; only a build this session
  ## started may show "Saving reusable CRBs", and the phase is reactive so
  ## the card leaves "Build in progress" when registration ends.
  expect_match(
    status_renderer,
    'finalizing <- identical(flow$stage %||% NULL, "building") &&',
    fixed = TRUE
  )
  expect_false(grepl(
    "isolate(builder_project_operation_phase())",
    status_renderer,
    fixed = TRUE
  ))
  expect_match(status_renderer, "protocol = protocol()", fixed = TRUE)
  expect_match(status_renderer, "busy_note()", fixed = TRUE)
  expect_match(
    status_renderer,
    "Export complete. Saving reusable CRBs to the Builder Project…",
    fixed = TRUE
  )
  expect_match(status_renderer, "finalizing = finalizing", fixed = TRUE)
  expect_match(status_renderer, "result = result()", fixed = TRUE)
  expect_false(grepl("selected_output()", status_renderer, fixed = TRUE))
  expect_false(grepl("isolate(build_flow())", status_renderer, fixed = TRUE))
  expect_false(grepl("isolate(protocol())", status_renderer, fixed = TRUE))
  expect_false(grepl("output$result_card", enhancements, fixed = TRUE))
  expect_false(grepl('uiOutput("result_card")', app, fixed = TRUE))
  expect_match(
    build_server,
    'identical(workflow()$stage, "build")',
    fixed = TRUE
  )
})

test_that("Build result survives unsafe folder selection and clears on acceptance", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this folder lifecycle test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_freeze_plan <- function(...) {
    args <- list(...)
    plan <- builder_stage_frozen_plan(FALSE)
    plan$review_identity <- args$review_identity
    plan$out_dir <- args$out_dir
    plan
  }

  shiny::testServer(app_env$server, {
    real_session <- session
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))
    real_session$setInputs(make_app = FALSE)
    real_session$flushReact()
    entries <- isolate(sets())
    dataset_check_marks(builder_stage_checked_marks(
      entries,
      app_env$builder_project_check_identity,
      builder_configuration_identity_cache
    ))
    snapshot <- isolate(current_review_snapshot())
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", snapshot = snapshot)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", snapshot = snapshot)
    ))
    folder_env <- environment(choose_build_folder)
    release_parent <- withr::local_tempdir()
    viewer_target <- builder_new_release_target(
      release_parent,
      make_app = TRUE,
      .random_bytes = function(size) as.raw(c(0x0a, 0x1b, 0x2c))
    )
    expect_identical(
      basename(viewer_target),
      "CerebroNexus_viewer_0a1b2c"
    )
    expect_false(file.exists(viewer_target))
    dir.create(viewer_target)
    dir.create(file.path(
      release_parent,
      "CerebroNexus_crb_0a1b2c"
    ))
    identifiers <- list(
      as.raw(c(0x0a, 0x1b, 0x2c)),
      as.raw(c(0x3d, 0x4e, 0x5f))
    )
    crb_target <- builder_new_release_target(
      release_parent,
      make_app = FALSE,
      .random_bytes = function(size) {
        value <- identifiers[[1L]]
        identifiers <<- identifiers[-1L]
        value
      }
    )
    expect_identical(
      basename(crb_target),
      "CerebroNexus_crb_3d4e5f"
    )
    expect_false(file.exists(crb_target))
    expect_error(
      builder_new_release_target(release_parent, make_app = NA),
      "release parent is unsafe",
      fixed = TRUE
    )
    expect_error(
      builder_new_release_target(
        release_parent,
        make_app = TRUE,
        .attempts = Inf
      ),
      "release parent is unsafe",
      fixed = TRUE
    )
    selected_output("/old/output")
    protocol(app_env$builder_request_protocol("worker-folder"))
    success <- app_env$builder_result_success(
      published = TRUE,
      built = "/old/output/dataset.crb"
    )
    result(success)
    choices <- list(
      list(status = "cancelled", path = NULL),
      list(status = "error", path = NULL, error = "picker failed"),
      list(status = "selected", path = "/unsafe/output"),
      list(status = "selected", path = "/new/output")
    )
    assign(
      "session",
      list(
        token = "folder-lifecycle-test",
        onFlushed = function(callback, once = FALSE) callback(),
        sendCustomMessage = function(...) invisible(NULL)
      ),
      envir = folder_env
    )
    assign(
      "builder_start_native_picker",
      function(...) {
        choice <- choices[[1L]]
        choices <<- choices[-1L]
        list(kind = "output_directory", process = NULL, result = choice)
      },
      envir = folder_env
    )
    assign(
      "builder_build_output_preflight",
      function(path) {
        if (identical(path, "/unsafe/output")) {
          return(list(
            ok = FALSE,
            error = paste(
              "The selected folder contains files that do not belong",
              "to this release: builder-project.json, datasets, sources.",
              "Choose an empty folder or an existing Builder output folder."
            )
          ))
        }
        list(
          ok = TRUE,
          error = NULL,
          path = path,
          expected_roots = character()
        )
      },
      envir = folder_env
    )
    assign(
      "builder_new_release_target",
      function(parent, make_app) {
        file.path(
          parent,
          if (isTRUE(make_app)) {
            "CerebroNexus_viewer_a1b2c3"
          } else {
            "CerebroNexus_crb_a1b2c3"
          }
        )
      },
      envir = folder_env
    )
    assign(
      "builder_release_state",
      function(...) {
        list(
          schema_version = 1L,
          identity = list(
            schema_version = 1L,
            exists = FALSE,
            entries = list()
          ),
          record = NULL
        )
      },
      envir = folder_env
    )
    notifications <- character()
    assign(
      "builder_notify",
      function(type, message, ...) {
        notifications <<- c(notifications, as.character(message))
      },
      envir = folder_env
    )

    pending_protocol <- app_env$builder_request_protocol("worker-pending")
    pending_protocol <- app_env$builder_enqueue(
      pending_protocol,
      app_env$builder_query("preview", "dataset-a", generation = 1L)
    )
    result(NULL)
    protocol(pending_protocol)
    busy_note("Preparing preview…")
    real_session$flushReact()
    pending_content <- paste(
      unlist(output$build_stage_status_content),
      collapse = " "
    )
    expect_identical(pending_content, "")
    pending_footer <- paste(unlist(output$build_stage_footer), collapse = " ")
    expect_match(pending_footer, " disabled", fixed = TRUE)
    expect_length(output$busy, 0L)
    protocol(app_env$builder_request_protocol("worker-folder"))
    busy_note(NULL)
    result(success)

    choose_build_folder()
    real_session$flushReact()
    expect_identical(result(), success)
    expect_identical(selected_output(), "/old/output")
    choose_build_folder()
    real_session$flushReact()
    expect_identical(result(), success)
    expect_identical(selected_output(), "/old/output")
    choose_build_folder()
    real_session$flushReact()
    expect_identical(result(), success)
    expect_identical(selected_output(), "/old/output")
    expect_true(any(grepl(
      "Choose an empty folder",
      notifications,
      fixed = TRUE
    )))
    choose_build_folder()
    real_session$flushReact()
    expect_null(result())
    expect_identical(
      selected_output(),
      file.path("/new/output", "CerebroNexus_crb_a1b2c3")
    )
    expect_identical(build_flow()$stage, "preparing")
    real_session$flushReact()
    content <- paste(unlist(output$build_stage_footer), collapse = " ")
    expect_match(content, "Preparing build…", fixed = TRUE)
    expect_false(grepl(">Build<", content, fixed = TRUE))
  })
})

test_that("workflow server owns loading and Configure rendering", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this workflow test.")
  }
  shiny::testServer(app_env$server, {
    expect_identical(workflow()$stage, "upload")
    expect_match(
      paste(unlist(output$workflow_progress), collapse = " "),
      "Data",
      fixed = TRUE
    )

    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(name = "Dataset A")
    )))
    session$flushReact()
    expect_identical(workflow()$stage, "upload")

    session$setInputs(workflow_stage_review = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "upload")

    session$setInputs(workflow_stage_upload = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "upload")
    session$flushReact()
    expect_identical(workflow()$stage, "upload")

    session$setInputs(workflow_stage_configure = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "configure")
  })

  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl(
    "builder_loading_workbench_ui",
    workflow_server,
    fixed = TRUE
  ))
  expect_match(workflow_server, "render_configure_workbench", fixed = TRUE)
  expect_match(workflow_server, "render_review_workbench", fixed = TRUE)
  expect_match(workflow_server, "render_build_workbench", fixed = TRUE)
  expect_match(
    workflow_server,
    "Unsupported Builder workflow stage",
    fixed = TRUE
  )

  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    workflow_server,
    "snapshot <- isolate(current_review_snapshot())",
    fixed = TRUE
  )
  foundation_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    foundation_server,
    "dataset_order <- reactiveVal(character())",
    fixed = TRUE
  )
  expect_match(
    review_server,
    "snapshot <- workflow()$review_snapshot",
    fixed = TRUE
  )
  expect_match(review_server, "input$back_to_settings", fixed = TRUE)
  expect_false(grepl("review_current_dataset", review_server, fixed = TRUE))
  expect_false(grepl("dataset_review_footer", review_server, fixed = TRUE))
})

test_that("build completion preserves decisions and always has an idle ack path", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  decision <- list(
    state = "needs_decision",
    publishable = FALSE,
    error = "Choose analyses to retry.",
    failed_analyses = "marker_genes",
    retry_closure = c("marker_genes", "enriched_pathways")
  )
  settled <- app_env$builder_as_result(decision)
  expect_s3_class(settled, "builder_result_needs_decision")
  expect_identical(settled$failed_analyses, "marker_genes")
  expect_identical(
    settled$retry_closure,
    c("marker_genes", "enriched_pathways")
  )
  recovery <- app_env$builder_result_recovery_required("Restore the backup.")
  expect_match(recovery$error, "Restore the backup", fixed = TRUE)

  protocol <- app_env$builder_request_protocol("worker-a")
  queued <- app_env$builder_enqueue(
    protocol,
    app_env$builder_command(
      "build",
      "session",
      payload = list(id = "build-a")
    )
  )
  dispatched <- app_env$builder_protocol_dispatch(queued)
  completed <- app_env$builder_protocol_complete(
    dispatched$protocol,
    app_env$builder_worker_response(
      dispatched$request,
      list(state = "recovery_required", error = "Restore the backup.")
    )
  )
  expect_length(completed$protocol$awaiting_ack, 1L)
  acknowledged <- app_env$builder_protocol_acknowledge(
    completed$protocol,
    dispatched$request$request_id
  )
  expect_length(acknowledged$awaiting_ack, 0L)
  expect_identical(acknowledged$build_status, "idle")
})

test_that("Review inputs fail explicitly and recover without rebuilding inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    invalid <- list(
      welcome_message = "Welcome",
      point_size = 5,
      variable_to_compare = FALSE,
      host = "127.0.0.1",
      port = 0,
      max_request_size = 8000,
      display_mode = "normal",
      launch_browser = TRUE,
      show_upload_ui = FALSE
    )
    expect_false(validate_review_inputs(invalid))
    expect_false(review_validation()$ok)
    expect_match(review_validation()$error, "Review options", fixed = TRUE)
    expect_null(current_review_snapshot())
    invalid_plan <- freeze_plan_for_output(
      tempfile("invalid-viewer-options-"),
      output_options = builder_build_options(make_app = TRUE)
    )
    expect_identical(invalid_plan$error_code, "empty_release")
    expect_false(app_env$builder_review_can_build(invalid_plan))

    invalid$port <- 8080L
    expect_true(validate_review_inputs(invalid))
    expect_true(review_validation()$ok)
    expect_s3_class(review_options(), "builder_review_options")
    expect_null(current_review_snapshot())
  })

  review_source <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(review_source, "render_configure_workbench", fixed = TRUE)
  expect_false(grepl(
    'uiOutput("review_app_options")',
    review_source,
    fixed = TRUE
  ))
  expect_false(grepl("output$workbench <-", review_source, fixed = TRUE))
  expect_false(grepl("output$actionbar <-", review_source, fixed = TRUE))
  expect_false(grepl("output$build_actions <-", review_source, fixed = TRUE))
})

test_that("external spatial images carry required App output through Review", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  real_builder_freeze_plan <- app_env$builder_freeze_plan
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    review_identity,
    app_options,
    app_auth,
    ...
  ) {
    if (!isTRUE(make_app)) {
      return(real_builder_freeze_plan(
        entries = entries,
        out_dir = out_dir,
        make_app = make_app,
        overwrite = overwrite,
        review_identity = review_identity,
        app_options = app_options,
        app_auth = app_auth
      ))
    }
    plan <- builder_stage_frozen_plan(TRUE)
    plan$review_identity <- review_identity
    plan$dataset_order <- vapply(entries, `[[`, character(1), "id")
    plan$app_options <- app_options
    plan$app_auth <- app_auth
    plan$out_dir <- out_dir
    plan$overwrite <- overwrite
    plan
  }
  shiny::testServer(app_env$server, {
    entry <- builder_task6_entry()
    source_path <- tempfile(fileext = ".png")
    png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source_path)
    inspected <- app_env$builder_read_image(source_path)
    image <- app_env$builder_alignment_record(
      source = list(
        name = "H&E.png",
        type = inspected$mime,
        size = inspected$bytes
      ),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      parameters = list(),
      section = list(id = "fov", kind = "spatial"),
      source_path = inspected$source_path
    )
    image$source_content_md5 <- inspected$source_content_md5
    entry$dataset_profile$spatial <- list(sections = "fov")
    entry$snapshot <- builder_task6_snapshot_identity()
    entry$settings$images <- list(fov = list(`H&E` = image))
    entry$settings$spatial_image_storage <- "external"
    use_state_only_fixture(list(entry))

    entries <- isolate(sets())
    dataset_check_marks(builder_stage_checked_marks(
      entries,
      app_env$builder_project_check_identity,
      builder_configuration_identity_cache
    ))
    snapshot <- current_review_snapshot()
    expect_true(app_env$builder_review_snapshot_valid(snapshot))
    expect_true(snapshot$make_app)

    configured <- app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "datasets_ready")
    )
    workflow(app_env$builder_reduce_workflow(
      configured,
      list(type = "open_review", snapshot = snapshot)
    ))
    result(app_env$builder_result_success(
      published = TRUE,
      built = "/old/output/dataset.crb"
    ))
    expect_false(is.null(result()))
    session$setInputs(confirm_review = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "build")
    expect_null(result())
    expect_true(build_mode())

    session$setInputs(build_output_mode = "crb")
    session$flushReact()
    expect_true(build_mode())

    app_env$app_capability$available <- FALSE
    session$setInputs(build_output_mode = "app")
    session$flushReact()
    expect_false(build_mode())

    required <- freeze_plan_for_output(
      tempfile("external-images-crb-"),
      output_options = builder_build_options(make_app = FALSE)
    )
    expect_true(required$make_app)
  })
})

test_that("workbench identity ignores settings writes but tracks selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    entry <- function(id) {
      list(
        id = id,
        revision = 0L,
        snapshot = list(
          path = paste0("/private/", id),
          owner_token = paste0("owner-", id),
          object_md5 = strrep(substr(id, nchar(id), nchar(id)), 32L)
        ),
        profile = list(marker = id, extras = list()),
        settings = list(name = id)
      )
    }
    use_state_only_fixture(list(entry("dataset-a"), entry("dataset-b")))
    session$flushReact()
    renders <- 0L
    tracker <- observe({
      current()
      renders <<- renders + 1L
    })
    withr::defer(tracker$destroy())
    session$flushReact()
    baseline <- renders

    changed <- sets()[[1L]]
    changed$settings$name <- "Dataset A renamed"
    expect_true(replace_entry(changed))
    session$flushReact()
    expect_identical(renders, baseline)

    use_state_only_fixture(list(entry("dataset-b"), entry("dataset-a")))
    session$flushReact()
    expect_identical(renders, baseline + 1L)
  })
})

test_that("workflow metadata changes do not invalidate an unchanged stage", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    renders <- 0L
    tracker <- observe({
      selected_workflow_stage()
      renders <<- renders + 1L
    })
    withr::defer(tracker$destroy())
    session$flushReact()
    baseline <- renders

    next_workflow <- isolate(workflow())
    next_workflow$revision <- next_workflow$revision + 1L
    workflow(next_workflow)
    session$flushReact()

    expect_identical(renders, baseline)
  })
})

test_that("unchanged configure actions ignore projection settings writes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  renders <- 0L
  original_actions_ui <- app_env$builder_configure_actions_ui
  app_env$builder_configure_actions_ui <- function(...) {
    renders <<- renders + 1L
    original_actions_ui(...)
  }
  shiny::testServer(app_env$server, {
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-dataset-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "dataset-a", extras = list()),
      settings = list(
        name = "Dataset A",
        included_projections = "umap",
        reductions = "umap",
        default_projection = "umap"
      )
    )
    use_state_only_fixture(list(entry))
    session$flushReact()
    output$configure_actions
    baseline <- renders

    changed <- sets()[[1L]]
    changed$settings$included_projections <- c("umap", "pca")
    changed$settings$reductions <- c("umap", "pca")
    expect_true(replace_entry(changed))
    session$flushReact()
    output$configure_actions

    expect_identical(renders, baseline)
  })
})

test_that("reverting a projection edit does not restore an old check", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-dataset-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "dataset-a", extras = list()),
      settings = list(
        name = "Dataset A",
        included_projections = "umap",
        reductions = "umap",
        default_projection = "umap"
      )
    )
    use_state_only_fixture(list(entry))
    dataset_check_marks(stats::setNames(
      app_env$builder_project_check_identity(entry),
      entry$id
    ))
    expect_identical(checked_dataset_ids(), entry$id)
    old_mark <- app_env$builder_project_check_identity(entry)
    same_settings_after_edits <- entry
    same_settings_after_edits$revision <- 2L
    expect_false(identical(
      old_mark,
      app_env$builder_project_check_identity(same_settings_after_edits)
    ))

    changed <- sets()[[1L]]
    changed$settings$included_projections <- c("umap", "pca")
    changed$settings$reductions <- c("umap", "pca")
    expect_true(replace_entry(changed))
    expect_length(checked_dataset_ids(), 0L)

    reverted <- sets()[[1L]]
    reverted$settings$included_projections <- "umap"
    reverted$settings$reductions <- "umap"
    expect_true(replace_entry(reverted))
    expect_length(checked_dataset_ids(), 0L)
  })
})

test_that("incomplete datasets focus their first unresolved setting", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    messages <- list()
    original_send <- session$sendCustomMessage
    session$sendCustomMessage <- function(type, message) {
      if (identical(type, "builder_focus_incomplete_setting")) {
        messages[[length(messages) + 1L]] <<- message
      }
      original_send(type, message)
    }
    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-dataset-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "dataset-a", extras = list()),
      settings = list(name = "Dataset A", organism = NULL)
    )))

    session$setInputs(complete_dataset_check = 1)
    session$flushReact()

    expect_length(dataset_check_marks(), 0L)
    expect_length(checked_dataset_ids(), 0L)
    expect_identical(
      messages,
      list(list(
        dataset = "dataset-a",
        generation = 0L,
        blocker = "settings_organism"
      ))
    )
  })
})

test_that("Apply to all sets the organism on unchecked datasets only", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    entry <- function(id, load_state = "loaded") {
      list(
        id = id,
        revision = 0L,
        load_state = load_state,
        snapshot = list(
          path = paste0("/private/", id),
          owner_token = paste0("owner-", id),
          object_md5 = strrep(substr(id, nchar(id), nchar(id)), 32L)
        ),
        profile = list(marker = id, extras = list()),
        settings = list(name = id, organism = NULL)
      )
    }
    use_state_only_fixture(list(
      entry("dataset-a"),
      entry("dataset-b"),
      entry("dataset-c"),
      entry("dataset-d", "artifact_ready")
    ))
    checked <- isolate(sets())[[3L]]
    dataset_check_marks(stats::setNames(
      app_env$builder_project_check_identity(checked),
      checked$id
    ))
    session$flushReact()
    expect_identical(current(), "dataset-a")

    session$setInputs(
      `core-rendered_for` = "dataset-a",
      `core-organism` = "mm"
    )
    session$setInputs(`core-organism_apply_all` = 1)
    session$flushReact()

    organisms <- vapply(
      isolate(sets()),
      function(entry) entry$settings$organism %||% "",
      character(1)
    )
    expect_identical(
      organisms,
      c("", "mm", "", "")
    )
    expect_identical(checked_dataset_ids(), "dataset-c")
  })
})

test_that("Next from a checked CRB opens the next unchecked dataset", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    entry <- function(id, load_state = "loaded") {
      list(
        id = id,
        revision = 0L,
        load_state = load_state,
        snapshot = list(
          path = paste0("/private/", id),
          owner_token = paste0("owner-", id),
          object_md5 = strrep(substr(id, nchar(id), nchar(id)), 32L)
        ),
        profile = list(marker = id, extras = list()),
        settings = list(name = id)
      )
    }
    use_state_only_fixture(list(
      entry("dataset-a", "artifact_ready"),
      entry("dataset-b")
    ))
    first <- isolate(sets())[[1L]]
    dataset_check_marks(stats::setNames(
      app_env$builder_project_check_identity(first),
      first$id
    ))
    session$flushReact()

    expect_identical(current(), "dataset-a")
    expect_identical(checked_dataset_ids(), "dataset-a")
    session$setInputs(complete_dataset_check = 1)
    session$flushReact()

    expect_identical(current(), "dataset-b")
    expect_identical(checked_dataset_ids(), "dataset-a")
  })
})

test_that("Builder auth accepts only the exact typed browser payload", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  capability <- new.env(parent = emptyenv())
  capability$available <- TRUE
  app_env$auth_capability <- function() {
    list(available = capability$available, reason = NULL)
  }
  shiny::testServer(app_env$server, {
    valid_accounts <- list(list(
      id = "auth-account-1",
      username = "user-a",
      password = "password-a"
    ))
    session$setInputs(
      builder_auth_accounts = list(
        enabled = "true",
        accounts = valid_accounts,
        nonce = 1
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)

    build_mode(TRUE)
    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = list(1)
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)

    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = 2
      )
    )
    session$flushReact()
    expect_true(auth_enabled())
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_identical(auth_accounts()[[1L]]$username, "user-a")

    session$setInputs(builder_auth_accounts = NULL)
    session$flushReact()
    expect_true(auth_enabled())
    expect_length(auth_accounts(), 1L)

    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = list(list(
          id = "auth-account-1",
          username = "user-a",
          password = "short"
        )),
        nonce = 3
      )
    )
    session$flushReact()
    expect_true(auth_enabled())
    expect_length(auth_accounts(), 1L)
    expect_false(auth_validation()$ok)
    expect_identical(
      auth_validation()$error,
      "Login accounts could not be saved."
    )
    expect_false(grepl("short", auth_validation()$error, fixed = TRUE))

    build_mode(FALSE)
    session$flushReact()
    expect_false(auth_enabled())
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_length(auth_accounts(), 0L)

    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = 4
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)
    expect_identical(
      auth_validation()$error,
      "Login accounts could not be saved."
    )

    capability$available <- FALSE
    build_mode(TRUE)
    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = 5
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)
    expect_identical(
      auth_validation()$error,
      "Login accounts could not be saved."
    )
  })
})

test_that("Build-only auth changes preserve the confirmed CRB review", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$auth_capability <- function() {
    list(available = TRUE, reason = NULL)
  }
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    review_identity,
    app_options,
    app_auth,
    ...
  ) {
    plan <- builder_stage_frozen_plan(make_app)
    plan$review_identity <- review_identity
    plan$dataset_order <- vapply(entries, `[[`, character(1), "id")
    plan$app_options <- app_options
    plan$app_auth <- app_auth
    plan$out_dir <- out_dir
    plan$overwrite <- overwrite
    plan
  }

  shiny::testServer(app_env$server, {
    added_messages <- list()
    original_send_custom_message <- session$sendCustomMessage
    session$sendCustomMessage <- function(type, message) {
      if (identical(type, "enhance_tables_added")) {
        added_messages[[length(added_messages) + 1L]] <<- message
      }
      original_send_custom_message(type, message)
    }
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))
    accounts_a <- app_env$builder_auth_validate_payload(
      TRUE,
      list(list(
        id = "auth-account-1",
        username = "user-a",
        password = "password-a"
      ))
    )$accounts
    accounts_b <- app_env$builder_auth_validate_payload(
      TRUE,
      c(
        unclass(accounts_a),
        list(list(
          id = "auth-account-2",
          username = "user-b",
          password = "password-b"
        ))
      )
    )$accounts
    auth_enabled(TRUE)
    auth_accounts(accounts_a)
    build_mode(TRUE)
    session$flushReact()

    entries <- isolate(sets())
    dataset_check_marks(builder_stage_checked_marks(
      entries,
      app_env$builder_project_check_identity,
      builder_configuration_identity_cache
    ))
    review_snapshot <- current_review_snapshot()
    expect_false(review_snapshot$make_app)
    reviewed <- app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "open_review", snapshot = review_snapshot)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", snapshot = review_snapshot)
    ))
    expect_identical(workflow()$stage, "build")

    review_validation(list(
      ok = FALSE,
      error = "Viewer App options are invalid."
    ))
    session$flushReact()
    expect_identical(workflow()$stage, "build")
    invalid_app_plan <- freeze_plan_for_output(
      tempfile("invalid-app-output-"),
      output_options = current_build_options()
    )
    expect_identical(invalid_app_plan$error_code, "invalid_review_options")
    build_mode(FALSE)
    crb_plan <- freeze_plan_for_output(
      tempfile("valid-crb-output-"),
      output_options = current_build_options()
    )
    expect_s3_class(crb_plan, "builder_build_plan")
    expect_identical(crb_plan$readiness, "ready")
    build_mode(TRUE)
    review_validation(list(ok = TRUE, error = NULL))

    auth_accounts(accounts_b)
    session$flushReact()

    expect_identical(workflow()$stage, "build")
    expect_identical(workflow()$review_snapshot, review_snapshot)

    review_options(builder_review_options(
      welcome_message = "Build-stage welcome",
      initial_page = "projection",
      host = "0.0.0.0",
      port = 4242L,
      launch_browser = FALSE,
      show_upload_ui = TRUE
    ))
    build_initial_dataset("dataset-a")
    plan_b <- freeze_plan_for_output(
      tempfile("builder-app-output-"),
      output_options = current_build_options()
    )
    expect_true(plan_b$make_app)
    expect_identical(plan_b$app_auth$account_count, 2L)
    expect_named(
      plan_b$app_auth,
      c("enabled", "account_count", "timeout_minutes")
    )
    expect_false("accounts" %in% names(plan_b$app_auth))
    expect_identical(plan_b$app_options$welcome_message, "Build-stage welcome")
    expect_identical(plan_b$app_options$host, "0.0.0.0")
    expect_identical(plan_b$app_options$port, 4242L)
    expect_false(plan_b$app_options$launch_browser)
    expect_true(plan_b$app_options$show_upload_ui)
    expect_identical(plan_b$app_options$initial_dataset, "dataset-a")
    expect_identical(plan_b$app_options$initial_page, "projection")
    expect_false(any(grepl(
      "password-a|password-b|user-a|user-b",
      capture.output(dput(plan_b))
    )))

    build_flow(list(stage = "building", plan = NULL))
    session$setInputs(build_output_mode = "crb")
    session$flushReact()
    expect_true(build_mode())
  })
})

test_that("Build enqueue retains auth after failure and resets only after success", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    expect_identical(
      names(formals(enqueue_build_plan)),
      c("plan", "auth_accounts")
    )
    validate_auth <- get(
      "builder_auth_validate_payload",
      envir = environment(enqueue_build_plan),
      inherits = TRUE
    )
    summarize_auth <- get(
      "builder_auth_summary",
      envir = environment(enqueue_build_plan),
      inherits = TRUE
    )
    accounts <- validate_auth(
      TRUE,
      list(list(
        id = "auth-account-1",
        username = "user-a",
        password = "password-a"
      ))
    )$accounts
    plan <- builder_stage_frozen_plan()
    plan$app_auth <- summarize_auth(TRUE, accounts)
    snapshot <- builder_stage_review_snapshot(plan)
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", snapshot = snapshot)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", snapshot = snapshot)
    ))
    fn_env <- environment(enqueue_build_plan)
    messages <- list()
    assign(
      "session",
      list(sendCustomMessage = function(type, message) {
        messages[[length(messages) + 1L]] <<- list(
          type = type,
          message = message
        )
      }),
      envir = fn_env
    )
    worker(list(alive = TRUE))
    request_protocol <- get(
      "builder_request_protocol",
      envir = fn_env,
      inherits = TRUE
    )
    protocol(request_protocol("worker-a"))
    auth_enabled(TRUE)
    auth_accounts(accounts)
    auth_validation(list(ok = TRUE, error = NULL))

    queued_payload <- NULL
    assign(
      "enqueue",
      function(payload) {
        queued_payload <<- payload
        FALSE
      },
      envir = fn_env
    )
    worker(list(alive = TRUE))
    expect_false(enqueue_build_plan(plan, auth_accounts = accounts))
    expect_s3_class(queued_payload$auth_accounts, "builder_auth_accounts")
    expect_identical(queued_payload$auth_accounts, accounts)
    expect_identical(auth_accounts(), accounts)
    expect_true(auth_validation()$ok)
    expect_length(messages, 1L)
    expect_identical(messages[[1L]]$type, "builder_build_dialog")
    expect_identical(messages[[1L]]$message, list(action = "close"))

    messages <- list()
    progress_root <- withr::local_tempdir()
    worker(list(alive = TRUE, snapshot_root = progress_root))
    assign(
      "enqueue",
      function(payload) {
        queued_payload <<- payload
        TRUE
      },
      envir = fn_env
    )
    expect_true(enqueue_build_plan(plan, auth_accounts = accounts))
    expect_identical(
      normalizePath(dirname(queued_payload$progress_path)),
      normalizePath(progress_root)
    )
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)
    expect_match(auth_validation()$error, "Set up", fixed = TRUE)
    expect_identical(
      vapply(messages, `[[`, character(1), "type"),
      "builder_auth_reset"
    )
    expect_identical(messages[[1L]]$message, list(reset = TRUE))
  })
})

test_that("Build dialogs cannot enqueue a stale frozen revision", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  shiny::testServer(app_env$server, {
    fn_env <- environment(builder_require_confirmed_build_plan)
    notifications <- character()
    dialog_messages <- list()
    assign(
      "builder_notify",
      function(type, message, ...) {
        notifications <<- c(notifications, as.character(message))
      },
      envir = fn_env
    )
    assign(
      "session",
      list(sendCustomMessage = function(type, message) {
        dialog_messages[[length(dialog_messages) + 1L]] <<- list(
          type = type,
          message = message
        )
      }),
      envir = fn_env
    )
    plan_a <- builder_stage_frozen_plan()
    snapshot <- builder_stage_review_snapshot(plan_a)
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", snapshot = snapshot)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", snapshot = snapshot)
    ))

    build_flow(list(stage = "confirming", plan = plan_a))
    workflow(app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "invalidate")
    ))
    expect_false(builder_require_confirmed_build_plan(plan_a))
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))

    workflow(app_env$builder_reduce_workflow(
      app_env$builder_reduce_workflow(
        app_env$builder_workflow_state(),
        list(type = "open_review", snapshot = snapshot)
      ),
      list(type = "confirm_review", snapshot = snapshot)
    ))
    relocated <- plan_a
    relocated$out_dir <- tempfile("relocated-output-")

    plan_b <- plan_a
    plan_b$review_identity <- builder_stage_review_identity(
      revisions = c(99L, 2L)
    )
    build_flow(list(stage = "conflict", plan = plan_a))
    guard <- builder_build_confirmation_status(isolate(workflow()), plan_b)
    expect_identical(guard, list(ok = FALSE, reason = "identity_mismatch"))
    expect_false(builder_require_confirmed_build_plan(plan_b))
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(workflow()$stage, "configure")
    expect_null(workflow()$review_snapshot)
    expect_null(workflow()$confirmation)
    expect_match(
      tail(notifications, 1L),
      "Settings changed. Review the updated plan before building.",
      fixed = TRUE
    )
    expect_true(any(vapply(
      dialog_messages,
      function(message) identical(message$message, list(action = "close")),
      logical(1)
    )))
    enqueued <- FALSE
    assign(
      "enqueue",
      function(...) {
        enqueued <<- TRUE
        TRUE
      },
      envir = environment(enqueue_build_plan)
    )
    expect_false(enqueue_build_plan(
      plan_b,
      auth_accounts = app_env$builder_auth_empty_accounts()
    ))
    expect_false(enqueued)
  })

  build_source <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    build_source,
    "builder_require_confirmed_build_plan(plan, plan$out_dir)",
    fixed = TRUE
  )
  expect_match(build_source, 'reason = "output_mismatch"', fixed = TRUE)
})

test_that("active Build states reject forged stage actions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_freeze_plan <- function(out_dir, review_identity, ...) {
    plan <- builder_stage_frozen_plan(FALSE)
    plan$review_identity <- review_identity
    plan$output_release$directory <- out_dir
    plan$output_release$targets <- file.path(
      out_dir,
      basename(plan$output_release$targets)
    )
    plan
  }
  selected_directory <- file.path(withr::local_tempdir(), "selected-output")
  dir.create(selected_directory)
  selected_directory <- normalizePath(
    selected_directory,
    winslash = "/",
    mustWork = TRUE
  )

  shiny::testServer(app_env$server, {
    real_session <- session
    picker_calls <- 0L
    notifications <- character()
    fn_env <- environment(builder_require_confirmed_build_plan)
    assign(
      "session",
      structure(
        list(
          token = real_session$token,
          sendCustomMessage = function(...) NULL,
          sendInputMessage = function(...) NULL,
          onFlushed = function(callback, once = FALSE) callback(),
          isClosed = function() real_session$isClosed()
        ),
        class = "ShinySession"
      ),
      envir = fn_env
    )
    assign(
      "builder_notify",
      function(type, message, ...) {
        notifications <<- c(notifications, as.character(message))
      },
      envir = fn_env
    )
    assign(
      "builder_start_native_picker",
      function(...) {
        picker_calls <<- picker_calls + 1L
        list(
          kind = "output_directory",
          process = NULL,
          result = list(status = "selected", path = selected_directory)
        )
      },
      envir = fn_env
    )
    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(name = "Dataset A")
    )))
    real_session$setInputs(make_app = FALSE)
    real_session$flushReact()
    entries <- isolate(sets())
    dataset_check_marks(builder_stage_checked_marks(
      entries,
      app_env$builder_project_check_identity,
      builder_configuration_identity_cache
    ))
    snapshot <- current_review_snapshot()
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", snapshot = snapshot)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", snapshot = snapshot)
    ))
    selected_output("/confirmed/output")

    for (index in seq_along(c("preparing", "building", "choosing_folder"))) {
      stage <- c("preparing", "building", "choosing_folder")[[index]]
      frozen_flow <- list(
        stage = stage,
        plan = NULL
      )
      build_flow(frozen_flow)
      notification_count <- length(notifications)
      real_session$setInputs(back_to_review = index)
      real_session$setInputs(build = index)
      real_session$flushReact()

      expect_identical(workflow()$stage, "build", info = stage)
      expect_identical(build_flow(), frozen_flow, info = stage)
      expect_identical(selected_output(), "/confirmed/output", info = stage)
      expect_identical(picker_calls, 0L, info = stage)
      expect_true(
        length(notifications) > notification_count,
        info = stage
      )
      expect_true(any(grepl(
        "Wait for the active build to finish",
        notifications,
        fixed = TRUE
      )))
    }

    build_flow(list(stage = "idle", plan = NULL))
    selected_output(NULL)
    output_preflight <- get("builder_build_output_preflight", envir = fn_env)
    assign(
      "builder_build_output_preflight",
      function(...) stop("preflight failed"),
      envir = fn_env
    )
    real_session$setInputs(build = 4L)
    real_session$flushReact()
    expect_identical(picker_calls, 1L)
    expect_null(selected_output())
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_match(
      tail(notifications, 1L),
      "The selected folder could not be checked: preflight failed",
      fixed = TRUE
    )
    assign("builder_build_output_preflight", output_preflight, envir = fn_env)
    real_session$setInputs(build = 5L)
    real_session$flushReact()
    expect_identical(picker_calls, 2L)
    release_directory <- selected_output()
    expect_identical(dirname(release_directory), selected_directory)
    expect_match(
      basename(release_directory),
      "^CerebroNexus_(viewer|crb)_[0-9a-f]{6}$"
    )
    navigation_result <- app_env$builder_result_success(
      published = TRUE,
      built = file.path(release_directory, "dataset.crb")
    )
    result(navigation_result)
    real_session$setInputs(back_to_review = 4L)
    real_session$flushReact()
    expect_identical(workflow()$stage, "review")
    expect_identical(result(), navigation_result)
  })
})

test_that("incomplete datasets can be removed before Review", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }

  shiny::testServer(app_env$server, {
    entries <- lapply(c("dataset-a", "dataset-b"), function(id) {
      entry <- builder_minimal_entry(id, id)
      entry$settings$organism <- NULL
      entry
    })
    use_state_only_fixture(entries)

    session$setInputs(drop_ds = list(id = "dataset-a", confirmed = TRUE))
    session$flushReact()

    expect_identical(
      vapply(store()$datasets, `[[`, character(1), "id"),
      "dataset-b"
    )
  })
})

test_that("active builds lock dataset imports and rail mutations", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)

  shiny::testServer(app_env$server, {
    real_session <- session
    notifications <- character()
    assign(
      "builder_notify",
      function(type, message, ...) {
        notifications <<- c(notifications, as.character(message))
      },
      envir = environment(start_load)
    )
    worker(list(alive = TRUE, snapshot_root = tempdir()))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-lock"))
    entries <- lapply(c("dataset-a", "dataset-b"), function(id) {
      list(
        id = id,
        source_id = id,
        output_id = id,
        selector_value = id,
        path = file.path(tempdir(), paste0(id, ".rds")),
        snapshot = builder_task6_snapshot_identity(),
        format = "RDS",
        profile = list(
          n_cells = 12L,
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA"
        ),
        settings = list(
          name = id,
          groups = "cluster",
          reductions = "umap",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          palette = list(cluster = c(one = "#111111")),
          analyses = character()
        )
      )
    })
    store(app_env$builder_reduce_state(
      app_env$builder_state(entries),
      list(type = "remove", id = "dataset-b")
    ))
    expect_true(store()$can_undo_remove)

    for (index in seq_along(c("queued", "building"))) {
      stage <- c("queued", "building")[[index]]
      build_flow(list(stage = stage, plan = NULL))
      active_protocol <- isolate(protocol())
      active_protocol$build_status <- c("queued", "running")[[index]]
      protocol(active_protocol)
      before <- isolate(store())
      before_imports <- isolate(imports())
      before_protocol <- isolate(protocol())

      expect_false(start_load(
        "example",
        paste0("locked-example-", index),
        "Locked example"
      ))
      real_session$setInputs(
        reorder_ds = list(id = "dataset-a", direction = "down"),
        drop_ds = list(id = "dataset-a", confirmed = TRUE),
        undo_remove = index,
        use_example = c("complete_viewer_data", "spatial")[[index]],
        dataset_files = data.frame(
          name = "forged.rds",
          datapath = tempfile(fileext = ".rds"),
          size = 1,
          type = "application/octet-stream"
        )
      )
      real_session$flushReact()

      expect_identical(store(), before, info = stage)
      expect_identical(imports(), before_imports, info = stage)
      expect_identical(protocol(), before_protocol, info = stage)
    }
    expect_true(any(grepl(
      "active build",
      tolower(notifications),
      fixed = TRUE
    )))

    build_flow(list(stage = "idle", plan = NULL))
    protocol(app_env$builder_request_protocol("worker-lock"))
    expect_true(start_load("example", "idle-example", "Idle example"))
    expect_length(imports()$entries, 1L)
    real_session$setInputs(undo_remove = 3L)
    real_session$flushReact()
    expect_false(store()$can_undo_remove)
    expect_length(store()$datasets, 2L)
  })
})

test_that("removing an artifact also cancels its in-flight source replacement", {
  source <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  removal <- sub(".*remove_dataset <- function\\(", "", source)
  removal <- sub("\\n\\}\\n\\n# observeEvent\\(input\\$drop_ds.*", "", removal)

  expect_match(removal, "import_of(id)", fixed = TRUE)
  expect_match(removal, "remove_pending_import(id)", fixed = TRUE)
})

test_that("Build recovery actions preserve confirmation only when safe", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this recovery test.")
  }
  app_env$auth_capability <- function() {
    list(available = TRUE, reason = NULL)
  }
  app_env$builder_operation_allowed <- function(...) TRUE
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    review_identity,
    app_options,
    app_auth,
    ...
  ) {
    plan <- builder_stage_frozen_plan(make_app)
    plan$review_identity <- review_identity
    plan$revision <- max(vapply(entries, `[[`, integer(1), "revision"))
    plan$dataset_order <- vapply(entries, `[[`, character(1), "id")
    plan$items <- list(plan$items[[1L]])
    plan$items[[1L]]$id <- entries[[1L]]$id
    plan$items[[1L]]$analyses <- entries[[1L]]$settings$analyses %||%
      character()
    plan$app_auth <- app_auth
    plan$out_dir <- out_dir
    plan$overwrite <- overwrite
    plan$existing_targets <- character()
    plan
  }

  shiny::testServer(app_env$server, {
    real_session <- session
    recovery_output <- tempfile("builder-recovery-output-")
    notifications <- character()
    enqueued <- list()
    restart_succeeds <- TRUE
    server_env <- environment(start_confirmed_build)
    assign(
      "builder_operation_allowed",
      function(...) TRUE,
      envir = server_env
    )
    assign(
      "builder_notify",
      function(type, message, ...) {
        notifications <<- c(notifications, as.character(message))
      },
      envir = server_env
    )
    assign(
      "restart_worker_protocol",
      function(...) restart_succeeds,
      envir = server_env
    )
    assign(
      "enqueue",
      function(payload) {
        enqueued[[length(enqueued) + 1L]] <<- payload
        queued <- isolate(protocol())
        queued$build_status <- "queued"
        protocol(queued)
        TRUE
      },
      envir = server_env
    )

    action_nonce <- 0L
    recovery_output <- file.path(tempdir(), "builder-recovery-output")
    valid_accounts <- app_env$builder_auth_validate_payload(
      TRUE,
      list(list(
        id = "auth-account-1",
        username = "user-a",
        password = "password-a"
      ))
    )$accounts
    set_case <- function(
      value,
      auth_missing = FALSE,
      auth_ready = FALSE,
      stale = FALSE,
      analyses = "marker_genes"
    ) {
      entry <- list(
        id = "dataset-a",
        revision = 0L,
        snapshot = list(
          path = "/private/dataset-a",
          owner_token = "owner-a",
          object_md5 = strrep("a", 32L)
        ),
        profile = list(marker = "a", extras = list()),
        settings = list(name = "Dataset A", analyses = analyses)
      )
      use_state_only_fixture(list(entry))
      auth_enabled(isTRUE(auth_ready))
      auth_accounts(
        if (isTRUE(auth_ready)) {
          valid_accounts
        } else {
          app_env$builder_auth_empty_accounts()
        }
      )
      build_mode(isTRUE(auth_ready) || isTRUE(auth_missing))
      real_session$flushReact()
      entries <- isolate(sets())
      dataset_check_marks(builder_stage_checked_marks(
        entries,
        app_env$builder_project_check_identity,
        builder_configuration_identity_cache
      ))
      snapshot <- isolate(current_review_snapshot())
      if (isTRUE(auth_missing)) {
        auth_enabled(TRUE)
      }
      reviewed <- app_env$builder_reduce_workflow(
        app_env$builder_workflow_state(),
        list(type = "open_review", snapshot = snapshot)
      )
      workflow(app_env$builder_reduce_workflow(
        reviewed,
        list(type = "confirm_review", snapshot = snapshot)
      ))
      if (isTRUE(stale)) {
        changed <- isolate(sets())[[1L]]
        changed$settings$name <- "Changed after review"
        replace_entry(changed, internal = TRUE)
      }
      selected_output(recovery_output)
      build_flow(list(stage = "idle", plan = NULL))
      worker(list(alive = TRUE))
      protocol(app_env$builder_request_protocol("worker-recovery"))
      result(value)
      notifications <<- character()
      enqueued <<- list()
      invisible(snapshot)
    }
    click_action <- function(id) {
      action_nonce <<- action_nonce + 1L
      do.call(
        real_session$setInputs,
        stats::setNames(list(action_nonce), id)
      )
      real_session$flushReact()
    }

    restart_failure <- app_env$builder_result_failure(
      "Worker stopped.",
      restartable_worker = TRUE
    )
    set_case(restart_failure)
    restart_succeeds <- TRUE
    click_action("restart_worker")
    expect_null(result())
    expect_identical(workflow()$stage, "build")
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(selected_output(), recovery_output)
    expect_match(
      paste(unlist(output$build_stage_footer), collapse = " "),
      ">Build<",
      fixed = TRUE
    )

    set_case(restart_failure)
    restart_succeeds <- FALSE
    click_action("restart_worker")
    expect_identical(result(), restart_failure)
    expect_match(
      tail(notifications, 1L),
      "could not restart",
      ignore.case = TRUE
    )

    # Missing login accounts are asked for in place; the reviewed plan, the
    # output and the result stay.
    set_case(restart_failure, auth_missing = TRUE)
    restart_succeeds <- TRUE
    click_action("restart_worker")
    expect_identical(workflow()$stage, "build")
    expect_identical(selected_output(), recovery_output)
    expect_s3_class(result(), "builder_result")
    expect_match(
      tail(notifications, 1L),
      "Enter the login accounts for this build, then build again.",
      fixed = TRUE
    )

    decision <- app_env$builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "dataset-a"
    )
    set_case(decision)
    click_action("remove_failed_analysis")
    expect_false("marker_genes" %in% entry_of("dataset-a")$settings$analyses)
    expect_identical(workflow()$stage, "configure")
    expect_null(selected_output())
    expect_null(result())
    expect_length(enqueued, 0L)
    expect_match(
      tail(notifications, 1L),
      "Optional work removed. Review the updated plan before building.",
      fixed = TRUE
    )

    missing_decision <- app_env$builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "missing"
    )
    set_case(missing_decision)
    click_action("remove_failed_analysis")
    expect_identical(result(), missing_decision)
    expect_length(enqueued, 0L)
    expect_match(
      tail(notifications, 1L),
      "could not be removed",
      ignore.case = TRUE
    )

    set_case(decision)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 1L)
    expect_null(result())
    expect_identical(build_flow()$stage, "building")

    set_case(decision, auth_ready = TRUE)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 1L)
    expect_null(result())
    expect_identical(build_flow()$stage, "building")
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)

    set_case(decision, auth_missing = TRUE)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 0L)
    expect_identical(workflow()$stage, "build")
    expect_identical(selected_output(), recovery_output)
    expect_identical(result(), decision)
    expect_match(
      tail(notifications, 1L),
      "Enter the login accounts for this build, then build again.",
      fixed = TRUE
    )

    set_case(decision, stale = TRUE)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 0L)
    expect_identical(workflow()$stage, "configure")
    expect_null(selected_output())
    expect_match(tail(notifications, 1L), "Review", fixed = TRUE)
    expect_match(tail(notifications, 1L), "before retrying", fixed = TRUE)
  })
})

test_that("Viewer and spatial preview contracts ignore settings-only revisions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  expect_true(exists(
    "builder_projection_preview_contract",
    envir = app_env,
    inherits = FALSE
  ))
  expect_true(exists(
    "builder_trajectory_preview_contract",
    envir = app_env,
    inherits = FALSE
  ))
  expect_true(exists(
    "builder_preview_revision_independent",
    envir = app_env,
    inherits = FALSE
  ))
  if (
    !exists(
      "builder_projection_preview_contract",
      envir = app_env,
      inherits = FALSE
    ) ||
      !exists(
        "builder_trajectory_preview_contract",
        envir = app_env,
        inherits = FALSE
      ) ||
      !exists(
        "builder_preview_revision_independent",
        envir = app_env,
        inherits = FALSE
      )
  ) {
    return(invisible(NULL))
  }

  expect_true(app_env$builder_preview_revision_independent(
    "projection_previews"
  ))
  expect_true(app_env$builder_preview_revision_independent(
    "trajectory_previews"
  ))
  expect_true(app_env$builder_preview_revision_independent(
    "spatial_preview"
  ))
  expect_false(app_env$builder_preview_revision_independent("preview"))

  entry <- list(
    id = "dataset-a",
    revision = 1L,
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    settings = list(
      default_group = "cluster",
      overview_point_size = 5,
      included_projections = "umap",
      default_projection = "umap",
      included_trajectories = list(monocle2 = "lineage_a"),
      default_trajectory = list(method = "monocle2", name = "lineage_a"),
      group_color_overrides = list()
    )
  )
  projections <- c("umap", "pca")
  trajectories <- list(monocle2 = c("lineage_a", "lineage_b"))
  projection_contract <- app_env$builder_projection_preview_contract(
    entry,
    projections
  )
  trajectory_contract <- app_env$builder_trajectory_preview_contract(
    entry,
    trajectories
  )

  cache <- app_env$builder_preview_cache_begin(
    list(),
    entry$id,
    projection_contract
  )
  cache <- app_env$builder_preview_cache_store(
    cache,
    entry$id,
    list(umap = list(x = 1, y = 2))
  )
  cache <- app_env$builder_preview_cache_begin(
    cache,
    "dataset-b",
    list(dataset = "dataset-b")
  )
  expect_true(app_env$builder_preview_cache_hit(
    cache,
    entry$id,
    projection_contract
  ))
  expect_identical(
    app_env$builder_preview_cache_frames(cache, entry$id),
    list(umap = list(x = 1, y = 2))
  )

  settings_only <- entry
  settings_only$revision <- 9L
  settings_only$settings$overview_point_size <- 12
  settings_only$settings$included_projections <- c("umap", "pca")
  settings_only$settings$default_projection <- "pca"
  settings_only$settings$included_trajectories <- list(
    monocle2 = "lineage_b"
  )
  settings_only$settings$default_trajectory <- list(
    method = "monocle2",
    name = "lineage_b"
  )
  settings_only$settings$group_color_overrides <- list(
    cluster = c(A = "#123456")
  )

  expect_identical(
    app_env$builder_projection_preview_contract(settings_only, projections),
    projection_contract
  )
  expect_identical(
    app_env$builder_trajectory_preview_contract(settings_only, trajectories),
    trajectory_contract
  )

  regrouped <- settings_only
  regrouped$settings$default_group <- "sample"
  expect_false(identical(
    app_env$builder_projection_preview_contract(regrouped, projections),
    projection_contract
  ))
  expect_identical(
    app_env$builder_trajectory_preview_contract(regrouped, trajectories),
    trajectory_contract
  )

  resnapshotted <- settings_only
  resnapshotted$snapshot$object_md5 <- strrep("b", 32L)
  expect_false(identical(
    app_env$builder_projection_preview_contract(resnapshotted, projections),
    projection_contract
  ))
  expect_false(identical(
    app_env$builder_trajectory_preview_contract(resnapshotted, trajectories),
    trajectory_contract
  ))
  expect_false(identical(
    app_env$builder_projection_preview_contract(
      settings_only,
      c(projections, "tsne")
    ),
    projection_contract
  ))
  expect_false(identical(
    app_env$builder_trajectory_preview_contract(
      settings_only,
      list(monocle2 = c("lineage_a", "lineage_b", "lineage_c"))
    ),
    trajectory_contract
  ))
})

test_that("table inventories stay with their dataset owner and project epoch", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  entry <- list(
    id = "ds1",
    revision = 2L,
    snapshot = list(
      path = "/private/project-a/ds1",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    settings = list(name = "Project A")
  )
  owner <- app_env$builder_table_inventory_owner(entry, 7)
  expect_true(app_env$builder_table_inventory_owner_is_current(
    owner,
    entry,
    7
  ))

  settings_only <- entry
  settings_only$revision <- 9L
  settings_only$settings$name <- "Renamed while the workbook is read"
  expect_true(app_env$builder_table_inventory_owner_is_current(
    owner,
    settings_only,
    7
  ))

  same_id_other_project <- entry
  same_id_other_project$snapshot$path <- "/private/project-b/ds1"
  same_id_other_project$snapshot$owner_token <- "owner-b"
  same_id_other_project$snapshot$object_md5 <- strrep("b", 32L)
  expect_false(app_env$builder_table_inventory_owner_is_current(
    owner,
    same_id_other_project,
    7
  ))
  expect_false(app_env$builder_table_inventory_owner_is_current(
    owner,
    entry,
    8
  ))
})

test_that("the Configure title follows a dataset rename", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this workflow test.")
  }
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(name = "Dataset A")
    )))
    session$flushReact()
    expect_identical(output$configure_dataset_name, "Dataset A")

    ## A rename does not rebuild the workbench; the title follows it.
    entries <- isolate(sets())
    entries[[1L]]$settings$name <- "Renamed A"
    sets(entries)
    session$flushReact()
    expect_identical(output$configure_dataset_name, "Renamed A")
  })

  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(review, 'id = "configure_dataset_name"', fixed = TRUE)
  expect_false(grepl(
    'paste("Configure", entry$settings$name %||% entry$id)',
    review,
    fixed = TRUE
  ))
})

test_that("the Configure footer counts datasets that are still loading", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  expect_identical(
    app_env$builder_configure_check_message(1L, 2L),
    "1 of 2 datasets checked · 1 remaining"
  )
  expect_identical(
    app_env$builder_configure_check_message(0L, 3L, 1L),
    "0 of 4 datasets checked · 1 still loading"
  )
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    review,
    "length(imports()$entries %||% list())",
    fixed = TRUE
  )
})
