builder_stage_contract_source_runtime(environment())
builder_profile_source_runtime(environment())
builder_plan_contract_source_runtime(environment())

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

test_that("Configure footer renders the primary action for each check state", {
  skip_if_not_installed("shiny")
  ui_env <- new.env(parent = asNamespace("shiny"))
  builder_repo_source(file.path("ui", "workflow.R"), local = ui_env)

  actions_html <- htmltools::renderTags(ui_env$builder_configure_actions_ui(
    "Wait for all datasets to finish loading.",
    can_continue = FALSE
  ))$html
  expect_match(
    actions_html,
    "Wait for all datasets to finish loading.",
    fixed = TRUE
  )
  expect_identical(
    lengths(regmatches(
      actions_html,
      gregexpr('id="continue_to_review"', actions_html, fixed = TRUE)
    )),
    1L
  )
  expect_match(actions_html, " disabled", fixed = TRUE)
  warning_html <- htmltools::renderTags(ui_env$builder_configure_actions_ui(
    "Every dataset needs a name.",
    can_continue = FALSE,
    warning = TRUE
  ))$html
  expect_match(
    warning_html,
    'class="builder-stage-footer-status is-warning"',
    fixed = TRUE
  )

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
    html <- htmltools::renderTags(ui_env$builder_configure_primary_action_ui(
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
    paste0(
      '<p id="build_output_crb_reason" class="hint builder-app-required-reason">',
      "Spatial images or ROI settings require CRB files + Viewer App output."
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    paste0(
      '<input type="radio" name="build_output_mode" value="crb" ',
      'disabled="disabled" aria-disabled="true" ',
      'aria-describedby="build_output_crb_reason"/>'
    ),
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
    # A dangling symbolic link takes its name; it is skipped, not followed
    # or removed.
    dangling <- file.path(release_parent, "CerebroNexus_viewer_6a7b8c")
    if (isTRUE(file.symlink(file.path(release_parent, "gone"), dangling))) {
      identifiers <- list(
        as.raw(c(0x6a, 0x7b, 0x8c)),
        as.raw(c(0x9d, 0x0e, 0x1f))
      )
      linked_target <- builder_new_release_target(
        release_parent,
        make_app = TRUE,
        .random_bytes = function(size) {
          value <- identifiers[[1L]]
          identifiers <<- identifiers[-1L]
          value
        }
      )
      expect_identical(
        basename(linked_target),
        "CerebroNexus_viewer_9d0e1f"
      )
      expect_true(nzchar(Sys.readlink(dangling)))
      expect_false(file.exists(file.path(release_parent, "gone")))
    }
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

test_that("a dataset name another dataset uses blocks the check", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    focus <- list()
    notices <- list()
    original_send <- session$sendCustomMessage
    session$sendCustomMessage <- function(type, message) {
      if (identical(type, "builder_focus_incomplete_setting")) {
        focus[[length(focus) + 1L]] <<- message$blocker
      }
      if (identical(type, "builder_notify") && is.null(message$clear)) {
        notices[[length(notices) + 1L]] <<- message$html
      }
      original_send(type, message)
    }
    entry <- function(id, name) {
      list(
        id = id,
        revision = 0L,
        snapshot = list(
          path = paste0("/private/", id),
          owner_token = paste0("owner-", id),
          object_md5 = strrep("a", 32L)
        ),
        profile = list(marker = id, extras = list()),
        settings = list(name = name, organism = "hg")
      )
    }
    use_state_only_fixture(list(
      entry("dataset-a", "PBMC"),
      entry("dataset-b", "pbmc ")
    ))
    session$flushReact()

    expect_match(configure_readiness()$message, "share a name")
    expect_false(configure_readiness()$can_continue)
    ## The footer beside Finish Checking says it too, as a warning.
    expect_match(configure_actions_state()$message, "share a name")
    expect_true(configure_actions_state()$warning)
    footer <- as.character(output$configure_actions$html)
    expect_match(footer, "is-warning", fixed = TRUE)
    expect_match(review_report()$msg, "share a name")

    session$setInputs(complete_dataset_check = 1)
    session$flushReact()
    expect_length(dataset_check_marks(), 0L)
    expect_identical(focus, list("settings_name"))
    expect_match(notices[[1L]], "PBMC", fixed = TRUE)
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
        nonce = 2
      )
    )
    session$flushReact()
    expect_true(auth_enabled())
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_identical(auth_accounts()[[1L]]$username, "user-a")

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

    capability$available <- FALSE
    build_mode(TRUE)
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

    frozen_flow <- list(stage = "building", plan = NULL)
    build_flow(frozen_flow)
    real_session$setInputs(back_to_review = 1L)
    real_session$setInputs(build = 1L)
    real_session$flushReact()

    expect_identical(workflow()$stage, "build")
    expect_identical(build_flow(), frozen_flow)
    expect_identical(selected_output(), "/confirmed/output")
    expect_identical(picker_calls, 0L)
    expect_true(any(grepl(
      "Wait for the active build to finish",
      notifications,
      fixed = TRUE
    )))

    build_flow(list(stage = "idle", plan = NULL))
    selected_output(NULL)
    real_session$setInputs(build = 2L)
    real_session$flushReact()
    expect_identical(picker_calls, 1L)
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
    real_session$setInputs(back_to_review = 2L)
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

    build_flow(list(stage = "building", plan = NULL))
    active_protocol <- isolate(protocol())
    active_protocol$build_status <- "running"
    protocol(active_protocol)
    before <- isolate(store())
    before_imports <- isolate(imports())
    before_protocol <- isolate(protocol())

    expect_false(start_load("example", "locked-example", "Locked example"))
    real_session$setInputs(
      reorder_ds = list(id = "dataset-a", direction = "down"),
      drop_ds = list(id = "dataset-a", confirmed = TRUE),
      undo_remove = 1L,
      use_example = "spatial",
      dataset_files = data.frame(
        name = "forged.rds",
        datapath = tempfile(fileext = ".rds"),
        size = 1,
        type = "application/octet-stream"
      )
    )
    real_session$flushReact()

    expect_identical(store(), before)
    expect_identical(imports(), before_imports)
    expect_identical(protocol(), before_protocol)
    expect_true(any(grepl(
      "active build",
      tolower(notifications),
      fixed = TRUE
    )))

    build_flow(list(stage = "idle", plan = NULL))
    protocol(app_env$builder_request_protocol("worker-lock"))
    expect_true(start_load("example", "idle-example", "Idle example"))
    expect_length(imports()$entries, 1L)
    real_session$setInputs(undo_remove = 2L)
    real_session$flushReact()
    expect_false(store()$can_undo_remove)
    expect_length(store()$datasets, 2L)
  })
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
      function(...) TRUE,
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
    set_case <- function(value, stale = FALSE) {
      entry <- list(
        id = "dataset-a",
        revision = 0L,
        snapshot = list(
          path = "/private/dataset-a",
          owner_token = "owner-a",
          object_md5 = strrep("a", 32L)
        ),
        profile = list(marker = "a", extras = list()),
        settings = list(name = "Dataset A", analyses = "marker_genes")
      )
      use_state_only_fixture(list(entry))
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
    expect_identical(
      as.character(output$configure_dataset_name$html),
      "Dataset A"
    )

    ## A rename does not rebuild the workbench; the title follows it.
    entries <- isolate(sets())
    entries[[1L]]$settings$name <- "Renamed A"
    sets(entries)
    session$flushReact()
    expect_identical(
      as.character(output$configure_dataset_name$html),
      "Renamed A"
    )
  })
})

test_that("a stale back_to_settings without a reviewed configuration is ignored", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
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
    session$setInputs(make_app = FALSE)
    session$flushReact()
    stage <- workflow()$stage
    expect_null(workflow()$review_snapshot)
    session$setInputs(back_to_settings = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, stage)
    expect_false(session$isClosed())

    ## From Review the button still goes back to Configure.
    dataset_check_marks(builder_stage_checked_marks(
      isolate(sets()),
      app_env$builder_project_check_identity,
      builder_configuration_identity_cache
    ))
    workflow(app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "open_review", snapshot = current_review_snapshot())
    ))
    session$setInputs(back_to_settings = 2L)
    session$flushReact()
    expect_identical(workflow()$stage, "configure")
  })
})

test_that("the workbook reading notice is cleared when reading ends", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
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
    shown <- character()
    cleared <- character()
    fn_env <- environment(schedule_enhance_table_files)
    assign(
      "builder_notify",
      function(type, message, target = NULL, id = NULL, ...) {
        if (startsWith(as.character(message), "Reading workbook sheets")) {
          shown <<- c(shown, id %||% "<none>")
        }
        invisible(TRUE)
      },
      envir = fn_env
    )
    assign(
      "builder_notify_clear",
      function(id, ...) {
        cleared <<- c(cleared, as.character(id))
        invisible(TRUE)
      },
      envir = fn_env
    )
    assign(
      "start_enhance_table_inventory",
      function(...) stop("No worker in this test."),
      envir = fn_env
    )
    schedule_enhance_table_files(
      data.frame(name = "a.xlsx", datapath = "/x/a.xlsx"),
      id = "dataset-a"
    )
    expect_length(shown, 1L)
    expect_true(nzchar(shown))
    expect_identical(cleared, shown)
  })
})

test_that("a dataset without any eligible Group points the check at Groups", {
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
      profile = list(
        marker = "dataset-a",
        extras = list(),
        group_candidates = character()
      ),
      settings = list(name = "Dataset A", organism = NULL)
    )))
    session$flushReact()
    ## What a loaded dataset without groups reports (a state-only fixture
    ## skips the settings checks).
    assign(
      "builder_cached_dataset_state",
      function(...) {
        list(
          blocking_ids = c("groups", "settings_groups", "settings_organism"),
          attention_ids = character(),
          checking_ids = character()
        )
      },
      envir = environment(open_next_unchecked_dataset)
    )

    session$setInputs(complete_dataset_check = 1)
    session$flushReact()

    expect_length(messages, 1L)
    expect_identical(messages[[1L]]$blocker, "settings_groups")
  })
})
