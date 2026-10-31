## Project dialogs and the save/open picker flow.

builder_dialogs_app <- function(env = parent.frame()) {
  withr::local_options(
    CerebroNexus.builder.project_worker_prewarm = FALSE,
    .local_envir = env
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"), .local_envir = env)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this dialog test.")
  }
  app_env
}

test_that("project pickers close the open dialog before the picker overlay", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  events <- new.env(parent = emptyenv())
  events$log <- character()
  testthat::local_mocked_bindings(
    removeModal = function(...) {
      events$log <- c(events$log, "remove")
      invisible()
    },
    .package = "shiny"
  )

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_folder)
    server_env$builder_operation_allowed <- function(...) TRUE
    server_env$builder_project_open_needs_confirmation <- function() FALSE
    server_env$builder_schedule_native_picker <- function(kind, ...) {
      events$log <- c(events$log, paste0("picker:", kind))
      expect_identical(isolate(builder_project_operation_phase()), "choosing")
      invisible(TRUE)
    }

    events$log <- character()
    request_builder_project_folder()
    expect_identical(events$log, c("remove", "picker:project_directory"))

    builder_project_operation_phase("idle")
    events$log <- character()
    session$setInputs(open_builder_project = 1)
    expect_identical(events$log, c("remove", "picker:project_manifest"))
  })
})

test_that("server path dialogs keep typed paths on a backdrop click", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  html <- htmltools::renderTags(app_env$builder_server_path_dialog(
    title = "Choose project folder",
    input_id = "builder_project_server_folder",
    action_id = "use_builder_project_server_folder",
    label = "Project folder",
    action_label = "Continue"
  ))$html
  expect_match(html, 'data-backdrop="static"', fixed = TRUE)
  expect_match(html, 'data-keyboard="false"', fixed = TRUE)
})

test_that("server path dialogs and errors name the allowed folders", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  root <- withr::local_tempdir()
  allowed <- file.path(root, "allowed")
  outside <- file.path(root, "outside")
  dir.create(allowed)
  dir.create(outside)
  label <- app_env$builder_server_path_roots_label(
    app_env$builder_server_path_roots(allowed)
  )
  expect_match(label, "allowed", fixed = TRUE)

  error <- tryCatch(
    app_env$builder_server_path_resolve(outside, "directory", roots = allowed),
    error = conditionMessage
  )
  expect_identical(
    error,
    paste0(
      "The path is outside the allowed server folders. Allowed: ",
      label,
      "."
    )
  )

  html <- htmltools::renderTags(app_env$builder_server_path_dialog(
    title = "Choose output folder",
    input_id = "builder_build_server_folder",
    action_id = "use_builder_build_server_folder",
    label = "Parent folder",
    action_label = "Build here",
    roots = app_env$builder_server_path_roots(allowed)
  ))$html
  expect_match(html, paste0("Allowed folders: ", label), fixed = TRUE)
  expect_match(
    html,
    'aria-describedby="builder_build_server_folder_roots"',
    fixed = TRUE
  )
  ## The default roots are shown when none are passed.
  default_html <- htmltools::renderTags(app_env$builder_server_path_dialog(
    title = "Open project",
    input_id = "builder_project_server_manifest",
    action_id = "use_builder_project_server_manifest",
    label = "Project file",
    action_label = "Open project"
  ))$html
  expect_match(
    default_html,
    app_env$builder_server_path_roots_label(app_env$builder_server_path_roots()),
    fixed = TRUE
  )
})

test_that("the first Save opens the folder picker without an extra dialog", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  expect_false(exists("builder_project_first_save_dialog", envir = app_env))
  modals <- new.env(parent = emptyenv())
  modals$shown <- 0L
  testthat::local_mocked_bindings(
    showModal = function(...) {
      modals$shown <- modals$shown + 1L
      invisible()
    },
    .package = "shiny"
  )

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_folder)
    pickers <- new.env(parent = emptyenv())
    pickers$kinds <- character()
    server_env$builder_operation_allowed <- function(...) TRUE
    server_env$builder_schedule_native_picker <- function(kind, ...) {
      pickers$kinds <- c(pickers$kinds, kind)
      invisible(TRUE)
    }
    expect_null(isolate(builder_project()))

    session$setInputs(save_builder_project = 1)
    expect_identical(pickers$kinds, "project_directory")
    expect_identical(modals$shown, 0L)

    ## The picker overlay carries the line the removed dialog used to show.
    message <- builder_activity_message(
      app_env$builder_activity_state(project_phase = "choosing"),
      picker_kind = isolate(builder_project_picker_kind())
    )
    expect_identical(message$busy_title, "Choose project folder")
    expect_identical(
      message$busy_message,
      paste(
        "The folder picker is open.",
        app_env$builder_project_save_location_note()
      )
    )
    expect_null(message$busy_detail)
    expect_true(message$picker_waiting)

    open_message <- builder_activity_message(
      app_env$builder_activity_state(project_phase = "choosing"),
      picker_kind = "open"
    )
    expect_identical(open_message$busy_title, "Open project")
    expect_null(open_message$busy_detail)
  })
})

test_that("the manual project folder dialog says what the folder holds", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  html <- htmltools::renderTags(app_env$builder_server_path_dialog(
    title = "Choose project folder",
    input_id = "builder_project_server_folder",
    action_id = "use_builder_project_server_folder",
    label = "Project folder",
    action_label = "Continue",
    note = app_env$builder_project_save_location_note()
  ))$html
  expect_match(html, "Includes source files and settings.", fixed = TRUE)
  expect_match(html, "Keep this page open", fixed = TRUE)
  server <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    server,
    'note = if (identical(kind, "create")) builder_project_save_location_note()',
    fixed = TRUE
  )
})

test_that("the Open project dialog speaks of the dataset cache, not CRBs", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  record <- function(id, default, actions, crb_ready = FALSE) {
    list(
      id = id,
      name = id,
      runtime_restore_status = list(
        actions = actions,
        default_action = default,
        checked = TRUE,
        crb_ready = crb_ready,
        message = "Configuration ready",
        detail = NULL,
        problem = NULL
      )
    )
  }
  manifest <- list(
    project = list(name = "P"),
    datasets = list(
      record("ds1", "reuse", c("reuse", "resume", "reload", "skip"), TRUE),
      record("ds2", "resume", c("resume", "reload", "skip")),
      record("ds3", "resume", c("resume", "reload", "skip")),
      record("ds4", "skip", c("resume", "skip"))
    )
  )
  html <- htmltools::renderTags(
    app_env$builder_project_restore_dialog(manifest)
  )$html
  text <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", "", html))
  expect_match(text, "1 cached 2 from source 1 skipped", fixed = TRUE)
  expect_match(html, ">Open from cache (fast)</option>", fixed = TRUE)
  expect_match(html, ">Reload from source file</option>", fixed = TRUE)
  expect_match(html, 'data-tone="ready">Cached</span>', fixed = TRUE)
  expect_match(html, 'data-tone="none">Not cached</span>', fixed = TRUE)
  expect_false(grepl("Cache none", html, fixed = TRUE))
  expect_match(
    html,
    'title="Stored dataset cache size">Not cached</span>',
    fixed = TRUE
  )
  expect_false(grepl("CRB", html, fixed = TRUE))
  expect_match(html, 'data-tone="ready"', fixed = TRUE)

  ## Nothing opens from the cache: the summary is neutral, not a success.
  uncached <- manifest
  uncached$datasets <- uncached$datasets[2:3]
  uncached_html <- htmltools::renderTags(
    app_env$builder_project_restore_dialog(uncached)
  )$html
  expect_match(
    uncached_html,
    'class="builder-project-restore-notice" data-tone="neutral"',
    fixed = TRUE
  )
  expect_match(uncached_html, "fa-info", fixed = TRUE)

  notice <- app_env$builder_project_restore_notice(4L, 0L, 0L, 1L, 3L)
  expect_identical(
    notice,
    "1 dataset will open from cache; 3 datasets will load their original sources."
  )

  browser <- paste(
    readLines(
      builder_test_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl("saved CRB", browser, fixed = TRUE))
  expect_match(browser, '" will open from cache; "', fixed = TRUE)
  expect_match(
    browser,
    '(counts.reused > 0 ? "ready" : "neutral")',
    fixed = TRUE
  )
})
