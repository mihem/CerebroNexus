builder_data_page_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  # Format labels and the deprecation note come from the format table.
  sys.source(builder_profile_inst_path("builder", "io.R"), envir = runtime)
  runtime$builder_example_directory <- function() list()
  runtime$builder_example_buttons_ui <- function(...) {
    shiny::tags$button(
      class = "builder-data-source example-btn",
      shiny::tags$span(
        class = "builder-data-source-copy",
        "Complete Viewer data"
      ),
      shiny::tags$span(
        class = "builder-data-source-tag is-example",
        "Load Example"
      )
    )
  }
  runtime$builder_stage_footer_ui <- function(status, ...) {
    shiny::tags$footer(
      class = "builder-stage-footer",
      shiny::tags$p(class = "builder-stage-footer-status", status),
      ...
    )
  }
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  runtime
}

builder_data_page_html <- function() {
  runtime <- builder_data_page_runtime()
  htmltools::renderTags(runtime$builder_empty_workbench_ui(
    formats = c("rds", "qs", "qs2"),
    examples = list()
  ))$html
}

test_that("Data page makes the whole dropzone the upload action", {
  withr::local_package("shiny")
  html <- builder_data_page_html()

  expect_match(html, "builder-dataset-dropzone-copy", fixed = TRUE)
  expect_match(html, "builder-add-datasets", fixed = TRUE)
  expect_match(html, '<button', fixed = TRUE)
  expect_match(html, 'type="button"', fixed = TRUE)
  expect_false(grepl('role="button"', html, fixed = TRUE))
  expect_identical(
    lengths(regmatches(html, gregexpr("Add your data", html, fixed = TRUE))),
    1L
  )
  expect_false(grepl("builder-dataset-dropzone-actions", html, fixed = TRUE))
  expect_false(grepl(">Add files<", html, fixed = TRUE))
  expect_false(grepl("builder-browse-dataset-files", html, fixed = TRUE))
  expect_false(grepl("builder-local-dataset-files", html, fixed = TRUE))
  expect_match(html, "Load Example", fixed = TRUE)
  expect_false(grepl("Start with one dataset", html, fixed = TRUE))
  expect_false(grepl("builder-stage-footer", html, fixed = TRUE))
})

test_that("Data page names readers disabled by missing packages", {
  runtime <- builder_data_page_runtime()
  formats <- list(
    list(id = "rds", label = "RDS", available = TRUE),
    list(
      id = "qs",
      label = "qs",
      available = FALSE,
      unavailable_reason = paste(
        "QS unavailable. Use RDS or QS2."
      ),
      unavailable_detail = "The R package 'qs' could not be loaded."
    )
  )
  html <- htmltools::renderTags(shiny::tagList(
    runtime$builder_empty_workbench_ui(formats = formats, examples = list()),
    runtime$builder_format_unavailable_ui(formats)
  ))$html

  expect_match(html, "builder-format-unavailable", fixed = TRUE)
  expect_match(html, "QS unavailable", fixed = TRUE)
  expect_match(html, "<details>", fixed = TRUE)
  expect_match(html, ">RDS<", fixed = TRUE)
  expect_false(grepl(">RDS, QS<", html, fixed = TRUE))
})

test_that("Data page stays usable while imports run", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(runtime$builder_empty_workbench_ui(
    dataset_count = 1L,
    formats = c("rds", "qs2"),
    examples = list(),
    loading_count = 2L
  ))$html
  idle <- htmltools::renderTags(runtime$builder_empty_workbench_ui(
    formats = c("rds", "qs2"),
    examples = list()
  ))$html

  expect_match(html, "builder-dataset-dropzone", fixed = TRUE)
  expect_match(html, "Configure Datasets", fixed = TRUE)
  expect_match(html, "builder-import-loading-note", fixed = TRUE)
  expect_match(
    html,
    "Loading in the background. Track each dataset in the dataset list.",
    fixed = TRUE
  )
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_false(grepl('aria-busy="true"', html, fixed = TRUE))
  expect_false(grepl("Preparing your workspace", html, fixed = TRUE))
  expect_false(grepl("builder-loading-skeleton", html, fixed = TRUE))
  expect_false(grepl("builder-import-loading-note", idle, fixed = TRUE))
  expect_false(exists("builder_importing_workbench_ui", envir = runtime))
})

test_that("opening a dataset from the Data step goes to Configure", {
  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  pick <- regmatches(
    js,
    regexpr(
      'var pick = target.closest\\(".builder-pick"\\);[\\s\\S]*?return;',
      js,
      perl = TRUE
    )
  )

  expect_length(pick, 1L)
  expect_match(
    pick,
    "#workbench [data-workflow-stage=\"upload\"]",
    fixed = TRUE
  )
  expect_match(pick, 'activityCapability("navigate_workflow")', fixed = TRUE)
  expect_match(pick, 'send("workflow_stage_configure"', fixed = TRUE)
})

test_that("Data page replaces loading copy when every import failed", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(
    runtime$builder_failed_import_workbench_ui(2L)
  )$html

  expect_match(html, "builder-import-failed-state", fixed = TRUE)
  expect_match(html, "Datasets could not be loaded", fixed = TRUE)
  expect_match(html, "Read the error in the dataset list", fixed = TRUE)
  expect_match(html, 'role="alert"', fixed = TRUE)
  expect_false(grepl("Preparing your workspace", html, fixed = TRUE))
  expect_false(grepl('aria-busy="true"', html, fixed = TRUE))
})

test_that("Dataset rail presents one concise readiness status", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  model <- list(
    index = 1L,
    id = "dataset-a",
    label = "Dataset A",
    cells = 180L,
    format = "Built-in example",
    import_elapsed_ms = 5500,
    readiness_label = "Blocked",
    checked = FALSE,
    selected = TRUE,
    confirm = FALSE,
    can_up = FALSE,
    can_down = TRUE
  )
  html <- htmltools::renderTags(
    runtime$builder_dataset_rail_row_ui(model)
  )$html

  expect_false(grepl("rail-readiness-status", html, fixed = TRUE))
  expect_false(grepl(">Blocked<", html, fixed = TRUE))
  expect_identical(
    lengths(regmatches(html, gregexpr("Needs checking", html, fixed = TRUE))),
    1L
  )
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'aria-atomic="true"', fixed = TRUE)
  ## A loaded row drops the load-time badge; the full line and the time move
  ## to the tooltip, and the built-in format reads "Example".
  expect_false(grepl("builder-load-time", html, fixed = TRUE))
  expect_match(html, ">180 cells · Example<", fixed = TRUE)
  expect_match(
    html,
    'title="180 cells · Built-in example · loaded in 5.5s"',
    fixed = TRUE
  )
})

test_that("Dataset rail distinguishes cached datasets from loaded sources", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  model <- list(
    index = 1L,
    id = "dataset-a",
    label = "Dataset A",
    cells = 180L,
    format = "qs",
    import_elapsed_ms = NA_real_,
    readiness_label = "CRB ready",
    checked = TRUE,
    selected = TRUE,
    confirm = FALSE,
    can_up = FALSE,
    can_down = FALSE
  )

  crb_html <- htmltools::renderTags(
    runtime$builder_dataset_rail_row_ui(model)
  )$html
  model$readiness_label <- "Ready"
  source_html <- htmltools::renderTags(
    runtime$builder_dataset_rail_row_ui(model)
  )$html

  expect_match(crb_html, ">Checked<", fixed = TRUE)
  expect_match(crb_html, "Cached · opens fast", fixed = TRUE)
  ## The readiness key stays internal; the visible text says "cache".
  expect_false(grepl("CRB", gsub("<[^>]*>", "", crb_html), fixed = TRUE))
  expect_match(source_html, ">Checked<", fixed = TRUE)
  expect_match(source_html, "Not cached", fixed = TRUE)
  expect_false(grepl("CRB", gsub("<[^>]*>", "", source_html), fixed = TRUE))
})

test_that("Data page shows its footer only after a dataset exists", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(runtime$builder_empty_workbench_ui(
    dataset_count = 1L,
    formats = c("rds", "qs", "qs2"),
    examples = list()
  ))$html

  expect_match(html, "builder-stage-footer", fixed = TRUE)
  expect_match(html, ">1 dataset<", fixed = TRUE)
  expect_match(html, "Configure Datasets", fixed = TRUE)
})

test_that("Data page upload action prefers native files with a browser fallback", {
  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "addDatasetFiles(addDatasets)", fixed = TRUE)
  expect_match(js, "function addDatasetFiles", fixed = TRUE)
  expect_match(js, 'send("builder_choose_dataset_files"', fixed = TRUE)
  expect_match(js, '"builder_dataset_picker_fallback"', fixed = TRUE)
  expect_match(js, "function openDatasetPicker(message)", fixed = TRUE)
  expect_false(grepl("choose_local_datasets", js, fixed = TRUE))
  expect_false(grepl("startFilePickerRecovery", js, fixed = TRUE))
  expect_false(grepl("datasetFileSource", js, fixed = TRUE))
  expect_false(grepl("tableFileSource", js, fixed = TRUE))
  expect_false(grepl(
    '".builder-dataset-dropzone.builder-add-datasets"',
    js,
    fixed = TRUE
  ))
  expect_false(grepl(
    "addDatasetFiles(datasetDropzone)",
    js,
    fixed = TRUE
  ))
  expect_false(grepl("browseDatasets", js, fixed = TRUE))
  expect_false(grepl("localDatasets", js, fixed = TRUE))
})

test_that("Data page updates do not replay full-page motion", {
  components <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  layout <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.layout.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(components, "#workbench.recalculating {", fixed = TRUE)
  expect_match(
    components,
    ".builder-stage-upload {\n  animation: none;",
    fixed = TRUE
  )
  expect_match(
    components,
    ".builder-notice--bar {",
    fixed = TRUE
  )
  expect_match(
    layout,
    ".builder-shell:not(:has(.rail .ds)) {\n  min-height: 0;",
    fixed = TRUE
  )
  expect_match(layout, "max-height: 0;", fixed = TRUE)
  expect_match(layout, "transition: min-height", fixed = TRUE)
})

test_that("the real example action uses the approved secondary label", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(app, '"Load Example"', fixed = TRUE)
})

test_that("dataset and table uploads do not have server-side file pickers", {
  imports <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "imports.R"),
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

  expect_false(grepl("choose_local_datasets", imports, fixed = TRUE))
  expect_false(grepl("builder_cancel_file_picker", imports, fixed = TRUE))
  expect_false(grepl("enhance-choose_local_tables", enhancements, fixed = TRUE))
  expect_false(grepl("native_table_picker_active", enhancements, fixed = TRUE))
})

test_that("Precision Scientific is the final Builder presentation layer", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  theme_path <- builder_profile_inst_path(
    "builder",
    "www",
    "builder.editorial.css"
  )

  expect_match(
    app,
    '"builder.features.css",\n  "builder.editorial.css"',
    fixed = TRUE
  )
  expect_true(file.exists(theme_path))

  theme <- paste(readLines(theme_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("--builder-font-serif:", theme, fixed = TRUE))
  expect_match(
    theme,
    "font-family: var(--builder-font-sans)",
    fixed = TRUE
  )
  expect_match(theme, ".builder-stage-header h2", fixed = TRUE)
  expect_match(theme, "--c-text-3: #68706b", fixed = TRUE)
  expect_match(theme, "touch-action: manipulation", fixed = TRUE)
  expect_match(theme, "-webkit-tap-highlight-color", fixed = TRUE)
  expect_match(theme, ".builder-import-loading-note", fixed = TRUE)
  expect_false(grepl(".builder-loading-skeleton", theme, fixed = TRUE))
  expect_match(
    theme,
    paste(
      ".builder-detected-content .builder-content-tag {",
      "  padding: .15rem var(--space-2);",
      "  border: 1px solid var(--c-border);",
      "  border-radius: var(--r-pill);",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_match(theme, ".builder-data-source:focus-visible", fixed = TRUE)
  layout <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.layout.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(layout, "min-height: calc(100dvh - 12rem)", fixed = TRUE)
  expect_match(theme, ".ds.is-active::before", fixed = TRUE)
  expect_match(
    theme,
    ".builder-configure-metadata-chips > span",
    fixed = TRUE
  )
  expect_match(theme, ".builder-viewer-card[open]", fixed = TRUE)
  expect_match(
    theme,
    paste0(
      ".ds[data-load-state=\"ready\"] .ds-actions {\n",
      "  display: grid;\n",
      "  grid-template-columns: auto auto minmax(0, 1fr);"
    ),
    fixed = TRUE
  )
  expect_match(
    theme,
    paste0(
      ".ds[data-load-state=\"ready\"] .ds-actions > button {\n",
      "  min-width: 0;"
    ),
    fixed = TRUE
  )
  expect_match(theme, "@media (max-width: 58rem)", fixed = TRUE)

  stage <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "core_stage", "stage.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(stage, 'h3("Essentials")', fixed = TRUE)
  expect_match(stage, 'h4("Viewer content")', fixed = TRUE)
  expect_match(stage, 'h4("Views")', fixed = TRUE)
  expect_false(grepl('h4("CRB content")', stage, fixed = TRUE))
})

test_that("Add files hints list RDS and QS2 and warn that QS is deprecated", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- builder_data_page_html()

  expect_match(html, ">RDS, QS2<", fixed = TRUE)
  expect_false(grepl("QS2, QS", html, fixed = TRUE))
  expect_match(
    html,
    "QS files are deprecated and support will end soon. Save as RDS or QS2.",
    fixed = TRUE
  )

  formats <- list(
    list(id = "rds", label = "RDS", available = TRUE),
    list(id = "qs2", label = "qs2", available = TRUE),
    list(id = "qs", label = "qs", available = TRUE, deprecated = TRUE)
  )
  expect_identical(
    runtime$builder_advertised_format_labels(formats),
    c("RDS", "QS2")
  )
  note <- htmltools::renderTags(
    runtime$builder_deprecated_format_note_ui(formats)
  )$html
  expect_match(note, "builder-format-deprecated", fixed = TRUE)

  # No note when the deprecated reader is unavailable or absent.
  formats[[3L]]$available <- FALSE
  expect_null(runtime$builder_deprecated_format_note_ui(formats))
  expect_null(runtime$builder_deprecated_format_note_ui(formats[1:2]))
  expect_true(isTRUE(
    runtime$builder_formats[[3L]]$deprecated
  ))
})
