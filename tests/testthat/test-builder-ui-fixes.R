## Builder UI regressions found while walking the real-data workflow.

builder_ui_fixes_app <- function(scope = parent.frame()) {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"), .local_envir = scope)
  sys.source("app.R", envir = app_env)
  app_env
}

test_that("a Project cache job attaches CerebroNexus when no source is sourced", {
  skip_if_not_installed("callr")
  skip_if_not(
    "CerebroNexus" %in% rownames(utils::installed.packages()),
    "CerebroNexus is not installed for a background R process"
  )
  app_env <- builder_ui_fixes_app()
  expression <- app_env$builder_project_crb_job_expression()
  runtime <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "builder_execute_plan <- function(plan, output, snapshots) {",
      "  list(found = exists('exportFromSeurat', mode = 'function'))",
      "}",
      "builder_project_prepare_build_artifacts <- function(value, ...) value"
    ),
    runtime
  )
  run <- function(load_package) {
    callr::r(
      function(expression, args) {
        list2env(args, envir = environment())
        eval(expression, envir = environment())
      },
      args = list(
        expression = expression,
        args = list(
          plan = list(source_snapshot_identities = list(), items = list()),
          output = tempdir(),
          root = tempdir(),
          previous_artifacts = list(),
          entry = list(id = "ds1"),
          runtime_files = runtime,
          load_package = load_package,
          source_utf8 = function(path, envir) sys.source(path, envir = envir)
        )
      ),
      libpath = .libPaths()
    )
  }
  expect_true(run(TRUE)$found)
  expect_false(run(FALSE)$found)
})

test_that("a Build that needs a decision names the failed analysis and why", {
  app_env <- builder_ui_fixes_app()
  log <- c(
    paste("pbmc3k:", intToUtf8(0x2713L), "Most expressed genes (0.4 s)"),
    paste0(
      "pbmc3k: ",
      intToUtf8(0x2717L),
      " Mitochondrial / ribosomal percentages: Layer `counts` ",
      "could not be found in `RNA` assay.\nAvailable layers: data"
    )
  )
  result <- app_env$builder_result_needs_decision(
    NULL,
    failures = "pbmc3k: analysis `percent_mt_ribo` failed.",
    analysis_log = log,
    failed_dataset_id = "ds1",
    retry_closure = "percent_mt_ribo"
  )
  model <- app_env$builder_build_status_model(result)
  expect_identical(
    model$failure_reasons,
    paste0(
      "pbmc3k: Mitochondrial / ribosomal percentages: Layer `counts` ",
      "could not be found in `RNA` assay."
    )
  )
  html <- as.character(app_env$builder_build_status_ui(model))
  expect_match(html, "Layer `counts` could not be found", fixed = TRUE)
  expect_match(html, "Analysis log", fixed = TRUE)
  expect_match(html, "Remove and review", fixed = TRUE)

  expect_match(
    app_env$builder_project_build_failure_text(list(analysis_log = log)),
    "stopped on an optional analysis: pbmc3k: Mitochondrial",
    fixed = TRUE
  )
  expect_identical(
    app_env$builder_project_build_failure_text(list(
      failures = "pbmc3k: analysis `most_expressed` failed."
    )),
    "pbmc3k: analysis `most_expressed` failed."
  )
  expect_identical(
    app_env$builder_project_build_failure_text(list()),
    "Dataset caches could not be prepared."
  )
})

test_that("raw-count analyses are blocked on an assay without counts", {
  app_env <- builder_ui_fixes_app()
  profile <- list(
    default_assay = "RNA",
    assay_profiles = list(
      RNA = list(layers = "data"),
      SCT = list(layers = c("counts", "data"))
    ),
    extras = list()
  )
  steps <- app_env$builder_analysis_steps()
  names(steps) <- vapply(steps, `[[`, character(1), "id")
  data_only <- app_env$builder_enhance_analysis_profile(profile, "hg", "RNA")
  expect_identical(data_only$analysis_layers, "data")
  expect_match(
    app_env$builder_step_blocked(steps$percent_mt_ribo, data_only, character()),
    "Needs a counts layer in the RNA assay",
    fixed = TRUE
  )
  expect_match(
    app_env$builder_step_blocked(steps$most_expressed, data_only, character()),
    "counts",
    fixed = TRUE
  )
  expect_null(
    app_env$builder_step_blocked(steps$marker_genes, data_only, character())
  )
  with_counts <- app_env$builder_enhance_analysis_profile(profile, "hg", "SCT")
  expect_null(
    app_env$builder_step_blocked(
      steps$percent_mt_ribo,
      with_counts,
      character()
    )
  )
  ## The default assay is used when none is chosen yet.
  expect_identical(
    app_env$builder_enhance_analysis_profile(profile, "hg")$analysis_assay,
    "RNA"
  )
})

test_that("Reset image returns a source-registered picture to its registration", {
  app_env <- builder_ui_fixes_app()
  source_bounds <- list(xmin = 0, xmax = 2000, ymin = 0, ymax = 2000)
  record <- app_env$builder_alignment_record(
    source = list(name = "hires.png", type = "image/png"),
    base_bounds = list(xmin = 100, xmax = 900, ymin = 100, ymax = 900),
    parameters = modifyList(
      app_env$builder_alignment_defaults(),
      list(scale = 1.4, dx = 30, flip_y = FALSE, flip_x = TRUE)
    ),
    section = list(id = "s1", kind = "spatial"),
    source_path = "hires.png"
  )
  record$source_registration <- list(
    base_bounds = source_bounds,
    flip_x = FALSE,
    flip_y = TRUE
  )
  reset <- app_env$builder_alignment_reset(record)
  expect_identical(reset$base_bounds, source_bounds)
  expect_true(reset$flip_y)
  expect_false(reset$flip_x)
  expect_identical(reset$scale, 1)
  expect_identical(reset$dx, 0)
  expect_identical(reset$source_registration$base_bounds, source_bounds)

  record$source_registration <- NULL
  plain <- app_env$builder_alignment_reset(record)
  expect_identical(plain$base_bounds, record$base_bounds)
  expect_false(plain$flip_y)
})

test_that("default groups prefer annotations over spatial grid indices", {
  app_env <- builder_ui_fixes_app()
  preselect <- app_env$builder_preselect_group
  expect_identical(preselect(c("array_row", "clusters")), "clusters")
  expect_identical(preselect(c("array_row", "array_col", "region")), "region")
  expect_identical(preselect(c("Cell_class", "batch")), "Cell_class")
  expect_identical(
    preselect(c("sample", "seurat_clusters")),
    c("seurat_clusters", "sample")
  )
  expect_identical(preselect("array_row"), "array_row")
  expect_identical(preselect(character()), character())
})

test_that("a dataset added twice gets a distinct name", {
  app_env <- builder_ui_fixes_app()
  unique_name <- app_env$builder_unique_dataset_name
  expect_identical(unique_name("visium", character()), "visium")
  expect_identical(unique_name("visium", c("pbmc", "visium")), "visium (2)")
  expect_identical(
    unique_name("visium", c("visium", "visium (2)")),
    "visium (3)"
  )
})

test_that("Review shows group columns by their real names", {
  app_env <- builder_ui_fixes_app()
  expect_identical(
    app_env$builder_review_group_label("Neuron_cluster_ID"),
    "Neuron_cluster_ID"
  )
  expect_identical(app_env$builder_review_group_label("louvain"), "louvain")
  expect_identical(app_env$builder_review_group_label(""), "Not selected")
})

test_that("Viewer appearance flags share a row and page cards have titles", {
  app_env <- builder_ui_fixes_app()
  html <- as.character(app_env$builder_viewer_display_ui(
    "core",
    list(id = "ds1", settings = list()),
    1L
  ))
  expect_match(html, "builder-display-flags", fixed = TRUE)
  expect_match(
    html,
    '<summary>\\s*<span class="builder-viewer-card-title">Projection</span>'
  )
})
