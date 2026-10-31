test_that("author display settings validate and survive the project payload", {
  display <- builderViewerDisplayDefaults(list(
    label_size = 18,
    font_family = "Georgia",
    gene_palette = "viridis",
    group_labels = FALSE,
    color_clip = .05
  ))
  expect_equal(display$label_size, 18)
  expect_identical(display$gene_palette, "viridis")
  expect_error(builderViewerDisplayDefaults(list(label_size = 0)), "invalid")
  expect_error(
    builderViewerDisplayDefaults(list(font_family = "url(evil)")),
    "invalid"
  )
  expect_error(builderViewerDisplayDefaults(list(unknown = TRUE)), "named")
  restored <- jsonlite::unserializeJSON(jsonlite::serializeJSON(display))
  expect_identical(builderViewerDisplayDefaults(restored), display)
  browser_roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(
    display,
    auto_unbox = TRUE
  ))
  expect_identical(builderViewerDisplayDefaults(browser_roundtrip), display)
  expect_identical(
    viewerDisplayDefaults(
      list(viewer_content = list(ds = list(display = restored))),
      "ds"
    ),
    display
  )
})

test_that("spatial attachments are verified in the worker and reused on repeated saves", {
  runtime <- new.env(parent = environment())
  for (file in c("io.R", "extras.R", "project.R")) {
    sys.source(builder_test_inst_path("builder", file), runtime)
  }
  root <- withr::local_tempdir()
  source <- file.path(withr::local_tempdir(), "image.png")
  png::writePNG(array(.5, c(32, 32, 3)), source)
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        fov = list(
          image = list(
            source_path = source,
            point_size = 4,
            point_opacity = .5
          )
        )
      )
    )
  )
  jobs <- runtime$builder_project_attachment_jobs(list(entry), root)
  expect_length(jobs, 1)
  expect_identical(jobs[[1]]$kind, "spatial")
  results <- callr::r(
    function(jobs, root, files) {
      env <- new.env()
      for (file in files) {
        sys.source(file, env)
      }
      env$builder_project_stage_attachment_jobs(jobs, root)
    },
    args = list(
      jobs = jobs,
      root = root,
      files = vapply(
        c("io.R", "extras.R", "project.R"),
        function(f) normalizePath(builder_test_inst_path("builder", f)),
        character(1)
      )
    )
  )
  expect_true(runtime$builder_project_attachment_results_match(jobs, results))
  adopted <- runtime$builder_project_apply_attachment_results(
    list(entry),
    results,
    root
  )
  record <- adopted[[1]]$settings$images$fov$image
  expect_identical(record$point_size, 4)
  expect_identical(
    unname(tools::md5sum(record$source_path)),
    unname(tools::md5sum(source))
  )
  expect_length(runtime$builder_project_attachment_jobs(adopted, root), 0)
  before <- file.info(record$source_path)$mtime
  runtime$builder_project_stage_spatial_assets(adopted[[1]], root)
  expect_identical(file.info(record$source_path)$mtime, before)
  png::writePNG(array(.2, c(40, 40, 3)), source)
  expect_error(
    runtime$builder_project_stage_attachment_jobs(jobs, root),
    "changed"
  )
  expect_false(runtime$builder_project_attachment_results_match(
    jobs,
    c(results, results)
  ))
})


test_that("checkpoint planning failures retain the original error and release the job", {
  env <- new.env(parent = environment())
  expressions <- parse(builder_test_inst_path("builder", "server", "project.R"))
  for (expr in expressions) {
    if (
      is.call(expr) &&
        identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("start_next_builder_project_crb_job"))
    ) {
      eval(expr, env)
    }
  }
  root <- withr::local_tempdir()
  env$isolate <- shiny::isolate
  env$builder_project_crb_task <- function() NULL
  env$builder_project <- function() list(root = root)
  env$builder_project_crb_jobs <- function() {
    list(ds1 = list(status = "queued", token = "token", identity = "digest"))
  }
  env$entry_of <- function(id) list(id = id)
  env$dataset_check_marks <- function() list(ds1 = "digest")
  env$builder_configuration_identity_cache <- NULL
  env$builder_project_check_identity <- function(...) "digest"
  env$.builder_project_text <- function(x) {
    is.character(x) && length(x) == 1L && nzchar(x)
  }
  env$builder_project_checkpoint_budget <- function(...) list(ok = TRUE)
  pending_error <- shiny::reactiveVal("The source changed during planning.")
  env$freeze_plan_for_output <- function(...) stop(pending_error())
  env$builder_project_fail_crb_job <- function(
    id,
    token,
    error,
    output = NULL
  ) {
    list(id = id, token = token, message = conditionMessage(error))
  }
  result <- env$start_next_builder_project_crb_job()
  expect_identical(result$id, "ds1")
  expect_identical(result$token, "token")
  expect_identical(result$message, "The source changed during planning.")
})


test_that("runtime identity is independent of path aliases and duplicate paths", {
  env <- new.env(parent = environment())
  sys.source(builder_test_inst_path("builder", "coordinator.R"), env)
  root <- withr::local_tempdir()
  a <- file.path(root, "a.R")
  b <- file.path(root, "b.R")
  writeLines("a <- 1", a)
  writeLines("b <- 2", b)
  alias <- file.path(root, "z.R")
  if (!isTRUE(suppressWarnings(file.symlink(a, alias)))) {
    skip("Symlinks unavailable")
  }
  expected <- env$builder_runtime_file_identity(c(alias, b, a))
  actual <- env$builder_runtime_file_identity(c(a, b))
  expect_identical(expected, actual)
  expect_silent(env$builder_runtime_assert_identity(expected, actual))
  writeLines("a <- 2", a)
  expect_error(
    env$builder_runtime_assert_identity(
      expected,
      env$builder_runtime_file_identity(c(a, b))
    ),
    "changed"
  )
})


test_that("Builder initial-page labels resolve to the actual Viewer tabs", {
  catalog <- builder_viewer_page_catalog()
  pages <- rbind(catalog$always, catalog$conditional)
  viewer <- getFromNamespace(".viewerInitialPageTabs", "CerebroNexus")()
  expect_setequal(pages$id, names(viewer))
  expect_identical(unname(viewer[pages$id]), pages$tab_name)
  expect_identical(pages$id[pages$label == "Linked views"], "linked_views")
})

test_that("restoring sources leaves the structure browsable but locks publication", {
  env <- new.env(parent = environment())
  sys.source(builder_test_inst_path("builder", "project.R"), env)
  activity <- env$builder_activity_state(
    project_phase = "restoring",
    server_imports = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  caps <- env$builder_activity_capabilities(activity)
  expect_false(caps$page_inert)
  expect_true(caps$select_dataset)
  expect_false(caps$save_project)
  expect_false(caps$build)
  expect_false(caps$edit_dataset)
  expect_true(caps$warn_before_unload)
})

test_that("page appearance defaults retain page semantics through typed JSON", {
  display <- builderViewerDisplayDefaults(list(
    projection_group_labels = FALSE,
    gene_keep_square = TRUE,
    gene_show_trend_line = FALSE,
    gene_trend_line_width = 4,
    spatial_region_outlines = TRUE,
    trajectory_draw_border = FALSE,
    trajectory_density_opacity = .3,
    trajectory_show_trend_line = FALSE,
    trajectory_trend_line_width = 5,
    ir_scatter_point_size = 2.3,
    ir_scatter_opacity = .45,
    hla_legend_mode = "never",
    hla_node_scale = 1.7
  ))
  options <- jsonlite::unserializeJSON(jsonlite::serializeJSON(
    list(viewer_content = list(ds = list(display = display)))
  ))
  expect_false(
    viewerPageDisplayDefaults(options, "ds", "projection")$group_labels
  )
  expect_true(viewerPageDisplayDefaults(options, "ds", "gene")$keep_square)
  expect_false(viewerPageDisplayDefaults(options, "ds", "gene")$show_trend_line)
  expect_equal(
    viewerPageDisplayDefaults(options, "ds", "gene")$trend_line_width,
    4
  )
  expect_equal(
    viewerPageDisplayDefaults(options, "ds", "trajectory")$density_opacity,
    .3
  )
  expect_false(
    viewerPageDisplayDefaults(options, "ds", "trajectory")$show_trend_line
  )
  expect_equal(
    viewerPageDisplayDefaults(options, "ds", "trajectory")$trend_line_width,
    5
  )
  expect_true(
    viewerPageDisplayDefaults(options, "ds", "spatial")$region_outlines
  )
  expect_false(
    viewerPageDisplayDefaults(options, "ds", "trajectory")$draw_border
  )
  expect_equal(
    viewerPageDisplayDefaults(options, "ds", "ir")$scatter_point_size,
    2.3
  )
  expect_equal(viewerPageDisplayDefaults(options, "ds", "hla")$node_scale, 1.7)
  expect_false(
    viewerPageDisplayDefaults(list(), "legacy", "spatial")$group_labels
  )
  expect_true(
    viewerPageDisplayDefaults(list(), "legacy", "trajectory")$draw_border
  )
  expect_error(
    builderViewerDisplayDefaults(list(ir_scatter_point_size = 20)),
    "invalid"
  )
  expect_error(
    builderViewerDisplayDefaults(list(hla_legend_mode = "hidden")),
    "invalid"
  )
})

test_that("a child build observes a stop request and cleans its request marker", {
  skip_if_not_installed("callr")
  env <- new.env(parent = environment())
  sys.source(builder_test_inst_path("builder", "build.R"), env)
  path <- file.path(withr::local_tempdir(), "progress.rds")
  expect_true(env$builder_build_progress_write(path, "datasets"))
  expect_true(env$builder_build_request_cancel(path))
  message <- callr::r(
    function(file, path) {
      env <- new.env(parent = globalenv())
      sys.source(file, env)
      tryCatch(
        {
          env$.builder_build_progress_callback(path)("viewer")
          "not cancelled"
        },
        builder_build_cancelled = conditionMessage
      )
    },
    args = list(
      file = builder_test_inst_path("builder", "build.R"),
      path = path
    )
  )
  expect_match(message, "stopped by the user")
  expect_identical(env$builder_build_progress_read(path), "datasets")
  env$builder_build_progress_remove(path)
  expect_false(env$builder_build_cancel_requested(path))
})

test_that("an intentional build stop is distinct from a build failure", {
  env <- new.env(parent = environment())
  sys.source(builder_test_inst_path("builder", "ui", "inspect_stage.R"), env)
  sys.source(builder_test_inst_path("builder", "ui", "build_status.R"), env)
  stopped <- env$builder_build_status_model(env$builder_result_failure(
    "Build stopped by the user. The staged output was not published."
  ))
  failed <- env$builder_build_status_model(env$builder_result_failure(
    "Disk full."
  ))
  expect_true(env$builder_build_was_stopped(stopped))
  expect_identical(
    env$builder_build_stage_status_label(list(
      state = "result",
      result_model = stopped
    )),
    "Build stopped"
  )
  expect_identical(
    env$builder_build_stage_status_label(list(
      state = "result",
      result_model = failed
    )),
    "Build failed"
  )
})
