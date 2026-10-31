builder_build_source_runtime <- function(local = parent.frame()) {
  source_root <- testthat::test_path("..", "..", "R")
  for (file in sort(list.files(
    source_root,
    pattern = "[.][Rr]$",
    full.names = TRUE
  ))) {
    sys.source(file, envir = local)
  }
  for (file in c(
    "spatial.R",
    "profile.R",
    "inspect.R",
    "analysis.R",
    "extras.R",
    "state.R",
    "app_bundle.R",
    "build.R"
  )) {
    builder_repo_source(file, local = local)
  }
}

builder_build_source_runtime()

test_that("build preparation normalizes numeric-like QC metadata", {
  skip_if_not_installed("SeuratObject")

  counts <- Matrix::Matrix(
    matrix(
      seq_len(40),
      nrow = 5,
      dimnames = list(
        paste0("G", seq_len(5)),
        paste0("cell", seq_len(8))
      )
    ),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(counts)
  object$total_counts <- factor(as.character(seq_len(8) * 100))
  object$total_genes <- as.character(seq_len(8) + 100)

  prepared <- .builder_build_prepare_qc(
    object,
    list(nUMI = "total_counts", nGene = "total_genes")
  )

  expect_identical(unname(prepared$total_counts), seq_len(8) * 100)
  expect_identical(unname(prepared$total_genes), seq_len(8) + 100)

  object$total_genes[[1L]] <- "not-a-number"
  expect_error(
    .builder_build_prepare_qc(
      object,
      list(nUMI = "total_counts", nGene = "total_genes")
    ),
    "must contain only numeric values",
    fixed = TRUE
  )
})

test_that("build preparation keeps Trekker image bytes external", {
  object <- SeuratObject::pbmc_small
  barcodes <- colnames(object)[1:2]
  object@misc$trekker <- list(
    barcodes = barcodes,
    x = c(1, 2),
    y = c(3, 4),
    clusters = c(0L, 1L)
  )
  alignment <- builder_alignment_record(
    source = list(name = "trekker.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
    parameters = list(dx = 2, dy = -1, scale = 1.25, rotation = 90),
    section = list(id = "trekker", kind = "trekker"),
    source_path = "C:/private/trekker.png"
  )
  item <- list(
    default_group = "groups",
    colors = list(groups = c(`0` = "#000000", `1` = "#ffffff")),
    trekker_alignment = alignment
  )

  prepared <- .builder_build_prepare_trekker(object, item)
  trekker <- prepared@misc$trekker

  expect_identical(trekker$builder_group, "groups")
  expect_identical(trekker$builder_colors, item$colors$groups)
  expect_identical(
    trekker$builder_group_values,
    as.character(object@meta.data[barcodes, "groups", drop = TRUE])
  )
  expect_null(trekker$histology_image)
  expect_null(trekker$histology_image_bounds)
  expect_identical(
    trekker$histology_alignment,
    builder_alignment_payload(alignment)
  )
})

builder_build_test_plan <- function(analyses = character()) {
  item <- list(
    id = "dataset-a",
    name = "Dataset A",
    filename = "dataset-a.crb",
    analyses = analyses,
    analysis_dependency_graph = builder_analysis_graph(analyses),
    artifact_identity = list(
      schema_version = 2L,
      cells = c("cell-b", "cell-a"),
      features = c("Gene2", "Gene1"),
      group_levels = list(cluster = c("B", "A")),
      projections = "umap",
      metadata = c("cell_barcode", "cluster"),
      spatial_sections = character()
    ),
    expression_backend = "embedded",
    sidecars = character(),
    colors = list(cluster = c(B = "#ffffff", A = "#000000")),
    viewer_page_expectations = list(
      visible_conditional = character(),
      hidden_conditional = character()
    )
  )
  structure(
    list(
      items = list(item),
      dataset_order = "dataset-a",
      make_app = FALSE,
      app_contract_version = 0L,
      app_options = list(
        enabled = FALSE,
        show_upload_ui = FALSE,
        initial_dataset = "dataset-a",
        initial_dataset_mode = "automatic",
        initial_page = "data_info",
        welcome_message = "Welcome to CerebroNexus!",
        point_size = list(overview_projection_point_size = 5),
        variable_to_compare = FALSE,
        host = "127.0.0.1",
        port = 8080L,
        max_request_size = 8000,
        display_mode = "normal",
        launch_browser = TRUE
      ),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

builder_build_test_hooks <- function(fail = NULL, outside = NULL) {
  list(
    open_snapshot = function(snapshot) list(snapshot = snapshot),
    prepare = function(object, item) object,
    run_analyses = function(object, item) {
      if (!is.null(fail) && fail %in% item$analyses) {
        return(list(
          object = object,
          completed = setdiff(item$analyses, fail),
          failed = fail,
          log = paste("failed", fail)
        ))
      }
      list(
        object = object,
        completed = item$analyses,
        failed = character(),
        log = character()
      )
    },
    export = function(object, item, path) {
      target <- if (is.null(outside)) path else outside
      saveRDS(list(ok = TRUE), target)
      target
    },
    attach_extras = function(path, object, item) list(path = path),
    verify = function(path, item) {
      list(
        valid = TRUE,
        path = path,
        page_contract = item$viewer_page_expectations
      )
    }
  )
}

test_that("build execution reuses the worker's loaded object", {
  stage <- withr::local_tempdir()
  hooks <- builder_build_test_hooks()
  hooks$open_snapshot <- function(snapshot) {
    stop("snapshot should not be reopened")
  }
  loaded <- list(source = "worker-memory")

  result <- builder_execute_plan(
    builder_build_test_plan(),
    stage,
    snapshots = list(`dataset-a` = list(id = "snapshot-a")),
    hooks = hooks,
    objects = list(`dataset-a` = loaded)
  )

  expect_identical(result$state, "success")
  expect_true(result$publishable)
})

builder_build_two_dataset_plan <- function(analyses = character()) {
  plan <- builder_build_test_plan(analyses)
  second <- plan$items[[1L]]
  second$id <- "dataset-b"
  second$name <- "Dataset B"
  second$filename <- "dataset-b.crb"
  plan$items <- list(plan$items[[1L]], second)
  plan$dataset_order <- c("dataset-a", "dataset-b")
  plan
}

test_that("Stop is honoured between datasets, not only at phase changes", {
  stage <- withr::local_tempdir()
  progress <- file.path(stage, "progress.rds")
  calls <- character()
  hooks <- builder_build_test_hooks()
  hooks$prepare <- function(object, item) {
    calls <<- c(calls, paste0("prepare:", item$id))
    object
  }
  hooks$export <- function(object, item, path) {
    calls <<- c(calls, paste0("export:", item$id))
    saveRDS(list(ok = TRUE), path)
    if (identical(item$id, "dataset-a")) {
      builder_build_request_cancel(progress)
    }
    path
  }

  expect_error(
    builder_execute_plan(
      builder_build_two_dataset_plan(),
      stage,
      snapshots = list(
        `dataset-a` = list(id = "snapshot-a"),
        `dataset-b` = list(id = "snapshot-b")
      ),
      hooks = hooks,
      on_progress = .builder_build_progress_callback(progress),
      check_cancel = .builder_build_cancel_callback(progress)
    ),
    class = "builder_build_cancelled"
  )
  expect_identical(calls, c("prepare:dataset-a", "export:dataset-a"))
})

test_that("Stop interrupts the analysis loop and is not reported as a failure", {
  stage <- withr::local_tempdir()
  progress <- file.path(stage, "progress.rds")
  steps <- character()
  hooks <- builder_build_test_hooks()
  hooks$run_analyses <- function(
    object,
    item,
    on_progress = function(...) NULL
  ) {
    for (step in item$analyses) {
      on_progress(step)
      steps <<- c(steps, step)
      builder_build_request_cancel(progress)
    }
    list(object = object, completed = item$analyses, failed = character())
  }
  hooks$export <- function(object, item, path) {
    stop("export must not run after Stop")
  }

  expect_error(
    builder_execute_plan(
      builder_build_test_plan(c("marker_genes", "enriched_pathways")),
      stage,
      snapshots = list(`dataset-a` = list(id = "snapshot-a")),
      hooks = hooks,
      check_cancel = .builder_build_cancel_callback(progress)
    ),
    "Build stopped by the user",
    class = "builder_build_cancelled"
  )
  expect_identical(steps, "marker_genes")
})

test_that("a cancel raised inside a hook is not wrapped as a dataset failure", {
  stage <- withr::local_tempdir()
  hooks <- builder_build_test_hooks()
  hooks$export <- function(object, item, path) {
    stop(structure(
      list(message = "Build stopped by the user.", call = NULL),
      class = c("builder_build_cancelled", "error", "condition")
    ))
  }

  expect_error(
    builder_execute_plan(
      builder_build_test_plan(),
      stage,
      snapshots = list(`dataset-a` = list(id = "snapshot-a")),
      hooks = hooks
    ),
    class = "builder_build_cancelled"
  )
})

test_that("default analysis hook forwards step progress for cancellation", {
  hook <- eval(body(builder_build_hooks)[[2L]]$run_analyses)
  expect_true("on_progress" %in% names(formals(hook)))
  expect_true(.builder_build_hook_accepts(hook, "on_progress"))
  expect_false(.builder_build_hook_accepts(
    function(object, item) NULL,
    "on_progress"
  ))
  expect_null(.builder_build_cancel_callback(NULL))
})

test_that("build execution releases working objects before CRB verification", {
  stage <- withr::local_tempdir()
  hooks <- builder_build_test_hooks()
  hooks$verify <- function(path, item) {
    execution <- parent.frame()
    expect_null(get("object", envir = execution, inherits = FALSE))
    expect_null(get("analyses", envir = execution, inherits = FALSE))
    expect_null(
      get("objects", envir = execution, inherits = FALSE)[[item$id]]
    )
    list(valid = TRUE, path = path)
  }

  result <- builder_execute_plan(
    builder_build_test_plan(),
    stage,
    snapshots = list(`dataset-a` = list(id = "snapshot-a")),
    hooks = hooks,
    objects = list(`dataset-a` = list(source = "worker-memory"))
  )

  expect_identical(result$state, "success")
  expect_true(result$publishable)
})

test_that("export failures retain the original condition trace", {
  stage <- withr::local_tempdir()
  hooks <- builder_build_test_hooks()
  hooks$export <- function(object, item, path) {
    fail_export <- function() {
      stop("original export failure", call. = TRUE)
    }
    fail_export()
  }

  result <- builder_execute_plan(
    builder_build_test_plan(),
    stage,
    snapshots = list(`dataset-a` = list(id = "snapshot-a")),
    hooks = hooks
  )

  expect_identical(result$state, "failure")
  expect_identical(result$error, "Dataset A: original export failure")
  diagnostic <- result$diagnostics$export
  expect_contains(diagnostic$class, "simpleError")
  expect_identical(diagnostic$message, "original export failure")
  expect_match(diagnostic$call, "fail_export", fixed = TRUE)
  expect_true(any(grepl("fail_export", diagnostic$calls, fixed = TRUE)))
})

test_that("retry recomputes the failed dependency closure", {
  graph <- builder_analysis_graph(c("marker_genes", "enriched_pathways"))
  expect_identical(
    builder_retry_closure(graph, "enriched_pathways"),
    c("marker_genes", "enriched_pathways")
  )
  expect_identical(
    builder_retry_closure(graph, "marker_genes"),
    c("marker_genes", "enriched_pathways")
  )
})

test_that("selected analysis failure cannot produce publishable output", {
  stage <- tempfile("builder-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  plan <- builder_build_test_plan(c("marker_genes", "enriched_pathways"))

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list(id = "snapshot-a")),
    hooks = builder_build_test_hooks(fail = "enriched_pathways")
  )

  expect_identical(result$state, "needs_decision")
  expect_false(result$publishable)
  expect_identical(result$failed_analyses, "enriched_pathways")
  expect_identical(
    result$retry_closure,
    c("marker_genes", "enriched_pathways")
  )
  expect_false(file.exists(file.path(stage, "dataset-a.crb")))
})

test_that("only an existing assigned stage can receive artifacts", {
  plan <- builder_build_test_plan()
  missing <- tempfile("missing-builder-stage-")
  expect_error(
    builder_execute_plan(
      plan,
      missing,
      snapshots = list(`dataset-a` = list()),
      hooks = builder_build_test_hooks()
    ),
    "assigned stage"
  )

  stage <- tempfile("builder-stage-")
  dir.create(stage)
  outside <- tempfile("outside-stage-", fileext = ".crb")
  on.exit(unlink(c(stage, outside), recursive = TRUE, force = TRUE), add = TRUE)
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = builder_build_test_hooks(outside = outside)
  )
  expect_identical(result$state, "failure")
  expect_false(result$publishable)
  expect_match(result$error, "outside the assigned stage")

  plan <- builder_build_test_plan()
  escaped_name <- paste0(basename(stage), "-escaped.crb")
  plan$items[[1L]]$filename <- file.path("..", escaped_name)
  escaped <- file.path(dirname(stage), escaped_name)
  on.exit(unlink(escaped), add = TRUE)
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = builder_build_test_hooks()
  )
  expect_identical(result$state, "failure")
  expect_match(result$error, "outside the assigned stage")
  expect_false(file.exists(escaped))
})

test_that("contract-v1 execution assembles App only after CRB verification", {
  stage <- tempfile("builder-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(.clearBundlePreflightCache(), add = TRUE)
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_contract_version <- 1L
  plan$app_options$enabled <- TRUE
  calls <- character()
  hooks <- builder_build_test_hooks()
  hooks$verify <- function(path, item) {
    calls <<- c(calls, "verify_crb")
    list(
      valid = TRUE,
      path = path,
      file_fingerprint = .bundlePreflightFingerprint(path),
      bundle_preflight = list(
        backend = list(type = "embedded", location = NULL, legacy = FALSE),
        spatial_catalog = list()
      )
    )
  }
  hooks$build_app <- function(request, stage, auth_material = NULL) {
    calls <<- c(calls, "build_app")
    expect_true(all(file.exists(request$cerebro_data)))
    preflight <- .preflightBundleData(
      request$cerebro_data,
      read_object = function(path) stop("verified CRB was read again")
    )
    expect_identical(preflight$backends[[1L]]$type, "embedded")
    expect_identical(preflight$spatial_catalogs[[1L]], list())
    app_dir <- file.path(stage, "cerebro_app")
    dir.create(app_dir)
    app_dir
  }
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = hooks
  )

  expect_identical(calls, c("verify_crb", "build_app"))
  expect_identical(result$state, "success")
  expect_true(result$publishable)
  expect_named(result$built, "dataset-a")
  expect_identical(
    result$app_dir,
    file.path(normalizePath(stage, winslash = "/"), "cerebro_app")
  )
  expect_null(result$app_verification)
  expect_true(is.list(result$app_tree_identity))
  expect_false(result$auth_enabled)
  expect_null(result$auth_env_file)
})

test_that("build execution reports dataset and Viewer phases in order", {
  stage <- withr::local_tempdir()
  on.exit(.clearBundlePreflightCache(), add = TRUE)
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_contract_version <- 1L
  plan$app_options$enabled <- TRUE
  hooks <- builder_build_test_hooks()
  hooks$verify <- function(path, item) {
    list(
      valid = TRUE,
      path = path,
      file_fingerprint = .bundlePreflightFingerprint(path),
      bundle_preflight = list(
        backend = list(type = "embedded", location = NULL, legacy = FALSE),
        spatial_catalog = list()
      )
    )
  }
  hooks$build_app <- function(request, stage, auth_material = NULL) {
    app_dir <- file.path(stage, "cerebro_app")
    dir.create(app_dir)
    app_dir
  }
  hooks$verify_app <- function(app_dir, request, auth_env_file = NULL) {
    structure(
      list(valid = TRUE, app_dir = app_dir),
      class = c("builder_app_verification", "list")
    )
  }
  phases <- character()

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = hooks,
    on_progress = function(phase) phases <<- c(phases, phase)
  )

  expect_identical(result$state, "success")
  expect_identical(phases, c("datasets", "viewer"))
})

test_that("build progress records reject unsupported phases", {
  root <- withr::local_tempdir()
  path <- file.path(root, ".build-progress-test.rds")

  expect_true(builder_build_progress_write(path, "viewer"))
  expect_identical(builder_build_progress_read(path), "viewer")
  expect_error(
    builder_build_progress_write(path, "invented"),
    "unsupported build phase",
    fixed = TRUE
  )
  expect_true(builder_build_progress_remove(path))
  expect_false(file.exists(path))

  expect_true(builder_build_progress_write(path, "verify"))
  expect_identical(builder_build_progress_read(path), "verify")
  expect_identical(
    builder_build_progress_note(list(items = list()), "publish"),
    "Publishing release atomically…"
  )
})

test_that("build progress names the dataset and step in progress", {
  root <- withr::local_tempdir()
  path <- file.path(root, ".build-progress-detail.rds")
  callback <- .builder_build_progress_callback(path)
  reported <- list()
  on_progress <- function(phase, detail = NULL) {
    reported[[length(reported) + 1L]] <<- c(
      list(phase = phase),
      builder_build_progress_detail({
        callback(phase, detail)
        path
      })
    )
  }
  plan <- builder_build_two_dataset_plan("marker_genes")
  hooks <- builder_build_test_hooks()
  hooks$run_analyses <- function(object, item, on_progress = NULL) {
    on_progress("Marker genes")
    list(object = object, completed = item$analyses, failed = character())
  }

  result <- builder_execute_plan(
    plan,
    withr::local_tempdir(),
    snapshots = list(`dataset-a` = list(), `dataset-b` = list()),
    hooks = hooks,
    on_progress = on_progress
  )

  expect_identical(result$state, "success")
  steps <- vapply(
    reported,
    function(value) {
      paste(
        value$dataset_index %||% "-",
        value$dataset_count %||% "-",
        value$step_label %||% value$phase,
        sep = "|"
      )
    },
    character(1)
  )
  expect_identical(
    steps,
    c(
      "-|-|datasets",
      "1|2|Opening dataset",
      "1|2|Preparing dataset",
      "1|2|Marker genes",
      "1|2|Writing CRB",
      "1|2|Verifying CRB",
      "2|2|Opening dataset",
      "2|2|Preparing dataset",
      "2|2|Marker genes",
      "2|2|Writing CRB",
      "2|2|Verifying CRB"
    )
  )
  last <- reported[[length(reported)]]
  expect_identical(last$dataset_name, "Dataset B")
  expect_true(is.numeric(last$started_at))
  expect_identical(builder_build_progress_read(path), "datasets")

  # Settlement still writes bare phases, which read back without detail.
  expect_true(builder_build_progress_write(path, "publish"))
  expect_identical(builder_build_progress_detail(path), list(phase = "publish"))

  # Settlement names the verification sub-step with a detail record.
  saveRDS(
    list(
      schema_version = 1L,
      phase = "verify",
      step_label = "Checking files",
      step_index = 12L,
      step_count = 230L
    ),
    path
  )
  expect_identical(
    builder_build_progress_detail(path),
    list(
      schema_version = 1L,
      phase = "verify",
      step_label = "Checking files",
      step_index = 12L,
      step_count = 230L
    )
  )
})

test_that("terminal progress preserves a missed Viewer phase", {
  root <- withr::local_tempdir()
  path <- file.path(root, ".build-progress-test.rds")
  plan <- list(items = list())

  expect_true(builder_build_progress_write(path, "viewer"))
  missed <- builder_build_progress_finish(plan, path, current_note = NULL)
  expect_identical(missed$note, "Packaging Viewer…")
  expect_true(missed$defer_settlement)
  expect_false(file.exists(path))

  expect_true(builder_build_progress_write(path, "viewer"))
  shown <- builder_build_progress_finish(
    plan,
    path,
    current_note = "Packaging Viewer…"
  )
  expect_identical(shown$note, "Packaging Viewer…")
  expect_false(shown$defer_settlement)
  expect_false(file.exists(path))
})

test_that("generated App execution remains closed without contract v1", {
  stage <- tempfile("builder-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_options$enabled <- TRUE

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = builder_build_test_hooks()
  )

  expect_identical(result$state, "failure")
  expect_match(result$error, "contract")
  expect_null(result$app_dir)
})

test_that("failed login build propagates authentication cleanup failure", {
  stage <- withr::local_tempdir()
  plan <- builder_build_test_plan()
  plan$app_auth <- list(
    enabled = TRUE,
    account_count = 1L,
    timeout_minutes = 15L
  )
  source_dir <- file.path(stage, ".builder-auth-source")
  dir.create(source_dir)
  credentials <- file.path(source_dir, "credentials.sqlite")
  writeBin(as.raw(1:8), credentials)
  Sys.chmod(credentials, "0600", use_umask = FALSE)
  env_file <- .builder_auth_write_env(
    file.path(stage, "viewer-auth.env"),
    strrep("a", 64L)
  )
  material <- list(
    source_dir = source_dir,
    credentials = credentials,
    env_file = env_file,
    descriptor = list(
      credentials = credentials,
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE",
      timeout_minutes = 15L
    )
  )
  original_cleanup <- .builder_auth_remove_partial_material
  assign(
    ".builder_auth_remove_partial_material",
    function(stage) {
      original_cleanup(stage, .unlink = function(...) 0L)
    },
    envir = environment(builder_execute_plan)
  )
  on.exit(
    assign(
      ".builder_auth_remove_partial_material",
      original_cleanup,
      envir = environment(builder_execute_plan)
    ),
    add = TRUE
  )

  expect_error(
    builder_execute_plan(
      plan,
      stage,
      snapshots = list(),
      hooks = builder_build_test_hooks()
    ),
    "authentication files could not be cleaned up"
  )
  expect_true(file.exists(credentials))
  expect_true(file.exists(env_file))
})

test_that("successful login execution removes source and retains only safe env path", {
  stage <- withr::local_tempdir()
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_contract_version <- 1L
  plan$app_options$enabled <- TRUE
  plan$app_auth <- list(
    enabled = TRUE,
    account_count = 1L,
    timeout_minutes = 15L
  )
  accounts <- builder_auth_validate_payload(
    TRUE,
    list(list(
      id = "auth-account-1",
      username = "execution-user-31a7",
      password = "execution-password-31a7"
    ))
  )$accounts
  material <- builder_auth_create_material(
    accounts,
    stage,
    .capability = function() list(available = TRUE, reason = NULL)
  )
  source_dir <- material$source_dir
  staged_env <- material$env_file
  hooks <- builder_build_test_hooks()
  hooks$build_app <- function(request, stage, auth_material = NULL) {
    app_dir <- file.path(stage, "cerebro_app")
    dir.create(app_dir)
    file.copy(
      auth_material$env_file,
      file.path(app_dir, "viewer-auth.env"),
      copy.mode = TRUE
    )
    app_dir
  }
  hooks$verify_app <- function(app_dir, request, auth_env_file = NULL) {
    expect_identical(auth_env_file, file.path(app_dir, "viewer-auth.env"))
    structure(
      list(valid = TRUE, app_dir = app_dir),
      class = c("builder_app_verification", "list")
    )
  }

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = hooks,
    auth_material = material
  )

  expect_identical(result$state, "success")
  expect_true(result$auth_enabled)
  expect_identical(
    result$auth_env_file,
    file.path(result$app_dir, "viewer-auth.env")
  )
  expect_false(dir.exists(source_dir))
  expect_false(file.exists(staged_env))
  expect_true(file.exists(result$auth_env_file))
  expect_false("auth_material" %in% names(result))
  expect_false(builder_auth_value_contains(result, "execution-user-31a7"))
  expect_false(builder_auth_value_contains(result, "execution-password-31a7"))
})

test_that("CRB read-back matches exact frozen artifact identity", {
  crb <- tempfile(fileext = ".crb")
  on.exit(unlink(crb), add = TRUE)
  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(c("Gene2", "Gene1"), c("cell-b", "cell-a"))
  )
  object$groups <- list(cluster = c("B", "A"))
  object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  object$expression_backend <- list(type = "embedded", location = NULL)
  object$marker_genes <- list()
  object$most_expressed_genes <- list()
  object$enriched_pathways <- list()
  object$extra_material <- list()
  object$immune_repertoire <- list()
  object$trajectories <- list()
  object$spatial <- list(
    `slice-a` = list(
      coordinates = data.frame(x = 1, y = 2),
      expression = matrix(1),
      histology_images = list(),
      molecules = NULL
    )
  )
  spatial_sidecar <- file.path(
    dirname(crb),
    paste0(tools::file_path_sans_ext(basename(crb)), ".spatial")
  )
  dir.create(spatial_sidecar)
  on.exit(unlink(spatial_sidecar, recursive = TRUE), add = TRUE)
  molecule_file <- file.path(spatial_sidecar, "0001.rds")
  saveRDS(data.frame(x = 1, y = 2, gene = "A"), molecule_file)
  object$spatial[["slice-a"]]$molecules <- structure(
    list(
      file = basename(molecule_file),
      md5 = unname(tools::md5sum(molecule_file))
    ),
    class = "CerebroSpatialMoleculeRef"
  )
  object$spatial_molecule_backend <- list(
    type = "directory",
    location = basename(spatial_sidecar)
  )
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(object, bindings = FALSE)
  saveRDS(object, crb)

  item <- builder_build_test_plan()$items[[1L]]
  item$artifact_identity$spatial_sections <- "slice-a"
  item$images <- list()
  item$viewer_page_expectations$visible_conditional <- "spatial"
  payload_reads <- 0L
  hydrations <- 0L
  observed <- builder_verify_crb(
    crb,
    item,
    .read_payload = function(path) {
      payload_reads <<- payload_reads + 1L
      readRDS(path)
    },
    .hydrate_crb = function(serialized, path) {
      hydrations <<- hydrations + 1L
      serialized
    }
  )
  expect_identical(payload_reads, 1L)
  expect_identical(hydrations, 1L)
  expect_true(observed$valid)
  expect_identical(
    observed$cell_identity,
    builder_axis_identity(item$artifact_identity$cells)
  )
  expect_identical(
    observed$feature_identity,
    builder_axis_identity(item$artifact_identity$features)
  )
  expect_identical(observed$cell_count, 2L)
  expect_identical(observed$feature_count, 2L)
  expect_null(observed$cells)
  expect_null(observed$features)
  expect_identical(observed$groups, names(item$artifact_identity$group_levels))
  expect_identical(observed$projections, item$artifact_identity$projections)
  expect_identical(observed$metadata, item$artifact_identity$metadata)
  expect_identical(
    observed$spatial_sections,
    item$artifact_identity$spatial_sections
  )
  expect_identical(observed$image_sections, character())
  expect_identical(
    observed$spatial_molecule_backend,
    list(type = "directory", location = basename(spatial_sidecar))
  )
  expect_identical(
    observed$spatial_molecule_path,
    normalizePath(spatial_sidecar, winslash = "/", mustWork = TRUE)
  )

  legacy <- readRDS(crb)
  legacy$spatial_molecule_backend <- c(
    legacy$spatial_molecule_backend,
    list(root = file.path(tempdir(), "retired-build-stage"))
  )
  saveRDS(legacy, crb)
  expect_true(builder_verify_crb(crb, item)$valid)
  expect_identical(
    names(readRDS(crb)$spatial_molecule_backend),
    c("type", "location")
  )

  compact_item <- item
  compact_item$artifact_identity$cells <- builder_axis_identity(
    item$artifact_identity$cells
  )
  compact_item$artifact_identity$features <- builder_axis_identity(
    item$artifact_identity$features
  )
  expect_true(builder_verify_crb(crb, compact_item)$valid)

  item$artifact_identity$cells <- rev(item$artifact_identity$cells)
  expect_error(builder_verify_crb(crb, item), "cell identity")
})

test_that("CRB read-back accepts sub-picounit transform serialization drift", {
  crb <- tempfile(fileext = ".crb")
  on.exit(unlink(crb), add = TRUE)
  source_coordinates <- data.frame(
    x = c(3243.684326171875, 3443.931640625),
    y = c(4736.96875, 5265.10546875),
    row.names = c("cell-b", "cell-a")
  )
  spec <- list(rotation_degrees = 78.2, scale = 1)
  transform <- .spx_coordinate_transform_normalize(spec, source_coordinates)
  coordinates <- .spx_apply_coordinate_transform(source_coordinates, spec)
  transform$source_coordinate_fingerprint <-
    .spx_coordinate_transform_fingerprint(source_coordinates)
  transform$transformed_coordinate_fingerprint <-
    .spx_coordinate_transform_fingerprint(coordinates)

  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(c("Gene2", "Gene1"), c("cell-b", "cell-a"))
  )
  object$groups <- list(cluster = c("B", "A"))
  object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  object$expression_backend <- list(type = "embedded", location = NULL)
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories"
  )) {
    object[[field]] <- list()
  }
  object$spatial <- list(
    `fov.2` = list(
      coordinates = coordinates,
      coordinate_transform = transform
    )
  )
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(object, bindings = FALSE)
  saveRDS(object, crb)

  item <- builder_build_test_plan()$items[[1L]]
  item$artifact_identity$spatial_sections <- "fov.2"
  json_spec <- spec
  json_spec$rotation_degrees <- spec$rotation_degrees + 2^-46
  expect_false(identical(
    spec$rotation_degrees,
    json_spec$rotation_degrees
  ))
  item$spatial_coordinate_transforms <- list(`fov.2` = json_spec)
  item$viewer_page_expectations$visible_conditional <- "spatial"

  expect_true(builder_verify_crb(crb, item)$valid)

  # Legacy hashes from a different R writer cannot be reproduced locally.
  # Only an exact immutable artifact recorded in the reuse plan can qualify.
  object$spatial$`fov.2`$coordinate_transform$transformed_coordinate_fingerprint <-
    paste(rep("a", 32), collapse = "")
  saveRDS(object, crb)
  expect_error(builder_verify_crb(crb, item), "coordinate transform differs")
  item$reused_artifact <- list(
    path = crb,
    fingerprint = list(md5 = unname(as.character(tools::md5sum(crb))))
  )
  expect_true(builder_verify_crb(crb, item)$valid)

  object$spatial$`fov.2`$coordinates$x[1] <- coordinates$x[1] + 1
  saveRDS(object, crb)
  expect_error(builder_verify_crb(crb, item), "coordinate transform differs")
  object$spatial$`fov.2`$coordinates <- coordinates
  object$spatial$`fov.2`$coordinate_transform$coordinate_fingerprint_version <- 2L
  saveRDS(object, crb)
  item$reused_artifact$fingerprint$md5 <- unname(as.character(tools::md5sum(
    crb
  )))
  expect_error(builder_verify_crb(crb, item), "coordinate transform differs")
  object$spatial$`fov.2`$coordinate_transform$transformed_coordinate_fingerprint <-
    .spx_coordinate_transform_fingerprint(coordinates)
  saveRDS(object, crb)
  expect_true(builder_verify_crb(crb, item)$valid)
})

test_that("CRB stores alignment only while image bytes remain external", {
  crb_path <- tempfile(fileext = ".crb")
  spatial_sidecar <- file.path(
    dirname(crb_path),
    paste0(tools::file_path_sans_ext(basename(crb_path)), ".spatial")
  )
  dir.create(spatial_sidecar)
  molecule_file <- file.path(spatial_sidecar, "0001.rds")
  saveRDS(data.frame(x = 1, y = 2, gene = "A"), molecule_file)
  on.exit(unlink(c(crb_path, spatial_sidecar), recursive = TRUE), add = TRUE)

  crb <- Cerebro$new()
  crb$setExpressionBackend(type = "embedded")
  crb$spatial <- list(
    `slice-a` = list(
      coordinates = data.frame(x = 1, y = 2),
      expression = Matrix::Matrix(1, nrow = 1L, ncol = 1L, sparse = TRUE),
      histology_images = list(
        Existing = list(
          histology_image = "data:image/png;base64,AA==",
          histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 2)
        )
      ),
      molecules = structure(
        list(
          file = basename(molecule_file),
          md5 = unname(tools::md5sum(molecule_file))
        ),
        class = "CerebroSpatialMoleculeRef"
      )
    )
  )
  crb$spatial_molecule_backend <- list(
    type = "directory",
    location = basename(spatial_sidecar)
  )
  saveRDS(crb, crb_path)

  make_alignment <- function(section_id, section_kind) {
    builder_alignment_record(
      source = list(
        name = "/private/tmp/shiny-upload/tissue.png",
        type = "image/png"
      ),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
      parameters = list(
        dx = 2,
        dy = -1,
        scale = 1.25,
        rotation = 90,
        flip_x = TRUE,
        flip_y = FALSE,
        image_opacity = 0.7,
        point_opacity = 0.8,
        point_size = 3
      ),
      section = list(id = section_id, kind = section_kind),
      source_path = "/private/tmp/shiny-upload/tissue.png"
    )
  }
  spatial_alignment <- make_alignment("slice-a", "spatial")
  trekker_alignment <- make_alignment("trekker", "trekker")
  trekker <- list(x = 1, y = 2, clusters = 0L)

  result <- builder_attach_crb_extras(
    crb_path,
    trekker = trekker,
    trekker_alignment = trekker_alignment,
    external_images = list(
      `slice-a` = list(`tissue.png` = spatial_alignment)
    )
  )

  expect_null(result$error)
  observed <- readRDS(crb_path)
  expect_identical(
    observed$spatial[["slice-a"]]$histology_alignment,
    builder_alignment_payload(spatial_alignment)
  )
  expect_length(observed$spatial[["slice-a"]]$histology_images, 0L)
  expect_identical(
    observed$trekker$histology_alignment,
    builder_alignment_payload(trekker_alignment)
  )
  expect_null(observed$trekker$histology_image)
  expect_null(observed$trekker$histology_image_bounds)
  expect_identical(
    observed$spatial_molecule_backend,
    list(type = "directory", location = basename(spatial_sidecar))
  )
  serialized <- paste(capture.output(str(observed)), collapse = "\n")
  expect_false(grepl("/private/tmp/shiny-upload", serialized, fixed = TRUE))
  expect_false(grepl("source_uri", serialized, fixed = TRUE))
})

test_that("CRB extras use the fast gzip compression level", {
  crb_path <- tempfile(fileext = ".crb")
  on.exit(unlink(crb_path), add = TRUE)

  crb <- Cerebro$new()
  crb$setExpressionBackend(type = "embedded")
  saveRDS(crb, crb_path)

  observed_compression <- NULL
  open_gz <- function(description, open, compression) {
    observed_compression <<- compression
    gzfile(description, open = open, compression = compression)
  }
  result <- builder_attach_crb_extras(
    crb_path,
    trekker = list(x = 1, y = 2, clusters = 0L),
    .open_gz = open_gz
  )

  expect_null(result$error)
  expect_identical(observed_compression, 1L)
})

test_that("CRB extras no-op does not open the exported CRB", {
  missing <- tempfile(fileext = ".crb")

  expect_identical(
    builder_attach_crb_extras(missing),
    list(applied = character(), trekker = FALSE)
  )
})

test_that("Trekker-only extras do not rewrite an exported CRB", {
  crb_path <- tempfile(fileext = ".crb")
  on.exit(unlink(crb_path), add = TRUE)

  crb <- new.env(parent = emptyenv())
  crb$trekker <- list(existing = TRUE)
  crb$addTrekker <- function(value) {
    crb$trekker <- value
    invisible(value)
  }
  class(crb) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(crb, bindings = FALSE)
  saveRDS(crb, crb_path, compress = FALSE)
  before <- unname(tools::md5sum(crb_path))

  object <- SeuratObject::pbmc_small
  object@misc$trekker <- list(
    barcodes = colnames(object)[1:2],
    x = c(1, 2),
    y = c(3, 4),
    clusters = c(0L, 1L)
  )
  item <- list(
    default_group = "groups",
    colors = list(groups = c(`0` = "#000000", `1` = "#ffffff")),
    trekker_alignment = NULL,
    spatial_image_storage = "external",
    images = list()
  )

  result <- .builder_build_attach_extras(crb_path, object, item)

  expect_true(result$trekker)
  expect_identical(unname(tools::md5sum(crb_path)), before)
})

test_that("external Spatial images materialize without entering CRB payloads", {
  skip_if_not_installed("png")
  root <- withr::local_tempdir()
  source_path <- file.path(root, "source.png")
  png::writePNG(matrix(seq(0, 1, length.out = 24L), nrow = 4L), source_path)
  inspected <- builder_read_image(source_path)
  record <- builder_alignment_record(
    source = list(name = "H&E.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
    parameters = list(
      dx = 2,
      dy = -1,
      scale = 1.25,
      rotation = 90,
      flip_x = TRUE,
      flip_y = FALSE,
      image_opacity = 0.8
    ),
    section = list(id = "slice-a", kind = "spatial"),
    source_path = inspected$source_path
  )
  record$source_content_md5 <- inspected$source_content_md5
  record$roi_field <- "sample_roi"
  record$roi_value <- "lesion"
  item <- list(
    id = "dataset-a",
    name = "Dataset A",
    images = list(`slice-a` = list(`H&E` = record))
  )

  external <- .builder_build_materialize_spatial_images(item, root)
  descriptor <- external$images[["dataset-a"]][["slice-a"]][["H&E"]]
  setting <- external$settings[["dataset-a"]][["slice-a"]][["H&E"]]

  expect_true(file.exists(descriptor$path))
  expect_match(basename(descriptor$path), "^i-[0-9a-f]{12}\\.png$")
  expect_match(basename(dirname(descriptor$path)), "^s-[0-9a-f]{12}$")
  expect_match(
    basename(dirname(dirname(descriptor$path))),
    "^d-[0-9a-f]{12}$"
  )
  expect_identical(
    basename(dirname(dirname(dirname(descriptor$path)))),
    ".spatial"
  )
  expect_identical(
    unname(descriptor$bounds),
    c(0, 10, 0, 8)
  )
  expect_identical(setting$image_opacity, 0.8)
  expect_identical(setting$scale_x, 1.25)
  expect_identical(setting$scale_y, 1.25)
  expect_identical(descriptor$roi_field, "sample_roi")
  expect_identical(descriptor$roi_value, "lesion")
  expect_identical(
    unname(as.character(tools::md5sum(descriptor$path))),
    inspected$source_content_md5
  )
  expect_identical(
    unname(file.size(descriptor$path)),
    unname(file.size(source_path))
  )
})

test_that("external Spatial paths cannot collide after normalization", {
  skip_if_not_installed("png")
  root <- withr::local_tempdir()
  sources <- file.path(root, c("first.png", "second.png"))
  png::writePNG(matrix(0, nrow = 2L, ncol = 2L), sources[[1L]])
  png::writePNG(matrix(1, nrow = 2L, ncol = 2L), sources[[2L]])
  make_record <- function(section, path) {
    inspected <- builder_read_image(path)
    record <- builder_alignment_record(
      source = list(name = "image.png", type = "image/png"),
      base_bounds = list(xmin = 0, xmax = 2, ymin = 0, ymax = 2),
      section = list(id = section, kind = "spatial"),
      source_path = inspected$source_path
    )
    record$source_content_md5 <- inspected$source_content_md5
    record
  }
  item <- list(
    id = "dataset a",
    name = "Dataset A",
    images = list(
      `fov 1` = list(image = make_record("fov 1", sources[[1L]])),
      `fov-1` = list(image = make_record("fov-1", sources[[2L]]))
    )
  )

  external <- .builder_build_materialize_spatial_images(item, root)
  first <- external$images[["dataset a"]][["fov 1"]]$image$path
  second <- external$images[["dataset a"]][["fov-1"]]$image$path

  expect_false(identical(first, second))
  expect_identical(
    unname(as.character(tools::md5sum(c(first, second)))),
    unname(as.character(tools::md5sum(sources)))
  )
})

test_that("external Spatial filenames are portable and collision-safe", {
  skip_if_not_installed("png")
  root <- withr::local_tempdir()
  sources <- file.path(root, paste0("source-", seq_len(5L), ".png"))
  for (index in seq_along(sources)) {
    png::writePNG(matrix(index / 5, nrow = 2L, ncol = 2L), sources[[index]])
  }
  source_names <- c(
    "Image.png",
    "image.png",
    "\u00e9.png",
    "e\u0301.png",
    paste0(strrep("a", 251L), ".png")
  )
  labels <- c("Image", "image", "\u00e9", "e\u0301", "long")
  records <- lapply(seq_along(sources), function(index) {
    inspected <- builder_read_image(sources[[index]])
    record <- builder_alignment_record(
      source = list(name = source_names[[index]], type = "image/png"),
      base_bounds = list(xmin = 0, xmax = 2, ymin = 0, ymax = 2),
      section = list(id = "fov", kind = "spatial"),
      source_path = inspected$source_path
    )
    record$source_content_md5 <- inspected$source_content_md5
    record
  })
  names(records) <- labels

  external <- .builder_build_materialize_spatial_images(
    list(
      id = "dataset-a",
      name = "Dataset A",
      images = list(fov = records)
    ),
    root
  )
  paths <- vapply(
    external$images[["dataset-a"]]$fov,
    `[[`,
    character(1),
    "path"
  )
  filenames <- basename(paths)

  expect_true(all(grepl("^[a-z0-9-]+\\.png$", filenames)))
  expect_true(all(grepl("^i-[0-9a-f]{12}\\.png$", filenames)))
  expect_length(unique(tolower(filenames)), length(filenames))
  expect_true(all(nchar(filenames, type = "bytes") <= 255L))
  expect_identical(
    unname(as.character(tools::md5sum(paths))),
    unname(as.character(tools::md5sum(sources)))
  )
})

test_that("read-back verifies frozen H5 sidecars", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")
  root <- tempfile("builder-sidecars-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  base_object <- new.env(parent = emptyenv())
  base_object$expression <- NULL
  base_object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  base_object$gene_data <- data.frame(
    row.names = c("Gene2", "Gene1")
  )
  base_object$groups <- list(cluster = c("B", "A"))
  base_object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories",
    "spatial"
  )) {
    base_object[[field]] <- list()
  }
  base_object$trekker <- NULL
  base_object$hla_typing <- NULL
  class(base_object) <- c("Cerebro_v1.3", "R6")

  item <- builder_build_test_plan()$items[[1L]]
  item$expression_backend <- "h5"
  item$sidecars <- "dataset-a.h5"
  object <- unserialize(serialize(base_object, NULL))
  object$expression_backend <- list(type = "h5", location = item$sidecars)
  lockEnvironment(object, bindings = FALSE)
  crb <- file.path(root, "h5.crb")
  matrix <- Matrix::Matrix(
    matrix(
      seq_len(4L),
      nrow = 2L,
      dimnames = list(
        item$artifact_identity$features,
        item$artifact_identity$cells
      )
    ),
    sparse = TRUE
  )
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(matrix), "CsparseMatrix"),
    file.path(root, item$sidecars),
    group = "expression"
  )
  saveRDS(object, crb)
  observed <- builder_verify_crb(crb, item)
  expect_true(observed$valid)
  expect_identical(observed$backend$type, "h5")
})

test_that("H5 sidecar identity is independent of CRB fallback fields", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")
  root <- withr::local_tempdir()
  item <- builder_build_test_plan()$items[[1L]]
  item$expression_backend <- "h5"
  item$sidecars <- "dataset-a.h5"
  object <- new.env(parent = emptyenv())
  object$expression <- NULL
  object$meta_data <- data.frame(
    cell_barcode = item$artifact_identity$cells,
    cluster = c("B", "A"),
    row.names = item$artifact_identity$cells
  )
  object$gene_data <- data.frame(row.names = item$artifact_identity$features)
  object$groups <- list(cluster = c("B", "A"))
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = item$artifact_identity$cells
    )
  )
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories",
    "spatial"
  )) {
    object[[field]] <- list()
  }
  object$trekker <- NULL
  object$hla_typing <- NULL
  object$expression_backend <- list(type = "h5", location = item$sidecars)
  class(object) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(object, bindings = FALSE)
  crb <- file.path(root, "dataset-a.crb")
  saveRDS(object, crb)

  write_h5 <- function(cells, features, path = file.path(root, item$sidecars)) {
    if (file.exists(path)) {
      unlink(path)
    }
    matrix <- Matrix::Matrix(
      matrix(
        seq_len(length(cells) * length(features)),
        nrow = length(features),
        dimnames = list(features, cells)
      ),
      sparse = TRUE
    )
    HDF5Array::writeTENxMatrix(
      methods::as(Matrix::t(matrix), "CsparseMatrix"),
      path,
      group = "expression"
    )
  }

  write_h5(item$artifact_identity$cells, item$artifact_identity$features)
  expect_true(builder_verify_crb(crb, item)$valid)

  write_h5(rev(item$artifact_identity$cells), item$artifact_identity$features)
  expect_error(builder_verify_crb(crb, item), "H5 sidecar cell identity")

  write_h5(item$artifact_identity$cells, rev(item$artifact_identity$features))
  expect_error(builder_verify_crb(crb, item), "H5 sidecar feature identity")
})

test_that("H5 sidecars reject links and escapes before opening", {
  skip_if_not_installed("HDF5Array")
  skip_on_os("windows")
  root <- withr::local_tempdir()
  item <- builder_build_test_plan()$items[[1L]]
  item$expression_backend <- "h5"
  item$sidecars <- "dataset-a.h5"
  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(
      item$artifact_identity$features,
      item$artifact_identity$cells
    )
  )
  object$meta_data <- data.frame(
    cell_barcode = item$artifact_identity$cells,
    cluster = c("B", "A"),
    row.names = item$artifact_identity$cells
  )
  object$gene_data <- data.frame(row.names = item$artifact_identity$features)
  object$groups <- list(cluster = c("B", "A"))
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = item$artifact_identity$cells
    )
  )
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories",
    "spatial"
  )) {
    object[[field]] <- list()
  }
  object$trekker <- NULL
  object$hla_typing <- NULL
  object$expression_backend <- list(type = "h5", location = item$sidecars)
  class(object) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(object, bindings = FALSE)
  crb <- file.path(root, "dataset-a.crb")
  saveRDS(object, crb)

  outside <- tempfile(fileext = ".h5")
  on.exit(unlink(outside), add = TRUE)
  writeBin(as.raw(c(1, 2, 3)), outside)
  expect_true(file.symlink(outside, file.path(root, item$sidecars)))
  expect_error(builder_verify_crb(crb, item), "sidecar does not match")

  unlink(file.path(root, item$sidecars))
  item$sidecars <- file.path("..", basename(outside))
  object$expression_backend$location <- item$sidecars
  saveRDS(object, crb)
  expect_error(builder_verify_crb(crb, item), "sidecar does not match")
})

test_that("CRB descriptors are checked before sidecar-backed objects open", {
  root <- withr::local_tempdir()
  item <- builder_build_test_plan()$items[[1L]]
  item$expression_backend <- "h5"
  item$sidecars <- "dataset-a.h5"
  writeBin(as.raw(0L), file.path(root, item$sidecars))

  object <- new.env(parent = emptyenv())
  object$expression_backend <- list(
    type = "h5",
    location = "unexpected.h5"
  )
  class(object) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(object, bindings = FALSE)
  crb <- file.path(root, "dataset-a.crb")
  saveRDS(object, crb)
  opened <- FALSE

  expect_error(
    builder_verify_crb(
      crb,
      item,
      .read_crb = function(path) {
        opened <<- TRUE
        stop("unsafe opener was called")
      }
    ),
    "sidecar location differs"
  )
  expect_false(opened)
})

test_that("reused artifacts hash the staged copy without a source pre-scan", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source.crb")
  stage <- file.path(root, "stage")
  dir.create(stage)
  writeBin(charToRaw("artifact"), source)
  verified <- list()
  original_copy <- .builder_build_copy_verified
  runtime <- environment(builder_execute_plan)
  assign(
    ".builder_build_copy_verified",
    function(source, target, fingerprint, ...) {
      verified[[length(verified) + 1L]] <<- list(
        source = normalizePath(source, winslash = "/"),
        target = normalizePath(target, winslash = "/", mustWork = FALSE)
      )
      original_copy(source, target, fingerprint, ...)
    },
    envir = runtime
  )
  withr::defer(
    assign(".builder_build_copy_verified", original_copy, envir = runtime)
  )
  item <- list(
    id = "ds1",
    name = "Dataset",
    filename = "ds1.crb",
    sidecars = character(),
    private_assets = "ds1.crb",
    reused_artifact = list(
      path = source,
      fingerprint = list(md5 = unname(tools::md5sum(source))),
      members = list()
    ),
    images = list()
  )
  plan <- structure(
    list(
      items = list(item),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    ),
    class = c("builder_build_plan", "list")
  )
  noop <- function(...) NULL
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(),
    objects = list(),
    hooks = list(
      open_snapshot = noop,
      prepare = noop,
      run_analyses = noop,
      export = noop,
      attach_extras = noop,
      verify = function(path, item) list(valid = TRUE)
    )
  )

  expect_identical(result$state, "success")
  expect_identical(
    verified,
    list(list(
      source = normalizePath(source, winslash = "/"),
      target = normalizePath(
        file.path(stage, "ds1.crb"),
        winslash = "/",
        mustWork = FALSE
      )
    ))
  )
})

test_that("published and retained CRBs become session reuse records", {
  runtime <- environment(builder_build_reuse_records)
  had_key <- exists("builder_plan_reuse_key", envir = runtime, inherits = FALSE)
  assign(
    "builder_plan_reuse_key",
    function(item) paste0(item$id, "::key"),
    envir = runtime
  )
  withr::defer(
    if (!had_key) rm("builder_plan_reuse_key", envir = runtime)
  )
  plan <- builder_build_two_dataset_plan()
  plan$make_app <- TRUE
  plan$items[[1L]]$sidecars <- "dataset-a.h5"
  release <- "C:/out/CerebroNexus_viewer_abc123"
  file_entry <- function(path, md5, size = 10) {
    list(path = path, type = "file", size = size, md5 = md5)
  }
  value <- list(
    state = "success",
    published = TRUE,
    built = c(
      `dataset-a` = file.path(
        release,
        "cerebro_app/private-data/dataset-a.crb"
      ),
      `dataset-b` = file.path(release, "cerebro_app/private-data/dataset-b.crb")
    ),
    verifications = list(
      `dataset-a` = list(valid = TRUE, path = "a"),
      `dataset-b` = list(valid = TRUE, path = "b")
    ),
    release = list(
      target = release,
      identity = list(
        entries = list(
          list(path = "cerebro_app", type = "directory", size = 0, md5 = NA),
          file_entry("cerebro_app/app.R", strrep("0", 32L)),
          file_entry(
            "cerebro_app/private-data/dataset-a.crb",
            strrep("A", 32L)
          ),
          file_entry("cerebro_app/private-data/dataset-a.h5", strrep("b", 32L)),
          file_entry("cerebro_app/private-data/dataset-b.crb", strrep("c", 32L))
        )
      )
    )
  )

  records <- builder_build_reuse_records(plan, value)

  expect_identical(
    vapply(records, `[[`, character(1), "key"),
    c("dataset-a::key", "dataset-b::key")
  )
  first <- records[[1L]]$record
  expect_identical(
    first$path,
    file.path(release, "cerebro_app/private-data/dataset-a.crb")
  )
  expect_identical(first$fingerprint, list(md5 = strrep("a", 32L), size = 10))
  expect_identical(length(first$members), 1L)
  expect_identical(first$members[[1L]]$target, "dataset-a.h5")
  expect_identical(first$members[[1L]]$fingerprint$md5, strrep("b", 32L))
  expect_null(first$verification)
  expect_identical(records[[1L]]$md5, strrep("a", 32L))
  expect_identical(records[[1L]]$verification, list(valid = TRUE, path = "a"))
  expect_identical(records[[2L]]$record$members, list())

  # A missing sidecar identity is not recorded as reusable.
  value$release$identity$entries[[4L]] <- NULL
  expect_identical(
    vapply(builder_build_reuse_records(plan, value), `[[`, "", "id"),
    "dataset-b"
  )

  retained <- list(
    state = "needs_decision",
    retained_artifacts = list(`dataset-b` = list(path = "kept.crb"))
  )
  kept <- builder_build_reuse_records(plan, retained)
  expect_identical(kept[[1L]]$record$path, "kept.crb")
  expect_identical(
    builder_build_reuse_records(plan, list(state = "failure")),
    list()
  )
})

builder_build_reuse_plan <- function(source, verification = NULL) {
  item <- list(
    id = "ds1",
    name = "Dataset",
    filename = "ds1.crb",
    expression_backend = "embedded",
    sidecars = character(),
    private_assets = "ds1.crb",
    reused_artifact = list(
      path = source,
      fingerprint = list(md5 = unname(tools::md5sum(source))),
      members = list(),
      verification = verification
    ),
    images = list()
  )
  structure(
    list(
      items = list(item),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("a reused CRB already read back in this session is not reopened", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source.crb")
  writeBin(charToRaw("artifact"), source)
  deep <- 0L
  noop <- function(...) NULL
  hooks <- list(
    open_snapshot = noop,
    prepare = noop,
    run_analyses = noop,
    export = noop,
    attach_extras = noop,
    verify = function(path, item) {
      deep <<- deep + 1L
      list(valid = TRUE, path = path, source = "deep")
    }
  )
  earlier <- list(
    valid = TRUE,
    path = "C:/old-release/cerebro_app/private-data/ds1.crb",
    file_fingerprint = list(size = 1),
    bundle_preflight = list(
      backend = list(type = "embedded"),
      spatial_backend = NULL,
      dataset_info = list(label = "Dataset", path = "C:/old-stage/ds1.crb")
    )
  )

  stage <- file.path(root, "stage-1")
  dir.create(stage)
  result <- builder_execute_plan(
    builder_build_reuse_plan(source, earlier),
    stage,
    snapshots = list(),
    hooks = hooks
  )
  staged <- file.path(normalizePath(stage, winslash = "/"), "ds1.crb")
  verification <- result$verifications$ds1
  expect_identical(result$state, "success")
  expect_identical(deep, 0L)
  expect_identical(verification$path, staged)
  expect_identical(verification$bundle_preflight$dataset_info$path, staged)
  expect_identical(
    verification$file_fingerprint$path,
    normalizePath(staged, winslash = "/")
  )

  # Without a recorded read-back the copy is verified in full.
  stage <- file.path(root, "stage-2")
  dir.create(stage)
  result <- builder_execute_plan(
    builder_build_reuse_plan(source),
    stage,
    snapshots = list(),
    hooks = hooks
  )
  expect_identical(deep, 1L)
  expect_identical(result$verifications$ds1$source, "deep")

  # A recorded read-back whose sidecar is missing falls back to the full one.
  stage <- file.path(root, "stage-3")
  dir.create(stage)
  plan <- builder_build_reuse_plan(source, earlier)
  plan$items[[1L]]$expression_backend <- "h5"
  plan$items[[1L]]$sidecars <- "ds1.h5"
  result <- builder_execute_plan(plan, stage, snapshots = list(), hooks = hooks)
  expect_identical(deep, 2L)
  expect_identical(result$verifications$ds1$source, "deep")
})

test_that("the worker seeds copied App data from request identities", {
  app_dir <- withr::local_tempdir()
  private <- file.path(app_dir, "private-data")
  dir.create(private)
  writeLines("crb bytes", file.path(private, "ds1.crb"))
  writeLines("h5 bytes", file.path(private, "ds1.h5"))
  writeLines("template", file.path(app_dir, "app.R"))
  md5 <- function(name) unname(tools::md5sum(file.path(private, name)))
  portable <- function(name, md5_value = md5(name)) {
    list(
      path = paste0("private-data/", name),
      type = "file",
      size = as.double(file.size(file.path(private, name))),
      md5 = md5_value
    )
  }
  request <- list(
    content_identities = list(
      `private-data/ds1.crb` = list(
        crb = portable("ds1.crb"),
        backend = list(
          type = "h5",
          entries = list(`private-data/ds1.h5` = portable("ds1.h5"))
        ),
        spatial_molecules = list(type = "none", entries = list())
      )
    )
  )
  hashed <- character()
  counting <- function(path) {
    hashed <<- c(hashed, basename(path))
    tools::md5sum(path)
  }

  seeded <- .builder_app_tree_identity(
    app_dir,
    .digest_file = counting,
    .previous = .builder_build_app_tree_seed(app_dir, request)
  )
  full <- .builder_app_tree_identity(app_dir)

  expect_identical(hashed, "app.R")
  expect_identical(
    .builder_app_tree_summary(seeded),
    .builder_app_tree_summary(full)
  )

  # A copy that differs from its input is reported with the input MD5, so the
  # coordinator's own full read rejects it.
  request$content_identities[[1L]]$crb$md5 <- strrep("0", 32L)
  wrong <- .builder_app_tree_identity(
    app_dir,
    .previous = .builder_build_app_tree_seed(app_dir, request)
  )
  expect_false(identical(
    .builder_app_tree_summary(wrong),
    .builder_app_tree_summary(full)
  ))
})

test_that("read-back rejects a Viewer page-gate mismatch", {
  crb <- tempfile(fileext = ".crb")
  on.exit(unlink(crb), add = TRUE)
  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(c("Gene2", "Gene1"), c("cell-b", "cell-a"))
  )
  object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  object$groups <- list(cluster = c("B", "A"))
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  object$expression_backend <- list(type = "embedded", location = NULL)
  object$marker_genes <- list(
    wilcox = list(cluster = data.frame(gene = "Gene1"))
  )
  object$most_expressed_genes <- list()
  object$enriched_pathways <- list()
  object$extra_material <- list()
  object$immune_repertoire <- list()
  object$trajectories <- list()
  object$spatial <- list()
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(object, bindings = FALSE)
  saveRDS(object, crb)

  expect_error(
    builder_verify_crb(crb, builder_build_test_plan()$items[[1L]]),
    "page contract"
  )
})

test_that("build preparation applies the frozen metadata policy", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object$secret_note <- rep("private", ncol(object))
  object$orig.ident <- factor(rep("sample_a", ncol(object)))
  item <- list(
    included_projections = "tsne",
    assay = "RNA",
    layer = "data",
    artifact_identity = list(
      group_levels = list(groups = sort(unique(object$groups)))
    ),
    metadata_policy = list(
      retained = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA",
        "orig.ident"
      )
    ),
    tables = list()
  )

  item$analyses <- character()
  prepared <- .builder_build_apply_metadata_policy(object, item)
  expect_setequal(
    colnames(prepared@meta.data),
    c("groups", "nCount_RNA", "nFeature_RNA", "orig.ident")
  )
  expect_contains(colnames(prepared@meta.data), "orig.ident")
  expect_false("secret_note" %in% colnames(prepared@meta.data))
})

test_that("build preparation follows the frozen immune source", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  cells <- SeuratObject::Cells(object)
  repertoire <- function(cell, suffix) {
    data.frame(
      barcode = cell,
      CTgene = paste0("TRB_", suffix),
      CTnt = paste0("nt_", suffix),
      CTaa = paste0("aa_", suffix),
      CTstrict = paste0("strict_", suffix),
      stringsAsFactors = FALSE
    )
  }
  object@misc$immune_repertoire <- list(
    unified = repertoire(cells[[1L]], "unified")
  )
  object@misc$tcr_data <- list(
    legacy = repertoire(cells[[2L]], "legacy")
  )

  item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = "preserved",
        evidence = list(selected_sources = "unified_misc")
      )
    )
  )
  unified <- .builder_build_prepare_immune(object, item)
  expect_named(unified@misc$immune_repertoire, "unified")
  expect_null(unified@misc$tcr_data)
  expect_null(unified@misc$bcr_data)

  item$manifest$immune_repertoire$disposition <- "converted"
  item$manifest$immune_repertoire$evidence$selected_sources <- "legacy_tcr"
  legacy <- .builder_build_prepare_immune(object, item)
  expect_named(legacy@misc$immune_repertoire, "legacy")
  expect_identical(
    legacy@misc$immune_repertoire$legacy$barcode,
    cells[[2L]]
  )
  expect_null(legacy@misc$tcr_data)

  motif_only_item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = NA_character_,
        evidence = list(selected_sources = character())
      ),
      hla_tcr_motifs = list(
        disposition = "converted",
        evidence = list(
          selected_sources = "legacy_tcr",
          selected_candidates = list(
            legacy_tcr = list(
              full_ir_ready = TRUE,
              hla_tcr_ready = TRUE
            )
          )
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, motif_only_item),
    "cannot hide only one Viewer page",
    fixed = TRUE
  )

  incompatible_item <- list(
    included_groups = "groups",
    manifest = list(
      immune_repertoire = list(
        disposition = "converted",
        evidence = list(
          selected_sources = "metadata",
          selected_candidates = list(metadata = list(normalized = list()))
        )
      ),
      hla_tcr_motifs = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(unified_misc = list())
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, incompatible_item),
    "cannot be realized by one frozen immune payload",
    fixed = TRUE
  )

  partial_motif_item <- list(
    manifest = list(
      hla_tcr_motifs = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(
            unified_misc = list(
              full_ir_ready = FALSE,
              hla_tcr_ready = TRUE
            )
          )
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, partial_motif_item),
    "cannot be exported as one immune payload",
    fixed = TRUE
  )

  coupled_item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(
            unified_misc = list(
              full_ir_ready = TRUE,
              hla_tcr_ready = TRUE
            )
          )
        )
      ),
      hla_tcr_motifs = list(
        disposition = "filtered",
        evidence = list(selected_sources = character())
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, coupled_item),
    "cannot hide only one Viewer page",
    fixed = TRUE
  )

  reverse_coupled_item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = "filtered",
        evidence = list(selected_sources = character())
      ),
      hla_tcr_motifs = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(
            unified_misc = list(
              full_ir_ready = TRUE,
              hla_tcr_ready = TRUE
            )
          )
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, reverse_coupled_item),
    "cannot hide only one Viewer page",
    fixed = TRUE
  )

  item$manifest$immune_repertoire$disposition <- "filtered"
  filtered <- .builder_build_prepare_immune(object, item)
  expect_null(filtered@misc$immune_repertoire)
  expect_null(filtered@misc$tcr_data)
  expect_null(filtered@misc$bcr_data)
})

test_that("a real example is exported and verified only inside its stage", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Seurat")
  object <- SeuratObject::pbmc_small
  cells <- SeuratObject::Cells(object)
  features <- rownames(object[["RNA"]])
  group_levels <- sort(unique(as.character(object$groups)))
  item <- list(
    id = "pbmc-small",
    name = "PBMC small",
    filename = "pbmc-small.crb",
    organism = "hg",
    assay = "RNA",
    layer = "data",
    included_groups = "groups",
    included_projections = "tsne",
    analyses = character(),
    analysis_dependency_graph = list(),
    artifact_identity = list(
      schema_version = 2L,
      cells = cells,
      features = features,
      group_levels = list(groups = group_levels),
      projections = "tsne",
      source_metadata = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA"
      ),
      metadata = c("cell_barcode", "groups", "nUMI", "nGene"),
      spatial_sections = character()
    ),
    tables = list(),
    images = list(),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    default_group = "groups",
    metadata_policy = list(
      included = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA"
      )
    ),
    expression_backend = "embedded",
    sidecars = character(),
    viewer_page_expectations = list(
      visible_conditional = character(),
      hidden_conditional = character()
    )
  )
  plan <- structure(
    list(
      items = list(item),
      make_app = FALSE,
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
  stage <- tempfile("builder-real-stage-")
  dir.create(stage, mode = "0700")
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  hooks <- list(
    open_snapshot = function(snapshot) unserialize(snapshot),
    prepare = .builder_build_prepare,
    run_analyses = function(object, item) {
      builder_run_analyses(object, item$analyses, item)
    },
    export = .builder_build_export,
    attach_extras = .builder_build_attach_extras,
    verify = builder_verify_crb
  )

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`pbmc-small` = serialize(object, NULL)),
    hooks = hooks
  )

  expect_null(result$error)
  expect_identical(result$state, "success")
  expect_true(result$publishable)
  expect_length(result$built, 1L)
  expect_true(startsWith(
    result$built[[1L]],
    normalizePath(stage, winslash = "/")
  ))
  expect_true(result$verifications[["pbmc-small"]]$valid)
  expect_identical(
    result$verifications[["pbmc-small"]]$file_fingerprint,
    .bundlePreflightFingerprint(result$built[[1L]])
  )
  expect_identical(
    result$verifications[["pbmc-small"]]$bundle_preflight$backend$type,
    "embedded"
  )
  expect_length(
    result$verifications[["pbmc-small"]]$bundle_preflight$spatial_catalog,
    0L
  )
  expect_identical(
    result$verifications[["pbmc-small"]]$metadata,
    item$artifact_identity$metadata
  )
})
