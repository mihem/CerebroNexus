privacy_test_sources <- function(root) {
  source_dir <- file.path(root, "source")
  dir.create(source_dir)

  h5 <- Cerebro$new()
  h5$setExpressionBackend(type = "h5", location = "matrix.h5")
  h5_crb <- file.path(source_dir, "h5-data.crb")
  saveRDS(h5, h5_crb)
  writeLines("H5 PAYLOAD", file.path(source_dir, "matrix.h5"))

  bpcells <- Cerebro$new()
  bpcells$setExpressionBackend(
    type = "bpcells",
    location = "matrix.bpcells"
  )
  bpcells_crb <- file.path(source_dir, "bpcells-data.crb")
  saveRDS(bpcells, bpcells_crb)
  dir.create(file.path(source_dir, "matrix.bpcells"))
  writeLines(
    "BPCELLS PAYLOAD",
    file.path(source_dir, "matrix.bpcells", "payload")
  )

  image <- write_dummy_png(file.path(root, "histology.png"))
  image_bytes <- readBin(
    image,
    what = "raw",
    n = as.integer(file.info(image)$size)
  )

  list(
    cerebro_data = c("H5" = h5_crb, "BPCells" = bpcells_crb),
    image = image,
    image_bytes = image_bytes
  )
}

privacy_start_app <- function(
  app_dir,
  port,
  root,
  libpath = NULL,
  exclude_package = FALSE,
  test_mode = FALSE
) {
  stdout <- file.path(root, paste0("app-", port, ".stdout"))
  stderr <- file.path(root, paste0("app-", port, ".stderr"))
  process <- callr::r_bg(
    function(app_dir, port, exclude_package, test_mode) {
      if (
        isTRUE(exclude_package) &&
          requireNamespace("CerebroNexus", quietly = TRUE)
      ) {
        stop("CerebroNexus is reachable; the library is not hermetic")
      }
      shiny::runApp(
        appDir = app_dir,
        port = port,
        host = "127.0.0.1",
        launch.browser = FALSE,
        quiet = TRUE,
        test.mode = test_mode
      )
    },
    args = list(
      app_dir = app_dir,
      port = port,
      exclude_package = exclude_package,
      test_mode = test_mode
    ),
    libpath = libpath,
    stdout = stdout,
    stderr = stderr,
    supervise = TRUE
  )

  list(
    process = process,
    base_url = paste0("http://127.0.0.1:", port),
    stdout = stdout,
    stderr = stderr
  )
}

privacy_hermetic_library <- function(root) {
  library <- tempfile("hermetic-library-", tmpdir = root)
  dir.create(library)
  linked <- FALSE
  junctions <- character()
  for (source in .libPaths()) {
    packages <- list.dirs(source, recursive = FALSE, full.names = FALSE)
    for (package in packages) {
      if (identical(package, "CerebroNexus")) {
        next
      }
      target <- file.path(library, package)
      if (!file.exists(target)) {
        ok <- suppressWarnings(tryCatch(
          file.symlink(file.path(source, package), target),
          error = function(error) FALSE
        ))
        if (!isTRUE(ok) && identical(.Platform$OS.type, "windows")) {
          ok <- suppressWarnings(tryCatch(
            Sys.junction(file.path(source, package), target),
            error = function(error) FALSE
          ))
          if (isTRUE(ok)) {
            junctions <- c(junctions, target)
          }
        }
        linked <- linked || isTRUE(ok)
      }
    }
  }
  if (length(junctions)) {
    cleanup_env <- tryCatch(
      testthat::teardown_env(),
      error = function(error) globalenv()
    )
    withr::defer(
      unlink(junctions, recursive = FALSE, force = TRUE),
      envir = cleanup_env
    )
  }
  if (!linked) NULL else library
}

privacy_source_builder_runtime <- function(contract_version = 1L) {
  runtime <- new.env(parent = globalenv())
  builder_profile_source_runtime(runtime)
  builder_dir <- builder_profile_inst_path("builder")
  package_inst <- dirname(builder_dir)
  sys.source(
    file.path(builder_dir, "core", "bundle_path_contract.R"),
    envir = runtime
  )
  for (file in c(
    "publish.R",
    "app_bundle.R",
    "report.R",
    "coordinator.R",
    "io.R",
    "spatial.R",
    "manifest.R",
    "content_tables.R",
    "content_immune.R",
    "content_spatial.R",
    "content.R",
    "profile.R",
    "inspect.R",
    "adapters.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "marker_import.R",
    "build.R",
    "prerequisite.R",
    "state.R",
    "plan.R",
    "worker.R",
    "session.R"
  )) {
    sys.source(file.path(builder_dir, file), envir = runtime)
  }
  ## The worker below is deliberately sourced from this checkout's explicit
  ## inst/ tree. Keep the parent verifier on that same trusted root: earlier
  ## tests may change which installed/load_all package system.file() resolves,
  ## and mixing those roots produces a false template-identity failure.
  runtime$.builder_app_package_path <- function(...) {
    path <- file.path(package_inst, ...)
    if (!nzchar(path) || !runtime$.builder_app_path_exists(path)) {
      stop("A package-owned trusted template is missing.", call. = FALSE)
    }
    path
  }
  actual <- runtime$builder_installed_app_contract_version()
  if (!is.null(contract_version)) {
    runtime$builder_installed_app_contract_version <- function(
      namespace = NULL
    ) {
      contract_version
    }
  }
  list(runtime = runtime, actual_contract_version = actual)
}

privacy_wait_for_builder <- function(runtime, worker, timeout = 120) {
  deadline <- Sys.time() + timeout
  repeat {
    polled <- runtime$builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      return(list(worker = worker, result = polled$result))
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the Builder integration fixture.")
    }
  }
}

privacy_builder_entry <- function(runtime, id, label, loaded) {
  value <- loaded$result$value
  list(
    id = id,
    path = NULL,
    example = id,
    format = value$format,
    profile = value$profile,
    dataset_profile = value$dataset_profile,
    snapshot = value$snapshot,
    revision = 0L,
    levels = value$levels,
    settings = runtime$builder_default_settings(value$profile, label)
  )
}

privacy_spatial_image_bounds <- function(coordinates) {
  stopifnot(
    is.data.frame(coordinates),
    all(c("x", "y") %in% names(coordinates)),
    nrow(coordinates) > 0L
  )
  list(
    xmin = min(coordinates$x),
    xmax = max(coordinates$x),
    ymin = min(coordinates$y),
    ymax = max(coordinates$y)
  )
}

privacy_build_dormant_app <- function(root, contract_version = 1L) {
  sourced <- privacy_source_builder_runtime(contract_version)
  runtime <- sourced$runtime
  snapshot_root <- file.path(root, "snapshots")
  dir.create(snapshot_root)
  worker <- runtime$builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = snapshot_root,
    snapshot_registry = list()
  )
  if (!is.null(worker$error)) {
    stop(worker$error)
  }
  on.exit(try(runtime$builder_worker_stop(worker), silent = TRUE), add = TRUE)
  worker$process$run(
    function(package_inst) {
      builder_worker_require_capability("build")
      .builder_app_package_path <<- local({
        trusted_root <- package_inst
        function(...) {
          path <- file.path(trusted_root, ...)
          if (!nzchar(path) || (!file.exists(path) && !dir.exists(path))) {
            stop("A package-owned trusted template is missing.", call. = FALSE)
          }
          path
        }
      })
      trusted_create_app <- CerebroNexus::createShinyApp
      lookup <- new.env(parent = environment(trusted_create_app))
      lookup$system.file <- local({
        trusted_root <- package_inst
        function(
          ...,
          package = "base",
          lib.loc = NULL,
          mustWork = FALSE
        ) {
          if (identical(package, "CerebroNexus")) {
            path <- file.path(trusted_root, ...)
            if (
              isTRUE(mustWork) &&
                (!file.exists(path) && !dir.exists(path))
            ) {
              stop("No file found", call. = FALSE)
            }
            return(path)
          }
          base::system.file(
            ...,
            package = package,
            lib.loc = lib.loc,
            mustWork = mustWork
          )
        }
      })
      environment(trusted_create_app) <- lookup
      builder_build_app <<- local({
        build_app <- builder_build_app
        trusted_app <- trusted_create_app
        function(
          request,
          stage,
          create_app = trusted_app,
          auth_material = NULL
        ) {
          build_app(
            request,
            stage,
            create_app = create_app,
            auth_material = auth_material
          )
        }
      })
      invisible(TRUE)
    },
    args = list(package_inst = dirname(builder_profile_inst_path("builder")))
  )

  load_example <- function(id, example) {
    runtime$builder_session_example(worker, id, example)
    loaded <- privacy_wait_for_builder(runtime, worker)
    worker <<- loaded$worker
    if (!is.null(loaded$result$value$error)) {
      stop(loaded$result$value$error)
    }
    worker <<- runtime$builder_worker_register_snapshot(
      worker,
      id,
      loaded$result$value$snapshot
    )
    loaded
  }
  first <- load_example("dataset-a", "complete_viewer_data")
  second <- load_example("dataset-b", "complete_viewer_data")
  entries <- list(
    privacy_builder_entry(runtime, "dataset-a", "Dataset A", first),
    privacy_builder_entry(runtime, "dataset-b", "Dataset B", second)
  )
  entries[[1L]]$settings$organism <- "hg"
  entries[[2L]]$settings$organism <- "hg"
  entries[[1L]]$settings$groups <- c("patient_id", "cluster")
  entries[[1L]]$settings$included_groups <- c("patient_id", "cluster")
  entries[[1L]]$settings$default_group <- "cluster"
  entries[[1L]]$settings$reductions <- "umap"
  entries[[1L]]$settings$included_projections <- "umap"
  entries[[1L]]$settings$default_projection <- "umap"
  entries[[1L]]$settings$initial_projections <- "umap"
  entries[[1L]]$settings$expression_backend <- "h5"
  entries[[2L]]$settings$groups <- c("cell_type", "region")
  entries[[2L]]$settings$included_groups <- c("cell_type", "region")
  entries[[2L]]$settings$default_group <- "region"
  entries[[2L]]$settings$reductions <- c("umap", "tsne")
  entries[[2L]]$settings$included_projections <- c("umap", "tsne")
  entries[[2L]]$settings$default_projection <- "tsne"
  entries[[2L]]$settings$initial_projections <- "tsne"
  entries[[2L]]$settings$spatial_image_storage <- "external"
  section <- entries[[2L]]$dataset_profile$spatial$sections[[1L]]
  image_file <- write_dummy_png(file.path(root, "builder-histology.png"))
  inspected <- runtime$builder_read_image(image_file)
  snapshot <- runtime$builder_open_snapshot(entries[[2L]]$snapshot)
  coordinates <- runtime$builder_spatial_contract(
    snapshot,
    image = section
  )$coordinates
  bounds <- privacy_spatial_image_bounds(coordinates)
  record <- runtime$builder_alignment_record(
    source = list(
      name = basename(image_file),
      type = inspected$mime,
      size = inspected$bytes
    ),
    base_bounds = bounds,
    section = list(id = section, kind = "spatial"),
    source_path = inspected$source_path
  )
  record$source_content_md5 <- inspected$source_content_md5
  entries[[2L]]$settings$images <- stats::setNames(
    list(record),
    section
  )
  release <- file.path(root, "release")
  plan <- runtime$builder_freeze_plan(
    entries,
    release,
    make_app = TRUE,
    app_options = list(
      initial_dataset = "dataset-b",
      show_upload_ui = FALSE
    ),
    app_auth = list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
  )
  if (!is.null(plan$error)) {
    stop(
      plan$error,
      " [",
      plan$error_code %||% "unknown",
      "]"
    )
  }
  protocol <- runtime$builder_request_protocol(worker$epoch)
  protocol <- runtime$builder_enqueue(
    protocol,
    runtime$builder_command(
      "build",
      "dataset-a",
      payload = list(id = "dormant-app-build")
    )
  )
  dispatched <- runtime$builder_protocol_dispatch(protocol)
  coordinator <- runtime$builder_coordinator_prepare(
    plan,
    dispatched$request$build_id
  )
  runtime$builder_session_build(
    worker,
    plan,
    dispatched$request,
    coordinator = coordinator
  )
  built <- privacy_wait_for_builder(runtime, worker, timeout = 180)
  worker <- built$worker
  if (!is.null(built$result$error)) {
    stop(built$result$error)
  }
  response <- built$result$value
  value <- response$value
  if (
    !identical(value$state, "success") ||
      !isTRUE(value$publishable)
  ) {
    stop(
      value$error %||%
        paste(
          "The dormant Builder fixture did not produce a publishable build:",
          paste(capture.output(str(value)), collapse = " ")
        )
    )
  }
  result <- runtime$builder_coordinator_publish(
    coordinator,
    value
  )
  list(
    actual_contract_version = sourced$actual_contract_version,
    result = result,
    app_dir = result$app_dir,
    release = release,
    section = section,
    image_path = image_file,
    image_bounds = bounds
  )
}

privacy_stop_app <- function(app) {
  if (app$process$is_alive()) {
    app$process$kill_tree()
  }
  try(app$process$wait(timeout = 5000), silent = TRUE)
  invisible(NULL)
}

privacy_app_logs <- function(app) {
  read_log <- function(path) {
    if (file.exists(path)) {
      paste(readLines(path, warn = FALSE), collapse = "\n")
    } else {
      "<missing>"
    }
  }
  paste(
    "stdout:",
    read_log(app$stdout),
    "stderr:",
    read_log(app$stderr),
    sep = "\n"
  )
}

privacy_wait_for_app <- function(app) {
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline) {
    if (!app$process$is_alive()) {
      testthat::fail(paste(
        "Generated app exited before ready",
        privacy_app_logs(app)
      ))
    }
    response <- tryCatch(
      httr::GET(app$base_url, httr::timeout(1)),
      error = function(error) NULL
    )
    if (!is.null(response) && httr::status_code(response) == 200L) {
      return(invisible(app))
    }
    Sys.sleep(0.05)
  }
  testthat::fail(paste(
    "Generated app did not become ready",
    privacy_app_logs(app)
  ))
}

privacy_get_from_app <- function(app, path) {
  if (!app$process$is_alive()) {
    testthat::fail(paste(
      "Generated app exited early",
      privacy_app_logs(app)
    ))
  }
  tryCatch(
    httr::GET(paste0(app$base_url, path), httr::timeout(5)),
    error = function(error) {
      if (!app$process$is_alive()) {
        testthat::fail(paste(
          "Generated app exited early",
          privacy_app_logs(app)
        ))
      }
      stop(error)
    }
  )
}

privacy_write_legacy_app <- function(app_dir) {
  dir.create(file.path(app_dir, "data"), recursive = TRUE)
  writeLines("LEGACY", file.path(app_dir, "data", "legacy.txt"))
  writeLines(
    c(
      "cerebro_root <- '.'",
      paste0(
        "shiny::addResourcePath('data', ",
        "file.path(cerebro_root, 'data'))"
      ),
      paste0(
        "shiny::shinyApp(",
        "ui = shiny::fluidPage('legacy'), ",
        "server = function(input, output, session) {})"
      )
    ),
    file.path(app_dir, "app.R")
  )
}
