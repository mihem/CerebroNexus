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
  skip_if_not_installed("callr")
  if (isTRUE(exclude_package)) {
    skip_if_package_in_r_library()
  }
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

## R's own library (.Library, .Library.site) is always on a child process's
## library path, so with CerebroNexus installed there no process can run
## without it. file.exists(), not system.file(): pkgload's system.file() shim
## finds the source tree whatever lib.loc says.
skip_if_package_in_r_library <- function() {
  skip_if(
    any(file.exists(file.path(
      c(.Library, .Library.site),
      "CerebroNexus",
      "DESCRIPTION"
    ))),
    "CerebroNexus is installed in R's own library, so no package-free process"
  )
}

privacy_hermetic_library <- function(root) {
  skip_if_package_in_r_library()
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
        ## Junctions on Windows: unlink() cannot delete a directory symbolic
        ## link there, and it would stay in the temporary folder.
        ok <- test_dir_link(file.path(source, package), target)
        if (ok && identical(.Platform$OS.type, "windows")) {
          junctions <- c(junctions, target)
        }
        linked <- linked || ok
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
