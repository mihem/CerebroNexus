test_that("dataset bundle paths preserve UTF-8 MD5 identity", {
  ids <- c("dataset-a", "123", "\u65e5\u672c\u8a9e", "a\nb", "caf\u00e9")
  hashes <- c(
    "e8fd00f1966896a78b48992444d4e937",
    "202cb962ac59075b964b07152d234b70",
    "00110af8b4393ef3f72c50be5b332bec",
    "8cdeb44417f3c26826595d5820cf5700",
    "07117fe4a1ebd544965dc19573183da2"
  )
  targets <- vapply(ids, CerebroNexus:::.datasetBundleTarget, character(1))
  expect_identical(
    unname(targets),
    paste0("private-data/dataset-", hashes, ".crb")
  )
  latin1 <- iconv(ids[[5L]], from = "UTF-8", to = "latin1")
  expect_identical(
    CerebroNexus:::.datasetBundleTarget(latin1),
    unname(targets[[5L]])
  )
  for (id in list(NULL, character(), "", NA_character_, c("a", "b"))) {
    expect_error(
      CerebroNexus:::.datasetBundleTarget(id),
      "non-empty dataset identity"
    )
  }
})

privacy_test_sources <- function(root) {
  source_dir <- file.path(root, "source")
  dir.create(source_dir)

  h5 <- Cerebro$new()
  h5$setExpressionBackend(type = "h5", location = "matrix.h5")
  h5$addSpatialData(
    "section",
    list(
      coordinates = data.frame(
        x = c(0, 1),
        y = c(0, 1),
        row.names = c("cell-1", "cell-2")
      ),
      expression = matrix(
        1:4,
        nrow = 2,
        dimnames = list(c("gene-1", "gene-2"), c("cell-1", "cell-2"))
      )
    )
  )
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

privacy_start_app <- function(app_dir, port, root) {
  stdout <- file.path(root, paste0("app-", port, ".stdout"))
  stderr <- file.path(root, paste0("app-", port, ".stderr"))
  process <- callr::r_bg(
    function(app_dir, port) {
      shiny::runApp(
        appDir = app_dir,
        port = port,
        host = "127.0.0.1",
        launch.browser = FALSE,
        quiet = TRUE
      )
    },
    args = list(app_dir = app_dir, port = port),
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

test_that("generated apps expose no bundled artifacts over HTTP", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")

  root <- withr::local_tempdir()
  sources <- privacy_test_sources(root)
  port <- httpuv::randomPort(host = "127.0.0.1")
  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = sources$cerebro_data,
    result_dir = app_dir,
    port = port,
    host = "127.0.0.1",
    launch_browser = FALSE,
    quiet = TRUE,
    spatial_images = list(
      H5 = list(section = c(Histology = sources$image))
    ),
    verbose = FALSE
  )

  private_paths <- c(
    file.path(app_dir, "private-data", "h5-data.crb"),
    file.path(app_dir, "private-data", "bpcells-data.crb"),
    file.path(app_dir, "private-data", "matrix.h5"),
    file.path(app_dir, "private-data", "matrix.bpcells", "payload")
  )
  expect_true(all(file.exists(private_paths)))
  expect_false(dir.exists(file.path(app_dir, "data")))
  spatial_image_target <- expected_spatial_image_target(
    "H5",
    "section",
    "Histology",
    "histology.png"
  )
  spatial_image <- file.path(app_dir, spatial_image_target)
  expect_true(file.exists(spatial_image))

  app <- privacy_start_app(app_dir, port, root)
  on.exit(privacy_stop_app(app), add = TRUE)
  privacy_wait_for_app(app)

  private_urls <- c(
    "/data/h5-data.crb",
    "/data/bpcells-data.crb",
    "/data/matrix.h5",
    "/data/matrix.bpcells/payload",
    "/private-data/h5-data.crb",
    "/private-data/bpcells-data.crb",
    "/private-data/matrix.h5",
    "/private-data/matrix.bpcells/payload",
    paste0("/", spatial_image_target)
  )
  statuses <- vapply(
    private_urls,
    function(path) httr::status_code(privacy_get_from_app(app, path)),
    integer(1)
  )
  expect_identical(unname(statuses), rep(404L, length(statuses)))
})

test_that("a running legacy data mapping cannot expose replacement data", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_on_os("windows")

  root <- withr::local_tempdir()
  sources <- privacy_test_sources(root)
  app_dir <- file.path(root, "app")
  privacy_write_legacy_app(app_dir)
  port <- httpuv::randomPort(host = "127.0.0.1")
  legacy_app <- privacy_start_app(app_dir, port, root)
  on.exit(privacy_stop_app(legacy_app), add = TRUE)
  privacy_wait_for_app(legacy_app)

  legacy_response <- privacy_get_from_app(legacy_app, "/data/legacy.txt")
  expect_identical(httr::status_code(legacy_response), 200L)

  createShinyApp(
    cerebro_data = sources$cerebro_data,
    result_dir = app_dir,
    overwrite = TRUE,
    port = port,
    host = "127.0.0.1",
    launch_browser = FALSE,
    quiet = TRUE,
    spatial_images = list(
      H5 = list(section = c(Histology = sources$image))
    ),
    verbose = FALSE
  )
  expect_true(legacy_app$process$is_alive())

  legacy_private_urls <- c(
    "/data/h5-data.crb",
    "/data/bpcells-data.crb",
    "/data/matrix.h5",
    "/data/matrix.bpcells/payload"
  )
  statuses <- vapply(
    legacy_private_urls,
    function(path) {
      httr::status_code(privacy_get_from_app(legacy_app, path))
    },
    integer(1)
  )
  expect_identical(unname(statuses), rep(404L, length(statuses)))
})
