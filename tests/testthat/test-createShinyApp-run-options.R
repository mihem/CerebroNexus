run_options_test_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  source_dir <- file.path(root, "source")
  dir.create(source_dir)
  crb <- file.path(source_dir, "dataset.crb")
  saveRDS(Cerebro$new(), crb)
  list(root = root, crb = crb)
}

run_options_build_app <- function(fixture, result_name = "app", ...) {
  result <- file.path(fixture$root, result_name)
  createShinyApp(
    cerebro_data = c("Dataset" = fixture$crb),
    result_dir = result,
    launch_browser = FALSE,
    verbose = FALSE,
    ...
  )
  result
}

run_options_parent_artifacts <- function(root) {
  list.files(
    root,
    pattern = "^\\.app-(stage-|backup-|build\\.lock)",
    all.files = TRUE,
    full.names = TRUE
  )
}

expect_invalid_run_option <- function(argument, value, prepare_calls) {
  fixture <- run_options_test_fixture()
  result <- file.path(fixture$root, "app")
  dir.create(result)
  marker <- file.path(result, "marker.txt")
  writeLines("KEEP", marker)
  before <- list.files(
    result,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
  calls_before <- prepare_calls$count
  arguments <- list(
    cerebro_data = c("Dataset" = fixture$crb),
    result_dir = result,
    max_request_size = 8000,
    port = 8080,
    host = "127.0.0.1",
    launch_browser = FALSE,
    quiet = FALSE,
    display_mode = "normal",
    verbose = FALSE
  )
  arguments[argument] <- list(value)

  expect_error(
    do.call(createShinyApp, arguments),
    argument,
    fixed = TRUE,
    info = paste("invalid case for", argument, deparse(value))
  )
  expect_identical(
    prepare_calls$count,
    calls_before,
    info = paste(argument, "must be checked before result preparation")
  )
  expect_identical(readLines(marker), "KEEP")
  expect_identical(
    list.files(
      result,
      all.files = TRUE,
      no.. = TRUE,
      recursive = TRUE,
      include.dirs = TRUE
    ),
    before
  )
  expect_length(run_options_parent_artifacts(fixture$root), 0L)
}

test_that("createShinyApp validates run options before target preparation", {
  prepare_calls <- new.env(parent = emptyenv())
  prepare_calls$count <- 0L
  testthat::local_mocked_bindings(
    .prepareBundleResultTarget = function(result_dir) {
      prepare_calls$count <- prepare_calls$count + 1L
      stop("result target preparation reached", call. = FALSE)
    },
    .package = "CerebroNexus"
  )

  invalid <- list(
    max_request_size = list(
      NULL,
      numeric(),
      c(1, 2),
      NA_real_,
      Inf,
      -Inf,
      0,
      -1,
      .Machine$double.xmax,
      "1",
      TRUE
    ),
    port = list(
      NULL,
      numeric(),
      c(8080, 8081),
      NA_real_,
      Inf,
      0,
      -1,
      65536,
      1.5,
      "8080",
      TRUE
    ),
    host = list(
      NULL,
      character(),
      c("127.0.0.1", "localhost"),
      NA_character_,
      "",
      127,
      TRUE
    ),
    launch_browser = list(
      NULL,
      logical(),
      c(TRUE, FALSE),
      NA,
      1,
      "FALSE"
    ),
    quiet = list(
      NULL,
      logical(),
      c(TRUE, FALSE),
      NA,
      0,
      "TRUE"
    ),
    show_upload_ui = list(
      NULL,
      logical(),
      c(TRUE, FALSE),
      NA,
      0,
      "TRUE"
    ),
    initial_page = list(
      character(),
      c("projection", "spatial"),
      NA_character_,
      "",
      "not-a-viewer-page",
      1,
      TRUE,
      factor("projection")
    ),
    display_mode = list(
      NULL,
      character(),
      c("auto", "normal"),
      NA_character_,
      "show",
      "AUTO",
      1,
      factor("auto")
    )
  )

  for (argument in names(invalid)) {
    for (value in invalid[[argument]]) {
      expect_invalid_run_option(argument, value, prepare_calls)
    }
  }
})

test_that("new run options preserve historical positional arguments", {
  arguments <- names(formals(createShinyApp))

  expect_identical(arguments[[14L]], "show_upload_ui")
  expect_identical(arguments[[15L]], "welcome_message")
  expect_identical(tail(arguments, 1L), "initial_page")
})

test_that("createShinyApp freezes typed run options into config", {
  skip_if_not_installed("callr")
  fixture <- run_options_test_fixture()
  request_mb <- 37.125
  port <- 61234
  host <- "run-options'host\\sentinel"
  expected_options <- list(
    port = 61234L,
    host = host,
    launch.browser = FALSE,
    quiet = TRUE,
    display.mode = "showcase"
  )
  expected_manifest <- list(
    schema_version = 1L,
    max_request_size_bytes = as.double(request_mb * 1024^2),
    shiny_app_options = expected_options
  )
  caller_options <- structure(
    list(
      TRUE,
      list(schema_version = 998L),
      list(schema_version = 999L),
      list(schema_version = 998L),
      list(schema_version = 999L),
      "keep-unnamed",
      "keep-missing-name"
    ),
    names = c(
      "exclude_trivial_metadata",
      ".bundle_run_options",
      ".bundle_run_options",
      ".bundle_backend_plan",
      ".bundle_backend_plan",
      "",
      NA_character_
    )
  )
  app <- run_options_build_app(
    fixture,
    max_request_size = request_mb,
    show_upload_ui = TRUE,
    port = port,
    host = host,
    quiet = TRUE,
    display_mode = "showcase",
    cerebro_options = caller_options
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    sum(names(config) == ".bundle_run_options", na.rm = TRUE),
    1L
  )
  expect_identical(
    sum(names(config) == ".bundle_backend_plan", na.rm = TRUE),
    1L
  )
  expect_identical(config[[".bundle_run_options"]], expected_manifest)
  expect_identical(
    config[[".bundle_backend_plan"]],
    list(
      schema_version = 1L,
      entries = list(
        "private-data/dataset.crb" = list(
          type = "embedded",
          mode = "embedded",
          location = NULL
        )
      )
    )
  )
  expect_identical(config[[which(names(config) == "")]], "keep-unnamed")
  expect_identical(
    config[[which(is.na(names(config)))]],
    "keep-missing-name"
  )
  expect_type(
    config[[".bundle_run_options"]]$max_request_size_bytes,
    "double"
  )
  expect_type(
    config[[".bundle_run_options"]]$shiny_app_options$port,
    "integer"
  )
  expect_type(
    config[[".bundle_run_options"]]$shiny_app_options$host,
    "character"
  )
  expect_type(
    config[[".bundle_run_options"]]$shiny_app_options$launch.browser,
    "logical"
  )
  expect_type(
    config[[".bundle_run_options"]]$shiny_app_options$quiet,
    "logical"
  )
  expect_type(
    config[[".bundle_run_options"]]$shiny_app_options$display.mode,
    "character"
  )

  app_file <- file.path(app, "app.R")
  app_source <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
  expect_silent(parse(file = app_file, keep.source = FALSE))
  expect_false(grepl(host, app_source, fixed = TRUE))
  expect_false(grepl(as.character(port), app_source, fixed = TRUE))
  expect_false(grepl(as.character(request_mb), app_source, fixed = TRUE))
  expect_false(grepl("CerebroNexus", app_source, fixed = TRUE))

  runtime <- callr::r(
    function(app_dir, expected_bytes) {
      setwd(app_dir)
      sentinel <- 12345
      options(shiny.maxRequestSize = sentinel)
      app <- source("app.R", local = new.env(parent = globalenv()))$value
      after_source <- getOption("shiny.maxRequestSize")
      during <- NULL
      later::later(
        function() {
          during <<- getOption("shiny.maxRequestSize")
          shiny::stopApp()
        },
        delay = 0.1
      )
      shiny::runApp(
        app,
        host = "127.0.0.1",
        port = httpuv::randomPort(),
        launch.browser = FALSE,
        quiet = TRUE
      )
      list(
        class = class(app),
        options = app$options,
        after_source = after_source,
        during = during,
        after_stop = getOption("shiny.maxRequestSize"),
        expected_bytes = expected_bytes,
        sentinel = sentinel
      )
    },
    args = list(
      app_dir = app,
      expected_bytes = expected_manifest$max_request_size_bytes
    )
  )
  expect_true("shiny.appobj" %in% runtime$class)
  expect_identical(runtime$options, expected_options)
  expect_identical(runtime$after_source, runtime$sentinel)
  expect_identical(runtime$during, runtime$expected_bytes)
  expect_identical(runtime$after_stop, runtime$sentinel)
})

test_that("closed Viewers cap request bodies while open Viewers keep the configured limit", {
  fixture <- run_options_test_fixture()
  closed_app <- run_options_build_app(
    fixture,
    result_name = "closed-request-app",
    max_request_size = 8000
  )
  small_closed_app <- run_options_build_app(
    fixture,
    result_name = "small-closed-request-app",
    max_request_size = 2
  )
  open_app <- run_options_build_app(
    fixture,
    result_name = "open-request-app",
    max_request_size = 37.125,
    show_upload_ui = TRUE
  )
  request_bytes <- function(app) {
    readRDS(file.path(app, "cerebro_config.rds"))[[
      ".bundle_run_options"
    ]]$max_request_size_bytes
  }

  expect_identical(request_bytes(closed_app), as.double(6 * 1024^2))
  expect_identical(request_bytes(small_closed_app), as.double(2 * 1024^2))
  expect_identical(request_bytes(open_app), as.double(37.125 * 1024^2))
})

test_that("show_upload_ui controls the generated Viewer upload mode", {
  fixture <- run_options_test_fixture()
  default_app <- run_options_build_app(
    fixture,
    result_name = "default-app"
  )
  closed_app <- run_options_build_app(
    fixture,
    result_name = "closed-app",
    show_upload_ui = FALSE
  )
  open_app <- run_options_build_app(
    fixture,
    result_name = "open-app",
    show_upload_ui = TRUE
  )

  expect_identical(
    readRDS(file.path(default_app, "cerebro_config.rds"))$mode,
    "closed"
  )
  expect_identical(
    readRDS(file.path(closed_app, "cerebro_config.rds"))$mode,
    "closed"
  )
  expect_false(
    readRDS(file.path(default_app, "cerebro_config.rds"))$show_upload_ui
  )
  expect_identical(
    readRDS(file.path(open_app, "cerebro_config.rds"))$mode,
    "open"
  )
  expect_true(readRDS(file.path(open_app, "cerebro_config.rds"))$show_upload_ui)
})

test_that("createShinyApp stores a validated initial Viewer page", {
  fixture <- run_options_test_fixture()
  app <- run_options_build_app(
    fixture,
    result_name = "initial-page-app",
    initial_page = "spatial"
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  server <- paste(
    readLines(file.path(app, "viewer", "shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_identical(config$initial_page, "spatial")
  expect_match(server, 'projection = "overview"', fixed = TRUE)
  expect_match(server, 'spatial = "spatial"', fixed = TRUE)
  expect_match(server, 'trekker = "trekker"', fixed = TRUE)
  expect_match(server, "viewerInitialPageDecision", fixed = TRUE)
  expect_match(server, "updateTabItems", fixed = TRUE)
})

test_that("createShinyApp accepts every supported display mode", {
  fixture <- run_options_test_fixture()

  for (index in seq_along(c("auto", "normal", "showcase"))) {
    mode <- c("auto", "normal", "showcase")[[index]]
    app <- run_options_build_app(
      fixture,
      result_name = paste0("app-mode-", index),
      display_mode = mode
    )
    manifest <- readRDS(file.path(app, "cerebro_config.rds"))[[
      ".bundle_run_options"
    ]]
    expect_identical(manifest$shiny_app_options$display.mode, mode)
  }
})

test_that("createShinyApp accepts boundary and whole-valued ports", {
  fixture <- run_options_test_fixture()
  ports <- list(1L, 65535L, 43210)

  for (index in seq_along(ports)) {
    app <- run_options_build_app(
      fixture,
      result_name = paste0("app-port-", index),
      port = ports[[index]]
    )
    manifest <- readRDS(file.path(app, "cerebro_config.rds"))[[
      ".bundle_run_options"
    ]]
    expect_identical(
      manifest$shiny_app_options$port,
      as.integer(ports[[index]])
    )
  }
})
