color_config_env <- new.env(parent = globalenv())
color_config_path <- viewer_test_path("color_config.R")

test_that("configured colours follow the loaded data set", {
  expect_true(file.exists(color_config_path))
  sys.source(color_config_path, envir = color_config_env)

  files <- c(
    "PBMC example" = "/data/pbmc.crb",
    "My data" = "/data/mine.crb"
  )
  configured <- list(
    "PBMC example" = list(sample = c(sample_1 = "#1f77b4")),
    "My data" = list(sample = c(a = "#111111", b = "#222222"))
  )

  expect_identical(
    color_config_env$resolve_configured_colors(
      configured,
      "/data/mine.crb",
      files
    ),
    configured[["My data"]]
  )
  expect_identical(
    color_config_env$resolve_configured_colors(
      configured,
      "/tmp/uploaded.crb",
      files
    ),
    list()
  )
})

test_that("a partial configured palette preserves default levels", {
  expect_true(file.exists(color_config_path))
  sys.source(color_config_path, envir = color_config_env)

  defaults <- c(a = "#aaaaaa", b = "#bbbbbb", c = "#cccccc")
  expect_identical(
    color_config_env$apply_configured_colors(
      defaults,
      c(b = "#000000", removed = "#ffffff")
    ),
    c(a = "#aaaaaa", b = "#000000", c = "#cccccc")
  )
})

test_that("createShinyApp rejects flat colour vectors", {
  root <- withr::local_tempdir()
  crb <- file.path(root, "dataset.crb")
  saveRDS(Cerebro$new(), crb)

  expect_error(
    createShinyApp(
      cerebro_data = c("PBMC" = crb),
      result_dir = file.path(root, "app"),
      colors = c("PBMC" = "#ff0000"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "named list"
  )
})

test_that("createShinyApp rejects invalid nested colour values", {
  root <- withr::local_tempdir()
  crb <- file.path(root, "dataset.crb")
  saveRDS(Cerebro$new(), crb)

  expect_error(
    createShinyApp(
      cerebro_data = c(PBMC = crb),
      result_dir = file.path(root, "app"),
      colors = list(
        PBMC = list(sample = c(pbmc_1 = "not-a-colour"))
      ),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "valid R colour"
  )
})

test_that("the Viewer sources configured colour support", {
  server <- paste(
    readLines(
      viewer_test_path("shiny_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  setup <- paste(
    readLines(
      viewer_test_path("color_setup.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(server, "viewer/color_config\\.R", fixed = FALSE)
  expect_match(setup, "resolve_configured_colors", fixed = TRUE)
  expect_match(setup, "apply_configured_colors", fixed = TRUE)
})

test_that("manual colours are isolated by loaded data set", {
  files <- c(A = "/data/a.crb", B = "/data/b.crb")
  configured <- list(
    A = list(group = c(a = "#aa0000", b = "#aa1111")),
    B = list(group = c(a = "#00bb00", b = "#11bb11"))
  )

  server <- function(input, output, session) {
    Cerebro.options <- list(colors = configured, crb_file_to_load = files)
    available_crb_files <- reactiveValues(selected = files[["A"]])
    data_set <- reactive(TRUE)
    getMetaData <- function() data.frame(group = c("a", "b"))
    getGroups <- function() "group"
    getGroupLevels <- function(group) c("a", "b")
    getCellCycle <- function() character()
    scope <- environment()
    sys.source(viewer_test_path("color_config.R"), envir = scope)
    sys.source(viewer_test_path("color_setup.R"), envir = scope)

    session$userData$colors <- reactive_colors
    session$userData$input_id <- color_input_id
    session$userData$select <- function(path) {
      available_crb_files$selected <- path
    }
  }

  shiny::testServer(server, {
    ids_a <- vapply(
      c("a", "b"),
      function(level) session$userData$input_id("group", level),
      character(1)
    )
    inputs_a <- stats::setNames(c("#cc0000", "#cc1111"), ids_a)
    do.call(session$setInputs, as.list(inputs_a))
    expect_identical(
      unname(session$userData$colors()$group),
      unname(inputs_a)
    )

    session$userData$select(files[["B"]])
    session$flushReact()
    ids_b <- vapply(
      c("a", "b"),
      function(level) session$userData$input_id("group", level),
      character(1)
    )

    expect_false(identical(ids_a, ids_b))
    expect_identical(
      session$userData$colors()$group,
      configured$B$group
    )
  })
})
