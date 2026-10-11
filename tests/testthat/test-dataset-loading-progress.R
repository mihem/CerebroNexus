test_that("dataset loading progress spans server work and browser presentation", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(viewer_test_path("www", "viewer-shell.js"), warn = FALSE),
    collapse = "\n"
  )
  css <- paste(
    readLines(viewer_test_path("www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, '"cerebro_dataset_load"', fixed = TRUE)
  expect_match(
    server,
    'datasetLoadProgress(dataset_label, 8, "Preparing dataset")',
    fixed = TRUE
  )
  expect_match(
    server,
    'datasetLoadProgress(dataset_label, 42, "Opening cells and metadata")',
    fixed = TRUE
  )
  expect_match(
    server,
    'datasetLoadProgress(dataset_label, 100, "Ready", done = TRUE)',
    fixed = TRUE
  )
  expect_match(javascript, 'document.addEventListener("change"', fixed = TRUE)
  expect_match(javascript, 'input.id !== "crb_file_selector"', fixed = TRUE)
  expect_match(
    javascript,
    'window.Shiny.addCustomMessageHandler("cerebro_dataset_load", update)',
    fixed = TRUE
  )
  expect_match(javascript, '".cerebro-dataset-loading-elapsed"', fixed = TRUE)
  expect_match(css, ".cerebro-dataset-loading-card", fixed = TRUE)
  expect_match(css, "place-items: center", fixed = TRUE)
})

test_that("page benchmarks can force and identify full dataset readiness", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    server,
    'input[["cerebro_benchmark_dataset_load_request"]]',
    fixed = TRUE
  )
  expect_match(server, "dataset_load_requested(TRUE)", fixed = TRUE)
  expect_match(server, 'loaded <- data_set()', fixed = TRUE)
  expect_match(server, 'identity <- viewerDatasetIdentity()', fixed = TRUE)
  expect_match(
    server,
    '"cerebro_benchmark_dataset_ready"',
    fixed = TRUE
  )
  expect_match(server, "dataset_fingerprint", fixed = TRUE)
})
