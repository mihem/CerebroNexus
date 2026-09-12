test_that("the 1M demo is opt-in and validates its sidecar", {
  helper <- viewer_test_path("million_cell_demo.R")
  expect_true(file.exists(helper))
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)

  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  expect_identical(env$viewerAddMillionCellDemo(original, ""), original)

  root <- tempfile("cerebro-1m-demo-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- file.path(root, "mouse.crb")
  file.create(crb)
  dir.create(file.path(root, "mouse.bpcells"))
  file.create(file.path(root, "mouse.bpcells", "shape"))

  configured <- env$viewerAddMillionCellDemo(original, crb)
  label <- "10x E18 mouse brain (1M)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$point_size[label]), 1)
  expect_equal(unname(configured$point_opacity[label]), 0.5)
  expect_equal(unname(configured$percentage_cells_to_show[label]), 10)

  unlink(file.path(root, "mouse.bpcells", "shape"))
  expect_error(
    env$viewerAddMillionCellDemo(original, crb),
    "adjacent non-empty .bpcells"
  )
})

test_that("the 1M preparation keeps unique gene symbols", {
  script <- testthat::test_path("..", "bench", "prepare_viewer_1m_data.R")
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  expect_identical(
    env$.viewer1mUniqueGeneSymbols(c("Cd3e", "Cd3e", "Ms4a1"), 3L),
    c("Cd3e", "Cd3e.1", "Ms4a1")
  )
  expect_error(
    env$.viewer1mUniqueGeneSymbols(c("Cd3e", ""), 2L),
    "non-empty"
  )
})

test_that("million-cell hover stays columnar until the browser needs it", {
  utility <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility)
  columns <- utility$cerebroProjectionHoverColumns(
    data.frame(
      cell_barcode = c("cell-1", "cell-2"),
      nUMI = c(1234, 9),
      nGene = c(321, 4),
      cluster = c("B", "A")
    ),
    groups = "cluster"
  )

  expect_identical(
    vapply(columns, `[[`, character(1), "label"),
    c("Transcripts", "Expressed genes", "cluster")
  )
  expect_identical(columns[[3L]]$levels, c("B", "A"))
  expect_identical(columns[[3L]]$values, c(0L, 1L))
})

test_that("specialist pages do not request the full linked bundle", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(engine, "function singlePayloadBundle", fixed = TRUE)
  expect_match(engine, "var vis = linkedVis;", fixed = TRUE)
  expect_no_match(engine, "linkedVis || !!singleId", fixed = TRUE)
})

test_that("specialist pages use binary transport when the browser supports it", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(utility, "cv_wire_pack_message", fixed = TRUE)
  expect_match(utility, '"cell_view_binary"', fixed = TRUE)
  expect_match(engine, "'cell_view_binary'", fixed = TRUE)
  expect_match(engine, "!ArrayBuffer.isView(data.color)", fixed = TRUE)
})

test_that("gene controls load transcriptome choices server-side", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "UI_projection_input_type.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_no_match(source, "list_of_genes()", fixed = TRUE)
  expect_match(source, "serverSideGeneSelector(", fixed = TRUE)
})

test_that("the real Viewer benchmark accepts the Canvas baseline", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_million_cell_viewer.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(
    readLines(benchmark_file, warn = FALSE),
    collapse = "\n"
  )

  expect_match(benchmark, "has_gpu_renderer <- file.exists", fixed = TRUE)
  expect_match(
    benchmark,
    "CEREBRO_VIEWER_BENCH_OVERVIEW_ONLY",
    fixed = TRUE
  )
  expect_match(benchmark, "canvas[id^=\\\"cv-cv-\\\"]", fixed = TRUE)
  expect_match(benchmark, "backend:'canvas2d'", fixed = TRUE)
  expect_match(benchmark, "cerebroLinkedViewsState", fixed = TRUE)
  expect_match(benchmark, "linked_ready_ms", fixed = TRUE)
  expect_match(benchmark, "gene_ready_ms", fixed = TRUE)
  expect_match(benchmark, "rgb_ready_ms", fixed = TRUE)
})

test_that("the cold-start benchmark measures an installed Viewer", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_million_cell_startup.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(
    readLines(benchmark_file, warn = FALSE),
    collapse = "\n"
  )

  expect_match(benchmark, "library(CerebroNexus)", fixed = TRUE)
  expect_no_match(benchmark, "load_all", fixed = TRUE)
  expect_match(benchmark, "library_ms", fixed = TRUE)
  expect_match(benchmark, "browser_load_ms", fixed = TRUE)
  expect_match(benchmark, "load_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "browser_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "process_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "CEREBRO_STARTUP_GATE_LABEL", fixed = TRUE)
  expect_match(
    benchmark,
    "CEREBRO_STARTUP_MAX_PROCESS_TO_DATA_MS",
    fixed = TRUE
  )
  expect_match(benchmark, "observed_ms >= gate_ms", fixed = TRUE)
})

test_that("optional page servers register after the first data flush", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, "deferred_viewer_server_files <- c(", fixed = TRUE)
  expect_match(server, '"marker_genes/server.R"', fixed = TRUE)
  expect_match(server, '"color_management/server.R"', fixed = TRUE)
  expect_match(server, "later::later(", fixed = TRUE)
  expect_match(server, "withReactiveDomain(session", fixed = TRUE)
  expect_match(
    server,
    "for (server_file in deferred_viewer_server_files)",
    fixed = TRUE
  )
  expect_match(server, "envir = server_scope", fixed = TRUE)
})

test_that("continuous colours keep stable paint order without comparison sort", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function quantisedOrder\\(vals, span\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "eval(fn);",
      "const values = [2, null, 1, 2, NaN, 0, 1];",
      "process.stdout.write(JSON.stringify(Array.from(quantisedOrder(values, 2))));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_identical(jsonlite::fromJSON(output), c(1L, 4L, 5L, 2L, 6L, 0L, 3L))
})

test_that("WebGPU RGB colours match the existing blend without CSS allocation", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function rgbGpuColor\\(r, g, b\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "const RGB_MIN=28, RGB_GREY_RGB=[217,219,222]; eval(fn);",
      "function old(r,g,b){const m=Math.max(r,g,b);let out;",
      "if(m<=RGB_MIN)out=RGB_GREY_RGB;else if(r>RGB_MIN&&g>RGB_MIN&&b>RGB_MIN)out=[0,0,0];",
      "else{const t=m/255;out=[r,g,b].map((v,i)=>Math.round(RGB_GREY_RGB[i]+(v/m*255-RGB_GREY_RGB[i])*t));}",
      "return out[0]|out[1]<<8|out[2]<<16|(m>RGB_MIN?0x1000000:0);}",
      "for(let r=0;r<256;r+=17)for(let g=0;g<256;g+=17)for(let b=0;b<256;b+=17)",
      "if(rgbGpuColor(r,g,b)!==old(r,g,b))process.exit(1);"
    ),
    runner
  )
  status <- system2("node", runner)

  expect_identical(status, 0L)
})

test_that("Gene projection delegates paint order without copying cell vectors", {
  runtime <- new.env(parent = globalenv())
  captured <- new.env(parent = emptyenv())
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$cerebroCellViewRender <- function(id, meta, data, hover, extra) {
    captured$data <- data
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  input <- list(
    coordinates = data.frame(x = c(3, 1, 2), y = c(6, 4, 5)),
    reset_axes = FALSE,
    expression_levels = c(30, 10, 20),
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      plot_order = "Highest expression on top",
      point_size = 1,
      point_opacity = 0.5,
      x_range = c(1, 3),
      y_range = c(4, 6),
      is_trajectory = FALSE,
      hover_info = FALSE,
      projection = "UMAP",
      n_dimensions = 2L
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_mode = "same",
      color_range = NULL,
      genes = "GeneA"
    ),
    selection_keys = c("c3", "c1", "c2"),
    hover_columns = list(),
    trajectory = list(),
    display_mode = "single",
    separate_panels = FALSE
  )

  runtime$expression_projection_update_plot(input)

  expect_identical(captured$data$x, c(3, 1, 2))
  expect_identical(captured$data$color, c(30, 10, 20))
  expect_identical(captured$data$paint_order, "highest")
  input$plot_parameters$plot_order <- "Random"
  runtime$expression_projection_update_plot(input)
  expect_identical(captured$data$paint_order, "natural")
})

test_that("hidden group filters activate after startup rendering", {
  source <- paste(
    readLines(
      viewer_test_path(
        "module",
        "group_filters",
        "group_filters_widget.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "domain$onFlushed(", fixed = TRUE)
  expect_match(source, "delay = 0.5", fixed = TRUE)
  expect_match(source, "suspendWhenHidden = FALSE", fixed = TRUE)
})
