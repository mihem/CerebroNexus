viewer_path <- viewer_test_path

test_that("gene conversion table escapes cell content", {
  original <- DT::datatable
  widget <- NULL
  testthat::local_mocked_bindings(
    datatable = function(...) {
      widget <<- original(...)
      widget
    },
    .package = "DT"
  )
  shiny::testServer(
    function(input, output, session) {
      Cerebro.options <- list(cerebro_root = viewer_app_test_path())
      read.table <- function(...) {
        data.frame(symbol = '<img src=x onerror="alert(1)">', id = "ENSG1")
      }
      viewer_server_load(
        viewer_server_statements("gene_id_conversion", "server.R"),
        environment(),
        names = 'output[["gene_info"]]'
      )
    },
    {
      for (species in c("human", "mouse")) {
        session$setInputs(geneIdConversion_organism = species)
        expect_no_error(output$gene_info)
        expect_identical(attr(widget$x$options, "escapeIdx"), "true")
        expect_true(widget$x$options$scrollX)
        expect_identical(widget$x$data$symbol, '<img src=x onerror="alert(1)">')
      }
    }
  )
})

test_that("Viewer no longer depends on shinyFiles", {
  imports <- packageDescription("CerebroNexus")[["Imports"]]
  expect_false("shinyFiles" %in% trimws(strsplit(imports, ",")[[1]]))
})

test_that("Specialist group filters share inclusive empty-selection behavior", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_path("utility_functions.R"), envir = env)
  metadata <- data.frame(
    sample = c("A", "A", "B"),
    cell_type = c("T", "B", "T"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    env$cerebroGroupFilterMask(
      metadata,
      list(sample = "A", cell_type = "T")
    ),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(
    env$cerebroGroupFilterMask(
      metadata,
      list(sample = character(), cell_type = c("T", "B"))
    ),
    rep(FALSE, 3)
  )
})

test_that("Projection renders an empty filter result instead of retaining cells", {
  shiny::testServer(
    function(input, output, session) {
      sys.source(viewer_test_path("utility_functions.R"), environment())
      metadata <- data.frame(sample = factor(c("A", "B")))
      viewer_loaded_dataset_context <- function() {
        list(epoch = "s", dataset_key = "A", generation = 1)
      }
      viewerProjectionFirstFrameMetadata <- function() metadata
      overview_projection_cells_to_show <- reactive(
        if (isTRUE(input$empty)) integer() else 1:2
      )
      overview_projection_parameters_plot <- function() {
        list(color_variable = "sample", projection = "umap")
      }
      overview_projection_parameters_other <- list(reset_axes = TRUE)
      getGroups <- function() "sample"
      reactive_group_colors <- function(...) c(A = "red", B = "blue")
      viewerProjectionAsset <- function(...) NULL
      overview_projection_client_percentage <- function() FALSE
      overview_projection_data <- function() {
        metadata[overview_projection_cells_to_show(), , drop = FALSE]
      }
      overview_projection_coordinates <- function() {
        list(
          x = seq_len(nrow(overview_projection_data())),
          y = seq_len(nrow(overview_projection_data()))
        )
      }
      sys.source(
        viewer_test_path("overview", "obj_projection_data_to_plot.R"),
        environment()
      )
    },
    {
      session$setInputs(sidebar = "overview")
      expect_equal(overview_projection_data_to_plot_raw()$cell_count, 2)
      session$setInputs(empty = TRUE)
      empty <- overview_projection_data_to_plot_raw()
      expect_equal(empty$cell_count, 0)
      expect_length(empty$color_assignments, 0)
      expect_length(empty$coordinates$x, 0)
      expect_equal(nrow(empty$cells_df), 0)
      session$setInputs(empty = FALSE)
      expect_equal(overview_projection_data_to_plot_raw()$cell_count, 2)
    }
  )
})

test_that("Gene expression panels share one global numeric range", {
  env <- new.env(parent = globalenv())
  sys.source(
    viewer_path("gene_expression", "func_color_scale.R"),
    envir = env
  )

  levels <- list(GeneA = c(0, 1), GeneB = c(2, 7))
  expect_equal(env$expressionValueRange(levels), c(0, 7))
  expect_equal(env$expressionValueRange(c(0.2, 0.9)), c(0.2, 0.9))
})

test_that("Different gene colours are stable and visually distinct", {
  root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(root)) {
    root <- testthat::test_path("../../inst/viewer")
  }
  env <- new.env(parent = globalenv())
  sys.source(
    file.path(root, "gene_expression", "func_color_scale.R"),
    envir = env
  )

  genes <- paste0("Gene", seq_len(9))
  shared <- env$expressionPanelColorScales(genes, "shared", "Cerebro orange")
  different <- env$expressionPanelColorScales(
    genes,
    "different",
    "Cerebro orange"
  )

  scale_key <- function(scale) paste(capture.output(dput(scale)), collapse = "")
  expect_length(unique(vapply(shared, scale_key, character(1))), 1)
  expect_length(unique(vapply(different, scale_key, character(1))), 9)
  expect_identical(
    different,
    env$expressionPanelColorScales(genes, "different", "Cerebro orange")
  )
})
