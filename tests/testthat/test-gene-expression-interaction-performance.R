test_that("Gene selection settles before expression extraction starts", {
  withr::local_package("magrittr")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      list_of_genes <- function() c("A", "B", "C")
      sys.source(
        viewer_test_path("gene_expression", "obj_selected_genes.R"),
        scope
      )
      seen <- list()
      observe({
        seen[[length(seen) + 1L]] <<- as.character(
          expression_selected_genes()$genes_to_display_present
        )
      })
    },
    {
      session$setInputs(
        expression_analysis_mode = "Gene(s)",
        expression_genes_input = "A"
      )
      expect_true(all(vapply(scope$seen, identical, logical(1), "A")))
      session$setInputs(expression_genes_input = c("A", "B"))
      session$elapse(50)
      session$setInputs(expression_genes_input = c("A", "B", "C"))
      expect_true(all(vapply(scope$seen, identical, logical(1), "A")))
      session$elapse(1000)
      expect_identical(tail(scope$seen, 1)[[1]], c("A", "B", "C"))
      expect_false(any(vapply(scope$seen, identical, logical(1), c("A", "B"))))
    }
  )
})

test_that("RGB mode reuses mounted selectors without resending all genes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      getGeneNames <- function() {
        stop("the complete gene list must not be rendered")
      }
      initializations <- character()
      serverSideGeneSelector <- function(session, id, ...) {
        initializations <<- c(initializations, id)
      }
      sys.source(
        viewer_test_path("gene_expression", "UI_projection_input_type.R"),
        scope
      )
    },
    {
      session$setInputs(
        expression_analysis_mode = "Gene(s)",
        expression_projection_genes_in_separate_panels = "combined"
      )
      first <- output$expression_projection_input_type_UI$html
      for (channel in c("r", "g", "b")) {
        expect_match(
          first,
          paste0('id="expression_rgb_gene_', channel, '"'),
          fixed = TRUE
        )
      }
      expect_setequal(
        scope$initializations,
        c(
          "expression_genes_input",
          paste0("expression_rgb_gene_", c("r", "g", "b"))
        )
      )
      session$setInputs(expression_projection_genes_in_separate_panels = "rgb")
      expect_identical(output$expression_projection_input_type_UI$html, first)
      session$setInputs(
        expression_projection_genes_in_separate_panels = "combined"
      )
      expect_identical(output$expression_projection_input_type_UI$html, first)
      expect_length(scope$initializations, 4L)
    }
  )
})

test_that("Gene projection host is present before dynamic controls bind", {
  ui <- htmltools::tagQuery(viewer_ui_test_pages()$gene_expression)
  expect_equal(ui$find("#expression_projection_cell_view_host")$length(), 1L)
  expect_equal(ui$find("#expression_projection_UI")$length(), 0L)
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      availableProjections <- function() c("first", "second")
      available_trajectories <- function() character()
      preferences <- list(
        use_webgl = TRUE,
        show_hover_info_in_projections = FALSE
      )
      viewerProjectionFirstFrameCoordinates <- function(...) {
        data.frame(x = 1:2, y = 2:1)
      }
      Cerebro.options <- list(point_size = 5, point_opacity = 1)
      current_page_display_defaults <- function(...) {
        list(draw_border = FALSE, keep_square = TRUE)
      }
      current_scatter_defaults <- function() {
        list(point_size = 5, point_opacity = 1)
      }
      available_crb_files <- list(selected = "tiny")
      sys.source(
        viewer_test_path("gene_expression", "obj_projection_parameters_plot.R"),
        scope
      )
    },
    {
      session$setInputs(
        expression_projection_plotting_order = "random",
        expression_projection_point_size = 5,
        expression_projection_point_opacity = 1,
        expression_projection_point_border = FALSE,
        expression_projection_keep_square = TRUE
      )
      expect_identical(
        expression_projection_parameters_plot()$projection,
        "first"
      )
      session$setInputs(
        coordviews_shared_base = list(projection = "second"),
        expression_projection_to_display = "obsolete"
      )
      expect_identical(
        expression_projection_parameters_plot()$projection,
        "second"
      )
    }
  )
})

test_that("empty Gene selection keeps the same summary without hydrating IDs", {
  scope <- viewer_runtime_env()
  full <- data.frame(
    cell_barcode = c("a", "b", "c"),
    cluster = c("X", "X", "Y")
  )
  scope$getGroups <- function() "cluster"
  scope$getObservationUnit <- function() list(plural = "cells")
  scope$getParameters <- function() list(main_group = "cluster")
  hydrated <- 0L
  scope$getMetaData <- function() {
    hydrated <<- hydrated + 1L
    full
  }
  scope$viewerProjectionFirstFrameMetadata <- function() full["cluster"]
  selection <- NULL
  scope$expression_projection_selected_cells <- function() selection
  scope$input <- list(expression_projection_to_display = "UMAP")
  scope$output <- list()
  scope$renderUI <- function(expr) eval(substitute(expr), parent.frame())
  # Composition is checked independently; no selection means no color lookup.
  expected <- as.character(scope$cerebroSelectionSummary(
    NULL,
    "UMAP",
    metadata = full
  ))
  sys.source(
    viewer_test_path("gene_expression", "out_number_of_selected_cells.R"),
    scope
  )
  expect_identical(
    as.character(scope$output$expression_number_of_selected_cells),
    expected
  )
  expect_identical(hydrated, 0L)
  selection <- data.frame(selection_key = c("a", "b"))
  scope$reactive_colors <- function() list(cluster = c(X = "red", Y = "blue"))
  expected <- as.character(scope$cerebroSelectionSummary(
    selection,
    "UMAP",
    metadata = full
  ))
  sys.source(
    viewer_test_path("gene_expression", "out_number_of_selected_cells.R"),
    scope
  )
  expect_identical(
    as.character(scope$output$expression_number_of_selected_cells),
    expected
  )
  expect_gt(hydrated, 0L)
})

test_that("viewerProcessMemo shares gene choice caches across sessions", {
  shared <- new.env(parent = emptyenv())
  make_scope <- function() {
    env <- new.env(parent = globalenv())
    env$.viewer_gene_list_process_cache <- shared
    viewer_runtime_env(env)
    env
  }
  s1 <- make_scope()
  s2 <- make_scope()
  calls <- 0L
  memo1 <- s1$viewerProcessMemo("sorted")
  memo2 <- s2$viewerProcessMemo("sorted")
  genes <- c("Z", "A", "M")
  compute <- function() {
    calls <<- calls + 1L
    sort(genes)
  }
  expect_identical(memo1(genes, compute), c("A", "M", "Z"))
  expect_identical(memo2(genes, compute), c("A", "M", "Z"))
  expect_identical(calls, 1L)
})

test_that("expression_projection_data_to_plot emits first value without debounce stall", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      viewer_loaded_dataset_context <- function() list(key = "tiny")
      data_set <- function() list()
      expression_projection_coordinates <- shiny::reactive({
        data.frame(UMAP_1 = c(0, 1), UMAP_2 = c(1, 0))
      })
      expression_projection_parameters_plot <- shiny::reactive({
        list(
          projection = input$proj %||% "UMAP",
          is_trajectory = FALSE
        )
      })
      expression_projection_expression_levels <- shiny::reactive({
        if (nzchar(input$genes %||% "")) c(1, 2) else numeric()
      })
      expression_selected_genes <- shiny::reactive({
        list(genes_to_display_present = input$genes %||% character())
      })
      expression_projection_parameters_color <- shiny::reactive(list())
      expression_projection_trajectory <- shiny::reactive(list())
      expression_projection_display_mode <- shiny::reactive("combined")
      expression_projection_parameters_other <- list(reset_axes = FALSE)
      expression_projection_render_key <- shiny::reactive("k1")
      expression_projection_cells_to_show <- shiny::reactive(c(1L, 2L))
      expression_projection_trajectory_rows <- shiny::reactive(NULL)
      expression_projection_client_percentage <- shiny::reactive(FALSE)
      sys.source(
        viewer_test_path("gene_expression", "obj_projection_data_to_plot.R"),
        scope
      )
    },
    {
      session$setInputs(sidebar = "geneExpression", proj = "UMAP", genes = "G1")
      ## First value arrives immediately without waiting for a 50 ms timer.
      first <- expression_projection_data_to_plot()
      expect_identical(first$plot_parameters$projection, "UMAP")
      ## A lone change after the delay also arrives immediately.
      session$elapse(100)
      session$setInputs(proj = "TSNE")
      second <- expression_projection_data_to_plot()
      expect_identical(second$plot_parameters$projection, "TSNE")
    }
  )
})
