test_that("IR error handling preserves validation and renders useful failures", {
  scope <- new.env(parent = asNamespace("shiny"))
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "server.R"),
      "safeRenderPlot"
    ),
    scope
  )
  expect_error(
    scope$safeRenderPlot(shiny::req(FALSE)),
    class = "shiny.silent.error"
  )
  expect_error(
    scope$safeRenderPlot(stop("figure margins too large")),
    class = "shiny.silent.error"
  )
  for (message in c(
    "subscript out of bounds",
    "attempt to select less than one element in get1index",
    "optimization failed",
    "disk unavailable"
  )) {
    plot <- suppressMessages(scope$safeRenderPlot(stop(message)))
    text <- ggplot2::ggplot_build(plot)$data[[1]]$label
    expected <- if (message == "disk unavailable") {
      "disk unavailable"
    } else if (message == "optimization failed") {
      "fitting failed"
    } else {
      "No data to display"
    }
    expect_match(text, expected, fixed = TRUE)
  }
})

test_that("IR renderers update parameters and isolate their session caches", {
  # Keep the real render expression and bindCache; replace only graphics-device
  # serialization and the external scRepertoire call to observe its inputs.
  render_value <- function(expr, ...) {
    shiny::reactive(substitute(expr), env = parent.frame(), quoted = TRUE)
  }
  testthat::local_mocked_bindings(
    renderPlotly = render_value,
    .package = "plotly"
  )
  calls <- list()
  record <- function(name) {
    function(...) {
      args <- list(...)
      calls[[length(calls) + 1L]] <<- list(name = name, args = args)
      ggplot2::ggplot(data.frame(x = 1:2, y = 2:3), ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    }
  }
  functions <- c(
    "clonalAbundance",
    "clonalHomeostasis",
    "clonalSizeDistribution"
  )
  mocks <- setNames(lapply(functions, record), functions)
  do.call(
    testthat::local_mocked_bindings,
    c(mocks, list(.package = "scRepertoire", .env = environment()))
  )
  server <- function(input, output, session) {
    scope <- environment()
    output <- new.env()
    renderPlot <- render_value
    `%>%` <- dplyr::`%>%`
    available_crb_files <- reactiveValues(selected = "A")
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "server.R"),
      scope,
      names = c("ir_bindCache", "safeRenderPlot")
    )
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "data.R"),
      scope,
      names = c(
        "ir_params",
        "ir_param",
        "ir_order_by",
        "IR_CLONE_SIZE_NAMES",
        "IR_CLONE_SIZE_DEFAULT",
        "ir_clone_size"
      )
    )
    req_plot_space <- function(...) NULL
    req_scRepertoire <- function() NULL
    getGroups <- function() "sample"
    ir_data <- function() {
      list(
        sample = data.frame(barcode = paste0(available_crb_files$selected, 1:2))
      )
    }
    ir_render_ggplotly <- function(expr, ...) expr
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "visualizations.R"),
      scope,
      names = c("IR_PALETTE", paste0("output$ir_plot_", functions))
    )
  }
  exercise <- function() {
    shiny::testServer(server, {
      session$setInputs(
        ir_cloneCall = "gene",
        ir_chain = "TRB",
        ir_groupBy = "",
        ir_p_order_by = ""
      )
      value <- function(name) scope$output[[paste0("ir_plot_", name)]]()
      latest <- function() tail(calls, 1)[[1]]$args
      value("clonalAbundance")
      expect_identical(latest()$chain, "TRB")
      expect_null(latest()$group.by)
      baseline <- length(calls)
      session$setInputs(
        ir_p_clone_size = "0.01,0.05,0.1,0.5,1",
        ir_d_point_size = 12
      )
      value("clonalAbundance")
      expect_length(calls, baseline)
      session$setInputs(
        ir_chain = "TRA",
        ir_groupBy = "sample",
        ir_p_scale = TRUE,
        ir_p_order_by = "alphanumeric"
      )
      value("clonalAbundance")
      expect_identical(latest()$chain, "TRA")
      expect_identical(latest()$group.by, "sample")
      expect_true(latest()$scale)
      expect_identical(latest()$order.by, "alphanumeric")
      scope$available_crb_files$selected <- "B"
      session$flushReact()
      value("clonalAbundance")
      expect_identical(latest()[[1]][[1]]$barcode, c("B1", "B2"))
      value("clonalHomeostasis")
      expect_equal(unname(latest()$cloneSize), c(.01, .05, .1, .5, 1))
      session$setInputs(ir_p_sd_threshold = -10)
      value("clonalSizeDistribution")
      expect_identical(latest()$cloneCall, "strict")
      expect_equal(latest()$threshold, 1)
      session$setInputs(ir_p_sd_threshold = 3, ir_p_sd_method = "complete")
      value("clonalSizeDistribution")
      expect_equal(latest()$threshold, 3)
      expect_identical(latest()$method, "complete")
    })
  }
  exercise()
  first_session <- length(calls)
  exercise()
  expect_equal(length(calls), first_session * 2L)
})

test_that("IR metadata joins preserve samples and only hydrate requested columns", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      reads <- 0L
      ir_data_raw <- function() {
        list(
          s1 = data.frame(barcode = c("c2", "c1")),
          s2 = data.frame(barcode = "c3")
        )
      }
      getMetaData <- function() {
        reads <<- reads + 1L
        data.frame(
          cell_barcode = c("c1", "c2", "c3"),
          condition = c("X", "Y", "X"),
          unused = 1:3
        )
      }
      viewer_server_load(
        viewer_server_statements("immune_repertoire", "data.R"),
        scope,
        names = c("ir_annotate_metadata", "ir_data_annotated", "ir_data")
      )
      ir_requested_metadata_columns <- reactive(input$fields %||% character())
    },
    {
      expect_identical(ir_data(), ir_data_raw())
      expect_equal(scope$reads, 0)
      session$setInputs(fields = "condition")
      result <- ir_data()
      expect_named(result, c("s1", "s2"))
      expect_named(result$s1, c("barcode", "condition"))
      expect_identical(result$s1$condition, c("Y", "X"))
      expect_equal(scope$reads, 1)
      session$setInputs(fields = character())
      expect_identical(ir_data(), ir_data_raw())
      expect_equal(scope$reads, 1)
    }
  )
})

test_that("Clonal projection paints before drawer binding and defers cell identities", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(viewer_test_path("clone_contract.R"), scope)
      context <- list(epoch = "s", dataset_key = "A", generation = 1)
      viewer_loaded_dataset_context <- function() context
      ir_receptor_types <- function() "TCR"
      availableProjections <- function() "umap"
      ir_umap_cells_to_show <- function() NULL
      current_scatter_defaults <- function() {
        list(point_size = 7, point_opacity = .6, percentage_cells_to_show = 100)
      }
      ## The clonal renderer reads the author display defaults through a
      ## server-level helper; this harness answers "no configured defaults".
      current_page_display_defaults <- function(page) list()
      viewerDatasetIdentity <- function() list(fingerprint = "A")
      builds <- barcode_reads <- 0L
      ir_data_annotated <- function() NULL
      ir_clonal_umap_data <- function(projection, receptor, clone_call, ...) {
        builds <<- builds + 1L
        stopifnot(projection == "umap", receptor == "TCR", clone_call == "gene")
        list(
          x = list(c(1, 2)),
          y = list(c(3, 4)),
          trace_codes = 1L,
          coloured_key = c(2L, 1L),
          coloured_rows = list(1:2),
          key_type = "cell_index"
        )
      }
      ir_clonal_umap_barcodes <- function(data) {
        barcode_reads <<- barcode_reads + 1L
        c("c1", "c2")[data$cell_index]
      }
      sent <- list()
      cerebroCellViewRender <- function(id, meta, data, hover, ...) {
        sent[[length(sent) + 1L]] <<- c(
          list(id = id, meta = meta, data = data, hover = hover),
          list(...)
        )
      }
      viewer_server_load(
        viewer_server_statements("immune_repertoire", "data.R"),
        scope,
        names = c("ir_param", "IR_CLONE_LABELS")
      )
      viewer_server_load(
        viewer_server_statements("immune_repertoire", "visualizations.R"),
        scope,
        names = c(
          "IR_EXPANSION_COLORS",
          "ir_projection_axis_names",
          "ir_projection_display_name",
          "ir_clonal_umap_last_request",
          "ir_clonal_umap_data_memo"
        ),
        events = "ir_clonalUMAP_projection_render_request"
      )
    },
    {
      session$setInputs(
        sidebar = "overview",
        ir_tabs = "Clonal UMAP",
        ir_clonalUMAP_projection_render_request = list(
          nonce = 1,
          dataset_context = context
        )
      )
      expect_length(scope$sent, 0)
      session$setInputs(sidebar = "immune_repertoire")
      expect_length(scope$sent, 1)
      frame <- scope$sent[[1]]
      expect_equal(frame$data$point_size, 7)
      expect_equal(frame$data$point_opacity, .6)
      expect_identical(frame$data$x, list(c(1, 2)))
      expect_null(frame$data$selection_key)
      expect_equal(scope$barcode_reads, 0)
      expect_identical(frame$deferred_aux()$selection_key, list(c("c2", "c1")))
      expect_equal(scope$barcode_reads, 1)
      session$setInputs(ir_cloneCall = "strict")
      expect_equal(scope$builds, 1)
      session$setInputs(
        ir_clonalUMAP_group_labels = FALSE,
        ir_clonalUMAP_point_border = TRUE
      )
      expect_length(scope$sent, 2)
      expect_true(scope$sent[[2]]$meta$appearance$draw_border)
      expect_false(scope$sent[[2]]$meta$appearance$group_labels)
      ## Appearance-only changes repaint without rebuilding the coordinates.
      expect_equal(scope$builds, 1)
      session$setInputs(ir_p_umap_group_by = "sample")
      expect_length(scope$sent, 2)
    }
  )
})
