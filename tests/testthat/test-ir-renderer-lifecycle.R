test_that("IR error handling preserves validation and renders useful failures", {
  scope <- ir_scope(
    c("ir_plot_cache_state", "ir_plot_failed", "safeRenderPlot"),
    "server.R"
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
  skip_if_not_installed("scRepertoire")
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
      names = c(
        "ir_dataset_cache_key",
        "ir_plot_cache_state",
        "ir_plot_failed",
        "ir_plot_cache",
        "ir_bindCache",
        "safeRenderPlot"
      )
    )
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "data.R"),
      scope,
      names = c(
        "ir_dataset_process_key",
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
    ir_encoded <- function() {
      list(samples = paste0(available_crb_files$selected, 1:2))
    }
    ir_row_values <- function(encoded, name, ...) name
    ir_render_ggplotly <- function(expr, ...) expr
    ## Abundance counts the clones in the app: record what it counts from and
    ## what it draws.
    ir_abundance_counts_for <- function(pars, encoded) {
      calls[[length(calls) + 1L]] <<- list(
        name = "abundance_counts",
        args = c(pars, list(data = encoded))
      )
      data.frame(sample = "s", abundance = 1L, n_clones = 1L)
    }
    ## Homeostasis is counted in the app: record the clone sizes it draws.
    ir_clonecall_col <- function(x) x
    ir_homeostasis_counts <- function(...) {
      data.frame(sample = "s", abundance = 1L, n_clones = 1L)
    }
    ## SizeDist clusters the exact profiles: record the linkage it draws.
    ir_size_distribution_profile <- function() {
      list(threshold = input$ir_p_sd_threshold)
    }
    ir_size_profile_plot <- function(profiles, method) {
      calls[[length(calls) + 1L]] <<- list(
        name = "size_profile_plot",
        args = c(profiles, list(method = method))
      )
      profiles
    }
    ir_homeostasis_plot <- function(counts, clone_size, ...) {
      calls[[length(calls) + 1L]] <<- list(
        name = "homeostasis_plot",
        args = list(cloneSize = clone_size)
      )
      counts
    }
    ir_clonal_abundance_plotly <- function(counts, ...) {
      force(counts)
      calls[[length(calls) + 1L]] <<- list(
        name = "abundance_plot",
        args = list(...)
      )
      counts
    }
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "visualizations.R"),
      scope,
      names = c(
        "IR_PALETTE",
        "ir_abundance_counts",
        paste0("output$ir_plot_", functions)
      )
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
      named <- function(name) {
        Filter(function(call) identical(call$name, name), calls)
      }
      last_counts <- function() tail(named("abundance_counts"), 1)[[1]]$args
      value("clonalAbundance")
      expect_identical(last_counts()$chain, "TRB")
      expect_null(last_counts()$groupBy)
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
      expect_identical(last_counts()$chain, "TRA")
      expect_identical(last_counts()$groupBy, "sample")
      expect_true(latest()$scale)
      expect_identical(latest()$order_by, "alphanumeric")
      expect_identical(latest()$legend, "sample")
      ## Scale and order redraw the same counts.
      counted <- length(named("abundance_counts"))
      session$setInputs(ir_p_scale = FALSE, ir_p_order_by = "")
      value("clonalAbundance")
      expect_false(latest()$scale)
      expect_length(named("abundance_counts"), counted)
      scope$available_crb_files$selected <- "B"
      session$flushReact()
      value("clonalAbundance")
      expect_identical(last_counts()$data$samples, c("B1", "B2"))
      value("clonalHomeostasis")
      expect_equal(unname(latest()$cloneSize), c(.01, .05, .1, .5, 1))
      session$setInputs(ir_p_sd_threshold = -10)
      value("clonalSizeDistribution")
      expect_identical(latest()$method, "ward.D2")
      expect_equal(latest()$threshold, -10)
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

test_that("Paired Scatter readiness reads the repertoire only on its tabs", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      reads <- 0L
      ir_compare_groups <- reactive({
        reads <<- reads + 1L
        c("a", "b")
      })
      ir_sample_meta <- reactive({
        reads <<- reads + 1L
        NULL
      })
      viewerPublishChanged <- function(value, current) {
        if (!identical(isolate(value()), current)) value(current)
      }
      ## The readiness observer and the two that update the pair controls.
      viewer_server_load(
        viewer_server_statements("immune_repertoire", "visualizations.R"),
        scope,
        names = c("ir_pair_tab_shown", "ir_paired_scatter_ready"),
        ## ... and the one that moves Y off the X group.
        events = c("ir_pair_tab_shown", "ir_pair_x_group")
      )
    },
    {
      session$setInputs(ir_tabs = "Clonal UMAP")
      session$flushReact()
      expect_equal(scope$reads, 0)
      expect_null(ir_paired_scatter_ready())
      session$setInputs(ir_tabs = "Paired Scatter")
      session$flushReact()
      expect_true(scope$reads > 0)
      expect_true(ir_paired_scatter_ready())
    }
  )
})

## Server scope for the clonal projection observer.
clonal_projection_server <- function(names_resource = NULL) {
  function(input, output, session) {
    scope <- environment()
    viewer_runtime_env(scope)
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
    ## The renderer reads the tab through viewerControlValue() (tabs.R).
    ir_tab <- function() input[["ir_tabs"]]
    viewerDatasetIdentity <- function() list(fingerprint = "A")
    builds <- barcode_reads <- 0L
    viewerCellNamesResource <- function() names_resource
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
      names = c(
        "ir_param",
        "IR_CLONE_LABELS",
        "ir_clonecall_col"
      )
    )
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "visualizations.R"),
      scope,
      names = c(
        "IR_EXPANSION_COLORS",
        "ir_projection_axis_names",
        "ir_projection_display_name",
        "ir_clonal_umap_last_request",
        "ir_clonal_umap_drawn_appearance",
        "ir_clonal_umap_appearance",
        "ir_clonal_umap_data_memo"
      ),
      events = c(
        "ir_clonalUMAP_projection_render_request",
        "ir_clonalUMAP_point_border"
      )
    )
  }
}

test_that("Clonal projection paints before drawer binding and defers cell identities", {
  shiny::testServer(
    clonal_projection_server(),
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
      ## Appearance is a browser patch: no new frame, no rebuilt coordinates.
      expect_length(scope$sent, 1)
      drawn <- ir_clonal_umap_drawn_appearance()$values
      expect_true(drawn$draw_border)
      expect_false(drawn$group_labels)
      expect_equal(scope$builds, 1)
    }
  )
})

test_that("Clonal projection sends canonical rows when the cell names are a resource", {
  resource <- list(protocol = "canonical-cell-names-v1", url = "names.bin.gz")
  shiny::testServer(
    clonal_projection_server(names_resource = resource),
    {
      session$setInputs(
        sidebar = "immune_repertoire",
        ir_tabs = "Clonal UMAP",
        ir_clonalUMAP_projection_render_request = list(
          nonce = 1,
          dataset_context = context
        )
      )
      aux <- scope$sent[[1]]$deferred_aux()
      ## cell_index keys 2 and 1, sent 0-based; no names are looked up.
      expect_identical(unclass(aux$selection_index[[1]]), c(1L, 0L))
      expect_identical(aux$cell_names_resource, resource)
      expect_null(aux$selection_key)
      expect_null(aux$hover$text)
      expect_equal(scope$barcode_reads, 0)
    }
  )
})
