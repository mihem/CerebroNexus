## Page code reads the method, trajectory and colour selectors through
## viewerControlValue() (server.R); here they are the inputs themselves.
trajectory_control_stubs <- function(scope) {
  eval(
    quote({
      trajectory_selected_method <- function() {
        input[["trajectory_selected_method"]]
      }
      trajectory_selected_name <- function() input[["trajectory_selected_name"]]
      trajectory_point_color <- function() input[["trajectory_point_color"]]
    }),
    scope
  )
  invisible(scope)
}

test_that("trajectory static frames fall back when filters or resources change", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      viewer_runtime_env(scope)
      sys.source(viewer_test_path("color_setup.R"), scope)
      sys.source(viewer_test_path("plotting_functions.R"), scope)
      trajectory_selection_ok <- function() TRUE
      trajectory_projection_active <- function() FALSE
      trajectory_projection_appearance <- reactiveValues(
        percentage_cells_to_show = 100
      )
      getGroups <- function() "group"
      preferences <- list(show_hover_info_in_projections = TRUE)
      resource <- list(
        cells = 2L,
        projection = list(url = "xy"),
        state = list(url = "state", levels = c("start", "end")),
        subset = list(url = "index")
      )
      viewerTrajectoryFrameAsset <- function(method, name) {
        if (isTRUE(input$canonical)) {
          return(c(resource, list(geometry_kind = "canonical_projection")))
        }
        resource
      }
      getGroupLevels <- function(group) c("A", "B")
      reactive_group_colors <- function(group) c(A = "#111111", B = "#222222")
      viewerMetadataCodesAsset <- function(name, levels) {
        if (!isTRUE(input$codes)) {
          return(NULL)
        }
        list(url = "codes", cells = 4L, levels = I(levels))
      }
      viewerPackCurrent <- function() NULL
      edges <- data.frame(
        source_dim_1 = 0,
        source_dim_2 = 1,
        target_dim_1 = 2,
        target_dim_2 = 3
      )
      viewerPackTrajectoryEdges <- function(...) edges
      trajectory_data_reactive <- function() {
        stop("must not hydrate full trajectory")
      }
      viewer_server_load(
        viewer_server_statements("trajectory", "projection_plot.R"),
        scope,
        names = c(
          "trajectory_projection_lines",
          "trajectory_active_group_filters",
          "trajectory_client_percentage",
          "trajectory_server_percentage",
          "trajectory_static_first_frame",
          "trajectory_static_frame",
          "trajectory_client_filters"
        )
      )
    },
    {
      session$setInputs(
        trajectory_point_color = "state",
        trajectory_selected_method = "m",
        trajectory_selected_name = "p"
      )
      frame <- trajectory_static_first_frame()
      expect_identical(frame$resource, resource)
      expect_identical(frame$levels, c("start", "end"))
      expect_equal(
        unlist(frame$trajectory_lines[[1]][c("x0", "y0", "x1", "y1")]),
        c(x0 = 0, y0 = 1, x1 = 2, y1 = 3)
      )
      for (url in c("xy", "state", "index")) {
        session$setInputs(
          trajectory_projection_projection_resource_failed = url
        )
        expect_null(trajectory_static_first_frame())
      }
      session$setInputs(
        trajectory_projection_projection_resource_failed = NULL,
        trajectory_projection_group_filter_group = "A"
      )
      expect_null(trajectory_static_first_frame())
      ## A filter that keeps every level (as the drawer binds) is no filter.
      session$setInputs(trajectory_projection_group_filter_group = c("B", "A"))
      expect_identical(trajectory_static_first_frame()$resource, resource)
      session$setInputs(
        trajectory_projection_group_filter_group = NULL,
        trajectory_point_color = "group"
      )
      expect_null(trajectory_static_first_frame())
      session$setInputs(trajectory_point_color = "state")
      trajectory_projection_appearance$percentage_cells_to_show <- 50
      expect_null(trajectory_static_first_frame())
      trajectory_projection_appearance$percentage_cells_to_show <- 100
      expect_identical(trajectory_static_first_frame()$resource, resource)
      ## A metadata group on a canonical frame uses its Viewer Pack codes.
      session$setInputs(trajectory_point_color = "group", canonical = TRUE)
      expect_null(trajectory_static_first_frame())
      session$setInputs(codes = TRUE)
      frame <- trajectory_static_first_frame()
      expect_identical(frame$color_variable, "group")
      expect_identical(frame$levels, c("A", "B"))
      expect_identical(unname(frame$colors), c("#111111", "#222222"))
      expect_identical(frame$resource$group$url, "codes")
      expect_identical(frame$resource$state, resource$state)
      ## With the group's Pack codes the browser filters the Pack frame.
      session$setInputs(
        trajectory_point_color = "state",
        trajectory_projection_group_filter_group = "A"
      )
      expect_identical(
        trajectory_static_first_frame()$resource$state,
        resource$state
      )
      expect_identical(as.integer(trajectory_client_filters()[[1]]$allowed), 0L)
    }
  )
})

test_that("trajectory preparation emits the first frame and coalesces rapid changes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      viewer_runtime_env(scope)
      viewer_loaded_dataset_context <- function() {
        list(epoch = "s", dataset_key = "A", generation = 1)
      }
      trajectory_selection_ok <- function() TRUE
      trajectory_data_reactive <- function() list(edges = data.frame())
      trajectory_projection_appearance <- reactiveValues(
        percentage_cells_to_show = 100,
        point_size = 3,
        point_opacity = .8,
        draw_border = TRUE,
        keep_square = FALSE
      )
      getGroups <- function() "group"
      getGroupLevels <- function(group) c("A", "B")
      trajectory_cells_reactive <- function(...) {
        data.frame(group = c("A", "B"), x = 1:2, y = 3:4)
      }
      preferences <- list(show_hover_info_in_projections = FALSE)
      trajectory_projection_cells_memo <- viewerMemoOne()
      viewerPackCurrent <- function() NULL
      viewer_server_load(
        viewer_server_statements("trajectory", "projection_plot.R"),
        scope,
        names = c(
          "trajectory_projection_lines",
          "trajectory_projection_feedback",
          "trajectory_projection_active",
          "trajectory_active_group_filters",
          "trajectory_client_percentage",
          "trajectory_server_percentage",
          "trajectory_projection_request",
          "trajectory_projection_prepared_raw",
          "trajectory_projection_prepared"
        )
      )
      seen <- list()
      observe({
        seen[[length(seen) + 1L]] <<- trajectory_projection_prepared()
      })
    },
    {
      session$setInputs(
        sidebar = "trajectory",
        trajectory_point_color = "group"
      )
      percentage <- function(frame) {
        frame$feedback$trajectory_percentage_cells_to_show
      }
      expect_equal(percentage(tail(scope$seen, 1)[[1]]), "100")
      ## Point size is a renderer patch: no new frame.
      frames <- length(scope$seen)
      trajectory_projection_appearance$point_size <- 4
      session$flushReact()
      session$elapse(1000)
      expect_length(scope$seen, frames)
      trajectory_projection_appearance$percentage_cells_to_show <- 60
      session$flushReact()
      session$elapse(50)
      trajectory_projection_appearance$percentage_cells_to_show <- 50
      session$flushReact()
      expect_equal(percentage(tail(scope$seen, 1)[[1]]), "100")
      session$elapse(1000)
      expect_equal(percentage(tail(scope$seen, 1)[[1]]), "50")
      expect_false(any(vapply(
        scope$seen,
        function(frame) identical(percentage(frame), "60"),
        logical(1)
      )))
      trajectory_projection_appearance$percentage_cells_to_show <- 100
      session$flushReact()
      session$elapse(1000)
      session$setInputs(trajectory_projection_group_filter_group = character())
      session$elapse(1000)
      expect_equal(nrow(tail(scope$seen, 1)[[1]]$cells_df), 0)
      session$setInputs(trajectory_projection_group_filter_group = c("A", "B"))
      session$elapse(1000)
      expect_equal(nrow(tail(scope$seen, 1)[[1]]$cells_df), 2)
    }
  )
})

test_that("a control binding with its value keeps the same shuffled trajectory cells", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      viewer_runtime_env(scope)
      viewer_loaded_dataset_context <- function() {
        list(epoch = "s", dataset_key = "A", generation = 1)
      }
      trajectory_selection_ok <- function() TRUE
      trajectory_data_reactive <- function() list(edges = data.frame())
      trajectory_projection_appearance <- reactiveValues(
        percentage_cells_to_show = 100,
        point_size = 3,
        point_opacity = .8,
        draw_border = TRUE,
        keep_square = FALSE
      )
      getGroups <- function() "group"
      getGroupLevels <- function(group) c("A", "B")
      cells <- data.frame(
        group = rep(c("A", "B"), 25),
        value = seq_len(50) / 10,
        x = seq_len(50),
        y = rev(seq_len(50))
      )
      trajectory_cells_reactive <- function(...) cells
      preferences <- list(show_hover_info_in_projections = FALSE)
      trajectory_projection_cells_memo <- viewerMemoOne()
      viewerPackCurrent <- function() NULL
      viewer_server_load(
        viewer_server_statements("trajectory", "projection_plot.R"),
        scope,
        names = c(
          "trajectory_projection_lines",
          "trajectory_projection_feedback",
          "trajectory_projection_active",
          "trajectory_active_group_filters",
          "trajectory_client_percentage",
          "trajectory_server_percentage",
          "trajectory_projection_request",
          "trajectory_projection_prepared_raw"
        )
      )
      seen <- list()
      observe({
        seen[[
          length(seen) + 1L
        ]] <<- trajectory_projection_prepared_raw()$cells_df
      })
    },
    {
      session$setInputs(
        sidebar = "trajectory",
        trajectory_point_color = "value"
      )
      first <- tail(scope$seen, 1)[[1]]
      ## The settings drawer's filter binds with every level: no filter.
      session$setInputs(trajectory_projection_group_filter_group = c("A", "B"))
      expect_gt(length(scope$seen), 1L)
      expect_identical(tail(scope$seen, 1)[[1]], first)
      ## A real change still builds new cells.
      session$setInputs(trajectory_projection_group_filter_group = "A")
      expect_identical(unique(tail(scope$seen, 1)[[1]]$group), "A")
    }
  )
})

test_that("trajectory consumers share row mapping and invalidate it on dataset changes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      viewer_runtime_env(scope)
      reads <- 0L
      metadata <- reactive({
        if (identical(input$revision, 2)) {
          return(data.frame(
            cell_barcode = c("b", "c", "a"),
            group = c("new-B", "new-C", "new-A"),
            value = c(80, 20, 40)
          ))
        }
        data.frame(
          cell_barcode = c("a", "b", "c"),
          group = c("T", "B", "T"),
          value = c(4, 8, 2)
        )
      })
      trajectory_data_reactive <- reactive({
        if (identical(input$revision, 2)) {
          return(list(
            meta = data.frame(
              pseudotime = c(5, 7),
              state = c("late", "early"),
              row.names = c("a", "c")
            )
          ))
        }
        list(
          meta = data.frame(
            pseudotime = c(0, 1),
            state = c("start", "end"),
            row.names = c("c", "a")
          )
        )
      })
      viewerPackCurrent <- function() NULL
      viewerPackTrajectoryIndex <- function(...) NULL
      getMetaData <- function() {
        reads <<- reads + 1L
        metadata()
      }
      viewerProjectionFirstFrameMetadata <- metadata
      viewer_server_load(
        viewer_server_statements("trajectory", "server.R"),
        scope,
        names = c("trajectory_row_index_reactive", "trajectory_cells_reactive")
      )
    },
    {
      session$setInputs(
        revision = 1,
        trajectory_selected_method = "m",
        trajectory_selected_name = "p"
      )
      expect_identical(scope$reads, 0L)
      groups <- trajectory_cells_reactive("group")
      values <- trajectory_cells_reactive("value")
      expect_identical(groups$cell_index, c(3L, 1L))
      expect_identical(groups$group, c("T", "T"))
      expect_identical(values$value, c(2, 4))
      expect_identical(rownames(values), c("c", "a"))
      expect_identical(scope$reads, 1L)
      session$setInputs(revision = 2)
      changed <- trajectory_cells_reactive(c("group", "value"))
      expect_identical(rownames(changed), c("a", "c"))
      expect_identical(changed$cell_index, c(3L, 2L))
      expect_identical(changed$value, c(40, 20))
      expect_identical(changed$group, c("new-A", "new-C"))
      expect_identical(changed$pseudotime, c(5, 7))
      expect_identical(changed$state, c("late", "early"))
      expect_identical(trajectory_cells_reactive("value")$value, c(40, 20))
      expect_identical(scope$reads, 2L)
      session$setInputs(revision = 1)
      expect_identical(trajectory_cells_reactive("value"), values)
      expect_identical(scope$reads, 3L)
    }
  )
})

test_that("supplementary trajectory controls appear only after projection and visibility", {
  panels <- list(
    distribution_along_pseudotime = c(
      "trajectory_distribution_along_pseudotime_UI",
      "trajectory_distribution_section_visible",
      "trajectory_distribution_along_pseudotime_select_variable"
    ),
    states_by_group = c(
      "trajectory_states_by_group_UI",
      "trajectory_states_section_visible",
      "states_by_group_select_other_group"
    ),
    expression_metrics = c(
      "trajectory_expression_metrics_UI",
      "trajectory_expression_section_visible",
      "trajectory_expression_metrics_tabs"
    )
  )
  for (file in names(panels)) {
    panel <- panels[[file]]
    scope <- viewer_ui_test_env()
    trajectory_control_stubs(scope)
    sys.source(viewer_test_path("plotting_functions.R"), scope)
    scope$output <- list()
    scope$renderUI <- function(expr) eval(substitute(expr), parent.frame())
    ## Supplementary panels read the author display defaults for their page; the
    ## deployment here configures none, so the builder defaults apply.
    sys.source(viewer_test_path("core", "viewer_content_contract.R"), scope)
    scope$current_page_display_defaults <- function(page) {
      viewerPageDisplayDefaults(list(), "tiny", page)
    }
    scope$trajectory_projection_sent <- function() ready
    scope$trajectory_selection_ok <- function() TRUE
    scope$getVariableToCompareChoices <- scope$getGroups <- function() "group"
    scope$getCellCycle <- function() character()
    scope$hasMitoColumn <- scope$hasRiboColumn <- scope$hasEryColumn <- function() {
      FALSE
    }
    scope$tabBox <- shinydashboard::tabBox
    scope$input <- list(
      trajectory_selected_method = "m",
      trajectory_selected_name = "p"
    )
    statements <- viewer_server_statements("trajectory", paste0(file, ".R"))
    render <- function() {
      viewer_server_load(
        statements,
        scope,
        names = paste0('output[["', panel[1], '"]]')
      )
    }
    ready <- FALSE
    scope$input[[panel[2]]] <- TRUE
    expect_error(render(), class = "shiny.silent.error")
    ready <- TRUE
    scope$input[[panel[2]]] <- FALSE
    expect_error(render(), class = "shiny.silent.error")
    scope$input[[panel[2]]] <- TRUE
    render()
    tags <- htmltools::tagQuery(shiny::div(scope$output[[panel[1]]]))
    expect_equal(tags$find(paste0("#", panel[3]))$length(), 1L, info = file)
  }
})

test_that("trajectory expression metric outputs keep their IDs, messages and cache keys", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      cache_keys <- list()
      violins <- list()
      ## The metric outputs are created in a top-level loop, which
      ## viewer_server_load() skips, so source the whole file.
      cachePlot <- function(x, ...) {
        scope$cache_keys[[length(scope$cache_keys) + 1L]] <- rlang::enquos(...)
        x
      }
      viewerProjectionFirstFrameMetadata <- function() data.frame(nUMI = 1)
      hasMitoColumn <- hasEryColumn <- function() TRUE
      hasRiboColumn <- function() FALSE
      getMitoColumn <- function() "percent.mt"
      getRiboColumn <- function() NULL
      getEryColumn <- function() "percent.hb"
      trajectory_selection_ok <- function() TRUE
      trajectory_data_reactive <- function() {
        list(meta = data.frame(state = factor(c("early", "late"))))
      }
      cerebro_group_colors <- function(n) rep("#000000", n)
      trajectory_cells_reactive <- function(column) data.frame(state = "early")
      sys.source(viewer_test_path("plotting_functions.R"), scope)
      cerebroViolinData <- function(table, metric, y_title, mode, ...) {
        scope$violins[[metric]] <- c(y_title = y_title, mode = mode)
        list(type = "violin", groups = I("early"))
      }
      available_crb_files <- reactiveValues(selected = "demo.crb")
      cerebroRegisterInfo <- function(...) invisible(NULL)
      sys.source(viewer_test_path("trajectory", "expression_metrics.R"), scope)
    },
    {
      session$setInputs(
        trajectory_selected_method = "m",
        trajectory_selected_name = "p"
      )
      metrics <- c("nUMI", "nGene", "percent_mt", "percent_ribo", "percent_ery")
      shown <- c(
        nUMI = "plot",
        nGene = "text",
        percent_mt = "plot",
        percent_ribo = "text",
        percent_ery = "plot"
      )
      for (metric in metrics) {
        id <- paste0("trajectory_states_", metric)
        expect_match(
          as.character(output[[paste0(id, "_UI")]]$html),
          paste0('id="', id, "_", shown[[metric]], '"'),
          fixed = TRUE,
          info = metric
        )
      }
      expect_identical(
        output$trajectory_states_nUMI_text,
        "Column with number of transcript per cell not available."
      )
      expect_identical(
        output$trajectory_states_nGene_text,
        "Column with number of expressed genes per cell not available."
      )
      expect_identical(
        output$trajectory_states_percent_mt_text,
        "Column with percentage of mitochondrial expression not available."
      )
      expect_identical(
        output$trajectory_states_percent_ribo_text,
        "Column with percentage of ribosomal expression not available."
      )
      expect_identical(
        output$trajectory_states_percent_ery_text,
        "Column with percentage of erythrocyte/hemoglobin expression not available."
      )

      for (metric in c("nUMI", "nGene", "percent_mt", "percent_ery")) {
        output[[paste0("trajectory_states_", metric, "_plot")]]
      }
      expect_identical(
        scope$violins,
        list(
          nUMI = c(y_title = "Number of transcripts", mode = "integer"),
          nGene = c(y_title = "Number of expressed genes", mode = "integer"),
          percent.mt = c(
            y_title = "Percentage of transcripts",
            mode = "percent"
          ),
          percent.hb = c(
            y_title = "Percentage of transcripts",
            mode = "percent"
          )
        )
      )
      expect_error(
        output$trajectory_states_percent_ribo_plot,
        class = "shiny.silent.error"
      )

      keys <- shiny::isolate(lapply(scope$cache_keys, function(quos) {
        unname(lapply(quos, rlang::eval_tidy))
      }))
      expect_identical(
        keys,
        lapply(metrics, function(metric) list("m", "p", metric, "demo.crb"))
      )
    }
  )
})

test_that("with a Viewer Pack, trajectory % of cells is left to the browser", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      viewer_runtime_env(scope)
      viewer_loaded_dataset_context <- function() {
        list(epoch = "s", dataset_key = "A", generation = 1)
      }
      trajectory_selection_ok <- function() TRUE
      trajectory_data_reactive <- function() list(edges = data.frame())
      trajectory_projection_appearance <- reactiveValues(
        percentage_cells_to_show = 100,
        point_size = 3,
        point_opacity = .8,
        draw_border = TRUE,
        keep_square = FALSE
      )
      getGroups <- function() "group"
      getGroupLevels <- function(group) c("A", "B")
      cells <- data.frame(
        group = rep(c("A", "B"), 25),
        x = seq_len(50),
        y = rev(seq_len(50))
      )
      trajectory_cells_reactive <- function(...) cells
      preferences <- list(show_hover_info_in_projections = FALSE)
      trajectory_projection_cells_memo <- viewerMemoOne()
      viewerPackCurrent <- function() list(canonical_order = TRUE)
      viewer_server_load(
        viewer_server_statements("trajectory", "projection_plot.R"),
        scope,
        names = c(
          "trajectory_projection_lines",
          "trajectory_projection_feedback",
          "trajectory_projection_active",
          "trajectory_active_group_filters",
          "trajectory_client_percentage",
          "trajectory_server_percentage",
          "trajectory_client_percentage_fields",
          "trajectory_projection_request",
          "trajectory_projection_prepared_raw"
        )
      )
      seen <- list()
      observe({
        seen[[length(seen) + 1L]] <<- trajectory_projection_prepared_raw()
      })
    },
    {
      session$setInputs(
        sidebar = "trajectory",
        trajectory_point_color = "group"
      )
      first <- tail(scope$seen, 1)[[1]]
      expect_equal(nrow(first$cells_df), 50)
      trajectory_projection_appearance$percentage_cells_to_show <- 30
      session$flushReact()
      ## Every cell stays in the frame, which is not prepared again.
      expect_length(scope$seen, 1L)
      expect_equal(first$request$percentage, 100)
      expect_equal(
        isolate(scope$trajectory_projection_feedback())$trajectory_percentage_cells_to_show,
        "30"
      )
      expect_identical(
        isolate(scope$trajectory_client_percentage_fields()),
        list(client_percentage = TRUE, percentage_cells_to_show = 30)
      )
    }
  )
})

test_that("a trajectory frame that failed to send is sent on the next run", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      trajectory_control_stubs(scope)
      context <- list(epoch = "s", dataset_key = "A", generation = 1)
      viewer_loaded_dataset_context <- function() context
      viewerDatasetContextEqual <- function(a, b) identical(a, b)
      trajectory_projection_active <- function() TRUE
      trajectory_projection_parameters_other <- reactiveValues(
        reset_axes = TRUE
      )
      trajectory_projection_appearance <- reactiveValues(
        point_size = 3,
        point_opacity = 1,
        group_labels = FALSE,
        draw_border = FALSE,
        keep_square = FALSE
      )
      trajectory_static_first_frame <- function() {
        list(
          resource = list(cells = 2L),
          color_variable = "state",
          levels = "a",
          colors = c(a = "#000000"),
          hover = FALSE,
          trajectory_lines = list()
        )
      }
      trajectory_projection_feedback <- function() list()
      trajectory_projection_point_line <- function(...) list()
      trajectory_client_percentage_fields <- function() list()
      trajectory_projection_sent <- reactiveVal(FALSE)
      cerebroSendViewFeedback <- function(...) invisible(NULL)
      attempts <- 0L
      cerebroCellViewRender <- function(...) {
        attempts <<- attempts + 1L
        ## The first send stops part-way (e.g. a req() in the payload).
        req(attempts > 1L)
        attempts
      }
      viewer_server_load(
        viewer_server_statements("trajectory", "projection_plot.R"),
        scope,
        names = c(
          "trajectory_projection_last",
          "trajectory_projection_frame_key"
        ),
        events = "trajectory_projection_render_request"
      )
    },
    {
      session$setInputs(
        trajectory_percentage_cells_to_show = 100,
        trajectory_projection_render_request = list(
          nonce = 1,
          dataset_context = context
        )
      )
      expect_identical(scope$attempts, 1L)
      expect_null(scope$trajectory_projection_last$token)
      ## The same request and frame again: the failed frame is sent now.
      session$setInputs(trajectory_percentage_cells_to_show = 90)
      expect_identical(scope$attempts, 2L)
      expect_identical(scope$trajectory_projection_last$token, 2L)
      ## A frame that was sent is not sent again.
      session$setInputs(trajectory_percentage_cells_to_show = 80)
      expect_identical(scope$attempts, 2L)
    }
  )
})
