test_that("trajectory static frames fall back when filters or resources change", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(viewer_test_path("color_setup.R"), scope)
      sys.source(viewer_test_path("plotting_functions.R"), scope)
      trajectory_selection_ok <- function() TRUE
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
          "trajectory_static_first_frame"
        )
      )
    },
    {
      session$setInputs(
        coordviews_wire_supported = TRUE,
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
      session$setInputs(
        trajectory_projection_group_filter_group = NULL,
        trajectory_point_color = "group"
      )
      expect_null(trajectory_static_first_frame())
      session$setInputs(
        trajectory_point_color = "state",
        coordviews_wire_supported = FALSE
      )
      expect_null(trajectory_static_first_frame())
      session$setInputs(coordviews_wire_supported = TRUE)
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
    }
  )
})

test_that("trajectory preparation emits the first frame and coalesces rapid changes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
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
      trajectory_cells_reactive <- function(...) {
        data.frame(group = c("A", "B"), x = 1:2, y = 3:4)
      }
      preferences <- list(show_hover_info_in_projections = FALSE)
      viewer_server_load(
        viewer_server_statements("trajectory", "projection_plot.R"),
        scope,
        names = c(
          "trajectory_projection_lines",
          "trajectory_projection_active",
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

test_that("trajectory consumers share row mapping and invalidate it on dataset changes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
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
      plotlyViolin <- function(table, metric, y_title, mode, ...) {
        scope$violins[[metric]] <- c(y_title = y_title, mode = mode)
        plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers")
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
          percent.mt = c(y_title = "Percentage of transcripts", mode = "percent"),
          percent.hb = c(y_title = "Percentage of transcripts", mode = "percent")
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
