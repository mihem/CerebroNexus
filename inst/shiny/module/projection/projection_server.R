projection_server <- function(id, projection_type = "projections") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define data accessors based on type
    if (projection_type == "spatial") {
      get_available <- function() availableSpatial()
      get_data <- function(name) getSpatialData(name)$coordinates
    } else {
      get_available <- function() availableProjections()
      get_data <- function(name) getProjection(name)
    }

    # Source helpers
    if (exists("Cerebro.options") && !is.null(Cerebro.options[["cerebro_root"]])) {
      source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/module/projection/projection_helpers.R"), local = TRUE)
    }

    ##--------------------------------------------------------------------------##
    ## UI elements to set main parameters for the projection.
    ##--------------------------------------------------------------------------##
    output[["main_parameters_UI"]] <- renderUI({
      exclude_trivial <- FALSE
      if (exists('Cerebro.options') && !is.null(Cerebro.options[['exclude_trivial_metadata']])) {
        exclude_trivial <- Cerebro.options[['exclude_trivial_metadata']]
      }

      if (exclude_trivial == TRUE) {
        metadata_cols <- getGroups()
      } else {
        metadata_cols <- colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
      }

      available_opts <- get_available()

      ## prepare background image choices
      background_choices <- c("No Background")
      spatial_images     <- Cerebro.options[["spatial_images"]]
      selected_crb_file  <- available_crb_files$selected

      if (exists("Cerebro.options") && !is.null(spatial_images) &&  exists("available_crb_files") && !is.null(selected_crb_file)) {
        match_idx <- which(available_crb_files$files == selected_crb_file)
        if (length(match_idx) > 0) {
          current_name <- names(available_crb_files$files)[match_idx[1]]
          if (!is.null(current_name) && current_name %in% names(spatial_images)) {
            background_choices <- c("No Background", spatial_images[[current_name]])
          }
        }
      }

      # Build namespaced input IDs for conditionalPanel
      plot_type_id <- ns("plot_type")

      tagList(
        selectInput(
          ns("to_display"),
          label = "Projection",
          choices = available_opts
        ),
        selectInput(
          ns("plot_type"),
          label = "Plot type",
          choices = c("ImageDimPlot", "ImageFeaturePlot"),
          selected = "ImageDimPlot"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ImageDimPlot'", plot_type_id),
          selectInput(
            ns("point_color"),
            label = "Color cells by",
            choices = metadata_cols
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ImageFeaturePlot'", plot_type_id),
          selectizeInput(
            ns("feature_to_display"),
            label = "Feature/Gene",
            choices = getGeneNames(),
            multiple = FALSE,
            options = list(
              maxOptions = 1000,
              placeholder = 'Select a gene...',
              create = FALSE,
              loadThrottle = 300
            )
          )
        ),
        if (length(background_choices) > 1) {
          # Build the full namespaced input ID for conditionalPanel
          bg_input_id <- ns("background_image")
          tagList(
            selectInput(
              ns("background_image"),
              label = "Background image",
              choices = background_choices,
              selected = "No Background"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] != 'No Background'", bg_input_id),
              sliderInput(
                inputId = ns("background_opacity"),
                label = "Image opacity",
                min = 0,
                max = 1,
                value = 0.6,
                step = 0.05
              )
            )
          )
        }
      )
    })

    observeEvent(input[["main_parameters_info"]], {
      showModal(
        modalDialog(
          title = "Main parameters for projection",
          text = HTML("The elements in this panel allow you to control what and how results are displayed across the whole tab.<ul><li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li><li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li></ul>"),
          easyClose = TRUE, footer = NULL, size = "l"
        )
      )
    })

    ##--------------------------------------------------------------------------##
    ## UI elements to set additional parameters for the projection.
    ##--------------------------------------------------------------------------##
    output[["additional_parameters_UI"]] <- renderUI({
      default_point_size <- preferences[["scatter_plot_point_size"]][["default"]]
      if (exists("Cerebro.options") && !is.null(Cerebro.options[["point_size"]]) && is.list(Cerebro.options[["point_size"]]) && !is.null(Cerebro.options[["point_size"]][["spatial_projection_point_size"]])) {
        default_point_size <- Cerebro.options[["point_size"]][["spatial_projection_point_size"]]
      }

      tagList(
        sliderInput(ns("point_size"), label = "Point size", min = preferences[["scatter_plot_point_size"]][["min"]], max = preferences[["scatter_plot_point_size"]][["max"]], step = preferences[["scatter_plot_point_size"]][["step"]], value = default_point_size),
        sliderInput(ns("point_opacity"), label = "Point opacity", min = preferences[["scatter_plot_point_opacity"]][["min"]], max = preferences[["scatter_plot_point_opacity"]][["max"]], step = preferences[["scatter_plot_point_opacity"]][["step"]], value = preferences[["scatter_plot_point_opacity"]][["default"]]),
        sliderInput(ns("percentage_cells_to_show"), label = "Show % of cells", min = preferences[["scatter_plot_percentage_cells_to_show"]][["min"]], max = preferences[["scatter_plot_percentage_cells_to_show"]][["max"]], step = preferences[["scatter_plot_percentage_cells_to_show"]][["step"]], value = preferences[["scatter_plot_percentage_cells_to_show"]][["default"]])
      )
    })
    outputOptions(output, "additional_parameters_UI", suspendWhenHidden = FALSE)

    observeEvent(input[["additional_parameters_info"]], {
      showModal(
        modalDialog(
          title = "Additional parameters for projection",
          text = HTML("The elements in this panel allow you to control what and how results are displayed across the whole tab.<ul><li><b>Point size:</b> Controls how large the cells should be.</li><li><b>Point opacity:</b> Controls the transparency of the cells.</li><li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li></ul>"),
          easyClose = TRUE, footer = NULL, size = "l"
        )
      )
    })

    ##--------------------------------------------------------------------------##
    ## UI elements for group filters of projection plot.
    ##--------------------------------------------------------------------------##
    output[["group_filters_UI"]] <- renderUI({
      group_filters <- list()
      for ( i in getGroups() ) {
        group_filters[[i]] <- shinyWidgets::pickerInput(ns(paste0("group_filter_", i)), label = i, choices = getGroupLevels(i), selected = getGroupLevels(i), options = list("actions-box" = TRUE), multiple = TRUE)
      }
      group_filters
    })
    outputOptions(output, "group_filters_UI", suspendWhenHidden = FALSE)

    observeEvent(input[["group_filters_info"]], {
      showModal(modalDialog(title = "Group filters for projection", text = HTML("The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection."), easyClose = TRUE, footer = NULL, size = "l"))
    })

    ##--------------------------------------------------------------------------##
    ## UI elements to show group label in projection.
    ##--------------------------------------------------------------------------##
    output[["show_group_label_UI"]] <- renderUI({ tagList(checkboxInput(ns("show_group_label"), label = "Show group labels", value = FALSE)) })

    ##--------------------------------------------------------------------------##
    ## UI elements to set point border in projection.
    ##--------------------------------------------------------------------------##
    output[["point_border_UI"]] <- renderUI({ tagList(checkboxInput(ns("point_border"), label = "Draw border around points", value = FALSE)) })

    ##--------------------------------------------------------------------------##
    ## UI elements to select X and Y limits in projection.
    ##--------------------------------------------------------------------------##
    output[["scales_UI"]] <- renderUI({
      if (is.null(input[["to_display"]]) || is.na(input[["to_display"]]) || input[["to_display"]] %in% get_available() == FALSE) {
        projection_to_display <- get_available()[1]
      } else {
        projection_to_display <- input[["to_display"]]
      }
      coordinates <- get_data(projection_to_display)

      ## Apply same rotation logic as projection_coordinates
      if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_plot_rotation"]]) &&
          exists("available_crb_files") && !is.null(available_crb_files$selected)) {
        match_idx <- which(available_crb_files$files == available_crb_files$selected)
        if (length(match_idx) > 0) {
          current_name <- names(available_crb_files$files)[match_idx[1]]
          if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_plot_rotation"]])) {
            rotation_angle <- Cerebro.options[["spatial_plot_rotation"]][[current_name]]
            if (!is.null(rotation_angle) && rotation_angle != 0) {
              theta <- rotation_angle * pi / 180
              cos_theta <- cos(theta)
              sin_theta <- sin(theta)
              x <- coordinates[, 1]
              y <- coordinates[, 2]
              coordinates[, 1] <- x * cos_theta - y * sin_theta
              coordinates[, 2] <- x * sin_theta + y * cos_theta
            }
          }
        }
      }

      XYranges <- getXYranges(coordinates)
      tagList(
        sliderInput(ns("scale_x_manual_range"), label = "Range of X axis", min = XYranges$x$min, max = XYranges$x$max, value = c(XYranges$x$min, XYranges$x$max)),
        sliderInput(ns("scale_y_manual_range"), label = "Range of Y axis", min = XYranges$y$min, max = XYranges$y$max, value = c(XYranges$y$min, XYranges$y$max))
      )
    })
    outputOptions(output, "scales_UI", suspendWhenHidden = FALSE)

    ##--------------------------------------------------------------------------##
    ## Collect parameters for projection plot.
    ##--------------------------------------------------------------------------##
    projection_parameters_plot_raw <- reactive({

      # Validate required inputs
      if (is.null(input[["to_display"]])) {
        return(NULL)
      }
      if (!(input[["to_display"]] %in% get_available())) {
        return(NULL)
      }
      if (is.null(input[["plot_type"]])) {
        return(NULL)
      }

      # Determine color_variable based on plot_type
      plot_type <- input[["plot_type"]]
      color_variable <- NULL
      feature_to_display <- NULL

      if (plot_type == "ImageDimPlot") {
        if (is.null(input[["point_color"]])) {
          return(NULL)
        }
        if (!(input[["point_color"]] %in% colnames(getMetaData()))) {
          return(NULL)
        }
        color_variable <- input[["point_color"]]
      } else if (plot_type == "ImageFeaturePlot") {
        feature_to_display <- input[["feature_to_display"]]
        if (is.null(feature_to_display) || feature_to_display == "") {
          return(NULL)
        }
        color_variable <- feature_to_display
      }

      if (is.null(input[["point_size"]])) {
        return(NULL)
      }
      if (is.null(input[["point_opacity"]])) {
        return(NULL)
      }
      if (is.null(input[["scale_x_manual_range"]])) {
        return(NULL)
      }
      if (is.null(input[["scale_y_manual_range"]])) {
        return(NULL)
      }

      ## Get background image transform parameters from Cerebro.options
      background_opacity <- if (is.null(input[["background_opacity"]])) 1 else input[["background_opacity"]]
      background_flip_x  <- FALSE
      background_flip_y  <- FALSE
      background_scale_x <- 1
      background_scale_y <- 1

      if (exists("Cerebro.options") && exists("available_crb_files") && !is.null(available_crb_files$selected)) {
        match_idx <- which(available_crb_files$files == available_crb_files$selected)
        if (length(match_idx) > 0) {
          current_name <- names(available_crb_files$files)[match_idx[1]]

          # Get flip_x
          if (!is.null(Cerebro.options[["spatial_images_flip_x"]]) &&
              !is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_flip_x"]])) {
            background_flip_x <- Cerebro.options[["spatial_images_flip_x"]][[current_name]]
          }

          # Get flip_y
          if (!is.null(Cerebro.options[["spatial_images_flip_y"]]) &&
              !is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_flip_y"]])) {
            background_flip_y <- Cerebro.options[["spatial_images_flip_y"]][[current_name]]
          }

          # Get scale_x
          if (!is.null(Cerebro.options[["spatial_images_scale_x"]]) &&
              !is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_scale_x"]])) {
            background_scale_x <- Cerebro.options[["spatial_images_scale_x"]][[current_name]]
          }

          # Get scale_y
          if (!is.null(Cerebro.options[["spatial_images_scale_y"]]) &&
              !is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_scale_y"]])) {
            background_scale_y <- Cerebro.options[["spatial_images_scale_y"]][[current_name]]
          }
        }
      }

      parameters <- list(
        projection         = input[["to_display"]],
        n_dimensions       = ncol(get_data(input[["to_display"]])),
        plot_type          = plot_type,
        color_variable     = color_variable,
        feature_to_display = feature_to_display,
        point_size         = input[["point_size"]],
        point_opacity      = input[["point_opacity"]],
        draw_border        = if (is.null(input[["point_border"]])) FALSE else input[["point_border"]],
        group_labels       = input[["show_group_label"]],
        x_range            = input[["scale_x_manual_range"]],
        y_range            = input[["scale_y_manual_range"]],
        use_webgl          = preferences[["use_webgl"]],
        hover_info         = preferences[["show_hover_info_in_projections"]],
        # Background image parameters
        background_image   = input[["background_image"]],
        background_flip_x  = background_flip_x,
        background_flip_y  = background_flip_y,
        background_scale_x = background_scale_x,
        background_scale_y = background_scale_y,
        background_opacity = background_opacity
      )
      return(parameters)
    })

    projection_parameters_plot <- reactive({ projection_parameters_plot_raw() })

    ##--------------------------------------------------------------------------##
    ## Other parameters (reset_axes)
    ##--------------------------------------------------------------------------##
    parameters_other <- reactiveValues(reset_axes = FALSE)
    observeEvent(input[['to_display']], { parameters_other[['reset_axes']] <- TRUE })

    ##--------------------------------------------------------------------------##
    ## Select cells to show.
    ##--------------------------------------------------------------------------##
    projection_cells_to_show <- reactive({
      if (is.null(input[["percentage_cells_to_show"]])) {
        return(NULL)
      }

      cells_to_keep <- rep(TRUE, nrow(getMetaData()))
      for ( i in getGroups() ) {
        if ( !is.null(input[[paste0("group_filter_", i)]]) ) {
          cells_to_keep <- cells_to_keep & (getMetaData()[[i]] %in% input[[paste0("group_filter_", i)]])
        }
      }
      if ( input[["percentage_cells_to_show"]] < 100 ) {
        set.seed(42)
        cells_to_keep <- cells_to_keep & (runif(nrow(getMetaData())) < input[["percentage_cells_to_show"]] / 100)
      }
      return(which(cells_to_keep))
    })

    ##--------------------------------------------------------------------------##
    ## Coordinates of cells in projection.
    ##--------------------------------------------------------------------------##
    projection_coordinates <- reactive({
      req(projection_parameters_plot(), projection_cells_to_show())
      parameters <- projection_parameters_plot()
      indices <- projection_cells_to_show()

      if (!(parameters[["projection"]] %in% get_available())) {
        return(NULL)
      }

      if (length(indices) == 0) {
        return(get_data(parameters[["projection"]])[0, , drop = FALSE])
      }
      coordinates <- get_data(parameters[["projection"]])[indices, , drop = FALSE]

      ## Apply rotation to coordinates if configured
      if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_plot_rotation"]]) &&
          exists("available_crb_files") && !is.null(available_crb_files$selected)) {
        match_idx <- which(available_crb_files$files == available_crb_files$selected)
        if (length(match_idx) > 0) {
          current_name <- names(available_crb_files$files)[match_idx[1]]
          if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_plot_rotation"]])) {
            rotation_angle <- Cerebro.options[["spatial_plot_rotation"]][[current_name]]
            if (!is.null(rotation_angle) && rotation_angle != 0) {
              theta <- rotation_angle * pi / 180
              cos_theta <- cos(theta)
              sin_theta <- sin(theta)
              x <- coordinates[, 1]
              y <- coordinates[, 2]
              coordinates[, 1] <- x * cos_theta - y * sin_theta
              coordinates[, 2] <- x * sin_theta + y * cos_theta
            }
          }
        }
      }

      return(coordinates)
    })

    ##--------------------------------------------------------------------------##
    ## Cell meta data and position in projection.
    ##--------------------------------------------------------------------------##
    projection_data <- reactive({
      req(projection_cells_to_show())
      indices <- projection_cells_to_show()
      if (length(indices) == 0) { return(getMetaData()[0, ]) }
      cells_df <- getMetaData()[indices, , drop = FALSE]
      return(cells_df)
    })

    ##--------------------------------------------------------------------------##
    ## Hover info.
    ##--------------------------------------------------------------------------##
    projection_hover_info <- reactive({
      req(projection_data(), projection_parameters_plot())
      if ( projection_parameters_plot()[['hover_info']] == FALSE ) { return("none") }
      meta_data <- projection_data()
      if ( nrow(meta_data) == 0 ) { return(character(0)) }
      hover_info <- paste0("<b>Cell</b>: ", meta_data$cell_barcode)
      for ( i in getGroups() ) { hover_info <- paste0(hover_info, "<br><b>", i, "</b>: ", meta_data[[i]]) }
      if ( "n_genes_by_counts" %in% colnames(meta_data) ) { hover_info <- paste0(hover_info, "<br><b>Genes</b>: ", meta_data$n_genes_by_counts) }
      if ( "total_counts" %in% colnames(meta_data) ) { hover_info <- paste0(hover_info, "<br><b>UMIs</b>: ", meta_data$total_counts) }
      return(hover_info)
    })

    ##--------------------------------------------------------------------------##
    ## Collect data required to update projection.
    ##--------------------------------------------------------------------------##
    projection_data_to_plot_raw <- reactive({
      req(projection_data(), projection_coordinates(), projection_parameters_plot(), projection_hover_info())
      if (!(nrow(projection_data()) == length(projection_hover_info()) || projection_hover_info() == "none")) {
        return(NULL)
      }

      metadata <- projection_data()
      plot_parameters <- projection_parameters_plot()

      ## Handle ImageFeaturePlot (add gene expression data)
      if (!is.null(plot_parameters$plot_type) &&
          plot_parameters$plot_type == 'ImageFeaturePlot' &&
          !is.null(plot_parameters$feature_to_display)) {
        gene <- plot_parameters$feature_to_display
        if (gene %in% getGeneNames()) {
          # Use cell_barcode column if available, otherwise fallback to rownames
          if ("cell_barcode" %in% colnames(metadata)) {
            cells_to_extract <- metadata$cell_barcode
          } else {
            cells_to_extract <- rownames(metadata)
          }
          # Access expression matrix safely
          expression_data <- getExpressionMatrix()
          if (!is.null(expression_data) && gene %in% rownames(expression_data)) {
            expr_values <- as.vector(expression_data[gene, cells_to_extract])
            metadata[[gene]] <- expr_values
          }
        }
      }

      ## Get colors for groups (if applicable)
      if (
        plot_parameters[['color_variable']] %in% colnames(metadata) &&
        is.numeric(metadata[[ plot_parameters[['color_variable']] ]])
      ) {
        color_assignments <- NA
      } else {
        color_assignments <- assignColorsToGroups(metadata, plot_parameters[['color_variable']])
      }

      to_return <- list(
        cells_df          = metadata,
        coordinates       = projection_coordinates(),
        reset_axes        = isolate(parameters_other[['reset_axes']]),
        plot_parameters   = plot_parameters,
        color_assignments = color_assignments,
        hover_info        = projection_hover_info()
      )
      return(to_return)
    })
    projection_data_to_plot <- debounce(projection_data_to_plot_raw, 150)

    ##--------------------------------------------------------------------------##
    ## Plotly plot of the selected projection.
    ##--------------------------------------------------------------------------##
    output[["projection"]] <- plotly::renderPlotly({
      # Use onRender to verify DOM ID on client side
      plotly::plot_ly(type = 'scattergl', mode = 'markers', source = ns("projection")) %>%
      plotly::layout(xaxis = list(autorange = TRUE, mirror = TRUE, showline = TRUE, zeroline = FALSE), yaxis = list(autorange = TRUE, mirror = TRUE, showline = TRUE, zeroline = FALSE)) %>%
      htmlwidgets::onRender("
        function(el, x) {
          console.log('[projection_server] Plotly rendered. Element ID:', el.id);
        }
      ")
    })

    observeEvent(input[["projection_info"]], {
      showModal(modalDialog(title = "Dimensional reduction", text = HTML("Interactive projection of cells into 2-dimensional space based on their expression profile.<ul><li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics.</li></ul>"), easyClose = TRUE, footer = NULL, size = "l"))
    })

    ##--------------------------------------------------------------------------##
    ## Update plot.
    ##--------------------------------------------------------------------------##
    observeEvent(projection_data_to_plot(), {
      req(projection_data_to_plot())
      data <- projection_data_to_plot()

      # Use helper function
      # Safe check parameters before passing
      if (is.null(data$plot_parameters)) {
        return()
      }

      # Prepare background image data if applicable
      bg_data <- prepare_background_image(data$plot_parameters, data$coordinates)

      params <- prepare_projection_data(
        data                   = data$cells_df,
        coordinates            = data$coordinates,
        parameters             = data$plot_parameters,
        color_assignments      = data$color_assignments,
        hover_info             = data$hover_info,
        background_image_data  = bg_data$background_image_data,
        image_bounds           = bg_data$image_bounds
      )

      # Handle reset_axes
      if (data$reset_axes) {
        params$data$reset_axes      <- TRUE
        parameters_other$reset_axes <- FALSE
      } else {
        params$data$reset_axes <- FALSE
      }

      # FIX: Force plot_id to be a character string
      params$plot_id <- as.character(ns("projection"))

      params$uirevision <- input[[paste0("projection_uirevision")]]

      # Pass parameters as separate arguments (like other modules)
      shinyjs::js$updateProjectionPlot(
        params$meta,
        params$data,
        params$hover,
        params$group_centers,
        params$plot_id,
        params$uirevision
      )
    })

    ##--------------------------------------------------------------------------##
    ## Selected cells
    ##--------------------------------------------------------------------------##
    projection_selected_cells <- reactive({
      event_data <- plotly::event_data("plotly_selected", source = ns("projection"))
      if (is.null(event_data)) return(NULL)
      result <- event_data %>% dplyr::mutate(identifier = paste0(x, '-', y))
      return(result)
    })

    ## Show/hide clear selection button and scroll indicator based on selection
    observe({
      if (!is.null(projection_selected_cells()) && nrow(projection_selected_cells()) > 0) {
        shinyjs::show("clear_selection")
        # Show scroll down indicator
        shinyjs::js$showScrollDownIndicator("Charts generated below")
      } else {
        shinyjs::hide("clear_selection")
        shinyjs::js$hideScrollDownIndicator()
      }
    })

    ## Clear selection event
    observeEvent(input[["clear_selection"]], {
      # Hide scroll indicator first
      shinyjs::js$hideScrollDownIndicator()
      # Call JavaScript function to clear the selection
      plot_id <- ns("projection")
      shinyjs::js$projectionClearSelection(plot_id)
    })

    output[["number_of_selected_cells"]] <- renderText({
      if ( is.null(projection_selected_cells()) ) {
        number_of_selected_cells <- 0
        paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
      } else {
        number_of_selected_cells <- formatC(nrow(projection_selected_cells()), format = "f", big.mark = ",", digits = 0)
        paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
      }
    })

    ##--------------------------------------------------------------------------##
    ## Source components
    ##--------------------------------------------------------------------------##
    if (exists("Cerebro.options") && !is.null(Cerebro.options[["cerebro_root"]])) {
      source(paste0(Cerebro.options[["cerebro_root"]],
                    "/shiny/module/projection/server_components/UI_selected_cells_plot.R"), local = TRUE)
      source(paste0(Cerebro.options[["cerebro_root"]],
                    "/shiny/module/projection/server_components/out_selected_cells_plot.R"), local = TRUE)
      source(paste0(Cerebro.options[["cerebro_root"]],
                    "/shiny/module/projection/server_components/UI_selected_cells_table.R"), local = TRUE)
      source(paste0(Cerebro.options[["cerebro_root"]],
                    "/shiny/module/projection/server_components/out_selected_cells_table.R"), local = TRUE)
    }
  })
}
