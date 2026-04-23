##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
server <- function(input, output, session) {

  ##--------------------------------------------------------------------------##
  ## Load color setup, plotting and utility functions.
  ##--------------------------------------------------------------------------##
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/color_setup.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/plotting_functions.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/utility_functions.R"), local = TRUE)

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  preferences <- reactiveValues(
    scatter_plot_point_size = list(
      min = 1,
      max = 20,
      step = 1,
      default = ifelse(
        exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_default_point_size']]),
        Cerebro.options[['projections_default_point_size']],
        2
      )
    ),
    scatter_plot_point_opacity = list(
      min = 0.1,
      max = 1.0,
      step = 0.1,
      default = ifelse(
        exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_default_point_opacity']]),
        Cerebro.options[['projections_default_point_opacity']],
        1.0
      )
    ),
    scatter_plot_percentage_cells_to_show = list(
      min = 10,
      max = 100,
      step = 10,
      default = ifelse(
        exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_default_percentage_cells_to_show']]),
        Cerebro.options[['projections_default_percentage_cells_to_show']],
        100
      )
    ),
    use_webgl = TRUE,
    show_hover_info_in_projections = ifelse(
      exists('Cerebro.options') &&
      !is.null(Cerebro.options[['projections_show_hover_info']]),
      Cerebro.options[['projections_show_hover_info']],
      TRUE
    )
  )

  ## paths for storing plots
  available_storage_volumes <- c(
    Home = "~",
    shinyFiles::getVolumes()()
  )

  ##--------------------------------------------------------------------------##
  ## Load data set.
  ##--------------------------------------------------------------------------##

  ## reactive value holding list of available files and currently selected file
  available_crb_files <- reactiveValues(files = NULL, selected = NULL, names = NULL)

  ## listen to selected 'input_file', initialize before UI element is loaded
  observeEvent(input[['input_file']], ignoreNULL = FALSE, {
    path_to_load <- ''
    ## grab path from 'input_file' if one is specified
    if (
      !is.null(input[["input_file"]]) &&
      !is.na(input[["input_file"]]) &&
      file.exists(input[["input_file"]]$datapath)
    ) {
      path_to_load <- input[["input_file"]]$datapath
    ## take path or object from 'Cerebro.options' if it is set and points to an
    ## existing file or object
    } else if (
      exists('Cerebro.options') &&
      !is.null(Cerebro.options[["crb_file_to_load"]])
    ) {
      file_to_load <- Cerebro.options[["crb_file_to_load"]]
      ## check if file_to_load is a vector/list with multiple files (or single named file)
      if (length(file_to_load) > 1 || !is.null(names(file_to_load))) {
        ## store all available files
        available_crb_files$files <- file_to_load
        ## check if file_to_load has names (named list)
        file_names <- names(file_to_load)
        if (!is.null(file_names) && length(file_names) == length(file_to_load)) {
          ## if all files have names, store them
          available_crb_files$names <- file_names
        } else {
          ## if no names, set to NULL
          available_crb_files$names <- NULL
        }
        ## if a file is already selected, use it; otherwise use the smallest one by file size
        if (!is.null(available_crb_files$selected)) {
          path_to_load <- available_crb_files$selected
        } else {
          ## determine which file to select by default
          ## TRUE or NULL (default) -> select smallest file
          ## FALSE -> select first file
          pick_smallest <- TRUE
          if ( !is.null(Cerebro.options[["crb_pick_smallest_file"]]) ) {
            pick_smallest <- as.logical(Cerebro.options[["crb_pick_smallest_file"]])
          }

          if (isTRUE(pick_smallest)) {
            ## find the smallest file by file size
            file_sizes <- sapply(file_to_load, function(f) {
              if (file.exists(f)) {
                file.size(f)
              } else {
                Inf  ## if it's a variable/object, assign infinite size so it won't be selected
              }
            })
            smallest_idx <- which.min(file_sizes)
            path_to_load <- file_to_load[smallest_idx]
          } else {
            ## select the first file
            path_to_load <- file_to_load[1]
          }
        }
      } else {
        ## single file case
        available_crb_files$files <- NULL
        available_crb_files$names <- NULL
        if (file.exists(file_to_load) || exists(file_to_load)) {
          path_to_load <- file_to_load
        }
      }
    }
    ## assign path to example file if none of the above apply
    if (path_to_load=='') {
      path_to_load <- system.file("extdata/example.crb", package = "cerebroAppLite")
    }
    ## set reactive value to selected file path
    if (is.null(available_crb_files$selected) || available_crb_files$selected != path_to_load) {
      available_crb_files$selected <- path_to_load
    }
  })

  ## listen to selected file from dropdown (when multiple files available)
  observeEvent(input[['crb_file_selector']], {
    if (!is.null(input[['crb_file_selector']]) && !is.null(available_crb_files$files)) {
      if (is.null(available_crb_files$selected) || available_crb_files$selected != input[['crb_file_selector']]) {
        available_crb_files$selected <- input[['crb_file_selector']]
      }
    }
  })

  ## create reactive value holding the current data set
  data_set <- reactive({
    dataset_to_load <- available_crb_files$selected
    req(!is.null(dataset_to_load))
    if (exists(dataset_to_load)) {
      print(glue::glue("[{Sys.time()}] Load from variable: {dataset_to_load}"))
      data <- get(dataset_to_load)
    } else {
      ## log message
      print(glue::glue("[{Sys.time()}] File to load: {dataset_to_load}"))
      ## read the file
      data <- read_cerebro_file(dataset_to_load)
    }
    ## log message
    # message(data$print())
    ## use print(data) instead of data$print() because R6 objects don't have a print member by default
    print(data)
    ## check if 'expression' slot exists and print log message with its format
    ## if it does
    if ( !is.null(data$expression) ) {
      print(glue::glue("[{Sys.time()}] Format of expression data: {class(data$expression)}"))
    }
    ## return loaded data
    return(data)
  })

  ##--------------------------------------------------------------------------##
  ## Adjust default point size based on number of cells.
  ##--------------------------------------------------------------------------##
  observe({
    req(!is.null(data_set()))

    ## only proceed if default point size is not specified in options
    if (
      !exists('Cerebro.options') ||
      is.null(Cerebro.options[['projections_default_point_size']])
    ) {

      ## get number of cells
      number_of_cells <- ncol(data_set()$expression)

      ## adjust point size
      if ( number_of_cells < 500 ) {
        preferences$scatter_plot_point_size$default <- 8
      } else if ( number_of_cells < 2000 ) {
        preferences$scatter_plot_point_size$default <- 6
      } else if ( number_of_cells < 10000 ) {
        preferences$scatter_plot_point_size$default <- 3
      } else {
        preferences$scatter_plot_point_size$default <- 1
      }
    }
  })

  # list of available trajectories
  available_trajectories <- reactive({
    req(!is.null(data_set()))
    ## collect available trajectories across all methods and create selectable
    ## options
    available_trajectories <- c()
    available_trajectory_method <- getMethodsForTrajectories()
    ## check if at least 1 trajectory method exists
    if ( length(available_trajectory_method) > 0 ) {
      ## cycle through trajectory methods
      for ( i in seq_along(available_trajectory_method) ) {
        ## get current method and names of trajectories for this method
        current_method <- available_trajectory_method[i]
        available_trajectories_for_this_method <- getNamesOfTrajectories(current_method)
        ## check if at least 1 trajectory is available for this method
        if ( length(available_trajectories_for_this_method) > 0 ) {
          ## cycle through trajectories for this method
          for ( j in seq_along(available_trajectories_for_this_method) ) {
            ## create selectable combination of method and trajectory name and add
            ## it to the available trajectories
            current_trajectory <- available_trajectories_for_this_method[j]
            available_trajectories <- c(
              available_trajectories,
              glue::glue("{current_method} // {current_trajectory}")
            )
          }
        }
      }
    }
    # message(str(available_trajectories))
    return(available_trajectories)
  })

  # hover info for projection
  hover_info_projections <- reactive({
    # message('--> trigger "hover_info_projections"')
    if (
      !is.null(preferences[["show_hover_info_in_projections"]]) &&
      preferences[['show_hover_info_in_projections']] == TRUE
    ) {
      cells_df <- getMetaData()
      hover_info <- buildHoverInfoForProjections(cells_df)
      hover_info <- setNames(hover_info, cells_df$cell_barcode)
    } else {
      hover_info <- 'none'
    }
    # message(str(hover_info))
    return(hover_info)
  })

  ##--------------------------------------------------------------------------##
  ## Show "Spatial" tab if there are spatial projections in the data set.
  ##--------------------------------------------------------------------------

  output[["sidebar_item_spatial"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("Spatial", tabName = "spatial", icon = icon("images"))
  })

  show_spatial_tab <- reactive({
    req(!is.null(data_set()))
    spatial_projections <- grep("^Spatial_", availableProjections(), value = TRUE)
    if (length(spatial_projections) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_spatial",
      condition = show_spatial_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "Marker genes" tab if there are marker genes in the data set.
  ##--------------------------------------------------------------------------

  output[["sidebar_item_marker_genes"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("Marker genes", tabName = "markerGenes", icon = icon("list-alt"))
  })

  show_marker_genes_tab <- reactive({
    req(!is.null(data_set()))
    if (
      !is.null(getMethodsForMarkerGenes()) &&
      length(getMethodsForMarkerGenes()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_marker_genes",
      condition = show_marker_genes_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "BCR" tab if there is BCR data in the data set.
  ##--------------------------------------------------------------------------

  output[["sidebar_item_bcr"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("BCR", tabName = "bcr", icon = icon("dna"))
  })

  show_bcr_tab <- reactive({
    req(!is.null(data_set()))
    bcr_data <- getBCR()
    if (!is.null(bcr_data) && is.list(bcr_data) && length(bcr_data) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_bcr",
      condition = show_bcr_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "TCR" tab if there is TCR data in the data set.
  ##--------------------------------------------------------------------------

  output[["sidebar_item_tcr"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("TCR", tabName = "tcr", icon = icon("dna"))
  })

  show_tcr_tab <- reactive({
    req(!is.null(data_set()))
    tcr_data <- getTCR()
    if (!is.null(tcr_data) && is.list(tcr_data) && length(tcr_data) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_tcr",
      condition = show_tcr_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "Enriched pathways" tab if there are enriched pathways in the data set.
  ##--------------------------------------------------------------------------

  output[["sidebar_item_enriched_pathways"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("Enriched pathways", tabName = "enrichedPathways", icon = icon("sitemap"))
  })

  show_enriched_pathways_tab <- reactive({
    req(!is.null(data_set()))
    if (
      !is.null(getMethodsForEnrichedPathways()) &&
      length(getMethodsForEnrichedPathways()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_enriched_pathways",
      condition = show_enriched_pathways_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "Trajectory" tab if there are trajectories in the data set.
  ##--------------------------------------------------------------------------

  ## the tab item needs to be in the `output`
  output[["sidebar_item_trajectory"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("Trajectory", tabName = "trajectory", icon = icon("random"))
  })

  ## this reactive value checks whether the tab should be shown or not
  show_trajectory_tab <- reactive({
    req(!is.null(data_set()))
    ## if at least one trajectory is present, return TRUE, otherwise FALSE
    if (
      !is.null(getMethodsForTrajectories()) &&
      length(getMethodsForTrajectories()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  ## listen to reactive value defined above and toggle visibility of trajectory
  ## tab accordingly
  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_trajectory",
      condition = show_trajectory_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "Extra material" tab if there is some extra material in the data set.
  ##--------------------------------------------------------------------------##

  ## the tab item needs to be in the `output`
  output[["sidebar_item_extra_material"]] <- renderMenu({
    ## require a data set to be loaded
    req(!is.null(data_set()))
    menuItem("Extra material", tabName = "extra_material", icon = icon("gift"))
  })

  ## this reactive value checks whether the tab should be shown or not
  show_extra_material_tab <- reactive({
    ## require a data set to be loaded
    req(!is.null(data_set()))
    ## if at least one piece of extra material is present, return TRUE,
    ## otherwise FALSE
    if (
      !is.null(getExtraMaterialCategories()) &&
      length(getExtraMaterialCategories()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  ## listen to reactive value defined above and toggle visibility of extra
  ## material tab accordingly
  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_extra_material",
      condition = show_extra_material_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Print log message when switching tab (for debugging).
  ##--------------------------------------------------------------------------##
  observe({
    print(glue::glue("[{Sys.time()}] Active tab: {input[['sidebar']]}"))
  })

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/load_data/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/overview/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/spatial/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/groups/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/marker_genes/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/gene_expression/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/gene_id_conversion/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/color_management/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/about/server.R"), local = TRUE)

  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/most_expressed_genes/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/enriched_pathways/server.R"), local = TRUE)
  ## Immune Repertoire tabs (BCR/TCR)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/immune_repertoire/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/trajectory/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/extra_material/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/analysis_info/server.R"), local = TRUE)
}
