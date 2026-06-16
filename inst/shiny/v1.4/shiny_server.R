##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
server <- function(input, output, session) {
  ##--------------------------------------------------------------------------##
  ## Load color setup, plotting and utility functions.
  ##--------------------------------------------------------------------------##
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/color_setup.R"),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/plotting_functions.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/utility_functions.R"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  preferences <- reactiveValues(
    overview_plot_point_size = list(
      min = 1,
      max = 20,
      step = 1,
      default = ifelse(
        exists('Cerebro.options') &&
          !is.null(Cerebro.options[['overview_default_point_size']]),
        Cerebro.options[['overview_default_point_size']],
        2
      )
    ),
    gene_expression_plot_point_size = list(
      min = 1,
      max = 20,
      step = 1,
      default = ifelse(
        exists('Cerebro.options') &&
          !is.null(Cerebro.options[['gene_expression_default_point_size']]),
        Cerebro.options[['gene_expression_default_point_size']],
        2
      )
    ),
    overview_plot_point_opacity = list(
      min = 0.1,
      max = 1.0,
      step = 0.1,
      default = ifelse(
        exists('Cerebro.options') &&
          !is.null(Cerebro.options[['overview_default_point_opacity']]),
        Cerebro.options[['overview_default_point_opacity']],
        1.0
      )
    ),
    gene_expression_plot_point_opacity = list(
      min = 0.1,
      max = 1.0,
      step = 0.1,
      default = ifelse(
        exists('Cerebro.options') &&
          !is.null(Cerebro.options[['gene_expression_default_point_opacity']]),
        Cerebro.options[['gene_expression_default_point_opacity']],
        1.0
      )
    ),
    overview_plot_percentage_cells_to_show = list(
      min = 10,
      max = 100,
      step = 10,
      default = ifelse(
        exists('Cerebro.options') &&
          !is.null(Cerebro.options[[
            'overview_default_percentage_cells_to_show'
          ]]),
        Cerebro.options[['overview_default_percentage_cells_to_show']],
        100
      )
    ),
    gene_expression_plot_percentage_cells_to_show = list(
      min = 10,
      max = 100,
      step = 10,
      default = ifelse(
        exists('Cerebro.options') &&
          !is.null(Cerebro.options[[
            'gene_expression_default_percentage_cells_to_show'
          ]]),
        Cerebro.options[['gene_expression_default_percentage_cells_to_show']],
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

  ## reactive value holding path to file of data set to load
  data_to_load <- reactiveValues(path = NULL)

  ## listen to selected 'input_file', initialize before UI element is loaded
  observeEvent(input[['input_file']], ignoreNULL = FALSE, {
    path_to_load <- ''
    ## grab path from 'input_file' if one is specified
    if (
      !is.null(input[["input_file"]]) &&
        all(!is.na(input[["input_file"]])) &&
        file.exists(input[["input_file"]]$datapath)
    ) {
      path_to_load <- input[["input_file"]]$datapath
      ## take path or object from 'Cerebro.options' if it is set and points to an
      ## existing file or object
    } else if (
      exists('Cerebro.options') &&
        !is.null(Cerebro.options[["crb_file_to_load"]]) &&
        length(Cerebro.options[["crb_file_to_load"]]) > 0
    ) {
      file_to_load <- Cerebro.options[["crb_file_to_load"]]
      if (any(file.exists(file_to_load)) || any(exists(file_to_load))) {
        path_to_load <- .GlobalEnv$Cerebro.options$crb_file_to_load
      }
    }
    ## assign path to example file if none of the above apply
    if (length(path_to_load) == 0 || all(path_to_load == '')) {
      path_to_load <- system.file(
        "extdata/v1.4/example.crb",
        package = "cerebroAppLite"
      )
    }
    ## set reactive value to new file path
    data_to_load$path <- path_to_load
  })

  ## create reactive value holding the current data set
  data_set <- reactive({
    req(data_to_load$path)
    dataset_to_load <- data_to_load$path
    if (exists(dataset_to_load)) {
      print(glue::glue(
        "[{Sys.time()}] Load data set from variable: {dataset_to_load}"
      ))
      data <- get(dataset_to_load)
    } else {
      ## Route through the process-level cache defined in utility_functions.R.
      ## get_or_load_crb() loads via read_cerebro_file() (qs/rds dispatch) and
      ## then re-attaches external expression backends (bpcells / h5) using
      ## paths rooted at the crb's parent directory. Cerebro.options can still
      ## override the matrix path via expression_matrix_BPCells /
      ## expression_matrix_h5 -- the helper picks that up internally.
      data <- get_or_load_crb(dataset_to_load)
    }
    ## log message
    message(data$print())
    ## check if 'expression' slot exists and print log message with its format
    ## if it does
    if (!is.null(data$expression)) {
      print(glue::glue(
        "[{Sys.time()}] Format of expression data: {class(data$expression)}"
      ))
    }
    ## return loaded data
    return(data)
  })

  # list of available trajectories
  available_trajectories <- reactive({
    req(!is.null(data_set()))
    ## collect available trajectories across all methods and create selectable
    ## options
    available_trajectories <- c()
    available_trajectory_method <- getMethodsForTrajectories()
    ## check if at least 1 trajectory method exists
    if (length(available_trajectory_method) > 0) {
      ## cycle through trajectory methods
      for (i in seq_along(available_trajectory_method)) {
        ## get current method and names of trajectories for this method
        current_method <- available_trajectory_method[i]
        available_trajectories_for_this_method <- getNamesOfTrajectories(
          current_method
        )
        ## check if at least 1 trajectory is available for this method
        if (length(available_trajectories_for_this_method) > 0) {
          ## cycle through trajectories for this method
          for (j in seq_along(available_trajectories_for_this_method)) {
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

  # available genes
  list_of_genes <- reactive({
    req(data_set())
    rownames(data_set()$expression)
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

  ## Dynamic sidebar: conditional tabs are inserted/removed based on dataset
  ## content (see insertConditionalTab() below). The old renderMenu +
  ## shinyjs::toggleElement pattern for trajectory and extra_material has been
  ## replaced.
  ##--------------------------------------------------------------------------##

  ##--------------------------------------------------------------------------##
  ## Print log message when switching tab (for debugging).
  ##--------------------------------------------------------------------------##
  observe({
    print(glue::glue("[{Sys.time()}] Active tab: {input[['sidebar']]}"))
  })

  ##--------------------------------------------------------------------------##
  ## Print message when session is closed due to inactivity.
  ##--------------------------------------------------------------------------##
  observeEvent(input$timeOut, {
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste(
        "Session timeout due to",
        input$timeOut,
        "inactivity -",
        Sys.time()
      ),
      footer = NULL
    ))
    session$close()
  })

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/load_data/server.R"),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/overview/server.R"),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/groups/server.R"),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/marker_genes/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/gene_expression/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/gene_id_conversion/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/color_management/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/about/server.R"),
    local = TRUE
  )
  ## Enhanced module servers.
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/most_expressed_genes/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/enriched_pathways/server.R"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Dynamic sidebar: insert/remove conditional tabs based on dataset content.
  ##--------------------------------------------------------------------------##
  insertConditionalTab <- function(
    tab_label,
    tab_name,
    icon_name,
    check_fn,
    placeholder_id = tab_name
  ) {
    item_id <- paste0("sidebar_item_", tab_name)
    placeholder_selector <- paste0(
      "#sidebar_item_",
      placeholder_id,
      "_placeholder"
    )
    show_reactive <- reactive({
      req(data_set())
      result <- tryCatch(check_fn(), error = function(e) FALSE)
      if (is.logical(result)) {
        return(result)
      }
      length(result) > 0
    })
    inserted <- reactiveVal(FALSE)
    observe({
      req(!is.null(data_set()))
      should_show <- show_reactive()
      is_inserted <- isolate(inserted())
      if (should_show && !is_inserted) {
        session$onFlushed(
          function() {
            insertUI(
              selector = placeholder_selector,
              where = "afterEnd",
              ui = tags$li(
                id = item_id,
                class = "treeview",
                menuItem(
                  tab_label,
                  tabName = tab_name,
                  icon = icon(icon_name)
                )$children
              ),
              immediate = TRUE
            )
            inserted(TRUE)
          },
          once = TRUE
        )
      } else if (!should_show && is_inserted) {
        removeUI(selector = paste0("#", item_id), immediate = TRUE)
        inserted(FALSE)
      }
    })
  }

  insertConditionalTab(
    "Enriched pathways",
    "enrichedPathways",
    "project-diagram",
    function() getMethodsForEnrichedPathways(),
    placeholder_id = "enriched_pathways"
  )
  insertConditionalTab("Extra material", "extra_material", "gift", function() {
    getExtraMaterialCategories()
  })

  ## Cleanup snapshot artifacts that may have been left by test runs.
  snapshot_dir <- file.path(
    Cerebro.options[["cerebro_root"]],
    "..",
    "..",
    "tests",
    "testthat",
    "_snaps"
  )
  new_pngs <- list.files(
    snapshot_dir,
    pattern = "\\.new\\.png$",
    full.names = TRUE
  )
  if (length(new_pngs) > 0) {
    file.remove(new_pngs)
  }

  ##--------------------------------------------------------------------------##
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/extra_material/server.R"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Export reactive values for testing (shinytest2).
  ##--------------------------------------------------------------------------##
  exportTestValues(
    overview_cells_to_show = {
      if (is.null(data_set())) {
        NULL
      } else {
        overview_projection_cells_to_show()
      }
    },
    expression_levels = {
      if (is.null(data_set())) {
        NULL
      } else {
        expression_projection_expression_levels()
      }
    }
  )
}
