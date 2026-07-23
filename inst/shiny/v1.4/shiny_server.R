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

  ## reactive values holding available .crb files and the current selection.
  ## In single-file mode only 'selected' is used; when >1 files are provided via
  ## Cerebro.options$crb_file_to_load, 'files'/'names' drive the sidebar dataset
  ## switcher rendered below.
  available_crb_files <- reactiveValues(
    files = NULL,
    selected = NULL,
    names = NULL
  )

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
      ## an uploaded file replaces the pre-configured data sets, so clear the
      ## switcher state — otherwise the dropdown keeps offering the old data
      ## sets, which no longer match what is loaded.
      available_crb_files$files <- NULL
      available_crb_files$names <- NULL
      ## take path or object from 'Cerebro.options' if it is set and points to an
      ## existing file or object
    } else if (
      exists('Cerebro.options') &&
        !is.null(Cerebro.options[["crb_file_to_load"]])
    ) {
      file_to_load <- Cerebro.options[["crb_file_to_load"]]
      ## multiple files (or a single named file) -> enable dataset switcher
      if (length(file_to_load) > 1 || !is.null(names(file_to_load))) {
        available_crb_files$files <- file_to_load
        file_names <- names(file_to_load)
        if (
          !is.null(file_names) &&
            length(file_names) == length(file_to_load)
        ) {
          available_crb_files$names <- file_names
        } else {
          available_crb_files$names <- NULL
        }

        ##--------------------------------------------------------------------##
        ## Check for a dataset specified in the URL (query string or path),
        ## e.g. '?dataset=sampleA' or '/sampleA'.
        ##--------------------------------------------------------------------##
        url_dataset <- NULL

        ## 1. query string (?dataset=...)
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query$dataset)) {
          url_dataset <- query$dataset
        }

        ## 2. pathname (e.g. /dataset_name or /app/dataset_name). Use only the
        ## LAST path segment as the token, so the app still resolves it when
        ## mounted under a sub-path (e.g. shiny-server at /app/TCR -> "TCR").
        if (
          is.null(url_dataset) &&
            !is.null(session$clientData$url_pathname)
        ) {
          path_val <- session$clientData$url_pathname
          path_val <- gsub("/$", "", path_val) # drop trailing slash
          segments <- strsplit(path_val, "/", fixed = TRUE)[[1]]
          segments <- segments[nzchar(segments)]
          if (length(segments) > 0) {
            ## URL-decode so links with encoded names (e.g. %20) still match
            url_dataset <- utils::URLdecode(segments[length(segments)])
          }
        }

        ## try to match url_dataset against available files
        if (!is.null(url_dataset)) {
          path_to_load <- match_dataset_by_url(
            url_dataset,
            available_crb_files$files,
            available_crb_files$names
          )
          if (path_to_load != '') {
            print(glue::glue(
              "[{Sys.time()}] Dataset selected via URL: {url_dataset} -> {path_to_load}"
            ))
          }
        }

        ## if not chosen via URL: keep current selection, else pick default.
        ## crb_pick_smallest_file TRUE/NULL -> smallest file; FALSE -> first.
        if (path_to_load != '') {
          ## already set by URL logic
        } else if (!is.null(available_crb_files$selected)) {
          path_to_load <- available_crb_files$selected
        } else {
          pick_smallest <- TRUE
          if (!is.null(Cerebro.options[["crb_pick_smallest_file"]])) {
            pick_smallest <- as.logical(
              Cerebro.options[["crb_pick_smallest_file"]]
            )
          }
          if (isTRUE(pick_smallest)) {
            file_sizes <- sapply(file_to_load, function(f) {
              if (file.exists(f)) {
                file.size(f)
              } else {
                Inf ## variable/object -> infinite size, skipped
              }
            })
            path_to_load <- file_to_load[which.min(file_sizes)]
          } else {
            path_to_load <- file_to_load[1]
          }
        }
      } else {
        ## single unnamed file
        available_crb_files$files <- NULL
        available_crb_files$names <- NULL
        if (file.exists(file_to_load) || exists(file_to_load)) {
          path_to_load <- file_to_load
        }
      }
    }
    ## assign path to example file if none of the above apply
    if (length(path_to_load) == 0 || all(path_to_load == '')) {
      ## Resolve relative to cerebro_root, never via the package: the exported
      ## bundle, inst/app.R and the installed launcher all point cerebro_root at
      ## a directory that carries extdata/, so this fallback stays self-contained
      ## even when the app runs without CerebroNexus installed (a package
      ## lookup would then resolve to "" and silently break the fallback).
      path_to_load <- file.path(
        Cerebro.options[["cerebro_root"]],
        "extdata/v1.4/example.crb"
      )
    }
    ## set reactive value to selected file path
    if (
      is.null(available_crb_files$selected) ||
        available_crb_files$selected != path_to_load
    ) {
      available_crb_files$selected <- path_to_load
    }
  })

  ## renderUI for the dataset switcher; shown only when >1 .crb files are
  ## available, inert (returns NULL) in single-file mode.
  output[["crb_file_selector_UI"]] <- renderUI({
    if (
      !is.null(available_crb_files$files) &&
        length(available_crb_files$files) > 1
    ) {
      choices <- available_crb_files$files
      names(choices) <- if (!is.null(available_crb_files$names)) {
        available_crb_files$names
      } else {
        basename(available_crb_files$files)
      }
      selected <- available_crb_files$selected
      if (is.null(selected)) {
        selected <- choices[1]
      }
      tagList(
        ## The "Select sample dataset" title already labels this control, so the
        ## selectInput's own label would just repeat it — drop it.
        titlePanel("Select sample dataset"),
        selectInput(
          inputId = "crb_file_selector",
          label = NULL,
          choices = choices,
          selected = selected,
          width = '350px'
        )
      )
    }
  })

  ## listen to the dataset switcher and update the current selection
  observeEvent(input[['crb_file_selector']], {
    if (
      !is.null(input[['crb_file_selector']]) &&
        !is.null(available_crb_files$files)
    ) {
      if (
        is.null(available_crb_files$selected) ||
          available_crb_files$selected != input[['crb_file_selector']]
      ) {
        available_crb_files$selected <- input[['crb_file_selector']]
      }
    }
  })

  ## create reactive value holding the current data set
  data_set <- reactive({
    req(!is.null(available_crb_files$selected))
    dataset_to_load <- available_crb_files$selected
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

  # hover info for projection.
  # Cached by (dataset path, hover toggle): selecting a different gene does
  # not re-build the per-cell hover strings because they only depend on the
  # metadata of the current dataset, not on the active gene. Unlike the
  # expression-level reactive, this chain has no gene dependency and no
  # isolate(), so the cache key stays consistent across gene switches.
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
  }) %>%
    cachePlot(
      preferences[["show_hover_info_in_projections"]],
      available_crb_files$selected
    )

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
    "Marker genes",
    "markerGenes",
    "list-alt",
    function() getMethodsForMarkerGenes(),
    placeholder_id = "marker_genes"
  )
  insertConditionalTab(
    "Most expressed genes",
    "mostExpressedGenes",
    "bullhorn",
    function() getGroupsWithMostExpressedGenes(),
    placeholder_id = "most_expressed_genes"
  )
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
  insertConditionalTab(
    "Immune repertoire",
    "immune_repertoire",
    "dna",
    function() {
      getImmuneRepertoire()
    }
  )
  insertConditionalTab(
    "Trajectory",
    "trajectory",
    "route",
    ## Only supported methods (monocle2) should surface the tab; an unsupported
    ## method would otherwise render a blank tab instead of the empty state.
    function() intersect(getMethodsForTrajectories(), c("monocle2"))
  )
  insertConditionalTab(
    "Spatial",
    "spatial",
    "map-pin",
    function() availableSpatial()
  )
  ## Trekker single-cell spatial mapping: its own bespoke page (not the generic
  ## Spatial tab). Shown only when the loaded .crb carries a `trekker` slot.
  insertConditionalTab(
    "Trekker",
    "trekker",
    "map-marked-alt",
    function() {
      tk <- tryCatch(data_set()$getTrekker(), error = function(e) NULL)
      !is.null(tk)
    }
  )
  insertConditionalTab(
    "HLA & TCR Motifs",
    "hla_tcr_motifs",
    "project-diagram",
    ## Show only when the data set actually carries a TCR (TRA/TRB). HLA typing
    ## is NOT required — the motif network works without it, and the Data & QC
    ## tab is where a user would add HLA, so the page must be reachable first.
    ##
    ## hla_detect_chains(), not the IR module's detect_chains(): the latter only
    ## scans the first three samples, so a cohort whose TCR data starts at sample
    ## four would hide this page while the core underneath could analyse it
    ## perfectly well — and the page is the only way to reach Data & QC, so there
    ## would be no way in. The gate has to agree with what the page can do.
    ## Bound into this scope by the module's core_shim, which is sourced before
    ## this closure is ever evaluated.
    function() {
      any(
        tryCatch(
          hla_detect_chains(getImmuneRepertoire()),
          error = function(e) character(0)
        ) %in%
          c("TRA", "TRB")
      )
    }
  )

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
  ## Shared module: group-filters widget used by projection-style tabs.
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/module/group_filters/group_filters_widget.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/extra_material/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/immune_repertoire/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/trajectory/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/spatial/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/trekker/server.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/hla_tcr_motifs/server.R"
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
