##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Projection.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements for plot of projection and input parameters.
##----------------------------------------------------------------------------##

output[["trajectory_projection_UI"]] <- renderUI({
  available_methods <- getMethodsForTrajectories()
  available_methods <- available_methods[available_methods %in% c("monocle2")]

  if (length(available_methods) == 0) {
    return(
      fluidRow(
        cerebroBox(
          title = "Trajectory",
          textOutput("trajectory_missing")
        )
      )
    )
  }

  tagList(
    fluidRow(
      class = "cerebro-viz-row",
      column(
        width = 3,
        offset = 0,
        class = "cerebro-param-col",
        cerebroBox(
          title = tagList(
            "Main parameters",
            cerebroInfoButton("trajectory_projection_main_parameters_info")
          ),
          tagList(
            uiOutput("trajectory_select_method_and_name_UI"),
            uiOutput("trajectory_projection_main_parameters_UI")
          )
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            cerebroInfoButton(
              "trajectory_projection_additional_parameters_info"
            )
          ),
          uiOutput("trajectory_projection_additional_parameters_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            cerebroInfoButton("trajectory_projection_group_filters_info")
          ),
          uiOutput("trajectory_projection_group_filters_UI"),
          collapsed = TRUE
        )
      ),
      column(
        width = 9,
        offset = 0,
        class = "cerebro-viz-col",
        shiny::tagAppendAttributes(
          cerebroBox(
            title = tagList(
              boxTitle("Trajectory"),
              cerebroInfoButton("trajectory_projection_info"),
              shinyFiles::shinySaveButton(
                "trajectory_projection_export",
                label = "export to PDF",
                title = "Export trajectory to PDF file.",
                filetype = "pdf",
                viewtype = "icon",
                class = "btn-xs"
              )
            ),
            tagList(
              plotly::plotlyOutput(
                "trajectory_projection",
                width = "auto",
                height = "60vh"
              ),
              tags$br(),
              fluidRow(
                column(
                  width = 8,
                  htmlOutput("trajectory_number_of_selected_cells")
                ),
                column(
                  width = 4,
                  tags$div(
                    class = "cerebro-selection-actions",
                    shinyjs::hidden(
                      actionButton(
                        inputId = "trajectory_projection_zoom_to_selection",
                        label = "Zoom to selection",
                        icon = icon("magnifying-glass-plus"),
                        class = "btn-xs btn-default"
                      )
                    ),
                    shinyjs::hidden(
                      actionButton(
                        inputId = "trajectory_projection_clear_selection",
                        label = "Clear selection",
                        icon = icon("eraser"),
                        class = "btn-xs btn-default btn-breathing"
                      )
                    )
                  )
                )
              )
            )
          ),
          class = "cerebro-viewport-gate"
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements for main parameters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_main_parameters_UI"]] <- renderUI({
  ## determine which metadata columns to include based on exclude_trivial_metadata
  exclude_trivial <- FALSE
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[['exclude_trivial_metadata']])
  ) {
    exclude_trivial <- Cerebro.options[['exclude_trivial_metadata']]
  }

  ## build choices based on setting
  if (exclude_trivial == TRUE) {
    ## only include groups from getGroups()
    metadata_cols <- getGroups()
  } else {
    ## include all metadata columns except cell_barcode
    metadata_cols <- colnames(getMetaData())[
      !colnames(getMetaData()) %in% c("cell_barcode")
    ]
  }

  selectInput(
    "trajectory_point_color",
    label = "Color cells by",
    choices = c(
      "state",
      "pseudotime",
      metadata_cols
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_main_parameters_info$text,
      title = trajectory_projection_main_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_main_parameters_info <- list(
  title = "Main parameters for projection of trajectory",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Choose a method:</b> Select the trajectory-inference method.</li>
      <li><b>Choose a trajectory:</b> Select the trajectory to display.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements for additional parameters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_additional_parameters_UI"]] <- renderUI({
  ## Start from a dynamic default sized to the cell count + canvas, falling back
  ## to the fixed default if that can't be computed. A configured preset (below)
  ## still takes precedence over this when one is set.
  default_point_size <- tryCatch(
    dynamicPointSize(
      n_points = nrow(getMetaData()),
      plot_width_px = session$clientData[[
        "output_trajectory_projection_width"
      ]],
      plot_height_px = session$clientData[[
        "output_trajectory_projection_height"
      ]],
      min = preferences[["gene_expression_plot_point_size"]][["min"]],
      max = preferences[["gene_expression_plot_point_size"]][["max"]],
      step = preferences[["gene_expression_plot_point_size"]][["step"]],
      fallback = preferences[["gene_expression_plot_point_size"]][["default"]]
    ),
    error = function(e) {
      preferences[["gene_expression_plot_point_size"]][["default"]]
    }
  )

  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["point_size"]]) &&
      is.list(Cerebro.options[["point_size"]]) &&
      !is.null(Cerebro.options[["point_size"]][["trajectory_point_size"]])
  ) {
    default_point_size <- Cerebro.options[["point_size"]][[
      "trajectory_point_size"
    ]]
  }

  tagList(
    sliderInput(
      "trajectory_point_size",
      label = "Point size",
      min = preferences[["gene_expression_plot_point_size"]][["min"]],
      max = preferences[["gene_expression_plot_point_size"]][["max"]],
      step = preferences[["gene_expression_plot_point_size"]][["step"]],
      value = default_point_size
    ),
    sliderInput(
      "trajectory_point_opacity",
      label = "Point opacity",
      min = preferences[["gene_expression_plot_point_opacity"]][["min"]],
      max = preferences[["gene_expression_plot_point_opacity"]][["max"]],
      step = preferences[["gene_expression_plot_point_opacity"]][["step"]],
      value = preferences[["gene_expression_plot_point_opacity"]][["default"]]
    ),
    sliderInput(
      "trajectory_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "min"
      ]],
      max = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "max"
      ]],
      step = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "step"
      ]],
      value = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "default"
      ]]
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "trajectory_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_additional_parameters_info$text,
      title = trajectory_projection_additional_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection of trajectory",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements for group filters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_group_filters_UI"]] <- renderUI({
  group_filters <- list()
  for (i in getGroups()) {
    group_filters[[i]] <- shinyWidgets::pickerInput(
      paste0("trajectory_projection_group_filter_", i),
      label = i,
      choices = getGroupLevels(i),
      selected = getGroupLevels(i),
      options = list("actions-box" = TRUE),
      multiple = TRUE
    )
  }
  group_filters
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "trajectory_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_group_filters_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_group_filters_info$text,
      title = trajectory_projection_group_filters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_group_filters_info <- list(
  title = "Group filters for projection of trajectory",
  text = HTML(
    "
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection.
    "
  )
)

##----------------------------------------------------------------------------##
## Plot of projection.
##----------------------------------------------------------------------------##
