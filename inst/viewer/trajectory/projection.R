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
    cerebroVizPageHeader(
      "Trajectory",
      "trajectory_projection_info",
      "Explore inferred cell-state transitions and pseudotime."
    ),
    fluidRow(
      class = "cerebro-viz-row cerebro-viz-top-layout",
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-toolbar-col",
        div(
          class = "cerebro-viz-toolbar",
          div(
            class = "cerebro-viz-primary",
            uiOutput("trajectory_select_method_and_name_UI"),
            uiOutput("trajectory_projection_main_parameters_UI"),
            shinyFiles::shinySaveButton(
              "trajectory_projection_export",
              label = "Export PDF",
              title = "Export trajectory to PDF file.",
              filetype = "pdf",
              viewtype = "icon",
              class = "cerebro-toolbar-export"
            )
          ),
          cerebroSettingsButton(
            "trajectory_projection_more_button",
            "trajectory_projection_more"
          ),
          cerebroSettingsDrawer(
            "trajectory_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("trajectory_projection_additional_parameters_UI"),
                uiOutput("trajectory_projection_group_labels_UI"),
                checkboxInput(
                  "trajectory_projection_point_border",
                  "Draw border around cells",
                  value = TRUE
                ),
                checkboxInput(
                  "trajectory_projection_keep_square",
                  "Keep plots square",
                  value = FALSE
                )
              ),
              cerebroInfoButton(
                "trajectory_projection_additional_parameters_info"
              )
            ),
            cerebroSettingsSection(
              "Data",
              uiOutput("trajectory_projection_data_parameters_UI")
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("trajectory_projection_group_filters_UI"),
              cerebroInfoButton("trajectory_projection_group_filters_info")
            )
          )
        )
      ),
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        cerebroSelectionStatus(
          "trajectory_projection",
          "trajectory_number_of_selected_cells"
        ),
        cerebroCellViewOutput("trajectory_projection")
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
    label = "Colour by",
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
      <li><b>Colour by:</b> Select which variable, categorical or continuous, from the meta data should be used to colour the cells.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements for additional parameters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_additional_parameters_UI"]] <- renderUI({
  appearance <- current_scatter_defaults()

  tagList(
    sliderInput(
      "trajectory_point_size",
      label = "Point size",
      min = preferences[["cell_point_size"]][["min"]],
      max = preferences[["cell_point_size"]][["max"]],
      step = preferences[["cell_point_size"]][["step"]],
      value = appearance$point_size
    ),
    sliderInput(
      "trajectory_point_opacity",
      label = "Point opacity",
      min = preferences[["cell_point_opacity"]][["min"]],
      max = preferences[["cell_point_opacity"]][["max"]],
      step = preferences[["cell_point_opacity"]][["step"]],
      value = appearance$point_opacity
    )
  )
})

output[["trajectory_projection_data_parameters_UI"]] <- renderUI({
  appearance <- current_scatter_defaults()

  tagList(
    sliderInput(
      "trajectory_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["cell_percentage_cells_to_show"]][[
        "min"
      ]],
      max = preferences[["cell_percentage_cells_to_show"]][[
        "max"
      ]],
      step = preferences[["cell_percentage_cells_to_show"]][[
        "step"
      ]],
      value = appearance$percentage_cells_to_show
    )
  )
})

output[["trajectory_projection_group_labels_UI"]] <- renderUI({
  color_variable <- input[["trajectory_point_color"]]
  req(color_variable)
  metadata <- getMetaData()
  categorical <- identical(color_variable, "state") ||
    (color_variable %in%
      colnames(metadata) &&
      !is.numeric(metadata[[color_variable]]))
  if (!categorical) {
    return(NULL)
  }
  checkboxInput(
    "trajectory_projection_group_labels",
    "Group labels",
    value = TRUE
  )
})

## Keep controls available while the settings drawer is hidden.
outputOptions(
  output,
  "trajectory_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "trajectory_projection_data_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "trajectory_projection_group_labels_UI",
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
## Shared group filters for the trajectory projection.
##----------------------------------------------------------------------------##

registerGroupFiltersUI(
  output,
  "trajectory_projection",
  getGroups = getGroups,
  getGroupLevels = getGroupLevels
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

registerGroupFiltersInfo(
  input,
  "trajectory_projection",
  title = "Group filters for projection of trajectory",
  text = HTML(
    "
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that pass all filters (for each grouping variable) are shown in the projection.
    "
  )
)

##----------------------------------------------------------------------------##
## Plot of projection.
##----------------------------------------------------------------------------##
