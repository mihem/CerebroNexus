##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Projection.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_projection_main_parameters_info",
  trajectory_projection_main_parameters_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_main_parameters_info <- list(
  title = "Main parameters for projection of trajectory",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Choose a method:</b> Select the trajectory method.</li>
      <li><b>Choose a trajectory:</b> Select the trajectory to display.</li>
      <li><b>Colour by:</b> Select a trajectory measure or one of the data set's registered grouping variables.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements for additional parameters of projection plot.
##----------------------------------------------------------------------------##

trajectory_initial_appearance <- isolate(current_scatter_defaults())
trajectory_initial_display <- isolate(current_page_display_defaults(
  "trajectory"
))
trajectory_projection_appearance <- reactiveValues(
  point_size = trajectory_initial_appearance$point_size,
  point_opacity = trajectory_initial_appearance$point_opacity,
  percentage_cells_to_show = trajectory_initial_appearance$percentage_cells_to_show,
  group_labels = trajectory_initial_display$group_labels,
  draw_border = trajectory_initial_display$draw_border,
  keep_square = trajectory_initial_display$keep_square
)

trajectory_update_appearance <- function(name, value) {
  if (!identical(trajectory_projection_appearance[[name]], value)) {
    trajectory_projection_appearance[[name]] <- value
    TRUE
  } else {
    FALSE
  }
}

output[["trajectory_projection_appearance_UI"]] <- renderUI({
  req(input[["trajectory_projection_more_render_request"]])
  color_variable <- input[["trajectory_point_color"]]
  categorical <- FALSE
  if (!is.null(color_variable)) {
    metadata <- trajectory_cells_reactive(color_variable)
    categorical <- identical(color_variable, "state") ||
      (color_variable %in%
        colnames(metadata) &&
        !is.numeric(metadata[[color_variable]]))
  }

  tagList(
    sliderInput(
      "trajectory_point_size",
      label = "Point size",
      min = preferences[["cell_point_size"]][["min"]],
      max = preferences[["cell_point_size"]][["max"]],
      step = preferences[["cell_point_size"]][["step"]],
      value = isolate(trajectory_projection_appearance$point_size)
    ),
    sliderInput(
      "trajectory_point_opacity",
      label = "Point opacity",
      min = preferences[["cell_point_opacity"]][["min"]],
      max = preferences[["cell_point_opacity"]][["max"]],
      step = preferences[["cell_point_opacity"]][["step"]],
      value = isolate(trajectory_projection_appearance$point_opacity)
    ),
    if (categorical) {
      checkboxInput(
        "trajectory_projection_group_labels",
        "Group labels",
        value = isolate(trajectory_projection_appearance$group_labels)
      )
    },
    checkboxInput(
      "trajectory_projection_point_border",
      "Draw border around cells",
      value = isolate(trajectory_projection_appearance$draw_border)
    ),
    checkboxInput(
      "trajectory_projection_keep_square",
      "Keep plots square",
      value = isolate(trajectory_projection_appearance$keep_square)
    )
  )
})

output[["trajectory_projection_data_parameters_UI"]] <- renderUI({
  req(input[["trajectory_projection_more_render_request"]])

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
      value = isolate(
        trajectory_projection_appearance$percentage_cells_to_show
      )
    )
  )
})

observe({
  appearance <- current_scatter_defaults()
  for (field in names(appearance)) {
    trajectory_update_appearance(field, appearance[[field]])
  }
})
observeEvent(
  input[["trajectory_projection_point_border"]],
  {
    trajectory_update_appearance(
      "draw_border",
      isTRUE(input[["trajectory_projection_point_border"]])
    )
  },
  ignoreNULL = TRUE
)
observeEvent(
  input[["trajectory_projection_keep_square"]],
  {
    trajectory_update_appearance(
      "keep_square",
      isTRUE(input[["trajectory_projection_keep_square"]])
    )
  },
  ignoreNULL = TRUE
)
observeEvent(
  input[["trajectory_projection_group_labels"]],
  {
    changed <- trajectory_update_appearance(
      "group_labels",
      isTRUE(input[["trajectory_projection_group_labels"]])
    )
    if (changed) {
      session$sendCustomMessage(
        "cell_view_appearance",
        list(
          id = "trajectory_projection",
          dataset_context = viewer_loaded_dataset_context(),
          dataset_fingerprint = viewerDatasetIdentity()$fingerprint,
          values = list(
            group_labels = trajectory_projection_appearance$group_labels
          )
        )
      )
    }
  },
  ignoreNULL = TRUE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_projection_additional_parameters_info",
  trajectory_projection_additional_parameters_info
)

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
  getGroupLevels = getGroupLevels,
  render_request = function() {
    input[["trajectory_projection_more_render_request"]]
  }
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
