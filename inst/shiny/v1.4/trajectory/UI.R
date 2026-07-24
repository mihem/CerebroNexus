##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

## Prepend the shared plotly layout factory and the shared projection-scatter
## renderer, then trajectory's thin wrappers — all in ONE extendShinyjs() text
## so they share a global scope (same pattern as spatial/UI.R).
## Shared projection engine loaded once app-wide (see shiny_UI.R); inline only
## this tab's thin wrappers over the window globals it exposes.
js_code_trajectory_projection <- cerebro_read_file(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/js_projection_update_plot.js"
  )
)

tab_trajectory <- tabItem(
  tabName = "trajectory",
  shinyjs::inlineCSS(
    "
    #trajectory_details_selected_cells_table .table th {
      text-align: center;
    }
    #states_by_group_table .table th {
      text-align: center;
    }
    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_trajectory_projection,
    functions = c(
      "trajectoryUpdatePlot2DContinuous",
      "trajectoryUpdatePlot2DCategorical",
      "trajectoryGetContainerDimensions",
      "trajectoryClearSelection",
      "trajectoryZoomToSelection"
    )
  ),
  uiOutput("trajectory_projection_UI"),
  uiOutput("trajectory_selected_cells_table_UI"),
  uiOutput("trajectory_distribution_along_pseudotime_UI"),
  uiOutput("trajectory_states_by_group_UI"),
  uiOutput("trajectory_expression_metrics_UI")
)
