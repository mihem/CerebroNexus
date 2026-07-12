##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

## Prepend the shared plotly layout factory and the shared projection-scatter
## renderer, then trajectory's thin wrappers — all in ONE extendShinyjs() text
## so they share a global scope (same pattern as spatial/UI.R).
js_code_trajectory_projection <- paste(
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/www/projection_layouts.js"
    )
  ),
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/www/projection_scatter.js"
    )
  ),
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/trajectory/js_projection_update_plot.js"
    )
  ),
  sep = "\n"
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
      "trajectoryGetContainerDimensions"
    )
  ),
  uiOutput("trajectory_select_method_and_name_UI"),
  uiOutput("trajectory_projection_UI"),
  uiOutput("trajectory_selected_cells_table_UI"),
  uiOutput("trajectory_distribution_along_pseudotime_UI"),
  uiOutput("trajectory_states_by_group_UI"),
  uiOutput("trajectory_expression_metrics_UI")
)
