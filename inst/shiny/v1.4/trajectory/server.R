##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Reactive to fetch trajectory data
##----------------------------------------------------------------------------##
trajectory_data_reactive <- reactive({
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
})

source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/select_method_and_name.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/projection.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/projection_plot.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/projection_export.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/selected_cells_table.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/distribution_along_pseudotime.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/states_by_group.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/trajectory/expression_metrics.R"
  ),
  local = TRUE
)
