##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Guard: is the selected method/name valid for the CURRENT dataset?
##
## Depends on data_set() so it re-evaluates on a dataset switch, when the
## trajectory_selected_method / _name inputs still hold the previous dataset's
## values. Every getTrajectory() consumer req()s this, so a stale selection bails
## out cleanly instead of throwing "Method `X` is not available." The available
## methods gate the name lookup, so getNamesOfTrajectories() is never called with
## a method the current dataset lacks (which would itself throw).
##----------------------------------------------------------------------------##
trajectory_selection_ok <- reactive({
  req(!is.null(data_set()))
  method <- input[["trajectory_selected_method"]]
  name <- input[["trajectory_selected_name"]]
  available_methods <- getMethodsForTrajectories()
  names_for_method <- if (
    !is.null(method) && length(method) == 1 && method %in% available_methods
  ) {
    getNamesOfTrajectories(method)
  } else {
    character(0)
  }
  trajectorySelectionValid(method, name, available_methods, names_for_method)
})

##----------------------------------------------------------------------------##
## Reactive to fetch trajectory data
##----------------------------------------------------------------------------##
trajectory_data_reactive <- reactive({
  req(trajectory_selection_ok())
  getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
})

source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/select_method_and_name.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/projection.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/projection_plot.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/projection_export.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/selected_cells_table.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/distribution_along_pseudotime.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/states_by_group.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/trajectory/expression_metrics.R"
  ),
  local = TRUE
)
