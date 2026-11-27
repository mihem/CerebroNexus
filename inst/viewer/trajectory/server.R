##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## The method, trajectory and colour selectors (select_method_and_name.R), read
## through viewerControlValue(): the first frame is drawn with what the
## selectors will bind with (first supported method, its first trajectory, the
## main group) before they reach the browser. The selectors are drawn with the
## values of the functions below, so both agree.
##----------------------------------------------------------------------------##
## `current` while it is one of `available`, else the first of `available`.
trajectory_selector_value <- function(current, available) {
  if (length(current) == 1L && !is.na(current) && current %in% available) {
    current
  } else if (length(available)) {
    available[[1L]]
  }
}
trajectory_default_method <- function(
  current = NULL,
  available = viewerSupportedTrajectoryMethods(getMethodsForTrajectories())
) {
  trajectory_selector_value(current, available)
}
trajectory_default_name <- function(
  method = trajectory_default_method(),
  current = NULL,
  available = if (!is.null(method)) getNamesOfTrajectories(method)
) {
  trajectory_selector_value(current, available)
}
trajectory_default_point_color <- function(
  colour_groups = viewerColourGroupChoices()
) {
  colour_groups$selected %||% "state"
}
trajectory_selected_method <- viewerControlValue(
  input,
  "trajectory_selected_method",
  trajectory_default_method
)
trajectory_selected_name <- viewerControlValue(
  input,
  "trajectory_selected_name",
  trajectory_default_name
)
trajectory_point_color <- viewerControlValue(
  input,
  "trajectory_point_color",
  trajectory_default_point_color
)

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
  method <- trajectory_selected_method()
  name <- trajectory_selected_name()
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
    trajectory_selected_method(),
    trajectory_selected_name()
  )
})

trajectory_row_index_reactive <- reactive({
  viewerTrajectoryCellIndex(
    trajectory_data_reactive(),
    trajectory_selected_method(),
    trajectory_selected_name()
  )
})

trajectory_cells_reactive <- function(columns = character(), barcodes = FALSE) {
  trajectory <- trajectory_data_reactive()
  cells <- trajectory[["meta"]]
  cell_index <- trajectory_row_index_reactive()
  metadata <- viewerProjectionFirstFrameMetadata()
  columns <- unique(as.character(columns))
  columns <- setdiff(
    columns[columns %in% colnames(metadata)],
    colnames(cells)
  )
  if (length(columns)) {
    ## Integer indices already establish the exact row mapping. Assign vectors
    ## directly to retain trajectory row names without rebuilding a joined
    ## data frame and validating its million row names for every consumer.
    for (column in columns) {
      cells[[column]] <- metadata[[column]][cell_index]
    }
  }
  cells[["cell_index"]] <- cell_index
  if (isTRUE(barcodes)) {
    cells[["cell_barcode"]] <- viewerCellBarcodesAt(cell_index)
  }
  if (anyNA(cells$pseudotime)) {
    cells <- cells[!is.na(cells$pseudotime), , drop = FALSE]
  }
  cells
}

for (module_file in c(
  "select_method_and_name.R",
  "projection.R",
  "projection_plot.R",
  "selected_cells_table.R",
  "distribution_along_pseudotime.R",
  "states_by_group.R",
  "expression_metrics.R"
)) {
  source(cerebro_root_path("viewer", "trajectory", module_file), local = TRUE)
}
