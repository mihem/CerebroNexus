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

trajectory_row_index_reactive <- reactive({
  trajectory <- trajectory_data_reactive()
  trajectory_cells <- rownames(trajectory[["meta"]])
  pack <- viewerPackCurrent()
  pack_cell_count <- suppressWarnings(as.integer(pack$cell_count))
  valid_pack_count <- length(pack_cell_count) == 1L &&
    !is.na(pack_cell_count)
  index <- viewerPackTrajectoryIndex(
    pack,
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  valid_index <- length(index) == nrow(trajectory[["meta"]]) &&
    !anyNA(index) &&
    isTRUE(all(index >= 1L))
  if (!isTRUE(valid_index)) {
    index <- NULL
  }
  pack_order_matches <-
    is.null(index) &&
    !is.null(pack) &&
    is.list(pack) &&
    valid_pack_count &&
    length(trajectory_cells) == pack_cell_count &&
    identical(
      viewerPackCellOrderFingerprint(trajectory_cells),
      as.character(pack$manifest$cell_order_fingerprint)
    )
  if (isTRUE(pack_order_matches)) {
    index <- seq_len(pack_cell_count)
  }
  if (is.null(index)) {
    metadata <- getMetaData()
    index <- match(trajectory_cells, metadata[["cell_barcode"]])
  }
  if (anyNA(index)) {
    stop("Trajectory cells do not match the canonical dataset order.")
  }
  as.integer(index)
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
    cell_barcodes <- viewerPackCellBarcodes(viewerPackCurrent(), cell_index)
    if (is.null(cell_barcodes)) {
      cell_barcodes <- getMetaData()[["cell_barcode"]][cell_index]
    }
    cells[["cell_barcode"]] <- cell_barcodes
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
