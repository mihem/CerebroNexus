##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
spatial_projection_cells_to_show <- reactive({
  req(input[["spatial_projection_percentage_cells_to_show"]])

  groups <- getGroups()

  # Collect all filters first
  group_filters <- list()
  for (i in groups) {
    filter_val <- input[[paste0("spatial_projection_group_filter_", i)]]
    group_filters[[i]] <- if (is.null(filter_val)) character() else filter_val
  }

  pct_cells <- input[["spatial_projection_percentage_cells_to_show"]]
  meta_data <- getMetaData()
  req(!is.null(meta_data))

  keep <- cerebroGroupFilterMask(meta_data, group_filters)

  # Get indices
  cells_indices <- which(keep)

  # Randomly subset if needed
  if (length(cells_indices) > 0 && pct_cells < 100) {
    n_to_keep <- ceiling(length(cells_indices) * (pct_cells / 100))
    cells_indices <- sample(cells_indices, n_to_keep)
  }

  # Shuffle for plotting order (avoid occlusion bias)
  # Check if we have cells to show
  if (length(cells_indices) > 0) {
    cells_to_show <- sample(cells_indices)
  } else {
    cells_to_show <- integer(0)
  }

  return(cells_to_show)
})
