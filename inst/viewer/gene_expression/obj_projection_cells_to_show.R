##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
expression_projection_cells_to_show <- reactive({
  req(input[["expression_projection_percentage_cells_to_show"]])
  groups <- getGroups()
  pct_cells <- input[["expression_projection_percentage_cells_to_show"]]
  group_filters <- list()
  ## store group filters
  for (i in groups) {
    filter_value <- input[[paste0(
      "expression_projection_group_filter_",
      i
    )]]
    group_filters[[i]] <- if (is.null(filter_value)) {
      character()
    } else {
      filter_value
    }
  }
  ## get cell meta data
  cells_df <- getMetaData() %>%
    dplyr::mutate(row_id = row_number())
  cells_df <- cells_df[
    cerebroGroupFilterMask(cells_df, group_filters),
    ,
    drop = FALSE
  ]
  cells_df <- cells_df %>%
    dplyr::select(cell_barcode, row_id)
  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, pct_cells)
  ## put rows in random order
  cells_df <- cells_df[sample(seq_len(nrow(cells_df))), ]
  cells_to_show <- cells_df$row_id
  return(cells_to_show)
})
