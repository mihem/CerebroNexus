##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
output[["spatial_details_selected_cells_table"]] <- DT::renderDataTable({
  ## don't proceed without these inputs
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    spatial_projection_data_to_plot()
  )
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (is.null(spatial_projection_selected_cells())) {
    ## prepare empty table
    meta_data %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
    ## ... selection has been made and at least 1 cell is in it
  } else {
    ## Use the actual plotted coordinates from spatial_projection_data_to_plot()
    plot_data <- spatial_projection_data_to_plot()

    ## extract cells for table - use the coordinates that were actually plotted
    cells_df <- cbind(
      plot_data$coordinates,
      plot_data$cells_df
    ) %>%
      as.data.frame()
    ## filter out non-selected cells with X-Y identifier
    cells_df <- cells_df %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2))
    cells_df[["selection_key"]] <- if ("cell_barcode" %in% colnames(cells_df)) {
      as.character(cells_df[["cell_barcode"]])
    } else {
      cells_df[["identifier"]]
    }
    selected_cells <- spatial_projection_selected_cells()
    cells_df <- cells_df %>%
      dplyr::filter(
        selection_key %in%
          selected_cells[["selection_key"]] |
          identifier %in% selected_cells[["identifier"]]
      ) %>%
      dplyr::select(-c(X1, X2, identifier, selection_key)) %>%
      ## Put cell_barcode first WHEN present; the rest of the spatial module
      ## treats it as optional (falls back to rownames), so any_of() keeps this
      ## table consistent instead of erroring on metadata without that column.
      dplyr::select(dplyr::any_of("cell_barcode"), dplyr::everything())
    ## check how many cells are left after filtering
    ## ... no cells are left
    if (nrow(cells_df) == 0) {
      ## prepare empty table
      getMetaData() %>%
        dplyr::slice(0) %>%
        prepareEmptyTable()
      ## ... at least 1 cell is left
    } else {
      ## prepare proper table
      prettifyTable(
        cells_df,
        filter = list(position = "top", clear = TRUE),
        dom = "Brtlip",
        show_buttons = TRUE,
        number_formatting = input[[
          "spatial_details_selected_cells_table_number_formatting"
        ]],
        color_highlighting = input[[
          "spatial_details_selected_cells_table_color_highlighting"
        ]],
        hide_long_columns = TRUE,
        download_file_name = "spatial_details_of_selected_cells"
      )
    }
  }
})
