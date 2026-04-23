##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
output[["details_selected_cells_table"]] <- DT::renderDataTable({
  ## don't proceed without these inputs
  req(
    input[["to_display"]],
    input[["to_display"]] %in% get_available(),
    projection_data_to_plot()
  )

  meta_data <- getMetaData()
  req(!is.null(meta_data))
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( is.null(projection_selected_cells()) ) {
    ## prepare empty table
    meta_data %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## ... selection has been made and at least 1 cell is in it
  } else {
    ## Use the actual plotted coordinates from projection_data_to_plot()
    plot_data <- projection_data_to_plot()

    ## extract cells for table - use the coordinates that were actually plotted
    cells_df <- cbind(
        plot_data$coordinates,
        plot_data$cells_df
      ) %>%
      as.data.frame()
    ## filter out non-selected cells with X-Y identifier
    cells_df <- cells_df %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
      dplyr::filter(identifier %in% projection_selected_cells()$identifier) %>%
      dplyr::select(-c(X1, X2, identifier)) %>%
      dplyr::select(cell_barcode, everything())
    ## check how many cells are left after filtering
    ## ... no cells are left
    if ( nrow(cells_df) == 0 ) {
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
        number_formatting = input[["details_selected_cells_table_number_formatting"]],
        color_highlighting = input[["details_selected_cells_table_color_highlighting"]],
        hide_long_columns = TRUE,
        download_file_name = "details_of_selected_cells"
      )
    }
  }
})
