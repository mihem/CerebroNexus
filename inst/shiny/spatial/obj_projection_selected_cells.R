##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
spatial_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(spatial_projection_data_to_plot())
  
  ## DEBUG: Print raw event data
  raw_event <- plotly::event_data("plotly_selected", source = "spatial_projection")
  message("=== DEBUG: spatial_projection_selected_cells ===")
  message("  Raw event_data is NULL: ", is.null(raw_event))
  message("  Raw event_data length: ", length(raw_event))
  message("  Raw event_data class: ", paste(class(raw_event), collapse = ", "))
  if (!is.null(raw_event) && length(raw_event) > 0) {
    message("  Raw event_data names: ", paste(names(raw_event), collapse = ", "))
    message("  Raw event_data structure:")
    print(str(raw_event))
    message("  First few rows (if data.frame):")
    if (is.data.frame(raw_event)) {
      print(head(raw_event))
    }
  }
  
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "spatial_projection")) ||
    length(plotly::event_data("plotly_selected", source = "spatial_projection")) == 0
  ) {
    message("  --> Returning NULL (no selection)")
    return(NULL)
  ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    result <- plotly::event_data("plotly_selected", source = "spatial_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))
    message("  --> Returning ", nrow(result), " selected cells")
    message("  Identifiers: ", paste(head(result$identifier, 5), collapse = ", "), 
            if(nrow(result) > 5) "..." else "")
    return(result)
  }
})
