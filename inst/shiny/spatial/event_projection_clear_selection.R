##----------------------------------------------------------------------------##
## Clear selection button event handler.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_clear_selection"]], {
  ## Hide scroll indicator first
  shinyjs::js$hideScrollDownIndicator()
  ## Call JavaScript function to clear the plotly selection
  shinyjs::js$spatialClearSelection()
})

##----------------------------------------------------------------------------##
## Toggle visibility of clear selection button.
##----------------------------------------------------------------------------##
observe({
  req(spatial_projection_data_to_plot())

  if (
    !is.null(plotly::event_data("plotly_selected", source = "spatial_projection")) &&
    length(plotly::event_data("plotly_selected", source = "spatial_projection")) > 0
  ) {
    shinyjs::show("spatial_projection_clear_selection")
  } else {
    shinyjs::hide("spatial_projection_clear_selection")
  }
})

##----------------------------------------------------------------------------##
## Show scroll-down indicator when cells are selected.
##----------------------------------------------------------------------------##
observeEvent(spatial_projection_selected_cells(), {
  if (!is.null(spatial_projection_selected_cells()) &&
      nrow(spatial_projection_selected_cells()) > 0) {
    shinyjs::js$showScrollDownIndicator("Charts generated below ↓")
  }
})
