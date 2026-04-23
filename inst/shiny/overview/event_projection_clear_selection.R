##----------------------------------------------------------------------------##
## Clear selection button event handler.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_clear_selection"]], {
  ## Call JavaScript function to clear the plotly selection
  shinyjs::js$overviewClearSelection()
})

##----------------------------------------------------------------------------##
## Toggle visibility of clear selection button.
##----------------------------------------------------------------------------##
observe({
  req(overview_projection_data_to_plot())
  
  if (
    !is.null(plotly::event_data("plotly_selected", source = "overview_projection")) &&
    length(plotly::event_data("plotly_selected", source = "overview_projection")) > 0
  ) {
    shinyjs::show("overview_projection_clear_selection")
  } else {
    shinyjs::hide("overview_projection_clear_selection")
  }
})
