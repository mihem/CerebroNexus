##----------------------------------------------------------------------------##
## Select content for marker genes.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element - show marker genes table if data exists.
##----------------------------------------------------------------------------##
output[["marker_genes_select_method_and_table_UI"]] <- renderUI({
  if (hasMarkerGenes()) {
    fluidRow()  ## empty - no selection needed, just show the table
  } else {
    fluidRow(
      cerebroBox(
        title = boxTitle("Marker genes"),
        textOutput("marker_genes_message_no_data_found")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_message_no_data_found"]] <- renderText({
  "No marker genes data available."
})
