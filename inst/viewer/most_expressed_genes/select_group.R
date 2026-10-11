##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##
## Select group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_select_group_UI"]] <- renderUI({
  groups <- unique(c(
    getGroupsWithMostExpressedGenes(),
    getGroupsWithMeanExpression()
  ))
  if (length(groups)) {
    selectInput(
      "most_expressed_genes_selected_group",
      label = "Grouping variable",
      choices = groups,
      width = "100%"
    )
  } else {
    p("No stored expression summaries available.")
  }
})
