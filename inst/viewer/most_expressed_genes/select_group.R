##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##
## Select group.
##----------------------------------------------------------------------------##

## The grouping variables with a stored expression summary, and the one the
## selector opens on. The selector is drawn with it, and table.R reads it
## through viewerControlValue() until the selector binds.
most_expressed_genes_groups <- function() {
  unique(c(
    getGroupsWithMostExpressedGenes(),
    getGroupsWithMeanExpression()
  ))
}
most_expressed_genes_default_group <- function(
  groups = most_expressed_genes_groups()
) {
  if (length(groups)) groups[[1L]]
}

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_select_group_UI"]] <- renderUI({
  groups <- most_expressed_genes_groups()
  if (length(groups)) {
    selectInput(
      "most_expressed_genes_selected_group",
      label = "Grouping variable",
      choices = groups,
      selected = most_expressed_genes_default_group(groups),
      width = "100%"
    )
  } else {
    p("No stored expression summaries available.")
  }
})
