## Compact feedback for custom gene names that are absent from the data set.
output[["expression_genes_displayed"]] <- renderUI({
  req(expression_selected_genes())
  missing <- expression_selected_genes()[["genes_to_display_missing"]]
  count <- length(expression_selected_genes()[["genes_to_display_present"]])
  fallback <- identical(input[["expression_projection_genes_in_separate_panels"]], "separate") &&
    count >= 2L && (count > 9L || ncol(expression_projection_coordinates()) != 2L)
  tagList(
    if (fallback) div(
      class = "cerebro-gene-validation",
      "Showing mean expression: Separate panels supports 2–9 genes in a 2D projection."
    ),
    if (length(missing)) div(
      class = "cerebro-gene-validation",
      icon("triangle-exclamation"),
      tags$span("Not found in this data set:"),
      tags$strong(paste(missing, collapse = ", "))
    )
  )
})
