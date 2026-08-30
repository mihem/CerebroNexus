##----------------------------------------------------------------------------##
## Collect color parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_parameters_color <- reactive({
  ## require input UI elements
  req(expression_projection_expression_levels())
  ## collect parameters
  parameters <- list(
    color_scale = "Cerebro orange",
    color_range = expressionValueRange(
      expression_projection_expression_levels()
    ),
    color_mode = if (
      identical(
        input[["expression_projection_gene_color_mode"]],
        "different"
      )
    ) {
      "different"
    } else {
      "shared"
    },
    genes = expression_selected_genes()[["genes_to_display_present"]],
    rgb_genes = expression_selected_genes()[["rgb_genes"]]
  )
  return(parameters)
})
