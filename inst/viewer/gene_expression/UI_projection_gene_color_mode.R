##----------------------------------------------------------------------------##
## Shared or per-gene colours for separate expression panels.
##----------------------------------------------------------------------------##
output[["expression_projection_gene_color_mode_UI"]] <- renderUI({
  req(expression_selected_genes())
  n_genes <- length(
    expression_selected_genes()[["genes_to_display_present"]]
  )
  display_mode <- input[["expression_projection_genes_in_separate_panels"]]
  if (is.null(display_mode)) {
    display_mode <- "combined"
  }
  if (n_genes <= 1 || display_mode != "separate") {
    return(NULL)
  }
  selected <- isolate(input[["expression_projection_gene_color_mode"]])
  if (is.null(selected)) {
    selected <- "shared"
  }
  control <- selectInput(
    inputId = "expression_projection_gene_color_mode",
    label = "Panel colors",
    choices = c(
      "Shared scale" = "shared",
      "Distinct colors" = "different"
    ),
    selected = selected
  )
  control$attribs$class <- paste(
    control$attribs$class,
    "cerebro-gene-control cerebro-control-unlocked"
  )
  control
})

outputOptions(
  output,
  "expression_projection_gene_color_mode_UI",
  suspendWhenHidden = FALSE
)
