##----------------------------------------------------------------------------##
## UI elements with switch to plot genes in separate panels.
##----------------------------------------------------------------------------##
output[["expression_projection_genes_in_separate_panels_UI"]] <- renderUI({
  selected <- input[["expression_projection_genes_in_separate_panels"]]
  if (is.null(selected)) {
    selected <- "combined"
  }
  selectInput(
    inputId = "expression_projection_genes_in_separate_panels",
    label = "Display mode",
    choices = c(
      "Mean expression" = "combined",
      "Separate panels" = "separate",
      "RGB co-expression" = "rgb"
    ),
    selected = selected
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_genes_in_separate_panels_UI",
  suspendWhenHidden = FALSE
)
