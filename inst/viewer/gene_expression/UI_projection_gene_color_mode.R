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
  if (identical(display_mode, "rgb")) {
    return(NULL)
  }
  disabled <- n_genes <= 1 || display_mode != "separate"
  selected <- input[["expression_projection_gene_color_mode"]]
  if (disabled || is.null(selected)) {
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
    "cerebro-gene-control",
    if (disabled) "is-disabled" else "cerebro-control-unlocked"
  )
  if (disabled) {
    control$attribs$title <- if (n_genes <= 1) {
      "Select at least 2 genes to configure panel colors."
    } else {
      "Choose Separate panels to configure panel colors."
    }
    shinyjs::disabled(control)
  } else {
    control
  }
})

outputOptions(
  output,
  "expression_projection_gene_color_mode_UI",
  suspendWhenHidden = FALSE
)
