##----------------------------------------------------------------------------##
## UI elements to choose which projection/trajectory to show.
##----------------------------------------------------------------------------##
output[["expression_projection_select_projection_UI"]] <- renderUI({
  selectInput(
    "expression_projection_to_display",
    label = "Projection",
    choices = list(
      "Projections" = as.list(availableProjections()),
      "Trajectories" = as.list(available_trajectories())
    ),
    selected = expression_projection_default_projection()
  )
})

##----------------------------------------------------------------------------##
## UI elements with switch to plot genes in separate panels.
##----------------------------------------------------------------------------##
output[["expression_projection_genes_in_separate_panels_UI"]] <- renderUI({
  selectInput(
    inputId = "expression_projection_genes_in_separate_panels",
    label = "Display mode",
    choices = c(
      "Mean expression" = "combined",
      "Separate panels" = "separate",
      "RGB co-expression" = "rgb"
    ),
    selected = expression_projection_default_display_mode
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_genes_in_separate_panels_UI",
  suspendWhenHidden = FALSE
)

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
    display_mode <- expression_projection_default_display_mode
  }
  if (n_genes <= 1 || display_mode != "separate") {
    return(NULL)
  }
  input_id <- expression_panel_mode_input_id()
  selected <- isolate(expression_panel_color_mode())
  control <- selectInput(
    inputId = input_id,
    label = "Panel colours",
    choices = c(
      "Shared scale" = "shared",
      "Distinct colours" = "different"
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

##----------------------------------------------------------------------------##
## UI elements to set group filters.
##----------------------------------------------------------------------------##
registerGroupFiltersUI(
  output,
  "expression_projection",
  getGroups = getGroups,
  getGroupLevels = getGroupLevels,
  render_request = function() {
    input[["expression_projection_more_render_request"]]
  }
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
registerGroupFiltersInfo(
  input,
  "expression_projection",
  title = "Group filters for gene (set) expression",
  text = HTML(
    "
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection, the expression by group, and expression by pseudotime (if applicable).
    "
  )
)

registerCellViewPointBorder(output, "expression_projection", function() {
  current_page_display_defaults("gene")
})
