##----------------------------------------------------------------------------##
## Select method and table (group).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Resolve both initial selectors together. Changing the method updates only
## the group choices, so the surrounding controls do not disappear and rebind.
##----------------------------------------------------------------------------##
output[["marker_genes_select_method_and_table_UI"]] <- renderUI({
  methods <- getMethodsForMarkerGenes()
  if (!length(methods)) {
    return(p("No marker-gene methods available."))
  }
  fluidRow(
    column(
      6,
      selectInput(
        "marker_genes_selected_method",
        label = "Method",
        choices = methods,
        selected = methods[[1L]],
        width = "100%"
      )
    ),
    column(
      6,
      selectInput(
        "marker_genes_selected_table",
        label = "Grouping variable",
        choices = getGroupsWithMarkerGenes(methods[[1L]]),
        width = "100%"
      )
    )
  )
})

observeEvent(
  input[["marker_genes_selected_method"]],
  {
    method <- input[["marker_genes_selected_method"]]
    req(method %in% getMethodsForMarkerGenes())
    choices <- getGroupsWithMarkerGenes(method)
    current <- isolate(input[["marker_genes_selected_table"]])
    selected <- if (length(current) == 1L && current %in% choices) {
      current
    } else if (length(choices)) {
      choices[[1L]]
    } else {
      character()
    }
    updateSelectInput(
      session,
      "marker_genes_selected_table",
      choices = choices,
      selected = selected
    )
  },
  ignoreInit = TRUE
)
