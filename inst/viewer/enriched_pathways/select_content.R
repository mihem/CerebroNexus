##----------------------------------------------------------------------------##
## Select method and table (group).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Resolve both initial selectors together. Changing the method updates only
## the group choices, so the surrounding controls do not disappear and rebind.
##----------------------------------------------------------------------------##
output[["enriched_pathways_select_method_and_table_UI"]] <- renderUI({
  methods <- getMethodsForEnrichedPathways()
  if (!length(methods)) {
    return(p("No pathway-enrichment methods available."))
  }
  fluidRow(
    column(
      6,
      selectInput(
        "enriched_pathways_selected_method",
        label = "Method",
        choices = methods,
        selected = methods[[1L]],
        width = "100%"
      )
    ),
    column(
      6,
      selectInput(
        "enriched_pathways_selected_table",
        label = "Grouping variable",
        choices = getGroupsWithEnrichedPathways(methods[[1L]]),
        width = "100%"
      )
    )
  )
})

observeEvent(
  input[["enriched_pathways_selected_method"]],
  {
    method <- input[["enriched_pathways_selected_method"]]
    req(method %in% getMethodsForEnrichedPathways())
    choices <- getGroupsWithEnrichedPathways(method)
    current <- isolate(input[["enriched_pathways_selected_table"]])
    selected <- if (length(current) == 1L && current %in% choices) {
      current
    } else if (length(choices)) {
      choices[[1L]]
    } else {
      character()
    }
    updateSelectInput(
      session,
      "enriched_pathways_selected_table",
      choices = choices,
      selected = selected
    )
  },
  ignoreInit = TRUE
)
