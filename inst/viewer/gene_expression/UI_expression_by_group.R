##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression by group
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with input selection (which group to show) and plot.
##----------------------------------------------------------------------------##
## Only layout changes should replace plot hosts and their input bindings.
## Keep this observer dormant while the Gene page is hidden.
expression_summary_group_height <- reactiveVal(NULL)
expression_summary_show_genes <- reactiveVal(FALSE)
observe({
  ## Subscribe to visibility only while waiting. Scrolling a finished summary
  ## out of view must not invalidate it; later data changes will check again.
  if (!isTRUE(isolate(input[["expression_summary_viewport_request"]]))) {
    req(input[["expression_summary_viewport_request"]], cancelOutput = TRUE)
  }
  req(
    identical(input[["sidebar"]], "geneExpression"),
    expression_projection_summary_ready()
  )
  summary <- tryCatch(
    expression_summary_data(),
    shiny.silent.error = function(e) NULL
  )
  expression_summary_group_height(
    if (is.null(summary)) {
      NULL
    } else {
      max(400, ceiling(length(summary$series) / 3) * 300)
    }
  )
  expression_summary_show_genes(!is.null(summary) && length(summary$genes) > 1)
})

output[["expression_by_group_UI"]] <- renderUI({
  height <- expression_summary_group_height()
  req(height)
  groups <- getGroups()
  selected <- isolate(input[["expression_by_group_selected_group"]])
  if (!length(selected) || !selected %in% groups) {
    selected <- groups[1]
  }
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by group"),
        cerebroInfoButton("expression_by_group_info")
      ),
      tagList(
        selectInput(
          "expression_by_group_selected_group",
          label = "Select a group to show expression by:",
          choices = groups,
          selected = selected,
          width = "100%"
        ),
        plotly::plotlyOutput(
          "expression_by_group",
          height = paste0(height, "px")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["expression_by_group"]] <- plotly::renderPlotly({
  if (!isTRUE(isolate(input[["expression_summary_viewport_request"]]))) {
    req(input[["expression_summary_viewport_request"]], cancelOutput = TRUE)
  }
  req(
    expression_projection_summary_ready(),
    expression_projection_data(),
    expression_summary_data(),
    input[["expression_by_group_selected_group"]]
  )
  cells_df <- expression_projection_data()
  selected_group <- input[["expression_by_group_selected_group"]]
  req(
    selected_group %in% colnames(cells_df),
    length(cells_df[[selected_group]]) == nrow(cells_df)
  )
  plotExpressionSummary(
    expression_summary_data()$series,
    cells_df[[selected_group]],
    reactive_colors()[[selected_group]]
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(input, "expression_by_group_info", expression_by_group_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_by_group_info <- list(
  title = "Expression levels by group",
  text = p(
    "Log-normalised expression by group. Mean expression mode averages the selected genes for each cell; Separate panels and RGB modes retain one panel per gene or populated channel."
  )
)
