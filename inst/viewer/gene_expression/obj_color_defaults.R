current_expression_color_defaults <- reactive({
  viewerExpressionColorSettings(
    Cerebro.options,
    viewer_requested_dataset_context()$dataset_key
  )
})

## A new input identity on each dataset load discards stale browser values.
## Within a load, the user's choice survives gene/panel UI rerenders.
expression_panel_mode_input_id <- reactive({
  expressionPanelModeInputId(viewer_requested_dataset_context())
})
expression_panel_color_mode <- reactive({
  value <- input[[expression_panel_mode_input_id()]]
  if (!is.null(value) && value %in% c("shared", "different")) {
    return(value)
  }
  if (current_expression_color_defaults()$panel_mode == "distinct") {
    "different"
  } else {
    "shared"
  }
})
