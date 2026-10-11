##----------------------------------------------------------------------------##
## Expression in selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##
output[["expression_in_selected_cells_UI"]] <- renderUI({
  req(
    expression_projection_summary_ready(),
    expression_projection_selected_cells()
  )
  summary <- expression_summary_data()
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels in selected cells"),
        cerebroInfoButton("expression_in_selected_cells_info")
      ),
      plotly::plotlyOutput(
        "expression_in_selected_cells",
        height = paste0(
          max(400, ceiling(length(summary$series) / 3) * 300),
          "px"
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##
output[["expression_in_selected_cells"]] <- plotly::renderPlotly({
  req(
    expression_projection_summary_ready(),
    expression_projection_data(),
    expression_projection_coordinates(),
    expression_summary_data(),
    expression_projection_selected_cells()
  )
  selected_cells <- expression_projection_selected_cells()
  cells_df <- bind_cols(
    expression_projection_coordinates(),
    expression_projection_data()
  )
  cells_df <- cells_df %>%
    dplyr::rename(X1 = 1, X2 = 2)
  ## Stable selection is published only after deferred identity is available.
  ## First-frame metadata can contain compact row numbers, not real barcodes.
  cells_df[["selection_key"]] <- as.character(
    getMetaData()[["cell_barcode"]][expression_projection_cells_to_show()]
  )
  stable_selection <- "selection_key" %in%
    colnames(selected_cells) &&
    any(
      !is.na(selected_cells[["selection_key"]]) &
        nzchar(as.character(selected_cells[["selection_key"]]))
    )
  identifier <- if (!stable_selection) {
    paste0(cells_df[["X1"]], "-", cells_df[["X2"]])
  } else {
    NULL
  }
  groups <- ifelse(
    selectedCellMask(
      cells_df[["selection_key"]],
      identifier,
      selected_cells
    ),
    "selected",
    "not selected"
  )
  groups <- factor(groups, levels = c("selected", "not selected"))
  plotExpressionSummary(
    expression_summary_data()$series,
    groups,
    setNames(c("#e74c3c", "#7f8c8d"), levels(groups))
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "expression_in_selected_cells_info",
  expression_in_selected_cells_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_in_selected_cells_info <- list(
  title = "Expression levels in selected cells",
  text = p(
    "This plot compares selected and non-selected cells. Mean expression mode averages the selected genes for each cell; Separate panels and RGB modes retain one panel per gene or populated channel."
  )
)
