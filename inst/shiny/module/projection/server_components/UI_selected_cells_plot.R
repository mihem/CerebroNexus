##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["selected_cells_plot_UI"]] <- renderUI({
  req(projection_selected_cells())

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Plot of selected cells"),
        cerebroInfoButton(ns("details_selected_cells_plot_info"))
      ),
      tagList(
        selectInput(
          ns("selected_cells_plot_select_variable"),
          label = "Variable to compare:",
          choices = getVariableToCompareChoices()
        ),
        plotly::plotlyOutput(ns("details_selected_cells_plot"))
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["details_selected_cells_plot_info"]], {
  showModal(
    modalDialog(
      title = "Plot of selected cells",
      text = p("Depending on the variable selected to color cells in the dimensional reduction, this plot will show different things. If you select a categorical variable, e.g. 'sample' or 'cluster', you will get a bar plot showing which groups the cells selected with the box or lasso tool come from. Instead, if you select a continuous variable, e.g. the number of transcripts (nUMI), you will see a violin/box plot showing the distribution of that variable in the selected vs. non-selected cells."),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
