##----------------------------------------------------------------------------##
## Show content or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------#
output[["extra_material_content_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  if (!length(categories)) {
    return(p("No extra material is included in this data set."))
  }
  req(input[["extra_material_selected_category"]] %in% categories)
  ## if selected category is `tables`
  if (input[["extra_material_selected_category"]] == 'tables') {
    if (!checkForExtraTables()) {
      return(p("No non-empty tables available."))
    }
    ##
    fluidRow(
      column(
        12,
        shinyWidgets::materialSwitch(
          inputId = "extra_material_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "extra_material_table_color_highlighting",
          label = "Highlight values with colours:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        )
      ),
      column(12, DT::dataTableOutput("extra_material_table"))
    )
    ## if selected category is `plots`
  } else if (input[["extra_material_selected_category"]] == 'plots') {
    ##
    fluidRow(
      column(
        12,
        shinyWidgets::materialSwitch(
          inputId = "extra_material_plot_interactive_switch",
          label = "Make plot interactive:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        )
      ),
      column(12, uiOutput("extra_material_plot_UI"))
    )
  }
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
output[["extra_material_table"]] <- DT::renderDataTable({
  req(input[["extra_material_selected_category"]] == "tables")
  selection <- extra_material_table_selection(
    extra_material_table_groups(),
    file_key = input[["extra_material_selected_file"]],
    sheet_key = input[["extra_material_selected_content"]]
  )
  req(!is.null(selection))
  results_df <- selection$sheet$table
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## if the table is empty, skip the processing and show and empty table
  ## (otherwise the procedure would result in an error)
  if (nrow(results_df) == 0) {
    results_df %>%
      as.data.frame() %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
    ## if there is at least 1 row, create proper table
  } else {
    prettifyTable(
      results_df,
      filter = extra_material_table_filter(nrow(results_df), ncol(results_df)),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = input[["extra_material_table_number_formatting"]],
      color_highlighting = input[["extra_material_table_color_highlighting"]],
      hide_long_columns = TRUE,
      download_file_name = paste0(
        "extra_material_tables_",
        gsub(
          "[^[:alnum:]_-]+",
          "_",
          paste(selection$group$label, selection$sheet$label, sep = "_")
        )
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element that contains interactive or plain version of plot, depending on
## switch status
##----------------------------------------------------------------------------##
output[["extra_material_plot_UI"]] <- renderUI({
  req(!is.null(input[["extra_material_plot_interactive_switch"]]))
  if (input[["extra_material_plot_interactive_switch"]] == TRUE) {
    plotly::plotlyOutput(
      "extra_material_plot_interactive",
      width = "auto",
      height = "70vh"
    )
  } else {
    plotOutput(
      "extra_material_plot_plain",
      width = "auto",
      height = "70vh"
    )
  }
})

##----------------------------------------------------------------------------##
## UI element that contains interactive version of plot
##----------------------------------------------------------------------------##
output[["extra_material_plot_interactive"]] <- plotly::renderPlotly({
  req(
    input[["extra_material_selected_category"]],
    input[["extra_material_selected_content"]]
  )
  ## fetch results
  plot <- getExtraPlot(input[["extra_material_selected_content"]])
  ## don't proceed if input is not of class "ggplot"
  req("ggplot" %in% class(plot))
  ## convert to plotly
  plot <- plotly::ggplotly(plot)
  ## return plot either with WebGL or without, depending on setting
  if (preferences$use_webgl == TRUE) {
    plot <- plot %>% plotly::toWebGL()
  }
  cerebro_plotly_toolbar(plot)
})

##----------------------------------------------------------------------------##
## UI element that contains plain version of plot
##----------------------------------------------------------------------------##
output[["extra_material_plot_plain"]] <- renderPlot({
  req(
    input[["extra_material_selected_category"]],
    input[["extra_material_selected_content"]]
  )
  ## fetch results
  plot <- getExtraPlot(input[["extra_material_selected_content"]])
  ## don't proceed if input is not of class "ggplot"
  req("ggplot" %in% class(plot))
  return(plot)
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(input, "extra_material_info", extra_material_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
extra_material_info <- list(
  title = "Extra material",
  text = HTML(
    "
    Browse tables and ggplot2 plots included with this dataset.<br>
    <br>
    Ask the dataset author for details about this material."
  )
)
