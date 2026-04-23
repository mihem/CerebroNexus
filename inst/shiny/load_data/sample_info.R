##----------------------------------------------------------------------------##
## Sample info.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements that show some basic information about the loaded data set.
##----------------------------------------------------------------------------##
output[["load_data_sample_info_UI"]] <- renderUI({
  req(getExperiment())

  # Row 1
  row1 <- list()
  if (!is.null(getExperiment()$experiment_name)) {
    row1[[length(row1)+1]] <- valueBoxOutput("load_data_experiment_name")
  }
  if (!is.null(getCellNames()) && length(getCellNames()) > 0) {
    row1[[length(row1)+1]] <- valueBoxOutput("load_data_number_of_cells")
  }
  if (!is.null(getGroups()) && length(getGroups()) > 0) {
    row1[[length(row1)+1]] <- valueBoxOutput("load_data_number_of_grouping_variables")
  }

  # Row 2
  row2 <- list()
  if (!is.null(getExperiment()$organism)) {
    row2[[length(row2)+1]] <- valueBoxOutput("load_data_organism")
  }
  if (!is.null(getExperiment()$date_of_analysis)) {
    row2[[length(row2)+1]] <- valueBoxOutput("load_data_date_of_analysis")
  }
  if (!is.null(getExperiment()$date_of_export)) {
    row2[[length(row2)+1]] <- valueBoxOutput("load_data_date_of_export")
  }

  ui_content <- list()
  if (length(row1) > 0) ui_content[[length(ui_content)+1]] <- fluidRow(row1)
  if (length(row2) > 0) ui_content[[length(ui_content)+1]] <- fluidRow(row2)

  if (length(ui_content) > 0) tagList(ui_content) else NULL
})

##----------------------------------------------------------------------------##
## Value boxes that show:
## - experiment name
## - number of cells in data set
## - number of grouping variables
## - organism
## - date of analysis
## - date of export
##----------------------------------------------------------------------------##
## experiment name
output[["load_data_experiment_name"]] <- renderValueBox({
  valueBox(
    value = ifelse(
      !is.null(getExperiment()$experiment_name),
      getExperiment()$experiment_name,
      'not available'
    ),
    subtitle = "Experiment",
    color = "light-blue"
  )
})

## number of cells
output[["load_data_number_of_cells"]] <- renderValueBox({
  valueBox(
    value = formatC(length(getCellNames()), format = "f", big.mark = ",", digits = 0),
    subtitle = "Cells",
    color = "light-blue"
  )
})

## number of grouping variables
output[["load_data_number_of_grouping_variables"]] <- renderValueBox({
  valueBox(
    value = paste0( length(getGroups()), " groupings"),
    # subtitle = "Grouping variablesfasfaasfafdaf",
    subtitle = paste0(getGroups(), collapse = ", "),
    color = "light-blue"
  )
})

## organism
output[["load_data_organism"]] <- renderValueBox({
  box(
    title = "Organism",
    width = 5,
    background = "light-blue",
    ifelse(
      !is.null(getExperiment()$organism),
      getExperiment()$organism,
      'not available'
    )
  )
})

## date of analysis
## as.character() because the date is otherwise converted to interger
output[["load_data_date_of_analysis"]] <- renderValueBox({
  box(
    title = "Date when data was analyzed",
    width = 5,
    background = "light-blue",
    ifelse(
      !is.null(getExperiment()$date_of_analysis),
      as.character(getExperiment()$date_of_analysis),
      'not available'
    )
  )
})

## date of export
## as.character() because the date is otherwise converted to interger
output[["load_data_date_of_export"]] <- renderValueBox({
  box(
    title = "Date when data was exported",
    width = 5,
    background = "light-blue",
    ifelse(
      !is.null(getExperiment()$date_of_export),
      as.character(getExperiment()$date_of_export),
      'not available'
    )
  )
})
